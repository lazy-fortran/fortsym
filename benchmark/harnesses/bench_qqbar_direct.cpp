// Direct FLINT qqbar benchmark for the values used by bench_algebraic.
//
// This intentionally excludes fortsym text parsing, canonical serialization,
// and the Fortran ABI. Rows therefore provide a separately scoped kernel
// baseline, not a claim that the timings are end-to-end interchangeable.

#include <algorithm>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <string>
#include <vector>

#include <flint/fmpq.h>
#include <flint/fmpz_poly.h>
#include <flint/qqbar.h>

namespace {

constexpr int kWarmups = 3;
constexpr int kRepetitions = 21;
constexpr int kBatch = 10;
constexpr int kColdRepetitions = 11;

const std::vector<std::string> kWorkloads{
    "normalize_irreducible", "normalize_reducible", "normalize_repeated",
    "gaussian_construct",    "add_trace",           "multiply_norm",
    "divide_gaussian",       "conjugate",           "signed_power",
    "principal_sqrt",        "component_signs"};
volatile int g_sign_sink = 0;

class FmpqValue {
public:
  FmpqValue() { fmpq_init(value); }
  ~FmpqValue() { fmpq_clear(value); }
  FmpqValue(const FmpqValue &) = delete;
  FmpqValue &operator=(const FmpqValue &) = delete;
  fmpq_t value;
};

class FmpzPolyValue {
public:
  FmpzPolyValue() { fmpz_poly_init(value); }
  ~FmpzPolyValue() { fmpz_poly_clear(value); }
  FmpzPolyValue(const FmpzPolyValue &) = delete;
  FmpzPolyValue &operator=(const FmpzPolyValue &) = delete;
  fmpz_poly_t value;
};

class QqbarValue {
public:
  QqbarValue() { qqbar_init(value); }
  ~QqbarValue() { qqbar_clear(value); }
  QqbarValue(const QqbarValue &) = delete;
  QqbarValue &operator=(const QqbarValue &) = delete;
  qqbar_t value;
};

class QqbarVector {
public:
  explicit QqbarVector(slong size)
      : size_(size), values_(_qqbar_vec_init(size)) {}
  ~QqbarVector() { _qqbar_vec_clear(values_, size_); }
  QqbarVector(const QqbarVector &) = delete;
  QqbarVector &operator=(const QqbarVector &) = delete;
  qqbar_ptr data() { return values_; }

private:
  slong size_;
  qqbar_ptr values_;
};

double seconds() {
  using Clock = std::chrono::steady_clock;
  return std::chrono::duration<double>(Clock::now().time_since_epoch()).count();
}

void set_from_polynomial(qqbar_t out, const std::vector<slong> &coefficients,
                         slong root_index, slong scale) {
  FmpzPolyValue polynomial;
  for (slong i = 0; i < static_cast<slong>(coefficients.size()); ++i) {
    fmpz_poly_set_coeff_si(polynomial.value, i, coefficients[i] * scale);
  }
  const slong degree = fmpz_poly_degree(polynomial.value);
  QqbarVector roots(degree);
  qqbar_roots_fmpz_poly(roots.data(), polynomial.value, 0);
  qqbar_set(out, roots.data() + root_index);
}

void set_rational(qqbar_t out, slong numerator, ulong denominator) {
  FmpqValue rational;
  fmpq_set_si(rational.value, numerator, denominator);
  qqbar_set_fmpq(out, rational.value);
}

void set_gaussian(qqbar_t out) {
  QqbarValue real_part;
  QqbarValue imag_part;
  set_rational(real_part.value, 1, 2);
  set_rational(imag_part.value, 3, 4);
  qqbar_set_re_im(out, real_part.value, imag_part.value);
}

bool perform(const std::string &workload, slong scale, qqbar_t out) {
  QqbarValue left;
  QqbarValue right;

  if (workload == "normalize_irreducible") {
    set_from_polynomial(out, {-2, 0, 1}, 0, scale);
  } else if (workload == "normalize_reducible") {
    set_from_polynomial(out, {6, -2, -3, 1}, 1, scale);
  } else if (workload == "normalize_repeated") {
    set_from_polynomial(out, {1, -2, 1}, 1, scale);
  } else if (workload == "gaussian_construct") {
    // The scaled source rationals reduce to the same exact components.
    FmpqValue real_value;
    FmpqValue imag_value;
    QqbarValue real_part;
    QqbarValue imag_part;
    fmpq_set_si(real_value.value, scale, 2 * scale);
    fmpq_set_si(imag_value.value, 3 * scale, 4 * scale);
    qqbar_set_fmpq(real_part.value, real_value.value);
    qqbar_set_fmpq(imag_part.value, imag_value.value);
    qqbar_set_re_im(out, real_part.value, imag_part.value);
  } else if (workload == "add_trace") {
    set_from_polynomial(left.value, {13, -16, 16}, 0, scale);
    set_from_polynomial(right.value, {13, -16, 16}, 1, scale);
    qqbar_add(out, left.value, right.value);
  } else if (workload == "multiply_norm") {
    set_from_polynomial(left.value, {13, -16, 16}, 0, scale);
    set_from_polynomial(right.value, {13, -16, 16}, 1, scale);
    qqbar_mul(out, left.value, right.value);
  } else if (workload == "divide_gaussian") {
    set_from_polynomial(left.value, {2, -2, 1}, 0, scale);
    set_from_polynomial(right.value, {2, -2, 1}, 1, scale);
    qqbar_div(out, left.value, right.value);
  } else if (workload == "conjugate") {
    set_from_polynomial(left.value, {13, -16, 16}, 0, scale);
    qqbar_conj(out, left.value);
  } else if (workload == "signed_power") {
    set_from_polynomial(left.value, {2, -2, 1}, 0, scale);
    qqbar_pow_si(out, left.value, 8);
  } else if (workload == "principal_sqrt" || workload == "component_signs") {
    set_from_polynomial(left.value, {2, 1}, 0, scale);
    qqbar_sqrt(out, left.value);
    if (workload == "component_signs") {
      g_sign_sink = qqbar_sgn_re(out) + 3 * qqbar_sgn_im(out);
    }
  } else {
    return false;
  }
  return true;
}

bool validate(const std::string &workload, const qqbar_t value) {
  QqbarValue expected;
  if (workload == "normalize_irreducible" ||
      workload == "normalize_reducible") {
    QqbarValue square;
    qqbar_mul(square.value, value, value);
    qqbar_set_si(expected.value, 2);
    return qqbar_equal(square.value, expected.value) &&
           qqbar_sgn_re(value) == 1 && qqbar_sgn_im(value) == 0;
  }
  if (workload == "normalize_repeated" || workload == "add_trace") {
    qqbar_one(expected.value);
    return qqbar_equal(value, expected.value);
  }
  if (workload == "gaussian_construct") {
    QqbarValue conjugate;
    QqbarValue trace;
    QqbarValue norm;
    qqbar_conj(conjugate.value, value);
    qqbar_add(trace.value, value, conjugate.value);
    qqbar_mul(norm.value, value, conjugate.value);
    qqbar_one(expected.value);
    if (!qqbar_equal(trace.value, expected.value)) {
      return false;
    }
    set_rational(expected.value, 13, 16);
    return qqbar_equal(norm.value, expected.value) &&
           qqbar_sgn_re(value) == 1 && qqbar_sgn_im(value) == 1;
  }
  if (workload == "multiply_norm") {
    set_rational(expected.value, 13, 16);
    return qqbar_equal(value, expected.value);
  }
  if (workload == "divide_gaussian") {
    qqbar_i(expected.value);
    return qqbar_equal(value, expected.value);
  }
  if (workload == "conjugate") {
    QqbarValue gaussian;
    QqbarValue trace;
    QqbarValue norm;
    set_gaussian(gaussian.value);
    qqbar_add(trace.value, value, gaussian.value);
    qqbar_mul(norm.value, value, gaussian.value);
    qqbar_one(expected.value);
    if (!qqbar_equal(trace.value, expected.value)) {
      return false;
    }
    set_rational(expected.value, 13, 16);
    return qqbar_equal(norm.value, expected.value) &&
           qqbar_sgn_re(value) == 1 && qqbar_sgn_im(value) == -1;
  }
  if (workload == "signed_power") {
    qqbar_set_si(expected.value, 16);
    return qqbar_equal(value, expected.value);
  }
  if (workload == "principal_sqrt" || workload == "component_signs") {
    QqbarValue square;
    qqbar_mul(square.value, value, value);
    qqbar_set_si(expected.value, -2);
    return qqbar_equal(square.value, expected.value) &&
           qqbar_sgn_re(value) == 0 && qqbar_sgn_im(value) == 1;
  }
  return false;
}

double percentile(const std::vector<double> &ordered, double fraction) {
  const auto index =
      static_cast<size_t>(fraction * static_cast<double>(ordered.size() - 1));
  return ordered[index];
}

void emit(const std::string &workload, const std::string &scope,
          std::vector<double> samples, int warmups, int repetitions, int batch,
          bool correct) {
  std::sort(samples.begin(), samples.end());
  std::cout << "1,flint_qqbar_direct_no_text," << scope << ',' << workload
            << ',' << warmups << ',' << repetitions << ',' << batch << ','
            << std::scientific << std::setprecision(8)
            << percentile(samples, 0.50) << ',' << percentile(samples, 0.05)
            << ',' << percentile(samples, 0.95) << ',' << samples.front() << ','
            << samples.back() << ',' << (correct ? "true" : "false") << '\n';
}

void benchmark_warm(const std::string &workload) {
  QqbarValue result;
  for (int repetition = 0; repetition < kWarmups; ++repetition) {
    for (int k = 0; k < kBatch; ++k) {
      perform(workload, 1, result.value);
    }
  }
  std::vector<double> samples(kRepetitions);
  for (int repetition = 0; repetition < kRepetitions; ++repetition) {
    const double started = seconds();
    for (int k = 0; k < kBatch; ++k) {
      perform(workload, 1, result.value);
    }
    samples[repetition] = (seconds() - started) / kBatch;
  }
  emit(workload, "warm_same_value", samples, kWarmups, kRepetitions, kBatch,
       validate(workload, result.value));
}

void benchmark_cold(const std::string &workload) {
  QqbarValue result;
  bool correct = true;
  std::vector<double> samples(kColdRepetitions);
  for (int repetition = 0; repetition < kColdRepetitions; ++repetition) {
    const double started = seconds();
    const bool ok = perform(workload, repetition + 2, result.value);
    samples[repetition] = seconds() - started;
    correct = correct && ok && validate(workload, result.value);
  }
  emit(workload, "cold_distinct_encoding", samples, 0, kColdRepetitions, 1,
       correct);
}

} // namespace

int main() {
  std::cout << "schema,backend,scope,workload,warmups,repetitions,batch,"
               "median_seconds,p05_seconds,p95_seconds,min_seconds,"
               "max_seconds,correct\n";
  for (const auto &workload : kWorkloads) {
    benchmark_warm(workload);
    benchmark_cold(workload);
  }
  return 0;
}
