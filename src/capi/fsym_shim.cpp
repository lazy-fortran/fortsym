/* Implementation of fortsym's shim over SymEngine. See fsym_shim.h for why
 * each entry point exists. */

#include <cstring>
#include <cstdint>
#include <cmath>
#include <charconv>
#include <cstdlib>
#include <dlfcn.h>
#include <limits>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>
#include <vector>
#include <unistd.h>

#include <mpfr.h>
#include <symengine/basic.h>
#include <symengine/mul.h>
#include <symengine/pow.h>
#include <symengine/printers.h>
#include <symengine/series.h>
#include <symengine/simplify.h>
#include <symengine/subs.h>
#include <symengine/symbol.h>
#include <symengine/symengine_casts.h>
#include <symengine/symengine_exception.h>
#include <symengine/visitor.h>

#include <flint/flint.h>
#include <flint/arb.h>
#include <flint/fmpq.h>
#include <flint/fmpz_poly.h>
#include <flint/qqbar.h>

#ifdef HAVE_SYMENGINE_FLINT
#include <symengine/polys/cancel.h>
#include <symengine/polys/uintpoly_flint.h>
#endif

#include "fsym_shim.h"

/* SymEngine 0.14 names its opaque C++ storage CRCPBasic; 0.15 exposes only the
 * CRCPBasic_C placeholder. Both document the same layout contract: the object
 * has the size and alignment of one RCP<const Basic>. Restate that private
 * storage here, with compile-time guards against any future layout change. */
struct CRCPBasic {
    SymEngine::RCP<const SymEngine::Basic> m;
};

static_assert(sizeof(CRCPBasic) == sizeof(CRCPBasic_C),
              "fsym_shim: CRCPBasic size no longer matches SymEngine's C "
              "placeholder; the cwrapper layout contract has changed.");
static_assert(alignof(CRCPBasic) == alignof(CRCPBasic_C),
              "fsym_shim: CRCPBasic alignment no longer matches SymEngine's C "
              "placeholder; the cwrapper layout contract has changed.");

namespace
{

/* Rendered-text buffer. Thread-local so concurrent Fortran callers cannot
 * overwrite each other's pending fetch. */
thread_local std::string g_render_buffer;
thread_local std::string g_exact_buffer;
thread_local std::string g_algebraic_buffer;
const int64_t kMaxExactPowExponent = 1000000;
const size_t kMaxExactInputBytes = 1024UL*1024UL;
const size_t kMaxExactOutputBytes = 16UL*1024UL*1024UL;
const slong kMaxAlgebraicDegree = 32;
const slong kMaxAlgebraicHeightBits = 4096;
const int64_t kMaxAlgebraicPowExponent = 64;
const size_t kMaxAlgebraicBytes = 64UL*1024UL;

constexpr bool supported_mpfr_abi(int major, int minor, int patch)
{
    /* MPFR 4.2.2 states that its changes from 4.2.1 alter neither ABI nor
     * API: https://www.mpfr.org/mpfr-4.2.2/.  Patch releases in this line
     * therefore share the ABI used by the exact-real projection below. */
    return major == 4 && minor == 2 && patch >= 1;
}

constexpr bool supported_flint_api(int major, int minor)
{
    /* FortSym uses FLINT's public fmpq, fmpz_poly, and qqbar interfaces.
     * FLINT publishes 3.0.0 through 3.6.0 as one 3.x release series:
     * https://flintlib.org/downloads.html.  The official python-flint
     * bindings likewise support every available FLINT version from 3.0:
     * https://github.com/flintlib/python-flint#compatibility-table. */
    return major == 3 && minor >= 0;
}

static_assert(sizeof(slong) >= sizeof(int64_t),
              "fsym_shim: FLINT slong cannot hold the exact-power exponent");
static_assert(supported_flint_api(3, 0),
              "fsym_shim: FLINT 3.0 must remain accepted");
static_assert(!supported_flint_api(2, 9),
              "fsym_shim: FLINT 2.x must remain rejected");
static_assert(supported_flint_api(__FLINT_VERSION, __FLINT_VERSION_MINOR),
              "fsym_shim: fortsym requires the FLINT 3.x public API");
static_assert(supported_mpfr_abi(4, 2, 1),
              "fsym_shim: MPFR 4.2.1 must remain accepted");
static_assert(!supported_mpfr_abi(4, 1, 0),
              "fsym_shim: older MPFR ABI must remain rejected");
static_assert(supported_mpfr_abi(MPFR_VERSION_MAJOR, MPFR_VERSION_MINOR,
                                 MPFR_VERSION_PATCHLEVEL),
              "fsym_shim: fortsym requires the MPFR 4.2 stable ABI");
static_assert(std::numeric_limits<double>::is_iec559
                  && std::numeric_limits<double>::digits == 53,
              "fsym_shim: exact real projection requires binary64 double");

class FmpqValue
{
public:
    FmpqValue() { fmpq_init(value); }
    ~FmpqValue() { fmpq_clear(value); }

    FmpqValue(const FmpqValue &) = delete;
    FmpqValue &operator=(const FmpqValue &) = delete;

    fmpq_t value;
};

class MpfrValue
{
public:
    explicit MpfrValue(mpfr_prec_t precision) { mpfr_init2(value, precision); }
    ~MpfrValue() { mpfr_clear(value); }

    MpfrValue(const MpfrValue &) = delete;
    MpfrValue &operator=(const MpfrValue &) = delete;

    mpfr_t value;
};

class ArbValue
{
public:
    ArbValue() { arb_init(value); }
    ~ArbValue() { arb_clear(value); }

    ArbValue(const ArbValue &) = delete;
    ArbValue &operator=(const ArbValue &) = delete;

    arb_t value;
};

class FmpzValue
{
public:
    FmpzValue() { fmpz_init(value); }
    ~FmpzValue() { fmpz_clear(value); }

    FmpzValue(const FmpzValue &) = delete;
    FmpzValue &operator=(const FmpzValue &) = delete;

    fmpz_t value;
};

class FmpzPolyValue
{
public:
    FmpzPolyValue() { fmpz_poly_init(value); }
    ~FmpzPolyValue() { fmpz_poly_clear(value); }

    FmpzPolyValue(const FmpzPolyValue &) = delete;
    FmpzPolyValue &operator=(const FmpzPolyValue &) = delete;

    fmpz_poly_t value;
};

class QqbarValue
{
public:
    QqbarValue() { qqbar_init(value); }
    ~QqbarValue() { qqbar_clear(value); }

    QqbarValue(const QqbarValue &) = delete;
    QqbarValue &operator=(const QqbarValue &) = delete;

    qqbar_t value;
};

class QqbarVector
{
public:
    explicit QqbarVector(slong size)
        : size_(size), values_(_qqbar_vec_init(size))
    {
    }
    ~QqbarVector() { _qqbar_vec_clear(values_, size_); }

    QqbarVector(const QqbarVector &) = delete;
    QqbarVector &operator=(const QqbarVector &) = delete;

    qqbar_ptr data() { return values_; }

private:
    slong size_;
    qqbar_ptr values_;
};

struct FlintStringDeleter
{
    void operator()(char *text) const noexcept
    {
        if (text != nullptr) {
            flint_free(text);
        }
    }
};

using FlintString = std::unique_ptr<char, FlintStringDeleter>;

bool bounded_text_length(const char *text, size_t maximum, size_t &length)
{
    length = 0;
    if (text == nullptr) {
        return false;
    }
    while (text[length] != '\0') {
        ++length;
        if (length > maximum) {
            return false;
        }
    }
    return length > 0;
}

bool parse_exact(FmpqValue &out, const char *text)
{
    size_t length = 0;
    if (!bounded_text_length(text, kMaxExactInputBytes, length)) {
        return false;
    }
    if (fmpq_set_str(out.value, text, 10) != 0) {
        return false;
    }
    if (fmpz_is_zero(fmpq_denref(out.value))) {
        return false;
    }
    fmpq_canonicalise(out.value);
    return true;
}

bool exact_output_fits(const fmpq_t value)
{
    size_t bytes = fmpz_sizeinbase(fmpq_numref(value), 10);
    if (fmpz_sgn(fmpq_numref(value)) < 0) {
        ++bytes;
    }
    if (!fmpz_is_one(fmpq_denref(value))) {
        const size_t denominator
            = fmpz_sizeinbase(fmpq_denref(value), 10);
        if (bytes > kMaxExactOutputBytes - 1
            || denominator > kMaxExactOutputBytes - bytes - 1) {
            return false;
        }
        bytes += denominator + 1;
    }
    return bytes <= kMaxExactOutputBytes;
}

bool exact_power_fits(const fmpq_t value, int64_t exponent)
{
    if (exponent == 0) {
        return true;
    }
    const uint64_t magnitude = exponent < 0
                                   ? static_cast<uint64_t>(-exponent)
                                   : static_cast<uint64_t>(exponent);
    size_t numerator = fmpz_sizeinbase(fmpq_numref(value), 10);
    size_t denominator = fmpz_sizeinbase(fmpq_denref(value), 10);
    if (exponent < 0) {
        const size_t temporary = numerator;
        numerator = denominator;
        denominator = temporary;
    }
    if (numerator > kMaxExactOutputBytes/magnitude
        || denominator > kMaxExactOutputBytes/magnitude) {
        return false;
    }
    const size_t numerator_result = numerator*magnitude;
    const size_t denominator_result = denominator*magnitude;
    if (numerator_result > kMaxExactOutputBytes - 2) {
        return false;
    }
    return denominator_result
           <= kMaxExactOutputBytes - numerator_result - 2;
}

size_t hold_exact(const fmpq_t value)
{
    if (!exact_output_fits(value)) {
        g_exact_buffer.clear();
        return 0;
    }
    FlintString text(fmpq_get_str(nullptr, 10, value));
    if (!text) {
        g_exact_buffer.clear();
        return 0;
    }
    g_exact_buffer = text.get();
    return g_exact_buffer.size();
}

bool algebraic_within_limits(const qqbar_t value)
{
    return qqbar_within_limits(value, kMaxAlgebraicDegree,
                               kMaxAlgebraicHeightBits)
           != 0;
}

bool parse_algebraic(QqbarValue &out, const char *text)
{
    constexpr std::string_view prefix("qqbar1:");
    size_t length = 0;
    if (!bounded_text_length(text, kMaxAlgebraicBytes, length)) {
        return false;
    }
    const std::string_view input(text, length);
    if (input.substr(0, prefix.size()) != prefix) {
        return false;
    }
    const size_t index_end = input.find(':', prefix.size());
    if (index_end == std::string_view::npos
        || index_end == prefix.size()) {
        return false;
    }

    size_t root_index = 0;
    const std::string_view index_text
        = input.substr(prefix.size(), index_end - prefix.size());
    const auto parsed_index = std::from_chars(
        index_text.data(), index_text.data() + index_text.size(), root_index);
    if (parsed_index.ec != std::errc()
        || parsed_index.ptr != index_text.data() + index_text.size()) {
        return false;
    }

    FmpzPolyValue polynomial;
    FmpzValue coefficient;
    const std::string_view coefficients = input.substr(index_end + 1);
    size_t start = 0;
    slong count = 0;
    while (start <= coefficients.size()) {
        size_t end = coefficients.find(',', start);
        if (end == std::string_view::npos) {
            end = coefficients.size();
        }
        const std::string_view token = coefficients.substr(start, end - start);
        if (token.empty() || count > kMaxAlgebraicDegree) {
            return false;
        }
        const std::string token_string(token);
        if (fmpz_set_str(coefficient.value, token_string.c_str(), 10) != 0
            || fmpz_bits(coefficient.value) > kMaxAlgebraicHeightBits) {
            return false;
        }
        fmpz_poly_set_coeff_fmpz(polynomial.value, count, coefficient.value);
        ++count;
        if (end == coefficients.size()) {
            break;
        }
        start = end + 1;
    }

    const slong degree = fmpz_poly_degree(polynomial.value);
    if (degree < 1 || degree > kMaxAlgebraicDegree
        || count != degree + 1
        || root_index >= static_cast<size_t>(degree)) {
        return false;
    }
    QqbarVector roots(degree);
    qqbar_roots_fmpz_poly(roots.data(), polynomial.value, 0);
    qqbar_set(out.value, roots.data() + root_index);
    return algebraic_within_limits(out.value);
}

size_t hold_algebraic(const qqbar_t value)
{
    g_algebraic_buffer.clear();
    if (!algebraic_within_limits(value)) {
        return 0;
    }

    const slong degree = qqbar_degree(value);
    QqbarVector conjugates(degree);
    qqbar_conjugates(conjugates.data(), value);
    slong root_index = -1;
    for (slong i = 0; i < degree; ++i) {
        if (qqbar_equal(value, conjugates.data() + i)) {
            root_index = i;
            break;
        }
    }
    if (root_index < 0) {
        return 0;
    }

    std::string serialized = "qqbar1:" + std::to_string(root_index) + ":";
    FmpzValue coefficient;
    for (slong i = 0; i <= degree; ++i) {
        fmpz_poly_get_coeff_fmpz(coefficient.value, QQBAR_POLY(value), i);
        FlintString text(fmpz_get_str(nullptr, 10, coefficient.value));
        if (!text) {
            return 0;
        }
        if (i != 0) {
            serialized.push_back(',');
        }
        serialized += text.get();
        if (serialized.size() > kMaxAlgebraicBytes) {
            return 0;
        }
    }
    g_algebraic_buffer = std::move(serialized);
    return g_algebraic_buffer.size();
}

size_t hold_algebraic_component(const char *text, bool real_part)
{
    try {
        QqbarValue input;
        QqbarValue component;
        g_algebraic_buffer.clear();
        if (!parse_algebraic(input, text)) {
            return 0;
        }
        if (real_part) {
            qqbar_re(component.value, input.value);
        } else {
            qqbar_im(component.value, input.value);
        }
        return hold_algebraic(component.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

bool algebraic_gaussian_rational_text(const char *text,
                                      std::string &expression)
{
    QqbarValue input;
    QqbarValue real_part;
    QqbarValue imag_part;
    FmpqValue real_value;
    FmpqValue imag_value;
    if (!parse_algebraic(input, text)) {
        return false;
    }
    qqbar_re(real_part.value, input.value);
    qqbar_im(imag_part.value, input.value);
    if (!qqbar_is_rational(real_part.value)
        || !qqbar_is_rational(imag_part.value)) {
        return false;
    }
    qqbar_get_fmpq(real_value.value, real_part.value);
    qqbar_get_fmpq(imag_value.value, imag_part.value);
    FlintString real_text(fmpq_get_str(nullptr, 10, real_value.value));
    FlintString imag_text(fmpq_get_str(nullptr, 10, imag_value.value));
    if (!real_text || !imag_text) {
        return false;
    }

    const bool real_zero = fmpz_is_zero(fmpq_numref(real_value.value));
    const bool imag_zero = fmpz_is_zero(fmpq_numref(imag_value.value));
    if (real_zero && imag_zero) {
        expression = "0";
    } else if (imag_zero) {
        expression = real_text.get();
    } else if (real_zero) {
        expression = "(" + std::string(imag_text.get()) + ")*I";
    } else {
        expression = std::string(real_text.get()) + " + ("
                     + std::string(imag_text.get()) + ")*I";
    }
    return true;
}

inline const SymEngine::RCP<const SymEngine::Basic> &deref(const basic s)
{
    return reinterpret_cast<const CRCPBasic *>(s)->m;
}

inline SymEngine::RCP<const SymEngine::Basic> &deref(basic s)
{
    return reinterpret_cast<CRCPBasic *>(s)->m;
}

/*! Default ceiling on intermediate node count during normalization. Expansion
 *  of a large kernel can grow super-linearly, and fortsym runs inside a build,
 *  where a hang is worse than an undecided answer. */
const unsigned long kDefaultMaxNodes = 200000UL;

/*! Rough size of an expression tree, used only as a growth guard. */
unsigned long node_count(const SymEngine::RCP<const SymEngine::Basic> &e,
                         unsigned long limit)
{
    unsigned long n = 1;
    for (const auto &arg : e->get_args()) {
        if (n > limit) {
            return n;
        }
        n += node_count(arg, limit);
    }
    return n;
}

/*! Canonicalizes exponentials so that structurally different spellings of the
 *  same quantity collapse:
 *
 *    exp(-I*(x + y))  ->  exp(-I*x - I*y)   (exponent expanded)
 *    exp(log(u))      ->  u
 *    exp(n*log(u))    ->  u**n
 *
 *  The first rule is what makes the angle-sum identities decidable: without it
 *  exp(-I*(x+y)) and exp(-I*x-I*y) are distinct terms and never cancel.
 *
 *  SymEngine represents exp(a) as Pow(E, a), so this hooks Pow. */
SymEngine::RCP<const SymEngine::Basic>
exp_normalize(const SymEngine::RCP<const SymEngine::Basic> &e)
{
    SymEngine::map_basic_basic replacements;
    for (const auto &arg : e->get_args()) {
        const auto normalized = exp_normalize(arg);
        if (normalized != arg) {
            replacements[arg] = normalized;
        }
    }
    const auto rebuilt = replacements.empty()
                             ? e
                             : SymEngine::xreplace(e, replacements);
    if (!SymEngine::is_a<SymEngine::Pow>(*rebuilt)) {
        return rebuilt;
    }

    const auto &power = SymEngine::down_cast<const SymEngine::Pow &>(*rebuilt);
    const auto base = power.get_base();
    auto exponent = power.get_exp();
    if (!SymEngine::eq(*base, *SymEngine::E)) {
        return rebuilt;
    }

    exponent = SymEngine::expand(exponent);
    if (SymEngine::is_a<SymEngine::Log>(*exponent)) {
        return exponent->get_args()[0];
    }
    if (SymEngine::is_a<SymEngine::Mul>(*exponent)) {
        for (const auto &factor : exponent->get_args()) {
            if (SymEngine::is_a<SymEngine::Log>(*factor)) {
                const auto cofactor = SymEngine::div(exponent, factor);
                return SymEngine::pow(factor->get_args()[0], cofactor);
            }
        }
    }
    return SymEngine::pow(base, exponent);
}

/*! Exponential normal form: the shared front half of fsym_zero_test and
 *  fsym_normal_form. Throws whatever SymEngine throws. */
SymEngine::RCP<const SymEngine::Basic>
normal_form(const SymEngine::RCP<const SymEngine::Basic> &e)
{
    return exp_normalize(SymEngine::rewrite_as_exp(e));
}

/*! True when every node of e lies in the fragment fsym_zero_test can decide:
 *  rational expressions over symbols, numbers, powers, logarithms and
 *  exponentials.
 *
 *  This guard is what keeps a non-zero canonical form from being reported as a
 *  non-identity. rewrite_as_exp does not throw on functions it has no rule for
 *  -- gamma, erf, zeta, the Bessel family and abs/sign/floor all pass through
 *  untouched -- so without this check gamma(x+1) - x*gamma(x) would normalize
 *  to itself, fail to cancel, and be reported as NONZERO even though it is a
 *  true identity. Confidently wrong is far worse than undecided, so anything
 *  outside the fragment yields FSYM_ZERO_UNKNOWN and the caller falls back to a
 *  numeric probe.
 *
 *  Structural cancellation to zero stays trustworthy regardless of the heads
 *  involved, so the caller applies this test only before reporting NONZERO. */
bool in_decidable_fragment(const SymEngine::RCP<const SymEngine::Basic> &e)
{
    switch (e->get_type_code()) {
    case SymEngine::SYMENGINE_SYMBOL:
    case SymEngine::SYMENGINE_INTEGER:
    case SymEngine::SYMENGINE_RATIONAL:
    case SymEngine::SYMENGINE_COMPLEX:
    case SymEngine::SYMENGINE_REAL_DOUBLE:
    case SymEngine::SYMENGINE_COMPLEX_DOUBLE:
    case SymEngine::SYMENGINE_CONSTANT:
    case SymEngine::SYMENGINE_ADD:
    case SymEngine::SYMENGINE_MUL:
    case SymEngine::SYMENGINE_POW:
    case SymEngine::SYMENGINE_LOG:
        break;
    default:
        return false;
    }

    for (const auto &arg : e->get_args()) {
        if (!in_decidable_fragment(arg)) {
            return false;
        }
    }
    return true;
}

/*! Cancel the common polynomial factor between numerator and denominator, so
 *  (x**2 - 1)/(x - 1) reads as x + 1.
 *
 *  SymEngine's cancel() dispatches to gcd_upoly, which FLINT provides only for
 *  univariate polynomials, so this handles the single-generator case and
 *  returns null otherwise. Multivariate rational expressions therefore keep
 *  their uncancelled spelling. That costs readability, not correctness:
 *  fsym_zero_test decides multivariate rational identities regardless, because
 *  it compares expanded numerators rather than cancelled fractions.
 *
 *  Returns null when cancellation does not apply, which the caller treats as
 *  "no candidate" rather than as an error. */
SymEngine::RCP<const SymEngine::Basic>
try_cancel(const SymEngine::RCP<const SymEngine::Basic> &e)
{
#ifdef HAVE_SYMENGINE_FLINT
    using namespace SymEngine;
    try {
        RCP<const Basic> numer, denom;
        as_numer_denom(e, outArg(numer), outArg(denom));
        if (eq(*denom, *one)) {
            return RCP<const Basic>();
        }

        RCP<const UIntPolyFlint> rn, rd, common;
        cancel<UIntPolyFlint>(numer, denom, outArg(rn), outArg(rd),
                              outArg(common));
        if (rn.is_null() || rd.is_null()) {
            return RCP<const Basic>(); // multivariate; cancel() left them unset
        }
        return div(rn->as_symbolic(), rd->as_symbolic());
    } catch (...) {
        return RCP<const Basic>();
    }
#else
    (void)e;
    return SymEngine::RCP<const SymEngine::Basic>();
#endif
}

} // namespace

extern "C" {

int fsym_make_temp_directory(char *path, size_t size)
{
    constexpr char pattern[] = "/tmp/fortsym_XXXXXX";
    if (path == nullptr || size < sizeof(pattern)) {
        return 0;
    }
    std::memcpy(path, pattern, sizeof(pattern));
    return ::mkdtemp(path) != nullptr;
}

int fsym_remove_temp_directory(const char *path)
{
    return path != nullptr && ::rmdir(path) == 0;
}

/* ---------------------------------------------------------------- probes -- */

int fsym_have_llvm(void)
{
#ifdef HAVE_SYMENGINE_LLVM
    return 1;
#else
    return 0;
#endif
}

int fsym_have_mpfr(void)
{
#ifdef HAVE_SYMENGINE_MPFR
    return 1;
#else
    return 0;
#endif
}

const char *fsym_symengine_version(void)
{
    return symengine_version();
}

/* ---------------------------------------------------------------- strings -- */

size_t fsym_str_render(const basic s, int mode)
{
    try {
        switch (mode) {
        case FSYM_STR_CCODE:
            g_render_buffer = SymEngine::ccode(
                *deref(s), SymEngine::CodePrinterPrecision::Double);
            break;
        case FSYM_STR_LATEX:
            g_render_buffer = SymEngine::latex(*deref(s));
            break;
        case FSYM_STR_JULIA:
            g_render_buffer = SymEngine::julia_str(*deref(s));
            break;
        case FSYM_STR_DEFAULT:
        default:
            g_render_buffer = SymEngine::str(*deref(s));
            break;
        }
    } catch (...) {
        g_render_buffer.clear();
        return 0;
    }
    return g_render_buffer.size();
}

size_t fsym_str_fetch(char *buf, size_t n)
{
    const size_t count = n < g_render_buffer.size() ? n : g_render_buffer.size();
    if (count > 0 && buf != nullptr) {
        std::memcpy(buf, g_render_buffer.data(), count);
    }
    return count;
}

/* ------------------------------------------------------ exact arithmetic -- */

size_t fsym_exact_normalize(const char *value)
{
    try {
        FmpqValue parsed;
        g_exact_buffer.clear();
        if (!parse_exact(parsed, value)) {
            return 0;
        }
        return hold_exact(parsed.value);
    } catch (...) {
        g_exact_buffer.clear();
        return 0;
    }
}

size_t fsym_exact_binary(const char *left, const char *right, int operation)
{
    try {
        FmpqValue lhs;
        FmpqValue rhs;
        FmpqValue result;
        g_exact_buffer.clear();
        if (!parse_exact(lhs, left) || !parse_exact(rhs, right)) {
            return 0;
        }

        switch (operation) {
        case FSYM_EXACT_ADD:
            fmpq_add(result.value, lhs.value, rhs.value);
            break;
        case FSYM_EXACT_SUB:
            fmpq_sub(result.value, lhs.value, rhs.value);
            break;
        case FSYM_EXACT_MUL:
            fmpq_mul(result.value, lhs.value, rhs.value);
            break;
        case FSYM_EXACT_DIV:
            if (fmpq_is_zero(rhs.value)) {
                return 0;
            }
            fmpq_div(result.value, lhs.value, rhs.value);
            break;
        default:
            return 0;
        }
        return hold_exact(result.value);
    } catch (...) {
        g_exact_buffer.clear();
        return 0;
    }
}

size_t fsym_exact_pow_si(const char *base, int64_t exponent)
{
    try {
        FmpqValue value;
        FmpqValue result;
        g_exact_buffer.clear();
        if (!parse_exact(value, base)) {
            return 0;
        }
        if (exponent < -kMaxExactPowExponent
            || exponent > kMaxExactPowExponent) {
            return 0;
        }
        if (exponent < 0 && fmpq_is_zero(value.value)) {
            return 0;
        }
        if (!exact_power_fits(value.value, exponent)) {
            return 0;
        }
        fmpq_pow_si(result.value, value.value, static_cast<slong>(exponent));
        return hold_exact(result.value);
    } catch (...) {
        g_exact_buffer.clear();
        return 0;
    }
}

size_t fsym_exact_fetch(char *buf, size_t n)
{
    try {
        const size_t count
            = n < g_exact_buffer.size() ? n : g_exact_buffer.size();
        if (count > 0 && buf != nullptr) {
            std::memcpy(buf, g_exact_buffer.data(), count);
        }
        return count;
    } catch (...) {
        g_exact_buffer.clear();
        return 0;
    }
}

int fsym_exact_get_d(const char *value, double *result)
{
    try {
        FmpqValue parsed;
        FmpqValue magnitude;
        FmpqValue minimum_normal;
        FmpqValue maximum_finite;
        MpfrValue rounded(53);
        if (result == nullptr || !parse_exact(parsed, value)) {
            return 0;
        }
        if (fmpq_is_zero(parsed.value)) {
            *result = 0.0;
            return 1;
        }
        fmpq_abs(magnitude.value, parsed.value);
        fmpz_one(fmpq_numref(minimum_normal.value));
        fmpz_one(fmpq_denref(minimum_normal.value));
        fmpz_mul_2exp(fmpq_denref(minimum_normal.value),
                      fmpq_denref(minimum_normal.value), 1022);
        fmpz_one(fmpq_numref(maximum_finite.value));
        fmpz_mul_2exp(fmpq_numref(maximum_finite.value),
                      fmpq_numref(maximum_finite.value), 53);
        fmpz_sub_ui(fmpq_numref(maximum_finite.value),
                    fmpq_numref(maximum_finite.value), 1);
        fmpz_mul_2exp(fmpq_numref(maximum_finite.value),
                      fmpq_numref(maximum_finite.value), 971);
        fmpz_one(fmpq_denref(maximum_finite.value));
        if (fmpq_cmp(magnitude.value, minimum_normal.value) < 0
            || fmpq_cmp(magnitude.value, maximum_finite.value) > 0) {
            return 0;
        }
        fmpq_get_mpfr(rounded.value, parsed.value, MPFR_RNDN);
        *result = mpfr_get_d(rounded.value, MPFR_RNDN);
        return std::isfinite(*result) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

int fsym_flint_is_shared(void)
{
    try {
        Dl_info information{};
        void *symbol = dlsym(RTLD_DEFAULT, "fmpq_add");
        if (symbol == nullptr || dladdr(symbol, &information) == 0
            || information.dli_fname == nullptr) {
            return 0;
        }
        const std::string path(information.dli_fname);
        return path.find(".so") != std::string::npos
               || path.find(".dylib") != std::string::npos;
    } catch (...) {
        return 0;
    }
}

int fsym_mpfr_is_shared(void)
{
    try {
        Dl_info information{};
        void *symbol = dlsym(RTLD_DEFAULT, "mpfr_get_d");
        if (symbol == nullptr || dladdr(symbol, &information) == 0
            || information.dli_fname == nullptr) {
            return 0;
        }
        const std::string path(information.dli_fname);
        return path.find(".so") != std::string::npos
               || path.find(".dylib") != std::string::npos;
    } catch (...) {
        return 0;
    }
}

int fsym_mpfr_ulp_error(const char *reference, double observed, double *error)
{
    try {
        if (reference == nullptr || error == nullptr
            || !std::isfinite(observed)) {
            return 0;
        }

        MpfrValue exact(512);
        MpfrValue got(512);
        MpfrValue difference(512);
        MpfrValue magnitude(512);
        MpfrValue minimum_normal(512);
        MpfrValue ulp(512);
        if (mpfr_set_str(exact.value, reference, 10, MPFR_RNDN) != 0
            || !mpfr_number_p(exact.value)) {
            return 0;
        }

        mpfr_set_d(got.value, observed, MPFR_RNDN);
        mpfr_sub(difference.value, got.value, exact.value, MPFR_RNDN);
        mpfr_abs(difference.value, difference.value, MPFR_RNDN);

        /* For a normal binary64 value with exponent e, the local spacing is
         * 2**(e - 53).  Below the normal range it is the fixed subnormal
         * spacing 2**-1074. */
        mpfr_set_ui(minimum_normal.value, 1, MPFR_RNDN);
        mpfr_mul_2si(minimum_normal.value, minimum_normal.value, -1022,
                     MPFR_RNDN);
        mpfr_abs(magnitude.value, exact.value, MPFR_RNDN);
        mpfr_set_ui(ulp.value, 1, MPFR_RNDN);
        if (mpfr_cmp(magnitude.value, minimum_normal.value) < 0) {
            mpfr_mul_2si(ulp.value, ulp.value, -1074, MPFR_RNDN);
        } else {
            const long exponent
                = static_cast<long>(mpfr_get_exp(exact.value)) - 53L;
            mpfr_mul_2si(ulp.value, ulp.value, exponent, MPFR_RNDN);
        }

        mpfr_div(difference.value, difference.value, ulp.value, MPFR_RNDN);
        const double result = mpfr_get_d(difference.value, MPFR_RNDN);
        if (!std::isfinite(result)) {
            return 0;
        }
        *error = result;
        return 1;
    } catch (...) {
        return 0;
    }
}

/* ---------------------------------------------------- algebraic numbers -- */

size_t fsym_algebraic_normalize(const char *value)
{
    try {
        QqbarValue parsed;
        g_algebraic_buffer.clear();
        if (!parse_algebraic(parsed, value)) {
            return 0;
        }
        return hold_algebraic(parsed.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

int fsym_algebraic_get_d(const char *value, double *result)
{
    try {
        QqbarValue input;
        QqbarValue real_part;
        if (result == nullptr || !parse_algebraic(input, value)
            || !qqbar_is_real(input.value)) {
            return 0;
        }
        if (qqbar_is_zero(input.value)) {
            *result = 0.0;
            return 1;
        }

        qqbar_re(real_part.value, input.value);
        for (slong precision = 256; precision <= 2048; precision *= 2) {
            ArbValue enclosure;
            qqbar_get_arb(enclosure.value, real_part.value, precision);
            if (!arb_can_round_mpfr(enclosure.value, 53, MPFR_RNDN)) {
                continue;
            }
            const double projected
                = arf_get_d(arb_midref(enclosure.value), ARF_RND_NEAR);
            if (!std::isfinite(projected)
                || (projected != 0.0
                    && std::fpclassify(projected) == FP_SUBNORMAL)) {
                return 0;
            }
            *result = projected;
            return 1;
        }
        return 0;
    } catch (...) {
        return 0;
    }
}

int fsym_algebraic_symengine_supported(const char *value)
{
    try {
        std::string expression;
        return algebraic_gaussian_rational_text(value, expression) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

CWRAPPER_OUTPUT_TYPE fsym_algebraic_to_symengine(basic out,
                                                  const char *value)
{
    try {
        std::string expression;
        if (!algebraic_gaussian_rational_text(value, expression)) {
            return SYMENGINE_NOT_IMPLEMENTED;
        }
        return basic_parse(out, expression.c_str());
    } catch (...) {
        return SYMENGINE_RUNTIME_ERROR;
    }
}

size_t fsym_algebraic_i(void)
{
    try {
        QqbarValue value;
        g_algebraic_buffer.clear();
        qqbar_i(value.value);
        return hold_algebraic(value.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

size_t fsym_algebraic_from_re_im(const char *real_part,
                                 const char *imag_part)
{
    try {
        size_t real_length = 0;
        size_t imag_length = 0;
        FmpqValue real_value;
        FmpqValue imag_value;
        QqbarValue real_algebraic;
        QqbarValue imag_algebraic;
        QqbarValue result;
        g_algebraic_buffer.clear();
        if (!bounded_text_length(real_part, kMaxAlgebraicBytes, real_length)
            || !bounded_text_length(imag_part, kMaxAlgebraicBytes,
                                    imag_length)
            || !parse_exact(real_value, real_part)
            || !parse_exact(imag_value, imag_part)) {
            return 0;
        }
        qqbar_set_fmpq(real_algebraic.value, real_value.value);
        qqbar_set_fmpq(imag_algebraic.value, imag_value.value);
        qqbar_set_re_im(result.value, real_algebraic.value,
                        imag_algebraic.value);
        return hold_algebraic(result.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

size_t fsym_algebraic_re(const char *value)
{
    return hold_algebraic_component(value, true);
}

size_t fsym_algebraic_im(const char *value)
{
    return hold_algebraic_component(value, false);
}

size_t fsym_algebraic_binary(const char *left, const char *right,
                             int operation)
{
    try {
        QqbarValue lhs;
        QqbarValue rhs;
        QqbarValue result;
        g_algebraic_buffer.clear();
        if (!parse_algebraic(lhs, left) || !parse_algebraic(rhs, right)
            || !qqbar_binop_within_limits(lhs.value, rhs.value,
                                          kMaxAlgebraicDegree,
                                          kMaxAlgebraicHeightBits)) {
            return 0;
        }
        switch (operation) {
        case FSYM_ALGEBRAIC_ADD:
            qqbar_add(result.value, lhs.value, rhs.value);
            break;
        case FSYM_ALGEBRAIC_SUB:
            qqbar_sub(result.value, lhs.value, rhs.value);
            break;
        case FSYM_ALGEBRAIC_MUL:
            qqbar_mul(result.value, lhs.value, rhs.value);
            break;
        case FSYM_ALGEBRAIC_DIV:
            if (qqbar_is_zero(rhs.value)) {
                return 0;
            }
            qqbar_div(result.value, lhs.value, rhs.value);
            break;
        default:
            return 0;
        }
        return hold_algebraic(result.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

size_t fsym_algebraic_unary(const char *value, int operation)
{
    try {
        QqbarValue input;
        QqbarValue result;
        g_algebraic_buffer.clear();
        if (!parse_algebraic(input, value)) {
            return 0;
        }
        switch (operation) {
        case FSYM_ALGEBRAIC_CONJ:
            qqbar_conj(result.value, input.value);
            break;
        case FSYM_ALGEBRAIC_SQRT:
            if (qqbar_degree(input.value) > kMaxAlgebraicDegree/2) {
                return 0;
            }
            qqbar_sqrt(result.value, input.value);
            break;
        default:
            return 0;
        }
        return hold_algebraic(result.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

size_t fsym_algebraic_pow_si(const char *base, int64_t exponent)
{
    try {
        QqbarValue value;
        QqbarValue result;
        g_algebraic_buffer.clear();
        if (!parse_algebraic(value, base)
            || exponent < -kMaxAlgebraicPowExponent
            || exponent > kMaxAlgebraicPowExponent
            || (exponent < 0 && qqbar_is_zero(value.value))) {
            return 0;
        }
        const uint64_t magnitude = exponent < 0
                                       ? static_cast<uint64_t>(-exponent)
                                       : static_cast<uint64_t>(exponent);
        if (magnitude > 0
            && static_cast<uint64_t>(qqbar_height_bits(value.value))
                   > static_cast<uint64_t>(kMaxAlgebraicHeightBits)/magnitude) {
            return 0;
        }
        qqbar_pow_si(result.value, value.value, static_cast<slong>(exponent));
        return hold_algebraic(result.value);
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

int fsym_algebraic_signs(const char *value, int *real_sign, int *imag_sign)
{
    try {
        QqbarValue parsed;
        if (real_sign == nullptr || imag_sign == nullptr
            || !parse_algebraic(parsed, value)) {
            return 0;
        }
        *real_sign = qqbar_sgn_re(parsed.value);
        *imag_sign = qqbar_sgn_im(parsed.value);
        return 1;
    } catch (...) {
        return 0;
    }
}

size_t fsym_algebraic_fetch(char *buf, size_t n)
{
    try {
        const size_t count = n < g_algebraic_buffer.size()
                                 ? n
                                 : g_algebraic_buffer.size();
        if (count > 0 && buf != nullptr) {
            std::memcpy(buf, g_algebraic_buffer.data(), count);
        }
        return count;
    } catch (...) {
        g_algebraic_buffer.clear();
        return 0;
    }
}

/* ------------------------------------------------------------- transforms -- */

CWRAPPER_OUTPUT_TYPE fsym_series(basic out, const basic ex, const basic var,
                                 unsigned int prec)
{
    try {
        if (!SymEngine::is_a<SymEngine::Symbol>(*deref(var))) {
            return SYMENGINE_DOMAIN_ERROR;
        }
        const auto sym
            = SymEngine::rcp_static_cast<const SymEngine::Symbol>(deref(var));
        deref(out) = SymEngine::series(deref(ex), sym, prec)->as_basic();
    } catch (const SymEngine::SymEngineException &) {
        return SYMENGINE_RUNTIME_ERROR;
    } catch (...) {
        return SYMENGINE_RUNTIME_ERROR;
    }
    return SYMENGINE_NO_EXCEPTION;
}

unsigned int fsym_count_ops(const basic ex)
{
    try {
        return SymEngine::count_ops({deref(ex)});
    } catch (...) {
        return 0u;
    }
}

CWRAPPER_OUTPUT_TYPE fsym_normal_form(basic out, const basic ex)
{
    try {
        deref(out) = normal_form(deref(ex));
    } catch (...) {
        return SYMENGINE_RUNTIME_ERROR;
    }
    return SYMENGINE_NO_EXCEPTION;
}

int fsym_zero_test(const basic ex, unsigned long max_nodes)
{
    using namespace SymEngine;

    const unsigned long limit = max_nodes == 0UL ? kDefaultMaxNodes : max_nodes;

    try {
        RCP<const Basic> e = deref(ex);

        // Cheapest test first: many residuals are already structurally zero.
        if (eq(*e, *zero)) {
            return FSYM_ZERO_TRUE;
        }

        // Exponential normal form. rewrite_as_exp has no rule for gamma, erf,
        // the Bessel family and friends; when it gives up, so does the
        // symbolic path, and the caller falls back to a numeric probe.
        RCP<const Basic> r = normal_form(e);

        // Fold onto a single rational function and expand the numerator. One
        // pass suffices for most inputs; expanding can expose fractions that
        // were nested inside a product (the tangent addition formula needs
        // this), so re-fold a bounded number of times.
        RCP<const Basic> numer, denom;
        for (int pass = 0; pass < 4; ++pass) {
            as_numer_denom(r, outArg(numer), outArg(denom));

            if (node_count(numer, limit) > limit) {
                return FSYM_ZERO_UNKNOWN;
            }

            numer = exp_normalize(expand(numer));
            if (eq(*numer, *zero)) {
                return FSYM_ZERO_TRUE;
            }
            if (eq(*numer, *r)) {
                break; // fixed point; further passes cannot help
            }
            r = numer;
        }

        // A non-zero numerator only means "not an identity" when the whole
        // expression lived in the fragment the normal form actually decides.
        // Outside it, normalization was a no-op and the verdict is unknown.
        if (!in_decidable_fragment(numer)) {
            return FSYM_ZERO_UNKNOWN;
        }

        // Inside the fragment the numerator is a fully expanded element of a
        // field where distinct canonical forms mean distinct functions, so a
        // non-zero numerator is a genuine non-identity rather than a failure to
        // simplify. Expressions that vanish only on part of the domain land
        // here, which is correct: they are not identities.
        return FSYM_ZERO_FALSE;
    } catch (...) {
        return FSYM_ZERO_UNKNOWN;
    }
}

CWRAPPER_OUTPUT_TYPE fsym_simplify(basic out, const basic ex)
{
    using namespace SymEngine;

    try {
        const RCP<const Basic> input = deref(ex);

        // Candidate forms. No single transform wins across inputs: expand is
        // right for polynomials and wrong for factored products, the
        // exponential normal form settles trigonometric identities but reads
        // badly, and SymEngine::simplify handles some power rewrites the others
        // miss. Generate them all and let the operation count decide, which is
        // what SymPy's top-level simplify does.
        std::vector<RCP<const Basic>> candidates;
        candidates.push_back(input);

        auto offer = [&candidates](const RCP<const Basic> &c) {
            if (!c.is_null()) {
                candidates.push_back(c);
            }
        };

        // Each transform is attempted independently: one throwing must not
        // deprive the ranking of the others.
        try {
            offer(SymEngine::simplify(input));
        } catch (...) {
        }
        try {
            offer(expand(input));
        } catch (...) {
        }
        try {
            RCP<const Basic> n, d;
            as_numer_denom(input, outArg(n), outArg(d));
            offer(div(expand(n), expand(d)));
        } catch (...) {
        }
        offer(try_cancel(input));
        offer(try_cancel(expand(input)));
        try {
            // Round trip through exponential normal form. This is what turns
            // sin(x)**2 + cos(x)**2 into 1.
            RCP<const Basic> n, d;
            as_numer_denom(normal_form(input), outArg(n), outArg(d));
            const auto folded = div(exp_normalize(expand(n)), expand(d));
            offer(folded);
            // ... and back to trigonometric form, which usually reads better
            // than a sum of complex exponentials.
            try {
                offer(SymEngine::simplify(rewrite_as_sin(folded)));
            } catch (...) {
            }
        } catch (...) {
        }

        // Lowest operation count wins; ties keep the earlier candidate, so the
        // input survives unless something is strictly simpler.
        RCP<const Basic> best = input;
        unsigned int best_ops = count_ops({input});
        for (const auto &c : candidates) {
            const unsigned int ops = count_ops({c});
            if (ops < best_ops) {
                best_ops = ops;
                best = c;
            }
        }

        deref(out) = best;
    } catch (...) {
        return SYMENGINE_RUNTIME_ERROR;
    }
    return SYMENGINE_NO_EXCEPTION;
}

} /* extern "C" */
