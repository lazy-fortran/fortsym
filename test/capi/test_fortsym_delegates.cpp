#include "fsym_shim.h"

#include <atomic>
#include <cstdlib>
#include <iostream>
#include <string>
#include <thread>
#include <vector>

namespace {
int failures = 0;
void check(bool condition, const char *message)
{
    if (!condition) {
        std::cerr << "FAIL " << message << '\n';
        ++failures;
    }
}
bool flag(const char *name)
{
    const char *value = std::getenv(name);
    return value != nullptr && std::string(value) == "1";
}
void counts(int group, uint64_t attempted, uint64_t succeeded, bool counted)
{
    uint64_t attempts = 99, successes = 99;
    check(fsym_delegate_counts(group, &attempts, &successes) == 1, "counter query");
    check(attempts == (counted ? attempted : 0), "attempt count");
    check(successes == (counted ? succeeded : 0), "accepted-result count");
}
}

int main()
{
    const bool exact = !flag("FORTSYM_DISABLE_FLINT_EXACT");
    const bool algebraic = !flag("FORTSYM_DISABLE_FLINT_ALGEBRAIC");
    const char *trace = std::getenv("FORTSYM_TRACE_DELEGATES");
    const bool counted = flag("FORTSYM_COUNT_DELEGATES") || !exact || !algebraic
        || flag("FORTSYM_DISABLE_SYMENGINE_EVAL") || (trace != nullptr && trace[0] != '\0');
    char buffer[128] = {};
    auto exact_result = [&](size_t size, const char *expected) {
        check((size > 0) == exact, "exact gate result");
        const size_t fetched = fsym_exact_fetch(buffer, sizeof(buffer));
        check(fetched == size, "exact pending slot cleared on refusal");
        if (exact) check(std::string(buffer, fetched) == expected, "exact arithmetic value");
    };
    exact_result(fsym_exact_normalize("6/8"), "3/4");
    exact_result(fsym_exact_binary("100000000000000000000", "1", FSYM_EXACT_ADD),
                 "100000000000000000001");
    exact_result(fsym_exact_pow_si("2", 80), "1208925819614629174706176");
    double projected = 37.0;
    check(fsym_exact_get_d("1/2", &projected) == (exact ? 1 : 0), "exact projection gate");
    check(projected == (exact ? 0.5 : 37.0), "exact refusal preserves output");
    check(fsym_exact_binary("1", "0", FSYM_EXACT_DIV) == 0, "failed exact attempt");
    check(fsym_exact_fetch(buffer, sizeof(buffer)) == 0, "failed exact clears pending slot");
    counts(FSYM_DELEGATE_FLINT_EXACT, 5, exact ? 4 : 0, counted);

    constexpr const char *imaginary = "qqbar1:0:1,0,1";
    constexpr const char *one = "qqbar1:0:-1,1";
    auto algebraic_result = [&](size_t size, const char *expected) {
        check((size > 0) == algebraic, "algebraic gate result");
        const size_t fetched = fsym_algebraic_fetch(buffer, sizeof(buffer));
        check(fetched == size, "algebraic pending slot cleared on refusal");
        if (algebraic) check(std::string(buffer, fetched) == expected, "algebraic arithmetic value");
    };
    algebraic_result(fsym_algebraic_normalize(imaginary), imaginary);
    algebraic_result(fsym_algebraic_i(), imaginary);
    algebraic_result(fsym_algebraic_from_re_im("1", "0"), one);
    algebraic_result(fsym_algebraic_re(imaginary), "qqbar1:0:0,1");
    algebraic_result(fsym_algebraic_im(imaginary), one);
    algebraic_result(fsym_algebraic_binary(imaginary, imaginary, FSYM_ALGEBRAIC_MUL), "qqbar1:0:1,1");
    algebraic_result(fsym_algebraic_unary(imaginary, FSYM_ALGEBRAIC_CONJ), "qqbar1:1:1,0,1");
    algebraic_result(fsym_algebraic_pow_si(imaginary, 2), "qqbar1:0:1,1");
    int real_sign = 37, imag_sign = 37;
    check(fsym_algebraic_signs(imaginary, &real_sign, &imag_sign) == (algebraic ? 1 : 0), "sign gate");
    check(real_sign == (algebraic ? 0 : 37) && imag_sign == (algebraic ? 1 : 37), "sign refusal preserves outputs");
    projected = 37.0;
    check(fsym_algebraic_get_d(one, &projected) == (algebraic ? 1 : 0), "algebraic projection gate");
    check(projected == (algebraic ? 1.0 : 37.0), "algebraic projection preserves refused output");
    check(fsym_algebraic_symengine_supported(imaginary) == (algebraic ? 1 : 0), "algebraic support gate");
    basic_struct *out = basic_new_heap();
    integer_set_si(out, 37);
    const auto rc = fsym_algebraic_to_symengine(out, imaginary);
    check(rc == (algebraic ? SYMENGINE_NO_EXCEPTION : SYMENGINE_NOT_IMPLEMENTED), "algebraic conversion gate");
    if (!algebraic) check(integer_get_si(out) == 37, "conversion refusal preserves output");
    basic_free_heap(out);
    check(fsym_algebraic_normalize("invalid") == 0, "failed algebraic attempt");
    check(fsym_algebraic_fetch(buffer, sizeof(buffer)) == 0, "failed algebraic clears pending slot");
    counts(FSYM_DELEGATE_FLINT_ALGEBRAIC, 13, algebraic ? 12 : 0, counted);

    // Concurrent calls exercise shared counters and independent pending slots.
    std::vector<std::thread> workers;
    std::atomic<int> bad{0};
    for (int worker = 0; worker < 4; ++worker) {
        workers.emplace_back([&] {
            for (int iteration = 0; iteration < 50; ++iteration) {
                const size_t size = fsym_exact_binary("1", "2", FSYM_EXACT_ADD);
                char local[8] = {};
                const size_t got = fsym_exact_fetch(local, sizeof(local));
                if ((exact && (size != 1 || got != 1 || local[0] != '3'))
                    || (!exact && (size != 0 || got != 0))) ++bad;
            }
        });
    }
    for (auto &worker : workers) worker.join();
    check(bad.load() == 0, "thread-local results");
    counts(FSYM_DELEGATE_FLINT_EXACT, 205, exact ? 204 : 0, counted);
    counts(FSYM_DELEGATE_SYMENGINE_EVAL, 0, 0, counted);
    uint64_t attempts = 37, successes = 37;
    check(fsym_delegate_counts(99, &attempts, &successes) == 0
          && attempts == 37 && successes == 37, "unknown delegate is refused");
    return failures == 0 ? 0 : 1;
}
