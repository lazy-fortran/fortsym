#include "fsym_shim.h"

#include <stdio.h>

static int expect_zero_test(const char *text, unsigned long limit, int expected)
{
    basic expression;
    int actual;
    basic_new_stack(expression);
    if (basic_parse(expression, text) != 0) {
        fprintf(stderr, "parse failed: %s\n", text);
        basic_free_stack(expression);
        return 0;
    }
    actual = fsym_zero_test(expression, limit);
    basic_free_stack(expression);
    if (actual != expected) {
        fprintf(stderr, "verdict %d, expected %d: %s\n", actual, expected, text);
        return 0;
    }
    return 1;
}

int main(void)
{
    int passed = 1;
    const char *identity =
        "s**(-alpha)*(1 + 1/s) - s**(-alpha) - s**(-1-alpha)";
    /* Exact distributivity, independently of any assumption about alpha. */
    passed = expect_zero_test(identity, 0, FSYM_ZERO_TRUE) && passed;
    passed = expect_zero_test(
        "s**alpha*(1 + 1/s) - s**alpha - s**(-1+alpha)",
        0, FSYM_ZERO_TRUE) && passed;
    passed = expect_zero_test(
        "s**(-alpha)*(1 + 1/s) - s**(-alpha) - s**(-1-alpha) + 1",
        0, FSYM_ZERO_FALSE) && passed;
    /* Normalizing exponent arithmetic must preserve branch-sensitive bases. */
    passed = expect_zero_test("sqrt(s**2) - s", 0, FSYM_ZERO_FALSE) && passed;
    passed = expect_zero_test("gamma(s+1) - s*gamma(s)",
                              0, FSYM_ZERO_UNKNOWN) && passed;
    /* Both the existing numerator limit and the new exponent-growth guard
       refuse a proof when the caller's explicit resource budget is exceeded. */
    passed = expect_zero_test(identity, 3, FSYM_ZERO_UNKNOWN) && passed;
    passed = expect_zero_test("s**((a+b)**1000000) - 1", 100,
                              FSYM_ZERO_UNKNOWN) && passed;
    return passed ? 0 : 1;
}
