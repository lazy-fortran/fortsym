#include "fsym_shim.h"

#include <stdio.h>
#include <string.h>

static int expect_render(const basic expression, int mode, const char *expected)
{
    char buffer[64];
    size_t length = fsym_str_render(expression, mode);

    if (length != strlen(expected) || length >= sizeof buffer) {
        fprintf(stderr, "render length %zu, expected [%s]\n", length, expected);
        return 0;
    }
    if (fsym_str_fetch(buffer, length) != length) {
        fputs("rendered expression could not be fetched\n", stderr);
        return 0;
    }
    buffer[length] = '\0';
    if (strcmp(buffer, expected) != 0) {
        fprintf(stderr, "rendered [%s], expected [%s]\n", buffer, expected);
        return 0;
    }
    return 1;
}

int main(void)
{
    basic x, sine, third;
    int passed;

    basic_new_stack(x);
    basic_new_stack(sine);
    basic_new_stack(third);
    passed = symbol_set(x, "x") == 0;
    passed = (basic_sin(sine, x) == 0) && passed;
    passed = (rational_set_si(third, 1, 3) == 0) && passed;
    if (passed) {
        /* C emission uses double math and floating-point division. */
        passed = expect_render(sine, FSYM_STR_CCODE, "sin(x)") && passed;
        passed = expect_render(third, FSYM_STR_DEFAULT, "1/3") && passed;
        passed = expect_render(third, FSYM_STR_CCODE, "1.0/3.0") && passed;
    }
    basic_free_stack(third);
    basic_free_stack(sine);
    basic_free_stack(x);
    return passed ? 0 : 1;
}
