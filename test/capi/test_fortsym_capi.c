#include "fortsym.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static void expect_text(const fortsym_expr *expression, const char *expected)
{
    char buffer[256];
    char message[128];
    size_t required = 0;
    int status = fortsym_expr_text(expression, buffer, sizeof buffer,
                                   &required, message, sizeof message);
    assert(status == FORTSYM_OK);
    if (required != strlen(expected) + 1 || strcmp(buffer, expected) != 0)
        fprintf(stderr, "got [%s] required=%zu expected [%s]\\n", buffer,
                required, expected);
    assert(required == strlen(expected) + 1);
    assert(strcmp(buffer, expected) == 0);
}

int main(void)
{
    char message[128];
    char buffer[256];
    size_t required = 0;
    int64_t integer_value = 0;
    int kind = 0;
    int equal = 0;
    int verdict = FORTSYM_ZERO_UNKNOWN;
    int known = 0;
    int status;
    fortsym_arena *arena = NULL;
    fortsym_arena *other_arena = NULL;
    fortsym_expr *x = NULL;
    fortsym_expr *y = NULL;
    fortsym_expr *one = NULL;
    fortsym_expr *sum = NULL;
    fortsym_expr *product = NULL;
    fortsym_expr *derivative = NULL;
    fortsym_expr *replacement = NULL;
    fortsym_expr *substituted = NULL;
    fortsym_expr *foreign = NULL;
    fortsym_expr *two = NULL;
    fortsym_expr *square = NULL;
    fortsym_expr *powered = NULL;
    fortsym_expr *expanded_input = NULL;
    fortsym_expr *factored = NULL;
    fortsym_expr *quotient_num = NULL;
    fortsym_expr *quotient_den = NULL;
    fortsym_expr *quotient = NULL;
    fortsym_expr *root = NULL;
    fortsym_expr *assumed = NULL;
    fortsym_expr *zero_expression = NULL;
    fortsym_expr *seven = NULL;
    fortsym_expr *sine = NULL;
    const fortsym_expr *root_argument[1];

    assert(fortsym_abi_version() == 4);
    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK && arena != NULL);
    status = fortsym_symbol(arena, "x", &x, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "y", &y, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_int(arena, 1, &one, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_add(arena, x, one, &sum, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_multiply(arena, sum, y, &product, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(product, "y*(x + 1)");

    status = fortsym_expr_kind(one, &kind, message, sizeof message);
    assert(status == FORTSYM_OK && kind == FORTSYM_INT);
    status = fortsym_expr_int_value(one, &integer_value, message, sizeof message);
    assert(status == FORTSYM_OK && integer_value == 1);
    status = fortsym_expr_exact_text(one, buffer, sizeof buffer, &required,
                                     message, sizeof message);
    assert(status == FORTSYM_OK && strcmp(buffer, "1") == 0);

    status = fortsym_int(arena, 2, &two, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_power(arena, x, two, &square, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_power(arena, sum, two, &powered, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_expand(arena, powered, &expanded_input, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_factor(arena, expanded_input, &factored, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(factored, "(x + 1)**2");
    status = fortsym_subtract(arena, square, one, &quotient_num, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_subtract(arena, x, one, &quotient_den, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_divide(arena, quotient_num, quotient_den, &quotient,
                            message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_factor(arena, quotient, &foreign, message,
                            sizeof message);
    assert(status == FORTSYM_UNSUPPORTED && foreign == NULL);
    root_argument[0] = square;
    status = fortsym_function(arena, "sqrt", root_argument, 1, &root,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assume(arena, x, FORTSYM_FACT_POSITIVE, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_REAL, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 0);
    status = fortsym_assumption_has(arena, x, 16, &known, message,
                                    sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    status = fortsym_assumption_push(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assume(arena, y, FORTSYM_FACT_NONNEGATIVE, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_NONNEGATIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_push(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_NONNEGATIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_pop(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_pop(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_NONNEGATIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 0);
    status = fortsym_assumption_pop(arena, message, sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_simplify(arena, root, &assumed, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(assumed, "x");

    status = fortsym_subtract(arena, x, x, &zero_expression, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, zero_expression, &verdict, message,
                               sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_TRUE);
    status = fortsym_int(arena, 7, &seven, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, seven, &verdict, message, sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_FALSE);
    root_argument[0] = x;
    status = fortsym_function(arena, "sin", root_argument, 1, &sine,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, sine, &verdict, message, sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_UNKNOWN);

    status = fortsym_differentiate(arena, x, x, &derivative, message,
                                   sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(derivative, "1");
    status = fortsym_int(arena, 2, &replacement, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_substitute(arena, product, x, replacement, &substituted, message,
                                sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(substituted, "y*(1 + 2)");

    status = fortsym_arena_new(&other_arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_add(other_arena, x, one, &foreign, message, sizeof message);
    assert(status == FORTSYM_FOREIGN_ARENA);
    assert(foreign == NULL);
    fortsym_arena_free(other_arena);

    status = fortsym_expr_equal(x, x, &equal, message, sizeof message);
    assert(status == FORTSYM_OK && equal == 1);
    status = fortsym_expr_argument(product, 99, &foreign, message,
                                   sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    assert(strstr(message, "invalid") != NULL);

    /* Expression ownership keeps the arena valid after its root is released. */
    fortsym_arena_free(arena);
    expect_text(product, "y*(x + 1)");

    fortsym_expr_free(foreign);
    fortsym_expr_free(assumed);
    fortsym_expr_free(sine);
    fortsym_expr_free(seven);
    fortsym_expr_free(zero_expression);
    fortsym_expr_free(root);
    fortsym_expr_free(square);
    fortsym_expr_free(factored);
    fortsym_expr_free(expanded_input);
    fortsym_expr_free(powered);
    fortsym_expr_free(quotient);
    fortsym_expr_free(quotient_den);
    fortsym_expr_free(quotient_num);
    fortsym_expr_free(two);
    fortsym_expr_free(substituted);
    fortsym_expr_free(sum);
    fortsym_expr_free(replacement);
    fortsym_expr_free(derivative);
    fortsym_expr_free(product);
    fortsym_expr_free(one);
    fortsym_expr_free(y);
    fortsym_expr_free(x);
    puts("test_fortsym_capi: all checks passed");
    return 0;
}
