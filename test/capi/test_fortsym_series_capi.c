#include "fortsym.h"

#include <assert.h>
#include <stdint.h>

int main(void)
{
    char message[256];
    fortsym_arena *arena = NULL;
    fortsym_expr *x = NULL, *zero = NULL, *one = NULL, *two = NULL;
    fortsym_expr *x2 = NULL, *x2_over_two = NULL, *expected = NULL;
    fortsym_expr *expected_sum = NULL;
    fortsym_expr *exponential = NULL, *series = NULL, *coefficient = NULL;
    fortsym_expr *difference = NULL, *simplified_difference = NULL;
    fortsym_expr *reciprocal = NULL;
    const fortsym_expr *args[1];
    int64_t coefficient_value = 0;
    int status;

    assert(fortsym_abi_version() == 101);
    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_symbol(arena, "x", &x, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 0, &zero, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) ==
           FORTSYM_OK);

    args[0] = x;
    assert(fortsym_function(arena, "exp", args, 1, &exponential, message,
                            sizeof message) == FORTSYM_OK);
    assert(fortsym_series(arena, exponential, x, zero, 2, &series, message,
                          sizeof message) == FORTSYM_OK);
    assert(fortsym_power(arena, x, two, &x2, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_divide(arena, x2, two, &x2_over_two, message,
                          sizeof message) == FORTSYM_OK);
    assert(fortsym_add(arena, one, x, &expected, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_add(arena, expected, x2_over_two, &expected_sum, message,
                       sizeof message) == FORTSYM_OK);
    {
        int verdict = FORTSYM_ZERO_UNKNOWN;
        assert(fortsym_subtract(arena, series, expected_sum, &difference,
                                message, sizeof message) == FORTSYM_OK);
        assert(fortsym_simplify(arena, difference, &simplified_difference,
                                message, sizeof message) == FORTSYM_OK);
        assert(fortsym_zero_test(arena, simplified_difference, &verdict,
                                 message, sizeof message) == FORTSYM_OK);
        assert(verdict == FORTSYM_ZERO_TRUE);
    }

    assert(fortsym_series_coeff(arena, x2, x, zero, 2, &coefficient, message,
                                sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_int_value(coefficient, &coefficient_value, message,
                                  sizeof message) == FORTSYM_OK);
    assert(coefficient_value == 1);

    assert(fortsym_divide(arena, one, x, &reciprocal, message,
                          sizeof message) == FORTSYM_OK);
    assert(fortsym_series(arena, reciprocal, x, zero, 2, &series, message,
                          sizeof message) == FORTSYM_UNSUPPORTED);
    assert(series == NULL);

    fortsym_expr_free(reciprocal);
    fortsym_expr_free(coefficient);
    fortsym_expr_free(series);
    fortsym_expr_free(simplified_difference);
    fortsym_expr_free(difference);
    fortsym_expr_free(expected_sum);
    fortsym_expr_free(expected);
    fortsym_expr_free(x2_over_two);
    fortsym_expr_free(x2);
    fortsym_expr_free(exponential);
    fortsym_expr_free(two);
    fortsym_expr_free(one);
    fortsym_expr_free(zero);
    fortsym_expr_free(x);
    fortsym_arena_free(arena);
    return 0;
}
