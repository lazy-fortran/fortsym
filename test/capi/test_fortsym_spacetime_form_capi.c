#include "fortsym.h"

#include <assert.h>

int main(void)
{
    char message[128];
    int signature[4] = {-1, 1, 1, 1};
    int verdict = FORTSYM_ZERO_UNKNOWN;
    int status;
    size_t mask;
    fortsym_arena *arena = NULL;
    fortsym_expr *t = NULL;
    fortsym_expr *x = NULL;
    fortsym_expr *y = NULL;
    fortsym_expr *z = NULL;
    fortsym_expr *parameter = NULL;
    fortsym_expr *zero = NULL;
    fortsym_expr *one = NULL;
    fortsym_expr *minus_one = NULL;
    fortsym_expr *two = NULL;
    fortsym_expr *check = NULL;
    fortsym_expr *interior_check = NULL;
    fortsym_expr *lie_check = NULL;
    fortsym_expr *output[16] = {0};
    const fortsym_expr *metric[16];
    const fortsym_expr *coordinates[4];
    const fortsym_expr *input[16];
    const fortsym_expr *curve[4];
    const fortsym_expr *vector[4];
    fortsym_expr *geodesic_output[4] = {0};
    fortsym_expr *interior_output[16] = {0};
    fortsym_expr *lie_output[16] = {0};
    fortsym_expr *laplace_output[16] = {0};

    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_symbol(arena, "t", &t, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "x", &x, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "y", &y, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "z", &z, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "lambda", &parameter, message,
                          sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 0, &zero, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, -1, &minus_one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) == FORTSYM_OK);

    for (mask = 0; mask < 16; ++mask) {
        metric[mask] = zero;
        input[mask] = zero;
    }
    metric[0] = minus_one;
    metric[5] = one;
    metric[10] = one;
    metric[15] = one;
    coordinates[0] = t;
    coordinates[1] = x;
    coordinates[2] = y;
    coordinates[3] = z;
    input[1] = t;
    input[2] = x;
    input[4] = y;
    input[8] = z;
    curve[0] = parameter;
    curve[1] = one;
    curve[2] = parameter;
    curve[3] = zero;
    vector[0] = one;
    vector[1] = zero;
    vector[2] = zero;
    vector[3] = zero;

    status = fortsym_spacetime_form_codifferential(
        arena, metric, 4, coordinates, signature, 1, input, 1, output,
        message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_add(arena, output[0], two, &check, message,
                       sizeof message) == FORTSYM_OK);
    assert(fortsym_zero_test(arena, check, &verdict, message,
                             sizeof message) == FORTSYM_OK);
    assert(verdict == FORTSYM_ZERO_TRUE);

    status = fortsym_spacetime_geodesic_residual(
        arena, metric, 4, coordinates, signature, 1, curve, parameter,
        geodesic_output, message, sizeof message);
    assert(status == FORTSYM_OK);
    for (mask = 0; mask < 4; ++mask) {
        assert(fortsym_zero_test(arena, geodesic_output[mask], &verdict,
                                 message, sizeof message) == FORTSYM_OK);
        assert(verdict == FORTSYM_ZERO_TRUE);
    }

    status = fortsym_spacetime_form_interior(
        arena, metric, 4, coordinates, signature, 1, vector, input, 1,
        interior_output, message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_subtract(arena, interior_output[0], t, &interior_check,
                            message, sizeof message) == FORTSYM_OK);
    assert(fortsym_zero_test(arena, interior_check, &verdict, message,
                             sizeof message) == FORTSYM_OK);
    assert(verdict == FORTSYM_ZERO_TRUE);

    status = fortsym_spacetime_form_lie(
        arena, metric, 4, coordinates, signature, 1, vector, input, 1,
        lie_output, message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_subtract(arena, lie_output[1], one, &lie_check,
                            message, sizeof message) == FORTSYM_OK);
    assert(fortsym_zero_test(arena, lie_check, &verdict, message,
                             sizeof message) == FORTSYM_OK);
    assert(verdict == FORTSYM_ZERO_TRUE);

    for (mask = 0; mask < 16; ++mask)
        input[mask] = zero;
    input[0] = t;
    status = fortsym_spacetime_form_laplace_de_rham(
        arena, metric, 4, coordinates, signature, 1, input, 0,
        laplace_output, message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_zero_test(arena, laplace_output[0], &verdict, message,
                             sizeof message) == FORTSYM_OK);
    assert(verdict == FORTSYM_ZERO_TRUE);

    fortsym_expr_free(check);
    fortsym_expr_free(interior_check);
    fortsym_expr_free(lie_check);
    for (mask = 0; mask < 16; ++mask)
        fortsym_expr_free(output[mask]);
    for (mask = 0; mask < 4; ++mask)
        fortsym_expr_free(geodesic_output[mask]);
    for (mask = 0; mask < 16; ++mask) {
        fortsym_expr_free(interior_output[mask]);
        fortsym_expr_free(lie_output[mask]);
        fortsym_expr_free(laplace_output[mask]);
    }
    fortsym_expr_free(two);
    fortsym_expr_free(minus_one);
    fortsym_expr_free(one);
    fortsym_expr_free(zero);
    fortsym_expr_free(z);
    fortsym_expr_free(parameter);
    fortsym_expr_free(y);
    fortsym_expr_free(x);
    fortsym_expr_free(t);
    fortsym_arena_free(arena);
    return 0;
}
