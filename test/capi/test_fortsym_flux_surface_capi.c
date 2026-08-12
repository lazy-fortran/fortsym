#include "fortsym.h"

#include <assert.h>
#include <stdio.h>
#include <string.h>

static void expect_text(const fortsym_expr *expression, const char *expected)
{
    char buffer[128];
    char message[128];
    size_t required = 0;
    int status = fortsym_expr_text(expression, buffer, sizeof buffer,
                                   &required, message, sizeof message);
    assert(status == FORTSYM_OK);
    if (strcmp(buffer, expected) != 0)
        fprintf(stderr, "got [%s], expected [%s]\n", buffer, expected);
    assert(strcmp(buffer, expected) == 0);
}

int main(void)
{
    char message[128];
    fortsym_arena *arena = NULL;
    fortsym_expr *u1 = NULL;
    fortsym_expr *u2 = NULL;
    fortsym_expr *u3 = NULL;
    fortsym_expr *one = NULL;
    fortsym_expr *sine = NULL;
    fortsym_expr *scalar = NULL;
    fortsym_expr *average = NULL;
    fortsym_expr *simplified = NULL;
    const fortsym_expr *coordinates[3];
    const fortsym_expr *position[3];
    const fortsym_expr *sine_argument[1];
    int status;

    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK && arena != NULL);
    status = fortsym_symbol(arena, "psi", &u1, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "theta", &u2, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "phi", &u3, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_int(arena, 1, &one, message, sizeof message);
    assert(status == FORTSYM_OK);
    sine_argument[0] = u2;
    status = fortsym_function(arena, "sin", sine_argument, 1, &sine,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_add(arena, one, sine, &scalar, message, sizeof message);
    assert(status == FORTSYM_OK);
    coordinates[0] = u1;
    coordinates[1] = u2;
    coordinates[2] = u3;
    position[0] = u1;
    position[1] = u2;
    position[2] = u3;

    status = fortsym_chart_flux_surface_average(
        arena, coordinates, position, 1, scalar, &average, message,
        sizeof message);
    assert(status == FORTSYM_OK && average != NULL);
    status = fortsym_simplify(arena, average, &simplified, message,
                               sizeof message);
    assert(status == FORTSYM_OK && simplified != NULL);
    expect_text(simplified, "1");

    status = fortsym_chart_flux_surface_average(
        arena, coordinates, position, 0, scalar, &average, message,
        sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT && average == NULL);

    fortsym_arena_free(arena);
    return 0;
}
