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

static void expect_simplified(fortsym_arena *arena,
                              const fortsym_expr *expression,
                              const char *expected)
{
    char message[128];
    fortsym_expr *simplified = NULL;
    int status = fortsym_simplify(arena, expression, &simplified,
                                  message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(simplified, expected);
    fortsym_expr_free(simplified);
}

int main(void)
{
    char message[128];
    fortsym_arena *arena = NULL;
    fortsym_expr *psi = NULL;
    fortsym_expr *theta = NULL;
    fortsym_expr *phi = NULL;
    fortsym_expr *zero = NULL;
    fortsym_expr *one = NULL;
    const fortsym_expr *coordinates[3];
    const fortsym_expr *position[3];
    const fortsym_expr *vector[3];
    fortsym_expr *output[8] = {0};
    int status;

    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK && arena != NULL);
    status = fortsym_symbol(arena, "psi", &psi, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "theta", &theta, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "phi", &phi, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_int(arena, 0, &zero, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_int(arena, 1, &one, message, sizeof message);
    assert(status == FORTSYM_OK);

    coordinates[0] = psi;
    coordinates[1] = theta;
    coordinates[2] = phi;
    position[0] = psi;
    position[1] = theta;
    position[2] = phi;
    vector[0] = zero;
    vector[1] = psi;
    vector[2] = one;
    status = fortsym_chart_hamada_residuals(
        arena, coordinates, position, vector, 1, output, message,
        sizeof message);
    assert(status == FORTSYM_OK);
    for (size_t index = 0; index < 5; ++index) {
        expect_text(output[index], "0");
        fortsym_expr_free(output[index]);
        output[index] = NULL;
    }

    vector[1] = theta;
    status = fortsym_chart_hamada_residuals(
        arena, coordinates, position, vector, 1, output, message,
        sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(output[1], "1");
    for (size_t index = 0; index < 5; ++index)
        fortsym_expr_free(output[index]);

    status = fortsym_chart_hamada_residuals(
        arena, coordinates, position, vector, 0, output, message,
        sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);

    vector[0] = zero;
    vector[1] = psi;
    vector[2] = one;
    status = fortsym_chart_b_flux_form(
        arena, coordinates, position, vector, 1, output, message,
        sizeof message);
    assert(status == FORTSYM_OK);
    expect_simplified(arena, output[3], "1");
    expect_simplified(arena, output[5], "-psi");
    expect_simplified(arena, output[6], "0");
    for (size_t index = 0; index < 8; ++index)
        fortsym_expr_free(output[index]);

    status = fortsym_chart_b_flux_form(
        arena, coordinates, position, vector, 0, output, message,
        sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);

    fortsym_arena_free(arena);
    return 0;
}
