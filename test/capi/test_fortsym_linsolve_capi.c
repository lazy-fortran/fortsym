#include "fortsym.h"

#include <assert.h>
#include <stddef.h>
#include <string.h>

static int has_text(fortsym_expr *values[], size_t count, const char *text)
{
    char buffer[128];
    char message[128];
    size_t required = 0;
    for (size_t index = 0; index < count; ++index) {
        assert(fortsym_expr_text(values[index], buffer, sizeof buffer,
                                 &required, message, sizeof message) ==
               FORTSYM_OK);
        if (strcmp(buffer, text) == 0)
            return 1;
    }
    return 0;
}

int main(void)
{
    char message[256];
    fortsym_arena *arena = NULL;
    fortsym_expr *one = NULL, *two = NULL, *three = NULL, *four = NULL;
    fortsym_expr *five = NULL, *six = NULL;
    fortsym_expr *x = NULL, *y = NULL;
    const fortsym_expr *matrix[4];
    const fortsym_expr *right_hand_side[2];
    const fortsym_expr *free_matrix[2];
    const fortsym_expr *free_right_hand_side[1];
    const fortsym_expr *free_variables[2];
    const fortsym_expr *inconsistent_matrix[2];
    const fortsym_expr *inconsistent_right_hand_side[2];
    const fortsym_expr *inconsistent_variables[1];
    fortsym_expr *values[2] = {0};
    size_t count = 0;

    assert(fortsym_abi_version() == 99);
    assert(fortsym_arena_new(&arena, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 3, &three, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 4, &four, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 5, &five, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 6, &six, message, sizeof message) ==
           FORTSYM_OK);

    /* Column-major: [ [1, 2], [3, 4] ]. */
    matrix[0] = one;
    matrix[1] = three;
    matrix[2] = two;
    matrix[3] = four;
    right_hand_side[0] = five;
    right_hand_side[1] = six;
    assert(fortsym_linsolve(arena, matrix, right_hand_side, 2, values, 2,
                            &count, message, sizeof message) == FORTSYM_OK);
    assert(count == 2);
    assert(has_text(values, count, "-4"));
    assert(has_text(values, count, "9/2"));
    fortsym_expr_free(values[0]);
    fortsym_expr_free(values[1]);
    values[0] = NULL;
    values[1] = NULL;

    assert(fortsym_symbol(arena, "x", &x, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_symbol(arena, "y", &y, message, sizeof message) ==
           FORTSYM_OK);
    /* x + y = 1 retains y as the supplied free parameter. */
    free_matrix[0] = one;
    free_matrix[1] = one;
    free_right_hand_side[0] = one;
    free_variables[0] = x;
    free_variables[1] = y;
    assert(fortsym_linsolve_parametric(
               arena, free_matrix, free_right_hand_side, free_variables, 1,
               2, values, 2, &count, message, sizeof message) == FORTSYM_OK);
    assert(count == 2);
    assert(has_text(values, count, "-(y - 1)"));
    assert(has_text(values, count, "y"));
    fortsym_expr_free(values[0]);
    fortsym_expr_free(values[1]);
    values[0] = NULL;
    values[1] = NULL;

    /* An inconsistent rectangular system is a successful empty result. */
    inconsistent_matrix[0] = one;
    inconsistent_matrix[1] = one;
    inconsistent_right_hand_side[0] = one;
    inconsistent_right_hand_side[1] = two;
    inconsistent_variables[0] = x;
    assert(fortsym_linsolve_parametric(
               arena, inconsistent_matrix, inconsistent_right_hand_side,
               inconsistent_variables, 2, 1, values, 2, &count, message,
               sizeof message) == FORTSYM_OK);
    assert(count == 0 && values[0] == NULL && values[1] == NULL);

    assert(fortsym_linsolve(arena, matrix, right_hand_side, 2, values, 1,
                            &count, message, sizeof message) ==
           FORTSYM_RESOURCE_LIMIT);
    assert(count == 2 && values[0] == NULL);

    fortsym_expr_free(six);
    fortsym_expr_free(five);
    fortsym_expr_free(y);
    fortsym_expr_free(x);
    fortsym_expr_free(four);
    fortsym_expr_free(three);
    fortsym_expr_free(two);
    fortsym_expr_free(one);
    fortsym_arena_free(arena);
    return 0;
}
