#include "fortsym.h"

#include <assert.h>
#include <stddef.h>
#include <string.h>

int main(void)
{
    char message[256];
    char text[128];
    size_t required = 0;
    fortsym_arena *arena = NULL;
    fortsym_expr *one = NULL, *two = NULL, *three = NULL, *four = NULL;
    fortsym_expr *row_one = NULL, *row_two = NULL, *matrix = NULL;
    fortsym_expr *determinant = NULL;
    const fortsym_expr *row_one_values[2];
    const fortsym_expr *row_two_values[2];
    const fortsym_expr *rows[2];

    assert(fortsym_abi_version() == 79);
    assert(fortsym_arena_new(&arena, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 3, &three, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 4, &four, message, sizeof message) ==
           FORTSYM_OK);

    row_one_values[0] = one;
    row_one_values[1] = two;
    row_two_values[0] = three;
    row_two_values[1] = four;
    assert(fortsym_function(arena, "List", row_one_values, 2, &row_one,
                            message, sizeof message) == FORTSYM_OK);
    assert(fortsym_function(arena, "List", row_two_values, 2, &row_two,
                            message, sizeof message) == FORTSYM_OK);
    rows[0] = row_one;
    rows[1] = row_two;
    assert(fortsym_function(arena, "List", rows, 2, &matrix, message,
                            sizeof message) == FORTSYM_OK);
    assert(fortsym_matrix_det(arena, matrix, &determinant, message,
                              sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(determinant, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "-2") == 0);

    fortsym_expr_free(determinant);
    fortsym_expr_free(matrix);
    fortsym_expr_free(row_two);
    fortsym_expr_free(row_one);
    fortsym_expr_free(four);
    fortsym_expr_free(three);
    fortsym_expr_free(two);
    fortsym_expr_free(one);
    fortsym_arena_free(arena);
    return 0;
}
