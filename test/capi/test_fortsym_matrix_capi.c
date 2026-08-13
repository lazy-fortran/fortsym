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
    fortsym_arena *foreign_arena = NULL;
    fortsym_expr *zero = NULL, *one = NULL, *two = NULL, *three = NULL, *four = NULL;
    fortsym_expr *foreign_one = NULL;
    fortsym_expr *row_one = NULL, *row_two = NULL, *matrix = NULL;
    fortsym_expr *bad_matrix = NULL;
    fortsym_expr *determinant = NULL, *rank = NULL, *inverse = NULL;
    fortsym_expr *transposed = NULL;
    fortsym_expr *null_row_one = NULL, *null_row_two = NULL;
    fortsym_expr *null_matrix = NULL, *nullspace = NULL, *rref = NULL;
    fortsym_expr *product = NULL, *scaled = NULL;
    fortsym_expr *sum = NULL, *difference = NULL, *negative = NULL, *divided = NULL;
    fortsym_expr *reduced = NULL, *pivots = NULL;
    fortsym_expr *inverse_row = NULL, *inverse_entry = NULL;
    const fortsym_expr *row_one_values[2];
    const fortsym_expr *row_two_values[2];
    const fortsym_expr *rows[2];
    const fortsym_expr *null_row_one_values[3];
    const fortsym_expr *null_row_two_values[3];
    const fortsym_expr *null_rows[2];

    assert(fortsym_abi_version() == 87);
    assert(fortsym_arena_new(&arena, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 0, &zero, message, sizeof message) == FORTSYM_OK);
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
    assert(fortsym_matrix_transpose(arena, matrix, &transposed, message,
                                    sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(transposed, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(1, 3), List(2, 4))") == 0);
    null_row_one_values[0] = one;
    null_row_one_values[1] = two;
    null_row_one_values[2] = three;
    null_row_two_values[0] = two;
    null_row_two_values[1] = four;
    null_row_two_values[2] = four;
    assert(fortsym_function(arena, "List", null_row_one_values, 3,
                            &null_row_one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_function(arena, "List", null_row_two_values, 3,
                            &null_row_two, message, sizeof message) == FORTSYM_OK);
    null_rows[0] = null_row_one;
    null_rows[1] = null_row_two;
    assert(fortsym_function(arena, "List", null_rows, 2, &null_matrix,
                            message, sizeof message) == FORTSYM_OK);
    assert(fortsym_matrix_nullspace(arena, null_matrix, &nullspace, message,
                                    sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(nullspace, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(-2, 1, 0))") == 0);
    assert(fortsym_matrix_rref(arena, null_matrix, &rref, message,
                               sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_argument(rref, 0, &reduced, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(reduced, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(1, 2, 0), List(0, 0, 1))") == 0);
    assert(fortsym_expr_argument(rref, 1, &pivots, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(pivots, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(0, 2)") == 0);
    assert(fortsym_matrix_multiply(arena, matrix, matrix, &product, message,
                                   sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(product, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(7, 10), List(15, 22))") == 0);
    assert(fortsym_matrix_multiply(arena, matrix, two, &scaled, message,
                                   sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(scaled, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(2, 4), List(6, 8))") == 0);
    assert(fortsym_matrix_add(arena, matrix, matrix, &sum, message,
                              sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(sum, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(2, 4), List(6, 8))") == 0);
    assert(fortsym_matrix_subtract(arena, matrix, matrix, &difference,
                                   message, sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(difference, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(0, 0), List(0, 0))") == 0);
    assert(fortsym_matrix_negate(arena, matrix, &negative, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(negative, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(-1, -2), List(-3, -4))") == 0);
    assert(fortsym_matrix_divide(arena, matrix, two, &divided, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(divided, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(1/2, 1), List(3/2, 2))") == 0);
    fortsym_expr_free(divided);
    divided = NULL;
    assert(fortsym_matrix_divide(arena, matrix, zero, &divided, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(divided, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "List(List(zoo, zoo), List(zoo, zoo))") == 0);
    fortsym_expr_free(divided);
    divided = NULL;
    fortsym_expr_free(scaled);
    fortsym_expr_free(product);
    fortsym_expr_free(negative);
    fortsym_expr_free(difference);
    fortsym_expr_free(sum);
    scaled = NULL;
    product = NULL;
    {
        const fortsym_expr *bad_rows[1] = {row_one};
        assert(fortsym_function(arena, "List", bad_rows, 1, &bad_matrix,
                                message, sizeof message) == FORTSYM_OK);
    }
    assert(fortsym_matrix_multiply(arena, bad_matrix, bad_matrix, &product,
                                   message, sizeof message) ==
           FORTSYM_UNSUPPORTED);
    assert(product == NULL);
    assert(fortsym_matrix_add(arena, bad_matrix, matrix, &sum, message,
                              sizeof message) == FORTSYM_UNSUPPORTED);
    assert(sum == NULL);
    assert(fortsym_arena_new(&foreign_arena, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(foreign_arena, 1, &foreign_one, message,
                       sizeof message) == FORTSYM_OK);
    assert(fortsym_matrix_multiply(arena, matrix, foreign_one, &product,
                                   message, sizeof message) ==
           FORTSYM_FOREIGN_ARENA);
    assert(product == NULL);
    assert(fortsym_matrix_add(arena, matrix, foreign_one, &sum, message,
                              sizeof message) == FORTSYM_FOREIGN_ARENA);
    assert(sum == NULL);
    assert(fortsym_matrix_divide(arena, matrix, foreign_one, &divided, message,
                                 sizeof message) == FORTSYM_FOREIGN_ARENA);
    assert(divided == NULL);
    fortsym_expr_free(bad_matrix);
    fortsym_expr_free(foreign_one);
    fortsym_arena_free(foreign_arena);
    fortsym_expr_free(pivots);
    fortsym_expr_free(reduced);
    fortsym_expr_free(rref);
    assert(fortsym_matrix_rank(arena, matrix, &rank, message,
                               sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(rank, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "2") == 0);
    assert(fortsym_matrix_inverse(arena, matrix, &inverse, message,
                                  sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_argument(inverse, 0, &inverse_row, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_argument(inverse_row, 0, &inverse_entry, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_expr_text(inverse_entry, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, "-2") == 0);
    fortsym_expr_free(inverse_entry);
    inverse_entry = NULL;
    fortsym_expr_free(inverse_row);
    inverse_row = NULL;

    fortsym_expr_free(transposed);
    fortsym_expr_free(nullspace);
    fortsym_expr_free(null_matrix);
    fortsym_expr_free(null_row_two);
    fortsym_expr_free(null_row_one);
    fortsym_expr_free(inverse);
    fortsym_expr_free(rank);
    fortsym_expr_free(determinant);
    fortsym_expr_free(matrix);
    fortsym_expr_free(row_two);
    fortsym_expr_free(row_one);
    fortsym_expr_free(four);
    fortsym_expr_free(three);
    fortsym_expr_free(two);
    fortsym_expr_free(one);
    fortsym_expr_free(zero);
    fortsym_arena_free(arena);
    return 0;
}
