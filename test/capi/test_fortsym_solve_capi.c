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
    fortsym_expr *x = NULL, *one = NULL, *two = NULL;
    fortsym_expr *square = NULL, *equation = NULL, *relation = NULL;
    fortsym_expr *roots[4] = {0};
    size_t count = 0;

    assert(fortsym_abi_version() == 77);
    assert(fortsym_arena_new(&arena, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "x", &x, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) ==
           FORTSYM_OK);

    assert(fortsym_power(arena, x, two, &square, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_subtract(arena, square, one, &equation, message,
                            sizeof message) == FORTSYM_OK);
    assert(fortsym_solve(arena, equation, x, roots, 4, &count, message,
                         sizeof message) == FORTSYM_OK);
    assert(count == 2);
    assert(has_text(roots, count, "1"));
    assert(has_text(roots, count, "-1"));
    for (size_t index = 0; index < count; ++index)
        fortsym_expr_free(roots[index]);
    memset(roots, 0, sizeof roots);

    assert(fortsym_relation(arena, x, one, FORTSYM_RELATION_EQUAL, &relation,
                            message, sizeof message) == FORTSYM_OK);
    assert(fortsym_solve(arena, relation, x, roots, 4, &count, message,
                         sizeof message) == FORTSYM_OK);
    assert(count == 1 && has_text(roots, count, "1"));
    fortsym_expr_free(roots[0]);
    memset(roots, 0, sizeof roots);

    assert(fortsym_solve(arena, equation, x, roots, 1, &count, message,
                         sizeof message) == FORTSYM_RESOURCE_LIMIT);
    assert(count == 2 && roots[0] == NULL && roots[1] == NULL);

    fortsym_expr_free(relation);
    fortsym_expr_free(equation);
    fortsym_expr_free(square);
    fortsym_expr_free(two);
    fortsym_expr_free(one);
    fortsym_expr_free(x);
    fortsym_arena_free(arena);
    return 0;
}
