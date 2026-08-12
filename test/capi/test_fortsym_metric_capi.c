#include "fortsym.h"

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

static void expect_text(fortsym_arena *arena, const fortsym_expr *value,
                        const char *expected)
{
    char text[32];
    char message[64];
    size_t required = 0;
    fortsym_expr *simplified = NULL;
    int status = fortsym_simplify(arena, value, &simplified, message,
                                  sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_expr_text(simplified, text, sizeof text, &required,
                               message, sizeof message);
    if (status != FORTSYM_OK)
        fprintf(stderr, "fortsym_expr_text failed: %d (%s)\n", status, message);
    assert(status == FORTSYM_OK);
    assert(required == strlen(expected) + 1);
    assert(strcmp(text, expected) == 0);
    fortsym_expr_free(simplified);
}

int main(void)
{
    char message[128];
    fortsym_arena *arena = NULL;
    fortsym_expr *zero = NULL;
    fortsym_expr *one = NULL;
    fortsym_expr *two = NULL;
    fortsym_expr *three = NULL;
    fortsym_expr *four = NULL;
    fortsym_expr *five = NULL;
    fortsym_expr *six = NULL;
    fortsym_expr *volume = NULL;
    fortsym_expr *inner = NULL;
    fortsym_expr *epsilon[27] = {0};
    const fortsym_expr *components[9];
    const fortsym_expr *left[3];
    const fortsym_expr *right[3];
    int signature[3] = {1, 1, 1};
    int status;

    assert(fortsym_abi_version() == 54);
    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    assert(fortsym_int(arena, 0, &zero, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 3, &three, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 4, &four, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 5, &five, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 6, &six, message, sizeof message) == FORTSYM_OK);
    components[0] = two;
    components[1] = one;
    components[2] = zero;
    components[3] = one;
    components[4] = three;
    components[5] = zero;
    components[6] = zero;
    components[7] = zero;
    components[8] = four;
    left[0] = one;
    left[1] = two;
    left[2] = three;
    right[0] = four;
    right[1] = five;
    right[2] = six;

    status = fortsym_metric_inner(
        arena, components, signature, 1, left, right, &inner, message,
        sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(arena, inner, "123");

    components[0] = one;
    components[1] = zero;
    components[2] = zero;
    components[3] = zero;
    components[4] = one;
    components[5] = zero;
    components[6] = zero;
    components[7] = zero;
    components[8] = one;

    status = fortsym_metric_volume_density(
        arena, components, signature, 1, &volume, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(arena, volume, "1");
    status = fortsym_metric_levi_civita(
        arena, components, signature, 1, -1, epsilon, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(arena, epsilon[21], "1");
    expect_text(arena, epsilon[15], "-1");
    for (size_t index = 0; index < 27; ++index)
        fortsym_expr_free(epsilon[index]);
    fortsym_expr_free(inner);
    fortsym_expr_free(six);
    fortsym_expr_free(five);
    fortsym_expr_free(four);
    fortsym_expr_free(three);
    fortsym_expr_free(two);
    fortsym_expr_free(volume);
    fortsym_expr_free(one);
    fortsym_expr_free(zero);
    fortsym_arena_free(arena);
    return 0;
}
