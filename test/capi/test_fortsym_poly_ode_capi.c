#include "fortsym.h"

#include <assert.h>
#include <stddef.h>
#include <string.h>

static void assert_text(const fortsym_expr *expression, const char *expected)
{
    char text[256];
    char message[256];
    size_t required = 0;
    assert(fortsym_expr_text(expression, text, sizeof text, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strcmp(text, expected) == 0);
}

static void assert_contains(const fortsym_expr *expression, const char *text)
{
    char actual[256];
    char message[256];
    size_t required = 0;
    assert(fortsym_expr_text(expression, actual, sizeof actual, &required,
                             message, sizeof message) == FORTSYM_OK);
    assert(strstr(actual, text) != NULL);
}

int main(void)
{
    char message[256];
    fortsym_arena *arena = NULL;
    fortsym_arena *foreign = NULL;
    fortsym_expr *x = NULL, *one = NULL, *two = NULL, *three = NULL;
    fortsym_expr *x2 = NULL, *x3 = NULL, *quadratic = NULL, *cubic = NULL;
    fortsym_expr *coefficient = NULL, *degree = NULL, *greatest = NULL;
    fortsym_expr *quotient = NULL, *remainder = NULL, *foreign_x = NULL;
    fortsym_expr *y = NULL, *derivative = NULL, *a = NULL, *rhs = NULL;
    fortsym_expr *problem = NULL, *solution = NULL;
    const fortsym_expr *function_arguments[1];

    assert(fortsym_arena_new(&arena, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_arena_new(&foreign, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "x", &x, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_int(arena, 1, &one, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 2, &two, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_int(arena, 3, &three, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_power(arena, x, two, &x2, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_power(arena, x, three, &x3, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_subtract(arena, x2, one, &quadratic, message,
                            sizeof message) == FORTSYM_OK);
    assert(fortsym_subtract(arena, x3, one, &cubic, message, sizeof message) ==
           FORTSYM_OK);

    assert(fortsym_poly_coefficient(arena, cubic, x, 3, &coefficient,
                                    message, sizeof message) == FORTSYM_OK);
    assert_text(coefficient, "1");
    fortsym_expr_free(coefficient);
    coefficient = NULL;
    assert(fortsym_poly_exponent(arena, cubic, x, &degree, message,
                                 sizeof message) == FORTSYM_OK);
    assert_text(degree, "3");
    fortsym_expr_free(degree);
    degree = NULL;
    assert(fortsym_poly_gcd(arena, cubic, quadratic, &greatest, message,
                            sizeof message) == FORTSYM_OK);
    assert_contains(greatest, "x");
    assert_contains(greatest, "1");
    assert(fortsym_poly_quotient(arena, cubic, quadratic, x, &quotient,
                                 message, sizeof message) == FORTSYM_OK);
    assert_contains(quotient, "x");
    assert(fortsym_poly_remainder(arena, cubic, quadratic, x, &remainder,
                                  message, sizeof message) == FORTSYM_OK);
    assert_contains(remainder, "x");
    assert_contains(remainder, "1");

    assert(fortsym_symbol(foreign, "x", &foreign_x, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_poly_coefficient(arena, cubic, foreign_x, 1, &coefficient,
                                    message, sizeof message) ==
           FORTSYM_FOREIGN_ARENA);
    assert(coefficient == NULL);

    function_arguments[0] = x;
    assert(fortsym_function(arena, "y", function_arguments, 1, &y, message,
                            sizeof message) == FORTSYM_OK);
    assert(fortsym_differentiate(arena, y, x, &derivative, message,
                                 sizeof message) == FORTSYM_OK);
    assert(fortsym_symbol(arena, "a", &a, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_multiply(arena, a, y, &rhs, message, sizeof message) ==
           FORTSYM_OK);
    assert(fortsym_relation(arena, derivative, rhs, FORTSYM_RELATION_EQUAL,
                            &problem, message, sizeof message) == FORTSYM_OK);
    assert(fortsym_solve_ode(arena, problem, y, x, &solution, message,
                             sizeof message) == FORTSYM_OK);
    {
        char text[512];
        size_t required = 0;
        assert(fortsym_expr_text(solution, text, sizeof text, &required,
                                 message, sizeof message) == FORTSYM_OK);
        assert(strstr(text, "Rule(y(x)") != NULL);
    }

    fortsym_expr_free(solution);
    fortsym_expr_free(problem);
    fortsym_expr_free(rhs);
    fortsym_expr_free(a);
    fortsym_expr_free(derivative);
    fortsym_expr_free(y);
    fortsym_expr_free(foreign_x);
    fortsym_expr_free(remainder);
    fortsym_expr_free(quotient);
    fortsym_expr_free(greatest);
    fortsym_expr_free(cubic);
    fortsym_expr_free(quadratic);
    fortsym_expr_free(x3);
    fortsym_expr_free(x2);
    fortsym_expr_free(three);
    fortsym_expr_free(two);
    fortsym_expr_free(one);
    fortsym_expr_free(x);
    fortsym_arena_free(foreign);
    fortsym_arena_free(arena);
    return 0;
}
