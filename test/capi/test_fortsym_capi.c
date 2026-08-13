#include "fortsym.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

static void expect_text(const fortsym_expr *expression, const char *expected)
{
    char buffer[256];
    char message[128];
    size_t required = 0;
    int status = fortsym_expr_text(expression, buffer, sizeof buffer,
                                   &required, message, sizeof message);
    assert(status == FORTSYM_OK);
    if (required != strlen(expected) + 1 || strcmp(buffer, expected) != 0)
        fprintf(stderr, "got [%s] required=%zu expected [%s]\\n", buffer,
                required, expected);
    assert(required == strlen(expected) + 1);
    assert(strcmp(buffer, expected) == 0);
}

int main(void)
{
    char message[128];
    char buffer[256];
    char short_buffer[2];
    size_t required = 0;
    int64_t integer_value = 0;
    int kind = 0;
    int number = 0;
    int predicate_verdict = FORTSYM_ZERO_UNKNOWN;
    int equal = 0;
    int verdict = FORTSYM_ZERO_UNKNOWN;
    int known = 0;
    int status;
    fortsym_arena *arena = NULL;
    fortsym_arena *other_arena = NULL;
    fortsym_expr *x = NULL;
    fortsym_expr *y = NULL;
    fortsym_expr *zero = NULL;
    fortsym_expr *one = NULL;
    fortsym_expr *sum = NULL;
    fortsym_expr *product = NULL;
    fortsym_expr *derivative = NULL;
    fortsym_expr *replacement = NULL;
    fortsym_expr *substituted = NULL;
    fortsym_expr *simultaneous = NULL;
    fortsym_expr *foreign = NULL;
    fortsym_expr *two = NULL;
    fortsym_expr *square = NULL;
    fortsym_expr *powered = NULL;
    fortsym_expr *power_identity = NULL;
    fortsym_expr *expanded_input = NULL;
    fortsym_expr *factored = NULL;
    fortsym_expr *quotient_num = NULL;
    fortsym_expr *quotient_den = NULL;
    fortsym_expr *quotient = NULL;
    fortsym_expr *root = NULL;
    fortsym_expr *assumed = NULL;
    fortsym_expr *relation = NULL;
    fortsym_expr *zero_expression = NULL;
    fortsym_expr *seven = NULL;
    fortsym_expr *sine = NULL;
    fortsym_expr *imaginary = NULL;
    fortsym_expr *real_part = NULL;
    fortsym_expr *imaginary_part = NULL;
    fortsym_expr *conjugated = NULL;
    fortsym_expr *conjugate_sum = NULL;
    fortsym_expr *minus_one = NULL;
    fortsym_expr *argument = NULL;
    fortsym_expr *modulus = NULL;
    fortsym_expr *complex_expanded = NULL;
    fortsym_expr *infinity = NULL;
    fortsym_expr *complex_infinity = NULL;
    fortsym_expr *undefined = NULL;
    fortsym_expr *expanded_infinity = NULL;
    fortsym_expr *expanded_complex_infinity = NULL;
    fortsym_expr *expanded_undefined = NULL;
    fortsym_expr *sentinel_re = NULL;
    fortsym_expr *sentinel_nan_re = NULL;
    fortsym_expr *sentinel_im = NULL;
    fortsym_expr *sentinel_abs = NULL;
    fortsym_expr *sentinel_arg = NULL;
    fortsym_expr *sentinel_conjugate = NULL;
    fortsym_expr *negative_infinity = NULL;
    fortsym_expr *two_thirds = NULL;
    fortsym_expr *negative_phase_power = NULL;
    fortsym_expr *negative_phase_result = NULL;
    fortsym_expr *periodic = NULL;
    fortsym_expr *periodic_simplified = NULL;
    fortsym_expr *log_zero = NULL;
    fortsym_expr *log_zero_simplified = NULL;
    fortsym_expr *exp_log_zero = NULL;
    fortsym_expr *exp_log_zero_simplified = NULL;
    fortsym_expr *negative_two = NULL;
    fortsym_expr *negative_log = NULL;
    fortsym_expr *negative_log_simplified = NULL;
    fortsym_expr *negative_four = NULL;
    fortsym_expr *half = NULL;
    fortsym_expr *three = NULL;
    fortsym_expr *five = NULL;
    fortsym_expr *twenty_one = NULL;
    fortsym_expr *sqrt_three = NULL;
    fortsym_expr *negative_imaginary = NULL;
    fortsym_expr *pole = NULL;
    fortsym_expr *pole_simplified = NULL;
    fortsym_expr *branch = NULL;
    fortsym_expr *branch_simplified = NULL;
    fortsym_expr *special = NULL;
    fortsym_expr *special_simplified = NULL;
    fortsym_expr *legendre = NULL;
    fortsym_expr *legendre_simplified = NULL;
    fortsym_expr *unknown_head = NULL;
    fortsym_expr *coordinates[3];
    fortsym_expr *position[3];
    fortsym_expr *symmetric_tensor[9] = {0};
    const fortsym_expr *tensor_components[9];
    int tensor_variance[2] = {FORTSYM_UPPER_VARIANCE, FORTSYM_UPPER_VARIANCE};
    const fortsym_expr *root_argument[1];
    const fortsym_expr *substitution_old[2];
    const fortsym_expr *substitution_new[2];
    const fortsym_expr *special_arguments[2];
    const fortsym_expr *legendre_arguments[3];

    assert(fortsym_abi_version() == 93);
    status = fortsym_arena_new(&arena, message, sizeof message);
    assert(status == FORTSYM_OK && arena != NULL);
    status = fortsym_symbol(arena, "x", &x, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "y", &y, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_symbol(arena, "z", &coordinates[2], message, sizeof message);
    assert(status == FORTSYM_OK);
    coordinates[0] = x;
    coordinates[1] = y;
    position[0] = x;
    position[1] = y;
    position[2] = coordinates[2];
    status = fortsym_int(arena, 1, &one, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_int(arena, 0, &zero, message, sizeof message);
    assert(status == FORTSYM_OK);
    tensor_components[0] = one;
    tensor_components[1] = zero;
    tensor_components[2] = zero;
    tensor_components[3] = zero;
    tensor_components[4] = one;
    tensor_components[5] = zero;
    tensor_components[6] = zero;
    tensor_components[7] = zero;
    tensor_components[8] = one;
    status = fortsym_chart_tensor_declare_symmetry(
        arena, (const fortsym_expr **)coordinates,
        (const fortsym_expr **)position, tensor_components, 2,
        tensor_variance, 0, 1, 2, FORTSYM_SYMMETRIC, symmetric_tensor,
        message, sizeof message);
    assert(status == FORTSYM_OK);
    for (size_t index = 0; index < 9; ++index)
        fortsym_expr_free(symmetric_tensor[index]);
    tensor_components[1] = one;
    status = fortsym_chart_tensor_declare_symmetry(
        arena, (const fortsym_expr **)coordinates,
        (const fortsym_expr **)position, tensor_components, 2,
        tensor_variance, 0, 1, 2, FORTSYM_SYMMETRIC, symmetric_tensor,
        message, sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    status = fortsym_add(arena, x, one, &sum, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_multiply(arena, sum, y, &product, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(product, "y*(x + 1)");
    status = fortsym_expr_is_number(product, &number, message, sizeof message);
    assert(status == FORTSYM_OK && number == 0);
    size_t operation_count = 0;
    status = fortsym_expr_operation_count(product, &operation_count, message,
                                          sizeof message);
    assert(status == FORTSYM_OK && operation_count == 2);
    status = fortsym_expr_free_symbols(product, buffer, sizeof buffer, &required,
                                       message, sizeof message);
    assert(status == FORTSYM_OK && required == 5);
    assert((buffer[0] == 'x' && buffer[2] == 'y') ||
           (buffer[0] == 'y' && buffer[2] == 'x'));
    assert(buffer[1] == '\0' && buffer[3] == '\0' && buffer[4] == '\0');
    status = fortsym_expr_free_symbols(product, short_buffer,
                                       sizeof short_buffer, &required,
                                       message, sizeof message);
    assert(status == FORTSYM_RESOURCE_LIMIT && required == 5);
    assert((short_buffer[0] == 'x' || short_buffer[0] == 'y') &&
           short_buffer[1] == '\0');

    status = fortsym_expr_kind(one, &kind, message, sizeof message);
    assert(status == FORTSYM_OK && kind == FORTSYM_INT);
    status = fortsym_expr_is_number(one, &number, message, sizeof message);
    assert(status == FORTSYM_OK && number == 1);
    status = fortsym_expr_is_algebraic(one, &predicate_verdict, message,
                                       sizeof message);
    assert(status == FORTSYM_OK && predicate_verdict == FORTSYM_ZERO_TRUE);
    status = fortsym_expr_int_value(one, &integer_value, message, sizeof message);
    assert(status == FORTSYM_OK && integer_value == 1);
    status = fortsym_expr_exact_text(one, buffer, sizeof buffer, &required,
                                     message, sizeof message);
    assert(status == FORTSYM_OK && strcmp(buffer, "1") == 0);

    status = fortsym_int(arena, 2, &two, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_power(arena, x, two, &square, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_power(arena, sum, two, &powered, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_power(arena, x, zero, &power_identity, message,
                           sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(power_identity, "1");
    fortsym_expr_free(power_identity);
    power_identity = NULL;
    status = fortsym_power(arena, x, one, &power_identity, message,
                           sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(power_identity, "x");
    fortsym_expr_free(power_identity);
    power_identity = NULL;
    status = fortsym_power(arena, one, x, &power_identity, message,
                           sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(power_identity, "1");
    status = fortsym_expand(arena, powered, &expanded_input, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_factor(arena, expanded_input, &factored, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(factored, "(x + 1)**2");
    status = fortsym_subtract(arena, square, one, &quotient_num, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_subtract(arena, x, one, &quotient_den, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_divide(arena, quotient_num, quotient_den, &quotient,
                            message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_factor(arena, quotient, &foreign, message,
                            sizeof message);
    assert(status == FORTSYM_UNSUPPORTED && foreign == NULL);
    root_argument[0] = square;
    status = fortsym_function(arena, "sqrt", root_argument, 1, &root,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_relation(arena, x, one, FORTSYM_RELATION_GREATER,
                              &relation, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assume_relation(arena, relation, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assume(arena, x, FORTSYM_FACT_POSITIVE, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_REAL, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assume(arena, y, FORTSYM_FACT_INTEGER, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_INTEGER, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_RATIONAL, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_REAL, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_ALGEBRAIC, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assume(arena, x, FORTSYM_FACT_RATIONAL, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_RATIONAL, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assume(arena, x, FORTSYM_FACT_NEGATIVE, message,
                            sizeof message);
    assert(status == FORTSYM_CONFLICT);
    assert(strstr(message, "contradictory assumptions") != NULL);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 0);
    status = fortsym_assumption_has(arena, x, 2048, &known, message,
                                    sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    status = fortsym_assumption_push(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assume(arena, y, FORTSYM_FACT_NONNEGATIVE, message,
                            sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_NONNEGATIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_push(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_NONNEGATIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_assumption_pop(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_pop(arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_assumption_has(arena, y, FORTSYM_FACT_NONNEGATIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 0);
    status = fortsym_assumption_pop(arena, message, sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    status = fortsym_assumption_has(arena, x, FORTSYM_FACT_POSITIVE, &known,
                                    message, sizeof message);
    assert(status == FORTSYM_OK && known == 1);
    status = fortsym_simplify(arena, root, &assumed, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(assumed, "x");

    status = fortsym_subtract(arena, x, x, &zero_expression, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, zero_expression, &verdict, message,
                               sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_TRUE);
    status = fortsym_int(arena, 7, &seven, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, seven, &verdict, message, sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_FALSE);
    root_argument[0] = x;
    status = fortsym_function(arena, "sin", root_argument, 1, &sine,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, sine, &verdict, message, sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_UNKNOWN);

    status = fortsym_constant(arena, "i", &imaginary, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_complex_operation(arena, imaginary, "re", &real_part,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(real_part, "0");
    status = fortsym_complex_operation(arena, imaginary, "im", &imaginary_part,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(imaginary_part, "1");
    status = fortsym_complex_operation(arena, imaginary, "abs", &modulus,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(modulus, "1");
    status = fortsym_complex_operation(arena, imaginary, "expand_complex",
                                       &complex_expanded, message,
                                       sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(complex_expanded, "i");
    status = fortsym_constant(arena, "oo", &infinity, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_constant(arena, "zoo", &complex_infinity, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_constant(arena, "nan", &undefined, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_complex_operation(arena, infinity, "expand_complex",
                                       &expanded_infinity, message,
                                       sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(expanded_infinity, "oo");
    status = fortsym_complex_operation(arena, complex_infinity,
                                       "expand_complex", &expanded_complex_infinity,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(expanded_complex_infinity, "nan");
    status = fortsym_complex_operation(arena, undefined, "expand_complex",
                                       &expanded_undefined, message,
                                       sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(expanded_undefined, "nan");
    status = fortsym_complex_operation(arena, infinity, "re", &sentinel_re,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(sentinel_re, "oo");
    status = fortsym_complex_operation(arena, infinity, "im", &sentinel_im,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(sentinel_im, "0");
    status = fortsym_complex_operation(arena, complex_infinity, "abs",
                                       &sentinel_abs, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(sentinel_abs, "oo");
    status = fortsym_complex_operation(arena, complex_infinity, "conjugate",
                                       &sentinel_conjugate, message,
                                       sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(sentinel_conjugate, "conjugate(zoo)");
    status = fortsym_complex_operation(arena, undefined, "re", &sentinel_nan_re,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(sentinel_nan_re, "nan");
    root_argument[0] = complex_infinity;
    status = fortsym_function(arena, "sin", root_argument, 1, &periodic,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, periodic, &periodic_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(periodic_simplified, "nan");
    root_argument[0] = zero;
    status = fortsym_function(arena, "log", root_argument, 1, &log_zero,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, log_zero, &log_zero_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(log_zero_simplified, "zoo");
    root_argument[0] = log_zero;
    status = fortsym_function(arena, "exp", root_argument, 1, &exp_log_zero,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, exp_log_zero, &exp_log_zero_simplified,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(exp_log_zero_simplified, "nan");
    status = fortsym_int(arena, -2, &negative_two, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = negative_two;
    status = fortsym_function(arena, "log", root_argument, 1, &negative_log,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, negative_log, &negative_log_simplified,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(negative_log_simplified, "log(2) + i*pi");
    root_argument[0] = zero;
    status = fortsym_function(arena, "gamma", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "zoo");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    root_argument[0] = negative_two;
    status = fortsym_function(arena, "loggamma", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "oo");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    status = fortsym_function(arena, "factorial", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "zoo");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    status = fortsym_int(arena, 5, &five, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = five;
    status = fortsym_function(arena, "factorial", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "120");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    status = fortsym_exact(arena, "21", &twenty_one, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = twenty_one;
    status = fortsym_function(arena, "factorial", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "51090942171709440000");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    special_arguments[0] = undefined;
    special_arguments[1] = x;
    status = fortsym_function(arena, "besselj", special_arguments, 2,
                              &special, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, special, &special_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(special_simplified, "besselj(nan, x)");
    legendre_arguments[0] = seven;
    legendre_arguments[1] = zero;
    legendre_arguments[2] = infinity;
    status = fortsym_function(arena, "legendrep", legendre_arguments, 3,
                              &legendre, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, legendre, &legendre_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(legendre_simplified, "nan");
    status = fortsym_complex_operation(arena, imaginary, "conjugate",
                                       &conjugated, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_add(arena, conjugated, imaginary, &conjugate_sum, message,
                         sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_zero_test(arena, conjugate_sum, &verdict, message,
                               sizeof message);
    assert(status == FORTSYM_OK && verdict == FORTSYM_ZERO_TRUE);
    status = fortsym_int(arena, -1, &minus_one, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = one;
    status = fortsym_function(arena, "atanh", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "oo");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    root_argument[0] = minus_one;
    status = fortsym_function(arena, "atanh", root_argument, 1, &pole,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, pole, &pole_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(pole_simplified, "-oo");
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    pole_simplified = NULL;
    pole = NULL;
    root_argument[0] = zero;
    status = fortsym_function(arena, "acosh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = minus_one;
    status = fortsym_function(arena, "acosh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*pi");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = minus_one;
    status = fortsym_function(arena, "sqrt", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    status = fortsym_int(arena, -4, &negative_four, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = negative_four;
    status = fortsym_function(arena, "sqrt", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "log", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    status = fortsym_multiply(arena, minus_one, imaginary, &negative_imaginary,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "log", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "asinh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "asinh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = one;
    status = fortsym_function(arena, "asinh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "log(sqrt(2) + 1)");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = minus_one;
    status = fortsym_function(arena, "asinh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-log(sqrt(2) + 1)");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "atanh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*pi*1/4");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "atanh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-i*pi*1/4");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    status = fortsym_multiply(arena, minus_one, infinity, &negative_infinity,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "atan", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*oo");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "atan", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-i*oo");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "acosh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "log(sqrt(2) + 1) + i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "acosh", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "log(sqrt(2) + 1) - i*pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "asin", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*log(sqrt(2) + 1)");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "asin", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-i*log(sqrt(2) + 1)");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = imaginary;
    status = fortsym_function(arena, "acos", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "-i*log(sqrt(2) + 1) + pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = negative_imaginary;
    status = fortsym_function(arena, "acos", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "i*log(sqrt(2) + 1) + pi*1/2");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    status = fortsym_rational(arena, 1, 2, &half, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = half;
    status = fortsym_function(arena, "asin", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "pi*1/6");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    root_argument[0] = half;
    status = fortsym_function(arena, "acos", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "pi*1/3");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    status = fortsym_int(arena, 3, &three, message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = three;
    status = fortsym_function(arena, "sqrt", root_argument, 1, &sqrt_three,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    root_argument[0] = sqrt_three;
    status = fortsym_function(arena, "atan", root_argument, 1, &branch,
                              message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, branch, &branch_simplified, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(branch_simplified, "pi*1/3");
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    branch_simplified = NULL;
    branch = NULL;
    status = fortsym_rational(arena, 2, 3, &two_thirds, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_power(arena, negative_infinity, two_thirds,
                           &negative_phase_power, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_simplify(arena, negative_phase_power,
                              &negative_phase_result, message,
                              sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(negative_phase_result, "oo*(-1)**(2/3)");
    status = fortsym_complex_operation(arena, negative_infinity, "arg",
                                       &sentinel_arg, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(sentinel_arg, "pi");
    status = fortsym_complex_operation(arena, minus_one, "arg", &argument,
                                       message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(argument, "pi");
    root_argument[0] = x;
    status = fortsym_function(arena, "unknown_complex", root_argument, 1,
                              &unknown_head, message, sizeof message);
    assert(status == FORTSYM_OK);
    foreign = NULL;
    status = fortsym_complex_operation(arena, unknown_head, "re", &foreign,
                                       message, sizeof message);
    assert(status == FORTSYM_UNSUPPORTED && foreign == NULL);

    status = fortsym_differentiate(arena, x, x, &derivative, message,
                                   sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(derivative, "1");
    status = fortsym_int(arena, 2, &replacement, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_substitute(arena, product, x, replacement, &substituted, message,
                                sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(substituted, "y*(1 + 2)");
    substitution_old[0] = x;
    substitution_old[1] = y;
    substitution_new[0] = y;
    substitution_new[1] = x;
    status = fortsym_substitute_many(
        arena, product, substitution_old, substitution_new, 2,
        &simultaneous, message, sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(simultaneous, "x*(y + 1)");
    fortsym_expr_free(simultaneous);
    simultaneous = NULL;
    status = fortsym_substitute_many(
        arena, product, NULL, NULL, 0, &simultaneous, message,
        sizeof message);
    assert(status == FORTSYM_OK);
    expect_text(simultaneous, "y*(x + 1)");

    status = fortsym_arena_new(&other_arena, message, sizeof message);
    assert(status == FORTSYM_OK);
    status = fortsym_add(other_arena, x, one, &foreign, message, sizeof message);
    assert(status == FORTSYM_FOREIGN_ARENA);
    assert(foreign == NULL);
    fortsym_arena_free(other_arena);

    status = fortsym_expr_equal(x, x, &equal, message, sizeof message);
    assert(status == FORTSYM_OK && equal == 1);
    status = fortsym_expr_argument(product, 99, &foreign, message,
                                   sizeof message);
    assert(status == FORTSYM_INVALID_ARGUMENT);
    assert(strstr(message, "invalid") != NULL);

    /* Expression ownership keeps the arena valid after its root is released. */
    fortsym_arena_free(arena);
    expect_text(product, "y*(x + 1)");

    fortsym_expr_free(foreign);
    fortsym_expr_free(assumed);
    fortsym_expr_free(relation);
    fortsym_expr_free(sine);
    fortsym_expr_free(argument);
    fortsym_expr_free(modulus);
    fortsym_expr_free(complex_expanded);
    fortsym_expr_free(expanded_undefined);
    fortsym_expr_free(expanded_complex_infinity);
    fortsym_expr_free(expanded_infinity);
    fortsym_expr_free(sentinel_arg);
    fortsym_expr_free(negative_infinity);
    fortsym_expr_free(sentinel_conjugate);
    fortsym_expr_free(sentinel_abs);
    fortsym_expr_free(sentinel_im);
    fortsym_expr_free(sentinel_re);
    fortsym_expr_free(sentinel_nan_re);
    fortsym_expr_free(negative_phase_result);
    fortsym_expr_free(negative_phase_power);
    fortsym_expr_free(two_thirds);
    fortsym_expr_free(periodic_simplified);
    fortsym_expr_free(periodic);
    fortsym_expr_free(special_simplified);
    fortsym_expr_free(special);
    fortsym_expr_free(exp_log_zero_simplified);
    fortsym_expr_free(exp_log_zero);
    fortsym_expr_free(log_zero_simplified);
    fortsym_expr_free(log_zero);
    fortsym_expr_free(negative_log_simplified);
    fortsym_expr_free(negative_log);
    fortsym_expr_free(negative_two);
    fortsym_expr_free(negative_four);
    fortsym_expr_free(half);
    fortsym_expr_free(sqrt_three);
    fortsym_expr_free(three);
    fortsym_expr_free(five);
    fortsym_expr_free(twenty_one);
    fortsym_expr_free(negative_imaginary);
    fortsym_expr_free(pole_simplified);
    fortsym_expr_free(pole);
    fortsym_expr_free(branch_simplified);
    fortsym_expr_free(branch);
    fortsym_expr_free(legendre_simplified);
    fortsym_expr_free(legendre);
    fortsym_expr_free(undefined);
    fortsym_expr_free(complex_infinity);
    fortsym_expr_free(infinity);
    fortsym_expr_free(zero);
    fortsym_expr_free(minus_one);
    fortsym_expr_free(conjugate_sum);
    fortsym_expr_free(conjugated);
    fortsym_expr_free(imaginary_part);
    fortsym_expr_free(real_part);
    fortsym_expr_free(imaginary);
    fortsym_expr_free(unknown_head);
    fortsym_expr_free(seven);
    fortsym_expr_free(zero_expression);
    fortsym_expr_free(root);
    fortsym_expr_free(square);
    fortsym_expr_free(factored);
    fortsym_expr_free(expanded_input);
    fortsym_expr_free(powered);
    fortsym_expr_free(power_identity);
    fortsym_expr_free(quotient);
    fortsym_expr_free(quotient_den);
    fortsym_expr_free(quotient_num);
    fortsym_expr_free(two);
    fortsym_expr_free(substituted);
    fortsym_expr_free(simultaneous);
    fortsym_expr_free(sum);
    fortsym_expr_free(replacement);
    fortsym_expr_free(derivative);
    fortsym_expr_free(product);
    fortsym_expr_free(one);
    fortsym_expr_free(y);
    fortsym_expr_free(x);
    puts("test_fortsym_capi: all checks passed");
    return 0;
}
