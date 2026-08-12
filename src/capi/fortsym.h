#ifndef FORTSYM_H
#define FORTSYM_H

/* Stable, ownership-safe C surface for the native fortsym expression graph.
 *
 * Handles are opaque. An expression keeps its arena alive, so callers may
 * release the arena before releasing its expressions. Every operation returns
 * its status and writes an optional diagnostic into the caller's buffer; there
 * is no process-global error slot.
 */

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct fortsym_arena fortsym_arena;
typedef struct fortsym_expr fortsym_expr;

enum fortsym_status {
    FORTSYM_OK = 0,
    FORTSYM_INVALID_ARGUMENT = 1,
    FORTSYM_INVALID_HANDLE = 2,
    FORTSYM_FOREIGN_ARENA = 3,
    FORTSYM_PARSE_ERROR = 4,
    FORTSYM_UNSUPPORTED = 5,
    FORTSYM_RESOURCE_LIMIT = 6,
    FORTSYM_CONFLICT = 7
};

enum fortsym_node_kind {
    FORTSYM_INT = 1,
    FORTSYM_RAT = 2,
    FORTSYM_REAL = 3,
    FORTSYM_SYMBOL = 4,
    FORTSYM_CONSTANT = 5,
    FORTSYM_ADD = 6,
    FORTSYM_MUL = 7,
    FORTSYM_POW = 8,
    FORTSYM_FUNCTION = 9,
    FORTSYM_BIG_INT = 10,
    FORTSYM_BIG_RAT = 11,
    FORTSYM_BIG_REAL = 12,
    FORTSYM_ALGEBRAIC = 13
};

enum fortsym_assumption_fact {
    FORTSYM_FACT_REAL = 1,
    FORTSYM_FACT_POSITIVE = 2,
    FORTSYM_FACT_NONNEGATIVE = 4,
    FORTSYM_FACT_NONZERO = 8,
    FORTSYM_FACT_INTEGER = 16,
    FORTSYM_FACT_RATIONAL = 512,
    FORTSYM_FACT_ALGEBRAIC = 1024,
    FORTSYM_FACT_ZERO = 64,
    FORTSYM_FACT_NEGATIVE = 128,
    FORTSYM_FACT_NONPOSITIVE = 256
};

enum fortsym_relation_kind {
    FORTSYM_RELATION_EQUAL = 1,
    FORTSYM_RELATION_UNEQUAL = 2,
    FORTSYM_RELATION_LESS = 3,
    FORTSYM_RELATION_LESS_EQUAL = 4,
    FORTSYM_RELATION_GREATER = 5,
    FORTSYM_RELATION_GREATER_EQUAL = 6
};

enum fortsym_verdict {
    FORTSYM_ZERO_UNKNOWN = 0,
    FORTSYM_ZERO_TRUE = 1,
    FORTSYM_ZERO_FALSE = 2
};

enum fortsym_tensor_variance {
    FORTSYM_LOWER_VARIANCE = -1,
    FORTSYM_UPPER_VARIANCE = 1
};

enum {
    FORTSYM_GEOMETRY_DIMENSION = 3,
    FORTSYM_TENSOR_MAX_RANK = 4,
    FORTSYM_FORM_COMPONENTS = 8,
    FORTSYM_FORM_MAX_DEGREE = 4
};

int fortsym_abi_version(void);

int fortsym_arena_new(fortsym_arena **out, char *message, size_t capacity);
void fortsym_arena_free(fortsym_arena *arena);

int fortsym_int(fortsym_arena *arena, int64_t value, fortsym_expr **out,
                char *message, size_t capacity);
int fortsym_rational(fortsym_arena *arena, int64_t numerator,
                     int64_t denominator, fortsym_expr **out, char *message,
                     size_t capacity);
int fortsym_real(fortsym_arena *arena, double value, fortsym_expr **out,
                 char *message, size_t capacity);
int fortsym_exact(fortsym_arena *arena, const char *value, fortsym_expr **out,
                  char *message, size_t capacity);
int fortsym_symbol(fortsym_arena *arena, const char *name, fortsym_expr **out,
                   char *message, size_t capacity);
int fortsym_assume(fortsym_arena *arena, const fortsym_expr *expression,
                   int fact, char *message, size_t capacity);
int fortsym_assume_relation(fortsym_arena *arena,
                            const fortsym_expr *relation, char *message,
                            size_t capacity);
int fortsym_assumption_push(fortsym_arena *arena, char *message,
                            size_t capacity);
int fortsym_assumption_pop(fortsym_arena *arena, char *message,
                           size_t capacity);
/* `known` is one when the arena proves the fact and zero when it is unknown. */
int fortsym_assumption_has(fortsym_arena *arena,
                           const fortsym_expr *expression, int fact,
                           int *known, char *message, size_t capacity);
int fortsym_constant(fortsym_arena *arena, const char *name,
                     fortsym_expr **out, char *message, size_t capacity);
int fortsym_add(fortsym_arena *arena, const fortsym_expr *left,
                const fortsym_expr *right, fortsym_expr **out, char *message,
                size_t capacity);
int fortsym_subtract(fortsym_arena *arena, const fortsym_expr *left,
                     const fortsym_expr *right, fortsym_expr **out,
                     char *message, size_t capacity);
int fortsym_multiply(fortsym_arena *arena, const fortsym_expr *left,
                     const fortsym_expr *right, fortsym_expr **out,
                     char *message, size_t capacity);
int fortsym_divide(fortsym_arena *arena, const fortsym_expr *left,
                   const fortsym_expr *right, fortsym_expr **out, char *message,
                   size_t capacity);
int fortsym_power(fortsym_arena *arena, const fortsym_expr *base,
                  const fortsym_expr *exponent, fortsym_expr **out,
                  char *message, size_t capacity);
int fortsym_add_many(fortsym_arena *arena, const fortsym_expr *const args[],
                     size_t count, fortsym_expr **out, char *message,
                     size_t capacity);
int fortsym_function(fortsym_arena *arena, const char *name,
                     const fortsym_expr *const args[], size_t count,
                     fortsym_expr **out, char *message, size_t capacity);
int fortsym_relation(fortsym_arena *arena, const fortsym_expr *left,
                     const fortsym_expr *right, int relation_kind,
                     fortsym_expr **out, char *message, size_t capacity);

int fortsym_substitute(fortsym_arena *arena, const fortsym_expr *expression,
                       const fortsym_expr *old_expression,
                       const fortsym_expr *new_expression, fortsym_expr **out,
                       char *message, size_t capacity);
/* Replacement expressions are not revisited, so the substitutions are
 * simultaneous and do not cascade. `count` may be zero. */
int fortsym_substitute_many(
    fortsym_arena *arena, const fortsym_expr *expression,
    const fortsym_expr *const old_expressions[],
    const fortsym_expr *const new_expressions[], size_t count,
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_differentiate(fortsym_arena *arena, const fortsym_expr *expression,
                          const fortsym_expr *variable, fortsym_expr **out,
                          char *message, size_t capacity);
int fortsym_expand(fortsym_arena *arena, const fortsym_expr *expression,
                   fortsym_expr **out, char *message, size_t capacity);
int fortsym_simplify(fortsym_arena *arena, const fortsym_expr *expression,
                     fortsym_expr **out, char *message, size_t capacity);
int fortsym_factor(fortsym_arena *arena, const fortsym_expr *expression,
                   fortsym_expr **out, char *message, size_t capacity);
/* Coordinate and magnetic operations use three expressions for both the
 * coordinate symbols and their Cartesian position map. The dimension
 * argument is retained in the ABI so callers can validate the fixed native
 * chart dimension explicitly. Each result array has owned expression handles
 * and must be released element by element. */
int fortsym_chart_sqrtg(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], size_t dimension, fortsym_expr **out,
    char *message, size_t capacity);
int fortsym_chart_jacobian(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], size_t dimension, fortsym_expr **out,
    char *message, size_t capacity);
/* Basis matrices use component-first, basis-second ordering. The nine output
 * handles represent e(component, index), with the component index varying
 * fastest. */
int fortsym_chart_covariant_basis(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_reciprocal_basis(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_grad(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *scalar,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_divergence(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_chart_div_density(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_chart_curl(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *covector[],
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_curl_density(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *covector[],
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_laplacian(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *scalar,
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_chart_map_jacobian(
    fortsym_arena *arena, const fortsym_expr *source_coordinates[],
    const fortsym_expr *source_position[], const fortsym_expr *target_coordinates[],
    const fortsym_expr *target_position[], const fortsym_expr *forward[],
    const fortsym_expr *inverse[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_map_inverse_jacobian(
    fortsym_arena *arena, const fortsym_expr *source_coordinates[],
    const fortsym_expr *source_position[], const fortsym_expr *target_coordinates[],
    const fortsym_expr *target_position[], const fortsym_expr *forward[],
    const fortsym_expr *inverse[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_map_tensor(
    fortsym_arena *arena, const fortsym_expr *source_coordinates[],
    const fortsym_expr *source_position[], const fortsym_expr *target_coordinates[],
    const fortsym_expr *target_position[], const fortsym_expr *forward[],
    const fortsym_expr *inverse[], const fortsym_expr *components[], size_t rank,
    const int variance[], int density_weight, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_map_form(
    fortsym_arena *arena, const fortsym_expr *source_coordinates[],
    const fortsym_expr *source_position[], const fortsym_expr *target_coordinates[],
    const fortsym_expr *target_position[], const fortsym_expr *forward[],
    const fortsym_expr *inverse[], const fortsym_expr *components[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_map_compose(
    fortsym_arena *arena, const fortsym_expr *source_coordinates[],
    const fortsym_expr *source_position[], const fortsym_expr *middle_coordinates[],
    const fortsym_expr *middle_position[], const fortsym_expr *target_coordinates[],
    const fortsym_expr *target_position[], const fortsym_expr *first_forward[],
    const fortsym_expr *first_inverse[], const fortsym_expr *following_forward[],
    const fortsym_expr *following_inverse[], fortsym_expr *forward_out[],
    fortsym_expr *inverse_out[], char *message, size_t capacity);
int fortsym_chart_b_cov(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_b_density(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_b_fourier(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *potential[],
    const fortsym_expr *mode, fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_b_fourier_density(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *potential[],
    const fortsym_expr *mode, fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_j_fourier(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *reluctivity[],
    const fortsym_expr *potential[], const fortsym_expr *mode,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_fourier_weak_form(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *reluctivity[], int mode,
    int *branch, int *trial_space, int *test_space, int *trial_components,
    int *test_components, int *boundary_trace, fortsym_expr **scalar_coefficient,
    fortsym_expr **transverse_curl_coefficient, fortsym_expr *transverse_mass[],
    char *message, size_t capacity);
int fortsym_chart_current_compatibility(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *current[], int mode,
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_metric_sqrtg(
    fortsym_arena *arena, const fortsym_expr *components[],
    const int signature[], int orientation, fortsym_expr **out,
    char *message, size_t capacity);
int fortsym_metric_volume_density(
    fortsym_arena *arena, const fortsym_expr *components[],
    const int signature[], int orientation, fortsym_expr **out,
    char *message, size_t capacity);
/* `variance` is -1 for the covariant and +1 for the contravariant
 * Levi-Civita tensor. Output uses first-slot-fastest order and has 27 slots. */
int fortsym_metric_levi_civita(
    fortsym_arena *arena, const fortsym_expr *components[],
    const int signature[], int orientation, int variance, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_metric_contravariant(
    fortsym_arena *arena, const fortsym_expr *components[],
    const int signature[], int orientation, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_form_star_metric(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *components[],
    const int signature[], int orientation, const fortsym_expr *input[],
    size_t degree, fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_metric_sqrtg(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_spacetime_metric_contravariant(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_christoffel(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_riemann(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_ricci(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_scalar_curvature(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr **out, char *message, size_t capacity);
int fortsym_spacetime_einstein(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_geodesic_residual(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *curve[], const fortsym_expr *parameter,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_form_d(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *input[], size_t degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_spacetime_form_wedge(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *left[], size_t left_degree,
    const fortsym_expr *right[], size_t right_degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_spacetime_form_star(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *input[], size_t degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_spacetime_form_codifferential(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *input[], size_t degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_spacetime_form_interior(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *vector[], const fortsym_expr *input[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_form_lie(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *vector[], const fortsym_expr *input[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_spacetime_form_laplace_de_rham(
    fortsym_arena *arena, const fortsym_expr *components[], int dimension,
    const fortsym_expr *coordinates[], const int signature[], int orientation,
    const fortsym_expr *input[], size_t degree, fortsym_expr *out[],
    char *message, size_t capacity);
/* Geometry tensor arrays use first-slot-fastest order, matching the native
 * `tensor_component` convention. The caller supplies an output array with
 * `3**rank` slots and releases each returned handle with fortsym_expr_free. */
int fortsym_chart_metric_covariant(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_metric_contravariant(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_christoffel(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_covariant_diff(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *components[],
    size_t rank, const int variance[], int density_weight,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_tensor_raise(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *components[],
    size_t rank, const int variance[], int density_weight, size_t slot,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_tensor_lower(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *components[],
    size_t rank, const int variance[], int density_weight, size_t slot,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_tensor_density(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *components[],
    size_t rank, const int variance[], int density_weight,
    int new_density_weight, fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_riemann(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_ricci(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_scalar_curvature(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr **out, char *message,
    size_t capacity);
int fortsym_chart_einstein(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], fortsym_expr *out[], char *message,
    size_t capacity);
/* Differential-form arrays contain all eight basis masks. Mask k is the
 * coefficient of the ordered wedge basis represented by the bit mask k.
 * Unused masks for a form degree are zero. Returned handles are owned by the
 * caller and must be released element by element. */
int fortsym_chart_form_add(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *left[], size_t left_degree,
    const fortsym_expr *right[], size_t right_degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_form_subtract(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *left[], size_t left_degree,
    const fortsym_expr *right[], size_t right_degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_form_negate(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *input[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_form_scale(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *input[], size_t degree,
    const fortsym_expr *factor, fortsym_expr *out[], char *message,
    size_t capacity);
int fortsym_chart_form_d(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *input[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_form_wedge(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *left[], size_t left_degree,
    const fortsym_expr *right[], size_t right_degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_form_star(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *input[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_form_interior(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    const fortsym_expr *input[], size_t degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_form_lie(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    const fortsym_expr *input[], size_t degree, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_chart_form_flat(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *vector[],
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_form_sharp(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], const fortsym_expr *input[], size_t degree,
    fortsym_expr *out[], char *message, size_t capacity);
int fortsym_chart_form_volume(
    fortsym_arena *arena, const fortsym_expr *coordinates[],
    const fortsym_expr *position[], int orientation, fortsym_expr *out[],
    char *message, size_t capacity);
int fortsym_zero_test(fortsym_arena *arena, const fortsym_expr *expression,
                      int *verdict, char *message, size_t capacity);
/* operation is one of "re", "im", "conjugate", "arg", "abs", or
 * "expand_complex". */
int fortsym_complex_operation(fortsym_arena *arena,
                              const fortsym_expr *expression,
                              const char *operation, fortsym_expr **out,
                              char *message, size_t capacity);

void fortsym_expr_free(fortsym_expr *expression);
int fortsym_expr_kind(const fortsym_expr *expression, int *kind,
                      char *message, size_t capacity);
/* `number` is one when the expression is numeric in SymPy's sense, including
 * numeric-only compound expressions, and zero for symbolic or Boolean ones. */
int fortsym_expr_is_number(const fortsym_expr *expression, int *number,
                           char *message, size_t capacity);
/* `verdict` uses enum fortsym_verdict: FORTSYM_ZERO_TRUE, FALSE, or UNKNOWN. */
int fortsym_expr_is_algebraic(const fortsym_expr *expression, int *verdict,
                              char *message, size_t capacity);
int fortsym_expr_arity(const fortsym_expr *expression, size_t *arity,
                       char *message, size_t capacity);
int fortsym_expr_argument(const fortsym_expr *expression, size_t index,
                          fortsym_expr **out, char *message, size_t capacity);
int fortsym_expr_equal(const fortsym_expr *left, const fortsym_expr *right,
                       int *equal, char *message, size_t capacity);
int fortsym_expr_node_count(const fortsym_expr *expression, size_t *count,
                            char *message, size_t capacity);
int fortsym_expr_operation_count(const fortsym_expr *expression, size_t *count,
                                 char *message, size_t capacity);
/* The output is name\0name\0...\0; `required` includes the final NUL. */
int fortsym_expr_free_symbols(const fortsym_expr *expression, char *buffer,
                              size_t capacity, size_t *required,
                              char *message, size_t message_capacity);
int fortsym_expr_text(const fortsym_expr *expression, char *buffer,
                      size_t capacity, size_t *required, char *message,
                      size_t message_capacity);
int fortsym_expr_name(const fortsym_expr *expression, char *buffer,
                      size_t capacity, size_t *required, char *message,
                      size_t message_capacity);
int fortsym_expr_exact_text(const fortsym_expr *expression, char *buffer,
                            size_t capacity, size_t *required, char *message,
                            size_t message_capacity);
int fortsym_expr_int_value(const fortsym_expr *expression, int64_t *value,
                           char *message, size_t capacity);
int fortsym_expr_real_value(const fortsym_expr *expression, double *value,
                            char *message, size_t capacity);

#ifdef __cplusplus
}
#endif

#endif /* FORTSYM_H */
