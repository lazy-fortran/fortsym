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
    FORTSYM_RESOURCE_LIMIT = 6
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
    FORTSYM_BIG_REAL = 12
};

enum fortsym_assumption_fact {
    FORTSYM_FACT_REAL = 1,
    FORTSYM_FACT_POSITIVE = 2,
    FORTSYM_FACT_NONNEGATIVE = 4,
    FORTSYM_FACT_NONZERO = 8
};

enum fortsym_verdict {
    FORTSYM_ZERO_UNKNOWN = 0,
    FORTSYM_ZERO_TRUE = 1,
    FORTSYM_ZERO_FALSE = 2
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

int fortsym_substitute(fortsym_arena *arena, const fortsym_expr *expression,
                       const fortsym_expr *old_expression,
                       const fortsym_expr *new_expression, fortsym_expr **out,
                       char *message, size_t capacity);
int fortsym_differentiate(fortsym_arena *arena, const fortsym_expr *expression,
                          const fortsym_expr *variable, fortsym_expr **out,
                          char *message, size_t capacity);
int fortsym_expand(fortsym_arena *arena, const fortsym_expr *expression,
                   fortsym_expr **out, char *message, size_t capacity);
int fortsym_simplify(fortsym_arena *arena, const fortsym_expr *expression,
                     fortsym_expr **out, char *message, size_t capacity);
int fortsym_factor(fortsym_arena *arena, const fortsym_expr *expression,
                   fortsym_expr **out, char *message, size_t capacity);
int fortsym_zero_test(fortsym_arena *arena, const fortsym_expr *expression,
                      int *verdict, char *message, size_t capacity);

void fortsym_expr_free(fortsym_expr *expression);
int fortsym_expr_kind(const fortsym_expr *expression, int *kind,
                      char *message, size_t capacity);
int fortsym_expr_arity(const fortsym_expr *expression, size_t *arity,
                       char *message, size_t capacity);
int fortsym_expr_argument(const fortsym_expr *expression, size_t index,
                          fortsym_expr **out, char *message, size_t capacity);
int fortsym_expr_equal(const fortsym_expr *left, const fortsym_expr *right,
                       int *equal, char *message, size_t capacity);
int fortsym_expr_node_count(const fortsym_expr *expression, size_t *count,
                            char *message, size_t capacity);
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
