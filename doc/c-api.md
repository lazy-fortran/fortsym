# C ABI

`src/capi/fortsym.h` is the public C contract (ABI version 26). It exposes opaque arena and
expression handles, exact scalar constructors, function application, arithmetic,
inspection, substitution, differentiation, and the first fixed-three-dimensional
chart, tensor, connection, and differential-form views. Chart calls include
signed Jacobian, covariant/reciprocal basis transport, tensor slot
raise/lower, density metadata changes, oriented metric volume forms, and
chart-owned vector calculus, and bidirectional chart-map tensor and form transport.
The form boundary also accepts the explicit degree-four zero extension produced
by `d` of a three-form; any degree-four input with a nonzero component is
rejected.
The native library retains an
arena while any expression handle refers to it; callers may therefore release
the arena before releasing its expressions.

`fortsym_zero_test` exposes the native three-valued query without creating a
new expression handle. It returns `FORTSYM_ZERO_TRUE` for a proved zero,
`FORTSYM_ZERO_FALSE` for a proved nonzero expression, and
`FORTSYM_ZERO_UNKNOWN` when the native decision procedure declines to decide.
An unknown verdict is still a successful C-ABI call; malformed handles and
unsupported execution return the ordinary status codes.

`fortsym_expr_is_number` reports SymPy-compatible numeric-expression status:
numeric atoms, named constants, domain sentinels, and compound expressions
whose children are all numeric return one; symbolic expressions and Boolean
relations return zero. The query is owned by `fortsym_predicates:is_number` in
the native predicate module and does not duplicate classification in the ABI
or Python adapter.

`fortsym_expr_is_algebraic` reports the exact-domain query using the same
three-valued `enum fortsym_verdict` as `fortsym_zero_test`: exact integers,
rationals, FLINT algebraic atoms, and supported exact algebraic compounds
return `FORTSYM_ZERO_TRUE`; proven transcendental values return
`FORTSYM_ZERO_FALSE`; and unresolved values return `FORTSYM_ZERO_UNKNOWN`.
The classification is owned by `fortsym_predicates:is_algebraic` and is not
reimplemented in the ABI or Python adapter. `FORTSYM_FACT_ALGEBRAIC` is also
accepted by the assumption APIs and closes over the native exact-domain facts.

`fortsym_expr_operation_count` counts operation occurrences in the expression
tree using SymPy's `count_ops` semantics: n-ary sums and products count one
operation per operand link, and shared native nodes count once per tree
occurrence. Canonical reciprocal products are counted as divisions. It is
distinct from `fortsym_expr_node_count`, which counts shared DAG nodes.

`fortsym_expr_free_symbols` writes the distinct free symbol names as
`name\0name\0...\0`; `required` includes the final NUL. Constants and applied
function heads are not returned. The order is an implementation detail of the
native traversal; callers should treat the result as a set.

`fortsym_substitute_many` applies paired old/new expressions simultaneously;
replacement expressions are not revisited, so swaps do not cascade. The
`count` argument may be zero and all non-empty arrays must refer to the same
arena as the root expression.

`fortsym_assumption_has` reports whether the arena proves one of the supported
facts (`real`, `zero`, `negative`, `nonpositive`, `positive`, `nonnegative`,
`nonzero`, or `algebraic`) for an expression. Its `known` output is one for a proven fact and
zero for an unknown fact; absence is not a proof of the opposite predicate.

`FORTSYM_CONFLICT` reports contradictory assumptions. Compound relation
ingestion is transactional: a failed child does not leave facts from earlier
children in the arena, and the diagnostic buffer contains the conflict reason.

`fortsym_assumption_push` and `fortsym_assumption_pop` provide nested,
exception-safe scopes over the arena's assumption context. Push clones the
current facts; assumptions recorded after the push are discarded by the
matching pop. A pop without a matching push returns
`FORTSYM_INVALID_ARGUMENT`. Expression handles remain valid across both
operations.

`fortsym_complex_operation` exposes the supported native complex-domain
operations `re`, `im`, `abs`, `conjugate`, `arg`, and `expand_complex`. It returns a new expression when
the expression's reality, branch, and singularity conditions are decidable;
otherwise it returns `FORTSYM_UNSUPPORTED` with the native refusal reason.

Every fallible operation returns a status and accepts a caller-owned diagnostic
buffer. No process-global error state is used. Text accessors report the
required buffer size, including the terminating NUL, and return a resource
status when the supplied buffer is too small.

Operations whose native implementation is not yet available are intentionally
absent from this ABI. They will be added as versioned API additions when their
native semantics and independent tests exist.
