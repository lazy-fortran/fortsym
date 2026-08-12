# C ABI

`src/capi/fortsym.h` is the public C contract (ABI version 42). It exposes opaque arena and
expression handles, exact scalar constructors, function application, arithmetic,
inspection, substitution, differentiation, and the first fixed-three-dimensional
chart, tensor, connection, and differential-form views. Chart calls include
signed Jacobian, covariant/reciprocal basis transport, tensor slot
raise/lower, density metadata changes, oriented metric volume forms, and
chart-owned vector calculus, and bidirectional chart-map tensor and form transport.
The chart calculus distinguishes ordinary vectors from weight-one vector densities:
`fortsym_chart_curl_density` returns the metric-free alternating derivative of a
covector, while `fortsym_chart_div_density` differentiates a contravariant density
componentwise. The existing `fortsym_chart_curl` and `fortsym_chart_divergence`
remain the ordinary vector operations.
`fortsym_chart_tensor_permute` reorders typed tensor slots, and
`fortsym_chart_tensor_symmetrize` projects two slots to their symmetric or
antisymmetric part while preserving variance and density metadata.
`fortsym_chart_tensor_contract` contracts two one-based opposite-variance
slots and returns the remaining native tensor components; the Python facade
adds named-index validation before calling this same operation.
`fortsym_chart_tensor_product` forms a tensor product with left slots followed
by right slots and preserves the summed density weight.
`fortsym_chart_b_density` exposes the native `sqrtg B^i` magnetic view alongside
`fortsym_chart_b_cov`; both consume the same native magnetic owner.
The form boundary also accepts the explicit degree-four zero extension produced
by `d` of a three-form; any degree-four input with a nonzero component is
rejected.
Chart operations reject maps whose forward or inverse Jacobian is identically
zero, while a determinant that can vanish only on a coordinate locus remains a
conditional map and is not silently treated as globally singular.
`fortsym_chart_j_fourier` exposes the generic fixed-3D Fourier curl-curl
operator `J = curl(nu curl(A))`; its 3x3 reluctivity is passed in
first-slot-fastest (column-major) order and the symmetry derivative is `i*n`.
`fortsym_chart_fourier_weak_form` adds the paper's native mode descriptor and
coefficient blocks. It accepts an integer mode, returns the longitudinal
nodal or transverse edge branch, and returns the scalar coefficient, curl
coefficient, and 2x2 transverse mass block as native expression handles.
The metric owner is also available through `fortsym_metric_sqrtg`,
`fortsym_metric_volume_density`, `fortsym_metric_levi_civita`, and
`fortsym_metric_contravariant`; `fortsym_chart_form_star_metric` applies the
same native Hodge owner to an explicitly supplied signature and orientation.
The Levi-Civita call takes variance `-1` or `+1` and returns 27 components in
first-slot-fastest order; the volume-density call is always positive and does
not absorb metric orientation.
The dimension-aware relativity owner is transported by the
`fortsym_spacetime_*` calls for inverse metric, Christoffel, Riemann, Ricci,
scalar curvature, and Einstein tensors. The spacetime form calls add native
degree-aware exterior derivative, wedge, metric Hodge-star, and
codifferential transport over the same four-coordinate owner. The geodesic
residual call substitutes an explicit parameterized curve into the native
Christoffel owner before assembling `x''^a + Gamma^a_bc x'^b x'^c`. The
spacetime form boundary also transports contraction, the Cartan Lie derivative,
and the Laplace--de Rham composition.
The Maxwell form calls add native `F=d(A)`, gauge transformation
`A -> A + d(chi)`, and the source residual `d(*F)-J`, with potentials and
currents represented as degree-one and degree-three spacetime forms.
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
