# C ABI

`src/capi/fortsym.h` is the public C contract (ABI version 96). It exposes opaque arena and
expression handles, exact scalar constructors, function application, arithmetic,
inspection, substitution, differentiation, and the first fixed-three-dimensional
chart, tensor, connection, and differential-form views. Chart calls include
signed Jacobian, covariant/reciprocal basis transport, tensor slot
raise/lower, density metadata changes, oriented metric volume forms, and
chart-owned vector calculus, and bidirectional chart-map tensor and form transport.
The scalar algebra surface also includes exact bounded `fortsym_together`,
`fortsym_cancel`, `fortsym_apart`, and `fortsym_collect`, plus verified
one-variable indefinite `fortsym_integrate`, and verified bounded finite/infinite
`fortsym_limit`, and bounded `fortsym_series`/`fortsym_series_coeff`;
verified exact-rational one-right-hand-side `fortsym_linsolve` and
`fortsym_linsolve_parametric`, and bounded dense
matrix determinant `fortsym_matrix_det`, and exact matrix rank
`fortsym_matrix_rank`, exact matrix inverse `fortsym_matrix_inverse`, matrix
transpose `fortsym_matrix_transpose`, and bounded exact nullspace
`fortsym_matrix_nullspace`, and bounded exact RREF
`fortsym_matrix_rref`, bounded exact dense multiplication or scalar scaling
`fortsym_matrix_multiply`, elementwise addition/subtraction
`fortsym_matrix_add`/`fortsym_matrix_subtract`, negation
`fortsym_matrix_negate`, scalar division `fortsym_matrix_divide`, and
tri-state diagonal and zero-matrix predicates
`fortsym_matrix_is_diagonal`/`fortsym_matrix_is_zero_matrix`, and boolean
triangular predicates `fortsym_matrix_is_upper`/`fortsym_matrix_is_lower`;
the three-valued antisymmetry predicate `fortsym_matrix_is_anti_symmetric`,
and boolean symbolic-entry predicate `fortsym_matrix_is_symbolic`;
unsupported options, unverified antiderivatives, and limits or series outside
their theorem fragments return `FORTSYM_UNSUPPORTED` with a diagnostic.
`fortsym_solve` returns a caller-owned array of distinct verified roots for one
equation in one symbol. Exact univariate polynomials and verified scalar-linear
equations are supported; the caller supplies output capacity and receives the
root count. Unsupported domains and options return `FORTSYM_UNSUPPORTED`, and
a too-small output array returns `FORTSYM_RESOURCE_LIMIT` without producing
handles.
`fortsym_solveset` uses the same equation input and returns two caller-owned
arrays: distinct verified roots and verified denominator poles for the bounded
rational fragment. The Python facade represents a nontrivial pair as
`Complement`; `fortsym_solve` retains its list-oriented contract. Both output
capacities are checked before any handle is returned.
`fortsym_linsolve_parametric` accepts a column-major rectangular exact-rational
coefficient matrix, one right-hand side, and explicit variable handles. It
returns one value per variable for a consistent system, retaining supplied
free variables, or a successful zero-count result for an inconsistent system.
The lower-level square `fortsym_linsolve` contract remains unchanged.
The chart calculus distinguishes ordinary vectors from weight-one vector densities:
`fortsym_chart_curl_density` returns the metric-free alternating derivative of a
covector, while `fortsym_chart_div_density` differentiates a contravariant density
componentwise. The existing `fortsym_chart_curl` and `fortsym_chart_divergence`
remain the ordinary vector operations.
`fortsym_chart_field_line_derivative` returns the directional derivative
`B^i partial_i f` for any contravariant coordinate vector and scalar.
`fortsym_chart_surface_measure` returns the positive induced two-dimensional
measure on a coordinate surface `u[normal_index] = constant`; the explicit
metric owner provides the corresponding `fortsym_metric_surface_measure`.
`fortsym_chart_flux_surface_average` owns the matching coordinate-surface
metadata and averages a scalar over both remaining coordinates on
`[0,2*pi]`. It returns `FORTSYM_UNSUPPORTED` when either the weighted
numerator or normalization falls outside the verified native definite-integral
subset; it never substitutes an unverified numerical result.
`fortsym_chart_flux_normal_residual` returns `B^label`,
`fortsym_chart_straight_field_line_residual` returns
`B^angle_one - iota*B^angle_two`, and
`fortsym_chart_boozer_residuals` returns the five native expressions
`(B_label, d1 B1, d2 B1, d1 B2, d2 B2)` for a selected label. These calls
share the `fortsym_flux` owner and expose residuals for an independent zero
oracle; they do not construct an equilibrium.
`fortsym_chart_hamada_residuals` has the same residual layout for contravariant
components and checks the Hamada contract that the two angular components are
flux functions. The Boozer operation requires covariant components; Hamada
requires contravariant components.
`fortsym_chart_tensor_density` changes only density metadata. The companion
`fortsym_chart_tensor_density_factor` multiplies every component by one
arena-owned scalar factor and increments the density weight, so it is the
native C spelling of `sqrtg B^i` when the factor is `sqrtg`.
`fortsym_chart_tensor_permute` reorders typed tensor slots, and
`fortsym_chart_tensor_symmetrize` projects two slots to their symmetric or
antisymmetric part while preserving variance and density metadata.
`fortsym_chart_tensor_declare_symmetry` checks and records one same-variance
pair declaration without changing components. A false declaration is refused.
`fortsym_chart_tensor_contract` contracts two one-based opposite-variance
slots and returns the remaining native tensor components; the Python facade
adds named-index validation before calling this same operation.
`fortsym_chart_tensor_product` forms a tensor product with left slots followed
by right slots and preserves the summed density weight.
`fortsym_chart_tensor_lie` computes the coordinate Lie derivative along a
weight-zero contravariant vector tensor. It preserves the input tensor's
variance and density weight; density weight `w` contributes the explicit
`+w*T*partial_i(X^i)` term.
The supplied-connection operations accept `Gamma^a_bc` in the same
first-slot-fastest order: `fortsym_chart_connection_torsion` returns
`T^a_bc = Gamma^a_bc - Gamma^a_cb`,
`fortsym_chart_connection_nonmetricity` returns
`Q_ijk = -nabla_k g_ij`, and the two covariant operations use the supplied
coefficients for arbitrary typed tensors and preserve their density metadata.
`fortsym_chart_connection_riemann` returns the rank-four `R^a_bcd` tensor
computed directly from supplied coefficients and their coordinate tuple, so
curvature does not require a metric owner. Its `convention` argument accepts
`1` for the standard sign and `-1` for the exact opposite sign.
`fortsym_chart_covariant_divergence` contracts the first contravariant tensor
slot with the derivative slot and preserves the remaining slot metadata.
`fortsym_chart_first_bianchi_residual` returns the native rank-four residual
`R^a_bcd + R^a_cdb + R^a_dbc`, so callers can verify the torsion-free identity
with their own zero oracle.
`fortsym_chart_second_bianchi_residual` returns the native rank-five residual
`nabla_e R^a_bcd + nabla_c R^a_bde + nabla_d R^a_bec`.
`fortsym_chart_geodesic_residual` returns the three chart components of
`x''^a + Gamma^a_bc x'^b x'^c`, substituting the parameterized curve into the
chart connection before returning the residual.
`fortsym_chart_form_closed` and `fortsym_spacetime_form_closed` return the
three-valued zero verdict for every coefficient of `d(form)`.
`fortsym_chart_b_density` exposes the native `sqrtg B^i` magnetic view alongside
`fortsym_chart_b_cov`; both consume the same native magnetic owner.
`fortsym_chart_b_con_form` and `fortsym_chart_b_density_form` recover the
contravariant field and weight-one density from an eight-slot degree-two form
array. Both accept an explicit orientation (`+1` or `-1`) and return three
expression handles; the C ABI keeps tensor metadata in the higher-level
Fortran/Python owners.
`fortsym_chart_b_flux_form` is the forward companion: it accepts a three-slot
contravariant `B^i`, an orientation, and returns the eight-slot degree-two form
`beta = i_B(orientation*Omega)`. Its nonzero slots use the same mask order as
the generic form API, so the C call and the reverse bridges are exact views of
one native form owner.
`fortsym_chart_h_cov` applies `H_i = nu_ij B^j` to a 3x3 reluctivity and a
contravariant magnetic vector, while `fortsym_chart_h_con` raises the resulting
covector with the chart metric.
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
nodal or transverse edge branch, and returns the longitudinal diffusion,
transverse curl coefficient, and 2x2 transverse mass block as native
expression handles. `fortsym_chart_fourier_longitudinal_residual` and
`fortsym_chart_fourier_transverse_residual` transport the corresponding
strong residuals; the transverse mode is an expression handle.
`fortsym_chart_fourier_source` and `fortsym_chart_fourier_load` transport
two-slot source/load arrays with branch and component metadata. The zero mode
uses the first slot as a scalar longitudinal source or normal load; nonzero
modes use both slots for the transverse source or tangential load. The native
boundary contract does not absorb a surface measure or weak-form sign, and
invalid branch/trace combinations return `FORTSYM_INVALID_ARGUMENT`.
The metric owner is also available through `fortsym_metric_sqrtg`,
`fortsym_metric_volume_density`, `fortsym_metric_levi_civita`,
`fortsym_metric_contravariant`, `fortsym_metric_grad`,
`fortsym_metric_divergence`, and `fortsym_metric_laplacian`;
`fortsym_metric_inner` contracts two three-component contravariant vectors
with the supplied metric and is the shared native path for metric norms and
magnetic `B**2` calculations;
the last three take the explicit coordinate tuple and position map needed to
transport chart inputs while keeping the metric owner authoritative.
`fortsym_chart_form_star_metric` applies the
same native Hodge owner to an explicitly supplied signature and orientation.
The Levi-Civita call takes variance `-1` or `+1` and returns 27 components in
first-slot-fastest order; the volume-density call is always positive and does
not absorb metric orientation.
The dimension-aware relativity owner is transported by the
`fortsym_spacetime_*` calls for inverse metric, `flat`, `sharp`, metric
gradient, divergence, Laplace--Beltrami/wave operator, Christoffel, Riemann,
Ricci, scalar curvature, and Einstein tensors. The spacetime form calls add native
degree-aware exterior derivative, oriented top-degree volume forms, wedge, metric Hodge-star, and
codifferential transport over the same four-coordinate owner. The geodesic
residual call substitutes an explicit parameterized curve into the native
Christoffel owner before assembling `x''^a + Gamma^a_bc x'^b x'^c`. The
spacetime form boundary also transports contraction, the Cartan Lie derivative,
and the Laplace--de Rham composition.
The Maxwell form calls add native `F=d(A)`, gauge transformation
`A -> A + d(chi)`, and the source residual `d(*F)-J`, with potentials and
currents represented as degree-one and degree-three spacetime forms.
`fortsym_spacetime_form_volume` returns the oriented top-degree form
`Omega = sigma*sqrt(abs(det(g))) du^0^...^du^(n-1)` for runtime dimensions
one through four; the sign is explicit and remains separate from the positive
metric `sqrtg` scalar.
`fortsym_spacetime_form_from_tensor` and
`fortsym_spacetime_tensor_from_form` bridge the same runtime owners. The tensor
side must be an exact lower, weight-zero, fully antisymmetric tensor; refusal
is explicit for upper slots, density weights, repeated-index nonzeros, and
non-antisymmetric components. Tensor arrays use first-slot-fastest order, while
form arrays use the existing 16 mask slots.
Runtime spacetime tensors also expose metric covariant differentiation through
`fortsym_spacetime_tensor_covariant_diff`. It appends a lower derivative slot,
transports every declared upper or lower tensor slot, and preserves the
density weight. The operation accepts input ranks zero through four and
returns the fixed four-coordinate C representation with inactive components
zero. The runtime tensor ceiling is rank five.
`fortsym_spacetime_tensor_covariant_divergence` directly contracts the first
upper slot with the covariant derivative and returns the remaining slots,
avoiding an intermediate full derivative tensor.
`fortsym_spacetime_tensor_lie` transports a tensor of any supported rank along
an ordinary weight-zero contravariant spacetime vector, preserving its slot
variance and density weight. The related metric Killing residual is the Lie
derivative of the covariant metric tensor.
`fortsym_spacetime_tensor_declare_symmetry` validates one same-variance slot
pair componentwise and returns unchanged output components; a false declaration
returns `FORTSYM_INVALID_ARGUMENT`. Slots are one-based and the kind is
`FORTSYM_SYMMETRIC` or `FORTSYM_ANTISYMMETRIC`.
`fortsym_spacetime_tensor_symmetrize` projects one same-variance pair; pass zero
for symmetrization and a nonzero value for antisymmetrization.
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
