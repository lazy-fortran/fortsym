# Fortran API

`use fortsym` provides the expression type, arena type, constructors, operators,
and the expression functions under their ordinary Fortran names such as `exp`,
`sqrt`, and `erf`. The facade follows the same vocabulary as `fortsym_expr`;
the generic interfaces accept expression handles while the language's intrinsic
names remain the natural spelling for native symbolic expressions.
The lower-level `fortsym_expr` and `fortsym_arena` modules remain available.

The default facade also provides concise geometry constructors that reuse the
same native owners: `u = coords(x, y, z)`, `c = make_chart(u, position)`, and
`g = make_metric(c)`. The `make_` prefix is intentional: exporting generic
names `chart` or `metric` would prevent ordinary Fortran programs from using
those natural names for local variables. Explicit arenas and the lower-level
`chart_create` and `metric_create` names remain available for isolated or
concurrent work.

`fortsym_domain` owns topology declarations separately from coordinate and
metric calculus. `manifold_create(name, dimension, has_boundary,
simply_connected)` and `patch_create(manifold, name, open_domain, has_boundary,
simply_connected)` create value-semantic metadata owners; the corresponding
queries expose the declaration without attempting to infer topology from a
chart. `chart_create_on_patch(a, patch, u, x)` explicitly attaches a valid
three-dimensional patch to a `chart_t`; `chart_has_patch`, `chart_patch`, and
`chart_valid` expose and validate that relationship. The Python
`fortsym.sympy.Manifold`/`Patch` compatibility names carry the same flags, and
`Chart(..., patch=patch)`/`CoordSystem(name, patch)` retain the declaration in
the fixed-three-dimensional diffgeom subset.

Symbol names follow one rule: name the symbol as you would name the Fortran
variable, using the spelling that the printer reads. Names such as `varphi_2`,
`gamma_1`, `theta_bar`, and `B_r_hat` work as symbolic names, Fortran
identifiers, and readable typeset input. Never put LaTeX markup in a symbol
name. A name such as `\varphi_2` is the arena identity and is emitted verbatim
by every dialect. Use the notation override map when the desired notation
cannot be a Fortran identifier.

Fortran emission validates symbol names at the source boundary. Invalid names
such as `\[Alpha]` or `Global`x` are refused with a diagnostic rather than
written into generated source. Symbolic names remain case-sensitive; when a
kernel contains both `Gamma` and `gamma` (or `B` and `b`), only the colliding
Fortran spellings are deterministically renamed and the generated source
records the mapping in a comment.

## Default arena

Assigning character data to an `expr_t` creates one symbol. The right-hand side
is treated as a name, so `x = "x+y"` creates a symbol named `x+y` and does not
parse an expression.

```fortran
use fortsym
type(expr_t) :: mu, sigma, e

mu = "mu"
sigma = "sigma"
e = (mu + 1) / sigma
```

The module owns one saved default arena for the process lifetime. It is a
single-threaded convenience state. `default_arena()` returns the same arena
when an explicit constructor needs to share that state. `reset()` is safe
before first use and can be called more than once. It clears the node store and
invalidates every expression made before the reset, including handles whose
node index is reused later. The arena pointer remains usable after reset.
Discard pre-reset expressions at every reset boundary. Use explicit arenas for
concurrency, isolation, and library code that outlives one problem.

## Coordinate charts and magnetic views

`fortsym_chart` owns the generic three-dimensional chart. Give it coordinate
expressions and a Cartesian position map; `metric_covariant`, `sqrtg`,
`jacobian`, `covariant_basis`, `reciprocal_basis`, and the differential
operators follow from that one map. Basis matrices use component-first,
basis-second ordering, so `basis(k, i)` is the Cartesian component `k` of
the basis vector associated with coordinate `u(i)`.
`fortsym_magnetic` owns the derived magnetic views: `b_con`, `b_cov`,
`b_density`, `b_fourier`, `b_fourier_density`, and `j_fourier`. The
`b_fourier` interfaces
accept either an integer mode or an expression mode, so a paper derivation can
use a literal mode while a symbolic check keeps `n` in the expression tree.
`magnetic_field_t` packages `B^i`, `B_i`, and `sqrtg B^i` as typed tensor
views with variance and density weight retained across the three representations.
`b_con_form(c, beta, orientation)` is the inverse oriented Hodge/metric bridge
from a magnetic degree-two form to `B^i`; `b_density_form` adds the explicit
weight-one `sqrtg` factor. The default orientation is `+1`, and invalid or
foreign forms are refused by returning an invalid owner.
`magnetic_chart(c, potential, label_index)` packages those views with a
`flux_surface_t` in a `magnetic_chart_t` owner. `magnetic_chart_upper`,
`magnetic_chart_lower`, and `magnetic_chart_density` return the existing typed
views without a second component store; `magnetic_chart_average` delegates to
the verified surface-average owner. `magnetic_chart_potential_form` returns
`A = A_i du^i` and `magnetic_chart_flux_form` returns `beta = d(A)`; the latter
is a degree-two form whose exterior derivative is the native closedness check.
The chart operation `surface_measure(chart, normal_index)` returns the positive
induced measure on `u(normal_index)=constant`; `metric_surface_measure` is the
same operation for an explicit metric and uses `sqrt(abs(det(g_surface)))`.
`flux_surface(chart, label_index)` packages that label and the two remaining
angle coordinates as a `flux_surface_t`. `flux_surface_measure` returns its
positive induced measure, while `flux_surface_average` verifies the weighted
numerator and normalization with `definite_integral` on `[0,2*pi]` (or supplied
upper bounds) before returning a result. Unsupported symbolic integrals return
`ok=.false.` and a diagnostic instead of an unverified value.
`fortsym_flux` adds the reusable `flux_coordinate_t` descriptor. Construct it
with `flux_coordinates(chart, label_index, kind)` using `FLUX_GENERIC`,
`FLUX_CLEBSCH`, `FLUX_STRAIGHT_FIELD_LINE`, `FLUX_BOOZER`, or `FLUX_HAMADA`.
`flux_normal_residual` checks `B^label`,
`straight_field_line_residual` checks `B^angle_one - iota*B^angle_two`, and
`clebsch_residuals` checks the three components of
`B - grad(alpha) cross grad(beta)` using the signed chart Jacobian, while
`boozer_residuals` returns
`(B_label, d1 B1, d2 B1, d1 B2, d2 B2)` for the descriptor's ordered angles.
These are identity owners, not equilibrium solvers; callers use the native
engine or an independent oracle to establish that residuals vanish.
The generic chart operation `field_line_derivative(chart, vector, scalar)`
returns `vector(i) * d(scalar)/du(i)` and is also the magnetic `B dot grad`
operation.
`h_cov(chart, reluctivity, B_con)` applies the explicit constitutive convention
`H_i = nu_ij B^j`; `h_con(chart, H_cov)` raises that covector with `g^ij`.
The two operations are intentionally separate, so anisotropic material data
and metric index conversion remain independently reusable.
The chart also owns `grad`, `divergence`, `curl`, and `laplacian`; `curl` takes
covector components and returns contravariant components. Ordinary
`divergence` uses the positive chart volume law
`diff(sqrtg*v^i, u^i)/sqrtg`; the signed chart Jacobian is reserved for the
orientation-sensitive curl convention.

`fortsym_metric` owns explicit metric metadata. Use `metric_from_chart` for a
chart-induced Euclidean metric or `metric_create` for a supplied metric, and
always pass the signature and orientation when they are physically meaningful.
`metric_sqrtg` is positive and never absorbs orientation; `metric_det` and
`metric_contravariant` retain the metric's own signature.
`orientation_t` and `signature_t` are the shared declaration owners. Use
`metric_create_metadata` when typed declarations are already available;
`metric_signature_type` and `metric_orientation_type` return typed views.
Use `metric_coordinate(g, i)` when a single coordinate is needed in a hot
path; it avoids materializing the full coordinate tuple.
The dimension-aware `spacetime_metric_t` owner uses the same metadata types
through `spacetime_metric_create_metadata` and its corresponding typed-view
accessors.
When the metric carries `coordinates=...`, `metric_grad`,
`metric_divergence`, and `metric_laplacian` use that same explicit coordinate
tuple. `metric_grad` returns the contravariant components
`g^ij diff(f, u^j)`, `metric_divergence` applies
`diff(sqrtg*v^i, u^i)/sqrtg`, and `metric_laplacian` is their composition.
`metric_inner(g, left, right)` contracts contravariant component vectors as
`g_ij left^i right^j`; using the same vector twice is the native path for a
metric norm squared such as `B**2`, including nonorthogonal metrics.
These are metric-owner operations, so they also work for pseudo-Riemannian
signatures; orientation remains owned by the volume-form layer.
`volume_form(metric_owner)` uses the stored orientation, while
`volume_form(metric_owner, sign)` makes an explicit oriented view.
The `fortsym_volume` owner adds `metric_volume_density` for the positive
volume density, `levi_civita_symbol(i,j,k)` for the raw alternating symbol,
and `metric_levi_civita(metric_owner, variance)` for the oriented covariant
(`variance=-1`) or contravariant (`variance=1`) tensor.
The `fortsym_maxwell` owner adds `maxwell_field_strength`,
`maxwell_gauge_transform`, and `maxwell_residual` for the native
four-dimensional form equations `F=d(A)`, `A+d(chi)`, and `d(*F)-J`.

```fortran
use fortsym_chart, only: DIM
use fortsym_expr, only: expr_t
use fortsym_metric, only: metric_t, metric_create, metric_sqrtg, &
    metric_contravariant, metric_inner
use fortsym_volume, only: metric_volume_density, metric_levi_civita
type(metric_t) :: spacetime_metric
type(expr_t) :: g(DIM, DIM), sqrtg_value, g_inverse(DIM, DIM)
spacetime_metric = metric_create(g, [-1, 1, 1], -1)
sqrtg_value = metric_sqrtg(spacetime_metric)
g_inverse = metric_contravariant(spacetime_metric)
```

```fortran
use fortsym
type(arena_t), target :: a
type(chart_t) :: chart
type(expr_t) :: u(DIM), x(DIM), A(DIM), B(DIM), n

call a%init()
u(1) = "Z"
u(2) = "R"
u(3) = "phi"
x(1) = u(2)*cos(u(3))
x(2) = u(2)*sin(u(3))
x(3) = u(1)
n = "n"
A(1) = u(1)
A(2) = u(2)
A(3) = num(a, 0)
chart = chart_create(a, u, x)
B = b_fourier(chart, A, n)
! For a 3x3 reluctivity nu, J = curl(nu curl(A)):
! J = j_fourier(chart, nu, A, n)
```

The convenience facade and the lower-level modules call the same owners; they
do not maintain separate metric, variance, or density representations.

`fortsym_chart_map` owns bidirectional coordinate transitions. Supply target
coordinates as functions of source coordinates and the inverse map explicitly.
`map_valid` reports whether the transition passed the native structural
validation; identically singular forward or inverse Jacobians are refused.
When the source and target charts were created with `chart_create_on_patch`,
the map retains both declarations through `chart_map_source_patch` and
`chart_map_target_patch`. Composition compares the intermediate chart's patch
metadata as well as its coordinate expressions, so a transition cannot silently
cross an unrelated declared patch.
The tensor transform applies `K` to upper slots and `L` to lower slots. For a
stored density weight `w`, it multiplies by `abs(det(K))**(-w)` before the
slot transforms. This is the passive coordinate law for a density. In
particular, `D^i = sqrtg*B^i` is a weight-`+1` contravariant density, while
orientation remains a separate signed top-form choice.

```fortran
use fortsym_chart_map, only: chart_map_t, chart_map_create, compose_maps, &
    chart_map_has_source_patch, chart_map_has_target_patch, &
    chart_map_source_patch, chart_map_target_patch, transform_tensor, &
    transform_form, pullback
type(chart_map_t) :: transition
transition = chart_map_create(source, target, target_in_source, source_in_target)
target_tensor = transform_tensor(transition, source_tensor)
target_form = transform_form(transition, source_form)
target_form = pullback(transition, source_form)
combined = compose_maps(transition, following)
```

The first native differential-form layer is three-dimensional and uses the
coordinate coframe. `form_scalar`, `form_one`, `form_two`, and `form_three`
construct typed forms; `d`, `wedge`, `star`, `interior`, and `lie` operate on
them. `flat` and `sharp` are the metric-owned one-form/vector conversions.
`curl_density` returns the contravariant weight-one density associated with a
covector, and `div_density` is its componentwise scalar-density divergence.
They are the natural coordinate operations for differential forms and the
Fourier finite-element formulation; `curl` and `divergence` remain the
ordinary vector-valued operations.
`form_zero(chart, 4)` names the explicit zero extension returned by `d` of a
three-form; it is preserved by native form operations and chart-map transport.
The magnetic two-form is therefore a derived identity, not a second magnetic
field representation:

```fortran
type(expr_t) :: A(DIM)
type(form_t) :: A_form, B_form, volume, flux_form

A_form = form_one(chart, A)
B_form = d(chart, A_form)
volume = volume_form(chart)
flux_form = interior(chart, b_con(chart, A), volume)
```

On an orientation-preserving chart, `flux_form` and `B_form` agree and
`d(chart, flux_form)` is zero. Higher-dimensional manifolds, arbitrary tensor
rank, and full Python form parity remain explicit roadmap work; the fixed-3D
transport facade is documented in `python/README.md`.

`fortsym_form_tensor` is the single bridge between the two typed owners. It
does not add a second component store: `tensor_from_form(chart, alpha)` returns
the fully antisymmetric lower-tensor view, while `form_from_tensor(tensor)`
returns the compact form view. The latter accepts only an exact weight-zero,
lower-variance antisymmetric tensor; non-antisymmetric and density-weighted
inputs are refused rather than silently projected. The Python facade exposes
the same vocabulary as `chart.tensor_from_form(form)`,
`chart.form_from_tensor(tensor)`, `form.to_tensor()`, and `tensor.to_form()`.

## Typed coordinate tensors

`fortsym_tensor` owns the first typed tensor subset. A `tensor_t` is bound to a
chart arena and records rank, the variance of every slot (`UPPER` or
`LOWER_VARIANCE`), and an integer density weight. Chart-created tensors also
retain a value-semantic owner key consisting of the coordinate tuple and
position map. Metric-created tensors retain their explicit coordinate tuple
without inventing a position map. `tensor_same_chart` and
`tensor_chart_compatible` expose this boundary; products, metric operations,
derivatives, conversions, and chart-map transport refuse incompatible owners.
Unbound scalar tensors remain convenient and acquire the bound owner when
combined with a chart-owned result. The facade aliases
`vector`/`covector` are the short constructors; `tensor_from_components` and
`tensor_from_matrix` cover general fixed-rank data. Components use
first-slot-fastest indexing and are read with an explicit index tuple.

```fortran
type(tensor_t) :: Bup, Bdown, outer, scalar
type(expr_t) :: B(DIM), value
integer :: index(1), none(0)

B(1) = "B1"
B(2) = "B2"
B(3) = "B3"
Bup = vector(chart, B)
Bdown = lower(chart, Bup, 1)
Bup = raise(chart, Bdown, 1)
outer = tensor_product(Bup, Bdown)
scalar = contract(outer, 1, 2)
index(1) = 1
value = tensor_component(Bup, index)
value = tensor_component(scalar, none)
```

The typed tensor owner also provides the coordinate Lie derivative:

```fortran
type(tensor_t) :: X, T, transported
X = vector(chart, B)
T = tensor_scalar(value)
transported = lie(chart, X, T)
```

`lie` and `lie_derivative` preserve tensor variance and density metadata. The
transport vector must be an ordinary weight-zero contravariant vector. For a
tensor density of weight `w`, the native coordinate formula includes
`+w*T*d_i(X^i)`; upper slots contribute `-T(...,k,...)*d_k(X^i)` and lower
slots contribute `+T(...,k,...)*d_i(X^k)`. The implementation writes directly
into the fixed component store, without rank-dependent component arrays.

`raise` and `lower` preserve density weight and all non-selected slots.
They accept either a chart or an explicit `metric_t` owner, so the same tensor
operation can be used for a supplied Lorentzian metric without reconstructing
a chart. `metric_covariant_tensor(metric_owner)` and
`metric_contravariant_tensor(metric_owner)` create typed metric tensors with
the owner's arena and metadata contract.
`contract` requires opposite variance and removes the two selected slots;
`trace` is its named alias. The current native subset is rank five or less in
three-dimensional charts. A checked overload also accepts `index_t` values
from `fortsym_index`: `index_type("T", 3)` declares a space and
`make_index(space, slot, variance, label, dummy)` declares a one-based native
slot. It requires the same space, opposite variance, and matching nonempty
labels. Arbitrary dimensions, full symmetry-group transport, and full Python
tensor parity remain roadmap work; the fixed-rank Python transport is
documented in `python/README.md`.
The tensor owner records pairwise symmetry metadata with
`tensor_symmetry(tensor, first, second)`. It returns `SYMMETRY_NONE`,
`SYMMETRIC`, or `ANTISYMMETRIC`. `declare_symmetry` checks every stored
component before accepting a declaration; a false declaration returns an
invalid tensor rather than silently changing components. `symmetrize` and
`antisymmetrize` set the corresponding declaration. Tensor products,
permutations, contractions, and covariant derivatives preserve declarations
on surviving slots. Raising or lowering a slot clears declarations involving
that slot because its variance has changed. Metric tensors and tensor views of
forms carry their guaranteed pair symmetry explicitly.

`tensor_product(left, right)` is the single native outer-product owner; the
left slots precede the right slots and density weights add.
`density(tensor, integer)` changes only the stored density metadata for a
view. `density(tensor, factor)` is the component-density constructor: it
multiplies every component by the scalar `factor` and increments the density
weight. Thus `Bden = density(Bup, sqrtg(chart))` is the generic native spelling
of `sqrtg B^i`, while `density(Bup, 1)` remains a metadata-only view.

`fortsym_form` follows the same owner contract. `form_same_chart` and
`form_chart_compatible` compare the coordinate tuple and, when present, the
position map; `form_t` views preserve that key through wedge, `d`, Hodge,
contraction, Lie, metric, conversion, and chart-map operations. Forms and
tensors are converted only through `fortsym_form_tensor`; neither
owner imports the other's storage layout. This keeps the short facade names
consistent with the lower-level owners and makes the exact antisymmetry and
density boundary visible at every API layer.

## Connections and curvature

`fortsym_connection` owns the first covariant-calculus subset. It consumes a
chart and a typed tensor, appends the derivative as a lower slot, applies the
Christoffel term for every upper and lower slot, and applies the documented
`-w*Gamma^m_mk*T` density-weight term. `covariant_derivative` is the readable
alias of the short `covariant_diff` name; both are exported by the facade.
`christoffel_tensor` also accepts a `metric_t` carrying explicit coordinates;
metrics without coordinates are intentionally refused for derivative-based
connection construction.
`covariant_diff` and `covariant_derivative` use the same overload rule and
share the chart/metric component kernel.
The same metric-owner overload is available for `riemann_tensor`,
`ricci_tensor`, `scalar_curvature`, and `einstein_tensor` when coordinates are
present.

```fortran
type(tensor_t) :: metric_value, metric_derivative, curvature
type(expr_t) :: scalar

metric_value = metric_covariant_tensor(chart)
metric_derivative = covariant_diff(chart, metric_value)
curvature = riemann_tensor(chart)
scalar = scalar_curvature(chart)
```

`covariant_divergence(tensor)` is the physicists' first-slot divergence: it
forms the covariant derivative and contracts the first contravariant slot with
the appended lower derivative slot. It preserves density weight and refuses a
scalar or a tensor whose first slot is not contravariant. For a weight-one
vector density it reduces to `div_density`, namely
`partial_i(sqrtg*V^i)` when the stored components are `sqrtg*V^i`.

The current fixed-three-dimensional subset also provides typed Christoffel,
geodesic residuals, Ricci, and Einstein tensors. Its convention is
`R^a_bcd = d_c Gamma^a_db - d_d Gamma^a_cb + Gamma^a_cm Gamma^m_db -`
`Gamma^a_dm Gamma^m_cb`; pseudo-Riemannian sign-corpus coverage and geodesic
solving remain roadmap work.
`first_bianchi_residual` returns the rank-four residual
`R^a_bcd + R^a_cdb + R^a_dbc`; it is zero for the torsion-free metric
connection and is exposed as a residual rather than a hidden Boolean.
`second_bianchi_residual` returns the rank-five residual
`nabla_e R^a_bcd + nabla_c R^a_bde + nabla_d R^a_bec`. The typed tensor owner
now preserves that derivative slot and exposes the same residual through the
Python/SymPy facade.
`geodesic_residual(chart, curve, parameter)` and its coordinate-aware
`metric_t` and supplied `connection_t` overloads return
`x''^a + Gamma^a_bc x'^b x'^c`, after substituting the parameterized curve into
the connection. The supplied-connection view does not require a metric. The
current owner is fixed to three-dimensional charts; geodesic solving and
variational mechanics remain separate roadmap work.

`connection_t` is the explicit affine-connection owner. Construct the
coordinate Levi-Civita owner with `connection_from_chart(chart)` or
`connection_from_metric(metric)`, or supply `Gamma^a_bc` directly with
`connection_create(components, coordinates)`. `torsion(connection)` returns
`T^a_bc = Gamma^a_bc - Gamma^a_cb`; `nonmetricity(connection, metric)` returns
`Q_ijk = -nabla_k g_ij`, with the derivative slot last to match
`covariant_diff`. The connection overloads of `covariant_diff`,
`covariant_derivative`, and `covariant_divergence` use exactly the same typed
slot and density kernel as the chart and metric owners. `CONNECTION_STANDARD`
and `CONNECTION_OPPOSITE` select the sign of the supplied connection's
Riemann view. The affine coefficients, torsion, covariant derivatives, and
geodesic residual are unchanged by that curvature-sign choice. Geodesic
solving remains separate roadmap work.
`riemann_tensor(connection)` differentiates the supplied coefficients directly
and returns `R^a_bcd` without inferring or constructing a metric. The Python
facade exposes the same view as `Connection.riemann()`.

The dimension-aware `spacetime_metric_t` owner provides the matching
relativity operations `spacetime_metric_flat`, `spacetime_metric_sharp`,
`spacetime_metric_grad`, `spacetime_metric_divergence`, and
`spacetime_metric_laplacian`. They use the runtime dimension and explicit
signature, retain zero slots for ABI transport, and define
`sharp(flat(v))=v` on valid components. Divergence uses positive
`sqrt(abs(det(g)))`; for a Lorentzian metric the scalar Laplace--Beltrami
operator is the coordinate wave operator.

`spacetime_tensor_covariant_diff(metric, tensor)` appends a lower derivative
slot to a runtime spacetime tensor of rank at most four, producing the
rank-five representation needed for covariant derivatives of curvature
tensors. It applies the
Christoffel transport term to every upper and lower slot and the
`-w*Gamma^m_mk*T` density term, while preserving the input density weight.
The Python `SpacetimeTensor.covariant_diff()` method and its
`covariant_derivative()` alias expose the same owner.

`spacetime_tensor_covariant_divergence(metric, tensor)` contracts the first
upper slot with that derivative and returns the remaining tensor. The direct
kernel preserves density weight and is equivalent to forming
`covariant_diff(metric, tensor)` followed by a first-slot/last-slot
contraction, without materializing that intermediate tensor.

`lie(metric, vector, tensor)` and `lie_derivative(metric, vector, tensor)`
use the runtime tensor owner for coordinate Lie transport without a metric
connection. The vector must be an ordinary weight-zero contravariant field;
upper slots contribute `-T^k*d_k X^a`, lower slots contribute
`+T_k*d_b X^k`, and a density of weight `w` contributes
`+w*T*d_k X^k`. `lie(metric, vector, spacetime_metric_covariant_tensor(metric))`
is the Killing residual.

## Relativity and magnetic-flux examples

The built examples keep physical assumptions at the application boundary:

```text
fo exec example_de_sitter
fo exec example_gps_newtonian_limit
fo exec example_magnetic_flux_coordinates
fo exec example_boozer_coordinates
fo exec example_spherical_coordinates
fo exec example_cylindrical_fourier
```

`example_de_sitter` uses the flat-slicing metric
`ds^2 = -dt^2 + exp(2 H t)(dx^2 + dy^2 + dz^2)` and verifies
`G_ab + 3 H^2 g_ab = 0` and `R = 12 H^2`. The GPS example uses the weak,
static metric `g_tt = -(1 + 2 Phi)` in units with `c = 1`. Its checked
Christoffel component gives `r'' = -d_r Phi` in the slow-motion limit. The
printed clock correction is `d tau/dt - 1 = Phi - v^2/2`; restoring units
gives `Phi/c^2 - v^2/(2 c^2)`.

The GPS example follows the order-`1/c^2` separation of gravitational and
second-order Doppler effects described by Pascual-Sánchez, *Introducing
Relativity in Global Navigation Satellite Systems*,
[`arXiv:gr-qc/0507121`](https://arxiv.org/abs/gr-qc/0507121). The broader
survey *Relativity in the Global Positioning System* is available in
[`Living Reviews in Relativity`](https://www.maths.tcd.ie/EMIS/journals/LRG/Articles/lrr-2003-1/).
The example proves the symbolic weak-field identity. It does not model Earth
rotation, signal propagation, orbital eccentricity, or a full post-Newtonian
GNSS solution.

`example_magnetic_flux_coordinates` uses
`R = R0 + a sqrt(psi) cos(theta)` and `Z = a sqrt(psi) sin(theta)` with
`phi` as the toroidal angle. The covector potential `A = psi dphi` produces a
field tangent to `psi = constant` surfaces. The generic chart, metric, volume,
form, and magnetic owners perform the derivation. The application contains no
second magnetic representation.

`example_boozer_coordinates` is a compact analytic Boozer-coordinate fixture,
not an equilibrium solver. With `q = (psi, theta, phi)` it uses

```text
g_ij = diag(1, h^2, h^2),       sqrt(g) = h^2,
B_i = (0, I(psi), G(psi)),     B^i = (0, I/h^2, G/h^2).
```

It proves that `B_theta` and `B_phi` are independent of the angular
coordinates, hence are flux functions, and checks `div(B) = 0` after raising
the covector with the native metric owner. This intentionally isolates the
Boozer representation: a real equilibrium calculation must still construct
the coordinate map and solve the corresponding magnetic equations. The
standard Boozer covariant representation and its flux-function angular
components are summarized in [FusionWiki's Boozer-coordinate reference](https://fusionwiki.ciemat.es/wiki/Boozer_coordinates)
and the [Boozer-coordinate notes](https://sites.fusion.ciemat.es/jlvelasco/files/notes/Boozer.pdf).

`example_spherical_coordinates` uses the regular patch `r > 0` and
`0 < theta < pi`. It derives the Cartesian chart basis and Jacobian, checks
the reciprocal basis and diagonal metric, then uses the compact explicit
metric owner for Christoffel symbols and vector calculus. It verifies
`grad(r)`, `div(r e_r)`, a nontrivial curl, and
`Laplace--Beltrami(r^2) = 6` without carrying the raw Cartesian chain-rule
expansion through every connection component.

`example_cylindrical_fourier` follows the Albert--Bíro--Lainer ordering
`(Z, R, phi)`. It keeps `sqrt(g) B^i` as the primary Fourier density, so the
displayed result does not require the branch choice `sqrt(R**2) = R`. The
example checks the three density components for
`A = Z R dZ + R**2 dR` and a symbolic symmetry mode `n`.

`symbols` assigns whitespace- or comma-separated names to scalar outputs:

```fortran
call symbols("mu sigma best xi", mu, sigma, best, xi, ok=good)
```

`good` is false when an output has no corresponding name or when extra names
remain. The output without a name is an invalid `expr_t`.

`num` and `rat` construct compact exact integers and rationals. `exact` accepts
an arbitrary-size integer or rational string and canonicalizes it in the same
arena:

```fortran
huge = exact(default_arena(), "9223372036854775808", ok=good)
half = exact(default_arena(), "6/-8", ok=good)
```

`operation_count(expression)` counts operation occurrences with SymPy's
`count_ops` tree semantics. Atoms cost zero; n-ary sums and products count one
operation per operand link; repeated references to an interned child count once
per written occurrence; canonical reciprocal products count as divisions.
`expr_t%node_count()` remains the separate shared-DAG node measure.

`subs_many(expression, old, new)` applies paired replacement arrays
simultaneously. Replacement expressions are not revisited, so swaps and other
coupled replacements do not cascade. It returns the same `engine_result_t`
status contract as `subs`; the arrays must have equal size and belong to the
expression's arena.

`free_symbols(expression, names)` collects the distinct symbolic names
reachable from the expression into caller-owned allocatable `names`. Constants
and applied-function heads are excluded. The lower-level `fortsym_eval` owner
provides `collect_free_symbols` for internal consumers; the facade keeps
`free_symbols` as the easy native spelling and avoids an allocatable
function-result temporary.

`oo_expr` constructs the positive-infinity sentinel. It remains a structural
domain value, not a finite real literal. Native simplification currently
matches the finite scalar and integer-power rules for `oo` and `zoo`: known
signs determine directed products, `0*oo` and `0*zoo` become `nan`, and
symbolic products such as `oo*x` remain unevaluated. Finite real Fortran
kernel emission still refuses these domain values.

```fortran
infinity = oo_expr(default_arena())
```

`zoo_expr` and `nan_expr` construct complex-infinity and undefined/NaN
sentinels. They are structural domain values, not floating-point payloads;
the supported native domain rules include the finite scalar and integer-power
cases above, plus the declared `nan` propagation rules. Finite real emission
refuses all three.

```fortran
complex_infinity = zoo_expr(default_arena())
undefined = nan_expr(default_arena())
```

Native simplification propagates `nan` through addition, multiplication, the
supported numeric unary heads, and powers. Universal construction identities
are canonicalized in the shared arena: `x**0 = 1`, `x**1 = x`, and
`1**x = 1` except for the
`oo`/`zoo`/`nan` exponents, which produce `nan`, and `sqrt(x)**2 = x`. The
defined sentinel power exception is `nan**0 = 1`; a NaN base or exponent in every
other supported power produces `nan`. Unknown function heads remain structural
and are not assigned guessed
domain rules. Non-integer powers, symbolic-factor products, and
operation-specific function/limit rules remain separate roadmap work. The
current unary fragment covers `sqrt`, `abs`, `exp`, and `log` on known domain
sentinels; unknown factors remain unevaluated, and `sqrt(-oo)` is represented
as the structural complex product `i*oo`. Compact rational powers of `oo` and
`zoo` are also handled. Positive rational powers of `-oo` retain SymPy's
normalized principal phase, such as `(-oo)**(2/3) = oo*(-1)**(2/3)` and
`(-oo)**(4/3) = -oo*(-1)**(1/3)`; negative rational powers remain `0`.
Principal square roots of exact negative perfect-square rationals are also
canonicalized, so `sqrt(-1)=i` and `sqrt(-4)=i*2`; irrational negative roots
remain unevaluated.
Direct domain rules also cover `sign`, `floor`, `ceiling`, `sinh`, `cosh`, and
`tanh`; `sign(zoo)` stays unevaluated because its value is not determined.
The periodic heads `sin`, `cos`, and `tan` map `zoo` to `nan`; their `+/-oo`
results are SymPy accumulation bounds and remain explicit applied heads until
fortsym has a bounded-set representation.
The inverse heads `asin`, `acos`, `atan`, `asinh`, `acosh`, and `atanh` also
match the representable `oo`/`-oo` results from SymPy 1.14.0. The
accumulation-bound results for `atan(zoo)` and `atanh(zoo)` remain applied
heads until a compatible bounded-set representation exists.
The exact imaginary branch points `atan(i)` and `atan(-i)` are canonicalized to
`i*oo` and `-i*oo`; accumulation-bound results at `atan(zoo)` remain applied.
The exact real poles `atanh(1)` and `atanh(-1)` are canonicalized to `oo` and
`-oo`; unsupported complex and accumulation-bound cases remain unevaluated.
The exact imaginary branch points `atanh(i)` and `atanh(-i)` are canonicalized
to `i*pi/4` and `-i*pi/4`; broader complex inverse branches remain unevaluated.
The exact principal branch points `acosh(0)` and `acosh(-1)` are likewise
canonicalized to `i*pi/2` and `i*pi`; other negative-real branches remain
unevaluated.
The exact imaginary branch points `acosh(i)` and `acosh(-i)` are canonicalized
to `log(sqrt(2) + 1) + i*pi/2` and `log(sqrt(2) + 1) - i*pi/2`.
The exact imaginary branch points `asin(i)` and `asin(-i)` are canonicalized
to `i*log(sqrt(2) + 1)` and `-i*log(sqrt(2) + 1)`.
The exact imaginary branch points `acos(i)` and `acos(-i)` are canonicalized
to `pi/2 - i*log(sqrt(2) + 1)` and `pi/2 + i*log(sqrt(2) + 1)`.
The exact principal real unit-circle values are also canonicalized:
`asin(±1/2)`, `asin(±sqrt(2)/2)`, and `asin(±sqrt(3)/2)` return signed
`pi/6`, `pi/4`, and `pi/3`, while the matching `acos` values return their
principal angles from `pi/6` through `5*pi/6`.
The exact real tangent values `atan(±sqrt(3))` and `atan(±1/sqrt(3))` return
signed `pi/3` and `pi/6`; other exact tangent arguments remain unevaluated.
The exact real inverse-hyperbolic values `asinh(1)` and `asinh(-1)` are
canonicalized to `log(sqrt(2) + 1)` and `-log(sqrt(2) + 1)`.
Finite gamma-family poles follow SymPy's representable boundaries: exact
non-positive integer `gamma(n)` becomes `zoo`, exact non-positive integer
`loggamma(n)` becomes `oo`, and exact negative integer `factorial(n)` becomes
`zoo`. Nonnegative integer factorials through `factorial(1000)` are evaluated
exactly: orders through 20 use the checked compact integer path, while larger
supported orders use the existing arbitrary-size exact-arithmetic owner. Orders
above 1000, overflowing resource budgets, and non-integer factorials remain
applied. Broader non-integer and accumulation-bound pole cases remain applied.
The exact imaginary branch points `asinh(i)` and `asinh(-i)` are canonicalized
to `i*pi/2` and `-i*pi/2`; broader complex inverse branches remain unevaluated.
The reciprocal-hyperbolic heads `csch`, `sech`, and `coth` likewise have
direct scalar rules: `csch` and `sech` tend to zero, while `coth` tends to
the corresponding signed one; every `zoo` case becomes `nan`.
The error-function heads `erf` and `erfc` have the scalar limits
`erf(oo)=1`, `erf(-oo)=-1`, `erfc(oo)=0`, and `erfc(-oo)=2`; their `zoo`
applications remain unevaluated.
The gamma heads add the representable limits `gamma(oo)=oo` and
`loggamma(oo)=oo`; `loggamma(-oo)` and `loggamma(zoo)` are `zoo`, while
pole-sensitive `gamma(-oo)` and `gamma(zoo)` remain unevaluated.
The shared positive-infinity branch also gives `factorial(oo)=oo`; its
negative and complex-infinity applications remain unevaluated.
The two-argument `atan2` head evaluates the directed `(+/-oo, +/-oo)`
quadrants (`0`, `pi`, and `-pi`); complex-infinity and other ambiguous pairs
remain applied heads.
The Bessel heads add `besselj(order, +/-oo)=0` and
`besseli(order, oo)=oo`. Symbolic order at negative infinity uses the phase
`oo*(-1)**order`, and integer orders reduce to signed infinity; unsupported
exact non-integer and complex-infinity cases remain applied heads. NaN is not
generically propagated through the
order-bearing Bessel and Legendre heads: unresolved NaN arguments remain
applied, while `besseli(nan, -oo)` follows SymPy's representable `nan` result.
Native `legendrep(degree, order, argument)` keeps its Fortran spelling; the
adapter's `legendre(degree, argument)` maps to order zero. Nonnegative integer
degrees through two at `+/-oo` and `zoo` use the representable parity/domain
rules; degree three and higher become `nan`. Negative integer degrees use the
`P(-n-1, x) = P(n, x)` identity, while other degrees and orders remain applied
heads.

The exact integer and rational fragment preserves canonical values through
construction and native arithmetic. Exact complex or algebraic values remain a
separate domain from ordinary `num`, `rat`, and `exact` leaves. Construct a
canonical FLINT `qqbar1` value with `algebraic_expr`:

```fortran
type(expr_t) :: root
root = algebraic_expr(default_arena(), qqbar_text, ok=good)
```

`algebraic_expr` retains the exact value as an `NK_ALGEBRAIC` arena atom, and
`root%algebraic_text()` returns its canonical `qqbar1` spelling. The native
engine combines pure algebraic expressions with exact `+`, `*`, and integer
powers. Native simplification also combines algebraic coefficients in mixed
expressions and uses the FLINT sign oracle for algebraic zero, one, and
definitely-nonzero guards. `real64`
expression
evaluation refuses algebraic atoms. Fortran kernel emission accepts exact real
algebraic atoms through a checked `algebraic_to_real` projection. The
projection uses FLINT's rigorous Arb enclosure and refuses non-real, subnormal,
overflow, and ambiguous-rounding values. SymEngine accepts atoms whose exact
real and imaginary components are rational and converts them to an exact
`re + im*I` expression. Higher-degree or otherwise non-rational atoms retain a
named refusal. `print_expr` displays the canonical payload, and the
native/backend text parsers accept it as one opaque lossless token; it is not a
Fortran source literal.

The lower-level `fortsym_complexdom` module provides `re_part`, `im_part`,
`abs_of`, `conjugate`, and `complex_expand` with an explicit assumption context. Algebraic atoms are accepted
there through FLINT's exact real and imaginary projections, and conjugation is
exact. Structural conjugation commutes with the supported heads `exp`, `sin`,
`cos`, `sinh`, `cosh`, `tan`, and `tanh` wherever they are defined. Rectangular
splitting also covers the entire heads `exp`, `sin`, `cos`,
`sinh`, and `cosh` through their addition identities. `tanh` is also split
through its rectangular quotient, and `tan` through the corresponding
meromorphic quotient; `log` uses the principal `log(abs) + i*Arg` form, and
`sqrt` uses the principal polar half-angle form. An identically zero
denominator or logarithm argument is refused, while a nontrivial denominator
remains in the result to preserve pointwise poles. `complex_expand` preserves
`oo` and `nan` and maps `zoo` to `nan`, matching SymPy's defined sentinel
boundary. The direct projections also match the defined sentinel cases:
`re(oo)=oo`, `re(-oo)=-oo`, `im(oo)=0`, `arg(-oo)=pi`, `Abs(zoo)=oo`, and
undefined projections return `nan`; `conjugate(zoo)` remains an applied head.
The same branch and reality refusals apply to other unsupported expressions.

The main `fortsym` facade exposes the same owner as `re_part`, `im_part`,
`abs_of`, `conjugate`, `arg_of`, and `complex_expand`, each returning the common `engine_result_t`. The
facade creates a local assumption context when none is supplied, while an
explicit context must belong to the expression's arena. The C ABI and
`fortsym.sympy` adapter provide the corresponding `re`/`im`/`Abs`/`conjugate`/`arg`/`expand_complex`
boundary and preserve the complex-domain refusal diagnostics.

`real_text_expr` retains a bounded finite decimal literal as `NK_BIG_REAL`.
The arena validates its decimal syntax and preserves the original digits rather
than converting the value through `real64`.

The `fortsym_predicates` module exposes `is_number(expression)`. It returns true for
numeric atoms, named constants, domain sentinels, exact algebraic atoms, and
compound expressions whose children are all numeric. Symbols and relation
objects return false. The C ABI and Python compatibility layer call this same
native owner.

It also exposes `is_algebraic(expression, assumptions)`, which returns the
shared `VERDICT_TRUE`, `VERDICT_FALSE`, or `VERDICT_UNKNOWN` values. Exact
integers, rationals, FLINT algebraic atoms, `I`, and supported exact rational
powers are proved algebraic; `pi`, `e`, and supported transcendental heads are
proved non-algebraic; machine reals and unresolved symbols remain unknown.
The optional immutable assumption context can prove `algebraic_valued`
symbols. The public `fortsym` facade re-exports the predicate and the existing
shared verdict constants, so there is no second predicate-specific verdict
vocabulary.

Requested-precision numeric evaluation uses one generic with two result forms.
Pass a character variable for the decimal text, or pass `numeric_real_text_t`
to retain the requested precision with the value:

```fortran
type(numeric_real_text_t) :: numeric
call numeric_precision_text(pi_expr(default_arena()), 40, numeric, good, why)
```

`numeric%digits` records the requested decimal precision.

Accuracy measurement for a caller-owned real64 kernel lives in
`fortsym_accuracy`, not in the numeric result type. `measure_accuracy` samples
the declared input matrix, evaluates the substituted expression through the
MPFR reference path, and reports maximum and RMS local-ULP error. The report
also retains the input that reached the maximum, the reference and observed
values, the derivative-based condition number when it can be evaluated, and
the count of refused samples. This is an independently checked bound over the
declared sample set. It is evidence for that set, not a proof over every real
input.

## Explicit arenas

An explicit arena remains the first-class API for independent problems and
concurrent work:

```fortran
type(arena_t), target :: a
type(expr_t) :: x

call a%init()
x = sym(a, "x")
```

Expressions built with an explicit arena can use every operator and expression
function exported by `fortsym`. Expressions combined by an operator must belong
to the same arena. The default and explicit forms can be mixed when the
explicit constructor receives `default_arena()`.
The core operations use the expression's owning arena, so the same `subs`,
`diff`, `simplify`, `refine`, `expand`, and `factor` calls work without an
explicit-arena variant or a second calling syntax.

## Explicit assumption contexts

The facade provides value-style assumption contexts for isolated derivations.
`make_assumption_context` creates an empty context for an arena, and
`with_assumption` returns a copied context with one supported fact or relation.
The parent is unchanged, so sibling contexts can be used independently:

```fortran
type(assumption_context_t) :: base, positive_x, nonnegative_y
logical :: good

base = make_assumption_context(a)
positive_x = with_assumption(base, positive(x), good)
nonnegative_y = with_assumption(base, nonnegative(y), good)
result = simplify(sqrt(x**2), assumptions=positive_x)
```

The same value-style constructor accepts bounded relational expressions from
the facade. Positive/nonnegative facts are derived from exact positive or zero
lower bounds; negative/nonpositive facts are derived from exact negative or
zero upper bounds:

```fortran
relation_context = with_assumption(base, greater(x, num(a, 1)), good)
result = refine(sqrt(x**2), assumptions=relation_context)
```

`equal`, `unequal`, `less`, `less_equal`, `greater`, and `greater_equal` keep
the native relation vocabulary in snake_case. Exact equality records zero or
the corresponding sign, `unequal(expression, 0)` records nonzero, and `And`
relations are ingested transactionally. Bounds that do not imply a supported
sign and foreign-arena relations are refused. `Element(x, Rationals)`,
`Element(x, Integers)`, and `Element(x, PositiveIntegers)` are also accepted
through the same native fact owner.

The supported constructors are `real_valued`, `rational_valued`,
`integer_valued`, `positive_integer`, `zero`, `negative`, `nonpositive`,
`positive`, `nonnegative`, and `nonzero`. Rational facts imply real, integer
facts imply rational and real, and the `positive_integer` shorthand implies
integer, rational, and positive. Sign facts close over
their sound implications; nonnegative plus nonpositive infers zero, while
contradictory facts are refused with an explanatory `ok`/diagnostic result.
The native guarded simplifier uses these facts for `sqrt(x**2)` and `abs(x)`:
positive/nonnegative values return `x`, negative/nonpositive values return
`-x`, and zero returns `0`; unknown reality remains unevaluated.
It also reduces `log(exp(x))` when `x` is real and `exp(log(x))` when `x` is
nonzero. Without the required fact, these compositions remain unevaluated so
branch-sensitive identities are never guessed.
The exact singularity `simplify(log(0))` follows SymPy and returns `zoo`; the
existing sentinel propagation therefore returns `nan` for
`simplify(exp(log(0)))`. Numeric real evaluation and complex rectangular
splitting still refuse the pole rather than claiming a finite value.
Exact negative real arguments use the principal logarithm branch: for example,
`simplify(log(-2))` returns `log(2) + i*pi`, while unsupported non-exact branch
cases remain unevaluated.
The exact imaginary points follow the same principal branch:
`simplify(log(i))` returns `i*pi/2` and `simplify(log(-i))` returns `-i*pi/2`.
Under the principal-square-root convention, `sqrt(x)**2` reduces to `x` for
symbolic `x` without an additional assumption; this is distinct from the
branch-sensitive `sqrt(x**2)` rewrite above.
`simplify`, `refine`, `expand`, `factor`, `diff`, and `zero_test`
accept the optional `assumptions=` context. `refine` is the named entry point
for applying these supported facts; it shares guarded rewrite ownership with
the native simplifier. A context from another arena is refused with a
diagnostic. The default facade remains unchanged and does not consult an
implicit process-global context.

## Core operations

The easy facade uses the same short names as the owning modules:

```fortran
type(engine_result_t) :: result

f = (x + 1) * (x + 2)
result = subs(f, x, y)
g = result%value
result = diff(f, x)
d = result%value
result = simplify(f)
s = result%value
result = refine(sqrt(x**2), assumptions=positive_x)
r = result%value
result = expand(f)
e = result%value
result = factor(f)
p = result%value
```

`diff` is the evaluated native derivative. The `fortsym_diff` module remains
available for the deliberately unsimplified derivative DAG. `subs` is structural and
simultaneous for its one replacement pair; `subs_many` applies paired arrays
simultaneously. `simplify`, `refine`, `expand`, and
`factor` use the native engine in the expression's arena. All six functions
return the
same `engine_result_t` as the native engine. `%ok` reports whether the operation
succeeded, `%value` contains the resulting expression, and `%message` contains a
diagnostic on refusal. Native conditional results may also populate
`%conditional` and `%condition`. Use `%value` only after checking `%ok`.

## Zero query

`zero_test(expression)` is the one symbolic zero query in the easy facade. It
returns an `engine_result_t`; `%ok` reports whether the query executed and
`%verdict` is `VERDICT_TRUE` for a proved zero, `VERDICT_FALSE` for a proved
nonzero expression, and `VERDICT_UNKNOWN` when the native engine declines to
decide. `%value` contains the expression result produced by the engine when the
query succeeds.
`verdict_name` renders those outcomes as `ZERO`, `NONZERO`, and `UNKNOWN`.
`fortsym_check` contains the assertion helpers and numeric probe. They are test
utilities, not additional symbolic predicates.
