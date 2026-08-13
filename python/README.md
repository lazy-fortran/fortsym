# fortsym Python bindings

This package is a standard-library-only `ctypes` facade over the installed
fortsym C ABI. It does not import SymPy or depend on a Fortran compiler ABI.

Set `FORTSYM_LIBRARY` to the absolute path of the shared library when using a
wheel or an installed CMake tree. A source checkout can instead use
`PYTHONPATH=python`; the package then searches `build/lib` automatically.
The CMake install places the package sources under
`share/fortsym/python/fortsym`, so that directory can be added to `PYTHONPATH`.

Expression handles own their native references and are released by `close()`
or garbage collection. Exact integers and `fractions.Fraction` values are
passed as decimal strings when they do not fit the compact C ABI scalars.
`Expr.free_symbols` is cached on its owning expression; its symbol handles are
kept alive by that cache and remain valid while the expression is alive.
The native `Expr.factor()` method and top-level `factor()` function expose
bounded polynomial factorisation; factorizations that would discard a domain
condition are refused by the C ABI.
`Expr.series(variable, point, order)` and `series(...)` expose bounded native
Taylor polynomials through the requested highest degree; `series_coeff(...)`
returns one exact coefficient. The native result omits SymPy's `O(...)` term
and refuses singular points, non-finite coefficients, and unsupported symbolic
derivatives.
`Expr.solve(variable)`, `solve(expression, variable)`, and the matching
`fortsym.sympy.solve(...)` expose distinct verified roots for one equation in
one symbol. Exact univariate polynomials and bounded rational functions use the
native root verifier; the fallback is limited to verified scalar-linear
equations. Rational candidates are checked against the original residual so
denominator poles are excluded. Unsupported domains, options, and equations
return an explicit refusal.
`solveset(...)` uses the native set-solving path and returns the
SymPy-compatible bounded `FiniteSet`/`EmptySet` result shape. Symbolic rational
denominator zeros are preserved as a native-owned bounded `Complement`
application; domains beyond the default complex-domain fragment are explicit
refusals.
`FiniteSet(...)` is a native-owned immutable set application with SymPy-shaped
membership, iteration, and brace printing; `EmptySet` remains the singleton
empty-result adapter.
`Tuple(...)` is a native-owned immutable tuple application with SymPy's
iteration, indexing, and printing shape. `linsolve((matrix, right_hand_side),
symbols)` exposes the verified native square exact-rational one-right-hand-side
fragment and returns a `FiniteSet(Tuple(...))` result. Symbolic coefficients,
singular or non-square systems, matrix objects, free parameters, and alternate
forms are explicit refusals.
`And`, `Or`, `Not`, `Xor`, `Implies`, and `Equivalent` use the shared native
application owner and accept the same symbolic operands as the bounded SymPy
surface. `&`, `|`, `^`, and `~` are the corresponding expression operators;
`Not`/`~` negates a relational node to its complementary relation. Boolean
identities for empty calls and Python `True`/`False` are handled at the adapter
boundary. `Eq` and `Ne` return Python booleans for exact numeric and identical
symbol expressions; exact numeric ordering relations do the same, while
undecidable symbolic ordering remains a native relation. Expressions from
different arenas are rejected. Broader Boolean simplification and condition
solving remain explicit roadmap work.
`Matrix(rows)` constructs a bounded exact dense matrix, supports `(row,
column)` indexing, `det()`/`det(matrix)`, `rank()`, `inv()`, `transpose()`,
`.T`, `nullspace()`, `rref()`, elementwise `+`/`-` and unary negation, and
multiplication or scalar division with `*`/`@`/`/`, delegating exact matrix operations to the
native owner. `nullspace()` returns SymPy-compatible column matrices, while
`rref()` returns `(reduced_matrix, pivot_tuple)`. Ragged, singular matrices and
broader options remain explicit
refusals.

## Native geometry facade

`fortsym.Chart` is a small transport facade over the native Fortran chart and
magnetic, tensor, connection, and form owners. It accepts three coordinate
expressions and their Cartesian position map, then exposes `sqrtg()`, metric
and curvature views, covariant differentiation, `b_fourier()`,
`b_fourier_density()`, `j_fourier()`, and `b_cov()`. `j_fourier(nu, A, n)`
computes the native fixed-3D Fourier curl-curl operator; `nu` accepts a nested
3x3 matrix or a nine-entry first-slot-fastest (column-major) sequence.
`Chart.reluctivity_density(physical)` converts an isotropic physical scalar or
a Cartesian 3x3 reluctivity matrix to the covariant Fourier constitutive
density `nu_ij = e_i^a physical_ab e_j^b / sqrtg`. It returns a `Tensor` with
variance `(-1, -1)` and density weight `-1`, ready for the Fourier metadata and
residual owners.
`Chart.fourier_weak_form(nu, n)` returns the native Albert--Bíro--Lainer
branch descriptor. `n=0` selects a scalar nodal form with a normal boundary
trace and the `longitudinal_diffusion` block `nubar_t`; `n!=0` selects a
two-component edge form with the transformed transverse mass block and a
tangential boundary trace. The returned `FourierWeakForm` contains native
expression handles and integer metadata; the Python layer does not
recalculate the constitutive reduction. `Chart.fourier_longitudinal_residual`
and `Chart.fourier_transverse_residual` expose the corresponding strong
residuals, with the latter accepting an integer or symbolic mode.
`Chart.fourier_longitudinal_flux(nu, A3, component)` returns the selected
component of `nubar_t grad_t(A3)`. `Chart.fourier_transverse_flux(nu, a)`
returns `nu33 curl_t(a)`. These are the coefficients in the paper's boundary
terms after integration by parts. The caller supplies the boundary normal,
tangent convention, surface measure, and finite-element quadrature.
`Chart.fourier_longitudinal_boundary_flux(nu, A3, normal)` returns `n_i q_i`,
while `Chart.fourier_transverse_boundary_flux(nu, a, normal)` returns the two
coefficients `s_k q`; `fourier_transverse_boundary_contraction` contracts those
coefficients with an edge test pair. These helpers keep the normal components
in the coordinate convention and leave surface measure, quadrature, and the
weak-form minus sign to the caller.
`Chart.fourier_source(current, n)` and `Chart.fourier_load(load, trace, n)`
create typed source/load records that match the corresponding weak-form
branch: the zero mode takes one scalar component and a normal trace, while a
nonzero mode takes two transverse components and a tangential trace. Their
`.value` is scalar for the longitudinal branch and a two-tuple for the
transverse branch; `.matches(weak)` rejects branch, trace, mode, or component
count mismatches. These records stop at the source/load contract; quadrature,
mesh, finite-element basis evaluation, and assembly remain FortFEM-owned (or
caller-owned for another numerical client).
Pass a `fortsym.sympy.Patch` as `Chart(..., patch=patch)` when the chart is
declared on a coordinate patch; `Chart.has_patch` and `Chart.patch` retain that
metadata without inferring topology from expressions. `CoordSystem(name,
patch)` attaches the same patch to its native chart.
`Chart.metric_owner(g, signature, orientation)` creates an explicit native
metric owner. `Metric.sqrtg()`, `Metric.contravariant()`, and
`Form.star(metric)` use that owner, so Lorentzian signatures and orientation
remain explicit instead of being inferred from a chart. `Metric.volume_density()`
returns positive `sqrt(abs(det(g)))`, and `Metric.levi_civita()` returns the
oriented covariant tensor by default or the contravariant tensor when passed
`"contravariant"`; neither operation hides orientation in `sqrtg()`.
`Metric.grad(scalar)`, `Metric.divergence(vector)`, and
`Metric.laplacian(scalar)` are the corresponding coordinate-aware operators;
they use the metric inverse and positive `sqrt(abs(det(g)))` owned by the same
metric. The returned gradient is contravariant, and divergence accepts a
three-component contravariant vector. `Metric.inner(left, right)` contracts
contravariant components with the metric, and `Metric.norm_squared(vector)`
is its self-contraction convenience for quantities such as `B**2`; both use
the same native nonorthogonal-metric path.
`Signature(values)` and `Orientation(sign)` are typed declarations accepted by
`Metric` and `SpacetimeMetric`. Their `dimension`, signed counts, and
`is_lorentzian` properties are available directly; `signature_type` and
`orientation_type` return typed views while the existing tuple/int properties
remain compatible.
`Chart.field_line_derivative(vector, scalar)` returns the directional derivative
`vector[i] * diff(scalar, coordinate[i])`. A `MagneticField` exposes the same
operation as `field.field_line_derivative(scalar)` using its typed `B^i` view.
`Chart.surface_measure(normal_index=1)` and `Metric.surface_measure(...)`
return the positive induced measure on a coordinate surface. They keep the
surface measure separate from the signed chart Jacobian and oriented volume.
`Chart.flux_surface(label_index=1)` returns a typed `FluxSurface` owner with
`.label`, `.measure()`, and `.average(scalar)`. The native average integrates
the induced measure over the two remaining coordinates on `[0,2*pi]` and
raises `FortSymError` when its verified exact-integral subset cannot establish
the numerator or normalization.
`Chart.magnetic_chart(potential, label_index=1)` returns a `MagneticChart`
facade bundling that surface with the native `MagneticField` views. Its
`.upper`, `.lower`, and `.density` properties retain the `B^i`, `B_i`, and
weight-one `sqrtg B^i` metadata; `.divergence()`, `.field_line_derivative()`,
and `.average()` delegate to the same chart and field owners. `.potential_form()`
returns `A` and `.flux_form()` returns the closed magnetic two-form
`beta = d(A)`.
`Form.b_con(orientation=1)` and `Form.b_density(orientation=1)` recover typed
`B^i` and weight-one `sqrt(g) B^i` from a degree-two magnetic form through the
same native C ABI operation; `Chart.b_con_form()` and `Chart.b_density_form()`
are the corresponding chart-owner methods.
`Chart.b_flux_form(vector, orientation=1)` and
`Tensor.b_flux(orientation=1)` construct the forward magnetic two-form
`beta = i_B(orientation*Omega)` from a weight-zero contravariant vector. The
forward and reverse methods share the native form owner and preserve the
explicit orientation convention.
`Chart.h_cov(reluctivity, vector)` applies `H_i = nu_ij B^j`, and
`Chart.h_con(covariant)` raises `H_i` with the chart metric.
`MagneticField.h_cov()` and `.h_con()` provide typed covariant and
contravariant `H` views from its `B^i` view.
`Chart.flux_coordinates(label_index=1, kind=FLUX_GENERIC)` returns a
`FluxCoordinates` descriptor with the ordered angular indices. Its
`.normal(vector)`, `.straight_field_residual(vector, iota)`,
`.clebsch_residuals(vector, alpha, beta)`, and `.boozer_residuals(covariant)`
methods call the native residual owners; Clebsch uses
`B = grad(alpha) cross grad(beta)` with the signed chart Jacobian and requires
`kind=FLUX_CLEBSCH`. Boozer returns
`(B_label, d1 B1, d2 B1, d1 B2, d2 B2)` and requires `kind=FLUX_BOOZER`.
`.boozer_valid(covariant)` and `.clebsch_valid(...)` use the native zero oracle
and return `True`, `False`, or `None`. The descriptor checks identities only
and does not hide an equilibrium solver.
`SpacetimeMetric` is the dimension-aware four-coordinate facade for the
relativity owner. It exposes `sqrtg()`, inverse metric, Christoffel, Riemann,
Ricci, scalar curvature, Einstein, and second-Bianchi views using the same native expression
arena and first-slot-fastest indexing. Its `one_form()`, `two_form()`,
`three_form()`, and `four_form()` constructors return native `SpacetimeForm`
owners; `scalar_form()` constructs degree zero. These expose
`d()`/`exterior_diff`, `wedge()`, `star()`/`hodge_star`,
and `codifferential()`/`codiff()` with degree-aware components and the explicit
metric signature and orientation. `SpacetimeForm.field_strength()` returns
`F=d(A)`, `gauge_transform(chi)` returns `A+d(chi)`, and
`maxwell_residual(current)` returns the native source residual `d(star(F))-J`.
`SpacetimeMetric.volume(orientation)` returns the oriented top-degree form
`Omega = sigma*sqrt(abs(det(g))) du^0^...^du^(n-1)`, using the runtime metric
dimension (1--4) and explicit orientation. It keeps the signed form separate
from the positive `sqrtg()` scalar and is the canonical owner for contractions
such as `X.interior(metric.volume())`.
The same `SpacetimeForm` owner supports metric dimensions 1--4; unused
coordinates remain zero ABI slots, while `star()` and all de Rham operators
use the metric's runtime dimension. See
`example/example_2d_spacetime_forms.f90` and its matching SymPy-oracle test
for a compact 2D example.
`SpacetimeForm.to_tensor()` and `SpacetimeTensor.to_form()` are the shared
runtime bridge between compact form coefficients and first-slot-fastest tensor
components. The tensor view is exact lower, weight zero, and fully
antisymmetric with pair metadata on every slot pair; upper slots, density
weights, repeated-index nonzeros, and
non-antisymmetric components raise a native error.
`SpacetimeMetric.covariant()` and `.contravariant()` return typed metric
tensors. `SpacetimeMetric.vector()`/`.covector()` create explicit component
views, and `SpacetimeTensor.raise_()`/`.lower()` plus `.density(factor)` keep
variance and density weight visible instead of inferring them from names.
`SpacetimeTensor.product()`, `.contract()`, `.trace()`, and `.permute()` use
the same native runtime-dimension tensor owner; slot variance and summed
density weight remain explicit in the returned view.
`SpacetimeTensor.symmetry(i, j)` and `.symmetries` expose the same pairwise
metadata vocabulary as `Tensor`; `.declare_symmetry(i, j, kind)` validates the
active components through the native C ABI and raises `FortSymError` for a
false declaration. Metric tensors carry their checked symmetric pair, and
surviving declarations propagate through products, permutations, contractions,
covariant derivatives, Lie transport, density views, and unchanged slots of
variance-changing views.
`.symmetrize(i, j)` and `.antisymmetrize(i, j)` perform the corresponding
native projections and return only the selected pair declaration.
`SpacetimeTensor.covariant_diff()` (alias `.covariant_derivative()`) appends a
lower derivative slot, applies the metric Christoffel terms to every slot, and
preserves density weight for supported input rank at most four. The rank-five
output is the native ceiling and covers derivatives of curvature-like tensors.
Rank-one `SpacetimeTensor` views support ordinary zero-based indexing and Python
slices, returning a tuple of borrowed component views; slicing higher-rank
tensors is refused explicitly.
`SpacetimeTensor.covariant_divergence()` (alias `.divergence()`) contracts
the first contravariant slot with the derivative slot using a direct native
kernel, preserving the remaining variance and density weight. It supports
all positive ranks representable by the runtime tensor owner.
`SpacetimeTensor.lie(vector)` (alias `.lie_derivative()`) transports any
supported tensor rank along a weight-zero contravariant vector, including the
stored tensor-density term. `SpacetimeMetric.killing(vector)` is the same
owner applied to the covariant metric tensor; `SpacetimeMetric.lie(vector, tensor)`
exposes the explicit metric-owner spelling.
`SpacetimeMetric.flat(vector)` lowers a vector into a native one-form and
`SpacetimeMetric.sharp(one_form)` raises it back; the returned vector carries
upper variance metadata. `grad(scalar)` returns a contravariant metric
gradient, while `divergence(vector)` and `laplacian(scalar)` use the positive
`sqrt(abs(det(g)))` volume factor. In Lorentzian signature the latter is the
coordinate wave operator.
`geodesic_residual(curve, parameter)` returns the four native components of
the parameterized geodesic equation.
`Chart.geodesic_residual(curve, parameter)` provides the matching three-
component chart-owner operation and substitutes the curve into the chart
connection before evaluating it.
`Chart.connection(coefficients=None)` creates the chart Levi-Civita connection
or a supplied `Gamma^a_bc` owner. `Connection.torsion()`,
`Connection.nonmetricity(metric)`, `Connection.covariant_diff(tensor)`, and
`Connection.covariant_divergence(tensor)` all route through the same native
connection owner. `Connection.riemann()` differentiates supplied coefficients
without requiring a metric. Pass `convention=CONNECTION_STANDARD` or
`convention=CONNECTION_OPPOSITE` to select the Riemann sign; the stored
coefficients and all covariant calculus remain unchanged. `Tensor.covariant_diff(connection)` and
`Tensor.covariant_divergence(connection)` are matching convenience spellings.
`Connection.geodesic_residual(curve, parameter)` evaluates the same geodesic
equation directly from supplied coefficients, without requiring a metric.
`SpacetimeForm.interior(vector)` and `.lie(vector)` provide contraction and
the Cartan Lie derivative, with `interior_product` and `lie_derivative` as
matching aliases. `.laplace_de_rham()` composes the same native `d` and
codifferential owners.
`Tensor.lie(vector)` and `.lie_derivative(vector)` provide the coordinate Lie
derivative for typed tensors. The vector must be an ordinary weight-zero
contravariant tensor; a tensor density of weight `w` receives the explicit
`+w*T*diff(vector[i], coordinate[i])` transport term.
`Chart.killing(vector)` and `Metric.killing(vector)` return the symmetric lower
tensor `L_vector(g)`. A zero result is the native Killing-vector condition;
both methods reuse the tensor-Lie owner and require an ordinary weight-zero
contravariant vector.
`Tensor.permute()`, `symmetrize()`, and `antisymmetrize()` perform native
slot operations using zero-based Python slot numbers.
`Tensor.symmetry(i, j)` returns `SYMMETRY_NONE`, `SYMMETRIC`, or
`ANTISYMMETRIC`. `Tensor.declare_symmetry(i, j, kind)` checks every component
through the native C ABI before recording the declaration. A false declaration
raises `FortSymError`. `Tensor.symmetries` returns declared pairs as
`(i, j, kind)` tuples. The SymPy spelling facade provides
`TensorSymmetry.no_symmetry`, `.fully_symmetric`, `.fully_antisymmetric`, and
pair constructors for `Chart.tensor(..., symmetries=...)`.
`Tensor.product(other)` (also `tensor_product`) routes the outer product
through the same native owner; left slots precede right slots and density
weights add.
The native `Tensor.trace()`/`trace()` spellings and compatibility
`tensorcontraction(tensor, (i, j))` all use the same checked contraction owner;
`fortsym.sympy.tensorproduct` is the SymPy spelling for the native product for
both chart tensors and runtime spacetime tensors.
`IndexType(name, dimension, category)` and `IndexType.index(slot, variance,
label, dummy)` provide checked, zero-based named slots; `Tensor.contract(i, j)`
accepts those labels and validates their space, variance, and dummy name before
using the native contraction owner. `fortsym.sympy.TensorIndexType` and
`TensorIndex` are spelling-compatible aliases for the same classes.
`Chart.one_form()`, `two_form()`, and
`three_form()` construct native `Form` objects. Forms expose the fixed
three-dimensional basis-mask components plus `d()`, `wedge()`, `star()`,
`codifferential()`/`codiff()`, `laplace_de_rham()`, `interior()`, `lie()`,
`flat()`, and `sharp()`; pass a `Metric` to the metric-aware Hodge,
codifferential, or Laplace--de Rham operation. The de Rham Laplacian follows
the mathematician's sign convention, so its scalar flat-space value is
`-sum(diff(f, x_i, 2))`. `form * form` is the concise wedge spelling and
scalar multiplication is coefficient scaling. The same
classes are re-exported as `fortsym.sympy.Chart`, `fortsym.sympy.Tensor`, and
`fortsym.sympy.Form`; they do not reimplement geometry in Python.
`Form.is_closed` and `SpacetimeForm.is_closed` use the native three-valued
zero engine on every coefficient of `d(form)`, returning `True`, `False`, or
`None` when the symbolic verdict is undecidable.

```python
import fortsym.sympy as sp

Z, R, phi, n = sp.symbols("Z R phi n")
chart = sp.Chart(
    (Z, R, phi),
    (R*sp.cos(phi), R*sp.sin(phi), Z),
)
A1 = sp.Function("A1")(Z, R)
A2 = sp.Function("A2")(Z, R)
A = (A1, A2, sp.Integer(0))
B_up = chart.b_fourier(A, n)
B_density = chart.b_fourier_density(A, n)
weak = chart.fourier_weak_form(
    ((2, 3, 0), (5, 7, 0), (0, 0, sp.Symbol("nu33"))), 2,
)
assert weak.branch_name == "transverse"
source = chart.fourier_source((A1, A2), 2)
load = chart.fourier_load((A1, A2), sp.TRACE_TANGENTIAL, 2)
assert source.matches(weak)
assert load.matches(weak)
g = chart.metric_covariant()
dg = g.covariant_diff()
v_density = chart.vector((Z, R, phi), density_weight=1)
divergence = v_density.covariant_divergence()
R = chart.riemann()
bianchi = chart.first_bianchi_residual()
second_bianchi = chart.second_bianchi_residual()
scalar_R = chart.scalar_curvature()
x, y, z = sp.symbols("x y z")
alpha = chart.one_form((y*z, x**2, y + z**2))
beta = alpha.d()
assert (beta[3] - (2*x - z)).simplify() == 0
```

`Tensor` components use first-slot-fastest order and zero-based Python indices;
`variance` is a tuple of `1` (upper) and `-1` (lower), and `density_weight`
is explicit. `chart.metric()`, `chart.christoffel()`, `chart.ricci()`, and
`chart.einstein()` return the corresponding native tensor views. `Tensor` and
`Form` and `Chart` are transport/lifetime facades: formulas and metadata
validation stay in the native Fortran owners. Form components use the native
bit masks `1`, `2`, `4` for `dx`, `dy`, `dz`; `3`, `5`, `6` for the ordered
two-form basis; and `7` for the volume form. Full arbitrary-dimensional
`diffgeom` parity, map generalization, and metric signatures remain roadmap
work; fixed-three-dimensional native pullback transport is available through
`ChartMap.pullback()`.
`Tensor.density(integer)` keeps the components and changes only the metadata.
`Tensor.density(factor)` multiplies the native components by one scalar factor
and increments the weight, so `Bup.density(chart.sqrtg())` is the direct
generic spelling of `sqrtg B^i`.

`Chart.magnetic_field(potential)` returns a `MagneticField` containing typed
`upper`, `lower`, and weight-one `density` tensor views. Its components come
from the native curl, covariant magnetic, and density operations.

The spherical chart has the same component-first API. On the regular patch,
use the chart for the embedding and basis, and a compact metric owner for
repeated connection and vector-calculus work:

```python
r, theta, phi = sp.symbols("r theta phi")
chart = sp.Chart(
    (r, theta, phi),
    (r*sp.sin(theta)*sp.cos(phi),
     r*sp.sin(theta)*sp.sin(phi),
     r*sp.cos(theta)),
)
metric = chart.metric_owner(
    ((1, 0, 0), (0, r**2, 0),
     (0, 0, r**2*sp.sin(theta)**2)),
)
assert tuple(value.simplify() for value in metric.grad(r)) == (1, 0, 0)
assert metric.divergence((r, r-r, r-r)).simplify() == 3
assert metric.laplacian(r**2).simplify() == 6
```

The native and SymPy-oracle tests also compare the reciprocal basis, signed
Jacobian, six standard spherical Christoffel components, and the chart curl.

For an analytic Boozer-coordinate representation, keep the flux functions in
the covariant angular components and raise them with the displayed metric:

```python
psi, theta, phi = sp.symbols("psi theta phi")
h, I, G = sp.symbols("h I G")
chart = sp.Chart((psi, theta, phi), (psi, theta, phi))
metric = chart.metric_owner(((1, 0, 0), (0, h**2, 0), (0, 0, h**2)))
B_cov = (0, I, G)                 # I(psi), G(psi): surface constants
B_up = (0, I/h**2, G/h**2)
volume = (h**2, 0, 0, 0, 0, 0, 0, 0)  # h**2 dpsi wedge dtheta wedge dphi
flux_two_form = (G, -I, 0)        # i_B(volume), in (psi-theta, psi-phi, theta-phi)
```

The native Fortran example and the SymPy-oracle test check the metric form,
the angular derivatives of `B_cov`, the raised components, zero divergence,
and this flux two-form.

Hamada coordinates use the contravariant counterpart of this contract: the
angular components `B^theta` and `B^phi` are flux functions. Use
`chart.flux_coordinates(1, kind=sp.FLUX_HAMADA)` and
`hamada_residuals(chart.vector((Bpsi, Btheta, Bphi)))` to obtain the same five
residuals, or `hamada_valid(...)` for the native zero verdict.

`ChartMap` transforms components between two charts. Its `forward` tuple maps
source coordinates to target coordinates and its `inverse` tuple maps back.
Upper tensor slots use the forward Jacobian, lower slots use the inverse
Jacobian, and density weights are applied through the absolute Jacobian
determinant. Forms use the inverse Jacobian on each coframe factor, with the
signed determinant for top forms. The result is expressed in the target
coordinate symbols. Native tensor and form owners retain a value-semantic
chart key (coordinate tuple plus position map), so a transform refuses an
object from a different source chart even when it shares the same expression
arena. A map exposes the source and target declarations through
`source_patch` and `target_patch` when its charts belong to explicit patches;
composition requires the intermediate chart and patch to match. Identically
singular forward or inverse Jacobians are
refused by the native boundary; pointwise singular loci remain explicit
conditions of the supplied map. `transition.compose(following)` applies two
native transitions in sequence:

```python
x, y, z, p, q, s = sp.symbols("x y z p q s")
source = sp.Chart((x, y, z), (x, y, z))
target = sp.Chart((p, q, s), ((p - q)/2, q, s))
transition = sp.ChartMap(
    source, target, (2*x + y, y, z), ((p - q)/2, q, s)
)
target_vector = transition.transform(source.vector((x, y, z)))
assert tuple(value.simplify() for value in target_vector) == (p, q, s)
target_form = transition.transform(source.one_form((x, y, z)))
```

The geometry API currently covers the first magnetic-paper, fixed-rank
connection, and fixed-three-dimensional form subsets. The
Wolfram/Python source-script translator and full three-frontend differential
comparison remain roadmap work.

## `fortsym.sympy` diffgeom compatibility subset

The compatibility facade also exports the familiar first `sympy.diffgeom`
names without importing SymPy. `Manifold`, `Patch`, and `CoordSystem` create a
fixed-three-dimensional identity chart by default; pass `symbols=(...)` and
`position=(...)` to bind another native chart. `base_scalars()`,
`base_vectors()`, and `base_oneforms()` provide coordinate fields, while
`Differential`, `WedgeProduct`, `TensorProduct`, and `LieDerivative` evaluate
through the native expression, tensor, and form owners.

```python
M = sp.Manifold("M", 3)
P = sp.Patch("P", M)
C = sp.CoordSystem("C", P, symbols=("x", "y", "z"))
x, y, z = C.base_scalars()
dx, dy, dz = C.base_oneforms()
beta = sp.WedgeProduct(dx, dy)
assert beta.rcall(C.base_vector(0))[2].simplify() == 1
assert sp.Differential(x + y).rcall(C.base_vector(0)).simplify() == 1
```

This is the first native-backed naming bridge, not full `diffgeom` parity:
coordinate transformations, arbitrary dimensions, covariant derivative
operators, and broader tensor products remain explicit roadmap items.

## `fortsym.sympy` compatibility subset

`fortsym.sympy` is a drop-in import spelling for the declared subset below. It
does not import SymPy. Unsupported names raise
`fortsym.sympy.UnsupportedOperationError`; they do not return a guessed result.

| Surface | Supported semantics |
|---|---|
| `Symbol`, `symbols`, `Integer`, `Rational`, `Float`, `pi`, `E`, `I`, `oo`, `zoo`, `nan` | exact native construction and structural equality; `Rational` accepts integer, `Fraction`, rational/decimal-string, finite-float, and native exact-number inputs, with SymPy-compatible `zoo`/`nan` zero-denominator results; finite native `Float` values compare and hash like Python floats while retaining IEEE negative-zero storage; `nan` follows the declared propagation rules, and finite-scalar/integer-power `oo`/`zoo` rules match SymPy while symbolic factors remain unevaluated |
| `Expr.is_number`, `Expr.is_algebraic`, `Expr.is_rational`, `Expr.is_integer`, `Expr.is_zero`, `Expr.is_nonzero`, `Expr.is_real`, `Expr.is_positive`, `Expr.is_nonnegative`, `Expr.is_negative`, `Expr.is_nonpositive` | SymPy-compatible numeric and three-valued predicates backed by one native expression owner; `is_number` is Boolean, while the domain and sign predicates return `True`, `False`, or `None` |
| `Expr.node_count`, `Expr.free_symbols` | `node_count` reports shared native DAG nodes; `free_symbols` is a cached `frozenset` of native symbol handles for the distinct free symbols, excluding constants and applied-function heads |
| `algebraic=True`, `rational=True`, `integer=True`, `real=True`, `zero=True`, `positive=True`, `nonnegative=True`, `nonzero=True`, `negative=True`, `nonpositive=True` | native arena facts; algebraic, rational, and integer facts close over their supported exact domains, sign facts close to real/nonzero/zero implications, and contradictory combinations raise `InconsistentAssumptions` |
| `Q.algebraic`, `Q.rational`, `Q.integer`, `Q.real`, `Q.zero`, `Q.positive`, `Q.nonnegative`, `Q.nonzero`, `Q.negative`, `Q.nonpositive`, `ask`, `assuming`, `And` | nested, reversible assumption queries and transactional compound scopes backed by the native arena context; `Q.algebraic` uses the native algebraic result and preserves SymPy's `None` boundary for constructor-attached symbol assumptions and undecided function heads; bounded relational facts are accepted in scopes |
| `Add`, `Mul`, `Pow`, `Function` | native operator construction; universal power identities (`x**0`, `x**1`, `1**x` outside sentinel exponents, and `sqrt(x)**2`) are canonicalized in the shared arena, with `1**oo`, `1**zoo`, and `1**nan` becoming `nan`; `isinstance` checks use native node kinds |
| `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `sinh`, `cosh`, `tanh`, `csch`, `sech`, `coth`, `erf`, `erfc`, `gamma`, `loggamma`, `factorial`, `besselj`, `besseli`, `legendre`, `asinh`, `acosh`, `atanh`, `exp`, `log`, `sqrt`, `Abs`, `sign`, `floor`, `ceiling` | native applied-function nodes; direct sentinel rules for the declared heads match SymPy where the result is representable, including `sin(zoo)`, `cos(zoo)`, and `tan(zoo)` becoming `nan`, finite gamma-family poles, and exact nonnegative factorials through `factorial(1000)`, while larger, non-integer, accumulation-bound, and pole-sensitive cases are explicit refusals |
| `re`, `im`, `Abs`, `expand_complex`, `conjugate`, `arg` | native complex-domain projections, rectangular expansion, modulus, and principal argument; direct `oo`/`-oo`/`zoo`/`nan` sentinel boundaries match SymPy, while unknown reality, unresolved branches, and decidable zero arguments for `arg` raise `UnsupportedOperationError` |
| `diff`, `Derivative` | native differentiation, including repeated variables; `evaluate=False` retains a typed wrapper with `.doit()` |
| `subs`, `subs_many`, `Expr.xreplace`, `Expr.replace`, `Expr.match`, `Wild`, `expand` | native substitution and expansion; unordered mappings follow SymPy's node-count/structural ordering, explicit sequences retain caller order, `subs_many` is the paired-sequence native spelling, `Expr.xreplace` performs exact-node replacement without a final expansion, exact `Expr.replace` supports changed-match `map=True` and `exact=True`/`False`, exact `Expr.match` returns `{}` or `None`, and bounded `Wild` patterns support fixed structural slots, `exclude` and `properties` filters, single-Wild additive and multiplicative remainders, and distinct-Wild root partitions |
| `count_ops` | `count_ops(expression)` returns the SymPy-compatible non-visual operation count, including canonical divisions; `visual=True` is an explicit refusal until visual operation-expression construction is implemented |
| `Subs` | typed wrapper with `.doit()` for explicit `(old, new)` pairs |
| `simplify`, `refine`, `factor` | native bounded simplification, principal-square-root powers, universal power-constructor identities, exact real unit-circle `asin`/`acos` values, exact real tangent `atan` values, exact real `asinh(±1)` values, exact negative perfect-square roots, exact `asin(±i)`, `acos(±i)`, and `asinh(±i)` branch points, exact `log(0) = zoo` and its `exp(log(0)) = nan` propagation, principal-branch exact negative real and imaginary logarithms, exact `atan(±i)`, exact `atanh(1)`/`atanh(-1)` poles and `atanh(±i)` branch points, exact `acosh(0)`/`acosh(-1)` and `acosh(±i)` branch points, finite gamma-family poles, exact factorial values through `factorial(1000)`, compact-rational `oo`/`zoo` powers and normalized positive rational `-oo` phases, signed/zero-guarded `sqrt`/`Abs` refinement, direct known-domain rules for the supported elementary heads, real/nonzero-guarded `log`/`exp` composition, and polynomial factorisation; unsupported domain rewrites and refinement assumptions are refused |
| `Eq`, `Ne`, `Gt`, `Ge`, `Lt`, `Le` and `Expr` comparisons | SymPy-compatible relational constructor spellings at the adapter boundary; exact numeric decisions and identical-expression `Eq`/`Ne` return booleans, symbolic ordering remains relational, and exact sign/zero bounds plus transactional `And` facts are ingested by native scopes |
| `And`, `Or`, `Not`, `Xor`, `Implies`, `Equivalent`; `&`, `|`, `^`, `~` | bounded native Boolean applications with SymPy constructor names, Boolean identity evaluation including mixed constant `Equivalent`, relational negation, native structural printing, and shared expression-operator syntax; broader Boolean simplification is an explicit roadmap item |
| `together`, `cancel`, `apart`, `collect` | exact bounded multivariate rational/polynomial operations with named resource-limit refusals; the basic SymPy spellings and variable selection are supported, while advanced options remain explicit refusals |
| `integrate` | verified one-variable indefinite integration in the SymPy-compatible default complex domain; unsupported antiderivatives, multiple variables, and options are explicit refusals |
| `limit` | verified finite and infinite limits for the native bounded theorem fragment; finite poles and unsupported asymptotics are explicit refusals |
| `series` | bounded Taylor polynomial through the requested SymPy term count, with the `O(...)` term omitted; singular/non-finite coefficients, unsupported symbolic derivatives, and unsupported options are explicit refusals |
| `solve` | distinct verified roots for one equation in one symbol; exact univariate polynomials, bounded rational functions, and verified scalar-linear equations; unsupported domains/options are explicit refusals |
| `solveset` | bounded native-owned `FiniteSet` plus singleton `EmptySet` result over distinct verified polynomial/rational/scalar-linear roots, with native denominator-pole transport to a native-owned `Complement` for symbolic rational functions; non-default domains and unsupported equations are explicit refusals |
| `linsolve` | verified square exact-rational systems with one explicit right-hand side, returned as `FiniteSet(Tuple(...))`; symbolic, singular, non-square, free-parameter, matrix-object, and alternate forms are explicit refusals |
| `Matrix` | bounded exact dense construction, row-major flat scalar indexing and slicing, `(row, column)` indexing, row/column/block/reverse/stepped/empty 2-D slices, native-backed determinant, exact rank/inverse, transpose/`.T`, bounded `nullspace()`, `rref()`, elementwise `+`/`-`, unary negation, and exact `*`/`@` products or scalar scaling/division; ragged, singular, and broader matrix-expression operations are explicit refusals |

`Wild(name, exclude=(), properties=())` is an adapter-only pattern object. Its
direct, fixed-shape, single-Wild remainder, and bounded distinct-Wild
additive/multiplicative root partition matches are differential-tested against
SymPy 1.14.0; the partition matcher accepts at most three distinct direct
Wild nodes. Weighted-coefficient and broader recursive matching remain
explicit future gaps.

For `atan2(y, x)`, the adapter evaluates the representable directed-infinity
quadrants for `y` and `x` both equal to `+oo` or `-oo`; complex infinity and
other ambiguous domain cases remain applied heads.
For the Bessel heads, `besselj(order, +/-oo)` is zero and
`besseli(order, oo)` is positive infinity. For symbolic order,
`besseli(order, -oo)` is `oo*(-1)**order`; integer orders reduce to signed
infinity, while unsupported exact non-integer and complex-infinity cases remain
applied heads. NaN in an order-bearing head remains applied unless an existing
directed-domain rule proves a result; `besseli(nan, -oo)` is the representable
`nan` boundary.
`legendre(degree, argument)` is the SymPy spelling for the native
`legendrep(degree, 0, argument)` owner. Its infinity rules cover nonnegative
integer degrees through two and order zero; degree three and higher map to
`nan` at infinity. Negative integer degrees use SymPy's
`P(-n-1, x) = P(n, x)` identity; symbolic, noninteger, and other unsupported
degrees remain applied heads.
The periodic heads `sin`, `cos`, and `tan` map complex infinity to `nan`.
Their `+/-oo` results are SymPy `AccumBounds` values and therefore remain
explicit applied heads until fortsym has a bounded-set representation.

The complex-domain functions `re`, `im`, `Abs`, `expand_complex`, `conjugate`, and `arg` use the same
native complex-domain owner rather than constructing duplicate applied heads.
Their immutable results are cached per assumption epoch; adding or removing
assumptions invalidates the cache. `arg` returns the principal branch when its
supported rectangular split is decidable. `expand_complex` uses that split for
the supported exact and assumption-resolved fragment and matches the direct
`oo`/`-oo`/`zoo`/`nan` sentinel boundaries (`zoo` becomes `nan`). The same
owner returns `re(oo)=oo`, `im(oo)=0`, `arg(-oo)=pi`, `Abs(zoo)=oo`, and the
corresponding `nan` projections; `conjugate(zoo)` remains an applied head.
`Abs` uses the same owner for exact, algebraic, and assumption-resolved complex
expressions, while unknown reality keeps SymPy's unevaluated `abs(...)`
fallback.
Repeated `diff` calls through the SymPy adapter likewise reuse the simplified
derivative of an immutable expression and variable; the raw low-level
`Expr.diff` and C-ABI derivative remain available separately.

The compatibility layer guarantees native structural equality only for
operations listed as construction or transformation above. Unsupported
assumption forms and relational bounds raise `UnsupportedOperationError`, while
contradictory sign, zero, and compound facts raise `InconsistentAssumptions`;
they are not silently ignored. Matrix,
ordering, and unevaluated-expression semantics remain outside the subset.
