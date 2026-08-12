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

## Native geometry facade

`fortsym.Chart` is a small transport facade over the native Fortran chart and
magnetic, tensor, connection, and form owners. It accepts three coordinate
expressions and their Cartesian position map, then exposes `sqrtg()`, metric
and curvature views, covariant differentiation, `b_fourier()`,
`b_fourier_density()`, `j_fourier()`, and `b_cov()`. `j_fourier(nu, A, n)`
computes the native fixed-3D Fourier curl-curl operator; `nu` accepts a nested
3x3 matrix or a nine-entry first-slot-fastest (column-major) sequence.
`Chart.fourier_weak_form(nu, n)` returns the native Albert--Bíro--Lainer
branch descriptor. `n=0` selects a scalar nodal form with a normal boundary
trace; `n!=0` selects a two-component edge form with the transformed
transverse mass block and a tangential boundary trace. The returned
`FourierWeakForm` contains native expression handles and integer metadata;
the Python layer does not recalculate the constitutive reduction.
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
`Chart.h_cov(reluctivity, vector)` applies `H_i = nu_ij B^j`, and
`Chart.h_con(covariant)` raises `H_i` with the chart metric.
`MagneticField.h_cov()` and `.h_con()` provide typed covariant and
contravariant `H` views from its `B^i` view.
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
`Tensor.permute()`, `symmetrize()`, and `antisymmetrize()` perform native
slot operations using zero-based Python slot numbers.
`Tensor.product(other)` (also `tensor_product`) routes the outer product
through the same native owner; left slots precede right slots and density
weights add.
The native `Tensor.trace()`/`trace()` spellings and compatibility
`tensorcontraction(tensor, (i, j))` all use the same checked contraction owner;
`fortsym.sympy.tensorproduct` is the SymPy spelling for the native product.
`IndexType(name, dimension, category)` and `IndexType.index(slot, variance,
label, dummy)` provide checked, zero-based named slots; `Tensor.contract(i, j)`
accepts those labels and validates their space, variance, and dummy name before
using the native contraction owner. `fortsym.sympy.TensorIndexType` and
`TensorIndex` are spelling-compatible aliases for the same classes.
`Chart.one_form()`, `two_form()`, and
`three_form()` construct native `Form` objects. Forms expose the fixed
three-dimensional basis-mask components plus `d()`, `wedge()`, `star()`,
`interior()`, `lie()`, `flat()`, and `sharp()`; `form * form` is the concise
wedge spelling and scalar multiplication is coefficient scaling. The same
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

`ChartMap` transforms components between two charts. Its `forward` tuple maps
source coordinates to target coordinates and its `inverse` tuple maps back.
Upper tensor slots use the forward Jacobian, lower slots use the inverse
Jacobian, and density weights are applied through the absolute Jacobian
determinant. Forms use the inverse Jacobian on each coframe factor, with the
signed determinant for top forms. The result is expressed in the target
coordinate symbols. Identically singular forward or inverse Jacobians are
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
| `Symbol`, `symbols`, `Integer`, `Rational`, `Float`, `pi`, `E`, `I`, `oo`, `zoo`, `nan` | exact native construction and structural equality; `Float(-0.0)` retains its IEEE sign, `nan` follows the declared propagation rules, and finite-scalar/integer-power `oo`/`zoo` rules match SymPy while symbolic factors remain unevaluated |
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
| `Eq`, `Ne`, `Gt`, `Ge`, `Lt`, `Le` and `Expr` comparisons | SymPy-compatible relational constructor spellings at the adapter boundary; exact sign/zero bounds and transactional `And` facts are ingested by native scopes |
| `together`, `cancel`, `apart`, `collect`, `integrate`, `limit`, `series`, `solve`, `Matrix` | explicit refusal until their semantics are covered |

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
