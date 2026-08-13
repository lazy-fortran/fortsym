# Feature matrix

The observed consumer requirements, source-class evidence, bounded semantics,
priorities, and independent oracles are machine-readable in
`consumer-requirements.toml`; aggregate scan coverage and operation counts are
in `consumer-audit.json`. Exact upstream releases, inspected capabilities,
licenses, and adaptation boundaries are in `upstream-baselines.toml`.

Status values are `yes`, `partial`, and `no`. `Partial` means that the public
operation handles a stated fragment or that only one concrete backend exposes
it.

Chart topology metadata is an explicit partial capability: native fixed-3D
charts can retain a declared `patch_t`, and the Python `Chart`/`CoordSystem`
facades carry the matching `Patch`; topology is never inferred from expressions.

| Area | Native | SymEngine backend | Other backends | Required next fragment |
|---|---:|---:|---:|---|
| Hash-consed scalar expression DAG | stable structural order | conversion | conversion | persist a versioned cross-process structural digest |
| Free-symbol traversal | distinct native names | conversion | conversion | bound symbols, indexed objects, and full expression hierarchy |
| Arbitrary-precision integers and rationals | native scalar arithmetic yes | yes | yes | requested-precision real and complex evaluation |
| Real literals and real evaluation | yes, with sampled MPFR ULP bounds | yes | partial | rigorous intervals and arbitrary-precision operations |
| Complex and algebraic domains | bounded `qqbar` atoms, mixed coefficient folding, exact `Re`/`Im`, conjugation, principal-branch `log` including exact `log(±i)`, exact `asin(±i)`, `acos(±i)`, exact real unit-circle `asin`/`acos` values, exact real tangent `atan` values, exact real `asinh(±1)` values, `atan(±i)`, `atanh(±i)`, and `acosh(±i)` branch points, principal-branch `sqrt`, exact negative perfect-square roots, exact `asinh(±i)` branch points, `tan`/`sinh`/`cosh`/`tanh` rectangular splitting, lossless text IO, and checked real binary64 kernel projection | exact Gaussian-rational conversion; higher-degree atoms refused | partial | full expression promotion, higher-degree conversion, and complex code generation |
| Applied functions and Bessel `J` | yes | opaque Bessel head | partial | broader special-function rules |
| Parse, print, and Fortran dialect | deterministic | yes | yes | versioned serialization format |
| Structural, simultaneous, exact-node substitution/replacement, and bounded wildcard matching | yes | conversion | conversion | callable/wildcard replacement, weighted-coefficient and recursive commutative matching, and rule-based applied-function substitution |
| Operation counting | SymPy-compatible non-visual tree count | conversion | conversion | visual operation expression and CSE |
| Differentiation | yes | yes | Yacas concrete method | memoization and domain-sensitive rules |
| Multivariate symbolic partials | yes | opaque | opaque | function assumptions and rewrite rules |
| Coordinate charts, reciprocal bases, metrics, `sqrtg`, magnetic components, and Fourier modes | native first subset: charts, signed Jacobian, covariant and reciprocal basis matrices, shared typed `signature_t`/`orientation_t` metadata, explicit fixed-3D `metric_t` signature/orientation owner, positive metric `sqrtg` and `volume_density`, positive induced coordinate-surface measures, typed `flux_surface_t` metadata and verified periodic averages, typed `flux_coordinate_t` metadata with native normal, straight-field-line, Boozer, and Hamada residuals, `magnetic_chart_t` bundling the chart/surface/typed B views without duplicate storage, oriented Levi-Civita symbol/tensor owner, chart `grad`/ordinary `div`/`curl`/`laplacian`, metric-owner `grad`/`divergence`/`laplacian`, explicit `div_density`/`curl_density`, typed magnetic `B^i`/`B_i`/`sqrtg B^i` views, symmetry-mode curl, generic Fourier curl-curl/current, native n=0/n≠0 reduction checks and weak-form metadata, bidirectional chart-map tensor/form transport, fixed-3D map composition, identically-singular-Jacobian refusal, and executable cylindrical/Boozer/spherical derivation baselines; Python `Chart`/`ChartMap`/`FluxSurface`/`FluxCoordinates`/`MagneticChart`/`Metric` transport facade with `Signature`/`Orientation` declarations and independent SymPy reduction oracle | conversion/oracle path | partial | explicit dimensions, metric/form/connection integration, complete flux-coordinate descriptors, paper source assembly and full FEM corpus |
| Typed coordinate tensors and indexed algebra | native fixed-3D chart/metric-owner `tensor_t` plus the runtime-dimension 1--5 `spacetime_tensor_t`: rank/slot variance/density metadata, value-semantic chart owner keys `(u, x)` or metric coordinate keys `u`, checked fixed-3D `index_type_t`/`index_t` labels, metric `raise`/`lower`, tensor product, opposite-variance contraction, trace, slot permutation, pairwise symmetry declarations with refusal of false declarations, two-slot symmetrization/antisymmetrization, covariant differentiation through rank-five curvature-like tensors, direct first-slot covariant divergence, coordinate Lie derivative with density weights, incompatible-chart refusal, and independent nonorthogonal-chart checks; Python/SymPy transport for fixed 3D components and runtime spacetime variance/density/algebra operations | C ABI 69 declaration/refusal operation plus spacetime raise/lower/density-factor/product/contraction/permutation/covariant-differentiation/divergence/Lie-transport operations | partial | arbitrary dimensions, arbitrary symmetry groups beyond pair declarations, native metadata queries for raw C arrays, dummy-index canonicalization, singular-metric proofs, and full Python/SymPy parity |
| Connections and curvature | native fixed-3D subset plus runtime-dimension spacetime tensor `covariant_diff` through rank-five outputs, direct first-slot `covariant_divergence`, and coordinate Lie derivative/Killing transport for supported ranks: typed `connection_t`, supplied affine coefficients, torsion, nonmetricity, typed Christoffel, supplied-connection Riemann with standard/opposite sign conventions and geodesic residual, `covariant_diff`, first-slot `covariant_divergence`, chart/metric-owner geodesic residuals, Riemann, Ricci, scalar curvature, Einstein tensor, first- and second-Bianchi residuals, Killing residuals, plus metric compatibility and density-weight checks; C ABI 69 and Python/SymPy views | no public nodes | no | pseudo-Riemannian sign corpus beyond the supplied-connection component checks, geodesic solving/variational mechanics, arbitrary-dimensional transport, arbitrary symmetry groups, and full differential comparison |
| Differential forms and de Rham identities | native fixed-3D coordinate forms and one dimension-aware `spacetime_form_t` owner for runtime dimensions 1--4: scalar through degree-four forms, value-semantic chart/metric owner keys, `d`, chart/metric-owner oriented volume forms, metric `star`, `codifferential`/`codiff`, Laplace--de Rham, interior product, Cartan `lie`, `flat`, `sharp`, magnetic-chart potential/flux forms with `i_B(volume)=dA`, degree-four zero extension, incompatible-chart refusal, chart-map pullback checks, and the first Maxwell owner for `F=d(A)`, gauge shifts, and `d(*F)-J` | C ABI plus `fortsym.Form`/`fortsym.SpacetimeForm`/`fortsym.sympy` transport and the first native-backed `Manifold`/`Patch`/`CoordSystem`/`Differential`/`WedgeProduct`/`LieDerivative` names | no | arbitrary dimensions beyond four/rank, coordinate transformations beyond fixed-3D maps, full Maxwell media/topology, patch/boundary metadata, and full Python/SymPy `diffgeom` parity |
| JVP, VJP, gradient, and HVP | yes | verification | verification | consumer conformance corpus |
| Simplification | partial: principal-square-root powers, guarded `sqrt`/`abs`, exact negative perfect-square roots, exact real unit-circle `asin`/`acos` values, exact real tangent `atan` values, exact real `asinh(±1)` values, exact `asin(±i)`, `acos(±i)`, and `asinh(±i)` branch points, exact `log(0)=zoo` with sentinel propagation, principal-branch exact negative real and imaginary logs, exact `atan(±i)` and `atanh(1)`/`atanh(-1)` poles plus `atanh(±i)` branch points, exact `acosh(0)`/`acosh(-1)` and `acosh(±i)` branch points, finite gamma-family poles, exact factorial values through `factorial(1000)`, and real/nonzero-guarded `log`/`exp` compositions | partial | partial | broader guarded elementary rewrites and polynomial normal form |
| Expansion | bounded multinomial | yes | Yacas concrete method | sparse polynomial expansion without the 100,000-term fast-path bound |
| Zero decision | partial, with C/Python three-valued `is_zero` and `is_nonzero` | partial | weak simplify test | broader predicate inference and polynomial certificates |
| Polynomial GCD and rational cancellation | no | partial, univariate | partial | multivariate domains |
| Factor, apart, and together | no | no public operation | Yacas factor only | typed engine operations |
| Assumptions and conditions | sign/zero facts, compound `And` ingestion, bounded relational sign facts, nested C/Python scopes, immutable Fortran contexts, `Q`/`ask` queries, `refine`, signed/zero guarded `sqrt` and `abs`, exact `log(0)` sentinel handling, real/nonzero-guarded `log`/`exp` composition, three-valued predicate properties, and conflict diagnostics | no public context | no public context | broader predicate inference and returned conditions |
| Series and coefficient extraction | Taylor | C shim only | no public operation | Laurent series and singular points |
| Linear and polynomial solve | scalar linear | no public operation | Yacas concrete method | univariate polynomial roots |
| Integration | no | no generic operation | Yacas concrete method | rational integration first |
| Limits and asymptotics | no | no public operation | Yacas concrete method | Taylor limits, then Gruntz scales |
| CSE and Fortran kernel generation | yes | candidate source | candidate source | runtime-aware cost model |
| Symbolic matrices and general symbolic tensors | exact dense rational systems over `expr_t` arrays; coordinate tensors have a separate typed owner | no public nodes | no | conditional symbolic pivots and broader matrix operations |

FortNum's required surface is construction, elementary functions, substitution,
differentiation products, verification, CSE, Fortran emission, stability
rewrites, and Enzyme wrappers. MHD1D additionally requires assumptions,
series coefficients, scalar linear solving, multivariate rational
simplification, and Taylor or Fourier projection.

The matrix does not claim Mathematica-wide coverage. The competitive target is
the measured FortNum and MHD1D fragment, followed by named algebraic domains.

Fortran callers start with the short `use fortsym` surface and its documented
default arena. Character assignment creates symbols and `symbols(...)` fills
scalar names. Explicit arenas remain the first-class API for concurrency,
embedding, and independent derivations. The naming rule and the no-LaTeX-name
boundary are maintained in [`fortran-api.md`](fortran-api.md).

## Verified toolchain paths

This is a local compatibility snapshot, not a claim that every supported
toolchain has been exercised:

| Path | Toolchain | Evidence | Result |
|---|---|---|---|
| Native gate | `fo`, GNU Fortran 16.1.1 | full static, build, test, and lint stages | passed |
| CMake/CTest | GNU Fortran/C/C++ 16.1.1, system SymEngine 0.14.0 | fresh Release configure, build, and 55-test CTest run | 54 passed; one optional SymPy CTest skipped |
| Code generation | CMake codegen targets | kernel, IR, backend, WL, and simple-kernel tests | passed |
| CUDA generation | `nvcc` 13.3.73, RTX 5060 Ti, driver 610.57.04 | CUDA emitter CTest plus independent generated `.cu` compilation | passed |
| NVIDIA Fortran | `nvfortran` 26.5-0 | independent compilation of a generated Fortran kernel | passed |

The complete corpus and all supported compiler/toolchain combinations remain
release-closure work in `ROADMAP.md`.
