# fortsym

Fortran-native computer algebra: derive symbolically, generate kernels, and
assert correctness — inside the normal build and test loop, with no notebook
and no Python round-trip.

fortsym is a **multi-engine frontend**. It owns its expression representation
and drives several computer algebra systems underneath, because no single one is
good at everything: SymEngine is fastest and best at manipulation and code
generation but cannot integrate, the current SymPy and Maxima adapters provide
slow simplification checks, and Yacas integrates and factors but cannot
simplify trigonometry. fortsym runs the engines that are present, compares their
answers, and keeps the one that produces the smallest kernel.

The native Fortran backend currently performs arbitrary-precision integer and
rational arithmetic with a checked 64-bit fast path, collection of like terms
and integer powers, bounded polynomial expansion, differentiation, Taylor
coefficients and series, scalar linear solving with computed-rule replacement,
exact fractional-monomial `Exponent`, bounded requested-precision `N[expr, p]`
evaluation (17--512 decimal digits), and conservative zero decisions. Broader
domains remain on the documented roadmap.

Guarded native rewrites accept an explicit `assumption_context_t`. Positive,
nonnegative, nonzero, and real-valued facts are stored on interned expressions.
For example, `positive(x)` permits `sqrt(x**2)` and `abs(x)` to reduce to `x`;
without the context the same zero test returns `UNKNOWN`.

Wired: SymEngine and Yacas (linked in-process), plus SymPy and Maxima
separate-process simplification/zero-test adapters. Each declares only callable
operations and is only asked for those — Yacas does not claim zero testing,
because its `Simplify` cannot close trigonometric identities and the engine
that can should get the work.

Your code never names an engine.

## Python and SymPy boundary

Python callers use the installed `fortsym` package through the stable C ABI.
`DIA_SYMPY` describes the expression spelling used by the subprocess adapter,
which provides simplification and zero-test checks. It does not interpret an
arbitrary SymPy script or provide a SymPy session. The existing consumer
inventory contains no SymPy-authored derivation that requires one, so the
supported Python path is host-language composition through the package API.
The Wolfram session remains a separate compatibility surface because the
repository has an existing `.wl` derivation corpus to read.

```fortran
use fortsym
type(expr_t) :: x, y, e, d
type(engine_result_t) :: derivative

x = "x"
y = "y"
e = sin(x*y)**2 + cos(x*y)

derivative = diff(e, x)
d = derivative%value
```

The convenience layer, default-arena lifetime rules, and the explicit-arena
API are described in [`doc/fortran-api.md`](doc/fortran-api.md). Verification
and code generation have dedicated modules because they have their own result
and output contracts.

## Why

Three problems this exists to solve, all drawn from real code in these repos:

- **The CAS sits outside the build.** Hundreds of notebook scripts derive an
  identity, print `PASS`/`FAIL`, and stop there — the Fortran is then written by
  hand to match, and nothing detects drift afterwards.
- **Generated kernels outlive their generators.** Optimised code with `t1, t7,
  t13` temporaries and an `automatically generated` banner, with no generator
  left in the repository, cannot be regenerated or trusted.
- **One CAS is never enough.** Each has blind spots, and picking one means
  inheriting them.

fortsym keeps the generator in the repository, regenerates it in CI, and checks
generated code against the symbolic definition in the same test binary.

## What it does

**Decide identities.** A real zero-decision procedure, not a bag of rewrite
rules: expressions go to exponential normal form, so the Pythagorean,
hyperbolic, angle-sum, multiple-angle, tangent-addition, exp/log and rational
identities are all decided. Crucially it also refuses to decide what it cannot:
`sqrt(x²)−x` and `atan x + atan(1/x) − π/2` hold only on part of the domain and
are correctly *not* reported as identities, and expressions outside the decidable
fragment (gamma, Bessel, zeta) return `UNKNOWN` rather than a confident guess.

**Generate kernels.** Common subexpression elimination, then Fortran source with
kind-suffixed literals, integer exponents kept integer, correct `atan2` argument
order, precedence-driven parenthesisation and 132-column continuations.
Candidate simplifications from every engine are *verified equivalent first*,
then ranked by operation count after CSE — so the winner is the cheapest form
that is provably the same function.

Generated numerical leaves select a stable target identity: `fortran_cpu`,
`fortran_openmp_target`, `fortran_openacc`, or `cuda`. The target only chooses
source spelling and leaf decoration; fortsym emits no parallel schedule, data
movement, memory management, or runtime dispatch. The historical dual-target
flags remain as a compatibility default so committed FortNum kernels remain
byte-identical while new callers can request one target explicitly.

The IR emitter also accepts explicit source policies for small-power expansion,
constant folding, constant-division elimination, and FMA shaping. These are
deterministic source choices rather than compiler fast-math flags; the `pure`
procedure option remains opt-in for compatibility with downstream consumers.

The legacy expression emitter exposes target-specific CSE choices on
`kernel_spec_t`: `CSE_NONE`, `CSE_THRESHOLDED`, or `CSE_FULL`, plus a
`remat_threshold` that trades repeated arithmetic for shorter temporary live
ranges. The default is full CSE with threshold zero, preserving existing
generated output until a consumer measures another choice.

**Cross-check engines.** When several engines answer, agreement raises
confidence and **disagreement is reported as a finding**, not averaged away: it
means one of them is wrong. Per-engine timings fall out of normal operation, so
the test run produces a benchmark table. Measured here over the same nine
questions:

| engine | decided | per call |
|---|---|---|
| symengine (linked) | 7/9 | 0.0001 s |
| yacas (linked) | — | 0.08 s |
| maxima (subprocess) | 4/9 | 0.41 s |
| sympy (subprocess) | 4/9 | 0.38 s |

**Do differential geometry.** Give it a coordinate chart and it derives the
basis, metric, inverse metric, Jacobian, Christoffel symbols and
grad/div/curl/laplacian. The tests assert the identities rather than stored
answers -- `g^ik g_kj = delta`, `det g = J^2`, `curl grad = 0`, `div curl = 0` --
which is what catches a raised index left lowered or a missing Jacobian weight.

The native `paper_magnetic_native` example extends that surface to the
Albert--Bíró--Lainer Fourier reduction: `b_fourier` accepts either an integer
mode or a symbolic `n`, while `b_cov` and `b_fourier_density` keep covariant,
contravariant, and density components distinct. The Wolfram runner accepts the
companion `paper_magnetic.wl` assignments through the same expression arena.
`example_cylindrical_fourier` is the compact executable derivation in the
paper's `(Z, R, phi)` ordering. It checks the branch-free density components
`sqrt(g) B^Z`, `sqrt(g) B^R`, and `sqrt(g) B^phi` directly.
The Python `fortsym.Chart` facade exposes the same native operations; it is
also re-exported from `fortsym.sympy` without importing SymPy.

The native `forms_native` example shows the next layer: a covector potential
`A`, its magnetic two-form `dA`, and the checked identity
`i_B(volume) = dA`, with `d(dA) = 0` proved in the same expression arena.

The native `tensor_native` example adds typed coordinate tensors: `vector` and
`covector` constructors record slot variance, `raise` and `lower` use the
chart metric, and `tensor_product` plus `contract` handle the first indexed
algebra subset. The same `tensor_t` owner backs the facade and lower-level
modules, including density weights.

The native `connection_native` example extends this with covariant
differentiation and typed curvature views. `fortsym_connection` owns
Christoffel, geodesic residuals, Riemann, Ricci, scalar-curvature, Einstein,
and second-Bianchi construction, with independent checks for metric
compatibility and
tensor-density weights.

The relativity examples continue from those owners. `example_de_sitter`
checks the de Sitter vacuum equation `G_ab + Lambda g_ab = 0` with
`Lambda = 3 H**2`; `example_gps_newtonian_limit` derives the radial weak-field
geodesic term and prints the first-order gravitational plus second-order
Doppler clock correction. `example_magnetic_flux_coordinates` builds nested
toroidal surfaces labelled by `psi`, verifies `B^psi = 0`, and checks
`i_B(volume) = dA` and `div(B) = 0`.
`example_boozer_coordinates` shows the analytic Boozer representation: the
covariant angular components `B_theta = I(psi)` and `B_phi = G(psi)` are flux
functions, while the metric and raised components carry the coordinate volume
factor. It also prints the diagonal metric, the oriented volume form, and
`i_B(Omega) = B_phi dpsi wedge dtheta - B_theta dpsi wedge dphi`.
`example_spherical_coordinates` derives the regular-patch volume factor,
reciprocal basis, selected Christoffel symbols, and the spherical gradient,
divergence, curl, and Laplace--Beltrami operator. Its compact metric-owner
path keeps the example fast and readable while the chart path checks the
Cartesian embedding independently.

**Read Fortran back.** Point it at a source file and a variable name and it
returns the symbolic expression that file computes, so a hand-written kernel can
be checked against its definition without anyone transcribing the code into the
checker by hand.

**Build derivative products contracted.** `jvp`, `vjp`, `gradient` and `hvp`
never form a Jacobian or a Hessian, and the implicit-function builders emit the
actions of `R_y` and `R_p` directly — differentiating the defining equation, so
the solver iteration that found the solution never enters the derivative.

**Substitute structurally.** `subs(expression, old, new)` replaces a symbol or
an arbitrary subexpression. `subs_many(expression, old, new)` applies all
pairwise replacements simultaneously, so swaps and coupled governing-equation
substitutions do not cascade through replacement expressions. The
`fortsym.sympy` compatibility layer also provides exact-node `Expr.xreplace`;
it does not perform a final expansion. `Expr.match` returns `{}` for equal
structure and `None` otherwise, and the compatibility layer supports bounded
`Wild` patterns with fixed structural slots plus `exclude` and `properties`
filters plus bounded single-Wild additive and multiplicative remainder
matching and distinct-Wild root partitions; exact non-wildcard `Expr.replace`
reuses that replacement owner and
supports `map=True`; broad callable, wildcard replacement, weighted-coefficient
solving, and broader recursive commutative partitioning remain outside the
subset.

**Solve exact dense rational systems.** `solve_exact_linear_system` accepts an
`expr_t` coefficient matrix and one or more right-hand sides. It uses
deterministic exact elimination, rejects unsupported symbolic pivots rather
than guessing, and reconstructs every residual before returning a solution.

**Differentiate Bessel functions.** `besselj(n, x)` and `besseli(n, x)`
represent the ordinary and modified Bessel functions of the first kind.
Native differentiation uses their standard order recurrences, including the
chain rule; real evaluation supports integer orders through the corresponding
series. Generated Fortran spells them as the standard `bessel_jn` and
`bessel_in` intrinsics.

**Keep multivariate derivatives symbolic.** Differentiating an opaque applied
function such as `func("psi", [r, u])` visits every argument and produces a
canonical `partial_derivative` application. Mixed partial indices are sorted,
so the two differentiation orders share one DAG node.

**Assert, with the strength of the claim visible.** A symbolic decision and a
numeric probe are both useful and are not the same thing:

```
PASS         pythagorean (decidable)
PASS(probe)  gamma recurrence   [no counterexample in 98 points]
```

`check_identity` is the strict variant that refuses probe evidence.

## Native Wolfram corpus backend

The repository also ships a native Fortran runner for the Wolfram-language
subset used by the corpus. The parser grammar and its refusal boundary are
published in
[`doc/compatibility-wolfram.md`](doc/compatibility-wolfram.md).
After `fo build`, run an original derivation with:

```bash
build/fo/app/fortsym_wl_run path/to/script.wl
```

It parses and evaluates the `.wl` source directly in Fortran and emits the
same `R<TAB>name<TAB>value` / `T<TAB>seconds` protocol used by
[fortsym-bench](https://github.com/lazy-fortran/fortsym-bench). It does not
invoke Mathematica, Mathics, or Python. Associations, plots, exact polynomial
operations, guarded definite integrals, numerical Wolfram operations, and the
documented parser subset are covered by native tests and corpus sweeps.

The benchmark keeps the original `.wl` file as the shared source for Mathics
and the native backend. Python companions are generated separately for the
SymPy oracle; their inventory and persistent oracle cache live in
`fortsym-bench`.
There is not yet a full-corpus `.wl`-to-`.f90` translator: native Fortran still
interprets the Wolfram source at runtime for corpus runs, while code generation
starts from an already-built `expr_t` graph. The 2026-08-02 bounded inventory
ran over all 384 sources and emitted 0 Fortran files: 384 were explicitly
refused, with no compile error, timeout, or runner error. Full executable
Fortran coverage of all corpus scripts remains open work.

The current bounded source-to-source slice accepts a sequential stream of up to
128 scalar assignments. Standalone top-level `Null` statements are accepted as
no-op cell separators; `Null` used as a value remains refused. The
`fortsym_wl_to_f90 input.wl output.f90` command accepts forms
such as `r = 2*x + Sin[x]` and `s := r + 1`, infers scalar inputs, expands only
earlier assignments, and emits a compilable Fortran subroutine using the
existing kernel emitter, including n-ary scalar `Min` and `Max` intrinsics. It
deliberately refuses control flow, forward or
reassigned names, arrays, non-Fortran identifiers, and unsupported expressions;
this is an incremental entry point, not full corpus translation.

The executable is installed by `fo install`; use `fo exec fortsym_wl_to_f90`
for the repository-managed invocation.

On the 384-file corpus sweep recorded 2026-08-03, the v108 native cache contains
378 successful script rows, three explicit unsupported rows, three timeouts,
and no native runner errors; it did not crash. The latest bounded
CharacteristicPolynomial/LegendreP/Diagonal/list-selector/Coefficient/Solve/
FoldList/ArrayFlatten/Total/PseudoInverse/SingularValueList audit was followed
by a native-cache refresh of 384 rows in 121.39 seconds with a 1.61 GiB peak
RSS; the rebuild also refreshed changed SymPy rows. The v108 warm audit takes
1.10 seconds at about 500 MiB RSS.
After the quoted-string,
Total, PseudoInverse, and diagonal singular-value slices, plus the v17 bounded
polynomial translator transition, the warm audit starts no backend
subprocesses. The latest v124 stable cache-only whole-corpus tally is 4,024
agreements, 386 declared differences, 7 unsupported outcomes, 74 timeouts, 59
errors, 2 unavailable oracle rows, 205 oracle disagreements, and 262
oracle-missing bindings across 4,879 emitted bindings from 384 corpus sources.
The native-fingerprint refresh took 100.03 seconds and peaked at 803 MiB RSS;
the subsequent cache-only pass took 0.98 seconds at 489 MiB RSS. Raw SymPy,
Mathics3 10.0.1, native results, and comparison verdicts were reused from the
persistent caches on the warm pass. A broader native Solve/Part/Integrate
expansion was rejected after exceeding the bounded audit limit; the stable
backend retains the prior bounded native surface plus named-character suffix
parsing. The v43-v124 batches add
source-faithful ECNL equation strings, numeric validity estimates,
Maxwell/flux-pumping companions, math10y and Suydam recoveries, large-step LTE
reconstruction, Sympl3 field forms, normal-stability numeric parity, math3y,
and Cartesian-primitive, math8y, perpendicular-block, math10y, math6-1y,
cylinder-spectrum, math11y, Mercier, Appendix-B, math14y, and math15y
recoveries, with 68 independent tests. The v59 native slice fixes implicit
scientific-literal precedence and adds vector-matrix `Dot` evaluation; v60 adds
bounded numeric `FindRoot`.
The v18
Solve-rule/fractional-`Exponent`
transition refreshed four SymPy rows in 10.46 seconds at 399 MiB RSS. A
current one-worker audit of the affected 11-script slice, using the rebuilt
native runner, took 42.39 seconds at 804 MiB RSS and reported 181 agreements,
63 differences, 2 unavailable oracle rows, 9 errors, and 54 oracle-missing
bindings. This focused result is not a replacement for a new whole-corpus
baseline; it confirms that the five computed-Solve/fractional-`Exponent`
bindings exposed by the slice now agree. The subsequent v19 SymPy `Thread`
transition refreshed 16 rows in 16.51 seconds at 399 MiB RSS. Its current
16-script one-worker parity slice took 17.43 seconds at 508 MiB RSS, converted
two native `Thread` bindings from differences to agreements, and reported 249
agreements, 58 differences, 1 unavailable oracle row, 1 timeout, 5 errors, 26
oracle disagreements, and 58 oracle-missing bindings. Neither focused result
replaces a new whole-corpus baseline. The v20 positive-level `Map` transition
refreshed six SymPy rows in 4.07 seconds at 403 MiB RSS. Its current six-script
parity slice took 1.00 second at 404 MiB RSS; the nested-map bindings remain
outside the scored intersection because the selected rows are dominated by
plotting/file-I/O or Mathics failures. The
v21 bounded numeric `Piecewise` transition then ran the same six-script slice
in 0.85 second at 403 MiB RSS; it preserved 21 agreements, 6 differences, 3
unavailable oracle rows, 2 oracle disagreements, and 3 oracle-missing
bindings, with no scored native tally change. The
v22 numeric `Boole` transition then refreshed three SymPy rows in 2.91
seconds at 404 MiB RSS and ran a three-script parity slice with 21 agreements,
7 differences, 1 unavailable oracle row, 1 oracle disagreement, and 1
oracle-missing binding; it also produced no scored native tally change. The
v23 numeric `Which` transition then served a five-script warm slice in 0.71
second at 404 MiB RSS: 60 agreements, 16 differences, 1 timeout, 1
unavailable oracle row, 3 oracle disagreements, and 73 oracle-missing
bindings. Its cached timeout/unavailable outcomes were not rerun. The
v24 bounded `TrigReduce` transition then served a six-script warm slice in
0.72 second at 404 MiB RSS: 101 agreements, 48 differences, 3 unavailable
oracle rows, 1 timeout, and 103 oracle-missing bindings. Its cached timeout
was not rerun. The v25 bounded symbolic 2x2 `Solve` transition then served
the corpus script that exposed the gap in 0.78 second at 404 MiB RSS: 33
agreements, 10 differences, 1 unavailable oracle row, and 1 oracle-missing
binding, with no timeout or runner error. The v26 verified exponential-product
`Integrate` transition then served a three-script slice in 0.78 second at
404 MiB RSS: 5 agreements, 1 difference, 1 unsupported backend outcome, 1
unavailable oracle row, and 1 oracle disagreement, with no timeout or runner
error. The v27 native definite/multiple-`Integrate` transition then evaluated
the measured nested-limit script in a warm one-worker audit in 0.77 seconds at
402 MiB RSS. Native and SymPy now produce the complete outer-to-inner result;
Mathics retains a partial unevaluated result, so the three bindings are
reported as oracle disagreements rather than scored as native errors. The
native collection slice also evaluates bounded exact `Range`,
`DiagonalMatrix`, rectangular `Diagonal`, bounded `LegendreP`, bounded exact
`CharacteristicPolynomial` for explicit square matrices up to dimension 16,
bounded non-negative `MatrixPower`, bounded block-matrix `ArrayFlatten`,
`RowReduce`, `NullSpace`, `MatrixRank`,
`LinearSolve`, `Minors`, `Length`, recursive `Flatten`, list
`Append`/`Join`, bounded list `Thread`, bounded positive-level `Map`, bounded
numeric `Piecewise` branch selection, numeric `Boole` conditions, numeric
`Which` selection, bounded `TrigReduce`, bounded symbolic 2x2 `Solve`,
verified products of exponential factors in `Integrate`, bounded
trigonometric-square integration, formal finite symbolic endpoints for entire
definite integrands, bounded polynomial-product expansion, and bounded list
selectors, bounded univariate and multivariate `CoefficientList`, bounded
`FoldList[Plus, initial, list]`, bounded explicit-list `Total`, bounded
full-rank numeric `PseudoInverse`, diagonal/zero numeric `SingularValueList`,
numeric `Max`/`Min`, and bounded requested-precision `N`; multiline
Wolfram dot products, and scalar and per-dataset `Joined -> True/False`
overrides for native
`ListPlot`/`ListLinePlot`
are preserved by both the native parser and the SymPy translator. Unsupported
selector shapes remain opaque rather than losing a binding. The v34 bounded
large-step, kinetic-bridge, and Bacc/Rosa/Posch companion refreshes recover
eight previously opaque SymPy bindings and six verified machine-precision
numeric comparisons. The Mathics 10.0.1 UV runner now restores `$Assumptions`
safely after local integrals; its failures remain cached as 60 errors and 69
timeouts rather than being rerun. The remaining parity gap is still
substantial and is tracked in [ROADMAP.md](ROADMAP.md). The v35 cycle also
refreshes the `math8y`, `sympl3_`, and general-Maxwell companions, and adds a
Mathics Boolean-assumptions shim. The v36 cycle expands the helical-core
derivative and bounded Solve companions; v37 adds source-faithful
phase-transform and flux-coordinate SymPy companions with independent tests.
The v38 cycle adds source-faithful NAE/DESC geometry and Appendix-B integral
companions. The v39 cycle adds source-faithful math6-2y, math11y,
two-component-energy, and dynamo-diagnostics companions with independent
tests; the dynamo bridge also reduces its focused runtime from about 29
seconds to 1.1 seconds. The v40 memo-feedback companion recovers the source
magnetic norm and its dependent geometry bindings, adding seven agreements
with an independent test. The v41 cylinder-spectrum companion recovers the
source-faithful force-balance and pressure-slope bindings with an independent
test. The v42 Bacc/Rosa/Posch companion recovers the source magnetic-field
integral and removes one oracle disagreement. The v43-v60 batches raise the
score to 3,853 agreements and 468 differences, with 354 oracle-missing bindings;
the successful
Mathics inventory remains 255 rows, with 60 errors and 69 bounded timeouts.

## Current corpus state

The v124 audit on 2026-08-03 uses native commit `2f3c34b`, SymPy cache version
29, comparison cache version 20, and Mathics3 10.0.1 installed as a UV tool. It
reports 4,024 agreements, 386 declared differences, 7 unsupported outcomes,
74 timeouts, 59 backend errors, 2 unavailable oracle rows, 205 oracle
disagreements, and 262 oracle-missing bindings across 4,879 emitted bindings
from the 384-source corpus. The native refresh took 100.03 seconds at 803 MiB
RSS; its cache-only follow-up took 0.98 seconds at 489 MiB RSS. The v122
The v122 closeout adds source-faithful Flux44, separatrix, KiLCA collision,
normal-stability, and Neort appA companion recoveries. The comparator now also
bounds expensive equivalence attempts by expression size, preventing a
pathological SymPy simplification from hanging the corpus run. Full corpus
parity remains open work.

The v109 follow-up advances the comparison cache to version 18, recovers bounded
`archive-tu/math6-1y` and `vector2d` bindings, and adds equation-aware comparison
for explicitly annotated rearrangements. Root commit `23b0e07` also adds the
bounded literal-only `FileNameJoin` subset; its focused Fortran regression passes.
The aggregate v109 audit reuses the cached native rows from v108; the new native
subset is verified separately so the fast cache-only timing does not hide a
full native refresh.

## Build

```bash
fo                    # static checks, build, tests, lint, format
fo test               # test suites
fo build --asan       # arena and handle lifetime checks
```

or with CMake directly:

```bash
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug
cmake --build build -j
ctest --test-dir build --output-on-failure
```

`flake.nix` provides the exact dependency closure CI builds against — FLINT and
MPFR are pinned to a single version each, so a machine whose distribution ships
another one cannot configure at all. `nix develop` enters Tier 1; `nix develop
.#tier2` adds the optional engines.

SymEngine is taken from the system by default; `-DFORTSYM_USE_SYSTEM_DEPS=OFF`
builds it from source instead. Yacas is fetched and built at a pinned tag by
the CMake path — a few seconds, nothing to install. The fpm path cannot build a
C++ dependency from source, so there Yacas simply reports unavailable.

Optional extra engines are detected at run time and skipped when absent.
`scripts/bootstrap.sh` reports what is missing and prints the command to install
it; it never installs anything on its own.

## Licence and provenance

fortsym is MIT licensed — see [LICENSE](LICENSE).

The supported surface and planned native engine work are tracked in:

- [CAS backend architecture](doc/architecture.md)
- [consumer requirement matrix](doc/consumer-requirements.toml)
- [pinned upstream and algorithm baseline](doc/upstream-baselines.toml)
- [feature matrix](doc/feature-matrix.md)
- [roadmap](doc/roadmap.md)
- [benchmark protocol](doc/benchmarks.md)
- [algorithm and benchmark provenance](PROVENANCE.md)

**[LEGAL.md](LEGAL.md) is the authoritative record** of every dependency, its
licence, whether it is linked or run as a separate process, and the obligations
that follow. Read it before adding a dependency or redistributing a build. In
short:

- Linked in-process: SymEngine (MIT), Yacas (LGPL-2.1+), FLINT, GMP and MPFR
  (LGPL) — the LGPL components dynamically, so they remain replaceable.
- Run as separate processes: SymPy (BSD), Maxima and other GPL engines. Process
  separation keeps them out of fortsym's link closure, and none is required.
- **Wolfram and Mathematica are excluded entirely** — not as a backend, and not
  as an oracle for developing fortsym itself. The reasoning is recorded in
  LEGAL.md §5.
- Contributors must never paste or transcribe GPL engine source into fortsym.
