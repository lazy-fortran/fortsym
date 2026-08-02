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
coefficients and series, scalar linear solving, bounded requested-precision
`N[expr, p]` evaluation (17--512 decimal digits), and conservative zero
decisions. Broader domains remain on the documented roadmap.

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

```fortran
type(expr_t) :: x, y, e

x = sym("x")
y = sym("y")
e = sin(x*y)**2 + cos(x*y)

call check_zero("pythagorean", sin(x)**2 + cos(x)**2 - 1)
call emit_kernel("dedx", diff(e, x), file="src/generated/dedx.f90")
```

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

Generated pure numerical leaves may optionally carry OpenMP
`declare target` and OpenACC `routine seq` annotations. These flags only make
the same procedure body callable on a device; fortsym deliberately emits no
parallel schedule, data movement, memory management, or runtime dispatch.
OpenMP annotation requires a generated module wrapper so the public procedure
can be named in the module specification.

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
substitutions do not cascade through replacement expressions.

**Solve exact dense rational systems.** `solve_exact_linear_system` accepts an
`expr_t` coefficient matrix and one or more right-hand sides. It uses
deterministic exact elimination, rejects unsupported symbolic pivots rather
than guessing, and reconstructs every residual before returning a solution.

**Differentiate Bessel functions.** `besselj(n, x)` represents the Bessel
function of the first kind and native differentiation uses the standard
order recurrence, including the chain rule. Generated Fortran spells it as the
standard `bessel_jn` intrinsic.

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
subset used by the corpus. After `fo build`, run an original derivation with:

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

On the 384-file corpus sweep recorded 2026-08-02, the native runner completed
380 scripts (344 with non-empty results and 36 valid empty result sets), timed
out on one heavy script, reported one runner error, and explicitly refused two
unsupported constructs. It did not crash. The latest bounded
CharacteristicPolynomial/LegendreP/Diagonal/list-selector/Coefficient/Solve/
FoldList/ArrayFlatten/Total audit refreshed 380 native rows in 1:11.83 with a
3.04 GiB peak RSS. After the quoted-string and Total slices, a fully warm audit
now takes 1.11 seconds at 418 MiB RSS and starts no backend subprocesses. The
current binding-level tally is 3,209 agreements, 727 declared differences, 20
unsupported outcomes, 38 timeouts, 122 errors, 192 oracle disagreements, and
797 oracle-missing bindings. The
native collection slice also evaluates bounded exact `Range`,
`DiagonalMatrix`, rectangular `Diagonal`, bounded `LegendreP`, bounded exact
`CharacteristicPolynomial` for explicit square matrices up to dimension 16,
bounded non-negative `MatrixPower`, bounded block-matrix `ArrayFlatten`,
`RowReduce`, `NullSpace`, `MatrixRank`,
`LinearSolve`, `Minors`, `Length`, recursive `Flatten`, list
`Append`/`Join` and bounded list
selectors, bounded univariate and multivariate `CoefficientList`, bounded
`FoldList[Plus, initial, list]`, bounded explicit-list `Total`, and bounded
requested-precision `N`; multiline
Wolfram dot products
are preserved by both the native parser and the SymPy translator. Unsupported
selector shapes remain opaque rather than losing a binding. The remaining
parity gap is still substantial and is tracked in [ROADMAP.md](ROADMAP.md).

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
