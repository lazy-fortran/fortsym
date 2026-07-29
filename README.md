# fortsym

Fortran-native computer algebra: derive symbolically, generate kernels, and
assert correctness — inside the normal build and test loop, with no notebook
and no Python round-trip.

fortsym is a **multi-engine frontend**. It owns its expression representation
and drives several computer algebra systems underneath, because no single one is
good at everything: SymEngine is fastest and best at manipulation and code
generation but cannot integrate, SymPy and Maxima are broad but slow, and Yacas
integrates and factors but cannot simplify trigonometry. fortsym runs the
engines that are present, compares their answers, and keeps the one that
produces the smallest kernel.

Wired: SymEngine and Yacas (linked in-process), SymPy and Maxima (separate
processes). Each declares what it can do and is only asked for that — Yacas
does not claim zero testing, because its `Simplify` cannot close trigonometric
identities and the engine that can should get the work.

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
