# fortsym

Fortran-native computer algebra: derive symbolically, generate kernels, and
assert correctness — inside the normal build and test loop, with no notebook
and no Python round-trip.

fortsym is a **multi-engine frontend**. It owns its expression representation
and drives several computer algebra systems underneath, because no single one is
good at everything: SymEngine is fastest and best at manipulation and code
generation but cannot integrate, Yacas integrates and factors but cannot
simplify trigonometry, SymPy and Maxima are broad but slow. fortsym runs the
engines that are present, compares their answers, and keeps the one that
produces the smallest kernel.

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

## Three things it does

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

**Cross-check engines.** When several engines answer, agreement raises
confidence and **disagreement is reported as a finding**, not averaged away: it
means one of them is wrong. Per-engine timings fall out of normal operation, so
the test run produces a benchmark table.

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
builds it from source instead. Yacas is always built from source at a pinned
tag — it takes a few seconds and needs nothing installed.

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
