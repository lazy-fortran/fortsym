# Correctness and performance benchmarks

## Result contract

Benchmark programs write machine-readable records containing:

- fortsym, SymEngine, SymPy, compiler, and dependency revisions
- compiler flags, CPU model, operating system, affinity, and governor
- workload name, semantic domain, assumptions, and output coverage
- warmup count, batch size, repetition count, and process count
- median, dispersion, confidence interval, peak resident memory, and failures
- correctness-oracle name and result

Cold startup, conversion, operation, printing, compilation, and generated
kernel execution are separate measurements. Direct SymEngine C or C++ calls
form the library baseline. fortsym reports native-only and end-to-end rows.

## Workloads and oracles

| Workload | Measurements | Independent oracle |
|---|---|---|
| Construction and interning | time per new or reused node, memory, scaling | arity invariants and stable structural digest |
| Expand and multiply | time, memory, output term count | exact coefficient map and finite-field evaluations |
| Differentiate, JVP, VJP, HVP | time, output DAG size | closed forms, finite differences, adjoint identity, mixed partials |
| Simplify and rational cancel | time, solved rate, `UNKNOWN`, operation count | polynomial normal form or assumption-aware interval checks |
| Parse, print, substitute | throughput and malformed-input rate | grammar fixtures plus semantic evaluation |
| CSE and code generation | analysis, emission, compile, and execution time | compiled kernel versus independent numeric evaluation |
| Polynomial GCD, factor, resultant | time, memory, degree, terms | divisibility, reconstruction, Bezout, specialization |
| Integration | solved-correct rate, timeout, expression size | differentiate the answer and high-precision quadrature |
| Series and limits | time, order, coverage | coefficient recomposition and high-precision asymptotics |
| Solve and exact linear algebra | time, roots or rank, coverage | substitution, reconstruction, and matrix identities |
| FortNum generators | total time, memory, compiled runtime, numeric error | FortNum behavioral tests and derivative identities |

The current council table is diagnostic timing. It mixes single calls,
subprocess startup, and in-process operations without environment metadata, so
it is not evidence for performance parity.

## Protocol

Pin dependency revisions and record the complete build command. Use fixed CPU
affinity and a documented frequency policy. Warm each in-process workload,
calibrate a batch above the timer resolution, and run independent processes.
Report the median and a dispersion measure. Keep output validation inside every
measured batch or validate an identical replay immediately afterward.

Compare identical domains, assumptions, and requested output. A backend timeout,
unsupported operation, or wrong answer contributes to coverage and failure
counts. It cannot disappear from the timing sample.

Report categories separately. A single geometric mean across construction,
algebra, subprocess engines, and generated kernel execution has no useful
interpretation.

## Baselines

- SymEngine official benchmark inputs:
  <https://github.com/symengine/symengine/tree/fac9314c78f2809570494017efc6603befeb4eda/benchmarks>
- SymPy's standalone ASV repository, metadata only pending an explicit license:
  <https://github.com/sympy/sympy_benchmarks/tree/84973d029ecc6cc1df3e0369cb1e7c0492048ef8>

Only inputs with compatible licenses and provenance enter this repository.
Wolfram notebooks, scripts, and outputs remain excluded by `LEGAL.md`.

## Current harness

`benchmark/harnesses/bench_sympy.py` compares the declared `fortsym.sympy`
subset with SymPy 1.14.0. It measures cold end-to-end construction plus
operation and warm core operation separately for expansion, differentiation,
simplification, signed refinement, real/nonzero-guarded `log`/`exp` composition,
principal-square-root powers, direct domain-function simplification of
`sqrt(-oo)`, exact `log(0)`, exact negative-real `log`, and exact `atanh(1)`/`atanh(-1)`, gamma-family domain-head
simplification, inverse domain-head
simplification, and reciprocal-hyperbolic domain-head simplification embedded
in a symbolic fourth-degree expression, directed-infinity `atan2` domain-head
simplification, and Bessel infinity-domain simplification,
Legendre infinity-domain simplification,
and the principal `(-oo)**(3/2)` domain-power branch plus the normalized
`(-oo)**(2/3)` phase, relational
and compound-assumption construction, factorization, and supported assumption
queries. It also records cold `power_constructor` and
`power_one_constructor` rows for the universal `x**0` and `x**1` construction
identities. Every workload passes through a SymPy
correctness check before timing; domain expressions use structural equality
because subtracting equal infinities is itself undefined. The JSON report includes the individual
samples, median, min/max, native-to-SymPy ratio, Python and platform metadata,
and the timing parameters.
The correctness report also checks the direct `sin(zoo)`, `cos(zoo)`, and
`tan(zoo)` boundaries. They are not timing rows: a one-node sentinel call is
dominated by the compatibility ABI crossing rather than the native algorithm,
and would not be a meaningful SymPy performance comparison.
It also checks the unresolved NaN boundaries for Bessel and Legendre heads,
including the representable `besseli(nan, -oo)` result; these are correctness
cases rather than standalone timing rows for the same reason. The same report
checks the high-degree `legendre(3, oo) = nan` boundary and the supported
negative-integer identity cases. It also checks symbolic and integer
`besseli(order, -oo)` phase boundaries, and the zero result for negative
rational exponents. The normalized positive rational phase also has the
matched `domain_phase` cold and warm timing rows; the remaining sentinel cases
are correctness boundaries rather than standalone timing rows. The same
correctness and parity matrix includes `assumption_query` and
`integer_assumption_query`, the latter comparing native `Q.integer` on an
integer-assumed symbol with SymPy 1.14.0 in both cold and warm scopes.
It also includes `rational_assumption_query` and
`algebraic_assumption_query`, which compare the corresponding `Q.rational` and
`Q.algebraic` results in both scopes.
The matrix also includes warm-core `number_predicate` and
`algebraic_predicate` rows, which compare the cached native `Expr.is_number`
and `Expr.is_algebraic` queries on numeric applied expressions.
Its one-node cold end-to-end call is intentionally a diagnostic rather than
an enforced parity row because the ctypes crossing dominates the native
predicate work; the correctness matrix still covers the cold construction.
The `power_constructor:cold_end_to_end` row is likewise a diagnostic: it
includes fresh symbol and result-handle construction and must be passed as an
explicit waiver when parity is enforced. In the latest 2026-08-12 run the
`power_constructor` ratio was 1.53x; the matching
`power_one_constructor:cold_end_to_end` row was 1.51x and is
covered by the same explicit waiver. Thus the recorded matrix has 56 rows, 54
enforced rows, and zero unwaived violations for those construction diagnostics.
The `domain_log_zero` cold and warm rows are also one-node ABI diagnostics:
they measured 5.07x and 3.43x SymPy respectively in the recorded run, and are
explicitly waived for the same construction-versus-simplification boundary.
The `domain_log_negative` cold and warm rows are enforced because the native
principal-branch rewrite is substantially faster than SymPy: the recorded
ratios were 0.005x and 0.029x. The expanded matrix therefore has 60 rows, 56
enforced rows, and zero unwaived violations before the pole diagnostic is
added. The `domain_atanh_pole` cold and warm rows are explicit one-node ABI
diagnostics, measured at 5.34x and 4.15x SymPy; they are waived for the same
construction-versus-simplification boundary. The final matrix has 62 rows,
56 enforced rows, and zero unwaived violations.

Run it from a built checkout with:

```text
FORTSYM_LIBRARY=build/lib/libfortsym.so PYTHONPATH=python \
  python3 benchmark/harnesses/bench_sympy.py \
  --output benchmark/results/sympy-1.14.0.json
```

The harness reports ratios by default. `--enforce-parity` makes it fail when a
native median exceeds the SymPy median. An explicit `--waive operation:scope`
is required for a known exception, and the waiver is recorded in the JSON
report. The current default command is diagnostic. CI or a release benchmark
uses `--enforce-parity` after selecting a pinned machine baseline.

### Focused Wolfram corpus audit

On 2026-08-11, fortsym revision `8ea637a` was checked against the public
`fortsym-bench` revision `16985bcb` using SymPy, Mathics, and the native
`fortsym-wl` backend. The bounded slice covered four scripts
(`math6-1y.wl`, `math8y.wl`, `math10y.wl`, and `math11y.wl`), 189 bindings,
and a 60-second per-binding timeout. It produced 136 agreements, 33 declared
differences, 10 oracle disagreements, and 12 oracle-missing cases. There were
no timeouts, backend errors, unsupported bindings, or translation failures.

This is runner and coverage evidence only, not a parity claim: the full
384-script corpus, independent oracle review, and performance gating remain
open. The benchmark command intentionally returns a nonzero status when
declared differences or oracle disagreements are present.

The committed `bench_complexdom` target, based on the 2026-08-11 working tree,
times 10,000 alternating `sinh` and `cosh` rectangular splits, 10,000 `tan`
rectangular splits, 10,000 `tanh` rectangular splits, 10,000 principal-branch
`log` and `sqrt` splits, and 10,000 structural conjugations of both tangent
heads on prebuilt expressions. Native Fortran took 72.883 ms cold and 0.708 ms warm for
the first workload; matched SymPy 1.14.0 took 6.566109 s cold and 9.389 ms
warm. Native was therefore about 90x faster cold and 13x faster warm. For
`tan` splitting, native took 115.868 ms cold and 0.663 ms warm, while SymPy took
16.236093 s cold and 13.072 ms warm; native was about 140x faster cold and 20x
faster warm. For `tanh` splitting, native took 102.566 ms cold and 0.705 ms
warm, while SymPy took 14.985402 s cold and 12.575 ms warm; native was about
146x faster cold and 18x faster warm. For `log` splitting, native took 104.091
ms cold and 0.637 ms warm, while SymPy took 42.552546 s cold and 14.554 ms
warm; native was about 409x faster cold and 23x faster warm. For principal-branch
`sqrt` splitting, native took 115.458 ms cold and 0.764 ms warm, while SymPy took
14.320794 s cold and 14.853 ms warm; native was about 124x faster cold and 19x
faster warm. For structural `conjugate(tan(...))`, native took 15.147 ms cold and
0.578 ms warm, while
SymPy took 12.283449 s cold and 7.448 ms warm; native was about 811x faster
cold and 13x faster warm. For `conjugate(tanh(...))`, native took 15.672 ms
cold and 0.612 ms warm, while SymPy took 12.296273 s cold and 7.207 ms warm;
native was about 785x faster cold and 12x faster warm. These rows are
diagnostic rather than a release baseline; broader complex-domain workloads
and a pinned machine record remain open.

`fo exec bench_native` writes CSV rows for warm, batched end-to-end native and
SymEngine simplify, differentiation, and expansion calls. Each row includes a
correctness result. This initial harness measures conversion and result
construction with the operation. It does not represent direct SymEngine kernel
time or establish performance parity. The current scope repeats one immutable
expression, so native cache hits are part of the measured workload. Cold,
distinct-expression rows use unique small real shifts and bypass those cache
entries. The CSV row records separate warmup, repetition, and batch counts for
the two scopes.

`fo exec bench_complexdom` writes the `complexdom_v1` native rows used for the
complex-domain cache comparison. Its cold scope clears the assumption-context
pair and single-result caches before each split or conjugation call; its warm
scope primes the relevant cache and then reuses the prebuilt expressions. All
rows validate that the operation succeeded. Compare the split rows with SymPy
1.14.0 `expand_complex` and the conjugation rows with `conjugate` on the same
prebuilt expressions, clearing `sympy.core.cache` before each cold call and
retaining it for the warm call.

The SymPy differential harness also includes `domain_complex`, `domain_abs`,
and `domain_expand_complex` workloads for the public complex-domain adapters.
They compare exact and real-assumption expressions in both cold end-to-end and
warm cached-core scopes, and the parity gate rejects an unwaived native slowdown.
The same differential suite covers the direct `oo`/`-oo`/`zoo`/`nan` boundaries
for all six complex-domain operations; those sentinel cases are correctness
boundaries rather than separate timing rows.

Pinned result CSV files and their TOML environment records live under
`benchmark/results`. A record with uncontrolled affinity or governor is
diagnostic and cannot support a release performance claim.

Native and SymEngine benchmark suites construct separate fresh arenas and
engine instances. This prevents one engine's expanded result or cache growth
from changing the other engine's cold conversion and interning cost. The two
suites use the same input formulae, shifts, warmup counts, batches, validation
points, and correctness oracles. The Python C-ABI arena retains its native
engine and memoization caches across warm calls, while scoped assumptions are
synchronized before each operation. The Python facade also reuses an expanded
result for the same expression until the arena's assumption epoch changes, and
reuses simplified derivatives for repeated `(expression, variable)` calls.
The matched differentiation diagnostic after that cache was added measured
native/SymPy ratios of about 0.14 cold and 0.06 warm; the remaining full-suite
54-workload enforced parity run also passed with zero correctness failures and
zero parity violations; the warm predicate and algebraic-assumption rows were
all at or below the SymPy 1.14.0 median in the recorded run on 2026-08-12.
The warm `number_predicate` and `algebraic_predicate` ratios were 0.32× and
0.75×; `algebraic_assumption_query` was 0.02× warm and 0.12× cold. The
separately waived cold power-constructor diagnostics were 1.53× for `x**0`
and 1.51× for `x**1`; `domain_log_zero` was 5.07× cold and 3.43× warm;
`domain_log_negative` was 0.005× cold and 0.029× warm; `domain_atanh_pole`
was 5.34× cold and 4.15× warm.

`fo exec bench_algebraic` measures the public Fortran `qqbar1` bridge, including
text validation, FLINT reconstruction, the exact operation, canonical
serialization, and immediate fetch. It covers irreducible, reducible, and
repeated-root normalization; Gaussian-rational construction; arithmetic;
conjugation; signed powers; principal square root; exact component signs; and a
one-shot near-64-KiB height refusal. Warm rows reuse one value. Cold rows scale
the defining polynomial or rational text, producing distinct encodings of the
same exact value and therefore preserving the independent expected answer.
Rows report minimum, 5th percentile, median, 95th percentile, maximum, and an
exact correctness flag derived from minimal polynomials, traces, norms,
defining identities, and branch signs.

`bench_qqbar_direct` applies the same eleven successful values and operations
directly to FLINT. The bridge-only resource refusal has no direct baseline. The
direct harness deliberately omits decimal parsing, `qqbar1` serialization, and
the Fortran boundary, and its backend label says `direct_no_text`; it is a
kernel floor, not an end-to-end parity row. Because it is a C++-only target,
reproduce it with a standalone CMake build of that target. Both harnesses use a
monotonic wall clock and the same sample counts. Percentiles select the
one-based sample at `1 + floor(p * (n - 1))` after sorting. Peak resident memory
and the complete commands belong in each pinned result's TOML record rather
than in per-operation CSV rows.

The pinned `2026-07-29-ryzen5950x-gcc16-multinomial` diagnostic compares the
native engine immediately before and after bounded multinomial expansion using
the same native suite order and an arena untouched by another engine. Median
cold expansion of `(x + y + c)^7` fell from 3.260 ms to 0.412 ms, a 7.91x
speedup and 87.4% time reduction. SymEngine 0.14.0 took 0.155 ms in its own
fresh arena, so native remained 2.66x slower. Logical CPU affinity was fixed,
but the `powersave` governor was not controlled; these are diagnostic results,
not a release-level parity claim.

Its metadata records the exact base revision, SHA-256-addressed native-only
baseline patch, separate replay commands, `fo` version/backend, compile flags,
link libraries, affinity, and observed governor. The CSV identifies patched
baseline rows by both base revision and patch hash rather than presenting the
modified source tree as an unmodified Git revision.

The pinned `2026-07-29-ryzen5950x-gcc16-native-exact` diagnostic measures the
subsequent promotion of native scalar arithmetic from checked `int64` to a
lazy compact coefficient with bounded FLINT fallback. Against its immediate
committed parent, median cold expansion moved from 0.600 ms to 0.633 ms
(5.51% slower). SymEngine moved from 0.267 ms to 0.284 ms in the paired
processes, so the native/SymEngine ratio changed from 2.242x to 2.230x
(0.55% lower). Affinity was fixed, but the `powersave` governor was not.
The unchanged SymEngine row moved 6.09% between runs, so this diagnostic
cannot separate implementation cost from machine variation and does not
support a release performance claim. The metadata records replay commands and
SHA-256 hashes for both raw harness outputs; the combined CSV preserves every
raw measurement field and adds the revision labels.

The pinned `2026-07-29-ryzen5950x-gcc16-algebraic` diagnostic covers all eleven
successful algebraic workloads plus the bridge-only resource refusal. Every
one of its 45 rows passed. Warm end-to-end add, multiply, and divide took
1.34x, 1.47x, and 1.16x the separately scoped direct-FLINT no-text floor.
The full text/serialization/ABI wrapper gap was larger for small branch
queries: principal square root and component-sign rows were 16.57x and 27.73x
the direct kernel floor, but the run does not isolate individual overheads.
The near-64-KiB height refusal completed in 1.50 ms. GNU `time` recorded
42,760 KiB command-level RSS for the `fo exec` driver plus bridge child and
11,724 KiB for the direct executable; these scopes are not comparable.
Affinity was fixed and the `powersave` governor was not controlled, so these
measurements are diagnostic and do not establish performance parity or a
general resource bound.
