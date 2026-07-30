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

`fo exec bench_native` writes CSV rows for warm, batched end-to-end native and
SymEngine simplify, differentiation, and expansion calls. Each row includes a
correctness result. This initial harness measures conversion and result
construction with the operation. It does not represent direct SymEngine kernel
time or establish performance parity. The current scope repeats one immutable
expression, so native cache hits are part of the measured workload. Cold,
distinct-expression rows use unique small real shifts and bypass those cache
entries. The CSV row records separate warmup, repetition, and batch counts for
the two scopes.

Pinned result CSV files and their TOML environment records live under
`benchmark/results`. A record with uncontrolled affinity or governor is
diagnostic and cannot support a release performance claim.

Native and SymEngine benchmark suites construct separate fresh arenas and
engine instances. This prevents one engine's expanded result or cache growth
from changing the other engine's cold conversion and interning cost. The two
suites use the same input formulae, shifts, warmup counts, batches, validation
points, and correctness oracles.

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
