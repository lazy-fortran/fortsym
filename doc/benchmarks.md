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
principal-square-root powers, relational and compound-assumption construction,
factorization, and supported assumption queries. Every workload passes through a SymPy
correctness check before timing. The JSON report includes the individual
samples, median, min/max, native-to-SymPy ratio, Python and platform metadata,
and the timing parameters. Run it from a built checkout with:

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
