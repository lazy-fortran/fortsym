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
  <https://github.com/symengine/symengine/tree/master/benchmarks>
- SymPy ASV benchmarks:
  <https://github.com/sympy/sympy_benchmarks>

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
