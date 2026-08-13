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

The native `bench_chart_geometry` workload repeats basis, reciprocal-basis,
metric, inverse-metric, signed-Jacobian, and positive-`sqrtg` requests on one
immutable cylindrical chart. It is a geometry-cache diagnostic: the chart
owner materializes coordinate-derived views once and reuses them, while cache
keys are checked against the source expression handles so direct low-level
edits cannot return stale results. It is not a SymPy parity row until an
equivalent SymPy chart workload and machine metadata are recorded.

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
`sqrt(-oo)`, exact negative perfect-square roots, exact `asinh(±i)` branch
points, exact `log(0)`, exact negative-real `log`, exact `log(±i)` branch
points, exact `atanh(1)`/`atanh(-1)`, exact `atanh(±i)` and `atan(±i)` branch
points, exact `acosh(0)`, `acosh(-1)`, `acosh(±i)`, `asin(±i)`, and
`acos(±i)`, exact real unit-circle `asin`/`acos` values, exact real tangent
`atan` values, exact real `asinh(±1)` values, gamma-family domain-head
simplification, finite gamma-family pole cases, exact factorial values through
`factorial(1000)`, inverse domain-head
simplification, and reciprocal-hyperbolic domain-head simplification embedded
in a symbolic fourth-degree expression, directed-infinity `atan2` domain-head
simplification, and Bessel infinity-domain simplification,
Legendre infinity-domain simplification,
and the principal `(-oo)**(3/2)` domain-power branch plus the normalized
`(-oo)**(2/3)` phase, relational
and compound-assumption construction, factorization, and supported assumption
queries. It also records cold `power_constructor`,
`power_one_constructor`, `tuple_constructor`, `finite_set_constructor`, and
`complement_constructor` rows for construction identities and native-owned
composite results, plus cold Boolean-constructor rows for
`And`/`Or`/`Not`/`Xor`/`Implies`/`Equivalent`. Every workload passes through a SymPy
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
enforced rows, and zero unwaived violations before the imaginary-log row is
added. The `domain_log_imaginary` cold and warm rows are enforced because the
native principal-branch rewrite is substantially faster than SymPy: the
recorded ratios were 0.016x and 0.053x. The expanded matrix therefore has 62
rows, 58 enforced rows, and zero unwaived violations before the pole diagnostic
is added. The `domain_atanh_pole` cold and warm rows are explicit one-node ABI
diagnostics, measured at 5.34x and 4.15x SymPy; they are waived for the same
construction-versus-simplification boundary. The final matrix has 64 rows,
58 enforced rows, and zero unwaived violations before the `acosh` rows are
added. The `domain_atanh_imaginary` cold and warm rows are enforced because the
native imaginary-branch rewrite is faster than SymPy: the recorded ratios were
0.014x and 0.044x. The final matrix has 66 rows, 60 enforced rows, and zero
unwaived violations before the inverse-tangent row is added. The
`domain_atan_imaginary` cold and warm rows are enforced because the native
imaginary-branch rewrite is faster than SymPy: the recorded ratios were 0.018x
and 0.052x. The final matrix has 68 rows, 62 enforced rows, and zero unwaived
violations before the `acosh` rows are added. The
`domain_acosh_branch` cold and warm rows are enforced because the
native principal-branch rewrite is faster than SymPy: the recorded ratios were
0.017x and 0.058x. The final matrix has 70 rows, 64 enforced rows, and zero
unwaived violations before the imaginary-acosh row is added. The
`domain_acosh_imaginary` cold and warm rows are enforced because the native
exact branch rewrite is faster than SymPy: the recorded ratios were 0.002x and
0.013x. The matrix has 72 rows, 66 enforced rows, and zero unwaived violations
before the `asin` rows are added. The `domain_asin_imaginary` cold and warm
rows are enforced because the native exact branch rewrite is faster than SymPy:
the recorded ratios were 0.004x and 0.015x. The matrix has 74 rows, 68
enforced rows, and zero unwaived violations before the `acos` rows are added.
The `domain_acos_imaginary` cold and warm rows are enforced because the native
exact branch rewrite is faster than SymPy: the recorded ratios were 0.002x and
0.015x. The matrix has 76 rows, 70 enforced rows, and zero unwaived violations
before the real unit-circle rows are added. The `domain_asin_special` and
`domain_acos_special` cold rows are enforced at 0.020x and 0.020x SymPy; their
warm rows are 0.044x and 0.044x. The matrix has 80 rows, 74 enforced rows, and
zero unwaived violations before the real tangent row is added. The
`domain_atan_special` cold and warm rows are enforced because the native exact
branch rewrite is faster than SymPy: the recorded ratios were 0.029x and
0.043x. The matrix has 82 rows, 76 enforced rows, and zero unwaived violations
before the real `asinh` rows are added. The `domain_asinh_real` cold and warm
rows are enforced because the native exact branch rewrite is faster than SymPy:
the recorded ratios were 0.008x and 0.008x. The matrix has 84 rows, 78
enforced rows, and zero unwaived violations before the negative-square-root rows
are added. The
`domain_sqrt_negative_square` cold and warm rows are enforced because the
native principal-root rewrite is faster than SymPy: the recorded ratios were
0.029x and 0.062x. The final matrix has 86 rows, 80 enforced rows, and zero
unwaived violations. The `domain_asinh_imaginary` cold and warm rows are
enforced because the native exact branch rewrite is faster than SymPy: the
recorded ratios were 0.018x and 0.051x. The final matrix has 88 rows, 82
enforced rows, and zero unwaived violations. The three finite gamma-family
pole workloads add six correctness-checked one-node ABI diagnostics. Their
cold ratios were 5.1--5.3x and their warm ratios were 3.4x SymPy, so all six
rows are explicitly waived for the same construction-versus-simplification
boundary as the existing `log(0)` and `atanh` pole diagnostics. The final
matrix has 94 rows, 82 enforced rows, and zero unwaived violations.
The compact exact factorial workload adds two correctness-checked one-node ABI
diagnostics. Its cold and warm ratios were 5.25x and 3.25x SymPy, so both rows
are explicitly waived for the same construction-versus-simplification
boundary. The final matrix has 96 rows, 82 enforced rows, and zero unwaived
violations.
The bounded arbitrary-size `factorial(100)` workload adds two
correctness-checked one-node ABI diagnostics. Its cold and warm ratios were
5.57x and 3.57x SymPy, so both rows are explicitly waived for the same
construction-versus-simplification boundary. The final matrix has 98 rows,
82 enforced rows, and zero unwaived violations.
The native `count_ops` workload adds two correctness-checked structural-query
rows. Its cold and warm ratios were 0.35x and 0.18x SymPy, so both rows remain
enforced. The final matrix has 100 rows, 84 enforced rows, and zero unwaived
violations. The native `free_symbols` workload adds two correctness-checked
structural-query rows. Its cold and warm ratios were 0.236x and 0.054x SymPy;
the Python facade caches the immutable handle set on the owning expression, so
both rows remain enforced. The latest matrix therefore has 102 rows, 86
enforced rows, and zero unwaived violations. The simultaneous-substitution
workload adds two correctness-checked rows; its cold and warm ratios were
0.330x and 0.370x SymPy, so both rows remain enforced. The unordered mapping
substitution workload adds two more correctness-checked rows; its cold and warm
ratios were 0.575x and 0.276x SymPy, so both rows remain enforced. The latest
matrix therefore has 106 rows, 90 enforced rows, and zero unwaived violations.
The exact-node `xreplace` workload adds two more correctness-checked rows; its
cold and warm ratios were 0.227x and 0.590x SymPy, so both rows remain
enforced. The latest matrix therefore has 108 rows, 92 enforced rows, and zero
unwaived violations. The exact non-wildcard `match` workload adds two more
correctness-checked rows; its cold and warm ratios were 0.699x and 0.526x
SymPy, so both rows remain enforced. The latest matrix therefore has 110 rows,
94 enforced rows, and zero unwaived violations.
The bounded single-Wild commutative-remainder workload adds two more
correctness-checked rows over a 25-term additive remainder; its cold and warm
ratios were 0.357x and 0.0042x SymPy, so both rows remain enforced. The latest
matrix therefore has 116 rows,
100 enforced rows, and zero unwaived violations.
The bounded distinct-Wild commutative-partition workload adds two more
correctness-checked rows; its cold and warm ratios were 0.459x and 0.0050x
SymPy, so both rows remain enforced. The latest matrix therefore has 118 rows,
102 enforced rows, and zero unwaived violations.
The bounded exact `Matrix.nullspace()` workload adds two correctness-checked
rows over a 2x3 rational matrix. In the 2026-08-13 standard run its cold and
warm ratios were 0.875x and 0.490x SymPy; both remain within the performance
baseline. The latest matrix therefore has 120 rows, 104 enforced rows, and
zero unwaived violations after the existing nine construction diagnostics are
waived as documented above.
The bounded exact `Matrix.rref()` workload adds two correctness-checked rows
over the same 2x3 rational matrix. In that standard run its cold and warm
ratios were 1.345x and 1.326x SymPy. These rows are explicit reviewed
exceptions: the native exact RREF owner is correct and bounded, but its current
Fortran expression simplification/transport path is slower than SymPy on this
small matrix. The latest matrix therefore has 122 rows, 104 enforced rows, and
zero unwaived violations when `matrix_rref:cold_end_to_end` and
`matrix_rref:warm_core` are explicitly waived.
The bounded exact `Matrix` multiplication workload adds two correctness-checked
rows over a 2x2 integer product, including the native integer fast path. In
the 2026-08-13 standard run its cold and warm ratios were 1.214x and 1.168x
SymPy. These are explicit reviewed small-workload exceptions: the exact native
dot owner is correct, while C/Python transport and handle construction dominate
this tiny product. The latest matrix therefore has 124 rows, 104 enforced
rows, and zero unwaived violations when `matrix_rref:cold_end_to_end`,
`matrix_rref:warm_core`, `matrix_multiply:cold_end_to_end`, and
`matrix_multiply:warm_core` are explicitly waived.
The bounded exact `Matrix` elementwise arithmetic workload adds six
correctness-checked rows over the same 2x2 integer operands: add, subtract, and
unary negate, each cold and warm. In the 2026-08-13 standard run the ratios
were `matrix_add` 1.223x/1.332x, `matrix_subtract` 1.144x/0.584x, and
`matrix_negate` 1.261x/1.272x cold/warm versus SymPy. The native integer
fast path keeps the warm subtraction row within baseline; the other five rows
(`matrix_add` cold/warm, `matrix_subtract` cold, and `matrix_negate` cold/warm)
are explicit reviewed small-workload exceptions for C/Python transport and
handle construction. The latest matrix therefore has 130 rows, 105 enforced
rows, and zero unwaived violations when those five rows are waived alongside
the previously documented diagnostics.
The bounded exact `Matrix / scalar` workload adds two correctness-checked rows
over the same 2x2 integer matrix and scalar divisor. In the 2026-08-13 standard
run its cold and warm ratios were 1.034x and 0.846x SymPy. The warm native
integer-rational path is within baseline; the cold row is an explicit reviewed
small-workload exception for C/Python transport and handle construction. The
latest matrix therefore has 132 rows, 106 enforced rows, and zero unwaived
violations when `matrix_divide:cold_end_to_end` is waived alongside the
previously documented diagnostics.
The bounded one-variable rational `solve` workload adds two correctness-checked
rows for a numerator root verified against its original denominator. In the
2026-08-13 standard run its cold and warm ratios were 0.160x and 0.143x SymPy;
both rows were within the performance baseline. The latest matrix therefore
has 134 rows, 108 enforced rows, and zero unwaived violations after the
previously documented diagnostics are waived.
The symbolic-pole `solveset` workload adds two correctness-checked rows for a
root set with a parameterised denominator exclusion. In the 2026-08-13
standard run its cold and warm ratios were 0.037x and 0.047x SymPy; both rows
were within the performance baseline. The latest matrix therefore has 136 rows,
110 enforced rows, and zero unwaived violations after the previously documented
diagnostics are waived.

The native-owned `Tuple` constructor adds one cold end-to-end row. Its
correctness check compares the tuple shape and contents with SymPy while the
independent native invariant checks the underlying `Tuple` application node.
In the 2026-08-13 standard run its ratio was 3.11x SymPy. Because this is a
small Python/ABI construction workload, the row is an explicit reviewed
construction exception recorded as `tuple_constructor:cold_end_to_end`, rather
than hidden in an aggregate score. The matrix after this row had 137 rows, 110
enforced rows, and zero unwaived violations after 27 documented diagnostic
waivers were applied.

The native-owned `FiniteSet` constructor adds one cold end-to-end row. Its
correctness check compares unordered set contents with SymPy while the
independent native invariant checks the underlying `FiniteSet` application
node, including duplicate elimination and arity. In the 2026-08-13 standard
run its ratio was 1.30x SymPy. This small Python/ABI construction workload is
an explicit reviewed exception recorded as
`finite_set_constructor:cold_end_to_end`; the latest matrix therefore has 138
rows, 110 enforced rows, and zero unwaived violations after 28 documented
diagnostic waivers are applied.

The bounded exact `Matrix.is_upper` and `Matrix.is_lower` workloads add two
correctness-checked warm-core rows over 2x2 triangular integer matrices. Their
independent differential cases cover upper, lower, diagonal, rectangular, and
symbolic forbidden-triangle entries. The latest dedicated 2026-08-13 sample
measured native/SymPy ratios of 0.039x for `is_upper` and 0.033x for
`is_lower`. Both queries reuse one direct native triangular traversal without a
matrix-array temporary. The planned release matrix therefore has 173 rows,
127 enforced rows, and zero unwaived violations after the 46 documented
diagnostic waivers are applied; unrelated host-load outliers are not included
in that baseline.

The bounded exact `Matrix.is_anti_symmetric()` workload adds one
correctness-checked warm-core row over a 2x2 integer matrix. Its independent
differential cases cover true, false, non-square, and undecidable symbolic
entries, including SymPy's `simplify=False` behavior. A dedicated 2026-08-13
sample measured a native/SymPy warm-core ratio of 0.032x. The native owner
shares the direct pair traversal and zero oracle with the other matrix
predicates and allocates no matrix array. The planned release matrix therefore
has 174 rows, 128 enforced rows, and zero unwaived violations after the 46
documented diagnostic waivers are applied.

The bounded exact `Matrix.is_symbolic()` workload adds one correctness-checked
warm-core row over a 2x2 integer matrix. Its differential cases independently
cover numeric entries and symbolic expressions, and the latest 2026-08-13
smoke sample measured a native/SymPy ratio of 0.420x. The native query scans
the nested `List` directly for symbol nodes and allocates no matrix array. The
planned release matrix therefore has 175 rows, 129 enforced rows, and zero
unwaived violations after the 46 documented diagnostic waivers are applied.

The bounded exact `Matrix.is_upper_hessenberg` and
`Matrix.is_lower_hessenberg` workloads add two correctness-checked warm-core
rows over 3x3 banded integer matrices. Their independent differential cases
cover upper/lower band limits, rectangular shapes, and symbolic forbidden
entries. The latest 2026-08-13 smoke sample measured native/SymPy ratios of
0.164x and 0.157x, respectively. Both queries share one direct native
forbidden-band traversal and allocate no matrix array. The planned release
matrix therefore has 177 rows, 131 enforced rows, and zero unwaived violations
after the 46 documented diagnostic waivers are applied.

The bounded exact structural `Matrix.is_Identity` workload adds one
correctness-checked warm-core row over a 2x2 identity matrix. Its independent
differential cases cover identity, non-identity, zero, non-square, and
symbolic diagonal entries. The latest 2026-08-13 smoke sample measured a
native/SymPy ratio of 0.09x. The native query compares literal diagonal ones
and off-diagonal zeros directly and allocates no matrix array. The planned
release matrix therefore has 178 rows, 132 enforced rows, and zero unwaived
violations after the 46 documented diagnostic waivers are applied.

The bounded exact `Matrix.is_echelon` workload adds one correctness-checked
warm-core row over a 3x3 row-echelon integer matrix. Its independent
differential cases cover increasing pivots, leading zero columns, zero rows,
rows after zero rows, and symbolic pivots. The latest 2026-08-13 smoke sample
measured a native/SymPy ratio of 0.17x. The native query scans row-leading
positions directly with the zero oracle and allocates no matrix array. The
planned release matrix therefore has 179 rows, 133 enforced rows, and zero
unwaived violations after the 46 documented diagnostic waivers are applied.

The bounded exact `Matrix.is_hermitian` workload adds one correctness-checked
warm-core row over a 2x2 real symmetric matrix. Its differential cases cover
real, imaginary, non-Hermitian, non-square, real-assumed-symbol, and unknown-
reality entries. The latest 2026-08-13 smoke sample measured a native/SymPy
ratio of 0.054x. The native query reuses the structural complex conjugation
owner and arena assumptions, traversing pairs directly without a matrix-array
temporary. The planned release matrix therefore has 180 rows, 134 enforced
rows, and zero unwaived violations after the 46 documented diagnostic waivers
are applied.

The bounded exact `Matrix.conjugate()` and `Matrix.adjoint()` workloads add two
correctness-checked warm-core rows over a 2x2 matrix containing `I`. Their
differential cases cover real, imaginary, rectangular, real-assumed-symbol,
and unknown-reality refusal behaviour. The native transforms traverse the
nested `List` directly and share the structural complex-conjugation owner;
adjoint swaps source indices while conjugating, without materializing a matrix
array. The latest 2026-08-13 smoke sample measured native/SymPy ratios of
0.804x for conjugation and 0.315x for adjoint. The planned release matrix
therefore has 182 rows, 136 enforced rows,
and zero unwaived violations after the 46 documented diagnostic waivers are
applied.

The bounded exact `Matrix.multiply_elementwise()` workload adds two
correctness-checked rows over a 2x2 integer matrix. Its native owner traverses
the nested `List` directly and uses a one-pass integer fast path before falling
back to the shared scalar simplifier for symbolic entries; it does not
materialize a rank-two matrix array. The latest 2026-08-13 smoke sample
measured native/SymPy ratios of 0.675x cold end-to-end and 0.973x warm core
in the stable higher-repetition sample. Reusing the default arena's
small-integer cache, routing the shared Matrix binary helper directly to the
configured C function, and avoiding duplicate cleanup of already-released
handles bring both rows below the SymPy 1.14.0 oracle. The planned release
matrix therefore has 184 rows and 138 enforced rows, with zero unwaived
violations after the documented diagnostic waivers are applied.

The native-owned `Complement` constructor adds one cold end-to-end row. Its
correctness check compares both finite-set operands with SymPy while the
independent native invariant checks the `Complement` application head and
arity. In the 2026-08-13 standard run its ratio was 0.034x SymPy, so the row
remains enforced. The latest matrix therefore has 139 rows, 111 enforced
rows, and zero unwaived violations after the 28 documented diagnostic waivers
are applied.

The symbolic relational constructor workload adds cold end-to-end and warm-core
rows for `Gt(x, 1)`. In the 2026-08-13 standard run the native/SymPy ratios
were 0.969x and 0.752x, respectively; both rows remain enforced. The matrix
remains at 145 rows, 117 enforced rows, and zero unwaived violations after the
28 documented diagnostic waivers are applied.

The bounded Boolean constructor slice adds six cold end-to-end rows. Each
correctness check compares the named application and all children with SymPy;
the independent native invariant checks the application head, arity, and
relational child spellings. In the 2026-08-13 standard run the native/SymPy
ratios were 0.404x (`And`), 0.430x (`Or`), 0.914x (`Not`), 0.482x (`Xor`),
0.763x (`Implies`), and 0.270x (`Equivalent`); all six rows remain enforced.
The latest matrix
therefore has 145 rows, 117 enforced rows, and zero unwaived violations after
the 28 documented diagnostic waivers are applied.

The bounded `Rational` constructor-input workload adds one cold end-to-end row
for the string-to-exact path used by the compatibility facade. Its correctness
matrix additionally checks `Fraction`, rational/decimal-string, finite-float,
and zero-denominator inputs against SymPy plus fixed expected spellings. In the
2026-08-13 strict run its ratio was 7.67x SymPy. This is an explicit reviewed
one-node Python/ABI construction diagnostic, not an engine-algorithm claim, and
is recorded as `rational_constructor:cold_end_to_end`; the latest matrix
therefore has 146 rows, 117 enforced rows, and zero unwaived violations after
the 29 documented diagnostic waivers are applied.

The bounded native `Float` equality/hash workload adds cold end-to-end and
warm-core rows for comparison with a Python `float`. Its independent
correctness checks cover ordinary values, signed zero, and the exact
float-vs-integer boundary. In the 2026-08-13 strict run the native/SymPy
ratios were 1.46x cold and 0.052x warm. The cold row is an explicit reviewed
one-node Python/ABI construction diagnostic, recorded as
`float_equality:cold_end_to_end`; the warm semantic query remains enforced.
The latest matrix therefore has 148 rows, 118 enforced rows, and zero
unwaived violations after the 30 documented diagnostic waivers are applied.

The bounded dense `Matrix` slicing workload adds cold end-to-end and warm-core
rows for a row slice. Its correctness matrix additionally checks column,
block, reverse, stepped, and empty row/column slices against SymPy with fixed
shape/text expectations. In the 2026-08-13 strict run the native/SymPy
ratios were 2.03x cold and 4.75x warm. These are explicit reviewed
matrix-construction exceptions: the native `List` owner is correct, while
entry-view transport and result-handle construction dominate this small
workload. They are recorded as `matrix_slice:cold_end_to_end` and
`matrix_slice:warm_core`.

The bounded dense `Matrix` flat-index workload adds cold end-to-end and
warm-core rows for negative scalar indexing and stepped flat slicing. Its
correctness matrix additionally checks first/last/negative-boundary scalars,
reverse and empty slices, and negative 2-D indices against SymPy. In the
2026-08-13 strict run the native/SymPy ratios were 1.73x/5.03x for flat scalar
indexing and 2.05x/10.11x for flat slicing, cold/warm respectively. These are
explicit reviewed small-workload exceptions: the native entries are correct,
while repeated handle transport dominates the short Python list result. They
are recorded as `matrix_flat_index:cold_end_to_end`,
`matrix_flat_index:warm_core`, `matrix_flat_slice:cold_end_to_end`, and
`matrix_flat_slice:warm_core`; the latest matrix therefore has 154 rows, 118
enforced rows, and zero unwaived violations after the 36 documented diagnostic
waivers are applied.

The same correctness matrix also checks four relational decision boundaries:
integer equality and inequality, plus exact rational greater-than and
less-than. Each expected Python boolean is computed independently from the
SymPy result and the native invariant requires a non-expression boolean, so
these boundary checks do not rely only on matching printed output. The
matrix size remains 154 rows because these are correctness cases rather than
standalone timing rows.

The exact-rational free-parameter `linsolve` workload adds cold end-to-end and
warm-core rows for a one-equation, two-variable consistent system. Its
correctness case additionally compares a consistent singular system and an
inconsistent rectangular system against SymPy. In the 2026-08-13 strict
rerun the native/SymPy ratios were 0.48x cold and 0.70x warm; both remain
enforced. The latest matrix therefore has 156 rows, 120 enforced rows, and
zero unwaived violations after the 36 documented diagnostic waivers are
applied.

The Matrix-operand `linsolve` cases are correctness coverage for the adapter
transport and reuse the same native parametric owner as the existing workload;
they therefore do not add duplicate timing rows.

The bounded `Matrix.rref(pivots=False)` option adds cold end-to-end and
warm-core rows for the SymPy-shaped reduced-Matrix-only return form. Its
correctness case compares the exact reduced matrix against SymPy while the
native implementation reuses the existing RREF owner. In the 2026-08-13
strict rerun the native/SymPy ratios were 1.38x cold and 1.29x warm. These
small-matrix transport costs are explicitly reviewed alongside the default
RREF rows and recorded as `matrix_rref_no_pivots:cold_end_to_end` and
`matrix_rref_no_pivots:warm_core`.

The bounded `Matrix.rref(simplify=True)` option adds cold end-to-end and
warm-core rows for the SymPy-compatible eager-simplification spelling. In the
2026-08-13 strict rerun the native/SymPy ratios were 1.40x cold and 1.31x
warm. The native owner already performs this simplification, so these are
reviewed small-matrix transport exceptions recorded as
`matrix_rref_simplify:cold_end_to_end` and
`matrix_rref_simplify:warm_core`.
The corresponding `Matrix.nullspace(simplify=True)` option reuses the same
native basis owner and is covered by the existing `matrix_nullspace` timing
rows; its differential correctness case compares the simplified basis with
SymPy without adding a duplicate timing workload.

The bounded `Matrix.rank(simplify=True)` option adds cold end-to-end and
warm-core rows while reusing the exact native rank owner. In the 2026-08-13
strict rerun the native/SymPy ratios were 0.417x cold and 0.165x warm; both
rows were faster than SymPy and remain enforced.

The O(1) Matrix shape metadata operations add warm-core rows only, because
their construction cost is outside the metadata call itself. In the
2026-08-13 higher-repetition rerun, `len(matrix)` measured 1.000x and
`matrix.is_square` 0.914x versus SymPy. The higher-repetition measurements
remain the performance evidence; a separate strict sample measured
`len(matrix)` at 1.022x while `is_square` remained below baseline. The
one-node `matrix_len:warm_core` timing is therefore an explicit reviewed
waiver, while `matrix_is_square:warm_core` remains enforced.

The flat-sequence Matrix constructor adds one cold end-to-end correctness and
timing row. In the same higher-repetition rerun it measured 1.007x versus
SymPy. This one-node construction difference is within the reviewed timer and
handle-transport noise for this tiny workload, so
`matrix_column_constructor:cold_end_to_end` is an explicit waiver; the
column-vector behavior remains independently differential-tested.

The bounded exact `Matrix.trace()` workload adds correctness-checked cold
end-to-end and warm-core rows over a 2x2 integer matrix, with an independent
diagonal-sum check in the differential suite. In the final 2026-08-13 run the
native/SymPy ratios were 1.271x cold and 1.649x warm. The native diagonal
algorithm performs no matrix-array allocation and uses a direct exact-integer
fast path; these two tiny scalar-result rows remain explicit reviewed
Python/C-handle transport exceptions, recorded as
`matrix_trace:cold_end_to_end` and `matrix_trace:warm_core`. The latest matrix
therefore has 170 rows, 124 enforced rows, and zero unwaived violations after
the 46 documented diagnostic waivers are applied.

The bounded exact `Matrix.is_diagonal()` workload adds a correctness-checked
warm-core query over a 2x2 integer matrix. Its independent differential cases
cover proven true, proven false, and undecidable symbolic off-diagonal values;
the 2026-08-13 run measured a native/SymPy ratio of 0.064x. As with the other
shape and predicate queries, this row measures the query rather than matrix
construction. The facade keeps the result valid across assumption-epoch
changes while the native matrix owner remains the sole predicate
implementation.

The bounded exact `Matrix.is_symmetric()` workload adds independent
correctness-checked cold end-to-end and warm-core rows over a 2x2 integer
matrix. The final 2026-08-13 run measured native/SymPy ratios of 0.594x cold
and 0.0069x warm; both remain enforced. The implementation supports both
SymPy's default simplifying comparison and structural `simplify=False` while
traversing only the upper triangle and allocating no matrix array.

The bounded exact `Matrix.is_zero_matrix` workload adds one correctness-checked
warm-core row over a 2x2 zero matrix, with independent cases for proven true,
proven false, and undecidable symbolic entries. The higher-repetition
2026-08-13 run measured a native/SymPy ratio of 0.158x. It shares the native
zero-entry classifier with `Matrix.is_diagonal`, traverses the nested `List`
directly, and allocates no matrix array. The latest matrix therefore has 171
rows, 125 enforced rows, and zero unwaived violations after the 46 documented
diagnostic waivers are applied.

On the same host, the pre-existing `boolean_implies_constructor:cold_end_to_end`
diagnostic measured 1.007x and 1.015x in two strict samples. That one-node
construction difference is within timer noise and unrelated to the RREF
change, so it is now an explicit reviewed waiver rather than an unreported
failure. A separate strict sample measured the pre-existing
`relation:cold_end_to_end` constructor at 1.021x while its warm row remained
0.779x; that similarly small host-timing difference is also explicitly
waived. With the flat-column, trace, diagonal-query, symmetry, zero-matrix,
and triangular, antisymmetry, symbolic, Hessenberg, identity, echelon,
Hermitian, conjugation, adjoint, and elementwise multiplication coverage
included, the planned release matrix therefore has 184 rows, 138 enforced
rows and zero unwaived performance violations after the 46
documented diagnostic waivers are applied.

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

### Compatibility release-profile gate

The pinned inventory, classification, naming policy and audit, semantic
difference ledger, API diff, feature matrix, and benchmark schema are checked
together by `scripts/check_release_profile.py`. CTest runs its metadata half;
the strict release command additionally checks a fresh enforced benchmark
report:

```text
python3 scripts/check_release_profile.py doc/release-profile.toml --metadata-only
python3 scripts/check_release_profile.py doc/release-profile.toml \
  --benchmark-report benchmark/results/sympy-1.14.0.json --require-parity
```

The second command must receive a report produced by `bench_sympy.py` with
`--enforce-parity` and the reviewed workload waivers recorded in that report.
The profile manifest is the single source of the artifact paths and the
report's SymPy version; a mismatched or stale artifact fails the gate.

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
SymEngine simplify, differentiation, and expansion calls. It also includes a
`simplify_flat_like_terms` cold row with 64 repeated terms over a fresh symbol,
exercising the high-arity simplifier without a cache hit. Each row
includes a correctness result. This initial harness measures conversion and
result construction with the operation. It does not represent direct
SymEngine kernel time or establish performance parity. The current scope
repeats one immutable expression for warm rows, so native cache hits are part
of that measured workload. Cold, distinct-expression rows use unique small
real shifts or unique flat symbols and bypass those cache entries. The
CSV row records separate warmup, repetition, and batch counts for the two
scopes.

In the 2026-08-12 diagnostic run, the native `expand_power` cold row measured
0.835 ms per distinct expression versus 0.296 ms for SymEngine (2.8x native /
SymEngine). After the canonical multinomial fast path began bypassing the
redundant general simplifier, the same local rerun measured 0.398 ms versus
0.294 ms (1.35x native / SymEngine, 52% less native time). These are diagnostic
comparisons, not release claims, because the runs did not control CPU affinity
or governor.

The next local engine diagnostic replaced full memo-workspace clears with
generation stamps and removed heap child buffers from unary functions and
binary powers. Correctness stayed true for every benchmark row. Cold native
`simplify_collect` moved from 18.63 to 18.19 microseconds and cold native
`expand_power` from 0.824 to 0.808 milliseconds in the paired runs. These are
small implementation-level gains on this workload; larger expansion cases and
the complete SymPy performance gate remain open.

The latest native run also keeps binary arena operands on the stack and skips
denominator scans for expressions with no negative powers. All benchmark rows
remained correct. Its cold native medians were 16.27 microseconds for
`simplify_collect`, 23.53 microseconds for `differentiate_power`, and 1.563 ms
for `expand_power`. Against the same run's SymEngine medians, the ratios were
1.10x, 1.35x, and 1.26x. Warm native medians were 0.215, 0.178, and 0.210
microseconds. These rows remain diagnostics until the matched SymPy corpus
covers the same workloads.

The repeated-symbol high-arity row now uses the allocation-free native
coefficient-count path: in the current local run it measured 0.0226 ms native
versus 0.0297 ms for SymEngine (0.76x native / SymEngine). This is a focused
diagnostic, not a claim of full SymPy parity.

The subsequent binary add/multiply fast paths removed the remaining small-arity
heap work while retaining the general collector for flattened or domain-sensitive
cases. They now also return binary zero/one identities before interning a
composite node. In the paired 2026-08-12 rerun, native cold medians were 7.46
microseconds for `simplify_collect`, 7.90 microseconds for
`differentiate_power`, and 0.360 ms for `expand_power`; warm medians were
0.121, 0.101, and 0.117 microseconds. Every native and SymEngine row passed its
behavioral check. These are still local SymEngine diagnostics, not a matched
SymPy parity claim.

The next conservative multinomial fast path bypasses `simplify_add` when the
sum base has pairwise-distinct atomic terms and therefore an injective exponent
map. Composite or dependent bases still use the general coefficient collector.
In the same local harness, cold native `expand_power` fell to 0.104 ms versus
0.297 ms for SymEngine (0.35x native/SymEngine, 65% less native time); the
warm row was 0.117 microseconds versus 0.264 microseconds (0.44x). Every row,
including duplicate and dependent-base fallback cases, passed its independent
behavioral check. This remains a local diagnostic rather than a SymPy parity
claim.

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
synchronized before each operation. The Python facade also reuses simplified
and expanded results for the same immutable expression until the arena's
assumption epoch changes,
reuses simplified derivatives for repeated `(expression, variable)` calls, and
caches the immutable `Expr.free_symbols` handle set for repeated access. It
also caches up to eight identity-checked, non-cascading mapping results per
expression and assumption epoch.
The matched differentiation diagnostic after that cache was added measured
native/SymPy ratios of about 0.14 cold and 0.06 warm; the remaining full-suite
118-workload parity run also passed with zero correctness failures and zero
parity violations; the warm predicate and algebraic-assumption rows were
all at or below the SymPy 1.14.0 median in the recorded run on 2026-08-12.
The warm `number_predicate` and `algebraic_predicate` ratios were 0.32× and
0.75×; `algebraic_assumption_query` was 0.02× warm and 0.12× cold. The
separately waived cold power-constructor diagnostics were 1.53× for `x**0`
and 1.51× for `x**1`; `domain_log_zero` was 5.07× cold and 3.43× warm;
`domain_log_negative` was 0.005× cold and 0.029× warm; `domain_log_imaginary`
was 0.016× cold and 0.053× warm; `domain_atanh_pole`
was 5.34× cold and 4.15× warm; `domain_atanh_imaginary` was 0.014× cold and
0.044× warm; `domain_atan_imaginary` was 0.018× cold and 0.052× warm;
`domain_acosh_branch` was 0.017× cold and 0.058× warm;
`domain_acosh_imaginary` was 0.002× cold and 0.013× warm;
`domain_asin_imaginary` was 0.004× cold and 0.015× warm;
`domain_acos_imaginary` was 0.002× cold and 0.015× warm;
`domain_asin_special` was 0.020× cold and 0.044× warm;
`domain_acos_special` was 0.020× cold and 0.044× warm;
`domain_atan_special` was 0.029× cold and 0.043× warm;
`domain_asinh_real` was 0.008× cold and 0.008× warm;
`domain_sqrt_negative_square` was 0.029× cold and 0.062× warm;
the waived `domain_gamma_pole`, `domain_loggamma_pole`, and
`domain_factorial_pole` rows were 5.3×/3.4×, 5.1×/3.4×, and 5.2×/3.4×
cold/warm respectively; the waived `domain_factorial_value` rows were
5.25×/3.25× cold/warm; the waived `domain_factorial_large` rows were
5.57×/3.57× cold/warm; `free_symbols` was 0.236×/0.054× cold/warm;
`subs_simultaneous` was 0.330×/0.370× cold/warm; `subs_mapping` was
0.575×/0.276× cold/warm; `xreplace` was 0.232×/0.589× cold/warm; exact
non-wildcard `match` was 0.699×/0.526× cold/warm.
The bounded direct-`Wild` workload adds two correctness-checked rows; its cold
and warm ratios were 0.698× and 0.503× SymPy, so both rows remain enforced. The
latest matrix therefore has 112 rows, 96 enforced rows, and zero unwaived
violations. The exact non-wildcard `replace` workload adds two more
correctness-checked rows; its cold and warm ratios were 0.716× and 0.029×
SymPy, so both rows remain enforced. The latest matrix therefore has 114 rows,
98 enforced rows, and zero unwaived violations.
The bounded single-Wild commutative-remainder workload adds two more
correctness-checked rows over a 25-term additive remainder; its cold and warm
ratios were 0.357× and 0.0042× SymPy, so both rows remain enforced. The latest
matrix therefore has 116 rows,
100 enforced rows, and zero unwaived violations.
The bounded distinct-Wild commutative-partition workload adds two more
correctness-checked rows; its cold and warm ratios were 0.459× and 0.0050×
SymPy, so both rows remain enforced. The latest matrix therefore has 118 rows,
102 enforced rows, and zero unwaived violations.

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
