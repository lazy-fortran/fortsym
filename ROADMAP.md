# fortsym roadmap

The target is **100% coverage of the fortsym-bench corpus**: 384 Wolfram-language
derivations across 50 projects, every result either agreeing with an open-source
oracle or refused with a named construct. Nothing here is scheduled by taste —
each milestone is ordered by how many corpus scripts it unblocks, and every
method is chosen from the literature and cited.

Operational rules live in `doc/roadmap.md`. Licences and clean-room boundaries
live in `LEGAL.md` and `COMPATIBILITY.md`. Method provenance lives in
`doc/provenance.md`, and every algorithm below has an entry there.

## Acceptance

A milestone is done when it has a callable public operation, an **independent**
behavioural oracle (not another CAS agreeing with it), documentation, a
reproducible benchmark row, and a measurable rise in corpus coverage. A
capability bit is added only with the operation it describes. Unsupported input
returns a diagnostic or `UNKNOWN` — never a guess.

### Regression gate

Every change follows the same evidence path:

1. State the supported semantics and the refusal boundary.
2. Add an independently derived behavioral test. A test that only reproduces
   the implementation's output is insufficient.
3. Add adversarial cases for wrong signs, domains, singularities, precision,
   shape, and resource limits when they apply.
4. Run the focused native tests, the required `fo` pipeline, and the affected
   corpus slice against SymPy and Mathics.
5. Compare agreement, disagreement, refusal, timeout, error, and runtime counts
   with the committed baseline.
6. Update cache scope or versioning when semantics change. Keep cached results
   for unaffected features.

A milestone cannot merge if it introduces a silent fallback, a new hang, a
loss of exactness, or an unexplained coverage regression. Oracle disagreement
remains a diagnostic outcome. It is never converted into native agreement.

## Performance contract

Performance is a release criterion, not a follow-up.

| Workload | Target | Current |
|---|---|---|
| expand, diff, simplify on the SymEngine benchmark inputs | within **1.5×** of SymEngine 0.14.0 | 2.66× on `(x+y+c)^7` |
| any corpus script | faster than SymPy on the same input | not yet measured |
| generated kernel runtime | at or below the hand-written routine it replaces | not yet measured |

Rules that follow from that contract:

- Sparse multivariate arithmetic uses **heap-based multiplication and division**
  (Monagan & Pearce 2007, 2010), which is the current state of the art for
  sparse distributed polynomials and beats geobucket at the sizes the corpus
  reaches. Dense univariate falls back to **Kronecker substitution** onto FLINT
  `fmpz_poly`.
- The arena stays hash-consed with cache-dense node layout. No allocation in
  inner loops; memoisation keyed on interned ids.
- Every claim cites a pinned result file under `benchmark/results` produced with
  fixed CPU affinity and a documented frequency governor. A run without those is
  diagnostic and cannot support a release statement (`doc/benchmarks.md`).

## Architecture and routing

Best-of-breed means choosing the strongest appropriate engine for each domain.
The native evaluator remains the exact expression and dispatch layer. It does
not absorb numerical algorithms that belong in a numerical library.

| Domain | Owner | Boundary |
|---|---|---|
| exact expressions and algebraic normalization | fortsym arena and native engine | preserves exact nodes and explicit refusal |
| sparse and dense polynomial algebra | modular algorithms with FLINT-backed kernels | promoted only after independent polynomial identities pass |
| exact elementary integration | native integration modules | bounded rules first, Hermite and Risch-class work next |
| numerical integration and fitting | fortnum | exposed through `NIntegrate`, `FindRoot`, `Interpolation`, and `Fit` adapters |
| arbitrary-precision and certified numerics | Arb or MPFR-backed numerical layer | values carry precision or error information |
| plotting and rendering | fortplot | fortsym samples expressions and passes structured data |
| differential comparison | SymPy and Mathics subprocess oracles | raw outputs and verdicts are cached separately |

The boundary is semantic. `Integrate` must preserve symbolic parameters and
exact identities. `NIntegrate` may dispatch to fortnum only after the expression,
domain, parameters, precision, and error policy are numeric and explicit.

## Measured state

Latest measured corpus-wide state, measured 2026-08-03, `fortsym-bench` at
384 scripts with the current native and comparison caches:

This table is a committed native baseline. It is updated only after the root
backend and benchmark harness revisions used to produce it have been committed.
Interrupted experiments and uncommitted generated artifacts do not change the
reported state.

| | |
|---|---:|
| native cache rows with successful results | **378 / 384 (98%)** |
| native rows refusing with a named construct | 3 |
| native rows exceeding the time budget | 3 |
| native rows ending in a runner error | 0 |
| crashes | **0** |
| v74 native/SymPy audit after cache and native parity changes | 1:10.89 / 1.61 GiB |
| v75 cache-preserving oracle refresh | 6.10 s / 491 MiB |
| latest v75 warm compact raw-output and verdict audit | 0.81 s / 491 MiB |
| latest v76 UV warm compact raw-output and verdict audit | 0.90 s / 490 MiB |
| latest v77 UV warm compact raw-output and verdict audit | 1.06 s / 488 MiB |
| latest v78 UV warm compact raw-output and verdict audit | 1.00 s / 488 MiB |
| latest v79 UV warm compact raw-output and verdict audit | 1.33 s / 489 MiB |
| latest v80 UV warm compact raw-output and verdict audit | 1.10 s / 488 MiB |
| latest v81 UV warm compact raw-output and verdict audit | 1.06 s / 488 MiB |
| v81 native cache-preserving rebuild audit | 1:32.48 / 1.54 GiB |
| latest v82 UV warm compact raw-output and verdict audit | 1.06 s / 489 MiB |
| latest v83 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v84 UV warm compact raw-output and verdict audit | 0.95 s / 489 MiB |
| latest v85 UV warm compact raw-output and verdict audit | 1.79 s / 490 MiB |
| latest v86 UV warm compact raw-output and verdict audit | 1.11 s / 489 MiB |
| latest v87 UV warm compact raw-output and verdict audit | 1.08 s / 489 MiB |
| latest v88 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v89 UV warm compact raw-output and verdict audit | 0.96 s / 489 MiB |
| latest v90 UV warm compact raw-output and verdict audit | 0.94 s / 489 MiB |
| latest v91 UV warm compact raw-output and verdict audit | 0.92 s / 489 MiB |
| latest v92 UV warm compact raw-output and verdict audit | 1.01 s / 489 MiB |
| latest v93 UV warm compact raw-output and verdict audit | 1.11 s / 489 MiB |
| latest v94 UV warm compact raw-output and verdict audit | 1.04 s / 489 MiB |
| latest v95 UV warm compact raw-output and verdict audit | 0.89 s / 489 MiB |
| latest v96 UV warm compact raw-output and verdict audit | 0.89 s / 489 MiB |
| latest v97 UV warm compact raw-output and verdict audit | 0.86 s / 489 MiB |
| latest v98 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| latest v99 UV warm compact raw-output and verdict audit | 1.01 s / 489 MiB |
| latest v100 UV warm compact raw-output and verdict audit | 0.93 s / 489 MiB |
| peak RSS during that refresh | 3.04 GiB |
| latest warm compact raw-output and verdict audit after SymPy v17 | 1.24 s / 456 MiB |
| current v18 focused native/SymPy/Mathics slice (11 scripts, one worker) | 42.39 s / 804 MiB |
| current v19 focused `Thread` slice (16 scripts, one worker) | 17.43 s / 508 MiB |
| current v20 focused positive-level `Map` slice (6 scripts, one worker) | 1.00 s / 404 MiB |
| current v21 focused numeric `Piecewise` slice (6 scripts, one worker) | 0.85 s / 403 MiB |
| current v22 focused numeric `Boole` slice (3 scripts, one worker) | 2.91 s / 404 MiB |
| current v23 focused numeric `Which` slice (5 scripts, one worker) | 0.71 s / 404 MiB |
| current v24 focused bounded `TrigReduce` slice (6 scripts, one worker) | 0.72 s / 404 MiB |
| current v25 focused symbolic 2x2 `Solve` slice (1 script, one worker) | 0.78 s / 404 MiB |
| current v26 focused exponential-product `Integrate` slice (3 scripts, one worker) | 0.78 s / 404 MiB |
| current v27 focused nested-limit `Integrate` slice (1 script, one worker) | 0.77 s / 402 MiB |

Read that honestly: 99% is *native scripts that ran and emitted bindings*, not
correctness. Scoring against an oracle is what makes it coverage.

The latest cache-only binding-level audit reports 3,922 agreements, 476 declared
differences, 7 unsupported outcomes, 75 timeouts, 61 errors, 2 unavailable
oracle rows, 197 oracle disagreements, and 287 oracle-missing bindings across
4,885 bindings. The v43-v100
parity batch adds source-faithful ECNL equation strings, numeric validity
estimates, Maxwell/flux-pumping companions, math10y and Suydam recoveries,
large-step LTE reconstruction, Sympl3 field forms, normal-stability numeric
parity, math3y, Cartesian-primitive, math8y, perpendicular-block, math10y,
math6-1y, cylinder-spectrum, math11y, Mercier, Appendix-B, math14y, and math15y
recoveries, with 75 independent tests. The v59 native work fixes implicit
scientific-literal precedence and adds vector-matrix `Dot` evaluation; v60 adds
bounded numeric `FindRoot`. The v70 cycle adds exact identity-matrix fractional
powers and source-faithful numeric `Abs`/Mathics root handling, with selective
SymPy cache invalidation. The v71 cycle adds bounded `Do` assignment loops and
six source-faithful `math10y` companion recoveries; v74 adds named-derivative
lowering, two joined-plot recoveries, and bounded source-to-Fortran `Do`
lowering. The v75 refresh records the named-derivative companion alignment; it
does not increase the agreement tally. The v76 cycle adds source-faithful
`math10y`/`math11y` recoveries and bounded stateless `For` lowering in the native
source-to-Fortran translator. The v77 cycle adds a source-faithful `math15y`
recovery and bounded stateless `While` lowering. The v78 cycle adds three
source-faithful `math10y` ellipsoid bindings and a cylinder-spectrum recovery.
The v79 cycle adds source-faithful `jDotB` and memo34 radial-product bindings.
The v80 cycle adds final `math10y` `Which` coverage and large-step recurrence
bindings; these pass focused tests but do not change the scored tally. The v81
cycle adds dynamic bounded `If` codegen, six `vector2d` fields, and a flux
access-condition correction. The v82 cycle adds a memo37 factored-derivative
binding and a source-faithful cylinder-spectrum derivative tree. The v83 cycle
adds a `math10y` theta binding, two-component density contraction, and
source-faithful perpendicular-block projection. The v84 cycle adds a symbolic
`peng` Dot preservation, physical-weighted energy, and sequential SWR recovery;
the unresolved-class counts improve, but the agreement tally does not. The v85
cycle adds bounded integer `While` increment/decrement lowering. The v86 cycle
adds `math10y` fourth-root coverage, a `sympl3_` denominator correction, and a
memo34 Bessel-kernel branch. The v87 cycle adds a Gaussian-integral `math10y`
binding, the heat-kernel `k3`, and a `gc_drift` Christoffel tensor. The v88
cycle adds the `math10y` derivative binding and `gc_drift` `gradBmod`. The v89
cycle adds second-derivative `math10y` coverage, a source-faithful pressure
slope, and scalar reassignment codegen. The v90 cycle adds strict bounded
`For` forms and another source-to-Fortran coverage gate. The v91 cycle adds
source-faithful `math10y` weighted second derivatives, heat-kernel tails, and a
two-component flux-temperature-slope binding. The v92 cycle adds transformed
`math11y` ODEs, `gc_drift` gradients, a large-step fast-free binding, four gvec
validation bindings, and a bounded Fortran adapter test. The v93 cycle adds
heat, gvec export, `math10y`, and archive `math6-1y` recoveries. The v94 cycle
adds gvec Fourier, archive `math6-1y`/`math15y`, `math11y`, and two-component
energy-identity recoveries. The v95 cycle adds ECNL, zero-family, GH NTV,
flux-handedness, and project-NTV recoveries. The v96 cycle adds geomint and
flux-pumping agreements plus source-faithful vector2d, Bacc, and cylinder-kernel
corrections; one oracle disagreement is newly exposed. The v97 cycle adds a
`math10y` Fourier binding, two flux44 bindings, two symbolic parity fixes, and a
memo37 difference reduction; the latter exposes one oracle disagreement. The v98
cycle adds gvec `II_tt`, two-component `qField`, and another `math10y` binding;
the archive plot recovery remains a declared native-serialization difference. The
v99 cycle adds gvec `II_zz`, two-component `fluxTslope`, another `math11y` binding,
and the bounded `Do` source-to-Fortran adapter. The full adapter inventory emits 1
of 384 sources and refuses 383. The v100 cycle adds ECNL, `math13y`, `math15y`,
and appA flux-coordinate recoveries. A trial
Levi-Civita native
lowering was
reverted after its measured corpus regression. The benchmark harness now
invalidates native cache rows at version 2.
The v37
phase-transform and flux-coordinate companion translations, followed by the
v38 NAE/DESC and Appendix-B translations, recovered 42 agreements and 52
previously oracle-missing bindings in the warm audit while resolving one SymPy
timeout. The v39 math6-2y, math11y, two-component-energy, and
dynamo-diagnostics companions recover a further 27 agreements and 25
oracle-missing bindings; the dynamo bridge reduces its focused runtime from
about 29 seconds to 1.1 seconds. The
v40 memo-feedback companion recovers the source magnetic-norm binding with an
independent test, adding one agreement and removing one oracle-missing
binding. The v41 cylinder-spectrum companion recovers the source-faithful
force-balance and pressure-slope bindings with an independent test, adding one
agreement and removing one oracle-missing binding. The v42
Bacc/Rosa/Posch companion recovers the source magnetic-field integral with an
independent numerical quadrature test and removes one oracle disagreement. The
v43 batch is committed and cached, but the target remains open until the
declared native subset and the available oracle overlap agree.

The current native collection slice includes bounded `Array`, `ConstantArray`,
and `Outer` expansion, bounded exact `Range`, `DiagonalMatrix`, rectangular
`Diagonal`, bounded `LegendreP`, bounded exact `CharacteristicPolynomial`
for explicit square matrices up to dimension 16, and bounded non-negative
`MatrixPower`, bounded explicit-list `Total`, bounded full-rank numeric
`PseudoInverse`, diagonal/zero numeric `SingularValueList`, numeric `Max`/`Min`,
exact
`RowReduce`, `NullSpace`, `MatrixRank`, bounded exact `LinearSolve` and
`Minors`, `Exponent` including exact fractional monomials, computed scalar
`Solve` rule replacement, bounded list `Thread`, bounded positive-level `Map`, bounded numeric
`Piecewise` branch selection, numeric `Boole` and `Which` conditions, bounded
`TrigReduce`, bounded symbolic 2x2 `Solve`, bounded polynomial
gcd/quotient/remainder and rational
numerator/denominator extraction, verified exponential-product `Integrate`,
structural `Length`, recursive `Flatten`, dynamic exact dimensions,
bounded block-matrix `ArrayFlatten`, bounded Cartesian `Curl`, and opaque preservation for unsupported
dimensions or computed heads. The independent tests cover literal, rational,
symbolic, canonical empty-list, and bounded-preserved forms. Requested-
precision `N` now has a bounded native path for 17--512 decimal digits, with a
named refusal above that limit; list `Append`/`Join`, bounded list selectors,
rectangular `Diagonal`, bounded `LegendreP`, bounded `CharacteristicPolynomial`,
scalar `Joined -> True/False` overrides for `ListPlot`/`ListLinePlot`, and
multiline dot-product continuation also have
independent tests. Unsupported selector and matrix shapes remain opaque.
Relative to the prior numeric baseline, the parser slice added seven
agreements, removed one binding error, and exposed two newly visible bindings
from the translated assignment stream; the subsequent selector slice preserved
the aggregate tally, and the diagonal slice added one agreement while removing
two declared differences after both translators preserved unsupported symbolic
shapes. Its downstream `Last[Diagonal[s]]` case is correctly oracle-missing.
The Legendre slice added two agreements and removed two declared differences.
The characteristic-polynomial slice passes independent determinant tests and
the two-file corpus probe, but does not change the aggregate tally: one source
reassigns its `cp` binding later to plotting `Point` values, while the mapped
form in the other source remains outside the translated assignment subset.
The MatrixPower slice passes independent multiplication and identity tests;
the measured corpus rows still have no scored increase because their exposed
uses are inside unsupported `MatrixForm`/negative-power paths.
The Coefficient slice added independent SymPy lowering for `Coefficient` and
`CoefficientList`, native bounded multivariate `CoefficientList`, and eight
agreements while removing eight differences and one oracle disagreement. The
following Solve slice normalizes single-variable roots to Wolfram rule lists,
adds one agreement, and removes one oracle disagreement. The bounded
`FoldList[Plus, initial, list]` slice then removes one declared difference;
the refreshed native collection also makes one previously hidden binding an
agreement, for a net gain of two agreements and one fewer difference. Cache
version 19 keeps unaffected older-version SymPy rows reusable, including the
version-16 diagonal/extrema rows and older quoted-string rows, instead of
forcing another broad oracle refresh.
The bounded `ArrayFlatten` slice then evaluates rectangular block matrices and
adds three agreements while removing three declared differences. The
quoted-string translator alignment then maps SymPy string atoms to the native
comparison hash, adds 51 agreements, and removes 45 differences plus six oracle
disagreements. The bounded native `Total` slice then adds three agreements and
removes four differences across explicit scalar and vector-list sums. The
bounded full-rank numeric `PseudoInverse` slice adds two agreements and removes
two differences in the duplicated matrix-course cases. The diagonal/zero
`SingularValueList` and numeric extrema slice then adds seven agreements and
removes seven differences. The SymPy v17 transition adds bounded `Exponent`,
`PolynomialGCD`, `PolynomialQuotient`, `PolynomialRemainder`, `Numerator`, and
`Denominator` lowering; it turns one native binding into an agreement and
exposes one non-polynomial form as oracle-missing, for a net aggregate change
of one agreement, two fewer differences, and one more oracle-missing binding.
The v18 transition then lets serialized SymPy `Rule`/`RuleDelayed` heads feed
`ReplaceAll` and preserves exact fractional-monomial `Exponent` results. It
refreshed four affected SymPy rows in 10.46 seconds at 399 MiB RSS. The
current 11-script focused audit with the rebuilt native runner took 42.39
seconds at 804 MiB RSS and confirmed five computed-Solve/fractional-Exponent
bindings as agreements. The v19 bounded `Thread` transition then refreshed 16
SymPy rows in 16.51 seconds at 399 MiB RSS. Its current 16-script focused
audit took 17.43 seconds at 508 MiB RSS, converted two native `Thread`
bindings from differences to agreements, and reported 249 agreements, 58
differences, 1 unavailable oracle row, 1 timeout, 5 errors, 26 oracle
disagreements, and 58 oracle-missing bindings. These focused slices do not
close the remaining parity gap. The v20 positive-level `Map` transition then
refreshed six SymPy rows in 4.07 seconds at 403 MiB RSS. Its six-script focused
audit took 1.00 second at 404 MiB RSS; the selected rows were dominated by
plotting/file-I/O or Mathics failures, so the slice did not change the scored
native tally. The v21 bounded numeric `Piecewise` transition then ran the same
six-script focused audit in 0.85 second at 403 MiB RSS. It preserved 21
agreements, 6 differences, 3 unavailable oracle rows, 2 oracle disagreements,
and 3 oracle-missing bindings, with no scored native tally change because the
slice was dominated by symbolic/IO limitations outside this subset. The
v22 numeric `Boole` transition refreshed three SymPy rows in 2.91 seconds at
404 MiB RSS. Its focused audit preserved 21 agreements, 7 differences, 1
unavailable oracle row, 1 oracle disagreement, and 1 oracle-missing binding,
with no scored native tally change because all exposed Boole uses remain inside
larger symbolic or untranslated-function gaps. The
v23 numeric `Which` transition then served a five-script warm slice in 0.71
second at 404 MiB RSS. It preserved 60 agreements, 16 differences, 1 timeout,
1 unavailable oracle row, 3 oracle disagreements, and 73 oracle-missing
bindings; its cached timeout/unavailable outcomes were not rerun. The
v24 bounded `TrigReduce` transition then served a six-script warm slice in
0.72 second at 404 MiB RSS. It preserved 101 agreements, 48 differences, 3
unavailable oracle rows, 1 timeout, and 103 oracle-missing bindings; its cached
timeout was not rerun. The v25 bounded symbolic 2x2 `Solve` transition then
served the exposing corpus script in 0.78 second at 404 MiB RSS. It produced
33 agreements, 10 differences, 1 unavailable oracle row, and 1 oracle-missing
binding, with no timeout or runner error. The
v26 verified exponential-product `Integrate` transition then served a
three-script slice in 0.78 second at 404 MiB RSS. It produced 5 agreements, 1
difference, 1 unsupported backend outcome, 1 unavailable oracle row, and 1
oracle disagreement, with no timeout or runner error. The
v27 native definite/multiple-`Integrate` transition then evaluated the measured
nested-limit script in a warm one-worker audit in 0.77 seconds at 402 MiB RSS.
Native and SymPy now produce the complete outer-to-inner result; Mathics
retains a partial unevaluated result, so the three bindings remain explicit
oracle disagreements rather than being scored as native errors. The
subsequent native plotting slice adds independent per-dataset
`Joined -> {True, False}` coverage for `ListPlot` and `ListLinePlot`; the
focused plotting suite passes, while the broader plotting family remains open.
The highest-impact work remains the plotting family, `Solve` beyond
scalar linear cases, polynomial heads, and `DSolve`/`NDSolve`.

### The oracle ceiling (#47)

That number is now established, and it changes the target.

| | scripts | share |
|---|---:|---:|
| Mathics produces results | 255 | 66% |
| fortsym-wl completes | 379 | 99% |
| **Mathics and fortsym-wl both complete** | **252** | **66%** |

Mathics fails or is unavailable on 129 scripts: 60 errors, 69 timeouts, and
no unavailable rows. Those outcomes are cached by source digest, runner
version, and executable fingerprint, so later native audits do not rerun the
oracle.

**100% coverage is unreachable with Mathics as the sole oracle**, and no amount
of fortsym work changes that: 145 natively completed scripts currently have no
successful Mathics result to compare against. The benchmark now also compares
native results against SymPy when Mathics is unavailable and requires both
oracles to agree before scoring a shared binding. Raising the ceiling means
completing the SymPy translation/runtime, reporting the Mathics defects
upstream, or adding a third oracle for the overlap.

Until it moves, every coverage number states its denominator. "99% complete
natively" and "61% share a successful Mathics result" are different
claims.

The historical full refresh includes the SymPy refresh required by the
translator cache-version change and took 4:54. The latest LegendreP refresh of
the SymPy oracle took 6:59.96 with two workers and a 3.86 GiB peak RSS. The
quoted-string translator refresh took 2:00.16 with a 543 MiB peak RSS. The
subsequent native `Total`/`PseudoInverse`/`SingularValueList` audit took 1:14.19
with a 3.04 GiB peak RSS; the v16 SymPy transition refreshed 22 rows in 8.93
seconds; the v17 polynomial transition refreshed eight rows in 11.28 seconds;
the v18 Solve-rule/fractional-`Exponent` transition refreshed four rows in
10.46 seconds at 399 MiB RSS; the v19 bounded `Thread` transition refreshed 16
rows in 16.51 seconds at 399 MiB RSS. The latest warm audit takes 0.56 seconds
at 344 MiB RSS. The v20 positive-level `Map` transition refreshed six rows in
4.07 seconds at 403 MiB RSS.
Once raw results and comparison verdicts are cached, the same full audit takes
0.56 seconds at 344 MiB RSS. Future compatible cache transitions retain
unaffected older-version rows. These are
harness measurements, not a
capability comparison: Mathics evaluates integrals fortsym refuses, and the native path
still has one 60-second timeout.

What running the corpus has already bought, none of which was found by tests:

- A parser segfault on `-Inverse[g] . c`, from negating a term that had already
  failed to parse. Five scripts crashed; now none do.
- `wl_eval` dispatched only on `NK_FUNC`, so `1 + Integrate[x, x]` reported the
  unevaluated `Integrate` as though it were the answer. Unimplemented heads
  nested in arithmetic now refuse.
- The `Series` order convention, off by one against Mathics in both directions.
- Implicit multiplication, the single largest parse-refusal cause.
- Refusing `Transpose[jac]` for a symbolic `jac`, which disagreed with the
  oracle on a *correct* answer — Mathics leaves it unevaluated too.

The current native baseline is 2 named refusals and 1 timeout at script level;
binding-level unsupported constructs remain tracked by the benchmark report.

### What the corpus says to build next

Refusals grouped by cause, measured rather than guessed. This is the list that
orders the work, and it has repeatedly disagreed with intuition:

| Cause | Refusals |
|---|---:|
| parse | 518 → 279 |
| plotting family (Show, Plot3D, ContourPlot, Graphics, ListPlot, …) | ~300 |
| `Solve` beyond the scalar linear case | 78 |
| definite and multiple `Integrate` | ~123 |
| `N` beyond the bounded 17--512 digit path | refresh the refusal count |
| polynomial heads (`Coefficient`, `Together`, `Factor`, …) | ~60 |
| `DSolve` / `NDSolve` | 45 |

Two findings worth keeping:

- **Plotting is the largest single construct family**, mostly from teaching
  material, and fortplot already provides contour, contourf, pcolormesh,
  surface, scatter, streamplot and quiver. Nothing upstream is missing.
- **239 of the 518 parse refusals were one bug**, not missing syntax: a
  statement was split at every depth-zero newline, so a derivation broken after
  a trailing operator was truncated into a prefix that still parsed. Measuring
  causes rather than counting messages is what found it.

### How defects are found here

Every module added since the corpus went live has been written, then attacked
by an independent reviewer whose brief is to produce a wrong answer with real
program output, not to approve. That pass has found, among others: an
antiderivative emitting `log` without an absolute value; a positivity predicate
treating `exp(z)` as positive for imaginary `z`, reintroducing the very
`sqrt(x*y)` sign error its own docstring quoted; a limit deciding the
power-domain rule from an exponent *after* substitution, asserting a finite
limit for a function undefined on both sides; a summation guard defeated by
integer overflow in its own term count.

None of these were caught by the modules' own tests, all of which passed. The
lesson is recorded here because it sets the standard for new work: a passing
test written by the same author as the code is weak evidence, and the review
pass is not optional.

## Milestones

Ordered by corpus impact. Counts are call sites across the 384-script corpus.

### M1 — Polynomial and rational core (#28) · 2189 sites

Content, primitive part, exact division, GCD, square-free decomposition,
resultant, `cancel`, `together`, `apart`, `factor`.

- GCD: **sparse modular** (Zippel 1979) with **dense modular** (Brown 1971) for
  low variable counts, and **GCDHEU** (Char, Geddes & Gonnet 1989) as the fast
  path for small inputs. Monagan & Wittkopf (2000) for the sparse-modular
  refinements.
- Univariate factorisation: **Zassenhaus** with **van Hoeij lattice
  reduction** (van Hoeij 2002) to kill the exponential recombination case.
- Multivariate factorisation: **EEZ / Wang** multivariate Hensel lifting
  (Wang 1978).
- Oracle: divisibility, Bezout, and finite-field evaluation — not another CAS.

### M2 — Assumptions (#29) · 895 sites

Inequality ranges, domain membership, scoped contexts, `refine`, compound
inference.

- Representation: an immutable fact set with a **congruence-closure**-style
  propagation over interned expressions (Nelson & Oppen 1980).
- Oracle: sampling within the declared domain; a rewrite must hold at sampled
  admissible points and be refused outside them.

### M3 — Series (#35) · 321 sites

Laurent series, expansion at infinity and at singular points, composition,
inversion, series of special functions.

- **Lazy/recursive power series** with Newton iteration for reciprocal and
  reversion; **Brent & Kung (1978)** for composition.
- Order convention documented and identical in both frontends. Wolfram's
  `Series[f,{x,0,n}]` includes `x^n`; SymPy's `series(f,x,0,n)` does not.
  fortsym-bench found this on its first corpus entry.

### M4 — Limits (#32) · 137 sites

- **Gruntz's algorithm** (Gruntz 1996) over most-rapidly-varying subexpressions.
  It is the only method in wide use that is correct on nested exponentials, and
  it is what SymPy implements.
- Taylor-derived limits first for the cheap finite cases.

### M5 — Complex domain (#33) · 782 sites

`re`, `im`, `conjugate`, `arg`, guarded `abs`, `complex_expand`, and promotion
of the bounded `qqbar` bridge into arena nodes.

- Exact algebraic numbers through **FLINT `qqbar`** (Johansson 2020), already
  pinned as a dependency.
- Oracle: high-precision evaluation and `z * conj(z) == abs(z)^2`.

### M6 — Special functions (#34) · 1040 sites

Bessel `J`,`Y`,`I`,`K`; gamma family; `erf`/`erfc`; elliptic `K`,`E`,`F`;
Legendre.

- Numerics through **Arb** ball arithmetic (Johansson 2017), which gives
  rigorous enclosures rather than best-effort floats.
- Symbolic relations from **DLMF** (Olver et al.), cited per rule.
- Unblocks KiLCA, whose entire conductivity tensor is Bessel and gamma.

### M7 — Integration (#31) · 350 sites

- Current native work covers bounded exact rules and guarded definite
  integration. It must retain the explicit refusal boundary while it grows.
- Rational part: **Hermite reduction** then **Lazard–Rioboo–Trager** for the
  logarithmic part. Bronstein, *Symbolic Integration I* (2005), is the reference
  implementation guide.
- Then the **Risch** algorithm for elementary extensions, in decision-procedure
  form with domain-aware tests.
- Oracle: differentiate the answer and decide zero.

### M8 — Solving (#36) · 76 sites

Univariate roots by radicals to degree four, `RootOf` representation above,
elimination for systems, Gröbner only where a traced case needs it
(**F4**, Faugère 1999; bounded Buchberger otherwise).

### M9 — Matrices (#30) · 589 sites

- Determinant and linear solve by **fraction-free Bareiss** (Bareiss 1968).
- Rational systems by **Dixon p-adic lifting** (Dixon 1982), which is the fast
  path and what makes large exact systems tractable.
- Eigenvalues via characteristic polynomial through M8.

### M10 — Numerics and the fortnum boundary (#37) · 686 sites

Requested-precision real and complex evaluation, `NIntegrate`, `FindRoot`,
`Interpolation`, `Fit`.

**Anything numerical that is not symbolic-expression evaluation belongs in
fortnum, not here** — quadrature, root finding, interpolation and fitting are
fortnum's subject. fortsym contributes only the evaluator that turns an
expression into a callable, plus the bridge. Work lands in fortnum when the
corpus demands a routine fortnum lacks, and nowhere else.

The adapter must reject symbolic or under-specified calls before entering
fortnum. Each numeric result carries its requested precision, estimated error,
domain transformations, and failure status so a plausible floating value
cannot masquerade as an exact proof.

- Ball arithmetic through **Arb**; adaptive quadrature via
  **Gauss–Kronrod** with the Petras/Molin rigorous-error treatment for the cases
  that need certification.

### M11 — Plotting through fortplot (#44) · 2794 sites

`Plot`, `Plot3D`, `ContourPlot`, `ParametricPlot`, `ListPlot`, `StreamPlot`,
`DensityPlot`, `VectorPlot`, `LogPlot`, `Show`, `GraphicsGrid`, `PlotLegends`.

Plotting was a deferred area. The corpus makes it the **single largest
construct family**, mostly from the teaching material, so it is scheduled.

fortsym does not gain a plotting implementation. It gains a **dependency on
fortplot**, which already provides plot, contour, contourf, pcolormesh, surface,
3-D, scatter, streamplot, quiver, errorbar, subplots and legends. fortsym's job
is sampling an expression into arrays and mapping options; anything missing is
fixed **in fortplot**, upstream, not worked around here.

### M12 — Sums, piecewise, trig rewrites, ODEs (#38, #39, #40, #43)

Indexed sums (**Gosper 1978**, **Zeilberger 1990** for the hypergeometric
cases), piecewise with branch emission, public trig/power rewrites, and finally
`DSolve`.

### M13 — Codegen completion (#41, #42)

Binding opaque applied functions and their `Derivative` nodes to
consumer-supplied procedures, and mapping special-function heads to a Fortran
runtime. #41 is what unblocks SIMPLE's canonical-field Hessians; #42 is what
lets KiLCA's orphaned generated kernels be regenerated. A standalone full-
corpus `.wl`-to-`.f90` translator does not exist yet: the native runner
interprets `.wl` at runtime, and current codegen consumes an existing `expr_t`
graph. This remains a separate completion gate for the Fortran parity target.
The current bounded implementation parses sequential streams of up to 128
scalar assignments, expands earlier assignments, infers scalar inputs, and
reuses the kernel emitter. Its focused test compiles the emitted subroutine and
checks it against an independently derived numeric Fortran oracle. Control
flow, forward or reassigned names, arrays, non-Fortran names, and unsupported
expression forms remain intentionally refused.

## Roadmap maintenance

The top-level roadmap is the release-level plan. `doc/roadmap.md` holds the
operational checklist and `doc/provenance.md` holds algorithm references. A
milestone changes in one commit with its implementation, independent tests,
benchmark evidence, and documentation.

After each committed change:

```bash
fo
fortsym-bench run corpus --backend sympy mathics fortsym-wl --jobs 4 \
  --timeout 60 --report /tmp/fortsym-corpus.json
```

The report is the source of truth for measured counts. The roadmap records the
commit, date, cache state, and remaining gap. It does not claim 100% parity
until every overlapping binding agrees or has a documented oracle limitation.

## Simplification strategy

Candidate ranking stays the mechanism: generate candidates, verify equivalence,
rank by operation count after CSE. Beyond that, **bounded equality saturation**
over an e-graph (Willsey et al., *egg*, POPL 2021) is evaluated only once M2's
domain guards exist — an e-graph that rewrites without guards will happily prove
something false on a restricted domain.

## What stays out

Units, geometry, combinatorics and broad theorem proving remain outside the
corpus and therefore outside the plan. New corpus evidence can promote any of
them.
