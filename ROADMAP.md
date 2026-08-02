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

Latest committed corpus-wide baseline, measured 2026-08-02, `fortsym-bench` at
384 scripts (`--jobs 2`, 60-second script budget, after the diagonal
singular-value slice and before the focused v18 transition):

This table is a committed native baseline. It is updated only after the root
backend and benchmark harness revisions used to produce it have been committed.
Interrupted experiments and uncommitted generated artifacts do not change the
reported state.

| | |
|---|---:|
| scripts completing natively | **380 / 384 (99%)** |
| of those with non-empty result sets | 344 |
| valid empty native result sets | 36 |
| scripts refusing with a named construct | 2 |
| scripts exceeding the time budget | 1 |
| scripts ending in a runner error | 1 |
| crashes | **0** |
| full audit after the bounded native singular-value refresh | 1:14.19 |
| peak RSS during that refresh | 3.04 GiB |
| latest warm compact raw-output and verdict audit after SymPy v17 | 1.24 s / 456 MiB |
| current v18 focused native/SymPy/Mathics slice (11 scripts, one worker) | 42.39 s / 804 MiB |
| current v19 focused `Thread` slice (16 scripts, one worker) | 17.43 s / 508 MiB |
| current v20 focused positive-level `Map` slice (6 scripts, one worker) | 1.00 s / 404 MiB |
| current v21 focused numeric `Piecewise` slice (6 scripts, one worker) | 0.85 s / 403 MiB |

Read that honestly: 99% is *native scripts that ran and emitted bindings*, not
correctness. Scoring against an oracle is what makes it coverage.

The final binding-level audit reports 3,219 agreements, 716 declared
differences, 20 unsupported outcomes, 38 timeouts, 117 errors, 192 oracle
disagreements, and 798 oracle-missing bindings. The target remains open until
the declared native subset and the available oracle overlap agree.

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
`Piecewise` branch selection, bounded polynomial
gcd/quotient/remainder and rational
numerator/denominator extraction, structural `Length`, recursive `Flatten`, dynamic exact dimensions,
bounded block-matrix `ArrayFlatten`, and opaque preservation for unsupported
dimensions or computed heads. The independent tests cover literal, rational,
symbolic, canonical empty-list, and bounded-preserved forms. Requested-
precision `N` now has a bounded native path for 17--512 decimal digits, with a
named refusal above that limit; list `Append`/`Join`, bounded list selectors,
rectangular `Diagonal`, bounded `LegendreP`, bounded `CharacteristicPolynomial`, and multiline dot-product continuation also have
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
highest-impact work remains the plotting family, `Solve` beyond
scalar linear cases, definite and multiple `Integrate`, polynomial heads, and
`DSolve`/`NDSolve`.

### The oracle ceiling (#47)

That number is now established, and it changes the target.

| | scripts | share |
|---|---:|---:|
| Mathics produces results | 235 | 61% |
| fortsym-wl completes | 380 | 99% |
| **Mathics and fortsym-wl both complete** | **233** | **61%** |

Mathics fails or is unavailable on 149 scripts: 107 errors, 30 timeouts, and
12 unavailable rows. Those outcomes are cached by source digest, runner
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
rows in 16.51 seconds at 399 MiB RSS. The latest warm audit takes 1.24 seconds
at 456 MiB RSS. The v20 positive-level `Map` transition refreshed six rows in
4.07 seconds at 403 MiB RSS.
Once raw results and comparison verdicts are cached, the same full audit takes
1.24 seconds at 456 MiB RSS. Future compatible cache transitions retain
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
lets KiLCA's orphaned generated kernels be regenerated.

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
