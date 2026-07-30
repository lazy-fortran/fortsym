# CAS roadmap

## Acceptance rules

Each milestone has a callable public operation, an independent behavioral
oracle, documentation, and a reproducible benchmark row. A capability bit is
added only with the operation it describes. A speed statement cites a pinned
result file for matching semantics. Unsupported input returns a diagnostic or
`UNKNOWN`.

The exact upstream release and license baseline is
`upstream-baselines.toml`. “SymEngine coverage” means the applicable
capabilities observed in pinned SymEngine 0.14.0, not an unversioned project
name. SymPy and Yacas requirements are limited to operations evidenced in
`consumer-requirements.toml`; their broad unrelated subsystems are not implied.

## Milestones

### 1. Consumer compatibility

- Preserve FortNum's public API and byte-stable generated kernels.
- Run FortNum's behavioral `fo` suite after emitter or core representation
  changes.
- Verify the path dependency revision against `fortsym.lock` during local
  regeneration.
- Register MHD1D's existing screw-pinch CAS test and replace its manual
  substitution workaround.
- The latest disposable FortNum compatibility probe built 82/82 targets and
  passed 95/95 behavioral tests with the current fortsym code. Canonical
  ordering changed 33 of 39 generated files (131 reviewed line pairs); all 39
  regenerated outputs were installed for that gate. The committed FortNum lock
  was not changed, so this is compatibility evidence rather than a provenance
  update.
- MHD1D builds and passes 6/6 behavioral tests. Its screw-pinch CAS test remains
  parked: `fpm.toml` still cites fpm dependency and native-link propagation,
  while the newer MHD1D roadmap says the cited issue is closed and instructs
  registration only after the actively edited fortsym worktree can be pinned
  to an author-chosen state.

### 2. Native arithmetic and canonical forms

- Arbitrary-precision integers and canonical rationals are now first-class,
  hash-consed arena nodes using the pinned FLINT shared interface, with compact
  signed-64-bit nodes retained as a transparent fast representation.
- Finite normal Fortran projection is pinned to MPFR 4.2.2 nearest-even
  binary64; subnormal and overflow-range exact values are refused through the
  checked printing/codegen status channel.
- The ownership-safe FLINT `fmpq` bridge now supplies canonical normalization,
  add/subtract/multiply/divide, and resource-bounded signed powers. Native
  addition, numeric-factor multiplication, integer powers, and like-term
  coefficients use a checked compact fast path and promote coherently onto the
  bridge; a result beyond the arena scalar budget leaves its operation
  structural.
- The ownership-safe bounded `qqbar` bridge now provides a lossless
  minimal-polynomial/root-index format, Gaussian rationals, exact arithmetic,
  conjugation, principal square roots, and exact component signs. Promote it
  into arena nodes, parsing/printing, native expressions, and conversion next;
  use Calcium or Arb/Acb only where their three-valued or rigorous enclosure
  semantics are explicit.
- Its pinned 45-row diagnostic passed every exact oracle. Warm end-to-end
  add/multiply/divide were 1.34x/1.47x/1.16x the separately scoped direct-FLINT
  no-text floor; a near-64-KiB height refusal completed in 1.50 ms.
- Native bottom-up simplification uses DAG memoization; nonnegative powers of
  sums use checked, bounded multinomial composition enumeration.
- Collect numeric factors, like terms, and integer powers.
- Hash tables now resize before their average chain exceeds one node.
- The pinned same-machine diagnostic reduced median cold expansion from
  3.260 ms to 0.412 ms (7.91x); native remains 2.66x slower than SymEngine
  0.14.0 on the isolated `(x + y + c)^7` workload.
- The subsequent pinned exact-arithmetic diagnostic measured 0.600 ms before
  and 0.633 ms after promotion (+5.51%) under an uncontrolled `powersave`
  governor. The paired native/SymEngine ratio decreased from 2.242x to 2.230x,
  while the unchanged SymEngine row moved 6.09%; neither change isolates
  implementation cost.
- Stable structural ordering excludes arena and name-table indices; independent
  cross-arena tests require identical printing and byte-identical Fortran.
- Benchmark construction, interning, simplification, expansion, and memory
  against direct SymEngine calls.

### 3. Domains and assumptions

- Add immutable assumption contexts for real, positive, nonnegative, and
  nonzero expressions.
- Guard square-root, logarithm, power, and cancellation rules by the context.
- Return conditions with conditional transformations.
- Close the theta-pinch and denominator cases tracked in issue 15.

### 4. Polynomial and rational algebra

- Define dense and sparse polynomial views over arbitrary-precision integer and
  rational domains.
- Implement content, primitive part, exact division, subresultant polynomial
  remainder sequences, GCD, square-free decomposition, resultant, cancellation,
  factorization, together, and apart.
- Add modular reconstruction or finite-field evaluation as an independent
  identity oracle.
- Extend to multivariate rational expressions needed by MHD script 42.
- Add Gröbner bases only when a traced consumer case requires ideal operations:
  start with a resource-bounded Buchberger method over rational ideals,
  preserve the requested `lex`, `grlex`, or `grevlex` monomial order, and
  verify every result with the S-polynomial criterion and generator reduction.

### 5. Series and solving

- Expose Taylor and Laurent series plus coefficient extraction.
- Implement exact scalar linear solving, then univariate polynomial solving.
- Verify solutions by substitution and polynomial reconstruction.
- Close the axis-series case tracked in issue 16.

### 6. Special functions and projection

- Extend Bessel rules from the implemented `J` derivative recurrence according
  to consumer cases.
- Add Taylor bookkeeping and exact trigonometric or Fourier coefficient
  projection for MHD scripts 42 and 52.
- Keep functions outside a decision procedure opaque and return `UNKNOWN`.

### 7. Integration and limits

- Add generic backend entry points for integration and limits.
- Implement rational integration before transcendental methods.
- Check indefinite integrals by exact differentiation.
- Add Taylor-derived limits, followed by an explicitly scoped asymptotic scale
  method.

### 8. Optimization

- Memoize native calculus over shared DAGs.
- Evaluate bounded equality saturation only after domain guards exist.
- Rank verified candidates by generated operation count, live ranges, and
  measured kernel runtime.
- Publish results per workload family with coverage and uncertainty.

## Deferred areas

Plotting, fixture integration, units, geometry, combinatorics, and broad
theorem proving are outside the current consumer fragment. Differential
equation solving is evidenced but remains behind exact scalar solve,
integration, series, and assumptions because those are prerequisites. New
consumer evidence can promote another area into a bounded milestone.
