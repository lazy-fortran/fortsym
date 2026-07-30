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

### 2. Native arithmetic and canonical forms

- Replace signed 64-bit exact storage with arbitrary-precision integer and
  canonical rational storage using the pinned FLINT shared interface.
- Add exact complex algebraic values through a bounded `qqbar` representation;
  use Calcium or Arb/Acb only where their three-valued or rigorous enclosure
  semantics are explicit.
- Native bottom-up simplification uses DAG memoization; nonnegative powers of
  sums use checked, bounded multinomial composition enumeration.
- Collect numeric factors, like terms, and integer powers.
- Hash tables now resize before their average chain exceeds one node.
- The pinned same-machine diagnostic reduced median cold expansion from
  3.260 ms to 0.412 ms (7.91x); native remains 2.66x slower than SymEngine
  0.14.0 on the isolated `(x + y + c)^7` workload.
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
