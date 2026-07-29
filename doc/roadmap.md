# CAS roadmap

## Acceptance rules

Each milestone has a callable public operation, an independent behavioral
oracle, documentation, and a reproducible benchmark row. A capability bit is
added only with the operation it describes. A speed statement cites a pinned
result file for matching semantics. Unsupported input returns a diagnostic or
`UNKNOWN`.

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

- Add checked integer and rational arithmetic.
- Implement native bottom-up simplification and expansion with DAG memoization.
- Collect numeric factors, like terms, and integer powers.
- Resize hash tables and replace node-index ordering with a stable semantic
  order where generated output depends on it.
- Benchmark construction, interning, simplification, expansion, and memory
  against direct SymEngine calls.

### 3. Domains and assumptions

- Add immutable assumption contexts for real, positive, nonnegative, and
  nonzero expressions.
- Guard square-root, logarithm, power, and cancellation rules by the context.
- Return conditions with conditional transformations.
- Close the theta-pinch and denominator cases tracked in issue 15.

### 4. Polynomial and rational algebra

- Define dense and sparse polynomial views over integer and rational domains.
- Implement content, primitive part, exact division, subresultant polynomial
  remainder sequences, GCD, square-free decomposition, resultant, cancellation,
  together, and apart.
- Add modular reconstruction or finite-field evaluation as an independent
  identity oracle.
- Extend to multivariate rational expressions needed by MHD script 42.

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

General differential equation solving, plotting, fixture integration, units,
geometry, combinatorics, and broad theorem proving are outside the current
consumer fragment. New consumer evidence can promote one of these areas into a
bounded milestone.

