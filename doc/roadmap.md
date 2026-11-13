# Development directions

The [feature matrix](feature-matrix.md) describes the implemented fragments.
Open work is tracked in the [issue tracker](https://github.com/lazy-fortran/fortsym/issues).
The compatibility reference is the versioned profile in
[`upstream-baselines.toml`](upstream-baselines.toml), with the release checks
listed in [`release-profile.toml`](release-profile.toml).

Broader compatibility work concerns:

- Exact domains, precision rules, ownership and consistent expression semantics
  across Fortran, C, Python and the SymPy facade.
- Scoped assumptions, branch-aware simplification and explicit conditions.
- General polynomial and rational algebra, algebraic roots and bounded
  elimination procedures.
- Differentiation, series, limits, integration and transforms beyond the
  currently documented fragments.
- Equation solving, solution conditions and exact matrix operations beyond the
  documented native algorithms.
- Special functions, discrete mathematics and selected set and logic APIs.
- Array, tensor, differential-form and geometry interfaces with explicit chart,
  variance, orientation and domain metadata.
- Derivation recording and standalone export beyond the
  [scalar API](derivation-api.md), with replay of original source and retained
  assumptions, domain conditions and independent witnesses.
- Parsing, printing and numerical kernel generation with explicit precision,
  rounding and singularity handling.

Each extension needs a public operation, documented limits and an independent
behavioral oracle. Reference-system comparison uses the same cases and matching
semantics; another system's answer alone is not a proof. Unsupported domains or
resource limits return a diagnostic or `UNKNOWN`, without guessed results.
Cold, warm and core-operation performance are measured separately from
correctness using the [benchmark contracts](benchmarks.md).

The expression representation, exact arithmetic and public vocabulary stay
shared across frontends. Separate-process engines remain optional; SymEngine
is a build dependency. Other engines do not define native semantics.
Symbolic geometry belongs here; meshes, finite-element basis
assembly and numerical sparse solvers belong in their numerical libraries.
The [geometry derivation ledger](geometry-derivations.md) records conventions
and refusal boundaries for those interfaces.
