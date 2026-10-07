# Executable derivation export

## Contract

- Input: a recorded supported derivation from native Fortran or the Python
  SymPy-compatible facade.
- Output: readable Python source executable with pinned ordinary SymPy alone;
  explicit backend selection permits replay through `fortsym.sympy`.
- One artifact preserves definitions and ordered calculations. Final-expression
  printing is a separate operation.
- Native generation needs no Python, SymPy installation or subprocess engine.
- Supported scope grows by reviewed slices; unsupported syntax/operations
  return step/source diagnostics and never become guessed results.

## Retained information

- Versioned record, exact source/content hashes, generator/backend versions
  and declared required capabilities.
- Scoped symbol identity, explicit assumptions, exact constants and parameter
  definitions. Arbitrary integers and rationals survive without float rounding.
- Ordered constructions, substitutions and calculus/algebra calls, including
  options, bounds, conditions, branches and named intermediate results.
- Explicit checks and independent witnesses, their inputs and evidence status:
  identity, numeric probe, counterexample, UNKNOWN, refused, blocked or not run.
- Domain exclusions remain visible after cancellation and transformation.
- Observed engine results are evidence, not silently promoted proof axioms.

## Recording boundaries

- Native operations and supported drop-in constructors/methods must feed one
  mathematical record owner through the ordinary public APIs.
- Python control flow, arbitrary assertions, custom helpers and external
  side effects are not reconstructed from the expression graph.
- Check helpers explicitly register witnesses; the record states its coverage.
- A source-authored derivation replay supplies the full integration gate.
- Asset manifests and an isolated runner package relative inputs/outputs when
  needed; scalar export does not claim to recreate arbitrary filesystem code.

## Neutral interchange

- FortGen defines exact mathematical Expr IR independently of expression arenas
  and SymPy objects. FortSym owns its adapter and derivation semantics.
- FortGen's existing proposed binary64 scalar Kernel IR describes a numerical
  computation. It remains a separate layer with explicit rounding/lowering.
- Canonical structural serialization supports deterministic diffs; it does not
  imply semantic equivalence or erase assumptions.
- Keep mathematical branch semantics, floating-point reassociation, FMA,
  signed zero, complex operations and target precision explicit at their
  respective boundaries.
- Lean may later check precise claims against the public contract; CAS checks
  and sampled probes do not become formal proofs by serialization.

## Behavioral gates

- Use the same original source and the same exported artifact across replays.
- Retain original independent algebraic/residual/numerical/units checks.
- Compare semantic values and structured results, assumptions/conditions,
  exceptional behavior and numerical output, not string similarity.
- Fresh processes distinguish native FortSym execution from optional external
  SymPy delegation. Report pins and installed optional backend configuration.
- Negative cases corrupt an operation, omit a condition or request unsupported
  behavior; the gate must detect each failure.
- Include ordinary reference-tool defects and resource exhaustion explicitly.
- Run the native CMake/CTest authority plus focused resident Fo feedback when
  implementing; optional Python gates supplement the native contract.

## Open design decisions

- Minimal first transcript API and explicit check-witness registration.
- Neutral serialization and symbol/assumption scope representation.
- Standalone backend selection and supported submodule imports, especially
  units. Source rewriting must not alter physical formulas.
- Conditional result representation when native and reference tools have
  different result containers or inference strength.
- Exact mathematical equivalence versus explicitly permitted numerical
  lowering; avoid global framework migration before representative gates.

The implementation order and atomic issues are in [PLAN.md](../PLAN.md).
