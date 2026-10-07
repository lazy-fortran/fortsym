# FortSym implementation plan

This file owns current work order. [ROADMAP.md](ROADMAP.md) owns the broader
SymPy-compatible surface and completion rules. Export design is in
[doc/derivation-export.md](doc/derivation-export.md).

## Goal

- Author or execute a supported derivation with FortSym, retaining its actual
  definitions, ordered operations, assumptions, conditions and checks.
- Export a readable standalone script for ordinary SymPy users.
- Replay the same exported artifact with ordinary SymPy and `fortsym.sympy`.
- Keep native authoring, recording, export and generated-code use free of a
  Python requirement. Python remains optional for either Python frontend.
- Establish correspondence to consumer code with independent behavioral
  evidence. Agreement between two spellings alone is insufficient.

## Current boundary

- `DIA_SYMPY` prints expressions; it does not export complete derivations.
- The bounded Python drop-in owns no separate algebra and imports no SymPy.
- The optional external SymPy adapter simplifies/tests zeros; its symbol
  declarations currently omit assumption facts. See [#81](https://github.com/lazy-fortran/fortsym/issues/81).
- Scalar Kernel IR lowers exact atoms to floating literals. It cannot serve
  as an exact mathematical interchange or derivation history.
- No fresh whole-course replay or complete exporter validation is claimed.

## Work order

| Stage | Deliverable | Dependency / review gate |
|---|---|---|
| H0 | Scalar backend integrated; obsolete snapshots archived | Independent native gates passed; finished refs removed; active GS worktree preserved |
| E0 | Exact neutral Expr IR in FortGen [#8](https://github.com/lazy-fortran/fortgen/issues/8) | Review literal/domain/branch contract before implementing adapters |
| D0 | Native/API derivation recording [#78](https://github.com/lazy-fortran/fortsym/issues/78) | Can design alongside E0; retain operations when executed |
| A0 | External assumption boundary [#81](https://github.com/lazy-fortran/fortsym/issues/81) | Independent reproducer; share reviewed assumption serialization |
| E1 | Independent exact adapters [FortGen #9](https://github.com/lazy-fortran/fortgen/issues/9) | E0; Python-free native path and ordinary SymPy entry independent of FortSym |
| D1 | Runnable standalone export [#79](https://github.com/lazy-fortran/fortsym/issues/79) | D0 + E0; fresh isolated execution of emitted scripts |
| D2 | Full source/export replay [#80](https://github.com/lazy-fortran/fortsym/issues/80) | D1; original independent checks plus negative fixtures |
| D3 | Expand supported corpus slices | Measured D2 gaps become atomic issues, never silent substitutions |

## Replay sequence

1. Run a source-authored SymPy derivation with pinned ordinary SymPy.
2. Execute the same derivation through `fortsym.sympy`, recording its native
   operations and explicit check witnesses.
3. Export the standalone script **from that FortSym execution**.
4. Run that identical artifact with ordinary SymPy and with the FortSym drop-in.
5. Compare intermediate/final meaning, conditions, independent source checks,
   exceptions and numerical outputs. Record backend configuration per run.

## Representative corpus slices

- Exact scalar definitions, assumptions, substitutions and derivatives first.
- Coordinate orientation and covariant/contravariant identities next.
- Matrices, moment reductions, explicit limits/integrals and complex branches.
- Units, SI/CGS conversion, structured results and larger kinetic/MHD cases.
- Real plasma/fusion course repositories own their source/asset hash manifests,
  attribution and original checks. Shared 2026 copies are generated exports.
- CAS fixtures use licensed public material or independently authored minimal
  examples; external source/data remain read-only and outputs isolated.
- Current course APIs exceed the bounded drop-in: units, unevaluated objects,
  multivariate polynomial/matrix forms and branch-sensitive operations need
  individual supported/refused/blocked/not-run classifications.
- An ordinary-SymPy dependency defect is a reproducible finding. Keep its
  patch/provenance in the owning dependency; retain an independent oracle.

## Ownership

- FortSym: algebra, assumptions, mathematical derivation records, evidence,
  frontend recording and standalone SymPy export.
- FortGen: public neutral exact Expr IR, computational Kernel IR, lowering
  policies and target emission. Direct SymPy input does not depend on FortSym.
- Consumers: physical theory, source-pinned implementation correspondence,
  numerical algorithms and independent physical checks.
- Third-party solver derivations use a separately owned correspondence
  location; they do not require FortSym infrastructure in upstream solvers.
- Execution schedules, launch geometry and application data are separate.

## Verified status

- Verified source base: `bb3b1e090f6855707243d564c1c500b0bd5c15b0`.
- Shared scalar backend and one-way adapter are integrated from
  [PR #77](https://github.com/lazy-fortran/fortsym/pull/77) and
  [FortGen #7](https://github.com/lazy-fortran/fortgen/pull/7).
- Native CMake/CTest: 113 passed; one optional SymPy differential test skipped
  where SymPy was unavailable. With ordinary SymPy 1.14.0: 77 checks passed.
- Compiled typed-intrinsic oracle: 32 finite real128 samples. The old
  `fix/typed-real-generic-literals` implementation is superseded.
- Fo's frozen consumer-audit fixture passed with explicit fixture inputs;
  its incomplete inventory is not evidence of complete dependency closure.
- Three dirty October 5 snapshots are archived with exact bases, binary
  patches and copied untracked sources. Their four untracked files match main.
- Finished feature branches and six obsolete worktrees were removed after
  preserving exact commit refs and archived source/evidence. Only main and
  the active pinned Grad–Shafranov worktree remain; no open PRs.
- Remaining open issues: [#78](https://github.com/lazy-fortran/fortsym/issues/78),
  [#79](https://github.com/lazy-fortran/fortsym/issues/79),
  [#80](https://github.com/lazy-fortran/fortsym/issues/80),
  [#81](https://github.com/lazy-fortran/fortsym/issues/81). No exporter completion
  or whole-course replay is claimed.
