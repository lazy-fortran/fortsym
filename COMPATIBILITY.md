# Compatibility policy

fortsym pursues two independent compatibility frontends:

1. **SymPy** — a drop-in replacement for the SymPy subset its consumers use, so
   that a working SymPy program becomes a fortsym program by changing one
   import:

   ```python
   import fortsym.sympy as sp     # was: import sympy as sp
   ```

2. **Wolfram language** — a documented subset that reads the derivation
   scripts fortsym's consumers already own, checked against **Mathics** as an
   open-source behavioural oracle. No Wolfram product is involved at any point;
   see `LEGAL.md` §5.1.

This document states what those claims mean, what evidence may be used to build
them, and what may not. `LEGAL.md` governs licences and linking; this file
governs provenance for compatibility work.

## 1. Compatible behaviour is not copied implementation

Reimplementing a documented API is lawful. Copying an implementation is a
licence question. The two are kept apart here by rule, not by judgement in the
moment.

- **Behaviour** — the fact that `expand((x+1)**2)` returns `x**2 + 2*x + 1`, the
  spelling of a function, the order of its arguments, the type it returns — is
  an unprotectable idea. Reimplement it freely.
- **Implementation** — the algorithm as written, its source text, its comments,
  its test files — carries the licence of the project it came from.

## 2. Permitted evidence

- Published mathematics, cited by reference in the source comment.
- Public API documentation of the system being matched.
- Programs owned by fortsym's own users, including translated consumer scripts.
- Tests written independently from documented behaviour.
- Permissively licensed source, **with attribution and file-level provenance**.

SymPy is BSD-3-Clause. Adapting SymPy source into fortsym is permitted provided
the notice is retained and `doc/provenance.md` records the original file and
revision. This is the only CAS implementation fortsym may adapt from.

**Open-source oracles are permitted without restriction.** Running SymPy,
Mathics, Maxima, Giac, PARI or Singular as a separate process and comparing
their answers to fortsym's is ordinary use of published software, and it is how
fortsym establishes correctness. Observing an oracle's *answers* is not
copying its *implementation*; only the latter carries the licence obligation.

## 3. Prohibited evidence

- Decompiled binaries or leaked source.
- Undocumented proprietary protocols.
- Copying GPL implementation or GPL tests into fortsym's MIT link closure —
  Maxima, GiNaC, Giac, PARI, Singular, Sage, Mathics. Running them is fine;
  reading their source into fortsym is not.
- Any use of Wolfram software or services to develop, tune, validate or
  benchmark fortsym. See `LEGAL.md` §5. Mathics replaces that need entirely.

## 4. Wording

fortsym is **not affiliated with, endorsed by, or sponsored by** the SymPy
project or NumFOCUS. "SymPy" is used nominatively, to name the API being
matched.

Acceptable: "compatible with the documented SymPy API for the supported
subset". Not acceptable: naming the product after SymPy, or any phrasing that
implies endorsement.

## 5. Scope of the compatibility claim

The claim is always **subset-scoped and tested**, never open-ended. A published
compatibility table lists every supported class, function and keyword argument,
and every known semantic difference. Anything outside it raises a typed error
naming the unsupported construct.

Silent divergence is the one failure mode this policy exists to prevent: a
compatibility layer that quietly returns a different answer is worse than one
that refuses, because the caller acts on it.

The exact current SymPy surface is generated from the pinned release profile;
it is not inferred from whatever SymPy happens to be installed:

```text
python3 scripts/compatibility_profile.py doc/release-profile.toml
python3 scripts/compatibility_profile.py doc/release-profile.toml --format json
```

The command reports top-level names, supported SymPy class paths, and supported
method paths. It validates every artifact against the same SymPy version first,
and exits with an error if inventory, classification, naming, ledger, or API
diff files are mixed across baselines. Pass `--sympy-version VERSION` when a
caller wants an additional exact-baseline assertion.

## 6. Contributor checklist for compatibility PRs

- [ ] No source copied from a GPL project.
- [ ] Any adapted BSD source retains its notice and is recorded in
      `doc/provenance.md` with original file and revision.
- [ ] New behaviour is covered by an independently written test.
- [ ] The compatibility table is updated, including known differences.
- [ ] Unsupported input raises a typed error rather than a wrong answer.
