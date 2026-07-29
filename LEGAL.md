# Legal and provenance notes for fortsym

fortsym is a multi-engine symbolic frontend. It links some computer algebra
code into its own process and drives other engines as separate programs. Those
two situations carry different licence obligations, so this document records
what fortsym depends on, how each dependency is reached, and which rules
contributors have to follow to keep the result distributable.

**This is not legal advice.** It records the project's engineering policy and
the reasoning behind it. Anyone shipping fortsym inside a product, or using it
under an institutional site licence, should confirm the terms that apply to
them.

## 1. fortsym's own licence

fortsym is MIT licensed. See [LICENSE](LICENSE).

Everything under `src/`, `test/`, `app/` and `cmake/` is original work written
for this project unless a file header says otherwise.

## 2. The rule that shapes the architecture

Licence obligations attach to *linking*, not to *using*. A program that links a
library forms one work with it; a program that starts another program and
exchanges data with it at arm's length does not.

fortsym therefore splits engines into two tiers, and the split is a licence
boundary before it is a design choice:

- **Tier 1 — linked into the fortsym process.** Permissive and LGPL libraries
  only. This is the only tier that can appear in a hot path.
- **Tier 2 — driven as a separate process**, exchanging expressions as text.
  Anything else, including GPL engines. Nothing from Tier 2 enters fortsym's
  link closure, and fortsym works fully without any of it.

A consequence worth stating plainly: **fortsym has no build-time or run-time
dependency on any Tier 2 engine.** They are detected at run time; absence is
recorded and the engine is skipped, never a build failure.

## 3. Tier 1 — linked dependencies

| Component | Version | Licence | SPDX | How linked | Upstream |
|---|---|---|---|---|---|
| SymEngine | 0.14.0 | MIT | `MIT` | static or shared | https://github.com/symengine/symengine |
| Yacas | 1.9.1 | LGPL-2.1-or-later | `LGPL-2.1-or-later` | **shared only**, built from source | https://github.com/grzegorzmazur/yacas |
| FLINT | 3.6.0 | LGPL-3.0-or-later | `LGPL-3.0-or-later` | **shared only** | https://github.com/flintlib/flint |
| GMP | 6.3.0 | LGPL-3.0+ / GPL-2.0+ dual | `LGPL-3.0-or-later` | **shared only**, transitive | https://gmplib.org |
| MPFR | 4.2.2 | LGPL-3.0-or-later | `LGPL-3.0-or-later` | **shared only**, transitive | https://www.mpfr.org |

SymEngine is MIT, so it imposes nothing beyond attribution.

Yacas is LGPL-2.1-or-later and is not packaged for the target distribution, so
fortsym builds it from source through `cmake/deps/yacas.cmake` at a pinned tag.
It is built and linked as a **shared** library, which keeps the LGPL relinking
obligation satisfied the same way the system LGPL libraries do. It is a small,
self-contained C++ tree with no dependencies of its own, so building it does not
make installation cumbersome — which is the reason it qualifies for Tier 1
despite not being packaged.

FLINT, GMP and MPFR are LGPL. The LGPL permits a differently licensed program
to link them provided the recipient can replace the library with a modified
version. fortsym satisfies that by linking them **dynamically**, which is also
how the distribution packages ship them. Two obligations follow and are binding
on anyone redistributing a fortsym build:

1. Do not statically link FLINT, GMP or MPFR into a distributed binary without
   also providing what the LGPL requires for relinking.
2. Ship the LGPL licence texts and the attribution notices with any binary
   distribution.

The CMake fetch path (`-DFORTSYM_USE_SYSTEM_DEPS=OFF`, see
`cmake/deps/symengine.cmake`) builds SymEngine from source but still expects
GMP from the system, precisely so the LGPL components stay shared.

### Interface definitions reproduced from upstream

`src/capi/fsym_shim.cpp` restates `struct CRCPBasic`, which SymEngine defines in
its own `cwrapper.cpp` rather than in an installed header. Reproducing it is
necessary to convert SymEngine's opaque C handle back to a C++ object, and
`cwrapper.h` documents the layout contract for exactly this purpose. SymEngine
is MIT, so this is permitted; the file carries a comment recording where the
definition comes from and `static_assert`s that the layout still matches.

No other third-party source is copied into fortsym.

## 4. Tier 2 — engines driven as separate processes

Detected at run time, never linked, never bundled, never required.

| Engine | Licence | SPDX | Reached via | Upstream |
|---|---|---|---|---|
| SymPy | BSD-3-Clause | `BSD-3-Clause` | `python3` subprocess | https://www.sympy.org |
| Maxima | GPL-2.0-or-later | `GPL-2.0-or-later` | `maxima` subprocess | https://maxima.sourceforge.io |
| GiNaC | GPL-2.0-or-later | `GPL-2.0-or-later` | subprocess (optional) | https://www.ginac.de |
| Giac/Xcas | GPL-3.0-or-later | `GPL-3.0-or-later` | subprocess (optional) | https://xcas.univ-grenoble-alpes.fr |
| FriCAS | modified BSD | `BSD-3-Clause` | subprocess (optional) | https://fricas.github.io |
| PARI/GP | GPL-2.0-or-later | `GPL-2.0-or-later` | subprocess (optional) | https://pari.math.u-bordeaux.fr |
| Singular | GPL-2.0 / GPL-3.0 | `GPL-3.0-or-later` | subprocess (optional) | https://www.singular.uni-kl.de |

SymPy and FriCAS are permissive; they are in Tier 2 for engineering reasons
rather than legal ones. Linking SymPy would mean embedding CPython, and FriCAS
and REDUCE would each drag in a Lisp runtime — too large to justify against the
"do not make installation cumbersome" rule. Yacas, by contrast, is small and
dependency-free, so it is built from source and linked (§3).

**GPL engines.** Driving Maxima, GiNaC, Giac, PARI or Singular as a separate
program, passing expressions in and reading results back, does not make fortsym
a derivative work of them. What *would*: copying their source or a transcribed
algorithm into fortsym (see §6), or linking them.

**Engine output.** Results computed by a Tier 2 engine belong to whoever ran
it. fortsym does not check engine output into this repository, and generated
kernels under `src/generated/` are produced by the Tier 1 path only, so that
the committed artefacts carry no third-party licence question.

## 5. Mathematica and Wolfram products: deliberately excluded

**fortsym does not have, and will not gain, a Wolfram backend. Mathematica is
also not used as a development or verification oracle for fortsym itself.**

The reasoning, recorded so the decision is not silently revisited:

- The Wolfram Mathematica License Agreement prohibits decompiling,
  disassembling and reverse engineering the software.
- Wolfram's Terms of Use prohibit using the Services to "create, train, or
  improve (directly or indirectly) a substantially similar product or service."

fortsym is a computer algebra frontend. Whether it counts as "substantially
similar" is a judgement call, and using Mathematica output to develop, tune or
validate fortsym's own simplifier is close enough to the prohibited reading that
the project does not do it. Excluding Wolfram entirely costs little — the
open engines cover the same ground — and removes the question rather than
managing it.

Note on scope, since the two documents differ: the "substantially similar
product" wording was found in Wolfram's Terms of Use covering their Services
(cloud and Wolfram|Alpha API). It was not confirmed to appear in the desktop
Mathematica agreement or in any particular site licence. The exclusion is
deliberately conservative and does not depend on resolving that.

**What this does not restrict.** Using your own licensed Mathematica for your
own work — including checking physics derivations that fortsym also checks — is
ordinary licensed use and is unaffected. The boundary is that such output must
not be fed back into fortsym's own algorithms, test corpus, or benchmark
baselines.

Consequences enforced in the repository:

- No `wolfram`, `wolframscript`, `math` or `WolframKernel` invocation anywhere
  in `src/`, `test/`, `app/` or CI.
- No `.wl`, `.wls` or `.nb` files, and no output derived from them.
- fortsym's identity corpus (`test/`) is derived from standard mathematical
  results or cross-checked against open engines only.

## 6. Rules for contributors

1. **Do not paste or transcribe code from a GPL engine** — Maxima, GiNaC, Giac,
   PARI, Singular, Sage — into fortsym. This is the realistic way an MIT project
   acquires a GPL obligation, and a process boundary does not save you from it.
   Reimplement from published mathematics and cite the source.
2. **Do not add a Tier 1 dependency without recording it in §3**, including its
   licence and whether it may be linked statically.
3. **Do not make any Tier 2 engine required.** Every code path must work when
   only Tier 1 is present.
4. **Do not add Wolfram in any form**, per §5.
5. **Cite the mathematics, not an implementation.** Where an algorithm follows a
   published method, name the reference in the source comment.

## 7. Attribution required in binary distributions

A binary distribution of fortsym must carry: fortsym's MIT licence; SymEngine's
MIT licence; and the LGPL texts and notices for FLINT, GMP and MPFR, together
with whatever the LGPL requires to permit relinking against modified versions.

Distributing fortsym does not require shipping anything for Tier 2 engines,
because none of them is included or linked.
