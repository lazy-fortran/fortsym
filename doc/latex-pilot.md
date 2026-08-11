# Real-manuscript LaTeX pilot

Issue #68 was tested against the magnetic-paper derivation in
`/home/ert/code/paper_magnetic/paper_magnetic.py`, using its Eq. (40), and an
exported copy of the current `paper_magnetic_comment.lyx`. The consumer
repository was not modified. The small driver
`app/fortsym_latex_paper_pilot.f90` builds the two sides of the first Ampère
component, registers the notation required by the manuscript, and writes
[`doc/latex-pilot/eqs.tex`](latex-pilot/eqs.tex).

The pilot uses one macro for each named side:

```latex
\eqAmpereOne = \eqAmpereOneRhs
```

This keeps the relation itself document-owned because `expr_t` currently
represents expressions, not relations. The generated file is included in the
exported manuscript copy before that equation. Since the local LaTeX
installation has no `IEEEtran.cls`, the copy uses the standard `article`
class; it builds successfully with `pdflatex` and produces a seven-page PDF.
The only diagnostics are pre-existing undefined references and an overfull
box in the manuscript.

## Findings

The generated sides contain seven symbolic leaves. Six need explicit notation
registration (`d2_nu33_curl_a`, `nu22`, `A1`, `nu21`, `A2`, and `J1`); `n`
renders acceptably without an override. The pilot registers all seven so the
output is intentional and reviewable. There are no typesetting failures.

One macro per named result is the right shape for the current writer: it gives
the manuscript stable names and keeps the left/right relation visible at the
use site. A future relation API could generate one named relation, but that is
outside this pilot.

No additional SymPy printer settings were needed. The output is correct but
slightly unidiomatic in one place: commutative canonicalisation prints
`A_{1}\,\nu_{22}` where the manuscript writes `\nu_{22}A_{1}`. The derivative
and curl expression is currently one registered opaque leaf; the scalar tree
does not yet carry derivative/operator semantics. These findings are tracked
separately rather than changing the fixed emitter conventions speculatively.

## Reproduction

From the repository root:

```text
fo build
build/bin/fortsym_latex_paper_pilot /tmp/paper-magnetic-eqs.tex
lyx -batch -f all -E latex /tmp/paper_magnetic_comment.tex \
    /home/ert/code/paper_magnetic/paper_magnetic_comment.lyx
```

Make a copy of the exported file, replace its unavailable `IEEEtran` class
with `article`, insert

```latex
\input{/tmp/paper-magnetic-eqs.tex}
\begin{equation}
\eqAmpereOne = \eqAmpereOneRhs
\end{equation}
```

before `\end{document}`, and run:

```text
pdflatex -interaction=nonstopmode -halt-on-error \
    -output-directory=/tmp /tmp/paper-magnetic-pilot.tex
```
