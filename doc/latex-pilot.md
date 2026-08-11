# Real-manuscript LaTeX pilot

Issue #68 was tested against the magnetic-paper derivation in
`/home/ert/code/paper_magnetic/paper_magnetic.py`, using its Eq. (40), and an
exported copy of the current `paper_magnetic_comment.lyx`. The consumer
repository was not modified. The small driver
`app/fortsym_latex_paper_pilot.f90` builds the two sides of the first Ampère
component, registers the notation required by the manuscript, and writes
[`doc/latex-pilot/eqs.tex`](latex-pilot/eqs.tex).

The pilot now uses one relation macro:

```latex
\eqAmpereOne
```

`latex_t%relation` renders the left and right `expr_t` values into one macro
with a document-owned equals sign. The generated file is included in the
exported manuscript copy before the equation. Since the local LaTeX
installation has no `IEEEtran.cls`, the copy uses the standard `article`
class; it builds successfully with `pdflatex` and produces a seven-page PDF.
The only diagnostics are pre-existing undefined references and an overfull
box in the manuscript.

## Findings

The generated relation contains the derivative node
`partial(nu33*curl_t(a), 2)` and the scalar leaves `nu33`, `n`, `A1`, `nu22`,
`nu21`, `A2`, and `J1`. The pilot registers the manuscript notation for those
leaves. The printer has evidence-based rules for `partial` and `curl_t`, and
the independent test checks that symbolic differentiation propagates through
the partial node. There are no typesetting failures.

One macro per named relation matches the manuscript use site. The relation API
keeps the two sides as expressions while making the equality part of the
generated artifact.

No additional SymPy printer settings were needed. Products retain the arena's
canonical semantic order. This can print `A_{1}\,\nu_{22}` where the manuscript
writes `\nu_{22}A_{1}`. The pilot treats that order as a reproducibility policy
and does not add a commutative reordering setting. A consumer can register a
larger notation leaf when a published grouping carries meaning.

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
\eqAmpereOne
\end{equation}
```

before `\end{document}`, and run:

```text
pdflatex -interaction=nonstopmode -halt-on-error \
    -output-directory=/tmp /tmp/paper-magnetic-pilot.tex
```
