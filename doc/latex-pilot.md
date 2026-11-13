# LaTeX relation example

The executable [`fortsym_latex_paper_pilot`](../app/fortsym_latex_paper_pilot.f90)
builds two symbolic sides of an Ampère relation, registers notation for its
scalar leaves, and writes a named LaTeX macro. It uses
`latex_t%relation` so the expressions remain separate values while the generated
artifact owns the equals sign.

After building, run:

```sh
build/bin/fortsym_latex_paper_pilot equation.tex
```

Load `amsmath` in the document preamble and include the resulting file:

```latex
\usepackage{amsmath}
\input{equation.tex}
```

Use the macro inside the document:

```latex
\begin{equation}
\eqAmpereOne
\end{equation}
```

The program registers LaTeX names for scalar symbols and prints a symbolic derivative.
Products retain canonical expression order; the printer does not reorder
commutative factors to imitate a manuscript. Register desired symbol spellings
with `latex_t%name`. See the
[LaTeX API](latex-api.md) for notation registration and output contracts.
