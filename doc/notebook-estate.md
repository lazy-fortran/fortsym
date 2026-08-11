# Mathematica notebook estate

Issue #69 audited the tracked Mathematica notebooks in the consumer
repositories under `/home/ert/code`. The inventory found 26 notebooks in four
repositories. A notebook is considered a live candidate here when it is not in
an explicitly archived `old/` directory. That is a routing classification, not
a claim that the owning project still publishes every file.

| Repository | Notebook count | Live candidates | Explicitly superseded | Content represented |
| --- | ---: | ---: | ---: | --- |
| DESC | 1 | 1 | 0 | Second-order NAE geometry development |
| KiLCA | 10 | 10 | 0 | Conductivity and tensor collision, drift, formula, power, and vacuum derivations |
| paper_magnetic | 13 | 5 | 8 | Cylindrical, spherical, and paper magnetic-field derivations |
| profit | 2 | 2 | 0 | Pendulum and trigonometric-exponential drafts |
| **Total** | **26** | **18** | **8** | |

The eight superseded files are exactly the files under
`paper_magnetic/old/`. The remaining 18 are not marked superseded in their
repository and need owner-level archival decisions if they are no longer
live.

The complete tracked inventory is:

- DESC: `docs/dev_notes/NAE_to_DESC_geometry_2nd_order.nb`.
- KiLCA: `flre/conductivity/tests/W.nb`, `flre/tensor/coll.nb`,
  `flre/tensor/coll_new.nb`, `flre/tensor/cond.nb`, `flre/tensor/drift.nb`,
  `flre/tensor/formulas.nb`, `flre/tensor/mag_drift.nb`,
  `flre/tensor/power.nb`, `flre/tensor/test_gamma.nb`,
  `flre/tensor/vacuo.nb`.
- paper_magnetic live candidates: `paper_magnetic.nb`,
  `doc/cyl_analyt_l0.nb`, `doc/cyl_analyt_t0.nb`, `doc/cyl_analyt_tn.nb`,
  `doc/spherical.nb`.
- paper_magnetic superseded files: `old/cart_analyt.nb`, `old/inductor.nb`,
  `old/levicivita.nb`, `old/paper_magnetic.nb`,
  `old/paper_magnetic_old.nb`, `old/radia1.nb`, `old/test_det.nb`,
  `old/testsnb.nb`.
- profit: `draft/Pendulum/cosine.nb`, `draft/Pendulum/sqexpsin.nb`.

## Derivation-share sample

The sample measure counts imported notebook cells by style. It reports Input,
Output, and prose or heading cells separately, then uses

`Input / (Input + Output + prose)`

as a reproducible derivation-share proxy. It is not semantic classification:
an Input cell can contain a plot or an exploratory command, and cached Output
cells are part of the document.

| Notebook | Input | Output | Prose/headings | Input-cell proxy |
| --- | ---: | ---: | ---: | ---: |
| `paper_magnetic.nb` | 23 | 20 | 3 | 50.0% |
| `doc/cyl_analyt_l0.nb` | 15 | 24 | 5 | 34.1% |
| `KiLCA/flre/tensor/drift.nb` | 88 | 122 | 0 | 41.9% |
| `DESC/docs/dev_notes/NAE_to_DESC_geometry_2nd_order.nb` | 50 | 23 | 61 | 37.3% |
| **Total** | **176** | **189** | **69** | **40.6%** |

The sample is therefore materially derivation-bearing, but it is also mixed
with presentation and exploration. The result does not justify a general box
parser or a promise of whole-notebook conversion.

## Route decision

Use a documented Mathematica export route. Do not add a Mathematica box parser
to fortsym. The exporter preserves the notebook kernel's StandardForm
interpretation without evaluating cells, produces ordinary `.wl` input, and
allows unsupported cells to remain visible for owner review. The committed
helper is `scripts/export_notebook_inputs.wls`.

From the repository root, export one notebook and also retain one file per
Input cell for coverage measurement:

```text
math -noprompt -run 'Get["scripts/export_notebook_inputs.wls"]; Exit[]' -- \
  input.nb output.wl /tmp/fortsym-notebook-cells
```

The helper imports the notebook, selects `Cell[BoxData[...], "Input"]`, holds
each StandardForm expression with `ToExpression[..., HoldComplete]`, removes
`Null` cells, and writes InputForm text. Each exported cell is also written as
`cell-NNN.wl` when the optional directory is supplied. Translate cells with
`fortsym_wl_to_f90 cell-NNN.wl cell-NNN.f90` and report accepted versus refused
cells. The whole `output.wl` stream may still be refused when it contains
non-assignment statements.

The end-to-end demonstration used
`paper_magnetic/doc/cyl_analyt_l0.nb`. It exported 26 input cells to
`cyl_analyt_l0.wl`. Fortsym accepted 1/26 cells (3.8%) and refused 25/26 with
named diagnostics. The accepted cell was a scalar assignment. The refusals
show the current boundary clearly: solver calls, plots, and other non-scalar
or non-assignment cells remain owner work. This is coverage evidence for the
route, not a claim that this notebook is ready for unattended conversion.

The next implementation work, if an owner wants more coverage, is to select
specific derivation cells and extend the existing Wolfram grammar against
independent numerical or compiled Fortran oracles. Converting all 26 files,
reproducing graphics, and parsing arbitrary Mathematica boxes are not part of
this issue.
