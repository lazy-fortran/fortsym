# Wolfram frontend coverage: corrugation-resistance derivation

The existing source `fortsym-bench/corpus/proj-flux_pumping/39_corrugation_resistance.wl`
was run through the native Wolfram subset reader on 2026-08-10. The command
was:

```text
fpm run --target fortsym_wl_run -- \
  /mnt/storage/code/lazy-fortran/fortsym-bench/corpus/proj-flux_pumping/39_corrugation_resistance.wl
```

The initial run produced 30 bindings and named 20 refusals. After two bounded
gap closures, the same source produced 40 bindings and named 10 refusals.
Coverage of the 50 top-level bindings is therefore 80%. The latest process
exited successfully and reported 1.258917 seconds for the evaluation pass.

Bindings evaluated:

`divB`, `divConstraint`, `tRule`, `Bvec`, `Bmag`, `lhsId`, `rhsId`, `tangency`,
`detuning`, `pRule`, `ord`, `onSurf`, `BgradChi`, `Xtest`, `BzOn`, `B2On`,
`F0`, `tilt0`, `tComposite`, `FconsTilt`, `FnaiveTilt`, `diffNC`, `c2`, `xs`,
`ys`, `rs`, `BxS`, `ByS`, `BrS`, `BtS`, `pShift`, `tShift`, `R0fix`, `B0fix`,
`d0fix`, `mfix`, `kfix`, `sample`, `ratioOn`, `figData`.

Refusals, in source order:

| Binding | Diagnostic |
|---|---|
| `figdir` | `FileNameJoin` needs non-empty literal string components |
| `$Assumptions` | parser reached unexpected end of input |
| `CCsol`, `Fcons`, `Fnaive`, `csSquare`, `ratioVar` | definite `Integrate` has no verified antiderivative |
| `flipRadius` | `Part` of a non-list |
| `figIota` | `NIntegrate` is not implemented |
| `figCR` | `ListPlot` data contains a non-number |

The first two gap closures added function-valued head replacement for rules
such as `t -> Function[z, z + 1]`, recognized the parser's canonical
`Pattern[name, Blank[]]` form, and allowed bounded real-valued `Table` ranges.
The next measurement should isolate the definite-integration and numeric
plotting requirements before changing either evaluator.
