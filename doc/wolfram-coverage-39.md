# Wolfram frontend coverage: corrugation-resistance derivation

The existing source `fortsym-bench/corpus/proj-flux_pumping/39_corrugation_resistance.wl`
was run through the native Wolfram subset reader on 2026-08-10. The command
was:

```text
fpm run --target fortsym_wl_run -- \
  /mnt/storage/code/lazy-fortran/fortsym-bench/corpus/proj-flux_pumping/39_corrugation_resistance.wl
```

The run produced 30 bindings and named 20 refusals. Coverage of the 50
top-level bindings was therefore 60%. The process exited successfully and
reported 0.032271 seconds for the evaluation pass.

Bindings evaluated:

`divB`, `divConstraint`, `Bmag`, `lhsId`, `rhsId`, `tangency`, `detuning`,
`ord`, `onSurf`, `Xtest`, `B2On`, `F0`, `tComposite`, `diffNC`, `c2`, `xs`,
`ys`, `rs`, `BxS`, `ByS`, `BrS`, `BtS`, `pShift`, `tShift`, `R0fix`, `B0fix`,
`d0fix`, `mfix`, `kfix`, `ratioOn`.

Refusals, in source order:

| Binding | Diagnostic |
|---|---|
| `figdir` | `FileNameJoin` needs non-empty literal string components |
| `$Assumptions` | parser reached unexpected end of input |
| `tRule`, `pRule`, `tilt0` | `Function` is not implemented |
| `Bvec` | replacement needs `Rule` or `RuleDelayed` |
| `BgradChi`, `BzOn`, `flipRadius` | `Part` of a non-list |
| `CCsol`, `Fcons`, `Fnaive`, `csSquare`, `ratioVar` | definite `Integrate` has no verified antiderivative |
| `FconsTilt`, `FnaiveTilt` | replacement needs `Rule` or `RuleDelayed` |
| `sample` | `Table` needs integer range bounds |
| `figData`, `figIota` | `Table` needs integer range bounds |
| `figCR` | `ListPlot` needs an explicit list of data |

The highest-frequency reachable gap is the combination of function-valued
rules and downstream replacement. The next measurement should isolate that
construct from the derivation's integration and plotting requirements before
changing the evaluator.
