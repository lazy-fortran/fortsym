# Native Wolfram-language runner

fortsym includes a Fortran interpreter for the subset of the Wolfram language
used in an existing corpus of research derivation scripts. It does not invoke
Mathematica, Mathics or Python. The accepted grammar and what is refused are
listed in [compatibility-wolfram](compatibility-wolfram.md).

## Running a script

```bash
build/bin/fortsym_wl_run path/to/script.wl   # CMake build
fo exec fortsym_wl_run path/to/script.wl     # fo build
```

The runner parses and evaluates the `.wl` source directly and prints results in
the `R<TAB>name<TAB>value` / `T<TAB>seconds` format used by
[fortsym-bench](https://github.com/lazy-fortran/fortsym-bench). The evaluated
subset includes associations, plots, exact polynomial and matrix operations,
bounded `Solve` and `Integrate`, list operations and numerical functions; each
supported head has native tests. Unsupported forms remain unevaluated rather
than being dropped.

## Translating to Fortran

`fortsym_wl_to_f90 input.wl output.f90` translates a sequential stream of up to
128 scalar assignments (`r = 2*x + Sin[x]`, `s := r + 1`) into a compilable
Fortran subroutine. It infers scalar inputs, expands earlier assignments and
uses the normal kernel emitter. It refuses control flow, reassigned names,
arrays, names that are not valid Fortran identifiers and unsupported
expressions. This is an entry point for simple scripts, not a translator for the
whole corpus: corpus runs still interpret the Wolfram source at run time.

## Corpus comparison

fortsym-bench compares the native runner with SymPy translations and Mathics on
the 384-script corpus. The audit of 2026-08-03 agreed on 4,024 of 4,879 emitted
bindings, with 386 declared differences; the remainder are timeouts, errors,
unsupported forms or bindings without a usable reference value. Full parity is
open work tracked in the [roadmap](roadmap.md).
