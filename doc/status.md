# Verification status and limits

FortSym is research software with a tested, bounded symbolic interface.
The [feature matrix](feature-matrix.md) lists implemented and partial operations;
it does not claim complete SymPy or Wolfram Language compatibility.

The registered native tests check expression construction, exact arithmetic,
symbolic identities, derivative products, parsing and generated kernels.
Independent oracles include algebraic reconstruction, analytic values,
finite differences, adjoint identities and compilation of generated source.
Optional tests compare overlapping operations with SymPy and Mathics in
separate processes. The build instructions in [building.md](building.md)
describe the dependency configurations these checks require.

The [scalar derivation API](derivation-api.md) records and exports its stated
exact scalar fragment. Whole-program replay, arbitrary Python control flow,
units and matrix derivation recording remain outside that fragment. The
[Wolfram frontend](compatibility-wolfram.md) reads a documented textual subset;
it does not read notebook boxes or reproduce arbitrary evaluation state.

Zero decisions return `UNKNOWN` outside their supported decision procedures.
An external engine agreement or a numerical sample is evidence, not a proof.
Generated enclosure kernels are conditional on the consumer's
[enclosure runtime](rigorous-runtime.md); emitter tests do not prove a general
floating-point enclosure model.

Performance results apply to their recorded workload, configuration and
measurement scope. They do not establish general performance parity with
another computer algebra system. Broader compatibility and verification work
is listed in the [development directions](roadmap.md).
