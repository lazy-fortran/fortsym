# One symbolic source, four CPU storage variants

`fortsym_kernel_emit:emit_cpu_precision_kernels(roots,spec,ok,message,compute)`
accepts a vector of expressions and the existing scalar `kernel_emit_spec_t`.
Put primal and derivative products in the same `roots` vector. FortSym lowers
the expressions once to shared IR and delegates family emission to
[FortGen](https://github.com/lazy-fortran/fortgen). The generated names are
`NAME_fp64`, `NAME_fp32`, `NAME_fp16`, `NAME_bf16`; the half interfaces store
IEEE encodings as `integer(int16)` and use fp32 arithmetic by default. Optional
`compute=PRECISION_REAL64` selects fp64 arithmetic for half storage. Generated
source records the supplied generator/revision provenance.

This API is additive and CPU-only. Existing scalar, batch, enclosure and CUDA
emitters retain their contracts. It does not choose precision for certificates
or provide a rounding enclosure. The consumer must assess its conditioned
roundoff budget and scale values before half-storage boundaries as needed.
Generated CPU files require only FortGen's storage runtime; consumers build
without FortSym or a CAS. Commit generated files in the consumer and retain
its generator, regeneration comparison and independent accuracy oracle.

`test_precision_family` generates a primal and symbolic derivative together,
compiles all four variants and checks them against independent polynomial
identities at 257 points. Tolerances derive from unit roundoff and conditioning.
See FortGen's precision documentation for supported names and runtime semantics.
