# Generating a tensor-product apply

FortSym lowers the caller's symbolic pointwise expressions once, then passes
Kernel IR v1 and the tensor schedule parameters to FortGen. The public API is
in `fortsym_kernel_emit`:

```fortran
source = emit_tensor_fortran(roots, leaf_spec, tensor_spec, ok, message)
source = emit_tensor_cuda(roots, leaf_spec, tensor_spec, ok, message)
```

`roots(:)` is an `expr_t` array. `leaf_spec` is `kernel_emit_spec_t` and supplies
ordered input names and output names. `tensor_spec` is `tensor_kernel_spec_t`:
`name` (FortSym `str_t`), `dimension`, `nloc`, `nq`, `batch` (default 8),
`ncoeff`, `precision` (REAL32/REAL64), and `cell_tables` (default false).
The first `d+1` leaf arguments are the interpolated value followed by reference
gradients 1..d. Subsequent inputs are coefficients at this quadrature point.
The first output is the value/test contribution, followed by gradient/test
contributions. Emission returns empty source and a diagnostic on failure.

The exact generator entry point for a matrix acting on reference gradients is
`app/gen_tensor_apply.f90`. It defines `f_0=0` and
`f_i=sum_j G(i,j)*grad_j(u)` as expressions in the consumer's arena. All
geometry, physical coefficients and integration weights must already be in G.
The generator has no knowledge of these choices.

For MF1's apply, build and call:

```sh
cmake --build BUILD --target gen_tensor_apply -j4
# target output dimension degree nq_increment cell_tables precision_bits
BUILD/bin/gen_tensor_apply fortran apply_p4.f90 3 4 1 0 64
BUILD/bin/gen_tensor_apply cuda apply_p4.cuh 3 4 1 0 32
```

Here `nloc=degree+1`, `nq=nloc+nq_increment`, and `cell_tables=1` selects
independent per-direction tables for each cell, as needed for non-uniform
splines. Degrees 1..8, dimensions 2/3 and increments 0/1 are supported.
The same calls from a `tools/codegen/app/gen_*_products.f90` generator are:

```fortran
tensor%name = str('tensor_apply')
tensor%dimension = d
tensor%nloc = p+1
tensor%nq = p+2
tensor%batch = 8
tensor%ncoeff = d*d
tensor%cell_tables = .false.
tensor%precision = PRECISION_REAL64
! roots and leaf%args/outputs are defined exactly as in gen_tensor_apply.
source = emit_tensor_fortran(roots, leaf, tensor, ok, message)
```

The emitted CPU module `tensor_apply_module` exports
`tensor_apply(nc,u,b,db,coef,y)`. Use `u(8,(p+1)**d)`,
`y(8,(p+1)**d)`, `coef(8,nq**d,d*d)` and
`b(nq,p+1,d,nt)`, `db(nq,p+1,d,nt)`, with `nt=1` or `nc`.
Coefficient `(i-1)*d+j` holds `G(i,j)`. Direction 1 is fastest in the flattened
tensor index. `0<=nc<=8` allows partial batches. OpenMP over independent
batches belongs to the caller. The generated kernel uses SIMD across cells and
private scratch for every call. Gather/scatter and global DOF maps remain in
the consumer.

CUDA exports `tensor_apply<<<nc,threads>>>(nc,u,b,db,coef,y)`.
Use `threads=32*ceiling(real(max(nloc,nq)**d)/32)`, at most 1024. Tables
are in shared memory, one block owns one cell, and threads own tensor indices.
The CUDA ABI is cell-major for fields and coefficients. For zero-based indices,
`u[cell*nloc**d+i]` and `coef[(cell*d*d+k)*nq**d+q]`; table layout remains
`b[q+nq*(i+nloc*(axis+d*table_cell))]`. The CPU and GPU arrays therefore need
different packing. See the [FortGen contract](https://github.com/lazy-fortran/fortgen/blob/main/spec/tensor-kernels.md)
for launch preconditions, shared-memory limits, arithmetic and benchmark scope.

The schedules do not create quadrature tables or assert basis properties.
Dense Kronecker-product oracles in FortGen cover every supported size/layout
and both precisions. FortSym's registered `test_tensor_apply` compiles the
symbolically emitted product and checks an independent dense, nonsymmetric G
apply, including the zero value channel. The existing emitted-kernel audit
continues to check the unchanged scalar backend.
