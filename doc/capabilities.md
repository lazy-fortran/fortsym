# Capabilities

This page describes what fortsym does in more detail than the README. The
[Fortran API](fortran-api.md) gives the calling conventions, and the
[feature matrix](feature-matrix.md) lists the supported operations.

## Why fortsym exists

Three problems in existing research codes motivated it:

- The computer algebra system sits outside the build. Notebook scripts derive
  an identity and print `PASS`, the Fortran is then written by hand, and nothing
  detects later drift.
- Generated kernels outlive their generators. Optimised code with `t1, t7, t13`
  temporaries and no generator left in the repository can be neither regenerated
  nor trusted.
- Every computer algebra system has blind spots.

fortsym keeps the generator in the repository, regenerates it in CI and checks
generated code against the symbolic definition in the same test binary.

## Native core

The native Fortran backend provides arbitrary-precision integer and rational
arithmetic (with a checked 64-bit fast path), collection of like terms and
integer powers, bounded polynomial expansion, differentiation, Taylor
coefficients and series, scalar linear solving, requested-precision numerical
evaluation (17 to 512 decimal digits) and conservative zero decisions.

Simplifications that need domain facts take an explicit `assumption_context_t`.
Positive, nonnegative, nonzero and real-valued facts are attached to
expressions: with `positive(x)`, `sqrt(x**2)` and `abs(x)` reduce to `x`;
without it the same zero test returns `UNKNOWN`.

Other native operations:

- `subs` replaces a symbol or subexpression; `subs_many` applies several
  replacements simultaneously, so swaps do not cascade.
- `solve_exact_linear_system` solves dense systems with exact rational
  elimination, refuses unsupported symbolic pivots and checks every residual.
- Bessel functions `besselj`, `besseli`, `besselk` and complete elliptic
  integrals `elliptic_k(m)`, `elliptic_e(m)` (parameter `m = k**2`) are
  differentiated with their standard recurrences (DLMF 19.4.1 for the elliptic
  integrals). Generated code calls the corresponding
  [fortnum](https://github.com/lazy-fortran/fortnum) routines.
- Derivatives of opaque functions such as `func("psi", [r, u])` stay symbolic as
  canonical partial-derivative nodes, with mixed partials in sorted order.
- `fortsym_lie:lie_series` builds the truncated Lie series
  `sum_{k=0}^{order} t^k L_V^k(f)/k!` for orders 0 to 12.

## Engines

SymEngine and Yacas are linked in process; SymPy and Maxima run as separate
processes when installed. Each engine declares the operations it supports and is
only asked for those (Yacas, for example, does not claim zero testing because it
cannot close trigonometric identities). Answers, assumptions and conditions from
each engine are kept. Agreement between engines is supporting evidence, not a
proof; a disagreement requires an independent check. See
[architecture](architecture.md).

## Identity decisions

Expressions are brought to an exponential normal form, which decides the
Pythagorean, hyperbolic, angle-sum, multiple-angle, tangent-addition, exp/log
and rational identities. Identities that hold only on part of the domain
(`sqrt(x**2) - x`, `atan(x) + atan(1/x) - pi/2`) are not reported as identities,
and expressions outside the decidable fragment (gamma, Bessel, zeta) return
`UNKNOWN`. Radical identities additionally use bounded polynomial reduction by
the defining relations of the principal roots; only a zero remainder counts as a
proof.

Checks show the strength of the evidence:

```
PASS         pythagorean (decidable)
PASS(probe)  gamma recurrence   [no counterexample in 98 points]
```

`check_identity` is the strict variant that refuses probe evidence.

## Kernel generation

Kernel generation applies common-subexpression elimination and writes Fortran
with kind-suffixed literals, integer exponents, correct `atan2` argument order,
minimal parentheses and 132-column continuation lines. Candidate
simplifications from all engines are first checked for equivalence and then
ranked by operation count after CSE.

Scalar Fortran and CUDA emission goes through the
[fortgen](https://github.com/lazy-fortran/fortgen) backend. A target identity
(`fortran_cpu`, `fortran_openmp_target`, `fortran_openacc`, `cuda`) chooses the
source spelling and leaf decoration; fortsym emits no parallel schedule or data
movement. Further options:

- explicit source policies for small-power expansion, constant folding,
  constant-division elimination and FMA shaping (no fast-math flags);
- CSE level (`CSE_NONE`, `CSE_THRESHOLDED`, `CSE_FULL`) and a rematerialisation
  threshold on `kernel_spec_t`; complex scalar types and mixed argument types;
- ordered output dependencies, where a later output may read an earlier one;
- `array_temporaries` to keep thousands of CSE temporaries in one local array;
- CPU precision variants (fp64, fp32, fp16, bf16 storage) from one source, see
  [precision](../docs/precision.md);
- CUDA batch kernels with structure-of-arrays storage via
  `emit_cuda_batch_ir`.

`fortsym_rigorous_emit` renders one expression graph as a floating-point kernel
and as an enclosure kernel whose outputs contain the exact value for all inputs
in given balls or intervals; see [rigorous runtime](rigorous-runtime.md).

## Derivative products

`jvp`, `vjp`, `gradient` and `hvp` never form a Jacobian or Hessian. The
implicit-function builders emit the actions of the residual derivatives `R_y`
and `R_p` directly, so the iteration that found the solution does not enter
the derivative.

## Output certificates

`fortsym_certificate` derives, from a symmetric/skew split `K = S + A`, the
complementary functionals that bracket `<s, K^{-1} s>`, gap indicators, a
dual-weighted-residual estimate for non-symmetric outputs and parameter
gradients. Each derived formula records which statements are proved and which
are only checked numerically. See [certificate](certificate.md).

## Differential geometry

Given a coordinate chart, fortsym derives the basis, metric, inverse metric,
Jacobian, Christoffel symbols and grad/div/curl/Laplacian. Tests assert
identities rather than stored answers (`g^ik g_kj = delta`, `det g = J^2`,
`curl grad = 0`, `div curl = 0`). On top of this:

- typed tensors with recorded variance, `raise`/`lower`, tensor products and
  contraction (`tensor_native` app);
- differential forms, e.g. the magnetic two-form `dA` and `i_B(volume) = dA`
  (`forms_native` app);
- connections and curvature: Riemann, Ricci, scalar curvature, Einstein tensor,
  Bianchi identity (`connection_native` app);
- examples for magnetic flux coordinates, Boozer coordinates, spherical and
  cylindrical coordinates, de Sitter space and the weak-field GPS clock
  correction in [`example/`](../example).

The `paper_magnetic_native` app implements the Albert-Bíró-Lainer Fourier
reduction of the magnetic field, with covariant, contravariant and density
components kept distinct; `example_cylindrical_fourier` is the compact version
in `(Z, R, phi)` ordering. The Python
`fortsym.Chart` class exposes the same operations. Geometry derivations are
described further in [geometry-derivations](geometry-derivations.md).

## Fortran readback

Given a source file and a variable name, fortsym returns the expression that the
file computes, so a hand-written kernel can be checked against its definition.
This is exact expression algebra; it does not model floating-point rounding or
control flow.

## Cross-engine timings

Per-engine timings are recorded during normal operation. On one set of nine
identity questions:

| Engine | Decided | Time per call |
|---|---|---|
| SymEngine (linked) | 7/9 | 0.0001 s |
| Yacas (linked) | not asked | 0.08 s |
| Maxima (subprocess) | 4/9 | 0.41 s |
| SymPy (subprocess) | 4/9 | 0.38 s |

See [benchmarks](benchmarks.md) for the benchmark protocol.
