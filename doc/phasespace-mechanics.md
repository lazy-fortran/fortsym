# Phase-space (first-order) Lagrangian mechanics

`fortsym_phasespace` is a general symplectic-mechanics layer over arbitrary
coordinates `q_1..q_n`. It knows nothing about any particular physical
system; a caller supplies its own one-form coefficients and Hamiltonian-like
term.

## Phase-space Lagrangian

A `phase_lagrangian_t` represents
`L = sum_i a_i(q) qdot_i - h(q)`, built with

```fortran
use fortsym_phasespace, only: phase_lagrangian_t, phase_lagrangian_create

pl = phase_lagrangian_create(q, a, h, ok, message)
```

## Symplectic form and Pfaffian

`symplectic_form(pl)` returns the antisymmetric matrix
`omega_ij = d_i a_j - d_j a_i`. `pfaffian(omega, ok, message)` computes its
Pfaffian by the standard recursive definition
`Pf(A) = sum_{j=2}^{n} (-1)^j A(1,j) Pf(A with rows/cols 1,j removed)`,
supported for even dimension 2, 4, or 6 (`det(omega) = Pf(omega)**2`; the
Pfaffian itself, unlike a bare square root of the determinant, keeps the
correct sign and stays polynomial). It is the Liouville / phase-space
density: the flow below preserves `Pf(omega) dq_1 ^ ... ^ dq_n`.

## Equations of motion

```fortran
call phase_space_rates(pl, qdot, ok, message, pf=pf)
```

solves `omega qdot = grad h` for `qdot` by symbolic matrix inversion
(`fortsym_matrix`), for any `n` whose symplectic form is invertible; `pf` is
filled from `pfaffian` when the caller asks for it. Simplify the result
before further symbolic or numeric use -- like `diff`, `phase_space_rates`
does not simplify, and the raw fraction-free inverse can carry removable
singularities that a naive numeric evaluator trips over even where the
simplified rate is perfectly well-defined.

## Noether rate, reparametrisation, and the classical residual

- `noether_rate(pl, k, qdot)` returns `d/dt(a_k) = sum_j (d_j a_k) qdot_j`;
  the caller substitutes the solved `qdot` and simplifies to read off a
  conserved quantity.
- `reparametrize_rates(rates, k)` returns `rates(i)/rates(k)`, i.e.
  `dq_i/dq_k`, for using `q_k` as the new independent variable.
- `el_residual(L, q, qdot, qddot)` is the classical second-order
  Euler-Lagrange residual `d/dt(dL/dqdot_i) - dL/dq_i` for an ordinary
  `L(q, qdot)`, with `q` and `qdot` treated as independent symbols and the
  total time derivative expanded purely formally through a caller-supplied
  `qddot` (an independent symbol standing for `d(qdot)/dt`); no relation
  between `q` and `qdot` is assumed.

See `test/calculus/test_fortsym_phasespace.f90` for worked, independently
hand-derived oracles: a charged particle in a uniform magnetic field
(circular motion at the cyclotron frequency), a canonical pendulum (exact
energy conservation and the small-angle simple-harmonic limit), and a
generic linear drift model with a closed-form rate vector.
