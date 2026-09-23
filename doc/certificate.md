# Certified outputs: symbolic complementary bounds and proof obligations

`fortsym_certificate` derives the functionals that bracket a linear output
of `K f = s`, and `fortsym_obligation` records the identities each derived
formula must satisfy. Kernels are emitted with `fortsym_rigorous_emit`
(`doc/rigorous-runtime.md`), so the same spec gives a floating-point trial
kernel and a rigorous evaluation kernel.

The workflow is the loop

```
FORWARD  ->  ADJOINT  ->  CERTIFY  ->  INDICATE  ->  REFINE
trial f      trial g      [lo, hi]     eta_i >= 0    enlarge where eta_i is large
```

Trial solutions come from any solver and need no trust: every admissible
trial gives a valid bound. Only the functionals, their constraints, and the
arithmetic that evaluates them are trusted, and those are derived and
checked here.

## Setting

A real inner-product space splits by parity, `V = V_e + V_o`. The operator
is `K = S + A` with `S` symmetric positive semidefinite and parity
preserving, and `A` skew (`A^* = -A`) and parity flipping. `P_0` is the
projection onto the null space of `S_e`.

Even source `s`, output `q = <s, K^{-1} s> = <s, Q^{-1} s>`,
`Q = S_e + A^* S_o^{-1} A`:

| functional | admissible trials | inequality |
|---|---|---|
| `L(u) = 2<s,u> - <u,S_e u> - <A u, S_o^{-1} A u>` | any even `u` | `L(u) <= q` |
| `U(w) = <r, S_e^+ r> + <w, S_o w>`, `r = s + A w` | odd `w` with `P_0 r = 0` | `q <= U(w)` |

The optimum is `u = f_e`, `w = -f_o`. An odd source `s_o` enters through the
even transfer `t = A^* S_o^{-1} s_o`:
`<s_o, K^{-1} s_o> = <s_o, S_o^{-1} s_o> - <t, Q^{-1} t>` and
`<s_o, K^{-1} s_e> = -<t, Q^{-1} s_e>`. Mixed coefficients follow by
polarisation: `<x, Q^{-1} y> = (q(alpha x + y/alpha) - q(alpha x - y/alpha))/4`
for every `alpha > 0`, with bracket endpoints `[(lo+ - hi-)/4, (hi+ - lo-)/4]`.

**Gap indicators.** With diagonal `S` the gap splits into nonnegative
per-level terms on admissible `w`,

```
U(w) - L(u) = sum_i eta_i,
eta_i = (r_i - sigma_i u_i)**2 / sigma_i     (even, sigma_i > 0)
eta_i = (sigma_i w_i - (A u)_i)**2 / sigma_i (odd)
```

so a caller refines only the levels (Legendre degree, Fourier mode, energy
index, cell, time step) whose `eta_i` is large.

**Non-symmetric outputs (residual pairing, DWR).** For `J = <a, K^{-1} b>`,
trials `f` with `P_0 (b - K f) = 0` and `g` with `P_0 (a - K^* g) = 0`:

```
|J - <a, f> - <g, b - K f>|  <=  ||b - K f||_{S^+} ||a - K^* g||_{S^+}.
```

Proof: with `e = K^{-1}(b - K f)` the left side is `|<a - K^* g, e>|`, and
`||e||_S^2 = <K e, e> = <b - K f, e> <= ||b - K f||_{S^+} ||e||_S` because
`<A e, e> = 0`. The estimate is the dual-weighted-residual estimate of
Becker and Rannacher; the radius is its rigorous remainder bound, the
product of two residual norms, and is second order. Both squared norms are
sums of per-level terms, which are the DWR indicators.

**Gradients from the same dual information.** For parameters `theta` in
`K`, `a`, `b`: `dJ/dtheta = <a_theta, f> + <g, b_theta - K_theta f>`, exact at
the primal and dual solutions.

## Finite splits

`operator_split_t` holds `sigma(:)` (S diagonal in an orthonormal basis),
the matrix `a(:, :)`, and `parity(:)` (0 even, 1 odd). Entries are
expressions and may carry symbolic parameters.

| procedure | result | obligations recorded |
|---|---|---|
| `split_obligations(p, ledger, label)` | none | `A` skew, parity pattern, even null levels proven zero, other `sigma_i > 0` |
| `split_bounds(p, s, prefix)` | `split_bounds_t`: `lower`, `upper`, `constraints`, trial symbols `u`, `w` | none |
| `split_exact(p, s, b, ledger, label, q, u, w)` | exact `q` and optimal trials (small systems) | Schur rows, `L` and `U` tight at the optimum, optimum admissible |
| `split_indicators(p, s, b, ledger, label)` | `eta(:)` | gap equals the sum of indicators on the constraint set |
| `odd_source_transfer(p, s_o)` | `t` | none |
| `split_dwr(p, a, b, prefix)` | `dwr_t`: `estimate`, `primal_norm2`, `dual_norm2`, constraints, per-level indicators, trials `f`, `g` | none |
| `dwr_exact(p, a, b, d, ledger, label, J, f, g)` | exact `J`, primal and dual solutions | estimate exact, both residuals and constraints vanish |
| `dwr_gradient(p, a, b, d, theta)` | `dJ/dtheta` in terms of `f`, `g` | callers check it against `diff(J, theta)` |
| `weighted_adjoint(k, weight)` | `K^*` for `<x, y> = sum w_i x_i y_i` | none |
| `polarisation(q_plus, q_minus)` | `(q_plus - q_minus)/4` | none |

## Ladders

A ladder has levels `l = 0, 1, ...` with parity `(-1)**l`, `S = sigma(l)` on
level `l`, and `A` coupling `l` to `l + 1` through first-order operators
`a D + b` in the remaining variables, where `D = sum_k c_k d/dx_k` has
constant coefficients on a torus. This is the structure of a Legendre
expansion in pitch angle, but nothing in the module refers to one.

| procedure | meaning |
|---|---|
| `derivation_t` | coordinates, coefficients `c_k`, wavenumber symbols; `derivation_symbol(d)` is `sum c_k kappa_k` (`D` acts as `i` times it on a Fourier mode) |
| `apply_derivation(d, f)` | `D f` |
| `first_order_adjoint(op, d, weight)` | `(a D + b)^* = -a D + b - D(a w)/w` in `L^2(w dx)`, by integration by parts |
| `ladder_obligations(ledger, label, d, weight, sigma, up, dn, l)` | constant coefficients, `sigma(0) = 0`, `sigma > 0` above, and skewness `A_{l+1,l} = -(A_{l,l+1})^*`, symbolic in `l` |
| `constraint_obligations(ledger, label, d, up0, s0, mu, potentials)` | the level-0 constraint `s_0 + A_{0,1} w_1 = 0` with `w_1 = mu F` becomes `D F = g`; checks that `mu` is an integrating factor and that `g` is a divergence (zero torus mean, so every nonresonant mode is solvable); returns `g` |

## Proof obligations

`prove_zero(ledger, label, residual)` asks the native decision procedure
and SymEngine. A zero verdict with no contrary verdict is PROVED. A
"nonzero" verdict or a disagreement is never taken on its own and never
averaged: outside their decidable fragments both engines refute true
identities (symbolic exponents such as `x*x**(l-1) = x**l`, normal forms
that overflowed), so the residual is probed. If the probe confirms, the
item FAILS; if the probe vanishes, the item is PROBED and its evidence
starts with `FINDING` so the conflict stays visible. If no engine decides, the residual is
evaluated to 40 digits at random exact rational points, with every opaque
function value and derivative replaced by an independent rational (jet
substitution); vanishing at all points is PROBED, which the report labels as
evidence rather than proof. `prove_positive` checks a sign exactly for
numbers and by probing otherwise. `ledger_holds` is false when any item
failed or is unknown, and when probes are disallowed and any item was only
probed. Consumers refuse to emit kernels when the ledger does not hold.

The cross-check is not decorative: on the 4x4 example the native engine
once refuted the Ritz tightness identity after its rational normal form
overflowed 64-bit coefficients, while SymEngine and exact evaluation gave
zero. The native engine now leaves such residuals undecided, and it no
longer refutes residuals that contain powers with symbolic exponents.

## Examples

- `example/example_certificate_matrix.f90`: a 4x4 split with one symbolic
  coupling and a null level; `q` against dense real64 elimination; the
  bracket at perturbed trials; emitted kernels.
- `example/example_certificate_advection_diffusion.f90`: periodic
  `-kappa f'' + v f' = s` in a Fourier basis; symmetric output against the
  Fourier value, the non-symmetric output `<sin x, K^{-1} cos x>` with its
  DWR estimate and radius, per-mode indicators, `dJ/dv`, and the adjoint of
  the skew-symmetrised variable-velocity advection.

## Prior art

- Complementary variational principles for non-self-adjoint and complex
  problems: A. V. Cherkaev and L. V. Gibiansky, *Variational principles for
  complex conductivity, viscoelasticity, and similar problems in media with
  complex moduli*, J. Math. Phys. 35 (1994) 127--145.
- Dual-weighted-residual estimation: R. Becker and R. Rannacher, *An
  optimal control approach to a posteriori error estimation in finite
  element methods*, Acta Numerica 10 (2001) 1--102.
- Goal-oriented error estimation and adaptivity: J. T. Oden and
  S. Prudhomme, *Goal-oriented error estimation and adaptivity for the finite
  element method*, Comput. Math. Appl. 41 (2001) 735--756.
