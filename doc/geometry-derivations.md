# Geometry derivation ledger

This document is the executable companion to
[ROADMAP.md](../ROADMAP.md). It records the convention before an API is
implemented, gives a short derivation that can be checked independently, and
names the native owner and expected refusal boundary.

The sources are used for mathematics and notation, never as implementation
sources. The D'haeseleer--Hitchon--Callen--Shohet book is the local reference
copy and is also hosted by [KU Leuven](https://www.mech.kuleuven.be/en/tme/research/energy-systems-integration-modeling/pdf-publications/Dhaeseleer_et_al_Flux_Coordinates_and_Magnetic_Field_Structure_DEF/view).
The open papers are [Albert--Biro--Lainer](https://arxiv.org/abs/2008.13681)
and [MacKay](https://arxiv.org/abs/1912.11150); Carroll's open
[relativity notes](https://arxiv.org/abs/gr-qc/9712019) provide the
relativity vocabulary.

## Convention ledger

The fixed-3D starter profile uses coordinates u^i, an embedding x^a(u), and a
declared orientation. The dimension-aware spacetime owner uses the same index
meanings with a metric signature. sqrtg is always the positive metric factor;
the signed Jacobian and orientation are separate owners.

| Symbol | Meaning | Native owner | Weight/type |
|---|---|---|---|
| e_i = partial_i x | tangent/coordinate basis | fortsym_chart | vector with lower coordinate label |
| e^i = grad(u^i) | reciprocal basis | fortsym_chart | dual basis |
| g_ij = e_i dot e_j | metric | fortsym_metric | covariant rank-2 tensor, weight 0 |
| J = det(partial_i x^a) | signed map Jacobian | fortsym_chart | signed scalar |
| sqrtg = sqrt(abs(det(g))) | positive volume factor | fortsym_metric | scalar, weight +1 |
| A^i | vector components | fortsym_tensor | upper slot, weight 0 |
| A_i | covector components | fortsym_tensor/form | lower slot, weight 0 |
| mathcal A^i = sqrtg A^i | contravariant vector density | fortsym_tensor | upper slot, weight +1 |
| F_ij = partial_i A_j - partial_j A_i | magnetic 2-form components | fortsym_form | antisymmetric lower rank 2 |
| mathcal B^i = 1/2 epsilon^ijk F_jk | magnetic flux density | tensor/magnetic owner | upper slot, weight +1 |

The last three rows prevent a common notation collision. If a source writes
magnetic two-form components B_ij, they are a covariant antisymmetric
2-form. They are not the same object as vector components B^i, and neither is
interchangeable with mathcal B^i = sqrtg B^i without an explicit
Hodge/orientation operation.

## Derivation 1: reciprocal bases, metric, Jacobian, and sqrtg

This is the implementation order in D'haeseleer et al. Chapter 2,
Sections 2.2--2.6 (printed pages 8--38), with determinant and divergence
cross-checks revisited in Chapters 13--14 (printed pages 201--214).

For x = x(u), define:

    e_i         = partial_i x
    e^i dot e_j = delta^i_j
    g_ij        = e_i dot e_j
    g^ij        = inverse(g_ij)
    J           = det(e_1, e_2, e_3)
    sqrtg       = sqrt(abs(det(g_ij)))

For a regular Euclidean embedding, g = E^T E, hence:

    det(g) = det(E)^2 = J^2
    sqrtg  = abs(J)
    dV     = sqrtg du^1 du^2 du^3

The sign of J is not discarded: it is the orientation used by an oriented
volume form. A singular J, a singular metric, or a mismatch between metric
coordinates and a tensor owner is a named refusal.

The cylindrical fixture uses the paper's order (u^1,u^2,u^3) = (Z,R,phi)
and x = (R cos(phi), R sin(phi), Z):

    g_ij  = diag(1, 1, R^2)
    J     = R
    sqrtg = abs(R)

The regular physical branch R > 0 may simplify sqrtg to R, but the native
owner must remain branch-safe. The independent checks are
det(g)-J**2, e^i dot e_j-delta(i,j), and a direct matrix inverse.

## Derivation 2: covariant and contravariant components

The same vector is expanded in reciprocal bases:

    A = A^i e_i = A_i e^i
    A_i = A dot e_i = g_ij A^j
    A^i = g^ij A_j

Thus the upper/lower spelling is component metadata, not a second physical
field. lower(A,g) and raise(A,g) must be inverse views when the metric owner is
valid, preserve density weight, and refuse a wrong chart, metric arena, or
incompatible signature.

For a passive coordinate change u' = u'(u), let

    K^i_j = partial(u'^i)/partial(u^j)

A weight-w tensor density transforms as an ordinary tensor in every slot
multiplied by abs(det(K))**(-w). For a vector density:

    mathcal A'^i = abs(det(K))**(-1) K^i_j mathcal A^j

The absolute determinant belongs to density transformation. Orientation is
tracked separately by the signed map Jacobian and oriented top form. The
composition test must compare one direct map with two sequential maps,
including a left-handed intermediate map.

## Derivation 3: divergence and density cancellation

For an ordinary vector A^i, the Levi-Civita connection gives:

    div(A) = nabla_i A^i
           = partial_i A^i + Gamma^i_i j A^j
           = (1/sqrtg) partial_i(sqrtg A^i)

because Gamma^i_i j = partial_j(log(sqrtg)). Therefore the natural
metric-free input is the weight-+1 vector density:

    mathcal A^i = sqrtg A^i
    div(A)      = partial_i mathcal A^i / sqrtg

The Albert--Biro--Lainer convention calls the density-valued output
div_density(A) = partial_i mathcal A^i; its input/output types are recorded
in the owner so that a scalar density is not silently returned as an ordinary
scalar.

Independent checks are direct Christoffel contraction, the sqrtg formula, a
coordinate-map transformation, and a numerical probe away from a coordinate
singularity.

## Derivation 4: magnetic 2-form, vector, and density

Let A = A_i du^i be a vector-potential one-form. Then:

    F = dA
    F_ij = partial_i A_j - partial_j A_i
    beta = F
    mathcal B^i = (1/2) epsilon^ijk F_jk

Here epsilon^ijk is the contravariant Levi-Civita density of weight +1. The
contraction is the orientation-aware bridge from a covariant antisymmetric
2-form to a contravariant magnetic density. With the metric/orientation volume
form Omega, the same object is:

    beta = i_B(Omega)
    d(beta) = d(d(A)) = 0

The code must check both representations, not merely print matching strings:
form_from_tensor(F), interior(B,Omega), d(A), and the component density must
agree. A non-antisymmetric or density-weighted tensor must be refused by the
form bridge.

## Derivation 5: the Albert--Biro--Lainer Fourier branches

The paper's Section II and equations (5)--(27), (32)--(42), and (45)--(51)
are the computational bridge. For a symmetry coordinate x^3, use:

    f(x^1,x^2,x^3) = sum_n f_n(x^1,x^2) exp(i*n*x^3)

The required representations are covariant A_k, contravariant current density
mathcal J^k, and covariant reluctivity density nu_kl of weight -1. For the
cylindrical order (Z,R,phi):

    g_ij = diag(1, 1, R**2)
    sqrtg = abs(R)
    B^1_n = -i*n*A_2 / sqrtg
    B^2_n =  i*n*A_1 / sqrtg
    B^3_n = (partial_1*A_2 - partial_2*A_1) / sqrtg
    sqrtg*B^1_n = -i*n*A_2
    sqrtg*B^2_n =  i*n*A_1
    sqrtg*B^3_n =  partial_1*A_2 - partial_2*A_1

The native constitutive bridge starts from a physical Cartesian reluctivity
`hat(nu)_ab`. With `e_i^a = partial_i x^a`, the coordinate quantity used by
the Fourier owner is:

    nu_ij = e_i^a hat(nu)_ab e_j^b / sqrtg

For an isotropic scalar `nu_phys`, this is
`nu_ij = nu_phys (e_i dot e_j) / sqrtg`. `reluctivity_density` owns both
overloads: a scalar isotropic input and a full Cartesian matrix input. The
returned component matrix is tagged as a covariant rank-two density of weight
`-1` by the Python tensor facade; the Fortran component owner remains usable
directly with `fourier_constitutive`. It refuses an invalid chart or an input
expression from another arena; singular-map domains remain explicit rather
than being assigned a spurious branch. The independent cylindrical
and anisotropic checks compare the component contraction and `sqrtg` against
an independently assembled SymPy Jacobian.

For n=0, the longitudinal scalar branch is:

    -div_t(nu_bar_t grad_t A_3) = J^3

For n/=0, gauge A_3=0 and solve the transverse branch:

    curl_t(nu_33 curl_t a) + n**2 nu_bar_t a = j
    i*n div_t(nu_bar_t a) = J^3
    div_t(j) + i*n*J^3 = 0

The weak-form owner retains the branch, harmonic, test-space, boundary trace,
constitutive density, and current-compatibility metadata. Its `n=0` diffusion
block is `nubar_t`; its nonzero-mode block is the `nu33` transverse curl
coefficient plus `n**2*nubar_t`. The native residual owners implement the
strong forms directly, while the C/Python facades transport their expression
handles without recalculating the reduction. The boundary coefficients needed
after integration by parts are native owners too:

    q_i = nubar_t(i,j) partial_j A_3
    q   = nu_33 curl_t(a)

The scalar boundary contribution is `-w n_i q_i`. The edge contribution is
`-w_k s_k q`, with `s_k = -E_t(k,j)n_j`. `fourier_longitudinal_flux` and
`fourier_transverse_flux` return `q_i` and `q`.
`fourier_longitudinal_boundary_flux` contracts the former with `n_i`, and
`fourier_transverse_boundary_flux` returns `s_k q`; the corresponding
`fourier_transverse_boundary_contraction` also contracts with an edge test
pair. These helpers retain the coordinate normal convention and leave surface
measure, quadrature, mesh ownership, and the weak-form minus sign to the caller.
Density and general density transformation records, source/load records, and
finite-element basis assembly remain separate work. These formulas are
summarized from the paper, not copied from its source code.

## Derivation 6: relativity bridge

The same owner contract applies to a pseudo-Riemannian metric. A minimal
four-dimensional record contains:

    g_ab, signature, orientation, sqrt(abs(det(g)))
    Gamma^a_bc = 1/2 g^ad (partial_b g_dc + partial_c g_db - partial_d g_bc)
    R^a_bcd  = partial_c Gamma^a_db - partial_d Gamma^a_cb
               + Gamma^a_ce Gamma^e_db - Gamma^a_de Gamma^e_cb
    G_ab     = R_ab - 1/2 R g_ab

The code must state the Riemann sign convention and compare an invariant, not
only a component. The first examples are flat Minkowski, de Sitter in a
declared coordinate patch, and the weak-field Newtonian limit used for GPS:
the metric, connection, geodesic residual, curvature scalar, and expansion
parameter are all explicit records with domain assumptions. Coordinate
singularities and a signature mismatch are refusals, not simplification
warnings.

## Delivery checklist

- [x] Record the source families, notation bridge, density convention, and
  owner/refusal contract in ROADMAP.md and this ledger.
- [x] Keep the cylindrical sqrtg B^i and magnetic form derivations in native
  examples with independent zero checks.
- [x] Add a chart-derived nonorthogonal off-diagonal metric fixture with
  signed Jacobian and positive volume factor. The chart-map test independently
  checks forward/inverse and composed coordinate maps, tensor slots, and
  density transport; a toroidal/flux-specific fixture remains open.
- [x] Add the native typed B_ij/2-form-to-beta density bridge with explicit
  orientation. `b_con_form` applies the chart Hodge map and metric raise;
  `b_density_form` then adds the weight-`+1` `sqrtg` factor. The nonorthogonal
  magnetic test checks both orientations against the independently assembled
  `interior(B, volume_form)` form. The public C ABI and both Python facades
  carry the same bridge without duplicating its geometry algebra.
- [x] Complete the first n=0/n/=0 Fourier strong-residual owner and its
  Python facade. The descriptor now exposes `nubar_t` for the n=0 scalar
  diffusion block, and native/C/Python residuals cover the paper's
  longitudinal and transverse equations.
- [x] Add the two integration-by-parts boundary flux coefficients to the
  native, C, and Python owners, with independent component checks against the
  paper equations.
- [x] Add the native `reluctivity_density` scalar and Cartesian-matrix
  conversion, with covariant weight `-1` metadata in the Python/SymPy facade
  and an independent cylindrical Jacobian oracle.
- [ ] Add density transformation records, boundary-normal contractions,
  finite-element basis generation, and variational assembly.
- [x] Add executable de Sitter and GPS/Newtonian-limit derivation records.
  The native programs are CMake execution tests, and the Python/SymPy
  differential record checks the Einstein tensor, scalar curvature, wave
  operator, weak-field Christoffel coefficient, and Newtonian radial
  acceleration against independently assembled formulas.
- [ ] Run every record through native Fortran, fortsym.sympy, and the
  Wolfram input frontend without duplicating the geometry implementation.
