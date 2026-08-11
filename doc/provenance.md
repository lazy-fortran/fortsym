# Method and source provenance

Every algorithm fortsym implements, and where it came from. `COMPATIBILITY.md`
requires an entry here before compatibility or algorithm work merges; this file
is the record that requirement produces.

Two distinct things are tracked, and conflating them is the mistake this file
exists to prevent:

- **Methods** — published mathematics, reimplemented from the paper. Citing a
  paper creates no licence obligation. This is the normal case.
- **Reused source** — code adapted from another project. Carries that project's
  licence, and needs the original file and revision recorded.

## Reused source

| What | From | Licence | Where | Notes |
|---|---|---|---|---|
| `struct CRCPBasic` layout | SymEngine `cwrapper.cpp` | MIT | `src/capi/fsym_shim.cpp` | Restated, not copied wholesale; `static_assert` guards the layout. See `LEGAL.md` §3. |

Nothing else. If this table grows, the licence review in `COMPATIBILITY.md` §6
applies.

## Third-party repository inventory

This is the repository-level record for compatibility and algorithm work. The
`doc/upstream-baselines.toml` records the full inspected-path and revision
details for the pinned entries. “Learned” means a public behaviour, API, or
published design was studied; “reused” means source was adapted. A repository
with no reuse remains an oracle or reference, not an implementation source.

| Repository | Licence | Revision or standing | Learned / reused |
|---|---|---|---|
| [symengine/symengine](https://github.com/symengine/symengine) | MIT | 0.14.0, `fac9314c78f2809570494017efc6603befeb4eda` | C API and expression capabilities learned; `cwrapper.cpp`’s `CRCPBasic` layout restated in `src/capi/fsym_shim.cpp`, as recorded above. |
| [sympy/sympy](https://github.com/sympy/sympy) | BSD-3-Clause | 1.14.0, `fe935ceb303891d1f8bea4c03b19fd9ec9464b02` | Public compatibility behaviour, domains, algorithm documentation, and LaTeX printer conventions learned; no source reused. |
| [grzegorzmazur/yacas](https://github.com/grzegorzmazur/yacas) | LGPL-2.1-or-later | 1.9.1, `afade5b432c9d602daefe58abfd88bd985d73e51` | Linked backend and black-box behaviour learned; no source adapted. |
| [flintlib/flint](https://github.com/flintlib/flint) | LGPL-3.0-or-later | 3.6.0, `8d5454b96761fafe4d5a9da76a369a602f500f49` | C API and arithmetic capabilities learned; fortsym has thin original wrappers only. |
| [gmp/gmp](https://gmplib.org) | LGPL-3.0-or-later / GPL-2.0-or-later | 6.3.0 dependency | Linked integer dependency; no source reused. |
| [mpfr/mpfr](https://gitlab.inria.fr/mpfr/mpfr) | LGPL-3.0-or-later | 4.2.2, `eaa57b856d814af32f97d5e5eb129fb986235323` | C API and rounding semantics learned; fortsym has thin original wrappers only. |
| [codeberg.org/ginac/ginac](https://codeberg.org/ginac/ginac) | GPL-2.0-or-later | 1.8.10, `e41da5219650da501b0a4ccda9842e6964777c9a` | Documentation and architecture comparison only; no source or transcribed algorithm reused. |
| [sympy/sympy_benchmarks](https://github.com/sympy/sympy_benchmarks) | NOASSERTION | `84973d029ecc6cc1df3e0369cb1e7c0492048ef8` | Repository metadata inspected only; no workload or source imported. |
| [Mathics3/Mathics](https://github.com/Mathics3/mathics-core) | GPL-3.0-or-later | Behavioural oracle | Wolfram-language behaviour learned through subprocess use; no source or tests reused. |
| [Mathics3/mathics-scanner](https://github.com/Mathics3/mathics-scanner) | GPL-3.0-or-later | Behavioural oracle support | Tokenisation behaviour learned through the Mathics oracle; no source reused. |
| [WolframResearch/WolframClientForPython](https://github.com/WolframResearch/WolframClientForPython) | MIT | Public serialization reference | Serialization conventions for compatibility boundaries learned; no source reused. |
| Maxima | GPL-2.0-or-later | Optional subprocess oracle | Algebraic behaviour learned from black-box comparisons; no source reused. |
| Giac/Xcas | GPL-3.0-or-later | Optional subprocess oracle | Algebraic behaviour learned from black-box comparisons; no source reused. |
| FriCAS | Modified BSD | Optional subprocess oracle | Algebraic behaviour learned from black-box comparisons; no source reused. |
| PARI/GP | GPL-2.0-or-later | Optional subprocess oracle | Number-theory behaviour learned from black-box comparisons; no source reused. |
| Singular | GPL-2.0 / GPL-3.0 | Optional subprocess oracle | Polynomial behaviour learned from black-box comparisons; no source reused. |

The optional-engine licences and upstream links are also recorded in `LEGAL.md`
§4. Wolfram products are explicitly excluded by `LEGAL.md` §5; the Wolfram
client entry above is a public Python repository, not Mathematica or a Wolfram
service.

## Methods

Reimplemented from published descriptions. No source was taken from any
implementation listed as a reference.

### Polynomial and rational algebra

| Method | Citation |
|---|---|
| Sparse modular GCD | Zippel, *Probabilistic algorithms for sparse polynomials*, EUROSAM 1979 |
| Dense modular GCD | Brown, *On Euclid's algorithm and the computation of polynomial GCDs*, JACM 18(4), 1971 |
| Heuristic GCD (GCDHEU) | Char, Geddes & Gonnet, *GCDHEU*, J. Symbolic Computation 7, 1989 |
| Sparse-modular refinements | Monagan & Wittkopf, *On the design and implementation of Brown's algorithm*, ISSAC 2000 |
| Univariate factorisation | Zassenhaus, *On Hensel factorization I*, J. Number Theory 1, 1969 |
| Factor recombination | van Hoeij, *Factoring polynomials and the knapsack problem*, J. Number Theory 95, 2002 |
| Multivariate Hensel lifting | Wang, *An improved multivariate polynomial factoring algorithm*, Math. Comp. 32, 1978 |
| Heap-based sparse arithmetic | Monagan & Pearce, ISSAC 2007 and *Sparse polynomial division using a heap*, JSC 46, 2011 |
| Gröbner bases | Faugère, *A new efficient algorithm for computing Gröbner bases (F4)*, JPAA 139, 1999 |

### Calculus

| Method | Citation |
|---|---|
| Rational integration | Bronstein, *Symbolic Integration I: Transcendental Functions*, 2nd ed., Springer 2005 |
| Logarithmic part | Lazard & Rioboo 1990; Trager 1976 |
| Elementary integration | Risch, *The problem of integration in finite terms*, Trans. AMS 139, 1969 |
| Limits | Gruntz, *On computing limits in a symbolic manipulation system*, PhD thesis, ETH Zürich, 1996 |
| Power series composition | Brent & Kung, *Fast algorithms for manipulating formal power series*, JACM 25(4), 1978 |
| Hypergeometric summation | Gosper, PNAS 75, 1978; Zeilberger, *A fast algorithm for proving terminating hypergeometric identities*, Discrete Math 80, 1990 |

### Linear algebra

| Method | Citation |
|---|---|
| Fraction-free elimination | Bareiss, *Sylvester's identity and multistep integer-preserving Gaussian elimination*, Math. Comp. 22, 1968 |
| Rational system solving | Dixon, *Exact solution of linear equations using p-adic expansions*, Numer. Math. 40, 1982 |

### Domains, numerics, simplification

| Method | Citation |
|---|---|
| Exact algebraic numbers | Johansson, *Calcium / qqbar* — FLINT documentation and ISSAC 2020 |
| Ball arithmetic | Johansson, *Arb: efficient arbitrary-precision midpoint-radius interval arithmetic*, IEEE Trans. Computers 66, 2017 |
| Special function relations | Olver et al., *NIST Digital Library of Mathematical Functions*, dlmf.nist.gov — cited per rule in source |
| Fact propagation | Nelson & Oppen, *Fast decision procedures based on congruence closure*, JACM 27(2), 1980 |
| Equality saturation | Willsey et al., *egg: Fast and extensible equality saturation*, POPL 2021 |
| Probabilistic zero testing | Schwartz, JACM 27, 1980; Zippel 1979 |

## Behavioural oracles

Systems fortsym runs and compares against. Running an oracle is ordinary use of
published software and creates no obligation; **reading its source into fortsym
does**, and none of these is a permitted implementation source.

| Oracle | Licence | Reached via | Used for |
|---|---|---|---|
| SymPy | BSD-3-Clause | subprocess | Python-path corpus oracle; the only CAS fortsym may adapt source from, with notice |
| Mathics | GPL-3.0 | subprocess | Wolfram-path corpus oracle |
| Maxima, Giac, PARI, Singular | GPL | subprocess | optional cross-checks |
| SymEngine | MIT | linked | in-process cross-check |
| Yacas | LGPL-2.1 | linked | integration and factoring where native is absent |

## Upstream dependencies

Versions, licences and linking rules are in `LEGAL.md` §3 and
`doc/upstream-baselines.toml`, which is authoritative.
