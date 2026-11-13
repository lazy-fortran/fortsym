# Third-party software

FortSym's own source is covered by [LICENSE](LICENSE). Its linked libraries
retain their own licences and copyright notices:

| Library | Licence | Source |
|---|---|---|
| SymEngine | MIT | [SymEngine 0.14.0](https://github.com/symengine/symengine/tree/v0.14.0) |
| Yacas | LGPL-2.1-or-later | [Yacas 1.9.1](https://github.com/grzegorzmazur/yacas/tree/v1.9.1) |
| FLINT | LGPL-3.0-or-later | [FLINT 3.6.0](https://github.com/flintlib/flint/tree/v3.6.0) |
| GMP | LGPL-3.0-or-later or GPL-2.0-or-later | [GMP](https://gmplib.org/) |
| MPFR | LGPL-3.0-or-later | [MPFR](https://www.mpfr.org/) |
| MPC | LGPL-3.0-or-later | [GNU MPC](https://www.multiprecision.org/mpc/) |
| FortGen | MIT | [FortGen](https://github.com/lazy-fortran/fortgen) |
| FortPlot | MIT | [FortPlot](https://github.com/lazy-fortran/fortplot) |

Yacas was started by Ayal Pinkus and includes contributions by Serge Winitzki,
Jitse Niesen, Grzegorz Mazur and the other contributors listed in its `AUTHORS`
file. FortSym links its shared libraries and distributes its script library
without modification.

The CMake install includes these notices, FortSym's licence, SymEngine's MIT
notice, the FLINT, GMP, MPFR and MPC contributor lists, the LGPL 2.1 and LGPL 3.0
texts and the GPL 3.0 text in
`share/licenses/fortsym`. It also includes the original Yacas `COPYING` and
`AUTHORS` files and the FortGen and FortPlot MIT notices. Binary distributors
must retain the copyright notices of the actual FLINT, GMP, MPFR and MPC packages
they distribute, provide the corresponding source as required by their
licences, and preserve the ability to replace their shared libraries.

Optional SymPy and Maxima engines run as separate processes; they are not
included in a FortSym binary distribution. See [LEGAL.md](LEGAL.md) for the
dependency and source-use policy.
