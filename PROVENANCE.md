# Algorithm and benchmark provenance

This file records mathematics, inspected implementations, and benchmark inputs.
An entry distinguishes implementation derived from a publication from code
adapted under an upstream license. GPL engine source is not copied or
transcribed.

The machine-readable inspection baseline is
[`doc/upstream-baselines.toml`](doc/upstream-baselines.toml). It pins exact
release revisions, licenses, inspected paths, feature notes, planned
adaptations, and independent oracles. The release set inspected on 2026-07-29
is SymEngine 0.14.0, SymPy 1.14.0, Yacas 1.9.1, FLINT 3.6.0 (including Arb,
Acb, and Calcium), and GiNaC 1.8.10. Zotero was checked first on 2026-07-29 but
yielded no directly relevant source for this milestone, so the bibliography
below was checked against the primary publications and official upstream
documentation.

| Area | Mathematical source | Upstream code inspected | Use in fortsym |
|---|---|---|---|
| Bessel `J` derivative | NIST DLMF 10.6.1, citing Watson and Olver | none | recurrence in `fortsym_diff` |
| Zero-decision limitation | Richardson, *Journal of Symbolic Logic* 33(4), 1968, DOI 10.2307/2271358 | none | scope source requiring `UNKNOWN` outside proved fragments, not an algorithm source |
| Exponential-rational zero normal form | standard rational-function normalization; original local composition | SymEngine 0.14.0 `rewrite.cpp`, `functions.cpp`, `expand.cpp`, and `polys/basic_conversions.cpp`, MIT | local fragment guard plus rewrite, normalization, denominator folding, and expanded-numerator decision in `fsym_shim.cpp` |
| Hash-consed expression DAG | standard structural hashing | SymEngine public headers, MIT | original Fortran arena implementation |
| Stable expression order | bytewise lexical and recursive structural total ordering | GiNaC 1.8.10 documentation used only to identify its internal order as unsuitable for stable serialization | original Fortran merge sort and comparator; node and name-table indices excluded |
| Arbitrary-precision exact scalar bridge | canonical rational arithmetic | FLINT 3.6.0 `fmpq` C API, LGPL-3.0-or-later | original version/shared-link checked C ABI ownership shim and Fortran value-string wrapper; no FLINT allocation escapes |
| Simplification architecture | Caviness, JACM 17, 1970; Moses, CACM 14, 1971 | SymEngine and SymPy public source, MIT and BSD-3-Clause | design references, no copied code |
| Equality saturation candidate | Willsey et al., PACMPL 5, 2021, DOI 10.1145/3434304 | egg documentation, MIT | roadmap evaluation only |
| Bounded multinomial expansion | multinomial theorem, NIST DLMF 26.4 | SymEngine 0.14.0 expansion used only as a benchmark baseline | original checked composition enumerator with reusable scratch storage and a 100,000-term cap; bound failures preserve the unexpanded power |
| Subresultant PRS | Brown, ACM TOMS 4(3), 1978, DOI 10.1145/355791.355795 | SymPy public source, BSD-3-Clause | planned polynomial milestone |
| Multivariate factorization | Wang, *Mathematics of Computation* 32, 1978, DOI 10.1090/S0025-5718-1978-0568284-3 | SymPy public source, BSD-3-Clause | planned polynomial milestone |
| Gröbner bases | Buchberger, *Bruno Buchberger's PhD thesis 1965: An algorithm for finding the basis elements of the residue class ring of a zero dimensional polynomial ideal*, JSC 41(3-4), 2006, DOI 10.1016/j.jsc.2005.09.007 | SymPy and FLINT public source, BSD-3-Clause and LGPL-3.0+ | bounded rational-domain milestone when consumer cases require it |
| Symbolic integration | Bronstein, *Symbolic Integration I*, 2nd edition, 2005 | SymPy public source, BSD-3-Clause | planned integration milestone |
| Limits and asymptotic scales | Gruntz, ETH thesis 11432, 1996, DOI 10.3929/ethz-a-001631582 | SymPy public source, BSD-3-Clause | planned limits milestone |
| CAS architecture taxonomy | Meurer et al., PeerJ Computer Science 3:e103, 2017, DOI 10.7717/peerj-cs.103 | SymPy public source, BSD-3-Clause | feature and benchmark classification |
| Embedded CAS architecture | Bauer, Frink, and Kreckel, JSC 33, 2002, DOI 10.1006/jsco.2001.0494 | GiNaC source not copied, GPL | architecture comparison only |
| Benchmark inputs | SymEngine 0.14.0 benchmark directory, MIT | pinned official source | planned attributed ports after semantic review |

The standalone `sympy_benchmarks` repository at inspected revision
`84973d029ecc6cc1df3e0369cb1e7c0492048ef8` has no detected license file or
GitHub license declaration. Its workload code is therefore not an importable
BSD corpus. It may be used only as metadata until its copyright holders publish
compatible terms. SymPy's own in-tree tests and examples remain governed by
SymPy's BSD-3-Clause license.

Source comments cite the exact formula or algorithm used. Any future adapted
code records the upstream revision, files, license notice, local files, and
deviations in a new row before merge.
