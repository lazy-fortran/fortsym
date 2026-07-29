# Algorithm and benchmark provenance

This file records mathematics, inspected implementations, and benchmark inputs.
An entry distinguishes implementation derived from a publication from code
adapted under an upstream license. GPL engine source is not copied or
transcribed.

| Area | Mathematical source | Upstream code inspected | Use in fortsym |
|---|---|---|---|
| Bessel `J` derivative | NIST DLMF 10.6.1, citing Watson and Olver | none | recurrence in `fortsym_diff` |
| Three-valued zero decisions | Richardson, *Journal of Symbolic Logic* 33(4), 1968, DOI 10.2307/2271358 | SymEngine public API | `UNKNOWN` outside supported fragments |
| Hash-consed expression DAG | standard structural hashing | SymEngine public headers, MIT | original Fortran arena implementation |
| Simplification architecture | Caviness, JACM 17, 1970; Moses, CACM 14, 1971 | SymEngine and SymPy public source, MIT and BSD-3-Clause | design references, no copied code |
| Equality saturation candidate | Willsey et al., PACMPL 5, 2021, DOI 10.1145/3434304 | egg documentation, MIT | roadmap evaluation only |
| Subresultant PRS | Brown, ACM TOMS 4(3), 1978 | SymPy public source, BSD-3-Clause | planned polynomial milestone |
| Multivariate factorization | Wang, *Mathematics of Computation* 32, 1978, DOI 10.1090/S0025-5718-1978-0568284-3 | SymPy public source, BSD-3-Clause | planned polynomial milestone |
| Symbolic integration | Bronstein, *Symbolic Integration I*, 2nd edition, 2005 | SymPy public source, BSD-3-Clause | planned integration milestone |
| Limits and asymptotic scales | Gruntz, ETH thesis 11432, 1996, DOI 10.3929/ethz-a-001631582 | SymPy public source, BSD-3-Clause | planned limits milestone |
| CAS architecture taxonomy | Meurer et al., PeerJ Computer Science 3:e103, 2017, DOI 10.7717/peerj-cs.103 | SymPy public source, BSD-3-Clause | feature and benchmark classification |
| Embedded CAS architecture | Bauer, Frink, and Kreckel, JSC 33, 2002, DOI 10.1006/jsco.2001.0494 | GiNaC source not copied, GPL | architecture comparison only |
| Benchmark inputs | SymEngine benchmark directory, MIT; SymPy benchmark suite, BSD-3-Clause | official upstream repositories | planned attributed ports |

Source comments cite the exact formula or algorithm used. Any future adapted
code records the upstream revision, files, license notice, local files, and
deviations in a new row before merge.

