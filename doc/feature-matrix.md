# Feature matrix

The observed consumer requirements, source-class evidence, bounded semantics,
priorities, and independent oracles are machine-readable in
`consumer-requirements.toml`; aggregate scan coverage and operation counts are
in `consumer-audit.json`.

Status values are `yes`, `partial`, and `no`. `Partial` means that the public
operation handles a stated fragment or that only one concrete backend exposes
it.

| Area | Native | SymEngine backend | Other backends | Required next fragment |
|---|---:|---:|---:|---|
| Hash-consed scalar expression DAG | yes | conversion | conversion | stable semantic ordering, resized hash table |
| Signed 64-bit integers and rationals | yes | yes | yes | checked arithmetic, arbitrary precision |
| Real literals and real evaluation | yes | yes | partial | high precision and intervals |
| Complex and algebraic domains | no | conversion only | partial | exact complex and algebraic numbers |
| Applied functions and Bessel `J` | yes | opaque Bessel head | partial | broader special-function rules |
| Parse, print, and Fortran dialect | yes | yes | yes | stable serialization |
| Structural and simultaneous substitution | yes | conversion | conversion | rule-based applied-function substitution |
| Differentiation | yes | yes | advertised only | memoization and domain-sensitive rules |
| Multivariate symbolic partials | yes | opaque | opaque | function assumptions and rewrite rules |
| JVP, VJP, gradient, and HVP | yes | verification | verification | consumer conformance corpus |
| Simplification | partial | partial | partial | guarded rewrites and polynomial normal form |
| Expansion | partial | yes | advertised only | sparse polynomial expansion without order bound |
| Zero decision | partial | partial | weak simplify test | polynomial certificates and assumptions |
| Polynomial GCD and rational cancellation | no | partial, univariate | partial | multivariate domains |
| Factor, apart, and together | no | no public operation | advertised only | typed engine operations |
| Assumptions and conditions | sign facts | no public context | no public context | compound inference and returned conditions |
| Series and coefficient extraction | Taylor | C shim only | no public operation | Laurent series and singular points |
| Linear and polynomial solve | scalar linear | no public operation | advertised only | univariate polynomial roots |
| Integration | no | no generic operation | Yacas concrete method | rational integration first |
| Limits and asymptotics | no | no public operation | advertised only | Taylor limits, then Gruntz scales |
| CSE and Fortran kernel generation | yes | candidate source | candidate source | runtime-aware cost model |
| Symbolic matrices and tensors | arrays in chart tools | no public nodes | no | exact linear algebra if consumers require it |

FortNum's required surface is construction, elementary functions, substitution,
differentiation products, verification, CSE, Fortran emission, stability
rewrites, and Enzyme wrappers. MHD1D additionally requires assumptions,
series coefficients, scalar linear solving, multivariate rational
simplification, and Taylor or Fourier projection.

The matrix does not claim Mathematica-wide coverage. The competitive target is
the measured FortNum and MHD1D fragment, followed by named algebraic domains.
