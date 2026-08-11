# Feature matrix

The observed consumer requirements, source-class evidence, bounded semantics,
priorities, and independent oracles are machine-readable in
`consumer-requirements.toml`; aggregate scan coverage and operation counts are
in `consumer-audit.json`. Exact upstream releases, inspected capabilities,
licenses, and adaptation boundaries are in `upstream-baselines.toml`.

Status values are `yes`, `partial`, and `no`. `Partial` means that the public
operation handles a stated fragment or that only one concrete backend exposes
it.

| Area | Native | SymEngine backend | Other backends | Required next fragment |
|---|---:|---:|---:|---|
| Hash-consed scalar expression DAG | stable structural order | conversion | conversion | persist a versioned cross-process structural digest |
| Arbitrary-precision integers and rationals | native scalar arithmetic yes | yes | yes | requested-precision real and complex evaluation |
| Real literals and real evaluation | yes, with sampled MPFR ULP bounds | yes | partial | rigorous intervals and arbitrary-precision operations |
| Complex and algebraic domains | bounded `qqbar` atoms, mixed coefficient folding, exact `Re`/`Im`, conjugation, lossless text IO, and checked real binary64 kernel projection | exact Gaussian-rational conversion; higher-degree atoms refused | partial | full expression promotion, higher-degree conversion, and complex code generation |
| Applied functions and Bessel `J` | yes | opaque Bessel head | partial | broader special-function rules |
| Parse, print, and Fortran dialect | deterministic | yes | yes | versioned serialization format |
| Structural and simultaneous substitution | yes | conversion | conversion | rule-based applied-function substitution |
| Differentiation | yes | yes | Yacas concrete method | memoization and domain-sensitive rules |
| Multivariate symbolic partials | yes | opaque | opaque | function assumptions and rewrite rules |
| JVP, VJP, gradient, and HVP | yes | verification | verification | consumer conformance corpus |
| Simplification | partial: principal-square-root powers, guarded `sqrt`/`abs`, and real/nonzero-guarded `log`/`exp` compositions | partial | partial | broader guarded elementary rewrites, polynomial normal form, and remaining warm-cache parity |
| Expansion | bounded multinomial | yes | Yacas concrete method | sparse polynomial expansion without the 100,000-term fast-path bound |
| Zero decision | partial, with C/Python three-valued `is_zero` and `is_nonzero` | partial | weak simplify test | broader predicate inference and polynomial certificates |
| Polynomial GCD and rational cancellation | no | partial, univariate | partial | multivariate domains |
| Factor, apart, and together | no | no public operation | Yacas factor only | typed engine operations |
| Assumptions and conditions | sign/zero facts, compound `And` ingestion, bounded relational sign facts, nested C/Python scopes, immutable Fortran contexts, `Q`/`ask` queries, `refine`, signed/zero guarded `sqrt` and `abs`, real/nonzero-guarded `log`/`exp` composition, three-valued predicate properties, and conflict diagnostics | no public context | no public context | broader predicate inference and returned conditions |
| Series and coefficient extraction | Taylor | C shim only | no public operation | Laurent series and singular points |
| Linear and polynomial solve | scalar linear | no public operation | Yacas concrete method | univariate polynomial roots |
| Integration | no | no generic operation | Yacas concrete method | rational integration first |
| Limits and asymptotics | no | no public operation | Yacas concrete method | Taylor limits, then Gruntz scales |
| CSE and Fortran kernel generation | yes | candidate source | candidate source | runtime-aware cost model |
| Symbolic matrices and tensors | exact dense rational systems over `expr_t` arrays | no public nodes | no | conditional symbolic pivots and broader matrix operations |

FortNum's required surface is construction, elementary functions, substitution,
differentiation products, verification, CSE, Fortran emission, stability
rewrites, and Enzyme wrappers. MHD1D additionally requires assumptions,
series coefficients, scalar linear solving, multivariate rational
simplification, and Taylor or Fourier projection.

The matrix does not claim Mathematica-wide coverage. The competitive target is
the measured FortNum and MHD1D fragment, followed by named algebraic domains.

Fortran callers start with the short `use fortsym` surface and its documented
default arena. Character assignment creates symbols and `symbols(...)` fills
scalar names. Explicit arenas remain the first-class API for concurrency,
embedding, and independent derivations. The naming rule and the no-LaTeX-name
boundary are maintained in [`fortran-api.md`](fortran-api.md).
