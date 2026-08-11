# fortsym roadmap

This is the release plan for a Fortran-native symbolic system with the useful
surface and semantics of SymPy, while remaining small, deterministic, and easy
to embed in Fortran programs.

The first compatibility target is the pinned SymPy 1.14.0 baseline in
`doc/upstream-baselines.toml`. A later SymPy release gets a new compatibility
profile; behaviour must not change silently underneath an existing profile.

The roadmap is deliberately broader than the current 384-file Wolfram corpus.
The corpus remains an important workload, but it is not the definition of the
whole public API.

## Governing rules

### API shape

- The canonical Fortran API is internally consistent and concise. A good
  short name is kept even when SymPy uses a longer name.
- The convenience facade and lower-level Fortran modules use the same canonical
  names. The facade adds defaults and overloads; it does not create a second
  vocabulary.
- The Python `fortsym.sympy` layer is the compatibility adapter. It uses SymPy
  names, signatures, options, return conventions, and exceptions where they
  are part of the compatibility target.
- Existing names may change freely before the first stable release. During the
  transition, an old name may be a documented deprecated alias, but every
  concept has one canonical name.
- Fortran features are preferred when they make the API simpler: generic
  interfaces, operators, optional arguments, derived-type methods, and the
  default arena. For example:

  ```fortran
  x = "x"
  y = "y"
  f = sin(x) + y
  ```

- The default facade is a simple process-local, single-threaded convenience
  mode. Explicit arenas and assumption contexts are the same API with explicit
  state for concurrency, isolation, and library embedding.
- `fortsym_check` is a test/assertion module, not the symbolic user API.
  `check_zero`, `check_identity`, and `probe_zero` must not multiply the public
  symbolic naming surface. The user-facing query will converge on one
  three-valued predicate operation corresponding to SymPy's `ask(Q.zero(...))`.

### Architecture

Every public concept has one owner:

| Responsibility | Owner |
|---|---|
| expression identity, literals, functions | `fortsym_arena`, `fortsym_expr` |
| default state and short facade | `fortsym` |
| assumptions and predicates | `fortsym_assume` |
| substitution | `fortsym_subs` |
| polynomial/rational algebra | `fortsym_poly` and focused algebra modules |
| calculus | `fortsym_diff`, series, limits, integration, transforms |
| equations and systems | solver modules |
| matrices, arrays, tensors | matrix/tensor modules |
| numerical evaluation | `fortsym_numeric` and numerical adapters |
| parsing and printing | `fortsym_parse`, `fortsym_print`, dialect modules |
| engine dispatch | `fortsym_engine`, native and external adapters |
| verification and benchmarks | verify/test/benchmark modules |
| code generation | codegen modules |

Modules may depend down this list only through declared interfaces. A new
algorithm does not introduce a second implementation of an existing concept,
and a backend adapter does not leak its representation into the public API.

### Correctness and performance

SymPy is the mandatory behavioural and performance reference for every
overlapping operation. Native code is not complete until it has:

- the same mathematical result or the same documented unevaluated/conditional
  result as the reference;
- matching branch, domain, assumption, precision, option, and error semantics;
- an end-to-end benchmark on the same input and a core-operation benchmark that
  separates conversion overhead;
- a median and tail latency no worse than SymPy for the supported workload;
- no unexplained memory or expression-size regression; and
- a named refusal when parity is not yet implemented.

SymPy is not the only correctness oracle. Independent algebraic, numerical,
compiler/run, recurrence, residual, or interval oracles remain mandatory so a
SymPy defect or shared misunderstanding cannot become a fortsym proof.

No external CAS is required by the native path. SymPy, SymEngine, Yacas,
Maxima, and Mathics remain optional differential references or explicitly
selected backends.

### Definition of done

Every checklist item requires all of the following:

- [ ] canonical Fortran API and one owning module;
- [ ] compatible Python adapter names and signatures where applicable;
- [ ] documented supported inputs, outputs, conditions, and refusals;
- [ ] independent behavioural tests;
- [ ] SymPy differential tests for exact and boundary cases;
- [ ] SymPy performance comparison on representative and adversarial inputs;
- [ ] branch, domain, singularity, precision, and resource-limit tests;
- [ ] C ABI and code-generation coverage when the operation crosses those APIs;
- [ ] `FO_TEST_TIMEOUT=60 fo`, focused tests, and affected corpus checks;
- [ ] no new array-temporary warnings or unexplained allocations; and
- [ ] documentation and benchmark baseline updated in the same change.

## Phase 0 — freeze the reference and inventory the surface

- [x] Freeze the SymPy 1.14.0 compatibility profile in
  [`doc/upstream-baselines.toml`](doc/upstream-baselines.toml#L36-L44).
- [x] Generate a deterministic inventory of public SymPy modules, functions,
  classes, declared public methods, signatures, options, exceptions, return
  annotations, documentation sections, and import failures with
  [`scripts/inventory_sympy_api.py`](scripts/inventory_sympy_api.py), recorded
  for SymPy 1.14.0 in [`doc/sympy-api-inventory.json`](doc/sympy-api-inventory.json).
  Inherited methods are resolved through the recorded class hierarchy.
- [x] Classify every inventoried symbol and every Fortran public declaration
  with the consistent multi-layer ownership policy in
  [`scripts/classify_sympy_api.py`](scripts/classify_sympy_api.py) and
  [`doc/sympy-api-classification.json`](doc/sympy-api-classification.json).
  A supported compatibility name is recorded as `python-adapter`,
  `delegated`, and `native` together; explicit or out-of-subset names are
  `refused`, while native facades and external engines retain their own
  `facade` and `external` layers.
- [x] Record semantic differences separately from implementation differences
  in [`doc/sympy-api-differences.toml`](doc/sympy-api-differences.toml), with
  [`scripts/check_sympy_api_differences.py`](scripts/check_sympy_api_differences.py)
  requiring every currently supported SymPy root name to appear in the
  semantic ledger.
- [x] Add a versioned API-diff report so new or removed SymPy names cannot pass
  unnoticed, using [`scripts/diff_sympy_api.py`](scripts/diff_sympy_api.py),
  the frozen [`doc/sympy-api-baseline-1.14.0.json`](doc/sympy-api-baseline-1.14.0.json),
  and the clean [`doc/sympy-api-diff-1.14.0.json`](doc/sympy-api-diff-1.14.0.json).
- [x] Add a common differential harness for exact results, symbolic results,
  conditions, exceptions, and unevaluated objects in
  [`test/python/test_fortsym_sympy_differential.py`](test/python/test_fortsym_sympy_differential.py),
  registered as `test_fortsym_sympy_differential` in
  [`test/CMakeLists.txt`](test/CMakeLists.txt).
- [x] Add a common benchmark harness with cold-start, warm-cache, end-to-end,
  and core-operation measurements in
  [`benchmark/harnesses/bench_sympy.py`](benchmark/harnesses/bench_sympy.py),
  with the measurement contract documented in
  [`doc/benchmarks.md`](doc/benchmarks.md).
- [x] Make the benchmark report fail when a supported native workload is slower
  than the SymPy baseline without an explicit waiver. Use
  `--enforce-parity` for the gate and `--waive operation:scope` for a recorded,
  named exception.

## Phase 1 — make the Fortran facade coherent

- [x] Audit every public export for duplicate concepts and inconsistent names
  with [`scripts/audit_api_naming.py`](scripts/audit_api_naming.py) and
  [`doc/sympy-api-naming-audit.json`](doc/sympy-api-naming-audit.json).
  The audit covers all 85 `use fortsym` exports, all 12 native Python facade
  exports, and all 41 `fortsym.sympy` adapter exports. It keeps the concise
  native vocabulary separate from the SymPy compatibility vocabulary and
  records the remaining canonical-name decisions for the next checklist item.
- [x] Select canonical short names for constructors, functions, predicates,
  results, statuses, and contexts. The decisions are recorded in
  [`doc/api-naming-policy.toml`](doc/api-naming-policy.toml), validated by
  [`scripts/check_api_naming_policy.py`](scripts/check_api_naming_policy.py),
  and cover every concept in the naming audit. Native names remain concise
  and consistent with their owning Fortran modules; the Python adapter keeps
  SymPy spellings as its deliberate compatibility boundary.
- [ ] Remove `_expr` suffixes and other facade-only aliases where they do not
  carry useful disambiguation.
- [ ] Make constructors, arithmetic operators, elementary functions,
  substitution, differentiation, simplification, expansion, equality, and
  numerical evaluation available through one easy facade.
- [ ] Keep `x = "x"` and `symbols(...)` as the default-state entry points.
- [ ] Make default-state reset, stale-handle behaviour, and global-state
  lifetime explicit and tested.
- [ ] Add the same procedures to explicit-arena usage without a second syntax.
- [ ] Define the one public three-valued query API and move assertion helpers
  behind a testing-oriented module.
- [ ] Add consistent result/status types instead of operation-specific status
  vocabularies.
- [ ] Test single-threaded facade use, explicit concurrent arenas, and
  cross-arena refusal.

## Phase 2 — expression core and exact domains

- [ ] Complete the expression hierarchy needed by the compatibility profile:
  atoms, numbers, symbols, applied functions, unevaluated operations,
  relations, Boolean objects, sets, tuples, rules, and indexed objects.
- [ ] Complete canonical hashing, ordering, equality, traversal, matching,
  replacement, and controlled evaluation.
- [ ] Complete exact integer/rational/real/complex and algebraic domains.
- [ ] Integrate algebraic values into arena expressions, printers, conversion,
  simplification, and code generation.
- [ ] Add infinities, NaN, signed zero, complex infinity, and domain-aware
  undefined results.
- [ ] Add arbitrary-precision evaluation with explicit precision and accuracy.
- [ ] Preserve exactness through every constructor, operator, and adapter.

## Phase 3 — assumptions, predicates, and safe simplification

- [ ] Implement the SymPy-compatible predicate vocabulary and three-valued
  query semantics.
- [ ] Support local contexts, global convenience assumptions, scoped context
  managers in Python, and immutable explicit contexts in Fortran.
- [ ] Implement `refine`, relational facts, compound inference, and conflict
  diagnostics.
- [ ] Complete safe elementary simplification: powers, logarithms, radicals,
  trigonometry, inverse functions, exponentials, absolute values, and special
  constants.
- [ ] Match SymPy branch conventions while preserving fortsym's refusal of
  unsafe identities.
- [ ] Implement the general simplification families: `powsimp`, `powdenest`,
  `trigsimp`, `radsimp`, `ratsimp`, `sqrtdenest`, `fu`, `combsimp`,
  `hyperexpand`, `logcombine`, `posify`, and `refine`.
- [ ] Implement structural tools such as `count_ops`, `cse`, `collect`,
  `expand`, `rewrite`, `replace`, and `match`.

## Phase 4 — polynomial and rational algebra

- [ ] Complete dense and sparse multivariate polynomial representations.
- [ ] Complete exact domains, coercions, polynomial rings, fraction fields,
  algebraic extensions, and `DomainMatrix` equivalents.
- [ ] Complete division, GCD, extended GCD, square-free factorization,
  resultants, discriminants, subresultants, and reconstruction certificates.
- [ ] Complete `factor`, `factor_list`, `cancel`, `together`, `apart`,
  `collect`, coefficient extraction, and polynomial conversion.
- [ ] Implement multivariate factorization and Groebner bases with bounded
  fallback/refusal semantics.
- [ ] Implement roots, root isolation, `RootOf`, algebraic root selection,
  and numerical-root comparison.
- [ ] Remove fixed expression-order limits where SymPy has a defined result;
  replace them with explicit resource budgets and named failure.

## Phase 5 — calculus and transforms

- [ ] Complete scalar, multivariate, implicit, indexed, matrix, tensor, and
  special-function differentiation.
- [ ] Complete Taylor, Laurent, Puiseux, formal, asymptotic, Fourier, and
  infinity series, including composition and inversion.
- [ ] Complete residues, `Order`, coefficient extraction, and sequence limits.
- [ ] Implement general finite, one-sided, directional, infinite, and complex
  limits, including Gruntz-style asymptotic ordering.
- [ ] Complete rational integration with Hermite and Lazard--Rioboo--Trager
  methods.
- [ ] Add elementary Risch and heuristic-Risch integration.
- [ ] Add Meijer-G and special-function integration where SymPy supports it.
- [ ] Complete definite, improper, multiple, and parameterized integration with
  convergence conditions.
- [ ] Implement Fourier, Laplace, inverse, sine, cosine, and related transforms.
- [ ] Implement symbolic sums and products, including telescoping,
  hypergeometric, Gosper, Zeilberger, and convergence cases.

## Phase 6 — equations and solvers

- [ ] Complete `solve`, `solveset`, real/complex solves, `linsolve`,
  `nonlinsolve`, and condition-set results.
- [ ] Complete polynomial, rational, radical, transcendental, inverse-function,
  piecewise, and relational solving.
- [ ] Add inequality reduction and compound-domain reasoning.
- [ ] Add Diophantine solving, recurrences, and elimination.
- [ ] Complete exact linear algebra and polynomial-system solving.
- [ ] Add ODE classification and solving, including higher-order, nonlinear,
  separable, systems, and boundary/initial conditions.
- [ ] Add PDE solving where included in the compatibility profile.
- [ ] Add numerical solving with residuals, convergence status, precision, and
  explicit nonconvergence.

## Phase 7 — matrices, arrays, tensors, and geometry

- [ ] Complete dense, sparse, immutable, block, symbolic, and domain matrices.
- [ ] Add determinant, inverse, rank, nullspace, decompositions, eigenvalues,
  Jordan forms, matrix functions, and matrix calculus.
- [ ] Complete N-dimensional arrays, indexed objects, reshaping, contraction,
  tensor products, and tensor canonicalization.
- [ ] Add vector calculus, differential geometry, manifolds, and geometric
  intersection/measurement operations.
- [ ] Preserve exact symbolic pivots and return conditions rather than making
  unconditional simplifications.

## Phase 8 — functions, discrete mathematics, and domains

- [ ] Complete elementary, combinatorial, distribution, gamma, Bessel, Airy,
  elliptic, hypergeometric, Meijer-G, orthogonal-polynomial, zeta, polylog,
  Mathieu, Lambert-W, and related special-function families.
- [ ] Implement defining identities, recurrences, ODEs, transformations,
  asymptotics, branch cuts, exact values, and precision-aware evaluation.
- [ ] Complete sets, Boolean logic, satisfiability, predicates, and condition
  sets.
- [ ] Complete combinatorics, permutations, groups, partitions, polyhedra,
  subsets, and enumeration.
- [ ] Complete number theory: primes, factorization, modular arithmetic,
  residues, continued fractions, Diophantine utilities, and number fields.
- [ ] Complete statistics, random variables, distributions, moments,
  expectations, and probability transformations.
- [ ] Add units and dimensional analysis if included in the chosen profile.
- [ ] Add physics domains: mechanics, quantum, optics, control, continuum
  mechanics, HEP, and hydrogen modules.
- [ ] Add Lie algebra, category-theory, holonomic, and other documented SymPy
  topic modules selected by the profile.

## Phase 9 — parsing, printing, code generation, and plotting

- [ ] Complete Python, Fortran, Mathematica, Maxima, Maple, LaTeX, MathML,
  JSON/structural, and other selected parser/printer compatibility.
- [ ] Match exact round trips for assumptions, unevaluated objects, sets,
  matrices, arrays, functions, and conditions.
- [ ] Complete C, C++, Fortran, Rust, Julia, Octave, and selected AST/codegen
  backends.
- [ ] Complete custom function bindings, declarations, loops, arrays,
  conditionals, multi-output routines, and runtime mapping.
- [ ] Complete `lambdify`-style numerical backends and vectorized evaluation.
- [ ] Complete the selected SymPy plotting API while retaining fortplot as the
  rendering owner where it is the better implementation.
- [ ] Keep the full Wolfram corpus and consumer corpus as differential suites,
  not as the only supported-language definition.

## Phase 10 — Python compatibility layer

- [ ] Make `fortsym.sympy` match the selected SymPy names, signatures, options,
  return classes, exceptions, assumptions, and unevaluated semantics.
- [ ] Ensure it never imports SymPy at runtime.
- [ ] Add compatibility tests generated from the SymPy API inventory.
- [ ] Keep Fortran-native names available through the main `fortsym` package;
  do not force Python naming choices into the Fortran facade.
- [ ] Document every intentional difference and every temporary refusal.
- [ ] Provide stable conversion between Fortran expressions, C handles, and
  Python compatibility objects.

## Phase 11 — performance, correctness, and release closure

- [ ] Run the complete SymPy differential corpus for every supported module.
- [ ] Run independent mathematical and numerical oracle suites for every
  algorithmic family.
- [ ] Add branch-cut, singularity, domain, precision, malformed-input, and
  resource-limit fuzzing.
- [ ] Compare cold-start, warm-cache, conversion, core-operation, memory, and
  expression-growth metrics against SymPy.
- [ ] Require native to meet or beat SymPy on every supported consumer and
  benchmark workload before marking that workload complete.
- [ ] Audit every public symbol for naming consistency and duplicate concepts.
- [ ] Audit every module for single responsibility and dependency direction.
- [ ] Run `fo`, CMake/CTest, Python tests, corpus tests, code-generation tests,
  and CUDA/compiler tests on the supported toolchains.
- [ ] Publish the compatibility matrix, performance baseline, refusal table,
  and migration notes.
- [ ] Replace temporary aliases with the final canonical API.
- [ ] Declare parity only when every selected SymPy-profile item is implemented,
  tested, benchmarked, documented, and either matched or explicitly excluded.

## Current status

The native Fortran engine already provides a useful exact kernel-oriented
fragment: expression DAGs, arbitrary-size integer/rational arithmetic,
substitution, differentiation, bounded expansion, factor/cancellation
candidates, series, scalar solving, assumptions, conservative zero decisions,
code generation, and many exact elementary/special-function identities.

The Python layer is intentionally a small SymPy-compatible adapter rather than
a claim of parity. The default Fortran facade is intentionally simpler than
SymPy and will remain so where its concise names and Fortran interfaces are
clearer.

Issue #11, the old standalone native-engine tracker, is superseded by this
roadmap. Its remaining work is now distributed across the phases above instead
of being tracked as one unbounded issue.

## Per-change checklist

- [ ] Audit Slopqueue jobs that are actually running and avoid duplicate work.
- [ ] Inspect open PRs, reviews, CI, and the worktree.
- [ ] Choose one smallest complete checklist item.
- [ ] Confirm its canonical Fortran name, Python SymPy name, owner module, and
  refusal boundary.
- [ ] Add independent correctness tests and SymPy differential/performance
  tests.
- [ ] Implement with no duplicate public vocabulary and no array temporaries.
- [ ] Run focused tests, the full `fo` gate, and affected corpus benchmarks.
- [ ] Update this roadmap and the compatibility matrix.
- [ ] Commit and push to `main`.
- [ ] Re-audit CI, Slopqueue, PRs, and the worktree.
