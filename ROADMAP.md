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
  symbolic naming surface. The user-facing query is `zero_test`, with the
  existing `VERDICT_TRUE`, `VERDICT_FALSE`, and `VERDICT_UNKNOWN` outcomes.

### Architecture

Every public concept has one owner:

| Responsibility | Owner |
|---|---|
| expression identity, literals, functions | `fortsym_arena`, `fortsym_expr` |
| default state and short facade | `fortsym` |
| assumptions and predicates | `fortsym_assume` and `fortsym_relation` |
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
  The audit covers all 130 `use fortsym` exports, all 12 native Python facade
  exports, and all 84 `fortsym.sympy` adapter exports. It keeps the concise
  native vocabulary separate from the SymPy compatibility vocabulary and
  records the remaining canonical-name decisions for the next checklist item.
- [x] Select canonical short names for constructors, functions, predicates,
  results, statuses, and contexts. The decisions are recorded in
  [`doc/api-naming-policy.toml`](doc/api-naming-policy.toml), validated by
  [`scripts/check_api_naming_policy.py`](scripts/check_api_naming_policy.py),
  and cover every concept in the naming audit. Native names remain concise
  and consistent with their owning Fortran modules; the Python adapter keeps
  SymPy spellings as its deliberate compatibility boundary.
- [x] Remove `_expr` suffixes and other facade-only aliases where they do not
  carry useful disambiguation. The native facade now re-exports the
  `fortsym_expr` elementary-function vocabulary directly (`sin`, `exp`,
  `sqrt`, and the rest) and uses `default_arena`/`reset` without a redundant
  module prefix. Suffixes that distinguish expression constructors or typed
  constants, including `real_expr`, `real_text_expr`, `pi_expr`, `e_expr`, and
  `i_expr`, remain intentional. The generated API classification and naming
  audit were updated with the migration.
- [x] Make constructors, arithmetic operators, elementary functions,
  substitution, differentiation, simplification, expansion, equality, and
  numerical evaluation available through one easy facade. `fortsym` now
  forwards the core operations to their owning modules or the native engine,
  using the shared `engine_result_t` status and value contract.
- [x] Keep `x = "x"` and `symbols(...)` as the default-state entry points.
  The convenience test covers both the single-symbol assignment and the
  whitespace/comma-separated `symbols(...)` helper.
- [x] Make default-state reset, stale-handle behaviour, and global-state
  lifetime explicit and tested. The contract covers reset before first use,
  repeated reset, generation-protected handle invalidation after index reuse,
  and the process-local single-threaded lifetime of the default arena.
- [x] Add the same procedures to explicit-arena usage without a second syntax.
  The facade dispatches from each `expr_t` owner, and the convenience test
  exercises substitution, differentiation, simplification, expansion, and
  factorisation in an independent explicit arena.
- [x] Define the one public three-valued query API and move assertion helpers
  behind a testing-oriented module. `fortsym` exports `zero_test` and the
  shared verdict vocabulary. `fortsym_check` owns `check_zero`,
  `check_identity`, and `probe_zero`.
- [x] Add consistent result/status types instead of operation-specific status
  vocabularies. The easy facade uses `engine_result_t` for all core expression
  operations and the zero query. `%ok`, `%value`, `%verdict`, and `%message`
  provide one native contract.
- [x] Test single-threaded facade use, explicit concurrent arenas, and
  cross-arena refusal. The convenience test exercises the default state,
  interleaves operations in two independent explicit arenas, and checks the
  diagnostic plus invalid result returned for a cross-arena substitution.

## Phase 2 — expression core and exact domains

- [ ] Complete the expression hierarchy needed by the compatibility profile:
  atoms, numbers, symbols, applied functions, unevaluated operations,
  relations, Boolean objects, sets, tuples, rules, and indexed objects.
- [ ] Complete canonical hashing, ordering, equality, traversal, matching,
  replacement, and controlled evaluation.
- [ ] Complete exact integer/rational/real/complex and algebraic domains.
  - [x] Preserve arbitrary-size integer and rational construction and native
    arithmetic in the current scalar fragment. `num`, `rat`, and `exact` share
    canonical arena nodes, with SymPy differential coverage for large values
    and rational normalization.
  - [x] Retain bounded finite decimal literals as precision-bearing `NK_BIG_REAL`
    nodes without conversion through `real64`, and reject malformed or
    non-finite text at the arena boundary.
  - [x] Expose requested decimal precision in typed real and complex text
    results through `numeric_real_text_t` and `numeric_complex_text_t`, while
    keeping `numeric_precision_text` as the one generic native name.
  - [x] Add independently verified sample-set accuracy bounds to real
    operations. `fortsym_accuracy.measure_accuracy` evaluates each declared
    sample through the MPFR reference path, compares the caller-owned real64
    kernel in local ULPs, and retains maximum/RMS error, the maximizing input,
    reference and observed values, condition data when defined, and refusal
    counts. The independent test kernel adds one `spacing(x)` and therefore
    provides a known one-ULP oracle. The bound applies to the declared sample
    set, not to every real input.
  - [ ] Integrate exact complex and algebraic values into arena expressions and
    native operations.
    - [x] Store canonical FLINT `qqbar1` values as `NK_ALGEBRAIC` arena atoms,
      expose `algebraic_expr` and `%algebraic_text()`, and fold pure exact
      algebraic `+`, `*`, and integer powers in the native engine. The native
      zero query uses the FLINT component-sign oracle. Real64 evaluation and
      higher-degree SymEngine conversion retain explicit refusal semantics.
    - [x] Integrate algebraic atoms with the existing complex-domain boundary.
      `fortsym_complexdom` handles exact real, pure-imaginary, and mixed atoms
      in `re_part` and `im_part` through FLINT's exact qqbar projections, and
      delegates exact algebraic conjugation to FLINT.
    - [x] Add lossless parser/printer round trips for canonical `qqbar1`
      atoms in the native and backend text dialects. The parser keeps the
      payload opaque, then delegates validation and canonicalization to the
      FLINT-backed arena constructor.
    - [x] Convert exact Gaussian-rational algebraic atoms through the SymEngine
      boundary as exact rational `re + im*I` expressions. Higher-degree or
      otherwise non-rational atoms retain an explicit refusal.
    - [x] Project exact real algebraic atoms to finite normal binary64 literals
      for Fortran printing and kernel IR/code generation through a checked FLINT
      Arb enclosure. Non-real, subnormal, overflow, and ambiguous-rounding
      values retain explicit refusal semantics.
    - [x] Extend native simplification to collect algebraic coefficients in
      mixed sums and products, combine them with exact rational coefficients,
      reduce integer powers through the FLINT bridge, and canonicalize proved
      algebraic zero results.
    - [x] Use the FLINT component-sign oracle in native zero, one, and
      definitely-nonzero guards, including algebraic linear solve conditions.
    - [ ] Extend algebraic values through full native simplification, then
      complete the remaining complex-domain operations, higher-degree
      conversion, and complex code generation.
      - [x] Extend rectangular complex splitting to the entire `sinh` and
        `cosh` heads. The independent complex evaluator verifies the addition
        identities and the real-valued parts; branch-sensitive heads remain
        explicit refusals.
      - [x] Add rectangular `tanh` splitting with SymPy's equivalent
        denominator form, an explicit identically-zero-denominator refusal,
        independent complex evaluation, and matched cold/warm benchmark rows.
      - [x] Extend structural conjugation to `sinh`, `cosh`, and `tanh`; the
        independent complex evaluator checks all three against `conjg`.
      - [x] Add shared rectangular quotient handling for `tan`, including an
        exact pole refusal, independent complex evaluation, and matched
        benchmark rows.
      - [x] Complete structural conjugation for meromorphic tangent heads with
        per-context memoization; matched warm native workloads are about 12x
        faster than SymPy for `tan` and 11x faster for `tanh`.
      - [x] Add principal-branch `log` rectangular splitting as
        `log(sqrt(Re**2 + Im**2)) + i*Arg`, with a zero-argument refusal,
        independent complex evaluation, and matched benchmark rows.
      - [x] Add principal-branch `sqrt` rectangular splitting through the
        polar half-angle form, including exact negative-real and zero cases,
        independent complex evaluation, and matched benchmark rows.
      - [x] Expose the shared complex-domain owner through the main Fortran
        facade, C ABI v9, and SymPy adapter as `re_part`/`re`, `im_part`/`im`,
        `abs_of`/`Abs`, `conjugate`, and `arg_of`/`arg`. Unknown reality,
        unresolved branches, and `Arg` at decidable zero remain explicit
        refusals; `Abs` retains SymPy's unevaluated fallback for unknown
        reality, and repeated Python calls reuse immutable results until the
        assumption epoch changes.
      - [x] Expose the existing rectangular expansion owner through the main
        Fortran facade, C ABI v10, and SymPy adapter as
        `complex_expand`/`expand_complex`. The adapter accepts SymPy's
        `deep=True`/`False` option for the supported recursive fragment;
        unknown reality and unsupported branches remain named refusals.
      - [x] Match `expand_complex`'s defined domain-sentinel boundaries:
        `oo` and `nan` remain themselves while `zoo` becomes `nan`, with
        independent Fortran, C ABI, and SymPy differential coverage.
      - [x] Match the direct complex-domain sentinel boundaries for `re`,
        `im`, `Abs`, `arg`, and `conjugate`, including signed `-oo`; `zoo`
        and `nan` follow SymPy's defined projection results while
        `conjugate(zoo)` remains an applied head.
- [ ] Add infinities, NaN, signed zero, complex infinity, and domain-aware
  undefined results.
  - [x] Add the canonical native `oo_expr` positive-infinity sentinel. Native
    parsing and printing preserve `oo`, the Python adapter matches SymPy's
    `oo` spelling, and finite real Fortran kernel emission refuses the domain
    sentinel instead of emitting an invalid literal.
  - [x] Add canonical native `zoo_expr` and `nan_expr` sentinels. Native and
    Wolfram-dialect parsing/printing preserve complex infinity and undefined
    values, the Python adapter matches SymPy's `zoo`/`nan` spellings, and
    finite real Fortran kernel emission refuses both values.
  - [x] Preserve IEEE signed zero through `NK_REAL` construction, interning,
    native parsing/printing, Fortran emission, and the Python adapter. The
    independent IEEE checks keep `+0.0` and `-0.0` distinct; SymPy 1.14.0
    canonicalizes its own `Float(-0.0)` to `0.0`, which is recorded as an
    adapter semantic extension rather than silently treated as parity.
  - [x] Add SymPy-matched `nan` propagation for native `Add`, `Mul`, supported
    numeric unary heads, and `Pow` edge cases: `nan**0` is `1`, while a NaN
    base or exponent in every other supported power is `nan`. Unknown heads
    remain opaque instead of receiving guessed domain semantics.
  - [x] Add the finite-scalar and integer-power directed-domain fragment for
    `oo` and `zoo`: known signs produce `oo`/`-oo`, zero times either sentinel
    produces `nan`, opposite directions and mixed `oo`/`zoo` additions produce
    `nan`, and symbolic products remain structural.
  - [x] Add direct unary domain rules for `sqrt`, `Abs`, `exp`, and `log`.
    Known sentinel inputs match SymPy 1.14.0, including `sqrt(-oo) = i*oo`;
    symbolic factors remain unevaluated rather than receiving guessed rules.
  - [x] Add the safe compact-rational power fragment: positive and negative
    rational powers of `oo`/`zoo` follow SymPy, and `(-oo)` powers with an odd
    half-integer exponent produce the principal `+/-i*oo` phase. Other
    unsupported branch-sensitive rational powers remain explicit powers.
  - [x] Extend the `-oo` rational-power boundary to positive non-half
    exponents with SymPy's normalized principal phase,
    `(-oo)**(p/q) = oo*(-1)**(p/q)` up to the corresponding real sign;
    negative rational exponents remain `0`.
  - [x] Add direct domain rules for `sign`, `floor`, `ceiling`, `sinh`,
    `cosh`, and `tanh`, and expose those canonical names through the Python
    facade. Known sentinel results match SymPy 1.14.0; `sign(zoo)` remains
    unevaluated.
  - [x] Add representable inverse-head domain rules for `asin`, `acos`,
    `atan`, `asinh`, `acosh`, and `atanh`. The exact `oo`/`-oo` results match
    SymPy 1.14.0; the accumulation-bound results for `atan(zoo)` and
    `atanh(zoo)` remain explicit applied-head refusals.
  - [x] Add direct reciprocal-hyperbolic domain rules for `csch`, `sech`, and
    `coth`. Their `oo`/`-oo`/`zoo` results match the representable SymPy
    1.14.0 scalar fragment (`0`, `+/-1`, and `nan`).
  - [x] Add direct error-function domain rules for `erf` and `erfc`.
    `erf(±oo)` and `erfc(±oo)` match the scalar SymPy 1.14.0 limits, while
    `erf(zoo)` and `erfc(zoo)` remain explicit applied-head refusals.
  - [x] Add representable gamma-family domain rules for `gamma` and
    `loggamma`. Positive infinity and the `loggamma` complex-infinity cases
    match SymPy 1.14.0; pole-sensitive `gamma(-oo)` and `gamma(zoo)` remain
    explicit applied-head refusals.
  - [x] Extend the shared positive-infinity gamma-family rule to `factorial`:
    `factorial(oo)=oo`, while negative and complex-infinity inputs remain
    explicit applied-head refusals.
  - [x] Expose the existing `atan2` operation through the SymPy adapter and
    evaluate its directed `(+/-oo, +/-oo)` quadrants; complex-infinity and
    other ambiguous pairs remain explicit applied-head refusals.
  - [x] Add shared Bessel infinity rules and adapter names: `besselj(order,
    +/-oo)=0` and `besseli(order,oo)=oo`; negative-real `besseli` and
    complex-infinity cases remain explicit applied-head refusals.
  - [x] Map SymPy's `legendre(degree, argument)` to the native `legendrep`
    owner and add integer degree/order-zero infinity rules; symbolic,
    noninteger, and unsupported degree/order cases remain refusals.
  - [x] Add representable complex-infinity rules for `sin`, `cos`, and `tan`:
    each becomes `nan` like SymPy, while their `+/-oo` accumulation-bound
    results remain explicit applied-head refusals.
  - [x] Match NaN boundaries for order-bearing Bessel and Legendre heads:
    unresolved NaN arguments remain applied, while `besseli(nan, -oo)` becomes
    `nan` through the existing directed-domain rule.
  - [x] Match SymPy's high-degree Legendre infinity boundary: integer degrees
    three and higher become `nan` at `+/-oo` and `zoo`; lower representable
    degrees retain their existing parity/domain rules.
  - [x] Apply SymPy's negative-integer Legendre identity at infinity:
    `P(-n-1, x) = P(n, x)` for the supported integer boundary fragment,
    including the resulting high-degree `nan` cases.
  - [x] Match the representable negative-real Bessel-I phase: symbolic order
    returns `oo*(-1)**order`, integer order returns signed infinity, and
    unsupported exact non-integer orders remain explicit refusals.
  - [ ] Complete remaining operation-specific `oo`/`zoo` semantics for
    non-integer powers, functions, limits, assumptions, and numerical evaluation.
- [ ] Add arbitrary-precision evaluation with explicit precision and accuracy.
- [ ] Preserve exactness through every constructor, operator, and adapter.

## Phase 3 — assumptions, predicates, and safe simplification

- [ ] Implement the SymPy-compatible predicate vocabulary and three-valued
  query semantics.
  - [x] Expose native zero verdicts through the C ABI and map them to the
    SymPy adapter's `Expr.is_zero` and `Expr.is_nonzero` properties, preserving
    `True`, `False`, and `None` for proven zero, proven nonzero, and unknown.
  - [x] Expose the local real, sign, and zero facts through one C-ABI query
    and matching SymPy predicate properties, including the native implication
    closure.
  - [x] Expose the existing native integer fact consistently through the
    Fortran facade and the SymPy adapter: `integer_valued` and
    `positive_integer` remain concise native helpers, while Python supports
    `integer=True`, `Q.integer`, `ask(Q.integer(x))`, and `Expr.is_integer`.
    Exact integer and rational node kinds match SymPy's `True`/`False` results;
    unknown symbols and floats remain `None`. Differential tests and matched
    cold/warm benchmark rows use SymPy 1.14.0 as the oracle.
  - [x] Expose the existing exact rational domain through the same owner:
    native `rational_valued` and Python `rational=True`, `Q.rational`,
    `ask(Q.rational(x))`, and `Expr.is_rational`. Integer facts close over
    rational and real; exact integer/rational nodes are `True`, while floats
    and unknown symbols remain `None`. The C ABI, Fortran facade, differential
    tests, and matched performance rows share this one fact vocabulary.
  - [x] Add the scalar `Expr.is_number` predicate through one native owner.
    Numeric atoms, named constants, domain sentinels, exact algebraic atoms,
    and numeric-only compound expressions return `True`; symbols and Boolean
    relations return `False`. Fortran, the C ABI, the Python adapter, the
    independent native tests, and SymPy 1.14.0 differential cases use the
    same implementation. The immutable Python result is cached; its warm-core
    benchmark row is enforced, while the one-node cold call is recorded as a
    conversion-dominated diagnostic rather than a native algorithm claim.
  - [x] Add the exact-domain `Expr.is_algebraic` predicate through the same
    native predicate owner. Exact integer, rational, and FLINT algebraic atoms,
    `I`, exact rational powers such as `sqrt(2)`, and the native algebraic
    assumption closure return `True`; proven transcendental constants and
    supported transcendental heads return `False`, while machine reals and
    unresolved symbols remain `None`. The Fortran facade, C ABI, Python
    adapter, `algebraic=True`, differential cases, and warm benchmark row all
    use one implementation.
  - [x] Add `Q.algebraic` and `ask(Q.algebraic(x))` at the Python boundary
    without creating a second classifier. Exact values and local Q facts match
    SymPy 1.14.0; constructor-attached algebraic symbols preserve SymPy's
    `None` dispatcher result, and unsupported function heads remain undecided
    rather than being guessed from a native false result.
- [ ] Support local contexts, global convenience assumptions, scoped context
  managers in Python, and immutable explicit contexts in Fortran.
  - [x] Add nested native context push/pop with exception-safe Python
    `Q`, `ask`, and `assuming` scopes; restore the previous arena facts after
    every scope and keep foreign-arena facts refused.
  - [x] Add immutable explicit assumption contexts to the Fortran facade and
    make native transformations accept them without process-global state.
- [ ] Implement `refine`, relational facts, compound inference, and conflict
  diagnostics.
  - [x] Add native and Python `refine` for the supported Q-fact fragment by
    routing one or more supported facts through the native guarded simplifier;
    keep scopes reversible and refuse unsupported assumption forms.
  - [x] Add SymPy-compatible relational constructors and bounded lower-bound
    ingestion with explicit domain and arena validation. The supported native
    fragment accepts exact sign-implying bounds and `Equal`/`Unequal` at zero;
    bounds that do not imply a supported sign and foreign arenas refuse
    explicitly.
  - [x] Add transactional compound inference and conflict diagnostics without
    guessing from contradictory or unsupported facts. `And` facts close over
    the native sign/zero implications, and every refused compound leaves its
    parent context unchanged.
- [ ] Complete safe elementary simplification: powers, logarithms, radicals,
  trigonometry, inverse functions, exponentials, absolute values, and special
  constants.
  - [x] Use the canonical `negative`, `nonpositive`, and `zero` facts in the
    existing guarded `sqrt` and `abs` rewrites. Negative and nonpositive real
    values become `-x`; zero becomes `0`; unknown reality remains unevaluated.
  - [x] Add domain-guarded `log(exp(x))` for real `x` and `exp(log(x))` for
    nonzero `x`; unsupported domain cases remain unevaluated.
  - [x] Match the principal-square-root power convention by reducing
    `sqrt(x)**2` to `x` for symbolic `x`.
  - [x] Canonicalize universal power-constructor identities in the shared
    arena: exact exponent zero becomes `1`, exact base one becomes `1` except
    for SymPy's `oo`/`zoo`/`nan` exponent exceptions, which become `nan`, and
    principal `sqrt(x)**2` becomes `x`; branch-sensitive and undecidable power
    cases remain unevaluated.
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
  - [x] Run a bounded four-script SymPy, Mathics, and native corpus slice:
    189 bindings produced 136 agreements, 33 declared differences, 10
    oracle disagreements, and 12 oracle-missing cases, with no timeouts or
    execution errors. The complete 384-script corpus remains open.
- [ ] Run independent mathematical and numerical oracle suites for every
  algorithmic family.
- [ ] Add branch-cut, singularity, domain, precision, malformed-input, and
  resource-limit fuzzing.
- [ ] Compare cold-start, warm-cache, conversion, core-operation, memory, and
  expression-growth metrics against SymPy.
  - [x] Record a matched cold-operation diagnostic for native `sinh`/`cosh`
    splitting: 66.314 ms for 10,000 native calls versus 6.580117 s for 10,000
    SymPy 1.14.0 calls with its cache cleared before each call. The native
    cold path is therefore about 99x faster.
  - [x] Add `bench_complexdom` with explicit cold and warm scopes, cache
    correctness validation, and a context-local cache regression.
  - [x] Close the complex-domain warm-cache gap for the supported `sinh`/`cosh`
    scope: native is 22x faster than the matched warm SymPy 1.14.0
    `expand_complex` workload. Broader complex-domain workloads remain open.
  - [x] Retain one native engine and its memoization caches per C-ABI arena;
    synchronize scoped assumptions before each call so warm native workloads
    do not discard their cache state.
  - [x] Reuse a Python facade's expanded result while its assumption epoch is
    unchanged, and invalidate that result when assumptions change.
  - [x] Close the repeated `differentiate:warm_core` gap by caching one
    simplified derivative per immutable `(expression, variable)` pair in the
    expression owner. The cache preserves the raw low-level `Expr.diff` and
    C-ABI contract; the SymPy adapter now matches SymPy's repeated-derivative
    reuse and is faster in both matched cold and warm scopes.
  - [x] Run the enforced 54-workload cold/warm parity matrix with a fresh
    native C-ABI arena per workload; every declared workload, including
    differentiation, rational/integer/algebraic assumptions, and the warm
    numeric and algebraic predicate queries, is at or below the SymPy 1.14.0
    median in the recorded run. The cold predicate calls remain documented
    ABI-crossing diagnostics.
  - [x] Add the universal power-constructor case to the correctness matrix and
    record its cold ABI-crossing cost separately: the 55th row is an explicit
    diagnostic, while the original 54 workload rows remain enforced. The
    constructor result matches SymPy 1.14.0; the one-node cold boundary was
    1.53x SymPy in the recorded run and is therefore explicitly waived rather
    than presented as native-core parity.
- [ ] Require native to meet or beat SymPy on every supported consumer and
  benchmark workload before marking that workload complete.
- [ ] Keep the native Fortran build free of compiler-generated array temporaries.
  - [x] Reuse the existing linear-factor buffer in the rational-root `x`-factor
    path instead of constructing a temporary divisor array; the full `fo` gate
    now reports no array-temporary warning for this path.
- [x] Audit every public symbol for naming consistency and duplicate concepts.
  - [x] Assign every public Fortran facade export to one owning naming concept;
    string conversion and code generation remain native-only vocabularies.
- [x] Audit every module for single responsibility and dependency direction.
  - [x] Verify one module per source owner, known internal dependencies, no
    cycles, and no implementation import of the convenience facade.
  - [x] Record the current module graph and intentional orchestration edges in
    `doc/module-architecture.json`.
- [ ] Run `fo`, CMake/CTest, Python tests, corpus tests, code-generation tests,
  and CUDA/compiler tests on the supported toolchains.
  - [x] Run the full `fo` gate and the Python adapter suite.
  - [x] Configure, build, and run the GNU CMake/CTest suite: 50 tests passed;
    the optional SymPy differential CTest remained skipped in that environment.
  - [x] Run the code-generation tests, including the CUDA emitter test.
  - [x] Compile generated Fortran with `nvfortran` 26.5 and generated CUDA with
    `nvcc` 13.3 on the visible RTX 5060 Ti; broader compiler/corpus coverage
    remains open.
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
