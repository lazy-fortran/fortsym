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
| manifolds, patches, charts, bases, maps | geometry toolkit modules |
| metrics, signatures, orientations, volume, epsilon | metric toolkit module |
| indexed tensor algebra and densities | tensor toolkit module |
| connections, covariant derivatives, curvature | connection toolkit module |
| exterior algebra and de Rham operations | form toolkit module |
| plasma and flux-coordinate constructions | magnetic toolkit module |
| optional toolkit discovery | explicit central registry module |
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
  The audit currently records all 215 `use fortsym` exports, 18 native Python
  facade exports, and 127 public adapter names/methods. It keeps the concise
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
  - [x] Expose the distinct free-symbol traversal through native
    `free_symbols`, C ABI `fortsym_expr_free_symbols`, and Python
    `Expr.free_symbols` without adding a second tree walk.
  - [x] Expose the existing simultaneous substitution owner through native
    `subs_many`, C ABI `fortsym_substitute_many`, and
    `fortsym.sympy.subs(..., simultaneous=True)` without adding a second
    substitution traversal.
  - [x] Match SymPy 1.14's deterministic ordering for unordered substitution
    mappings in the declared fragment: descending node count followed by the
    supported structural sort key. Explicit replacement sequences retain their
    caller order, and cascading or unsupported matching remains explicit.
  - [x] Add the SymPy `Expr.xreplace` exact-node boundary through the existing
    simultaneous substitution owner. It replaces only matched DAG nodes,
    performs normal constructor evaluation without a final `expand`, and
    refuses non-mapping inputs without adding a native alias or second walk.
  - [x] Add exact non-wildcard `Expr.match` through the existing native
    equality owner. Equal structural expressions return `{}`, unequal
    expressions return `None`; wildcard matching is tracked as a separate
    bounded adapter fragment below.
  - [x] Add exact non-wildcard `Expr.replace` through the existing exact-node
    replacement owner. `map=True` reports only changed exact matches,
    `exact=True`/`False` are accepted at the boundary, and callable or
    wildcard replacement remains an explicit refusal.
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
  - [x] Canonicalize finite gamma-family poles: exact non-positive integer
    `gamma(n)` becomes `zoo`, exact non-positive integer `loggamma(n)` becomes
    `oo`, and exact negative integer `factorial(n)` becomes `zoo`; broader
    non-integer and accumulation-bound pole cases remain explicit refusals.
  - [x] Canonicalize compact exact nonnegative factorial values through the
    existing checked `factorial_i64` owner: `factorial(0)` through
    `factorial(20)` become exact integers; arbitrary-size, overflowing, and
    non-integer inputs remain explicitly unevaluated.
  - [x] Extend exact integer factorial evaluation through order `1000` using
    the existing exact-arithmetic multiplication owner: orders `21` through
    `1000` produce arbitrary-size exact integers, while larger orders remain
    bounded refusals rather than triggering unbounded work.
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
  - [x] Match SymPy's exact logarithm singularity boundary: native
    simplification maps `log(0)` to `zoo`, and the existing sentinel
    propagation maps `exp(log(0))` to `nan`; real numeric and complex-domain
    pole evaluators continue to refuse undefined finite values.
  - [x] Match the principal-square-root power convention by reducing
    `sqrt(x)**2` to `x` for symbolic `x`.
  - [x] Canonicalize universal power-constructor identities in the shared
    arena: exact exponent zero becomes `1`, exact base one becomes `1` except
    for SymPy's `oo`/`zoo`/`nan` exponent exceptions, which become `nan`, and
    principal `sqrt(x)**2` becomes `x`; branch-sensitive and undecidable power
    cases remain unevaluated.
  - [x] Canonicalize the universal exact exponent-one identity at construction:
    `x**1` returns `x`, including domain sentinels, without broadening the
    branch-sensitive power rules.
- [ ] Match SymPy branch conventions while preserving fortsym's refusal of
  unsafe identities.
  - [x] Canonicalize exact negative real logarithms on the principal branch:
    `log(-2)` and `log(-2/3)` become `log(2) + i*pi` and
    `log(2/3) + i*pi`; non-exact and unsupported branch cases remain
    unevaluated.
  - [x] Canonicalize exact imaginary logarithms on the principal branch:
    `log(i)` becomes `i*pi/2` and `log(-i)` becomes `-i*pi/2`; unsupported
    non-exact complex branches remain unevaluated.
  - [x] Canonicalize the exact real inverse-hyperbolic poles:
    `atanh(1)` becomes `oo` and `atanh(-1)` becomes `-oo`; unsupported
    accumulation and complex-infinity cases remain refused or applied.
  - [x] Canonicalize the exact imaginary inverse-hyperbolic branch points:
    `atanh(i)` becomes `i*pi/4` and `atanh(-i)` becomes `-i*pi/4`; broader
    complex inverse branches remain unevaluated.
  - [x] Canonicalize the exact imaginary inverse-tangent branch points:
    `atan(i)` becomes `i*oo` and `atan(-i)` becomes `-i*oo`; accumulation-bound
    results at `zoo` remain unevaluated.
  - [x] Canonicalize the exact principal inverse-hyperbolic branch points:
    `acosh(0)` becomes `i*pi/2` and `acosh(-1)` becomes `i*pi`; unsupported
    negative-real branches remain unevaluated.
  - [x] Canonicalize the exact imaginary inverse-hyperbolic branch points:
    `acosh(i)` becomes `log(sqrt(2) + 1) + i*pi/2` and `acosh(-i)` becomes
    `log(sqrt(2) + 1) - i*pi/2`; broader complex branches remain unevaluated.
  - [x] Canonicalize the exact imaginary inverse-trigonometric branch points:
    `asin(i)` becomes `i*log(sqrt(2) + 1)` and `asin(-i)` becomes
    `-i*log(sqrt(2) + 1)`; broader complex branches remain unevaluated.
  - [x] Canonicalize the exact imaginary inverse-trigonometric branch points:
    `acos(i)` becomes `pi/2 - i*log(sqrt(2) + 1)` and `acos(-i)` becomes
    `pi/2 + i*log(sqrt(2) + 1)`; broader complex branches remain unevaluated.
  - [x] Canonicalize exact principal real unit-circle inverse branches:
    `asin(±1/2)`, `asin(±sqrt(2)/2)`, and `asin(±sqrt(3)/2)` map to their
    signed `pi/6`, `pi/4`, and `pi/3` angles; `acos(±1/2)`,
    `acos(±sqrt(2)/2)`, and `acos(±sqrt(3)/2)` map to the corresponding
    principal angles through `5*pi/6`.
  - [x] Canonicalize exact principal real tangent inverse branches:
    `atan(±sqrt(3))` maps to signed `pi/3`, and `atan(±1/sqrt(3))` maps to
    signed `pi/6`; other exact tangent arguments remain unevaluated.
  - [x] Canonicalize exact real inverse-hyperbolic branch points:
    `asinh(1)` becomes `log(sqrt(2) + 1)` and `asinh(-1)` becomes
    `-log(sqrt(2) + 1)`; broader real inverse-hyperbolic branches remain
    unevaluated.
  - [x] Canonicalize principal square roots of exact negative perfect-square
    rationals: `sqrt(-1)` becomes `i` and `sqrt(-4)` becomes `i*2`; irrational
    negative roots remain unevaluated and Gaussian-rational results stay in
    the structural expression vocabulary.
  - [x] Canonicalize the exact imaginary inverse-hyperbolic branch points:
    `asinh(i)` becomes `i*pi/2` and `asinh(-i)` becomes `-i*pi/2`; broader
    complex inverse branches remain unevaluated until their domain rules are
    covered.
- [ ] Implement the general simplification families: `powsimp`, `powdenest`,
  `trigsimp`, `radsimp`, `ratsimp`, `sqrtdenest`, `fu`, `combsimp`,
  `hyperexpand`, `logcombine`, `posify`, and `refine`.
- [ ] Implement structural tools such as `count_ops`, `cse`, `collect`,
  `expand`, `rewrite`, `replace`, and `match`.
    - [x] Implement non-visual `count_ops` through one native operation-count
      owner, with SymPy tree semantics and an explicit `visual=True` refusal.
    - [x] Expose the exact non-wildcard `Expr.replace` boundary through
      `xreplace`, including its changed-match `map` result and exact-option
      validation without adding a second replacement traversal.
    - [x] Add the adapter-only `Wild` vocabulary and structural wildcard
      matching for direct wildcards and fixed-shape expression slots, including
      SymPy-compatible `exclude` and `properties` filters.
    - [x] Add bounded single-Wild additive and multiplicative remainder
      matching at expression roots, including repeated-Wild scalar remainders;
      broader recursive matcher rules remain open.
    - [x] Add bounded distinct-Wild additive and multiplicative root
      partitioning with fixed direct factors and identity bindings; weighted
      coefficient solving and broader recursive matcher rules remain open. The
      bounded partitioner accepts at most three distinct direct Wild nodes.

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
  intersection/measurement operations. The first-class physics and geometry
  scope is specified in Phase 7A below.
- [ ] Preserve exact symbolic pivots and return conditions rather than making
  unconditional simplifications.

## Phase 7A — differential geometry, tensor calculus, forms, and magnetic coordinates

This toolkit is a primary product surface, not an example layered on top of
the scalar engine. It must serve the notation used by relativity, continuum
mechanics, electromagnetics, and magnetically confined plasmas while retaining
the short Fortran spelling and the SymPy-compatible Python boundary.

### Source synthesis and executable corpus

The geometry toolkit combines four views that are often taught separately:

1. D'haeseleer, Hitchon, Callen, and Shohet, *Flux Coordinates and Magnetic
   Field Structure* (Springer, 1991), supplies the physicist's coordinate-first
   language: tangent and reciprocal bases, covariant and contravariant
   components, metric coefficients, Jacobians, tensor transformations, and
   divergence. Chapters 2--3 are the notation bridge, Chapters 5--8 develop
   Clebsch, straight-field-line, Boozer, and Hamada systems, and Chapters
   13--14 make component transformations and divergence explicit. The local
   reference is the copy in the shared documents; the bibliographic record and
   publisher link are kept in [`doc/provenance.md`](doc/provenance.md).
2. Carroll's free [Lecture Notes on General Relativity](https://arxiv.org/abs/gr-qc/9712019)
   supplies the relativity progression: manifold, chart, tangent and dual
   spaces, tensor densities, volume forms, connections, geodesics, curvature,
   Bianchi identities, and Lie derivatives. Its [author page](https://preposterousuniverse.com/spacetimeandgeometry/)
   is the stable reading index.
3. MacKay's [*Differential forms for plasma physics*](https://arxiv.org/abs/1912.11150)
   supplies the coordinate-free layer: forms as alternating covariant tensors,
   contraction, pullback, `d`, Lie derivative, magnetic flux forms, helicity,
   and Maxwell equations on Riemannian or Lorentzian manifolds.
4. Albert, Bíró, and Lainer, [*2D Fourier finite element formulation for
   magnetostatics in curvilinear coordinates with a symmetry direction*](https://arxiv.org/abs/2008.13681),
   supplies the bridge back to computational plasma physics: weight-`+1`
   densities, the metric-free divergence and curl, the two-dimensional
   Levi-Civita density, constitutive reduction, Fourier harmonics, and the
   distinction between nodal and edge finite-element spaces.

The synthesis rule is: use abstract tensors and forms for definitions and
invariants, coordinate components for physicists' formulas and generated
kernels, and explicit densities only at operations whose natural codomain is a
density. Every conversion between those views is a named owner operation.
The API must never infer a metric, orientation, density weight, or index space
from a variable name such as `B`, `sqrtg`, or `nu`.

#### The physicist--mathematician bridge

The same field must be readable in both traditions without silently changing
what it means:

| Physics notation | Coordinate-free/native object | Required interpretation |
|---|---|---|
| `e_i = partial_i r` | Tangent basis of `T M` | Lower coordinate label identifies the basis direction; it is not a covector. |
| `e^i = grad(u^i)` | Reciprocal basis of `T M` | `e^i dot e_j = delta^i_j`; the upper label records the dual basis. |
| `B = B^i e_i` | Vector field | `B^i` are contravariant components and determine field-line slopes. |
| `B = B_i e^i` | Metric-dual covector `B_flat` | `B_i = g_ij B^j`; these are not a second physical field. |
| `B_i du^i` | One-form / degree-one form | The coefficients are lower components and transform covariantly without a metric. |
| `sqrtg B^i` | Weight-`+1` vector density | It is the natural divergence numerator, not an unmarked vector. |
| `Omega` | Oriented top form | `Omega = sign(J) sqrt(abs(det(g))) du^1 wedge ...`; orientation is separate from `sqrtg`. |
| `beta = i_B(Omega)` | Magnetic flux two-form | This is the metric/orientation-aware bridge from `B^i` to differential forms. |

For a flux-surface label `psi`, a magnetic field tangent to the surfaces has
`B^psi = 0`. In Boozer coordinates the covariant angular components are
surface constants, conventionally written `B_theta = I(psi)` and
`B_phi = G(psi)` (normalization and signs depend on the toroidal/poloidal
orientation). The contravariant components still carry the field-line
geometry and the Jacobian. In an axisymmetric interpretation these covariant
components encode enclosed toroidal and poloidal currents. The implementation
must therefore keep `B^i`, `B_i`, `sqrtg B^i`, `beta`, `J`, and `Omega` as
distinct typed views, with explicit transformations and no name-based
coercion.

The checked analytic fixture makes the useful constants visible: in the
covariant representation `B_psi=0`, `B_theta=I(psi)`, and
`B_phi=G(psi)`, so the angular covariant components are constant on each
flux surface. Its displayed diagonal metric is
`g_ij=diag(1,h^2,h^2)` with `sqrtg=h^2`; raising the field gives
`B^theta=I(psi)/h^2` and `B^phi=G(psi)/h^2`, which generally vary with the
angles. This distinction is part of the example's acceptance criteria.

Forms add the mathematician's invariant layer:

```text
alpha = (1/k!) alpha_i1...ik du^i1 wedge ... wedge du^ik
d(alpha)                  # metric-free exterior derivative
i_X(alpha)                # contraction with a vector field
L_X(alpha) = i_X(d(alpha)) + d(i_X(alpha))
star(alpha)               # metric, signature, and orientation required
```

Consequently `d`, wedge, pullback, contraction, and Lie derivative belong to
the form/geometry owners, while `flat`, `sharp`, Hodge star, codifferential,
and metric volume operations belong to metric-aware owners. A component result
is accepted only when its variance, density weight, signature, orientation, and
index space survive the round trip to the invariant representation.

The existing Wolfram scripts and their generated Python translations are
requirements for this work. The native Fortran implementation, the
`fortsym.sympy` adapter, and the Wolfram input frontend must execute the same
derivation records. SymPy 1.14.0 remains the behavioral and performance oracle.
Wolfram or Mathics results are additional differential evidence and a source
of input coverage. A translated script is not accepted merely because it
reproduces its own expected strings. The normalized record must retain the
source location, assumptions, index variance, density weight, orientation,
metric signature, and native owner for every result.

The Python compatibility boundary follows SymPy's indexed-tensor vocabulary
where it is useful: `TensorIndexType` maps to a native index space,
`TensorIndex` to a checked index label, `TensorHead` to a typed tensor field,
`TensorSymmetry` to a native slot-symmetry declaration, and
`TensorProduct`/`tensorcontraction`/`tensorpermute` to native tensor IR
operations. `CoordSystem`, `Differential`, `WedgeProduct`, and
`LieDerivative` remain the corresponding `fortsym.sympy` spellings for the
form layer. The Fortran facade keeps the shorter native names `index_type`,
`index`, `tensor_head`, `symmetry`, `product`, `contract`, `permute`, `d`,
`wedge`, and `lie`, with one owner and one conversion path for each concept.

The target module graph is deliberately package-like while remaining ordinary
Fortran:

```text
fortsym_expr / fortsym_arena
        |
fortsym_geom        (manifold, patch, chart, basis, map)
        |
fortsym_metric      (metric, signature, orientation, volume, epsilon)
        +--> fortsym_tensor       (indices, variance, symmetry, density)
        +--> fortsym_form         (wedge, d, pullback, i, lie, star)
        +--> fortsym_connection   (nabla, geodesic, curvature)
        +--> fortsym_magnetic     (flux and Fourier physics)
        |
fortsym_registry     (explicit optional toolkit capabilities)
        |
fortsym facade / C ABI / Python adapters
```

The registry contains procedure pointers and capability records only. The
expression arena does not depend on a physics toolkit, and a toolkit does not
reach sideways into another toolkit's storage. A central registration call is
allowed because it is easy to inspect and test in Fortran. Linker discovery,
hidden initialization, and a universal `physics_t` object are out of scope.

### Convention synthesis and derivation contracts

The geometry API has two linked representations. A chart owns coordinates and
the map into an embedding or an abstract coordinate manifold. Tensor and form
objects own components, index variance, symmetry, density weight, and chart.
The facade provides short names, while the owners provide the same operations
for explicit-arena and library code. There is one vocabulary for both paths.

The following table is the contract for the first physics profile. The index
letters `a`, `i`, and `j` are labels only. A concrete object carries its
dimension and index space, so an index is never matched by its printed name
alone.

| Object | Definition | Native meaning and invariant |
|---|---|---|
| Coordinates | `x^a = x^a(u^i)` | `chart_t`; `u^i` are coordinates and `x^a` are embedding or target coordinates. |
| Index space | `i, j, ... in V` | A dimensioned `index_type`; printed index names are labels, never identity. |
| Index label | `i` or `i` lowered | An `index` carries its index space, variance, free/dummy role, and optional name. |
| Tangent basis | `e_i = partial_i x` | A basis vector with a lower coordinate label. |
| Reciprocal basis | `e^i = grad(u^i)` | A dual basis with `e^i dot e_j = delta^i_j`. |
| Metric | `g_ij = e_i dot e_j` | A `(0,2)` tensor owned by `metric_t`; `g^ij` is its inverse. The signature is explicit. |
| Connection | `nabla_k` | A connection owned by `connection_t`; Levi-Civita is one named constructor, not an implicit global assumption. |
| Vector | `B = B^i e_i` | Upper components are contravariant. |
| Covector | `B_flat = B_i du^i` | `B_i = g_ij B^j`; `flat` and `sharp` are metric-owned conversions. |
| Form | `alpha = (1/k!) alpha_i1...ik du^i1 wedge ...` | An antisymmetric covariant tensor owned by `form_t`; `d`, pullback, contraction, and `lie` do not require a metric. |
| Volume form | `Omega = sigma sqrt(abs(det(g))) du^1 wedge ...` | An oriented top form. `sigma` is orientation and is not hidden in `sqrtg`. |
| Flux form | `beta = i_B Omega` | A two-form in three dimensions. It is the pullback-stable magnetic representation. |
| Vector density | `Bden^i = sqrtg B^i` | A contravariant density of weight `+1` in this profile. It is not an unmarked vector. |

For a passive coordinate change with `K^i_j = partial(u'^i)/partial(u^j)`,
the density convention is recorded as

```text
T'^(upper...)_(lower...) = abs(det(K))^(-w)
                         * K^(upper...) * inverse(K)_(lower...) * T

(nabla_k T)^(a1...ap)_(b1...bq)
  = partial_k T^(a1...ap)_(b1...bq)
  + sum_r Gamma^ar_(k,c) T^(a1...c...ap)_(b1...bq)
  - sum_s Gamma^c_(k,bs) T^(a1...ap)_(b1...c...bq)
  - w Gamma^c_(c,k) T^(a1...ap)_(b1...bq)
```

where `w` is the stored density weight and the displayed products stand for
the corresponding slot transformations. This convention makes
`sqrtg = sqrt(abs(det(g)))` and `sqrtg B^i` weight `+1`, so
`nabla_i(sqrtg B^i) = partial_i(sqrtg B^i)`. It matches the density
definition `U_density = sqrtg**w U` used by the Albert, Bíró, and Lainer
formulation. Orientation is a separate signed choice. A signed
`J = det(partial(x)/partial(u))` is retained when orientation matters. For a
positive-definite Euclidean metric, `sqrtg = sqrt(det(g))`; for a
pseudo-Riemannian metric the absolute determinant is mandatory. The
implementation must refuse a singular map, an incompatible index space, a
degenerate metric, or an operation that would erase sign, signature, or
density metadata.

The first-class object model is staged around these metadata owners:

- [ ] `manifold_t` stores dimension and optional boundary status. `patch_t`
  stores an open coordinate domain and its parent manifold. `chart_t` stores
  coordinate expressions, a chart map, and orientation, while a chart map
  records its source and target patches and its forward and inverse maps.
- [x] `index_type_t` stores dimension, name, and whether an index space is
  tangent, cotangent, spacetime, internal, or a user-declared compatible
  space. `index_t` stores variance and free/dummy role. The first native
  fixed-3D contraction slice requires the same index space, opposite variance,
  and matching nonempty labels.
- [x] Add the declaration-only `fortsym_domain` owner for `manifold_t` and
  `patch_t`, including dimension, parent, open-domain, boundary, and
  simply-connected metadata. The Python diffgeom names carry the same flags;
  topology is never inferred from coordinate expressions.
- [ ] `metric_t` stores a nondegenerate component tensor, signature
  `(n_plus, n_minus, n_zero)`, orientation compatibility, and its inverse.
  `metric_from_chart` is a convenience constructor, not a second metric
  representation. Euclidean and Lorentzian metrics use the same tensor and
  Hodge owners.
- [x] Add the fixed-three-dimensional metric-owner subset: explicit component
  storage, signature and orientation metadata, inverse, determinant, and
  positive `sqrtg`, with independent Euclidean and Lorentzian checks. Full
  symbolic nondegeneracy proofs remain open; structurally all-zero metrics are
  refused at construction.
- [x] Preserve an explicit coordinate tuple in chart-induced metrics and let
  `christoffel_tensor(metric_t)` derive the same connection as the chart path.
  A supplied metric without coordinates is refused for derivative-based
  connection construction rather than inferring variables from free symbols.
- [x] Apply the same owner rule to `covariant_diff` and
  `covariant_derivative`; chart and coordinate-aware metric calls share one
  slot/density component kernel and independent metric-compatibility checks.
- [x] Add the physicists' first-slot `covariant_divergence` owner and expose it
  through C ABI 44 and both Python facades. It contracts the appended lower
  derivative slot against the first contravariant slot, preserves the free-slot
  variance and density weight, and independently matches `div_density` for a
  weight-one vector density on a nonorthogonal chart.
- [x] Extend the owner rule through `riemann_tensor`, `ricci_tensor`,
  `scalar_curvature`, and `einstein_tensor`, with independent flat-chart
  contraction and curvature checks. Pseudo-Riemannian sign-convention corpus
  coverage remains open.
- [x] Add the torsion-free first-Bianchi residual
  `R^a_bcd + R^a_cdb + R^a_dbc` as a native rank-four owner, with C ABI 45,
  Python/SymPy facade access, and an independent non-flat metric check. The
  second Bianchi identity now has the corresponding rank-five derivative
  representation and independent flat/non-flat checks; arbitrary dimensions
  and supplied non-Levi-Civita connections remain open.
- [ ] `orientation_t` and the positive volume density are separate metadata.
  `volume_form(metric, orientation)` may change sign, while `sqrtg(metric)`
  never absorbs orientation. `epsilon` constructors distinguish the symbol,
  the tensor, and the tensor density.
- [x] Add positive coordinate-surface measures for chart-induced and explicit
  metrics. The native kernels compute the induced two-metric minor directly,
  without array temporaries; C ABI 53 and both Python facades keep this
  measure separate from signed `J`, positive `sqrtg`, and oriented volume.
- [x] Add the first typed tensor Lie-derivative owner. `tensor_lie_derivative`
  transports every upper and lower slot, preserves density metadata, and adds
  the explicit `+w*T*partial_i(X^i)` term for a weight-`w` tensor density.
  The Fortran `lie`/`lie_derivative` facade, C ABI 54, Python `Tensor.lie`, and
  independent scalar/vector/covector/density checks all share this owner.
- [x] `connection_t` stores the connection coefficients and its convention.
  `connection_from_chart` and `connection_from_metric` construct the
  torsion-free, metric-compatible owner; `connection_create` accepts a
  supplied affine connection with torsion or nonmetricity. Typed torsion,
  nonmetricity, covariant differentiation, and divergence share the native
  slot/density kernel, with C ABI 55, Python/SymPy access, and independent
  component oracles.
- [ ] `tensor_t` and `form_t` retain the owner metadata through all views.
  A form is an antisymmetric covariant tensor with a degree, and a density is
  a tensor with an explicit integer weight. `form_t` and `tensor_t` share
  conversion checks without sharing storage layouts.

The core identities are the derivation contracts for every implementation:

```text
e^i dot e_j = delta^i_j
g^ik g_kj = delta^i_j
B_i = g_ij B^j
grad(f) = (d(f))^sharp
L_B Omega = div(B) Omega
div(B) = 1/sqrtg * partial_i(sqrtg B^i)
beta = i_B Omega
d(d(alpha)) = 0
L_X(alpha) = i_X(d(alpha)) + d(i_X(alpha))
```

### Example derivations that become executable examples

The following derivations are the minimum teaching and regression corpus. Each
one must exist as a native Fortran example, a `fortsym.sympy` example, and a
source-normalized differential record. The expected expressions below are
mathematical contracts. The tests must derive them from the owners and compare
them with an independent component or numerical oracle.

#### Cylindrical coordinates and the magnetic density

Use the paper ordering `u = (Z, R, phi)` and the Cartesian embedding
`x = (R*cos(phi), R*sin(phi), Z)`. The tangent basis and metric are:

    e_Z   = (0, 0, 1)
    e_R   = (cos(phi), sin(phi), 0)
    e_phi = (-R*sin(phi), R*cos(phi), 0)
    g_ij  = diag(1, 1, R**2)
    J     = R
    sqrtg = abs(R)                 # sqrtg = R under R > 0
    Omega = R dZ wedge dR wedge dphi

For a covariant potential `A = A_1 dZ + A_2 dR` with Fourier dependence
`exp(i*n*phi)`, the form, vector, and density representations must agree:

    B^1_n = -i*n*A_2 / R
    B^2_n =  i*n*A_1 / R
    B^3_n = (partial_Z A_2 - partial_R A_1) / R
    B_1   = B^1_n
    B_2   = B^2_n
    B_3   = R**2 * B^3_n
    D^1_n = sqrtg*B^1_n = -i*n*A_2
    D^2_n = sqrtg*B^2_n =  i*n*A_1
    D^3_n = sqrtg*B^3_n =  partial_Z A_2 - partial_R A_1

The form check is:

    beta = i_B(Omega)
         = (partial_Z A_2 - partial_R A_1) dZ wedge dR
           - i*n*A_1 dZ wedge dphi
           - i*n*A_2 dR wedge dphi
         = d(A)
    d(beta) = 0

This is the smallest example that catches a transposed reciprocal basis, a
missing `sqrtg`, a signed-density mistake, and a wrong Fourier sign.

#### Boozer flux coordinates and current-linked covariant components

Use `u = (psi, theta, phi)` on nested magnetic surfaces. The minimum
representation contract is:

```text
B^psi = 0
B_theta = I(psi),       B_phi = G(psi)
sqrtg B^theta = iota(psi),  sqrtg B^phi = 1       # chosen normalization
beta = i_B(Omega)
```

The native example must separate the representation identity from the
equilibrium problem: it may use an analytic metric fixture, but must also
provide a later chart-based path for a supplied equilibrium map. The current
analytic fixture and its independent native/Python checks cover the metric
form, angular derivatives of `B_theta` and `B_phi`, the raised field, zero
divergence, `B dot grad(psi) = 0`, `beta = i_B(Omega)`, and `d(beta) = 0`.
The chart-based supplied-equilibrium path and current/flux normalization when
selected remain the next Boozer-specific extensions.
Left- and right-handed charts are separate cases; no sign is hidden in
`sqrtg`. This contract is grounded in the Boozer sections of D'haeseleer et
al. and the local flux-coordinate notes, not in the name of a variable.

#### Density transformation and divergence

For `K^i_j = partial(u'^i)/partial(u^j)`, a weight-`+1` contravariant density
obeys:

    D'^i = abs(det(K))**(-1) K^i_j D^j
    div(B) = partial_i(D^i) / sqrtg

The native example must apply a nonorthogonal map and an orientation-reversing
map separately. The first checks the slot transformation and the second checks
that the positive density does not inherit the sign of `J`. The signed
orientation remains visible in `Omega` and in oriented top-form pullbacks.

#### Relativity baseline in spherical Minkowski coordinates

Use the Lorentzian metric with signature `(-,+,+,+)` in
`u = (t, r, theta, phi)`:

    g = diag(-1, 1, r**2, r**2*sin(theta)**2)
    sqrtg = r**2*abs(sin(theta))
    Omega = r**2*sin(theta) dt wedge dr wedge dtheta wedge dphi

On the regular patch `r > 0`, `0 < theta < pi`, derive the Christoffel symbols
from `g`, then prove:

    nabla_lambda g_mu nu = 0
    R^rho_sigma mu nu = 0
    R_mu nu = 0
    R = 0
    G_mu nu = 0

The same example must be run with the opposite Riemann sign convention and
show the documented sign change in `R^rho_sigma mu nu`, while the flatness
verdict remains unchanged. A curved conformal two-dimensional metric and a
four-dimensional cosmological metric follow after this baseline so that the
curvature tests do not rely only on a flat result.

#### Differential-form Maxwell baseline

On an oriented metric manifold, use a potential one-form `A`, field two-form
`F`, current one-form `J`, and the metric Hodge operator:

    F = d(A)
    d(F) = 0
    d(star(F)) = star(J)
    d(star(J)) = 0

The first equation is a definition and the second is the de Rham identity.
The third and fourth require a metric, signature, orientation, and constitutive
convention. The API must preserve that distinction. The three-dimensional
plasma specialization identifies `F` with the magnetic flux form `beta`; the
Lorentzian spacetime specialization uses the four-dimensional electromagnetic
two-form. The Python adapter exposes the SymPy-compatible names, while the
native form owner performs wedge, `d`, pullback, contraction, and Hodge
operations.

#### Albert--Bíro--Lainer Fourier curl-curl reduction

For `H = nu B`, `J = curl(H)`, and `A_n exp(i*n*x^3)`, the generic native
operator is the full three-dimensional reduction:

    B_n = curl_n(A_n)
    J_n = curl_n(nu B_n)

For the paper's block metric and `A_3 = 0`, the transverse block becomes:

    J^1_n = curl_t(nu_33 curl_t(A_t))_1 + n**2 nubar_t^(1,j) A_j
    J^2_n = curl_t(nu_33 curl_t(A_t))_2 + n**2 nubar_t^(2,j) A_j
    nubar_t = -E_t nu_t E_t

Here `E_t` is the two-dimensional antisymmetric density. The native owner must
retain the distinction between raw components, density components, and the
constitutive tensor. The next paper slice adds the `n = 0` longitudinal scalar
weak form, the `n != 0` transverse edge form, boundary traces, and the
compatibility condition for the current density.

The implementation keeps the following distinctions visible in names and
types. These are the Levi-Civita symbol, Levi-Civita tensor, and
Levi-Civita density. They also include the signed Jacobian and positive
`sqrtg`, plus a vector, its flat covector, its flux form, and its vector
density. Plasma literature can call the flux form or density a contravariant
representation. The API records which object is present instead of resolving
that wording by convention.

Every derivation case must specify its chart, metric signature, orientation,
assumptions, source representation, target representation, invariant checks,
and independent oracle. The minimum case set is:

- [ ] Cartesian, cylindrical, and spherical coordinates, including reciprocal
  bases, `sqrtg`, Christoffel symbols, gradient, divergence, curl, and scalar
  Laplacian.
- [ ] A nonorthogonal torus or flux chart with off-diagonal metric terms,
  including signed Jacobian, raise/lower, density transformation, and a
  round-trip coordinate change.
- [ ] A magnetic potential `A` with `beta = d(A) = i_B(Omega)`, followed by
  `d(beta) = 0`, divergence, and the component/form comparison.
- [ ] The Albert, Bíró, and Lainer Fourier reduction, with separate `n = 0`
  and `n != 0` branches, density components, constitutive transformation, and
  weak-form metadata.
- [ ] Flat and curved pseudo-Riemannian examples with an explicit signature,
  connection sign convention, geodesic residual, and curvature invariant.

Each case is implemented in the native owner first, exercised through the
Fortran facade and `fortsym.sympy`, translated from the Wolfram/Python corpus,
and checked against SymPy plus an independent determinant, component,
numerical, or residual oracle. The Python and Wolfram frontends add syntax and
conversion only. They do not maintain a second geometry implementation.

### Staged delivery order for the geometry toolkit

- [ ] **7A.0 Contract and ownership.** Freeze the notation above, implement
  `manifold_t`, `patch_t`, `index_type_t`, `index_t`, `orientation_t`, and
  `signature_t`, and publish one module graph and naming audit. The source
  synthesis and executable derivation contracts are documented here; the
  native metadata owners remain open.
- [ ] **7A.0a Physicist/mathematician notation gate.** Add executable notation
  and refusal tests for `B^i` versus `B_i`, `sqrtg B^i` versus `B^i`, `Omega`
  versus `sqrtg`, and `beta = i_B(Omega)`. Include a left-handed chart, a
  Lorentzian metric, and a form-only calculation so metric-dependent and
  metric-free operations cannot be confused.
- [ ] **7A.1 Charts and volume.** Split map/basis ownership from metric
  ownership, generalize `chart_t` to explicit dimensions, and expose
  Jacobian, reciprocal basis, metric, inverse metric, signed `J`, positive
  `sqrtg`, epsilon objects, volume forms, surface measures, and
  coordinate-change composition. The fixed-3D chart-map composition subset is
  implemented and independently checked.
- [ ] **7A.2 Components and densities.** Add scalar, vector, covector, tensor,
  and density transformations, `raise`, `lower`, `flat`, `sharp`, and
  Jacobian-weighted `div`. Gate this stage with nonorthogonal, left-handed,
  and Lorentzian charts. Correct the current signed-Jacobian divergence path
  before marking the stage complete.
- [ ] **7A.3 Indexed tensor algebra.** Add arbitrary supported rank, products,
  contractions, permutation, symmetry, traces, canonical dummy indices, and
  refusal messages for variance or index-space errors. Match SymPy's
  `TensorIndexType`, `TensorIndex`, `TensorHead`, and `TensorSymmetry` at the
  Python boundary.
- [ ] **7A.4 Connections and vector calculus.** Extend covariant derivatives to
  every slot and every density weight, then derive `grad`, `curl`, `div`, and
  `laplacian` from the same metric, volume, and connection owners. Add
  torsion, nonmetricity, Levi-Civita construction, geodesic solving, Riemann/Ricci/
  Einstein tensors, Bianchi identities, Killing tests, and named sign
  conventions.
- [ ] **7A.5 Forms and topology.** Extend degree-`k` forms, pullbacks,
  interior products, Lie derivatives, Hodge star, codifferential,
  Laplace-de Rham, Maxwell forms, gauge transformations, and patch/boundary
  metadata. Add de Rham cohomology/refusal conditions for global exactness.
- [ ] **7A.6 Physics toolkits.** Add magnetic and flux-coordinate descriptors,
  field-line and flux-surface operations, Clebsch/Boozer/Hamada identities,
  and the paper's Fourier FEM reductions without coupling them into generic
  chart, tensor, or form code. The current `b_*` and `j_fourier` subset is the
  first checked slice, not completion of the toolkit. The first executable
  nested-flux-surface example now checks `B^psi=0`, `div(B)=0`, and
  `i_B(volume)=dA`; the analytic Boozer fixture now verifies that the
  covariant angular components are flux functions, shows the metric form, and
  checks the raised-field divergence. Equilibrium construction plus
  Clebsch/Hamada descriptors and field-line labels remain open.
- [ ] **7A.7 Frontends and corpus.** Translate every supported Wolfram and
  Python corpus record to the native IR, preserve assumptions and refusal
  conditions, and expose SymPy-compatible names through one conversion path.
  Include readable Fortran and Python examples for every derivation contract.
- [ ] **7A.8 Verification and performance.** Run independent mathematical and
  numerical checks for every case, then enforce matched SymPy correctness and
  performance rows for cold construction, warm operations, conversion, and
  generated kernels. No stage is complete while a supported row is slower or
  less correct without a documented, accepted boundary condition.

### Conventions and object model

- [ ] Freeze one notation table for coordinates, bases, components, metrics,
  orientations, and densities. Coordinate components use Einstein indices:
  `A(i)` is contravariant, `A_(i)` is covariant, and a repeated upper/lower
  index is a contraction. The printed forms remain easy to read in Fortran
  and Python even when the internal representation stores explicit index
  metadata.
- [ ] Represent every tensor field with its dimension, valence `(p, q)`, index
  types, slot variance, symmetry declarations, and density weight. A density
  is never represented as an unmarked tensor with a suggestive name.
- [x] Implement the first chart-bound metadata subset: `tensor_t` records the
  three-dimensional chart arena, rank through four, every slot's upper/lower
  variance, and an explicit density weight.
- [ ] Make coordinate charts, maps, manifolds, patches, tangent/cotangent
  spaces, and orientation explicit owners. A chart is the map and its
  coordinate variables. A metric is a separate object that can be induced by
  a Euclidean embedding or supplied directly, including pseudo-Riemannian
  signatures for relativity.
- [ ] Keep the canonical Fortran vocabulary short and shared by internals and
  the facade: `chart_t`, `metric_t`, `tensor_t`, `form_t`, `basis`, `metric`,
  `jacobian`, `sqrtg`, `raise`, `lower`, `contract`, `covariant_diff`, `div`,
  `curl`, `grad`, `wedge`, `d`, `star`, and `pullback`. The Python adapter
  exposes the selected SymPy names such as `CoordSystem`, `TensorIndexType`,
  `TensorIndex`, `TensorHead`, `TensorProduct`, `WedgeProduct`, and
  `Differential`, with one conversion path to the native objects.
- [ ] Define the supported tensor-density convention, including the sign and
  absolute-value behavior of Jacobians, and test composition of two coordinate
  changes. `sqrtg` carries orientation separately from the positive metric
  volume factor. No operation may silently replace a signed Jacobian with
  `sqrt(det(g))`.
- [x] Keep geometry as independent modules with declared downward
  dependencies: `fortsym_geom` owns charts and bases, `fortsym_metric` owns
  metrics and volume forms, `fortsym_tensor` owns indexed algebra,
  `fortsym_form` owns exterior algebra, `fortsym_connection` owns covariant
  derivatives and curvature, and `fortsym_magnetic` owns flux-coordinate
  constructions. The facade only registers and forwards operations.
- [x] Add `fortsym_connection` as the independent owner of the first
  covariant-derivative and curvature subset; keep the facade as a forwarding
  layer and avoid coupling the connection module to magnetic physics.
- [x] Add the first independent `fortsym_form` owner for fixed three-dimensional
  coordinate forms. It stores antisymmetric components once and provides the
  native exterior-algebra operations without coupling the expression arena to
  a physics package.
- [ ] Translate every supported geometry derivation into native `fortsym`
  construction. The Wolfram and Python scripts are coverage inputs and
  differential tests. They are never the implementation behind a native
  result, and the native path never shells out to either frontend.
- [ ] Add a natural Fortran authoring layer for geometry: character assignment
  for coordinates and tensor labels, generic constructors for scalar/vector/
  covector/tensor fields, array constructors that preserve index metadata,
  overloaded `(*)` for metric contraction where the slots are unambiguous,
  explicit `contract` for readable Einstein sums, and derived-type methods
  for `raise`, `lower`, `diff`, `div`, `curl`, and `star`. The simple path must
  read like the mathematics while `explicit_arena` and explicit index objects
  remain available for concurrency and library code.
- [ ] Provide compact named constructors for the recurring objects:
  `coords`, `chart`, `metric`, `basis`, `vector`, `covector`, `tensor`,
  `density`, and `form`. Optional keyword arguments select variance, density
  weight, symmetry, orientation, and assumptions without introducing a
  second family of near-synonyms.
- [ ] Make index-safe operations pleasant in both modes. In the default
  facade, `g = metric(chart)`, `b = vector(chart, "B")`, `b_lower = lower(b,
  g)`, and `sqrtg = volume(chart, g)` must be sufficient for common work. In
  explicit mode, every object carries its arena and chart and cross-arena or
  cross-chart operations fail at the boundary with a useful message.
- [ ] Document and test the intended short form. It should look like this,
  with the same names available through the lower-level modules:

  ```fortran
  Z = "Z"
  R = "R"
  phi = "phi"
  u = coords(Z, R, phi)
  c = chart(u, position)
  g = metric(c)
  A = covector(c, "A")
  B = curl(c, A)
  Bup = raise(B, g)
  Bden = density(Bup, sqrtg(c))
  ```

  The convenience constructors may allocate expression handles, but native
  tensor contractions and generated kernels must not create compiler array
  temporaries.
- [ ] Add a small Einstein-notation frontend for Fortran source, limited to a
  documented grammar that lowers to the same native tensor IR. It may use
  short declarations such as `B%up(1)` and `B%down(1)` or named index maps,
  but it must remain ordinary Fortran preprocessing or library calls, with no
  custom compiler or hidden global state.
- [ ] Define a small central toolkit registry for optional geometry modules.
  Registration is explicit Fortran procedure registration, without linker
  discovery or a dependency from the core expression arena to a physics
  package. Each toolkit has a capability record, version, owner module, and
  refusal boundary so a later plugin system can replace the registry without
  turning the engine into a monolith.

### Reciprocal bases, metric, and densities

For a coordinate map `x^a = x^a(u^i)`, the toolkit must derive and verify the
following chain without asking users to manually transpose an array:

```text
e_i       = partial_i x                  tangent/covariant basis
e^i       = grad(u^i)                    reciprocal/contravariant basis
e^i dot e_j = delta^i_j
g_ij      = e_i dot e_j
g^ij      = inverse(g_ij)
J         = det(partial_i x^a)           signed orientation Jacobian
sqrtg     = sqrt(det(g_ij))              positive metric volume factor
```

- [ ] Generalize the existing `fortsym_chart` implementation beyond a fixed
  three-dimensional Euclidean embedding while preserving its simple chart
  constructor and explicit-arena form.
- [x] Expose the current fixed-three-dimensional chart owner through the C ABI
  and Python `Chart` facade for signed `jacobian`, covariant basis, and
  reciprocal basis. Basis transport preserves component-first ordering and
  is checked against an independent SymPy matrix oracle.
- [x] Expose metric-owned `raise` and `lower` for chart-bound tensor slots
  through the C ABI and Python `Tensor`, preserving density weight and
  checking the round trip against an independent metric matrix.
- [x] Expose native density metadata changes and concise `Chart.vector` and
  `Chart.covector` constructors. A density conversion preserves components,
  variance, and chart ownership while changing only its explicit weight.
- [x] Add an oriented metric `volume_form` and `Chart.volume(orientation)`
  owner. The sign is explicit and independent from positive `sqrtg`, with
  native and SymPy-backed checks for both orientations.
- [x] Expose the chart-owned `grad`, `divergence`, `curl`, and `laplacian`
  operators through the C ABI and Python `Chart`, keeping curl's covector
  input and contravariant output convention explicit.
- [x] Expose the metric-free weight-`+1` density operators `curl_density` and
  `div_density` through the same native, C, and Python owners. Their tests use
  independently constructed symbolic alternating and componentwise
  derivatives; ordinary `curl` and `divergence` retain their vector semantics.
- [x] Add `volume_form(metric_t, orientation)` alongside the chart overload.
  The metric owner supplies positive `sqrtg`, while its stored orientation or
  an explicit override supplies the sign; native checks compare both paths.
- [x] Add the fixed-three-dimensional metric volume owner: `volume_density`
  returns positive `sqrt(abs(det(g)))`, while `levi_civita_symbol` and
  `metric_levi_civita(metric, variance)` keep raw symbol, oriented covariant
  tensor, and oriented contravariant tensor conventions explicit. Native, C,
  Python, and SymPy differential checks cover Euclidean and Lorentzian
  signatures; general-dimensional and separate density-tensor constructors
  remain open.
- [x] Package magnetic `B^i`, `B_i`, and `sqrtg B^i` in one typed
  `magnetic_field_t` view. The established component operators remain the
  single derivation owner; the wrapper only adds variance and density metadata.
- [x] Add the independent `fortsym_chart_map` owner for bidirectional
  coordinate transitions. It transforms scalar, vector, covector, mixed
  tensor, density, and differential-form components into target coordinates
  through explicit forward and inverse maps, with native and SymPy oracles.
- [ ] Add reciprocal bases, inverse coordinate maps when available, signed
  Jacobians, `sqrtg`, volume forms, surface measures, and metric signature
  checks. Singular maps and incompatible dimensions become named refusals.
- [ ] Add exact identities for `g^ik g_kj = delta^i_j`, `det(g) = J**2` in a
  positive Euclidean chart, `e^i dot e_j = delta^i_j`, and the chain rule for
  two composed charts. Include nonorthogonal toroidal, cylindrical, and
  spherical charts so a diagonal metric cannot hide an index error.
- [x] Add the fixed-three-dimensional `chart_map_t` transition subset for
  scalar, vector, covector, mixed tensor, and tensor-density components.
  Forward and inverse coordinate maps are explicit, and results are returned
  in target coordinates.
- [ ] Generalize coordinate transformations across supported dimensions and
      chart-map validation. The fixed-three-dimensional composition subset and
      identically-singular-Jacobian refusal are complete; generalize dimensions
      and verify round trips against independently constructed Jacobian matrices.
- [x] Add native composition of two fixed-three-dimensional chart maps and
  prove tensor and form transport composition through the Fortran facade and
  Python/SymPy boundary.
- [x] Add metric-owner overloads of `raise`, `lower`,
  `metric_covariant_tensor`, and `metric_contravariant_tensor`. They preserve
  the underlying geometric object, update slot variance, and reject a
  cross-arena or invalid metric; symbolic determinant proofs remain open.
- [x] Implement the first nonorthogonal-chart `raise`/`lower` subset. It uses
  the chart metric and inverse metric, preserves density weight and untouched
  slots, and refuses cross-arena or wrong-variance inputs.

### Tensor algebra and covariant calculus

- [ ] Implement indexed tensor construction, tensor products, Einstein
  contraction, permutation, symmetrization, antisymmetrization, trace,
  component extraction, and canonical dummy-index renaming. All contractions
  are checked for variance and index-space compatibility before expression
  construction.
- [x] Implement the first fixed-rank subset: component construction and
  extraction, tensor products, opposite-variance contraction, and trace, with
  independent nonorthogonal-chart checks. Components use a documented
  first-slot-fastest ordering.
- [x] Add fixed-rank slot permutation plus two-slot symmetrization and
  antisymmetrization through the native owner, C ABI, Python facade, and
  independent Fortran/SymPy checks. Named index spaces, arbitrary-rank
  symmetry declarations, and dummy-index canonicalization remain open.
- [x] Add the first value-semantic named-index slice: native `index_type_t` /
  `index_t` spaces, variance and dummy metadata, compatible-label checks, and
  an overloaded native `contract`. Expose the same checked operation through
  C ABI 41, `fortsym.IndexType`/`Index`, and the SymPy spelling aliases
  `TensorIndexType`/`TensorIndex`. Native slots are one-based; Python slots
  are zero-based. Arbitrary dimensions, index declarations independent of a
  fixed chart, canonical dummy renaming, and `TensorHead` remain open.
- [x] Route tensor products through the native owner at C ABI 42 and the
  Python/SymPy facade. `Tensor.product`/`tensor_product` now preserves slot
  order and summed density weight, and `diffgeom.TensorProduct` no longer
  reconstructs components in Python.
- [x] Add native-backed closedness verdicts at C ABI 43 and both Python
  facades. Form closedness is defined as a zero `d(form)` coefficient vector,
  with `None` preserved for undecidable symbolic coefficients.
- [x] Align tensor facade spellings: native/Python `product`, `tensor_product`,
  `contract`, and `trace` share one owner, while `fortsym.sympy` adds
  `tensorproduct` and `tensorcontraction` adapters without duplicating tensor
  arithmetic.
- [ ] Implement covariant differentiation of arbitrary tensor valence,
  including the correct Christoffel term for every upper and lower slot. The
  existing `christoffel` operation becomes a special case of the connection
  owner and remains available through the facade.
- [x] Implement the first fixed-three-dimensional subset for typed tensors of
  rank at most three: `covariant_diff`/`covariant_derivative` appends a lower
  derivative slot, applies every slot's Christoffel term, and honors density
  weight. Metric compatibility and an independent nonlinear nonorthogonal
  chart check are required gates.
- [x] Implement the first fixed-three-dimensional torsion/nonmetricity owner
  for supplied affine connections, including independent covariant derivative
  and divergence checks.
- [ ] Add geodesic solving, named Riemann-sign conventions, and a
  pseudo-Riemannian sign corpus without coupling connections to magnetic
  physics.
- [x] Implement the first fixed-three-dimensional curvature views:
  Christoffel, Riemann, Ricci, scalar curvature, and Einstein tensors, with a
  named Riemann convention and independent flat-chart identities. Full
  pseudo-Riemannian and relativity parity remains open.
- [x] Implement the fixed-three-dimensional second differential Bianchi
  residual as a typed rank-five tensor, with C ABI 46 and Python/SymPy access.
  Flat, nonorthogonal, and genuinely curved metric cases are checked against
  SymEngine's independent zero oracle; arbitrary dimensions and non-Levi-Civita
  connections remain open.
- [x] Add fixed-three-dimensional chart and coordinate-aware metric geodesic
  residuals through the native owner, C ABI 47, and the Python chart facade.
  Cylindrical coordinates check the nonzero radial Christoffel term against
  an independent SymEngine oracle; solving, variational mechanics, and
  arbitrary-dimensional transport remain open.
- [x] Add the first coordinate-aware metric vector-calculus owner: contravariant
  `metric_grad`, metric `metric_divergence`, and metric `metric_laplacian`,
  transported through the generic Fortran facade, C ABI 48, and Python/SymPy
  `Metric`. Independent native and SymPy checks cover Euclidean and
  pseudo-Riemannian signatures; arbitrary dimensions, curl/forms derivation,
  and full connection reuse remain open.
- [x] Extend the dimension-aware relativity owner with metric `flat`/`sharp`,
  contravariant gradient, divergence, and Laplace--Beltrami/wave operator,
  transported through the Fortran facade, C ABI 51, and Python
  `SpacetimeMetric`. Minkowski tests cover `sharp(flat(v))`, variance metadata,
  the signed gradient, divergence, and the linear-scalar wave identity;
  curved-space and arbitrary-dimensional tensor transport remain open.
- [ ] Derive vector calculus from tensor/forms primitives. `grad`, `div`,
  `curl`, and `laplacian` must share the same metric, orientation, and density
  conventions rather than maintaining separate coordinate formulas.
- [ ] Add invariant checks for tensor type, density weight, free-index shape,
  symmetry, and dimensional consistency. Refusal messages must identify the
  offending index or convention.

### Differential forms and the de Rham layer

- [ ] Implement degree-`k` forms with antisymmetric slots, scalar and function
  coefficients, `wedge`, exterior derivative `d`, pullback, interior product,
  Lie derivative, and exact/closed checks.
- [x] Implement the first fixed-three-dimensional subset: `form_t`, scalar,
  one-, two-, and three-form constructors, `wedge`, `d`, metric `star`,
  interior product, Cartan `lie`, and metric `flat`/`sharp`.
- [x] Add the first dimension-aware four-dimensional spacetime form owner:
  degree-zero through degree-four constructors, antisymmetric `wedge`,
  coordinate-aware `d`, Lorentzian/Riemannian metric `star`, and the native
  `codifferential` path. The owner proves `d(d(A)) = 0`, graded antisymmetry,
  and the signature-dependent Hodge involution on the Minkowski baseline.
  The current ABI and both Python facades transport `d`, `wedge`, and `star`;
  pullback, arbitrary dimension, and topology remain open.
- [x] Transport the native spacetime codifferential, contraction, Cartan Lie
  derivative, and Laplace--de Rham composition through the current ABI and
  Python facades. Their Minkowski identities are checked against direct signed
  divergence and wave-operator formulas; arbitrary-dimensional forms and
  source equations remain open.
- [ ] Generalize codifferential, Laplace-de Rham, and the conversion between
  vectors and one-forms using `flat` and `sharp` beyond the implemented
  fixed-three-dimensional and dimension-aware spacetime owners. Higher-degree
  arbitrary-dimensional forms remain open.
- [ ] Prove and test the structural identities `d(d(alpha)) = 0`, the graded
  Leibniz rule, pullback composition, Cartan's identity, and the appropriate
  signed `star(star(alpha))` rule in each supported signature.
- [x] Prove the first coordinate-form identities independently on a
  nonorthogonal chart: `d(d(alpha)) = 0`, graded Leibniz, Cartan's identity,
  Euclidean-signature Hodge involution, and `sharp(flat(v)) = v`.
- [x] Add fixed-three-dimensional chart-map form transport for degrees zero
  through three, including signed top-form Jacobians and independent native
  and SymPy coframe checks; preserve the degree-four zero extension returned
  by `d` of a three-form across the C/Python boundary.
- [ ] Add a Maxwell and plasma vocabulary built from forms: vector potential
  one-form `A`, magnetic flux two-form `beta = i_B(volume)`, `d(beta) = 0`,
  `beta = d(A)` on a declared simply connected patch, electric field form,
  Faraday law, and gauge transformations. Vector-calculus spellings remain
  derived convenience views.
- [x] Add the first four-dimensional Maxwell-form owner: `F = d(A)`, the gauge
  view `A -> A + d(chi)`, and the source residual `d(star(F)) - J` for a
  degree-three current form. Native, C, Python, and SymPy differential checks
  prove gauge invariance and a constructed zero residual; patch topology,
  boundary conditions, constitutive media, and global exactness remain open.
- [x] Add the first magnetic-form identity: `beta = i_B(volume) = d(A)` and
  `d(beta) = 0` for the native chart/magnetic owners. Keep the full Maxwell,
  gauge, and topology vocabulary open until the form domain carries patch
  and boundary metadata.
- [x] Add native-backed `Form.is_closed` and `SpacetimeForm.is_closed` with
  the shared three-valued zero-verdict contract. Closedness is computed from
  the native exterior derivative and remains `None` when any coefficient is
  undecidable; exactness, patch topology, and global boundary metadata remain
  open.
- [ ] Add Python compatibility for the selected `sympy.diffgeom` and
  `sympy.tensor` operations, while keeping the native form algebra independent
  of SymPy at runtime. Add Wolfram translations for the corresponding
  `ExteriorDerivative`, wedge, tensor-product, and contraction records where
  the corpus uses them.
- [x] Expose the first fixed-three-dimensional native form subset through the
  C ABI and both Python facades: form construction, `d`, wedge, `star`,
  interior, Lie, `flat`, and `sharp`. Python transports native handles and
  leaves broader `diffgeom`/form parity open.
- [x] Add the first native-backed SymPy `diffgeom` naming bridge:
  `Manifold`, `Patch`, `CoordSystem`, coordinate fields, `Differential`,
  `WedgeProduct`, `TensorProduct`, and `LieDerivative`. Compare contractions
  and Lie derivatives against SymPy's independent diffgeom oracle; keep
  coordinate transformations, arbitrary dimensions, and full tensor parity
  open.
- [x] Expose the first fixed-three-dimensional tensor/connection subset through
  the C ABI and both Python facades: chart metrics, Christoffel/Riemann/Ricci/
  Einstein views, scalar curvature, typed variance/density metadata, and
  covariant differentiation. Python transports native handles and does not
  duplicate geometry formulas; broader `diffgeom`/tensor parity remains open.
- [x] Add explicit metric-owner Hodge transport for supplied Riemannian or
  Lorentzian component metrics, including inverse metric and positive
  `sqrt(abs(det(g)))`. Keep the chart-induced convenience path and the explicit
  metric path on one native Hodge implementation.
- [x] Add the first four-dimensional Maxwell-form transport slice: a native
  `SpacetimeMetric.one_form()` potential can produce `F = d(A)`, and the
  Python facade checks `d(F) = 0`, `A wedge A = 0`, and the Lorentzian
  `star(star(F)) = -F` identity against direct component expectations. Full
  current/source equations, gauge metadata, and patch topology remain open.

### Magnetic and flux-coordinate toolkit

The plasma convention distinguishes the tangent basis `e_i` from the
reciprocal basis `e^i`. A vector has both component forms:

```text
B = B^i e_i = B_i e^i
B_i = g_ij B^j
B^i = g^ij B_j
div(B) = (1/sqrtg) partial_i(sqrtg B^i)
```

`B_i` is a covector component. `B^i` is a contravariant vector component.
`sqrtg B^i` is a contravariant vector density of weight `+1` in the selected
convention. These are separate typed objects even when a plasma code stores
only the density because the density is the quantity that differentiates
without an explicit volume factor.

- [ ] Add `magnetic_chart` and flux-surface metadata without coupling the
  generic chart or tensor modules to a particular equilibrium code.
- [x] Implement the first native magnetic views: `B_cov`, `B_con`,
  `B_density`, reciprocal bases, `sqrtg`, and the Fourier-mode curl and density
  used by `paper_magnetic`. The spelling is concise in Fortran and maps to
  clear SymPy names in Python.
- [x] Add `H_cov` and `H_con` conversions with the explicit constitutive
  convention `H_i = nu_ij B^j` and metric raising `H^i = g^ij H_j`.
- [x] Add the generic field-line derivative `B^i partial_i f` through the
  chart owner, C ABI 52, Python facade, and flux-surface identity test.
- [x] Add coordinate-surface measures as the foundation for flux-surface
  integrals and FEM boundary terms; positive measures remain distinct from
  magnetic vector densities and oriented volume forms.
- [ ] Add magnetic surfaces, flux-surface averages, and Jacobian-weighted
  divergence.
- [x] Provide metric contraction for `B**2` through the shared `metric_inner`
  owner.
- [ ] Support Clebsch, straight-field-line, Boozer, and Hamada coordinate
  descriptors as reusable data and verified identities. The analytic Boozer
  example is only the first representation fixture; add symbolic relations and
  refusal on inconsistent or singular input. Do not encode a particular
  equilibrium solver in this toolkit.
- [ ] Add the common plasma identities `B dot grad(psi) = 0`, the reciprocal
  basis relations, `B = B^i e_i = B_i e^i`, `div(B) = 0`, and the magnetic
  differential equation in both component and form notation.
- [ ] Make flux-surface and line integrals carry their measure and orientation
  explicitly. A result such as `sqrtg B^theta = f(psi)` must retain whether it
  is a density identity, a scalar identity, or an equality after averaging.
- [ ] Add transformations between physical vector components, coordinate
  components, covariant components, contravariant components, and density
  components to the Python and Wolfram frontends. Round-trip tests must use
  nonorthogonal, left-handed, and periodic coordinates.

### Albert, Bíró, and Lainer 2D Fourier FEM derivation

The `paper_magnetic` corpus is the first end-to-end geometry derivation. For
coordinates `(x^1, x^2, x^3) = (Z, R, phi)` with an axisymmetric block metric,

```text
g_ij = [[g11, g12, 0],
        [g12, g22, 0],
        [0,   0,   g33]]
sqrtg = sqrt(det(g_ij))
A_i(x^1, x^2, x^3) = A_i,n(x^1, x^2) exp(i n x^3)
```

the magnetic field of the covariant vector potential has the component
derivation

```text
B^1_n = -i n A_2,n / sqrtg
B^2_n =  i n A_1,n / sqrtg
B^3_n = (partial_1 A_2,n - partial_2 A_1,n) / sqrtg
B_1,n = g11 B^1_n + g12 B^2_n
B_2,n = g12 B^1_n + g22 B^2_n
B_3,n = g33 B^3_n
```

Thus `sqrtg B^i_n` is the natural contravariant density in the reduced
problem. The corpus also derives the antisymmetric density
`E = [[0, 1/J], [-1/J, 0]]`, the constitutive transform
`nubar = -E nut E`, the cylindrical/cartesian tensor transforms, the
zero-mode and nonzero-mode curl-curl equations, and the de Rham relations
between the two-dimensional gradient, scalar curl, and divergence.

- [ ] Import every `paper_magnetic*.wl`, `levicivita.wl`, cylindrical,
  spherical, and determinant derivation into the named geometry corpus and
  record its translated Python counterpart, source assumptions, outputs, and
  unsupported statements.
- [ ] Make the Wolfram reader and Python frontend execute those scripts through
  one normalized assignment/derivation representation. Compare native
  Fortran, `fortsym.sympy`, SymPy, and independent residual identities for
  every supported result.
- [ ] Add a native translator for the supported Wolfram/Python derivation
  subset. It must emit ordinary `fortsym` calls and operators, preserve
  assumptions, index variance, density weight, forms, and refusal conditions,
  and produce a manifest that points from each source assignment to its
  owning native module. Translation is complete only when the native result
  passes semantic comparison and an independent identity check.
- [x] Implement the reusable first subset of the metric block, `sqrtg`,
  reciprocal basis, `B_i`, `B^i`, vector-density components, and the Fourier
  mode derivative as native toolkit operations rather than script-specific
  code.
- [x] Add the generic fixed-3D Fourier curl-curl/current owner
  `j_fourier(chart, reluctivity, potential, mode)` with the paper's
  `d/du3 = i*n` reduction, C ABI, Python facade, and independent SymPy and
  native identities. The paper's block reduction is now a specialization of
  this operator.
- [x] Add the reusable fixed-three-dimensional Levi-Civita symbol/tensor and
  positive volume-density owner. The constitutive transformation
  `nubar_t = -E_t nu_t E_t` and the zero/nonzero mode branch reduction are
  implemented natively; the reduced two-dimensional Levi-Civita density and
  its FEM assembly ownership remain open.
- [x] Generate the paper's scalar longitudinal and transverse weak-form
  coefficient blocks from the native constitutive owner. The descriptor
  preserves scalar nodal versus two-dimensional edge spaces, normal versus
  tangential boundary traces, and the mode-dependent transverse mass block.
  Source assembly and generated source terms remain open.
- [ ] Add independent checks for the paper identities, including the de Rham
  diagram, `n = 0` reduction to the full three-dimensional curl-curl form,
  `n != 0` transverse reduction, tensor transformation round trips, and
  boundary/singularity behavior at the cylindrical axis.
- [ ] Add Fortran and Python examples that derive the formulas in the same
  order as the paper, then emit compact Fortran kernels for selected Fourier
  modes. The generated kernel must preserve density components and avoid
  array temporaries.
- [x] Add a native `paper_magnetic` example that is readable without the
  source scripts: declare `Z`, `R`, `phi`, `n`, `A`, and `g`, construct
  `sqrtg`, `B%up`, `B%down`, and the Fourier curl, and assert the paper's
  equations through native `zero_test`.
- [x] Add the native Python `Chart` facade for the same first subset, with
  native C-ABI transport and no duplicate geometry implementation.
- [x] Add the Python/SymPy facade for the weak-form descriptor and compare its
  branch metadata and constitutive coefficients with SymPy. The corresponding
  Wolfram frontend records and all-three result-tree comparison remain open.

### Relativity and geometry examples

- [ ] Add a flat Cartesian-to-polar/cylindrical example that derives
  `sqrtg`, reciprocal bases, Christoffel symbols, and the scalar Laplacian.
- [x] Add spherical coordinates and verify the volume element, reciprocal
  basis, Christoffel symbols, gradient, divergence, curl, and
  Laplace--Beltrami operator against independent component formulas and
  SymPy. The example keeps the expensive connection/vector-calculus path on
  the compact explicit metric owner, avoiding raw Cartesian chain-rule growth;
  the chart path remains independently checked for basis, metric, Jacobian,
  and curl.
- [ ] Add a nonorthogonal torus and a flux-coordinate chart with off-diagonal
  metric terms. Verify that raising, lowering, reciprocal bases, and
  Jacobian-weighted operators remain correct.
- [ ] Add a pseudo-Riemannian two-dimensional and four-dimensional example
  with geodesics, curvature, Ricci, scalar curvature, and Einstein tensor.
  Include a flat-space zero-curvature case and a curved-space case with an
  independently known invariant.
- [ ] Add electromagnetic forms, a magnetic two-form, and a gauge-equivalent
  vector-potential example. Compare the form and tensor/vector outputs in both
  frontends.
- [x] Add the first dimension-aware four-dimensional pseudo-Riemannian owner:
  explicit coordinates, signature, positive `sqrt(abs(det(g)))`, inverse
  metric, Christoffel symbols, Riemann, Ricci, scalar curvature, and Einstein
  tensors. Verify spherical Minkowski flatness and selected nonzero connection
  coefficients natively. Geodesics, 2D parity, curved invariants, and frontend
  transport remain open.
- [x] Add the first runtime-dimension parity check with a curved two-dimensional
  metric `g = diag(1, exp(2x))`, the known scalar curvature `R = -2`, and
  native-zero unused tensor slots. General 2D pseudo-Riemannian and curved
  invariant coverage remains open.
- [x] Transport that four-dimensional owner through the ABI and both Python
  facades, including inverse metric, Christoffel, Riemann, Ricci, scalar, and
  Einstein views. The Python result trees are differentially checked against
  independently assembled SymPy Christoffel expressions; full frontend corpus
  generation and Wolfram translation remain open.
- [x] Transport the first dimension-aware spacetime-form owner through the
  current ABI and both Python facades. The native owner remains the single implementation;
  the Python test independently checks a component of `d(A)`, `d(d(A))`, the
  Lorentzian Hodge sign, and wedge antisymmetry.
- [x] Add a parameterized geodesic residual owner and current-ABI/Python transport:
  `x''^a + Gamma^a_bc x'^b x'^c`, with explicit substitution of the curve
  into the metric connection. Spherical-coordinate residual checks now cover
  nonzero Christoffel dependence; geodesic solving and variational mechanics
  remain open.
- [x] Extend the four-dimensional form owner with contraction and Cartan Lie
  derivative. Independent tests compare `L_X(alpha)` with
  `i_X(d(alpha)) + d(i_X(alpha))`; arbitrary-dimensional pullback and topology
  remain open.
- [x] Add the first native Laplace--de Rham owner through the current ABI. The flat
  Lorentzian scalar case is checked against the signed wave-operator formula;
  higher-degree curved-space simplification and boundary/topology metadata
  remain open.
- [ ] Publish short Fortran, Python, and Wolfram examples for every completed
  derivation. Each example must show construction, simplification, a named
  identity check, and code generation where applicable.

### Verification, performance, and release gates

- [ ] Add a geometry differential harness that runs the original Wolfram
  inputs, generated Python inputs, native Fortran inputs, and SymPy oracle
  checks from one case manifest. It must compare expressions semantically,
  preserve unevaluated conditions, and report unsupported branches by name.
- [ ] Add independent verification from coordinate-change composition,
  determinant identities, index contractions, exterior-algebra identities,
  finite differences, numerical point samples, and residuals of generated
  kernels. SymPy agreement alone is insufficient.
- [ ] Benchmark cold construction, warm symbolic operations, tensor
  canonicalization, density transforms, form operations, and code generation
  against equivalent SymPy workloads. Benchmark the paper's Fourier mode
  derivation at several mode numbers and expression sizes. Enforce the
  SymPy performance rule for every supported row.
- [ ] Add resource limits for tensor rank, form degree, dummy-index search,
  Fourier mode count, expression growth, and singular chart detection. A
  limit produces a stable refusal with the offending budget.
- [ ] Keep the toolkit modules usable without the Python or Wolfram frontends,
  and keep both frontends usable without importing SymPy at native runtime.
  Update the module graph, API inventory, naming audit, feature matrix,
  refusal table, and generated examples together with each completed slice.

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
  - [x] Make the linked SymEngine conversion DAG-aware for larger expressions:
    cache one backend handle per shared arena node during a call, while keeping
    the lean uncached path for small expressions so conversion bookkeeping does
    not slow ordinary scalar workloads.
  - [x] Add the fixed-size `metric_inner` contraction path without lowering a
    vector into a temporary array; nonorthogonal metric norms now share one
    native owner across Fortran, C, and Python.
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
  - [x] Extend that construction diagnostic to the exponent-one constructor;
    the additional row is correctness-checked against SymPy 1.14.0 and kept
    explicitly waived for the same Python-ABI cost boundary; it measured 1.51x
    SymPy in the recorded run.
  - [x] Add the exact `log(0)` singularity to the correctness and performance
    matrix. Its cold and warm one-node simplification rows are explicit ABI
    diagnostics, measured at 5.07x and 3.43x SymPy respectively in the
    recorded run; the 54 core rows remain enforced with zero unwaived
    violations.
  - [x] Make native expansion reuse arena-sized memo workspaces and skip the
    redundant second expand/simplify pass when the first result is already in
    expanded form. The independent numerical expansion checks remain green;
    the matched `expand_power` cold row fell from 1.36 ms to 0.835 ms, then to
    0.398 ms after the canonical multinomial result began bypassing the general
    simplifier (about 1.35x SymEngine and 52% less native time in the latest
    local diagnostic).
  - [x] Add the exact `log(i)`/`log(-i)` branch points to the correctness and
    performance matrix. Their cold and warm rows are enforced and measured at
    0.016x and 0.053x SymPy; the 58 substantive rows remain enforced with
    zero unwaived violations.
  - [x] Add the exact `atanh(1)`/`atanh(-1)` pole boundaries to the correctness and
    performance matrix. Their cold and warm one-node simplification rows are
    explicit ABI diagnostics, measured at 5.34x and 4.15x SymPy respectively;
    the 58 substantive rows remain enforced with zero unwaived violations.
  - [x] Add the exact `atanh(i)`/`atanh(-i)` branch points to the correctness
    and performance matrix. Their cold and warm rows are enforced and measured
    at 0.014x and 0.044x SymPy; the 60 substantive rows remain enforced with
    zero unwaived violations.
  - [x] Add the exact `atan(i)`/`atan(-i)` branch points to the correctness and
    performance matrix. Their cold and warm rows are enforced and measured at
    0.018x and 0.052x SymPy; the 62 substantive rows remain enforced with
    zero unwaived violations.
  - [x] Add the exact `acosh(0)`/`acosh(-1)` branch points to the correctness
    and performance matrix. Their cold and warm rows are enforced and measured
    at 0.017x and 0.058x SymPy; the 64 substantive rows remain enforced with
    zero unwaived violations.
  - [x] Add the exact `acosh(i)`/`acosh(-i)` branch points to the correctness
    and performance matrix. Their cold and warm rows are enforced and measured
    at 0.002x and 0.013x SymPy; the 66 substantive rows remain enforced with
    zero unwaived violations before the `asin` rows are added.
  - [x] Add the exact `asin(i)`/`asin(-i)` branch points to the correctness and
    performance matrix. Their cold and warm rows are enforced and measured at
    0.004x and 0.015x SymPy; the 68 substantive rows remain enforced with zero
    unwaived violations before the `acos` rows are added.
  - [x] Add the exact `acos(i)`/`acos(-i)` branch points to the correctness and
    performance matrix. Their cold and warm rows are enforced and measured at
    0.002x and 0.015x SymPy; the 70 substantive rows remain enforced with zero
    unwaived violations before the real unit-circle rows are added.
  - [x] Add exact real unit-circle `asin`/`acos` values to the correctness and
    performance matrix. The two cold rows are enforced at 0.020x SymPy and the
    two warm rows at 0.044x; the 74 substantive rows remain enforced
    with zero unwaived violations before the real tangent rows are added.
  - [x] Add exact real tangent `atan` values to the correctness and performance
    matrix. Its cold and warm rows are enforced at 0.029x and 0.043x SymPy;
    the 76 substantive rows remain enforced with zero unwaived violations.
  - [x] Add exact real `asinh(±1)` values to the correctness and performance
    matrix. Its cold and warm rows are enforced at 0.008x and 0.008x SymPy;
    the 78 substantive rows remain enforced with zero unwaived violations.
  - [x] Add exact negative perfect-square roots to the correctness and
    performance matrix. The cold and warm rows are enforced and measured at
    0.029x and 0.062x SymPy; the 80 substantive rows remain enforced with
    zero unwaived violations.
  - [x] Add the exact `asinh(i)`/`asinh(-i)` branch points to the correctness
    and performance matrix. Their cold and warm rows are enforced and measured
    at 0.018x and 0.051x SymPy; the 82 substantive rows remain enforced with
    zero unwaived violations.
  - [x] Add finite gamma-family pole cases to the correctness and performance
    matrix. The six cold and warm rows are correctness-checked ABI diagnostics
    and explicitly waived at 5.1--5.3x cold and 3.4x warm SymPy; the final
    94-workload matrix retains 82 enforced rows with zero unwaived violations.
  - [x] Add compact exact factorial values to the correctness and performance
    matrix. The two cold and warm rows are correctness-checked ABI diagnostics
    and explicitly waived at 5.25x cold and 3.25x warm SymPy; the final
    96-workload matrix retains 82 enforced rows with zero unwaived violations.
  - [x] Add bounded arbitrary-size factorial values to the correctness and
    performance matrix. The `factorial(100)` cold and warm rows are
    correctness-checked ABI diagnostics and explicitly waived at 5.57x cold
    and 3.57x warm SymPy; the final 98-workload matrix retains 82 enforced
    rows with zero unwaived violations.
  - [x] Add native `free_symbols` correctness and performance coverage. The
    C ABI and Python facade reuse one native traversal; the Python facade
    caches the immutable handle set, measuring 0.236x cold and 0.054x warm
    SymPy in the 102-row matrix with zero unwaived violations.
  - [x] Add simultaneous substitution correctness and performance coverage.
    The 104-row matrix measured native/SymPy ratios of 0.330x cold and 0.370x
    warm with zero unwaived violations.
  - [x] Add unordered mapping substitution ordering correctness and performance
    coverage. The 106-row matrix measured native/SymPy ratios of 0.575x cold
    and 0.276x warm with zero unwaived violations.
  - [x] Add exact-node `xreplace` correctness and performance coverage. The
    108-row matrix measured native/SymPy ratios of 0.227x cold and 0.590x warm
    with zero unwaived violations.
  - [x] Add exact non-wildcard `match` correctness and performance coverage.
    The 110-row matrix measured native/SymPy ratios of 0.699x cold and 0.526x
    warm, with zero correctness failures and zero unwaived violations.
  - [x] Add bounded `Wild` matching correctness and performance coverage. The
    112-row matrix measured native/SymPy ratios of 0.698x cold and 0.503x warm
    for direct wildcard matching, with zero correctness failures and zero
    unwaived violations.
  - [x] Add exact non-wildcard `replace` correctness and performance coverage.
    The 114-row matrix measured native/SymPy ratios of 0.716x cold and 0.029x
    warm, with zero correctness failures and zero unwaived violations.
  - [x] Add bounded single-Wild commutative remainder correctness and
    performance coverage. The 116-row matrix measured native/SymPy ratios of
    0.357x cold and 0.0042x warm on a 25-term additive remainder, with 100
    correctness cases, zero correctness failures, and zero unwaived violations.
  - [x] Add bounded distinct-Wild commutative partition correctness and
    performance coverage. The 118-row matrix measured native/SymPy ratios of
    0.459x cold and 0.0050x warm, with 101 correctness cases, zero correctness
    failures, and zero unwaived violations.
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
