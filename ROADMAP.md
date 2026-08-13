# fortsym roadmap

FortSym is a Fortran-native symbolic algebra system with a small, consistent
Fortran API and a SymPy-compatible Python facade. The target is parity with the
pinned SymPy profile in [`doc/upstream-baselines.toml`](doc/upstream-baselines.toml),
one well-defined compatibility slice at a time.

This roadmap is about symbolic computation. It is not a finite-element or
finite-volume roadmap.

## Scope boundary: FortSym and FortFEM

FortSym owns:

- expression construction, exact domains, assumptions, simplification, and
  symbolic calculus;
- polynomial, rational, equation, matrix, tensor, and differential-form
  operations that are part of the declared SymPy-compatible surface;
- symbolic coordinate and physics reductions when they are useful as general
  symbolic operations;
- the native Fortran facade, stable C ABI, Python facade, printers/parsers, and
  source-level kernel generation.

FortFEM owns meshes, finite-element families and bases, element/Piola maps,
quadrature, degrees of freedom, local/global assembly, sparse systems, and
numerical solvers. FortSym may provide expressions, typed symbolic records,
and generated kernels for FortFEM, but must not add FE-specific owners to
FortSym.

Existing magnetic, relativity, tensor, form, and chart code remains useful
native functionality. It is maintained and tested, but new work in those
areas is a roadmap item only when it closes a SymPy-compatible symbolic gap or
protects an existing public invariant. Paper-specific Fourier/FEM reductions,
FE basis evaluation, assembly, and solver workflows are FortFEM work.

## Non-negotiable design rules

- SymPy is the oracle for overlapping correctness and performance. Every
  supported operation needs differential tests against the pinned SymPy
  version and a benchmark on the same workload.
- A SymPy result is not enough as the only proof. Use an independent
  algebraic, numerical, residual, interval, recurrence, or compiler oracle as
  appropriate.
- A feature is either implemented and verified, delegated explicitly, or
  refused with a useful diagnostic. No silent fallback or guessed result.
- Every concept has one owner and one canonical internal name. The Fortran
  facade and internals share that vocabulary; `fortsym.sympy` adapts names,
  signatures, options, and return conventions only at the Python boundary.
- Keep concise names where they are clearer than SymPy's name. When a name is
  not already better, use SymPy's spelling so porting code is mechanical.
- The default global-state facade is the easy mode. Explicit arenas and
  assumption contexts use the same operations for isolation and concurrency.
- Prefer operators, generic interfaces, optional arguments, derived-type
  methods, and simple assignments such as `x = "x"` when they reduce syntax.
- Keep hot paths free of avoidable array temporaries and expression copies.
  Resource bounds must be explicit and failures deterministic.
- Do not add a second implementation in a facade, backend adapter, test, or
  example. Tests compare independently computed behaviour, not repository
  state or duplicated formulas.

## Definition of done for every parity slice

- [ ] SymPy 1.14.0 reference cases cover ordinary, boundary, branch, domain,
      option, and refusal behaviour.
- [ ] An independent behavioural oracle checks the result or invariant.
- [ ] The canonical Fortran owner, convenience facade, C ABI, and Python
      adapter are aligned wherever that layer is public.
- [ ] The operation's names, signatures, return shape, exceptions, conditions,
      precision, and refusal messages are documented.
- [ ] Cold-start, warm-cache, and core-operation benchmarks compare with
      SymPy; supported workloads are at least as fast as SymPy or have an
      explicit reviewed exception.
- [ ] Resource limits, singularities, invalid handles, foreign arenas, and
      unsupported options are tested.
- [ ] No avoidable array-temporary or expression-growth regression is added.
- [ ] `FO_TEST_TIMEOUT=60 fo`, focused tests, and the relevant differential
      and benchmark suites pass.
- [ ] The feature matrix, API classification, naming audit, and benchmark
      baseline are updated in the same change.

## Current baseline

The following are already present as bounded native slices. Their boundaries
are part of the public contract; a checked item here does not claim full
SymPy parity for the entire family.

- [x] Pin SymPy 1.14.0 and maintain the generated API inventory,
      classification, naming audit, and semantic-difference ledger.
- [x] Provide the independent differential-test and benchmark harnesses.
- [x] Provide the hash-consed native expression arena, exact compact and
      promoted numeric representations, symbols, functions, relations,
      operators, substitution, free-symbol traversal, printing, and parsing.
- [x] Provide the default Fortran facade and explicit-arena mode with one
      consistent vocabulary, plus the stable ownership-safe C ABI.
- [x] Provide the first assumptions/predicate/refinement layer and
      three-valued native zero decisions.
- [x] Provide bounded native expansion, simplification, differentiation,
      factorisation, together/cancel/apart/collect, complex-domain operations,
      and exact polynomial/rational primitives.
- [x] Provide verified bounded one-variable integration, finite/infinite
      limits, and Taylor series/coefficient extraction through the C, Python,
      and SymPy facades.
- [x] Keep native coordinate, tensor, form, relativity, and magnetic owners
      separate from the symbolic core and separate from FortFEM ownership.

## Phase 0 — parity inventory and release contract

- [x] Freeze the reference profile and regenerate the inventory from the
      installed SymPy version.
- [x] Classify every public name as native, delegated, adapter-only, or
      refused; record one canonical name for each concept.
- [x] Maintain a differential harness that can run the same case table with
      `sympy` and `fortsym.sympy` without duplicating the cases.
- [x] Maintain cold/warm/core benchmark measurements and fail the gate when a
      supported workload regresses beyond its reviewed budget.
- [x] Make the inventory, classification, naming audit, difference ledger,
      feature matrix, and benchmark report a single checked release gate via
      `doc/release-profile.toml` and `scripts/check_release_profile.py`.
- [x] Add a compatibility-profile command that reports the exact supported
      SymPy names and refuses to mix baselines via
      `scripts/compatibility_profile.py`.

## Phase 1 — coherent core semantics

### Expression model and domains

- [ ] Complete the expression hierarchy and canonical ordering needed by the
      compatibility profile: atoms, applications, relational/Boolean nodes,
      sets, tuples, matrices, indexed objects, and unevaluated forms. The
      native-owned `FiniteSet`, `Tuple`, and `Complement` result fragments and
      the bounded native Boolean application slice and the evaluated
      relational-constructor slice are complete; the remaining
      families stay explicitly bounded below.
- [x] Add the bounded relational/Boolean constructor slice: `And`, `Or`,
      `Not`, `Xor`, `Implies`, and `Equivalent`, shared native application
      ownership, `&`/`|`/`^`/`~` expression syntax, Boolean identity edges, and
      complementary relational negation. Full Boolean simplification and
      condition reduction remain later work.
- [x] Evaluate decidable relational constructors through the native
      simplifier: exact integer/rational `Eq`/`Ne`/ordering results and
      identical-expression `Eq`/`Ne` agree with SymPy, while unknown symbolic
      ordering remains relational and cross-arena operands are refused.
- [x] Keep the supported tensor indexing boundary explicit: rank-one
      `SpacetimeTensor` slices return borrowed component views, while
      higher-rank slices are refused and the native-vs-SymPy test boundary
      compares through the independent semantic oracle.
- [x] Align the bounded `Rational` constructor with SymPy for integer,
      `Fraction`, rational/decimal-string, finite-float, and native exact-number
      inputs, including canonical reduction and `zoo`/`nan` zero-denominator
      results; non-finite, precision, and broader exact-domain conversion remain
      explicitly deferred.
- [x] Align bounded real-literal equality and hashing with SymPy: native
      `Float` values compare symmetrically with Python `float` values and use
      the corresponding Python-float hash, while exact rationals remain distinct
      from binary floats.
- [x] Expose bounded dense `Matrix` row, column, block, reverse, stepped, and
      empty 2-D slices through the native `List` owner, including existing
      column-vector results; flat indexing and broader matrix-expression
      slicing remain explicitly outside this slice.
- [x] Expose bounded dense `Matrix` row-major flat scalar indexing and
      slicing, including negative, stepped, reverse, and empty selections;
      matrix expressions and broader indexing options remain explicitly
      outside this slice.
- [ ] Complete exact integer, rational, real, complex, algebraic, infinity,
      NaN, signed-zero, and complex-infinity semantics, including conversion
      and precision rules.
- [ ] Make equality, hashing, matching, traversal, replacement, printing, and
      parsing agree across native Fortran, C, Python, and SymPy spellings.
- [ ] Define ownership and lifetime rules for every composite return value;
      no borrowed handle may escape its owner.
- [ ] Add explicit precision/accuracy APIs and preserve exactness through all
      constructors, operators, and adapters.

### Assumptions and simplification

- [ ] Complete the SymPy predicate vocabulary, inference rules, scoped
      contexts, conflict diagnostics, and `Q`/`ask`/`refine` semantics.
- [ ] Complete safe branch-aware simplification for powers, radicals,
      logarithms, elementary functions, special values, and algebraic signs.
- [ ] Implement the structural tools required by the profile, including
      `count_ops`, `cse`, `collect`, `expand`, `powsimp`, `powdenest`, and
      `powexpand`, with explicit option handling.
- [x] Keep `zero_test` as the one public three-valued native query. Test-only
      assertion helpers remain in `fortsym_check` and do not become competing
      user-facing names; the naming audit and Fortran/C API documentation
      enforce this boundary.

## Phase 2 — polynomial and rational algebra

- [ ] Complete dense/sparse multivariate polynomial representations, domains,
      coercions, rings, fraction fields, coefficient extraction, degree,
      division, GCD, extended GCD, and square-free decomposition.
- [ ] Complete `factor`, `factor_list`, `cancel`, `together`, `apart`,
      `collect`, `terms_gcd`, and the corresponding polynomial constructors
      with SymPy-compatible options and conditions.
- [ ] Complete bounded-to-general univariate and multivariate factorisation,
      algebraic roots, root isolation, `RootOf`, and root selection without
      returning unverified roots.
- [ ] Add bounded Groebner bases and polynomial-system elimination, with
      domain/resource refusals rather than silent coefficient loss.
- [ ] Remove arbitrary native expression-order limits where SymPy defines a
      result; retain only measured, documented resource limits.

## Phase 3 — calculus, series, and transforms

### Differentiation and series

- [ ] Complete scalar, multivariate, implicit, indexed, matrix, tensor, and
      unevaluated derivative semantics for `diff`, `Derivative`, and `doit`.
- [ ] Complete Taylor, Laurent, Puiseux, formal, asymptotic, and composed
      series, including `Order` and coefficient extraction.
- [ ] Complete residues, sequence limits, directional limits, one-sided
      limits, complex limits, and asymptotic ordering.

### Integration and transforms

- [ ] Complete rational integration through Hermite and
      Lazard--Rioboo--Trager methods.
- [ ] Add the supported elementary, Risch/heuristic-Risch, Meijer-G, and
      special-function integration families with domain conditions.
- [ ] Complete definite, improper, multiple, and parameterised integration,
      including convergence and condition reporting.
- [ ] Complete Fourier, Laplace, inverse, sine, cosine, and related transform
      APIs, plus symbolic sums/products and telescoping.

## Phase 4 — equations and exact linear algebra

- [x] Expose the already-tested bounded polynomial and scalar-linear solver
      through the canonical Fortran, C, Python, and SymPy facades. Return
      distinct verified roots in the SymPy-compatible list shape and refuse
      unsupported domains/options.
- [x] Add bounded `solveset` return semantics on that same verified path,
      including `FiniteSet`/`EmptySet` and explicit non-default-domain
      refusals, without duplicating the native root algorithm.
- [x] Extend one-variable `solve`/`solveset` to bounded exact rational
      functions by solving the combined numerator and verifying every candidate
      against the original residual, so denominator poles are never returned.
- [x] Preserve bounded symbolic rational denominator poles in `solveset` as
      native-verified `Complement` exclusions through the C ABI and Python
      facade; keep `solve`'s list contract unchanged and refuse unsupported
      domain/condition families explicitly.
- [x] Expose verified square exact-rational one-right-hand-side `linsolve`
      through the canonical Fortran, C, Python, and SymPy facades as
      `FiniteSet(Tuple(...))`; singular, symbolic, free-parameter, matrix
      object, and alternate input forms remain explicit refusals.
- [x] Extend the exact-rational `linsolve` owner to rectangular consistent
      systems with free parameters and to explicit inconsistent-system
      `EmptySet` results, reusing native RREF through one versioned C-ABI path;
      symbolic coefficient domains and broader condition handling remain
      explicit refusals.
- [x] Expose bounded exact dense `Matrix` construction, `(row, column)`
      indexing, and native-backed determinant through the canonical facades;
      ragged rows and broader matrix operations remain explicit refusals.
- [x] Expose bounded exact dense `Matrix.rank()` through the canonical
      facades, using the existing native RREF owner and refusing malformed or
      over-bounded forms.
- [x] Expose bounded exact dense `Matrix.inv()` through the canonical facades,
      using the existing exact inverse owner and refusing singular or malformed
      forms.
- [x] Expose bounded exact dense `Matrix.transpose()` and `.T` through the
      canonical facades, preserving shape and refusing malformed forms.
- [x] Expose bounded exact dense `Matrix.nullspace()` through the canonical
      facades, returning column matrices with exact RREF basis vectors and
      refusing malformed or over-bounded forms.
- [x] Expose bounded exact dense `Matrix.rref()` through the canonical facades,
      returning the reduced matrix and zero-based pivot tuple with explicit
      refusal of unsupported options and over-bounded forms.
- [x] Align bounded `Matrix.rref(pivots=False)` with SymPy by returning only
      the reduced Matrix while reusing the same native RREF owner; custom zero
      functions, conditional pivots, and other unsupported options remain
      explicit refusals.
- [x] Expose bounded exact dense `Matrix` products and scalar scaling through
      `*`/`@`, reusing the native matrix-dot owner and refusing mismatched
      dimensions, foreign arenas, and unsupported operands.
- [x] Expose bounded exact dense `Matrix` elementwise addition, subtraction,
      and unary negation through the canonical facades, reusing one native
      nested-`List` owner and refusing mismatched dimensions and non-matrices.
- [x] Expose bounded exact dense `Matrix / scalar` through the canonical
      facades, including symbolic and zero divisors, while refusing matrix
      right-division and foreign arenas.
- [ ] Complete `solve`, `solveset`, real/complex solves, `linsolve`, and
      solution conditions for polynomial, rational, radical, inverse-function,
      and supported transcendental equations.
- [ ] Add inequality reduction, compound-domain reasoning, Diophantine
      solving, recurrences, and elimination where present in the profile.
- [ ] Complete dense/sparse exact matrices: construction, indexing, slicing,
      determinant, inverse, rank, nullspace, decompositions, eigenvalues,
      characteristic/minimal polynomials, and `DomainMatrix` equivalents.
- [ ] Keep matrix algorithms symbolic and reusable. Meshes, FE matrices,
      sparse assembly, and numerical linear solvers remain FortFEM or another
      numerical package's responsibility.
- [ ] Complete ODE classification/solving and include PDE solving only when it
      is explicitly in the selected compatibility profile.

## Phase 5 — functions and discrete mathematics

- [ ] Complete the elementary and special-function families in the inventory:
      evaluation, rewriting, expansion, inverse functions, branch cuts,
      conjugation, real/imaginary parts, and differentiation/integration.
- [ ] Complete combinatorics, partitions, permutations, number theory,
      prime/factorisation utilities, modular arithmetic, continued fractions,
      and discrete recurrences.
- [ ] Complete sets, intervals, finite/infinite sets, tuples, logic, Boolean
      simplification, predicates, and set-valued solve results.
- [ ] Complete statistics/probability objects and distributions only for the
      names selected into the compatibility profile; do not create a second
      numerical statistics engine in the symbolic core.

## Phase 6 — arrays, tensors, geometry, and physics compatibility

This phase covers symbolic APIs that SymPy users expect. It does not import
finite-element responsibilities into FortSym.

- [ ] Complete the selected SymPy matrix-expression, N-dimensional-array,
      indexed, tensor, and Einstein-contraction APIs on shared expression and
      exact-domain owners.
- [ ] Complete the selected `sympy.diffgeom` names—manifolds, patches,
      coordinate systems, differential forms, pullbacks, Lie derivatives,
      and metric operations—without duplicating the existing native owners.
- [ ] Complete the selected symbolic physics modules only where their public
      expressions, identities, or transformations are part of the profile.
- [ ] Preserve variance, density, owner, chart, signature, orientation, and
      domain metadata through every symbolic operation and generated kernel.
- [ ] Keep magnetic-coordinate, relativity, and form examples as regression
      cases for these symbolic APIs. They are not a reason to add meshes,
      quadrature, FE bases, Piola maps, assembly, or numerical solvers here.

## Phase 7 — input/output, numerics, and code generation

- [ ] Complete SymPy-compatible text, LaTeX, pretty, MathML, Wolfram, and
      Fortran parsing/printing for the supported expression hierarchy.
- [ ] Complete `evalf`, numerical substitution, lambdify-style dispatch, and
      explicit precision/rounding/error reporting without weakening exact
      native results.
- [ ] Complete common-subexpression elimination and Fortran kernel generation
      with a cost model measured against SymPy and the generated compiler.
- [ ] Ensure generated code has no avoidable array temporaries, no hidden
      domain changes, and explicit handling for singular/conditional branches.
- [ ] Keep external engines optional and version-checked. They may supply a
      differential result or a selected backend, but never define native
      semantics or leak their representation into the public API.

## Phase 8 — parity release gates

- [ ] Every inventoried supported name has one executable case table shared by
      the SymPy oracle and the native adapter.
- [ ] Every supported family has independent algebraic/numerical/residual
      checks and adversarial resource-limit cases.
- [ ] Every supported family has cold, warm, and core benchmarks with the same
      inputs, and native performance is at least SymPy's for the declared
      workload or the exception is documented and approved.
- [ ] The full native `fo` gate, C ABI tests, Python facade tests, SymPy
      differential tests, API audits, examples, and benchmark gates pass.
- [ ] The public API has one canonical name per concept, one owner per
      implementation, consistent default/global and explicit-state modes, and
      no untracked aliases.
- [ ] The release documentation lists supported features, known differences,
      refusal boundaries, licenses/provenance, performance, and FortFEM handoff
      contracts.

## Work selection rule

At the start of each work session, select the smallest unchecked slice that:

1. closes a named SymPy compatibility gap;
2. has a clear native owner and independent oracle;
3. improves a user-visible supported path or removes a documented refusal;
4. has a measurable correctness and performance target; and
5. does not belong to FortFEM.

If a proposed change is primarily about FE discretisation, mesh data,
quadrature, basis evaluation, assembly, sparse numerical systems, or a
paper-specific FEM workflow, move it to `../fortfem` and add only the symbolic
interface contract here.
