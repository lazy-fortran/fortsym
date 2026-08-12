# CAS backend architecture

## Scope

fortsym targets symbolic work that produces or verifies Fortran kernels. The
supported fragment is versioned and explicit. General zero equivalence for
elementary functions is undecidable, so an engine must return `UNKNOWN` outside
its decision procedures rather than infer a Boolean answer.

The public expression type is independent of every backend. `arena_t` stores a
hash-consed directed acyclic graph, and `expr_t` holds an arena pointer and a
node index. Backends translate this graph to their own representation. The
native backend operates on it directly. The ordinary Fortran entry point is
`use fortsym`, which supplies a documented default arena and character-to-symbol
assignment. The explicit modules remain the first-class choice for independent
or concurrent derivations. See [`fortran-api.md`](fortran-api.md) for the
naming rule and lifetime contract.

## Layers

1. `src/core` owns expression identity, exact scalar literals, applied
   functions, and substitution.
2. `src/io` parses and prints canonical expressions and backend dialects.
3. `src/calculus` implements mechanical differentiation, contracted derivative
   products, coordinate geometry, typed tensor algebra, covariant connections
   and curvature, and the native differential-form layer.
4. `src/engine` declares operations and wraps native or external algorithms.
5. `src/algebra` implements deterministic exact operations over expression
   arrays, including dense rational linear systems, bounded symbolic matrices,
   and the bounded sparse multivariate polynomial/rational layer.
6. `src/council` compares supported engine answers and records disagreement.
7. `src/verify` supplies independent real evaluation and three-valued checks.
8. `src/codegen` selects shared subexpressions and emits Fortran kernels.

### Module ownership audit

The structural module audit is reproducible with:

```text
python3 scripts/audit_module_architecture.py doc/module-architecture.json
```

At the current revision it covers 69 Fortran modules and 338 internal `use`
edges. It requires one named module per source owner, matching filenames,
known internal dependencies, an acyclic module graph, and no implementation
module importing the convenience facade. The generated report records the
cross-directory edges for review; aggregation by `fortsym`, the public C ABI,
Wolfram I/O, and the native engine is intentional orchestration, not a second
public vocabulary. Generated source units without a module are listed rather
than treated as module owners.

Kernel targets are represented by stable integer identities with canonical
names: `fortran_cpu`, `fortran_openmp_target`, `fortran_openacc`, and `cuda`.
The target descriptor selects only source spelling and leaf decoration; it
never branches the backend-neutral IR. CPU and CUDA leaves carry no Fortran
directives, while the two offload targets select their corresponding directive.
The combined OpenMP/OpenACC identity remains available for compatibility with
committed FortNum kernels. The unset descriptor preserves the historical
logical flags in `emit_kernel`, so regenerating existing dual-target artifacts
does not create formatting-only diffs.

Kernel emission can also return an optional machine-readable operation-cost
record through `emit_kernel(..., cost_record=record)`. The record uses the
`fortsym.operation_cost.v1` JSON schema: totals count distinct nodes in the
union of the hash-consed output DAGs, `roots` count each output independently,
and `transcendentals` reports named function heads separately. `flops` counts
additions, multiplications, and divisions; `instructions` is the corresponding
structural operation count after collapsing direct multiply-plus-add patterns
into `fma_candidates`. These are symbolic cost metadata, not a disassembly or
runtime claim.

`kernel_emit_spec_t%policy` makes the source-level floating-point choices
explicit: positive integer powers up to `small_power_limit` can be expanded,
exact zero and one elements can be folded, constant reciprocals can be emitted
as literals, and one-use products in sums can be shaped for FMA recognition.
These transformations preserve the exact expression's real-valued semantics;
they do not enable compiler reassociation. Purity remains an opt-in field until
the downstream `fortnum#73` contract is settled.

The `fortsym_backend` module is the bounded synthesis handoff. It serializes
canonical native expressions under `fortsym.expression.v1`, exposes typed
`PROVED`, `DISPROVED`, and `UNKNOWN` evidence with a versioned JSON record, and
packages kernel source with the existing `fortsym.operation_cost.v1` metadata.
Source generation alone always carries `UNKNOWN`: only a consumer's Fortran
readback and equivalence assessment can establish a proof. This protocol is
stable enough for FortFront integration without claiming Lean/Why3 certificates
or IEEE floating-point equivalence.

The expression emitter's `kernel_spec_t` separately controls CSE per target
variant. `CSE_FULL` preserves the historical naming of repeated compounds;
`CSE_NONE` rematerialises them; and `CSE_THRESHOLDED` rematerialises nodes
whose recursively charged tree cost is below `remat_threshold`. The threshold
is an explicit heuristic for register/live-range trade-offs, not a claim about
machine registers or runtime performance.

An engine capability is a promise that its corresponding type-bound operation
is callable. A capability bit without an operation entry point is invalid and
must not be used for dispatch.

The polynomial layer treats opaque subexpressions as exact generators, so a
proved polynomial identity remains valid when those generators are mapped back
to expression nodes. It uses checked rational coefficients and refuses on
overflow, floating-point coefficients, resource limits, or factorisation that
has not been proved complete. Together, cancel, apart, GCD, division,
coefficient, collect, exponent, and numerator/denominator operations therefore
return either an exact expression or a named refusal.

Native integration uses the exact partial-fraction layer as a front end for its
verified elementary rules. The candidate is differentiated and checked before
it is returned; rational shapes outside the bounded rule set are named
refusals, so a partial antiderivative is never exposed as a result.

The bounded ODE layer in `fortsym_ode` solves one first-order linear equation
through an integrating factor. It accepts one optional value condition and
returns a Wolfram rule set only after substituting the candidate back into the
original derivative node and zero-testing the residual. Exponential forcing
uses a direct particular solution when its parameter denominator is decidably
nonzero; nonlinear, higher-order, multi-equation, separable, resonant, and
numeric ODEs remain explicit refusals.

Complex-domain operations use one rectangular splitter for `Re`, `Im`,
conjugation, `Arg`, `Abs`, and `ComplexExpand`. It requires explicit real facts
for symbols and refuses branch-sensitive or unknown-reality cases. Exact
algebraic atoms use FLINT's exact qqbar real and imaginary projections, and
exact algebraic conjugation delegates to the same bridge. The complex test
oracle evaluates the original expression independently rather than
reconstructing the expected parts from the splitter.

The `fortsym_metric` owner now carries fixed-three-dimensional metric
components, signature, orientation, inverse, determinant, positive `sqrtg`,
and oriented volume-form input independently of a chart. `metric_from_chart`
is its convenience constructor; it copies expression handles, not symbolic
subgraphs. The
`fortsym` facade, C ABI, and `fortsym.sympy` adapter all call the relevant
owner;
they add only result/error transport and SymPy spelling translation, so there
is no second projection or conjugation implementation at a public boundary.
The same ownership rule applies to coordinate charts: `fortsym_chart` owns
the map-derived metric and operators, `fortsym_chart_map` owns bidirectional
coordinate transitions and form pullbacks, `fortsym_magnetic` owns variance and
density views,
`fortsym_tensor` owns typed components, `fortsym_form` owns exterior algebra,
`fortsym_form_tensor` owns the exact bridge between their views without
duplicating either storage layout,
`fortsym_connection` owns covariant/curvature views, and `fortsym_flux` owns
coordinate-label metadata and flux-coordinate residual identities.
`fortsym_magnetic`
also packages its `B^i`, `B_i`, and vector-density views as typed tensors
without duplicating their component derivation. Chart maps retain source and
target patch declarations and validate the intermediate patch during
composition. The Python `Chart`,
`ChartMap`, `Tensor`, and `Form` classes only transport handles and metadata
through the C ABI.

The planned first-class geometry package split is:

```text
fortsym_geom -> fortsym_metric -> {fortsym_tensor, fortsym_form,
                                    fortsym_connection, fortsym_magnetic}
                           \-> fortsym_registry
```

`fortsym_geom` owns manifolds, patches, charts, bases, and coordinate maps.
`fortsym_metric` owns signatures, orientations, volume densities, volume
forms, and epsilon objects. The downstream modules consume those interfaces
and do not store private metric or chart copies. `fortsym_registry` contains
explicit capability records and procedure registration for optional toolkits;
it has no linker discovery or hidden initialization path. This split is the
implementation target recorded in the differential-geometry section of
[`ROADMAP.md`](../ROADMAP.md), while the current fixed-three-dimensional
modules are the migration starting point.

The registry owner is `src/calculus/fortsym_registry.f90`. A caller creates
and initializes `toolkit_registry_t`, registers a name, version, owner module,
capability description, and refusal boundary, and may attach a procedure
pointer probe. `register_builtin_geometry` fills an explicit registry with the
ten shipped geometry owners. The call is visible in application code, so a
host can register only the toolkits it ships and can add private owners beside
them without changing the core expression arena.

The native limit layer proves continuity substitutions, bounded L'Hopital
steps, polynomial degree ratios at infinity, and a restricted single-monomial
growth ordering. It checks a full two-sided finite neighborhood before
returning a value; one-sided behavior, poles, oscillation, and undecidable
growth are explicit `UNKNOWN`/refusal outcomes.

Polynomial solving extracts exact rational roots and closes quadratic factors
with exact square-root expressions. It verifies the returned root count and
reconstructs the polynomial through an independent test path; irreducible
higher-degree factors and systems are named refusals, never floating-point
guesses.

The native series engine computes Taylor coefficients by repeated exact
differentiation. Its bounded Laurent extension first identifies an integer
power of the shifted variable, regularises that pole, and then uses the same
coefficient path; a singularity it cannot classify exactly is refused.

Native special-function simplification is deliberately identity-based. It
proves `erf(0)`, `erfc(0)`, positive integer Gamma values through a checked
factorial cap, `Gamma(1/2)`, and integer-order `J`/`I` values at zero. It does
not approximate special functions or infer analytic continuation, asymptotic,
or branch-sensitive identities; those inputs remain symbolic until an
independent numeric backend is available.

The numeric boundary has two precision-preserving paths. `numeric_precision_text`
returns a closed real expression through the bounded MPFR evaluator as decimal
text, while `numeric_complex_text` first proves a supported rectangular split
and evaluates its real and imaginary expressions independently. Neither path
silently projects the requested precision through real64. `numeric_callable_t`
stores an ordered symbol list and exposes checked real64 point evaluation plus
the precision-text form for a downstream fortnum algorithm. It refuses free
symbols, non-finite points, branch-sensitive or unknown heads, and does not
implement quadrature, root finding, interpolation, or rigorous Arb balls.

`fortsym_accuracy` is the verification owner for caller-owned real64 kernels.
`measure_accuracy` substitutes a declared sample matrix, obtains an MPFR
reference for each closed expression, and compares the kernel result in local
ULPs. Its report retains the maximum and RMS error, the maximizing sample,
reference and observed values, the derivative-based condition number when
defined, and refused-sample counts. The reported maximum is a bound over the
declared sample set. The independent test kernel adds one `spacing(x)` so the
expected one-ULP result does not come from the measurement implementation.

Plotting is a separate adapter boundary. `fortsym_plot` samples closed real
expressions and maps the bounded `Plot`, parametric, list, field, and `Show`
fragments into fortplot data; it never implements a renderer. Undefined curve
samples are split instead of joined across a pole, partially undefined fields
are refused, and unsupported graphics forms remain named refusals. Both the
fpm and CMake paths pin the fortplot revision, while fortplot owns raster
output, legends, and panel layout.

Fortran kernel emission uses the dialect's declared function map rather than
passing symbolic heads through as identifiers. Standard intrinsics cover
`erf`, `erfc`, `gamma`, `log_gamma`, `bessel_jn`, and `bessel_yn`; the default
`fortnum_special` module supplies `bessel_in` and `bessel_kn`. Bessel orders
must be integer literals in the scalar kernel IR, and an unmapped head is
refused before source is returned. A consumer can override the special-module
name in `kernel_emit_spec_t` when its dependency exposes the same procedures.

Tier-2 adapters accept only plain symbol identifiers, known exact constants,
and the audited function vocabulary before rendering an expression as CAS
source. Unsupported names return `UNKNOWN`. SymPy receives an explicit local
symbol dictionary so Python globals cannot capture free names. Each subprocess
uses input and output files inside an atomically created mode-0700 temporary
directory; the directory and its files are removed after the framed reply is
read. Reply records are accumulated without truncation up to a 16 MiB bound;
larger records are refused instead of parsing a valid-looking prefix.

## Representation constraints

Exact arena nodes have a compact signed-64-bit representation and an
arbitrary-precision canonical decimal representation. Construction normalizes
through FLINT and downcasts every representable result to the compact node, so
zero, one, and small exponents retain the existing fast path. Native
simplification first combines compact coefficients with checked `int64`
arithmetic and promotes addition, multiplication, integer powers, and
like-term coefficients to FLINT on overflow or whenever an arbitrary-precision
operand participates. The scalar bridge normalizes and computes base-ten
integer/rational values through the shared FLINT 3.6.0
`fmpq` C interface pinned in `upstream-baselines.toml`; values return through a
single-slot thread-local render/fetch boundary and are copied immediately, so
no FLINT object or allocation enters the Fortran representation. Input,
rendered-output, and power-exponent budgets bound memory before large powers
are constructed. A binary operation may render up to 16 MiB, while one arena
scalar accepts at most 1 MiB; when a result crosses that boundary the parser
retains the original structural operation instead of creating an invalid or
partially interned scalar. The `fo` test gate resolves FLINT's `fmpq_add` and
MPFR's `mpfr_get_d` and requires both providers to be shared objects; CMake
rejects either static archive at configuration. Parsing, lossless CAS-dialect
printing, differentiation as
constants, and real probing when representable accept both representations.
Algebraic values are stored as `NK_ALGEBRAIC` atoms containing canonical
`qqbar1` text. The native engine promotes pure non-rational algebraic `+`, `*`,
and integer powers through the FLINT bridge; Gaussian-rational forms containing
the native `i` constant remain structural so the compatibility printers retain
the standard `i` vocabulary. Native simplification also collects
algebraic coefficients in mixed sums and products. Its zero, one, and
definitely-nonzero guards use the independent FLINT component-sign result.
The native and backend text parsers treat
`qqbar1`
payloads as opaque lossless atoms and delegate validation to the FLINT bridge.
Real64 expression evaluation still refuses algebraic atoms. Fortran kernel
emission accepts exact real algebraic atoms after a checked projection. The
SymEngine boundary converts Gaussian-rational atoms to exact rational complex
expressions and refuses higher-degree or otherwise non-rational atoms. A
real64 Fortran kernel projects an arbitrary exact value in the finite normal
binary64 range through a FLINT Arb enclosure whose midpoint is guaranteed to
round uniquely to nearest-even binary64, then emits one short typed literal.
Subnormal, overflow-range, non-real, and ambiguous-rounding projections are
refused. `print_expr_in`,
`print_expr_sub`, `emit_statements`,
and `emit_kernel` expose an optional success flag and return an empty string
when that projection is non-finite; they never emit a plausible wrong value or
an invalid oversized literal. Exact symbolic work must simplify or select
another numeric domain before kernel generation when binary64 rounding is
unacceptable. If a promoted scalar exceeds the arena's 1 MiB payload budget,
native simplification preserves the structural operation rather than exposing
a partial rewrite.

The exact algebraic bridge uses FLINT `qqbar` values (minimal polynomial plus
isolating enclosure) behind an ownership-safe C boundary. Its lossless
`qqbar1` format stores the primitive minimal-polynomial coefficients and the
root index in FLINT's canonical conjugate order. Degree 32, coefficient-height
4096 bits, 64 KiB serialization, and signed-power magnitude 64 are fortsym
resource limits rather than intrinsic `qqbar` restrictions. Exact Gaussian
rationals, arithmetic, conjugation, principal square roots, and real/imaginary
signs are callable; arena nodes and expression-engine promotion remain. The
format and adaptation are specified in `algebraic-format.md`. Rigorous
approximate real and complex evaluation uses Arb/Acb balls and may not be
silently converted into an exact result. Calcium predicates are three-valued;
resource limits or unimplemented functions map to `UNKNOWN`, not false.

Addition and multiplication flatten nested nodes and sort children by a local
total structural order: kind precedence, exact scalar payload or head name, and
lexicographically recursive children with arity as the final tie-break. Node
and name-table indices are excluded because they depend on construction
history. This makes printing, CSE traversal, and generated Fortran
byte-identical for identical expression trees constructed in different arenas.
The order is deliberately not inherited from a backend: GiNaC 1.8.10, for
example, documents that its internal canonical order is not a stable
user-visible serialization order.

The interning bucket table doubles and rehashes existing collision chains
before its average load exceeds one node per bucket. Rehashing never changes a
node index or argument slice, so existing `expr_t` handles remain valid.
The text pool used by symbols, function heads, and arbitrary exact payloads has
an independent resizing hash table; distinct large coefficients therefore do
not induce a quadratic linear scan.

Applied functions may have any arity. Native differentiation represents an
unknown partial derivative as
`DerivativeN(head, i1, ..., iN, arg1, ..., argM)`. Sorted derivative indices
give mixed partials one structural form under the usual smooth-function
convention. The Fortran kernel boundary makes the consumer contract explicit:
`kernel_spec_t%bindings` maps the applied head and derivative index tuple to a
consumer expression. An empty tuple binds the ordinary application; a
nonempty tuple binds a `DerivativeN` node. The replacement may be a component
or array expression, or a procedure-call template containing `%args%`. An
unbound applied head is refused before source emission, so generated Fortran
never silently invents an external procedure or variable.

Conditional scalar nodes use the same explicit boundary. `Piecewise` stores a
`List` of ordered `{value, condition}` pairs followed by its default value;
`If` and `Boole` carry their ordinary condition and branch arguments. The
native engine simplifies only exact numeric relations, differentiation is
branchwise, and the Fortran scalar emitter uses typed nested `merge` calls.
Predicates outside the supported relational/logical vocabulary are refused;
the derivative at a branch boundary is not asserted by the library.

## Backend contract

Every operation returns a result with:

- success or a diagnostic
- a value when the operation constructs an expression
- elapsed operation time
- `ZERO`, `NONZERO`, or `UNKNOWN` for decision procedures
- an explicit validity condition when a result is conditional

Domain assumptions and conditional results use an explicit context and
condition field. They must not be encoded in a backend-specific global state.
The assumption context records real, zero, negative, nonpositive, positive,
nonnegative, nonzero, and integer-domain facts on symbols and expressions.
Exact relations, conjunctions, and `Element` membership derive only
implications that are provably valid. Compound ingestion is transactional and
rejects contradictory facts with a diagnostic; cloned contexts provide scoped
refinement without mutating the parent. The public Fortran facade derives
value-style contexts with `with_assumption` and passes them explicitly to
native operations. `refine` is the named assumption-aware facade operation,
while guarded rewrite ownership remains in the native simplifier; unsupported
inference is refused rather than guessed.

External engines remain optional. A native operation must not silently invoke
an external engine. Benchmarks record native operation time, conversion time,
and total dispatch time separately.

Expensive native operations accept an optional `resource_limit_t` containing a
per-call node budget and an absolute monotonic deadline. Recursive simplifier
and expansion work charges the same call-local record, so a refusal does not
partially publish an expression or leak a limit into another engine/session.
`new_resource_limit` supplies the ordinary relative-seconds constructor; a
zero field means unlimited. The result diagnostic names the operation and the
limit that stopped it.

Native simplification reports a nonzero-denominator condition whenever a
rewrite changes an expression containing a symbolic negative integer power.
This conservative guard covers cancellation such as `x*x**(-1) -> 1`; the
condition bit is retained with the cached expression identifier so warm lookup
remains constant-time.

## Verification

The engine that proposes a simplification cannot be its sole verifier.
Polynomial operations use coefficient reconstruction, divisibility, or
finite-field evaluation as independent oracles. Differentiation uses closed
forms, finite differences, dual-number identities, adjoint identities, and
mixed-partial symmetry. Integration is checked by differentiating its result.
Generated kernels are compiled and compared numerically with an independent
evaluator.

Floating probes are evidence only. Reports include their sample points,
undefined-point count, tolerance, and seed. They are never described as exact
polynomial identity tests.

## Compatibility boundary

FortNum consumes the concrete API and byte-stable generated source. Changes to
the following require a FortNum compatibility run:

- arena, expression, parser, differentiation, and derivative-product APIs
- `engine_result_t` and the SymEngine factory
- kernel and Enzyme wrapper specifications
- CSE ordering, operation counts, banners, wrapping, literal spelling, shaped
  outputs, and device annotations

FortNum's local generator uses a path dependency while its provenance stamp
reads a lock file. A regeneration must verify that the dependency checkout
matches the lock before claiming that revision.

The latest revision-addressed FortNum and MHD1D compatibility probe, including
reviewed generated-source deltas and any lock or registration limitation, is
recorded in `consumer-verification.toml`.
