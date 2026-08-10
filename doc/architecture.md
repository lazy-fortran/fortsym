# CAS backend architecture

## Scope

fortsym targets symbolic work that produces or verifies Fortran kernels. The
supported fragment is versioned and explicit. General zero equivalence for
elementary functions is undecidable, so an engine must return `UNKNOWN` outside
its decision procedures rather than infer a Boolean answer.

The public expression type is independent of every backend. `arena_t` stores a
hash-consed directed acyclic graph, and `expr_t` holds an arena pointer and a
node index. Backends translate this graph to their own representation. The
native backend operates on it directly.

## Layers

1. `src/core` owns expression identity, exact scalar literals, applied
   functions, and substitution.
2. `src/io` parses and prints canonical expressions and backend dialects.
3. `src/calculus` implements mechanical differentiation and contracted
   derivative products.
4. `src/engine` declares operations and wraps native or external algorithms.
5. `src/algebra` implements deterministic exact operations over expression
   arrays, beginning with dense rational linear systems.
6. `src/council` compares supported engine answers and records disagreement.
7. `src/verify` supplies independent real evaluation and three-valued checks.
8. `src/codegen` selects shared subexpressions and emits Fortran kernels.

Kernel emission can also return an optional machine-readable operation-cost
record through `emit_kernel(..., cost_record=record)`. The record uses the
`fortsym.operation_cost.v1` JSON schema: totals count distinct nodes in the
union of the hash-consed output DAGs, `roots` count each output independently,
and `transcendentals` reports named function heads separately. `flops` counts
additions, multiplications, and divisions; `instructions` is the corresponding
structural operation count after collapsing direct multiply-plus-add patterns
into `fma_candidates`. These are symbolic cost metadata, not a disassembly or
runtime claim.

An engine capability is a promise that its corresponding type-bound operation
is callable. A capability bit without an operation entry point is invalid and
must not be used for dispatch.

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
constants, real probing when representable, and SymEngine conversion accept
both representations. A real64 Fortran kernel projects an arbitrary exact
value in the finite normal binary64 range through a 53-bit MPFR 4.2.2 value
with nearest-even rounding and emits one short typed literal. Subnormal and
overflow-range projections are conservatively refused. `print_expr_in`,
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
convention.

## Backend contract

Every operation returns a result with:

- success or a diagnostic
- a value when the operation constructs an expression
- elapsed operation time
- `ZERO`, `NONZERO`, or `UNKNOWN` for decision procedures
- an explicit validity condition when a result is conditional

Domain assumptions and conditional results use an explicit context and
condition field. They must not be encoded in a backend-specific global state.
The assumption context will begin with `real`, `positive`, `nonnegative`, and
`nonzero` facts on symbols and expressions.

External engines remain optional. A native operation must not silently invoke
an external engine. Benchmarks record native operation time, conversion time,
and total dispatch time separately.

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
