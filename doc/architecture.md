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
5. `src/council` compares supported engine answers and records disagreement.
6. `src/verify` supplies independent real evaluation and three-valued checks.
7. `src/codegen` selects shared subexpressions and emits Fortran kernels.

An engine capability is a promise that its corresponding type-bound operation
is callable. Capability bits without an operation entry point are legacy
declarations and must not be used for dispatch.

## Representation constraints

The current arena number nodes use signed 64-bit numerators and denominators.
Code that combines those nodes must detect overflow or decline the
transformation. The arbitrary-precision scalar bridge now normalizes and
computes base-ten integer/rational values through the shared FLINT 3.6.0
`fmpq` C interface pinned in `upstream-baselines.toml`; values return through a
single-slot thread-local render/fetch boundary and are copied immediately, so
no FLINT object or allocation enters the Fortran representation. Input,
rendered-output, and power-exponent budgets bound memory before large powers
are constructed. The `fo` test gate also resolves FLINT's `fmpq_add` symbol and
requires it to come from a shared object; CMake rejects a static archive at
configuration. Promoting arena storage and native simplification onto that
bridge is the next exact-domain step. Exact algebraic complex values use bounded
`qqbar` objects (minimal polynomial plus isolating enclosure), where the bounds
are fortsym resource limits rather than an intrinsic restriction of `qqbar`.
Rigorous
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
