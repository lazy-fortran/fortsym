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

The current exact number types use signed 64-bit numerators and denominators.
They do not provide arbitrary-precision arithmetic. Code that combines exact
numbers must detect overflow or decline the transformation. Arbitrary-precision
integer, rational, real, and complex domains are a backend milestone.

Addition and multiplication flatten nested nodes and sort child node indices.
This provides sharing inside one construction history. Node indices are not a
cross-arena canonical order. Stable serialization and deterministic semantic
ordering remain separate work.

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

Domain assumptions and conditional results need an explicit context and
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

