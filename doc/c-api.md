# C ABI

`src/capi/fortsym.h` is the public C contract (ABI version 9). It exposes opaque arena and
expression handles, exact scalar constructors, function application, arithmetic,
inspection, substitution, and differentiation. The native library retains an
arena while any expression handle refers to it; callers may therefore release
the arena before releasing its expressions.

`fortsym_zero_test` exposes the native three-valued query without creating a
new expression handle. It returns `FORTSYM_ZERO_TRUE` for a proved zero,
`FORTSYM_ZERO_FALSE` for a proved nonzero expression, and
`FORTSYM_ZERO_UNKNOWN` when the native decision procedure declines to decide.
An unknown verdict is still a successful C-ABI call; malformed handles and
unsupported execution return the ordinary status codes.

`fortsym_assumption_has` reports whether the arena proves one of the supported
facts (`real`, `zero`, `negative`, `nonpositive`, `positive`, `nonnegative`, or
`nonzero`) for an expression. Its `known` output is one for a proven fact and
zero for an unknown fact; absence is not a proof of the opposite predicate.

`FORTSYM_CONFLICT` reports contradictory assumptions. Compound relation
ingestion is transactional: a failed child does not leave facts from earlier
children in the arena, and the diagnostic buffer contains the conflict reason.

`fortsym_assumption_push` and `fortsym_assumption_pop` provide nested,
exception-safe scopes over the arena's assumption context. Push clones the
current facts; assumptions recorded after the push are discarded by the
matching pop. A pop without a matching push returns
`FORTSYM_INVALID_ARGUMENT`. Expression handles remain valid across both
operations.

`fortsym_complex_operation` exposes the supported native complex-domain
operations `re`, `im`, `abs`, `conjugate`, and `arg`. It returns a new expression when
the expression's reality, branch, and singularity conditions are decidable;
otherwise it returns `FORTSYM_UNSUPPORTED` with the native refusal reason.

Every fallible operation returns a status and accepts a caller-owned diagnostic
buffer. No process-global error state is used. Text accessors report the
required buffer size, including the terminating NUL, and return a resource
status when the supplied buffer is too small.

Operations whose native implementation is not yet available are intentionally
absent from this ABI. They will be added as versioned API additions when their
native semantics and independent tests exist.
