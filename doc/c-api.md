# C ABI

`src/capi/fortsym.h` is the public C contract. It exposes opaque arena and
expression handles, exact scalar constructors, function application, arithmetic,
inspection, substitution, and differentiation. The native library retains an
arena while any expression handle refers to it; callers may therefore release
the arena before releasing its expressions.

Every fallible operation returns a status and accepts a caller-owned diagnostic
buffer. No process-global error state is used. Text accessors report the
required buffer size, including the terminating NUL, and return a resource
status when the supplied buffer is too small.

Operations whose native implementation is not yet available are intentionally
absent from this ABI. They will be added as versioned API additions when their
native semantics and independent tests exist.
