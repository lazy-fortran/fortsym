# Exact algebraic value format

The bounded algebraic bridge represents an element of
\(\overline{\mathbb{Q}}\) as:

```text
qqbar1:<root-index>:<c0>,<c1>,...,<cd>
```

The coefficients are the primitive integer minimal polynomial
\(c_0+c_1x+\cdots+c_dx^d\). `root-index` is zero-based in FLINT's canonical
conjugate order: descending real roots first, then nonreal roots by descending
real part, ascending absolute imaginary part, and upper-half-plane root first.
This makes conjugates distinct without storing a rounded approximation.

Normalization accepts a bounded integer polynomial, computes all roots through
`qqbar_roots_fmpz_poly`, selects the requested sorted root, and serializes that
value's unique reduced minimal polynomial and conjugate index. Reducible and
repeated-root inputs therefore normalize to the minimal representation.
Malformed, out-of-range, or over-budget input is refused.

The bridge currently provides exact Gaussian-rational construction, the
imaginary unit, addition, subtraction, multiplication, division, conjugation,
signed integer powers, principal square roots, exact real/imaginary projections,
and exact real/imaginary sign predicates. Division by zero and negative powers
of zero are rejected before calling FLINT. Square root follows FLINT's
documented principal branch.

## Resource contract

- serialized input and output: at most 64 KiB
- algebraic degree: at most 32
- minimal-polynomial coefficient height: at most 4096 bits
- signed power exponent magnitude: at most 64
- binary arithmetic: FLINT's degree-product and height-sum preflight must pass

Results are checked again after each operation. A resource refusal returns an
empty value with `ok=.false.`; it is not a mathematical `false` result.
No `qqbar_t`, isolating enclosure, or FLINT allocation crosses the C boundary.

## Provenance and adaptation

The algorithm and branch semantics follow FLINT 3.6.0 `qqbar`, pinned at
revision
[`8d5454b96761fafe4d5a9da76a369a602f500f49`](https://github.com/flintlib/flint/tree/8d5454b96761fafe4d5a9da76a369a602f500f49).
The relevant primary documentation and implementation are
[`doc/source/qqbar.rst`](https://github.com/flintlib/flint/blob/8d5454b96761fafe4d5a9da76a369a602f500f49/doc/source/qqbar.rst)
and
[`src/qqbar`](https://github.com/flintlib/flint/tree/8d5454b96761fafe4d5a9da76a369a602f500f49/src/qqbar).
FLINT is LGPL-3.0-or-later and remains a shared linked dependency. The
`qqbar1` text envelope, ownership boundary, validation, and resource policy are
original fortsym wrappers; no upstream implementation code is copied.

`NK_ALGEBRAIC` now stores the bounded `qqbar1` value as an arena atom. Native
scalar arithmetic and the complex-domain `Re`, `Im`, and conjugation boundary
use it. Parser, printer round trips, SymEngine conversion, and code generation
integration remain incomplete and are the next coherent fragment.
