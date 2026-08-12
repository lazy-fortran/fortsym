# Fortran API

`use fortsym` provides the expression type, arena type, constructors, operators,
and the expression functions under their ordinary Fortran names such as `exp`,
`sqrt`, and `erf`. The facade follows the same vocabulary as `fortsym_expr`;
the generic interfaces accept expression handles while the language's intrinsic
names remain the natural spelling for native symbolic expressions.
The lower-level `fortsym_expr` and `fortsym_arena` modules remain available.

Symbol names follow one rule: name the symbol as you would name the Fortran
variable, using the spelling that the printer reads. Names such as `varphi_2`,
`gamma_1`, `theta_bar`, and `B_r_hat` work as symbolic names, Fortran
identifiers, and readable typeset input. Never put LaTeX markup in a symbol
name. A name such as `\varphi_2` is the arena identity and is emitted verbatim
by every dialect. Use the notation override map when the desired notation
cannot be a Fortran identifier.

Fortran emission validates symbol names at the source boundary. Invalid names
such as `\[Alpha]` or `Global`x` are refused with a diagnostic rather than
written into generated source. Symbolic names remain case-sensitive; when a
kernel contains both `Gamma` and `gamma` (or `B` and `b`), only the colliding
Fortran spellings are deterministically renamed and the generated source
records the mapping in a comment.

## Default arena

Assigning character data to an `expr_t` creates one symbol. The right-hand side
is treated as a name, so `x = "x+y"` creates a symbol named `x+y` and does not
parse an expression.

```fortran
use fortsym
type(expr_t) :: mu, sigma, e

mu = "mu"
sigma = "sigma"
e = (mu + 1) / sigma
```

The module owns one saved default arena for the process lifetime. It is a
single-threaded convenience state. `default_arena()` returns the same arena
when an explicit constructor needs to share that state. `reset()` is safe
before first use and can be called more than once. It clears the node store and
invalidates every expression made before the reset, including handles whose
node index is reused later. The arena pointer remains usable after reset.
Discard pre-reset expressions at every reset boundary. Use explicit arenas for
concurrency, isolation, and library code that outlives one problem.

`symbols` assigns whitespace- or comma-separated names to scalar outputs:

```fortran
call symbols("mu sigma best xi", mu, sigma, best, xi, ok=good)
```

`good` is false when an output has no corresponding name or when extra names
remain. The output without a name is an invalid `expr_t`.

`num` and `rat` construct compact exact integers and rationals. `exact` accepts
an arbitrary-size integer or rational string and canonicalizes it in the same
arena:

```fortran
huge = exact(default_arena(), "9223372036854775808", ok=good)
half = exact(default_arena(), "6/-8", ok=good)
```

`oo_expr` constructs the positive-infinity sentinel. It remains a structural
domain value, not a finite real literal. Native simplification currently
matches the finite scalar and integer-power rules for `oo` and `zoo`: known
signs determine directed products, `0*oo` and `0*zoo` become `nan`, and
symbolic products such as `oo*x` remain unevaluated. Finite real Fortran
kernel emission still refuses these domain values.

```fortran
infinity = oo_expr(default_arena())
```

`zoo_expr` and `nan_expr` construct complex-infinity and undefined/NaN
sentinels. They are structural domain values, not floating-point payloads;
the supported native domain rules include the finite scalar and integer-power
cases above, plus the declared `nan` propagation rules. Finite real emission
refuses all three.

```fortran
complex_infinity = zoo_expr(default_arena())
undefined = nan_expr(default_arena())
```

Native simplification propagates `nan` through addition, multiplication, the
supported numeric unary heads, and powers. Universal construction identities
are canonicalized in the shared arena: `x**0 = 1`, `x**1 = x`, and
`1**x = 1` except for the
`oo`/`zoo`/`nan` exponents, which produce `nan`, and `sqrt(x)**2 = x`. The
defined sentinel power exception is `nan**0 = 1`; a NaN base or exponent in every
other supported power produces `nan`. Unknown function heads remain structural
and are not assigned guessed
domain rules. Non-integer powers, symbolic-factor products, and
operation-specific function/limit rules remain separate roadmap work. The
current unary fragment covers `sqrt`, `abs`, `exp`, and `log` on known domain
sentinels; unknown factors remain unevaluated, and `sqrt(-oo)` is represented
as the structural complex product `i*oo`. Compact rational powers of `oo` and
`zoo` are also handled. Positive rational powers of `-oo` retain SymPy's
normalized principal phase, such as `(-oo)**(2/3) = oo*(-1)**(2/3)` and
`(-oo)**(4/3) = -oo*(-1)**(1/3)`; negative rational powers remain `0`.
Principal square roots of exact negative perfect-square rationals are also
canonicalized, so `sqrt(-1)=i` and `sqrt(-4)=i*2`; irrational negative roots
remain unevaluated.
Direct domain rules also cover `sign`, `floor`, `ceiling`, `sinh`, `cosh`, and
`tanh`; `sign(zoo)` stays unevaluated because its value is not determined.
The periodic heads `sin`, `cos`, and `tan` map `zoo` to `nan`; their `+/-oo`
results are SymPy accumulation bounds and remain explicit applied heads until
fortsym has a bounded-set representation.
The inverse heads `asin`, `acos`, `atan`, `asinh`, `acosh`, and `atanh` also
match the representable `oo`/`-oo` results from SymPy 1.14.0. The
accumulation-bound results for `atan(zoo)` and `atanh(zoo)` remain applied
heads until a compatible bounded-set representation exists.
The exact real poles `atanh(1)` and `atanh(-1)` are canonicalized to `oo` and
`-oo`; unsupported complex and accumulation-bound cases remain unevaluated.
The exact principal branch points `acosh(0)` and `acosh(-1)` are likewise
canonicalized to `i*pi/2` and `i*pi`; other negative-real branches remain
unevaluated.
The exact imaginary branch points `asinh(i)` and `asinh(-i)` are canonicalized
to `i*pi/2` and `-i*pi/2`; broader complex inverse branches remain unevaluated.
The reciprocal-hyperbolic heads `csch`, `sech`, and `coth` likewise have
direct scalar rules: `csch` and `sech` tend to zero, while `coth` tends to
the corresponding signed one; every `zoo` case becomes `nan`.
The error-function heads `erf` and `erfc` have the scalar limits
`erf(oo)=1`, `erf(-oo)=-1`, `erfc(oo)=0`, and `erfc(-oo)=2`; their `zoo`
applications remain unevaluated.
The gamma heads add the representable limits `gamma(oo)=oo` and
`loggamma(oo)=oo`; `loggamma(-oo)` and `loggamma(zoo)` are `zoo`, while
pole-sensitive `gamma(-oo)` and `gamma(zoo)` remain unevaluated.
The shared positive-infinity branch also gives `factorial(oo)=oo`; its
negative and complex-infinity applications remain unevaluated.
The two-argument `atan2` head evaluates the directed `(+/-oo, +/-oo)`
quadrants (`0`, `pi`, and `-pi`); complex-infinity and other ambiguous pairs
remain applied heads.
The Bessel heads add `besselj(order, +/-oo)=0` and
`besseli(order, oo)=oo`. Symbolic order at negative infinity uses the phase
`oo*(-1)**order`, and integer orders reduce to signed infinity; unsupported
exact non-integer and complex-infinity cases remain applied heads. NaN is not
generically propagated through the
order-bearing Bessel and Legendre heads: unresolved NaN arguments remain
applied, while `besseli(nan, -oo)` follows SymPy's representable `nan` result.
Native `legendrep(degree, order, argument)` keeps its Fortran spelling; the
adapter's `legendre(degree, argument)` maps to order zero. Nonnegative integer
degrees through two at `+/-oo` and `zoo` use the representable parity/domain
rules; degree three and higher become `nan`. Negative integer degrees use the
`P(-n-1, x) = P(n, x)` identity, while other degrees and orders remain applied
heads.

The exact integer and rational fragment preserves canonical values through
construction and native arithmetic. Exact complex or algebraic values remain a
separate domain from ordinary `num`, `rat`, and `exact` leaves. Construct a
canonical FLINT `qqbar1` value with `algebraic_expr`:

```fortran
type(expr_t) :: root
root = algebraic_expr(default_arena(), qqbar_text, ok=good)
```

`algebraic_expr` retains the exact value as an `NK_ALGEBRAIC` arena atom, and
`root%algebraic_text()` returns its canonical `qqbar1` spelling. The native
engine combines pure algebraic expressions with exact `+`, `*`, and integer
powers. Native simplification also combines algebraic coefficients in mixed
expressions and uses the FLINT sign oracle for algebraic zero, one, and
definitely-nonzero guards. `real64`
expression
evaluation refuses algebraic atoms. Fortran kernel emission accepts exact real
algebraic atoms through a checked `algebraic_to_real` projection. The
projection uses FLINT's rigorous Arb enclosure and refuses non-real, subnormal,
overflow, and ambiguous-rounding values. SymEngine accepts atoms whose exact
real and imaginary components are rational and converts them to an exact
`re + im*I` expression. Higher-degree or otherwise non-rational atoms retain a
named refusal. `print_expr` displays the canonical payload, and the
native/backend text parsers accept it as one opaque lossless token; it is not a
Fortran source literal.

The lower-level `fortsym_complexdom` module provides `re_part`, `im_part`,
`abs_of`, `conjugate`, and `complex_expand` with an explicit assumption context. Algebraic atoms are accepted
there through FLINT's exact real and imaginary projections, and conjugation is
exact. Structural conjugation commutes with the supported heads `exp`, `sin`,
`cos`, `sinh`, `cosh`, `tan`, and `tanh` wherever they are defined. Rectangular
splitting also covers the entire heads `exp`, `sin`, `cos`,
`sinh`, and `cosh` through their addition identities. `tanh` is also split
through its rectangular quotient, and `tan` through the corresponding
meromorphic quotient; `log` uses the principal `log(abs) + i*Arg` form, and
`sqrt` uses the principal polar half-angle form. An identically zero
denominator or logarithm argument is refused, while a nontrivial denominator
remains in the result to preserve pointwise poles. `complex_expand` preserves
`oo` and `nan` and maps `zoo` to `nan`, matching SymPy's defined sentinel
boundary. The direct projections also match the defined sentinel cases:
`re(oo)=oo`, `re(-oo)=-oo`, `im(oo)=0`, `arg(-oo)=pi`, `Abs(zoo)=oo`, and
undefined projections return `nan`; `conjugate(zoo)` remains an applied head.
The same branch and reality refusals apply to other unsupported expressions.

The main `fortsym` facade exposes the same owner as `re_part`, `im_part`,
`abs_of`, `conjugate`, `arg_of`, and `complex_expand`, each returning the common `engine_result_t`. The
facade creates a local assumption context when none is supplied, while an
explicit context must belong to the expression's arena. The C ABI and
`fortsym.sympy` adapter provide the corresponding `re`/`im`/`Abs`/`conjugate`/`arg`/`expand_complex`
boundary and preserve the complex-domain refusal diagnostics.

`real_text_expr` retains a bounded finite decimal literal as `NK_BIG_REAL`.
The arena validates its decimal syntax and preserves the original digits rather
than converting the value through `real64`.

The `fortsym_predicates` module exposes `is_number(expression)`. It returns true for
numeric atoms, named constants, domain sentinels, exact algebraic atoms, and
compound expressions whose children are all numeric. Symbols and relation
objects return false. The C ABI and Python compatibility layer call this same
native owner.

It also exposes `is_algebraic(expression, assumptions)`, which returns the
shared `VERDICT_TRUE`, `VERDICT_FALSE`, or `VERDICT_UNKNOWN` values. Exact
integers, rationals, FLINT algebraic atoms, `I`, and supported exact rational
powers are proved algebraic; `pi`, `e`, and supported transcendental heads are
proved non-algebraic; machine reals and unresolved symbols remain unknown.
The optional immutable assumption context can prove `algebraic_valued`
symbols. The public `fortsym` facade re-exports the predicate and the existing
shared verdict constants, so there is no second predicate-specific verdict
vocabulary.

Requested-precision numeric evaluation uses one generic with two result forms.
Pass a character variable for the decimal text, or pass `numeric_real_text_t`
to retain the requested precision with the value:

```fortran
type(numeric_real_text_t) :: numeric
call numeric_precision_text(pi_expr(default_arena()), 40, numeric, good, why)
```

`numeric%digits` records the requested decimal precision.

Accuracy measurement for a caller-owned real64 kernel lives in
`fortsym_accuracy`, not in the numeric result type. `measure_accuracy` samples
the declared input matrix, evaluates the substituted expression through the
MPFR reference path, and reports maximum and RMS local-ULP error. The report
also retains the input that reached the maximum, the reference and observed
values, the derivative-based condition number when it can be evaluated, and
the count of refused samples. This is an independently checked bound over the
declared sample set. It is evidence for that set, not a proof over every real
input.

## Explicit arenas

An explicit arena remains the first-class API for independent problems and
concurrent work:

```fortran
type(arena_t), target :: a
type(expr_t) :: x

call a%init()
x = sym(a, "x")
```

Expressions built with an explicit arena can use every operator and expression
function exported by `fortsym`. Expressions combined by an operator must belong
to the same arena. The default and explicit forms can be mixed when the
explicit constructor receives `default_arena()`.
The core operations use the expression's owning arena, so the same `subs`,
`diff`, `simplify`, `refine`, `expand`, and `factor` calls work without an
explicit-arena variant or a second calling syntax.

## Explicit assumption contexts

The facade provides value-style assumption contexts for isolated derivations.
`make_assumption_context` creates an empty context for an arena, and
`with_assumption` returns a copied context with one supported fact or relation.
The parent is unchanged, so sibling contexts can be used independently:

```fortran
type(assumption_context_t) :: base, positive_x, nonnegative_y
logical :: good

base = make_assumption_context(a)
positive_x = with_assumption(base, positive(x), good)
nonnegative_y = with_assumption(base, nonnegative(y), good)
result = simplify(sqrt(x**2), assumptions=positive_x)
```

The same value-style constructor accepts bounded relational expressions from
the facade. Positive/nonnegative facts are derived from exact positive or zero
lower bounds; negative/nonpositive facts are derived from exact negative or
zero upper bounds:

```fortran
relation_context = with_assumption(base, greater(x, num(a, 1)), good)
result = refine(sqrt(x**2), assumptions=relation_context)
```

`equal`, `unequal`, `less`, `less_equal`, `greater`, and `greater_equal` keep
the native relation vocabulary in snake_case. Exact equality records zero or
the corresponding sign, `unequal(expression, 0)` records nonzero, and `And`
relations are ingested transactionally. Bounds that do not imply a supported
sign and foreign-arena relations are refused. `Element(x, Rationals)`,
`Element(x, Integers)`, and `Element(x, PositiveIntegers)` are also accepted
through the same native fact owner.

The supported constructors are `real_valued`, `rational_valued`,
`integer_valued`, `positive_integer`, `zero`, `negative`, `nonpositive`,
`positive`, `nonnegative`, and `nonzero`. Rational facts imply real, integer
facts imply rational and real, and the `positive_integer` shorthand implies
integer, rational, and positive. Sign facts close over
their sound implications; nonnegative plus nonpositive infers zero, while
contradictory facts are refused with an explanatory `ok`/diagnostic result.
The native guarded simplifier uses these facts for `sqrt(x**2)` and `abs(x)`:
positive/nonnegative values return `x`, negative/nonpositive values return
`-x`, and zero returns `0`; unknown reality remains unevaluated.
It also reduces `log(exp(x))` when `x` is real and `exp(log(x))` when `x` is
nonzero. Without the required fact, these compositions remain unevaluated so
branch-sensitive identities are never guessed.
The exact singularity `simplify(log(0))` follows SymPy and returns `zoo`; the
existing sentinel propagation therefore returns `nan` for
`simplify(exp(log(0)))`. Numeric real evaluation and complex rectangular
splitting still refuse the pole rather than claiming a finite value.
Exact negative real arguments use the principal logarithm branch: for example,
`simplify(log(-2))` returns `log(2) + i*pi`, while unsupported non-exact branch
cases remain unevaluated.
Under the principal-square-root convention, `sqrt(x)**2` reduces to `x` for
symbolic `x` without an additional assumption; this is distinct from the
branch-sensitive `sqrt(x**2)` rewrite above.
`simplify`, `refine`, `expand`, `factor`, `diff`, and `zero_test`
accept the optional `assumptions=` context. `refine` is the named entry point
for applying these supported facts; it shares guarded rewrite ownership with
the native simplifier. A context from another arena is refused with a
diagnostic. The default facade remains unchanged and does not consult an
implicit process-global context.

## Core operations

The easy facade uses the same short names as the owning modules:

```fortran
type(engine_result_t) :: result

f = (x + 1) * (x + 2)
result = subs(f, x, y)
g = result%value
result = diff(f, x)
d = result%value
result = simplify(f)
s = result%value
result = refine(sqrt(x**2), assumptions=positive_x)
r = result%value
result = expand(f)
e = result%value
result = factor(f)
p = result%value
```

`diff` is the evaluated native derivative. The `fortsym_diff` module remains
available for the deliberately unsimplified derivative DAG. `subs` is structural and
simultaneous for its one replacement pair. `simplify`, `refine`, `expand`, and
`factor` use the native engine in the expression's arena. All six functions
return the
same `engine_result_t` as the native engine. `%ok` reports whether the operation
succeeded, `%value` contains the resulting expression, and `%message` contains a
diagnostic on refusal. Native conditional results may also populate
`%conditional` and `%condition`. Use `%value` only after checking `%ok`.

## Zero query

`zero_test(expression)` is the one symbolic zero query in the easy facade. It
returns an `engine_result_t`; `%ok` reports whether the query executed and
`%verdict` is `VERDICT_TRUE` for a proved zero, `VERDICT_FALSE` for a proved
nonzero expression, and `VERDICT_UNKNOWN` when the native engine declines to
decide. `%value` contains the expression result produced by the engine when the
query succeeds.
`verdict_name` renders those outcomes as `ZERO`, `NONZERO`, and `UNKNOWN`.
`fortsym_check` contains the assertion helpers and numeric probe. They are test
utilities, not additional symbolic predicates.
