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
domain value, not a finite real literal: arithmetic and numerical evaluation
must apply their own infinity rules, and finite real Fortran kernel emission
refuses it.

```fortran
infinity = oo_expr(default_arena())
```

`zoo_expr` and `nan_expr` construct complex-infinity and undefined/NaN
sentinels. They are structural domain values, not floating-point payloads;
finite real emission refuses all three until operation-specific domain rules
are implemented.

```fortran
complex_infinity = zoo_expr(default_arena())
undefined = nan_expr(default_arena())
```

Native simplification propagates `nan` through addition, multiplication, the
supported numeric unary heads, and powers. The defined power exception is
`nan**0 = 1`; a NaN base or exponent in every other supported power produces
`nan`. Unknown function heads remain structural and are not assigned guessed
domain rules. Directed arithmetic for `oo` and `zoo` is a separate roadmap
step.

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

The lower-level `fortsym_complexdom` module provides `re_part`, `im_part`, and
`conjugate` with an explicit assumption context. Algebraic atoms are accepted
there through FLINT's exact real and imaginary projections, and conjugation is
exact. Structural conjugation commutes with the supported heads `exp`, `sin`,
`cos`, `sinh`, `cosh`, `tan`, and `tanh` wherever they are defined. Rectangular
splitting also covers the entire heads `exp`, `sin`, `cos`,
`sinh`, and `cosh` through their addition identities. `tanh` is also split
through its rectangular quotient, and `tan` through the corresponding
meromorphic quotient; `log` uses the principal `log(abs) + i*Arg` form, and
`sqrt` uses the principal polar half-angle form. An identically zero
denominator or logarithm argument is refused, while a nontrivial denominator
remains in the result to preserve pointwise poles. The same branch and reality
refusals apply to other unsupported expressions.

`real_text_expr` retains a bounded finite decimal literal as `NK_BIG_REAL`.
The arena validates its decimal syntax and preserves the original digits rather
than converting the value through `real64`.

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
sign and foreign-arena relations are refused.

The supported constructors are `real_valued`, `zero`, `negative`,
`nonpositive`, `positive`, `nonnegative`, and `nonzero`. Sign facts close over
their sound implications; nonnegative plus nonpositive infers zero, while
contradictory facts are refused with an explanatory `ok`/diagnostic result.
The native guarded simplifier uses these facts for `sqrt(x**2)` and `abs(x)`:
positive/nonnegative values return `x`, negative/nonpositive values return
`-x`, and zero returns `0`; unknown reality remains unevaluated.
It also reduces `log(exp(x))` when `x` is real and `exp(log(x))` when `x` is
nonzero. Without the required fact, these compositions remain unevaluated so
branch-sensitive identities are never guessed.
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
