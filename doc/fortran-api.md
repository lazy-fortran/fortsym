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

The default arena is process-local mutable state for single-threaded programs.
`default_arena()` returns it when an explicit API needs the same arena.
`reset()` clears it and invalidates every expression made from it.
Expressions from before a reset must be discarded before the arena is reused.

`symbols` assigns whitespace- or comma-separated names to scalar outputs:

```fortran
call symbols("mu sigma best xi", mu, sigma, best, xi, ok=good)
```

`good` is false when an output has no corresponding name or when extra names
remain. The output without a name is an invalid `expr_t`.

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
