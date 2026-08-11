# Fortran API

`use fortsym` provides the expression type, arena type, constructors, operators,
and the expression functions under names such as `exp_expr`, `sqrt_expr`, and
`erf_expr`. The suffix keeps these names distinct from Fortran intrinsics.
The lower-level `fortsym_expr` and `fortsym_arena` modules remain available.

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
`fortsym_default_arena()` returns it when an explicit API needs the same arena.
`fortsym_reset()` clears it and invalidates every expression made from it.
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
explicit constructor receives `fortsym_default_arena()`.
