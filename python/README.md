# fortsym Python bindings

This package is a standard-library-only `ctypes` facade over the installed
fortsym C ABI. It does not import SymPy or depend on a Fortran compiler ABI.

Set `FORTSYM_LIBRARY` to the absolute path of the shared library when using a
wheel or an installed CMake tree. A source checkout can instead use
`PYTHONPATH=python`; the package then searches `build/lib` automatically.
The CMake install places the package sources under
`share/fortsym/python/fortsym`, so that directory can be added to `PYTHONPATH`.

Expression handles own their native references and are released by `close()`
or garbage collection. Exact integers and `fractions.Fraction` values are
passed as decimal strings when they do not fit the compact C ABI scalars.
The native `Expr.factor()` method and top-level `factor()` function expose
bounded polynomial factorisation; factorizations that would discard a domain
condition are refused by the C ABI.

## `fortsym.sympy` compatibility subset

`fortsym.sympy` is a drop-in import spelling for the declared subset below. It
does not import SymPy. Unsupported names raise
`fortsym.sympy.UnsupportedOperationError`; they do not return a guessed result.

| Surface | Supported semantics |
|---|---|
| `Symbol`, `symbols`, `Integer`, `Rational`, `Float`, `pi`, `E`, `I` | exact native construction and structural equality |
| `Expr.is_zero`, `Expr.is_nonzero`, `Expr.is_real`, `Expr.is_positive`, `Expr.is_nonnegative` | SymPy-compatible three-valued predicates backed by native zero and assumption queries (`True`, `False`, or `None`) |
| `real=True`, `positive=True`, `nonnegative=True`, `nonzero=True` | native arena facts; positive/nonnegative/real facts affect guarded simplification |
| `Add`, `Mul`, `Pow`, `Function` | native operator construction; `isinstance` checks use native node kinds |
| `sin`, `cos`, `tan`, `exp`, `log`, `sqrt`, `Abs` | native applied-function nodes |
| `diff`, `Derivative` | native differentiation, including repeated variables; `evaluate=False` retains a typed wrapper with `.doit()` |
| `subs`, `expand` | native substitution and expansion |
| `Subs` | typed wrapper with `.doit()` for explicit `(old, new)` pairs |
| `simplify`, `factor` | native bounded simplification and polynomial factorisation; domain-conditional factorisations are refused |
| `together`, `cancel`, `apart`, `collect`, `integrate`, `limit`, `series`, `solve`, `Matrix` | explicit refusal until their semantics are covered |

The compatibility layer guarantees native structural equality only for
operations listed as construction or transformation above. Assumptions outside
the listed four facts, false assumptions, and unsupported option combinations
raise `UnsupportedOperationError`; they are not silently ignored. Matrix,
ordering, and unevaluated-expression semantics remain outside the subset.
