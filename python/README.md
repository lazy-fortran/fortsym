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
| `Symbol`, `symbols`, `Integer`, `Rational`, `Float`, `pi`, `E`, `I`, `oo`, `zoo`, `nan` | exact native construction and structural equality; `Float(-0.0)` retains its IEEE sign, `nan` follows the declared propagation rules, and finite-scalar/integer-power `oo`/`zoo` rules match SymPy while symbolic factors remain unevaluated |
| `Expr.is_number`, `Expr.is_algebraic`, `Expr.is_rational`, `Expr.is_integer`, `Expr.is_zero`, `Expr.is_nonzero`, `Expr.is_real`, `Expr.is_positive`, `Expr.is_nonnegative`, `Expr.is_negative`, `Expr.is_nonpositive` | SymPy-compatible numeric and three-valued predicates backed by one native expression owner; `is_number` is Boolean, while the domain and sign predicates return `True`, `False`, or `None` |
| `algebraic=True`, `rational=True`, `integer=True`, `real=True`, `zero=True`, `positive=True`, `nonnegative=True`, `nonzero=True`, `negative=True`, `nonpositive=True` | native arena facts; algebraic, rational, and integer facts close over their supported exact domains, sign facts close to real/nonzero/zero implications, and contradictory combinations raise `InconsistentAssumptions` |
| `Q.algebraic`, `Q.rational`, `Q.integer`, `Q.real`, `Q.zero`, `Q.positive`, `Q.nonnegative`, `Q.nonzero`, `Q.negative`, `Q.nonpositive`, `ask`, `assuming`, `And` | nested, reversible assumption queries and transactional compound scopes backed by the native arena context; `Q.algebraic` uses the native algebraic result and preserves SymPy's `None` boundary for constructor-attached symbol assumptions and undecided function heads; bounded relational facts are accepted in scopes |
| `Add`, `Mul`, `Pow`, `Function` | native operator construction; universal power identities (`x**0`, `x**1`, `1**x` outside sentinel exponents, and `sqrt(x)**2`) are canonicalized in the shared arena, with `1**oo`, `1**zoo`, and `1**nan` becoming `nan`; `isinstance` checks use native node kinds |
| `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `sinh`, `cosh`, `tanh`, `csch`, `sech`, `coth`, `erf`, `erfc`, `gamma`, `loggamma`, `factorial`, `besselj`, `besseli`, `legendre`, `asinh`, `acosh`, `atanh`, `exp`, `log`, `sqrt`, `Abs`, `sign`, `floor`, `ceiling` | native applied-function nodes; direct sentinel rules for the declared heads match SymPy where the result is representable, including `sin(zoo)`, `cos(zoo)`, and `tan(zoo)` becoming `nan`, while accumulation-bound and pole-sensitive cases remain explicit refusals |
| `re`, `im`, `Abs`, `expand_complex`, `conjugate`, `arg` | native complex-domain projections, rectangular expansion, modulus, and principal argument; direct `oo`/`-oo`/`zoo`/`nan` sentinel boundaries match SymPy, while unknown reality, unresolved branches, and decidable zero arguments for `arg` raise `UnsupportedOperationError` |
| `diff`, `Derivative` | native differentiation, including repeated variables; `evaluate=False` retains a typed wrapper with `.doit()` |
| `subs`, `expand` | native substitution and expansion |
| `Subs` | typed wrapper with `.doit()` for explicit `(old, new)` pairs |
| `simplify`, `refine`, `factor` | native bounded simplification, principal-square-root powers, universal power-constructor identities, compact-rational `oo`/`zoo` powers and normalized positive rational `-oo` phases, signed/zero-guarded `sqrt`/`Abs` refinement, direct known-domain rules for the supported elementary heads, real/nonzero-guarded `log`/`exp` composition, and polynomial factorisation; unsupported domain rewrites and refinement assumptions are refused |
| `Eq`, `Ne`, `Gt`, `Ge`, `Lt`, `Le` and `Expr` comparisons | SymPy-compatible relational constructor spellings at the adapter boundary; exact sign/zero bounds and transactional `And` facts are ingested by native scopes |
| `together`, `cancel`, `apart`, `collect`, `integrate`, `limit`, `series`, `solve`, `Matrix` | explicit refusal until their semantics are covered |

For `atan2(y, x)`, the adapter evaluates the representable directed-infinity
quadrants for `y` and `x` both equal to `+oo` or `-oo`; complex infinity and
other ambiguous domain cases remain applied heads.
For the Bessel heads, `besselj(order, +/-oo)` is zero and
`besseli(order, oo)` is positive infinity. For symbolic order,
`besseli(order, -oo)` is `oo*(-1)**order`; integer orders reduce to signed
infinity, while unsupported exact non-integer and complex-infinity cases remain
applied heads. NaN in an order-bearing head remains applied unless an existing
directed-domain rule proves a result; `besseli(nan, -oo)` is the representable
`nan` boundary.
`legendre(degree, argument)` is the SymPy spelling for the native
`legendrep(degree, 0, argument)` owner. Its infinity rules cover nonnegative
integer degrees through two and order zero; degree three and higher map to
`nan` at infinity. Negative integer degrees use SymPy's
`P(-n-1, x) = P(n, x)` identity; symbolic, noninteger, and other unsupported
degrees remain applied heads.
The periodic heads `sin`, `cos`, and `tan` map complex infinity to `nan`.
Their `+/-oo` results are SymPy `AccumBounds` values and therefore remain
explicit applied heads until fortsym has a bounded-set representation.

The complex-domain functions `re`, `im`, `Abs`, `expand_complex`, `conjugate`, and `arg` use the same
native complex-domain owner rather than constructing duplicate applied heads.
Their immutable results are cached per assumption epoch; adding or removing
assumptions invalidates the cache. `arg` returns the principal branch when its
supported rectangular split is decidable. `expand_complex` uses that split for
the supported exact and assumption-resolved fragment and matches the direct
`oo`/`-oo`/`zoo`/`nan` sentinel boundaries (`zoo` becomes `nan`). The same
owner returns `re(oo)=oo`, `im(oo)=0`, `arg(-oo)=pi`, `Abs(zoo)=oo`, and the
corresponding `nan` projections; `conjugate(zoo)` remains an applied head.
`Abs` uses the same owner for exact, algebraic, and assumption-resolved complex
expressions, while unknown reality keeps SymPy's unevaluated `abs(...)`
fallback.
Repeated `diff` calls through the SymPy adapter likewise reuse the simplified
derivative of an immutable expression and variable; the raw low-level
`Expr.diff` and C-ABI derivative remain available separately.

The compatibility layer guarantees native structural equality only for
operations listed as construction or transformation above. Unsupported
assumption forms and relational bounds raise `UnsupportedOperationError`, while
contradictory sign, zero, and compound facts raise `InconsistentAssumptions`;
they are not silently ignored. Matrix,
ordering, and unevaluated-expression semantics remain outside the subset.
