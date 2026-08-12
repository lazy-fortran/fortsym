"""A deliberately declared SymPy-compatible subset.

This module is a syntax adapter over :mod:`fortsym`, not a SymPy reimplementation
and never imports SymPy.  Names outside the table in ``python/README.md`` raise
``UnsupportedOperationError`` instead of silently changing the calculation.
"""

from __future__ import annotations

from fractions import Fraction

from .. import (
    Arena, Expr, FortSymError, _Assumption, _CONFLICT, _FACT_INTEGER,
    _FACT_RATIONAL, _FACT_ALGEBRAIC, _default,
)


class UnsupportedOperationError(NotImplementedError):
    """The requested SymPy operation is outside fortsym's declared subset."""


class InconsistentAssumptions(ValueError):
    """The supplied assumptions cannot hold simultaneously."""


def _coerce(value):
    if isinstance(value, Expr):
        return _default()._check(value)
    if isinstance(value, Fraction):
        return Rational(value.numerator, value.denominator)
    if isinstance(value, bool):
        return Integer(int(value))
    if isinstance(value, int):
        return Integer(value)
    if isinstance(value, float):
        return Float(value)
    raise TypeError(f"cannot sympify {type(value).__name__}")


def sympify(value):
    if isinstance(value, str):
        if value.isidentifier():
            return Symbol(value)
        raise UnsupportedOperationError("sympify of source expressions")
    return _coerce(value)


class _KindMeta(type):
    _kinds = frozenset()

    def __instancecheck__(cls, instance):
        return isinstance(instance, Expr) and instance.kind in cls._kinds


_ASSUMPTION_FACTS = {
    "integer": _FACT_INTEGER,
    "rational": _FACT_RATIONAL,
    "algebraic": _FACT_ALGEBRAIC,
    "real": 1,
    "zero": 64,
    "negative": 128,
    "nonpositive": 256,
    "positive": 2,
    "nonnegative": 4,
    "nonzero": 8,
}

_ALGEBRAIC_FACTS = _FACT_ALGEBRAIC | _FACT_INTEGER | _FACT_RATIONAL
_Q_ALGEBRAIC_UNDECIDED_HEADS = frozenset({
    "gamma", "loggamma", "factorial", "besselj", "besseli",
    "legendrep", "legendreq",
})

# These are the class-order slots used by SymPy 1.14 for the supported named
# heads.  The adapter keeps this ordering local to unordered mapping handling;
# the native expression owner remains responsible for matching and rebuilding.
_SYMPY_FUNCTION_ORDER = {
    "exp": 10,
    "log": 11,
    "sin": 20,
    "cos": 21,
    "tan": 22,
    "sinh": 30,
    "cosh": 31,
    "tanh": 32,
    "coth": 33,
    "conjugate": 40,
    "re": 41,
    "im": 42,
    "arg": 43,
}
_ONE_COEFFICIENT = (0, Fraction(1))


def _number_key(expression):
    text = expression.exact_text
    if text:
        try:
            return (0, Fraction(text))
        except (ValueError, ZeroDivisionError):
            pass
    return (1, str(expression))


def _coefficient_key(value):
    if isinstance(value, Expr):
        return _number_key(value)
    if isinstance(value, Fraction):
        return (0, value)
    return (1, str(value))


def _children_key(expression):
    children = expression.args
    try:
        return tuple(_sympy_sort_key(child) for child in children)
    finally:
        for child in children:
            child.close()


def _power_sort_key(base, exponent):
    base_key = _sympy_sort_key(base)
    return base_key[:4] + (_coefficient_key(exponent),)


def _sympy_sort_key(expression):
    cached = getattr(expression, "_sympy_sort_key_cache", None)
    if cached is not None:
        return cached
    result = _uncached_sympy_sort_key(expression)
    expression._sympy_sort_key_cache = result
    return result


def _uncached_sympy_sort_key(expression):
    """Return the supported fragment of SymPy's default sort key."""
    kind = expression.kind
    if kind in (1, 2, 3, 10, 11, 12, 13):
        return (1, kind, "Number", (), _number_key(expression))
    if kind == 4:
        return (2, 0, "Symbol", (expression.name,), _ONE_COEFFICIENT)
    if kind == 5:
        constant_order = {"oo": 3, "zoo": 4, "nan": 5,
                          "e": 10, "pi": 11, "i": 12}
        return (1, constant_order.get(expression.name, 10000),
                expression.name, (), _ONE_COEFFICIENT)
    if kind == 6:
        return (3, 1, "Add", (expression.arity, _children_key(expression)),
                _ONE_COEFFICIENT)
    if kind == 7:
        return (3, 0, "Mul", (expression.arity, _children_key(expression)),
                _ONE_COEFFICIENT)
    if kind == 8:
        children = expression.args
        try:
            return _power_sort_key(children[0], children[1])
        finally:
            for child in children:
                child.close()
    if kind == 9:
        name = expression.name
        if name == "sqrt" and expression.arity == 1:
            children = expression.args
            try:
                return _power_sort_key(children[0], Fraction(1, 2))
            finally:
                for child in children:
                    child.close()
        return (4, _SYMPY_FUNCTION_ORDER.get(name, 10000), name,
                (expression.arity, _children_key(expression)),
                _ONE_COEFFICIENT)
    return (100, kind, str(expression), (), _ONE_COEFFICIENT)


def _owned_substitution_value(value):
    expression = sympify(value)
    return expression, None if isinstance(value, Expr) else expression


def _ordered_substitutions(substitutions):
    """Sort an unordered mapping as SymPy does before replacement."""
    pairs = []
    temporaries = []
    signature = tuple((id(old), id(new))
                      for old, new in substitutions.items())
    for old, new in substitutions.items():
        old_expression, old_temporary = _owned_substitution_value(old)
        new_expression, new_temporary = _owned_substitution_value(new)
        pairs.append((old_expression, new_expression))
        if old_temporary is not None:
            temporaries.append(old_temporary)
        if new_temporary is not None:
            temporaries.append(new_temporary)
    pairs.sort(key=lambda pair: (-pair[0].node_count,
                                 _sympy_sort_key(pair[0])))
    return pairs, temporaries, signature


def _can_use_subs_many(expression, pairs, signature):
    """Prove that atomic replacements cannot cascade into another key."""
    cache = getattr(expression, "_subs_many_safe_cache", None)
    if cache is not None and signature in cache:
        return cache[signature]
    atomic_kinds = frozenset({1, 2, 3, 4, 5, 10, 11, 12, 13})
    safe = all(new.kind in atomic_kinds for _, new in pairs)
    if safe:
        safe = all(new != old for old, _ in pairs for _, new in pairs)
    if cache is None:
        cache = {}
        expression._subs_many_safe_cache = cache
    if signature not in cache and len(cache) >= 8:
        cache.pop(next(iter(cache)))
    cache[signature] = safe
    return safe


def _cached_subs_many(expression, pairs, substitutions, signature):
    cache = getattr(expression, "_sympy_subs_cache", None)
    if cache is None:
        cache = {}
        expression._sympy_subs_cache = cache
    key = (expression._arena._assumption_epoch, signature)
    inputs = tuple(substitutions.items())
    cached = cache.get(key)
    if cached is not None:
        cached_inputs, cached_result = cached
        if (all(left is cached_left and right is cached_right
                for (left, right), (cached_left, cached_right)
                in zip(inputs, cached_inputs)) and
                cached_result._handle is not None):
            return cached_result
    old = tuple(pair[0] for pair in pairs)
    replacement = tuple(pair[1] for pair in pairs)
    result = expression._subs_many(old, replacement)
    if key not in cache and len(cache) >= 8:
        cache.pop(next(iter(cache)))
    cache[key] = (inputs, result)
    return result


def _apply_assumptions(expression, assumptions):
    pending = []
    for name, value in assumptions.items():
        if name == "commutative" and value is True:
            continue
        if name not in _ASSUMPTION_FACTS or value is not True:
            raise UnsupportedOperationError(
                f"symbol assumption {name}={value!r}"
            )
        pending.append(_Assumption(
            expression, _ASSUMPTION_FACTS[name], name
        ))
    if pending:
        try:
            with _default().assuming(*pending):
                pass
        except FortSymError as error:
            if error.status == _CONFLICT:
                raise InconsistentAssumptions(str(error)) from error
            raise
        for fact in pending:
            _default().assume(fact.expression, fact.fact)
            fact.expression._known_facts |= fact.fact
    return expression


class _AssumptionPredicate:
    __slots__ = ("_fact", "_name")

    def __init__(self, name, fact):
        self._name = name
        self._fact = fact

    def __call__(self, expression):
        return _Assumption(sympify(expression), self._fact, self._name)


class _AssumptionQueries:
    integer = _AssumptionPredicate("integer", _FACT_INTEGER)
    rational = _AssumptionPredicate("rational", _FACT_RATIONAL)
    algebraic = _AssumptionPredicate("algebraic", _FACT_ALGEBRAIC)
    real = _AssumptionPredicate("real", 1)
    zero = _AssumptionPredicate("zero", 64)
    negative = _AssumptionPredicate("negative", 128)
    nonpositive = _AssumptionPredicate("nonpositive", 256)
    positive = _AssumptionPredicate("positive", 2)
    nonnegative = _AssumptionPredicate("nonnegative", 4)
    nonzero = _AssumptionPredicate("nonzero", 8)


Q = _AssumptionQueries()


def _ask_algebraic(expression):
    """Match SymPy's Q.algebraic dispatch over the native result.

    The native predicate owns the mathematical classification. These small
    adapter checks preserve SymPy 1.14's dispatcher boundary: constructor
    assumptions on symbols do not register a Q handler, while local Q facts
    do; unsupported function heads remain undecided.
    """
    kind = expression.kind
    if kind in (3, 12):
        return True
    if kind == 4 and expression._known_facts & _ALGEBRAIC_FACTS:
        return None
    if kind != 9:
        return expression.is_algebraic

    head = expression.name
    arguments = expression.args
    try:
        if head == "sqrt":
            for argument in arguments:
                if (argument.kind == 4 and
                        argument._known_facts & _ALGEBRAIC_FACTS):
                    return None
        value = expression.is_algebraic
        if value is not False:
            return value
        for argument in arguments:
            if argument.kind == 4:
                return None
            if argument.is_algebraic is False:
                return None
        if head in _Q_ALGEBRAIC_UNDECIDED_HEADS:
            return None
        return value
    finally:
        for argument in arguments:
            argument.close()


def ask(proposition):
    if not isinstance(proposition, _Assumption):
        raise TypeError("ask expects a Q fact")
    if proposition.fact == _FACT_INTEGER:
        return proposition.expression.is_integer
    if proposition.fact == _FACT_RATIONAL:
        return proposition.expression.is_rational
    if proposition.fact == _FACT_ALGEBRAIC:
        return _ask_algebraic(proposition.expression)
    return proposition.expression._assumption_fact(proposition.fact)


class _AdapterAssumptionScope:
    def __init__(self, facts):
        self._scope = _default().assuming(*facts)

    def __enter__(self):
        try:
            return self._scope.__enter__()
        except FortSymError as error:
            if error.status == 5:
                raise UnsupportedOperationError(str(error)) from error
            if error.status == _CONFLICT:
                raise InconsistentAssumptions(str(error)) from error
            raise

    def __exit__(self, *arguments):
        return self._scope.__exit__(*arguments)


def assuming(*facts):
    return _AdapterAssumptionScope(facts)


def And(*arguments):
    if not arguments:
        raise UnsupportedOperationError("empty And")
    values = [sympify(argument) for argument in arguments]
    if len(values) == 1:
        return values[0]
    temporary = [
        value for argument, value in zip(arguments, values)
        if not isinstance(argument, Expr)
    ]
    try:
        return _default().function("And", values)
    finally:
        for value in temporary:
            value.close()


def _relational(name, left, right):
    relation = sympify(left)._relation(name, right)
    if name == "Equal":
        relation._sympy_head = "Eq"
    elif name == "Unequal":
        relation._sympy_head = "Ne"
    return relation


def Eq(left, right):
    return _relational("Equal", left, right)


def Ne(left, right):
    return _relational("Unequal", left, right)


def Gt(left, right):
    return _relational("Greater", left, right)


def Ge(left, right):
    return _relational("GreaterEqual", left, right)


def Lt(left, right):
    return _relational("Less", left, right)


def Le(left, right):
    return _relational("LessEqual", left, right)


def refine(expression, assumptions=None):
    expression = sympify(expression)
    if assumptions is None:
        return expression.simplify()
    if isinstance(assumptions, _Assumption):
        facts = (assumptions,)
    elif isinstance(assumptions, Expr):
        facts = (assumptions,)
    elif isinstance(assumptions, (tuple, list)):
        facts = tuple(assumptions)
    else:
        raise UnsupportedOperationError("refine assumptions")
    with assuming(*facts):
        return expression.simplify()


class Symbol(Expr, metaclass=_KindMeta):
    _kinds = frozenset({4})

    def __new__(cls, name, **assumptions):
        expression = _default().symbol(str(name))
        return _apply_assumptions(expression, assumptions)


class Integer(Expr, metaclass=_KindMeta):
    _kinds = frozenset({1, 10})

    def __new__(cls, value):
        return _default().integer(int(value))


class Rational(Expr, metaclass=_KindMeta):
    _kinds = frozenset({2, 11})

    def __new__(cls, numerator, denominator=1):
        return _default().rational(int(numerator), int(denominator))


class Float(Expr, metaclass=_KindMeta):
    _kinds = frozenset({3, 12})

    def __new__(cls, value):
        return _default().real(float(value))


class Add(Expr, metaclass=_KindMeta):
    _kinds = frozenset({6})

    def __new__(cls, *args, evaluate=True):
        if not evaluate:
            raise UnsupportedOperationError("unevaluated Add")
        if not args:
            return Integer(0)
        result = sympify(args[0])
        for arg in args[1:]:
            result = result + sympify(arg)
        return result


class Mul(Expr, metaclass=_KindMeta):
    _kinds = frozenset({7})

    def __new__(cls, *args, evaluate=True):
        if not evaluate:
            raise UnsupportedOperationError("unevaluated Mul")
        if not args:
            return Integer(1)
        result = sympify(args[0])
        for arg in args[1:]:
            result = result * sympify(arg)
        return result


class Pow(Expr, metaclass=_KindMeta):
    _kinds = frozenset({8})

    def __new__(cls, base, exponent, evaluate=True):
        if not evaluate:
            raise UnsupportedOperationError("unevaluated Pow")
        return sympify(base) ** sympify(exponent)


class Function(Expr, metaclass=_KindMeta):
    _kinds = frozenset({9})

    def __new__(cls, name, *args, **_):
        if _:
            raise UnsupportedOperationError("function assumptions")
        if args:
            return _default().function(str(name), [_coerce(arg) for arg in args])

        def applied(*arguments):
            return _default().function(str(name), [_coerce(arg) for arg in arguments])

        applied.__name__ = str(name)
        return applied


class Derivative:
    def __new__(cls, expression, *variables, evaluate=True):
        result = sympify(expression)
        if not evaluate:
            instance = super().__new__(cls)
            instance.expression = result
            instance.variables = tuple(sympify(variable) for variable in variables)
            return instance
        for variable in variables:
            result = result.diff(sympify(variable))
        return result

    def doit(self):
        return diff(self.expression, *self.variables)

    def __str__(self):
        return "Derivative(" + str(self.expression) + ", " + ", ".join(
            str(variable) for variable in self.variables
        ) + ")"

    __repr__ = __str__


class Subs:
    def __new__(cls, expression, *substitutions, **_):
        instance = super().__new__(cls)
        instance.expression = sympify(expression)
        instance.substitutions = substitutions
        return instance

    def doit(self):
        result = self.expression
        for substitution in self.substitutions:
            if len(substitution) != 2:
                raise TypeError("Subs substitutions must be (old, new) pairs")
            result = result.subs(sympify(substitution[0]), sympify(substitution[1]))
        return result

    def __str__(self):
        return "Subs(" + str(self.expression) + ")"

    __repr__ = __str__


def symbols(names, **assumptions):
    values = tuple(Symbol(name, **assumptions)
                   for name in str(names).replace(",", " ").split())
    return values[0] if len(values) == 1 else values


def RationalNumber(numerator, denominator=1):
    return Rational(numerator, denominator)


def FunctionClass(name):
    return Function(name)


def _named_function(name):
    def applied(*args):
        expression = Function(name, *args)
        try:
            return expression.simplify()
        finally:
            expression.close()

    return applied


def _complex_operation(name, expression):
    temporary = not isinstance(expression, Expr)
    value = expression if not temporary else sympify(expression)
    try:
        return value._complex_operation(name)
    except FortSymError as error:
        if error.status == 5:
            raise UnsupportedOperationError(str(error)) from error
        raise
    finally:
        if temporary:
            value.close()


sin = _named_function("sin")
cos = _named_function("cos")
tan = _named_function("tan")
asin = _named_function("asin")
acos = _named_function("acos")
atan = _named_function("atan")
atan2 = _named_function("atan2")
sinh = _named_function("sinh")
cosh = _named_function("cosh")
tanh = _named_function("tanh")
csch = _named_function("csch")
sech = _named_function("sech")
coth = _named_function("coth")
erf = _named_function("erf")
erfc = _named_function("erfc")
gamma = _named_function("gamma")
loggamma = _named_function("loggamma")
factorial = _named_function("factorial")
besselj = _named_function("besselj")
besseli = _named_function("besseli")
_legendrep = _named_function("legendrep")


def re(expression):
    return _complex_operation("re", expression)


def im(expression):
    return _complex_operation("im", expression)


def conjugate(expression):
    return _complex_operation("conjugate", expression)


def arg(expression):
    return _complex_operation("arg", expression)


def legendre(degree, argument):
    return _legendrep(degree, Integer(0), argument)


asinh = _named_function("asinh")
acosh = _named_function("acosh")
atanh = _named_function("atanh")
exp = _named_function("exp")
log = _named_function("log")
sqrt = _named_function("sqrt")
_native_abs = _named_function("abs")


def Abs(expression):
    try:
        return _complex_operation("abs", expression)
    except UnsupportedOperationError:
        return _native_abs(expression)


def expand_complex(expression, deep=True):
    if not isinstance(deep, bool):
        raise TypeError("expand_complex deep must be a boolean")
    return _complex_operation("expand_complex", expression)


sign = _named_function("sign")
floor = _named_function("floor")
ceiling = _named_function("ceiling")


def diff(expression, *variables, **_):
    if _:
        raise UnsupportedOperationError("differentiation options")
    result = sympify(expression)
    for variable in variables:
        result = result._diff_simplified(sympify(variable))
    return result if variables else result.simplify()


def subs(expression, substitutions, new=None, *, simultaneous=False):
    result = sympify(expression)
    if new is not None:
        return result.subs(sympify(substitutions), sympify(new))
    if isinstance(substitutions, dict):
        if not substitutions:
            return result
        pairs, temporaries, signature = _ordered_substitutions(substitutions)
        try:
            if simultaneous:
                old = tuple(pair[0] for pair in pairs)
                replacement = tuple(pair[1] for pair in pairs)
                return result._subs_many(old, replacement)
            if _can_use_subs_many(result, pairs, signature):
                return _cached_subs_many(
                    result, pairs, substitutions, signature
                )
            current = result
            try:
                for old, replacement in pairs:
                    next_result = current._subs_raw(old, replacement)
                    if current is not result:
                        current.close()
                    current = next_result
                return current.expand()
            finally:
                if current is not result:
                    current.close()
        finally:
            for temporary in temporaries:
                temporary.close()
    raise TypeError("subs expects (old, new) or a mapping")


def expand(expression, **_):
    return sympify(expression).expand()


def count_ops(expression, visual=False):
    if visual:
        raise UnsupportedOperationError("count_ops(visual=True)")
    return sympify(expression).operation_count()


def simplify(expression, **_):
    if _:
        raise UnsupportedOperationError("simplification options")
    return sympify(expression).simplify()


def _unsupported(name):
    def operation(*_, **__):
        raise UnsupportedOperationError(name)

    return operation

def factor(expression, **options):
    if options:
        raise UnsupportedOperationError("factor options")
    try:
        return sympify(expression).factor()
    except FortSymError as error:
        raise UnsupportedOperationError(str(error)) from error


together = _unsupported("together")
cancel = _unsupported("cancel")
apart = _unsupported("apart")
collect = _unsupported("collect")
integrate = _unsupported("integrate")
limit = _unsupported("limit")
series = _unsupported("series")
solve = _unsupported("solve")
Matrix = _unsupported("Matrix")


pi = _default().constant("pi")
E = _default().constant("e")
I = _default().constant("i")
oo = _default().constant("oo")
zoo = _default().constant("zoo")
nan = _default().constant("nan")


__all__ = [
    "Arena", "Expr", "FortSymError", "UnsupportedOperationError",
    "InconsistentAssumptions",
    "Symbol", "symbols", "sympify", "Integer", "Rational", "Float",
    "Add", "Mul", "Pow", "Function", "Derivative", "Subs", "sin", "cos",
    "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", "csch",
    "sech", "coth", "erf", "erfc", "gamma", "loggamma", "factorial",
    "besselj", "besseli", "legendre", "expand_complex",
    "asinh", "acosh", "atanh", "exp", "log", "sqrt", "Abs", "sign",
    "floor", "ceiling", "re", "im", "conjugate", "arg", "diff", "subs", "expand",
    "simplify", "count_ops", "factor", "refine", "Eq", "Ne", "Gt", "Ge", "Lt", "Le", "And",
    "Q", "ask", "assuming", "together", "cancel", "apart", "collect",
    "integrate", "limit", "series", "solve", "Matrix", "pi", "E", "I",
    "oo", "zoo", "nan",
]
