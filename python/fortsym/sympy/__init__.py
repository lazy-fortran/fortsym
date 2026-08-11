"""A deliberately declared SymPy-compatible subset.

This module is a syntax adapter over :mod:`fortsym`, not a SymPy reimplementation
and never imports SymPy.  Names outside the table in ``python/README.md`` raise
``UnsupportedOperationError`` instead of silently changing the calculation.
"""

from __future__ import annotations

from fractions import Fraction

from .. import Arena, Expr, FortSymError, _default


class UnsupportedOperationError(NotImplementedError):
    """The requested SymPy operation is outside fortsym's declared subset."""


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
    "real": 1,
    "positive": 2,
    "nonnegative": 4,
    "nonzero": 8,
}


def _apply_assumptions(expression, assumptions):
    for name, value in assumptions.items():
        if name == "commutative" and value is True:
            continue
        if name not in _ASSUMPTION_FACTS or value is not True:
            raise UnsupportedOperationError(
                f"symbol assumption {name}={value!r}"
            )
    for name, value in assumptions.items():
        if name == "commutative":
            continue
        _default().assume(expression, _ASSUMPTION_FACTS[name])
    return expression


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
    return lambda *args: Function(name, *args)


sin = _named_function("sin")
cos = _named_function("cos")
tan = _named_function("tan")
exp = _named_function("exp")
log = _named_function("log")
sqrt = _named_function("sqrt")
Abs = _named_function("abs")


def diff(expression, *variables, **_):
    if _:
        raise UnsupportedOperationError("differentiation options")
    result = sympify(expression)
    for variable in variables:
        result = result.diff(sympify(variable))
    return result.simplify()


def subs(expression, substitutions, new=None):
    result = sympify(expression)
    if new is not None:
        return result.subs(sympify(substitutions), sympify(new))
    if isinstance(substitutions, dict):
        for old, replacement in substitutions.items():
            result = result.subs(sympify(old), sympify(replacement))
        return result
    raise TypeError("subs expects (old, new) or a mapping")


def expand(expression, **_):
    return sympify(expression).expand()


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


__all__ = [
    "Arena", "Expr", "FortSymError", "UnsupportedOperationError",
    "Symbol", "symbols", "sympify", "Integer", "Rational", "Float",
    "Add", "Mul", "Pow", "Function", "Derivative", "Subs", "sin", "cos",
    "tan", "exp", "log", "sqrt", "Abs", "diff", "subs", "expand",
    "simplify", "factor", "together", "cancel", "apart", "collect",
    "integrate", "limit", "series", "solve", "Matrix", "pi", "E", "I",
    "oo",
]
