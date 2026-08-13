"""A deliberately declared SymPy-compatible subset.

This module is a syntax adapter over :mod:`fortsym`, not a SymPy reimplementation
and never imports SymPy.  Names outside the table in ``python/README.md`` raise
``UnsupportedOperationError`` instead of silently changing the calculation.
"""

from __future__ import annotations

from decimal import Decimal
from fractions import Fraction
from itertools import count
from operator import index as _index

from .. import (
    Arena, Chart, ChartMap, FluxSurface, FluxCoordinates, MagneticChart, MagneticField, FourierWeakForm, FourierSource, FourierLoad, Metric, Connection,
    Orientation, Signature,
    SpacetimeMetric, SpacetimeForm, SpacetimeTensor, Tensor, IndexType, Index,
    Form, Expr,
    FortSymError, FOURIER_INVALID, FOURIER_LONGITUDINAL, FOURIER_TRANSVERSE,
    FLUX_GENERIC, FLUX_CLEBSCH, FLUX_STRAIGHT_FIELD_LINE, FLUX_BOOZER,
    FLUX_HAMADA, CLEBSCH_RESIDUAL_COUNT, BOOZER_RESIDUAL_COUNT,
    HAMADA_RESIDUAL_COUNT,
    INDEX_TANGENT, INDEX_COTANGENT, INDEX_SPACETIME, INDEX_INTERNAL, INDEX_USER,
    SPACE_NONE, SPACE_NODAL, SPACE_EDGE, TRACE_NONE, TRACE_NORMAL,
    TRACE_TANGENTIAL, _Assumption, _CONFLICT, _FACT_INTEGER, _FACT_RATIONAL,
    _FACT_ALGEBRAIC, SPACETIME_DIM, CONNECTION_STANDARD, CONNECTION_OPPOSITE,
    _default,
    SYMMETRY_NONE, SYMMETRIC, ANTISYMMETRIC,
    tensor_product as _tensor_product, contract as _contract,
)


TensorIndexType = IndexType
TensorIndex = Index


from ..diffgeom import (
    BaseScalarField, BaseVectorField, CoordinateSymbol, CoordSystem,
    Differential, LieDerivative, Manifold, Patch, TensorProduct, WedgeProduct,
)


class UnsupportedOperationError(NotImplementedError):
    """The requested SymPy operation is outside fortsym's declared subset."""


class InconsistentAssumptions(ValueError):
    """The supplied assumptions cannot hold simultaneously."""


class Tuple:
    """Native-owned tuple-valued result for the bounded compatibility subset."""

    @classmethod
    def _from_expression(cls, expression):
        instance = object.__new__(cls)
        instance._expression = expression
        return instance

    def __init__(self, *elements):
        values = tuple(sympify(element) for element in elements)
        self._expression = _default().function("Tuple", values)

    @property
    def args(self):
        return self._expression.args

    def __iter__(self):
        return iter(self.args)

    def __len__(self):
        return self._expression.arity

    def __getitem__(self, index):
        if isinstance(index, slice):
            values = self.args
            try:
                return Tuple(*values[index])
            finally:
                for value in values:
                    value.close()
        if index < 0:
            index += len(self)
        return self._expression.argument(index)

    def __eq__(self, other):
        return (isinstance(other, Tuple) and
                self._expression == other._expression)

    def __str__(self):
        values = self.args
        try:
            if len(values) == 1:
                return "(" + str(values[0]) + ",)"
            return "(" + ", ".join(str(element) for element in values) + ")"
        finally:
            for value in values:
                value.close()

    __repr__ = __str__

    def close(self):
        self._expression.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


def _set_sort_key(value):
    if isinstance(value, Tuple):
        expression = value._expression
    elif isinstance(value, Expr) and value.name == "Tuple":
        expression = value
    else:
        return _sympy_sort_key(value)
    elements = expression.args
    try:
        return (3, tuple(_sympy_sort_key(element) for element in elements))
    finally:
        for element in elements:
            element.close()


def _set_element_expression(value):
    if isinstance(value, Tuple):
        return value._expression, False
    if isinstance(value, Expr):
        return _default()._check(value), False
    return sympify(value), True


def _set_element_wrapper(expression):
    if expression.name == "Tuple":
        return Tuple._from_expression(expression)
    return expression


class FiniteSet:
    """Native-owned finite set result for the bounded compatibility subset."""

    @classmethod
    def _from_expression(cls, expression):
        instance = object.__new__(cls)
        instance._expression = expression
        return instance

    def __new__(cls, *elements):
        if not elements:
            return EmptySet
        return super().__new__(cls)

    def __init__(self, *elements):
        values = []
        temporaries = []
        try:
            for element in elements:
                value, temporary = _set_element_expression(element)
                if any(value == previous for previous in values):
                    if temporary:
                        value.close()
                    continue
                values.append(value)
                if temporary:
                    temporaries.append(value)
            values.sort(key=_set_sort_key)
            self._expression = _default().function("FiniteSet", values)
        finally:
            for value in temporaries:
                value.close()

    @property
    def args(self):
        return tuple(_set_element_wrapper(value)
                     for value in self._expression.args)

    def __iter__(self):
        return iter(self.args)

    def __len__(self):
        return self._expression.arity

    def __contains__(self, value):
        try:
            value, temporary = _set_element_expression(value)
        except (TypeError, UnsupportedOperationError):
            return False
        elements = self._expression.args
        try:
            return any(value == element for element in elements)
        finally:
            for element in elements:
                element.close()
            if temporary:
                value.close()

    def __eq__(self, other):
        return (isinstance(other, FiniteSet) and
                self._expression == other._expression)

    def __str__(self):
        elements = self.args
        try:
            return "{" + ", ".join(str(element) for element in elements) + "}"
        finally:
            for element in elements:
                element.close()

    __repr__ = __str__

    def close(self):
        self._expression.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class Complement:
    """Native-owned finite-set exclusion for bounded rational solveset."""

    def __init__(self, base, excluded):
        if not isinstance(base, FiniteSet) or not isinstance(excluded, FiniteSet):
            raise UnsupportedOperationError("finite-set complement form")
        self._expression = _default().function(
            "Complement", (base._expression, excluded._expression)
        )

    @property
    def args(self):
        base, excluded = self._expression.args
        return (FiniteSet._from_expression(base),
                FiniteSet._from_expression(excluded))

    def __iter__(self):
        base, excluded = self.args
        try:
            values = base.args
            return iter(values)
        finally:
            base.close()
            excluded.close()

    def __len__(self):
        base, excluded = self.args
        try:
            return len(base)
        finally:
            base.close()
            excluded.close()

    def __contains__(self, value):
        base, excluded = self.args
        try:
            return value in base and value not in excluded
        finally:
            base.close()
            excluded.close()

    def __eq__(self, other):
        return (isinstance(other, Complement) and
                self._expression == other._expression)

    def __str__(self):
        base, excluded = self.args
        try:
            return f"Complement({base}, {excluded})"
        finally:
            base.close()
            excluded.close()

    __repr__ = __str__

    def close(self):
        self._expression.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class _EmptySet:
    args = ()

    def __iter__(self):
        return iter(())

    def __len__(self):
        return 0

    def __contains__(self, _):
        return False

    def __str__(self):
        return "EmptySet"

    __repr__ = __str__

    def __eq__(self, other):
        return isinstance(other, _EmptySet)

    def __hash__(self):
        return hash(_EmptySet)


EmptySet = _EmptySet()


class TensorSymmetry:
    """SymPy-named pair-symmetry descriptor for native chart tensors.

    The native fixed-rank owner currently records pair declarations. The
    descriptor keeps that vocabulary at the compatibility boundary and
    supplies declarations to ``Chart.tensor(..., symmetries=...)``.
    """

    def __init__(self, rank, pairs=()):
        self.rank = int(rank)
        if self.rank < 0:
            raise ValueError("tensor symmetry rank must be nonnegative")
        normalized = []
        for first, second, kind in pairs:
            first, second = int(first), int(second)
            if first < 0 or second < 0 or first >= self.rank or second >= self.rank:
                raise IndexError("tensor symmetry slot is outside the tensor rank")
            normalized.append((first, second, kind))
        self.pairs = tuple(normalized)

    @classmethod
    def no_symmetry(cls, rank):
        return cls(rank)

    @classmethod
    def fully_symmetric(cls, rank):
        return cls(rank, (
            (first, second, SYMMETRIC)
            for first in range(int(rank))
            for second in range(first + 1, int(rank))
        ))

    @classmethod
    def fully_antisymmetric(cls, rank):
        return cls(rank, (
            (first, second, ANTISYMMETRIC)
            for first in range(int(rank))
            for second in range(first + 1, int(rank))
        ))

    @classmethod
    def pair_symmetric(cls, rank, first, second):
        return cls(rank, ((first, second, SYMMETRIC),))

    @classmethod
    def pair_antisymmetric(cls, rank, first, second):
        return cls(rank, ((first, second, ANTISYMMETRIC),))

    def __repr__(self):
        return f"TensorSymmetry({self.rank}, {self.pairs!r})"


_WILD_COUNTER = count()
_MAX_PARTITION_WILDCARDS = 3


def _wildcard_accepts(wildcard, expression):
    if any(expression == excluded for excluded in wildcard._exclude):
        return False
    for property_function in wildcard._properties:
        if not bool(property_function(expression)):
            return False
    return True


def _direct_wildcard(expression, wildcards):
    if expression.kind != 4:
        return None
    return wildcards.get(expression.name)


def _has_wildcard(expression):
    return bool(getattr(expression, "_wildcard_patterns", {}))


def _has_numeric_factor(expression):
    arguments = expression.args
    try:
        return any(argument.kind in (1, 2, 3, 10, 11, 12, 13)
                   for argument in arguments)
    finally:
        for argument in arguments:
            argument.close()


def tensorproduct(*args):
    """SymPy spelling for the native tensor-product owner."""
    if not args:
        raise TypeError("tensorproduct requires at least one tensor")
    result = args[0]
    for value in args[1:]:
        if not isinstance(result, (Tensor, SpacetimeTensor)) or not isinstance(
                value, type(result)):
            return TensorProduct(*args)
        result = _tensor_product(result, value)
    return result


def tensorcontraction(tensor, *pairs):
    """SymPy spelling for checked native tensor contractions."""
    if len(pairs) == 1 and isinstance(pairs[0], (tuple, list)) and pairs[0] and \
            isinstance(pairs[0][0], (tuple, list)):
        pairs = tuple(pairs[0])
    result = tensor
    for pair in pairs:
        if len(pair) != 2:
            raise ValueError("tensorcontraction pairs must contain two slots")
        result = _contract(result, pair[0], pair[1])
    return result


def tensorpermute(tensor, order):
    """SymPy spelling for native tensor slot permutation."""
    if not isinstance(tensor, (Tensor, SpacetimeTensor)):
        raise TypeError("tensorpermute expects a typed tensor")
    return tensor.permute(order)


def _combine_owned(values, operation, owned):
    result = values[0]
    for value in values[1:]:
        result = operation(result, value)
        owned.append(result)
    return result


def _partition_arguments(expression):
    arguments = list(expression.args)
    arguments.sort(key=_sympy_sort_key)
    return tuple(arguments)


def _commutative_partition(expression, pattern, wildcards):
    """Partition one associative root among distinct direct Wild nodes."""
    if pattern.kind not in (6, 7):
        return None
    if pattern.kind == 6 and expression.kind == 7:
        return None
    pattern_args = pattern.args
    actual_args = ()
    actual_from_root = False
    owned = []
    bindings = {}
    try:
        fixed = []
        wildcard_sequence = []
        for argument in pattern_args:
            wildcard = _direct_wildcard(argument, wildcards)
            if wildcard is not None:
                wildcard_sequence.append(wildcard)
            elif _has_wildcard(argument):
                return None
            else:
                fixed.append(argument)
        present = set(wildcard_sequence)
        if (len(present) < 2 or
                len(present) > _MAX_PARTITION_WILDCARDS or
                len(present) != len(wildcard_sequence)):
            return None
        wildcard_sequence = [
            wildcard for wildcard in wildcards.values()
            if wildcard in present
        ]

        if expression.kind == pattern.kind:
            actual_args = _partition_arguments(expression)
            actual_from_root = True
        else:
            actual_args = (expression,)

        used = set()
        for expected in fixed:
            match = next(
                (index for index, actual in enumerate(actual_args)
                 if index not in used and actual == expected),
                None,
            )
            if match is None:
                return None
            used.add(match)
        remaining = [
            actual for index, actual in enumerate(actual_args)
            if index not in used
        ]
        if fixed and not actual_from_root and remaining:
            return None

        identity = (0 if pattern.kind == 6 else 1)
        for offset, wildcard in enumerate(reversed(wildcard_sequence)):
            if offset < len(wildcard_sequence) - 1:
                values = [remaining.pop()] if remaining else []
            else:
                values = remaining
                remaining = []
            if not values:
                value = expression._arena.integer(identity)
                owned.append(value)
            elif len(values) == 1:
                value = values[0]
            else:
                operation = (lambda left, right: left + right
                             if pattern.kind == 6 else left * right)
                value = _combine_owned(values, operation, owned)
            if not _wildcard_accepts(wildcard, value):
                return None
            bindings[wildcard] = value

        return bindings
    finally:
        keep = {id(value) for value in bindings.values()}
        for child in pattern_args:
            child.close()
        if actual_from_root:
            for child in actual_args:
                if id(child) not in keep:
                    child.close()
        for temporary in owned:
            if id(temporary) not in keep:
                temporary.close()


def _commutative_remainder(expression, pattern, wildcards):
    """Match one repeated or single Wild at an Add or Mul root."""
    if len(wildcards) != 1 or pattern.kind not in (6, 7):
        return None
    wildcard = next(iter(wildcards.values()))
    pattern_args = pattern.args
    actual_args = ()
    owned = []
    binding = None
    try:
        fixed = []
        wildcard_count = 0
        for argument in pattern_args:
            candidate = _direct_wildcard(argument, wildcards)
            if candidate is wildcard:
                wildcard_count += 1
            elif _has_wildcard(argument):
                return None
            else:
                fixed.append(argument)
        if not wildcard_count:
            return None

        if pattern.kind == 6:
            if (fixed and expression.kind == 7 and
                    not _has_numeric_factor(expression)):
                return None
            if expression.kind == 6:
                actual_args = expression.args
                used = set()
                exact_partition = True
                for expected in fixed:
                    match = next(
                        (index for index, actual in enumerate(actual_args)
                         if index not in used and actual == expected),
                        None,
                    )
                    if match is None:
                        exact_partition = False
                        break
                    used.add(match)
                if exact_partition:
                    remaining = [
                        actual for index, actual in enumerate(actual_args)
                        if index not in used
                    ]
                    if not remaining:
                        binding = expression._arena.integer(0)
                        owned.append(binding)
                    elif len(remaining) == 1:
                        binding = remaining[0]
                    else:
                        binding = _combine_owned(
                            remaining, lambda left, right: left + right, owned)
            elif len(fixed) == 1 and expression == fixed[0]:
                binding = expression._arena.integer(0)
                owned.append(binding)
            if binding is None:
                if fixed:
                    fixed_total = fixed[0]
                    if len(fixed) > 1:
                        fixed_total = _combine_owned(
                            fixed, lambda left, right: left + right, owned)
                    binding = expression - fixed_total
                    owned.append(binding)
                    binding = binding.expand()
                    owned.append(binding)
                else:
                    binding = expression
            if wildcard_count > 1:
                divisor = expression._arena.integer(wildcard_count)
                owned.append(divisor)
                binding = binding / divisor
                owned.append(binding)
        else:
            numeric = all(argument.kind in (1, 2, 3, 10, 11, 12, 13)
                          for argument in fixed)
            if numeric:
                if fixed:
                    coefficient = fixed[0]
                    if len(fixed) > 1:
                        coefficient = _combine_owned(
                            fixed, lambda left, right: left * right, owned)
                else:
                    coefficient = expression._arena.integer(1)
                    owned.append(coefficient)
                if coefficient.exact_text not in ("1", "1/1"):
                    binding = expression / coefficient
                    owned.append(binding)
                else:
                    binding = expression
            elif expression.kind == 7:
                actual_args = expression.args
                used = set()
                for expected in fixed:
                    match = next(
                        (index for index, actual in enumerate(actual_args)
                         if index not in used and actual == expected),
                        None,
                    )
                    if match is None:
                        return None
                    used.add(match)
                remaining = [
                    actual for index, actual in enumerate(actual_args)
                    if index not in used
                ]
                if not remaining:
                    binding = expression._arena.integer(1)
                    owned.append(binding)
                elif len(remaining) == 1:
                    binding = remaining[0]
                else:
                    binding = _combine_owned(
                        remaining, lambda left, right: left * right, owned)
            elif len(fixed) == 1 and expression == fixed[0]:
                binding = expression._arena.integer(1)
                owned.append(binding)
            else:
                return None

        if not _wildcard_accepts(wildcard, binding):
            return None
        return {wildcard: binding}
    finally:
        keep = {id(binding)} if binding is not None else set()
        for child in pattern_args:
            child.close()
        for child in actual_args:
            if id(child) not in keep:
                child.close()
        for temporary in owned:
            if id(temporary) not in keep:
                temporary.close()


def _match_pattern(expression, pattern, old=False):
    """Match structural wildcard nodes in the declared adapter fragment."""
    wildcards = getattr(pattern, "_wildcard_patterns", {})
    if getattr(pattern, "_wildcard_direct", False):
        wildcard = (pattern if isinstance(pattern, Wild)
                    else next(iter(wildcards.values())))
        return ({wildcard: expression}
                if _wildcard_accepts(wildcard, expression) else None)
    remainder_attempted = False
    if pattern.kind in (6, 7) and wildcards:
        remainder_attempted = True
        remainder = None
        if len(wildcards) >= 2:
            remainder = _commutative_partition(expression, pattern, wildcards)
        if remainder is None:
            remainder = _commutative_remainder(expression, pattern, wildcards)
        if remainder is not None:
            return remainder
    bindings = {}
    actual_children = []
    pattern_children = []

    def visit(actual, expected):
        if expected.kind in (6, 7) and len(wildcards) >= 2:
            partition = _commutative_partition(actual, expected, wildcards)
            if partition is not None:
                bindings.update(partition)
                return True
        wildcard = (wildcards.get(expected.name)
                    if expected.kind == 4 else None)
        if wildcard is not None:
            previous = bindings.get(wildcard)
            if previous is not None:
                return actual == previous
            if not _wildcard_accepts(wildcard, actual):
                return False
            bindings[wildcard] = actual
            return True

        if actual.kind != expected.kind:
            return False
        if actual.kind in (1, 2, 3, 4, 5, 10, 11, 12, 13):
            return actual == expected
        if actual.kind == 9 and (
                actual.name != expected.name or
                actual.arity != expected.arity):
            return False
        if actual.arity != expected.arity:
            return False
        actual_args = actual.args
        expected_args = expected.args
        actual_children.extend(actual_args)
        pattern_children.extend(expected_args)
        return all(visit(left, right)
                   for left, right in zip(actual_args, expected_args))

    try:
        if visit(expression, pattern):
            return dict(bindings)
        if remainder_attempted:
            return None
        return _commutative_remainder(expression, pattern, wildcards)
    finally:
        keep = {id(value) for value in bindings.values()}
        for child in pattern_children:
            child.close()
        closed = set()
        for child in actual_children:
            marker = id(child)
            if marker in closed:
                continue
            closed.add(marker)
            if marker not in keep:
                child.close()


def _coerce(value):
    if isinstance(value, Expr):
        return _default()._check(value)
    pattern_materializer = getattr(value, "_materialize_pattern", None)
    if pattern_materializer is not None:
        return _default()._check(pattern_materializer())
    pattern_expression = getattr(value, "_pattern_expression", None)
    if pattern_expression is not None:
        return _default()._check(pattern_expression)
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


_BOOLEAN_RELATION_NEGATIONS = {
    "Equal": "Unequal",
    "Unequal": "Equal",
    "Greater": "LessEqual",
    "GreaterEqual": "Less",
    "Less": "GreaterEqual",
    "LessEqual": "Greater",
}
_BOOLEAN_NUMBER_KINDS = frozenset({1, 2, 3, 10, 11, 12, 13})
_BOOLEAN_EXPRESSION_HEADS = frozenset({
    "Equal", "Unequal", "Greater", "GreaterEqual", "Less", "LessEqual",
    "And", "Or", "Not", "Xor", "Implies", "Equivalent",
})


def _boolean_values(arguments, name):
    values = []
    temporary = []
    try:
        for argument in arguments:
            if isinstance(argument, bool):
                values.append(argument)
                continue
            value = sympify(argument)
            _default()._check(value)
            if not isinstance(argument, Expr):
                temporary.append(value)
            if value.kind in _BOOLEAN_NUMBER_KINDS:
                if value.exact_text == "0":
                    values.append(False)
                    continue
                if value.exact_text in ("1", "1/1"):
                    values.append(True)
                    continue
                raise TypeError(
                    f"{name} expects Boolean or symbolic Boolean arguments"
                )
            if value.kind != 4 and not (
                    value.kind == 9 and value.name in _BOOLEAN_EXPRESSION_HEADS):
                raise TypeError(
                    f"{name} expects Boolean or symbolic Boolean arguments"
                )
            values.append(value)
    except Exception:
        for value in temporary:
            value.close()
        raise
    return values, temporary


def _keep_boolean_value(value, temporary):
    if isinstance(value, Expr):
        temporary[:] = [candidate for candidate in temporary
                        if candidate is not value]
    return value


def _boolean_function(name, values):
    return _default().function(name, [value for value in values
                                      if isinstance(value, Expr)])


def And(*arguments):
    values, temporary = _boolean_values(arguments, "And")
    try:
        if any(value is False for value in values):
            return False
        values = [value for value in values if value is not True]
        if not values:
            return True
        if len(values) == 1:
            return _keep_boolean_value(values[0], temporary)
        return _boolean_function("And", values)
    finally:
        for value in temporary:
            value.close()


def Or(*arguments):
    values, temporary = _boolean_values(arguments, "Or")
    try:
        if any(value is True for value in values):
            return True
        values = [value for value in values if value is not False]
        if not values:
            return False
        if len(values) == 1:
            return _keep_boolean_value(values[0], temporary)
        return _boolean_function("Or", values)
    finally:
        for value in temporary:
            value.close()


def Not(argument):
    values, temporary = _boolean_values((argument,), "Not")
    try:
        value = values[0]
        if isinstance(value, bool):
            return not value
        if value.kind == 9 and value.name in _BOOLEAN_RELATION_NEGATIONS:
            children = value.args
            try:
                return _relational(
                    _BOOLEAN_RELATION_NEGATIONS[value.name],
                    children[0], children[1],
                )
            finally:
                for child in children:
                    child.close()
        return _boolean_function("Not", (value,))
    finally:
        for value in temporary:
            value.close()


def Xor(*arguments):
    values, temporary = _boolean_values(arguments, "Xor")
    try:
        parity = sum(value is True for value in values) % 2
        values = [value for value in values if value is not False and
                  value is not True]
        if not values:
            return bool(parity)
        if len(values) == 1:
            if not parity:
                return _keep_boolean_value(values[0], temporary)
            return Not(values[0])
        result = _boolean_function("Xor", values)
        if not parity:
            return result
        try:
            return Not(result)
        finally:
            result.close()
    finally:
        for value in temporary:
            value.close()


def Implies(*arguments):
    if len(arguments) != 2:
        raise ValueError(
            f"{len(arguments)} operand(s) used for an Implies (pairs are required): "
            f"{arguments!r}"
        )
    values, temporary = _boolean_values(arguments, "Implies")
    try:
        left, right = values
        if left is False or right is True:
            return True
        if left is True:
            return _keep_boolean_value(right, temporary)
        if right is False:
            return Not(left)
        return _boolean_function("Implies", values)
    finally:
        for value in temporary:
            value.close()


def Equivalent(*arguments):
    values, temporary = _boolean_values(arguments, "Equivalent")
    try:
        if len(values) <= 1:
            return True
        if all(isinstance(value, bool) for value in values):
            return all(value is values[0] for value in values[1:])
        if any(isinstance(value, bool) for value in values):
            constants = [value for value in values if isinstance(value, bool)]
            symbolic = [value for value in values if not isinstance(value, bool)]
            if any(value != constants[0] for value in constants[1:]):
                return False
            if constants[0] is True:
                return And(*symbolic)
            negated = tuple(Not(value) for value in symbolic)
            try:
                return _boolean_function("And", negated)
            finally:
                for value in negated:
                    value.close()
        return _boolean_function("Equivalent", values)
    finally:
        for value in temporary:
            value.close()


def _relational(name, left, right):
    left_expression = sympify(left)
    if (isinstance(left, Expr) and
            left_expression._kind_cache not in _BOOLEAN_NUMBER_KINDS and
            not isinstance(right, Expr)):
        relation = left_expression._relation(name, right)
        if name == "Equal":
            relation._sympy_head = "Eq"
        elif name == "Unequal":
            relation._sympy_head = "Ne"
        return relation
    left_temporary = not isinstance(left, Expr)
    right_expression = right if isinstance(right, Expr) else None
    right_temporary = False
    relation = None
    simplified = None
    keep_relation = False
    try:
        left_kind = left_expression._kind_cache
        right_kind = (None if right_expression is None
                      else right_expression._kind_cache)
        same_expression = (
            right_expression is not None and
            left_expression._arena is right_expression._arena and
            left_expression is right_expression
        )
        if (not same_expression and left_kind == 4 and right_kind == 4):
            same_expression = (
                left_expression._arena is right_expression._arena and
                left_expression.name == right_expression.name
            )
        if same_expression:
            if name == "Equal":
                return True
            if name == "Unequal":
                return False
            # SymPy keeps x < x and x <= x unevaluated without assumptions.
            relation = left_expression._relation(name, right_expression)
            keep_relation = True
            return relation
        if left_kind not in _BOOLEAN_NUMBER_KINDS:
            relation = left_expression._relation(name, right)
            if name == "Equal":
                relation._sympy_head = "Eq"
            elif name == "Unequal":
                relation._sympy_head = "Ne"
            keep_relation = True
            return relation
        if (right_expression is None and
                left_kind in _BOOLEAN_NUMBER_KINDS and
                isinstance(right, (bool, int, float, Fraction))):
            right_expression = sympify(right)
            right_temporary = True
            right_kind = right_expression._kind_cache
        if (right_expression is not None and
                left_kind in _BOOLEAN_NUMBER_KINDS and
                right_kind in _BOOLEAN_NUMBER_KINDS):
            relation = left_expression._relation(name, right_expression)
            simplified = relation.simplify()
            if simplified.name in ("True", "False"):
                result = simplified.name == "True"
                simplified.close()
                relation.close()
                relation = None
                return result
        else:
            relation = left_expression._relation(name, right)
        if name == "Equal":
            relation._sympy_head = "Eq"
        elif name == "Unequal":
            relation._sympy_head = "Ne"
        keep_relation = True
        return relation
    finally:
        if simplified is not None and relation is not None:
            simplified.close()
        if relation is not None and not keep_relation:
            relation.close()
        if left_temporary:
            left_expression.close()
        if right_temporary:
            right_expression.close()


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


def _rational_input(value):
    """Convert one supported SymPy rational constructor input exactly."""
    if isinstance(value, Expr):
        if value.kind not in (1, 2, 10, 11):
            raise TypeError(f"invalid input: {value}")
        return Fraction(str(value))
    if isinstance(value, Decimal):
        raise TypeError(f"invalid input: {value}")
    if isinstance(value, float):
        return Fraction.from_float(value)
    try:
        return Fraction(value)
    except (TypeError, ValueError):
        raise TypeError(f"invalid input: {value}") from None


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
        numerator = _rational_input(numerator)
        denominator = _rational_input(denominator)
        if denominator == 0:
            return _default().constant("nan" if numerator == 0 else "zoo")
        value = numerator / denominator
        return _default().rational(value.numerator, value.denominator)


class Float(Expr, metaclass=_KindMeta):
    _kinds = frozenset({3, 12})

    def __new__(cls, value):
        return _default().real(float(value))


def _attach_pattern(expression, values):
    wildcards = {}
    for value in values:
        candidate = getattr(value, "_pattern_expression", None)
        if candidate is None:
            materializer = getattr(value, "_materialize_pattern", None)
            candidate = value if materializer is None else materializer()
        wildcards.update(getattr(candidate, "_wildcard_patterns", {}))
    if wildcards:
        expression._wildcard_patterns = wildcards
        expression._wildcard_matcher = _match_pattern
    return expression


class Wild:
    """A bounded SymPy-compatible structural wildcard."""

    def __new__(cls, name, exclude=(), properties=(), **assumptions):
        if assumptions:
            raise UnsupportedOperationError("Wild assumptions")
        instance = super().__new__(cls)
        instance.name = f"{name}_"
        instance._pattern_expression = None
        instance._pattern_arena = _default()
        instance._wildcard_direct = True
        instance._wildcard_matcher = _match_pattern
        if isinstance(exclude, (tuple, list, set, frozenset)):
            excluded = exclude
        else:
            excluded = (exclude,)
        instance._exclude = tuple(sympify(value) for value in excluded)
        if properties is None:
            properties = ()
        elif callable(properties):
            properties = (properties,)
        instance._properties = tuple(properties)
        return instance

    def _materialize_pattern(self):
        if self._pattern_expression is None:
            expression = self._pattern_arena.symbol(
                f"__fortsym_wild_{next(_WILD_COUNTER)}"
            )
            expression._wildcard_patterns = {expression.name: self}
            expression._wildcard_direct = True
            expression._wildcard_matcher = _match_pattern
            self._pattern_expression = expression
        return self._pattern_expression

    def __str__(self):
        return self.name

    __repr__ = __str__

    @property
    def args(self):
        return ()

    @property
    def exclude(self):
        return self._exclude

    @property
    def properties(self):
        return self._properties

    @property
    def is_symbol(self):
        return True

    def __add__(self, other): return self._materialize_pattern() + other
    def __radd__(self, other): return other + self._materialize_pattern()
    def __sub__(self, other): return self._materialize_pattern() - other
    def __rsub__(self, other): return other - self._materialize_pattern()
    def __mul__(self, other): return self._materialize_pattern() * other
    def __rmul__(self, other): return other * self._materialize_pattern()
    def __truediv__(self, other): return self._materialize_pattern() / other
    def __rtruediv__(self, other): return other / self._materialize_pattern()
    def __pow__(self, other): return self._materialize_pattern() ** other
    def __rpow__(self, other): return other ** self._materialize_pattern()
    def __neg__(self): return -self._materialize_pattern()


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
            arguments = [_coerce(arg) for arg in args]
            return _attach_pattern(
                _default().function(str(name), arguments), args
            )

        def applied(*arguments):
            values = [_coerce(arg) for arg in arguments]
            return _attach_pattern(
                _default().function(str(name), values), arguments
            )

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
        if getattr(expression, "_wildcard_patterns", None):
            return expression
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
    if isinstance(expression, Matrix):
        return expression.conjugate()
    return _complex_operation("conjugate", expression)


def adjoint(expression):
    if not isinstance(expression, Matrix):
        raise UnsupportedOperationError("adjoint requires a Matrix")
    return expression.adjoint()


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

def _native_operation(call):
    try:
        return call()
    except FortSymError as error:
        raise UnsupportedOperationError(str(error)) from error


def together(expression, deep=False, fraction=False):
    if deep or fraction:
        raise UnsupportedOperationError("together options")
    return _native_operation(lambda: sympify(expression).together())


def cancel(expression, *generators, **options):
    if generators or options:
        raise UnsupportedOperationError("cancel options")
    return _native_operation(lambda: sympify(expression).cancel())


def apart(expression, variable=None, full=False, **options):
    if full or options:
        raise UnsupportedOperationError("apart options")
    return _native_operation(
        lambda: sympify(expression).apart(
            None if variable is None else sympify(variable)
        )
    )


def collect(expression, variable, exact=False, distribute_order_term=None,
            evaluate=True):
    if exact or distribute_order_term is not None or not evaluate:
        raise UnsupportedOperationError("collect options")
    return _native_operation(
        lambda: sympify(expression).collect(sympify(variable))
    )


def integrate(expression, *variables, **options):
    if len(variables) != 1 or options:
        raise UnsupportedOperationError("integrate options or multiple variables")
    return _native_operation(
        lambda: sympify(expression).integrate(sympify(variables[0]))
    )


def _limit_direction(direction):
    if direction in ("-", -1):
        return -1
    if direction in ("+", 1):
        return 1
    if direction in ("+-", 0, None):
        return 0
    raise UnsupportedOperationError("limit direction")


def limit(expression, variable, point, dir="-", **options):
    if options:
        raise UnsupportedOperationError("limit options")
    variable = sympify(variable)
    point = sympify(point)
    if point == oo:
        point_kind = 1
        finite_point = None
    elif point == -oo:
        point_kind = 2
        finite_point = None
    else:
        point_kind = 0
        finite_point = point
    direction = _limit_direction(dir)
    return _native_operation(
        lambda: sympify(expression).limit(
            variable, finite_point, point_kind=point_kind, direction=direction
        )
    )


def series(expression, x=None, x0=0, n=6, dir="+", logx=None, cdir=0):
    if logx is not None or cdir not in (0, None):
        raise UnsupportedOperationError("series options")
    if dir not in ("+", "-", "+-", 1, -1, 0, None):
        raise UnsupportedOperationError("series direction")
    if not isinstance(n, int) or isinstance(n, bool):
        raise UnsupportedOperationError("series order")
    if n < 0:
        raise ValueError("Number of terms should be nonnegative")
    expression = sympify(expression)
    if x is None:
        symbols = expression.free_symbols
        if len(symbols) == 0:
            return expression
        if len(symbols) != 1:
            raise ValueError("x must be given for multivariate functions")
        x = next(iter(symbols))
    else:
        x = sympify(x)
    x0 = sympify(x0)
    if n == 0:
        return Integer(0)
    return _native_operation(
        lambda: expression.series(x, x0, n - 1)
    )


def solve(expression, *symbols, **options):
    """Return distinct verified roots for the bounded one-equation subset."""
    if len(symbols) > 1 or options:
        raise UnsupportedOperationError("solve options or multiple variables")
    expression = sympify(expression)
    variable = None if not symbols else sympify(symbols[0])
    return _native_operation(lambda: expression.solve(variable))


def solveset(expression, symbol=None, domain=None):
    """Return a verified finite root set for the bounded solveset fragment."""
    if domain is not None:
        raise UnsupportedOperationError("solveset domains")
    expression = sympify(expression)
    if symbol is None:
        symbols = expression.free_symbols
        if len(symbols) == 0:
            if expression == 0:
                raise UnsupportedOperationError("universal solveset result")
            return EmptySet
        if len(symbols) != 1:
            raise ValueError(
                "solveset without a variable needs exactly one free symbol"
            )
        symbol = next(iter(symbols))
    else:
        symbol = sympify(symbol)
    roots, excluded = _native_operation(lambda: expression._solveset(symbol))
    try:
        surviving = [root for root in roots
                     if not any(root == pole for pole in excluded)]
        base = EmptySet if not surviving else FiniteSet(*surviving)
        if not surviving or not excluded:
            return base
        if all(root.is_number and pole.is_number and root != pole
               for root in surviving for pole in excluded):
            return base
        excluded_set = FiniteSet(*excluded)
        try:
            return Complement(base, excluded_set)
        finally:
            base.close()
            excluded_set.close()
    finally:
        for root in roots:
            root.close()
        for pole in excluded:
            pole.close()


def _matrix_linsolve_rows(matrix, owned):
    if not isinstance(matrix, Matrix):
        return matrix
    if matrix.rows < 1 or matrix.cols < 1:
        raise UnsupportedOperationError("linsolve matrix dimensions")
    rows = []
    created = []
    try:
        for row in range(matrix.rows):
            entries = []
            for column in range(matrix.cols):
                entry = matrix._entry(row, column)
                entries.append(entry)
                created.append(entry)
            rows.append(entries)
        owned.extend(created)
        return rows
    except Exception:
        for entry in created:
            entry.close()
        raise


def _matrix_linsolve_rhs(right_hand_side, owned):
    if not isinstance(right_hand_side, Matrix):
        return right_hand_side
    if right_hand_side.rows < 1 or right_hand_side.cols < 1:
        raise UnsupportedOperationError("linsolve right-hand-side dimensions")
    if right_hand_side.rows == 1:
        indices = ((0, column) for column in range(right_hand_side.cols))
    elif right_hand_side.cols == 1:
        indices = ((row, 0) for row in range(right_hand_side.rows))
    else:
        raise UnsupportedOperationError("linsolve right-hand-side shape")
    values = []
    created = []
    try:
        for row, column in indices:
            value = right_hand_side._entry(row, column)
            values.append(value)
            created.append(value)
        owned.extend(created)
        return values
    except Exception:
        for entry in created:
            entry.close()
        raise


def linsolve(system, *symbols, **options):
    """Solve one explicit exact-rational linear system.

    The bounded native fragment accepts ``(matrix, right_hand_side)`` as
    nested Python sequences or native ``Matrix`` operands and requires the
    symbols explicitly. A Matrix right-hand side may be a row or column
    vector. Symbolic coefficients remain an explicit refusal. Consistent
    underdetermined systems retain the supplied free symbols in the result.
    """
    if options:
        raise UnsupportedOperationError("linsolve options")
    if len(symbols) != 1:
        raise UnsupportedOperationError("linsolve requires explicit symbols")
    if not isinstance(system, (tuple, list)) or len(system) != 2:
        raise UnsupportedOperationError("linsolve system form")
    matrix, right_hand_side = system
    owned = []
    try:
        matrix = _matrix_linsolve_rows(matrix, owned)
        if not isinstance(matrix, (tuple, list)) or not matrix:
            raise UnsupportedOperationError("linsolve matrix form")
        if not isinstance(matrix[0], (tuple, list)):
            raise UnsupportedOperationError("linsolve matrix rows")
        matrix_values = []
        for row in matrix:
            if not isinstance(row, (tuple, list)):
                raise UnsupportedOperationError("linsolve matrix rows")
            matrix_values.append([sympify(value) for value in row])
        matrix = matrix_values
        variable_count = len(matrix[0])
        if variable_count < 1 or any(
                len(row) != variable_count for row in matrix):
            raise UnsupportedOperationError("linsolve matrix dimensions")
        right_hand_side = _matrix_linsolve_rhs(right_hand_side, owned)
        if not isinstance(right_hand_side, (tuple, list)):
            raise UnsupportedOperationError("linsolve right-hand-side form")
        right_hand_side = [sympify(value) for value in right_hand_side]
        variables = symbols[0]
        if not isinstance(variables, (tuple, list)):
            raise UnsupportedOperationError("linsolve symbols form")
        variables = tuple(sympify(variable) for variable in variables)
        if len(variables) != variable_count:
            raise ValueError("linsolve symbols and matrix dimensions differ")
        if any(variable.kind != 4 for variable in variables):
            raise UnsupportedOperationError("linsolve symbols must be symbols")
        if any(left == right
               for index, left in enumerate(variables)
               for right in variables[index + 1:]):
            raise ValueError("linsolve symbols must be distinct")
        values = _native_operation(lambda: _default().linsolve_parametric(
            matrix, right_hand_side, variables
        ))
        try:
            if not values:
                return EmptySet
            result_tuple = Tuple(*values)
            try:
                return FiniteSet(result_tuple)
            finally:
                result_tuple.close()
        finally:
            for value in values:
                value.close()
    finally:
        for entry in owned:
            entry.close()


class Matrix:
    """Bounded exact dense matrix facade backed by one native List owner."""

    def __init__(self, rows):
        if not isinstance(rows, (tuple, list)) or not rows:
            raise UnsupportedOperationError("Matrix requires nonempty rows")
        nested = all(isinstance(row, (tuple, list)) for row in rows)
        if not nested and any(isinstance(row, (tuple, list)) for row in rows):
            raise UnsupportedOperationError("Matrix rows must be sequences")
        arena = _default()

        def make_list(values):
            native_values = []
            temporary_values = []
            try:
                for value in values:
                    if isinstance(value, str):
                        native_value = sympify(value)
                        temporary = None
                    else:
                        native_value, temporary = arena._coerce(value)
                    native_values.append(native_value)
                    if temporary is not None:
                        temporary_values.append(temporary)
                return arena.function("List", native_values)
            finally:
                for temporary in temporary_values:
                    temporary.close()

        if not nested:
            self._expression = make_list(rows)
            self.rows = len(rows)
            self.cols = 1
            self.shape = (self.rows, self.cols)
            self._column_vector = True
            return
        width = len(rows[0])
        if width == 0 or any(len(row) != width for row in rows):
            raise ValueError("Matrix rows must have equal nonzero length")
        native_rows = []
        row_handles = []
        try:
            for row in rows:
                native_row = make_list(row)
                native_rows.append(native_row)
                row_handles.append(native_row)
            self._expression = arena.function("List", native_rows)
        finally:
            for row_handle in row_handles:
                row_handle.close()
        self.rows = len(rows)
        self.cols = width
        self.shape = (self.rows, self.cols)
        self._column_vector = False

    def _matrix_binary(self, other, operation):
        if not isinstance(other, Matrix):
            return NotImplemented
        if self._expression._arena is not other._expression._arena:
            raise ValueError("matrix operands belong to different arenas")
        if self.shape != other.shape:
            raise ValueError("Matrix dimensions are not aligned")
        right_expression, temporary = other._matrix_expression()
        try:
            try:
                expression = self._expression._arena._result(
                    getattr(self._expression._arena._lib, operation),
                    self._expression._arena._require(),
                    self._expression._require(), right_expression._require(),
                )
            except FortSymError as error:
                raise UnsupportedOperationError(str(error)) from error
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.rows, self.cols)

    def __add__(self, other):
        return self._matrix_binary(other, "matrix_add")

    def __radd__(self, other):
        if isinstance(other, Matrix):
            return other.__add__(self)
        return NotImplemented

    def __sub__(self, other):
        return self._matrix_binary(other, "matrix_subtract")

    def multiply_elementwise(self, other):
        return self._matrix_binary(other, "matrix_multiply_elementwise")

    def __rsub__(self, other):
        if isinstance(other, Matrix):
            return other.__sub__(self)
        return NotImplemented

    def __neg__(self):
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.matrix_negate)
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(result, self.rows, self.cols)

    def __mul__(self, other):
        if isinstance(other, Matrix):
            if self._expression._arena is not other._expression._arena:
                raise ValueError("matrix operands belong to different arenas")
            if self.cols != other.rows:
                raise ValueError("Matrix dimensions are not aligned")
            right_expression, temporary = other._matrix_expression()
            try:
                expression = _native_operation(
                    lambda: self._expression.matrix_multiply(right_expression)
                )
            finally:
                if temporary is not None:
                    temporary.close()
            return self._from_expression(expression, self.rows, other.cols)
        scalar = sympify(other)
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(
                lambda: matrix_expression.matrix_multiply(scalar)
            )
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.rows, self.cols)

    def __rmul__(self, other):
        if isinstance(other, Matrix):
            return other.__mul__(self)
        scalar = sympify(other)
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(
                lambda: scalar.matrix_multiply(matrix_expression)
            )
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.rows, self.cols)

    def __truediv__(self, other):
        if isinstance(other, Matrix):
            return NotImplemented
        scalar = sympify(other)
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(
                lambda: matrix_expression.matrix_divide(scalar)
            )
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.rows, self.cols)

    def __rtruediv__(self, other):
        return NotImplemented

    def __matmul__(self, other):
        return self.__mul__(other)

    def __rmatmul__(self, other):
        return self.__rmul__(other)

    def __len__(self):
        return self.rows * self.cols

    @property
    def is_square(self):
        return self.rows == self.cols

    def __getitem__(self, key):
        if not isinstance(key, tuple):
            if isinstance(key, slice):
                return self._flat_slice(key)
            return self._flat_entry(self._flat_index(key))
        row, column = key
        if isinstance(row, slice) or isinstance(column, slice):
            row_indices = self._matrix_indices(row, self.rows)
            column_indices = self._matrix_indices(column, self.cols)
            return self._matrix_slice(row_indices, column_indices)
        return self._entry(
            self._matrix_index(row, self.rows),
            self._matrix_index(column, self.cols),
        )

    def _flat_index(self, index):
        value = _index(index)
        size = self.rows * self.cols
        if value < 0:
            value += size
        if value < 0 or value >= size:
            raise IndexError(f"Index out of range: a[{value}]")
        return value

    def _flat_entry(self, index):
        row, column = divmod(index, self.cols)
        return self._entry(row, column)

    def _flat_slice(self, index):
        values = []
        try:
            for flat_index in range(*index.indices(self.rows * self.cols)):
                values.append(self._flat_entry(flat_index))
            return values
        except Exception:
            for value in values:
                value.close()
            raise

    @staticmethod
    def _matrix_index(index, size):
        value = _index(index)
        if value < 0:
            value += size
        if value < 0 or value >= size:
            raise IndexError(f"Index out of range: a[{value}]")
        return value

    @staticmethod
    def _matrix_indices(index, size):
        if isinstance(index, slice):
            return range(*index.indices(size))
        return (Matrix._matrix_index(index, size),)

    def _matrix_slice(self, row_indices, column_indices):
        arena = self._expression._arena
        rows = []
        try:
            for row_index in row_indices:
                entries = []
                try:
                    for column_index in column_indices:
                        entries.append(self._entry(row_index, column_index))
                    rows.append(arena.function("List", entries))
                finally:
                    for entry in entries:
                        entry.close()
            expression = arena.function("List", rows)
        finally:
            for row in rows:
                row.close()
        return self._from_expression(
            expression, len(row_indices), len(column_indices)
        )

    def _entry(self, row, column):
        if self._column_vector:
            if column != 0:
                raise IndexError("column matrix has one column")
            return self._expression.argument(row)
        row_expression = self._expression.argument(row)
        try:
            return row_expression.argument(column)
        finally:
            row_expression.close()

    def det(self, **options):
        if options:
            raise UnsupportedOperationError("determinant options")
        expression, temporary = self._matrix_expression()
        try:
            return _native_operation(expression.det)
        finally:
            if temporary is not None:
                temporary.close()

    def trace(self):
        expression, temporary = self._matrix_expression()
        try:
            return _native_operation(expression.trace)
        finally:
            if temporary is not None:
                temporary.close()

    def is_diagonal(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_diagonal_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_diagonal)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_diagonal_cache = (epoch, result)
        return result

    @property
    def is_zero_matrix(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_zero_matrix_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_zero_matrix)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_zero_matrix_cache = (epoch, result)
        return result

    @property
    def is_upper(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_upper_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_upper)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_upper_cache = (epoch, result)
        return result

    @property
    def is_lower(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_lower_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_lower)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_lower_cache = (epoch, result)
        return result

    @property
    def is_upper_hessenberg(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_upper_hessenberg_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_upper_hessenberg)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_upper_hessenberg_cache = (epoch, result)
        return result

    @property
    def is_lower_hessenberg(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_lower_hessenberg_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_lower_hessenberg)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_lower_hessenberg_cache = (epoch, result)
        return result

    @property
    def is_Identity(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_Identity_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_identity)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_Identity_cache = (epoch, result)
        return result

    @property
    def is_echelon(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_echelon_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_echelon)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_echelon_cache = (epoch, result)
        return result

    @property
    def is_hermitian(self):
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_hermitian_cache", None)
        if cached is not None and cached[0] == epoch:
            return cached[1]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(expression.is_hermitian)
        finally:
            if temporary is not None:
                temporary.close()
        self._is_hermitian_cache = (epoch, result)
        return result

    def is_anti_symmetric(self, simplify=True):
        if simplify not in (False, True):
            raise UnsupportedOperationError("is_anti_symmetric options")
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_anti_symmetric_cache", None)
        if cached is not None and cached[:2] == (epoch, simplify):
            return cached[2]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(
                lambda: expression.is_anti_symmetric(simplify)
            )
        finally:
            if temporary is not None:
                temporary.close()
        self._is_anti_symmetric_cache = (epoch, simplify, result)
        return result

    def is_symbolic(self):
        expression, temporary = self._matrix_expression()
        try:
            return _native_operation(expression.is_symbolic)
        finally:
            if temporary is not None:
                temporary.close()

    def is_symmetric(self, simplify=True):
        if simplify not in (False, True):
            raise UnsupportedOperationError("is_symmetric options")
        epoch = self._expression._arena._assumption_epoch
        cached = getattr(self, "_is_symmetric_cache", None)
        if cached is not None and cached[:2] == (epoch, simplify):
            return cached[2]
        expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(
                lambda: expression.is_symmetric(simplify)
            )
        finally:
            if temporary is not None:
                temporary.close()
        self._is_symmetric_cache = (epoch, simplify, result)
        return result

    def rank(self, **options):
        simplify = options.pop("simplify", False)
        if simplify not in (False, True) or options:
            raise UnsupportedOperationError("rank options")
        expression, temporary = self._matrix_expression()
        try:
            return _native_operation(expression.rank)
        finally:
            if temporary is not None:
                temporary.close()

    @classmethod
    def _from_expression(cls, expression, rows, cols):
        matrix = cls.__new__(cls)
        matrix._expression = expression
        matrix.rows = rows
        matrix.cols = cols
        matrix.shape = (rows, cols)
        matrix._column_vector = False
        return matrix

    @classmethod
    def _from_column_expression(cls, expression, rows):
        matrix = cls.__new__(cls)
        matrix._expression = expression
        matrix.rows = rows
        matrix.cols = 1
        matrix.shape = (rows, 1)
        matrix._column_vector = True
        return matrix

    def _matrix_expression(self):
        if not self._column_vector:
            return self._expression, None
        row_handles = []
        try:
            for row_index in range(self.rows):
                entry = self._expression.argument(row_index)
                try:
                    row_handles.append(
                        self._expression._arena.function("List", [entry])
                    )
                finally:
                    entry.close()
            expression = self._expression._arena.function("List", row_handles)
        finally:
            for row in row_handles:
                row.close()
        return expression, expression

    def inv(self, **options):
        if options:
            raise UnsupportedOperationError("inverse options")
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(matrix_expression.inv)
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.rows, self.cols)

    def transpose(self):
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(matrix_expression.transpose)
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.cols, self.rows)

    def conjugate(self):
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(matrix_expression.matrix_conjugate)
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.rows, self.cols)

    def adjoint(self):
        matrix_expression, temporary = self._matrix_expression()
        try:
            expression = _native_operation(matrix_expression.matrix_adjoint)
        finally:
            if temporary is not None:
                temporary.close()
        return self._from_expression(expression, self.cols, self.rows)

    @property
    def T(self):
        return self.transpose()

    @property
    def H(self):
        return self.adjoint()

    def nullspace(self, simplify=False, iszerofunc=None):
        if simplify not in (False, True) or iszerofunc is not None:
            raise UnsupportedOperationError("nullspace options")
        matrix_expression, temporary = self._matrix_expression()
        try:
            basis = _native_operation(matrix_expression.nullspace)
        finally:
            if temporary is not None:
                temporary.close()
        vectors = []
        try:
            for index in range(basis.arity):
                vector = basis.argument(index)
                vectors.append(self._from_column_expression(vector, self.cols))
            return vectors
        finally:
            basis.close()

    def rref(self, iszerofunc=None, simplify=False, pivots=True,
             normalize_last=True):
        if (iszerofunc is not None or simplify not in (False, True) or
                pivots not in (True, False) or normalize_last is not True):
            raise UnsupportedOperationError("rref options")
        matrix_expression, temporary = self._matrix_expression()
        try:
            result = _native_operation(matrix_expression.rref)
        finally:
            if temporary is not None:
                temporary.close()
        reduced = None
        pivot_values = []
        try:
            reduced = result.argument(0)
            pivot_expression = result.argument(1)
            try:
                for index in range(pivot_expression.arity):
                    pivot = pivot_expression.argument(index)
                    try:
                        pivot_values.append(int(pivot.exact_text))
                    finally:
                        pivot.close()
            finally:
                pivot_expression.close()
            matrix = self._from_expression(reduced, self.rows, self.cols)
            reduced = None
            if pivots is False:
                return matrix
            return matrix, tuple(pivot_values)
        finally:
            if reduced is not None:
                reduced.close()
            result.close()

    def __str__(self):
        if self.rows == 0 or self.cols == 0:
            return f"Matrix({self.rows}, {self.cols}, [])"
        if self._column_vector:
            entries = []
            for row_index in range(self.rows):
                entry = self._expression.argument(row_index)
                try:
                    entries.append("[" + str(entry) + "]")
                finally:
                    entry.close()
            return "Matrix([" + ", ".join(entries) + "])"
        rows = []
        for row_index in range(self.rows):
            row = self._expression.argument(row_index)
            try:
                entries = []
                for column_index in range(self.cols):
                    entry = row.argument(column_index)
                    try:
                        entries.append(str(entry))
                    finally:
                        entry.close()
                rows.append("[" + ", ".join(entries) + "]")
            finally:
                row.close()
        return "Matrix([" + ", ".join(rows) + "])"

    __repr__ = __str__

    def __del__(self):
        try:
            self._expression.close()
        except Exception:
            pass


def det(matrix, **options):
    if options:
        raise UnsupportedOperationError("determinant options")
    if not isinstance(matrix, Matrix):
        raise UnsupportedOperationError("det requires a Matrix")
    return matrix.det()


def trace(matrix):
    if not isinstance(matrix, Matrix):
        raise UnsupportedOperationError("trace requires a Matrix")
    return matrix.trace()


def rank(matrix, **options):
    if not isinstance(matrix, Matrix):
        raise UnsupportedOperationError("rank requires a Matrix")
    return matrix.rank(**options)


pi = _default().constant("pi")
E = _default().constant("e")
I = _default().constant("i")
oo = _default().constant("oo")
zoo = _default().constant("zoo")
nan = _default().constant("nan")


__all__ = [
    "Arena", "Chart", "ChartMap", "FluxSurface", "FluxCoordinates", "MagneticChart", "MagneticField", "FourierWeakForm", "FourierSource", "FourierLoad", "Metric", "Connection", "Orientation", "Signature", "SpacetimeMetric", "SpacetimeForm", "SpacetimeTensor", "Tensor", "TensorIndexType", "TensorIndex", "TensorSymmetry", "Form", "Expr", "FortSymError", "UnsupportedOperationError",
    "FOURIER_INVALID", "FOURIER_LONGITUDINAL", "FOURIER_TRANSVERSE", "SPACE_NONE", "SPACE_NODAL", "SPACE_EDGE", "TRACE_NONE", "TRACE_NORMAL", "TRACE_TANGENTIAL", "FLUX_GENERIC", "FLUX_CLEBSCH", "FLUX_STRAIGHT_FIELD_LINE", "FLUX_BOOZER", "FLUX_HAMADA", "CLEBSCH_RESIDUAL_COUNT", "BOOZER_RESIDUAL_COUNT", "HAMADA_RESIDUAL_COUNT",
    "INDEX_TANGENT", "INDEX_COTANGENT", "INDEX_SPACETIME", "INDEX_INTERNAL", "INDEX_USER",
    "SPACETIME_DIM", "CONNECTION_STANDARD", "CONNECTION_OPPOSITE",
    "SYMMETRY_NONE", "SYMMETRIC", "ANTISYMMETRIC",
    "Manifold", "Patch", "CoordSystem", "CoordinateSymbol", "BaseScalarField",
    "BaseVectorField", "Differential", "WedgeProduct", "TensorProduct", "LieDerivative",
    "InconsistentAssumptions", "Tuple",
    "Symbol", "symbols", "sympify", "Integer", "Rational", "Float",
    "Add", "Mul", "Pow", "Function", "Wild", "Derivative", "Subs", "sin", "cos",
    "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh", "tanh", "csch",
    "sech", "coth", "erf", "erfc", "gamma", "loggamma", "factorial",
    "besselj", "besseli", "legendre", "expand_complex",
    "asinh", "acosh", "atanh", "exp", "log", "sqrt", "Abs", "sign",
    "floor", "ceiling", "re", "im", "conjugate", "adjoint", "arg", "diff", "subs", "expand",
    "simplify", "count_ops", "factor", "refine", "Eq", "Ne", "Gt", "Ge", "Lt", "Le", "And", "Or", "Not", "Xor", "Implies", "Equivalent",
    "Q", "ask", "assuming", "together", "cancel", "apart", "collect",
    "integrate", "limit", "series", "solve", "det", "trace", "rank", "solveset", "linsolve", "FiniteSet", "EmptySet", "Complement", "Tuple", "Matrix", "tensorproduct", "tensorcontraction", "tensorpermute", "pi", "E", "I",
    "oo", "zoo", "nan",
]
