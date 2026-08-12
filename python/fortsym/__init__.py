"""Small native Python facade over the stable fortsym C ABI.

The module deliberately depends only on the Python standard library.  It does
not import SymPy and it keeps the native arena alive through each expression
object that owns a handle into it.
"""

from __future__ import annotations

import ctypes
import ctypes.util
import os
from pathlib import Path
from threading import Lock
from fractions import Fraction
from functools import cached_property
import weakref
import sys
from typing import Iterable


_CVOID = ctypes.c_void_p
_CHAR_PTR = ctypes.POINTER(ctypes.c_char)
_SIZE = ctypes.c_size_t
_I64 = ctypes.c_int64
_FACT_REAL = 1
_FACT_POSITIVE = 2
_FACT_NONNEGATIVE = 4
_FACT_NONZERO = 8
_FACT_INTEGER = 16
_FACT_RATIONAL = 512
_FACT_ALGEBRAIC = 1024
_FACT_ZERO = 64
_FACT_NEGATIVE = 128
_FACT_NONPOSITIVE = 256
_CONFLICT = 7
_RELATIONS = {
    "Equal": ("==", 1),
    "Unequal": ("!=", 2),
    "Less": ("<", 3),
    "LessEqual": ("<=", 4),
    "Greater": (">", 5),
    "GreaterEqual": (">=", 6),
}


class FortSymError(RuntimeError):
    """A native operation failed with a status returned by the C ABI."""

    def __init__(self, status: int, message: str, operation: str = "operation"):
        self.status = status
        self.message = message
        self.operation = operation
        super().__init__(f"fortsym {operation} failed ({status}): {message}")


_library = None
_library_lock = Lock()


def _load_library():
    global _library
    if _library is not None:
        return _library
    with _library_lock:
        if _library is not None:
            return _library
        candidates = []
        configured = os.environ.get("FORTSYM_LIBRARY")
        if configured:
            candidates.append(Path(configured))
        package_root = Path(__file__).resolve().parents[2]
        candidates.extend(
            [
                Path(__file__).resolve().with_name("libfortsym.so"),
                Path(__file__).resolve().with_name("libfortsym.dylib"),
                Path(__file__).resolve().with_name("fortsym.dll"),
                package_root / "build" / "lib" / "libfortsym.so",
                package_root / "build" / "lib" / "libfortsym.dylib",
                package_root / "build" / "lib" / "fortsym.dll",
                Path(sys.prefix) / "lib" / "libfortsym.so",
                Path(sys.prefix) / "lib" / "libfortsym.dylib",
            ]
        )
        discovered = ctypes.util.find_library("fortsym")
        if discovered:
            candidates.append(Path(discovered))
        configured_error = None
        last_error = None
        for candidate in candidates:
            try:
                _library = ctypes.CDLL(str(candidate))
                _configure(_library)
                return _library
            except (OSError, AttributeError) as error:
                last_error = error
                if configured and candidate == Path(configured):
                    configured_error = error
                continue
        error = configured_error if configured_error is not None else last_error
        detail = f" ({error})" if error is not None else ""
        raise ImportError(
            "fortsym's native library was not found; set FORTSYM_LIBRARY "
            f"to libfortsym.so{detail}"
        )


def _configure(lib):
    def declare(name, result, arguments):
        function = getattr(lib, name)
        function.restype = result
        function.argtypes = arguments
        return function

    lib.abi_version = declare("fortsym_abi_version", ctypes.c_int, [])
    lib.arena_new = declare("fortsym_arena_new", ctypes.c_int,
                            [ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE])
    lib.arena_free = declare("fortsym_arena_free", None, [_CVOID])
    for name in ("int", "rational", "real", "exact", "symbol", "constant"):
        c_name = "fortsym_" + name
        if name == "int":
            args = [_CVOID, _I64, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE]
        elif name == "rational":
            args = [_CVOID, _I64, _I64, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE]
        elif name == "real":
            args = [_CVOID, ctypes.c_double, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE]
        else:
            args = [_CVOID, ctypes.c_char_p, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE]
        setattr(lib, name, declare(c_name, ctypes.c_int, args))
    for name in ("add", "subtract", "multiply", "divide", "power"):
        setattr(
            lib,
            name,
            declare(
                "fortsym_" + name,
                ctypes.c_int,
                [_CVOID, _CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
            ),
        )
    lib.function = declare(
        "fortsym_function",
        ctypes.c_int,
        [_CVOID, ctypes.c_char_p, ctypes.POINTER(_CVOID), _SIZE,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.relation = declare(
        "fortsym_relation", ctypes.c_int,
        [_CVOID, _CVOID, _CVOID, ctypes.c_int, ctypes.POINTER(_CVOID),
         _CHAR_PTR, _SIZE],
    )
    lib.substitute = declare(
        "fortsym_substitute",
        ctypes.c_int,
        [_CVOID, _CVOID, _CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.substitute_many = declare(
        "fortsym_substitute_many",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.differentiate = declare(
        "fortsym_differentiate",
        ctypes.c_int,
        [_CVOID, _CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_sqrtg = declare(
        "fortsym_chart_sqrtg",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _SIZE,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_jacobian = declare(
        "fortsym_chart_jacobian",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _SIZE,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    for name in ("b_cov", "b_fourier", "b_fourier_density"):
        arguments = [
            _CVOID,
            ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID),
        ]
        if name != "b_cov":
            arguments.append(_CVOID)
        arguments.extend([ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE])
        setattr(
            lib,
            "chart_" + name,
            declare("fortsym_chart_" + name, ctypes.c_int, arguments),
        )
    for name in ("covariant_basis", "reciprocal_basis", "metric_covariant",
                 "metric_contravariant", "christoffel",
                 "riemann", "ricci", "einstein"):
        setattr(
            lib,
            "chart_" + name,
            declare(
                "fortsym_chart_" + name,
                ctypes.c_int,
                [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
                 ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
            ),
        )
    lib.chart_covariant_diff = declare(
        "fortsym_chart_covariant_diff",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_scalar_curvature = declare(
        "fortsym_chart_scalar_curvature",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    form_binary_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _SIZE,
        ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    for name in ("add", "subtract", "wedge"):
        setattr(
            lib,
            "chart_form_" + name,
            declare(
                "fortsym_chart_form_" + name, ctypes.c_int,
                form_binary_arguments,
            ),
        )
    form_unary_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
        _SIZE,
    ]
    for name in ("negate", "d", "star"):
        setattr(
            lib,
            "chart_form_" + name,
            declare(
                "fortsym_chart_form_" + name, ctypes.c_int,
                form_unary_arguments,
            ),
        )
    lib.chart_form_scale = declare(
        "fortsym_chart_form_scale", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, _CVOID, ctypes.POINTER(_CVOID),
         _CHAR_PTR, _SIZE],
    )
    form_vector_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _SIZE,
        ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    for name in ("interior", "lie"):
        setattr(
            lib,
            "chart_form_" + name,
            declare(
                "fortsym_chart_form_" + name, ctypes.c_int,
                form_vector_arguments,
            ),
        )
    lib.chart_form_flat = declare(
        "fortsym_chart_form_flat", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_form_sharp = declare(
        "fortsym_chart_form_sharp", ctypes.c_int,
        form_unary_arguments,
    )
    lib.expand = declare(
        "fortsym_expand",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.simplify = declare(
        "fortsym_simplify",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.factor = declare(
        "fortsym_factor",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.zero_test = declare(
        "fortsym_zero_test",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.POINTER(ctypes.c_int), _CHAR_PTR, _SIZE],
    )
    lib.complex_operation = declare(
        "fortsym_complex_operation",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.c_char_p, ctypes.POINTER(_CVOID), _CHAR_PTR,
         _SIZE],
    )
    lib.assume = declare(
        "fortsym_assume",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.c_int, _CHAR_PTR, _SIZE],
    )
    lib.assume_relation = declare(
        "fortsym_assume_relation", ctypes.c_int,
        [_CVOID, _CVOID, _CHAR_PTR, _SIZE],
    )
    lib.assumption_push = declare(
        "fortsym_assumption_push", ctypes.c_int, [_CVOID, _CHAR_PTR, _SIZE]
    )
    lib.assumption_pop = declare(
        "fortsym_assumption_pop", ctypes.c_int, [_CVOID, _CHAR_PTR, _SIZE]
    )
    lib.assumption_has = declare(
        "fortsym_assumption_has",
        ctypes.c_int,
        [_CVOID, _CVOID, ctypes.c_int, ctypes.POINTER(ctypes.c_int),
         _CHAR_PTR, _SIZE],
    )
    lib.expr_free = declare("fortsym_expr_free", None, [_CVOID])
    lib.expr_kind = declare(
        "fortsym_expr_kind", ctypes.c_int,
        [_CVOID, ctypes.POINTER(ctypes.c_int), _CHAR_PTR, _SIZE],
    )
    lib.expr_is_number = declare(
        "fortsym_expr_is_number", ctypes.c_int,
        [_CVOID, ctypes.POINTER(ctypes.c_int), _CHAR_PTR, _SIZE],
    )
    lib.expr_is_algebraic = declare(
        "fortsym_expr_is_algebraic", ctypes.c_int,
        [_CVOID, ctypes.POINTER(ctypes.c_int), _CHAR_PTR, _SIZE],
    )
    lib.expr_arity = declare(
        "fortsym_expr_arity", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_SIZE), _CHAR_PTR, _SIZE],
    )
    lib.expr_argument = declare(
        "fortsym_expr_argument", ctypes.c_int,
        [_CVOID, _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.expr_equal = declare(
        "fortsym_expr_equal", ctypes.c_int,
        [_CVOID, _CVOID, ctypes.POINTER(ctypes.c_int), _CHAR_PTR, _SIZE],
    )
    lib.expr_node_count = declare(
        "fortsym_expr_node_count", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_SIZE), _CHAR_PTR, _SIZE],
    )
    lib.expr_operation_count = declare(
        "fortsym_expr_operation_count", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_SIZE), _CHAR_PTR, _SIZE],
    )
    lib.expr_free_symbols = declare(
        "fortsym_expr_free_symbols", ctypes.c_int,
        [_CVOID, _CHAR_PTR, _SIZE, ctypes.POINTER(_SIZE), _CHAR_PTR, _SIZE],
    )
    for name in ("text", "name", "exact_text"):
        setattr(
            lib,
            "expr_" + name,
            declare(
                "fortsym_expr_" + name,
                ctypes.c_int,
                [_CVOID, _CHAR_PTR, _SIZE, ctypes.POINTER(_SIZE), _CHAR_PTR, _SIZE],
            ),
        )
    lib.expr_int_value = declare(
        "fortsym_expr_int_value", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_I64), _CHAR_PTR, _SIZE],
    )
    lib.expr_real_value = declare(
        "fortsym_expr_real_value", ctypes.c_int,
        [_CVOID, ctypes.POINTER(ctypes.c_double), _CHAR_PTR, _SIZE],
    )


def _message():
    return ctypes.create_string_buffer(256)


def _decode(message):
    return message.value.decode("utf-8", "replace")


def _split_top_level(text, separator):
    pieces = []
    start = 0
    depth = 0
    index = 0
    while index < len(text):
        char = text[index]
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        elif separator == "*" and text.startswith("**", index):
            index += 2
            continue
        elif depth == 0 and text.startswith(separator, index):
            pieces.append(text[start:index])
            start = index + len(separator)
            index = start
            continue
        index += 1
    pieces.append(text[start:])
    return pieces


def _pretty_expanded(text):
    terms = _split_top_level(text, " + ")
    rendered = []
    for term in terms:
        factors = _split_top_level(term, "*")
        numeric = [factor for factor in factors if factor.lstrip("-").isdigit()]
        if numeric and len(factors) > 1:
            first = numeric[0]
            factors.remove(first)
            factors.insert(0, first)
        rendered.append("*".join(factors))
    return " + ".join(rendered)


class Arena:
    """An independent expression context."""

    def __init__(self):
        self._lib = _load_library()
        self._handle = _CVOID()
        self._integer_cache = {}
        self._assumption_epoch = 0
        self._algebraic_cache = weakref.WeakSet()
        message = _message()
        status = self._lib.arena_new(ctypes.byref(self._handle), message, len(message))
        if status:
            raise FortSymError(status, _decode(message), "arena_new")

    def close(self):
        if self._handle is not None:
            for expression in self._integer_cache.values():
                expression.close()
            self._integer_cache.clear()
            self._lib.arena_free(self._handle)
            self._handle = None

    def __enter__(self):
        return self

    def __exit__(self, *_):
        self.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass

    def _require(self):
        if self._handle is None:
            raise FortSymError(2, "arena is closed", "arena")
        return self._handle

    def _assumption_changed(self):
        self._assumption_epoch += 1
        for expression in self._algebraic_cache:
            expression.__dict__.pop("is_algebraic", None)
        self._algebraic_cache.clear()

    def _result(self, function, *arguments):
        output = _CVOID()
        message = _message()
        status = function(*arguments, ctypes.byref(output), message, len(message))
        if status:
            operation = function.__name__.removeprefix("fortsym_")
            raise FortSymError(status, _decode(message), operation)
        return Expr(self, output)

    def integer(self, value: int):
        value = int(value)
        if -(1 << 63) <= value <= (1 << 63) - 1:
            return self._result(self._lib.int, self._require(), value)
        return self.exact(str(value))

    def rational(self, numerator: int, denominator: int):
        numerator, denominator = int(numerator), int(denominator)
        if (-(1 << 63) <= numerator <= (1 << 63) - 1 and
                -(1 << 63) <= denominator <= (1 << 63) - 1):
            return self._result(self._lib.rational, self._require(), numerator,
                                denominator)
        return self.exact(f"{numerator}/{denominator}")

    def real(self, value: float):
        return self._result(self._lib.real, self._require(), float(value))

    def exact(self, value: str):
        return self._result(self._lib.exact, self._require(), value.encode())

    def symbol(self, name: str):
        return self._result(self._lib.symbol, self._require(), name.encode())

    def constant(self, name: str):
        return self._result(self._lib.constant, self._require(), name.encode())

    def assume(self, expression: "Expr", fact: int):
        expression = self._check(expression)
        message = _message()
        status = self._lib.assume(self._require(), expression._handle,
                                  int(fact), message, len(message))
        if status:
            raise FortSymError(status, _decode(message), "assume")
        self._assumption_changed()

    def assume_relation(self, relation: "Expr"):
        relation = self._check(relation)
        message = _message()
        status = self._lib.assume_relation(
            self._require(), relation._handle, message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "assume_relation")
        self._assumption_changed()

    def _assumption_push(self):
        message = _message()
        status = self._lib.assumption_push(
            self._require(), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "assumption_push")
        self._assumption_changed()

    def _assumption_pop(self):
        message = _message()
        status = self._lib.assumption_pop(
            self._require(), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "assumption_pop")
        self._assumption_changed()

    def assuming(self, *facts):
        return _AssumptionScope(self, facts)

    def function(self, name: str, arguments: Iterable["Expr"]):
        values = list(arguments)
        handles = (_CVOID * len(values))(
            *[self._check(expr)._handle for expr in values])
        output = _CVOID()
        message = _message()
        status = self._lib.function(self._require(), name.encode(), handles, len(values),
                                    ctypes.byref(output), message, len(message))
        if status:
            raise FortSymError(status, _decode(message), "function")
        return Expr(self, output)

    def _chart_sqrtg(self, coordinates, position):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        return self._result(
            self._lib.chart_sqrtg,
            self._require(), coordinate_handles, position_handles, 3,
        )

    def _chart_jacobian(self, coordinates, position):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        return self._result(
            self._lib.chart_jacobian,
            self._require(), coordinate_handles, position_handles, 3,
        )

    def _chart_many(self, operation, coordinates, position, values, mode=None):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        checked = [self._check(value) for value in values]
        value_handles = (_CVOID * 3)(*[value._handle for value in checked])
        output = (_CVOID * 3)()
        message = _message()
        arguments = [
            self._require(), coordinate_handles, position_handles, value_handles,
        ]
        if mode is not None:
            mode, temporary = self._coerce(mode)
            try:
                arguments.append(mode._handle)
                arguments.extend([output, message, len(message)])
                status = operation(*arguments)
            finally:
                if temporary is not None:
                    temporary.close()
        else:
            arguments.extend([output, message, len(message)])
            status = operation(*arguments)
        if status:
            name = operation.__name__.removeprefix("fortsym_chart_")
            raise FortSymError(status, _decode(message), name)
        return tuple(Expr(self, output[index]) for index in range(3))

    def _chart_tensor(self, operation, coordinates, position, rank):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        count = 3 ** rank
        output = (_CVOID * count)()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles, output,
            message, len(message)
        )
        if status:
            name = operation.__name__.removeprefix("fortsym_chart_")
            raise FortSymError(status, _decode(message), name)
        return tuple(Expr(self, output[index]) for index in range(count))

    def _chart_covariant_diff(self, chart, tensor):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        component_values = [self._check(value) for value in tensor.components]
        component_handles = (_CVOID * len(component_values))(
            *[value._handle for value in component_values]
        )
        variance_values = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_count = 3 ** (tensor.rank + 1)
        output = (_CVOID * output_count)()
        message = _message()
        status = self._lib.chart_covariant_diff(
            self._require(), coordinate_handles, position_handles,
            component_handles, tensor.rank, variance_values,
            tensor.density_weight, output, message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "covariant_diff")
        return tuple(Expr(self, output[index]) for index in range(output_count))

    def _chart_scalar(self, operation, coordinates, position):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        return self._result(
            operation, self._require(), coordinate_handles, position_handles,
        )

    def _chart_form_output(self, operation, coordinates, position, arguments,
                           count):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        output = (_CVOID * count)()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles, *arguments,
            output, message, len(message)
        )
        if status:
            name = operation.__name__.removeprefix("fortsym_chart_")
            raise FortSymError(status, _decode(message), name)
        return tuple(Expr(self, output[index]) for index in range(count))

    def _chart_form_unary(self, operation, form):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        return self._chart_form_output(
            operation, form.chart.coordinates, form.chart.position,
            [components, form.degree], 8,
        )

    def _chart_form_binary(self, operation, left, right):
        left_components = (_CVOID * 8)(
            *[self._check(value)._handle for value in left.components]
        )
        right_components = (_CVOID * 8)(
            *[self._check(value)._handle for value in right.components]
        )
        return self._chart_form_output(
            operation, left.chart.coordinates, left.chart.position,
            [left_components, left.degree, right_components, right.degree], 8,
        )

    def _chart_form_scale(self, form, factor):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        factor, temporary = self._coerce(factor)
        try:
            return self._chart_form_output(
                self._lib.chart_form_scale,
                form.chart.coordinates, form.chart.position,
                [components, form.degree, factor._handle], 8,
            )
        finally:
            if temporary is not None:
                temporary.close()

    def _chart_form_vector(self, operation, form, vector):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        vector_values = tuple(self._check(value) for value in vector)
        if len(vector_values) != 3:
            raise ValueError("form vector operations require three components")
        vector_handles = (_CVOID * 3)(
            *[value._handle for value in vector_values]
        )
        return self._chart_form_output(
            operation, form.chart.coordinates, form.chart.position,
            [vector_handles, components, form.degree], 8,
        )

    def _chart_form_flat(self, chart, vector):
        vector_values = tuple(self._check(value) for value in vector)
        if len(vector_values) != 3:
            raise ValueError("flat requires three vector components")
        vector_handles = (_CVOID * 3)(
            *[value._handle for value in vector_values]
        )
        return self._chart_form_output(
            self._lib.chart_form_flat, chart.coordinates, chart.position,
            [vector_handles], 8,
        )

    def _chart_form_sharp(self, form):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        return self._chart_form_output(
            self._lib.chart_form_sharp,
            form.chart.coordinates, form.chart.position,
            [components, form.degree], 3,
        )

    def _chart_inputs(self, coordinates, position):
        coordinates = tuple(self._check(value) for value in coordinates)
        position = tuple(self._check(value) for value in position)
        if len(coordinates) != 3 or len(position) != 3:
            raise ValueError("fortsym charts require three coordinates and positions")
        coordinate_handles = (_CVOID * 3)(
            *[value._handle for value in coordinates]
        )
        position_handles = (_CVOID * 3)(
            *[value._handle for value in position]
        )
        return coordinate_handles, position_handles

    def relation(self, left: "Expr", right: "Expr", name: str):
        left = self._check(left)
        right, temporary = self._coerce(right)
        try:
            output = _CVOID()
            message = _message()
            status = self._lib.relation(
                self._require(), left._handle, right._handle,
                _RELATIONS[name][1], ctypes.byref(output), message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), "relation")
            return Expr(self, output)
        finally:
            if temporary is not None:
                temporary.close()

    def _coerce(self, value):
        if isinstance(value, Expr):
            return self._check(value), None
        pattern_materializer = getattr(value, "_materialize_pattern", None)
        if pattern_materializer is not None:
            return self._check(pattern_materializer()), None
        pattern_expression = getattr(value, "_pattern_expression", None)
        if pattern_expression is not None:
            return self._check(pattern_expression), None
        if isinstance(value, Fraction):
            expression = self.rational(value.numerator, value.denominator)
            return expression, expression
        if isinstance(value, int):
            if -10 <= value <= 10:
                expression = self._integer_cache.get(value)
                if expression is None:
                    expression = self.integer(value)
                    self._integer_cache[value] = expression
                return expression, None
            expression = self.integer(value)
            return expression, expression
        if isinstance(value, float):
            expression = self.real(value)
            return expression, expression
        raise TypeError(f"cannot convert {type(value).__name__} to an expression")

    def _check(self, expression):
        if not isinstance(expression, Expr) or expression._arena is not self:
            raise ValueError("expression belongs to another arena")
        expression._require()
        return expression


class Chart:
    """A native three-dimensional coordinate chart.

    The object is a Python lifetime and spelling facade. Metric, tensor,
    connection, volume, and Fourier operations execute in native Fortran
    owners.
    """

    def __init__(self, coordinates, position):
        self.coordinates = tuple(coordinates)
        self.position = tuple(position)
        if len(self.coordinates) != 3 or len(self.position) != 3:
            raise ValueError("fortsym charts require three coordinates and positions")
        self._arena = self.coordinates[0]._arena
        self._arena._chart_inputs(self.coordinates, self.position)

    def sqrtg(self):
        return self._arena._chart_sqrtg(self.coordinates, self.position)

    def jacobian(self):
        """Return the signed determinant of the chart Jacobian."""
        return self._arena._chart_jacobian(self.coordinates, self.position)

    def covariant_basis(self):
        """Return ``e(component, index)`` in component-first flat order."""
        return self._basis_result(self._arena._lib.chart_covariant_basis)

    def reciprocal_basis(self):
        """Return the dual basis ``e^index`` in component-first flat order."""
        return self._basis_result(self._arena._lib.chart_reciprocal_basis)

    def tensor(self, components, variance=(), density_weight=0):
        return Tensor(self, components, variance, density_weight)

    def scalar(self, value, density_weight=0):
        return self.tensor((value,), (), density_weight)

    def metric(self, variance="covariant"):
        if variance in ("covariant", "lower", -1):
            return self.metric_covariant()
        if variance in ("contravariant", "upper", 1):
            return self.metric_contravariant()
        raise ValueError("metric variance must be covariant or contravariant")

    def metric_covariant(self):
        return self._tensor_result(
            self._arena._lib.chart_metric_covariant, 2, (-1, -1)
        )

    def metric_contravariant(self):
        return self._tensor_result(
            self._arena._lib.chart_metric_contravariant, 2, (1, 1)
        )

    def christoffel(self):
        return self._tensor_result(
            self._arena._lib.chart_christoffel, 3, (1, -1, -1)
        )

    def covariant_diff(self, tensor):
        if not isinstance(tensor, Tensor) or tensor.chart is not self:
            raise ValueError("covariant_diff expects a tensor from this chart")
        components = self._arena._chart_covariant_diff(self, tensor)
        return Tensor(
            self, components, tensor.variance + (-1,), tensor.density_weight,
            _owned=True,
        )

    covariant_derivative = covariant_diff

    def riemann(self):
        return self._tensor_result(
            self._arena._lib.chart_riemann, 4, (1, -1, -1, -1)
        )

    def ricci(self):
        return self._tensor_result(
            self._arena._lib.chart_ricci, 2, (-1, -1)
        )

    def scalar_curvature(self):
        return self._arena._chart_scalar(
            self._arena._lib.chart_scalar_curvature,
            self.coordinates, self.position,
        )

    def einstein(self):
        return self._tensor_result(
            self._arena._lib.chart_einstein, 2, (-1, -1)
        )

    def _tensor_result(self, operation, rank, variance, density_weight=0):
        components = self._arena._chart_tensor(
            operation, self.coordinates, self.position, rank
        )
        return Tensor(self, components, variance, density_weight, _owned=True)

    def _basis_result(self, operation):
        components = self._arena._chart_tensor(
            operation, self.coordinates, self.position, 2
        )
        return components

    def form(self, components, degree=0):
        return Form(self, components, degree)

    def scalar_form(self, value):
        return Form(self, (value,), 0)

    def one_form(self, values):
        return Form(self, values, 1)

    def two_form(self, values):
        return Form(self, values, 2)

    def three_form(self, value):
        return Form(self, (value,), 3)

    def flat(self, vector):
        components = self._arena._chart_form_flat(self, vector)
        return Form(self, components, 1, _owned=True)

    def b_cov(self, vector):
        return self._arena._chart_many(
            self._arena._lib.chart_b_cov,
            self.coordinates, self.position, vector,
        )

    def b_fourier(self, potential, mode):
        return self._arena._chart_many(
            self._arena._lib.chart_b_fourier,
            self.coordinates, self.position, potential, mode,
        )

    def b_fourier_density(self, potential, mode):
        return self._arena._chart_many(
            self._arena._lib.chart_b_fourier_density,
            self.coordinates, self.position, potential, mode,
        )


class Tensor:
    """A chart-bound native tensor view with explicit slot metadata.

    Components are flat in first-slot-fastest order, as in the Fortran tensor
    owner. Python component indices are zero-based and may be supplied through
    ``tensor[i, j]`` or ``tensor.component(i, j)``.
    """

    def __init__(self, chart, components, variance=(), density_weight=0,
                 _owned=False):
        if not isinstance(chart, Chart):
            raise TypeError("Tensor requires a fortsym Chart")
        self.chart = chart
        self._arena = chart._arena
        self.variance = tuple(int(value) for value in variance)
        self.rank = len(self.variance)
        if self.rank > 4:
            raise ValueError("native tensors support rank at most four")
        expected = 3 ** self.rank
        values = tuple(components)
        if len(values) != expected:
            raise ValueError(
                f"rank {self.rank} tensor requires {expected} components"
            )
        if any(value not in (-1, 1) for value in self.variance):
            raise ValueError("tensor variance entries must be -1 or 1")
        component_values = []
        temporaries = []
        for value in values:
            coerced, temporary = self._arena._coerce(value)
            component_values.append(coerced)
            if temporary is not None:
                temporaries.append(temporary)
        self.components = tuple(component_values)
        self._temporaries = tuple(temporaries)
        self.density_weight = int(density_weight)
        self._owned = bool(_owned)

    def component(self, *indices):
        if len(indices) != self.rank:
            raise IndexError("tensor index rank does not match")
        flat = 0
        scale = 1
        for index in indices:
            index = int(index)
            if index < 0 or index >= 3:
                raise IndexError("tensor index is outside the chart dimension")
            flat += index * scale
            scale *= 3
        value = self.components[flat]
        # A component is a borrowed view of the tensor-owned handle. Keep the
        # owner alive for expressions obtained from a temporary Tensor, e.g.
        # ``chart.riemann()[0, 0, 0, 0].simplify()``.
        value._tensor_owner = self
        return value

    def __getitem__(self, indices):
        if not isinstance(indices, tuple):
            indices = (indices,)
        return self.component(*indices)

    def __len__(self):
        return len(self.components)

    def __iter__(self):
        return iter(self.components)

    def covariant_diff(self):
        return self.chart.covariant_diff(self)

    covariant_derivative = covariant_diff

    def close(self):
        if self._owned:
            for component in self.components:
                component.close()
        for temporary in self._temporaries:
            temporary.close()
        self._temporaries = ()
        self.components = ()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class Form:
    """A chart-bound native differential form.

    The native owner stores all eight three-dimensional basis masks. Public
    constructors accept the nonzero ordered coefficients for the declared
    degree, while ``component(mask)`` uses the native bit-mask spelling.
    """

    def __init__(self, chart, components, degree=0, _owned=False):
        if not isinstance(chart, Chart):
            raise TypeError("Form requires a fortsym Chart")
        degree = int(degree)
        if degree < 0 or degree > 4:
            raise ValueError("native forms support degrees zero through four")
        self.chart = chart
        self._arena = chart._arena
        self.degree = degree
        values = self._normalise_components(components, degree)
        component_values = []
        temporaries = []
        for value in values:
            coerced, temporary = self._arena._coerce(value)
            component_values.append(coerced)
            if temporary is not None:
                temporaries.append(temporary)
        self.components = tuple(component_values)
        self._temporaries = tuple(temporaries)
        self._owned = bool(_owned)

    @staticmethod
    def _normalise_components(components, degree):
        if degree == 0 and isinstance(components, (Expr, int, float, Fraction)):
            values = (components,)
        else:
            values = tuple(components)
        if len(values) == 8:
            return values
        if degree == 0 and len(values) == 1:
            return (values[0], 0, 0, 0, 0, 0, 0, 0)
        if degree == 1 and len(values) == 3:
            return (0, values[0], values[1], 0, values[2], 0, 0, 0)
        if degree == 2 and len(values) == 3:
            return (0, 0, 0, values[0], 0, values[1], values[2], 0)
        if degree == 3 and len(values) == 1:
            return (0, 0, 0, 0, 0, 0, 0, values[0])
        if degree == 4 and len(values) == 0:
            return (0, 0, 0, 0, 0, 0, 0, 0)
        raise ValueError("form coefficients do not match the declared degree")

    def component(self, mask):
        mask = int(mask)
        if mask < 0 or mask >= 8:
            raise IndexError("form mask is outside the three-dimensional basis")
        value = self.components[mask]
        value._form_owner = self
        return value

    def __getitem__(self, mask):
        return self.component(mask)

    def __len__(self):
        return 8

    def __iter__(self):
        for mask in range(8):
            yield self.component(mask)

    def d(self):
        degree = min(self.degree + 1, 4)
        components = self._arena._chart_form_unary(
            self._arena._lib.chart_form_d, self
        )
        return Form(self.chart, components, degree, _owned=True)

    exterior_diff = d

    def star(self):
        components = self._arena._chart_form_unary(
            self._arena._lib.chart_form_star, self
        )
        return Form(self.chart, components, 3 - self.degree, _owned=True)

    hodge_star = star

    def wedge(self, other):
        self._check_other(other)
        components = self._arena._chart_form_binary(
            self._arena._lib.chart_form_wedge, self, other
        )
        return Form(self.chart, components, self.degree + other.degree,
                    _owned=True)

    def interior(self, vector):
        components = self._arena._chart_form_vector(
            self._arena._lib.chart_form_interior, self, vector
        )
        return Form(self.chart, components, max(0, self.degree - 1),
                    _owned=True)

    interior_product = interior

    def lie(self, vector):
        components = self._arena._chart_form_vector(
            self._arena._lib.chart_form_lie, self, vector
        )
        return Form(self.chart, components, self.degree, _owned=True)

    lie_derivative = lie

    def sharp(self):
        return self._arena._chart_form_sharp(self)

    def scale(self, factor):
        components = self._arena._chart_form_scale(self, factor)
        return Form(self.chart, components, self.degree, _owned=True)

    def __add__(self, other):
        self._check_other(other)
        components = self._arena._chart_form_binary(
            self._arena._lib.chart_form_add, self, other
        )
        return Form(self.chart, components, self.degree, _owned=True)

    def __sub__(self, other):
        self._check_other(other)
        components = self._arena._chart_form_binary(
            self._arena._lib.chart_form_subtract, self, other
        )
        return Form(self.chart, components, self.degree, _owned=True)

    def __neg__(self):
        components = self._arena._chart_form_unary(
            self._arena._lib.chart_form_negate, self
        )
        return Form(self.chart, components, self.degree, _owned=True)

    def __mul__(self, other):
        if isinstance(other, Form):
            return self.wedge(other)
        return self.scale(other)

    def __rmul__(self, other):
        return self.scale(other)

    def _check_other(self, other):
        if not isinstance(other, Form) or other.chart is not self.chart:
            raise ValueError("form operation expects a form from this chart")

    def close(self):
        if self._owned:
            for component in self.components:
                component.close()
        for temporary in self._temporaries:
            temporary.close()
        self._temporaries = ()
        self.components = ()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class Expr:
    """An immutable native expression handle."""

    def __init__(self, arena: Arena, handle):
        self._arena = arena
        self._lib = arena._lib
        self._handle = handle
        self._known_facts = 0
        self._pretty = False
        self._expanded_result = None
        self._expanded_epoch = -1
        self._diff_results = {}
        self._complex_results = {}
        self._match_results = {}
        self._replace_results = {}
        self._number_value = None
        self._free_symbols_cache = None
        self._node_count_cache = None
        self._kind_cache = None
        self._arity_cache = None
        self._name_cache = None
        self._borrowed = False

    def _require(self):
        if self._handle is None:
            raise FortSymError(2, "expression is closed", "expression")
        return self._handle

    def close(self):
        if self._borrowed:
            return
        self._expanded_result = None
        self._diff_results.clear()
        self._complex_results.clear()
        self._match_results.clear()
        self._replace_results.clear()
        self._number_value = None
        self._node_count_cache = None
        self._kind_cache = None
        self._arity_cache = None
        self._name_cache = None
        self.__dict__.pop("_wildcard_patterns", None)
        self.__dict__.pop("_wildcard_matcher", None)
        if self._free_symbols_cache is not None:
            for symbol in self._free_symbols_cache:
                symbol._release()
            self._free_symbols_cache = None
        self.__dict__.pop("is_algebraic", None)
        if self._handle is not None:
            self._release()

    def _release(self):
        if self._handle is not None:
            self._lib.expr_free(self._handle)
            self._handle = None

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass

    def _binary(self, function, other):
        pattern_materializer = getattr(other, "_materialize_pattern", None)
        if pattern_materializer is not None:
            other = pattern_materializer()
        other, temporary = self._arena._coerce(other)
        try:
            result = self._arena._result(
                function, self._arena._require(), self._require(),
                other._handle,
            )
            patterns = {}
            patterns.update(getattr(self, "_wildcard_patterns", {}))
            patterns.update(getattr(other, "_wildcard_patterns", {}))
            if patterns:
                result._wildcard_patterns = patterns
                matcher = (getattr(self, "_wildcard_matcher", None) or
                           getattr(other, "_wildcard_matcher", None))
                if matcher is not None:
                    result._wildcard_matcher = matcher
            return result
        finally:
            if temporary is not None:
                temporary.close()

    def _reverse_binary(self, function, other):
        left, temporary = self._arena._coerce(other)
        try:
            return left._binary(function, self)
        finally:
            if temporary is not None:
                temporary.close()

    def __add__(self, other): return self._binary(self._lib.add, other)
    def __radd__(self, other): return self._reverse_binary(self._lib.add, other)
    def __sub__(self, other): return self._binary(self._lib.subtract, other)
    def __rsub__(self, other): return self._reverse_binary(self._lib.subtract, other)
    def __mul__(self, other):
        if getattr(other, "_fortsym_form_field", False):
            return NotImplemented
        return self._binary(self._lib.multiply, other)
    def __rmul__(self, other): return self._reverse_binary(self._lib.multiply, other)
    def __truediv__(self, other): return self._binary(self._lib.divide, other)
    def __rtruediv__(self, other): return self._reverse_binary(self._lib.divide, other)
    def __pow__(self, other): return self._binary(self._lib.power, other)
    def __rpow__(self, other): return self._reverse_binary(self._lib.power, other)
    def __neg__(self): return self._reverse_binary(self._lib.subtract, 0)

    def _relation(self, name, other):
        return self._arena.relation(self, other, name)

    def __lt__(self, other): return self._relation("Less", other)
    def __le__(self, other): return self._relation("LessEqual", other)
    def __gt__(self, other): return self._relation("Greater", other)
    def __ge__(self, other): return self._relation("GreaterEqual", other)

    @property
    def args(self):
        return tuple(self.argument(index) for index in range(self.arity))

    def __bool__(self):
        if self.kind in (1, 2, 10, 11):
            return self.exact_text not in ("0", "0/1")
        if self.kind == 3:
            return float(str(self)) != 0.0
        raise TypeError("symbolic expressions do not have a truth value")

    def _subs_raw(self, old, new):
        old, old_temporary = self._arena._coerce(old)
        new, new_temporary = self._arena._coerce(new)
        try:
            return self._arena._result(
                self._lib.substitute, self._arena._require(), self._require(),
                old._handle, new._handle)
        finally:
            if old_temporary:
                old.close()
            if new_temporary:
                new.close()

    def subs(self, old, new):
        result = self._subs_raw(old, new)
        try:
            return result.expand()
        finally:
            result.close()

    def _subs_many_raw(self, old, new):
        if len(old) != len(new):
            raise ValueError("old and new replacement sequences differ in size")
        if not old:
            return self
        old_values = []
        new_values = []
        temporaries = []
        try:
            for value in old:
                coerced, temporary = self._arena._coerce(value)
                old_values.append(coerced)
                if temporary is not None:
                    temporaries.append(temporary)
            for value in new:
                coerced, temporary = self._arena._coerce(value)
                new_values.append(coerced)
                if temporary is not None:
                    temporaries.append(temporary)
            old_handles = (_CVOID * len(old_values))(
                *[value._handle for value in old_values]
            )
            new_handles = (_CVOID * len(new_values))(
                *[value._handle for value in new_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.substitute_many(
                self._arena._require(), self._require(), old_handles,
                new_handles, len(old_values), ctypes.byref(output), message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), "substitute_many")
            return Expr(self._arena, output)
        finally:
            for temporary in temporaries:
                temporary.close()

    def _subs_many(self, old, new):
        result = self._subs_many_raw(old, new)
        if result is self:
            return result
        try:
            return result.expand()
        finally:
            result.close()

    def xreplace(self, substitutions):
        if not isinstance(substitutions, dict):
            raise TypeError("xreplace expects a mapping")
        if not substitutions:
            return self
        return self._subs_many_raw(
            tuple(substitutions), tuple(substitutions.values())
        )

    def replace(self, query, value, map=False, exact=None):
        """Replace one exact, non-wildcard node through ``xreplace``."""
        if not isinstance(map, bool):
            raise TypeError("replace map must be a boolean")
        if exact is not None and not isinstance(exact, bool):
            raise TypeError("replace exact must be a boolean or None")
        if callable(query) or callable(value):
            raise NotImplementedError(
                "callable and wildcard replace patterns are unsupported"
            )
        if getattr(query, "_wildcard_matcher", None) is not None:
            raise NotImplementedError(
                "callable and wildcard replace patterns are unsupported"
            )
        if not map:
            key = (id(query), id(value), exact)
            cached = self._replace_results.get(key)
            if (cached is not None and cached[0] is query and
                    cached[1] is value and cached[2]._handle is not None):
                return cached[2]
            result = self.xreplace({query: value})
            self._replace_results[key] = (query, value, result)
            if len(self._replace_results) > 8:
                self._replace_results.pop(next(iter(self._replace_results)))
            return result
        old, old_temporary = self._arena._coerce(query)
        new, new_temporary = self._arena._coerce(value)
        keep_mapping = False
        try:
            result = self.xreplace({old: new})
            changed = result != self
            keep_mapping = changed
            mapping = {old: new} if changed else {}
            return result, mapping
        finally:
            if old_temporary is not None and not keep_mapping:
                old_temporary.close()
            if new_temporary is not None and not keep_mapping:
                new_temporary.close()

    def match(self, pattern, old=False):
        """Return exact or bounded compatibility-layer wildcard bindings.

        The native handle owns exact structural matching; the adapter owns
        direct, fixed-shape, and bounded root-level commutative Wild rules.
        ``old`` has no effect in this supported fragment.
        """
        direct_matcher = getattr(pattern, "_wildcard_matcher", None)
        if direct_matcher is not None and not isinstance(pattern, Expr):
            if getattr(pattern, "_pattern_arena", self._arena) is not self._arena:
                raise ValueError("expression belongs to another arena")
            key = id(pattern)
            cached = self._match_results.get(key)
            if cached is not None and cached[0] is pattern:
                return dict(cached[1])
            result = direct_matcher(self, pattern, old)
            self._match_results[key] = (pattern, result)
            return None if result is None else dict(result)
        pattern, temporary = self._arena._coerce(pattern)
        try:
            matcher = getattr(pattern, "_wildcard_matcher", None)
            if matcher is not None:
                key = id(pattern)
                cached = self._match_results.get(key)
                if cached is not None and cached[0] is pattern:
                    return dict(cached[1])
                result = matcher(self, pattern, old)
                self._match_results[key] = (pattern, result)
                if len(self._match_results) > 8:
                    self._match_results.pop(next(iter(self._match_results)))
                return None if result is None else dict(result)
            if self is pattern or self == pattern:
                return {}
            return None
        finally:
            if temporary is not None:
                temporary.close()

    def diff(self, variable):
        variable = self._arena._check(variable)
        return self._arena._result(self._lib.differentiate, self._arena._require(),
                                   self._require(), variable._handle)

    def _diff_simplified(self, variable):
        variable = self._arena._check(variable)
        key = variable.exact_text
        cached = self._diff_results.get(key)
        if cached is not None and cached._handle is not None:
            return cached
        raw = self.diff(variable)
        try:
            result = raw.simplify()
        finally:
            raw.close()
        self._diff_results[key] = result
        return result

    def expand(self):
        cached = self._expanded_result
        if (cached is not None and cached._handle is not None and
                self._expanded_epoch == self._arena._assumption_epoch):
            return cached
        self._expanded_result = None
        result = self._arena._result(self._lib.expand, self._arena._require(),
                                     self._require())
        result._pretty = True
        self._expanded_result = result
        self._expanded_epoch = self._arena._assumption_epoch
        return result

    def simplify(self):
        return self._arena._result(self._lib.simplify, self._arena._require(),
                                   self._require())

    def factor(self):
        return self._arena._result(self._lib.factor, self._arena._require(),
                                   self._require())

    def _complex_operation(self, operation):
        cached = self._complex_results.get(operation)
        if (cached is not None and cached[0] == self._arena._assumption_epoch
                and cached[1]._handle is not None):
            return cached[1]
        result = self._arena._result(
            self._lib.complex_operation, self._arena._require(),
            self._require(), operation.encode()
        )
        self._complex_results[operation] = (self._arena._assumption_epoch, result)
        return result

    def _zero_verdict(self):
        verdict = ctypes.c_int()
        message = _message()
        status = self._lib.zero_test(
            self._arena._require(), self._require(), ctypes.byref(verdict),
            message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "zero_test")
        return verdict.value

    def _assumption_fact(self, fact):
        known = ctypes.c_int()
        message = _message()
        status = self._lib.assumption_has(
            self._arena._require(), self._require(), int(fact),
            ctypes.byref(known), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "assumption_has")
        return True if known.value else None

    @property
    def is_zero(self):
        if self._assumption_fact(_FACT_ZERO) is True:
            return True
        if self._assumption_fact(_FACT_NONZERO) is True:
            return False
        verdict = self._zero_verdict()
        if verdict == 1:
            return True
        if verdict != 2:
            return None
        try:
            simplified = self.simplify()
        except FortSymError:
            return None
        try:
            if simplified.kind in (1, 2, 3, 10, 11, 12, 13):
                return False
            return None
        finally:
            simplified.close()

    @property
    def is_nonzero(self):
        if self._assumption_fact(_FACT_ZERO) is True:
            return False
        if self._assumption_fact(_FACT_NONZERO) is True:
            return True
        zero = self.is_zero
        return None if zero is None else not zero

    @property
    def is_real(self):
        if self._assumption_fact(_FACT_REAL) is True:
            return True
        if self.kind in (1, 2, 3, 10, 11, 12):
            return True
        return None

    @property
    def is_integer(self):
        if self._known_facts & _FACT_INTEGER:
            return True
        kind = self.kind
        if kind in (1, 10):
            return True
        if kind in (2, 11):
            return False
        if self._assumption_fact(_FACT_INTEGER) is True:
            return True
        return None

    @property
    def is_rational(self):
        if self._known_facts & _FACT_RATIONAL:
            return True
        if self.kind in (1, 2, 10, 11):
            return True
        if self._assumption_fact(_FACT_RATIONAL) is True:
            return True
        return None

    @property
    def is_positive(self):
        if self._assumption_fact(_FACT_POSITIVE) is True:
            return True
        if self._assumption_fact(_FACT_NONPOSITIVE) is True:
            return False
        return None

    @property
    def is_nonnegative(self):
        if self._assumption_fact(_FACT_NONNEGATIVE) is True:
            return True
        if self._assumption_fact(_FACT_NEGATIVE) is True:
            return False
        return None

    @property
    def is_negative(self):
        if self._assumption_fact(_FACT_NEGATIVE) is True:
            return True
        if self._assumption_fact(_FACT_NONNEGATIVE) is True:
            return False
        return None

    @property
    def is_nonpositive(self):
        if self._assumption_fact(_FACT_NONPOSITIVE) is True:
            return True
        if self._assumption_fact(_FACT_POSITIVE) is True:
            return False
        return None

    @property
    def is_number(self):
        if self._number_value is not None:
            return self._number_value
        number = ctypes.c_int()
        message = _message()
        status = self._lib.expr_is_number(
            self._require(), ctypes.byref(number), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "expr_is_number")
        self._number_value = bool(number.value)
        return self._number_value

    @cached_property
    def is_algebraic(self):
        if self._known_facts & (_FACT_ALGEBRAIC | _FACT_INTEGER | _FACT_RATIONAL):
            return True
        verdict = ctypes.c_int()
        message = _message()
        status = self._lib.expr_is_algebraic(
            self._require(), ctypes.byref(verdict), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "expr_is_algebraic")
        value = (True if verdict.value == 1 else
                 False if verdict.value == 2 else None)
        self._arena._algebraic_cache.add(self)
        return value

    @property
    def kind(self):
        self._require()
        if self._kind_cache is not None:
            return self._kind_cache
        value = ctypes.c_int()
        message = _message()
        status = self._lib.expr_kind(self._require(), ctypes.byref(value), message, len(message))
        if status: raise FortSymError(status, _decode(message), "expr_kind")
        self._kind_cache = value.value
        return self._kind_cache

    @property
    def arity(self):
        self._require()
        if self._arity_cache is not None:
            return self._arity_cache
        value = _SIZE()
        message = _message()
        status = self._lib.expr_arity(self._require(), ctypes.byref(value), message, len(message))
        if status: raise FortSymError(status, _decode(message), "expr_arity")
        self._arity_cache = value.value
        return self._arity_cache

    def argument(self, index):
        output = _CVOID()
        message = _message()
        status = self._lib.expr_argument(self._require(), int(index), ctypes.byref(output),
                                         message, len(message))
        if status: raise FortSymError(status, _decode(message), "expr_argument")
        return Expr(self._arena, output)

    def operation_count(self):
        count = _SIZE()
        message = _message()
        status = self._lib.expr_operation_count(
            self._require(), ctypes.byref(count), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "expr_operation_count")
        return count.value

    @property
    def node_count(self):
        if self._node_count_cache is not None:
            return self._node_count_cache
        count = _SIZE()
        message = _message()
        status = self._lib.expr_node_count(
            self._require(), ctypes.byref(count), message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), "expr_node_count")
        self._node_count_cache = count.value
        return count.value

    @property
    def free_symbols(self):
        if self._free_symbols_cache is not None:
            return self._free_symbols_cache
        size = 128
        while True:
            buffer = ctypes.create_string_buffer(size)
            required = _SIZE()
            message = _message()
            status = self._lib.expr_free_symbols(
                self._require(), buffer, size, ctypes.byref(required),
                message, len(message)
            )
            if status == 6 and required.value > size:
                size = required.value
                continue
            if status:
                raise FortSymError(status, _decode(message), "expr_free_symbols")
            payload = buffer.raw[:max(0, required.value - 1)]
            if not payload:
                self._free_symbols_cache = frozenset()
                return self._free_symbols_cache
            symbols = frozenset(
                self._arena.symbol(name.decode("utf-8", "replace"))
                for name in payload.split(b"\0")
                if name
            )
            for symbol in symbols:
                symbol._borrowed = True
            self._free_symbols_cache = symbols
            return symbols

    def _text(self, accessor):
        size = 128
        while True:
            buffer = ctypes.create_string_buffer(size)
            required = _SIZE()
            message = _message()
            status = accessor(self._require(), buffer, size, ctypes.byref(required),
                              message, len(message))
            if status == 6 and required.value > size:
                size = required.value
                continue
            if status:
                operation = accessor.__name__.removeprefix("fortsym_")
                raise FortSymError(status, _decode(message), operation)
            text = buffer.value.decode("utf-8", "replace")
            if accessor is self._lib.expr_text and self._pretty:
                return _pretty_expanded(text)
            return text

    def __str__(self):
        if self.kind == 9:
            head = getattr(self, "_sympy_head", None)
            if head is not None and self.arity == 2:
                left, right = self.args
                try:
                    return f"{head}({left}, {right})"
                finally:
                    left.close()
                    right.close()
            relation = _RELATIONS.get(self.name)
            operator = None if relation is None else relation[0]
            if operator is not None and self.arity == 2:
                left, right = self.args
                try:
                    return f"{left} {operator} {right}"
                finally:
                    left.close()
                    right.close()
        text = self._text(self._lib.expr_text)
        for name, wildcard in getattr(self, "_wildcard_patterns", {}).items():
            text = text.replace(name, getattr(wildcard, "name", name))
        return text
    def __repr__(self): return str(self)
    @property
    def name(self):
        self._require()
        if self._name_cache is None:
            self._name_cache = self._text(self._lib.expr_name)
        return self._name_cache
    @property
    def exact_text(self): return self._text(self._lib.expr_exact_text)

    def __eq__(self, other):
        if not isinstance(other, (Expr, int, Fraction)):
            return False
        if not isinstance(other, Expr):
            other, temporary = self._arena._coerce(other)
        else:
            temporary = None
            if other._arena is not self._arena:
                return False
        equal = ctypes.c_int()
        message = _message()
        try:
            status = self._lib.expr_equal(self._require(), other._require(),
                                          ctypes.byref(equal), message, len(message))
            if status:
                raise FortSymError(status, _decode(message), "expr_equal")
            return bool(equal.value)
        finally:
            if temporary:
                temporary.close()

    def __hash__(self):
        text = self.exact_text
        if "/" not in text:
            try:
                return hash(int(text))
            except ValueError:
                pass
        else:
            numerator, denominator = text.split("/", 1)
            try:
                return hash(Fraction(int(numerator), int(denominator)))
            except ValueError:
                pass
        return hash(str(self))


_default_arena = None


class _Assumption:
    __slots__ = ("expression", "fact", "name")

    def __init__(self, expression, fact, name):
        self.expression = expression
        self.fact = fact
        self.name = name


class _AssumptionScope:
    def __init__(self, arena, facts):
        self._arena = arena
        self._facts = tuple(facts)
        self._entered = False

    def __enter__(self):
        for fact in self._facts:
            if isinstance(fact, _Assumption):
                self._arena._check(fact.expression)
            elif isinstance(fact, Expr):
                self._arena._check(fact)
            else:
                raise TypeError("assuming expects Q facts or relations")
        self._arena._assumption_push()
        try:
            for fact in self._facts:
                if isinstance(fact, _Assumption):
                    self._arena.assume(fact.expression, fact.fact)
                else:
                    self._arena.assume_relation(fact)
        except BaseException:
            self._arena._assumption_pop()
            raise
        self._entered = True
        return self

    def __exit__(self, *_):
        if self._entered:
            self._arena._assumption_pop()
            self._entered = False
        return False


def _default():
    global _default_arena
    if _default_arena is None:
        _default_arena = Arena()
    return _default_arena


def Symbol(name: str): return _default().symbol(name)
def symbols(names: str):
    values = [Symbol(name) for name in names.replace(",", " ").split()]
    return values[0] if len(values) == 1 else tuple(values)
def Integer(value: int): return _default().integer(value)
def Rational(numerator: int, denominator: int = 1):
    return _default().rational(numerator, denominator)
def Float(value: float): return _default().real(value)
def Function(name: str): return lambda *args: _default().function(name, args)
def diff(expression: Expr, variable: Expr): return expression.diff(variable)
def subs(expression: Expr, old: Expr, new: Expr): return expression.subs(old, new)
def subs_many(expression: Expr, old, new): return expression._subs_many(old, new)
def factor(expression: Expr): return expression.factor()
def operation_count(expression: Expr): return expression.operation_count()
def free_symbols(expression: Expr): return expression.free_symbols


__all__ = [
    "Arena", "Chart", "Tensor", "Form", "Expr", "FortSymError", "Symbol", "symbols", "Integer",
    "Rational", "Float", "Function", "diff", "subs", "subs_many", "factor", "operation_count",
    "free_symbols",
]
