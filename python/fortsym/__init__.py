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
import sys
from typing import Iterable


_CVOID = ctypes.c_void_p
_CHAR_PTR = ctypes.POINTER(ctypes.c_char)
_SIZE = ctypes.c_size_t
_I64 = ctypes.c_int64


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
        for candidate in candidates:
            try:
                _library = ctypes.CDLL(str(candidate))
                _configure(_library)
                return _library
            except (OSError, AttributeError):
                continue
        raise ImportError(
            "fortsym's native library was not found; set FORTSYM_LIBRARY "
            "to libfortsym.so"
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
    lib.substitute = declare(
        "fortsym_substitute",
        ctypes.c_int,
        [_CVOID, _CVOID, _CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.differentiate = declare(
        "fortsym_differentiate",
        ctypes.c_int,
        [_CVOID, _CVOID, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
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
    lib.expr_free = declare("fortsym_expr_free", None, [_CVOID])
    lib.expr_kind = declare(
        "fortsym_expr_kind", ctypes.c_int,
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
        message = _message()
        status = self._lib.arena_new(ctypes.byref(self._handle), message, len(message))
        if status:
            raise FortSymError(status, _decode(message), "arena_new")

    def close(self):
        if self._handle is not None:
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

    def _coerce(self, value):
        if isinstance(value, Expr):
            return self._check(value), None
        if isinstance(value, Fraction):
            expression = self.rational(value.numerator, value.denominator)
            return expression, expression
        if isinstance(value, int):
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


class Expr:
    """An immutable native expression handle."""

    def __init__(self, arena: Arena, handle):
        self._arena = arena
        self._lib = arena._lib
        self._handle = handle
        self._pretty = False

    def _require(self):
        if self._handle is None:
            raise FortSymError(2, "expression is closed", "expression")
        return self._handle

    def close(self):
        if self._handle is not None:
            self._lib.expr_free(self._handle)
            self._handle = None

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass

    def _binary(self, function, other):
        other, temporary = self._arena._coerce(other)
        try:
            return self._arena._result(function, self._arena._require(),
                                       self._require(), other._handle)
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
    def __mul__(self, other): return self._binary(self._lib.multiply, other)
    def __rmul__(self, other): return self._reverse_binary(self._lib.multiply, other)
    def __truediv__(self, other): return self._binary(self._lib.divide, other)
    def __rtruediv__(self, other): return self._reverse_binary(self._lib.divide, other)
    def __pow__(self, other): return self._binary(self._lib.power, other)
    def __rpow__(self, other): return self._reverse_binary(self._lib.power, other)
    def __neg__(self): return self._reverse_binary(self._lib.subtract, 0)

    @property
    def args(self):
        return tuple(self.argument(index) for index in range(self.arity))

    def __bool__(self):
        if self.kind in (1, 2, 10, 11):
            return self.exact_text not in ("0", "0/1")
        if self.kind == 3:
            return float(str(self)) != 0.0
        raise TypeError("symbolic expressions do not have a truth value")

    def subs(self, old, new):
        old, old_temporary = self._arena._coerce(old)
        new, new_temporary = self._arena._coerce(new)
        try:
            result = self._arena._result(
                self._lib.substitute, self._arena._require(), self._require(),
                old._handle, new._handle)
            expanded = result.expand()
            result.close()
            return expanded
        finally:
            if old_temporary:
                old.close()
            if new_temporary:
                new.close()

    def diff(self, variable):
        variable = self._arena._check(variable)
        return self._arena._result(self._lib.differentiate, self._arena._require(),
                                   self._require(), variable._handle)

    def expand(self):
        result = self._arena._result(self._lib.expand, self._arena._require(),
                                     self._require())
        result._pretty = True
        return result

    def simplify(self):
        return self._arena._result(self._lib.simplify, self._arena._require(),
                                   self._require())

    @property
    def kind(self):
        value = ctypes.c_int()
        message = _message()
        status = self._lib.expr_kind(self._require(), ctypes.byref(value), message, len(message))
        if status: raise FortSymError(status, _decode(message), "expr_kind")
        return value.value

    @property
    def arity(self):
        value = _SIZE()
        message = _message()
        status = self._lib.expr_arity(self._require(), ctypes.byref(value), message, len(message))
        if status: raise FortSymError(status, _decode(message), "expr_arity")
        return value.value

    def argument(self, index):
        output = _CVOID()
        message = _message()
        status = self._lib.expr_argument(self._require(), int(index), ctypes.byref(output),
                                         message, len(message))
        if status: raise FortSymError(status, _decode(message), "expr_argument")
        return Expr(self._arena, output)

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

    def __str__(self): return self._text(self._lib.expr_text)
    def __repr__(self): return str(self)
    @property
    def name(self): return self._text(self._lib.expr_name)
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


__all__ = [
    "Arena", "Expr", "FortSymError", "Symbol", "symbols", "Integer",
    "Rational", "Float", "Function", "diff", "subs",
]
