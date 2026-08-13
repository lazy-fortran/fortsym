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

FOURIER_INVALID = 0
FOURIER_LONGITUDINAL = 1
FOURIER_TRANSVERSE = 2
SPACE_NONE = 0
SPACE_NODAL = 1
SPACE_EDGE = 2
TRACE_NONE = 0
TRACE_NORMAL = 1
TRACE_TANGENTIAL = 2
FLUX_GENERIC = 0
FLUX_CLEBSCH = 1
FLUX_STRAIGHT_FIELD_LINE = 2
FLUX_BOOZER = 3
FLUX_HAMADA = 4
CLEBSCH_RESIDUAL_COUNT = 3
BOOZER_RESIDUAL_COUNT = 5
HAMADA_RESIDUAL_COUNT = 5
SPACETIME_DIM = 4
SPACETIME_TENSOR_MAX_RANK = 5
CONNECTION_STANDARD = 1
CONNECTION_OPPOSITE = -1
SYMMETRY_NONE = 0
SYMMETRIC = 1
ANTISYMMETRIC = -1
INDEX_TANGENT = 1
INDEX_COTANGENT = 2
INDEX_SPACETIME = 3
INDEX_INTERNAL = 4
INDEX_USER = 5


def _symmetry_kind(value):
    if isinstance(value, str):
        names = {
            "none": SYMMETRY_NONE,
            "symmetric": SYMMETRIC,
            "sym": SYMMETRIC,
            "antisymmetric": ANTISYMMETRIC,
            "antisym": ANTISYMMETRIC,
        }
        try:
            value = names[value.lower()]
        except KeyError as error:
            raise ValueError("unknown tensor symmetry kind") from error
    value = int(value)
    if value not in (SYMMETRY_NONE, SYMMETRIC, ANTISYMMETRIC):
        raise ValueError("tensor symmetry kind must be 0, 1, or -1")
    return value


def _normalize_symmetries(rank, values):
    if values is None:
        return {}
    if isinstance(values, dict):
        values = ((pair[0], pair[1], kind)
                  for pair, kind in values.items())
    result = {}
    for item in values:
        if len(item) != 3:
            raise ValueError("tensor symmetries must be (first, second, kind)")
        first, second, kind = (int(item[0]), int(item[1]),
                               _symmetry_kind(item[2]))
        if first < 0 or first >= rank or second < 0 or second >= rank:
            raise IndexError("tensor symmetry slot is outside the tensor rank")
        if first == second:
            raise ValueError("tensor symmetry needs two distinct slots")
        if kind == SYMMETRY_NONE:
            continue
        result[tuple(sorted((first, second)))] = kind
    return result


class Orientation:
    """A signed orientation declaration shared by metric and form views."""

    __slots__ = ("value",)

    def __init__(self, value):
        value = int(value)
        if value not in (-1, 1):
            raise ValueError("orientation must be 1 or -1")
        self.value = value

    def __int__(self):
        return self.value

    def __repr__(self):
        return f"Orientation({self.value})"


class Signature:
    """A finite nondegenerate metric-signature declaration."""

    __slots__ = ("values",)

    def __init__(self, values):
        values = tuple(int(value) for value in values)
        if not 1 <= len(values) <= 16 or any(value not in (-1, 1)
                                             for value in values):
            raise ValueError("signature entries must be +/-1 and dimension 1..16")
        self.values = values

    @property
    def dimension(self):
        return len(self.values)

    @property
    def positive_count(self):
        return self.values.count(1)

    @property
    def negative_count(self):
        return self.values.count(-1)

    @property
    def is_lorentzian(self):
        return self.dimension >= 2 and 1 in (
            self.positive_count, self.negative_count
        )

    def __len__(self):
        return len(self.values)

    def __iter__(self):
        return iter(self.values)

    def __getitem__(self, index):
        return self.values[index]

    def __repr__(self):
        return f"Signature({self.values!r})"


def _index_variance(value):
    if value in (1, "upper", "up", "contravariant"):
        return 1
    if value in (-1, "lower", "down", "covariant"):
        return -1
    raise ValueError("index variance must be upper/contravariant or lower/covariant")


class IndexType:
    """A named finite index space used by checked tensor contractions."""

    __slots__ = ("name", "dimension", "category")

    def __init__(self, name, dimension=3, category=INDEX_USER):
        name = str(name).strip()
        dimension = int(dimension)
        category = int(category)
        if not name or len(name) > 64:
            raise ValueError("index-space name must contain 1 to 64 characters")
        if dimension < 1:
            raise ValueError("index-space dimension must be positive")
        if category < INDEX_TANGENT or category > INDEX_USER:
            raise ValueError("invalid index-space category")
        self.name = name
        self.dimension = dimension
        self.category = category

    def index(self, slot, variance, label=None, dummy=False):
        """Create a zero-based slot label in this index space."""
        slot = int(slot)
        if slot < 0 or slot >= self.dimension:
            raise IndexError("index slot is outside its index space")
        label = "" if label is None else str(label).strip()
        if len(label) > 64:
            raise ValueError("index label must contain at most 64 characters")
        return Index(self, slot, variance, label, dummy)

    __call__ = index


class Index:
    """A variance-aware, optionally labelled zero-based tensor slot."""

    __slots__ = ("space", "slot", "variance", "label", "dummy")

    def __init__(self, space, slot, variance, label="", dummy=False):
        if not isinstance(space, IndexType):
            raise TypeError("Index requires an IndexType")
        self.space = space
        self.slot = int(slot)
        self.variance = _index_variance(variance)
        self.label = str(label).strip()
        self.dummy = bool(dummy)
        if self.slot < 0 or self.slot >= space.dimension:
            raise IndexError("index slot is outside its index space")
        if len(self.label) > 64:
            raise ValueError("index label must contain at most 64 characters")

    def compatible(self, other):
        return (
            isinstance(other, Index)
            and self.space.name == other.space.name
            and self.space.dimension == other.space.dimension
            and self.space.category == other.space.category
            and self.variance == -other.variance
            and (not self.label or not other.label or self.label == other.label)
        )


def _matrix3_values(matrix):
    """Return a 3x3 matrix in the native first-slot-fastest order."""
    values = tuple(matrix)
    if len(values) == 3:
        try:
            rows = tuple(tuple(row) for row in values)
        except TypeError:
            rows = ()
        if len(rows) == 3 and all(len(row) == 3 for row in rows):
            return tuple(rows[row][column] for column in range(3)
                         for row in range(3))
    if len(values) == 9:
        return values
    raise ValueError("j_fourier reluctivity requires a 3x3 matrix")


def _matrix4_values(matrix):
    """Return a 4x4 matrix in the native first-slot-fastest order."""
    values = tuple(matrix)
    if len(values) == 4:
        try:
            rows = tuple(tuple(row) for row in values)
        except TypeError:
            rows = ()
        if len(rows) == 4 and all(len(row) == 4 for row in rows):
            return tuple(rows[row][column] for column in range(4)
                         for row in range(4))
    if len(values) == 16:
        return values
    raise ValueError("spacetime metric requires a 4x4 matrix")


def _tensor3_values(tensor):
    """Return a rank-three tensor in first-slot-fastest native order."""
    values = tuple(tensor)
    if len(values) == 3:
        try:
            planes = tuple(tuple(tuple(row) for row in plane) for plane in values)
        except TypeError:
            planes = ()
        if (len(planes) == 3 and all(len(plane) == 3 for plane in planes)
                and all(len(row) == 3 for plane in planes for row in plane)):
            return tuple(
                planes[first][second][third]
                for third in range(3)
                for second in range(3)
                for first in range(3)
            )
    if len(values) == 27:
        return values
    raise ValueError("connection coefficients require a 3x3x3 array")


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
    lib.chart_surface_measure = declare(
        "fortsym_chart_surface_measure", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), ctypes.c_int,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_flux_surface_average = declare(
        "fortsym_chart_flux_surface_average", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), ctypes.c_int,
         _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_jacobian = declare(
        "fortsym_chart_jacobian",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _SIZE,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_grad = declare(
        "fortsym_chart_grad", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CVOID,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_divergence = declare(
        "fortsym_chart_divergence", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_div_density = declare(
        "fortsym_chart_div_density", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_field_line_derivative = declare(
        "fortsym_chart_field_line_derivative", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_curl = declare(
        "fortsym_chart_curl", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_curl_density = declare(
        "fortsym_chart_curl_density", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_laplacian = declare(
        "fortsym_chart_laplacian", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CVOID,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    chart_map_matrix_arguments = [
        _CVOID,
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    for name in ("map_jacobian", "map_inverse_jacobian"):
        setattr(
            lib,
            "chart_" + name,
            declare(
                "fortsym_chart_" + name, ctypes.c_int,
                chart_map_matrix_arguments,
            ),
        )
    lib.chart_map_tensor = declare(
        "fortsym_chart_map_tensor", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_map_form = declare(
        "fortsym_chart_map_form", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_map_compose = declare(
        "fortsym_chart_map_compose", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    for name in ("b_cov", "b_density", "b_fourier", "b_fourier_density"):
        arguments = [
            _CVOID,
            ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID),
        ]
        if name not in ("b_cov", "b_density"):
            arguments.append(_CVOID)
        arguments.extend([ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE])
        setattr(
            lib,
            "chart_" + name,
            declare("fortsym_chart_" + name, ctypes.c_int, arguments),
        )
    for name in ("b_con_form", "b_density_form"):
        setattr(
            lib,
            "chart_" + name,
            declare(
                "fortsym_chart_" + name, ctypes.c_int,
                [
                    _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
                    ctypes.POINTER(_CVOID), _SIZE, ctypes.c_int,
                    ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
                ],
            ),
        )
    lib.chart_b_flux_form = declare(
        "fortsym_chart_b_flux_form", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_flux_normal_residual = declare(
        "fortsym_chart_flux_normal_residual", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.c_int, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_straight_field_line_residual = declare(
        "fortsym_chart_straight_field_line_residual", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.c_int, _CVOID,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_clebsch_residuals = declare(
        "fortsym_chart_clebsch_residuals", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, _CVOID, ctypes.c_int,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    for name in ("boozer_residuals", "hamada_residuals"):
        setattr(
            lib,
            "chart_" + name,
            declare(
                "fortsym_chart_" + name, ctypes.c_int,
                [
                    _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
                    ctypes.POINTER(_CVOID), ctypes.c_int,
                    ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
                ],
            ),
        )
    lib.chart_h_cov = declare(
        "fortsym_chart_h_cov", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_h_con = declare(
        "fortsym_chart_h_con", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_j_fourier = declare(
        "fortsym_chart_j_fourier", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CVOID,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_reluctivity_density_scalar = declare(
        "fortsym_chart_reluctivity_density_scalar", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CVOID,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_reluctivity_density_matrix = declare(
        "fortsym_chart_reluctivity_density_matrix", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_weak_form = declare(
        "fortsym_chart_fourier_weak_form", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int),
            ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int),
            ctypes.POINTER(ctypes.c_int), ctypes.POINTER(ctypes.c_int),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_longitudinal_flux = declare(
        "fortsym_chart_fourier_longitudinal_flux", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, ctypes.c_int,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_transverse_flux = declare(
        "fortsym_chart_fourier_transverse_flux", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_longitudinal_boundary_flux = declare(
        "fortsym_chart_fourier_longitudinal_boundary_flux", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_transverse_boundary_flux = declare(
        "fortsym_chart_fourier_transverse_boundary_flux", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_transverse_boundary_contraction = declare(
        "fortsym_chart_fourier_transverse_boundary_contraction", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_longitudinal_residual = declare(
        "fortsym_chart_fourier_longitudinal_residual", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, _CVOID, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_fourier_transverse_residual = declare(
        "fortsym_chart_fourier_transverse_residual", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_current_compatibility = declare(
        "fortsym_chart_current_compatibility", ctypes.c_int,
        [
            _CVOID,
            ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.c_int, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.metric_sqrtg = declare(
        "fortsym_metric_sqrtg", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.metric_volume_density = declare(
        "fortsym_metric_volume_density", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.metric_surface_measure = declare(
        "fortsym_metric_surface_measure", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.metric_levi_civita = declare(
        "fortsym_metric_levi_civita", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.metric_contravariant = declare(
        "fortsym_metric_contravariant", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.metric_inner = declare(
        "fortsym_metric_inner", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    metric_chart_scalar_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    lib.metric_grad = declare(
        "fortsym_metric_grad", ctypes.c_int, metric_chart_scalar_arguments,
    )
    lib.metric_laplacian = declare(
        "fortsym_metric_laplacian", ctypes.c_int, metric_chart_scalar_arguments,
    )
    lib.metric_divergence = declare(
        "fortsym_metric_divergence", ctypes.c_int,
        metric_chart_scalar_arguments,
    )
    lib.chart_form_star_metric = declare(
        "fortsym_chart_form_star_metric", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.chart_form_codifferential_metric = declare(
        "fortsym_chart_form_codifferential_metric", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.chart_form_laplace_de_rham_metric = declare(
        "fortsym_chart_form_laplace_de_rham_metric", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    spacetime_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    for name in (
        "spacetime_metric_contravariant", "spacetime_christoffel",
        "spacetime_riemann", "spacetime_ricci", "spacetime_einstein",
    ):
        setattr(
            lib, name, declare("fortsym_" + name, ctypes.c_int,
                               spacetime_arguments),
        )
    for name in ("spacetime_metric_sqrtg", "spacetime_scalar_curvature"):
        setattr(
            lib, name, declare(
                "fortsym_" + name, ctypes.c_int,
                spacetime_arguments[:-3] + [ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
            ),
        )
    spacetime_vector_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    for name in ("spacetime_metric_flat", "spacetime_metric_sharp"):
        setattr(
            lib, name, declare("fortsym_" + name, ctypes.c_int,
                               spacetime_vector_arguments),
        )
    spacetime_scalar_array_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    lib.spacetime_metric_grad = declare(
        "fortsym_spacetime_metric_grad", ctypes.c_int,
        spacetime_scalar_array_arguments,
    )
    spacetime_vector_scalar_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    lib.spacetime_metric_divergence = declare(
        "fortsym_spacetime_metric_divergence", ctypes.c_int,
        spacetime_vector_scalar_arguments,
    )
    lib.spacetime_metric_laplacian = declare(
        "fortsym_spacetime_metric_laplacian", ctypes.c_int,
        spacetime_scalar_array_arguments,
    )
    spacetime_tensor_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
        ctypes.c_int, _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    lib.spacetime_tensor_raise = declare(
        "fortsym_spacetime_tensor_raise", ctypes.c_int,
        spacetime_tensor_arguments,
    )
    lib.spacetime_tensor_lower = declare(
        "fortsym_spacetime_tensor_lower", ctypes.c_int,
        spacetime_tensor_arguments,
    )
    lib.spacetime_tensor_density_factor = declare(
        "fortsym_spacetime_tensor_density_factor", ctypes.c_int,
        spacetime_tensor_arguments[:-4] + [
            _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_tensor_permute = declare(
        "fortsym_spacetime_tensor_permute", ctypes.c_int,
        spacetime_tensor_arguments[:-4] + [
            ctypes.POINTER(ctypes.c_int), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_tensor_contract = declare(
        "fortsym_spacetime_tensor_contract", ctypes.c_int,
        spacetime_tensor_arguments[:-4] + [
            _SIZE, _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_tensor_product = declare(
        "fortsym_spacetime_tensor_product", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
            ctypes.c_int, ctypes.POINTER(_CVOID), _SIZE,
            ctypes.POINTER(ctypes.c_int), ctypes.c_int, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_tensor_covariant_diff = declare(
        "fortsym_spacetime_tensor_covariant_diff", ctypes.c_int,
        spacetime_tensor_arguments[:-4] + [
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_tensor_covariant_divergence = declare(
        "fortsym_spacetime_tensor_covariant_divergence", ctypes.c_int,
        spacetime_tensor_arguments[:-4] + [
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_tensor_lie = declare(
        "fortsym_spacetime_tensor_lie", ctypes.c_int,
        spacetime_tensor_arguments[:6] + [
            ctypes.POINTER(_CVOID),
        ] + spacetime_tensor_arguments[6:-4] + spacetime_tensor_arguments[-3:],
    )
    lib.spacetime_geodesic_residual = declare(
        "fortsym_spacetime_geodesic_residual", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.spacetime_form_d = declare(
        "fortsym_spacetime_form_d", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.spacetime_form_closed = declare(
        "fortsym_spacetime_form_closed", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.spacetime_form_star = declare(
        "fortsym_spacetime_form_star", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.spacetime_form_codifferential = declare(
        "fortsym_spacetime_form_codifferential", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    lib.spacetime_form_laplace_de_rham = declare(
        "fortsym_spacetime_form_laplace_de_rham", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR,
            _SIZE,
        ],
    )
    spacetime_maxwell_field_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    lib.spacetime_field_strength = declare(
        "fortsym_spacetime_field_strength", ctypes.c_int,
        spacetime_maxwell_field_arguments,
    )
    lib.spacetime_gauge_transform = declare(
        "fortsym_spacetime_gauge_transform", ctypes.c_int,
        spacetime_maxwell_field_arguments[:-4] + [ctypes.POINTER(_CVOID),
                                                   _CVOID,
                                                   ctypes.POINTER(_CVOID),
                                                   _CHAR_PTR, _SIZE],
    )
    lib.spacetime_maxwell_residual = declare(
        "fortsym_spacetime_maxwell_residual", ctypes.c_int,
        spacetime_maxwell_field_arguments[:-3] + [ctypes.POINTER(_CVOID),
                                                   ctypes.POINTER(_CVOID),
                                                   _CHAR_PTR, _SIZE],
    )
    lib.spacetime_form_wedge = declare(
        "fortsym_spacetime_form_wedge", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
            ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int), ctypes.c_int,
            ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _SIZE,
            ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
        ],
    )
    for name in ("spacetime_form_interior", "spacetime_form_lie"):
        setattr(
            lib, name, declare(
                "fortsym_" + name, ctypes.c_int,
                [
                    _CVOID, ctypes.POINTER(_CVOID), ctypes.c_int,
                    ctypes.POINTER(_CVOID), ctypes.POINTER(ctypes.c_int),
                    ctypes.c_int, ctypes.POINTER(_CVOID),
                    ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID),
                    _CHAR_PTR, _SIZE,
                ],
            ),
        )
    for name in ("covariant_basis", "reciprocal_basis", "metric_covariant",
                 "metric_contravariant", "christoffel",
                 "riemann", "first_bianchi_residual",
                 "second_bianchi_residual", "ricci", "einstein"):
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
    lib.chart_geodesic_residual = declare(
        "fortsym_chart_geodesic_residual", ctypes.c_int,
        [
            _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
            ctypes.POINTER(_CVOID), _CVOID, ctypes.POINTER(_CVOID),
            _CHAR_PTR, _SIZE,
        ],
    )
    lib.chart_covariant_diff = declare(
        "fortsym_chart_covariant_diff",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_covariant_divergence = declare(
        "fortsym_chart_covariant_divergence", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    tensor_metric_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
        ctypes.c_int, _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    for name in ("raise", "lower"):
        setattr(
            lib,
            "chart_tensor_" + name,
            declare(
                "fortsym_chart_tensor_" + name,
                ctypes.c_int,
                tensor_metric_arguments,
            ),
        )
    lib.chart_tensor_density = declare(
        "fortsym_chart_tensor_density",
        ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_density_factor = declare(
        "fortsym_chart_tensor_density_factor", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, _CVOID, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_form_from_tensor = declare(
        "fortsym_chart_form_from_tensor", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_from_form = declare(
        "fortsym_chart_tensor_from_form", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_permute = declare(
        "fortsym_chart_tensor_permute", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, ctypes.POINTER(ctypes.c_int), ctypes.POINTER(_CVOID),
         _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_contract = declare(
        "fortsym_chart_tensor_contract", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, _SIZE, _SIZE, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_product = declare(
        "fortsym_chart_tensor_product", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int), ctypes.c_int,
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int), ctypes.c_int,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_symmetrize = declare(
        "fortsym_chart_tensor_symmetrize", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, _SIZE, _SIZE, ctypes.c_int, ctypes.POINTER(_CVOID),
         _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_declare_symmetry = declare(
        "fortsym_chart_tensor_declare_symmetry", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         ctypes.c_int, _SIZE, _SIZE, ctypes.c_int, ctypes.POINTER(_CVOID),
         _CHAR_PTR, _SIZE],
    )
    lib.chart_tensor_lie = declare(
        "fortsym_chart_tensor_lie", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int), ctypes.c_int,
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int), ctypes.c_int,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_connection_torsion = declare(
        "fortsym_chart_connection_torsion", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    lib.chart_connection_nonmetricity = declare(
        "fortsym_chart_connection_nonmetricity", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(ctypes.c_int), ctypes.c_int,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
    )
    connection_tensor_arguments = [
        _CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
        ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _SIZE,
        ctypes.POINTER(ctypes.c_int), ctypes.c_int,
        ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE,
    ]
    lib.chart_connection_covariant_diff = declare(
        "fortsym_chart_connection_covariant_diff", ctypes.c_int,
        connection_tensor_arguments,
    )
    lib.chart_connection_covariant_divergence = declare(
        "fortsym_chart_connection_covariant_divergence", ctypes.c_int,
        connection_tensor_arguments,
    )
    lib.chart_connection_riemann = declare(
        "fortsym_chart_connection_riemann", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.c_int, ctypes.POINTER(_CVOID),
         _CHAR_PTR, _SIZE],
    )
    lib.chart_connection_geodesic_residual = declare(
        "fortsym_chart_connection_geodesic_residual", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID), _CVOID,
         ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
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
    for name in ("negate", "d", "star", "codifferential", "laplace_de_rham"):
        setattr(
            lib,
            "chart_form_" + name,
            declare(
                "fortsym_chart_form_" + name, ctypes.c_int,
                form_unary_arguments,
            ),
        )
    lib.chart_form_closed = declare(
        "fortsym_chart_form_closed", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.POINTER(_CVOID), _SIZE, ctypes.POINTER(ctypes.c_int),
         _CHAR_PTR, _SIZE],
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
    lib.chart_form_volume = declare(
        "fortsym_chart_form_volume", ctypes.c_int,
        [_CVOID, ctypes.POINTER(_CVOID), ctypes.POINTER(_CVOID),
         ctypes.c_int, ctypes.POINTER(_CVOID), _CHAR_PTR, _SIZE],
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

    def _chart_surface_measure(self, coordinates, position, normal_index):
        if int(normal_index) not in (1, 2, 3):
            raise ValueError("surface normal index must be 1, 2, or 3")
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        output = _CVOID()
        message = _message()
        status = self._lib.chart_surface_measure(
            self._require(), coordinate_handles, position_handles,
            int(normal_index), ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "surface_measure")
        return Expr(self, output)

    def _chart_flux_surface_average(self, coordinates, position, label_index,
                                    scalar):
        if int(label_index) not in (1, 2, 3):
            raise ValueError("flux-surface label index must be 1, 2, or 3")
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        scalar = self._check(scalar)
        output = _CVOID()
        message = _message()
        status = self._lib.chart_flux_surface_average(
            self._require(), coordinate_handles, position_handles,
            int(label_index), scalar._handle, ctypes.byref(output), message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "flux_surface_average")
        return Expr(self, output)

    def _chart_jacobian(self, coordinates, position):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        return self._result(
            self._lib.chart_jacobian,
            self._require(), coordinate_handles, position_handles, 3,
        )

    def _chart_scalar_array(self, operation, coordinates, position, scalar):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        scalar, temporary = self._coerce(scalar)
        try:
            output = (_CVOID * 3)()
            message = _message()
            status = operation(
                self._require(), coordinate_handles, position_handles,
                scalar._handle, output, message, len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return tuple(Expr(self, output[index]) for index in range(3))
        finally:
            if temporary is not None:
                temporary.close()

    def _chart_scalar_scalar(self, operation, coordinates, position, scalar):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        scalar, temporary = self._coerce(scalar)
        try:
            output = _CVOID()
            message = _message()
            status = operation(
                self._require(), coordinate_handles, position_handles,
                scalar._handle, ctypes.byref(output), message, len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return Expr(self, output)
        finally:
            if temporary is not None:
                temporary.close()

    def _chart_vector_scalar(self, operation, coordinates, position, vector):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        values = tuple(self._check(value) for value in vector)
        if len(values) != 3:
            raise ValueError("chart vector operators require three components")
        vector_handles = (_CVOID * 3)(*[value._handle for value in values])
        output = _CVOID()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles,
            vector_handles, ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return Expr(self, output)

    def _chart_vector_array(self, operation, coordinates, position, vector):
        coordinate_handles, position_handles = self._chart_inputs(
            coordinates, position
        )
        values = tuple(self._check(value) for value in vector)
        if len(values) != 3:
            raise ValueError("chart vector operators require three components")
        vector_handles = (_CVOID * 3)(*[value._handle for value in values])
        output = (_CVOID * 3)()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles,
            vector_handles, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(3))

    def _chart_geodesic_residual(self, chart, curve, parameter):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        curve_values = tuple(curve)
        if len(curve_values) != 3:
            raise ValueError("geodesic curves require three components")
        coerced = []
        temporaries = []
        try:
            for value in curve_values:
                expression, temporary = self._coerce(value)
                coerced.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            parameter_value, temporary = self._coerce(parameter)
            if temporary is not None:
                temporaries.append(temporary)
            curve_handles = (_CVOID * 3)(
                *[value._handle for value in coerced]
            )
            output = (_CVOID * 3)()
            message = _message()
            status = self._lib.chart_geodesic_residual(
                self._require(), coordinate_handles, position_handles,
                curve_handles, parameter_value._handle, output, message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), "geodesic_residual")
            return tuple(Expr(self, output[index]) for index in range(3))
        finally:
            for temporary in temporaries:
                temporary.close()

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

    def _chart_flux_scalar(self, operation, chart, vector, label_index,
                           rotational_transform=None):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporaries = []
        try:
            values = []
            for value in tuple(vector):
                expression, temporary = self._coerce(value)
                values.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            if len(values) != 3:
                raise ValueError("flux-coordinate vectors require three components")
            vector_handles = (_CVOID * 3)(*[value._handle for value in values])
            output = _CVOID()
            message = _message()
            arguments = [
                self._require(), coordinate_handles, position_handles,
                vector_handles, int(label_index),
            ]
            if rotational_transform is not None:
                transform, temporary = self._coerce(rotational_transform)
                if temporary is not None:
                    temporaries.append(temporary)
                arguments.append(transform._handle)
            arguments.extend([ctypes.byref(output), message, len(message)])
            status = operation(*arguments)
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return Expr(self, output)
        finally:
            for temporary in temporaries:
                temporary.close()

    def _chart_flux_array(self, operation, chart, vector, label_index, count):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporaries = []
        try:
            values = []
            for value in tuple(vector):
                expression, temporary = self._coerce(value)
                values.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            if len(values) != 3:
                raise ValueError("flux-coordinate vectors require three components")
            vector_handles = (_CVOID * 3)(*[value._handle for value in values])
            output = (_CVOID * count)()
            message = _message()
            status = operation(
                self._require(), coordinate_handles, position_handles,
                vector_handles, int(label_index), output, message, len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return tuple(
                Expr(self, output[index]) for index in range(count)
            )
        finally:
            for temporary in temporaries:
                temporary.close()

    def _chart_clebsch_residuals(
            self, chart, vector, alpha, beta, label_index):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporaries = []
        try:
            values = []
            for value in tuple(vector):
                expression, temporary = self._coerce(value)
                values.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            if len(values) != 3:
                raise ValueError("Clebsch residuals require three vector components")
            alpha, temporary = self._coerce(alpha)
            if temporary is not None:
                temporaries.append(temporary)
            beta, temporary = self._coerce(beta)
            if temporary is not None:
                temporaries.append(temporary)
            vector_handles = (_CVOID * 3)(*[value._handle for value in values])
            output = (_CVOID * CLEBSCH_RESIDUAL_COUNT)()
            message = _message()
            status = self._lib.chart_clebsch_residuals(
                self._require(), coordinate_handles, position_handles,
                vector_handles, alpha._handle, beta._handle, int(label_index),
                output, message, len(message),
            )
            if status:
                raise FortSymError(
                    status, _decode(message), "clebsch_residuals"
                )
            return tuple(
                Expr(self, output[index])
                for index in range(CLEBSCH_RESIDUAL_COUNT)
            )
        finally:
            for temporary in temporaries:
                temporary.close()

    def _chart_j_fourier(self, chart, reluctivity, potential, mode):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in reluctivity:
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential_values = []
            for value in potential:
                value, temporary = self._coerce(value)
                potential_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(reluctivity_values) != 9:
                raise ValueError("j_fourier reluctivity requires a 3x3 matrix")
            if len(potential_values) != 3:
                raise ValueError("j_fourier potential requires three components")
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            potential_handles = (_CVOID * 3)(
                *[value._handle for value in potential_values]
            )
            mode, temporary = self._coerce(mode)
            if temporary is not None:
                temporary_values.append(temporary)
            output = (_CVOID * 3)()
            message = _message()
            status = self._lib.chart_j_fourier(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential_handles, mode._handle,
                output, message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()

        if status:
            raise FortSymError(status, _decode(message), "j_fourier")
        return tuple(Expr(self, output[index]) for index in range(3))

    def _chart_reluctivity_density(self, chart, physical, scalar):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            output = (_CVOID * 9)()
            message = _message()
            if scalar:
                physical_value, temporary = self._coerce(physical)
                if temporary is not None:
                    temporary_values.append(temporary)
                status = self._lib.chart_reluctivity_density_scalar(
                    self._require(), coordinate_handles, position_handles,
                    physical_value._handle, output, message, len(message),
                )
            else:
                physical_values = []
                for value in _matrix3_values(physical):
                    value, temporary = self._coerce(value)
                    physical_values.append(value)
                    if temporary is not None:
                        temporary_values.append(temporary)
                physical_handles = (_CVOID * 9)(
                    *[value._handle for value in physical_values]
                )
                status = self._lib.chart_reluctivity_density_matrix(
                    self._require(), coordinate_handles, position_handles,
                    physical_handles, output, message, len(message),
                )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(status, _decode(message), "reluctivity_density")
        return tuple(Expr(self, output[index]) for index in range(9))

    def _chart_h_cov(self, chart, reluctivity, vector):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            vector_values = []
            for value in vector:
                value, temporary = self._coerce(value)
                vector_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(vector_values) != 3:
                raise ValueError("h_cov requires three magnetic components")
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            vector_handles = (_CVOID * 3)(
                *[value._handle for value in vector_values]
            )
            output = (_CVOID * 3)()
            message = _message()
            status = self._lib.chart_h_cov(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, vector_handles, output, message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), "h_cov")
            return tuple(Expr(self, output[index]) for index in range(3))
        finally:
            for temporary in temporary_values:
                temporary.close()

    def _chart_fourier_weak_form(self, chart, reluctivity, mode):
        if not isinstance(mode, int) or isinstance(mode, bool):
            raise TypeError("fourier_weak_form mode must be an integer")
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            branch = ctypes.c_int()
            trial_space = ctypes.c_int()
            test_space = ctypes.c_int()
            trial_components = ctypes.c_int()
            test_components = ctypes.c_int()
            boundary_trace = ctypes.c_int()
            diffusion = (_CVOID * 4)()
            transverse_curl = _CVOID()
            mass = (_CVOID * 4)()
            message = _message()
            status = self._lib.chart_fourier_weak_form(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, mode, ctypes.byref(branch),
                ctypes.byref(trial_space), ctypes.byref(test_space),
                ctypes.byref(trial_components), ctypes.byref(test_components),
                ctypes.byref(boundary_trace),
                diffusion, ctypes.byref(transverse_curl), mass, message,
                len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(status, _decode(message), "fourier_weak_form")
        diffusion_components = tuple(
            Expr(self, diffusion[index]) for index in range(4)
        )
        components = tuple(Expr(self, mass[index]) for index in range(4))
        return FourierWeakForm(
            chart, mode, branch.value, trial_space.value, test_space.value,
            trial_components.value, test_components.value, boundary_trace.value,
            diffusion_components,
            Expr(self, transverse_curl), components,
        )

    def _chart_fourier_longitudinal_flux(
            self, chart, reluctivity, potential, component):
        if not isinstance(component, int) or isinstance(component, bool):
            raise TypeError("longitudinal flux component must be an integer")
        if component not in (1, 2):
            raise ValueError("longitudinal flux component must be 1 or 2")
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential, temporary = self._coerce(potential)
            if temporary is not None:
                temporary_values.append(temporary)
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.chart_fourier_longitudinal_flux(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential._handle, component,
                ctypes.byref(output), message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message), "fourier_longitudinal_flux"
            )
        return Expr(self, output)

    def _chart_fourier_transverse_flux(
            self, chart, reluctivity, potential):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential_values = []
            for value in potential:
                value, temporary = self._coerce(value)
                potential_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(potential_values) != 2:
                raise ValueError(
                    "fourier_transverse_flux potential requires two components"
                )
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            potential_handles = (_CVOID * 2)(
                *[value._handle for value in potential_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.chart_fourier_transverse_flux(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential_handles,
                ctypes.byref(output), message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message), "fourier_transverse_flux"
            )
        return Expr(self, output)

    def _chart_fourier_longitudinal_boundary_flux(
            self, chart, reluctivity, potential, normal):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential, temporary = self._coerce(potential)
            if temporary is not None:
                temporary_values.append(temporary)
            normal_values = []
            for value in normal:
                value, temporary = self._coerce(value)
                normal_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(normal_values) != 2:
                raise ValueError(
                    "fourier_longitudinal_boundary_flux normal requires two "
                    "components"
                )
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            normal_handles = (_CVOID * 2)(
                *[value._handle for value in normal_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.chart_fourier_longitudinal_boundary_flux(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential._handle, normal_handles,
                ctypes.byref(output), message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message),
                "fourier_longitudinal_boundary_flux",
            )
        return Expr(self, output)

    def _chart_fourier_transverse_boundary_flux(
            self, chart, reluctivity, potential, normal):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential_values = []
            for value in potential:
                value, temporary = self._coerce(value)
                potential_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            normal_values = []
            for value in normal:
                value, temporary = self._coerce(value)
                normal_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(potential_values) != 2:
                raise ValueError(
                    "fourier_transverse_boundary_flux potential requires two "
                    "components"
                )
            if len(normal_values) != 2:
                raise ValueError(
                    "fourier_transverse_boundary_flux normal requires two "
                    "components"
                )
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            potential_handles = (_CVOID * 2)(
                *[value._handle for value in potential_values]
            )
            normal_handles = (_CVOID * 2)(
                *[value._handle for value in normal_values]
            )
            output = (_CVOID * 2)()
            message = _message()
            status = self._lib.chart_fourier_transverse_boundary_flux(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential_handles, normal_handles,
                output, message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message),
                "fourier_transverse_boundary_flux",
            )
        return tuple(Expr(self, output[index]) for index in range(2))

    def _chart_fourier_transverse_boundary_contraction(
            self, chart, reluctivity, potential, normal, test):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential_values = []
            for value in potential:
                value, temporary = self._coerce(value)
                potential_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            normal_values = []
            for value in normal:
                value, temporary = self._coerce(value)
                normal_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            test_values = []
            for value in test:
                value, temporary = self._coerce(value)
                test_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(potential_values) != 2:
                raise ValueError(
                    "fourier_transverse_boundary_contraction potential "
                    "requires two components"
                )
            if len(normal_values) != 2:
                raise ValueError(
                    "fourier_transverse_boundary_contraction normal requires "
                    "two components"
                )
            if len(test_values) != 2:
                raise ValueError(
                    "fourier_transverse_boundary_contraction test requires "
                    "two components"
                )
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            potential_handles = (_CVOID * 2)(
                *[value._handle for value in potential_values]
            )
            normal_handles = (_CVOID * 2)(
                *[value._handle for value in normal_values]
            )
            test_handles = (_CVOID * 2)(
                *[value._handle for value in test_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.chart_fourier_transverse_boundary_contraction(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential_handles, normal_handles,
                test_handles, ctypes.byref(output), message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message),
                "fourier_transverse_boundary_contraction",
            )
        return Expr(self, output)

    def _chart_fourier_longitudinal_residual(
            self, chart, reluctivity, potential, current):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential, temporary = self._coerce(potential)
            if temporary is not None:
                temporary_values.append(temporary)
            current, temporary = self._coerce(current)
            if temporary is not None:
                temporary_values.append(temporary)
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.chart_fourier_longitudinal_residual(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential._handle, current._handle,
                ctypes.byref(output), message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message), "fourier_longitudinal_residual"
            )
        return Expr(self, output)

    def _chart_fourier_transverse_residual(
            self, chart, reluctivity, potential, current, mode):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        temporary_values = []
        try:
            reluctivity_values = []
            for value in _matrix3_values(reluctivity):
                value, temporary = self._coerce(value)
                reluctivity_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            potential_values = []
            current_values = []
            for value in potential:
                value, temporary = self._coerce(value)
                potential_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            for value in current:
                value, temporary = self._coerce(value)
                current_values.append(value)
                if temporary is not None:
                    temporary_values.append(temporary)
            if len(potential_values) != 2:
                raise ValueError(
                    "fourier_transverse_residual potential requires two components"
                )
            if len(current_values) != 2:
                raise ValueError(
                    "fourier_transverse_residual current requires two components"
                )
            mode, temporary = self._coerce(mode)
            if temporary is not None:
                temporary_values.append(temporary)
            reluctivity_handles = (_CVOID * 9)(
                *[value._handle for value in reluctivity_values]
            )
            potential_handles = (_CVOID * 2)(
                *[value._handle for value in potential_values]
            )
            current_handles = (_CVOID * 2)(
                *[value._handle for value in current_values]
            )
            output = (_CVOID * 2)()
            message = _message()
            status = self._lib.chart_fourier_transverse_residual(
                self._require(), coordinate_handles, position_handles,
                reluctivity_handles, potential_handles, current_handles,
                mode._handle, output, message, len(message),
            )
        finally:
            for temporary in temporary_values:
                temporary.close()
        if status:
            raise FortSymError(
                status, _decode(message), "fourier_transverse_residual"
            )
        return tuple(Expr(self, output[index]) for index in range(2))

    def _chart_current_compatibility(self, chart, current, mode):
        if not isinstance(mode, int) or isinstance(mode, bool):
            raise TypeError("current_compatibility mode must be an integer")
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = tuple(self._check(value) for value in current)
        if len(values) != 3:
            raise ValueError("current_compatibility requires three components")
        handles = (_CVOID * 3)(*[value._handle for value in values])
        output = _CVOID()
        message = _message()
        status = self._lib.chart_current_compatibility(
            self._require(), coordinate_handles, position_handles, handles,
            mode, ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(
                status, _decode(message), "current_compatibility"
            )
        return Expr(self, output)

    def _metric_inputs(self, metric):
        component_values = tuple(metric.components)
        component_handles = (_CVOID * 9)(
            *[value._handle for value in component_values]
        )
        signature = (ctypes.c_int * 3)(*metric.signature)
        return component_handles, signature

    def _metric_chart_inputs(self, metric):
        coordinates, position = self._chart_inputs(
            metric.chart.coordinates, metric.chart.position
        )
        components, signature = self._metric_inputs(metric)
        return coordinates, position, components, signature

    def _metric_sqrtg(self, metric):
        components, signature = self._metric_inputs(metric)
        output = _CVOID()
        message = _message()
        status = self._lib.metric_sqrtg(
            self._require(), components, signature, metric.orientation,
            ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "metric_sqrtg")
        return Expr(self, output)

    def _metric_volume_density(self, metric):
        components, signature = self._metric_inputs(metric)
        output = _CVOID()
        message = _message()
        status = self._lib.metric_volume_density(
            self._require(), components, signature, metric.orientation,
            ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(
                status, _decode(message), "metric_volume_density"
            )
        return Expr(self, output)

    def _metric_surface_measure(self, metric, normal_index):
        if int(normal_index) not in (1, 2, 3):
            raise ValueError("surface normal index must be 1, 2, or 3")
        components, signature = self._metric_inputs(metric)
        output = _CVOID()
        message = _message()
        status = self._lib.metric_surface_measure(
            self._require(), components, signature, metric.orientation,
            int(normal_index), ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "surface_measure")
        return Expr(self, output)

    def _metric_levi_civita(self, metric, variance):
        components, signature = self._metric_inputs(metric)
        output = (_CVOID * 27)()
        message = _message()
        status = self._lib.metric_levi_civita(
            self._require(), components, signature, metric.orientation,
            int(variance), output, message, len(message),
        )
        if status:
            raise FortSymError(
                status, _decode(message), "metric_levi_civita"
            )
        return tuple(Expr(self, output[index]) for index in range(27))

    def _metric_contravariant(self, metric):
        components, signature = self._metric_inputs(metric)
        output = (_CVOID * 9)()
        message = _message()
        status = self._lib.metric_contravariant(
            self._require(), components, signature, metric.orientation,
            output, message, len(message),
        )
        if status:
            raise FortSymError(
                status, _decode(message), "metric_contravariant"
            )
        return tuple(Expr(self, output[index]) for index in range(9))

    def _metric_inner(self, metric, left, right):
        components, signature = self._metric_inputs(metric)
        left_values = []
        right_values = []
        temporaries = []
        try:
            for value in left:
                value, temporary = self._coerce(value)
                left_values.append(value)
                if temporary is not None:
                    temporaries.append(temporary)
            for value in right:
                value, temporary = self._coerce(value)
                right_values.append(value)
                if temporary is not None:
                    temporaries.append(temporary)
            if len(left_values) != 3 or len(right_values) != 3:
                raise ValueError("metric inner products require three components")
            left_handles = (_CVOID * 3)(
                *[value._handle for value in left_values]
            )
            right_handles = (_CVOID * 3)(
                *[value._handle for value in right_values]
            )
            output = _CVOID()
            message = _message()
            status = self._lib.metric_inner(
                self._require(), components, signature, metric.orientation,
                left_handles, right_handles, ctypes.byref(output), message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), "metric_inner")
            return Expr(self, output)
        finally:
            for temporary in temporaries:
                temporary.close()

    def _metric_scalar_operation(self, operation, metric, scalar, count):
        coordinates, position, components, signature = self._metric_chart_inputs(
            metric
        )
        scalar, temporary = self._coerce(scalar)
        try:
            output = (_CVOID * count)() if count > 1 else _CVOID()
            output_argument = output if count > 1 else ctypes.byref(output)
            message = _message()
            status = operation(
                self._require(), coordinates, position, components, signature,
                metric.orientation, scalar._handle, output_argument, message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            if count == 1:
                return Expr(self, output)
            return tuple(Expr(self, output[index]) for index in range(count))
        finally:
            if temporary is not None:
                temporary.close()

    def _metric_vector_operation(self, operation, metric, vector):
        coordinates, position, components, signature = self._metric_chart_inputs(
            metric
        )
        values = tuple(self._check(value) for value in vector)
        if len(values) != 3:
            raise ValueError("metric vector operators require three components")
        vector_handles = (_CVOID * 3)(*[value._handle for value in values])
        output = _CVOID()
        message = _message()
        status = operation(
            self._require(), coordinates, position, components, signature,
            metric.orientation, vector_handles, ctypes.byref(output), message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return Expr(self, output)

    def _metric_form_unary(self, operation, metric, form):
        coordinate_handles, position_handles = self._chart_inputs(
            form.chart.coordinates, form.chart.position
        )
        components, signature = self._metric_inputs(metric)
        values = (_CVOID * 8)(*[value._handle for value in form.components])
        output = (_CVOID * 8)()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles, components,
            signature, metric.orientation, values, form.degree, output,
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(8))

    def _metric_form_star(self, metric, form):
        return self._metric_form_unary(self._lib.chart_form_star_metric, metric, form)

    def _spacetime_inputs(self, metric):
        components = (_CVOID * (SPACETIME_DIM * SPACETIME_DIM))(
            *[value._handle for value in metric.components]
        )
        coordinates = (_CVOID * SPACETIME_DIM)(
            *[value._handle for value in metric.coordinates]
        )
        signature = (ctypes.c_int * SPACETIME_DIM)(*metric.signature)
        return components, coordinates, signature

    def _spacetime_array(self, operation, metric, count):
        components, coordinates, signature = self._spacetime_inputs(metric)
        output = (_CVOID * count)()
        message = _message()
        status = operation(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(count))

    def _spacetime_tensor_metric_operation(self, operation, metric, tensor, slot):
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        if tensor.rank == 0:
            raise ValueError("metric transforms require a tensor slot")
        slot = int(slot)
        if slot < 0 or slot >= tensor.rank:
            raise IndexError("spacetime tensor slot is outside the rank")
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * (SPACETIME_DIM ** tensor.rank))()
        message = _message()
        status = operation(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, tensor.rank, variance,
            tensor.density_weight, slot + 1, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_density_factor(self, metric, tensor, factor):
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        factor = self._check(factor)
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * (SPACETIME_DIM ** tensor.rank))()
        message = _message()
        status = self._lib.spacetime_tensor_density_factor(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, tensor.rank, variance,
            tensor.density_weight, factor._handle, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "spacetime_tensor_density_factor")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_permute(self, metric, tensor, order):
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        if tensor.variance is None:
            raise ValueError("spacetime tensor permutation needs slot variance")
        order = tuple(int(value) for value in order)
        if len(order) != tensor.rank or set(order) != set(range(tensor.rank)):
            raise ValueError("spacetime tensor permutation must contain every slot once")
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        order_values = (ctypes.c_int * tensor.rank)(
            *[value + 1 for value in order]
        )
        output = (_CVOID * (SPACETIME_DIM ** tensor.rank))()
        message = _message()
        status = self._lib.spacetime_tensor_permute(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, tensor.rank, variance,
            tensor.density_weight, order_values, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "spacetime_tensor_permute")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_contract(self, metric, tensor, first, second):
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        if tensor.variance is None:
            raise ValueError("spacetime tensor contraction needs slot variance")
        first, second = int(first), int(second)
        if first < 0 or first >= tensor.rank:
            raise IndexError("first contraction slot is outside the tensor rank")
        if second < 0 or second >= tensor.rank:
            raise IndexError("second contraction slot is outside the tensor rank")
        if first == second:
            raise ValueError("tensor contraction needs two distinct slots")
        if tensor.variance[first] == tensor.variance[second]:
            raise ValueError("tensor contraction needs opposite-variance slots")
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_rank = tensor.rank - 2
        output = (_CVOID * (SPACETIME_DIM ** output_rank))()
        message = _message()
        status = self._lib.spacetime_tensor_contract(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, tensor.rank, variance,
            tensor.density_weight, first + 1, second + 1, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "spacetime_tensor_contract")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_product(self, metric, left, right):
        if not isinstance(left, SpacetimeTensor) or left.metric is not metric:
            raise ValueError("left spacetime tensor must belong to this metric")
        if not isinstance(right, SpacetimeTensor) or right.metric is not metric:
            raise ValueError("right spacetime tensor must belong to this metric")
        if left.variance is None or right.variance is None:
            raise ValueError("spacetime tensor product needs slot variance")
        output_rank = left.rank + right.rank
        if output_rank > SPACETIME_TENSOR_MAX_RANK:
            raise ValueError("native spacetime tensors support rank at most five")
        components, coordinates, signature = self._spacetime_inputs(metric)
        left_values = (_CVOID * (SPACETIME_DIM ** left.rank))(
            *[value._handle for value in left.components]
        )
        right_values = (_CVOID * (SPACETIME_DIM ** right.rank))(
            *[value._handle for value in right.components]
        )
        left_variance = (ctypes.c_int * left.rank)(*left.variance)
        right_variance = (ctypes.c_int * right.rank)(*right.variance)
        output = (_CVOID * (SPACETIME_DIM ** output_rank))()
        message = _message()
        status = self._lib.spacetime_tensor_product(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, left_values, left.rank, left_variance,
            left.density_weight, right_values, right.rank, right_variance,
            right.density_weight, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "spacetime_tensor_product")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_covariant_diff(self, metric, tensor):
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        if tensor.variance is None:
            raise ValueError("spacetime covariant differentiation needs slot variance")
        if tensor.rank >= SPACETIME_TENSOR_MAX_RANK:
            raise ValueError("native spacetime covariant derivative supports input rank at most four")
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_rank = tensor.rank + 1
        output = (_CVOID * (SPACETIME_DIM ** output_rank))()
        message = _message()
        status = self._lib.spacetime_tensor_covariant_diff(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, tensor.rank, variance,
            tensor.density_weight, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "spacetime_tensor_covariant_diff")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_covariant_divergence(self, metric, tensor):
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        if tensor.rank < 1 or tensor.variance is None:
            raise ValueError("spacetime divergence needs a positive-rank tensor")
        if tensor.variance[0] != 1:
            raise ValueError("spacetime divergence needs a contravariant first slot")
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_rank = tensor.rank - 1
        output = (_CVOID * (SPACETIME_DIM ** output_rank))()
        message = _message()
        status = self._lib.spacetime_tensor_covariant_divergence(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, tensor.rank, variance,
            tensor.density_weight, output, message, len(message),
        )
        if status:
            raise FortSymError(
                status, _decode(message), "spacetime_tensor_covariant_divergence"
            )
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_tensor_lie(self, metric, vector, tensor):
        if not isinstance(vector, SpacetimeTensor) or vector.metric is not metric:
            raise ValueError("spacetime Lie derivative needs a vector from this metric")
        if vector.rank != 1 or vector.variance != (1,) or vector.density_weight != 0:
            raise ValueError("spacetime Lie derivative needs a weight-zero contravariant vector")
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not metric:
            raise ValueError("spacetime tensor must belong to this metric")
        if tensor.rank > 0 and tensor.variance is None:
            raise ValueError("spacetime Lie derivative needs slot variance")
        components, coordinates, signature = self._spacetime_inputs(metric)
        vector_values = (_CVOID * SPACETIME_DIM)(
            *[value._handle for value in vector.components]
        )
        values = (_CVOID * (SPACETIME_DIM ** tensor.rank))(
            *[value._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(
            *(tensor.variance or ())
        )
        output = (_CVOID * (SPACETIME_DIM ** tensor.rank))()
        message = _message()
        status = self._lib.spacetime_tensor_lie(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, vector_values, values, tensor.rank, variance,
            tensor.density_weight, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "spacetime_tensor_lie")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _spacetime_scalar(self, operation, metric):
        components, coordinates, signature = self._spacetime_inputs(metric)
        output = _CVOID()
        message = _message()
        status = operation(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, ctypes.byref(output), message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return Expr(self, output)

    def _spacetime_vector_array(self, operation, metric, vector):
        values = tuple(vector)
        if len(values) != SPACETIME_DIM:
            raise ValueError("spacetime vector operators require four components")
        coerced = []
        temporaries = []
        try:
            for value in values:
                expression, temporary = self._coerce(value)
                coerced.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            components, coordinates, signature = self._spacetime_inputs(metric)
            vector_values = (_CVOID * SPACETIME_DIM)(
                *[value._handle for value in coerced]
            )
            output = (_CVOID * SPACETIME_DIM)()
            message = _message()
            status = operation(
                self._require(), components, metric.dimension, coordinates,
                signature, metric.orientation, vector_values, output, message,
                len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return tuple(Expr(self, output[index]) for index in range(SPACETIME_DIM))
        finally:
            for temporary in temporaries:
                temporary.close()

    def _spacetime_scalar_array(self, operation, metric, scalar):
        expression, temporary = self._coerce(scalar)
        try:
            components, coordinates, signature = self._spacetime_inputs(metric)
            output = (_CVOID * SPACETIME_DIM)()
            message = _message()
            status = operation(
                self._require(), components, metric.dimension, coordinates,
                signature, metric.orientation, expression._handle, output,
                message, len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return tuple(Expr(self, output[index]) for index in range(SPACETIME_DIM))
        finally:
            if temporary is not None:
                temporary.close()

    def _spacetime_vector_scalar(self, operation, metric, vector):
        values = tuple(vector)
        if len(values) != SPACETIME_DIM:
            raise ValueError("spacetime vector operators require four components")
        coerced = []
        temporaries = []
        try:
            for value in values:
                expression, temporary = self._coerce(value)
                coerced.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            components, coordinates, signature = self._spacetime_inputs(metric)
            vector_values = (_CVOID * SPACETIME_DIM)(
                *[value._handle for value in coerced]
            )
            output = _CVOID()
            message = _message()
            status = operation(
                self._require(), components, metric.dimension, coordinates,
                signature, metric.orientation, vector_values,
                ctypes.byref(output), message, len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return Expr(self, output)
        finally:
            for temporary in temporaries:
                temporary.close()

    def _spacetime_scalar_scalar(self, operation, metric, scalar):
        expression, temporary = self._coerce(scalar)
        try:
            components, coordinates, signature = self._spacetime_inputs(metric)
            output = _CVOID()
            message = _message()
            status = operation(
                self._require(), components, metric.dimension, coordinates,
                signature, metric.orientation, expression._handle,
                ctypes.byref(output), message, len(message),
            )
            if status:
                raise FortSymError(status, _decode(message), operation.__name__)
            return Expr(self, output)
        finally:
            if temporary is not None:
                temporary.close()

    def _spacetime_geodesic_residual(self, metric, curve, parameter):
        components, coordinates, signature = self._spacetime_inputs(metric)
        curve_values = (_CVOID * SPACETIME_DIM)(
            *[value._handle for value in curve]
        )
        output = (_CVOID * SPACETIME_DIM)()
        message = _message()
        status = self._lib.spacetime_geodesic_residual(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, curve_values, parameter._handle, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "geodesic_residual")
        return tuple(Expr(self, output[index]) for index in range(SPACETIME_DIM))

    def _spacetime_form_unary(self, operation, metric, form, output_degree):
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * 16)(*[value._handle for value in form.components])
        output = (_CVOID * 16)()
        message = _message()
        status = operation(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, form.degree, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(16)), output_degree

    def _spacetime_form_closed(self, metric, form):
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * 16)(*[value._handle for value in form.components])
        verdict = ctypes.c_int()
        message = _message()
        status = self._lib.spacetime_form_closed(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, form.degree, ctypes.byref(verdict),
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "form_closed")
        return verdict.value

    def _spacetime_form_binary(self, operation, metric, left, right):
        components, coordinates, signature = self._spacetime_inputs(metric)
        left_values = (_CVOID * 16)(*[value._handle for value in left.components])
        right_values = (_CVOID * 16)(*[value._handle for value in right.components])
        output = (_CVOID * 16)()
        message = _message()
        status = operation(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, left_values, left.degree, right_values,
            right.degree, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(16)), \
            left.degree + right.degree

    def _spacetime_form_vector_operation(
            self, operation, metric, vector, form, output_degree):
        components, coordinates, signature = self._spacetime_inputs(metric)
        vector_values = (_CVOID * SPACETIME_DIM)(
            *[value._handle for value in vector]
        )
        values = (_CVOID * 16)(*[value._handle for value in form.components])
        output = (_CVOID * 16)()
        message = _message()
        status = operation(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, vector_values, values, form.degree, output,
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(16)), output_degree

    def _spacetime_field_strength(self, metric, potential):
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * 16)(*[value._handle for value in potential.components])
        output = (_CVOID * 16)()
        message = _message()
        status = self._lib.spacetime_field_strength(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "field_strength")
        return tuple(Expr(self, output[index]) for index in range(16))

    def _spacetime_gauge_transform(self, metric, potential, chi):
        components, coordinates, signature = self._spacetime_inputs(metric)
        values = (_CVOID * 16)(*[value._handle for value in potential.components])
        output = (_CVOID * 16)()
        message = _message()
        status = self._lib.spacetime_gauge_transform(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, values, chi._handle, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "gauge_transform")
        return tuple(Expr(self, output[index]) for index in range(16))

    def _spacetime_maxwell_residual(self, metric, potential, current):
        components, coordinates, signature = self._spacetime_inputs(metric)
        potential_values = (_CVOID * 16)(
            *[value._handle for value in potential.components]
        )
        current_values = (_CVOID * 16)(
            *[value._handle for value in current.components]
        )
        output = (_CVOID * 16)()
        message = _message()
        status = self._lib.spacetime_maxwell_residual(
            self._require(), components, metric.dimension, coordinates, signature,
            metric.orientation, potential_values, current_values, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "maxwell_residual")
        return tuple(Expr(self, output[index]) for index in range(16))

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

    def _chart_covariant_divergence(self, chart, tensor):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        component_handles = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance_values = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * (3 ** (tensor.rank - 1)))()
        message = _message()
        status = self._lib.chart_covariant_divergence(
            self._require(), coordinate_handles, position_handles,
            component_handles, tensor.rank, variance_values,
            tensor.density_weight, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "covariant_divergence")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_metric(self, operation, chart, tensor, slot):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        component_values = [self._check(value) for value in tensor.components]
        component_handles = (_CVOID * len(component_values))(
            *[value._handle for value in component_values]
        )
        variance_values = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_count = 3 ** tensor.rank
        output = (_CVOID * output_count)()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles,
            component_handles, tensor.rank, variance_values,
            tensor.density_weight, slot + 1, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(output_count))

    def _chart_tensor_density(self, chart, tensor, density_weight):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        component_values = [self._check(value) for value in tensor.components]
        component_handles = (_CVOID * len(component_values))(
            *[value._handle for value in component_values]
        )
        variance_values = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_count = 3 ** tensor.rank
        output = (_CVOID * output_count)()
        message = _message()
        status = self._lib.chart_tensor_density(
            self._require(), coordinate_handles, position_handles,
            component_handles, tensor.rank, variance_values,
            tensor.density_weight, int(density_weight), output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_density")
        return tuple(Expr(self, output[index]) for index in range(output_count))

    def _chart_tensor_density_factor(self, chart, tensor, factor):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        component_values = [self._check(value) for value in tensor.components]
        component_handles = (_CVOID * len(component_values))(
            *[value._handle for value in component_values]
        )
        variance_values = (ctypes.c_int * tensor.rank)(*tensor.variance)
        factor = self._check(factor)
        output_count = 3 ** tensor.rank
        output = (_CVOID * output_count)()
        message = _message()
        status = self._lib.chart_tensor_density_factor(
            self._require(), coordinate_handles, position_handles,
            component_handles, tensor.rank, variance_values,
            tensor.density_weight, factor._handle, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_density_factor")
        return tuple(Expr(self, output[index]) for index in range(output_count))

    def _chart_form_from_tensor(self, chart, tensor):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * 8)()
        message = _message()
        status = self._lib.chart_form_from_tensor(
            self._require(), coordinate_handles, position_handles, values,
            tensor.rank, variance, tensor.density_weight, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "form_from_tensor")
        return tuple(Expr(self, output[index]) for index in range(8))

    def _chart_tensor_from_form(self, chart, form):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        output = (_CVOID * (3 ** form.degree))()
        message = _message()
        status = self._lib.chart_tensor_from_form(
            self._require(), coordinate_handles, position_handles, values,
            form.degree, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_from_form")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_permute(self, chart, tensor, order):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        order_values = (ctypes.c_int * tensor.rank)(
            *[int(value) + 1 for value in order]
        )
        output = (_CVOID * len(tensor.components))()
        message = _message()
        status = self._lib.chart_tensor_permute(
            self._require(), coordinate_handles, position_handles, values,
            tensor.rank, variance, tensor.density_weight, order_values, output,
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_permute")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_contract(self, chart, tensor, first, second):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * (3 ** (tensor.rank - 2)))()
        message = _message()
        status = self._lib.chart_tensor_contract(
            self._require(), coordinate_handles, position_handles, values,
            tensor.rank, variance, tensor.density_weight, first + 1, second + 1,
            output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_contract")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_product(self, chart, left, right):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        left_values = (_CVOID * len(left.components))(
            *[self._check(value)._handle for value in left.components]
        )
        right_values = (_CVOID * len(right.components))(
            *[self._check(value)._handle for value in right.components]
        )
        left_variance = (ctypes.c_int * left.rank)(*left.variance)
        right_variance = (ctypes.c_int * right.rank)(*right.variance)
        output = (_CVOID * (3 ** (left.rank + right.rank)))()
        message = _message()
        status = self._lib.chart_tensor_product(
            self._require(), coordinate_handles, position_handles, left_values,
            left.rank, left_variance, left.density_weight, right_values,
            right.rank, right_variance, right.density_weight, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_product")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_symmetrize(self, chart, tensor, first, second, antisymmetric):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * len(tensor.components))()
        message = _message()
        status = self._lib.chart_tensor_symmetrize(
            self._require(), coordinate_handles, position_handles, values,
            tensor.rank, variance, tensor.density_weight, first + 1, second + 1,
            int(antisymmetric), output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_symmetrize")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_declare_symmetry(self, chart, tensor, first, second, kind):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * len(tensor.components))()
        message = _message()
        status = self._lib.chart_tensor_declare_symmetry(
            self._require(), coordinate_handles, position_handles, values,
            tensor.rank, variance, tensor.density_weight, first + 1, second + 1,
            int(kind), output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_symmetry")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_tensor_lie(self, chart, vector, tensor):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        vector_values = (_CVOID * len(vector.components))(
            *[self._check(value)._handle for value in vector.components]
        )
        vector_variance = (ctypes.c_int * vector.rank)(*vector.variance)
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output = (_CVOID * len(tensor.components))()
        message = _message()
        status = self._lib.chart_tensor_lie(
            self._require(), coordinate_handles, position_handles,
            vector_values, vector.rank, vector_variance, vector.density_weight,
            values, tensor.rank, variance, tensor.density_weight, output,
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "tensor_lie")
        return tuple(Expr(self, output[index]) for index in range(len(output)))

    def _chart_connection_torsion(self, connection):
        coordinate_handles, position_handles = self._chart_inputs(
            connection.chart.coordinates, connection.chart.position
        )
        values = (_CVOID * 27)(
            *[self._check(value)._handle for value in connection.components]
        )
        output = (_CVOID * 27)()
        message = _message()
        status = self._lib.chart_connection_torsion(
            self._require(), coordinate_handles, position_handles, values, output,
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "connection_torsion")
        return tuple(Expr(self, output[index]) for index in range(27))

    def _chart_connection_nonmetricity(self, connection, metric):
        coordinate_handles, position_handles = self._chart_inputs(
            connection.chart.coordinates, connection.chart.position
        )
        connection_values = (_CVOID * 27)(
            *[self._check(value)._handle for value in connection.components]
        )
        metric_values = (_CVOID * 9)(
            *[self._check(value)._handle for value in metric.components]
        )
        signature = (ctypes.c_int * 3)(*metric.signature)
        output = (_CVOID * 27)()
        message = _message()
        status = self._lib.chart_connection_nonmetricity(
            self._require(), coordinate_handles, position_handles,
            connection_values, metric_values, signature, metric.orientation, output,
            message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "connection_nonmetricity")
        return tuple(Expr(self, output[index]) for index in range(27))

    def _chart_connection_riemann(self, connection):
        coordinate_handles, position_handles = self._chart_inputs(
            connection.chart.coordinates, connection.chart.position
        )
        values = (_CVOID * 27)(
            *[self._check(value)._handle for value in connection.components]
        )
        output = (_CVOID * 81)()
        message = _message()
        status = self._lib.chart_connection_riemann(
            self._require(), coordinate_handles, position_handles, values,
            connection.convention, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "connection_riemann")
        return tuple(Expr(self, output[index]) for index in range(81))

    def _chart_connection_geodesic_residual(self, connection, curve, parameter):
        coordinate_handles, position_handles = self._chart_inputs(
            connection.chart.coordinates, connection.chart.position
        )
        connection_values = (_CVOID * 27)(
            *[self._check(value)._handle for value in connection.components]
        )
        curve_values = tuple(curve)
        if len(curve_values) != 3:
            raise ValueError("geodesic curves require three components")
        coerced = []
        temporaries = []
        try:
            for value in curve_values:
                expression, temporary = self._coerce(value)
                coerced.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            parameter_value, temporary = self._coerce(parameter)
            if temporary is not None:
                temporaries.append(temporary)
            curve_handles = (_CVOID * 3)(
                *[value._handle for value in coerced]
            )
            output = (_CVOID * 3)()
            message = _message()
            status = self._lib.chart_connection_geodesic_residual(
                self._require(), coordinate_handles, position_handles,
                connection_values, curve_handles, parameter_value._handle,
                output, message, len(message),
            )
            if status:
                raise FortSymError(
                    status, _decode(message), "connection_geodesic_residual"
                )
            return tuple(Expr(self, output[index]) for index in range(3))
        finally:
            for temporary in temporaries:
                temporary.close()

    def _chart_connection_tensor(self, operation, connection, tensor, output_rank):
        coordinate_handles, position_handles = self._chart_inputs(
            connection.chart.coordinates, connection.chart.position
        )
        connection_values = (_CVOID * 27)(
            *[self._check(value)._handle for value in connection.components]
        )
        values = (_CVOID * len(tensor.components))(
            *[self._check(value)._handle for value in tensor.components]
        )
        variance = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_count = 3 ** int(output_rank)
        output = (_CVOID * output_count)()
        message = _message()
        status = operation(
            self._require(), coordinate_handles, position_handles,
            connection_values, values, tensor.rank, variance,
            tensor.density_weight, output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
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

    def _chart_form_closed(self, form):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        coordinate_handles, position_handles = self._chart_inputs(
            form.chart.coordinates, form.chart.position
        )
        verdict = ctypes.c_int()
        message = _message()
        status = self._lib.chart_form_closed(
            self._require(), coordinate_handles, position_handles, components,
            form.degree, ctypes.byref(verdict), message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "form_closed")
        return verdict.value

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

    def _chart_form_volume(self, chart, orientation):
        coordinate_handles, position_handles = self._chart_inputs(
            chart.coordinates, chart.position
        )
        output = (_CVOID * 8)()
        message = _message()
        status = self._lib.chart_form_volume(
            self._require(), coordinate_handles, position_handles,
            int(orientation), output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "form_volume")
        return tuple(Expr(self, output[index]) for index in range(8))

    def _chart_form_sharp(self, form):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        return self._chart_form_output(
            self._lib.chart_form_sharp,
            form.chart.coordinates, form.chart.position,
            [components, form.degree], 3,
        )

    def _chart_b_form(self, chart, form, orientation, density):
        components = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        output = (_CVOID * 3)()
        message = _message()
        operation = (
            self._lib.chart_b_density_form if density
            else self._lib.chart_b_con_form
        )
        status = operation(
            self._require(), *self._chart_inputs(chart.coordinates, chart.position),
            components, form.degree, int(orientation), output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "b_form")
        return tuple(Expr(self, output[index]) for index in range(3))

    def _chart_b_flux_form(self, chart, vector, orientation):
        vector = tuple(self._check(value) for value in vector)
        if len(vector) != 3:
            raise ValueError("b_flux_form expects three contravariant components")
        vector_handles = (_CVOID * 3)(*[value._handle for value in vector])
        output = (_CVOID * 8)()
        message = _message()
        status = self._lib.chart_b_flux_form(
            self._require(), *self._chart_inputs(chart.coordinates, chart.position),
            vector_handles, int(orientation), output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "b_flux_form")
        return tuple(Expr(self, output[index]) for index in range(8))

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

    def _chart_map_inputs(self, chart_map):
        source_coordinates, source_position = self._chart_inputs(
            chart_map.source.coordinates, chart_map.source.position
        )
        target_coordinates, target_position = self._chart_inputs(
            chart_map.target.coordinates, chart_map.target.position
        )
        forward = (_CVOID * 3)(
            *[self._check(value)._handle for value in chart_map.forward]
        )
        inverse = (_CVOID * 3)(
            *[self._check(value)._handle for value in chart_map.inverse]
        )
        return (
            source_coordinates, source_position, target_coordinates,
            target_position, forward, inverse,
        )

    def _chart_map_matrix(self, operation, chart_map):
        arguments = self._chart_map_inputs(chart_map)
        output = (_CVOID * 9)()
        message = _message()
        status = operation(
            self._require(), *arguments, output, message, len(message)
        )
        if status:
            raise FortSymError(status, _decode(message), operation.__name__)
        return tuple(Expr(self, output[index]) for index in range(9))

    def _chart_map_tensor(self, chart_map, tensor):
        arguments = self._chart_map_inputs(chart_map)
        component_values = [self._check(value) for value in tensor.components]
        component_handles = (_CVOID * len(component_values))(
            *[value._handle for value in component_values]
        )
        variance_values = (ctypes.c_int * tensor.rank)(*tensor.variance)
        output_count = 3 ** tensor.rank
        output = (_CVOID * output_count)()
        message = _message()
        status = self._lib.chart_map_tensor(
            self._require(), *arguments, component_handles, tensor.rank,
            variance_values, tensor.density_weight, output, message,
            len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "map_tensor")
        return tuple(Expr(self, output[index]) for index in range(output_count))

    def _chart_map_form(self, chart_map, form):
        arguments = self._chart_map_inputs(chart_map)
        component_handles = (_CVOID * 8)(
            *[self._check(value)._handle for value in form.components]
        )
        output = (_CVOID * 8)()
        message = _message()
        status = self._lib.chart_map_form(
            self._require(), *arguments, component_handles, form.degree,
            output, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "map_form")
        return tuple(Expr(self, output[index]) for index in range(8))

    def _chart_map_compose(self, first, following):
        source_coordinates, source_position = self._chart_inputs(
            first.source.coordinates, first.source.position
        )
        middle_coordinates, middle_position = self._chart_inputs(
            first.target.coordinates, first.target.position
        )
        target_coordinates, target_position = self._chart_inputs(
            following.target.coordinates, following.target.position
        )
        first_forward = (_CVOID * 3)(
            *[self._check(value)._handle for value in first.forward]
        )
        first_inverse = (_CVOID * 3)(
            *[self._check(value)._handle for value in first.inverse]
        )
        following_forward = (_CVOID * 3)(
            *[self._check(value)._handle for value in following.forward]
        )
        following_inverse = (_CVOID * 3)(
            *[self._check(value)._handle for value in following.inverse]
        )
        forward = (_CVOID * 3)()
        inverse = (_CVOID * 3)()
        message = _message()
        status = self._lib.chart_map_compose(
            self._require(), source_coordinates, source_position,
            middle_coordinates, middle_position, target_coordinates,
            target_position, first_forward, first_inverse, following_forward,
            following_inverse, forward, inverse, message, len(message),
        )
        if status:
            raise FortSymError(status, _decode(message), "map_compose")
        return tuple(Expr(self, forward[index]) for index in range(3)), tuple(
            Expr(self, inverse[index]) for index in range(3)
        )

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

    def __init__(self, coordinates, position, patch=None):
        self.coordinates = tuple(coordinates)
        self.position = tuple(position)
        if len(self.coordinates) != 3 or len(self.position) != 3:
            raise ValueError("fortsym charts require three coordinates and positions")
        if patch is not None:
            manifold = getattr(patch, "manifold", None)
            dimension = getattr(manifold, "dim", None)
            if dimension != 3:
                raise ValueError("chart patch must belong to a three-dimensional manifold")
        self.patch = patch
        self._arena = self.coordinates[0]._arena
        self._arena._chart_inputs(self.coordinates, self.position)

    @property
    def has_patch(self):
        return self.patch is not None

    def sqrtg(self):
        return self._arena._chart_sqrtg(self.coordinates, self.position)

    def surface_measure(self, normal_index=1):
        """Return the positive measure on ``coordinate[normal_index]=const``."""
        return self._arena._chart_surface_measure(
            self.coordinates, self.position, normal_index
        )

    def flux_surface(self, label_index=1):
        """Describe ``coordinate[label_index]=constant`` as a flux surface."""
        return FluxSurface(self, label_index)

    def flux_coordinates(self, label_index=1, kind=FLUX_GENERIC):
        """Describe a flux-coordinate convention on this chart."""
        return FluxCoordinates(self, label_index, kind)

    def magnetic_chart(self, potential, label_index=1):
        """Bundle this chart, a potential, and its typed magnetic views."""
        return MagneticChart(self, potential, label_index)

    def jacobian(self):
        """Return the signed determinant of the chart Jacobian."""
        return self._arena._chart_jacobian(self.coordinates, self.position)

    def grad(self, scalar):
        return self._arena._chart_scalar_array(
            self._arena._lib.chart_grad, self.coordinates, self.position, scalar
        )

    def divergence(self, vector):
        return self._arena._chart_vector_scalar(
            self._arena._lib.chart_divergence,
            self.coordinates, self.position, vector,
        )

    def field_line_derivative(self, vector, scalar):
        """Return ``vector**i*diff(scalar, coordinate_i)``."""
        scalar, temporary = self._arena._coerce(scalar)
        try:
            coordinate_handles, position_handles = self._arena._chart_inputs(
                self.coordinates, self.position
            )
            values = tuple(self._arena._check(value) for value in vector)
            if len(values) != 3:
                raise ValueError(
                    "field-line derivatives require three vector components"
                )
            vector_handles = (_CVOID * 3)(*[value._handle for value in values])
            output = _CVOID()
            message = _message()
            status = self._arena._lib.chart_field_line_derivative(
                self._arena._require(), coordinate_handles, position_handles,
                vector_handles, scalar._handle, ctypes.byref(output), message,
                len(message),
            )
            if status:
                raise FortSymError(
                    status, _decode(message), "field_line_derivative"
                )
            return Expr(self._arena, output)
        finally:
            if temporary is not None:
                temporary.close()

    div = divergence

    def div_density(self, vector_density):
        return self._arena._chart_vector_scalar(
            self._arena._lib.chart_div_density,
            self.coordinates, self.position, vector_density,
        )

    def curl(self, covector):
        return self._arena._chart_vector_array(
            self._arena._lib.chart_curl, self.coordinates, self.position, covector
        )

    def curl_density(self, covector):
        return self._arena._chart_vector_array(
            self._arena._lib.chart_curl_density,
            self.coordinates, self.position, covector,
        )

    def laplacian(self, scalar):
        return self._arena._chart_scalar_scalar(
            self._arena._lib.chart_laplacian,
            self.coordinates, self.position, scalar,
        )

    def covariant_basis(self):
        """Return ``e(component, index)`` in component-first flat order."""
        return self._basis_result(self._arena._lib.chart_covariant_basis)

    def reciprocal_basis(self):
        """Return the dual basis ``e^index`` in component-first flat order."""
        return self._basis_result(self._arena._lib.chart_reciprocal_basis)

    def tensor(self, components, variance=(), density_weight=0, symmetries=None):
        result = Tensor(self, components, variance, density_weight)
        if symmetries is None:
            return result
        declarations = getattr(symmetries, "pairs", symmetries)
        for first, second, kind in declarations:
            next_result = result.declare_symmetry(first, second, kind)
            result.close()
            result = next_result
        return result

    def form_from_tensor(self, tensor):
        """Convert an exact weight-zero lower antisymmetric tensor to a form."""
        if not isinstance(tensor, Tensor) or tensor.chart is not self:
            raise ValueError("form_from_tensor expects a tensor from this chart")
        components = self._arena._chart_form_from_tensor(self, tensor)
        return Form(self, components, tensor.rank, _owned=True)

    def tensor_from_form(self, form):
        """Convert a native k-form to its lower antisymmetric tensor view."""
        if not isinstance(form, Form) or form.chart is not self:
            raise ValueError("tensor_from_form expects a form from this chart")
        components = self._arena._chart_tensor_from_form(self, form)
        symmetries = tuple((first, second, ANTISYMMETRIC)
                           for first in range(form.degree)
                           for second in range(first + 1, form.degree))
        return Tensor(
            self, components, (-1,) * form.degree, 0, _owned=True,
            _symmetries=symmetries,
        )

    def vector(self, components, density_weight=0):
        return Tensor(self, components, (1,), density_weight)

    def covector(self, components, density_weight=0):
        return Tensor(self, components, (-1,), density_weight)

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
            self._arena._lib.chart_metric_covariant, 2, (-1, -1),
            symmetries=((0, 1, SYMMETRIC),),
        )

    def metric_contravariant(self):
        return self._tensor_result(
            self._arena._lib.chart_metric_contravariant, 2, (1, 1),
            symmetries=((0, 1, SYMMETRIC),),
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
            _owned=True, _symmetries=tensor.symmetries,
        )

    def covariant_divergence(self, tensor):
        if not isinstance(tensor, Tensor) or tensor.chart is not self:
            raise ValueError("covariant_divergence expects a tensor from this chart")
        if tensor.rank < 1 or tensor.variance[0] != 1:
            raise ValueError("covariant_divergence requires a contravariant first slot")
        components = self._arena._chart_covariant_divergence(self, tensor)
        survivors = list(range(1, tensor.rank))
        positions = {source: target for target, source in enumerate(survivors)}
        symmetries = tuple((positions[first], positions[second], kind)
                           for (first, second), kind in tensor._symmetries.items()
                           if first in positions and second in positions)
        return Tensor(
            self, components, tensor.variance[1:], tensor.density_weight,
            _owned=True, _symmetries=symmetries,
        )

    covariant_derivative = covariant_diff

    def lie(self, vector, tensor):
        """Return the Lie derivative of ``tensor`` along ``vector``."""
        if not isinstance(vector, Tensor) or vector.chart is not self:
            raise ValueError("lie expects an ordinary vector from this chart")
        if vector.rank != 1 or vector.variance != (1,) or vector.density_weight != 0:
            raise ValueError("lie expects a weight-zero contravariant vector")
        if not isinstance(tensor, Tensor) or tensor.chart is not self:
            raise ValueError("lie expects a tensor from this chart")
        components = self._arena._chart_tensor_lie(self, vector, tensor)
        return Tensor(
            self, components, tensor.variance, tensor.density_weight, _owned=True,
            _symmetries=tensor.symmetries,
        )

    lie_derivative = lie

    def killing(self, vector):
        """Return the Killing residual ``L_vector(g)`` for this chart metric."""
        return self.lie(vector, self.metric_covariant())

    def riemann(self):
        return self._tensor_result(
            self._arena._lib.chart_riemann, 4, (1, -1, -1, -1)
        )

    def first_bianchi_residual(self):
        """Return ``R^a_bcd + R^a_cdb + R^a_dbc``."""
        return self._tensor_result(
            self._arena._lib.chart_first_bianchi_residual,
            4, (1, -1, -1, -1),
        )

    def second_bianchi_residual(self):
        """Return ``nabla_e R^a_bcd + nabla_c R^a_bde + nabla_d R^a_bec``."""
        return self._tensor_result(
            self._arena._lib.chart_second_bianchi_residual,
            5, (1, -1, -1, -1, -1),
        )

    def geodesic_residual(self, curve, parameter):
        """Return ``x''^a + Gamma^a_bc x'^b x'^c`` on this chart."""
        return self._arena._chart_geodesic_residual(self, curve, parameter)

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

    def _tensor_result(self, operation, rank, variance, density_weight=0,
                       symmetries=()):
        components = self._arena._chart_tensor(
            operation, self.coordinates, self.position, rank
        )
        return Tensor(
            self, components, variance, density_weight, _owned=True,
            _symmetries=symmetries,
        )

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

    def volume(self, orientation=1):
        if int(orientation) not in (-1, 1):
            raise ValueError("volume orientation must be 1 or -1")
        components = self._arena._chart_form_volume(self, orientation)
        return Form(self, components, 3, _owned=True)

    def flat(self, vector):
        components = self._arena._chart_form_flat(self, vector)
        return Form(self, components, 1, _owned=True)

    def b_cov(self, vector):
        return self._arena._chart_many(
            self._arena._lib.chart_b_cov,
            self.coordinates, self.position, vector,
        )

    def b_density(self, vector):
        return self._arena._chart_many(
            self._arena._lib.chart_b_density,
            self.coordinates, self.position, vector,
        )

    def b_con_form(self, beta, orientation=1):
        """Return typed ``B^i`` recovered from a magnetic two-form."""
        if not isinstance(beta, Form) or beta.chart is not self:
            raise ValueError("b_con_form expects a two-form from this chart")
        if beta.degree != 2:
            raise ValueError("b_con_form expects a degree-two form")
        components = self._arena._chart_b_form(self, beta, orientation, False)
        return self.vector(components)

    def b_flux_form(self, vector, orientation=1):
        """Return ``beta = i_B(orientation*Omega)`` as a degree-two form."""
        if isinstance(vector, Tensor):
            if vector.chart is not self or vector.rank != 1:
                raise ValueError("b_flux_form expects a vector from this chart")
            if vector.variance != (1,) or vector.density_weight != 0:
                raise ValueError("b_flux_form expects a weight-zero contravariant vector")
            vector = vector.components
        components = self._arena._chart_b_flux_form(self, vector, orientation)
        return Form(self, components, 2, _owned=True)

    def b_density_form(self, beta, orientation=1):
        """Return typed ``sqrt(g) B^i`` recovered from a magnetic two-form."""
        if not isinstance(beta, Form) or beta.chart is not self:
            raise ValueError("b_density_form expects a two-form from this chart")
        if beta.degree != 2:
            raise ValueError("b_density_form expects a degree-two form")
        components = self._arena._chart_b_form(self, beta, orientation, True)
        return self.vector(components, density_weight=1)

    def h_cov(self, reluctivity, vector):
        """Return covariant ``H_i = nu_ij*B^j`` components."""
        return self._arena._chart_h_cov(self, reluctivity, vector)

    def h_con(self, covariant):
        """Raise covariant ``H_i`` components to ``H^i`` with the metric."""
        return self._arena._chart_many(
            self._arena._lib.chart_h_con,
            self.coordinates, self.position, covariant,
        )

    def magnetic_field(self, potential):
        """Return typed ``B^i``, ``B_i``, and ``sqrtg B^i`` views."""
        upper = self.curl(potential)
        return MagneticField(
            self, upper, self.b_cov(upper), self.b_density(upper)
        )

    def metric_owner(self, components, signature=(1, 1, 1), orientation=1):
        """Create a native metric owner for this chart's expression arena."""
        return Metric(self, components, signature, orientation)

    def connection(self, coefficients=None, convention=CONNECTION_STANDARD):
        """Create the chart Levi-Civita or a supplied affine connection."""
        return Connection(self, coefficients, convention)

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

    def j_fourier(self, reluctivity, potential, mode):
        reluctivity = _matrix3_values(reluctivity)
        return self._arena._chart_j_fourier(
            self, reluctivity, potential, mode,
        )

    def reluctivity_density(self, physical):
        """Return covariant weight-minus-one reluctivity density components."""
        scalar = isinstance(physical, (Expr, int, float, Fraction))
        components = self._arena._chart_reluctivity_density(
            self, physical, scalar,
        )
        symmetries = ((0, 1, SYMMETRIC),) if scalar else ()
        return Tensor(
            self, components, (-1, -1), density_weight=-1, _owned=True,
            _symmetries=symmetries,
        )

    def fourier_weak_form(self, reluctivity, mode):
        """Return native n=0 or n!=0 Fourier variational metadata."""
        return self._arena._chart_fourier_weak_form(
            self, reluctivity, mode,
        )

    def fourier_longitudinal_flux(self, reluctivity, potential, component):
        """Return component ``q_i = nubar[i,j] * d_j(A_3)``."""
        return self._arena._chart_fourier_longitudinal_flux(
            self, reluctivity, potential, component,
        )

    def fourier_transverse_flux(self, reluctivity, potential):
        """Return ``q = nu33 * curl_t(A_1, A_2)`` for the edge branch."""
        return self._arena._chart_fourier_transverse_flux(
            self, reluctivity, potential,
        )

    def fourier_longitudinal_boundary_flux(
            self, reluctivity, potential, normal):
        """Return ``n_i q_i`` for the scalar branch boundary term."""
        return self._arena._chart_fourier_longitudinal_boundary_flux(
            self, reluctivity, potential, normal,
        )

    def fourier_transverse_boundary_flux(
            self, reluctivity, potential, normal):
        """Return ``s_k q`` for the edge branch boundary term."""
        return self._arena._chart_fourier_transverse_boundary_flux(
            self, reluctivity, potential, normal,
        )

    def fourier_transverse_boundary_contraction(
            self, reluctivity, potential, normal, test):
        """Return ``w_k s_k q`` for the edge boundary term."""
        return self._arena._chart_fourier_transverse_boundary_contraction(
            self, reluctivity, potential, normal, test,
        )

    def fourier_longitudinal_residual(self, reluctivity, potential, current):
        """Return the n=0 scalar Fourier residual."""
        return self._arena._chart_fourier_longitudinal_residual(
            self, reluctivity, potential, current,
        )

    def fourier_transverse_residual(
            self, reluctivity, potential, current, mode):
        """Return the two-component Fourier residual for any mode."""
        return self._arena._chart_fourier_transverse_residual(
            self, reluctivity, potential, current, mode,
        )

    def current_compatibility(self, current, mode):
        """Return the native metric-free Fourier current divergence."""
        return self._arena._chart_current_compatibility(self, current, mode)


class FluxSurface:
    """A coordinate flux surface with native periodic averaging."""

    def __init__(self, chart, label_index=1):
        if not isinstance(chart, Chart):
            raise TypeError("FluxSurface requires a fortsym Chart")
        label_index = int(label_index)
        if label_index not in (1, 2, 3):
            raise ValueError("flux-surface label index must be 1, 2, or 3")
        self.chart = chart
        self.label_index = label_index
        self.angle_indices = tuple(
            index for index in (1, 2, 3) if index != label_index
        )

    @property
    def label(self):
        return self.chart.coordinates[self.label_index - 1]

    def measure(self):
        return self.chart.surface_measure(self.label_index)

    def average(self, scalar):
        """Average over both angles on ``[0, 2*pi]``.

        Native definite integration verifies the numerator and normalization;
        unsupported symbolic integrands raise ``FortSymError`` explicitly.
        """
        scalar = self.chart._arena._check(scalar)
        return self.chart._arena._chart_flux_surface_average(
            self.chart.coordinates, self.chart.position, self.label_index,
            scalar,
        )


class FluxCoordinates:
    """Native residual checks for a labelled flux-coordinate chart."""

    def __init__(self, chart, label_index=1, kind=FLUX_GENERIC):
        if not isinstance(chart, Chart):
            raise TypeError("FluxCoordinates requires a fortsym Chart")
        label_index = int(label_index)
        kind = int(kind)
        if label_index not in (1, 2, 3):
            raise ValueError("flux-coordinate label index must be 1, 2, or 3")
        if kind not in (
                FLUX_GENERIC, FLUX_CLEBSCH, FLUX_STRAIGHT_FIELD_LINE,
                FLUX_BOOZER, FLUX_HAMADA):
            raise ValueError("unknown flux-coordinate kind")
        self.chart = chart
        self.surface = chart.flux_surface(label_index)
        self.label_index = label_index
        self.angle_indices = self.surface.angle_indices
        self.kind = kind

    @property
    def label(self):
        return self.surface.label

    @property
    def kind_name(self):
        return {
            FLUX_GENERIC: "generic",
            FLUX_CLEBSCH: "clebsch",
            FLUX_STRAIGHT_FIELD_LINE: "straight_field_line",
            FLUX_BOOZER: "boozer",
            FLUX_HAMADA: "hamada",
        }[self.kind]

    def normal(self, vector):
        """Return the native residual ``B^label``."""
        if isinstance(vector, Tensor):
            if vector.chart is not self.chart or vector.rank != 1:
                raise ValueError("flux normal expects a vector on this chart")
            if vector.variance != (1,):
                raise ValueError("flux normal expects contravariant components")
        return self.chart._arena._chart_flux_scalar(
            self.chart._arena._lib.chart_flux_normal_residual,
            self.chart, vector, self.label_index,
        )

    normal_residual = normal

    def straight_field_residual(self, vector, rotational_transform):
        """Return ``B**angle_one - iota*B**angle_two``."""
        if isinstance(vector, Tensor):
            if vector.chart is not self.chart or vector.rank != 1:
                raise ValueError("straight-field residual expects a vector on this chart")
            if vector.variance != (1,):
                raise ValueError("straight-field residual expects contravariant components")
        return self.chart._arena._chart_flux_scalar(
            self.chart._arena._lib.chart_straight_field_line_residual,
            self.chart, vector, self.label_index, rotational_transform,
        )

    def clebsch_residuals(self, vector, alpha, beta):
        """Return ``B - grad(alpha) cross grad(beta)`` components."""
        if self.kind != FLUX_CLEBSCH:
            raise ValueError("Clebsch residuals require kind=FLUX_CLEBSCH")
        if isinstance(vector, Tensor):
            if vector.chart is not self.chart or vector.rank != 1:
                raise ValueError("Clebsch residuals expect a vector on this chart")
            if vector.variance != (1,):
                raise ValueError("Clebsch residuals expect contravariant components")
        return self.chart._arena._chart_clebsch_residuals(
            self.chart, vector, alpha, beta, self.label_index,
        )

    def clebsch_valid(self, vector, alpha, beta):
        """Return True/False/None using the native zero oracle."""
        verdicts = [
            value.is_zero for value in self.clebsch_residuals(
                vector, alpha, beta
            )
        ]
        if any(value is False for value in verdicts):
            return False
        if any(value is None for value in verdicts):
            return None
        return True

    def boozer_residuals(self, covariant):
        """Return ``(B_label, d1 B1, d2 B1, d1 B2, d2 B2)``."""
        if self.kind != FLUX_BOOZER:
            raise ValueError("Boozer residuals require kind=FLUX_BOOZER")
        if isinstance(covariant, Tensor):
            if covariant.chart is not self.chart or covariant.rank != 1:
                raise ValueError("Boozer residuals expect a covector on this chart")
            if covariant.variance != (-1,):
                raise ValueError("Boozer residuals expect covariant components")
        return self.chart._arena._chart_flux_array(
            self.chart._arena._lib.chart_boozer_residuals,
            self.chart, covariant, self.label_index, BOOZER_RESIDUAL_COUNT,
        )

    def boozer_valid(self, covariant):
        """Return True/False/None using the native zero oracle."""
        verdicts = [value.is_zero for value in self.boozer_residuals(covariant)]
        if any(value is False for value in verdicts):
            return False
        if any(value is None for value in verdicts):
            return None
        return True

    def hamada_residuals(self, vector):
        """Return ``(B_label, d1 B1, d2 B1, d1 B2, d2 B2)``."""
        if self.kind != FLUX_HAMADA:
            raise ValueError("Hamada residuals require kind=FLUX_HAMADA")
        if isinstance(vector, Tensor):
            if vector.chart is not self.chart or vector.rank != 1:
                raise ValueError("Hamada residuals expect a vector on this chart")
            if vector.variance != (1,):
                raise ValueError("Hamada residuals expect contravariant components")
        return self.chart._arena._chart_flux_array(
            self.chart._arena._lib.chart_hamada_residuals,
            self.chart, vector, self.label_index, HAMADA_RESIDUAL_COUNT,
        )

    def hamada_valid(self, vector):
        """Return True/False/None using the native zero oracle."""
        verdicts = [value.is_zero for value in self.hamada_residuals(vector)]
        if any(value is False for value in verdicts):
            return False
        if any(value is None for value in verdicts):
            return None
        return True


class MagneticChart:
    """Typed magnetic-coordinate facade over one native chart owner."""

    def __init__(self, chart, potential, label_index=1):
        if not isinstance(chart, Chart):
            raise TypeError("MagneticChart requires a fortsym Chart")
        self.chart = chart
        self.surface = chart.flux_surface(label_index)
        self.potential = tuple(chart._arena._check(value) for value in potential)
        if len(self.potential) != 3:
            raise ValueError("magnetic potentials require three components")
        self.field = chart.magnetic_field(self.potential)

    @property
    def label(self):
        return self.surface.label

    @property
    def upper(self):
        return self.field.upper

    @property
    def lower(self):
        return self.field.lower

    @property
    def density(self):
        return self.field.density

    def measure(self):
        return self.surface.measure()

    def average(self, scalar):
        return self.surface.average(scalar)

    def potential_form(self):
        """Return the stored covariant potential one-form ``A``."""
        return self.chart.one_form(self.potential)

    def flux_form(self):
        """Return the magnetic flux two-form ``beta = d(A)``."""
        return self.potential_form().d()

    def divergence(self):
        """Return the chart divergence of the contravariant B view."""
        return self.chart.divergence(self.upper)

    def field_line_derivative(self, scalar):
        return self.field.field_line_derivative(scalar)

    def h_cov(self, reluctivity):
        return self.field.h_cov(reluctivity)

    def h_con(self, reluctivity):
        return self.field.h_con(reluctivity)


class MagneticField:
    """Typed magnetic views sharing one chart and native components."""

    def __init__(self, chart, upper, lower, density):
        self.chart = chart
        self.upper = Tensor(chart, upper, (1,))
        self.lower = Tensor(chart, lower, (-1,))
        self.density = Tensor(chart, density, (1,), 1)

    def h_cov(self, reluctivity):
        """Return covariant field intensity from this field's ``B^i``."""
        components = self.chart.h_cov(reluctivity, self.upper)
        return Tensor(self.chart, components, (-1,), _owned=True)

    def h_con(self, reluctivity):
        """Return contravariant field intensity from this field's ``B^i``."""
        covariant = self.h_cov(reluctivity)
        components = self.chart.h_con(covariant)
        return Tensor(self.chart, components, (1,), _owned=True)

    def field_line_derivative(self, scalar):
        """Return the derivative of a scalar along this magnetic field."""
        return self.chart.field_line_derivative(self.upper, scalar)


class Metric:
    """Explicit native metric metadata and operations for one chart arena."""

    def __init__(self, chart, components, signature=(1, 1, 1), orientation=1):
        if not isinstance(chart, Chart):
            raise TypeError("Metric requires a fortsym Chart")
        if isinstance(signature, Signature):
            signature = signature.values
        if isinstance(orientation, Orientation):
            orientation = orientation.value
        if len(signature) != 3 or any(int(value) not in (-1, 1)
                                      for value in signature):
            raise ValueError("metric signature requires three entries of +/-1")
        if int(orientation) not in (-1, 1):
            raise ValueError("metric orientation must be 1 or -1")
        values = _matrix3_values(components)
        self.chart = chart
        self._arena = chart._arena
        component_values = []
        temporaries = []
        for value in values:
            coerced, temporary = self._arena._coerce(value)
            component_values.append(coerced)
            if temporary is not None:
                temporaries.append(temporary)
        self.components = tuple(component_values)
        self._temporaries = tuple(temporaries)
        self.signature = tuple(int(value) for value in signature)
        self.orientation = int(orientation)

    @property
    def signature_type(self):
        return Signature(self.signature)

    @property
    def orientation_type(self):
        return Orientation(self.orientation)

    def sqrtg(self):
        return self._arena._metric_sqrtg(self)

    def volume_density(self):
        return self._arena._metric_volume_density(self)

    def surface_measure(self, normal_index=1):
        """Return the positive induced measure on a coordinate surface."""
        return self._arena._metric_surface_measure(self, normal_index)

    def levi_civita(self, variance="covariant"):
        if variance in ("covariant", "lower", -1):
            value = -1
        elif variance in ("contravariant", "upper", 1):
            value = 1
        else:
            raise ValueError(
                "Levi-Civita variance must be covariant or contravariant"
            )
        components = self._arena._metric_levi_civita(self, value)
        symmetries = tuple((first, second, ANTISYMMETRIC)
                           for first in range(3)
                           for second in range(first + 1, 3))
        return Tensor(
            self.chart, components, (value, value, value), _owned=True,
            _symmetries=symmetries,
        )

    def contravariant(self):
        components = self._arena._metric_contravariant(self)
        return Tensor(
            self.chart, components, (-1, -1), _owned=True,
            _symmetries=((0, 1, SYMMETRIC),),
        )

    def inner(self, left, right):
        """Return ``g_ij*left**i*right**j`` for contravariant components."""
        return self._arena._metric_inner(self, left, right)

    def norm_squared(self, vector):
        """Return the metric contraction of a vector with itself."""
        return self.inner(vector, vector)

    def grad(self, scalar):
        """Return contravariant ``g^ij*diff(scalar, coordinates[j])``."""
        return self._arena._metric_scalar_operation(
            self._arena._lib.metric_grad, self, scalar, 3
        )

    def divergence(self, vector):
        """Return the metric divergence of a contravariant vector."""
        return self._arena._metric_vector_operation(
            self._arena._lib.metric_divergence, self, vector
        )

    def laplacian(self, scalar):
        """Return the metric Laplace--Beltrami operator on a scalar."""
        return self._arena._metric_scalar_operation(
            self._arena._lib.metric_laplacian, self, scalar, 1
        )

    def hodge_star(self, form):
        if not isinstance(form, Form) or form.chart is not self.chart:
            raise ValueError("metric Hodge star requires a form on its chart")
        components = self._arena._metric_form_star(self, form)
        return Form(self.chart, components, 3 - form.degree, _owned=True)

    hodge = hodge_star

    def killing(self, vector):
        """Return the Killing residual ``L_vector(g)`` for this metric."""
        if not isinstance(vector, Tensor) or vector.chart is not self.chart:
            raise ValueError("killing expects a vector on this metric's chart")
        if vector.rank != 1 or vector.variance != (1,) or vector.density_weight != 0:
            raise ValueError("killing expects a weight-zero contravariant vector")
        metric_tensor = Tensor(
            self.chart, self.components, (-1, -1), _owned=False,
            _symmetries=((0, 1, SYMMETRIC),),
        )
        return self.chart.lie(vector, metric_tensor)

    def close(self):
        for temporary in self._temporaries:
            temporary.close()
        self._temporaries = ()
        self.components = ()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class SpacetimeTensor:
    """A small native-backed tensor view for the dimension-aware owner."""

    def __init__(self, metric, components, shape, variance=None,
                 density_weight=0, _owned=True):
        if not isinstance(metric, SpacetimeMetric):
            raise TypeError("SpacetimeTensor requires a SpacetimeMetric")
        self.metric = metric
        self._arena = metric._arena
        self.components = tuple(components)
        self.shape = tuple(int(value) for value in shape)
        expected = SPACETIME_DIM ** len(self.shape)
        if len(self.components) != expected:
            raise ValueError("spacetime tensor component count is inconsistent")
        if variance is not None:
            variance = tuple(int(value) for value in variance)
            if len(variance) != len(self.shape) or any(
                    value not in (-1, 1) for value in variance):
                raise ValueError("spacetime tensor variance does not match rank")
        self.variance = variance
        self.density_weight = int(density_weight)
        self._owned = bool(_owned)
        self._temporaries = ()

    @property
    def rank(self):
        return len(self.shape)

    def component(self, *indices):
        if len(indices) != len(self.shape):
            raise IndexError("spacetime tensor rank does not match")
        flat = 0
        scale = 1
        for index in indices:
            index = int(index)
            if index < 0 or index >= SPACETIME_DIM:
                raise IndexError("spacetime tensor index is outside dimension four")
            flat += index * scale
            scale *= SPACETIME_DIM
        value = self.components[flat]
        value._spacetime_owner = self
        return value

    def __getitem__(self, indices):
        if not isinstance(indices, tuple):
            indices = (indices,)
        return self.component(*indices)

    def __len__(self):
        return len(self.components)

    def __iter__(self):
        for value in self.components:
            value._spacetime_owner = self
            yield value

    def raise_(self, slot=0):
        """Raise one covariant slot with this tensor's metric."""
        if self.variance is None or self.variance[int(slot)] != -1:
            raise ValueError("raise_ requires a covariant spacetime slot")
        components = self._arena._spacetime_tensor_metric_operation(
            self._arena._lib.spacetime_tensor_raise, self.metric, self, slot
        )
        variance = list(self.variance)
        variance[int(slot)] = 1
        return SpacetimeTensor(
            self.metric, components, self.shape, variance,
            self.density_weight, _owned=True,
        )

    def lower(self, slot=0):
        """Lower one contravariant slot with this tensor's metric."""
        if self.variance is None or self.variance[int(slot)] != 1:
            raise ValueError("lower requires a contravariant spacetime slot")
        components = self._arena._spacetime_tensor_metric_operation(
            self._arena._lib.spacetime_tensor_lower, self.metric, self, slot
        )
        variance = list(self.variance)
        variance[int(slot)] = -1
        return SpacetimeTensor(
            self.metric, components, self.shape, variance,
            self.density_weight, _owned=True,
        )

    def density(self, factor_or_weight):
        """Return a density view, optionally multiplying by a factor."""
        if isinstance(factor_or_weight, Expr):
            factor = self._arena._check(factor_or_weight)
            components = self._arena._spacetime_tensor_density_factor(
                self.metric, self, factor
            )
            return SpacetimeTensor(
                self.metric, components, self.shape, self.variance,
                self.density_weight + 1, _owned=True,
            )
        return SpacetimeTensor(
            self.metric, self.components, self.shape, self.variance,
            int(factor_or_weight), _owned=False,
        )

    with_density = density

    def permute(self, order):
        """Return this tensor with slots reordered by zero-based slot order."""
        if self.variance is None:
            raise ValueError("spacetime tensor permutation needs slot variance")
        order = tuple(int(value) for value in order)
        if len(order) != self.rank or set(order) != set(range(self.rank)):
            raise ValueError(
                "spacetime tensor permutation must contain every slot once"
            )
        components = self._arena._spacetime_tensor_permute(
            self.metric, self, order
        )
        variance = tuple(self.variance[index] for index in order)
        return SpacetimeTensor(
            self.metric, components, self.shape, variance,
            self.density_weight, _owned=True,
        )

    def contract(self, first, second):
        """Contract two opposite-variance slots using zero-based slots."""
        if self.variance is None:
            raise ValueError("spacetime tensor contraction needs slot variance")
        first, second = int(first), int(second)
        if first < 0 or first >= self.rank:
            raise IndexError("first contraction slot is outside the tensor rank")
        if second < 0 or second >= self.rank:
            raise IndexError("second contraction slot is outside the tensor rank")
        if first == second:
            raise ValueError("tensor contraction needs two distinct slots")
        if self.variance[first] == self.variance[second]:
            raise ValueError("tensor contraction needs opposite-variance slots")
        components = self._arena._spacetime_tensor_contract(
            self.metric, self, first, second
        )
        variance = tuple(
            value for index, value in enumerate(self.variance)
            if index not in (first, second)
        )
        return SpacetimeTensor(
            self.metric, components, (4,) * (self.rank - 2), variance,
            self.density_weight, _owned=True,
        )

    trace = contract

    def product(self, other):
        """Return the native tensor product, with left slots first."""
        if not isinstance(other, SpacetimeTensor) or other.metric is not self.metric:
            raise ValueError("tensor factors must belong to the same metric")
        if self.variance is None or other.variance is None:
            raise ValueError("spacetime tensor product needs slot variance")
        output_rank = self.rank + other.rank
        if output_rank > SPACETIME_TENSOR_MAX_RANK:
            raise ValueError("native spacetime tensors support rank at most five")
        components = self._arena._spacetime_tensor_product(
            self.metric, self, other
        )
        return SpacetimeTensor(
            self.metric, components, (4,) * output_rank,
            self.variance + other.variance,
            self.density_weight + other.density_weight, _owned=True,
        )

    tensor_product = product

    def covariant_diff(self):
        """Append the lower derivative slot using the metric connection."""
        components = self._arena._spacetime_tensor_covariant_diff(
            self.metric, self
        )
        return SpacetimeTensor(
            self.metric, components, (4,) * (self.rank + 1),
            self.variance + (-1,), self.density_weight, _owned=True,
        )

    covariant_derivative = covariant_diff

    def covariant_divergence(self):
        """Contract the first upper slot with the covariant derivative."""
        components = self._arena._spacetime_tensor_covariant_divergence(
            self.metric, self
        )
        variance = self.variance[1:]
        return SpacetimeTensor(
            self.metric, components, (4,) * (self.rank - 1), variance,
            self.density_weight, _owned=True,
        )

    divergence = covariant_divergence

    def lie(self, vector):
        """Return the coordinate Lie derivative along a vector field."""
        components = self._arena._spacetime_tensor_lie(
            self.metric, vector, self
        )
        return SpacetimeTensor(
            self.metric, components, self.shape, self.variance,
            self.density_weight, _owned=True,
        )

    lie_derivative = lie

    def close(self):
        if self._owned:
            for value in self.components:
                value.close()
        for temporary in self._temporaries:
            temporary.close()
        self.components = ()
        self._temporaries = ()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class SpacetimeMetric:
    """Dimension-aware native metric owner, currently dimensions one through four."""

    def __init__(self, coordinates, components, dimension=4,
                 signature=(-1, 1, 1, 1), orientation=1):
        self.coordinates = tuple(coordinates)
        self.dimension = int(dimension)
        if len(self.coordinates) != SPACETIME_DIM:
            raise ValueError("spacetime metrics require four coordinates")
        if self.dimension < 1 or self.dimension > SPACETIME_DIM:
            raise ValueError("spacetime dimension must be between one and four")
        if isinstance(signature, Signature):
            signature = signature.values
        if isinstance(orientation, Orientation):
            orientation = orientation.value
        if len(signature) != SPACETIME_DIM or any(
                int(value) not in (-1, 1) for value in signature):
            raise ValueError("spacetime signature requires four +/-1 entries")
        if int(orientation) not in (-1, 1):
            raise ValueError("spacetime orientation must be 1 or -1")
        self._arena = self.coordinates[0]._arena
        self.coordinates = tuple(self._arena._check(value)
                                 for value in self.coordinates)
        values = _matrix4_values(components)
        component_values = []
        temporaries = []
        for value in values:
            coerced, temporary = self._arena._coerce(value)
            component_values.append(coerced)
            if temporary is not None:
                temporaries.append(temporary)
        self.components = tuple(component_values)
        self._temporaries = tuple(temporaries)
        self.signature = tuple(int(value) for value in signature)
        self.orientation = int(orientation)

    @property
    def signature_type(self):
        return Signature(self.signature)

    @property
    def orientation_type(self):
        return Orientation(self.orientation)

    def sqrtg(self):
        return self._arena._spacetime_scalar(
            self._arena._lib.spacetime_metric_sqrtg, self
        )

    def contravariant(self):
        return SpacetimeTensor(
            self,
            self._arena._spacetime_array(
                self._arena._lib.spacetime_metric_contravariant, self, 16
            ),
            (4, 4),
            variance=(1, 1),
            _owned=True,
        )

    def covariant(self):
        return SpacetimeTensor(
            self, self.components, (4, 4), variance=(-1, -1), _owned=False
        )

    def vector(self, values, density_weight=0):
        values = tuple(values)
        if len(values) != SPACETIME_DIM:
            raise ValueError("spacetime vectors require four components")
        coerced = []
        temporaries = []
        for value in values:
            expression, temporary = self._arena._coerce(value)
            coerced.append(expression)
            if temporary is not None:
                temporaries.append(temporary)
        result = SpacetimeTensor(
            self, coerced, (4,), variance=(1,),
            density_weight=density_weight, _owned=False,
        )
        result._temporaries = tuple(temporaries)
        return result

    def covector(self, values, density_weight=0):
        values = tuple(values)
        if len(values) != SPACETIME_DIM:
            raise ValueError("spacetime covectors require four components")
        coerced = []
        temporaries = []
        for value in values:
            expression, temporary = self._arena._coerce(value)
            coerced.append(expression)
            if temporary is not None:
                temporaries.append(temporary)
        result = SpacetimeTensor(
            self, coerced, (4,), variance=(-1,),
            density_weight=density_weight, _owned=False,
        )
        result._temporaries = tuple(temporaries)
        return result

    def scalar(self, value, density_weight=0):
        """Create a typed spacetime scalar or scalar density."""
        expression, temporary = self._arena._coerce(value)
        result = SpacetimeTensor(
            self, (expression,), (), variance=(),
            density_weight=density_weight, _owned=False,
        )
        if temporary is not None:
            result._temporaries = (temporary,)
        return result

    def flat(self, vector):
        """Lower a contravariant vector and return its one-form view."""
        values = self._arena._spacetime_vector_array(
            self._arena._lib.spacetime_metric_flat, self, vector
        )
        return SpacetimeForm(self, values, 1, _owned=True)

    def sharp(self, covector):
        """Raise a one-form and return its contravariant tensor view."""
        if not isinstance(covector, SpacetimeForm) or covector.metric is not self:
            raise ValueError("sharp requires a one-form on this metric")
        if covector.degree != 1:
            raise ValueError("sharp requires a one-form")
        values = tuple(covector[1 << index] for index in range(SPACETIME_DIM))
        components = self._arena._spacetime_vector_array(
            self._arena._lib.spacetime_metric_sharp, self, values
        )
        return SpacetimeTensor(
            self, components, (4,), variance=(1,), _owned=True
        )

    def grad(self, scalar):
        """Return the contravariant metric gradient of a scalar."""
        components = self._arena._spacetime_scalar_array(
            self._arena._lib.spacetime_metric_grad, self, scalar
        )
        return SpacetimeTensor(
            self, components, (4,), variance=(1,), _owned=True
        )

    def divergence(self, vector):
        """Return the metric divergence of a contravariant vector."""
        return self._arena._spacetime_vector_scalar(
            self._arena._lib.spacetime_metric_divergence, self, vector
        )

    def lie(self, vector, tensor):
        """Return the Lie derivative of a tensor along a vector field."""
        if not isinstance(tensor, SpacetimeTensor) or tensor.metric is not self:
            raise ValueError("lie expects a tensor from this metric")
        return tensor.lie(vector)

    lie_derivative = lie

    def killing(self, vector):
        """Return the metric Killing residual ``L_vector(g)``."""
        return self.covariant().lie(vector)

    def laplacian(self, scalar):
        """Return the metric Laplace--Beltrami or wave operator."""
        return self._arena._spacetime_scalar_scalar(
            self._arena._lib.spacetime_metric_laplacian, self, scalar
        )

    def geodesic_residual(self, curve, parameter):
        values = tuple(curve)
        if len(values) != SPACETIME_DIM:
            raise ValueError("geodesic curves require four components")
        coerced = []
        temporaries = []
        try:
            for value in values:
                expression, temporary = self._arena._coerce(value)
                coerced.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            parameter_value, temporary = self._arena._coerce(parameter)
            if temporary is not None:
                temporaries.append(temporary)
            return SpacetimeTensor(
                self,
                self._arena._spacetime_geodesic_residual(
                    self, tuple(coerced), parameter_value
                ),
                (4,),
                variance=(1,),
                _owned=True,
            )
        finally:
            for temporary in temporaries:
                temporary.close()

    def christoffel(self):
        return SpacetimeTensor(
            self,
            self._arena._spacetime_array(
                self._arena._lib.spacetime_christoffel, self, 64
            ),
            (4, 4, 4),
            variance=(1, -1, -1),
            _owned=True,
        )

    def riemann(self):
        return SpacetimeTensor(
            self,
            self._arena._spacetime_array(
                self._arena._lib.spacetime_riemann, self, 256
            ),
            (4, 4, 4, 4),
            variance=(1, -1, -1, -1),
            _owned=True,
        )

    def ricci(self):
        return SpacetimeTensor(
            self,
            self._arena._spacetime_array(
                self._arena._lib.spacetime_ricci, self, 16
            ),
            (4, 4),
            variance=(-1, -1),
            _owned=True,
        )

    def scalar_curvature(self):
        return self._arena._spacetime_scalar(
            self._arena._lib.spacetime_scalar_curvature, self
        )

    def einstein(self):
        return SpacetimeTensor(
            self,
            self._arena._spacetime_array(
                self._arena._lib.spacetime_einstein, self, 16
            ),
            (4, 4),
            variance=(-1, -1),
            _owned=True,
        )

    def one_form(self, values):
        return SpacetimeForm(self, values, 1)

    def two_form(self, values):
        return SpacetimeForm(self, values, 2)

    def three_form(self, values):
        return SpacetimeForm(self, values, 3)

    def four_form(self, value):
        return SpacetimeForm(self, (value,), 4)

    def scalar_form(self, value):
        return SpacetimeForm(self, value, 0)

    def close(self):
        for value in self._temporaries:
            value.close()
        self._temporaries = ()
        self.components = ()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class SpacetimeForm:
    """Native degree-k form on a :class:`SpacetimeMetric`."""

    def __init__(self, metric, components, degree, _owned=False):
        if not isinstance(metric, SpacetimeMetric):
            raise TypeError("SpacetimeForm requires a SpacetimeMetric")
        self.metric = metric
        self._arena = metric._arena
        self.degree = int(degree)
        if self.degree < 0 or self.degree > 4:
            raise ValueError("spacetime forms require degrees zero through four")
        values = self._normalise_components(components, self.degree)
        component_values = []
        temporaries = []
        borrowed_components = set()
        for value in values:
            coerced, temporary = self._arena._coerce(value)
            component_values.append(coerced)
            if temporary is not None:
                temporaries.append(temporary)
            elif any(coerced is cached for cached in self._arena._integer_cache.values()):
                borrowed_components.add(id(coerced))
        self.components = tuple(component_values)
        self._temporaries = tuple(temporaries)
        self._borrowed_components = borrowed_components
        self._owned = bool(_owned)

    @staticmethod
    def _normalise_components(components, degree):
        values = (components,) if degree == 0 and isinstance(
            components, (Expr, int, float, Fraction)
        ) else tuple(components)
        if len(values) == 16:
            return values
        if degree == 1 and len(values) == 4:
            result = [0] * 16
            for index, mask in enumerate((1, 2, 4, 8)):
                result[mask] = values[index]
            return tuple(result)
        if degree == 2 and len(values) == 6:
            result = [0] * 16
            index = 0
            for mask in range(16):
                if mask.bit_count() == 2:
                    result[mask] = values[index]
                    index += 1
            return tuple(result)
        if degree == 3 and len(values) == 4:
            result = [0] * 16
            index = 0
            for mask in range(16):
                if mask.bit_count() == 3:
                    result[mask] = values[index]
                    index += 1
            return tuple(result)
        if degree == 4 and len(values) == 1:
            result = [0] * 16
            result[15] = values[0]
            return tuple(result)
        if degree == 0 and len(values) == 1:
            return (values[0],) + (0,) * 15
        raise ValueError("spacetime form coefficients do not match degree")

    def component(self, mask):
        mask = int(mask)
        if mask < 0 or mask >= 16:
            raise IndexError("spacetime form mask is outside dimension four")
        value = self.components[mask]
        value._spacetime_form_owner = self
        return value

    def __getitem__(self, mask):
        return self.component(mask)

    def __len__(self):
        return 16

    def __iter__(self):
        for mask in range(16):
            yield self.component(mask)

    def d(self):
        components, degree = self._arena._spacetime_form_unary(
            self._arena._lib.spacetime_form_d, self.metric, self,
            self.degree + 1,
        )
        return SpacetimeForm(self.metric, components, degree, _owned=True)

    exterior_diff = d

    @property
    def is_closed(self):
        verdict = self._arena._spacetime_form_closed(self.metric, self)
        return True if verdict == 1 else False if verdict == 2 else None

    def field_strength(self):
        if self.degree != 1:
            raise ValueError("field_strength requires a potential one-form")
        components = self._arena._spacetime_field_strength(self.metric, self)
        return SpacetimeForm(self.metric, components, 2, _owned=True)

    def gauge_transform(self, chi):
        expression, temporary = self._arena._coerce(chi)
        try:
            components = self._arena._spacetime_gauge_transform(
                self.metric, self, expression
            )
            return SpacetimeForm(self.metric, components, 1, _owned=True)
        finally:
            if temporary is not None:
                temporary.close()

    def maxwell_residual(self, current):
        if self.degree != 1:
            raise ValueError("maxwell_residual requires a potential one-form")
        if not isinstance(current, SpacetimeForm) or current.metric is not self.metric:
            raise ValueError("maxwell_residual requires a current on this metric")
        if current.degree != 3:
            raise ValueError("Maxwell current must be a three-form")
        components = self._arena._spacetime_maxwell_residual(
            self.metric, self, current
        )
        return SpacetimeForm(self.metric, components, 3, _owned=True)

    def star(self):
        components, degree = self._arena._spacetime_form_unary(
            self._arena._lib.spacetime_form_star, self.metric, self,
            self.metric.dimension - self.degree,
        )
        return SpacetimeForm(self.metric, components, degree, _owned=True)

    hodge_star = star

    def codifferential(self):
        components, degree = self._arena._spacetime_form_unary(
            self._arena._lib.spacetime_form_codifferential, self.metric, self,
            self.degree - 1,
        )
        return SpacetimeForm(self.metric, components, degree, _owned=True)

    codiff = codifferential

    def laplace_de_rham(self):
        components, degree = self._arena._spacetime_form_unary(
            self._arena._lib.spacetime_form_laplace_de_rham, self.metric, self,
            self.degree,
        )
        return SpacetimeForm(self.metric, components, degree, _owned=True)

    def _vector_operation(self, operation, vector, output_degree):
        values = tuple(vector)
        if len(values) != SPACETIME_DIM:
            raise ValueError("spacetime vectors require four components")
        coerced = []
        temporaries = []
        try:
            for value in values:
                expression, temporary = self._arena._coerce(value)
                coerced.append(expression)
                if temporary is not None:
                    temporaries.append(temporary)
            components, degree = self._arena._spacetime_form_vector_operation(
                operation, self.metric, tuple(coerced), self, output_degree
            )
            return SpacetimeForm(self.metric, components, degree, _owned=True)
        finally:
            for temporary in temporaries:
                temporary.close()

    def interior(self, vector):
        return self._vector_operation(
            self._arena._lib.spacetime_form_interior, vector, self.degree - 1
        )

    interior_product = interior

    def lie(self, vector):
        return self._vector_operation(
            self._arena._lib.spacetime_form_lie, vector, self.degree
        )

    lie_derivative = lie

    def wedge(self, other):
        if not isinstance(other, SpacetimeForm) or other.metric is not self.metric:
            raise ValueError("spacetime wedge requires forms on one metric")
        components, degree = self._arena._spacetime_form_binary(
            self._arena._lib.spacetime_form_wedge, self.metric, self, other
        )
        return SpacetimeForm(self.metric, components, degree, _owned=True)

    def close(self):
        if self._owned:
            for value in self.components:
                if id(value) not in self._borrowed_components:
                    value.close()
        for temporary in self._temporaries:
            temporary.close()
        self.components = ()
        self._temporaries = ()
        self._borrowed_components = set()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class FourierWeakForm:
    """Native metadata and coefficient blocks for one Fourier FEM branch."""

    def __init__(self, chart, mode, branch, trial_space, test_space,
                 trial_components, test_components, boundary_trace,
                 longitudinal_diffusion, transverse_curl_coefficient, mass):
        self.chart = chart
        self.mode = mode
        self.branch = branch
        self.trial_space = trial_space
        self.test_space = test_space
        self.trial_components = trial_components
        self.test_components = test_components
        self.boundary_trace = boundary_trace
        self.longitudinal_diffusion = (
            (longitudinal_diffusion[0], longitudinal_diffusion[2]),
            (longitudinal_diffusion[1], longitudinal_diffusion[3]),
        )
        self.transverse_curl_coefficient = transverse_curl_coefficient
        self.transverse_mass = (
            (mass[0], mass[2]),
            (mass[1], mass[3]),
        )
        self.requires_current_compatibility = True

    @property
    def branch_name(self):
        return {
            FOURIER_LONGITUDINAL: "longitudinal",
            FOURIER_TRANSVERSE: "transverse",
        }.get(self.branch, "invalid")

    @property
    def has_gradient_term(self):
        return self.branch == FOURIER_LONGITUDINAL

    @property
    def has_curl_term(self):
        return self.branch == FOURIER_TRANSVERSE

    @property
    def has_mass_term(self):
        return self.branch == FOURIER_TRANSVERSE


class ChartMap:
    """A bidirectional coordinate transition owned by the native chart layer.

    ``forward`` gives target coordinates as functions of source coordinates.
    ``inverse`` gives source coordinates as functions of target coordinates.
    Tensor components are returned in the target coordinate symbols.
    """

    def __init__(self, source, target, forward, inverse):
        if not isinstance(source, Chart) or not isinstance(target, Chart):
            raise TypeError("ChartMap requires source and target Chart objects")
        if source._arena is not target._arena:
            raise ValueError("ChartMap source and target must share an arena")
        self.source = source
        self.target = target
        self._arena = source._arena
        self.forward = tuple(self._arena._check(value) for value in forward)
        self.inverse = tuple(self._arena._check(value) for value in inverse)
        if len(self.forward) != 3 or len(self.inverse) != 3:
            raise ValueError("ChartMap requires three forward and inverse coordinates")

    def jacobian(self):
        return self._arena._chart_map_matrix(
            self._arena._lib.chart_map_jacobian, self
        )

    @property
    def source_patch(self):
        """Declared patch owned by the source chart, if any."""
        return self.source.patch

    @property
    def target_patch(self):
        """Declared patch owned by the target chart, if any."""
        return self.target.patch

    def inverse_jacobian(self):
        return self._arena._chart_map_matrix(
            self._arena._lib.chart_map_inverse_jacobian, self
        )

    def compose(self, following):
        """Return the transition obtained by applying ``following`` next."""
        if not isinstance(following, ChartMap):
            raise TypeError("ChartMap.compose expects another ChartMap")
        if following.source is not self.target:
            raise ValueError("ChartMap.compose requires matching intermediate charts")
        if following._arena is not self._arena:
            raise ValueError("ChartMap.compose maps must share an arena")
        if ((self.target.patch is None) != (following.source.patch is None) or
                (self.target.patch is not None and
                 self.target.patch is not following.source.patch)):
            raise ValueError("ChartMap.compose requires matching intermediate patches")
        forward, inverse = self._arena._chart_map_compose(self, following)
        return ChartMap(self.source, following.target, forward, inverse)

    def transform(self, tensor):
        if isinstance(tensor, Tensor):
            if tensor.chart is not self.source:
                raise ValueError("ChartMap.transform expects a tensor from its source chart")
            components = self._arena._chart_map_tensor(self, tensor)
            return Tensor(
                self.target, components, tensor.variance, tensor.density_weight,
                _owned=True, _symmetries=tensor.symmetries,
            )
        if isinstance(tensor, Form):
            if tensor.chart is not self.source:
                raise ValueError("ChartMap.transform expects a form from its source chart")
            components = self._arena._chart_map_form(self, tensor)
            return Form(self.target, components, tensor.degree, _owned=True)
        raise TypeError("ChartMap.transform expects a Tensor or Form")

    def pullback(self, form):
        """Pull a differential form into the target coframe."""
        if not isinstance(form, Form):
            raise TypeError("ChartMap.pullback expects a Form")
        return self.transform(form)

    pushforward = transform


class Tensor:
    """A chart-bound native tensor view with explicit slot metadata.

    Components are flat in first-slot-fastest order, as in the Fortran tensor
    owner. Python component indices are zero-based and may be supplied through
    ``tensor[i, j]`` or ``tensor.component(i, j)``.
    """

    def __init__(self, chart, components, variance=(), density_weight=0,
                 _owned=False, _symmetries=()):
        if not isinstance(chart, Chart):
            raise TypeError("Tensor requires a fortsym Chart")
        self.chart = chart
        self._arena = chart._arena
        self.variance = tuple(int(value) for value in variance)
        self.rank = len(self.variance)
        if self.rank > 5:
            raise ValueError("native tensors support rank at most five")
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
        self._symmetries = _normalize_symmetries(self.rank, _symmetries)
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
        for component in self.components:
            component._tensor_owner = self
            yield component

    def symmetry(self, first, second):
        """Return the declared pair symmetry for two zero-based slots."""
        first, second = int(first), int(second)
        if first < 0 or first >= self.rank or second < 0 or second >= self.rank:
            raise IndexError("tensor symmetry slot is outside the tensor rank")
        if first == second:
            return SYMMETRY_NONE
        return self._symmetries.get(tuple(sorted((first, second))),
                                    SYMMETRY_NONE)

    @property
    def symmetries(self):
        """Return declared pairs as ``(first, second, kind)`` tuples."""
        return tuple((first, second, kind)
                     for (first, second), kind in sorted(self._symmetries.items()))

    def _copy_symmetries(self):
        return dict(self._symmetries)

    def declare_symmetry(self, first, second, kind):
        """Check and declare one same-variance slot pair natively."""
        first, second = int(first), int(second)
        kind = _symmetry_kind(kind)
        if kind == SYMMETRY_NONE:
            raise ValueError("declare_symmetry needs symmetric or antisymmetric")
        if first < 0 or first >= self.rank or second < 0 or second >= self.rank:
            raise IndexError("tensor symmetry slot is outside the tensor rank")
        if first == second:
            raise ValueError("tensor symmetry needs two distinct slots")
        components = self._arena._chart_tensor_declare_symmetry(
            self.chart, self, first, second, kind
        )
        symmetries = self._copy_symmetries()
        symmetries[tuple(sorted((first, second)))] = kind
        return Tensor(
            self.chart, components, self.variance, self.density_weight,
            _owned=True, _symmetries=symmetries,
        )

    def permute(self, order):
        """Return this tensor with slots reordered by zero-based slot order."""
        order = tuple(int(value) for value in order)
        if len(order) != self.rank or set(order) != set(range(self.rank)):
            raise ValueError("tensor permutation must contain every slot once")
        components = self._arena._chart_tensor_permute(self.chart, self, order)
        variance = tuple(self.variance[index] for index in order)
        positions = {source: target for target, source in enumerate(order)}
        symmetries = tuple((positions[first], positions[second], kind)
                           for (first, second), kind in self._symmetries.items())
        return Tensor(
            self.chart, components, variance, self.density_weight, _owned=True,
            _symmetries=symmetries,
        )

    def symmetrize(self, first, second):
        """Average two same-variance slots using zero-based slot numbers."""
        first, second = int(first), int(second)
        if first < 0 or first >= self.rank or second < 0 or second >= self.rank:
            raise IndexError("tensor symmetry slot is outside the tensor rank")
        components = self._arena._chart_tensor_symmetrize(
            self.chart, self, first, second, False
        )
        return Tensor(
            self.chart, components, self.variance, self.density_weight, _owned=True,
            _symmetries=((first, second, SYMMETRIC),),
        )

    def antisymmetrize(self, first, second):
        """Take the alternating part of two same-variance slots."""
        first, second = int(first), int(second)
        if first < 0 or first >= self.rank or second < 0 or second >= self.rank:
            raise IndexError("tensor antisymmetry slot is outside the tensor rank")
        components = self._arena._chart_tensor_symmetrize(
            self.chart, self, first, second, True
        )
        return Tensor(
            self.chart, components, self.variance, self.density_weight, _owned=True,
            _symmetries=((first, second, ANTISYMMETRIC),),
        )

    def contract(self, first, second):
        """Contract two opposite-variance slots.

        Integer slots are zero-based. ``Index`` arguments additionally check
        the named space, variance, and (when present) dummy label before the
        native contraction is called.
        """
        if isinstance(first, Index) or isinstance(second, Index):
            if not isinstance(first, Index) or not isinstance(second, Index):
                raise TypeError("typed contraction requires two Index values")
            if not first.compatible(second):
                raise ValueError("tensor indices are not a compatible dummy pair")
            if first.space.dimension != 3:
                raise ValueError("native chart tensors require three-dimensional indices")
            first_slot, second_slot = first.slot, second.slot
            if self.variance[first_slot] != first.variance:
                raise ValueError("first index variance does not match the tensor slot")
            if self.variance[second_slot] != second.variance:
                raise ValueError("second index variance does not match the tensor slot")
        else:
            first_slot, second_slot = int(first), int(second)
        if first_slot < 0 or first_slot >= self.rank:
            raise IndexError("first contraction slot is outside the tensor rank")
        if second_slot < 0 or second_slot >= self.rank:
            raise IndexError("second contraction slot is outside the tensor rank")
        if first_slot == second_slot:
            raise ValueError("tensor contraction needs two distinct slots")
        if self.variance[first_slot] == self.variance[second_slot]:
            raise ValueError("tensor contraction needs opposite-variance slots")
        components = self._arena._chart_tensor_contract(
            self.chart, self, first_slot, second_slot
        )
        variance = tuple(
            value for index, value in enumerate(self.variance)
            if index not in (first_slot, second_slot)
        )
        survivors = [index for index in range(self.rank)
                     if index not in (first_slot, second_slot)]
        positions = {source: target for target, source in enumerate(survivors)}
        symmetries = tuple((positions[first], positions[second], kind)
                           for (first, second), kind in self._symmetries.items()
                           if first in positions and second in positions)
        return Tensor(
            self.chart, components, variance, self.density_weight, _owned=True,
            _symmetries=symmetries,
        )

    def product(self, other):
        """Return the native tensor product, with left slots first."""
        if not isinstance(other, Tensor) or other.chart is not self.chart:
            raise ValueError("tensor factors must belong to the same chart")
        if self.rank + other.rank > 5:
            raise ValueError("native tensors support rank at most five")
        components = self._arena._chart_tensor_product(self.chart, self, other)
        symmetries = list(self.symmetries)
        symmetries.extend(
            (first + self.rank, second + self.rank, kind)
            for first, second, kind in other.symmetries
        )
        return Tensor(
            self.chart, components, self.variance + other.variance,
            self.density_weight + other.density_weight, _owned=True,
            _symmetries=symmetries,
        )

    tensor_product = product

    def to_form(self):
        """Convert this exact weight-zero lower antisymmetric tensor to a form."""
        return self.chart.form_from_tensor(self)

    def b_flux(self, orientation=1):
        """Return ``beta = i_B(orientation*Omega)`` for a contravariant vector."""
        if self.rank != 1 or self.variance != (1,) or self.density_weight != 0:
            raise ValueError("b_flux requires a weight-zero contravariant vector")
        return self.chart.b_flux_form(self, orientation)

    def trace(self, first, second):
        """Contract two opposite-variance slots using the short native name."""
        return self.contract(first, second)

    def covariant_diff(self, connection=None):
        if connection is None:
            return self.chart.covariant_diff(self)
        if not isinstance(connection, Connection) or connection.chart is not self.chart:
            raise ValueError("covariant_diff expects a connection from this chart")
        return connection.covariant_diff(self)

    covariant_derivative = covariant_diff

    def covariant_divergence(self, connection=None):
        if connection is None:
            return self.chart.covariant_divergence(self)
        if not isinstance(connection, Connection) or connection.chart is not self.chart:
            raise ValueError("covariant_divergence expects a connection from this chart")
        return connection.covariant_divergence(self)

    divergence = covariant_divergence

    def lie(self, vector):
        """Return the Lie derivative along a weight-zero vector tensor."""
        if not isinstance(vector, Tensor) or vector.chart is not self.chart:
            raise ValueError("lie expects an ordinary vector from this chart")
        if vector.rank != 1 or vector.variance != (1,) or vector.density_weight != 0:
            raise ValueError("lie expects a weight-zero contravariant vector")
        components = self._arena._chart_tensor_lie(self.chart, vector, self)
        return Tensor(
            self.chart, components, self.variance, self.density_weight, _owned=True,
            _symmetries=self._symmetries,
        )

    lie_derivative = lie

    def raise_(self, slot=0):
        """Raise one covariant slot with this tensor's chart metric."""
        if self.rank == 0:
            raise ValueError("cannot raise a slot on a scalar tensor")
        slot = int(slot)
        if slot < 0 or slot >= self.rank:
            raise IndexError("tensor slot is outside the tensor rank")
        if self.variance[slot] != -1:
            raise ValueError("raise_ requires a covariant slot")
        components = self._arena._chart_tensor_metric(
            self._arena._lib.chart_tensor_raise, self.chart, self, slot
        )
        variance = list(self.variance)
        variance[slot] = 1
        symmetries = tuple((first, second, kind)
                           for (first, second), kind in self._symmetries.items()
                           if slot not in (first, second))
        return Tensor(
            self.chart, components, variance, self.density_weight, _owned=True,
            _symmetries=symmetries,
        )

    def lower(self, slot=0):
        """Lower one contravariant slot with this tensor's chart metric."""
        if self.rank == 0:
            raise ValueError("cannot lower a slot on a scalar tensor")
        slot = int(slot)
        if slot < 0 or slot >= self.rank:
            raise IndexError("tensor slot is outside the tensor rank")
        if self.variance[slot] != 1:
            raise ValueError("lower requires a contravariant slot")
        components = self._arena._chart_tensor_metric(
            self._arena._lib.chart_tensor_lower, self.chart, self, slot
        )
        variance = list(self.variance)
        variance[slot] = -1
        symmetries = tuple((first, second, kind)
                           for (first, second), kind in self._symmetries.items()
                           if slot not in (first, second))
        return Tensor(
            self.chart, components, variance, self.density_weight, _owned=True,
            _symmetries=symmetries,
        )

    def density(self, weight_or_factor):
        """Set density metadata or multiply by one density factor.

        An integer sets the metadata as before. An ``Expr`` factor creates the
        corresponding component density, for example ``density(sqrtg)`` for
        ``sqrt(g) B**i``.
        """
        if isinstance(weight_or_factor, Expr):
            factor = self._arena._check(weight_or_factor)
            components = self._arena._chart_tensor_density_factor(
                self.chart, self, factor
            )
            return Tensor(
                self.chart, components, self.variance,
                self.density_weight + 1, _owned=True,
                _symmetries=self._symmetries,
            )
        weight = int(weight_or_factor)
        components = self._arena._chart_tensor_density(self.chart, self, weight)
        return Tensor(self.chart, components, self.variance, weight, _owned=True,
                      _symmetries=self._symmetries)

    with_density = density

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


class Connection:
    """A native supplied affine connection ``Gamma^a_bc`` on a chart."""

    def __init__(self, chart, coefficients=None,
                 convention=CONNECTION_STANDARD):
        if not isinstance(chart, Chart):
            raise TypeError("Connection requires a fortsym Chart")
        convention = int(convention)
        if convention not in (CONNECTION_STANDARD, CONNECTION_OPPOSITE):
            raise ValueError("connection convention must be 1 or -1")
        self.chart = chart
        self._arena = chart._arena
        self.convention = convention
        self._source = None
        if coefficients is None:
            self._source = chart.christoffel()
            values = self._source.components
        elif isinstance(coefficients, Tensor):
            if coefficients.chart is not chart or coefficients.rank != 3:
                raise ValueError("connection coefficients must be a rank-three chart tensor")
            values = coefficients.components
            self._source = coefficients
        else:
            values = _tensor3_values(coefficients)
        values = _tensor3_values(values)
        component_values = []
        temporaries = []
        for value in values:
            coerced, temporary = self._arena._coerce(value)
            component_values.append(coerced)
            if temporary is not None:
                temporaries.append(temporary)
        self.components = tuple(component_values)
        self._temporaries = tuple(temporaries)

    def christoffel(self):
        """Return the stored coefficients as a typed ``(1,2)`` tensor view."""
        return Tensor(
            self.chart, self.components, (1, -1, -1), _owned=False
        )

    coefficients = christoffel

    def torsion(self):
        components = self._arena._chart_connection_torsion(self)
        return Tensor(self.chart, components, (1, -1, -1), _owned=True)

    def nonmetricity(self, metric):
        if not isinstance(metric, Metric) or metric.chart is not self.chart:
            raise ValueError("nonmetricity requires a metric on this chart")
        components = self._arena._chart_connection_nonmetricity(self, metric)
        return Tensor(self.chart, components, (-1, -1, -1), _owned=True)

    def riemann(self):
        """Return the stored connection's typed ``R^a_bcd`` tensor."""
        components = self._arena._chart_connection_riemann(self)
        return Tensor(self.chart, components, (1, -1, -1, -1), _owned=True)

    curvature = riemann

    def geodesic_residual(self, curve, parameter):
        """Return ``x''^a + Gamma^a_bc x'^b x'^c`` for this connection."""
        return self._arena._chart_connection_geodesic_residual(
            self, curve, parameter
        )

    def covariant_diff(self, tensor):
        if not isinstance(tensor, Tensor) or tensor.chart is not self.chart:
            raise ValueError("covariant_diff expects a tensor from this chart")
        components = self._arena._chart_connection_tensor(
            self._arena._lib.chart_connection_covariant_diff, self, tensor,
            tensor.rank + 1,
        )
        return Tensor(
            self.chart, components, tensor.variance + (-1,),
            tensor.density_weight, _owned=True,
            _symmetries=tensor.symmetries,
        )

    covariant_derivative = covariant_diff

    def covariant_divergence(self, tensor):
        if not isinstance(tensor, Tensor) or tensor.chart is not self.chart:
            raise ValueError("covariant_divergence expects a tensor from this chart")
        if tensor.rank < 1 or tensor.variance[0] != 1:
            raise ValueError("covariant_divergence requires a contravariant first slot")
        components = self._arena._chart_connection_tensor(
            self._arena._lib.chart_connection_covariant_divergence,
            self, tensor, tensor.rank - 1,
        )
        survivors = list(range(1, tensor.rank))
        positions = {source: target for target, source in enumerate(survivors)}
        symmetries = tuple((positions[first], positions[second], kind)
                           for (first, second), kind in tensor._symmetries.items()
                           if first in positions and second in positions)
        return Tensor(
            self.chart, components, tensor.variance[1:],
            tensor.density_weight, _owned=True, _symmetries=symmetries,
        )

    divergence = covariant_divergence

    def close(self):
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

    def to_tensor(self):
        """Convert this form to its lower antisymmetric tensor view."""
        return self.chart.tensor_from_form(self)

    @property
    def is_closed(self):
        verdict = self._arena._chart_form_closed(self)
        return True if verdict == 1 else False if verdict == 2 else None

    def star(self, metric=None):
        if metric is not None:
            return metric.hodge_star(self)
        components = self._arena._chart_form_unary(
            self._arena._lib.chart_form_star, self
        )
        return Form(self.chart, components, 3 - self.degree, _owned=True)

    hodge_star = star

    def codifferential(self, metric=None):
        """Return the metric codifferential ``delta`` of this form."""
        if self.degree == 0:
            raise ValueError("codifferential is defined here for positive-degree forms")
        if metric is None:
            components = self._arena._chart_form_unary(
                self._arena._lib.chart_form_codifferential, self
            )
        else:
            if not isinstance(metric, Metric) or metric.chart is not self.chart:
                raise ValueError("metric codifferential requires this chart's metric")
            components = self._arena._metric_form_unary(
                self._arena._lib.chart_form_codifferential_metric, metric, self
            )
        return Form(self.chart, components, self.degree - 1, _owned=True)

    codiff = codifferential

    def laplace_de_rham(self, metric=None):
        """Return ``d(delta(form)) + delta(d(form))``."""
        if metric is None:
            components = self._arena._chart_form_unary(
                self._arena._lib.chart_form_laplace_de_rham, self
            )
        else:
            if not isinstance(metric, Metric) or metric.chart is not self.chart:
                raise ValueError("metric Laplace--de Rham requires this chart's metric")
            components = self._arena._metric_form_unary(
                self._arena._lib.chart_form_laplace_de_rham_metric, metric, self
            )
        return Form(self.chart, components, self.degree, _owned=True)

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

    def b_con(self, orientation=1):
        """Recover the contravariant magnetic field from a degree-two form."""
        return self.chart.b_con_form(self, orientation)

    def b_density(self, orientation=1):
        """Recover the weight-one magnetic density from a degree-two form."""
        return self.chart.b_density_form(self, orientation)

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
        self._simplified_result = None
        self._simplified_epoch = -1
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
        self._simplified_result = None
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
        cached = self._simplified_result
        if (cached is not None and cached._handle is not None and
                self._simplified_epoch == self._arena._assumption_epoch):
            return cached
        self._simplified_result = None
        result = self._arena._result(
            self._lib.simplify, self._arena._require(), self._require()
        )
        self._simplified_result = result
        self._simplified_epoch = self._arena._assumption_epoch
        return result

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
def tensor_product(left: Tensor, right: Tensor): return left.product(right)
def contract(tensor: Tensor, first, second): return tensor.contract(first, second)
def trace(tensor: Tensor, first, second): return tensor.trace(first, second)


__all__ = [
    "Arena", "Chart", "ChartMap", "FluxSurface", "FluxCoordinates", "MagneticChart", "MagneticField", "FourierWeakForm", "Metric", "Connection", "SpacetimeMetric", "SpacetimeForm", "SpacetimeTensor", "Tensor", "IndexType", "Index", "Form", "Expr", "FortSymError", "Orientation", "Signature", "Symbol", "symbols", "Integer",
    "FOURIER_INVALID", "FOURIER_LONGITUDINAL", "FOURIER_TRANSVERSE", "SPACE_NONE", "SPACE_NODAL", "SPACE_EDGE", "TRACE_NONE", "TRACE_NORMAL", "TRACE_TANGENTIAL", "FLUX_GENERIC", "FLUX_CLEBSCH", "FLUX_STRAIGHT_FIELD_LINE", "FLUX_BOOZER", "FLUX_HAMADA", "CLEBSCH_RESIDUAL_COUNT", "BOOZER_RESIDUAL_COUNT", "HAMADA_RESIDUAL_COUNT",
    "INDEX_TANGENT", "INDEX_COTANGENT", "INDEX_SPACETIME", "INDEX_INTERNAL", "INDEX_USER",
    "SPACETIME_DIM", "SPACETIME_TENSOR_MAX_RANK", "CONNECTION_STANDARD", "CONNECTION_OPPOSITE",
    "SYMMETRY_NONE", "SYMMETRIC", "ANTISYMMETRIC",
    "Rational", "Float", "Function", "diff", "subs", "subs_many", "factor", "operation_count", "tensor_product", "contract", "trace",
    "free_symbols",
]
