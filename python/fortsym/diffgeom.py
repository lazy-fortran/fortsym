"""Small native-owned subset of :mod:`sympy.diffgeom`.

The compatibility layer keeps SymPy's familiar names at the boundary while
the chart, derivative, wedge, contraction, and Lie operations stay in the
native ``Chart`` and ``Form`` owners.  This module deliberately starts with a
three-dimensional coordinate patch; unsupported coordinate transformations
remain explicit rather than becoming a second geometry implementation.
"""

from __future__ import annotations

from functools import reduce

from . import Chart, Expr, Form, Tensor, Arena


def _native(value):
    if isinstance(value, (BaseScalarField, CoordinateSymbol)):
        return value._expr
    if isinstance(value, Expr):
        return value
    return value


def _system(value):
    if isinstance(value, (BaseScalarField, CoordinateSymbol)):
        return value._system
    return getattr(value, "_diffgeom_system", None)


def _vector_components(vector, system):
    if isinstance(vector, BaseVectorField):
        if vector._system is not system:
            raise ValueError("vector belongs to another coordinate system")
        return vector.components
    if isinstance(vector, _VectorField):
        if vector._system is not system:
            raise ValueError("vector belongs to another coordinate system")
        return vector.components
    values = tuple(_native(value) for value in vector)
    if len(values) != 3:
        raise ValueError("native diffgeom vectors require three components")
    for value in values:
        if not isinstance(value, Expr) or value._arena is not system._arena:
            raise ValueError("vector components belong to another arena")
    return values


class Manifold:
    """Metadata owner for the fixed three-dimensional diffgeom subset."""

    def __init__(self, name, dim, boundary=False, simply_connected=False):
        if int(dim) != 3:
            raise NotImplementedError("native diffgeom currently requires dimension three")
        self.name = str(name)
        self.dim = 3
        self.boundary = bool(boundary)
        self.simply_connected = bool(simply_connected)

    def __repr__(self):
        return self.name

    __str__ = __repr__


class Patch:
    """Metadata owner for one coordinate patch."""

    def __init__(self, name, manifold, open_domain=True, boundary=False,
                 simply_connected=False):
        if not isinstance(manifold, Manifold):
            raise TypeError("patch requires a Manifold")
        self.name = str(name)
        self.manifold = manifold
        self.open_domain = bool(open_domain)
        self.boundary = bool(boundary)
        self.simply_connected = bool(simply_connected)

    def __repr__(self):
        return self.name

    __str__ = __repr__


class BaseScalarField:
    """A coordinate scalar proxy backed by one native expression handle."""

    def __init__(self, system, index, expression):
        self._system = system
        self.index = int(index)
        self._expr = expression

    @property
    def args(self):
        return (self._system, self.index)

    @property
    def name(self):
        return str(self._expr)

    def doit(self):
        return self

    def _result(self, value):
        if isinstance(value, Expr):
            value._diffgeom_system = self._system
        return value

    def __getattr__(self, name):
        return getattr(self._expr, name)

    def __str__(self):
        return str(self._expr)

    __repr__ = __str__

    def __hash__(self):
        return hash((id(self._system), self.index))

    def __eq__(self, other):
        if isinstance(other, (BaseScalarField, CoordinateSymbol)):
            return self._system is other._system and self.index == other.index
        if isinstance(other, Expr):
            return self._expr == other
        return False

    def __bool__(self):
        return bool(self._expr)

    def __add__(self, other):
        return self._result(self._expr + _native(other))

    def __radd__(self, other):
        return self._result(_native(other) + self._expr)

    def __sub__(self, other):
        return self._result(self._expr - _native(other))

    def __rsub__(self, other):
        return self._result(_native(other) - self._expr)

    def __mul__(self, other):
        if isinstance(other, _FormField):
            return other.scale(self._expr)
        return self._result(self._expr * _native(other))

    def __rmul__(self, other):
        if isinstance(other, _FormField):
            return other.scale(self._expr)
        return self._result(_native(other) * self._expr)

    def __truediv__(self, other):
        return self._result(self._expr / _native(other))

    def __rtruediv__(self, other):
        return self._result(_native(other) / self._expr)

    def __pow__(self, other):
        return self._result(self._expr ** _native(other))

    def __neg__(self):
        return self._result(-self._expr)

    def diff(self, variable):
        return self._result(self._expr.diff(_native(variable)))


class CoordinateSymbol(BaseScalarField):
    """SymPy's coordinate-symbol spelling for a base scalar."""


class BaseVectorField:
    """A coordinate basis vector with native coefficient components."""

    def __init__(self, system, index, components=None):
        self._system = system
        self.index = int(index)
        self.components = tuple(
            components if components is not None else system._unit_vector(self.index)
        )

    @property
    def args(self):
        return (self._system, self.index)

    def __str__(self):
        return f"e_{self._system.name}_{self.index}"

    __repr__ = __str__

    def doit(self):
        return self

    def __call__(self, expression):
        return _VectorField(self._system, self.components)(expression)

    def __mul__(self, scalar):
        return _VectorField(self._system, self.components).scale(_native(scalar))

    __rmul__ = __mul__

    def __add__(self, other):
        return _VectorField(self._system, self.components) + other


class _VectorField:
    def __init__(self, system, components):
        self._system = system
        self.components = tuple(components)

    def __call__(self, expression):
        expression = _native(expression)
        if not isinstance(expression, Expr):
            raise TypeError("vector fields act on native scalar expressions")
        result = self._system._arena.integer(0)
        for coefficient, coordinate in zip(self.components, self._system._coordinates):
            result = result + coefficient * expression.diff(coordinate)
        result._diffgeom_system = self._system
        return result

    def scale(self, scalar):
        return _VectorField(
            self._system,
            tuple(component * scalar for component in self.components),
        )

    def __add__(self, other):
        values = _vector_components(other, self._system)
        return _VectorField(
            self._system,
            tuple(left + right for left, right in zip(self.components, values)),
        )


class CoordSystem:
    """A SymPy-shaped view over a native three-dimensional ``Chart``.

    With no explicit symbols, ``CoordSystem("c", patch)`` creates the easy
    identity chart ``c_0, c_1, c_2``.  Pass ``symbols=(...)`` and optionally
    ``position=(...)`` to bind an existing native chart expression set.
    """

    def __init__(self, name, patch, symbols=None, relations=None, **kwargs):
        chart = kwargs.pop("chart", None)
        position = kwargs.pop("position", None)
        if kwargs:
            raise NotImplementedError("coordinate-system options")
        if not isinstance(patch, Patch):
            raise TypeError("coordinate system requires a Patch")
        self.name = str(name)
        self.patch = patch
        self.relations = {} if relations is None else dict(relations)
        self._owns_arena = False

        if chart is not None:
            if not isinstance(chart, Chart):
                raise TypeError("chart must be a native fortsym Chart")
            self._chart = chart
            self._arena = chart._arena
            self._coordinates = tuple(chart.coordinates)
        else:
            values = None if symbols is None else tuple(symbols)
            if values is None:
                self._arena = Arena()
                self._owns_arena = True
                values = tuple(self._arena.symbol(f"{self.name}_{i}") for i in range(3))
            else:
                native_values = tuple(_native(value) for value in values)
                if len(native_values) != 3:
                    raise ValueError("native diffgeom coordinate systems require three symbols")
                if not all(isinstance(value, Expr) for value in native_values):
                    self._arena = Arena()
                    self._owns_arena = True
                    native_values = tuple(self._arena.symbol(str(value)) for value in values)
                else:
                    self._arena = native_values[0]._arena
                values = native_values
            self._coordinates = tuple(values)
            if position is None:
                position = self._coordinates
            position = tuple(_native(value) for value in position)
            self._chart = Chart(self._coordinates, position)

        self._chart._diffgeom_system = self
        self.symbols = tuple(
            CoordinateSymbol(self, index, value)
            for index, value in enumerate(self._coordinates)
        )
        self._scalars = tuple(
            BaseScalarField(self, index, value)
            for index, value in enumerate(self._coordinates)
        )
        self._vectors = tuple(BaseVectorField(self, index) for index in range(3))
        self._oneforms = tuple(Differential(value) for value in self._scalars)

    @property
    def chart(self):
        return self._chart

    def __repr__(self):
        return self.name

    __str__ = __repr__

    def _unit_vector(self, index):
        one = self._arena.integer(1)
        zero = self._arena.integer(0)
        return tuple(one if i == index else zero for i in range(3))

    def base_scalar(self, coord_index):
        return self._scalars[int(coord_index)]

    coord_function = base_scalar

    def base_scalars(self):
        return self._scalars

    coord_functions = base_scalars

    def base_vector(self, index):
        return self._vectors[int(index)]

    def base_vectors(self):
        return self._vectors

    def base_oneform(self, index):
        return self._oneforms[int(index)]

    def base_oneforms(self):
        return self._oneforms

    def close(self):
        if self._owns_arena:
            self._arena.close()
            self._owns_arena = False

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass


class _FormField:
    """Private wrapper for a native form field expression."""

    _fortsym_form_field = True

    def __init__(self, form, label=None, owners=()):
        if not isinstance(form, Form):
            raise TypeError("form field requires a native Form")
        self.form = form
        self.chart = form.chart
        self.degree = form.degree
        self._label = label
        self._owners = tuple(owners)

    def __str__(self):
        return self._label if self._label is not None else "FormField"

    __repr__ = __str__

    def __getitem__(self, mask):
        return self.form[mask]

    def component(self, mask):
        return self.form.component(mask)

    def doit(self):
        return self

    def d(self):
        return _FormField(self.form.d())

    exterior_diff = d

    def _other(self, other):
        if not isinstance(other, _FormField):
            raise TypeError("form operation expects a differential form field")
        if self.chart is not other.chart:
            raise ValueError("form fields belong to different coordinate systems")
        return other

    def __add__(self, other):
        other = self._other(other)
        return _FormField(self.form + other.form)

    def __sub__(self, other):
        other = self._other(other)
        return _FormField(self.form - other.form)

    def __neg__(self):
        return _FormField(-self.form)

    def scale(self, scalar):
        scalar = _native(scalar)
        return _FormField(self.form.scale(scalar))

    def __mul__(self, other):
        if isinstance(other, _FormField):
            return WedgeProduct(self, other)
        return self.scale(other)

    def __rmul__(self, other):
        return self.scale(other)

    def wedge(self, other):
        return WedgeProduct(self, other)

    def lie(self, vector):
        return LieDerivative(vector, self)

    def rcall(self, vector):
        components = _vector_components(vector, _system_for_chart(self.chart))
        result = self.form.interior(components)
        if result.degree == 0:
            return result[0]
        return _FormField(result)

    def close(self):
        self.form.close()


def _system_for_chart(chart):
    return getattr(chart, "_diffgeom_system", None)


def _as_form_field(value):
    if isinstance(value, _FormField):
        return value
    if isinstance(value, Form):
        return _FormField(value)
    raise TypeError("expected a native form field")


class Differential(_FormField):
    """Native exterior derivative with SymPy's ``Differential`` spelling."""

    def __init__(self, form_field):
        if isinstance(form_field, _FormField):
            super().__init__(form_field.form, form_field._label, form_field._owners)
            return
        system = _system(form_field)
        expression = _native(form_field)
        if system is None or not isinstance(expression, Expr):
            raise TypeError("Differential requires a coordinate scalar field")
        values = tuple(expression.diff(coordinate) for coordinate in system._coordinates)
        form = system._chart.one_form(values)
        label = f"d({form_field})"
        if isinstance(form_field, BaseScalarField):
            label = f"d{form_field}"
        super().__init__(form, label, owners=values)
        self._argument = form_field


class WedgeProduct(_FormField):
    """Explicit exterior product that evaluates through the native owner."""

    def __init__(self, *args):
        if not args:
            raise TypeError("WedgeProduct requires at least one factor")
        self.args = tuple(args)
        fields = tuple(_as_form_field(value) for value in args)
        first = fields[0]
        form = reduce(lambda left, right: left.wedge(right.form), fields[1:], first.form)
        super().__init__(form, f"WedgeProduct({', '.join(map(str, args))})", owners=fields)


class TensorProduct:
    """First native tensor-product subset for one-form factors."""

    def __init__(self, *args):
        if not args:
            raise TypeError("TensorProduct requires at least one factor")
        self.args = tuple(args)

    def __repr__(self):
        return f"TensorProduct({', '.join(map(str, self.args))})"

    __str__ = __repr__

    def doit(self):
        fields = tuple(_as_form_field(value) for value in self.args)
        if any(field.degree != 1 for field in fields):
            raise NotImplementedError("TensorProduct currently requires one-forms")
        if any(field.chart is not fields[0].chart for field in fields[1:]):
            raise ValueError("tensor factors belong to different coordinate systems")
        if len(fields) != 2:
            raise NotImplementedError("native TensorProduct currently has two slots")
        left, right = fields
        left_tensor = left.chart.covector(tuple(left.form[mask] for mask in (1, 2, 4)))
        right_tensor = right.chart.covector(tuple(right.form[mask] for mask in (1, 2, 4)))
        result = left_tensor.product(right_tensor)
        result._tensor_owners = fields
        return result


class LieDerivative:
    """Cartan Lie derivative routed to native forms or scalar differentiation."""

    def __init__(self, vector, expression):
        self.args = (vector, expression)

    def __repr__(self):
        return f"LieDerivative({self.args[0]}, {self.args[1]})"

    __str__ = __repr__

    def doit(self):
        vector, expression = self.args
        system = vector._system
        components = _vector_components(vector, system)
        if isinstance(expression, (Form, _FormField)):
            field = _as_form_field(expression)
            return _FormField(field.form.lie(components))
        value = _native(expression)
        if not isinstance(value, Expr):
            raise TypeError("LieDerivative requires a scalar or form field")
        result = _VectorField(system, components)(value)
        return result
