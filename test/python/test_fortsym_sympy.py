import subprocess
import sys
import unittest

import fortsym
import fortsym.sympy as sp

try:
    import sympy as oracle
except ModuleNotFoundError:
    oracle = None


class SympySubsetTest(unittest.TestCase):
    def test_native_subset_and_kind_classes(self):
        x, y = sp.symbols("x y")
        self.assertIsInstance(x, sp.Symbol)
        self.assertIsInstance(x + y, sp.Add)
        self.assertIsInstance(x * y, sp.Mul)
        self.assertIsInstance(x**2, sp.Pow)
        self.assertIsInstance(sp.sin(x), sp.Function)
        self.assertIsInstance(sp.Derivative(x**2, x, evaluate=False), sp.Derivative)
        self.assertEqual(sp.Derivative(x**2, x, evaluate=False).doit(), 2*x)
        self.assertEqual(sp.Subs(x + 1, (x, 2)).doit(), 3)
        self.assertEqual(sp.expand((x + 1) ** 2), x**2 + 2*x + 1)
        self.assertEqual(sp.factor(x**2 + 2*x + 1), (x + 1)**2)
        self.assertEqual(sp.simplify(sp.diff(sp.exp(x*y), x)), y*sp.exp(x*y))
        self.assertEqual(sp.subs((x + 1) ** 2, {x: 2}), 9)

    def test_geometry_classes_are_reexported(self):
        x, y, z = sp.symbols("geometry_x geometry_y geometry_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        metric = chart.metric()
        self.assertIsInstance(metric, sp.Tensor)
        self.assertEqual(metric.variance, (-1, -1))
        self.assertEqual(metric[0, 0].simplify(), 1)
        self.assertEqual(chart.scalar_curvature().simplify(), 0)
        form = chart.one_form((y, z, x))
        self.assertIsInstance(form, sp.Form)
        self.assertEqual(form.d().degree, 2)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_chart_jacobian_and_dual_basis_match_sympy(self):
        u, v, w = sp.symbols("basis_u basis_v basis_w")
        chart = sp.Chart((u, v, w), (u + v, 2*v + w, w))
        covariant = chart.covariant_basis()
        reciprocal = chart.reciprocal_basis()

        oracle_u, oracle_v, oracle_w = oracle.symbols(
            "basis_u basis_v basis_w"
        )
        position = oracle.Matrix((
            oracle_u + oracle_v,
            2*oracle_v + oracle_w,
            oracle_w,
        ))
        coordinates = oracle.Matrix((oracle_u, oracle_v, oracle_w))
        expected_covariant = position.jacobian(coordinates)
        expected_reciprocal = expected_covariant.inv().T

        def native_matrix(values):
            return oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
                str(values[row + 3*column].simplify())
            ))

        actual_covariant = native_matrix(covariant)
        actual_reciprocal = native_matrix(reciprocal)
        self.assertEqual(actual_covariant, expected_covariant)
        self.assertEqual(actual_reciprocal, expected_reciprocal)
        self.assertEqual(
            oracle.sympify(str(chart.jacobian().simplify())),
            expected_covariant.det(),
        )
        self.assertEqual(
            actual_reciprocal.T * actual_covariant,
            oracle.eye(3),
        )

        vector = chart.tensor((1, 2, 3), variance=(1,))
        lowered = vector.lower()
        expected_lowered = expected_covariant.T * expected_covariant * oracle.Matrix(
            (1, 2, 3)
        )
        actual_lowered = oracle.Matrix(tuple(
            oracle.sympify(str(lowered[index].simplify()))
            for index in range(3)
        ))
        self.assertEqual(lowered.variance, (-1,))
        self.assertEqual(actual_lowered, expected_lowered)
        self.assertEqual(lowered.raise_().variance, (1,))
        self.assertEqual(
            oracle.Matrix(tuple(
                oracle.sympify(str(lowered.raise_()[index].simplify()))
                for index in range(3)
            )),
            oracle.Matrix((1, 2, 3)),
        )
        density = vector.density(1)
        self.assertEqual(density.variance, (1,))
        self.assertEqual(density.density_weight, 1)
        self.assertEqual(
            tuple(density[index].simplify() for index in range(3)),
            (1, 2, 3),
        )
        self.assertEqual(chart.vector((1, 2, 3)).variance, (1,))
        self.assertEqual(chart.covector((1, 2, 3)).variance, (-1,))

        expected_volume = oracle.sqrt(
            (expected_covariant.T * expected_covariant).det()
        )
        actual_volume = oracle.sympify(str(chart.volume()[7].simplify()))
        actual_reversed_volume = oracle.sympify(
            str(chart.volume(-1)[7].simplify())
        )
        self.assertEqual(actual_volume, expected_volume)
        self.assertEqual(actual_reversed_volume, -expected_volume)

        scalar = oracle_u**2 + oracle_v*oracle_w
        expected_gradient = (
            expected_covariant.T * expected_covariant
        ).inv() * oracle.Matrix((
            oracle.diff(scalar, oracle_u),
            oracle.diff(scalar, oracle_v),
            oracle.diff(scalar, oracle_w),
        ))
        actual_gradient = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in chart.grad(u**2 + v*w)
        ))
        self.assertEqual(actual_gradient, expected_gradient)

        expected_divergence = sum(
            oracle.diff(coordinate, coordinate)
            for coordinate in (oracle_u, oracle_v, oracle_w)
        )
        self.assertEqual(
            oracle.sympify(str(chart.divergence((u, v, w)).simplify())),
            expected_divergence,
        )

        covector = (oracle_v*oracle_w, oracle_u**2, oracle_u*oracle_v)
        expected_curl = oracle.Matrix((
            oracle.diff(covector[2], oracle_v) - oracle.diff(covector[1], oracle_w),
            oracle.diff(covector[0], oracle_w) - oracle.diff(covector[2], oracle_u),
            oracle.diff(covector[1], oracle_u) - oracle.diff(covector[0], oracle_v),
        )) / expected_covariant.det()
        actual_curl = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in chart.curl((v*w, u**2, u*v))
        ))
        self.assertEqual(actual_curl, expected_curl)
        expected_laplacian = sum(
            oracle.diff(expected_covariant.det()*expected_gradient[index], coordinate)
            for index, coordinate in enumerate((oracle_u, oracle_v, oracle_w))
        ) / expected_covariant.det()
        self.assertEqual(
            oracle.sympify(str(chart.laplacian(u**2 + v*w).simplify())),
            expected_laplacian,
        )

        left_handed = sp.Chart((u, v, w), (-u, v, w))
        self.assertEqual(left_handed.jacobian().simplify(), -1)
        self.assertEqual(left_handed.sqrtg().simplify(), 1)
        self.assertEqual(left_handed.reciprocal_basis()[0].simplify(), -1)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_chart_map_transforms_tensor_slots_and_densities(self):
        x, y, z, p, q, s = sp.symbols(
            "map_x map_y map_z map_p map_q map_s"
        )
        source = sp.Chart((x, y, z), (x, y, z))
        target = sp.Chart((p, q, s), ((p - q)/2, q, s))
        transition = sp.ChartMap(
            source, target, (2*x + y, y, z), ((p - q)/2, q, s)
        )
        op, oq, os = oracle.symbols("map_p map_q map_s")

        jacobian = oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
            str(transition.jacobian()[row + 3*column].simplify())
        ))
        inverse = oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
            str(transition.inverse_jacobian()[row + 3*column].simplify())
        ))
        self.assertEqual(jacobian, oracle.Matrix(((2, 1, 0), (0, 1, 0), (0, 0, 1))))
        self.assertEqual(
            inverse,
            oracle.Matrix(((oracle.Rational(1, 2), -oracle.Rational(1, 2), 0),
                           (0, 1, 0), (0, 0, 1))),
        )

        vector = transition.transform(source.vector((x, y, z)))
        self.assertEqual(
            tuple(oracle.sympify(str(vector[index].simplify())) for index in range(3)),
            (op, oq, os),
        )
        covector = transition.transform(source.covector((2*x, y, z)))
        self.assertEqual(
            tuple(oracle.sympify(str(covector[index].simplify())) for index in range(3)),
            ((op - oq)/2, (-op + 3*oq)/2, os),
        )

        matrix_values = tuple(range(1, 10))
        mixed = source.tensor(matrix_values, variance=(1, -1))
        transformed = transition.transform(mixed)
        source_matrix = oracle.Matrix(3, 3, lambda row, column: matrix_values[row + 3*column])
        expected = jacobian * source_matrix * inverse
        actual = oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
            str(transformed[row, column].simplify())
        ))
        self.assertEqual(actual, expected)

        density = transition.transform(source.vector((x, y, z), density_weight=1))
        self.assertEqual(
            tuple(oracle.sympify(str(density[index].simplify())) for index in range(3)),
            (2*op, 2*oq, 2*os),
        )

        one_form = transition.pullback(source.one_form((x, y, z)))
        self.assertEqual(
            tuple(oracle.sympify(str(one_form[mask].simplify()))
                  for mask in (1, 2, 4)),
            ((op - oq)/4, (-op + 5*oq)/4, os),
        )
        two_form = transition.transform(source.two_form((x, y, z)))
        self.assertEqual(
            tuple(oracle.sympify(str(two_form[mask].simplify()))
                  for mask in (3, 5, 6)),
            ((op - oq)/4, oq/2, -oq/2 + os),
        )
        three_form = transition.transform(source.three_form(1))
        self.assertEqual(
            oracle.sympify(str(three_form[7].simplify())),
            oracle.Rational(1, 2),
        )

        r, t, v = sp.symbols("map_r map_t map_v")
        final = sp.Chart((r, t, v), ((r - t)/2, t, v))
        following = sp.ChartMap(
            target, final, (p + q, q, s), (r - t, t, v)
        )
        composed = transition.compose(following)
        composed_vector = composed.transform(source.vector((x, y, z)))
        self.assertEqual(
            tuple(oracle.sympify(str(value.simplify()))
                  for value in composed_vector),
            (oracle.Symbol("map_r"), oracle.Symbol("map_t"), oracle.Symbol("map_v")),
        )
        composed_one_form = composed.transform(source.one_form((x, y, z)))
        self.assertEqual(
            tuple(oracle.sympify(str(composed_one_form[mask].simplify()))
                  for mask in (1, 2, 4)),
            ((oracle.Symbol("map_r") - 2*oracle.Symbol("map_t"))/4,
             (-oracle.Symbol("map_r") + 4*oracle.Symbol("map_t"))/2,
             oracle.Symbol("map_v")),
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_diffgeom_names_use_native_owner_and_match_oracle(self):
        from sympy.diffgeom import (
            CoordSystem as OracleCoordSystem,
            Differential as OracleDifferential,
            LieDerivative as OracleLieDerivative,
            Manifold as OracleManifold,
            Patch as OraclePatch,
            WedgeProduct as OracleWedgeProduct,
        )

        native_manifold = sp.Manifold("native_M", 3)
        native_patch = sp.Patch("native_P", native_manifold)
        native = sp.CoordSystem("native_c", native_patch)
        nx, ny, nz = native.base_scalars()
        nvx, nvy, _ = native.base_vectors()
        ndx, ndy, _ = native.base_oneforms()

        oracle_manifold = OracleManifold("oracle_M", 3)
        oracle_patch = OraclePatch("oracle_P", oracle_manifold)
        reference = OracleCoordSystem("oracle_c", oracle_patch)
        ox, oy, oz = reference.base_scalars()
        ovx, ovy, _ = reference.base_vectors()
        odx, ody, _ = reference.base_oneforms()

        def scalar_text(value):
            if isinstance(value, fortsym.Expr):
                value = value.simplify()
            return oracle.sympify(str(value))

        native_differential = sp.Differential(nx + ny)
        oracle_differential = OracleDifferential(ox + oy)
        self.assertEqual(
            scalar_text(native_differential.rcall(nvx)),
            oracle_differential.rcall(ovx),
        )

        native_wedge = sp.WedgeProduct(ndx, ndy)
        oracle_wedge = OracleWedgeProduct(odx, ody)
        self.assertEqual(
            scalar_text(native_wedge.rcall(nvx)[2].simplify()),
            oracle_wedge.rcall(ovx).rcall(ovy),
        )

        native_lie = sp.LieDerivative(nvx, ndy).doit()
        oracle_lie = OracleLieDerivative(ovx, ody)
        self.assertEqual(
            scalar_text(native_lie.rcall(nvy)),
            oracle_lie.rcall(ovy),
        )
        self.assertEqual(
            scalar_text(sp.LieDerivative(nvx, nx + ny).doit()),
            OracleLieDerivative(ovx, ox + oy).doit(),
        )
        native.close()

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_paper_magnetic_transverse_form_translation(self):
        from sympy.diffgeom import (
            CoordSystem as OracleCoordSystem,
            Differential as OracleDifferential,
            Manifold as OracleManifold,
            Patch as OraclePatch,
        )

        native_x1, native_x2, native_x3 = sp.symbols(
            "paper_x1 paper_x2 paper_x3"
        )
        native_manifold = sp.Manifold("paper_M", 3)
        native_patch = sp.Patch("paper_P", native_manifold)
        native = sp.CoordSystem(
            "paper_c", native_patch,
            symbols=(native_x1, native_x2, native_x3),
        )
        x1, x2, x3 = native.base_scalars()
        dx, dy, dz = native.base_oneforms()
        a1 = x1*x2 + x2**2
        a2 = x1**2 + x1*x2
        potential = a1*dx + a2*dy
        magnetic = potential.d()

        oracle_manifold = OracleManifold("paper_M_ref", 3)
        oracle_patch = OraclePatch("paper_P_ref", oracle_manifold)
        reference = OracleCoordSystem(
            "paper_c", oracle_patch,
            symbols=tuple(
                oracle.Symbol(name, real=True)
                for name in ("paper_x1", "paper_x2", "paper_x3")
            ),
        )
        ox1, ox2, ox3 = reference.base_scalars()
        ovx, ovy, _ = reference.base_vectors()
        oa1 = ox1*ox2 + ox2**2
        oa2 = ox1**2 + ox1*ox2
        expected_curl = (
            OracleDifferential(oa2).rcall(ovx)
            - OracleDifferential(oa1).rcall(ovy)
        )

        actual = magnetic[3].simplify()
        expected = oracle.sympify(str(expected_curl))
        actual = oracle.sympify(str(actual))
        self.assertEqual(oracle.simplify(actual - expected), 0)
        self.assertEqual(magnetic.d()[7].simplify(), 0)
        native.close()

    def test_simultaneous_substitution(self):
        x, y = sp.symbols("simultaneous_x simultaneous_y")
        self.assertEqual(
            sp.subs(x + y, {x: y, y: x}, simultaneous=True), x + y
        )
        self.assertEqual(
            sp.subs(x + y, {x: y + 1, y: 2}, simultaneous=True), y + 3
        )
        self.assertEqual(sp.subs(x + y, {}), x + y)

    def test_unordered_mapping_uses_sympy_order(self):
        x, y, z = sp.symbols("mapping_order_x mapping_order_y mapping_order_z")
        f = sp.Function("mapping_order_f")
        self.assertEqual(
            sp.subs(f(x**2 + x), {x: y, x**2: z}), f(y + z)
        )
        self.assertEqual(
            sp.subs(f(sp.sin(x)), {
                sp.cos(x): z, sp.sin(x): sp.cos(x)
            }), f(z)
        )
        self.assertEqual(sp.subs(x + y, {x: y, y: 2}), 4)
        self.assertEqual(sp.subs(x + y, {x: y + 1, y: 2}), 5)

    def test_xreplace_uses_exact_nodes_without_expanding(self):
        x, y, z = sp.symbols("xreplace_x xreplace_y xreplace_z")
        f = sp.Function("xreplace_f")
        self.assertEqual(f(x).xreplace({x: y}), f(y))
        self.assertEqual((x**2 + x).xreplace({x**2: y}), x + y)
        self.assertEqual(
            f(x + y).xreplace({x + y: z, y: x + y}), f(z)
        )
        expression = (x + 1)**2
        self.assertIs(expression.xreplace({}), expression)
        with self.assertRaises(TypeError):
            expression.xreplace([(x, y)])

    def test_replace_uses_exact_nodes_and_map_contract(self):
        x, y, z = sp.symbols("replace_x replace_y replace_z")
        f = sp.Function("replace_f")
        self.assertEqual(f(x + 1).replace(x, y), f(y + 1))
        self.assertEqual(
            (x + 1).replace(x + 1, z, exact=False), z
        )
        result, replacements = (f(x) + x).replace(x, y, map=True)
        self.assertEqual(result, f(y) + y)
        self.assertEqual(replacements, {x: y})
        result.close()
        cached = x.replace(x, y)
        cached.close()
        self.assertEqual(x.replace(x, y), y)
        unchanged, replacements = (x + 1).replace(z, y, map=True)
        self.assertEqual(unchanged, x + 1)
        self.assertEqual(replacements, {})
        with self.assertRaises(NotImplementedError):
            x.replace(sp.Wild("replace_a"), y)
        with self.assertRaises(TypeError):
            x.replace(x, y, map=1)

    def test_match_uses_exact_structural_boundary(self):
        x, y = sp.symbols("match_x match_y")
        f = sp.Function("match_f")
        self.assertEqual(x.match(x), {})
        self.assertIsNone(x.match(y))
        self.assertEqual((x + y).match(y + x), {})
        self.assertEqual(f(x + 1).match(f(x + 1)), {})
        self.assertIsNone(f(x + 1).match(f(x + 2)))
        self.assertEqual(x.match(x, old=True), {})

    def test_match_supports_structural_wildcards(self):
        x, y = sp.symbols("wild_x wild_y")
        a = sp.Wild("wild_a")
        b = sp.Wild("wild_b")
        f = sp.Function("wild_f")
        self.assertEqual(x.match(a), {a: x})
        self.assertEqual((x + 1).match(a + 1), {a: x})
        self.assertEqual(
            f(x + y).match(f(a + b)), {a: x, b: y}
        )
        self.assertIsNone(x.match(sp.Wild("not_x", exclude=(x,))))
        integer = sp.Wild(
            "wild_integer", properties=lambda value: value.is_integer is True
        )
        self.assertEqual(sp.Integer(2).match(integer), {integer: sp.Integer(2)})
        self.assertIsNone(sp.Rational(1, 2).match(integer))

    def test_match_supports_bounded_commutative_remainders(self):
        x, y = sp.symbols("remainder_x remainder_y")
        a = sp.Wild("remainder_a")
        self.assertEqual((x + y).match(x + a), {a: y})
        self.assertEqual((2*x).match(x*a), {a: sp.Integer(2)})
        self.assertEqual((x*y).match(x*a), {a: y})
        self.assertEqual(x.match(x + a), {a: sp.Integer(0)})
        self.assertEqual((x + y).match(a + a), {a: (x + y)/2})
        self.assertIsNone((x*y).match(x + a))

        excluded = sp.Wild("remainder_excluded", exclude=(y,))
        self.assertIsNone((x + y).match(x + excluded))
        integer = sp.Wild(
            "remainder_integer",
            properties=lambda value: value.is_integer is True,
        )
        self.assertEqual((x + 2).match(x + integer), {
            integer: sp.Integer(2),
        })
        self.assertIsNone((x + y).match(x + integer))

    def test_match_supports_bounded_commutative_partitions(self):
        x, y, z = sp.symbols("partition_x partition_y partition_z")
        a = sp.Wild("partition_a")
        b = sp.Wild("partition_b")
        c = sp.Wild("partition_c")
        self.assertEqual((x + y).match(a + b), {a: x, b: y})
        self.assertEqual((x + y + z).match(a + b), {
            a: x + y, b: z,
        })
        self.assertEqual((x + y + z).match(x + a + b), {
            a: y, b: z,
        })
        self.assertEqual((x + y + z).match(a + b + c), {
            a: x, b: y, c: z,
        })
        self.assertEqual((x + y + 1).match(a + b), {
            a: x + 1, b: y,
        })
        self.assertEqual((x * y).match(a * b), {a: x, b: y})
        self.assertEqual((2*x*y).match(x*a*b), {
            a: sp.Integer(2), b: y,
        })
        self.assertEqual(x.match(a + b), {
            a: sp.Integer(0), b: x,
        })
        self.assertEqual(x.match(a * b), {
            a: sp.Integer(1), b: x,
        })
        d = sp.Wild("partition_d")
        self.assertIsNone((x + y).match(a + b + c + d))
        self.assertIsNone((x * y).match(a + b))
        self.assertEqual((x + y).match(a * b), {
            a: sp.Integer(1), b: x + y,
        })
        f = sp.Function("partition_f")
        self.assertEqual(f(x + y).match(f(a + b)), {
            a: x, b: y,
        })

    def test_refusal_and_truth_contract(self):
        self.assertFalse(bool(sp.Integer(0)))
        with self.assertRaises(TypeError):
            bool(sp.Symbol("z"))
        self.assertEqual(sp.factor(sp.Symbol("z") + 1), sp.Symbol("z") + 1)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.factor((sp.Symbol("z")**2 - 1)/(sp.Symbol("z") - 1))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.factor(sp.Symbol("z") + 1, modulus=2)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.symbols("z", prime=True)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.symbols("z", positive=False)
        result = subprocess.run(
            [
                sys.executable,
                "-c",
                "import sys; import fortsym.sympy; "
                "assert 'sympy' not in sys.modules",
            ],
            check=False,
        )
        self.assertEqual(result.returncode, 0)

    def test_supported_assumptions_reach_native_simplifier(self):
        x = sp.Symbol("x", real=True)
        self.assertEqual(sp.simplify(sp.sqrt(x**2)), sp.Abs(x))

        y = sp.Symbol("y", nonnegative=True)
        self.assertEqual(sp.simplify(sp.sqrt(y**2)), y)

        z = sp.Symbol("z", positive=True)
        self.assertEqual(sp.simplify(sp.sqrt(z**2)), z)
        self.assertEqual(sp.simplify(sp.Abs(z)), z)

        negative = sp.Symbol("signed_negative_simplify", negative=True)
        nonpositive = sp.Symbol("signed_nonpositive_simplify", nonpositive=True)
        zero = sp.Symbol("signed_zero_simplify", zero=True)
        negative_value = sp.simplify(-negative)
        nonpositive_value = sp.simplify(-nonpositive)
        self.assertEqual(sp.simplify(sp.sqrt(negative**2)), negative_value)
        self.assertEqual(sp.simplify(sp.Abs(negative)), negative_value)
        self.assertEqual(sp.simplify(sp.sqrt(nonpositive**2)), nonpositive_value)
        self.assertEqual(sp.simplify(sp.Abs(nonpositive)), nonpositive_value)
        self.assertEqual(sp.simplify(sp.sqrt(zero**2)), sp.Integer(0))
        self.assertEqual(sp.simplify(sp.Abs(zero)), sp.Integer(0))

        log_real = sp.Symbol("guarded_log_real", real=True)
        log_nonzero = sp.Symbol("guarded_log_nonzero", nonzero=True)
        unknown_log = sp.Symbol("guarded_log_unknown")
        self.assertEqual(sp.simplify(sp.log(sp.exp(log_real))), log_real)
        self.assertEqual(sp.simplify(sp.exp(sp.log(log_nonzero))), log_nonzero)
        self.assertEqual(
            str(sp.simplify(sp.log(sp.exp(unknown_log)))),
            str(sp.log(sp.exp(unknown_log))),
        )
        self.assertEqual(
            str(sp.simplify(sp.exp(sp.log(unknown_log)))),
            str(sp.exp(sp.log(unknown_log))),
        )

    def test_principal_sqrt_power_is_exact(self):
        x = sp.Symbol("principal_sqrt_power")
        raw = sp.sqrt(x)**2
        self.assertEqual(raw, x)
        self.assertEqual(sp.simplify(raw), x)

    def test_domain_sentinels_are_exposed(self):
        self.assertEqual(str(sp.oo), "oo")
        self.assertEqual(sp.oo.name, "oo")
        self.assertEqual(str(sp.zoo), "zoo")
        self.assertEqual(sp.zoo.name, "zoo")
        self.assertEqual(str(sp.nan), "nan")
        self.assertEqual(sp.nan.name, "nan")

    def test_signed_zero_is_preserved_by_native_float(self):
        positive = sp.Float(0.0)
        negative = sp.Float(-0.0)
        self.assertNotEqual(positive, negative)
        self.assertEqual(str(positive), "0.0000000000000000E+000")
        self.assertEqual(str(negative), "-0.0000000000000000E+000")

    def test_nan_domain_rules(self):
        x = sp.Symbol("nan_rule_x")
        self.assertEqual(sp.simplify(sp.nan + x), sp.nan)
        self.assertEqual(sp.simplify(sp.nan * 0), sp.nan)
        self.assertEqual(sp.simplify(sp.sqrt(sp.nan)), sp.nan)
        self.assertEqual(sp.simplify(sp.nan**0), sp.Integer(1))
        self.assertEqual(sp.simplify(sp.nan**x), sp.nan)
        self.assertEqual(sp.simplify(x**sp.nan), sp.nan)

    def test_directed_domain_rules(self):
        x = sp.Symbol("domain_rule_x")
        cases = [
            (sp.oo + 3, sp.oo),
            (sp.oo + (-sp.oo), sp.nan),
            (sp.oo * 0, sp.nan),
            (sp.oo * 2, sp.oo),
            (sp.oo * -2, sp.simplify(-sp.oo)),
            (sp.oo**0, sp.Integer(1)),
            (sp.oo**2, sp.oo),
            (sp.oo**-2, sp.Integer(0)),
            ((-sp.oo)**2, sp.oo),
            ((-sp.oo)**3, sp.simplify(-sp.oo)),
            (sp.zoo + 1, sp.zoo),
            (sp.zoo + sp.zoo, sp.nan),
            (sp.zoo + sp.oo, sp.nan),
            (sp.zoo * 0, sp.nan),
            (sp.zoo * 2, sp.zoo),
            (sp.zoo * sp.oo, sp.zoo),
            (sp.zoo**0, sp.Integer(1)),
            (sp.zoo**2, sp.zoo),
            (sp.zoo**-2, sp.Integer(0)),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertEqual(str(sp.simplify(sp.oo * x)), "domain_rule_x*oo")
        self.assertEqual(str(sp.simplify(sp.zoo * x)), "domain_rule_x*zoo")

    def test_directed_domain_functions(self):
        x = sp.Symbol("domain_function_x")
        cases = [
            (sp.sqrt(sp.oo), sp.oo),
            (sp.sqrt(sp.zoo), sp.zoo),
            (sp.Abs(-sp.oo), sp.oo),
            (sp.Abs(sp.zoo), sp.oo),
            (sp.exp(sp.oo), sp.oo),
            (sp.exp(-sp.oo), sp.Integer(0)),
            (sp.exp(sp.zoo), sp.nan),
            (sp.log(sp.oo), sp.oo),
            (sp.log(-sp.oo), sp.oo),
            (sp.log(sp.zoo), sp.zoo),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertEqual(
            sp.simplify(sp.sqrt(-sp.oo)), sp.simplify(sp.I * sp.oo)
        )
        self.assertEqual(
            str(sp.simplify(sp.sqrt(sp.oo * x))),
            "sqrt(domain_function_x*oo)",
        )
        self.assertEqual(
            str(sp.simplify(sp.exp(sp.oo * x))),
            "exp(domain_function_x*oo)",
        )

    def test_directed_atan2_heads(self):
        cases = [
            (sp.atan2(sp.oo, sp.oo), sp.Integer(0)),
            (sp.atan2(-sp.oo, sp.oo), sp.Integer(0)),
            (sp.atan2(sp.oo, -sp.oo), sp.pi),
            (sp.atan2(-sp.oo, -sp.oo), sp.Integer(-1) * sp.pi),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertIsInstance(
            sp.simplify(sp.atan2(sp.zoo, sp.oo)), sp.Function
        )

    def test_bessel_domain_heads(self):
        order = sp.Symbol("bessel_order")
        cases = [
            (sp.besselj(order, sp.oo), sp.Integer(0)),
            (sp.besselj(order, -sp.oo), sp.Integer(0)),
            (sp.besseli(order, sp.oo), sp.oo),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertEqual(
            str(sp.simplify(sp.besseli(order, -sp.oo))),
            "oo*(-1)**bessel_order",
        )
        self.assertEqual(
            sp.simplify(sp.besseli(sp.Integer(1), -sp.oo)),
            sp.Integer(-1) * sp.oo,
        )
        self.assertIsInstance(
            sp.simplify(sp.besselj(order, sp.zoo)), sp.Function
        )
        self.assertIsInstance(
            sp.simplify(sp.besseli(order, sp.zoo)), sp.Function
        )
        self.assertIsInstance(
            sp.simplify(sp.besselj(sp.nan, order)), sp.Function
        )
        self.assertIsInstance(
            sp.simplify(sp.besseli(order, sp.nan)), sp.Function
        )
        self.assertEqual(sp.simplify(sp.besseli(sp.nan, -sp.oo)), sp.nan)

    def test_legendre_domain_heads(self):
        cases = [
            (sp.legendre(2, sp.oo), sp.oo),
            (sp.legendre(1, -sp.oo), sp.Integer(-1) * sp.oo),
            (sp.legendre(2, -sp.oo), sp.oo),
            (sp.legendre(0, sp.zoo), sp.Integer(1)),
            (sp.legendre(2, sp.zoo), sp.zoo),
            (sp.legendre(3, sp.oo), sp.nan),
            (sp.legendre(3, -sp.oo), sp.nan),
            (sp.legendre(3, sp.zoo), sp.nan),
            (sp.legendre(-1, sp.oo), sp.Integer(1)),
            (sp.legendre(-2, -sp.oo), sp.Integer(-1) * sp.oo),
            (sp.legendre(-3, sp.oo), sp.oo),
            (sp.legendre(-4, sp.zoo), sp.nan),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertIsInstance(
            sp.simplify(sp.legendre(sp.Rational(1, 2), sp.oo)), sp.Function
        )
        self.assertIsInstance(
            sp.simplify(sp.legendre(sp.nan, sp.Symbol("legendre_argument"))),
            sp.Function,
        )

    def test_complex_domain_operations(self):
        self.assertEqual(sp.re(sp.I), sp.Integer(0))
        self.assertEqual(sp.im(sp.I), sp.Integer(1))
        self.assertEqual(sp.conjugate(sp.Integer(2)), sp.Integer(2))
        self.assertEqual(sp.arg(sp.Integer(-1)), sp.pi)
        self.assertEqual(sp.Abs(sp.I), sp.Integer(1))
        self.assertEqual(sp.Abs(sp.Integer(2) + sp.I), sp.sqrt(sp.Integer(5)))
        self.assertEqual(str(sp.Abs(sp.Symbol("abs_unknown"))),
                         "abs(abs_unknown)")
        self.assertEqual(sp.expand_complex(sp.I), sp.I)
        self.assertEqual(sp.expand_complex(sp.exp(sp.I), deep=False),
                         sp.cos(1) + sp.I*sp.sin(1))
        self.assertEqual(str(sp.expand_complex(sp.oo)), str(sp.oo))
        self.assertEqual(str(sp.expand_complex(sp.zoo)), str(sp.nan))
        self.assertEqual(str(sp.expand_complex(sp.nan)), str(sp.nan))
        sentinel_cases = {
            "re": ((sp.oo, "oo"), (-sp.oo, "-oo"),
                   (sp.zoo, "nan"), (sp.nan, "nan")),
            "im": ((sp.oo, "0"), (-sp.oo, "0"),
                   (sp.zoo, "nan"), (sp.nan, "nan")),
            "Abs": ((sp.oo, "oo"), (-sp.oo, "oo"),
                    (sp.zoo, "oo"), (sp.nan, "nan")),
            "arg": ((sp.oo, "0"), (-sp.oo, "pi"),
                    (sp.zoo, "nan"), (sp.nan, "nan")),
            "conjugate": ((sp.oo, "oo"), (-sp.oo, "-oo"),
                          (sp.zoo, "conjugate(zoo)"), (sp.nan, "nan")),
            "expand_complex": ((sp.oo, "oo"), (-sp.oo, "-oo"),
                                (sp.zoo, "nan"), (sp.nan, "nan")),
        }
        for name, cases in sentinel_cases.items():
            function = getattr(sp, name)
            for value, expected in cases:
                with self.subTest(function=name, value=str(value)):
                    self.assertEqual(str(function(value)), expected)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.expand_complex(sp.Symbol("expand_complex_unknown"))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.re(sp.Symbol("complex_unknown"))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.im(sp.Symbol("complex_unknown_im"))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.conjugate(sp.Symbol("complex_unknown_conjugate"))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.arg(sp.Integer(0))

    def test_noninteger_domain_powers(self):
        cases = [
            (sp.oo**sp.Rational(1, 2), sp.oo),
            (sp.oo**sp.Rational(2, 3), sp.oo),
            (sp.oo**sp.Rational(-1, 2), sp.Integer(0)),
            (sp.zoo**sp.Rational(1, 2), sp.zoo),
            (sp.zoo**sp.Rational(4, 3), sp.zoo),
            (sp.zoo**sp.Rational(-1, 2), sp.Integer(0)),
            ((-sp.oo)**sp.Rational(1, 2), sp.simplify(sp.I * sp.oo)),
            ((-sp.oo)**sp.Rational(3, 2), sp.simplify(-sp.I * sp.oo)),
            ((-sp.oo)**sp.Rational(5, 2), sp.simplify(sp.I * sp.oo)),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        phase_cases = [
            (sp.Rational(1, 3), "oo*(-1)**(1/3)"),
            (sp.Rational(2, 3), "oo*(-1)**(2/3)"),
            (sp.Rational(4, 3), "-oo*(-1)**(1/3)"),
        ]
        for exponent, expected in phase_cases:
            with self.subTest(exponent=str(exponent)):
                self.assertEqual(
                    str(sp.simplify((-sp.oo)**exponent)), expected
                )

    def test_direct_domain_heads(self):
        cases = [
            (sp.sign(sp.oo), sp.Integer(1)),
            (sp.sign(-sp.oo), sp.Integer(-1)),
            (sp.sign(sp.zoo), sp.sign(sp.zoo)),
            (sp.floor(sp.oo), sp.oo),
            (sp.floor(-sp.oo), sp.simplify(-sp.oo)),
            (sp.floor(sp.zoo), sp.zoo),
            (sp.ceiling(sp.oo), sp.oo),
            (sp.ceiling(-sp.oo), sp.simplify(-sp.oo)),
            (sp.ceiling(sp.zoo), sp.zoo),
            (sp.sinh(sp.oo), sp.oo),
            (sp.sinh(-sp.oo), sp.simplify(-sp.oo)),
            (sp.sinh(sp.zoo), sp.nan),
            (sp.cosh(-sp.oo), sp.oo),
            (sp.cosh(sp.zoo), sp.nan),
            (sp.tanh(sp.oo), sp.Integer(1)),
            (sp.tanh(-sp.oo), sp.Integer(-1)),
            (sp.tanh(sp.zoo), sp.nan),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)

    def test_inverse_domain_heads(self):
        cases = [
            (sp.asin(sp.oo), sp.Integer(-1) * sp.I * sp.oo),
            (sp.asin(-sp.oo), sp.I * sp.oo),
            (sp.asin(sp.zoo), sp.zoo),
            (sp.acos(sp.oo), sp.I * sp.oo),
            (sp.acos(-sp.oo), sp.Integer(-1) * sp.I * sp.oo),
            (sp.acos(sp.zoo), sp.zoo),
            (sp.atan(sp.oo), sp.Rational(1, 2) * sp.pi),
            (sp.atan(-sp.oo), sp.Rational(-1, 2) * sp.pi),
            (sp.asinh(sp.oo), sp.oo),
            (sp.asinh(-sp.oo), sp.Integer(-1) * sp.oo),
            (sp.asinh(sp.zoo), sp.zoo),
            (sp.acosh(sp.oo), sp.oo),
            (sp.acosh(-sp.oo), sp.oo),
            (sp.acosh(sp.zoo), sp.zoo),
            (sp.atanh(sp.oo), sp.Rational(-1, 2) * sp.I * sp.pi),
            (sp.atanh(-sp.oo), sp.Rational(1, 2) * sp.I * sp.pi),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertIsInstance(sp.simplify(sp.atan(sp.zoo)), sp.Function)
        self.assertIsInstance(sp.simplify(sp.atanh(sp.zoo)), sp.Function)

    def test_reciprocal_hyperbolic_domain_heads(self):
        cases = [
            (sp.csch(sp.oo), sp.Integer(0)),
            (sp.csch(-sp.oo), sp.Integer(0)),
            (sp.csch(sp.zoo), sp.nan),
            (sp.sech(sp.oo), sp.Integer(0)),
            (sp.sech(-sp.oo), sp.Integer(0)),
            (sp.sech(sp.zoo), sp.nan),
            (sp.coth(sp.oo), sp.Integer(1)),
            (sp.coth(-sp.oo), sp.Integer(-1)),
            (sp.coth(sp.zoo), sp.nan),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)

    def test_error_function_domain_heads(self):
        cases = [
            (sp.erf(sp.oo), sp.Integer(1)),
            (sp.erf(-sp.oo), sp.Integer(-1)),
            (sp.erfc(sp.oo), sp.Integer(0)),
            (sp.erfc(-sp.oo), sp.Integer(2)),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertIsInstance(sp.simplify(sp.erf(sp.zoo)), sp.Function)
        self.assertIsInstance(sp.simplify(sp.erfc(sp.zoo)), sp.Function)

    def test_gamma_domain_heads(self):
        cases = [
            (sp.gamma(sp.oo), sp.oo),
            (sp.factorial(sp.oo), sp.oo),
            (sp.loggamma(sp.oo), sp.oo),
            (sp.loggamma(-sp.oo), sp.zoo),
            (sp.loggamma(sp.zoo), sp.zoo),
        ]
        for expression, expected in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(sp.simplify(expression), expected)
        self.assertIsInstance(sp.simplify(sp.gamma(-sp.oo)), sp.Function)
        self.assertIsInstance(sp.simplify(sp.gamma(sp.zoo)), sp.Function)
        self.assertIsInstance(sp.simplify(sp.factorial(-sp.oo)), sp.Function)
        self.assertIsInstance(sp.simplify(sp.factorial(sp.zoo)), sp.Function)

    def test_three_valued_zero_predicates(self):
        x = sp.Symbol("predicate_x")
        cases = [
            (True, False, sp.Integer(0)),
            (False, True, sp.Integer(7)),
            (True, False, x - x),
            (None, None, sp.sin(x)),
            (None, None, x),
        ]
        for expected_zero, expected_nonzero, expression in cases:
            with self.subTest(expression=str(expression)):
                self.assertEqual(expression.is_zero, expected_zero)
                self.assertEqual(expression.is_nonzero, expected_nonzero)

    def test_supported_assumption_predicates(self):
        integer = sp.Symbol("predicate_integer", integer=True)
        rational = sp.Symbol("predicate_rational", rational=True)
        real = sp.Symbol("predicate_real", real=True)
        positive = sp.Symbol("predicate_positive", positive=True)
        nonnegative = sp.Symbol("predicate_nonnegative", nonnegative=True)
        nonzero = sp.Symbol("predicate_nonzero", nonzero=True)

        self.assertEqual((integer.is_integer, integer.is_rational,
                          integer.is_real, sp.ask(sp.Q.integer(integer)),
                          sp.ask(sp.Q.rational(integer))),
                         (True, True, True, True, True))
        self.assertEqual((rational.is_rational, rational.is_integer,
                          rational.is_real, sp.ask(sp.Q.rational(rational))),
                         (True, None, True, True))
        self.assertEqual((sp.Integer(2).is_integer,
                          sp.Integer(2).is_rational,
                          sp.Rational(2, 3).is_integer,
                          sp.Rational(2, 3).is_rational,
                          sp.Float(2.0).is_integer,
                          sp.Float(2.0).is_rational,
                          sp.Symbol("predicate_unknown").is_integer),
                         (True, True, False, True, None, None, None))
        self.assertEqual((real.is_real, real.is_positive,
                          real.is_nonnegative, real.is_nonzero),
                         (True, None, None, None))
        self.assertEqual((positive.is_real, positive.is_positive,
                          positive.is_nonnegative, positive.is_nonzero),
                         (True, True, True, True))
        self.assertEqual((nonnegative.is_real, nonnegative.is_positive,
                          nonnegative.is_nonnegative, nonnegative.is_nonzero),
                         (True, None, True, None))
        self.assertEqual((nonzero.is_real, nonzero.is_positive,
                          nonzero.is_nonnegative, nonzero.is_nonzero),
                         (True, None, None, True))

        negative = sp.Symbol("predicate_negative", negative=True)
        zero = sp.Symbol("predicate_zero", zero=True)
        self.assertEqual((negative.is_negative, negative.is_nonpositive,
                          negative.is_positive, negative.is_nonnegative,
                          negative.is_nonzero),
                         (True, True, False, False, True))
        self.assertEqual((zero.is_zero, zero.is_nonpositive,
                          zero.is_nonnegative, zero.is_positive,
                          zero.is_negative, zero.is_nonzero),
                         (True, True, True, False, False, False))

    def test_scoped_assumptions_restore_nested_default_state(self):
        x = sp.Symbol("scoped_positive")
        y = sp.Symbol("scoped_nonnegative")
        positive = sp.Q.positive(x)
        nonnegative = sp.Q.nonnegative(y)

        self.assertIsNone(sp.ask(positive))
        with sp.assuming(positive):
            self.assertTrue(sp.ask(positive))
            self.assertTrue(sp.ask(sp.Q.real(x)))
            with sp.assuming(nonnegative):
                self.assertTrue(sp.ask(nonnegative))
                self.assertTrue(sp.ask(sp.Q.real(y)))
            self.assertIsNone(sp.ask(nonnegative))
        self.assertIsNone(sp.ask(positive))

    def test_scoped_assumptions_reject_foreign_expression(self):
        with fortsym.Arena() as arena:
            foreign = arena.symbol("foreign_scope")
            with self.assertRaises(ValueError):
                with sp.assuming(sp.Q.positive(foreign)):
                    pass
            with self.assertRaises(ValueError):
                sp.refine(sp.sqrt(sp.Symbol("foreign_target")**2), foreign > 0)

    def test_scoped_assumptions_restore_after_exception(self):
        x = sp.Symbol("exception_scope")
        fact = sp.Q.positive(x)
        with self.assertRaisesRegex(RuntimeError, "scope body"):
            with sp.assuming(fact):
                self.assertTrue(sp.ask(fact))
                raise RuntimeError("scope body")
        self.assertIsNone(sp.ask(fact))

    def test_refine_uses_native_scoped_assumptions(self):
        x = sp.Symbol("refine_x")
        self.assertEqual(sp.refine(sp.sqrt(x**2), sp.Q.positive(x)), x)
        self.assertEqual(sp.refine(sp.sqrt(x**2), sp.Q.real(x)), sp.Abs(x))
        self.assertIsNone(sp.ask(sp.Q.positive(x)))

    def test_relational_facts_use_bounded_native_ingestion(self):
        x = sp.Symbol("relation_x")
        self.assertEqual(str(sp.Gt(x, 1)), "relation_x > 1")
        self.assertEqual(sp.refine(sp.sqrt(x**2), x > 1), x)
        self.assertEqual(sp.refine(sp.sqrt(x**2), x >= 0), x)
        with sp.assuming(sp.Ne(x, 0)):
            self.assertTrue(sp.ask(sp.Q.nonzero(x)))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.refine(sp.sqrt(x**2), x < 1)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.refine(sp.sqrt(x**2), x > -1)

    def test_compound_assumptions_are_transactional_and_infer(self):
        x = sp.Symbol("compound_x")
        supported = sp.And(x > 1, sp.Ne(x, 0))
        with sp.assuming(supported):
            self.assertTrue(sp.ask(sp.Q.positive(x)))
            self.assertTrue(sp.ask(sp.Q.nonzero(x)))
            self.assertEqual(sp.refine(sp.sqrt(x**2), supported), x)
        self.assertIsNone(sp.ask(sp.Q.positive(x)))

        contradictory = sp.And(x > 1, x < 0)
        with self.assertRaises(sp.InconsistentAssumptions):
            with sp.assuming(contradictory):
                pass
        self.assertIsNone(sp.ask(sp.Q.positive(x)))
        with self.assertRaises(sp.InconsistentAssumptions):
            sp.Symbol("contradictory_symbol", positive=True, negative=True)


if __name__ == "__main__":
    unittest.main()
