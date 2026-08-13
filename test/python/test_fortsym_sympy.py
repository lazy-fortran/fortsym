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
    def test_typed_geometry_metadata_facade(self):
        signature = sp.Signature((-1, 1, 1))
        orientation = sp.Orientation(-1)
        self.assertTrue(signature.is_lorentzian)
        self.assertEqual((signature.positive_count, signature.negative_count), (2, 1))
        self.assertEqual(orientation.value, -1)

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

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_simplify_cache_matches_sympy(self):
        x = sp.Symbol("simplify_cache_x")
        expression = sp.sqrt(x**2)
        first = expression.simplify()
        second = expression.simplify()
        self.assertIs(first, second)
        ox = oracle.Symbol("simplify_cache_x")
        expected = oracle.simplify(oracle.sqrt(ox**2))
        self.assertEqual(oracle.sympify(str(first)), expected)
        with sp.assuming(sp.Q.positive(x)):
            refined = expression.simplify()
            self.assertIsNot(refined, first)
            self.assertEqual(str(refined), "simplify_cache_x")

    def test_geometry_classes_are_reexported(self):
        x, y, z = sp.symbols("geometry_x geometry_y geometry_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        surface = chart.flux_surface(1)
        self.assertIsInstance(surface, sp.FluxSurface)
        self.assertEqual(surface.label, x)
        self.assertEqual(surface.average(1 + sp.sin(y) + sp.cos(z)).simplify(), 1)
        metric = chart.metric()
        self.assertIsInstance(metric, sp.Tensor)
        self.assertEqual(metric.variance, (-1, -1))
        translation_killing = chart.killing(chart.vector((0, 0, 1)))
        for row in range(3):
            for column in range(3):
                self.assertEqual(translation_killing[row, column].simplify(), 0)

        explicit_metric = chart.metric_owner(
            ((1, 0, 0), (0, 1 + x**2, 0), (0, 0, 1)),
        )
        explicit_killing = explicit_metric.killing(chart.vector((0, 0, 1)))
        for row in range(3):
            for column in range(3):
                self.assertEqual(explicit_killing[row, column].simplify(), 0)
        non_killing = explicit_metric.killing(chart.vector((1, 0, 0)))
        self.assertEqual(non_killing[1, 1].simplify(), 2*x)

        rho, theta, zeta = sp.symbols("geometry_rho geometry_theta geometry_zeta")
        parameter = sp.Symbol("geometry_lambda")
        radius = sp.Symbol("geometry_R0")
        cylindrical = sp.Chart(
            (rho, theta, zeta),
            (rho*sp.cos(theta), rho*sp.sin(theta), zeta),
        )
        residual = cylindrical.geodesic_residual(
            (radius, parameter, 0), parameter
        )
        self.assertEqual((residual[0] + radius).simplify(), 0)
        self.assertEqual(residual[1].simplify(), 0)
        self.assertEqual(residual[2].simplify(), 0)
        self.assertEqual(metric[0, 0].simplify(), 1)
        self.assertEqual(chart.scalar_curvature().simplify(), 0)
        self.assertEqual(chart.first_bianchi_residual()[0, 1, 0, 1].simplify(), 0)
        self.assertEqual(
            chart.second_bianchi_residual()[0, 1, 0, 1, 2].simplify(), 0
        )
        form = chart.one_form((y, z, x))
        self.assertIsInstance(form, sp.Form)
        self.assertEqual(form.d().degree, 2)
        mixed = chart.tensor(tuple(range(1, 10)), variance=(1, -1))
        self.assertEqual(mixed.trace(0, 1).component().simplify(), 15)
        self.assertEqual(sp.tensorcontraction(mixed, (0, 1)).component().simplify(), 15)
        product = sp.tensorproduct(chart.vector((1, 2, 3)), chart.covector((4, 5, 6)))
        self.assertEqual(product.variance, (1, -1))
        self.assertEqual(product[0, 0].simplify(), 4)
        density_vector = chart.vector((x, y, z), density_weight=1)
        self.assertEqual(
            density_vector.covariant_divergence().component().simplify(), 3
        )
        component_density = chart.vector((x, y, z)).density(x + 2)
        self.assertEqual(component_density.density_weight, 1)
        self.assertEqual(component_density[0].simplify(), (x + 2)*x)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_native_three_dimensional_forms_match_sympy(self):
        x, y, z = sp.symbols("form3_x form3_y form3_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        one_form = chart.one_form((x, y, z))
        scalar_form = chart.scalar_form(x**2 + y**2 + z**2)
        ox, oy, oz = oracle.symbols("form3_x form3_y form3_z")
        oracle_coordinates = (ox, oy, oz)
        expected_codiff = -sum(oracle.diff(value, coordinate)
                                for value, coordinate in zip(
                                    (ox, oy, oz), oracle_coordinates))
        expected_laplace = -sum(
            oracle.diff(ox**2 + oy**2 + oz**2, coordinate, 2)
            for coordinate in oracle_coordinates
        )

        actual_codiff = oracle.sympify(str(one_form.codiff()[0].simplify()))
        actual_laplace = oracle.sympify(
            str(scalar_form.laplace_de_rham()[0].simplify())
        )
        self.assertEqual(oracle.simplify(actual_codiff - expected_codiff), 0)
        self.assertEqual(oracle.simplify(actual_laplace - expected_laplace), 0)
        self.assertEqual(
            (one_form.codifferential()[0] - one_form.codiff()[0]).simplify(), 0
        )

        metric = chart.metric_owner(
            ((1, 0, 0), (0, 1, 0), (0, 0, 1)),
        )
        metric_codiff = oracle.sympify(
            str(one_form.codifferential(metric)[0].simplify())
        )
        metric_laplace = oracle.sympify(
            str(scalar_form.laplace_de_rham(metric)[0].simplify())
        )
        self.assertEqual(oracle.simplify(metric_codiff - expected_codiff), 0)
        self.assertEqual(oracle.simplify(metric_laplace - expected_laplace), 0)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_form_tensor_roundtrip_matches_sympy(self):
        x, y, z = sp.symbols("form_tensor_x form_tensor_y form_tensor_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        source = chart.two_form((x**2 + y, x*y, z + 1))

        tensor = source.to_tensor()
        self.assertEqual(tensor.variance, (-1, -1))
        ox, oy, oz = oracle.symbols(
            "form_tensor_x form_tensor_y form_tensor_z"
        )
        self.assertEqual(
            oracle.sympify(str(tensor[0, 1].simplify())), ox**2 + oy
        )
        self.assertEqual(
            oracle.sympify(str(tensor[1, 0].simplify())), -(ox**2 + oy)
        )
        roundtrip = tensor.to_form()
        for mask, expected in (
            (3, ox**2 + oy), (5, ox*oy), (6, oz + 1)
        ):
            actual = oracle.sympify(str(roundtrip[mask].simplify()))
            self.assertEqual(actual, expected)

        nonsymmetric = chart.tensor(
            (1, 0, 0, 0, 0, 0, 0, 0, 0), variance=(-1, -1)
        )
        with self.assertRaises(fortsym.FortSymError):
            nonsymmetric.to_form()
        with self.assertRaises(fortsym.FortSymError):
            tensor.density(1).to_form()

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_tensor_symmetry_metadata_matches_sympy_vocabulary(self):
        from sympy.tensor.tensor import TensorSymmetry as OracleTensorSymmetry

        x, y, z = sp.symbols("tensor_symmetry_x tensor_symmetry_y tensor_symmetry_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        descriptor = sp.TensorSymmetry.pair_symmetric(2, 0, 1)
        oracle_descriptor = OracleTensorSymmetry.fully_symmetric(2)
        self.assertEqual(descriptor.rank, oracle_descriptor.rank)

        matrix = chart.tensor(
            (1, 2, 3, 2, 4, 5, 3, 5, 6), variance=(1, 1),
            symmetries=descriptor,
        )
        self.assertEqual(matrix.symmetry(0, 1), sp.SYMMETRIC)
        self.assertEqual(matrix.symmetries, ((0, 1, sp.SYMMETRIC),))
        self.assertEqual(
            oracle.Matrix(
                3, 3,
                lambda row, column: oracle.sympify(str(matrix[row, column])),
            ),
            oracle.Matrix(((1, 2, 3), (2, 4, 5), (3, 5, 6))),
        )
        self.assertEqual(matrix.permute((1, 0)).symmetry(0, 1), sp.SYMMETRIC)
        self.assertEqual(matrix.lower(0).symmetry(0, 1), sp.SYMMETRY_NONE)

        product = matrix.product(chart.vector((x, y, z)))
        self.assertEqual(product.symmetry(0, 1), sp.SYMMETRIC)
        self.assertEqual(product.covariant_diff().symmetry(0, 1), sp.SYMMETRIC)

        form_tensor = chart.two_form((x, y, z)).to_tensor()
        self.assertEqual(form_tensor.symmetry(0, 1), sp.ANTISYMMETRIC)
        false_declaration = chart.tensor(
            (1, 2, 3, 4, 5, 6, 7, 8, 9), variance=(1, 1)
        )
        with self.assertRaises(fortsym.FortSymError):
            false_declaration.declare_symmetry(0, 1, sp.SYMMETRIC)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_flux_coordinate_residuals_match_sympy(self):
        psi, theta, phi = sp.symbols("flux_owner_psi flux_owner_theta flux_owner_phi")
        i0, i1, g0, g1 = sp.symbols(
            "flux_owner_I0 flux_owner_I1 flux_owner_G0 flux_owner_G1"
        )
        chart = sp.Chart((psi, theta, phi), (psi, theta, phi))
        owner = chart.flux_coordinates(1, kind=sp.FLUX_BOOZER)
        self.assertIsInstance(owner, sp.FluxCoordinates)
        self.assertEqual(owner.angle_indices, (2, 3))
        self.assertEqual(owner.kind_name, "boozer")

        i_flux = i0 + i1*psi
        g_flux = g0 + g1*psi
        covariant = chart.covector((0, i_flux, g_flux))
        residuals = owner.boozer_residuals(covariant)
        self.assertEqual(
            tuple(oracle.sympify(str(value.simplify())) for value in residuals),
            (0, 0, 0, 0, 0),
        )
        self.assertTrue(owner.boozer_valid(covariant))

        bad = chart.covector((0, i_flux + theta, g_flux))
        self.assertEqual(
            oracle.sympify(str(owner.boozer_residuals(bad)[1].simplify())), 1
        )
        self.assertFalse(owner.boozer_valid(bad))
        self.assertEqual(
            oracle.sympify(str(owner.normal(chart.vector((0, i_flux, g_flux))).simplify())),
            0,
        )
        straight = owner.straight_field_residual(
            chart.vector((0, sp.Symbol("flux_owner_iota"), 1)),
            sp.Symbol("flux_owner_iota"),
        )
        self.assertEqual(oracle.sympify(str(straight.simplify())), 0)

        hamada = chart.flux_coordinates(1, kind=sp.FLUX_HAMADA)
        contravariant = chart.vector((0, i_flux, g_flux))
        self.assertEqual(
            tuple(oracle.sympify(str(value.simplify()))
                  for value in hamada.hamada_residuals(contravariant)),
            (0, 0, 0, 0, 0),
        )
        self.assertTrue(hamada.hamada_valid(contravariant))
        bad_hamada = chart.vector((0, i_flux + theta, g_flux))
        self.assertEqual(
            oracle.sympify(str(hamada.hamada_residuals(bad_hamada)[1].simplify())),
            1,
        )
        self.assertFalse(hamada.hamada_valid(bad_hamada))

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_flux_surface_average_matches_sympy(self):
        x, y, z = sp.symbols("average_x average_y average_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        actual = chart.flux_surface(1).average(1 + sp.sin(y) + sp.cos(z))
        ox, oy, oz = oracle.symbols("average_x average_y average_z")
        integrand = 1 + oracle.sin(oy) + oracle.cos(oz)
        expected = oracle.integrate(
            oracle.integrate(integrand, (oy, 0, 2*oracle.pi)),
            (oz, 0, 2*oracle.pi),
        ) / (2*oracle.pi)**2
        self.assertEqual(oracle.sympify(str(actual.simplify())), expected)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_magnetic_chart_matches_sympy_curl(self):
        psi, theta, phi = sp.symbols("magnetic_psi magnetic_theta magnetic_phi")
        chart = sp.Chart((psi, theta, phi), (psi, theta, phi))
        owner = chart.magnetic_chart((sp.Integer(0), psi, sp.Integer(0)), label_index=1)
        self.assertIsInstance(owner, sp.MagneticChart)
        actual = tuple(oracle.sympify(str(value.simplify())) for value in owner.upper)
        o_psi, o_theta, o_phi = oracle.symbols(
            "magnetic_psi magnetic_theta magnetic_phi"
        )
        potential = (0, o_psi, 0)
        expected = (
            oracle.diff(potential[2], o_theta) - oracle.diff(potential[1], o_phi),
            oracle.diff(potential[0], o_phi) - oracle.diff(potential[2], o_psi),
            oracle.diff(potential[1], o_psi) - oracle.diff(potential[0], o_theta),
        )
        self.assertEqual(actual, expected)
        self.assertEqual(oracle.sympify(str(owner.divergence().simplify())), 0)
        flux_form = owner.flux_form()
        self.assertEqual(oracle.sympify(str(flux_form[3].simplify())), 1)
        self.assertTrue(flux_form.is_closed)
        recovered = flux_form.b_con()
        recovered_actual = tuple(
            oracle.sympify(str(recovered[index].simplify()))
            for index in range(3)
        )
        self.assertEqual(recovered_actual, expected)
        recovered_density = flux_form.b_density()
        self.assertEqual(recovered_density.variance, (1,))
        self.assertEqual(recovered_density.density_weight, 1)
        self.assertEqual(
            tuple(
                oracle.sympify(str(recovered_density[index].simplify()))
                for index in range(3)
            ),
            tuple(
                oracle.sympify(str(owner.density[index].simplify()))
                for index in range(3)
            ),
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_tensor_lie_derivative_matches_independent_sympy_formula(self):
        x, y, z = sp.symbols("lie_x lie_y lie_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        vector = chart.vector((x, y, 0))
        upper = chart.vector((x**2, x*y, z))
        lower = chart.covector((x*y, z, 0))

        actual_upper = upper.lie(vector)
        actual_lower = lower.lie(vector)
        oracle_coordinates = oracle.symbols("lie_x lie_y lie_z")
        oracle_vector = (oracle_coordinates[0], oracle_coordinates[1], 0)
        oracle_upper = (
            oracle_coordinates[0]**2,
            oracle_coordinates[0]*oracle_coordinates[1],
            oracle_coordinates[2],
        )
        oracle_lower = (
            oracle_coordinates[0]*oracle_coordinates[1],
            oracle_coordinates[2],
            0,
        )
        expected_upper = tuple(
            sum(
                oracle_vector[k] * oracle.diff(oracle_upper[i], oracle_coordinates[k])
                - oracle_upper[k] * oracle.diff(oracle_vector[i], oracle_coordinates[k])
                for k in range(3)
            )
            for i in range(3)
        )
        expected_lower = tuple(
            sum(
                oracle_vector[k] * oracle.diff(oracle_lower[i], oracle_coordinates[k])
                + oracle_lower[k] * oracle.diff(oracle_vector[k], oracle_coordinates[i])
                for k in range(3)
            )
            for i in range(3)
        )
        self.assertEqual(
            tuple(oracle.sympify(str(actual_upper[i].simplify())) for i in range(3)),
            expected_upper,
        )
        self.assertEqual(
            tuple(oracle.sympify(str(actual_lower[i].simplify())) for i in range(3)),
            expected_lower,
        )

        density = chart.scalar(x).density(1)
        actual_density = chart.lie(vector, density)
        self.assertEqual(
            oracle.sympify(str(actual_density.component().simplify())),
            3 * oracle_coordinates[0],
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_supplied_connection_matches_independent_sympy_formula(self):
        x, y, z = sp.symbols("connection_x connection_y connection_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        gamma = [[[0, 0, 0] for _ in range(3)] for _ in range(3)]
        gamma[0][0][1] = x
        gamma[0][1][0] = 2*y
        connection = chart.connection(gamma)
        metric = chart.metric_owner(((1, 0, 0), (0, 1, 0), (0, 0, 1)))
        vector = chart.vector((x, y, z))

        ox, oy, oz = oracle.symbols(
            "connection_x connection_y connection_z"
        )
        coordinates = (ox, oy, oz)
        oracle_gamma = [[[0 for _ in range(3)] for _ in range(3)]
                        for _ in range(3)]
        oracle_gamma[0][0][1] = ox
        oracle_gamma[0][1][0] = 2*oy
        oracle_vector = (ox, oy, oz)
        expected_torsion = tuple(
            oracle_gamma[a][b][c] - oracle_gamma[a][c][b]
            for c in range(3) for b in range(3) for a in range(3)
        )
        actual_torsion = tuple(
            oracle.sympify(str(value.simplify()))
            for value in connection.torsion()
        )
        self.assertEqual(actual_torsion, tuple(expected_torsion))

        # R^1_212 = d_1 Gamma^1_22 - d_2 Gamma^1_12
        #             + Gamma^1_1m Gamma^m_22 - Gamma^1_2m Gamma^m_12.
        actual_riemann = oracle.sympify(
            str(connection.riemann()[0, 1, 0, 1].simplify())
        )
        self.assertEqual(actual_riemann, -2*ox*oy)

        opposite = chart.connection(
            gamma, convention=sp.CONNECTION_OPPOSITE
        )
        actual_opposite = oracle.sympify(
            str(opposite.riemann()[0, 1, 0, 1].simplify())
        )
        self.assertEqual(actual_opposite, 2*ox*oy)

        parameter = sp.Symbol("connection_lambda")
        actual_geodesic = tuple(
            oracle.sympify(str(value.simplify()))
            for value in connection.geodesic_residual(
                (parameter, parameter, 0), parameter
            )
        )
        olambda = oracle.Symbol("connection_lambda")
        self.assertEqual(actual_geodesic, (3*olambda, 0, 0))

        expected_nonmetricity = []
        for k in range(3):
            for j in range(3):
                for i in range(3):
                    expected_nonmetricity.append(sum(
                        oracle_gamma[m][k][i] * (1 if m == j else 0)
                        + oracle_gamma[m][k][j] * (1 if i == m else 0)
                        for m in range(3)
                    ))
        actual_nonmetricity = tuple(
            oracle.sympify(str(value.simplify()))
            for value in connection.nonmetricity(metric)
        )
        self.assertEqual(actual_nonmetricity, tuple(expected_nonmetricity))

        expected_derivative = []
        for k in range(3):
            for i in range(3):
                expected_derivative.append(
                    oracle.diff(oracle_vector[i], coordinates[k]) + sum(
                        oracle_gamma[i][k][m] * oracle_vector[m]
                        for m in range(3)
                    )
                )
        actual_derivative = tuple(
            oracle.sympify(str(value.simplify()))
            for value in connection.covariant_diff(vector)
        )
        self.assertEqual(actual_derivative, tuple(expected_derivative))
        self.assertEqual(
            oracle.sympify(str(connection.covariant_divergence(vector).component().simplify())),
            sum(expected_derivative[3*i + i] for i in range(3)),
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_tensor_index_labels_contract_through_native_owner(self):
        x, y, z = sp.symbols("index_x index_y index_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        indices = sp.TensorIndexType("T", 3)
        upper = indices.index(0, "upper", "i", dummy=True)
        lower = indices.index(1, "lower", "i", dummy=True)
        mismatch = indices.index(1, "lower", "j", dummy=True)
        tensor = chart.tensor(tuple(range(1, 10)), variance=(1, -1))
        contracted = tensor.contract(upper, lower)
        expected = oracle.trace(oracle.Matrix(3, 3, lambda row, column: row + 3*column + 1))
        self.assertEqual(
            oracle.sympify(str(contracted.component().simplify())), expected
        )
        with self.assertRaisesRegex(ValueError, "compatible dummy pair"):
            tensor.contract(upper, mismatch)

    def test_explicit_lorentzian_metric_hodge_owner(self):
        t, x, y = sp.symbols("metric_t metric_x metric_y")
        chart = sp.Chart((t, x, y), (t, x, y))
        metric = chart.metric_owner(
            ((-1, 0, 0), (0, 1, 0), (0, 0, 1)),
            signature=(-1, 1, 1),
        )
        self.assertEqual(metric.sqrtg().simplify(), 1)
        inverse = metric.contravariant()
        self.assertEqual(inverse.variance, (-1, -1))
        self.assertEqual(inverse[0, 0].simplify(), -1)
        alpha = chart.one_form((1, 2, 3))
        self.assertEqual(
            tuple(value.simplify() for value in alpha.star(metric)),
            (0, 0, 0, 3, 0, -2, -1, 0),
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_metric_vector_calculus_matches_sympy(self):
        t, x, y = sp.symbols("calculus_t calculus_x calculus_y")
        chart = sp.Chart((t, x, y), (t, x, y))
        metric = chart.metric_owner(
            ((-1, 0, 0), (0, 1, 0), (0, 0, 1)),
            signature=(-1, 1, 1),
        )
        scalar = t + x + y
        gradient = metric.grad(scalar)
        vector = (t, x, y)
        oracle_metric = oracle.diag(-1, 1, 1)
        oracle_coordinates = oracle.symbols(
            "calculus_t calculus_x calculus_y"
        )
        oracle_scalar = sum(oracle_coordinates)
        expected_gradient = tuple(
            sum(
                oracle_metric[i, j] * oracle.diff(
                    oracle_scalar, oracle_coordinates[j]
                )
                for j in range(3)
            )
            for i in range(3)
        )
        actual_gradient = tuple(
            oracle.sympify(str(value.simplify())) for value in gradient
        )
        self.assertEqual(actual_gradient, expected_gradient)

        expected_divergence = sum(
            oracle.diff(value, coordinate)
            for value, coordinate in zip(oracle_coordinates, oracle_coordinates)
        )
        self.assertEqual(
            oracle.sympify(str(metric.divergence(vector).simplify())),
            expected_divergence,
        )
        self.assertEqual(
            oracle.sympify(str(metric.laplacian(scalar).simplify())),
            oracle.Integer(0),
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_boozer_flux_functions_and_metric_form_match_sympy(self):
        psi, theta, phi = sp.symbols("boozer_psi boozer_theta boozer_phi")
        h0, epsilon = sp.symbols("boozer_h0 boozer_epsilon")
        i0, i1, g0, g1 = sp.symbols(
            "boozer_I0 boozer_I1 boozer_G0 boozer_G1"
        )
        h = h0*(1 + epsilon*sp.cos(theta))
        i_flux = i0 + i1*psi
        g_flux = g0 + g1*psi
        chart = sp.Chart((psi, theta, phi), (psi, theta, phi))
        metric = chart.metric_owner(
            ((1, 0, 0), (0, h**2, 0), (0, 0, h**2)),
        )
        inverse = metric.contravariant()

        oracle_psi, oracle_theta, oracle_phi = oracle.symbols(
            "boozer_psi boozer_theta boozer_phi"
        )
        oracle_h0, oracle_epsilon = oracle.symbols(
            "boozer_h0 boozer_epsilon"
        )
        oracle_i0, oracle_i1, oracle_g0, oracle_g1 = oracle.symbols(
            "boozer_I0 boozer_I1 boozer_G0 boozer_G1"
        )
        oracle_h = oracle_h0*(1 + oracle_epsilon*oracle.cos(oracle_theta))
        expected_metric = oracle.diag(1, oracle_h**2, oracle_h**2)
        actual_metric = oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
            str(metric.components[row + 3*column].simplify())
        ))
        self.assertEqual(actual_metric, expected_metric)
        expected_inverse = expected_metric.inv()
        actual_inverse = oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
            str(inverse[row, column].simplify())
        ))
        for row in range(3):
            for column in range(3):
                self.assertEqual(
                    oracle.simplify(
                        actual_inverse[row, column] - expected_inverse[row, column]
                    ),
                    0,
                )
        actual_volume_squared = oracle.sympify(
            str(metric.volume_density().simplify())
        )**2
        self.assertEqual(
            oracle.simplify(
                actual_volume_squared - oracle.Abs(expected_metric.det())
            ),
            0,
        )

        expected_i = oracle_i0 + oracle_i1*oracle_psi
        expected_g = oracle_g0 + oracle_g1*oracle_psi
        actual_i = sp.diff(i_flux, theta).simplify()
        actual_g = sp.diff(g_flux, theta).simplify()
        self.assertEqual(
            oracle.sympify(str(actual_i)), oracle.diff(expected_i, oracle_theta)
        )
        self.assertEqual(
            oracle.sympify(str(actual_g)), oracle.diff(expected_g, oracle_theta)
        )
        self.assertEqual(
            oracle.sympify(str(sp.diff(i_flux, phi).simplify())),
            oracle.diff(expected_i, oracle_phi),
        )
        self.assertEqual(
            oracle.sympify(str(sp.diff(g_flux, phi).simplify())),
            oracle.diff(expected_g, oracle_phi),
        )
        expected_upper = oracle.Matrix((
            0, expected_i/oracle_h**2, expected_g/oracle_h**2,
        ))
        actual_upper = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in (sp.Integer(0), i_flux/h**2, g_flux/h**2)
        ))
        self.assertEqual(actual_upper, expected_upper)
        expected_flux_two_form = oracle.Matrix((expected_g, -expected_i, 0))
        actual_flux_two_form = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in (g_flux, -i_flux, sp.Integer(0))
        ))
        self.assertEqual(actual_flux_two_form, expected_flux_two_form)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_spacetime_metric_forms_and_wave_operator_match_sympy(self):
        t, x, y, z = sp.symbols("spacetime_t spacetime_x spacetime_y spacetime_z")
        coordinates = (t, x, y, z)
        metric = sp.SpacetimeMetric(
            coordinates,
            ((-1, 0, 0, 0), (0, 1, 0, 0),
             (0, 0, 1, 0), (0, 0, 0, 1)),
            signature=(-1, 1, 1, 1),
        )
        vector = coordinates
        covector = metric.flat(vector)
        self.assertEqual(covector.degree, 1)
        oracle_coordinates = oracle.symbols(
            "spacetime_t spacetime_x spacetime_y spacetime_z"
        )
        expected_covector = (-oracle_coordinates[0],) + oracle_coordinates[1:]
        self.assertEqual(
            tuple(
                oracle.sympify(str(covector[1 << index].simplify()))
                for index in range(4)
            ),
            expected_covector,
        )
        raised = metric.sharp(covector)
        self.assertEqual(raised.variance, (1,))
        self.assertEqual(
            tuple(
                oracle.sympify(str(value.simplify())) for value in raised
            ),
            oracle_coordinates,
        )

        scalar = sum(coordinates)
        oracle_metric = oracle.diag(-1, 1, 1, 1)
        oracle_scalar = sum(oracle_coordinates)
        expected_gradient = tuple(
            sum(
                oracle_metric[i, j] * oracle.diff(
                    oracle_scalar, oracle_coordinates[j]
                )
                for j in range(4)
            )
            for i in range(4)
        )
        actual_gradient = tuple(
            oracle.sympify(str(metric.grad(scalar)[index].simplify()))
            for index in range(4)
        )
        self.assertEqual(actual_gradient, expected_gradient)
        expected_divergence = sum(
            oracle.diff(value, coordinate)
            for value, coordinate in zip(oracle_coordinates, oracle_coordinates)
        )
        self.assertEqual(
            oracle.sympify(str(metric.divergence(vector).simplify())),
            expected_divergence,
        )
        self.assertEqual(
            oracle.sympify(str(metric.laplacian(scalar).simplify())),
            oracle.Integer(0),
        )

        hubble = sp.Symbol("spacetime_H")
        scale = sp.exp(hubble*t)
        curved = sp.SpacetimeMetric(
            coordinates,
            ((-1, 0, 0, 0), (0, scale**2, 0, 0),
             (0, 0, scale**2, 0), (0, 0, 0, scale**2)),
            signature=(-1, 1, 1, 1),
        )
        oracle_hubble = oracle.Symbol("spacetime_H")
        self.assertEqual(
            oracle.sympify(str(curved.laplacian(t).simplify())),
            -3*oracle_hubble,
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_de_sitter_and_gps_records_match_sympy(self):
        t, x, y, z = sp.symbols("record_t record_x record_y record_z")
        hubble = sp.Symbol("record_H")
        scale = sp.exp(hubble*t)
        curved = sp.SpacetimeMetric(
            (t, x, y, z),
            ((-1, 0, 0, 0), (0, scale**2, 0, 0),
             (0, 0, scale**2, 0), (0, 0, 0, scale**2)),
            signature=(-1, 1, 1, 1),
        )
        oracle_coordinates = oracle.symbols(
            "record_t record_x record_y record_z"
        )
        oracle_hubble = oracle.Symbol("record_H")
        oracle_metric = oracle.diag(
            -1,
            oracle.exp(oracle_hubble*oracle_coordinates[0])**2,
            oracle.exp(oracle_hubble*oracle_coordinates[0])**2,
            oracle.exp(oracle_hubble*oracle_coordinates[0])**2,
        )
        einstein = curved.einstein()
        expected_einstein = -3*oracle_hubble**2*oracle_metric
        for row in range(4):
            for column in range(4):
                actual = oracle.sympify(
                    str(einstein[row, column].simplify())
                )
                self.assertEqual(
                    oracle.simplify(actual - expected_einstein[row, column]),
                    0,
                )
        self.assertEqual(
            oracle.sympify(str(curved.scalar_curvature().simplify())),
            12*oracle_hubble**2,
        )
        self.assertEqual(
            oracle.sympify(str(curved.laplacian(t).simplify())),
            -3*oracle_hubble,
        )

        radius, theta, phi = sp.symbols(
            "record_radius record_theta record_phi"
        )
        mu = sp.Symbol("record_mu")
        potential = -mu/radius
        weak = sp.SpacetimeMetric(
            (t, radius, theta, phi),
            ((-(1 + 2*potential), 0, 0, 0), (0, 1, 0, 0),
             (0, 0, 1, 0), (0, 0, 0, 1)),
            signature=(-1, 1, 1, 1),
        )
        christoffel = weak.christoffel()
        oracle_radius, oracle_mu = oracle.symbols("record_radius record_mu")
        expected_derivative = oracle.diff(-oracle_mu/oracle_radius, oracle_radius)
        actual_derivative = oracle.sympify(
            str(christoffel[1, 0, 0].simplify())
        )
        self.assertEqual(
            oracle.simplify(actual_derivative - expected_derivative), 0
        )
        self.assertEqual(
            oracle.simplify(-expected_derivative + oracle_mu/oracle_radius**2),
            0,
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_spacetime_tensor_variance_and_density_match_sympy(self):
        t, x, y, z = sp.symbols("tensor_oracle_t tensor_oracle_x tensor_oracle_y tensor_oracle_z")
        metric = sp.SpacetimeMetric(
            (t, x, y, z),
            ((2, 1, 0, 0), (1, 1, 0, 0),
             (0, 0, 0, 0), (0, 0, 0, 0)),
            dimension=2,
            signature=(1, 1, 1, 1),
        )
        upper = metric.vector((t, x, 0, 0))
        lower = upper.lower()
        roundtrip = lower.raise_()
        oracle_metric = oracle.Matrix(((2, 1), (1, 1)))
        oracle_vector = oracle.Matrix((oracle.Symbol("tensor_oracle_t"),
                                       oracle.Symbol("tensor_oracle_x")))
        expected_lower = oracle_metric * oracle_vector
        self.assertEqual(
            tuple(oracle.sympify(str(lower[index].simplify())) for index in (0, 1)),
            tuple(expected_lower),
        )
        self.assertEqual(
            tuple(oracle.sympify(str(roundtrip[index].simplify())) for index in (0, 1)),
            tuple(oracle_vector),
        )
        expected_inverse = oracle_metric.inv()
        actual_inverse = metric.contravariant()
        self.assertEqual(
            oracle.Matrix(tuple(
                oracle.sympify(str(actual_inverse[i, j].simplify()))
                for i in range(2) for j in range(2)
            )).reshape(2, 2), expected_inverse,
        )
        density = upper.density(metric.sqrtg())
        self.assertEqual(density.density_weight, 1)
        self.assertEqual(
            tuple(oracle.sympify(str(density[index].simplify()))
                  for index in (0, 1)),
            tuple(oracle.sqrt(oracle_metric.det()) * oracle_vector),
        )
        product = sp.tensorproduct(upper, lower)
        expected_product = oracle_vector * expected_lower.T
        self.assertEqual(
            oracle.Matrix(tuple(
                oracle.sympify(str(product[i, j].simplify()))
                for i in range(2) for j in range(2)
            )).reshape(2, 2), expected_product,
        )
        contracted = sp.tensorcontraction(product, (0, 1))
        self.assertEqual(
            oracle.sympify(str(contracted[()].simplify())),
            (oracle_vector.T * expected_lower)[0],
        )
        permuted = sp.tensorpermute(product, (1, 0))
        self.assertEqual(
            tuple(oracle.sympify(str(permuted[i, j].simplify()))
                  for i, j in ((0, 1), (1, 0))),
            tuple(oracle.sympify(str(product[j, i].simplify()))
                  for i, j in ((0, 1), (1, 0))),
        )
        curved_metric = sp.SpacetimeMetric(
            (t, x, y, z),
            ((1, 0, 0, 0), (0, sp.exp(2*t), 0, 0),
             (0, 0, 0, 0), (0, 0, 0, 0)),
            dimension=2,
            signature=(1, 1, 1, 1),
        )
        curved_vector = curved_metric.vector((0, 1, 0, 0))
        derivative = curved_vector.covariant_diff()
        self.assertEqual(
            oracle.sympify(str(derivative[1, 0].simplify())), 1
        )
        self.assertEqual(
            oracle.sympify(str(derivative[0, 1].simplify())),
            -oracle.exp(2*oracle.Symbol("tensor_oracle_t")),
        )
        self.assertEqual(
            oracle.sympify(str(derivative[1, 1].simplify())), 0
        )
        metric_derivative = curved_metric.covariant().covariant_diff()
        self.assertEqual(
            oracle.sympify(str(metric_derivative[1, 1, 0].simplify())), 0
        )
        killing = curved_metric.killing(curved_vector)
        self.assertEqual(oracle.sympify(str(killing[1, 1].simplify())), 0)
        dilation = curved_metric.vector((t, 0, 0, 0))
        lie_metric = curved_metric.lie(dilation, curved_metric.covariant())
        tensor_t = oracle.Symbol("tensor_oracle_t")
        self.assertEqual(
            oracle.sympify(str(lie_metric[0, 0].simplify())), 2
        )
        self.assertEqual(
            oracle.sympify(str(lie_metric[1, 1].simplify())),
            2*tensor_t*oracle.exp(2*tensor_t),
        )
        self.assertEqual(
            oracle.sympify(str(curved_metric.lie(
                dilation, curved_metric.scalar(t)
            )[()].simplify())), tensor_t
        )
        self.assertEqual(
            oracle.sympify(str(curved_metric.lie(
                dilation, curved_metric.scalar(1, density_weight=1)
            )[()].simplify())), 1
        )
        self.assertEqual(
            oracle.sympify(str(curved_vector.covariant_divergence()[()].simplify())),
            0,
        )
        self.assertEqual(
            oracle.sympify(str(
                dilation.covariant_divergence()[()].simplify()
            )), 1 + tensor_t,
        )
        density_vector = curved_metric.vector(
            (t, 0, 0, 0), density_weight=1
        )
        self.assertEqual(
            oracle.sympify(str(density_vector.divergence()[()].simplify())), 1
        )
        self.assertEqual(
            oracle.sympify(str(
                curved_metric.contravariant().covariant_divergence()[1].simplify()
            )), 0,
        )
        curved_density = curved_metric.vector(
            (0, sp.exp(t), 0, 0), density_weight=1
        )
        density_derivative = curved_density.covariant_diff()
        self.assertEqual(
            oracle.sympify(str(density_derivative[1, 0].simplify())),
            oracle.exp(oracle.Symbol("tensor_oracle_t")),
        )
        zero = sp.Integer(0)
        rank_four_components = [zero] * (fortsym.SPACETIME_DIM ** 4)
        rank_four_components[0] = t
        rank_four = sp.SpacetimeTensor(
            metric, rank_four_components, (4, 4, 4, 4),
            variance=(1, -1, 1, -1),
        )
        rank_five = rank_four.covariant_diff()
        self.assertEqual(rank_five.rank, 5)
        self.assertEqual(
            oracle.sympify(str(rank_five[0, 0, 0, 0, 0].simplify())), 1
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_metric_volume_and_levi_civita_match_sympy(self):
        x, y, z = sp.symbols("volume_x volume_y volume_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        metric = chart.metric_owner(
            ((1, 0, 0), (0, 4, 0), (0, 0, 9)),
            orientation=-1,
        )
        oracle_metric = oracle.diag(1, 4, 9)
        expected_volume = oracle.sqrt(oracle_metric.det())
        self.assertEqual(
            oracle.sympify(str(metric.volume_density().simplify())),
            expected_volume,
        )
        lower = metric.levi_civita()
        upper = metric.levi_civita("contravariant")
        self.assertEqual(lower.variance, (-1, -1, -1))
        self.assertEqual(upper.variance, (1, 1, 1))
        self.assertEqual(
            oracle.sympify(str(lower[0, 1, 2].simplify())), -expected_volume
        )
        self.assertEqual(
            oracle.sympify(str(lower[0, 2, 1].simplify())), expected_volume
        )
        expected_upper = -expected_volume / oracle_metric.det()
        self.assertEqual(
            oracle.sympify(str(upper[0, 1, 2].simplify())), expected_upper
        )
        self.assertEqual(
            oracle.sympify(str(metric.surface_measure(1).simplify())), 6
        )
        self.assertEqual(
            oracle.sympify(str(metric.surface_measure(2).simplify())), 3
        )
        self.assertEqual(
            oracle.sympify(str(metric.surface_measure(3).simplify())), 2
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_metric_inner_matches_sympy_on_nonorthogonal_metric(self):
        x, y, z = sp.symbols("inner_x inner_y inner_z")
        chart = sp.Chart((x, y, z), (x, y, z))
        metric = chart.metric_owner(
            ((2, 1, 0), (1, 3, 0), (0, 0, 4)),
        )
        left = (x, y, z)
        right = (1, 2, 3)
        oracle_metric = oracle.Matrix(((2, 1, 0), (1, 3, 0), (0, 0, 4)))
        oracle_left = oracle.Matrix(oracle.symbols("inner_x inner_y inner_z"))
        expected = (oracle_left.T * oracle_metric * oracle.Matrix(right))[0]
        actual = oracle.sympify(str(metric.inner(left, right).simplify()))
        self.assertEqual(actual, expected)
        expected_norm = (oracle_left.T * oracle_metric * oracle_left)[0]
        actual_norm = oracle.sympify(str(metric.norm_squared(left).simplify()))
        self.assertEqual(oracle.expand(actual_norm - expected_norm), 0)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_magnetic_constitutive_views_match_sympy(self):
        u, v, w = sp.symbols("h_u h_v h_w")
        chart = sp.Chart((u, v, w), (u + v, v, w))
        reluctivity = ((2, 1, 0), (0, 3, 0), (0, 0, 4))
        magnetic = (u, v, w)
        covariant = chart.h_cov(reluctivity, magnetic)
        contravariant = chart.h_con(covariant)
        oracle_u, oracle_v, oracle_w = oracle.symbols("h_u h_v h_w")
        oracle_b = oracle.Matrix((oracle_u, oracle_v, oracle_w))
        oracle_nu = oracle.Matrix(reluctivity)
        oracle_metric = oracle.Matrix(((1, 1, 0), (1, 2, 0), (0, 0, 1)))
        expected_covariant = oracle_nu * oracle_b
        expected_contravariant = oracle_metric.inv() * expected_covariant
        actual_covariant = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify())) for value in covariant
        ))
        actual_contravariant = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify())) for value in contravariant
        ))
        self.assertEqual(actual_covariant, expected_covariant)
        self.assertEqual(actual_contravariant, expected_contravariant)
        field = chart.magnetic_field((v*w, u**2, u*v))
        self.assertEqual(field.h_cov(reluctivity).variance, (-1,))
        self.assertEqual(field.h_con(reluctivity).variance, (1,))

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_spherical_minkowski_relativity_owner_matches_sympy(self):
        t, r, theta, phi = sp.symbols("rel_t rel_r rel_theta rel_phi")
        metric = sp.SpacetimeMetric(
            (t, r, theta, phi),
            ((-1, 0, 0, 0), (0, 1, 0, 0),
             (0, 0, r**2, 0), (0, 0, 0, r**2*sp.sin(theta)**2)),
            signature=(-1, 1, 1, 1),
        )
        inverse = metric.contravariant()
        self.assertEqual(inverse[0, 0].simplify(), -1)
        self.assertEqual((inverse[2, 2] - 1/r**2).simplify(), 0)
        christoffel = metric.christoffel()
        self.assertEqual((christoffel[1, 2, 2] + r).simplify(), 0)
        self.assertEqual(
            (christoffel[3, 2, 3] - sp.cos(theta)/sp.sin(theta)).simplify(),
            0,
        )
        ox, or_, ot, op = oracle.symbols("rel_t rel_r rel_theta rel_phi")
        oracle_metric = oracle.diag(
            -1, 1, or_**2, or_**2*oracle.sin(ot)**2
        )
        oracle_coordinates = (ox, or_, ot, op)
        oracle_inverse = oracle_metric.inv()

        def oracle_gamma(a, b, c):
            return oracle.simplify(sum(
                oracle_inverse[a, ell] * (
                    oracle.diff(oracle_metric[ell, c], oracle_coordinates[b])
                    + oracle.diff(oracle_metric[ell, b], oracle_coordinates[c])
                    - oracle.diff(oracle_metric[b, c], oracle_coordinates[ell])
                ) / 2
                for ell in range(4)
            ))

        expected_gamma_rtt = oracle_gamma(1, 2, 2)
        expected_gamma_phitp = oracle_gamma(3, 2, 3)
        self.assertEqual(
            oracle.simplify(
                oracle.sympify(str(christoffel[1, 2, 2].simplify()))
                - expected_gamma_rtt
            ),
            0,
        )
        self.assertEqual(
            oracle.simplify(
                oracle.sympify(str(christoffel[3, 2, 3].simplify()))
                - expected_gamma_phitp
            ),
            0,
        )
        self.assertEqual(metric.scalar_curvature().simplify(), 0)
        self.assertEqual(metric.ricci()[0, 0].simplify(), 0)
        self.assertEqual(metric.einstein()[3, 3].simplify(), 0)

        curved_2d = sp.SpacetimeMetric(
            (t, r, theta, phi),
            ((1, 0, 0, 0), (0, sp.exp(2*t), 0, 0),
             (0, 0, 0, 0), (0, 0, 0, 0)),
            dimension=2,
            signature=(1, 1, 1, 1),
        )
        self.assertEqual((curved_2d.scalar_curvature() + 2).simplify(), 0)
        self.assertEqual(curved_2d.contravariant()[2, 2].simplify(), 0)

        parameter = sp.Symbol("geodesic_lambda")
        curve = (parameter, 1, parameter, 0)
        residual = metric.geodesic_residual(curve, parameter)
        self.assertEqual((residual[1] + 1).simplify(), 0)
        self.assertEqual(residual[0].simplify(), 0)

    def test_native_spacetime_forms(self):
        t, x, y, z = sp.symbols("form_t form_x form_y form_z")
        metric = sp.SpacetimeMetric(
            (t, x, y, z),
            ((-1, 0, 0, 0), (0, 1, 0, 0),
             (0, 0, 1, 0), (0, 0, 0, 1)),
            signature=(-1, 1, 1, 1),
        )
        potential = metric.one_form((x*y, t*z, t*x, t + y))
        field = potential.d()
        self.assertTrue(field.is_closed)
        self.assertFalse(potential.is_closed)
        self.assertEqual(
            tuple(value.simplify() for value in potential.field_strength()),
            tuple(value.simplify() for value in field),
        )
        self.assertIsInstance(field, sp.SpacetimeForm)
        self.assertEqual((field[3] - (z - y)).simplify(), 0)
        closed = field.d()
        for mask in range(16):
            self.assertEqual(closed[mask].simplify(), 0)
        self.assertTrue(closed.is_closed)
        gauge = potential.gauge_transform(t*x)
        gauge_field = gauge.field_strength()
        for mask in range(16):
            self.assertEqual((gauge_field[mask] - field[mask]).simplify(), 0)
        source = field.star().d()
        residual = potential.maxwell_residual(source)
        for mask in range(16):
            if mask.bit_count() == 3:
                self.assertEqual(residual[mask].simplify(), 0)
        radial = metric.one_form((t, x, y, z))
        self.assertEqual((radial.codifferential()[0] + 2).simplify(), 0)
        self.assertEqual(
            (radial.codiff()[0] - radial.codifferential()[0]).simplify(), 0
        )
        scalar_form = metric.scalar_form(t**2 + x**2 + y**2 + z**2)
        self.assertEqual((scalar_form.laplace_de_rham()[0] + 4).simplify(), 0)

        two_form = metric.two_form((y, 2, 3, 4, 5, 6))
        self.assertFalse(two_form.is_closed)
        double_star = two_form.star().star()
        for mask in (3, 5, 6, 9, 10, 12):
            self.assertEqual((double_star[mask] + two_form[mask]).simplify(), 0)
        for mask in (3, 5, 6, 9, 10, 12):
            self.assertEqual(potential.wedge(potential)[mask].simplify(), 0)

        vector = (1, 0, 0, 0)
        test_field = metric.two_form((z - y, 0, t, 0, 0, 0))
        contraction = test_field.interior(vector)
        self.assertEqual((contraction[2] - (z - y)).simplify(), 0)
        lie_field = test_field.lie(vector)
        self.assertEqual((lie_field[6] - 1).simplify(), 0)
        self.assertEqual(
            (lie_field[6] - contraction.d()[6]).simplify(), 0
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_runtime_two_dimensional_spacetime_forms_match_sympy(self):
        t, x, y, z = sp.symbols("form2_t form2_x form2_y form2_z")
        metric = sp.SpacetimeMetric(
            (t, x, y, z),
            ((1, 0, 0, 0), (0, 1, 0, 0),
             (0, 0, 0, 0), (0, 0, 0, 0)),
            dimension=2,
            signature=(1, 1, 1, 1),
        )
        alpha = metric.one_form((x, t**2, 0, 0))
        field = alpha.d()
        self.assertEqual(field.degree, 2)
        self.assertEqual((field[3] - (2*t - 1)).simplify(), 0)
        self.assertTrue(field.is_closed)

        alpha_star_star = alpha.star().star()
        for mask in (1, 2):
            self.assertEqual(
                (alpha_star_star[mask] + alpha[mask]).simplify(), 0
            )

        scalar = metric.scalar_form(t**2)
        scalar_star = scalar.star()
        scalar_star_star = scalar_star.star()
        self.assertEqual(scalar_star.degree, 2)
        self.assertEqual(scalar_star_star.degree, 0)
        self.assertEqual((scalar_star_star[0] - t**2).simplify(), 0)
        self.assertEqual((scalar.laplace_de_rham()[0] + 2).simplify(), 0)

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
        tangent = expected_covariant[:, 1:]
        expected_surface = oracle.sqrt((tangent.T * tangent).det())
        self.assertEqual(
            oracle.sympify(str(chart.surface_measure(1).simplify())),
            expected_surface,
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
        matrix_values = tuple(range(1, 10))
        matrix = chart.tensor(matrix_values, variance=(1, 1))
        transposed = matrix.permute((1, 0))
        self.assertEqual(transposed.variance, (1, 1))
        self.assertEqual(
            oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
                str(transposed[row, column].simplify())
            )),
            oracle.Matrix(3, 3, lambda row, column:
                          matrix_values[column + 3*row]),
        )
        symmetric = matrix.symmetrize(0, 1)
        antisymmetric = matrix.antisymmetrize(0, 1)
        self.assertEqual(
            oracle.sympify(str(symmetric[0, 1].simplify())), 3
        )
        self.assertEqual(
            oracle.sympify(str(antisymmetric[0, 1].simplify())), 1
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
        expected_field_line = sum(
            vector*oracle.diff(scalar, coordinate)
            for vector, scalar, coordinate in zip(
                (oracle_v*oracle_w, oracle_u**2, oracle_u*oracle_v),
                (oracle_u + oracle_v*oracle_w,)*3,
                (oracle_u, oracle_v, oracle_w),
            )
        )
        actual_field_line = oracle.sympify(str(
            chart.field_line_derivative(
                (v*w, u**2, u*v), u + v*w
            ).simplify()
        ))
        self.assertEqual(actual_field_line, expected_field_line)
        density_vector = (u**2, u*v, sp.sin(w))
        oracle_density_vector = (
            oracle_u**2, oracle_u*oracle_v, oracle.sin(oracle_w)
        )
        expected_density_divergence = sum(
            oracle.diff(component, coordinate)
            for component, coordinate in zip(
                oracle_density_vector, (oracle_u, oracle_v, oracle_w)
            )
        )
        self.assertEqual(
            oracle.sympify(str(chart.div_density(density_vector).simplify())),
            expected_density_divergence,
        )

        covector = (oracle_v*oracle_w, oracle_u**2, oracle_u*oracle_v)
        expected_curl = oracle.Matrix((
            oracle.diff(covector[2], oracle_v) - oracle.diff(covector[1], oracle_w),
            oracle.diff(covector[0], oracle_w) - oracle.diff(covector[2], oracle_u),
            oracle.diff(covector[1], oracle_u) - oracle.diff(covector[0], oracle_v),
        )) / expected_covariant.det()
        expected_curl_density = oracle.Matrix((
            oracle.diff(covector[2], oracle_v) - oracle.diff(covector[1], oracle_w),
            oracle.diff(covector[0], oracle_w) - oracle.diff(covector[2], oracle_u),
            oracle.diff(covector[1], oracle_u) - oracle.diff(covector[0], oracle_v),
        ))
        actual_curl = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in chart.curl((v*w, u**2, u*v))
        ))
        self.assertEqual(actual_curl, expected_curl)
        actual_curl_density = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in chart.curl_density((v*w, u**2, u*v))
        ))
        self.assertEqual(actual_curl_density, expected_curl_density)
        field = chart.magnetic_field((v*w, u**2, u*v))
        self.assertEqual(field.upper.variance, (1,))
        self.assertEqual(field.lower.variance, (-1,))
        self.assertEqual(field.density.variance, (1,))
        self.assertEqual(field.density.density_weight, 1)
        self.assertEqual(
            oracle.Matrix(tuple(
                oracle.sympify(str(value.simplify()))
                for value in field.upper
            )),
            actual_curl,
        )
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
    def test_spherical_chart_calculus_matches_sympy(self):
        r, theta, phi = sp.symbols(
            "spherical_chart_r spherical_chart_theta spherical_chart_phi"
        )
        chart = sp.Chart(
            (r, theta, phi),
            (r*sp.sin(theta)*sp.cos(phi),
             r*sp.sin(theta)*sp.sin(phi),
             r*sp.cos(theta)),
        )
        or_, ot, op = oracle.symbols(
            "spherical_chart_r spherical_chart_theta spherical_chart_phi"
        )
        oracle_position = oracle.Matrix((
            or_*oracle.sin(ot)*oracle.cos(op),
            or_*oracle.sin(ot)*oracle.sin(op),
            or_*oracle.cos(ot),
        ))
        oracle_coordinates = oracle.Matrix((or_, ot, op))
        expected_basis = oracle_position.jacobian(oracle_coordinates)
        expected_metric = expected_basis.T*expected_basis
        metric_owner = chart.metric_owner(
            ((1, 0, 0), (0, r**2, 0),
             (0, 0, r**2*sp.sin(theta)**2)),
        )

        def native_matrix(values):
            return oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
                str(values[row + 3*column].simplify())
            ))

        actual_basis = native_matrix(chart.covariant_basis())
        actual_reciprocal = native_matrix(chart.reciprocal_basis())
        metric_tensor = chart.metric()
        actual_metric = oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
            str(metric_tensor[row, column].simplify())
        ))
        self.assertEqual(actual_basis, expected_basis)
        self.assertEqual(actual_reciprocal, expected_basis.inv().T)
        for row in range(3):
            for column in range(3):
                self.assertEqual(
                    oracle.simplify(
                        actual_metric[row, column] - expected_metric[row, column]
                    ),
                    0,
                )
        actual_jacobian = oracle.sympify(str(chart.jacobian().simplify()))
        self.assertEqual(
            oracle.simplify(actual_jacobian - expected_basis.det()), 0
        )
        actual_sqrtg_squared = oracle.sympify(
            str(chart.sqrtg().simplify())
        )**2
        self.assertEqual(
            oracle.simplify(actual_sqrtg_squared - expected_metric.det()), 0
        )

        christoffel = chart.christoffel()
        gamma_cases = (
            (1, 2, 2, -or_),
            (1, 3, 3, -or_*oracle.sin(ot)**2),
            (2, 1, 2, 1/or_),
            (2, 3, 3, -oracle.sin(ot)*oracle.cos(ot)),
            (3, 1, 3, 1/or_),
            (3, 2, 3, oracle.cos(ot)/oracle.sin(ot)),
        )
        for first, second, third, expected in gamma_cases:
            actual = oracle.sympify(
                str(christoffel[first - 1, second - 1, third - 1].simplify())
            )
            self.assertEqual(oracle.simplify(actual - expected), 0)

        actual_gradient = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in metric_owner.grad(r)
        ))
        self.assertEqual(actual_gradient, oracle.Matrix((1, 0, 0)))
        self.assertEqual(
            oracle.sympify(
                str(metric_owner.divergence((r, r - r, r - r)).simplify())
            ),
            3,
        )
        expected_curl = oracle.Matrix((
            2*oracle.cos(ot), -2*oracle.sin(ot)/or_, 0,
        ))
        actual_curl = oracle.Matrix(tuple(
            oracle.sympify(str(value.simplify()))
            for value in chart.curl((
                r - r, r - r, r**2*sp.sin(theta)**2
            ))
        ))
        self.assertEqual(
            oracle.simplify(actual_curl - expected_curl), oracle.zeros(3, 1)
        )
        self.assertEqual(
            oracle.sympify(str(metric_owner.laplacian(r**2).simplify())), 6
        )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_chart_map_transforms_tensor_slots_and_densities(self):
        x, y, z, p, q, s = sp.symbols(
            "map_x map_y map_z map_p map_q map_s"
        )
        manifold = sp.Manifold("map_M", 3)
        source_patch = sp.Patch("source", manifold)
        target_patch = sp.Patch("target", manifold)
        source = sp.Chart((x, y, z), (x, y, z), patch=source_patch)
        target = sp.Chart(
            (p, q, s), ((p - q)/2, q, s), patch=target_patch
        )
        transition = sp.ChartMap(
            source, target, (2*x + y, y, z), ((p - q)/2, q, s)
        )
        self.assertIs(transition.source_patch, source_patch)
        self.assertIs(transition.target_patch, target_patch)
        singular = sp.ChartMap(
            source, target, (x, x, z), (p, q, s)
        )
        with self.assertRaisesRegex(fortsym.FortSymError, "zero Jacobian"):
            singular.jacobian()
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
            (op/2, oq/2, os/2),
        )

        reversed_transition = sp.ChartMap(
            source, target, (-x, y, z), (-p, q, s)
        )
        reversed_density = reversed_transition.transform(
            source.vector((x, y, z), density_weight=1)
        )
        self.assertEqual(
            tuple(oracle.sympify(str(reversed_density[index].simplify()))
                  for index in range(3)),
            (op, oq, os),
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
        top_zero = transition.transform(source.three_form(1).d())
        self.assertEqual(top_zero.degree, 4)
        self.assertEqual(oracle.sympify(str(top_zero[0].simplify())), 0)

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

        other_patch = sp.Patch("other", manifold)
        mismatched_middle = sp.Chart(
            (p, q, s), ((p - q)/2, q, s), patch=other_patch
        )
        mismatched = sp.ChartMap(
            mismatched_middle, final, (p + q, q, s), (r - t, t, v)
        )
        with self.assertRaisesRegex(ValueError, "matching intermediate charts"):
            transition.compose(mismatched)

        wrong_source = sp.Chart((x, y, z), (x + y + 1, y, z))
        wrong_owner = sp.ChartMap(
            wrong_source, target, (2*x + y, y, z), ((p - q)/2, q, s)
        )
        with self.assertRaisesRegex(ValueError, "source chart"):
            wrong_owner.transform(source.vector((x, y, z)))
        with self.assertRaisesRegex(ValueError, "source chart"):
            wrong_owner.transform(source.one_form((x, y, z)))

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_paper_magnetic_fourier_current_matches_sympy(self):
        x1, x2, x3, n = sp.symbols(
            "current_x1 current_x2 current_x3 current_n"
        )
        chart = sp.Chart((x1, x2, x3), (x1, x2, x3))
        reluctivity = ((2, 3, 0), (5, 7, 0), (0, 0, 11))
        potential = (x1*x2, x1**2, 0)
        actual = chart.j_fourier(reluctivity, potential, n)

        ox, oy, on = oracle.symbols("current_x1 current_x2 current_n")
        expected = (
            on**2*(7*ox*oy - 5*ox**2),
            on**2*(-3*ox*oy + 2*ox**2) - 11,
            oracle.I*on*(-13*ox + 7*oy),
        )
        actual = tuple(
            oracle.sympify(str(value.simplify()), locals={"i": oracle.I})
            for value in actual
        )
        for actual_value, expected_value in zip(actual, expected):
            self.assertEqual(oracle.simplify(actual_value - expected_value), 0)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_paper_cylindrical_fourier_density_matches_sympy(self):
        z, radius, phi, mode = sp.symbols(
            "paper_cyl_z paper_cyl_R paper_cyl_phi paper_cyl_n"
        )
        chart = sp.Chart(
            (z, radius, phi),
            (radius*sp.cos(phi), radius*sp.sin(phi), z),
        )
        actual = chart.b_fourier_density(
            (z*radius, radius**2, z - z), mode
        )

        oz, o_radius, o_mode = oracle.symbols(
            "paper_cyl_z paper_cyl_R paper_cyl_n"
        )
        expected = (
            -oracle.I*o_mode*o_radius**2,
            oracle.I*o_mode*oz*o_radius,
            -oz,
        )
        for actual_value, expected_value in zip(actual, expected):
            parsed = oracle.sympify(
                str(actual_value.simplify()), locals={"i": oracle.I}
            )
            self.assertEqual(oracle.simplify(parsed - expected_value), 0)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_physical_reluctivity_density_matches_sympy(self):
        z, radius, phi, physical = sp.symbols(
            "density_z density_R density_phi density_nu"
        )
        chart = sp.Chart(
            (z, radius, phi),
            (radius*sp.cos(phi), radius*sp.sin(phi), z),
        )
        scalar_density = chart.reluctivity_density(physical)
        self.assertEqual(scalar_density.variance, (-1, -1))
        self.assertEqual(scalar_density.density_weight, -1)
        self.assertEqual(scalar_density.symmetry(0, 1), sp.SYMMETRIC)

        oz, o_radius, o_phi, o_physical = oracle.symbols(
            "density_z density_R density_phi density_nu"
        )
        oracle_coordinates = (oz, o_radius, o_phi)
        oracle_position = (
            o_radius*oracle.cos(o_phi),
            o_radius*oracle.sin(o_phi),
            oz,
        )
        jacobian = oracle.Matrix(oracle_position).jacobian(oracle_coordinates)
        volume = oracle.sqrt(jacobian.det()**2)
        expected_scalar = o_physical*(jacobian.T*jacobian)/volume

        def parsed_tensor(tensor):
            return oracle.Matrix(3, 3, lambda row, column: oracle.sympify(
                str(tensor[row, column].simplify())
            ))

        actual_scalar = parsed_tensor(scalar_density)
        for row in range(3):
            for column in range(3):
                self.assertEqual(
                    oracle.simplify(actual_scalar[row, column] -
                                    expected_scalar[row, column]),
                    0,
                )
        longitudinal = chart.fourier_weak_form(scalar_density, 0)
        self.assertEqual(longitudinal.branch, sp.FOURIER_LONGITUDINAL)

        physical_matrix = ((2, 0, 0), (0, 3, 0), (0, 0, 5))
        matrix_density = chart.reluctivity_density(physical_matrix)
        self.assertEqual(matrix_density.variance, (-1, -1))
        self.assertEqual(matrix_density.density_weight, -1)
        oracle_physical = oracle.diag(2, 3, 5)
        expected_matrix = jacobian.T*oracle_physical*jacobian/volume
        actual_matrix = parsed_tensor(matrix_density)
        for row in range(3):
            for column in range(3):
                self.assertEqual(
                    oracle.simplify(actual_matrix[row, column] -
                                    expected_matrix[row, column]),
                    0,
                )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_paper_fourier_weak_form_branch_matches_sympy(self):
        u1, u2, u3, nu33 = sp.symbols(
            "weak_u1 weak_u2 weak_u3 weak_nu33"
        )
        chart = sp.Chart((u1, u2, u3), (u1, u2, u3))
        reluctivity = ((2, 3, 0), (5, 7, 0), (0, 0, nu33))

        longitudinal = chart.fourier_weak_form(reluctivity, 0)
        self.assertEqual(longitudinal.branch, sp.FOURIER_LONGITUDINAL)
        self.assertEqual(longitudinal.branch_name, "longitudinal")
        self.assertEqual(longitudinal.trial_space, sp.SPACE_NODAL)
        self.assertEqual(longitudinal.boundary_trace, sp.TRACE_NORMAL)
        expected_diffusion = oracle.Matrix(((7, -5), (-3, 2)))
        actual_diffusion = oracle.Matrix(2, 2, lambda row, column: oracle.sympify(
            str(longitudinal.longitudinal_diffusion[row][column].simplify())
        ))
        self.assertEqual(actual_diffusion, expected_diffusion)
        self.assertTrue(longitudinal.has_gradient_term)
        self.assertFalse(longitudinal.has_mass_term)

        transverse = chart.fourier_weak_form(reluctivity, 2)
        self.assertEqual(transverse.branch, sp.FOURIER_TRANSVERSE)
        self.assertEqual(transverse.trial_space, sp.SPACE_EDGE)
        self.assertEqual(transverse.boundary_trace, sp.TRACE_TANGENTIAL)
        self.assertEqual(
            transverse.transverse_curl_coefficient.simplify(), nu33
        )
        expected_mass = oracle.Matrix(((28, -20), (-12, 8)))
        actual_mass = oracle.Matrix(2, 2, lambda row, column: oracle.sympify(
            str(transverse.transverse_mass[row][column].simplify())
        ))
        self.assertEqual(actual_mass, expected_mass)
        current = chart.current_compatibility((u1**2, u2**2, u3), 3)
        self.assertEqual(
            current.simplify(), 2*u1 + 2*u2 + 3*sp.I*u3
        )

        ox1, ox2, onu33 = oracle.symbols(
            "weak_u1 weak_u2 weak_nu33"
        )
        native_a3 = u1**2*u2 + u1
        native_source_3 = u1 - u2
        a3 = ox1**2*ox2 + ox1
        source_3 = ox1 - ox2
        actual_longitudinal = chart.fourier_longitudinal_residual(
            reluctivity, native_a3, native_source_3
        )
        expected_longitudinal = (
            -oracle.diff(7*oracle.diff(a3, ox1) - 5*oracle.diff(a3, ox2), ox1)
            - oracle.diff(-3*oracle.diff(a3, ox1) +
                          2*oracle.diff(a3, ox2), ox2)
            - source_3
        )
        expected_flux_one = 7*oracle.diff(a3, ox1) - 5*oracle.diff(a3, ox2)
        expected_flux_two = -3*oracle.diff(a3, ox1) + 2*oracle.diff(a3, ox2)
        actual_flux_one = chart.fourier_longitudinal_flux(
            reluctivity, native_a3, 1
        )
        actual_flux_two = chart.fourier_longitudinal_flux(
            reluctivity, native_a3, 2
        )
        for actual_value, expected_value in (
                (actual_flux_one, expected_flux_one),
                (actual_flux_two, expected_flux_two)):
            self.assertEqual(
                oracle.simplify(
                    oracle.sympify(str(actual_value.simplify())) -
                    expected_value
                ),
                0,
            )
        self.assertEqual(
            oracle.simplify(
                oracle.sympify(str(actual_longitudinal.simplify())) -
                expected_longitudinal
            ),
            0,
        )

        a1 = ox1**2 + ox2
        a2 = ox1*ox2
        source_t = (ox1 - ox2, ox1 + 2*ox2)
        native_a = (u1**2 + u2, u1*u2)
        native_source_t = (u1 - u2, u1 + 2*u2)
        actual_transverse = chart.fourier_transverse_residual(
            reluctivity, native_a, native_source_t, 2
        )
        curl_t = oracle.diff(a2, ox1) - oracle.diff(a1, ox2)
        expected_transverse = (
            oracle.diff(onu33*curl_t, ox2) + 4*(7*a1 - 5*a2) - source_t[0],
            -oracle.diff(onu33*curl_t, ox1) + 4*(-3*a1 + 2*a2) - source_t[1],
        )
        actual_flux = chart.fourier_transverse_flux(
            reluctivity, native_a
        )
        self.assertEqual(
            oracle.simplify(
                oracle.sympify(str(actual_flux.simplify())) - onu33*curl_t
            ),
            0,
        )
        normal = (2, 3)
        expected_longitudinal_boundary = (
            normal[0]*expected_flux_one + normal[1]*expected_flux_two
        )
        actual_longitudinal_boundary = chart.fourier_longitudinal_boundary_flux(
            reluctivity, native_a3, normal
        )
        self.assertEqual(
            oracle.simplify(
                oracle.sympify(
                    str(actual_longitudinal_boundary.simplify())
                ) - expected_longitudinal_boundary
            ),
            0,
        )
        expected_edge_boundary = (-normal[1]*onu33*curl_t,
                                  normal[0]*onu33*curl_t)
        actual_edge_boundary = chart.fourier_transverse_boundary_flux(
            reluctivity, native_a, normal
        )
        for actual_value, expected_value in zip(
                actual_edge_boundary, expected_edge_boundary):
            self.assertEqual(
                oracle.simplify(
                    oracle.sympify(str(actual_value.simplify())) -
                    expected_value
                ),
                0,
            )
        actual_edge_contraction = (
            chart.fourier_transverse_boundary_contraction(
                reluctivity, native_a, normal, (u1, u2)
            )
        )
        expected_edge_contraction = (
            ox1*expected_edge_boundary[0] + ox2*expected_edge_boundary[1]
        )
        self.assertEqual(
            oracle.simplify(
                oracle.sympify(str(actual_edge_contraction.simplify())) -
                expected_edge_contraction
            ),
            0,
        )
        for actual_value, expected_value in zip(
                actual_transverse, expected_transverse):
            self.assertEqual(
                oracle.simplify(
                    oracle.sympify(str(actual_value.simplify())) -
                    expected_value
                ),
                0,
            )

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_paper_fourier_zero_and_transverse_reductions_match_sympy(self):
        x1, x2, x3 = sp.symbols("reduction_x1 reduction_x2 reduction_x3")
        nu33 = sp.Symbol("reduction_nu33")
        chart = sp.Chart((x1, x2, x3), (x1, x2, x3))
        reluctivity = ((2, 3, 0), (5, 7, 0), (0, 0, nu33))
        potential = (x1*x2, x1**2, sp.Integer(0))

        zero_mode = chart.j_fourier(reluctivity, potential, 0)
        full_curl_curl = chart.curl(
            chart.h_cov(reluctivity, chart.curl(potential))
        )
        for actual_value, expected_value in zip(zero_mode, full_curl_curl):
            actual = oracle.sympify(
                str(actual_value.simplify()), locals={"i": oracle.I}
            )
            expected = oracle.sympify(
                str(expected_value.simplify()), locals={"i": oracle.I}
            )
            self.assertEqual(oracle.simplify(actual - expected), 0)

        transverse = chart.j_fourier(reluctivity, potential, 2)
        expected_transverse = (
            4*(7*x1*x2 - 5*x1**2),
            4*(-3*x1*x2 + 2*x1**2) - nu33,
        )
        for index, expected_value in enumerate(expected_transverse):
            actual = oracle.sympify(
                str(transverse[index].simplify()), locals={"i": oracle.I}
            )
            expected = oracle.sympify(
                str(expected_value), locals={"i": oracle.I}
            )
            self.assertEqual(oracle.simplify(actual - expected), 0)

    @unittest.skipIf(oracle is None, "SymPy is not installed")
    def test_diffgeom_names_use_native_owner_and_match_oracle(self):
        from sympy.diffgeom import (
            CoordSystem as OracleCoordSystem,
            Differential as OracleDifferential,
            LieDerivative as OracleLieDerivative,
            Manifold as OracleManifold,
            Patch as OraclePatch,
            TensorProduct as OracleTensorProduct,
            WedgeProduct as OracleWedgeProduct,
        )

        native_manifold = sp.Manifold("native_M", 3)
        native_patch = sp.Patch("native_P", native_manifold)
        declared = sp.Manifold("declared_M", 3, simply_connected=True)
        declared_patch = sp.Patch(
            "declared_P", declared, open_domain=False, boundary=True,
            simply_connected=True,
        )
        self.assertTrue(declared.simply_connected)
        self.assertFalse(declared_patch.open_domain)
        self.assertTrue(declared_patch.boundary)
        self.assertTrue(declared_patch.simply_connected)
        native = sp.CoordSystem("native_c", native_patch)
        self.assertTrue(native.chart.has_patch)
        self.assertIs(native.chart.patch, native_patch)
        x, y, z = sp.symbols("diffgeom_x diffgeom_y diffgeom_z")
        direct_chart = sp.Chart((x, y, z), (x, y, z), patch=native_patch)
        self.assertTrue(direct_chart.has_patch)
        self.assertIs(direct_chart.patch, native_patch)
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
        native_product = sp.TensorProduct(ndx, ndy).doit()
        oracle_product = OracleTensorProduct(odx, ody)
        self.assertEqual(
            scalar_text(native_product[0, 1].simplify()),
            oracle_product.rcall(ovx).rcall(ovy),
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
