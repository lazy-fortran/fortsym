import gc
import subprocess
import sys
import unittest
from fractions import Fraction

import fortsym


class NativePackageTest(unittest.TestCase):
    def test_typed_geometry_metadata_facade(self):
        signature = fortsym.Signature((-1, 1, 1))
        orientation = fortsym.Orientation(-1)
        self.assertTrue(signature.is_lorentzian)
        self.assertEqual((signature.positive_count, signature.negative_count), (2, 1))
        self.assertEqual(int(orientation), -1)
        with fortsym.Arena() as arena:
            x, y, z = [arena.symbol(name) for name in ("meta_x", "meta_y", "meta_z")]
            metric = fortsym.Chart((x, y, z), (x, y, z)).metric_owner(
                ((-1, 0, 0), (0, 1, 0), (0, 0, 1)),
                signature=signature, orientation=orientation,
            )
            self.assertEqual(metric.signature_type.values, (-1, 1, 1))
            self.assertEqual(metric.orientation_type.value, -1)
            time = arena.symbol("meta_t")
            spacetime = fortsym.SpacetimeMetric(
                (time, x, y, z),
                ((-1, 0, 0, 0), (0, 1, 0, 0),
                 (0, 0, 1, 0), (0, 0, 0, 1)),
                signature=fortsym.Signature((-1, 1, 1, 1)),
                orientation=orientation,
            )
            self.assertEqual(spacetime.signature_type.negative_count, 1)
            self.assertEqual(spacetime.orientation_type.value, -1)

    def test_runtime_spacetime_tensor_variance_and_density(self):
        with fortsym.Arena() as arena:
            t, x, y, z = [arena.symbol(name)
                          for name in ("tensor_t", "tensor_x", "tensor_y", "tensor_z")]
            metric = fortsym.SpacetimeMetric(
                (t, x, y, z),
                ((2, 1, 0, 0), (1, 1, 0, 0),
                 (0, 0, 0, 0), (0, 0, 0, 0)),
                dimension=2,
                signature=(1, 1, 1, 1),
            )
            upper = metric.vector((t, x, 0, 0))
            lower = upper.lower()
            roundtrip = lower.raise_()
            self.assertEqual(lower.variance, (-1,))
            self.assertEqual(roundtrip.variance, (1,))
            self.assertEqual((roundtrip[0] - t).simplify(), 0)
            self.assertEqual((roundtrip[1] - x).simplify(), 0)
            self.assertEqual(metric.covariant().variance, (-1, -1))
            inverse = metric.contravariant()
            self.assertEqual(inverse.variance, (1, 1))
            self.assertEqual(inverse[0, 1].simplify(), -1)
            density = upper.density(metric.sqrtg())
            self.assertEqual(density.density_weight, 1)
            self.assertEqual((density[0] - metric.sqrtg()*upper[0]).simplify(), 0)
            product = upper.product(lower)
            self.assertEqual(product.variance, (1, -1))
            contracted = product.contract(0, 1)
            self.assertEqual(contracted.rank, 0)
            self.assertEqual(
                (contracted[()] - (2*t**2 + 2*t*x + x**2)).simplify(), 0
            )
            permuted = product.permute((1, 0))
            self.assertEqual(permuted.variance, (-1, 1))
            self.assertEqual((permuted[0, 1] - product[1, 0]).simplify(), 0)
            self.assertEqual(permuted[2, 0].simplify(), 0)
            self.assertEqual(
                density.product(lower).density_weight, 1
            )
            exp_2t = arena.function("exp", (2*t,))
            exp_t = arena.function("exp", (t,))
            curved = fortsym.SpacetimeMetric(
                (t, x, y, z),
                ((1, 0, 0, 0), (0, exp_2t, 0, 0),
                 (0, 0, 0, 0), (0, 0, 0, 0)),
                dimension=2,
                signature=(1, 1, 1, 1),
            )
            curved_vector = curved.vector((0, 1, 0, 0))
            derivative = curved_vector.covariant_diff()
            self.assertEqual(derivative.variance, (1, -1))
            self.assertEqual((derivative[1, 0] - 1).simplify(), 0)
            self.assertEqual((derivative[0, 1] + exp_2t).simplify(), 0)
            self.assertEqual(derivative[1, 1].simplify(), 0)
            metric_derivative = curved.covariant().covariant_diff()
            self.assertEqual(metric_derivative[1, 1, 0].simplify(), 0)
            killing = curved.killing(curved_vector)
            self.assertEqual(killing[1, 1].simplify(), 0)
            dilation = curved.vector((t, 0, 0, 0))
            lie_metric = curved.lie(dilation, curved.covariant())
            self.assertEqual((lie_metric[0, 0] - 2).simplify(), 0)
            self.assertEqual(
                (lie_metric[1, 1] - 2*t*exp_2t).simplify(), 0
            )
            self.assertEqual(
                (curved.lie(dilation, curved.scalar(t))[()] - t).simplify(), 0
            )
            density_scalar = curved.scalar(1, density_weight=1)
            self.assertEqual(
                curved.lie(dilation, density_scalar)[()].simplify(), 1
            )
            self.assertEqual(
                curved_vector.covariant_divergence()[()].simplify(), 0
            )
            self.assertEqual(
                (dilation.covariant_divergence()[()] - (1 + t)).simplify(), 0
            )
            density_vector = curved.vector((t, 0, 0, 0), density_weight=1)
            self.assertEqual(
                density_vector.divergence()[()].simplify(), 1
            )
            self.assertEqual(
                curved.contravariant().covariant_divergence()[1].simplify(), 0
            )
            curved_density = curved.vector((0, exp_t, 0, 0), density_weight=1)
            density_derivative = curved_density.covariant_diff()
            self.assertEqual(
                (density_derivative[1, 0] - exp_t).simplify(), 0
            )

            # Rank five is the runtime ceiling and is needed for a covariant
            # derivative of a rank-four curvature-like tensor.
            zero = arena.integer(0)
            rank_four_components = [zero] * (fortsym.SPACETIME_DIM ** 4)
            rank_four_components[0] = t
            rank_four = fortsym.SpacetimeTensor(
                metric, rank_four_components, (4, 4, 4, 4),
                variance=(1, -1, 1, -1),
            )
            rank_five = rank_four.covariant_diff()
            self.assertEqual(rank_five.rank, 5)
            self.assertEqual((rank_five[0, 0, 0, 0, 0] - 1).simplify(), 0)

    def test_native_construction_and_transformations(self):
        with fortsym.Arena() as arena:
            x = arena.symbol("x")
            y = arena.symbol("y")
            expression = (x + 1) * y
            self.assertEqual(str(expression), "y*(x + 1)")
            self.assertEqual(str(((x + 1) ** 2).expand()), "x**2 + 2*x + 1")
            self.assertEqual(str(((x + 1) ** 2).expand().factor()), "(x + 1)**2")
            self.assertEqual(str(x.diff(x)), "1")
            self.assertEqual(expression.subs(x, 2), y * 3)
            self.assertEqual(((x + 1) ** 2).subs(x, 2), 9)
            self.assertEqual(expression.arity, 2)
            self.assertEqual(x.node_count, 1)
            self.assertEqual(expression.node_count, 5)

        # Expression ownership keeps native inspection valid after close().
        arena = fortsym.Arena()
        x = arena.symbol("x")
        arena.close()
        self.assertEqual(str(x), "x")
        with self.assertRaisesRegex(fortsym.FortSymError, "arena failed"):
            x + 1
        x.close()
        gc.collect()

    def test_public_constructors_do_not_need_sympy(self):
        x, y = fortsym.symbols("x y")
        self.assertEqual(str(x + y), "x + y")
        self.assertEqual(str(fortsym.Rational(2, 3)), "2/3")
        self.assertEqual(fortsym.Integer(2**100).exact_text, str(2**100))
        self.assertEqual(str(fortsym.Symbol("z") + Fraction(1, 3)), "z + 1/3")
        huge_fraction = Fraction(2**100 + 1, 3**100)
        self.assertEqual(
            fortsym.Rational(huge_fraction.numerator, huge_fraction.denominator).exact_text,
            f"{huge_fraction.numerator}/{huge_fraction.denominator}",
        )
        result = subprocess.run(
            [
                sys.executable,
                "-c",
                "import sys; import fortsym; "
                "assert 'sympy' not in sys.modules",
            ],
            check=False,
        )
        self.assertEqual(result.returncode, 0)

    def test_foreign_arena_is_rejected(self):
        left = fortsym.Arena()
        right = fortsym.Arena()
        try:
            with self.assertRaises(ValueError):
                left.symbol("x") + right.symbol("y")
        finally:
            left.close()
            right.close()

    def test_native_chart_frontend_uses_geometry_owner(self):
        with fortsym.Arena() as arena:
            z, r, phi, mode = [arena.symbol(name)
                                for name in ("Z", "R", "phi", "n")]
            cos_phi = arena.function("cos", (phi,))
            sin_phi = arena.function("sin", (phi,))
            chart = fortsym.Chart(
                (z, r, phi), (r*cos_phi, r*sin_phi, z)
            )
            a1 = arena.function("A1", (z, r))
            a2 = arena.function("A2", (z, r))
            potential = (a1, a2, arena.integer(0))

            density = chart.b_fourier_density(potential, mode)
            expected_1 = (-arena.constant("i")*mode*a2).simplify()
            expected_2 = (arena.constant("i")*mode*a1).simplify()
            expected_3 = (a2.diff(z) - a1.diff(r)).simplify()
            self.assertEqual(density[0].simplify(), expected_1)
            self.assertEqual(density[1].simplify(), expected_2)
            self.assertEqual(density[2].simplify(), expected_3)
            self.assertEqual(len(chart.b_cov(chart.b_fourier(potential, mode))), 3)

            with self.assertRaises(ValueError):
                fortsym.Chart((z, r), (r, z))

    def test_native_flux_surface_owner(self):
        with fortsym.Arena() as arena:
            psi, theta, phi = [arena.symbol(name)
                               for name in ("surface_psi", "surface_theta",
                                            "surface_phi")]
            chart = fortsym.Chart((psi, theta, phi), (psi, theta, phi))
            surface = chart.flux_surface(1)
            self.assertEqual(surface.label, psi)
            self.assertEqual(surface.angle_indices, (2, 3))
            self.assertEqual(surface.measure().simplify(), 1)
            scalar = 1 + arena.function("sin", (theta,)) + \
                arena.function("cos", (phi,))
            self.assertEqual(surface.average(scalar).simplify(), 1)

    def test_native_magnetic_chart_owner(self):
        with fortsym.Arena() as arena:
            psi, theta, phi = [arena.symbol(name)
                               for name in ("magnetic_psi", "magnetic_theta",
                                            "magnetic_phi")]
            zero = arena.integer(0)
            chart = fortsym.Chart((psi, theta, phi), (psi, theta, phi))
            owner = chart.magnetic_chart((zero, psi, zero), label_index=1)
            self.assertIsInstance(owner, fortsym.MagneticChart)
            self.assertEqual(owner.label, psi)
            self.assertEqual(owner.upper.variance, (1,))
            self.assertEqual(owner.lower.variance, (-1,))
            self.assertEqual(owner.density.density_weight, 1)
            self.assertEqual(owner.upper[2].simplify(), 1)
            self.assertEqual(owner.divergence().simplify(), 0)
            self.assertEqual(owner.field_line_derivative(psi).simplify(), 0)
            self.assertEqual(owner.potential_form().degree, 1)
            flux_form = owner.flux_form()
            self.assertEqual(flux_form.degree, 2)
            self.assertEqual(flux_form[3].simplify(), 1)
            self.assertTrue(flux_form.is_closed)

    def test_native_tensor_and_connection_frontend(self):
        with fortsym.Arena() as arena:
            z, r, phi = [arena.symbol(name)
                         for name in ("Z", "R", "phi")]
            chart = fortsym.Chart((z, r, phi), (z, z*r, phi))

            metric = chart.metric_covariant()
            self.assertEqual((metric.rank, metric.variance), (2, (-1, -1)))
            self.assertEqual(metric[0, 0].simplify(), (r**2 + 1).simplify())

            christoffel = chart.christoffel()
            self.assertEqual(
                christoffel[1, 0, 1].simplify(),
                (1/z).simplify(),
            )

            # The temporary scalar tensor borrows r; destroying it must not
            # invalidate the chart coordinates used by later operations.
            gradient = chart.scalar(r).covariant_diff()
            self.assertEqual(gradient.variance, (-1,))
            self.assertEqual(gradient[1].simplify(), 1)

            metric_derivative = metric.covariant_diff()
            self.assertEqual(metric_derivative.rank, 3)
            self.assertEqual(metric_derivative[0, 1, 1].simplify(), 0)

            density_vector = chart.vector((z, r, phi), density_weight=1)
            density_divergence = density_vector.covariant_divergence()
            self.assertEqual(density_divergence.rank, 0)
            self.assertEqual(
                density_divergence.component().simplify(),
                chart.div_density((z, r, phi)).simplify(),
            )

            cartesian = fortsym.Chart(
                (z, r, phi), (z, r, phi)
            )
            self.assertEqual(cartesian.scalar_curvature().simplify(), 0)
            self.assertEqual(cartesian.riemann()[0, 0, 0, 0].simplify(), 0)
            bianchi = cartesian.first_bianchi_residual()
            self.assertEqual(bianchi[0, 1, 0, 1].simplify(), 0)
            second_bianchi = cartesian.second_bianchi_residual()
            self.assertEqual(second_bianchi[0, 1, 0, 1, 2].simplify(), 0)

            rho, theta, zeta = [arena.symbol(name)
                                for name in ("rho", "theta", "zeta")]
            parameter = arena.symbol("lambda")
            radius = arena.symbol("R0")
            cylindrical = fortsym.Chart(
                (rho, theta, zeta),
                (rho*arena.function("cos", (theta,)),
                 rho*arena.function("sin", (theta,)), zeta),
            )
            residual = cylindrical.geodesic_residual(
                (radius, parameter, 0), parameter
            )
            self.assertEqual((residual[0] + radius).simplify(), 0)
            self.assertEqual(residual[1].simplify(), 0)
            self.assertEqual(residual[2].simplify(), 0)

    def test_native_differential_form_frontend(self):
        with fortsym.Arena() as arena:
            x, y, z = [arena.symbol(name) for name in ("form_x", "form_y", "form_z")]
            chart = fortsym.Chart((x, y, z), (x, y, z))
            one = chart.one_form((y*z, x**2, y + z**2))
            derivative = one.d()
            self.assertEqual(derivative.degree, 2)
            self.assertEqual((derivative[3] - (2*x - z)).simplify(), 0)
            self.assertEqual((derivative[5] + y).simplify(), 0)
            self.assertEqual((derivative[6] - 1).simplify(), 0)
            self.assertEqual(derivative.d()[7].simplify(), 0)
            self.assertEqual(one.wedge(one)[3].simplify(), 0)
            self.assertEqual(one.star().star()[1].simplify(), y*z)
            self.assertEqual(chart.flat((x, y, z)).sharp()[2].simplify(), z)
            contracted = derivative.interior((x, y, z))[1]
            self.assertEqual((contracted + 2*x*y - 2*y*z).simplify(), 0)

    def test_top_degree_zero_form_crosses_python_boundary(self):
        with fortsym.Arena() as arena:
            x, y, z = [arena.symbol(name) for name in ("top_x", "top_y", "top_z")]
            chart = fortsym.Chart((x, y, z), (x, y, z))
            top = chart.three_form(x + y + z).d()

            self.assertEqual(top.degree, 4)
            self.assertEqual(top[0].simplify(), 0)
            self.assertTrue(top.is_closed)
            self.assertEqual(top.d()[0].simplify(), 0)


if __name__ == "__main__":
    unittest.main()
