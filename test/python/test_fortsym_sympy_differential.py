#!/usr/bin/env python3
"""Differential tests for the declared fortsym.sympy subset.

SymPy supplies the reference result and exception behaviour.  The fixed text
assertions and independent parser check keep a shared printer or parser bug
from making the comparison vacuous.
"""

import sys
import unittest

try:
    import sympy as oracle
except ModuleNotFoundError as error:
    if error.name == "sympy":
        print("SKIP: SymPy 1.14.0 is not installed", file=sys.stderr)
        raise SystemExit(77)
    raise

import fortsym.sympy as native


class SympyDifferentialTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if oracle.__version__ != "1.14.0":
            raise unittest.SkipTest(
                f"differential baseline requires SymPy 1.14.0, found {oracle.__version__}"
            )
        cls.locals = {
            "x": oracle.Symbol("x"),
            "y": oracle.Symbol("y"),
            "i": oracle.I,
            "differential_x": oracle.Symbol("differential_x"),
            "differential_y": oracle.Symbol("differential_y"),
            "abs": oracle.Abs,
        }

    def assert_equivalent(self, label, expected, actual):
        expected_text = str(expected).replace("Abs(", "abs(")
        actual_text = str(actual)

        expected_parsed = oracle.sympify(expected_text, locals=self.locals)
        actual_parsed = oracle.sympify(actual_text, locals=self.locals)
        self.assertEqual(
            oracle.simplify(actual_parsed - expected_parsed),
            oracle.Integer(0),
            label,
        )

    @staticmethod
    def expression_cases(api):
        x, y = api.symbols("differential_x differential_y")
        return {
            "integer": api.Integer(2**100),
            "rational": api.Rational(2, 3),
            "large integer normalization": api.Integer(10**80),
            "rational normalization": api.Rational(6, -8),
            "addition": x + y,
            "multiplication": (x + 1) * y,
            "power": (x + 1)**2,
            "function": api.exp(x * y),
            "derivative": api.diff(api.exp(x * y), x),
            "substitution": api.expand((x + 1)**2).subs(x, 2),
            "expansion": api.expand((x + 1)**2),
            "simplification": api.simplify(api.sqrt(x**2)),
            "refinement": api.refine(api.sqrt(x**2), api.Q.positive(x)),
            "factorisation": api.factor(x**2 + 2*x + 1),
        }

    @staticmethod
    def relation_cases(api):
        x = api.Symbol("relation_x")
        return {
            "equal": api.Eq(x, 1),
            "unequal": api.Ne(x, 1),
            "greater": api.Gt(x, 1),
            "greater_equal": api.Ge(x, 1),
            "less": api.Lt(x, 1),
            "less_equal": api.Le(x, 1),
        }

    @staticmethod
    def assumption_cases(api):
        cases = {}
        for assumption in (
            "rational", "integer", "real", "nonnegative", "positive", "negative", "nonpositive", "zero"
        ):
            symbol = api.Symbol(
                "condition_" + assumption, **{assumption: True}
            )
            cases[assumption] = api.simplify(api.sqrt(symbol**2))
        return cases

    @staticmethod
    def predicate_cases(api):
        x = api.Symbol("predicate_x")
        return [api.Integer(0), api.Integer(7), x - x, api.sin(x), x]

    @staticmethod
    def scoped_assumption_trace(api):
        x = api.Symbol("scoped_positive")
        y = api.Symbol("scoped_nonnegative")
        positive = api.Q.positive(x)
        nonnegative = api.Q.nonnegative(y)
        trace = [api.ask(positive), api.ask(api.Q.real(x))]
        with api.assuming(positive):
            trace.extend((api.ask(positive), api.ask(api.Q.real(x))))
            with api.assuming(nonnegative):
                trace.extend((api.ask(nonnegative), api.ask(api.Q.real(y))))
            trace.extend((api.ask(nonnegative), api.ask(api.Q.real(y))))
        trace.extend((api.ask(positive), api.ask(api.Q.real(x))))
        return trace

    def test_exact_and_symbolic_results(self):
        oracle_cases = self.expression_cases(oracle)
        native_cases = self.expression_cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assert_equivalent(label, expected, native_cases[label])

    def test_polynomial_operations_match_sympy(self):
        oracle_x, oracle_y = oracle.symbols("poly_x poly_y")
        native_x, native_y = native.symbols("poly_x poly_y")
        cases = [
            ("together", lambda: oracle.together(
                (oracle_x + 1) / (oracle_x + 2)),
             lambda: native.together((native_x + 1) / (native_x + 2))),
            ("cancel", lambda: oracle.cancel(
                (oracle_x**2 - 1) / (oracle_x - 1)),
             lambda: native.cancel((native_x**2 - 1) / (native_x - 1))),
            ("apart", lambda: oracle.apart(
                1 / (oracle_x * (oracle_x + 1)), oracle_x),
             lambda: native.apart(
                 1 / (native_x * (native_x + 1)), native_x)),
            ("collect", lambda: oracle.collect(
                oracle_x + oracle_x*oracle_y, oracle_x),
             lambda: native.collect(native_x + native_x*native_y, native_x)),
        ]
        for label, oracle_call, native_call in cases:
            with self.subTest(label=label):
                self.assert_equivalent(label, oracle_call(), native_call())

    def test_verified_indefinite_integrals_match_sympy(self):
        oracle_x = oracle.Symbol("integral_x")
        native_x = native.Symbol("integral_x")
        cases = [
            ("power", oracle.integrate(oracle_x**2, oracle_x),
             native.integrate(native_x**2, native_x)),
            ("sine", oracle.integrate(oracle.sin(oracle_x), oracle_x),
             native.integrate(native.sin(native_x), native_x)),
            ("exponential", oracle.integrate(oracle.exp(2*oracle_x), oracle_x),
             native.integrate(native.exp(2*native_x), native_x)),
            ("logarithmic", oracle.integrate(1/oracle_x, oracle_x),
             native.integrate(1/native_x, native_x)),
        ]
        for label, expected, actual in cases:
            with self.subTest(label=label):
                self.assert_equivalent(label, expected, actual)

    def test_verified_limits_match_sympy(self):
        oracle_x = oracle.Symbol("limit_x")
        native_x = native.Symbol("limit_x")
        cases = [
            ("continuous", oracle.limit(oracle.sin(oracle_x) / oracle_x,
                                         oracle_x, 0),
             native.limit(native.sin(native_x) / native_x, native_x, 0)),
            ("cancelled quotient", oracle.limit(
                (oracle_x**2 - 1) / (oracle_x - 1), oracle_x, 1),
             native.limit((native_x**2 - 1) / (native_x - 1), native_x, 1)),
            ("polynomial infinity", oracle.limit(oracle_x**2, oracle_x,
                                                  oracle.oo),
             native.limit(native_x**2, native_x, native.oo)),
            ("decaying infinity", oracle.limit(1 / oracle_x, oracle_x,
                                                oracle.oo),
             native.limit(1 / native_x, native_x, native.oo)),
        ]
        for label, expected, actual in cases:
            with self.subTest(label=label):
                if expected in (oracle.oo, -oracle.oo):
                    self.assertEqual(str(actual), str(expected), label)
                else:
                    self.assert_equivalent(label, expected, actual)

    def test_bounded_taylor_series_match_sympy(self):
        oracle_x = oracle.Symbol("series_x")
        native_x = native.Symbol("series_x")
        cases = [
            ("exponential", oracle.series(oracle.exp(oracle_x), oracle_x, 0, 4).removeO(),
             native.series(native.exp(native_x), native_x, 0, 4)),
            ("sine", oracle.series(oracle.sin(oracle_x), oracle_x, 0, 5).removeO(),
             native.series(native.sin(native_x), native_x, 0, 5)),
            ("shifted polynomial", oracle.series((oracle_x + 2)**4,
                                                   oracle_x, 1, 3).removeO(),
             native.series((native_x + 2)**4, native_x, 1, 3)),
        ]
        for label, expected, actual in cases:
            with self.subTest(label=label):
                self.assert_equivalent(label, expected, actual)
        self.assert_equivalent(
            "exact Taylor coefficient",
            oracle.series((oracle_x + 2)**4, oracle_x, 0, 3).removeO().coeff(oracle_x, 2),
            ((native_x + 2)**4).series_coeff(native_x, native.Integer(0), 2),
        )

    def test_bounded_solve_matches_sympy(self):
        oracle_x = oracle.Symbol("solve_x")
        oracle_a = oracle.Symbol("solve_a")
        native_x = native.Symbol("solve_x")
        native_a = native.Symbol("solve_a")
        cases = [
            (oracle.solve(oracle_x**2 - 1, oracle_x),
             native.solve(native_x**2 - 1, native_x)),
            (oracle.solve((oracle_x - 1)**2, oracle_x),
             native.solve((native_x - 1)**2, native_x)),
            (oracle.solve(oracle.Eq(oracle_x, 2), oracle_x),
             native.solve(native.Eq(native_x, 2), native_x)),
            (oracle.solve(oracle_x + oracle_a, oracle_x),
             native.solve(native_x + native_a, native_x)),
            (oracle.solve(3, oracle_x),
             native.solve(3, native_x)),
        ]
        locals_map = {"solve_x": oracle_x, "solve_a": oracle_a}
        for expected, actual in cases:
            expected_values = [
                oracle.sympify(str(value), locals=locals_map)
                for value in expected
            ]
            actual_values = [
                oracle.sympify(str(value), locals=locals_map)
                for value in actual
            ]
            self.assertEqual(len(actual_values), len(expected_values))
            unmatched = list(expected_values)
            for value in actual_values:
                match = next(
                    (index for index, candidate in enumerate(unmatched)
                     if oracle.simplify(value - candidate) == 0),
                    None,
                )
                self.assertIsNotNone(match, (expected_values, actual_values))
                unmatched.pop(match)
            self.assertEqual(unmatched, [])

    def test_bounded_rational_solve_matches_sympy(self):
        oracle_x = oracle.Symbol("rational_solve_x")
        native_x = native.Symbol("rational_solve_x")
        cases = [
            ((oracle_x - 1) / (oracle_x + 1),
             (native_x - 1) / (native_x + 1)),
            ((oracle_x**2 - 1) / (oracle_x - 2),
             (native_x**2 - 1) / (native_x - 2)),
            (1 / (oracle_x - 1) - 1,
             1 / (native_x - 1) - 1),
            ((oracle_x**2 - 1) / (oracle_x - 1) - 2,
             (native_x**2 - 1) / (native_x - 1) - 2),
        ]
        for oracle_expression, native_expression in cases:
            expected = oracle.solve(oracle_expression, oracle_x)
            actual = native.solve(native_expression, native_x)
            self.assertEqual(len(actual), len(expected))
            unmatched = list(expected)
            for value in actual:
                parsed = oracle.sympify(str(value), locals=self.locals)
                match = next(
                    (index for index, candidate in enumerate(unmatched)
                     if oracle.simplify(parsed - candidate) == 0),
                    None,
                )
                self.assertIsNotNone(match, (expected, actual))
                unmatched.pop(match)
            self.assertEqual(unmatched, [])

    def test_solve_refuses_unsupported_options(self):
        native_x = native.Symbol("solve_refusal_x")
        with self.assertRaises(native.UnsupportedOperationError):
            native.solve(native.sin(native_x), native_x)
        with self.assertRaises(native.UnsupportedOperationError):
            native.solve(native_x**2 - 1, native_x + 1)
        with self.assertRaises(native.UnsupportedOperationError):
            native.solve(native_x**2 - 1, native_x, dict=True)
        with self.assertRaises(ValueError):
            native.solve(native.Symbol("solve_refusal_x") +
                         native.Symbol("solve_refusal_y") + 1)

    def test_bounded_solveset_matches_sympy(self):
        oracle_x = oracle.Symbol("solveset_x")
        native_x = native.Symbol("solveset_x")
        cases = [
            (oracle.solveset(oracle_x**2 - 1, oracle_x),
             native.solveset(native_x**2 - 1, native_x)),
            (oracle.solveset((oracle_x - 1) / (oracle_x + 1), oracle_x),
             native.solveset((native_x - 1) / (native_x + 1), native_x)),
            (oracle.solveset((oracle_x - 1) / (oracle_x - 1), oracle_x),
             native.solveset((native_x - 1) / (native_x - 1), native_x)),
            (oracle.solveset(1 / (oracle_x - 1) - 1, oracle_x),
             native.solveset(1 / (native_x - 1) - 1, native_x)),
            (oracle.solveset((oracle_x - 1)**2),
             native.solveset((native_x - 1)**2)),
            (oracle.solveset(oracle.Eq(oracle_x, 2), oracle_x),
             native.solveset(native.Eq(native_x, 2), native_x)),
            (oracle.solveset(3), native.solveset(3)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(actual), str(expected))
                self.assertEqual(len(actual), len(expected.args))

    def test_solveset_refuses_nondefault_domains(self):
        native_x = native.Symbol("solveset_refusal_x")
        with self.assertRaises(native.UnsupportedOperationError):
            native.solveset(native_x**2 - 1, native_x, domain=1)

    def test_solveset_preserves_symbolic_rational_poles(self):
        oracle_x, oracle_a, oracle_b = oracle.symbols(
            "solveset_pole_x solveset_pole_a solveset_pole_b"
        )
        native_x, native_a, native_b = native.symbols(
            "solveset_pole_x solveset_pole_a solveset_pole_b"
        )
        expected = oracle.solveset(
            (oracle_x - 1) / (oracle_a * oracle_x + oracle_b), oracle_x
        )
        actual = native.solveset(
            (native_x - 1) / (native_a * native_x + native_b), native_x
        )
        self.assertEqual(str(actual), str(expected))
        self.assertIsInstance(actual, native.Complement)

    def test_complement_result_is_native_owned_and_matches_sympy(self):
        oracle_x = oracle.Symbol("complement_x")
        oracle_y = oracle.Symbol("complement_y")
        native_x = native.Symbol("complement_x")
        native_y = native.Symbol("complement_y")
        base = native.FiniteSet(native_x)
        excluded = native.FiniteSet(native_y)
        actual = native.Complement(base, excluded)
        base.close()
        excluded.close()
        expected = oracle.Complement(
            oracle.FiniteSet(oracle_x), oracle.FiniteSet(oracle_y)
        )
        try:
            self.assertEqual(str(actual), str(expected))
            self.assertEqual(actual._expression.name, "Complement")
            self.assertEqual(actual._expression.arity, 2)
            views = actual.args
            try:
                self.assertEqual(len(views), 2)
                self.assertEqual(str(views[0]), str(oracle.FiniteSet(oracle_x)))
                self.assertEqual(str(views[1]), str(oracle.FiniteSet(oracle_y)))
            finally:
                for view in views:
                    view.close()
            probe_x = native.Symbol("complement_x")
            probe_y = native.Symbol("complement_y")
            try:
                self.assertIn(probe_x, actual)
                self.assertNotIn(probe_y, actual)
            finally:
                probe_x.close()
                probe_y.close()
        finally:
            actual.close()

    def test_bounded_linsolve_matches_sympy(self):
        oracle_x, oracle_y = oracle.symbols("linsolve_x linsolve_y")
        native_x, native_y = native.symbols("linsolve_x linsolve_y")
        matrix = ((1, 2), (3, 4))
        right_hand_side = (5, 6)
        expected = oracle.linsolve(
            (oracle.Matrix(matrix), oracle.Matrix(right_hand_side)),
            (oracle_x, oracle_y),
        )
        actual = native.linsolve(
            (matrix, right_hand_side), (native_x, native_y)
        )
        self.assertEqual(str(actual), str(expected))
        self.assertEqual(len(actual), 1)
        self.assertIsInstance(actual.args[0], native.Tuple)
        self.assertEqual(tuple(str(value) for value in actual.args[0]),
                         tuple(str(value) for value in next(iter(expected))))

    def test_tuple_result_is_native_owned_and_matches_sympy(self):
        expected = oracle.Tuple(self.locals["x"], 2)
        actual = native.Tuple(native.Symbol("x"), 2)
        self.assertEqual(str(actual), str(expected))
        self.assertEqual(len(actual), 2)
        self.assertEqual(actual._expression.name, "Tuple")
        self.assertEqual(actual._expression.arity, 2)
        values = actual.args
        try:
            self.assertEqual([str(value) for value in values], ["x", "2"])
        finally:
            for value in values:
                value.close()
        last = actual[-1]
        try:
            self.assertEqual(str(last), "2")
        finally:
            last.close()
        sliced = actual[:1]
        try:
            self.assertEqual(str(sliced), "(x,)")
            first = sliced[0]
            try:
                self.assertEqual(str(first), "x")
            finally:
                first.close()
        finally:
            sliced.close()
        actual.close()

    def test_finite_set_result_is_native_owned_and_matches_sympy(self):
        self.assertEqual(str(native.FiniteSet()), str(oracle.FiniteSet()))
        self.assertIs(native.FiniteSet(), native.EmptySet)
        expected = oracle.FiniteSet(oracle.Symbol("set_x"), 2)
        input_symbol = native.Symbol("set_x")
        actual = native.FiniteSet(input_symbol, 2, input_symbol)
        input_symbol.close()
        try:
            self.assertEqual(str(actual), str(expected))
            self.assertEqual(actual._expression.name, "FiniteSet")
            self.assertEqual(actual._expression.arity, 2)
            values = actual.args
            try:
                self.assertEqual(sorted(str(value) for value in values),
                                 ["2", "set_x"])
            finally:
                for value in values:
                    value.close()
            probe = native.Symbol("set_x")
            try:
                self.assertIn(probe, actual)
            finally:
                probe.close()
            self.assertIn(2, actual)
        finally:
            actual.close()

    def test_bounded_matrix_and_det_match_sympy(self):
        oracle_matrix = oracle.Matrix([[1, 2], [3, 4]])
        native_matrix = native.Matrix([[1, 2], [3, 4]])
        self.assertEqual(native_matrix.shape, (2, 2))
        self.assertEqual(str(native_matrix), "Matrix([[1, 2], [3, 4]])")
        self.assertEqual(str(native_matrix[0, 1]), "2")
        self.assertEqual(str(native_matrix.det()), str(oracle_matrix.det()))
        self.assertEqual(str(native.det(native_matrix)), str(oracle.det(oracle_matrix)))
        self.assertEqual(str(native_matrix.rank()), str(oracle_matrix.rank()))
        self.assertEqual(str(native.rank(native_matrix)), str(oracle_matrix.rank()))
        self.assertEqual(str(native_matrix.inv()), str(oracle_matrix.inv()))
        self.assertEqual(str(native_matrix.transpose()), str(oracle_matrix.transpose()))
        self.assertEqual(str(native_matrix.T), str(oracle_matrix.T))
        self.assertEqual(native_matrix.transpose().shape, (2, 2))

        oracle_right = oracle.Matrix([[2, 0], [1, 2]])
        native_right = native.Matrix([[2, 0], [1, 2]])
        self.assertEqual(str(native_matrix * native_right),
                         str(oracle_matrix * oracle_right))
        self.assertEqual(str(native_matrix @ native_right),
                         str(oracle_matrix @ oracle_right))
        self.assertEqual(str(native_matrix + native_right),
                         str(oracle_matrix + oracle_right))
        self.assertEqual(str(native_matrix - native_right),
                         str(oracle_matrix - oracle_right))
        self.assertEqual(str(-native_matrix), str(-oracle_matrix))
        self.assertEqual(str(native_matrix * 2), str(oracle_matrix * 2))
        self.assertEqual(str(2 * native_matrix), str(2 * oracle_matrix))
        self.assertEqual(str(native_matrix / 2), str(oracle_matrix / 2))
        self.assertEqual(
            str(native_matrix / native.Rational(2, 3)),
            str(oracle_matrix / oracle.Rational(2, 3)),
        )
        self.assertEqual(str(native_matrix / 0), str(oracle_matrix / 0))
        oracle_symbolic = oracle.Matrix([[self.locals["x"], 2], [3, 4]])
        native_symbolic = native.Matrix([[native.Symbol("x"), 2], [3, 4]])
        oracle_product = oracle_symbolic * oracle_right
        native_product = native_symbolic * native_right
        for row in range(2):
            for column in range(2):
                self.assert_equivalent(
                    f"symbolic matrix multiplication ({row}, {column})",
                    oracle_product[row, column],
                    native_product[row, column],
                )
        oracle_sum = oracle_symbolic + oracle_right
        native_sum = native_symbolic + native_right
        oracle_difference = oracle_symbolic - oracle_right
        native_difference = native_symbolic - native_right
        for label, expected_matrix, actual_matrix in (
                ("symbolic matrix addition", oracle_sum, native_sum),
                ("symbolic matrix subtraction", oracle_difference,
                 native_difference)):
            for row in range(2):
                for column in range(2):
                    self.assert_equivalent(
                        f"{label} ({row}, {column})",
                        expected_matrix[row, column],
                        actual_matrix[row, column],
                    )
        oracle_negative = -oracle_symbolic
        native_negative = -native_symbolic
        for row in range(2):
            for column in range(2):
                self.assert_equivalent(
                    f"symbolic matrix negation ({row}, {column})",
                    oracle_negative[row, column],
                    native_negative[row, column],
                )
        oracle_division = oracle_symbolic / self.locals["y"]
        native_division = native_symbolic / native.Symbol("y")
        for row in range(2):
            for column in range(2):
                self.assert_equivalent(
                    f"symbolic matrix division ({row}, {column})",
                    oracle_division[row, column],
                    native_division[row, column],
                )
        self.assertEqual(
            str(native.Matrix([[2**62]]) * native.Matrix([[4]])),
            str(oracle.Matrix([[2**62]]) * oracle.Matrix([[4]])),
        )
        with self.assertRaises(ValueError):
            native.Matrix([[1, 2]]) * native.Matrix([[1, 2]])
        with self.assertRaises(ValueError):
            native.Matrix([[1, 2]]) + native.Matrix([[1, 2], [3, 4]])
        with self.assertRaises(TypeError):
            native_matrix + 2
        with self.assertRaises(TypeError):
            2 - native_matrix
        with self.assertRaises(TypeError):
            2 / native_matrix
        with self.assertRaises(TypeError):
            native_matrix / native_right

        oracle_null_matrix = oracle.Matrix([[1, 2, 3], [2, 4, 4]])
        native_null_matrix = native.Matrix([[1, 2, 3], [2, 4, 4]])
        oracle_basis = oracle_null_matrix.nullspace()
        native_basis = native_null_matrix.nullspace()
        self.assertEqual(len(native_basis), len(oracle_basis))
        for actual, expected in zip(native_basis, oracle_basis):
            self.assertEqual(actual.shape, (3, 1))
            self.assertEqual(str(actual), str(expected))
        oracle_reduced, oracle_pivots = oracle_null_matrix.rref()
        native_reduced, native_pivots = native_null_matrix.rref()
        self.assertEqual(str(native_reduced), str(oracle_reduced))
        self.assertEqual(
            tuple(str(value) for value in native_pivots),
            tuple(str(value) for value in oracle_pivots),
        )
        with self.assertRaises(native.UnsupportedOperationError):
            native_null_matrix.nullspace(simplify=True)
        with self.assertRaises(native.UnsupportedOperationError):
            native_null_matrix.nullspace(iszerofunc=lambda value: True)
        with self.assertRaises(native.UnsupportedOperationError):
            native_null_matrix.rref(simplify=True)
        with self.assertRaises(native.UnsupportedOperationError):
            native_null_matrix.rref(pivots=False)

        oracle_singular = oracle.Matrix([[1, 2], [2, 4]])
        native_singular = native.Matrix([[1, 2], [2, 4]])
        self.assertEqual(str(native_singular.rank()), str(oracle_singular.rank()))
        with self.assertRaises(native.UnsupportedOperationError):
            native_singular.inv()

    def test_matrix_refuses_ragged_rows(self):
        with self.assertRaises(ValueError):
            native.Matrix([[1, 2], [3]])

    def test_linsolve_refuses_unverified_forms(self):
        native_x, native_y = native.symbols("linsolve_refusal_x linsolve_refusal_y")
        system = (((1, 2), (3, 4)), (5, 6))
        with self.assertRaises(native.UnsupportedOperationError):
            native.linsolve(system)
        with self.assertRaises(native.UnsupportedOperationError):
            native.linsolve(system, (native_x, native_y), dict=True)
        with self.assertRaises(native.UnsupportedOperationError):
            native.linsolve(
                (((native_x, 2), (3, 4)), (5, 6)),
                (native_x, native_y),
            )
        with self.assertRaises(native.UnsupportedOperationError):
            native.linsolve(
                (((1, 2), (2, 4)), (5, 10)),
                (native_x, native_y),
            )

    def test_power_constructor_identities_match_oracle(self):
        def cases(api):
            x = api.Symbol("power_constructor_x")
            return {
                "zero_exponent": x**0,
                "one_exponent": x**1,
                "one_exponent_sentinel": api.oo**1,
                "one_base": api.Integer(1)**x,
                "one_base_sentinel": api.Integer(1)**api.oo,
                "principal_sqrt_square": api.sqrt(api.pi)**2,
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                if label in ("one_exponent_sentinel", "one_base_sentinel"):
                    self.assertEqual(str(native_cases[label]), str(expected))
                else:
                    self.assert_equivalent(label, expected, native_cases[label])

    def test_relational_constructor_spellings(self):
        oracle_cases = self.relation_cases(oracle)
        native_cases = self.relation_cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assertEqual(str(native_cases[label]), str(expected))

    def test_compound_constructor_matches_oracle_shape(self):
        oracle_x = oracle.Symbol("compound_relation_x")
        native_x = native.Symbol("compound_relation_x")
        oracle_compound = oracle.And(oracle_x > 1, oracle.Ne(oracle_x, 0))
        native_compound = native.And(native_x > 1, native.Ne(native_x, 0))
        self.assertEqual(
            [type(argument).__name__ for argument in oracle_compound.args],
            ["StrictGreaterThan", "Unequality"],
        )
        native_arguments = native_compound.args
        try:
            self.assertEqual(
                [argument.name for argument in native_arguments],
                ["Greater", "Unequal"],
            )
        finally:
            for argument in native_arguments:
                argument.close()
            native_compound.close()

    def test_boolean_constructors_and_operators_match_sympy(self):
        oracle_x, oracle_y = oracle.symbols("boolean_x boolean_y")
        native_x, native_y = native.symbols("boolean_x boolean_y")
        cases = [
            ("And", oracle.And(oracle_x > 0, oracle_y < 1),
             native.And(native_x > 0, native_y < 1)),
            ("Or", oracle.Or(oracle_x > 0, oracle_y < 1),
             native.Or(native_x > 0, native_y < 1)),
            ("Xor", oracle.Xor(oracle_x > 0, oracle_y < 1),
             native.Xor(native_x > 0, native_y < 1)),
            ("Implies", oracle.Implies(oracle_x > 0, oracle_y < 1),
             native.Implies(native_x > 0, native_y < 1)),
            ("Equivalent", oracle.Equivalent(oracle_x > 0, oracle_y < 1),
             native.Equivalent(native_x > 0, native_y < 1)),
        ]
        expected_heads = {
            "And": "And", "Or": "Or", "Xor": "Xor",
            "Implies": "Implies", "Equivalent": "Equivalent",
        }
        for label, expected, actual in cases:
            with self.subTest(label=label):
                self.assertEqual(actual.name, expected_heads[label])
                actual_arguments = actual.args
                try:
                    self.assertEqual(len(actual_arguments), len(expected.args))
                    self.assertEqual(
                        [argument.name for argument in actual_arguments],
                        [
                            {
                                "StrictGreaterThan": "Greater",
                                "StrictLessThan": "Less",
                            }.get(type(argument).__name__, type(argument).__name__)
                            for argument in expected.args
                        ],
                    )
                    self.assertEqual(
                        [str(argument) for argument in actual_arguments],
                        [str(argument) for argument in expected.args],
                    )
                finally:
                    for argument in actual_arguments:
                        argument.close()
                    actual.close()

        operator_cases = [
            (oracle.And(oracle_x > 0, oracle_y < 1),
             (native_x > 0) & (native_y < 1)),
            (oracle.Or(oracle_x > 0, oracle_y < 1),
             (native_x > 0) | (native_y < 1)),
            (oracle.Xor(oracle_x > 0, oracle_y < 1),
             (native_x > 0) ^ (native_y < 1)),
            (oracle.Not(oracle_x > 0), ~(native_x > 0)),
        ]
        for expected, actual in operator_cases:
            with self.subTest(operator=str(expected)):
                self.assertEqual(str(actual), str(expected))
                actual.close()

    def test_boolean_constructor_boundaries_match_sympy(self):
        native_x = native.Symbol("boolean_boundary_x")
        self.assertEqual(native.And(), True)
        self.assertEqual(native.Or(), False)
        self.assertEqual(native.Xor(), False)
        self.assertEqual(native.Equivalent(), True)
        self.assertEqual(native.Not(True), False)
        self.assertEqual(native.Not(False), True)
        self.assertEqual(str(native.And(True, native_x)), str(native_x))
        self.assertEqual(str(native.Or(False, native_x)), str(native_x))
        self.assertEqual(str(native.Implies(False, native_x)), "True")
        self.assertEqual(str(native.Equivalent(True, native_x)), str(native_x))
        native_y = native.Symbol("boolean_boundary_y")
        self.assertEqual(
            str(native.Equivalent(True, native_x, native_y)),
            str(oracle.Equivalent(True, oracle.Symbol("boolean_boundary_x"),
                                  oracle.Symbol("boolean_boundary_y"))),
        )
        self.assertEqual(
            str(native.Equivalent(False, native_x, native_y)),
            str(oracle.Equivalent(False, oracle.Symbol("boolean_boundary_x"),
                                  oracle.Symbol("boolean_boundary_y"))),
        )
        with self.assertRaises(TypeError):
            native.And(2, native_x)
        with self.assertRaises(TypeError):
            native.Or(2, native_x)
        with self.assertRaises(TypeError):
            native.And(native_x + 1, native_y)
        with self.assertRaises(ValueError):
            native.Implies(native_x)
        with self.assertRaises(ValueError):
            native.Implies()
        native_x.close()
        native_y.close()

    def test_assumption_condition_results(self):
        oracle_cases = self.assumption_cases(oracle)
        native_cases = self.assumption_cases(native)
        for assumption, expected in oracle_cases.items():
            with self.subTest(assumption=assumption):
                self.assert_equivalent(assumption, expected, native_cases[assumption])

    def test_exact_domain_predicates_match_oracle(self):
        def cases(api):
            return {
                "integer": api.Integer(2),
                "rational": api.Rational(2, 3),
                "float": api.Float(2.0),
                "unknown": api.Symbol("domain_predicate_unknown"),
                "rational_symbol": api.Symbol(
                    "domain_predicate_rational", rational=True
                ),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            actual = native_cases[label]
            with self.subTest(label=label):
                self.assertEqual(actual.is_rational, expected.is_rational)
                self.assertEqual(actual.is_integer, expected.is_integer)
                self.assertEqual(actual.is_real, expected.is_real)
                self.assertEqual(
                    native.ask(native.Q.rational(actual)),
                    oracle.ask(oracle.Q.rational(expected)),
                )

    def test_number_predicate_matches_oracle(self):
        def cases(api):
            x = api.Symbol("number_predicate_unknown")
            return {
                "integer": api.Integer(2),
                "rational": api.Rational(2, 3),
                "float": api.Float(2.0),
                "pi": api.pi,
                "infinity": api.oo,
                "nan": api.nan,
                "numeric_function": api.sin(1),
                "numeric_sum": api.pi + api.sqrt(2),
                "integer_symbol": api.Symbol(
                    "number_predicate_integer", integer=True
                ),
                "unknown_symbol": x,
                "symbolic_sum": x + 1,
                "relation": api.Eq(x, 1),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assertEqual(native_cases[label].is_number,
                                 expected.is_number)

    def test_count_ops_matches_oracle(self):
        def cases(api):
            x, y, z = api.symbols("count_ops_x count_ops_y count_ops_z")
            return {
                "symbol": x,
                "sum": x + y + z,
                "product": (x + 1) * y,
                "power": (x + 1)**2,
                "quotient": x / (y + 1),
                "function": api.sin(x * y),
                "rational": api.Rational(2, 3),
                "relation": api.Eq(x, 1),
                "compound": api.And(x > 0, x < 1),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assertEqual(
                    native.count_ops(native_cases[label]),
                    oracle.count_ops(expected),
                )
        with self.assertRaises(native.UnsupportedOperationError):
            native.count_ops(native_cases["symbol"], visual=True)

    def test_simultaneous_substitution_matches_oracle(self):
        oracle_x, oracle_y = oracle.symbols(
            "differential_subs_x differential_subs_y"
        )
        native_x, native_y = native.symbols(
            "differential_subs_x differential_subs_y"
        )
        oracle_cases = [
            oracle_x + oracle_y,
            oracle_x + 2*oracle_y,
            oracle.sin(oracle_x) + oracle_y,
        ]
        native_cases = [
            native_x + native_y,
            native_x + 2*native_y,
            native.sin(native_x) + native_y,
        ]
        oracle_replacements = {oracle_x: oracle_y, oracle_y: oracle_x}
        native_replacements = {native_x: native_y, native_y: native_x}
        for oracle_expression, native_expression in zip(oracle_cases, native_cases):
            with self.subTest(expression=str(oracle_expression)):
                expected = oracle_expression.subs(
                    oracle_replacements, simultaneous=True
                )
                actual = native.subs(
                    native_expression, native_replacements, simultaneous=True
                )
                self.assert_equivalent(
                    "simultaneous substitution", expected, actual
                )

    def test_unordered_mapping_substitution_matches_oracle(self):
        def cases(api):
            x, y, z = api.symbols(
                "mapping_order_x mapping_order_y mapping_order_z"
            )
            f = api.Function("mapping_order_f")
            return [
                (f(x**2 + x), {x: y, x**2: z}),
                (f(api.sin(x)), {
                    api.cos(x): z, api.sin(x): api.cos(x)
                }),
                (api.sin(x * api.exp(x)), {x: y, api.exp(x): 3}),
                (x + y, {x: y, y: 2}),
                (x + y, {x: y + 1, y: 2}),
            ]

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for index, ((oracle_expression, oracle_mapping),
                    (native_expression, native_mapping)) in enumerate(
                        zip(oracle_cases, native_cases)
                    ):
            with self.subTest(index=index):
                expected = oracle_expression.subs(oracle_mapping)
                actual = native.subs(native_expression, native_mapping)
                self.assert_equivalent(
                    "unordered mapping substitution", expected, actual
                )

    def test_xreplace_matches_oracle(self):
        def cases(api):
            x, y, z = api.symbols(
                "xreplace_x xreplace_y xreplace_z"
            )
            f = api.Function("xreplace_f")
            return [
                (f(x), {x: y}),
                (x**2 + x, {x**2: y}),
                (f(x + y), {x + y: z, y: x + y}),
                ((x + 1)**2, {x: x + 1}),
            ]

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for index, ((oracle_expression, oracle_mapping),
                    (native_expression, native_mapping)) in enumerate(
                        zip(oracle_cases, native_cases)
                    ):
            with self.subTest(index=index):
                expected = oracle_expression.xreplace(oracle_mapping)
                actual = native_expression.xreplace(native_mapping)
                self.assert_equivalent("xreplace", expected, actual)

    def test_replace_matches_oracle(self):
        def normalized(result):
            if isinstance(result, tuple):
                expression, replacements = result
                return str(expression), {
                    str(key): str(value)
                    for key, value in replacements.items()
                }
            return str(result)

        def cases(api):
            x, y, z = api.symbols("replace_x replace_y replace_z")
            f = api.Function("replace_f")
            return [
                (f(x) + x, x, y, {}),
                (f(x) + x, x, y, {"map": True}),
                (x + 1, x + 1, z, {"exact": False}),
                (x + 1, z, y, {"map": True}),
            ]

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for index, (oracle_case, native_case) in enumerate(
                zip(oracle_cases, native_cases)):
            with self.subTest(index=index):
                oracle_expression, oracle_query, oracle_value, options = oracle_case
                native_expression, native_query, native_value, _ = native_case
                expected = oracle_expression.replace(
                    oracle_query, oracle_value, **options
                )
                actual = native_expression.replace(
                    native_query, native_value, **options
                )
                self.assertEqual(normalized(actual), normalized(expected))

    def test_exact_match_matches_oracle(self):
        def cases(api):
            x, y = api.symbols("match_x match_y")
            f = api.Function("match_f")
            return [
                (x, x),
                (x, y),
                (x + y, y + x),
                (f(x + 1), f(x + 1)),
                (f(x + 1), f(x + 2)),
            ]

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for index, ((oracle_expression, oracle_pattern),
                    (native_expression, native_pattern)) in enumerate(
                        zip(oracle_cases, native_cases)
                    ):
            with self.subTest(index=index):
                self.assertEqual(
                    native_expression.match(native_pattern),
                    oracle_expression.match(oracle_pattern),
                )

    def test_wildcard_match_matches_oracle(self):
        def equivalent(expected, actual):
            if expected is None or actual is None:
                return expected is actual
            expected_values = {
                str(key): value for key, value in expected.items()
            }
            actual_values = {
                str(key): value for key, value in actual.items()
            }
            if expected_values.keys() != actual_values.keys():
                return False
            for key in expected_values:
                expected_value = oracle.sympify(str(expected_values[key]))
                actual_value = oracle.sympify(str(actual_values[key]))
                if oracle.simplify(actual_value - expected_value) != 0:
                    return False
            return True

        def cases(api):
            x, y = api.symbols("wild_x wild_y")
            a = api.Wild("wild_a")
            b = api.Wild("wild_b")
            f = api.Function("wild_f")
            integer = api.Wild(
                "wild_integer",
                properties=(lambda value: value.is_integer is True,),
            )
            return [
                (x, a),
                (x + 1, a + 1),
                (x + y, x + a),
                (2*x, x*a),
                (x*y, x*a),
                (x, x + a),
                (x + y, a + a),
                (x + y, 2*a),
                (x*y, x + a),
                (x + y, a + b),
                (x + y + 1, a + b),
                (x + y + 1, a + b + 1),
                (x + y + x, x + a + b),
                (x*y, a*b),
                (2*x*y, x*a*b),
                (x, a + b),
                (x, a*b),
                (f(x + y), f(a + b)),
                (x, api.Wild("not_x", exclude=(x,))),
                (api.Integer(2), integer),
                (api.Rational(1, 2), integer),
            ]

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for index, ((oracle_expression, oracle_pattern),
                    (native_expression, native_pattern)) in enumerate(
                        zip(oracle_cases, native_cases)
                    ):
            with self.subTest(index=index):
                self.assertTrue(
                    equivalent(
                        oracle_expression.match(oracle_pattern),
                        native_expression.match(native_pattern),
                    )
                )

    def test_free_symbols_matches_oracle(self):
        def names(api, expression):
            if api is oracle:
                return {str(symbol) for symbol in expression.free_symbols}
            symbols = expression.free_symbols
            try:
                return {str(symbol) for symbol in symbols}
            finally:
                for symbol in symbols:
                    symbol.close()

        def cases(api):
            x, y = api.symbols("free_symbols_x free_symbols_y")
            return {
                "sum": x + y,
                "repeated": x + api.sin(x),
                "constant": api.pi + 1,
                "function": api.Function("free_symbols_f")(x),
                "relation": api.Eq(x, y),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assertEqual(
                    names(native, native_cases[label]),
                    names(oracle, expected),
                )

        cached_expression = native_cases["sum"]
        cached_symbols = cached_expression.free_symbols
        for symbol in cached_symbols:
            symbol.close()
        self.assertIs(cached_symbols, cached_expression.free_symbols)
        self.assertEqual({str(symbol) for symbol in cached_symbols},
                         {"free_symbols_x", "free_symbols_y"})

    def test_algebraic_predicate_matches_oracle(self):
        def cases(api):
            unknown = api.Symbol("algebraic_predicate_unknown")
            return {
                "integer": api.Integer(2),
                "rational": api.Rational(2, 3),
                "float": api.Float(2.0),
                "sqrt": api.sqrt(2),
                "imaginary": api.I,
                "pi": api.pi,
                "e": api.E,
                "infinity": api.oo,
                "complex_infinity": api.zoo,
                "nan": api.nan,
                "transcendental_function": api.sin(1),
                "sqrt_transcendental": api.sqrt(api.pi),
                "sqrt_unknown": api.sqrt(unknown),
                "algebraic_symbol": api.Symbol(
                    "algebraic_predicate_symbol", algebraic=True
                ),
                "integer_symbol": api.Symbol(
                    "algebraic_predicate_integer", integer=True
                ),
                "unknown_symbol": unknown,
                "mixed_sum": api.pi + api.sqrt(2),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assertEqual(native_cases[label].is_algebraic,
                                 expected.is_algebraic)

    def test_algebraic_assumption_query_matches_oracle(self):
        def cases(api):
            unknown = api.Symbol("algebraic_query_unknown")
            return {
                "integer": api.Integer(2),
                "rational": api.Rational(2, 3),
                "float": api.Float(2.0),
                "sqrt": api.sqrt(2),
                "imaginary": api.I,
                "pi": api.pi,
                "infinity": api.oo,
                "nan": api.nan,
                "unknown_symbol": unknown,
                "algebraic_symbol": api.Symbol(
                    "algebraic_query_symbol", algebraic=True
                ),
                "sqrt_transcendental": api.sqrt(api.pi),
                "gamma_algebraic": api.gamma(api.sqrt(2)),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assertEqual(
                    native.ask(native.Q.algebraic(native_cases[label])),
                    oracle.ask(oracle.Q.algebraic(expected)),
                )

        oracle_symbol = oracle.Symbol("algebraic_query_scoped")
        native_symbol = native.Symbol("algebraic_query_scoped")
        with oracle.assuming(oracle.Q.integer(oracle_symbol)):
            oracle_value = oracle.ask(oracle.Q.algebraic(oracle_symbol))
        with native.assuming(native.Q.integer(native_symbol)):
            native_value = native.ask(native.Q.algebraic(native_symbol))
        self.assertEqual(native_value, oracle_value)

    def test_guarded_log_exp_matches_oracle(self):
        def cases(api):
            real = api.Symbol("differential_log_real", real=True)
            nonzero = api.Symbol("differential_log_nonzero", nonzero=True)
            unknown = api.Symbol("differential_log_unknown")
            return {
                "log_exp_real": api.simplify(api.log(api.exp(real))),
                "exp_log_nonzero": api.simplify(api.exp(api.log(nonzero))),
                "unknown_log_exp": api.simplify(api.log(api.exp(unknown))),
                "unknown_exp_log": api.simplify(api.exp(api.log(unknown))),
            }

        oracle_cases = cases(oracle)
        native_cases = cases(native)
        for label, expected in oracle_cases.items():
            with self.subTest(label=label):
                self.assert_equivalent(label, expected, native_cases[label])

    def test_principal_sqrt_power_matches_oracle(self):
        oracle_x = oracle.Symbol("differential_sqrt_power")
        native_x = native.Symbol("differential_sqrt_power")
        oracle_raw = oracle.Pow(
            oracle.sqrt(oracle_x, evaluate=False), 2, evaluate=False
        )
        native_raw = native.sqrt(native_x)**2
        self.assert_equivalent(
            "principal sqrt power", oracle.simplify(oracle_raw),
            native.simplify(native_raw),
        )

    def test_complex_domain_operations_match_oracle(self):
        cases = [
            (oracle.re(oracle.I), native.re(native.I)),
            (oracle.im(oracle.I), native.im(native.I)),
            (oracle.conjugate(oracle.Integer(2)),
             native.conjugate(native.Integer(2))),
            (oracle.arg(oracle.Integer(-1)),
             native.arg(native.Integer(-1))),
            (oracle.Abs(oracle.I), native.Abs(native.I)),
            (oracle.Abs(oracle.Integer(2) + oracle.I),
             native.Abs(native.Integer(2) + native.I)),
            (oracle.expand_complex(oracle.exp(oracle.I)),
             native.expand_complex(native.exp(native.I))),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assert_equivalent("complex domain", expected, actual)

        for expected, actual in (
            (oracle.expand_complex(oracle.oo), native.expand_complex(native.oo)),
            (oracle.expand_complex(oracle.zoo), native.expand_complex(native.zoo)),
            (oracle.expand_complex(oracle.nan), native.expand_complex(native.nan)),
        ):
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(actual), str(expected))

        sentinel_cases = []
        for oracle_function, native_function in (
            (oracle.re, native.re), (oracle.im, native.im),
            (oracle.Abs, native.Abs), (oracle.arg, native.arg),
            (oracle.conjugate, native.conjugate),
            (oracle.expand_complex, native.expand_complex),
        ):
            for oracle_value, native_value in (
                (oracle.oo, native.oo), (-oracle.oo, -native.oo),
                (oracle.zoo, native.zoo), (oracle.nan, native.nan),
            ):
                sentinel_cases.append((
                    oracle_function(oracle_value),
                    native_function(native_value),
                ))
        for expected, actual in sentinel_cases:
            with self.subTest(expected=str(expected)):
                expected_parsed = oracle.sympify(
                    str(expected), locals=self.locals
                )
                actual_parsed = oracle.sympify(
                    str(actual), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected_parsed)

        expected_identity = oracle.simplify(oracle.conjugate(oracle.I) + oracle.I)
        actual_identity = native.simplify(native.conjugate(native.I) + native.I)
        self.assertEqual(expected_identity, oracle.Integer(0))
        self.assertTrue(actual_identity.is_zero)

        for function, expression in (
            (native.re, native.Symbol("differential_complex_re")),
            (native.im, native.Symbol("differential_complex_im")),
            (native.conjugate, native.Symbol("differential_complex_conjugate")),
        ):
            with self.subTest(function=function.__name__):
                with self.assertRaises(native.UnsupportedOperationError):
                    function(expression)

    def test_domain_sentinels_match_oracle_spelling(self):
        for name in ("oo", "zoo", "nan"):
            with self.subTest(name=name):
                self.assertEqual(str(getattr(native, name)), str(getattr(oracle, name)))
                self.assertEqual(getattr(native, name).name, name)

    def test_signed_zero_documents_native_ieee_extension(self):
        self.assertEqual(str(oracle.Float(-0.0)), "0.0")
        self.assertEqual(str(native.Float(-0.0)), "-0.0000000000000000E+000")
        self.assertNotEqual(native.Float(-0.0), native.Float(0.0))

    def test_nan_domain_rules_match_oracle(self):
        oracle_x = oracle.Symbol("nan_rule_x")
        native_x = native.Symbol("nan_rule_x")
        cases = [
            (oracle.nan + oracle_x, native.nan + native_x),
            (oracle.nan * 0, native.nan * 0),
            (oracle.sqrt(oracle.nan), native.sqrt(native.nan)),
            (oracle.nan**0, native.nan**0),
            (oracle.nan**oracle_x, native.nan**native_x),
            (oracle_x**oracle.nan, native_x**native.nan),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(native.simplify(actual)), str(expected))

    def test_nan_ordered_special_functions_match_oracle(self):
        oracle_order = oracle.Symbol("nan_order")
        native_order = native.Symbol("nan_order")
        oracle_argument = oracle.Symbol("nan_argument")
        native_argument = native.Symbol("nan_argument")
        cases = [
            (oracle.besselj(oracle.nan, oracle_argument),
             native.besselj(native.nan, native_argument)),
            (oracle.besseli(oracle_order, oracle.nan),
             native.besseli(native_order, native.nan)),
            (oracle.legendre(oracle.nan, oracle_argument),
             native.legendre(native.nan, native_argument)),
            (oracle.besseli(oracle.nan, -oracle.oo),
             native.besseli(native.nan, -native.oo)),
            (oracle.legendre(3, oracle.oo), native.legendre(3, native.oo)),
            (oracle.legendre(3, -oracle.oo),
             native.legendre(3, -native.oo)),
            (oracle.legendre(3, oracle.zoo),
             native.legendre(3, native.zoo)),
            (oracle.legendre(-1, oracle.oo), native.legendre(-1, native.oo)),
            (oracle.legendre(-2, -oracle.oo),
             native.legendre(-2, -native.oo)),
            (oracle.legendre(-3, oracle.oo),
             native.legendre(-3, native.oo)),
            (oracle.legendre(-4, oracle.zoo),
             native.legendre(-4, native.zoo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_text = str(native.simplify(actual)).replace(
                    "legendrep(", "legendre("
                )
                actual_text = actual_text.replace(
                    "legendre(nan, 0, ", "legendre(nan, "
                )
                actual_parsed = oracle.sympify(
                    actual_text, locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)

    def test_directed_domain_rules_match_oracle(self):
        oracle_x = oracle.Symbol("domain_rule_x")
        native_x = native.Symbol("domain_rule_x")
        cases = [
            (oracle.oo + 3, native.oo + 3),
            (oracle.oo + (-oracle.oo), native.oo + (-native.oo)),
            (oracle.oo * 0, native.oo * 0),
            (oracle.oo * 2, native.oo * 2),
            (oracle.oo * -2, native.oo * -2),
            (oracle.oo**0, native.oo**0),
            (oracle.oo**2, native.oo**2),
            (oracle.oo**-2, native.oo**-2),
            ((-oracle.oo)**2, (-native.oo)**2),
            ((-oracle.oo)**3, (-native.oo)**3),
            (oracle.zoo + 1, native.zoo + 1),
            (oracle.zoo + oracle.zoo, native.zoo + native.zoo),
            (oracle.zoo + oracle.oo, native.zoo + native.oo),
            (oracle.zoo * 0, native.zoo * 0),
            (oracle.zoo * 2, native.zoo * 2),
            (oracle.zoo * oracle.oo, native.zoo * native.oo),
            (oracle.zoo**0, native.zoo**0),
            (oracle.zoo**2, native.zoo**2),
            (oracle.zoo**-2, native.zoo**-2),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(native.simplify(actual)), str(expected))
        for label, expected, actual in [
            ("symbolic oo product", oracle.oo * oracle_x,
             native.simplify(native.oo * native_x)),
            ("symbolic zoo product", oracle.zoo * oracle_x,
             native.simplify(native.zoo * native_x)),
        ]:
            with self.subTest(label=label):
                expected_text = str(expected)
                actual_text = str(actual)
                self.assertEqual(
                    oracle.sympify(actual_text, locals=self.locals),
                    oracle.sympify(expected_text, locals=self.locals),
                )

    def test_directed_domain_functions_match_oracle(self):
        cases = [
            (oracle.sqrt(oracle.oo), native.sqrt(native.oo)),
            (oracle.sqrt(oracle.zoo), native.sqrt(native.zoo)),
            (oracle.Abs(-oracle.oo), native.Abs(-native.oo)),
            (oracle.Abs(oracle.zoo), native.Abs(native.zoo)),
            (oracle.exp(oracle.oo), native.exp(native.oo)),
            (oracle.exp(-oracle.oo), native.exp(-native.oo)),
            (oracle.exp(oracle.zoo), native.exp(native.zoo)),
            (oracle.log(oracle.oo), native.log(native.oo)),
            (oracle.log(-oracle.oo), native.log(-native.oo)),
            (oracle.log(oracle.zoo), native.log(native.zoo)),
            (oracle.log(0), native.log(0)),
            (oracle.exp(oracle.log(0)), native.exp(native.log(0))),
            (oracle.atanh(1), native.atanh(1)),
            (oracle.atanh(-1), native.atanh(-1)),
            (oracle.gamma(0), native.gamma(0)),
            (oracle.gamma(-3), native.gamma(-3)),
            (oracle.loggamma(0), native.loggamma(0)),
            (oracle.loggamma(-2), native.loggamma(-2)),
            (oracle.factorial(-1), native.factorial(-1)),
            (oracle.factorial(0), native.factorial(0)),
            (oracle.factorial(5), native.factorial(5)),
            (oracle.factorial(20), native.factorial(20)),
            (oracle.factorial(21), native.factorial(21)),
            (oracle.factorial(100), native.factorial(100)),
            (oracle.sin(oracle.zoo), native.sin(native.zoo)),
            (oracle.cos(oracle.zoo), native.cos(native.zoo)),
            (oracle.tan(oracle.zoo), native.tan(native.zoo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(native.simplify(actual)), str(expected))
        for expected, actual in (
            (oracle.log(-2), native.log(-2)),
            (oracle.log(oracle.Rational(-2, 3)),
             native.log(native.Rational(-2, 3))),
            (oracle.log(oracle.I), native.log(native.I)),
            (oracle.log(-oracle.I), native.log(-native.I)),
            (oracle.acosh(0), native.acosh(0)),
            (oracle.acosh(-1), native.acosh(-1)),
            (oracle.acosh(oracle.I), native.acosh(native.I)),
            (oracle.acosh(-oracle.I), native.acosh(-native.I)),
            (oracle.asin(oracle.I), native.asin(native.I)),
            (oracle.asin(-oracle.I), native.asin(-native.I)),
            (oracle.acos(oracle.I), native.acos(native.I)),
            (oracle.acos(-oracle.I), native.acos(-native.I)),
            (oracle.asin(oracle.Rational(1, 2)),
             native.asin(native.Rational(1, 2))),
            (oracle.asin(oracle.Rational(-1, 2)),
             native.asin(native.Rational(-1, 2))),
            (oracle.asin(oracle.sqrt(2) / 2),
             native.asin(native.sqrt(2) / 2)),
            (oracle.asin(-oracle.sqrt(2) / 2),
             native.asin(-native.sqrt(2) / 2)),
            (oracle.asin(oracle.sqrt(3) / 2),
             native.asin(native.sqrt(3) / 2)),
            (oracle.asin(-oracle.sqrt(3) / 2),
             native.asin(-native.sqrt(3) / 2)),
            (oracle.acos(oracle.Rational(1, 2)),
             native.acos(native.Rational(1, 2))),
            (oracle.acos(oracle.Rational(-1, 2)),
             native.acos(native.Rational(-1, 2))),
            (oracle.acos(oracle.sqrt(2) / 2),
             native.acos(native.sqrt(2) / 2)),
            (oracle.acos(-oracle.sqrt(2) / 2),
             native.acos(-native.sqrt(2) / 2)),
            (oracle.acos(oracle.sqrt(3) / 2),
             native.acos(native.sqrt(3) / 2)),
            (oracle.acos(-oracle.sqrt(3) / 2),
             native.acos(-native.sqrt(3) / 2)),
            (oracle.atan(oracle.sqrt(3)), native.atan(native.sqrt(3))),
            (oracle.atan(-oracle.sqrt(3)), native.atan(-native.sqrt(3))),
            (oracle.atan(1 / oracle.sqrt(3)),
             native.atan(1 / native.sqrt(3))),
            (oracle.atan(-1 / oracle.sqrt(3)),
             native.atan(-1 / native.sqrt(3))),
            (oracle.asinh(1), native.asinh(1)),
            (oracle.asinh(-1), native.asinh(-1)),
            (oracle.sqrt(-1), native.sqrt(-1)),
            (oracle.sqrt(-4), native.sqrt(-4)),
            (oracle.sqrt(oracle.Rational(-4, 9)),
             native.sqrt(native.Rational(-4, 9))),
        ):
            with self.subTest(expected=str(expected)):
                self.assert_equivalent(
                    "exact inverse branch",
                    expected, native.simplify(actual),
                )
        expected_text = str(oracle.I * oracle.oo)
        actual_text = str(native.simplify(native.sqrt(-native.oo)))
        self.assertEqual(
            oracle.sympify(actual_text, locals=self.locals),
            oracle.sympify(expected_text, locals=self.locals),
        )

    def test_periodic_infinity_refusals_match_oracle_boundary(self):
        for function in (oracle.sin, oracle.cos, oracle.tan):
            with self.subTest(function=function.__name__):
                self.assertIsInstance(function(oracle.oo), oracle.AccumBounds)
                actual = getattr(native, function.__name__)(native.oo)
                self.assertIsInstance(native.simplify(actual), native.Function)

    def test_noninteger_domain_powers_match_oracle(self):
        cases = [
            (oracle.oo**oracle.Rational(1, 2),
             native.oo**native.Rational(1, 2)),
            (oracle.oo**oracle.Rational(2, 3),
             native.oo**native.Rational(2, 3)),
            (oracle.oo**oracle.Rational(-1, 2),
             native.oo**native.Rational(-1, 2)),
            (oracle.zoo**oracle.Rational(1, 2),
             native.zoo**native.Rational(1, 2)),
            (oracle.zoo**oracle.Rational(4, 3),
             native.zoo**native.Rational(4, 3)),
            (oracle.zoo**oracle.Rational(-1, 2),
             native.zoo**native.Rational(-1, 2)),
            ((-oracle.oo)**oracle.Rational(1, 3),
             (-native.oo)**native.Rational(1, 3)),
            ((-oracle.oo)**oracle.Rational(2, 3),
             (-native.oo)**native.Rational(2, 3)),
            ((-oracle.oo)**oracle.Rational(4, 3),
             (-native.oo)**native.Rational(4, 3)),
            ((-oracle.oo)**oracle.Rational(-1, 3),
             (-native.oo)**native.Rational(-1, 3)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(native.simplify(actual)), str(expected))
        for expected, actual in [
            (oracle.I * oracle.oo,
             native.simplify((-native.oo)**native.Rational(1, 2))),
            (-oracle.I * oracle.oo,
             native.simplify((-native.oo)**native.Rational(3, 2))),
        ]:
            expected_text = str(expected)
            actual_text = str(actual)
            self.assertEqual(
                oracle.sympify(actual_text, locals=self.locals),
                oracle.sympify(expected_text, locals=self.locals),
            )

    def test_direct_domain_heads_match_oracle(self):
        cases = [
            (oracle.sign(oracle.oo), native.sign(native.oo)),
            (oracle.sign(-oracle.oo), native.sign(-native.oo)),
            (oracle.sign(oracle.zoo), native.sign(native.zoo)),
            (oracle.floor(oracle.oo), native.floor(native.oo)),
            (oracle.floor(-oracle.oo), native.floor(-native.oo)),
            (oracle.floor(oracle.zoo), native.floor(native.zoo)),
            (oracle.ceiling(oracle.oo), native.ceiling(native.oo)),
            (oracle.ceiling(-oracle.oo), native.ceiling(-native.oo)),
            (oracle.ceiling(oracle.zoo), native.ceiling(native.zoo)),
            (oracle.sinh(oracle.oo), native.sinh(native.oo)),
            (oracle.sinh(-oracle.oo), native.sinh(-native.oo)),
            (oracle.sinh(oracle.zoo), native.sinh(native.zoo)),
            (oracle.cosh(-oracle.oo), native.cosh(-native.oo)),
            (oracle.cosh(oracle.zoo), native.cosh(native.zoo)),
            (oracle.tanh(oracle.oo), native.tanh(native.oo)),
            (oracle.tanh(-oracle.oo), native.tanh(-native.oo)),
            (oracle.tanh(oracle.zoo), native.tanh(native.zoo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                self.assertEqual(str(native.simplify(actual)), str(expected))

    def test_inverse_domain_heads_match_oracle(self):
        cases = [
            (oracle.asin(oracle.oo), native.asin(native.oo)),
            (oracle.asin(-oracle.oo), native.asin(-native.oo)),
            (oracle.asin(oracle.zoo), native.asin(native.zoo)),
            (oracle.asin(oracle.I), native.asin(native.I)),
            (oracle.asin(-oracle.I), native.asin(-native.I)),
            (oracle.acos(oracle.I), native.acos(native.I)),
            (oracle.acos(-oracle.I), native.acos(-native.I)),
            (oracle.acos(oracle.oo), native.acos(native.oo)),
            (oracle.acos(-oracle.oo), native.acos(-native.oo)),
            (oracle.acos(oracle.zoo), native.acos(native.zoo)),
            (oracle.atan(oracle.oo), native.atan(native.oo)),
            (oracle.atan(-oracle.oo), native.atan(-native.oo)),
            (oracle.atan(oracle.I), native.atan(native.I)),
            (oracle.atan(-oracle.I), native.atan(-native.I)),
            (oracle.asinh(oracle.oo), native.asinh(native.oo)),
            (oracle.asinh(-oracle.oo), native.asinh(-native.oo)),
            (oracle.asinh(oracle.zoo), native.asinh(native.zoo)),
            (oracle.asinh(oracle.I), native.asinh(native.I)),
            (oracle.asinh(-oracle.I), native.asinh(-native.I)),
            (oracle.atanh(oracle.I), native.atanh(native.I)),
            (oracle.atanh(-oracle.I), native.atanh(-native.I)),
            (oracle.acosh(oracle.oo), native.acosh(native.oo)),
            (oracle.acosh(-oracle.oo), native.acosh(-native.oo)),
            (oracle.acosh(oracle.zoo), native.acosh(native.zoo)),
            (oracle.atanh(oracle.oo), native.atanh(native.oo)),
            (oracle.atanh(-oracle.oo), native.atanh(-native.oo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)
        for function in (oracle.atan, oracle.atanh):
            with self.subTest(function=function.__name__):
                expected = function(oracle.zoo)
                self.assertNotIsInstance(expected, oracle.Function)
                actual = getattr(native, function.__name__)(native.zoo)
                self.assertIsInstance(native.simplify(actual), native.Function)

    def test_reciprocal_hyperbolic_domain_heads_match_oracle(self):
        cases = [
            (oracle.csch(oracle.oo), native.csch(native.oo)),
            (oracle.csch(-oracle.oo), native.csch(-native.oo)),
            (oracle.csch(oracle.zoo), native.csch(native.zoo)),
            (oracle.sech(oracle.oo), native.sech(native.oo)),
            (oracle.sech(-oracle.oo), native.sech(-native.oo)),
            (oracle.sech(oracle.zoo), native.sech(native.zoo)),
            (oracle.coth(oracle.oo), native.coth(native.oo)),
            (oracle.coth(-oracle.oo), native.coth(-native.oo)),
            (oracle.coth(oracle.zoo), native.coth(native.zoo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)

    def test_error_function_domain_heads_match_oracle(self):
        cases = [
            (oracle.erf(oracle.oo), native.erf(native.oo)),
            (oracle.erf(-oracle.oo), native.erf(-native.oo)),
            (oracle.erfc(oracle.oo), native.erfc(native.oo)),
            (oracle.erfc(-oracle.oo), native.erfc(-native.oo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)
        for function in (oracle.erf, oracle.erfc):
            with self.subTest(function=function.__name__):
                expected = function(oracle.zoo)
                self.assertIsInstance(expected, oracle.Function)
                actual = getattr(native, function.__name__)(native.zoo)
                self.assertIsInstance(native.simplify(actual), native.Function)

    def test_atan2_domain_heads_match_oracle(self):
        cases = [
            (oracle.atan2(oracle.oo, oracle.oo),
             native.atan2(native.oo, native.oo)),
            (oracle.atan2(-oracle.oo, oracle.oo),
             native.atan2(-native.oo, native.oo)),
            (oracle.atan2(oracle.oo, -oracle.oo),
             native.atan2(native.oo, -native.oo)),
            (oracle.atan2(-oracle.oo, -oracle.oo),
             native.atan2(-native.oo, -native.oo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)

        # SymPy assigns a value to this complex-infinity case; the declared
        # native slice refuses it because zoo has no direction.
        actual = native.atan2(native.zoo, native.oo)
        self.assertIsInstance(native.simplify(actual), native.Function)

    def test_bessel_domain_heads_match_oracle(self):
        oracle_order = oracle.Symbol("bessel_order")
        native_order = native.Symbol("bessel_order")
        cases = [
            (oracle.besselj(oracle_order, oracle.oo),
             native.besselj(native_order, native.oo)),
            (oracle.besselj(oracle_order, -oracle.oo),
             native.besselj(native_order, -native.oo)),
            (oracle.besseli(oracle_order, oracle.oo),
             native.besseli(native_order, native.oo)),
            (oracle.besseli(oracle_order, -oracle.oo),
             native.besseli(native_order, -native.oo)),
            (oracle.besseli(1, -oracle.oo),
             native.besseli(1, -native.oo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)

        for actual in (
            native.besselj(native_order, native.zoo),
            native.besseli(native_order, native.zoo),
        ):
            with self.subTest(actual=str(actual)):
                self.assertIsInstance(native.simplify(actual), native.Function)

    def test_legendre_domain_heads_match_oracle(self):
        cases = [
            (oracle.legendre(2, oracle.oo), native.legendre(2, native.oo)),
            (oracle.legendre(1, -oracle.oo),
             native.legendre(1, -native.oo)),
            (oracle.legendre(2, -oracle.oo),
             native.legendre(2, -native.oo)),
            (oracle.legendre(0, oracle.zoo),
             native.legendre(0, native.zoo)),
            (oracle.legendre(2, oracle.zoo),
             native.legendre(2, native.zoo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)
        actual = native.legendre(native.Rational(1, 2), native.oo)
        self.assertIsInstance(native.simplify(actual), native.Function)

    def test_gamma_domain_heads_match_oracle(self):
        cases = [
            (oracle.gamma(oracle.oo), native.gamma(native.oo)),
            (oracle.factorial(oracle.oo), native.factorial(native.oo)),
            (oracle.loggamma(oracle.oo), native.loggamma(native.oo)),
            (oracle.loggamma(-oracle.oo), native.loggamma(-native.oo)),
            (oracle.loggamma(oracle.zoo), native.loggamma(native.zoo)),
        ]
        for expected, actual in cases:
            with self.subTest(expected=str(expected)):
                actual_parsed = oracle.sympify(
                    str(native.simplify(actual)), locals=self.locals
                )
                self.assertEqual(actual_parsed, expected)
        for expected, actual in [
            (oracle.gamma(-oracle.oo), native.gamma(-native.oo)),
            (oracle.gamma(oracle.zoo), native.gamma(native.zoo)),
            (oracle.factorial(-oracle.oo), native.factorial(-native.oo)),
            (oracle.factorial(oracle.zoo), native.factorial(native.zoo)),
        ]:
            with self.subTest(expected=str(expected)):
                self.assertIsInstance(expected, oracle.Function)
                self.assertIsInstance(native.simplify(actual), native.Function)

    def test_expand_cache_matches_oracle_and_invalidates_on_assumptions(self):
        oracle_x = oracle.Symbol("differential_expand_cache_x")
        native_x = native.Symbol("differential_expand_cache_x")
        oracle_expression = (oracle.sqrt(oracle_x**2) + 1)**2
        native_expression = (native.sqrt(native_x**2) + 1)**2

        oracle_first = oracle.expand(oracle_expression)
        oracle_second = oracle.expand(oracle_expression)
        native_first = native.expand(native_expression)
        native_second = native.expand(native_expression)
        self.assertEqual(
            oracle_second is oracle_first, native_second is native_first
        )
        self.assert_equivalent("unknown expansion", oracle_first, native_first)

        oracle_real = oracle.Symbol("differential_expand_cache_x", real=True)
        native.Symbol("differential_expand_cache_x", real=True)
        expected_real = oracle.expand((oracle.sqrt(oracle_real**2) + 1)**2)
        actual_real = native.expand(native_expression)
        self.assert_equivalent("assumption-invalidated expansion", expected_real,
                                actual_real)

    def test_differentiation_cache_matches_oracle(self):
        oracle_x, oracle_y = oracle.symbols("differential_diff_cache_x y")
        native_x, native_y = native.symbols("differential_diff_cache_x y")
        oracle_expression = oracle.exp(oracle_x * oracle_y)
        native_expression = native.exp(native_x * native_y)
        oracle_first = oracle.diff(oracle_expression, oracle_x)
        oracle_second = oracle.diff(oracle_expression, oracle_x)
        native_first = native.diff(native_expression, native_x)
        native_second = native.diff(native_expression, native_x)
        self.assertIs(oracle_second, oracle_first)
        self.assertIs(native_second, native_first)
        self.assert_equivalent("first cached derivative", oracle_first,
                                native_first)
        self.assert_equivalent("second cached derivative", oracle_second,
                                native_second)

    def test_three_valued_zero_predicates(self):
        oracle_cases = self.predicate_cases(oracle)
        native_cases = self.predicate_cases(native)
        for expected, actual in zip(oracle_cases, native_cases):
            with self.subTest(expression=str(expected)):
                self.assertEqual(actual.is_zero, expected.is_zero)
                self.assertEqual(actual.is_nonzero, expected.is_nonzero)

    def test_supported_assumption_predicates(self):
        for assumption in ("real", "positive", "nonnegative", "nonzero"):
            with self.subTest(assumption=assumption):
                oracle_symbol = oracle.Symbol("predicate_" + assumption,
                                              **{assumption: True})
                native_symbol = native.Symbol("predicate_" + assumption,
                                              **{assumption: True})
                for name in ("is_real", "is_positive", "is_nonnegative",
                             "is_nonzero"):
                    self.assertEqual(
                        getattr(native_symbol, name),
                        getattr(oracle_symbol, name),
                    )

    def test_scoped_assumption_context(self):
        self.assertEqual(
            self.scoped_assumption_trace(oracle),
            self.scoped_assumption_trace(native),
        )

    def test_exceptions_and_unevaluated_objects(self):
        oracle_x = oracle.Symbol("x")
        native_x = native.Symbol("x")

        oracle_unevaluated = oracle.Add(1, 2, evaluate=False)
        self.assertEqual(str(oracle_unevaluated), "1 + 2")
        with self.assertRaises(native.UnsupportedOperationError):
            native.Add(1, 2, evaluate=False)

        native_derivative = native.Derivative(native_x**2, native_x, evaluate=False)
        self.assertIsInstance(native_derivative, native.Derivative)
        self.assert_equivalent(
            "unevaluated derivative doit",
            oracle.Derivative(oracle_x**2, oracle_x).doit(),
            native_derivative.doit(),
        )

        refusals = []
        for label, oracle_call, native_call in refusals:
            with self.subTest(label=label):
                self.assertIsNotNone(oracle_call())
                with self.assertRaises(native.UnsupportedOperationError):
                    native_call()

    def test_oracle_is_real_sympy_and_not_an_adapter_alias(self):
        self.assertIs(sys.modules["sympy"], oracle)
        self.assertIsNot(oracle.Symbol, native.Symbol)
        self.assertEqual(str(native.Integer(2**100)), str(2**100))
        self.assertEqual(str(native.Rational(2, 3)), "2/3")


if __name__ == "__main__":
    unittest.main()
