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

    def test_exact_and_symbolic_results(self):
        oracle_x, oracle_y = oracle.symbols("x y")
        native_x, native_y = native.symbols("x y")
        cases = [
            ("integer", oracle.Integer(2**100), native.Integer(2**100)),
            ("rational", oracle.Rational(2, 3), native.Rational(2, 3)),
            ("addition", oracle_x + oracle_y, native_x + native_y),
            ("multiplication", (oracle_x + 1) * oracle_y,
             (native_x + 1) * native_y),
            ("power", (oracle_x + 1)**2, (native_x + 1)**2),
            ("function", oracle.exp(oracle_x * oracle_y),
             native.exp(native_x * native_y)),
            ("derivative", oracle.diff(oracle.exp(oracle_x * oracle_y), oracle_x),
             native.diff(native.exp(native_x * native_y), native_x)),
            ("substitution", oracle.expand((oracle_x + 1)**2).subs(oracle_x, 2),
             native.subs(native.expand((native_x + 1)**2), native_x, 2)),
            ("expansion", oracle.expand((oracle_x + 1)**2),
             native.expand((native_x + 1)**2)),
            ("simplification", oracle.simplify(oracle.sqrt(oracle_x**2)),
             native.simplify(native.sqrt(native_x**2))),
            ("factorisation", oracle.factor(oracle_x**2 + 2*oracle_x + 1),
             native.factor(native_x**2 + 2*native_x + 1)),
        ]
        for label, expected, actual in cases:
            with self.subTest(label=label):
                self.assert_equivalent(label, expected, actual)

    def test_assumption_condition_results(self):
        assumptions = ("real", "nonnegative", "positive")
        for assumption in assumptions:
            with self.subTest(assumption=assumption):
                oracle_x = oracle.Symbol("condition_" + assumption, **{assumption: True})
                native_x = native.Symbol("condition_" + assumption, **{assumption: True})
                expected = oracle.simplify(oracle.sqrt(oracle_x**2))
                actual = native.simplify(native.sqrt(native_x**2))
                self.assert_equivalent(assumption, expected, actual)

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

        refusals = [
            ("together", lambda: oracle.together((oracle_x + 1) / (oracle_x + 2)),
             lambda: native.together((native_x + 1) / (native_x + 2))),
            ("cancel", lambda: oracle.cancel((oracle_x**2 - 1) / (oracle_x - 1)),
             lambda: native.cancel((native_x**2 - 1) / (native_x - 1))),
            ("apart", lambda: oracle.apart(1 / (oracle_x * (oracle_x + 1)), oracle_x),
             lambda: native.apart(1 / (native_x * (native_x + 1)), native_x)),
            ("collect", lambda: oracle.collect(oracle_x + oracle_x*oracle.Symbol("y"), oracle_x),
             lambda: native.collect(native_x + native_x*native.Symbol("y"), native_x)),
            ("integrate", lambda: oracle.integrate(oracle.sin(oracle_x), oracle_x),
             lambda: native.integrate(native.sin(native_x), native_x)),
            ("limit", lambda: oracle.limit(oracle.sin(oracle_x) / oracle_x, oracle_x, 0),
             lambda: native.limit(native.sin(native_x) / native_x, native_x, 0)),
            ("series", lambda: oracle.series(oracle.exp(oracle_x), oracle_x, 0, 3),
             lambda: native.series(native.exp(native_x), native_x, 0, 3)),
            ("solve", lambda: oracle.solve(oracle_x**2 - 1, oracle_x),
             lambda: native.solve(native_x**2 - 1, native_x)),
            ("Matrix", lambda: oracle.Matrix([[1]]),
             lambda: native.Matrix([[1]])),
        ]
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
