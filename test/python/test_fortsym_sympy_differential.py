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
            "real", "nonnegative", "positive", "negative", "nonpositive", "zero"
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

    def test_assumption_condition_results(self):
        oracle_cases = self.assumption_cases(oracle)
        native_cases = self.assumption_cases(native)
        for assumption, expected in oracle_cases.items():
            with self.subTest(assumption=assumption):
                self.assert_equivalent(assumption, expected, native_cases[assumption])

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
