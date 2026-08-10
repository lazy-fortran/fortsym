import sys
import unittest

import fortsym.sympy as sp


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
        self.assertEqual(sp.simplify(sp.diff(sp.exp(x*y), x)), y*sp.exp(x*y))
        self.assertEqual(sp.subs((x + 1) ** 2, {x: 2}), 9)

    def test_refusal_and_truth_contract(self):
        self.assertFalse(bool(sp.Integer(0)))
        with self.assertRaises(TypeError):
            bool(sp.Symbol("z"))
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.factor(sp.Symbol("z") + 1)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.symbols("z", integer=True)
        with self.assertRaises(sp.UnsupportedOperationError):
            sp.symbols("z", positive=False)
        self.assertNotIn("sympy", sys.modules)

    def test_supported_assumptions_reach_native_simplifier(self):
        x = sp.Symbol("x", real=True)
        self.assertEqual(sp.simplify(sp.sqrt(x**2)), sp.Abs(x))

        y = sp.Symbol("y", nonnegative=True)
        self.assertEqual(sp.simplify(sp.sqrt(y**2)), y)

        z = sp.Symbol("z", positive=True)
        self.assertEqual(sp.simplify(sp.sqrt(z**2)), z)
        self.assertEqual(sp.simplify(sp.Abs(z)), z)


if __name__ == "__main__":
    unittest.main()
