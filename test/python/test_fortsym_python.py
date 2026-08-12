import gc
import subprocess
import sys
import unittest
from fractions import Fraction

import fortsym


class NativePackageTest(unittest.TestCase):
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


if __name__ == "__main__":
    unittest.main()
