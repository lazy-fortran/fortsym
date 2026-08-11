import subprocess
import sys
import unittest

import fortsym
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
        self.assertEqual(sp.factor(x**2 + 2*x + 1), (x + 1)**2)
        self.assertEqual(sp.simplify(sp.diff(sp.exp(x*y), x)), y*sp.exp(x*y))
        self.assertEqual(sp.subs((x + 1) ** 2, {x: 2}), 9)

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
            sp.symbols("z", integer=True)
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
        self.assertIsInstance(
            sp.simplify(sp.besseli(order, -sp.oo)), sp.Function
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
        refused = sp.simplify((-sp.oo)**sp.Rational(2, 3))
        self.assertIsInstance(refused, sp.Pow)
        self.assertNotEqual(refused, sp.oo)

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
        real = sp.Symbol("predicate_real", real=True)
        positive = sp.Symbol("predicate_positive", positive=True)
        nonnegative = sp.Symbol("predicate_nonnegative", nonnegative=True)
        nonzero = sp.Symbol("predicate_nonzero", nonzero=True)

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
