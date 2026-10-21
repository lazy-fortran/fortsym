"""Independently authored exact scalar derivation; ordinary SymPy source."""
import sympy as sp

x = sp.Symbol('x', positive=True)
t = sp.Symbol('t', real=True)
q = sp.Rational(9007199254740993, 10)
f = (x + 1)**3 + q
derivative = sp.diff(f, x)
at_two = derivative.subs(x, sp.Integer(2))
root = sp.sqrt(x**2)
denominator = t - 1
cancelled = sp.cancel((t**2 - 1) / denominator)

# Independent polynomial, exact-integer and domain witnesses.
assert at_two == 27
assert sp.simplify(q * 10 - 9007199254740993) == 0
assert sp.simplify(derivative - (3*x*x + 6*x + 3)) == 0
assert sp.simplify(root - x) == 0
assert sp.simplify(cancelled - (t + 1)) == 0
assert (2**2 - 1) // (2 - 1) == 3
named = dict(polynomial=f, derivative=derivative, at_two=at_two,
             positive_root=root, cancelled=cancelled, exact_q=q)

# Arbitrary Python assertions are not inferred. Register explicit witnesses.
if 'record' in globals():
    record.check_zero(derivative - (3*x*x + 6*x + 3), 'polynomial coefficients')
    record.check_zero(root - x, 'positive square root')
    record.condition_nonzero(denominator, 'excluded t=1 pole')
    record.check_unknown(sp.sqrt(t**2) - t, 'unresolved square root branch')
    for label, value in named.items():
        record.name(label, value)
