"""Seeded principal-radical differential oracle for issue #92."""

import random
import sys

try:
    import sympy as oracle
except ModuleNotFoundError as error:
    if error.name != "sympy":
        raise
    print("SKIP: SymPy is not installed")
    sys.exit(77)

import fortsym.sympy as native


def cases(api):
    rng = random.Random(92)
    x, y = api.symbols("x y")
    bases = [x, x*y, 1-x**2, 2+x+y**2, api.sqrt(x)+y, (1+x)/(1+y)]
    for i in range(120):
        b = bases[i % len(bases)]
        q = rng.choice([2, 3, 4, 5])
        p = rng.choice([1, 2, 3])
        g = b**api.Rational(p, q)
        c = rng.randint(1, 4)
        residual = c*(g**(q+1) - b**p*g)
        yield f"defining identity {i}", residual, True
        yield f"perturbed nonidentity {i}", residual + c, False
    for i in range(30):
        b = bases[i % len(bases)]
        q, r = rng.choice([(2, 3), (3, 4), (2, 5)])
        residual = b**api.Rational(1, q)*b**api.Rational(1, r)
        residual -= b**api.Rational(q+r, q*r)
        yield f"shared root {i}", residual, True
    yield "square root branch", api.sqrt(x**2)-x, False
    yield "product branch", api.sqrt(x*y)-api.sqrt(x)*api.sqrt(y), False
    yield "nested branch", (x**2)**api.Rational(1, 3)-x**api.Rational(2, 3), False
    g = x**api.Rational(3, 2)
    yield "powered rational product", g*g**2-x**3*g, True
    yield "powered rational nonidentity", g*g**2-x**6*g, False


def main():
    identities = nonidentities = unknown = 0
    for (label, actual, expected), (ref_label, reference, _) in zip(
        cases(native), cases(oracle), strict=True
    ):
        assert label == ref_label
        if "branch" in label:
            x, y = oracle.symbols("x y")
            witness = oracle.simplify(reference.subs({x: -1, y: -1}))
            assert witness.is_zero is False, (label, witness)
        else:
            reduced = oracle.simplify(reference)
            if expected:
                assert reduced.is_zero is True, (label, reduced)
            else:
                x, y = oracle.symbols("x y")
                witness = oracle.simplify(reduced.subs({x: 2, y: 3}))
                assert witness.is_zero is False, (label, witness)
        verdict = actual.is_zero
        if expected:
            assert verdict is True, (label, actual, verdict)
            identities += 1
        else:
            assert verdict is not True, (label, actual, verdict)
            nonidentities += 1
            unknown += verdict is None
    print(f"PASS SymPy {oracle.__version__}: {identities} identities, "
          f"{nonidentities} nonidentities ({unknown} unknown), 0 false proofs")


if __name__ == "__main__":
    main()
