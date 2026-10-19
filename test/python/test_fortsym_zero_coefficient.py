"""Issue #91: a substituted zero has zero coefficients in fresh processes."""

import subprocess
import sys
from pathlib import Path


def check_zero_coefficient():
    import fortsym.sympy as sp

    z, d, kk = sp.symbols("z d kk")
    u = sp.Symbol("imaginary_unit")
    e = ((2 - z)*(-d - kk)).subs(z, sp.Integer(2))
    p = e.subs(sp.I, u)
    assert p == 0, p
    for degree in (0, 1, 2):
        coefficient = p.coefficient(u, degree)
        assert coefficient == 0, (degree, coefficient)


if __name__ == "__main__":
    if sys.argv[1:] == ["--child"]:
        check_zero_coefficient()
    else:
        for run in range(100):
            child = subprocess.run(
                [sys.executable, str(Path(__file__).resolve()), "--child"],
                capture_output=True, text=True, timeout=30,
            )
            if child.returncode:
                raise AssertionError(
                    f"Fresh process {run + 1}/100 failed ({child.returncode}):\n"
                    f"{child.stdout}{child.stderr}"
                )
        print("Zero coefficient: 100 fresh processes passed")
