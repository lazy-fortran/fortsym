# Unsimplified composed exponents

The low-level `fortsym_diff` derivative retains unsimplified expressions.
Literal exact exponents preserve polynomial domains under repeated
differentiation. A composed constant exponent still requires normalization
before differentiation: the fourth derivative of `x**(2+1)` and the second
derivative of `x**(1+0)` can emit singular zero factors at `x=0`.

This manual reproducer emits and compiles both derivatives, then requires two
finite zeros at `x=0`. It currently fails that independent oracle with NaNs.
It lives outside the registered ordinary test inventory because it records an
open limitation rather than passing evidence.

With a configured FortSym build, run from a separate writable scratch directory:

```sh
gfortran -I /absolute/fortsym/build/include \
  /absolute/fortsym/doc/reproducers/composed-exponent/reproduce.f90 \
  -L /absolute/fortsym/build/lib -lfortsym \
  -Wl,-rpath,/absolute/fortsym/build/lib -o reproduce
./reproduce
```

Observed on the 2026-10-05 literal-exponent repair based on `ef981ff`:
both generated outputs are NaN, and the oracle exits nonzero. The next repair
should share native exact-expression normalization below differentiation,
without adding a second algebra evaluator or changing noninteger-power domains.

Chris&AI
