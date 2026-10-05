# Rigorous kernels and the enclosure runtime interface

`fortsym_rigorous_emit` renders one expression DAG as two Fortran leaves:

- `emit_float_kernel`: real64 (or complex(real64)) arithmetic, for trial
  solvers, preconditioners and anything else whose errors do not matter for
  correctness;
- `emit_rigorous_kernel`: every operation is a call into an enclosure
  runtime, so each output encloses the exact value of the expression for
  every input inside the argument enclosures.

Both leaves come from the same lowered scalar operation list. An optional
paired sine/cosine runtime call shares evaluation work in the rigorous leaf
while preserving these scalar expressions. A consumer that assembles a trial
operator in floating point and evaluates a certified functional with balls
therefore cannot end up with two different formulas: there is one spec.

## Emission API

```fortran
use fortsym_rigorous_emit
type(rigorous_kernel_spec_t) :: spec
type(str_t) :: float_src, ball_src
logical :: ok
character(:), allocatable :: why

spec%name = str("coupling")
spec%args = [str("l"), str("b"), str("db")]
spec%outputs = [str("kup"), str("kdn")]
spec%runtime = ball_runtime("my_ball_module", "ball_t")
ball_src = emit_rigorous_kernel([kup_expr, kdn_expr], spec, ok, why)
spec%name = str("coupling_f")
float_src = emit_float_kernel([kup_expr, kdn_expr], spec, ok, why)
```

`rigorous_kernel_spec_t` fields:

| field | meaning |
|---|---|
| `name`, `args`, `outputs` | subroutine name, input symbols (in order), one output per root |
| `runtime` | the runtime descriptor (below) |
| `complex_args`, `complex_outputs` | float leaf only: complex(real64) dummies |
| `pure_procedure` | default `.true.`; requires pure runtime procedures |
| `elemental_procedure` | emit an elemental leaf that maps over arrays |
| `horner` | default `.true.`: univariate polynomial sums use Horner's rule |
| `temp_prefix`, `generator` | temporary names and a provenance comment |

Every free symbol of a root must be an argument. A refused expression
returns `ok = .false.` and a diagnostic; there is no silent widening.

### Exactness of literals

- An integer up to 2**53 in magnitude is an exact real64 point.
- Larger exact integers, including arbitrary-precision atoms, are constructed
  from exact chunks of at most nine decimal digits by runtime scaling with
  `10**9` and addition. The resulting enclosure contains the integer; it is
  never supplied as a rounded real64 point.
- A rational with a power-of-two denominator is an exact point (`7/2`), and
  as a coefficient it is an exact `scale`.
- Any other rational `p/q` is a runtime division of exact integer points or
  their chunked enclosures, so `1/10` and `1/27!` are enclosed rather than
  rounded once at compile time. Runtime overflow may yield an uninformative
  enclosure and remains the consumer's explicit arithmetic failure.
- `pi` and `e` are `enclose(m, r)` with `r` above the real64 rounding error.
- The imaginary unit is `cpoint(0, 1)` (ball runtimes only).
- Decimal floating literals are refused: the value the author meant is
  unknown. Write exact rationals.

### Supported operations

Sums, products, quotients, integer powers (`powi` for |n| >= 2, `inv` for
negative powers), half-integer powers through `sqrt`, and `sqrt` itself. A
product's factors with negative exponents form a single denominator. A sum
whose terms are `c * b**k` for one base node `b` is evaluated by Horner's
rule, which is tighter than the monomial sum on enclosures.
Exact constant arithmetic in exponents is folded before classification, so
unsimplified derivative products containing `2-1` or `-1-1` require no caller
simplification. Symbolic or decimal exponents and cube roots are refused.
Optional `exp`, `sin`, `cos`
and `log` operations are emitted only when the runtime descriptor names
the corresponding enclosure procedures. Their contract is enclosure over
the entire input interval or ball; the emitter does not assume libm rounding.
Default descriptors leave these names unset and continue to refuse them.

An optional `sincos` descriptor names a subroutine
`sincos(argument, sine, cosine)`, with one input enclosure and two output
enclosures. It must enclose both exact images over the entire input enclosure,
and be pure/elemental whenever the generated leaf requires that property.
When sine and cosine have the identical lowered argument, the rigorous
renderer calls this procedure once at the earlier operation. The symbolic
DAG, derivatives, and floating leaf retain their separate scalar functions.
Unmatched operations use the scalar `sin`/`cos` fields and still require their
names; a descriptor naming only `sincos` suffices for complete pairs.
Default descriptors leave `sincos` empty, preserving existing dispatch.

The optional `abs` descriptor names a unary enclosure function for the entire
absolute-value image of its input, including intervals crossing zero.
Default descriptors leave this field empty and reject rigorous absolute
value; the floating leaf uses the Fortran intrinsic. Complex floating
absolute value has a real result. The real derivative `x/abs(x)` is supported
only for `x /= 0`; an enclosure reaching zero gives no finite derivative
certificate. This operation does not introduce a derivative at the cusp.

## Runtime interface

A runtime is a Fortran module with one enclosure type and the procedures
below. `rigorous_runtime_t` names them, so any module with this contract
plugs in; `ball_runtime()` and `interval_runtime()` return the reference
spellings and accept a module and type name override.

| role | reference ball | reference interval | contract (exact result enclosed for all operand values) |
|---|---|---|---|
| type | `ball_t` (`c`, `r`) | `interval_t` (`lo`, `hi`) | the enclosure |
| `point(x)` | `bpoint` | `ipoint` | the exact real64 `x` |
| `cpoint(x, y)` | `bcpoint` | (none) | the exact complex `x + i y` |
| `enclose(m, r)` | `benclose` | `ienclose` | all values with `abs(v - m) <= r` |
| `add(a, b)`, `sub(a, b)` | `badd`, `bsub` | `iadd`, `isub` | `a + b`, `a - b` |
| `mul(a, b)`, `div(a, b)` | `bmul`, `bdiv` | `imul`, `idiv` | `a * b`, `a / b` |
| `neg(a)`, `inv(a)` | `bneg`, `binv` | `ineg`, `iinv` | `-a`, `1 / a` |
| `sqrt(a)` | `bsqrt` | `isqrt` | principal square root |
| `powi(a, n)` | `bpowi` | `ipowi` | `a**n`, integer `n >= 2` |
| `scale(a, x)` | `bscale` | `iscale` | `a * x`, exact real64 `x` |
| `exp(a)`, `sin(a)`, `cos(a)`, `log(a)` | optional named procedures | optional named procedures | exact transcendental image of every input point |

All procedures take and return values (no allocation), and must be `pure`
when `pure_procedure` is requested and `elemental` when
`elemental_procedure` is requested. An operation outside its domain
(division by an enclosure of zero, a square root the runtime cannot bound)
returns an enclosure that carries no information, such as an infinite
radius or the whole real line; it never returns a finite wrong answer.

`powi` exists so that runtimes can be tighter than repeated products: an
even power of an interval containing zero starts at zero, and a ball power
uses `(|c| + r)**n - |c|**n` for the propagated radius.

## Reference runtimes

`test/codegen/runtime/fortsym_ball_runtime.f90` (complex midpoint-radius
balls, the arithmetic of the kinetic-compression `kc_ball` module) and
`test/codegen/runtime/fortsym_interval_runtime.f90` (real intervals) are
test fixtures, not library modules: fortsym does not own a numerical
runtime. Production runtimes (for example FortNum's verified arithmetic)
implement the same contract. Both fixtures move endpoints outward with the
branch-free successor/predecessor of Rump, Zimmermann, Boldo and Melquiond
(BIT 49, 2009), so they need no rounding-mode switch; compile them with
`-ffp-contract=off`.

## What the tests establish

`test_fortsym_rigorous_emit` compiles the emitted float, ball and interval
leaves with gfortran and evaluates them at 24 random exact dyadic points and
at the corners of small input boxes. The oracle is fortsym's 40-digit
requested-precision evaluation of the same expressions at the same exact
rationals. The test checks that the float leaf agrees to 1e-12, that point
enclosures contain the oracle and are no wider than 1e-12 relative, and that
widened enclosures contain the oracle over the whole input box. It detects
propagation errors (a mutated `bsqrt` radius fails it). It cannot establish
the soundness of a runtime's rounding analysis, which is at the unit
roundoff and needs a proof or an exhaustive check of the runtime itself.

The `exp`, `sin`, `cos`, and `log` descriptor fields are optional. For FortNum's
real interval runtime, set each field to its matching exported generic name.
Leaving a field empty rejects that operation rather than assuming a bound
for an intrinsic transcendental call. Floating leaves use the corresponding
Fortran intrinsic. The emitter's special-value compile/run fixture tests
runtime dispatch independently; numerical enclosure algorithms remain the
runtime owner's responsibility.

`test_fortsym_rigorous_sincos` additionally compiles paired, reversed-order,
unmatched, mixed and nested dispatch, and the derivatives of the shared
expression. Its interval fixture uses exact dyadic Taylor bounds on
`[-1/2,1/2]`; independent binary128 point and box-interior values must lie
inside the emitted value and derivative enclosures. This is an emitter
dispatch check, not evidence for a production runtime's rounding algorithm.

The real `log` hook requires strictly positive operands. Its runtime must
enclose the logarithm over the entire input interval and report a domain
failure for nonpositive input; a floating `log` call with ad hoc padding is
not a rigorous implementation. Absent `log` descriptors refuse emission.
The independent runtime fixture checks `log(1)=0` and binary128 floating
evaluation at a nonspecial value.
