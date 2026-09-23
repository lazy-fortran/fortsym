# Operator-overloaded typed kernel emission

`fortsym_kernel_typed` extends the backend-neutral `kernel_ir_t` lowering
already used by `fortsym_kernel_emit` (see [`architecture.md`](architecture.md))
with a third Fortran rendering: instead of `real(dp)` scalar arithmetic, the
emitted subroutine computes over an arbitrary caller-supplied derived type
through **operator overloading**. This is the spelling automatic
differentiation, dual/interval numbers, and similar operator-overloaded
numeric types need: the same CSE'd operation graph fortsym already produces,
but expressed only through `+`, `-`, `*`, `/`, integer `**`, and named
elementary-function calls, so a caller's own type (with its own operator and
function overloads) computes the answer.

## API

```fortran
use fortsym_kernel_typed, only: typed_kernel_spec_t, emit_typed_kernel

type(typed_kernel_spec_t) :: spec
type(str_t) :: source
logical :: ok
character(:), allocatable :: message

spec%name = str("my_kernel")
spec%args = [str("x"), str("y")]
spec%outputs = [str("f")]
spec%type_name = str("dual_t")            ! caller's numeric type
spec%literal_constructor = str("dual_t")  ! wrap every literal: dual_t(2.0_dp)

source = emit_typed_kernel([f], spec, ok, message)
```

`typed_kernel_spec_t` fields:

- `name`, `args`, `outputs`: subroutine name and dummy argument names, one
  input/output per symbol/root, in order.
- `type_name`: the Fortran derived-type name used for every argument, output,
  and temporary. It must already provide `+`, `-`, `*`, `/`, integer `**`,
  and whichever elementary functions the kernel calls.
- `temp_prefix`: prefix for generated temporaries (default `"t"`).
- `literal_constructor`: empty emits a bare real literal (`2.0_dp`), relying
  on a mixed-type operator overload between `real(dp)` and `type_name`.
  Nonempty wraps every literal in a call to that constructor name instead,
  e.g. `dual_t(2.0_dp)` -- the structure-constructor form works whenever the
  type's other components have default initializers.
- `functions`: a `function_name_map_t` of canonical fortsym function name ->
  caller-chosen procedure name (e.g. `"besselk" -> "my_bessel_k"`). A
  canonical name absent from the map is emitted verbatim, which is correct
  whenever the caller's type overloads the intrinsic name itself through a
  generic interface -- the ordinary convention, and how `sin`, `cos`,
  `sqrt`, and `exp` are expected to resolve by default.

## What is emitted

CSE falls out of the hash-consed arena for free: a shared subexpression is
already one IR node with several parents, so it becomes one temporary
assigned once and referenced by name everywhere it is used, exactly as in
`fortsym_kernel_emit`.

Supported operations: `+`, `-` (as negation inside a sum), `*`, `/` (through
a reciprocal power), an integer exponent (emitted as Fortran `**`), and
`sqrt`/`1/sqrt` for a literal `0.5`/`-0.5` exponent. Any other non-integer
power is refused (`ok = .false.`) rather than silently emitting a `**` most
operator-overloaded types do not implement.

## Example

`app/gen_dual_demo.f90` emits a kernel for
`f(x, y) = sin(x*y)**2 + sqrt(x)/y + 0.5*x` against a `dual_t` forward-mode
dual number and prints its value and both partial derivatives.
`test/codegen/test_fortsym_kernel_typed.f90` compiles the same emitted
subroutine against an independently hand-written `dual_t` (the standard AD
chain rule for `+`, `-`, `*`, `/`, integer `**`, `sin`, `cos`, `sqrt`, `exp`)
and checks the result against a hand-differentiated closed form and a
central finite difference -- two oracles never touched by the emitter.
