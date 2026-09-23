# Order-by-order Taylor-series (SSA) emission

`fortsym_kernel_taylor` emits the elementary-operation building blocks a
Taylor-coefficient (jet transport / power-series) generator sequences
together, using the same backend-neutral `kernel_ir_t` lowering as
`fortsym_kernel_emit` and `fortsym_kernel_typed`. It does not own an ODE
driver or series storage: it emits one Fortran subroutine that computes
coefficient `k` of an expression DAG, given coefficient `k` (and, through
caller-owned series arrays, coefficients `0..k-1`) of every input. A
downstream generator or hand-written driver calls this subroutine for
`k = 0, 1, 2, ...`, owning the series arrays -- including this kernel's own
temporaries, threaded through as arguments so their history survives between
calls -- and, for an ODE right-hand side, applies `y_{k+1} = f_k/(k+1)` to
advance the solution's own series one more order.

## Recurrences

Standard truncated-power-series/automatic-differentiation recurrences
(Moore 1966):

```text
c = a*b:            c_k = sum_{j=0}^{k} a_j b_{k-j}
c = a/b:            c_k = (a_k - sum_{j=1}^{k} b_j c_{k-j}) / b_0
s = sin(a), c = cos(a):
    s_k = (1/k) sum_{j=1}^{k} j a_j c_{k-j}
    c_k = -(1/k) sum_{j=1}^{k} j a_j s_{k-j}     (k >= 1; s_0, c_0 direct)
e = exp(a):
    e_k = (1/k) sum_{j=1}^{k} j a_j e_{k-j}       (k >= 1; e_0 direct)
```

Every other elementary operation -- `+`, `-`, and scaling by a true constant
-- is linear: `(a+b)_k = a_k + b_k`, `(c*a)_k = c*a_k`. The emitter inlines
these directly; it only calls out to `mul_name`/`div_name`/`sincos_name`/
`exp_name` for the four recurrences above, since those are the only ones
that convolve orders together.

## API

```fortran
use fortsym_kernel_taylor, only: taylor_emit_spec_t, emit_taylor_step

type(taylor_emit_spec_t) :: spec
spec%name = str("taylor_step")
spec%args = [str("x")]
spec%outputs = [str("f1"), str("f2"), str("f3")]
spec%mul_name = str("ts_mul")        ! caller-named recurrences; no default
spec%div_name = str("ts_div")
spec%sincos_name = str("ts_sincos")
spec%exp_name = str("ts_exp")

source = emit_taylor_step([f1, f2, f3], spec, ok, message)
```

The emitted subroutine's signature is
`subroutine <name>(k, args..., outputs..., temps...)`, where every array
argument (including the generated temporaries) is, by default,
`real(dp), intent(inout) :: name(0:*)`: the caller allocates them once,
seeds the known input coefficients, and calls the subroutine once per order.
Every input series (`spec%args`) is opaque to the emitter -- it is read only
through `mul_name`/`div_name`/`sincos_name`/`exp_name` calls and coefficient
indexing, never constructed or inspected -- so a caller-supplied history
(e.g. from an upstream ODE step) is a valid argument as-is.

## Generic element type

`spec%type_name` and `spec%literal_constructor` generalise every series
array's element type beyond `real(dp)`, the same knob
`fortsym_kernel_typed`'s `typed_kernel_spec_t` offers for scalar kernels:

```fortran
spec%type_name = str("dual_t")           ! default "": real(dp), unchanged
spec%literal_constructor = str("dual_t") ! default "": bare "2.0_dp" literals
```

With `type_name` set, every array argument, output, temporary, and the
internal constant-one series (needed for a negative integer power) is
declared `type(<type_name>), intent(inout) :: name(0:*)` instead of
`real(dp)`. `type_name` must already provide `+`, `-`, `*`, `/`, integer
`**`, and whatever `mul_name`/`div_name`/`sincos_name`/`exp_name` compute
over it -- a complex-dual number, an interval type, or any other
operator-overloaded numeric type works, exactly as for the typed scalar
kernel. With `literal_constructor` set, every literal the emitter writes
(a true constant such as the `2` in `2+x`, and the `0`/`1` identities the
constant-one series and additive constant-series contributions need) is
wrapped in a call to it, e.g. `dual_t(2.0_dp)`, instead of emitted bare.
Leaving both fields empty keeps the original `real(dp)`-only emission
byte-for-byte identical to before this knob existed.

Supported operations: `+`/`-` (linear, inlined), `*` (a pure-constant factor
is inlined as a scale; two series call `mul_name`), `/` and a negative
integer power (through a reciprocal against a constant-one series and
`div_name`), a positive integer power (chained multiplication), `sin`/`cos`
(jointly, one `sincos_name` call per occurrence), and `exp`. A non-integer
power or any other function is refused rather than silently approximated --
this module supplies exactly the minimum building-block set (mul, div,
sincos, exp, linear combination); a downstream generator needing `sqrt` or
another elementary function supplies its own recurrence and extends the
function dispatch.

## Example

`app/gen_taylor_demo.f90` emits the order-`k` step for
`sin(x)`, `x/(1-x)`, and `exp(x)` and prints the emitted source.
`test/codegen/test_fortsym_kernel_taylor.f90` compiles it against an
independently hand-written truncated-power-series harness (the same four
recurrences, written once, never touched by the emitter) and drives it for
`k = 0..8` at `x(t) = t`, checking the resulting coefficients against two
closed forms neither the emitter nor the harness computes: the `n!`-based
Maclaurin coefficients of `sin` and `exp`, and the geometric series
`t/(1-t) = sum_n t^n`.

`test/codegen/test_fortsym_kernel_taylor_typed.f90` exercises the generic
element-type knob: it emits the step for `f1(x) = (2+x)**(-2)` (a negative
integer power) and `f2(x) = (3+x)/(1+x)` (a division) with `type_name =
"dual_t"` and `literal_constructor = "dual_t"`, compiles it against an
independent hand-written forward-mode-AD `dual_t (v, d)` number type -- the
same `ts_mul`/`ts_div` recurrences, written once, layered on `dual_t`
arithmetic instead of bare `real(dp)` -- and checks both the value and the
`d/da` component of every coefficient at `x(t) = a*t`, `a = 2`, against
closed forms hand-derived from the binomial and geometric series of
`(2+a t)**(-2)` and `(3+a t)/(1+a t)`.
