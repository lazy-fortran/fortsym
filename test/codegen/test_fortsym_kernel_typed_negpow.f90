program test_fortsym_kernel_typed_negpow
    ! Independent oracle: standard Fortran requires a unary-minus exponent
    ! after ** to be parenthesized ("x**(-3)"); "x**-3" is only a GNU
    ! extension and gfortran warns on it. A negative integer power reaches
    ! emit_typed_kernel whenever a division is lowered to a reciprocal
    ! power internally (fortsym_kernel_typed.f90's power_text), so this
    ! checks the emitted TEXT never contains the unparenthesized form --
    ! a property of the Fortran language grammar itself, not of the
    ! emitter's own logic, so it is not circular.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(/)
    use fortsym_kernel_typed, only: typed_kernel_spec_t, emit_typed_kernel
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: x, y, f
    type(typed_kernel_spec_t) :: spec
    type(str_t) :: source
    logical :: ok
    character(:), allocatable :: message, text

    call arena%init()
    x = sym(arena, "x")
    y = sym(arena, "y")
    ! 1/x + 1/(x+y): division lowers to a reciprocal power (x**-1) inside
    ! the kernel IR whenever the denominator is reused, which is exactly
    ! what triggered the unparenthesized emission this guards against.
    f = num(arena, 1)/x + num(arena, 1)/(x + y) + num(arena, 1)/x

    spec%name = str("negpow_f")
    allocate (spec%args(2), spec%outputs(1))
    spec%args(1) = str("x")
    spec%args(2) = str("y")
    spec%outputs(1) = str("f")
    spec%type_name = str("dual_t")
    spec%literal_constructor = str("dual_t")

    source = emit_typed_kernel([f], spec, ok, message)
    if (.not. ok) then
        print *, "test_fortsym_kernel_typed_negpow: emission failed: ", message
        error stop 1
    end if

    text = chars(source)
    if (index(text, "**-") /= 0) then
        print *, "test_fortsym_kernel_typed_negpow: FAILED, found unparenthesized **-"
        print *, text
        error stop 1
    end if
    if (index(text, "**(-") == 0) then
        print *, "test_fortsym_kernel_typed_negpow: FAILED, expected a negative "// &
            "integer power to actually occur (test would not exercise the fix)"
        error stop 1
    end if
    print *, "test_fortsym_kernel_typed_negpow: PASS"

end program test_fortsym_kernel_typed_negpow
