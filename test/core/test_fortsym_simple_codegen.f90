program test_fortsym_simple_codegen
    ! The public facade must be sufficient for a consumer-owned generator:
    ! build expressions, differentiate them, and emit one executable kernel.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym
    implicit none

    integer, parameter :: dp = real64
    type(expr_t) :: x, y, expression, roots(2)
    type(kernel_spec_t) :: spec
    type(engine_result_t) :: result
    character(:), allocatable :: source
    logical :: ok
    integer :: failures

    failures = 0
    call reset()
    call symbols("x y", x, y, ok=ok)
    call check("symbols creates both generator inputs", ok, failures)

    expression = exp(x*y) + x**2
    roots(1) = expression
    result = diff(expression, x)
    call check("facade differentiates generator expression", result%ok, failures)
    if (result%ok) then
        roots(2) = result%value
    else
        roots(2) = expression
    end if
    result = simplify(roots(2))
    call check("facade simplifies generated derivative", result%ok, failures)
    if (result%ok) roots(2) = result%value

    spec%name = str("simple_codegen_kernel")
    spec%mode = KERNEL_SUBROUTINE
    spec%generator = str("test_fortsym_simple_codegen")
    allocate (spec%args(2), spec%outputs(2))
    spec%args(1) = str("x")
    spec%args(2) = str("y")
    spec%outputs(1) = str("value")
    spec%outputs(2) = str("dvalue_dx")
    source = chars(emit_kernel(roots, spec, ok))
    call check("facade emits a valid kernel", ok .and. len(source) > 0, failures)
    call check("kernel has the requested public name", &
        index(source, "subroutine simple_codegen_kernel") > 0, failures)
    call check("kernel contains the value expression", &
        index(source, "exp(") > 0 .and. index(source, "x*y") > 0, failures)
    call check("kernel contains the derivative output", &
        index(source, "dvalue_dx") > 0, failures)

    if (failures /= 0) error stop failures
    write (*, '(a)') "PASS fortsym simple code-generation facade"

contains

    subroutine check(label, condition, failures)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        integer, intent(inout) :: failures

        if (condition) then
            write (*, '(a)') "PASS "//label
        else
            write (*, '(a)') "FAIL "//label
            failures = failures + 1
        end if
    end subroutine check

end program test_fortsym_simple_codegen
