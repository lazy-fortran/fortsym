program test_fortsym_native_zero_limits
    ! Independent oracle: (s-1)*sum(s**j,j=0,n-1)=s**n-1 by
    ! telescoping. The rational identities hold wherever s /= 1.
    ! A bounded normalization refusal must not refute that exact identity.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, &
        VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_poly, only: poly_together
    implicit none

    type(arena_t), target :: arena
    type(native_engine_t) :: native
    type(symengine_engine_t) :: symengine
    type(expr_t) :: x, y, z, u, v, s, identity
    type(engine_result_t) :: result
    integer :: failures

    failures = 0
    call arena%init()
    native = make_native_engine(arena)
    symengine = make_symengine_engine(arena)
    x = sym(arena, 'x'); y = sym(arena, 'y'); z = sym(arena, 'z')
    u = sym(arena, 'u'); v = sym(arena, 'v')

    call expect_native('exact scalar zero', x - x, VERDICT_TRUE)
    call expect_native('exact scalar nonzero', 2 + 0*x, VERDICT_FALSE)
    call expect_native('canonical polynomial nonzero', x**2 + 1, VERDICT_FALSE)
    identity = geometric_residual(x, 8)
    call expect_native('bounded rational zero', identity, VERDICT_TRUE)
    call expect_native('bounded rational nonzero', identity + 1, VERDICT_FALSE)

    identity = geometric_residual(x, 65)
    call expect_refusal('degree-bound identity', identity, 'degree')
    call expect_unknown('degree-bound identity', identity)

    s = x + y + z + u + v
    identity = geometric_residual(s, 9)
    call expect_refusal('term-bound identity', identity, 'terms')
    call expect_unknown('term-bound identity', identity)
    ! Early exact simplification remains authoritative even when the input
    ! includes an expression that separately exceeds a normalization bound.
    call expect_native('exact zero with bounded subexpression', &
        identity - identity, VERDICT_TRUE)
    call expect_native('exact nonzero with bounded subexpression', &
        identity - identity + 1, VERDICT_FALSE)

    if (failures /= 0) error stop 'native zero resource regression failed'
    print *, 'PASS native zero resource bounds'

contains

    function geometric_residual(base, degree) result(residual)
        type(expr_t), intent(in) :: base
        integer, intent(in) :: degree
        type(expr_t) :: residual, series
        integer :: j

        series = 0*base
        do j = 0, degree - 1
            series = series + base**j
        end do
        residual = (base**degree - 1)/(base - 1) - series
    end function geometric_residual

    subroutine expect_native(label, expression, verdict)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        integer, intent(in) :: verdict

        result = native%zero_test(expression)
        call check(label, result%ok .and. result%verdict == verdict)
    end subroutine expect_native

    subroutine expect_refusal(label, expression, bound)
        character(*), intent(in) :: label, bound
        type(expr_t), intent(in) :: expression
        type(expr_t) :: folded
        logical :: normalized
        character(:), allocatable :: why

        result = native%simplify(expression)
        call check(label//' simplifies', result%ok)
        if (.not. result%ok) return
        call poly_together(arena, result%value, folded, normalized, why)
        call check(label//' exercises refusal', .not. normalized)
        call check(label//' names bound', index(why, bound) > 0)
    end subroutine expect_refusal

    subroutine expect_unknown(label, expression)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression

        call expect_native(label//' remains undecided', expression, VERDICT_UNKNOWN)
        result = symengine%zero_test(expression)
        call check(label//' independent exact zero', &
            result%ok .and. result%verdict == VERDICT_TRUE)
        result = symengine%zero_test(expression + 1)
        call check(label//' independent nonzero perturbation', &
            result%ok .and. result%verdict == VERDICT_FALSE)
    end subroutine expect_unknown

    subroutine check(label, passed)
        character(*), intent(in) :: label
        logical, intent(in) :: passed

        if (passed) then
            print *, 'PASS ', label
        else
            print *, 'FAIL ', label
            failures = failures + 1
        end if
    end subroutine check

end program test_fortsym_native_zero_limits
