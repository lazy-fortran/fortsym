program test_fortsym_radical_zero
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, rat, func, operator(+), operator(-), &
                            operator(*), operator(/), operator(**)
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_UNKNOWN
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    type(arena_t), target :: a
    type(native_engine_t) :: engine
    type(expr_t) :: x, y, g, h, b
    integer :: failures, checks

    failures = 0
    checks = 0
    call a%init()
    engine = make_native_engine(a)
    x = sym(a, 'x')
    y = sym(a, 'y')
    g = func('sqrt', [x])
    call expect('issue 92', g**3 - x*g, VERDICT_TRUE)
    call expect('rational powers', x**rat(a, 3_int64, 2_int64) - x*g, VERDICT_TRUE)
    g = x**rat(a, 3_int64, 2_int64)
    call expect('powered rational generator', g**3 - x**3*g, VERDICT_TRUE)
    call expect('powered rational product', g*g**2 - x**3*g, VERDICT_TRUE)
    call expect('powered rational nonidentity', g*g**2 - x**6*g, VERDICT_UNKNOWN)
    b = x*y
    g = func('sqrt', [b])
    call expect('multivariate base', g**3 - b*g, VERDICT_TRUE)
    b = 1 - x**2
    g = func('sqrt', [b])
    call expect('polynomial base', g**3 - b*g, VERDICT_TRUE)
    h = func('sqrt', [1 + g])
    call expect('nested base', h**3 - (1 + g)*h, VERDICT_TRUE)
    call expect('nested reduction', h**4 - 2*h**2 + x**2, VERDICT_TRUE)
    g = x**rat(a, 1_int64, 3_int64)
    call expect('cube root', g**4 - x*g, VERDICT_TRUE)
    call expect('negative power', x**rat(a, -1_int64, 3_int64)*x - &
                x**rat(a, 2_int64, 3_int64), VERDICT_TRUE)
    call expect('shared denominators', x**rat(a, 1_int64, 2_int64)*g - &
                x**rat(a, 5_int64, 6_int64), VERDICT_TRUE)
    b = (1 + x)/(1 + y)
    g = func('sqrt', [b])
    call expect('rational base', g**3 - b*g, VERDICT_TRUE)
    g = func('sqrt', [x])
    h = func('sqrt', [y])
    call expect('several roots', (g + h)**2 - x - y - 2*g*h, VERDICT_TRUE)
    call expect('nonidentity', g**3 - (x + 1)*g, VERDICT_UNKNOWN)
    call expect('square root branch', func('sqrt', [x**2]) - x, &
                VERDICT_UNKNOWN)
    call expect('product branch', func('sqrt', [x*y]) - g*h, &
                VERDICT_UNKNOWN)
    call expect('nested branch', (x**2)**rat(a, 1_int64, 3_int64) - &
                x**rat(a, 2_int64, 3_int64), VERDICT_UNKNOWN)
    call expect('degree bound', x**rat(a, 1_int64, 67_int64) - g, VERDICT_UNKNOWN)
    call expect('vanishing denominator', (g**3 - x*g)/(g**4 - x**2), &
                VERDICT_UNKNOWN)
    call expect('common root degree bound', x**rat(a, 1_int64, 8_int64)* &
                x**rat(a, 1_int64, 9_int64) - x**rat(a, 17_int64, 72_int64), &
                VERDICT_UNKNOWN)
    if (failures /= 0) error stop 'radical zero regression failed'
    print *, 'PASS radical zero checks:', checks
contains
    subroutine expect(label, expression, verdict)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        integer, intent(in) :: verdict
        type(engine_result_t) :: result

        checks = checks + 1
        result = engine%zero_test(expression)
        if (result%ok .and. result%verdict == verdict) return
        failures = failures + 1
        print *, 'FAIL ', label, ' expected ', verdict, ' got ', result%verdict
    end subroutine expect
end program test_fortsym_radical_zero
