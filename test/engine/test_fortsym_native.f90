program test_fortsym_native
    ! Independent oracles:
    !   * exact rational results and polynomial coefficients are written from
    !     elementary algebra, not copied from the implementation;
    !   * expansion is checked by numeric evaluation at several points;
    !   * differentiation is checked by centered finite differences;
    !   * the overflow case asserts preservation, never wrapped arithmetic.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_ADD
    use fortsym_expr
    use fortsym_assume, only: assumption_context_t, assume, positive
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_print, only: print_expr
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none

    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(assumption_context_t), target :: assumptions
    type(expr_t) :: x
    integer :: nfail

    nfail = 0
    call arena%init()
    engine = make_native_engine(arena)
    x = sym(arena, "x")

    call test_exact_arithmetic()
    call test_like_terms_and_powers()
    call test_expansion()
    call test_differentiation()
    call test_bessel_recurrence()
    call test_series()
    call test_linear_solve()
    call test_assumptions()
    call test_verdicts()
    call test_overflow_preservation()

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_native: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical,      intent(in) :: condition
        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check

    subroutine test_exact_arithmetic()
        type(engine_result_t) :: r

        r = engine%simplify(rat(arena, 1_int64, 2_int64) + &
            rat(arena, 1_int64, 3_int64))
        call check("rational addition succeeds", r%ok)
        call check("1/2 + 1/3 = 5/6", &
            r%value == rat(arena, 5_int64, 6_int64))

        r = engine%simplify(rat(arena, 2_int64, 3_int64)* &
            rat(arena, 9_int64, 10_int64))
        call check("2/3 * 9/10 = 3/5", &
            r%value == rat(arena, 3_int64, 5_int64))
    end subroutine test_exact_arithmetic

    subroutine test_like_terms_and_powers()
        type(engine_result_t) :: r

        r = engine%simplify(x + x + 2*x - 4*x)
        call check("like terms cancel exactly", r%value == num(arena, 0))

        r = engine%simplify(x*x*x**(-1))
        call check("integer powers collect", r%value == x)

        r = engine%simplify((x**2)**3)
        call check("nested integer powers combine", r%value == x**6)
    end subroutine test_like_terms_and_powers

    subroutine test_expansion()
        type(engine_result_t) :: r
        type(expr_t) :: original, expected
        real(dp), parameter :: points(*) = [-2.5_dp, -0.25_dp, 1.5_dp, 4.0_dp]
        integer :: k

        original = (x + 2)*(x - 3)
        expected = x**2 - x - 6
        r = engine%expand(original)
        call check("expansion succeeds", r%ok)

        do k = 1, size(points)
            call check_values("expanded polynomial agrees numerically", &
                r%value, expected, points(k), 1.0e-13_dp)
        end do
    end subroutine test_expansion

    subroutine test_differentiation()
        type(engine_result_t) :: r
        type(expr_t) :: f
        type(binding_t) :: bindings
        real(dp) :: point, step, symbolic, finite_difference, fplus, fminus
        logical :: defined

        f = x**4 - 3*x**2 + 2*x - 7
        r = engine%diff(f, x)
        call check("native differentiation succeeds", r%ok)

        bindings%names = [str("x")]
        allocate (bindings%values(1))
        bindings%n = 1
        point = 1.25_dp
        step = 1.0e-5_dp
        bindings%values(1) = point
        symbolic = eval_expr(r%value, bindings, defined)
        call check("native derivative evaluates", defined)
        bindings%values(1) = point + step
        fplus = eval_expr(f, bindings, defined)
        call check("upper finite-difference point evaluates", defined)
        bindings%values(1) = point - step
        fminus = eval_expr(f, bindings, defined)
        call check("lower finite-difference point evaluates", defined)
        finite_difference = (fplus - fminus)/(2*step)
        call check("native derivative agrees with finite difference", &
            abs(symbolic - finite_difference) < 1.0e-8_dp)
    end subroutine test_differentiation

    subroutine test_bessel_recurrence()
        type(engine_result_t) :: r

        r = engine%diff(besselj(0, x), x)
        call check("J0 derivative simplifies to -J1", &
            r%value == -besselj(1, x))
    end subroutine test_bessel_recurrence

    subroutine test_series()
        type(engine_result_t) :: r
        type(expr_t) :: expected

        r = engine%series(exp(x), x, num(arena, 0), 3)
        expected = 1 + x + x**2/2 + x**3/6
        call check("exp Taylor series succeeds", r%ok)
        call check_values("exp Taylor coefficients through order three", &
            r%value, expected, 0.25_dp, 1.0e-13_dp)

        r = engine%series_coeff((x + 2)**4, x, num(arena, 0), 2)
        call check("series coefficient succeeds", r%ok)
        call check("coefficient of x^2 in (x+2)^4 is 24", &
            r%value == num(arena, 24))
        if (r%value /= num(arena, 24)) then
            print *, "  got coefficient: ", chars(print_expr(r%value))
        end if

        call test_axis_series_case()
    end subroutine test_series

    subroutine test_axis_series_case()
        type(engine_result_t) :: derivative, coefficient, solution
        type(expr_t) :: radius, a1, a3, b2, bz0, lambda0, lambda2, c
        type(expr_t) :: bt, bz, lambda, residual, expected

        radius = sym(arena, "radius")
        a1 = sym(arena, "a1")
        a3 = sym(arena, "a3")
        b2 = sym(arena, "b2")
        bz0 = sym(arena, "bz0")
        lambda0 = sym(arena, "lambda0")
        lambda2 = sym(arena, "lambda2")
        c = sym(arena, "c")

        bt = a1*radius + a3*radius**3
        bz = bz0 + b2*radius**2
        lambda = lambda0 + lambda2*radius**2
        derivative = engine%diff(radius*bt, radius)
        call check("axis product derivative succeeds", derivative%ok)
        residual = c*derivative%value/(4*pi_expr(arena)*radius) - lambda*bz

        coefficient = engine%series_coeff(residual, radius, num(arena, 0), 0)
        call check("axis constant coefficient succeeds", coefficient%ok)
        solution = engine%solve(coefficient%value, a1)
        call check("axis coefficient solve succeeds", solution%ok)
        if (.not. solution%ok) then
            print *, "  coefficient: ", chars(print_expr(coefficient%value))
            print *, "  solve message: ", chars(solution%message)
        end if
        expected = 2*pi_expr(arena)*lambda0*bz0/c
        call check("axis regularity coefficient matches the MHD oracle", &
            solution%value == expected)
    end subroutine test_axis_series_case

    subroutine test_linear_solve()
        type(engine_result_t) :: r
        type(expr_t) :: a, b, c

        a = sym(arena, "a")
        b = sym(arena, "b")
        c = sym(arena, "c")

        r = engine%solve(3*a + 2, a)
        call check("numeric linear solve succeeds", r%ok)
        call check("3*a + 2 = 0 gives -2/3", &
            r%value == rat(arena, -2_int64, 3_int64))

        r = engine%solve(c*a - b, a)
        call check("symbolic linear solve succeeds", r%ok)
        call check("c*a-b=0 gives b/c", r%value == b/c)
        call check("symbolic coefficient produces a condition", r%conditional)
        call check("nonzero condition is reported", &
            chars(r%condition) == "linear coefficient must be nonzero")

        r = engine%solve(a**2 - 1, a)
        call check("nonlinear solve is declined", .not. r%ok)
    end subroutine test_linear_solve

    subroutine test_assumptions()
        type(native_engine_t) :: assumed_engine
        type(engine_result_t) :: r
        type(expr_t) :: u

        r = engine%zero_test(sqrt(x**2) - x)
        call check("sqrt(x^2)-x is unknown without a domain", &
            r%verdict == VERDICT_UNKNOWN)

        call assumptions%init(arena)
        call assume(assumptions, positive(x))
        assumed_engine = make_native_engine(arena, assumptions)
        r = assumed_engine%zero_test(sqrt(x**2) - x)
        call check("positive x permits sqrt(x^2)=x", &
            r%verdict == VERDICT_TRUE)
        r = assumed_engine%zero_test(abs(x) - x)
        call check("positive x permits abs(x)=x", r%verdict == VERDICT_TRUE)

        u = x**2 + 1
        call assume(assumptions, positive(u))
        assumed_engine = make_native_engine(arena, assumptions)
        r = assumed_engine%zero_test(sqrt(u**2) - u)
        call check("positive compound expression is honored", &
            r%verdict == VERDICT_TRUE)
        if (r%verdict /= VERDICT_TRUE) then
            print *, "  compound residual: ", chars(print_expr(r%value))
        end if
    end subroutine test_assumptions

    subroutine test_verdicts()
        type(engine_result_t) :: r

        r = engine%zero_test(x - x)
        call check("x-x is decided zero", r%verdict == VERDICT_TRUE)
        r = engine%zero_test(num(arena, 7))
        call check("nonzero exact number is decided", r%verdict == VERDICT_FALSE)
        r = engine%zero_test(sin(x))
        call check("unknown symbolic form stays unknown", &
            r%verdict == VERDICT_UNKNOWN)
    end subroutine test_verdicts

    subroutine test_overflow_preservation()
        type(engine_result_t) :: r
        type(expr_t) :: e

        e = num(arena, huge(0_int64)) + 1
        r = engine%simplify(e)
        call check("overflowing addition is preserved", r%value%kind() == NK_ADD)
        r = engine%zero_test(e)
        call check("overflowing addition is not given a verdict", &
            r%verdict == VERDICT_UNKNOWN)
    end subroutine test_overflow_preservation

    subroutine check_values(label, left, right, point, tolerance)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: left, right
        real(dp),     intent(in) :: point, tolerance
        type(binding_t) :: bindings
        real(dp) :: lv, rv
        logical :: left_ok, right_ok

        bindings%names = [str("x")]
        bindings%values = [point]
        bindings%n = 1
        lv = eval_expr(left, bindings, left_ok)
        rv = eval_expr(right, bindings, right_ok)
        call check(label, left_ok .and. right_ok .and. &
            abs(lv - rv) < tolerance)
    end subroutine check_values

end program test_fortsym_native
