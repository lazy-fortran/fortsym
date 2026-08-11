program test_fortsym_native
    ! Independent oracles:
    !   * exact rational results and polynomial coefficients are written from
    !     elementary algebra, not copied from the implementation;
    !   * expansion is checked by numeric evaluation at several points;
    !   * differentiation is checked by centered finite differences;
    !   * independently derived large exact values exercise overflow promotion;
    !   * the resource-limit case asserts preservation, never a partial result.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_ADD
    use fortsym_expr
    use fortsym_assume, only: assumption_context_t, assume, positive, &
        record_relation, clone_assumption_context, FACT_POSITIVE, FACT_REAL, &
        FACT_INTEGER
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_print, only: print_expr
    use fortsym_engine, only: engine_result_t, resource_limit_t, &
        new_resource_limit, VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, &
        CAP_FACTOR, has_cap
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
    call test_common_rational_factor()
    call test_polynomial_cancellation()
    call test_expansion()
    call test_expansion_limits()
    call test_resource_limits()
    call test_differentiation()
    call test_bessel_recurrence()
    call test_special_function_identities()
    call test_series()
    call test_linear_solve()
    call test_assumptions()
    call test_assumption_relations()
    call test_domain_conditions()
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
        type(expr_t) :: a, b, expected

        r = engine%simplify(rat(arena, 1_int64, 2_int64) + &
            rat(arena, 1_int64, 3_int64))
        call check("rational addition succeeds", r%ok)
        call check("1/2 + 1/3 = 5/6", &
            r%value == rat(arena, 5_int64, 6_int64))

        r = engine%simplify(rat(arena, 2_int64, 3_int64)* &
            rat(arena, 9_int64, 10_int64))
        call check("2/3 * 9/10 = 3/5", &
            r%value == rat(arena, 3_int64, 5_int64))

        ! Let a = 10^20 + 1 and b = 10^20 - 1. These identities are
        ! independently derived from addition and the difference of squares.
        a = exact(arena, "100000000000000000001")
        b = exact(arena, "99999999999999999999")
        r = engine%simplify(a + b)
        call check("large exact addition promotes", r%ok)
        call check("(10^20+1) + (10^20-1) = 2*10^20", &
            r%value == exact(arena, "200000000000000000000"))
        r = engine%simplify(a - b)
        call check("large exact subtraction promotes", r%ok)
        call check("(10^20+1) - (10^20-1) = 2", &
            r%value == num(arena, 2))

        r = engine%simplify(a*b)
        call check("large exact multiplication promotes", r%ok)
        call check("(10^20+1)*(10^20-1) = 10^40-1", &
            r%value == exact(arena, &
            "9999999999999999999999999999999999999999"))

        r = engine%simplify(a**2)
        call check("large exact power promotes", r%ok)
        call check("(10^20+1)^2 is exact", &
            r%value == exact(arena, &
            "10000000000000000000200000000000000000001"))
        r = engine%simplify(a**(-2))
        call check("large negative exact power promotes", r%ok)
        call check("(10^20+1)^(-2) is the reciprocal square", &
            r%value == exact(arena, &
            "1/10000000000000000000200000000000000000001"))
        r = engine%simplify(a/b)
        call check("large exact division promotes", r%ok)
        call check("(10^20+1)/(10^20-1) remains an exact quotient", &
            r%value == exact(arena, &
            "100000000000000000001/99999999999999999999"))

        r = engine%simplify(exact(arena, "1/100000000000000000000") + &
            exact(arena, "3/100000000000000000000"))
        call check("large rational addition promotes", r%ok)
        call check("4/10^20 reduces to 1/(25*10^18)", &
            r%value == exact(arena, "1/25000000000000000000"))
        r = engine%simplify(rat(arena, huge(0_int64), 2_int64) + &
            rat(arena, huge(0_int64), 2_int64))
        call check("overflowing compact rational sum downcasts exactly", &
            r%value == num(arena, huge(0_int64)))
        r = engine%simplify(rat(arena, -2_int64, 3_int64)**(-3))
        call check("negative rational reciprocal power keeps its sign", &
            r%value == rat(arena, -27_int64, 8_int64))

        expected = exact(arena, "200000000000000000000")*x
        r = engine%simplify(a*x + b*x)
        call check("large exact coefficients collect", r%value == expected)
        r = engine%simplify(a*x - a*x)
        call check("large exact coefficients cancel", &
            r%value == num(arena, 0))
    end subroutine test_exact_arithmetic

    subroutine test_like_terms_and_powers()
        type(engine_result_t) :: r

        r = engine%simplify(x + x + 2*x - 4*x)
        call check("like terms cancel exactly", r%value == num(arena, 0))

        r = engine%simplify(x*x*x**(-1))
        call check("integer powers collect", r%value == x)

        r = engine%simplify((x**2)**3)
        call check("nested integer powers combine", r%value == x**6)

        r = engine%simplify(i_expr(arena)**2)
        call check("integer powers of i are exact", r%value == num(arena, -1))
    end subroutine test_like_terms_and_powers

    subroutine test_common_rational_factor()
        type(engine_result_t) :: r
        type(expr_t) :: expected

        ! Independent oracle: factor 1/500 from the denominator, then
        ! multiply its reciprocal by the outer 1/5.  No implementation text
        ! is used to construct the expected expression.
        expected = rat(arena, 100_int64, 1_int64) / (num(arena, 3_int64)*x**2 + &
            num(arena, 25_int64))
        r = engine%simplify((rat(arena, 3_int64, 500_int64)*x**2 + &
            rat(arena, 1_int64, 20_int64))**(-1) / num(arena, 5_int64))
        call check("common rational factor simplification succeeds", r%ok)
        call check("common rational factor has canonical quotient", &
            r%value == expected)
    end subroutine test_common_rational_factor

    subroutine test_polynomial_cancellation()
        type(engine_result_t) :: r
        type(expr_t) :: y

        y = sym(arena, "y")
        r = engine%simplify((x**2 - 1)/(x - 1))
        call check("native polynomial cancellation succeeds", r%ok)
        call check("native polynomial cancellation preserves the quotient", &
            r%value == x + 1)
        call check("native polynomial cancellation reports its domain", &
            r%conditional .and. chars(r%condition) == &
            "cancelled denominator bases must be nonzero")

        r = engine%simplify((x**2 - y**2)/(x - y))
        call check("native multivariate cancellation succeeds", r%ok)
        call check("native multivariate cancellation preserves the quotient", &
            r%value == x + y)

        r = engine%simplify(x**2 + 2*x + 1)
        call check("native polynomial factor candidate succeeds", r%ok)
        call check("native polynomial factor candidate is selected", &
            r%value == (x + 1)**2)

        call check("native engine advertises factor capability", &
            has_cap(engine, CAP_FACTOR))
        r = engine%factor(x**2 + 2*x + 1)
        call check("native factor operation succeeds", r%ok)
        call check("native factor operation returns the exact factorisation", &
            r%value == (x + 1)**2)

        r = engine%factor((x**2 - 1)/(x - 1))
        call check("native factor operation on a quotient succeeds", r%ok)
        if (r%ok) then
            call check("native factor reports cancelled-domain condition", &
                r%conditional)
            if (r%conditional) call check("native factor condition is named", &
                chars(r%condition) == "cancelled denominator bases must be nonzero")
        end if
    end subroutine test_polynomial_cancellation

    subroutine test_expansion()
        type(engine_result_t) :: r
        type(expr_t) :: original, expected, y
        type(binding_t) :: bindings
        real(dp), parameter :: points(*) = [-2.5_dp, -0.25_dp, 1.5_dp, 4.0_dp]
        real(dp), parameter :: xpoints(*) = [-2.0_dp, -0.5_dp, 0.0_dp, 1.5_dp]
        real(dp), parameter :: ypoints(*) = [1.0_dp, 0.25_dp, -3.0_dp, 2.0_dp]
        real(dp) :: expanded_value, expected_value, scale
        logical :: defined
        integer :: k

        original = (x + 2)*(x - 3)
        expected = x**2 - x - 6
        r = engine%expand(original)
        call check("expansion succeeds", r%ok)

        do k = 1, size(points)
            call check_values("expanded polynomial agrees numerically", &
                r%value, expected, points(k), 1.0e-13_dp)
        end do

        ! C(7 + 2, 2) = 36 independently gives the number of monomials of
        ! total degree at most seven in x and y. Direct evaluation of the
        ! binomial power is a separate oracle from the distribution algorithm.
        y = sym(arena, "y")
        r = engine%expand((x + y)**0)
        call check("zero-power sum expands to one", r%value == num(arena, 1))
        r = engine%expand((x + y)**1)
        call check("first-power sum is unchanged", r%value == x + y)

        original = (x + y + 1)**7
        r = engine%expand(original)
        call check("seventh-power multinomial expansion succeeds", r%ok)
        call check("seventh-power expansion is a sum", r%value%kind() == NK_ADD)
        if (r%value%kind() == NK_ADD) then
            call check("seventh-power expansion has 36 monomials", &
                r%value%nargs() == 36)
        end if

        allocate (bindings%names(2))
        bindings%names(1) = str("x")
        bindings%names(2) = str("y")
        allocate (bindings%values(2))
        bindings%n = 2
        do k = 1, size(xpoints)
            bindings%values(1) = xpoints(k)
            bindings%values(2) = ypoints(k)
            expanded_value = eval_expr(r%value, bindings, defined)
            call check("multinomial expansion evaluates", defined)
            expected_value = (xpoints(k) + ypoints(k) + 1.0_dp)**7
            scale = max(1.0_dp, abs(expanded_value), abs(expected_value))
            call check("multinomial expansion agrees with direct power", &
                abs(expanded_value - expected_value) <= 1.0e-13_dp*scale)
        end do

        original = (x*y + sin(x) + 2)**4
        r = engine%expand(original)
        call check("non-atomic multinomial expansion succeeds", r%ok)
        call check("non-atomic expansion is a sum", r%value%kind() == NK_ADD)
        if (r%value%kind() == NK_ADD) then
            call check("non-atomic expansion has 15 monomials", &
                r%value%nargs() == 15)
        end if
        do k = 1, size(xpoints)
            bindings%values(1) = xpoints(k)
            bindings%values(2) = ypoints(k)
            expanded_value = eval_expr(r%value, bindings, defined)
            call check("non-atomic expansion evaluates", defined)
            expected_value = (xpoints(k)*ypoints(k) + sin(xpoints(k)) + &
                2.0_dp)**4
            scale = max(1.0_dp, abs(expanded_value), abs(expected_value))
            call check("non-atomic expansion agrees with direct power", &
                abs(expanded_value - expected_value) <= 1.0e-12_dp*scale)
        end do
    end subroutine test_expansion

    subroutine test_expansion_limits()
        type(engine_result_t) :: r
        type(expr_t) :: wide, original
        type(expr_t) :: five(5)
        type(binding_t) :: wide_bindings, five_bindings
        character(16) :: name
        real(dp) :: got, expected
        logical :: defined
        integer :: before, i

        ! C(34,5) = 278256 exceeds the documented 100000-term fast-path
        ! bound. Expansion must finish without materialising those terms and
        ! retain the exact input power.
        allocate (wide_bindings%names(30), wide_bindings%values(30))
        wide_bindings%n = 30
        do i = 1, 30
            write (name, '("wide_",i0)') i
            wide_bindings%names(i) = str(trim(name))
            wide_bindings%values(i) = real(i, dp)/100.0_dp
            if (i == 1) then
                wide = sym(arena, trim(name))
            else
                wide = wide + sym(arena, trim(name))
            end if
        end do
        original = wide**5
        before = arena%size()
        r = engine%expand(original)
        call check("over-cap expansion succeeds conservatively", r%ok)
        call check("over-cap expansion preserves the power", r%value == original)
        call check("over-cap expansion allocates no expression nodes", &
            arena%size() == before)
        got = eval_expr(r%value, wide_bindings, defined)
        expected = sum(wide_bindings%values)**5
        call check("over-cap preserved power evaluates", defined)
        call check("over-cap preserved power keeps its value", &
            abs(got - expected) <= 1.0e-12_dp*max(1.0_dp, abs(expected)))

        ! Five balanced parts of degree 32 have multinomial coefficient
        ! 27753207637726771200, beyond signed int64, while C(36,4) = 58905
        ! remains under the term cap. The coefficient preflight must detect
        ! this independently and leave the arena untouched.
        allocate (five_bindings%names(5), five_bindings%values(5))
        five_bindings%n = 5
        do i = 1, 5
            write (name, '("coeff_",i0)') i
            five_bindings%names(i) = str(trim(name))
            five_bindings%values(i) = real(i, dp)/20.0_dp
            five(i) = sym(arena, trim(name))
        end do
        wide = five(1) + five(2) + five(3) + five(4) + five(5)
        original = wide**32
        before = arena%size()
        r = engine%expand(original)
        call check("coefficient-overflow expansion succeeds conservatively", r%ok)
        call check("coefficient-overflow preserves the power", &
            r%value == original)
        call check("coefficient-overflow preflight allocates no nodes", &
            arena%size() == before)
        got = eval_expr(r%value, five_bindings, defined)
        expected = sum(five_bindings%values)**32
        call check("coefficient-overflow preserved power evaluates", defined)
        call check("coefficient-overflow preserved power keeps its value", &
            abs(got - expected) <= 1.0e-12_dp*max(1.0_dp, abs(expected)))
    end subroutine test_expansion_limits

    subroutine test_resource_limits()
        type(engine_result_t) :: r
        type(resource_limit_t) :: limit
        type(expr_t) :: original

        original = x + sym(arena, "resource_y")
        limit = new_resource_limit(1_int64)
        r = engine%simplify(original, limit)
        call check("simplify refuses its node budget", .not. r%ok)
        call check("simplify budget names the operation", &
            index(chars(r%message), "simplify") > 0)
        call check("simplify budget names the limit", &
            index(chars(r%message), "node budget") > 0)
        call check("simplify budget preserves the expression", &
            r%value == original)

        original = (x + sym(arena, "resource_y"))**2
        limit = new_resource_limit(4_int64)
        r = engine%simplify(original, limit)
        call check("simplify charges recursive visits", .not. r%ok)
        call check("recursive node refusal preserves the expression", &
            r%value == original)

        r = engine%expand((x + num(arena, 1))**2, limit)
        call check("expand refuses its node budget", .not. r%ok)
        call check("expand budget names the operation", &
            index(chars(r%message), "expand") > 0)

        limit = new_resource_limit(seconds=-1.0_dp)
        r = engine%series(x, x, num(arena, 0), 1, limit)
        call check("series refuses an expired deadline", .not. r%ok)
        call check("series deadline names the operation", &
            index(chars(r%message), "series") > 0 .and. &
            index(chars(r%message), "deadline") > 0)

        limit = new_resource_limit(1_int64)
        r = engine%solve(x + num(arena, 1), x, limit)
        call check("solve refuses its node budget", .not. r%ok)
        call check("solve budget names the operation", &
            index(chars(r%message), "solve") > 0)

        ! Limits belong to a call. A refused operation must not poison the
        ! engine or a later independent session.
        r = engine%simplify(original)
        call check("unbounded simplify after refusal succeeds", r%ok)
        call check("unbounded simplify still returns the right value", &
            r%value == original)
    end subroutine test_resource_limits

    subroutine test_differentiation()
        type(engine_result_t) :: r
        type(expr_t) :: f
        type(binding_t) :: bindings
        real(dp) :: point, step, symbolic, finite_difference, fplus, fminus
        logical :: defined

        f = x**4 - 3*x**2 + 2*x - 7
        r = engine%diff(f, x)
        call check("native differentiation succeeds", r%ok)

        allocate (bindings%names(1))
        bindings%names(1) = str("x")
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

    subroutine test_special_function_identities()
        type(engine_result_t) :: r
        type(expr_t) :: args(2)

        ! Independent exact oracles: erf(0)=0, erfc(0)=1,
        ! Gamma(n)=(n-1)!, Gamma(1/2)=sqrt(pi), and positive integer
        ! Bessel orders vanish at zero while J_0(0)=I_0(0)=1.
        r = engine%simplify(erf(num(arena, 0_int64)))
        call check("erf(0) simplifies to zero", r%value == num(arena, 0))
        r = engine%simplify(erfc(num(arena, 0_int64)))
        call check("erfc(0) simplifies to one", r%value == num(arena, 1))

        r = engine%simplify(gamma(num(arena, 5_int64)))
        call check("Gamma(5) simplifies to 24", r%value == num(arena, 24))
        r = engine%simplify(gamma(rat(arena, 1_int64, 2_int64)))
        call check("Gamma(1/2) simplifies to sqrt(pi)", &
            r%value == sqrt(pi_expr(arena)))

        r = engine%simplify(besselj(0, num(arena, 0_int64)))
        call check("J_0(0) simplifies to one", r%value == num(arena, 1))
        r = engine%simplify(besselj(2, num(arena, 0_int64)))
        call check("J_2(0) simplifies to zero", r%value == num(arena, 0))

        args(1) = num(arena, 3_int64)
        args(2) = num(arena, 0_int64)
        r = engine%simplify(func("besseli", args))
        call check("I_3(0) simplifies to zero", r%value == num(arena, 0))
        args(1) = num(arena, 0_int64)
        r = engine%simplify(func("besseli", args))
        call check("I_0(0) simplifies to one", r%value == num(arena, 1))
    end subroutine test_special_function_identities

    subroutine test_series()
        type(engine_result_t) :: r
        type(expr_t) :: expected, laurent_expected

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

        r = engine%laurent_series(exp(x)/x, x, num(arena, 0), -1, 2)
        laurent_expected = 1/x + 1 + x/2 + x**2/6
        call check("Laurent series with a simple pole succeeds", r%ok)
        if (r%ok) call check_values("Laurent coefficients reconstruct away from pole", &
            r%value, laurent_expected, 0.25_dp, 1.0e-12_dp)

        r = engine%laurent_series(exp(x), x, num(arena, 0), -1, 1)
        call check("unrecognised negative order is refused", .not. r%ok)
        call check("Laurent refusal names the singular order", &
            index(chars(r%message), "integer power") > 0)
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

    subroutine test_assumption_relations()
        type(assumption_context_t) :: parent, child
        type(expr_t) :: y, relation, args(2)
        logical :: ok
        character(:), allocatable :: why

        y = sym(arena, "y")
        args(1) = x
        args(2) = num(arena, 1)
        relation = func("Greater", args)
        call parent%init(arena)
        call record_relation(parent, relation, ok, why)
        call check("x > 1 relation is recorded", ok)
        call check("x > 1 implies positivity", &
            parent%has(x, FACT_POSITIVE))
        call check("x > 1 implies reality", parent%has(x, FACT_REAL))

        call clone_assumption_context(child, parent)
        args(1) = y
        args(2) = num(arena, 0)
        relation = func("Greater", args)
        call record_relation(child, relation, ok, why)
        call check("scoped relation is recorded", ok)
        call check("scoped relation implies y positive", &
            child%has(y, FACT_POSITIVE))
        call check("scoped relation does not leak to parent", &
            .not. parent%has(y, FACT_POSITIVE))

        args(1) = y
        args(2) = sym(arena, "Integers")
        relation = func("Element", args)
        call record_relation(child, relation, ok, why)
        call check("integer domain is recorded", ok .and. &
            child%has(y, FACT_INTEGER) .and. child%has(y, FACT_REAL))
    end subroutine test_assumption_relations

    subroutine test_domain_conditions()
        type(engine_result_t) :: r
        type(binding_t) :: bindings
        type(expr_t) :: original
        real(dp) :: value
        logical :: defined

        allocate (bindings%names(1), bindings%values(1))
        bindings%names(1) = str("x")
        bindings%values(1) = 0.0_dp
        bindings%n = 1

        original = x*x**(-1)
        value = eval_expr(original, bindings, defined)
        call check("reciprocal cancellation oracle is undefined at zero", &
            .not. defined)
        r = engine%simplify(original)
        call check("reciprocal cancellation produces one away from zero", &
            r%value == num(arena, 1))
        call check("reciprocal cancellation reports its nonzero condition", &
            r%conditional .and. chars(r%condition) == &
            "cancelled denominator bases must be nonzero")
        r = engine%simplify(original)
        call check("cached cancellation retains its nonzero condition", &
            r%conditional .and. chars(r%condition) == &
            "cancelled denominator bases must be nonzero")
    end subroutine test_domain_conditions

    subroutine test_verdicts()
        type(engine_result_t) :: r
        type(expr_t) :: y

        y = sym(arena, "y")

        r = engine%zero_test(x - x)
        call check("x-x is decided zero", r%verdict == VERDICT_TRUE)
        r = engine%zero_test(num(arena, 7))
        call check("nonzero exact number is decided", r%verdict == VERDICT_FALSE)
        r = engine%zero_test(exact(arena, "18446744073709551616"))
        call check("nonzero arbitrary exact number is decided", &
            r%verdict == VERDICT_FALSE)
        r = engine%zero_test(sin(x))
        call check("unknown symbolic form stays unknown", &
            r%verdict == VERDICT_UNKNOWN)

        r = engine%zero_test(exp(x + y) - exp(x)*exp(y))
        call check("exponential addition law is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(x) - exp(x/2)**2)
        call check("integer powers of exponentials are normalised", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(x)*exp(-x) - 1)
        call check("opposite exponential factors cancel", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(x) - exp(2*x))
        call check("distinct formal exponentials are nonzero", &
            r%verdict == VERDICT_FALSE)
        r = engine%zero_test(exp(x) + sin(y))
        call check("unsupported heads remain unknown in the fragment", &
            r%verdict == VERDICT_UNKNOWN)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)) + 1)
        call check("Euler periodic constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(2*i_expr(arena)*pi_expr(arena)) - 1)
        call check("even Euler periodic constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)) - 1)
        call check("Euler periodic constant nonidentity is nonzero", &
            r%verdict == VERDICT_FALSE)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)/2) - i_expr(arena))
        call check("fractional periodic constants remain unknown", &
            r%verdict == VERDICT_UNKNOWN)
        r = engine%zero_test(exp(log(x)) - x)
        call check("exponential logarithm identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(2*log(x)) - x**2)
        call check("integer logarithm power identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(log(x) + y) - x*exp(y))
        call check("logarithm factor in an exponential is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(rat(arena, 1_int64, 2_int64)*log(x)) - sqrt(x))
        call check("fractional logarithm powers remain unknown", &
            r%verdict == VERDICT_UNKNOWN)
        r = engine%zero_test(sin(x)**2 + cos(x)**2 - 1)
        call check("trigonometric Pythagorean identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sin(x + y) - sin(x)*cos(y) - cos(x)*sin(y))
        call check("trigonometric sine addition identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(cos(x + y) - cos(x)*cos(y) + sin(x)*sin(y))
        call check("trigonometric cosine addition identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sinh(x + y) - sinh(x)*cosh(y) - cosh(x)*sinh(y))
        call check("hyperbolic sine addition identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(tan(x + y) - (tan(x) + tan(y))/ &
            (1 - tan(x)*tan(y)))
        call check("tangent addition identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sin(x) + cos(x))
        call check("unproved trigonometric nonidentity remains unknown", &
            r%verdict == VERDICT_UNKNOWN)
    end subroutine test_verdicts

    subroutine test_overflow_preservation()
        type(engine_result_t) :: r
        type(expr_t) :: e, left, right, original, addend
        character(:), allocatable :: huge_left, huge_right, huge_addend

        e = num(arena, huge(0_int64)) + 1
        r = engine%simplify(e)
        call check("int64 overflow promotes to an exact integer", &
            r%value == exact(arena, "9223372036854775808"))
        e = exact(arena, "-9223372036854775808") - 1
        r = engine%simplify(e)
        call check("negative int64 overflow promotes exactly", &
            r%value == exact(arena, "-9223372036854775809"))
        r = engine%simplify(-exact(arena, "-9223372036854775808"))
        call check("negating the minimum int64 promotes exactly", &
            r%value == exact(arena, "9223372036854775808"))
        ! (3037*10^6 + 500)^2 = 9223372037000250000.
        r = engine%simplify(num(arena, 3037000500_int64)**2)
        call check("compact power overflow promotes exactly", &
            r%value == exact(arena, "9223372037000250000"))

        ! Each factor fits the 1 MiB scalar input budget, but their product has
        ! 1,050,003 digits. The engine must keep the exact symbolic product when
        ! that result cannot be interned as one arena scalar.
        huge_left = "1"//repeat("0", 525000)//"1"
        huge_right = "1"//repeat("0", 525000)//"2"
        left = exact(arena, huge_left)
        right = exact(arena, huge_right)
        original = left*right
        r = engine%simplify(original)
        call check("oversize exact product simplification succeeds", r%ok)
        call check("oversize exact product is preserved structurally", &
            r%value == original)

        ! Adding two 1,048,576-digit strings of nines has 1,048,577 digits.
        ! Coefficient collection must likewise retain the original sum.
        huge_addend = repeat("9", 1048576)
        addend = exact(arena, huge_addend)
        original = addend*x + addend*x
        r = engine%simplify(original)
        call check("oversize exact coefficient sum succeeds", r%ok)
        call check("oversize exact coefficient sum is preserved structurally", &
            r%value == original)
    end subroutine test_overflow_preservation

    subroutine check_values(label, left, right, point, tolerance)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: left, right
        real(dp),     intent(in) :: point, tolerance
        type(binding_t) :: bindings
        real(dp) :: lv, rv
        logical :: left_ok, right_ok

        allocate (bindings%names(1), bindings%values(1))
        bindings%names(1) = str("x")
        bindings%values(1) = point
        bindings%n = 1
        lv = eval_expr(left, bindings, left_ok)
        rv = eval_expr(right, bindings, right_ok)
        call check(label, left_ok .and. right_ok .and. &
            abs(lv - rv) < tolerance)
    end subroutine check_values

end program test_fortsym_native
