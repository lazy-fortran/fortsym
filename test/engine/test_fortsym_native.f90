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
    use fortsym_arena, only: arena_t, NK_ADD, NK_FUNC
    use fortsym_expr
    use fortsym_assume, only: assumption_context_t, assume, zero, negative, &
        nonpositive, positive, nonnegative, nonzero, real_valued, &
        record_relation, &
        clone_assumption_context, FACT_ZERO, FACT_NEGATIVE, FACT_NONPOSITIVE, &
        FACT_POSITIVE, FACT_REAL, FACT_NONZERO, FACT_INTEGER
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
    call test_nan_domain_rules()
    call test_directed_domain_rules()
    call test_directed_domain_functions()
    call test_directed_domain_heads()
    call test_directed_atan2_heads()
    call test_directed_bessel_heads()
    call test_directed_legendre_heads()
    call test_directed_inverse_heads()
    call test_reciprocal_hyperbolic_heads()
    call test_error_function_domain_heads()
    call test_gamma_domain_heads()
    call test_noninteger_domain_powers()
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
        type(expr_t) :: sqrt_argument

        r = engine%simplify(x + x + 2*x - 4*x)
        call check("like terms cancel exactly", r%value == num(arena, 0))

        r = engine%simplify(x*x*x**(-1))
        call check("integer powers collect", r%value == x)

        r = engine%simplify((x**2)**3)
        call check("nested integer powers combine", r%value == x**6)

        r = engine%simplify(i_expr(arena)**2)
        call check("integer powers of i are exact", r%value == num(arena, -1))

        sqrt_argument = sym(arena, "sqrt_argument")
        r = engine%simplify(sqrt(sqrt_argument)**2)
        call check("square of principal square root is exact", &
            r%value == sqrt_argument)
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
        type(expr_t) :: loggamma_args(1)
        type(expr_t) :: extremum_args(3)
        type(expr_t) :: legendre_args(3)

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

        loggamma_args(1) = num(arena, 1_int64)
        r = engine%simplify(func("loggamma", loggamma_args))
        call check("loggamma(1) simplifies to zero", &
            r%value == num(arena, 0_int64))
        loggamma_args(1) = num(arena, 5_int64)
        r = engine%simplify(func("loggamma", loggamma_args))
        call check("loggamma(5) simplifies to log(24)", &
            r%value == log(num(arena, 24_int64)))
        loggamma_args(1) = rat(arena, 1_int64, 2_int64)
        r = engine%simplify(func("loggamma", loggamma_args))
        call check("loggamma(1/2) simplifies to log(sqrt(pi))", &
            r%value == log(sqrt(pi_expr(arena))))
        loggamma_args(1) = rat(arena, 3_int64, 2_int64)
        r = engine%simplify(func("loggamma", loggamma_args))
        call check("loggamma(3/2) simplifies by the half recurrence", &
            r%value == log(sqrt(pi_expr(arena))*rat(arena, 1_int64, 2_int64)))
        loggamma_args(1) = rat(arena, 5_int64, 2_int64)
        r = engine%simplify(func("loggamma", loggamma_args))
        call check("loggamma(5/2) simplifies by the half recurrence", &
            r%value == log(sqrt(pi_expr(arena))*rat(arena, 3_int64, 4_int64)))
        loggamma_args(1) = num(arena, 0_int64)
        r = engine%simplify(func("loggamma", loggamma_args))
        call check("loggamma pole remains opaque", r%value%kind() == NK_FUNC)

        loggamma_args(1) = num(arena, 1_int64)
        r = engine%simplify(func("log10", loggamma_args))
        call check("log10(1) simplifies to zero", &
            r%value == num(arena, 0_int64))
        loggamma_args(1) = num(arena, 1000_int64)
        r = engine%simplify(func("log10", loggamma_args))
        call check("log10(1000) simplifies to three", &
            r%value == num(arena, 3_int64))
        loggamma_args(1) = rat(arena, 1_int64, 100_int64)
        r = engine%simplify(func("log10", loggamma_args))
        call check("log10(1/100) simplifies to minus two", &
            r%value == num(arena, -2_int64))
        loggamma_args(1) = num(arena, 12_int64)
        r = engine%simplify(func("log10", loggamma_args))
        call check("non-power-of-ten log10 remains opaque", &
            r%value%kind() == NK_FUNC)

        legendre_args(1) = num(arena, 0_int64)
        legendre_args(2) = num(arena, 0_int64)
        legendre_args(3) = x
        r = engine%simplify(func("legendrep", legendre_args))
        call check("LegendreP(0,0,x) simplifies to one", &
            r%value == num(arena, 1_int64))
        legendre_args(1) = num(arena, 3_int64)
        r = engine%simplify(func("legendrep", legendre_args))
        call check("LegendreP(3,0,x) simplifies successfully", r%ok)
        call check_values("LegendreP(3,0,x) matches its defining polynomial", &
            r%value, (5*x**3 - 3*x)/2, 0.3_dp, 1.0e-13_dp)
        legendre_args(3) = rat(arena, 3_int64, 10_int64)
        r = engine%simplify(func("legendrep", legendre_args))
        call check("LegendreP(3,0,3/10) is exact", &
            r%value == rat(arena, -153_int64, 400_int64))
        legendre_args(2) = num(arena, 1_int64)
        legendre_args(3) = x
        r = engine%simplify(func("legendrep", legendre_args))
        call check("associated Legendre order remains opaque", &
            r%value%kind() == NK_FUNC)
        legendre_args(1) = num(arena, 17_int64)
        legendre_args(2) = num(arena, 0_int64)
        r = engine%simplify(func("legendrep", legendre_args))
        call check("over-cap Legendre degree remains opaque", &
            r%value%kind() == NK_FUNC)

        extremum_args(1) = num(arena, 3_int64)
        extremum_args(2) = num(arena, 1_int64)
        extremum_args(3) = num(arena, 2_int64)
        r = engine%simplify(func("min", extremum_args))
        call check("min over exact integers selects the least", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(func("max", extremum_args))
        call check("max over exact integers selects the greatest", &
            r%value == num(arena, 3_int64))
        extremum_args(1) = rat(arena, -1_int64, 2_int64)
        extremum_args(2) = rat(arena, 1_int64, 3_int64)
        extremum_args(3) = rat(arena, 1_int64, 4_int64)
        r = engine%simplify(func("min", extremum_args))
        call check("min over exact rationals compares safely", &
            r%value == rat(arena, -1_int64, 2_int64))
        r = engine%simplify(func("max", extremum_args))
        call check("max over exact rationals compares safely", &
            r%value == rat(arena, 1_int64, 3_int64))
        extremum_args(1) = x
        r = engine%simplify(func("min", extremum_args))
        call check("symbolic min remains opaque", r%value%kind() == NK_FUNC)

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
        type(expr_t) :: u, z, negative_x, nonpositive_x, zero_x
        type(expr_t) :: log_real, log_nonzero, unknown_log

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

        negative_x = sym(arena, "negative_x")
        call assume(assumptions, negative(negative_x))
        nonpositive_x = sym(arena, "nonpositive_x")
        call assume(assumptions, nonpositive(nonpositive_x))
        zero_x = sym(arena, "zero_x")
        call assume(assumptions, zero(zero_x))
        assumed_engine = make_native_engine(arena, assumptions)
        r = assumed_engine%simplify(sqrt(negative_x**2))
        call check("negative x permits sqrt(x^2)=-x", &
            r%value == -negative_x)
        r = assumed_engine%simplify(abs(negative_x))
        call check("negative x permits abs(x)=-x", &
            r%value == -negative_x)
        r = assumed_engine%simplify(sqrt(nonpositive_x**2))
        call check("nonpositive x permits sqrt(x^2)=-x", &
            r%value == -nonpositive_x)
        r = assumed_engine%simplify(abs(nonpositive_x))
        call check("nonpositive x permits abs(x)=-x", &
            r%value == -nonpositive_x)
        r = assumed_engine%simplify(sqrt(zero_x**2))
        call check("zero x permits sqrt(x^2)=0", &
            r%value == num(arena, 0_int64))
        r = assumed_engine%simplify(abs(zero_x))
        call check("zero x permits abs(x)=0", &
            r%value == num(arena, 0_int64))

        log_real = sym(arena, "log_real")
        call assume(assumptions, real_valued(log_real))
        log_nonzero = sym(arena, "log_nonzero")
        call assume(assumptions, nonzero(log_nonzero))
        assumed_engine = make_native_engine(arena, assumptions)
        r = assumed_engine%simplify(log(exp(log_real)))
        call check("real x permits log(exp(x))=x", r%value == log_real)
        r = assumed_engine%simplify(exp(log(log_nonzero)))
        call check("nonzero x permits exp(log(x))=x", &
            r%value == log_nonzero)
        unknown_log = sym(arena, "unknown_log")
        r = engine%simplify(log(exp(unknown_log)))
        call check("unknown log/exp composition remains guarded", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(exp(log(unknown_log)))
        call check("unknown exp/log composition remains guarded", &
            r%value%kind() == NK_FUNC)

        z = sym(arena, "z")
        call assume(assumptions, nonzero(z))
        call check("nonzero z implies real z", assumptions%has(z, FACT_REAL))

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
        type(assumption_context_t) :: parent, child, negative_context
        type(assumption_context_t) :: compound_context, zero_context
        type(expr_t) :: y, relation, first, second, compound, args(2)
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

        args(1) = x
        args(2) = num(arena, -1)
        relation = func("Greater", args)
        call record_relation(parent, relation, ok, why)
        call check("lower bound without a sign implication is refused", .not. ok)

        call negative_context%init(arena)
        args(1) = x
        args(2) = num(arena, 0)
        relation = func("Less", args)
        call record_relation(negative_context, relation, ok, why)
        call check("x < 0 relation is recorded", ok)
        call check("x < 0 implies negativity", &
            negative_context%has(x, FACT_NEGATIVE))

        call record_relation(parent, relation, ok, why)
        call check("contradictory positive and negative facts are refused", &
            .not. ok)
        call check("contradiction has a diagnostic", &
            index(why, "contradictory assumptions:") == 1)
        call check("refused contradiction does not alter the parent", &
            parent%has(x, FACT_POSITIVE) .and. &
            .not. parent%has(x, FACT_NEGATIVE))

        call compound_context%init(arena)
        args(1) = x
        args(2) = num(arena, 1)
        first = func("Greater", args)
        args(2) = num(arena, -1)
        second = func("Greater", args)
        args(1) = first
        args(2) = second
        compound = func("And", args)
        call record_relation(compound_context, compound, ok, why)
        call check("refused compound relation is transactional", .not. ok)
        call check("compound refusal leaves no partial fact", &
            .not. compound_context%has(x, FACT_POSITIVE))

        args(1) = x
        args(2) = num(arena, 0)
        second = func("Unequal", args)
        args(1) = first
        args(2) = second
        compound = func("And", args)
        call record_relation(compound_context, compound, ok, why)
        call check("supported compound relation is recorded", ok)
        call check("compound relation keeps inferred facts", &
            compound_context%has(x, FACT_POSITIVE) .and. &
            compound_context%has(x, FACT_NONZERO))

        call zero_context%init(arena)
        call assume(zero_context, nonnegative(y))
        call assume(zero_context, nonpositive(y))
        call check("nonnegative and nonpositive infer zero", &
            zero_context%has(y, FACT_ZERO))
        call assume(zero_context, nonzero(y), ok, why)
        call check("zero and nonzero facts conflict", .not. ok)
        call check("zero conflict has a diagnostic", &
            index(why, "zero and nonzero") > 0)

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

    subroutine test_nan_domain_rules()
        type(engine_result_t) :: r
        type(expr_t) :: undefined
        type(expr_t) :: bessel_args(2), legendre_args(3)

        undefined = const(arena, "nan")

        r = engine%simplify(undefined + x)
        call check("nan absorbs addition", r%ok .and. r%value == undefined)

        r = engine%simplify(undefined*num(arena, 0_int64))
        call check("nan absorbs multiplication by zero", &
            r%ok .and. r%value == undefined)

        r = engine%simplify(undefined*x)
        call check("nan absorbs multiplication by a symbol", &
            r%ok .and. r%value == undefined)

        r = engine%simplify(sqrt(undefined))
        call check("sqrt(nan) is nan", r%ok .and. r%value == undefined)

        r = engine%simplify(undefined**num(arena, 0_int64))
        call check("nan to the zeroth power is one", &
            r%ok .and. r%value == num(arena, 1_int64))

        r = engine%simplify(undefined**x)
        call check("nan to an unknown power is nan", &
            r%ok .and. r%value == undefined)

        r = engine%simplify(x**undefined)
        call check("an unknown base to nan is nan", &
            r%ok .and. r%value == undefined)

        bessel_args(1) = undefined
        bessel_args(2) = x
        r = engine%simplify(func("besselj", bessel_args))
        call check("besselj with a nan order remains applied", &
            r%ok .and. r%value%kind() == NK_FUNC)
        bessel_args(1) = x
        bessel_args(2) = undefined
        r = engine%simplify(func("besseli", bessel_args))
        call check("besseli with a nan argument remains applied", &
            r%ok .and. r%value%kind() == NK_FUNC)

        legendre_args(1) = undefined
        legendre_args(2) = num(arena, 0_int64)
        legendre_args(3) = x
        r = engine%simplify(func("legendrep", legendre_args))
        call check("legendre with a nan degree remains applied", &
            r%ok .and. r%value%kind() == NK_FUNC)
    end subroutine test_nan_domain_rules

    subroutine test_directed_domain_rules()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, undefined, negative_infinity

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        undefined = nan_expr(arena)
        negative_infinity = -infinity

        r = engine%simplify(infinity + num(arena, 3_int64))
        call check("oo plus a finite scalar is oo", r%value == infinity)
        r = engine%simplify(infinity + negative_infinity)
        call check("oo plus negative oo is nan", r%value == undefined)
        r = engine%simplify(infinity*num(arena, 0_int64))
        call check("oo times zero is nan", r%value == undefined)
        r = engine%simplify(infinity*num(arena, 2_int64))
        call check("oo times a positive scalar is oo", r%value == infinity)
        r = engine%simplify(infinity*num(arena, -2_int64))
        call check("oo times a negative scalar is negative oo", &
            r%value == negative_infinity)
        r = engine%simplify(infinity**num(arena, 0_int64))
        call check("oo to the zeroth power is one", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(infinity**num(arena, 2_int64))
        call check("oo to a positive power is oo", r%value == infinity)
        r = engine%simplify(infinity**num(arena, -2_int64))
        call check("oo to a negative power is zero", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(negative_infinity**num(arena, 2_int64))
        call check("negative oo to an even power is oo", r%value == infinity)
        r = engine%simplify(negative_infinity**num(arena, 3_int64))
        call check("negative oo to an odd power is negative oo", &
            r%value == negative_infinity)

        r = engine%simplify(complex_infinity + num(arena, 1_int64))
        call check("zoo plus a finite scalar is zoo", &
            r%value == complex_infinity)
        r = engine%simplify(complex_infinity + complex_infinity)
        call check("zoo plus zoo is nan", r%value == undefined)
        r = engine%simplify(complex_infinity + infinity)
        call check("zoo plus oo is nan", r%value == undefined)
        r = engine%simplify(complex_infinity*num(arena, 0_int64))
        call check("zoo times zero is nan", r%value == undefined)
        r = engine%simplify(complex_infinity*num(arena, 2_int64))
        call check("zoo times a nonzero scalar is zoo", &
            r%value == complex_infinity)
        r = engine%simplify(complex_infinity*infinity)
        call check("zoo times oo is zoo", r%value == complex_infinity)
        r = engine%simplify(complex_infinity**num(arena, 0_int64))
        call check("zoo to the zeroth power is one", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(complex_infinity**num(arena, 2_int64))
        call check("zoo to a positive power is zoo", &
            r%value == complex_infinity)
        r = engine%simplify(complex_infinity**num(arena, -2_int64))
        call check("zoo to a negative power is zero", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(infinity*x)
        call check("oo times a symbol remains unevaluated", r%value == infinity*x)
        r = engine%simplify(complex_infinity*x)
        call check("zoo times a symbol remains unevaluated", &
            r%value == complex_infinity*x)
    end subroutine test_directed_domain_rules

    subroutine test_directed_domain_functions()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, undefined, negative_infinity
        type(expr_t) :: imaginary_infinity

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        undefined = nan_expr(arena)
        negative_infinity = -infinity
        imaginary_infinity = i_expr(arena)*infinity

        r = engine%simplify(sqrt(infinity))
        call check("sqrt(oo) is oo", r%value == infinity)
        r = engine%simplify(sqrt(negative_infinity))
        call check("sqrt(-oo) is i*oo", r%value == imaginary_infinity)
        r = engine%simplify(sqrt(complex_infinity))
        call check("sqrt(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(abs(negative_infinity))
        call check("abs(-oo) is oo", r%value == infinity)
        r = engine%simplify(abs(complex_infinity))
        call check("abs(zoo) is oo", r%value == infinity)
        r = engine%simplify(exp(infinity))
        call check("exp(oo) is oo", r%value == infinity)
        r = engine%simplify(exp(negative_infinity))
        call check("exp(-oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(exp(complex_infinity))
        call check("exp(zoo) is nan", r%value == undefined)
        r = engine%simplify(log(infinity))
        call check("log(oo) is oo", r%value == infinity)
        r = engine%simplify(log(negative_infinity))
        call check("log(-oo) is oo", r%value == infinity)
        r = engine%simplify(log(complex_infinity))
        call check("log(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(sin(complex_infinity))
        call check("sin(zoo) is nan", r%value == undefined)
        r = engine%simplify(cos(complex_infinity))
        call check("cos(zoo) is nan", r%value == undefined)
        r = engine%simplify(tan(complex_infinity))
        call check("tan(zoo) is nan", r%value == undefined)
        r = engine%simplify(sin(infinity))
        call check("sin(oo) remains an accumulation-bound refusal", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(cos(infinity))
        call check("cos(oo) remains an accumulation-bound refusal", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(tan(infinity))
        call check("tan(oo) remains an accumulation-bound refusal", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(sqrt(infinity*x))
        call check("sqrt of symbolic infinity remains unevaluated", &
            r%value == sqrt(infinity*x))
        r = engine%simplify(exp(infinity*x))
        call check("exp of symbolic infinity remains unevaluated", &
            r%value == exp(infinity*x))
    end subroutine test_directed_domain_functions

    subroutine test_directed_domain_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, negative_infinity

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        negative_infinity = -infinity

        r = engine%simplify(unary_function("sign", infinity))
        call check("sign(oo) is one", r%value == num(arena, 1_int64))
        r = engine%simplify(unary_function("sign", negative_infinity))
        call check("sign(-oo) is negative one", &
            r%value == num(arena, -1_int64))
        r = engine%simplify(unary_function("sign", complex_infinity))
        call check("sign(zoo) remains unevaluated", r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("floor", infinity))
        call check("floor(oo) is oo", r%value == infinity)
        r = engine%simplify(unary_function("floor", negative_infinity))
        call check("floor(-oo) is negative oo", &
            r%value == negative_infinity)
        r = engine%simplify(unary_function("floor", complex_infinity))
        call check("floor(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(unary_function("ceiling", infinity))
        call check("ceiling(oo) is oo", r%value == infinity)
        r = engine%simplify(unary_function("ceiling", negative_infinity))
        call check("ceiling(-oo) is negative oo", &
            r%value == negative_infinity)
        r = engine%simplify(unary_function("ceiling", complex_infinity))
        call check("ceiling(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(sinh(infinity))
        call check("sinh(oo) is oo", r%value == infinity)
        r = engine%simplify(sinh(negative_infinity))
        call check("sinh(-oo) is negative oo", r%value == negative_infinity)
        r = engine%simplify(sinh(complex_infinity))
        call check("sinh(zoo) is nan", r%value == nan_expr(arena))
        r = engine%simplify(cosh(negative_infinity))
        call check("cosh(-oo) is oo", r%value == infinity)
        r = engine%simplify(cosh(complex_infinity))
        call check("cosh(zoo) is nan", r%value == nan_expr(arena))
        r = engine%simplify(tanh(infinity))
        call check("tanh(oo) is one", r%value == num(arena, 1_int64))
        r = engine%simplify(tanh(negative_infinity))
        call check("tanh(-oo) is negative one", &
            r%value == num(arena, -1_int64))
        r = engine%simplify(tanh(complex_infinity))
        call check("tanh(zoo) is nan", r%value == nan_expr(arena))
    end subroutine test_directed_domain_heads

    subroutine test_directed_atan2_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, negative_infinity, complex_infinity

        infinity = oo_expr(arena)
        negative_infinity = -infinity
        complex_infinity = zoo_expr(arena)

        r = engine%simplify(atan2(infinity, infinity))
        call check("atan2(oo,oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(atan2(negative_infinity, infinity))
        call check("atan2(-oo,oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(atan2(infinity, negative_infinity))
        call check("atan2(oo,-oo) is pi", r%value == pi_expr(arena))
        r = engine%simplify(atan2(negative_infinity, negative_infinity))
        call check("atan2(-oo,-oo) is negative pi", &
            r%value == -pi_expr(arena))
        r = engine%simplify(atan2(complex_infinity, infinity))
        call check("atan2(zoo,oo) remains an applied head", &
            r%value%kind() == NK_FUNC)
    end subroutine test_directed_atan2_heads

    subroutine test_directed_bessel_heads()
        type(engine_result_t) :: r
        type(expr_t) :: order, infinity, negative_infinity, complex_infinity
        type(expr_t) :: undefined
        type(expr_t) :: args(2)

        order = sym(arena, "bessel_order")
        infinity = oo_expr(arena)
        negative_infinity = -infinity
        complex_infinity = zoo_expr(arena)
        undefined = nan_expr(arena)
        args(1) = order
        args(2) = infinity

        r = engine%simplify(func("besselj", args))
        call check("besselj(order,oo) is zero", &
            r%value == num(arena, 0_int64))
        args(2) = negative_infinity
        r = engine%simplify(func("besselj", args))
        call check("besselj(order,-oo) is zero", &
            r%value == num(arena, 0_int64))
        args(2) = infinity
        r = engine%simplify(func("besseli", args))
        call check("besseli(order,oo) is oo", r%value == infinity)
        args(2) = negative_infinity
        r = engine%simplify(func("besseli", args))
        call check("besseli(order,-oo) has the symbolic phase", &
            r%value == (num(arena, -1_int64)**order)*infinity)
        args(1) = num(arena, 1_int64)
        r = engine%simplify(func("besseli", args))
        call check("besseli(1,-oo) is negative oo", &
            r%value == negative_infinity)
        args(1) = undefined
        r = engine%simplify(func("besseli", args))
        call check("besseli(nan,-oo) is nan", r%value == undefined)
        args(2) = complex_infinity
        r = engine%simplify(func("besselj", args))
        call check("besselj(order,zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(func("besseli", args))
        call check("besseli(order,zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
    end subroutine test_directed_bessel_heads

    subroutine test_directed_legendre_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, negative_infinity, complex_infinity
        type(expr_t) :: args(3)

        infinity = oo_expr(arena)
        negative_infinity = -infinity
        complex_infinity = zoo_expr(arena)
        args(1) = num(arena, 2_int64)
        args(2) = num(arena, 0_int64)
        args(3) = infinity

        r = engine%simplify(func("legendrep", args))
        call check("legendre(2,oo) is oo", r%value == infinity)
        args(1) = num(arena, 1_int64)
        args(3) = negative_infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(1,-oo) is negative oo", &
            r%value == negative_infinity)
        args(1) = num(arena, 2_int64)
        r = engine%simplify(func("legendrep", args))
        call check("legendre(2,-oo) is oo", r%value == infinity)
        args(1) = num(arena, 0_int64)
        args(3) = complex_infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(0,zoo) is one", r%value == num(arena, 1_int64))
        args(1) = num(arena, 2_int64)
        r = engine%simplify(func("legendrep", args))
        call check("legendre(2,zoo) is zoo", r%value == complex_infinity)
        args(1) = num(arena, 3_int64)
        args(3) = infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(3,oo) is nan", r%value == nan_expr(arena))
        args(3) = negative_infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(3,-oo) is nan", r%value == nan_expr(arena))
        args(3) = complex_infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(3,zoo) is nan", r%value == nan_expr(arena))
        args(1) = num(arena, -1_int64)
        args(3) = infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(-1,oo) is one", &
            r%value == num(arena, 1_int64))
        args(1) = num(arena, -2_int64)
        args(3) = negative_infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(-2,-oo) is negative oo", &
            r%value == negative_infinity)
        args(1) = num(arena, -3_int64)
        args(3) = infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(-3,oo) is oo", r%value == infinity)
        args(1) = num(arena, -4_int64)
        args(3) = complex_infinity
        r = engine%simplify(func("legendrep", args))
        call check("legendre(-4,zoo) is nan", r%value == nan_expr(arena))
        args(1) = rat(arena, 1_int64, 2_int64)
        r = engine%simplify(func("legendrep", args))
        call check("noninteger legendre degree remains an applied head", &
            r%value%kind() == NK_FUNC)
    end subroutine test_directed_legendre_heads

    subroutine test_directed_inverse_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, negative_infinity
        type(expr_t) :: half_pi, negative_half_pi
        type(expr_t) :: imaginary_half_pi, negative_imaginary_half_pi

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        negative_infinity = -infinity
        half_pi = rat(arena, 1_int64, 2_int64)*pi_expr(arena)
        negative_half_pi = rat(arena, -1_int64, 2_int64)*pi_expr(arena)
        imaginary_half_pi = i_expr(arena)*half_pi
        negative_imaginary_half_pi = rat(arena, -1_int64, 2_int64)* &
            i_expr(arena)*pi_expr(arena)

        r = engine%simplify(asin(infinity))
        call check("asin(oo) is negative i oo", &
            r%value == -i_expr(arena)*infinity)
        r = engine%simplify(asin(negative_infinity))
        call check("asin(-oo) is i oo", r%value == i_expr(arena)*infinity)
        r = engine%simplify(asin(complex_infinity))
        call check("asin(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(acos(infinity))
        call check("acos(oo) is i oo", r%value == i_expr(arena)*infinity)
        r = engine%simplify(acos(negative_infinity))
        call check("acos(-oo) is negative i oo", &
            r%value == -i_expr(arena)*infinity)
        r = engine%simplify(acos(complex_infinity))
        call check("acos(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(atan(infinity))
        call check("atan(oo) is pi over two", r%value == half_pi)
        r = engine%simplify(atan(negative_infinity))
        call check("atan(-oo) is negative pi over two", &
            r%value == negative_half_pi)
        r = engine%simplify(atan(complex_infinity))
        call check("atan(zoo) remains an applied head", r%value%kind() == NK_FUNC)
        r = engine%simplify(asinh(infinity))
        call check("asinh(oo) is oo", r%value == infinity)
        r = engine%simplify(asinh(negative_infinity))
        call check("asinh(-oo) is negative oo", r%value == negative_infinity)
        r = engine%simplify(asinh(complex_infinity))
        call check("asinh(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(acosh(infinity))
        call check("acosh(oo) is oo", r%value == infinity)
        r = engine%simplify(acosh(negative_infinity))
        call check("acosh(-oo) is oo", r%value == infinity)
        r = engine%simplify(acosh(complex_infinity))
        call check("acosh(zoo) is zoo", r%value == complex_infinity)
        r = engine%simplify(atanh(infinity))
        call check("atanh(oo) is negative i pi over two", &
            r%value == negative_imaginary_half_pi)
        r = engine%simplify(atanh(negative_infinity))
        call check("atanh(-oo) is i pi over two", &
            r%value == imaginary_half_pi)
        r = engine%simplify(atanh(complex_infinity))
        call check("atanh(zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
    end subroutine test_directed_inverse_heads

    subroutine test_reciprocal_hyperbolic_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, negative_infinity

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        negative_infinity = -infinity

        r = engine%simplify(unary_function("csch", infinity))
        call check("csch(oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("csch", negative_infinity))
        call check("csch(-oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("csch", complex_infinity))
        call check("csch(zoo) is nan", r%value == nan_expr(arena))
        r = engine%simplify(unary_function("sech", infinity))
        call check("sech(oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("sech", negative_infinity))
        call check("sech(-oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("sech", complex_infinity))
        call check("sech(zoo) is nan", r%value == nan_expr(arena))
        r = engine%simplify(unary_function("coth", infinity))
        call check("coth(oo) is one", r%value == num(arena, 1_int64))
        r = engine%simplify(unary_function("coth", negative_infinity))
        call check("coth(-oo) is negative one", &
            r%value == num(arena, -1_int64))
        r = engine%simplify(unary_function("coth", complex_infinity))
        call check("coth(zoo) is nan", r%value == nan_expr(arena))
    end subroutine test_reciprocal_hyperbolic_heads

    subroutine test_error_function_domain_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, negative_infinity

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        negative_infinity = -infinity

        r = engine%simplify(unary_function("erf", infinity))
        call check("erf(oo) is one", r%value == num(arena, 1_int64))
        r = engine%simplify(unary_function("erf", negative_infinity))
        call check("erf(-oo) is negative one", &
            r%value == num(arena, -1_int64))
        r = engine%simplify(unary_function("erf", complex_infinity))
        call check("erf(zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("erfc", infinity))
        call check("erfc(oo) is zero", r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("erfc", negative_infinity))
        call check("erfc(-oo) is two", r%value == num(arena, 2_int64))
        r = engine%simplify(unary_function("erfc", complex_infinity))
        call check("erfc(zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
    end subroutine test_error_function_domain_heads

    subroutine test_gamma_domain_heads()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, negative_infinity

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        negative_infinity = -infinity

        r = engine%simplify(unary_function("gamma", infinity))
        call check("gamma(oo) is oo", r%value == infinity)
        r = engine%simplify(unary_function("gamma", negative_infinity))
        call check("gamma(-oo) remains an applied head", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("gamma", complex_infinity))
        call check("gamma(zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("factorial", infinity))
        call check("factorial(oo) is oo", r%value == infinity)
        r = engine%simplify(unary_function("factorial", negative_infinity))
        call check("factorial(-oo) remains an applied head", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("factorial", complex_infinity))
        call check("factorial(zoo) remains an applied head", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("loggamma", infinity))
        call check("loggamma(oo) is oo", r%value == infinity)
        r = engine%simplify(unary_function("loggamma", negative_infinity))
        call check("loggamma(-oo) is zoo", r%value == complex_infinity)
        r = engine%simplify(unary_function("loggamma", complex_infinity))
        call check("loggamma(zoo) is zoo", r%value == complex_infinity)
    end subroutine test_gamma_domain_heads

    subroutine test_noninteger_domain_powers()
        type(engine_result_t) :: r
        type(expr_t) :: infinity, complex_infinity, undefined, negative_infinity
        type(expr_t) :: phase

        infinity = oo_expr(arena)
        complex_infinity = zoo_expr(arena)
        undefined = nan_expr(arena)
        negative_infinity = -infinity

        r = engine%simplify(infinity**rat(arena, 1_int64, 2_int64))
        call check("oo to a positive rational power is oo", &
            r%value == infinity)
        r = engine%simplify(infinity**rat(arena, 2_int64, 3_int64))
        call check("oo to a non-half rational power is oo", &
            r%value == infinity)
        r = engine%simplify(infinity**rat(arena, -1_int64, 2_int64))
        call check("oo to a negative rational power is zero", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(complex_infinity**rat(arena, 1_int64, 2_int64))
        call check("zoo to a positive rational power is zoo", &
            r%value == complex_infinity)
        r = engine%simplify(complex_infinity**rat(arena, 4_int64, 3_int64))
        call check("zoo to a non-half rational power is zoo", &
            r%value == complex_infinity)
        r = engine%simplify(complex_infinity**rat(arena, -1_int64, 2_int64))
        call check("zoo to a negative rational power is zero", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(negative_infinity**rat(arena, 1_int64, 2_int64))
        call check("negative oo square root is i*oo", &
            r%value == i_expr(arena)*infinity)
        r = engine%simplify(negative_infinity**rat(arena, 3_int64, 2_int64))
        call check("negative oo three-halves power is negative i*oo", &
            r%value == -(i_expr(arena)*infinity))
        r = engine%simplify(negative_infinity**rat(arena, 5_int64, 2_int64))
        call check("negative oo five-halves power is i*oo", &
            r%value == i_expr(arena)*infinity)
        phase = num(arena, -1_int64)**rat(arena, 1_int64, 3_int64)
        r = engine%simplify(negative_infinity**rat(arena, 1_int64, 3_int64))
        call check("negative oo one-third power keeps the principal phase", &
            r%value == infinity*phase)
        phase = num(arena, -1_int64)**rat(arena, 2_int64, 3_int64)
        r = engine%simplify(negative_infinity**rat(arena, 2_int64, 3_int64))
        call check("negative oo two-thirds power keeps the principal phase", &
            r%value == infinity*phase)
        phase = num(arena, -1_int64)**rat(arena, 1_int64, 3_int64)
        r = engine%simplify(negative_infinity**rat(arena, 4_int64, 3_int64))
        call check("negative oo four-thirds power normalizes its sign", &
            r%value == (-infinity)*phase)
        r = engine%simplify(undefined**rat(arena, 1_int64, 2_int64))
        call check("nan to a noninteger power is nan", r%value == undefined)
    end subroutine test_noninteger_domain_powers

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
        r = engine%zero_test(unary_function("csc", x)*sin(x) - 1)
        call check("csc(x)*sin(x)-1 is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(unary_function("sec", x)*cos(x) - 1)
        call check("sec(x)*cos(x)-1 is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(unary_function("cot", x)*sin(x) - cos(x))
        call check("cot(x)*sin(x)-cos(x) is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(unary_function("csch", x)*sinh(x) - 1)
        call check("csch(x)*sinh(x)-1 is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(unary_function("sech", x)*cosh(x) - 1)
        call check("sech(x)*cosh(x)-1 is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(unary_function("coth", x)*sinh(x) - cosh(x))
        call check("coth(x)*sinh(x)-cosh(x) is decided zero", &
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
        call check("Euler quarter-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(-i_expr(arena)*pi_expr(arena)/2) + i_expr(arena))
        call check("negative Euler quarter-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(3*i_expr(arena)*pi_expr(arena)/2) + i_expr(arena))
        call check("third-quarter Euler constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)/4) - &
            (1 + i_expr(arena))/sqrt(num(arena, 2)))
        call check("Euler eighth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(3*i_expr(arena)*pi_expr(arena)/4) - &
            (-1 + i_expr(arena))/sqrt(num(arena, 2)))
        call check("Euler three-eighth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)/3) - &
            (1 + i_expr(arena)*sqrt(num(arena, 3)))/2)
        call check("Euler sixth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)/6) - &
            (sqrt(num(arena, 3)) + i_expr(arena))/2)
        call check("Euler twelfth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(i_expr(arena)*pi_expr(arena)/4) - i_expr(arena))
        call check("other fractional periodic constants remain unknown", &
            r%verdict == VERDICT_UNKNOWN)
        r = engine%zero_test(sin(pi_expr(arena)/6) - rat(arena, 1_int64, 2_int64))
        call check("sine sixth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(cos(pi_expr(arena)/3) - rat(arena, 1_int64, 2_int64))
        call check("cosine sixth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sin(pi_expr(arena)/4) - &
            1/sqrt(num(arena, 2_int64)))
        call check("sine eighth-turn constant is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%simplify(sin(pi_expr(arena)/6))
        call check("native simplify evaluates sine sixth-turn constant", &
            r%value == rat(arena, 1_int64, 2_int64))
        r = engine%simplify(cos(pi_expr(arena)/3))
        call check("native simplify evaluates cosine sixth-turn constant", &
            r%value == rat(arena, 1_int64, 2_int64))
        r = engine%simplify(tan(pi_expr(arena)/4))
        call check("native simplify evaluates tangent eighth-turn constant", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(sin(pi_expr(arena)))
        call check("native simplify evaluates sine half-turn constant", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(sin(pi_expr(arena)/2))
        call check("native simplify evaluates sine quarter-turn constant", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(tan(pi_expr(arena)/2))
        call check("native simplify preserves tangent pole", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(sqrt(num(arena, 4_int64)))
        call check("native simplify evaluates exact integer square root", &
            r%value == num(arena, 2_int64))
        r = engine%simplify(sqrt(rat(arena, 4_int64, 9_int64)))
        call check("native simplify evaluates exact rational square root", &
            r%value == rat(arena, 2_int64, 3_int64))
        r = engine%simplify(sqrt(num(arena, 2_int64)))
        call check("native simplify preserves irrational square root", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(sqrt(num(arena, -4_int64)))
        call check("native simplify preserves negative square root", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(abs(num(arena, -7_int64)))
        call check("native simplify evaluates exact integer absolute value", &
            r%value == num(arena, 7_int64))
        r = engine%simplify(abs(rat(arena, -2_int64, 3_int64)))
        call check("native simplify evaluates exact rational absolute value", &
            r%value == rat(arena, 2_int64, 3_int64))
        r = engine%simplify(abs(x))
        call check("native simplify preserves symbolic absolute value", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("sign", rat(arena, -2_int64, 3_int64)))
        call check("native simplify evaluates exact rational sign", &
            r%value == num(arena, -1_int64))
        r = engine%simplify(unary_function("floor", rat(arena, -7_int64, 3_int64)))
        call check("native simplify evaluates exact rational floor", &
            r%value == num(arena, -3_int64))
        r = engine%simplify(unary_function("ceiling", rat(arena, -7_int64, 3_int64)))
        call check("native simplify evaluates exact rational ceiling", &
            r%value == num(arena, -2_int64))
        r = engine%simplify(unary_function("floor", x))
        call check("native simplify preserves symbolic floor", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(sin(-x))
        call check("native simplify applies odd sine parity", &
            r%value == -sin(x))
        r = engine%simplify(cos(-x))
        call check("native simplify applies even cosine parity", &
            r%value == cos(x))
        r = engine%simplify(unary_function("csc", -x))
        call check("native simplify applies odd cosecant parity", &
            r%value == -unary_function("csc", x))
        r = engine%simplify(unary_function("sec", -x))
        call check("native simplify applies even secant parity", &
            r%value == unary_function("sec", x))
        r = engine%simplify(abs(-x))
        call check("native simplify applies absolute-value parity", &
            r%value == abs(x))
        r = engine%simplify(abs(abs(x)))
        call check("native simplify collapses nested absolute value", &
            r%value == abs(x))
        r = engine%simplify(unary_function("asin", num(arena, 1_int64)))
        call check("native simplify evaluates asin(1)", &
            r%value == rat(arena, 1_int64, 2_int64)*pi_expr(arena))
        r = engine%simplify(unary_function("acos", num(arena, -1_int64)))
        call check("native simplify evaluates acos(-1)", &
            r%value == pi_expr(arena))
        r = engine%simplify(unary_function("atan", num(arena, -1_int64)))
        call check("native simplify evaluates atan(-1)", &
            r%value == rat(arena, -1_int64, 4_int64)*pi_expr(arena))
        r = engine%simplify(unary_function("acosh", num(arena, 1_int64)))
        call check("native simplify evaluates acosh(1)", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("atanh", num(arena, 0_int64)))
        call check("native simplify evaluates atanh(0)", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(unary_function("csc", pi_expr(arena)/2))
        call check("native simplify evaluates csc(pi/2)", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(unary_function("sec", pi_expr(arena)/3))
        call check("native simplify evaluates sec(pi/3)", &
            r%value == num(arena, 2_int64))
        r = engine%simplify(unary_function("cot", pi_expr(arena)/4))
        call check("native simplify evaluates cot(pi/4)", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(unary_function("csc", num(arena, 0_int64)))
        call check("native simplify preserves csc pole", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(unary_function("sech", num(arena, 0_int64)))
        call check("native simplify evaluates sech(0)", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(exp(i_expr(arena)*pi_expr(arena)))
        call check("native simplify evaluates Euler half-turn", &
            r%value == num(arena, -1_int64))
        r = engine%simplify(exp(i_expr(arena)*pi_expr(arena)/2))
        call check("native simplify evaluates Euler quarter-turn", &
            r%value == i_expr(arena))
        r = engine%simplify(exp(i_expr(arena)*pi_expr(arena)/4))
        call check("native simplify evaluates Euler eighth-turn", &
            r%value == (1 + i_expr(arena))/sqrt(num(arena, 2_int64)))
        r = engine%simplify(exp(i_expr(arena)*pi_expr(arena)/6))
        call check("native simplify evaluates Euler twelfth-turn", &
            r%value == i_expr(arena)*rat(arena, 1_int64, 2_int64) + &
            sqrt(num(arena, 3_int64))*rat(arena, 1_int64, 2_int64))
        r = engine%simplify(exp(i_expr(arena)*pi_expr(arena)/5))
        call check("native simplify preserves unsupported Euler fraction", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(exp(num(arena, 1_int64)))
        call check("native simplify rewrites exp(1) to e", &
            r%value == e_expr(arena))
        r = engine%simplify(exp(rat(arena, 1_int64, 2_int64)))
        call check("native simplify rewrites exact fractional exp", &
            r%value == e_expr(arena)**rat(arena, 1_int64, 2_int64))
        r = engine%simplify(exp(num(arena, -2_int64)))
        call check("native simplify rewrites negative exact exp", &
            r%value == e_expr(arena)**num(arena, -2_int64))
        r = engine%simplify(log(num(arena, -1_int64)))
        call check("native simplify evaluates log(-1)", &
            r%value == i_expr(arena)*pi_expr(arena))
        r = engine%simplify(log(i_expr(arena)))
        call check("native simplify evaluates log(i)", &
            r%value == i_expr(arena)*rat(arena, 1_int64, 2_int64)* &
            pi_expr(arena))
        r = engine%simplify(log(-i_expr(arena)))
        call check("native simplify evaluates log(-i)", &
            r%value == rat(arena, -1_int64, 2_int64)*i_expr(arena)* &
            pi_expr(arena))
        r = engine%simplify(log(e_expr(arena)))
        call check("native simplify evaluates log(e)", &
            r%value == num(arena, 1_int64))
        r = engine%simplify(log(e_expr(arena)**rat(arena, 3_int64, 2_int64)))
        call check("native simplify inverts exact e power", &
            r%value == rat(arena, 3_int64, 2_int64))
        r = engine%simplify(log(exp(rat(arena, 1_int64, 2_int64))))
        call check("native simplify inverts exact exponential", &
            r%value == rat(arena, 1_int64, 2_int64))
        r = engine%simplify(log(e_expr(arena)**i_expr(arena)))
        call check("native simplify preserves complex e power log", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(atan2(num(arena, 0_int64), num(arena, 1_int64)))
        call check("native simplify evaluates atan2(0,1)", &
            r%value == num(arena, 0_int64))
        r = engine%simplify(atan2(num(arena, 0_int64), num(arena, -1_int64)))
        call check("native simplify evaluates atan2(0,-1)", &
            r%value == pi_expr(arena))
        r = engine%simplify(atan2(num(arena, 1_int64), num(arena, 0_int64)))
        call check("native simplify evaluates atan2(1,0)", &
            r%value == rat(arena, 1_int64, 2_int64)*pi_expr(arena))
        r = engine%simplify(atan2(num(arena, -1_int64), num(arena, 0_int64)))
        call check("native simplify evaluates atan2(-1,0)", &
            r%value == rat(arena, -1_int64, 2_int64)*pi_expr(arena))
        r = engine%simplify(atan2(num(arena, 1_int64), num(arena, -1_int64)))
        call check("native simplify evaluates atan2(1,-1)", &
            r%value == rat(arena, 3_int64, 4_int64)*pi_expr(arena))
        r = engine%simplify(atan2(num(arena, -1_int64), num(arena, 1_int64)))
        call check("native simplify evaluates atan2(-1,1)", &
            r%value == rat(arena, -1_int64, 4_int64)*pi_expr(arena))
        r = engine%simplify(atan2(num(arena, 1_int64), num(arena, 1_int64)))
        call check("native simplify evaluates atan2(1,1)", &
            r%value == rat(arena, 1_int64, 4_int64)*pi_expr(arena))
        r = engine%simplify(atan2(num(arena, 0_int64), num(arena, 0_int64)))
        call check("native simplify preserves atan2 origin", &
            r%value%kind() == NK_FUNC)
        r = engine%simplify(atan2(x, sym(arena, "y")))
        call check("native simplify preserves symbolic atan2", &
            r%value%kind() == NK_FUNC)
        r = engine%zero_test(exp(log(x)) - x)
        call check("exponential logarithm identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(2*log(x)) - x**2)
        call check("integer logarithm power identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(log(x) + y) - x*exp(y))
        call check("logarithm factor in an exponential is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(rat(arena, 1_int64, 2_int64)*log(x)) - &
            x**rat(arena, 1_int64, 2_int64))
        call check("rational logarithm power is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(exp(rat(arena, 1_int64, 2_int64)*log(x)) - sqrt(x))
        call check("sqrt spelling matches the rational power fragment", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sqrt(x)**2 - x)
        call check("square of principal square root is zero", &
            r%verdict == VERDICT_TRUE)
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
        r = engine%zero_test(cosh(x)**2 - sinh(x)**2 - 1)
        call check("hyperbolic Pythagorean identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(tanh(x + y) - (tanh(x) + tanh(y))/ &
            (1 + tanh(x)*tanh(y)))
        call check("hyperbolic tangent addition identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(tanh(x + y) - (tanh(x) + tanh(y))/ &
            (1 - tanh(x)*tanh(y)))
        call check("wrong hyperbolic tangent addition sign is rejected", &
            r%verdict == VERDICT_FALSE)
        r = engine%zero_test(tanh(x) - sinh(x)/cosh(x))
        call check("hyperbolic tangent quotient is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(tan(x + y) - (tan(x) + tan(y))/ &
            (1 - tan(x)*tan(y)))
        call check("tangent addition identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(tan(x + y) - (tan(x) + tan(y))/ &
            (1 + tan(x)*tan(y)))
        call check("complex tangent boundary remains unknown", &
            r%verdict == VERDICT_UNKNOWN)
        r = engine%zero_test(sin(x) + cos(x))
        call check("unproved trigonometric nonidentity remains unknown", &
            r%verdict == VERDICT_UNKNOWN)
        r = engine%zero_test(sin(2*x) - 2*sin(x)*cos(x))
        call check("sine double-angle identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(cos(2*x) - 1 + 2*sin(x)**2)
        call check("cosine double-angle identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sin(3*x) - 3*sin(x) + 4*sin(x)**3)
        call check("sine triple-angle identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sin(x)*cos(y) - &
            (sin(x + y) + sin(x - y))/2)
        call check("sine-cosine product-to-sum is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(sin(x) - 2*sin(x/2)*cos(x/2))
        call check("sine half-angle identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test((x**2 - 1)/(x - 1) - (x + 1))
        call check("rational quotient identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(1/(x - 1) + 1/(x + 1) - &
            2*x/(x**2 - 1))
        call check("rational partial-fraction identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test((x**3 - y**3)/(x - y) - &
            (x**2 + x*y + y**2))
        call check("multivariate rational identity is decided zero", &
            r%verdict == VERDICT_TRUE)
        r = engine%zero_test(1/(x - 1) + 1/(x + 1))
        call check("nonzero rational residual is rejected", &
            r%verdict == VERDICT_FALSE)
    end subroutine test_verdicts

    function unary_function(name, argument) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: argument
        type(expr_t), allocatable :: arguments(:)
        type(expr_t) :: e

        allocate (arguments(1))
        arguments(1) = argument
        e = func(name, arguments)
    end function unary_function

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
