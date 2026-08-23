program test_fortsym_diff
    ! Independent oracles:
    !   * the Bessel recurrence is the published DLMF 10.6.1 identity;
    !   * the numeric derivative uses a centred finite difference of the
    !     compiler's BESSEL_JN intrinsic, not fortsym's symbolic rule.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, exact, func, besselj, besseli, besselk, legendrep, &
        legendreq, log, operator(+), operator(-), operator(*), operator(/), &
        operator(==)
    use fortsym_diff, only: diff, partial_derivative
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_subs, only: subs_many
    implicit none

    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(expr_t) :: x, got
    type(binding_t) :: bindings
    real(dp) :: point, step, symbolic_value, numeric_value
    logical :: defined
    integer :: nfail

    nfail = 0
    call arena%init()
    x = sym(arena, "x")
    allocate (bindings%names(1), bindings%values(1))
    bindings%names(1) = str("x")
    bindings%values(1) = 1.25_dp
    bindings%n = 1

    ! An arbitrary exact scalar is constant by definition; this independently
    ! pins the new node kinds in the mechanical derivative dispatch.
    got = diff(exact(arena, "18446744073709551616"), x)
    call check("arbitrary exact integer derivative is zero", &
        got == num(arena, 0))

    got = diff(besselj(0, x), x)
    point = 1.25_dp
    step = 1.0e-5_dp
    symbolic_value = eval_expr(got, bindings, defined)
    numeric_value = (bessel_jn(0, point + step) - &
        bessel_jn(0, point - step))/(2*step)
    call check("J0 derivative is numerically defined", defined)
    call check("J0 derivative agrees with finite difference", &
        abs(symbolic_value - numeric_value) < 1.0e-9_dp)

    got = diff(besselj(1, 3*x), x)
    symbolic_value = eval_expr(got, bindings, defined)
    numeric_value = (bessel_jn(1, 3*(point + step)) - &
        bessel_jn(1, 3*(point - step)))/(2*step)
    call check("chained Bessel derivative is numerically defined", defined)
    call check("Bessel derivative applies the chain rule", &
        abs(symbolic_value - numeric_value) < 1.0e-8_dp)

    got = diff(besseli(0, x), x)
    symbolic_value = eval_expr(got, bindings, defined)
    numeric_value = (modified_i0_series(point + step) - &
        modified_i0_series(point - step))/(2*step)
    call check("I0 derivative is numerically defined", defined)
    call check("I0 derivative agrees with finite difference", &
        abs(symbolic_value - numeric_value) < 1.0e-8_dp)

    got = diff(besseli(1, 3*x), x)
    symbolic_value = eval_expr(got, bindings, defined)
    numeric_value = (modified_i1_series(3*(point + step)) - &
        modified_i1_series(3*(point - step)))/(2*step)
    call check("chained modified-Bessel derivative is numerically defined", defined)
    call check("modified-Bessel derivative applies the chain rule", &
        abs(symbolic_value - numeric_value) < 1.0e-7_dp)

    got = diff(besselk(0, x), x)
    symbolic_value = eval_expr(got, bindings, defined)
    numeric_value = (numeric_besselk(0, point + step) - &
        numeric_besselk(0, point - step))/(2*step)
    call check("K0 derivative is numerically defined", defined)
    call check("K0 derivative agrees with finite difference", &
        abs(symbolic_value - numeric_value) < 1.0e-8_dp)

    got = diff(besselk(1, 3*x), x)
    symbolic_value = eval_expr(got, bindings, defined)
    numeric_value = (numeric_besselk(1, 3*(point + step)) - &
        numeric_besselk(1, 3*(point - step)))/(2*step)
    call check("chained modified-Bessel-K derivative is numerically defined", defined)
    call check("modified-Bessel-K derivative applies the chain rule", &
        abs(symbolic_value - numeric_value) < 1.0e-7_dp)

    call test_multivariate_partials()
    call test_legendre_derivative()
    call test_reciprocal_derivatives()

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_diff: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical,      intent(in) :: condition
        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check

    subroutine test_multivariate_partials()
        type(expr_t) :: r, u, psi, dpsi_dr, dpsi_du
        type(expr_t) :: mixed_ru, mixed_ur
        type(expr_t) :: psi_args(2)

        r = sym(arena, "r")
        u = sym(arena, "u")
        psi_args(1) = r
        psi_args(2) = u
        psi = func("psi", psi_args)

        dpsi_dr = diff(psi, r)
        dpsi_du = diff(psi, u)

        ! Independent chain-rule oracle: r and u are independent symbols, so
        ! differentiating with respect to either selects the matching partial.
        call check("first argument produces partial 1", &
            dpsi_dr == partial_derivative(psi, 1))
        call check("second argument produces partial 2", &
            dpsi_du == partial_derivative(psi, 2))

        ! Schwarz's theorem is an independent metamorphic oracle. The two
        ! differentiation paths must reach one canonical mixed partial.
        mixed_ru = diff(dpsi_dr, u)
        mixed_ur = diff(dpsi_du, r)
        call check("mixed partials commute", mixed_ru == mixed_ur)
    end subroutine test_multivariate_partials

    subroutine test_legendre_derivative()
        type(expr_t) :: p1, p2, q0, q1, derivative, reduced
        type(expr_t) :: old(2), replacement(2)

        p2 = legendrep(num(arena, 2), num(arena, 0), x)
        derivative = diff(p2, x)

        ! Independent Rodrigues oracle:
        ! P_1(x)=x and P_2(x)=(3x^2-1)/2, hence P_2'(x)=3x.
        p1 = legendrep(num(arena, 2) - 1, num(arena, 0), x)
        old(1) = p1
        old(2) = p2
        replacement(1) = x
        replacement(2) = (3*x*x - 1)/2
        reduced = subs_many(derivative, old, replacement)
        symbolic_value = eval_expr(reduced, bindings, defined)
        call check("Legendre derivative is numerically defined", defined)
        call check("Legendre derivative agrees with Rodrigues formula", &
            abs(symbolic_value - 3.0_dp*point) < 1.0e-13_dp)

        q1 = legendreq(num(arena, 1), num(arena, 0), x)
        q0 = legendreq(num(arena, 1) - 1, num(arena, 0), x)
        derivative = diff(q1, x)
        old(1) = q0
        old(2) = q1
        replacement(1) = log((x + 1)/(x - 1))/2
        replacement(2) = x*replacement(1) - 1
        reduced = subs_many(derivative, old, replacement)
        symbolic_value = eval_expr(reduced, bindings, defined)
        numeric_value = (q1_closed(point + step) - &
            q1_closed(point - step))/(2*step)
        call check("Legendre Q derivative is numerically defined", defined)
        call check("Legendre Q derivative agrees with closed form", &
            abs(symbolic_value - numeric_value) < 1.0e-9_dp)
    end subroutine test_legendre_derivative

    subroutine test_reciprocal_derivatives()
        call check_reciprocal_derivative("csc")
        call check_reciprocal_derivative("sec")
        call check_reciprocal_derivative("cot")
        call check_reciprocal_derivative("csch")
        call check_reciprocal_derivative("sech")
        call check_reciprocal_derivative("coth")
    end subroutine test_reciprocal_derivatives

    subroutine check_reciprocal_derivative(name)
        character(*), intent(in) :: name
        type(expr_t) :: argument, original, derivative
        real(dp) :: left, right, reciprocal_step

        reciprocal_step = 1.0e-6_dp
        argument = 2*x + 0.3_dp
        original = unary_function(name, argument)
        derivative = diff(original, x)
        symbolic_value = eval_expr(derivative, bindings, defined)
        left = reciprocal_value(name, 2*(point - reciprocal_step) + 0.3_dp)
        right = reciprocal_value(name, 2*(point + reciprocal_step) + 0.3_dp)
        numeric_value = (right - left)/(2*reciprocal_step)
        call check(name//" derivative is numerically defined", defined)
        call check(name//" derivative agrees with finite difference", &
            abs(symbolic_value - numeric_value) < 1.0e-8_dp)
    end subroutine check_reciprocal_derivative

    function unary_function(name, argument) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: argument
        type(expr_t) :: e
        type(expr_t) :: arguments(1)

        arguments(1) = argument
        e = func(name, arguments)
    end function unary_function

    pure function reciprocal_value(name, value) result(y)
        character(*), intent(in) :: name
        real(dp), intent(in) :: value
        real(dp) :: y

        select case (name)
        case ("csc");  y = 1.0_dp/sin(value)
        case ("sec");  y = 1.0_dp/cos(value)
        case ("cot");  y = cos(value)/sin(value)
        case ("csch"); y = 1.0_dp/sinh(value)
        case ("sech"); y = 1.0_dp/cosh(value)
        case ("coth"); y = cosh(value)/sinh(value)
        end select
    end function reciprocal_value

    pure function q1_closed(value) result(q1_value)
        real(dp), intent(in) :: value
        real(dp) :: q1_value

        q1_value = 0.5_dp*value*log((value + 1.0_dp)/(value - 1.0_dp)) - 1.0_dp
    end function q1_closed

    ! Independent numerical oracle for the two modified Bessel values used
    ! above. The power series follows directly from the defining series and
    ! does not call the symbolic derivative or the library's function head.
    pure function modified_i0_series(value) result(result_value)
        real(dp), intent(in) :: value
        real(dp) :: result_value, term
        integer :: k

        result_value = 1.0_dp
        term = 1.0_dp
        do k = 0, 200
            term = term*(value*value/4.0_dp)/real((k + 1)*(k + 1), dp)
            result_value = result_value + term
            if (abs(term) < 1.0e-16_dp*max(1.0_dp, abs(result_value))) exit
        end do
    end function modified_i0_series

    pure function modified_i1_series(value) result(result_value)
        real(dp), intent(in) :: value
        real(dp) :: result_value, term
        integer :: k

        term = value/2.0_dp
        result_value = term
        do k = 0, 200
            term = term*(value*value/4.0_dp)/real((k + 1)*(k + 2), dp)
            result_value = result_value + term
            if (abs(term) < 1.0e-16_dp*max(1.0_dp, abs(result_value))) exit
        end do
    end function modified_i1_series

    function numeric_besselk(order, value) result(result_value)
        ! Independent oracle: direct composite Simpson quadrature of the
        ! defining integral K_n(x)=int_0^infinity exp(-x cosh(t)) cosh(n t) dt.
        integer, intent(in) :: order
        real(dp), intent(in) :: value
        real(dp) :: result_value, h, t, term, weight
        integer, parameter :: nsteps = 4096
        integer :: k, n

        result_value = 0.0_dp
        n = abs(order)
        h = 12.0_dp/real(nsteps, dp)
        do k = 0, nsteps
            t = real(k, dp)*h
            term = exp(-value*cosh(t))*cosh(real(n, dp)*t)
            if (k == 0 .or. k == nsteps) then
                weight = 1.0_dp
            else if (mod(k, 2) == 0) then
                weight = 2.0_dp
            else
                weight = 4.0_dp
            end if
            result_value = result_value + weight*term
        end do
        result_value = result_value*h/3.0_dp
    end function numeric_besselk

end program test_fortsym_diff
