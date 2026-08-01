program test_fortsym_poly
    ! Polynomial and rational algebra checked by properties, not by strings.
    !
    ! Nothing here compares against text the implementation produced. Every
    ! check is a statement that would still be true if the module were thrown
    ! away and rewritten:
    !
    !   * Together, Cancel and Apart must not change the *value* of the
    !     expression. Both sides are substituted at several rational points
    !     where no denominator vanishes and evaluated by fortsym's independent
    !     numeric evaluator; agreement at more points than the degree of the
    !     numerator and denominator combined is not something a wrong rewrite
    !     survives.
    !   * a gcd must divide both inputs exactly -- checked by asking for the
    !     polynomial remainder, which is a different code path -- and on
    !     inputs built as g*u, g*v with u, v coprime by construction it must
    !     reach the full degree of g.
    !   * a factorisation must multiply back to the input, checked numerically
    !     at many points.
    !   * coefficients must reconstruct the polynomial: sum c_k x**k has to
    !     agree with the input everywhere.
    !   * refusals are checked too, because the whole point of the module is
    !     that it declines instead of guessing: Factor on a quartic with no
    !     rational root, and a floating-point coefficient anywhere.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, rat, real_expr, sin, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**)
    use fortsym_subs, only: subs_many
    use fortsym_numeric, only: numeric_value
    use fortsym_poly, only: poly_together, poly_cancel, poly_apart, &
                            poly_factor, poly_coefficient, &
                            poly_coefficient_list, poly_collect, &
                            poly_exponent, poly_gcd_expr, poly_divide, &
                            poly_numerator, poly_denominator
    implicit none

    integer, parameter :: dp = real64
    real(dp), parameter :: TOL = 1.0e-9_dp

    type(arena_t), target :: arena
    integer :: nfail = 0

    call arena%init()

    call test_together_value()
    call test_cancel_value()
    call test_apart_value()
    call test_gcd_divides()
    call test_factor_multiplies_back()
    call test_factor_refuses_quartic()
    call test_coefficients_reconstruct()
    call test_collect_and_exponent()
    call test_division()
    call test_numerator_denominator()
    call test_float_refused()

    if (nfail /= 0) then
        print *, "test_fortsym_poly: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_poly: all checks passed"

contains

    ! ------------------------------------------------------------ helpers --

    !> Numeric value of e with x -> vx, y -> vy, or .false. if it cannot be
    !> evaluated there (a pole, say).
    function value_at(e, x, vx, y, vy, v, ok) result(done)
        type(expr_t), intent(in)  :: e, x, y
        real(dp),     intent(in)  :: vx, vy
        real(dp),     intent(out) :: v
        logical,      intent(out) :: ok
        logical :: done
        type(expr_t) :: at
        character(:), allocatable :: why

        at = subs_many(e, [x, y], [real_expr(arena, vx), real_expr(arena, vy)])
        call numeric_value(at, v, ok, why)
        done = ok
    end function value_at

    !> Both expressions agree at a spread of rational points.
    subroutine same_value(label, a1, a2, x, y)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: a1, a2, x, y
        real(dp) :: v1, v2, px, py
        logical  :: ok1, ok2, dummy
        integer  :: k, agreed

        agreed = 0
        do k = 1, 9
            px = 0.25_dp + real(k, dp)*0.7_dp
            py = 1.5_dp + real(k, dp)*0.37_dp
            dummy = value_at(a1, x, px, y, py, v1, ok1)
            dummy = value_at(a2, x, px, y, py, v2, ok2)
            if (.not. ok1) cycle
            if (.not. ok2) cycle
            agreed = agreed + 1
            if (abs(v1 - v2) > TOL*max(1.0_dp, abs(v1))) then
                call fail(label//": values differ at a sample point")
                return
            end if
        end do
        if (agreed < 5) call fail(label//": too few usable sample points")
    end subroutine same_value

    subroutine fail(message)
        character(*), intent(in) :: message
        print *, "FAIL: ", message
        nfail = nfail + 1
    end subroutine fail

    subroutine expect_ok(label, ok, why)
        character(*),              intent(in) :: label
        logical,                   intent(in) :: ok
        character(:), allocatable, intent(in) :: why

        if (.not. ok) call fail(label//" refused: "//why)
    end subroutine expect_ok

    ! -------------------------------------------------------------- tests --

    subroutine test_together_value()
        type(expr_t) :: x, y, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")
        ! 1/(x + 1) + 1/(x - 1) + y/(x*x - 1)
        e = num(arena, 1)/(x + num(arena, 1)) + num(arena, 1)/(x - num(arena, 1)) &
            + y/(x*x - num(arena, 1))
        call poly_together(arena, e, r, ok, why)
        call expect_ok("Together", ok, why)
        if (.not. ok) return
        call same_value("Together", e, r, x, y)
    end subroutine test_together_value

    subroutine test_cancel_value()
        type(expr_t) :: x, y, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")
        ! (x**2 - y**2)/(x - y), which must cancel to x + y
        e = (x**2 - y**2)/(x - y)
        call poly_cancel(arena, e, r, ok, why)
        call expect_ok("Cancel", ok, why)
        if (.not. ok) return
        call same_value("Cancel", e, r, x, y)
        ! Independent structural consequence of a real cancellation: the
        ! result must no longer have a denominator, so it evaluates at x = y.
        block
            real(dp) :: v
            logical  :: okv, done
            done = value_at(r, x, 2.0_dp, y, 2.0_dp, v, okv)
            if (.not. okv) call fail("Cancel: the common factor survived")
            if (okv) then
                if (abs(v - 4.0_dp) > TOL) call fail("Cancel: wrong limit value")
            end if
        end block
    end subroutine test_cancel_value

    subroutine test_apart_value()
        type(expr_t) :: x, y, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")
        ! (3x + 5)/((x - 1)*(x + 2)**2)
        e = (num(arena, 3)*x + num(arena, 5))/ &
            ((x - num(arena, 1))*(x + num(arena, 2))**2)
        call poly_apart(arena, e, x, .true., r, ok, why)
        call expect_ok("Apart", ok, why)
        if (.not. ok) return
        call same_value("Apart", e, r, x, y)
    end subroutine test_apart_value

    subroutine test_gcd_divides()
        type(expr_t) :: x, y, g, u, v, p, q, r, rem
        logical :: ok
        character(:), allocatable :: why
        real(dp) :: val
        logical  :: okv, done

        x = sym(arena, "x")
        y = sym(arena, "y")
        ! g = x**2 - y**2 by construction; u and v share no factor.
        g = x**2 - y**2
        u = x + num(arena, 3)*y + num(arena, 1)
        v = x - num(arena, 2)*y + num(arena, 5)
        p = g*u
        q = g*v
        call poly_gcd_expr(arena, p, q, r, ok, why)
        call expect_ok("PolynomialGCD", ok, why)
        if (.not. ok) return

        ! Divisibility, through the independent division path.
        call poly_divide(arena, p, r, x, .false., rem, ok, why)
        call expect_ok("PolynomialGCD divisibility", ok, why)
        if (ok) then
            done = value_at(rem, x, 1.3_dp, y, 0.7_dp, val, okv)
            if (.not. okv) call fail("gcd: remainder did not evaluate")
            if (okv) then
                if (abs(val) > TOL) call fail("gcd does not divide the first "// &
                                              "input exactly")
            end if
        end if
        call poly_divide(arena, q, r, x, .false., rem, ok, why)
        if (ok) then
            done = value_at(rem, x, 1.3_dp, y, 0.7_dp, val, okv)
            if (okv) then
                if (abs(val) > TOL) call fail("gcd does not divide the second "// &
                                              "input exactly")
            end if
        end if

        ! Degree maximality: g itself is the answer up to a constant, so
        ! p/gcd must agree with u up to that constant. Compare their ratio at
        ! two points; a gcd of lower degree would leave an x-dependent ratio.
        block
            type(expr_t) :: quo
            real(dp) :: r1, r2, qv, uv
            call poly_divide(arena, p, r, x, .true., quo, ok, why)
            call expect_ok("PolynomialQuotient", ok, why)
            if (.not. ok) return
            done = value_at(quo, x, 1.3_dp, y, 0.7_dp, qv, okv)
            if (.not. okv) return
            done = value_at(u, x, 1.3_dp, y, 0.7_dp, uv, okv)
            if (.not. okv) return
            r1 = qv/uv
            done = value_at(quo, x, 2.1_dp, y, 1.9_dp, qv, okv)
            if (.not. okv) return
            done = value_at(u, x, 2.1_dp, y, 1.9_dp, uv, okv)
            if (.not. okv) return
            r2 = qv/uv
            if (abs(r1 - r2) > TOL) call fail("gcd did not reach the full "// &
                                              "common degree")
        end block
    end subroutine test_gcd_divides

    subroutine test_factor_multiplies_back()
        type(expr_t) :: x, y, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")
        ! (x - 1)**2 (x + 3) (2x + 1), expanded by hand into a polynomial the
        ! factoriser has to rediscover.
        e = num(arena, 2)*x**4 + num(arena, 3)*x**3 - num(arena, 11)*x**2 &
            - num(arena, 3)*x + num(arena, 9)
        call poly_factor(arena, e, r, ok, why)
        call expect_ok("Factor", ok, why)
        if (.not. ok) return
        call same_value("Factor", e, r, x, y)
    end subroutine test_factor_multiplies_back

    subroutine test_factor_refuses_quartic()
        type(expr_t) :: x, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        ! x**4 + 4 = (x**2 - 2x + 2)(x**2 + 2x + 2): no rational root, so
        ! anything short of a real factoriser must refuse rather than call it
        ! irreducible.
        e = x**4 + num(arena, 4)
        call poly_factor(arena, e, r, ok, why)
        if (ok) then
            call fail("Factor claimed a factorisation of x**4 + 4 it cannot "// &
                      "have found")
        end if
    end subroutine test_factor_refuses_quartic

    subroutine test_coefficients_reconstruct()
        type(expr_t) :: x, y, e, c, acc
        type(expr_t), allocatable :: list(:)
        logical :: ok
        character(:), allocatable :: why
        integer :: k

        x = sym(arena, "x")
        y = sym(arena, "y")
        e = (y + num(arena, 2))*x**3 - num(arena, 7)*x + y*y

        call poly_coefficient(arena, e, x, 3, c, ok, why)
        call expect_ok("Coefficient", ok, why)
        if (.not. ok) return
        call same_value("Coefficient", c, y + num(arena, 2), x, y)

        call poly_coefficient_list(arena, e, x, list, ok, why)
        call expect_ok("CoefficientList", ok, why)
        if (.not. ok) return
        if (size(list) /= 4) then
            call fail("CoefficientList: wrong length for a cubic")
            return
        end if
        acc = list(1)
        do k = 2, size(list)
            acc = acc + list(k)*(x**(k - 1))
        end do
        call same_value("CoefficientList", e, acc, x, y)
    end subroutine test_coefficients_reconstruct

    subroutine test_collect_and_exponent()
        type(expr_t) :: x, y, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")
        e = (x + y)**3
        call poly_collect(arena, e, x, r, ok, why)
        call expect_ok("Collect", ok, why)
        if (ok) call same_value("Collect", e, r, x, y)

        call poly_exponent(arena, e, x, r, ok, why)
        call expect_ok("Exponent", ok, why)
        if (.not. ok) return
        if (r%int_value() /= 3) call fail("Exponent: wrong degree")
    end subroutine test_collect_and_exponent

    subroutine test_division()
        type(expr_t) :: x, y, p, q, quo, rem, back
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")
        p = x**3 + num(arena, 2)*x + num(arena, 5)
        q = x**2 - num(arena, 1)
        call poly_divide(arena, p, q, x, .true., quo, ok, why)
        call expect_ok("PolynomialQuotient", ok, why)
        if (.not. ok) return
        call poly_divide(arena, p, q, x, .false., rem, ok, why)
        call expect_ok("PolynomialRemainder", ok, why)
        if (.not. ok) return
        ! The division identity is the oracle: p = q*quo + rem everywhere.
        back = q*quo + rem
        call same_value("division identity", p, back, x, y)
    end subroutine test_division

    subroutine test_numerator_denominator()
        type(expr_t) :: x, y, e, n, d
        real(dp) :: v
        logical :: okv, done

        x = sym(arena, "x")
        y = sym(arena, "y")
        e = (x + num(arena, 1))/(y*y)
        n = poly_numerator(arena, e)
        d = poly_denominator(arena, e)
        call same_value("Numerator/Denominator", e, n/d, x, y)
        done = value_at(d, x, 3.0_dp, y, 2.0_dp, v, okv)
        if (.not. okv) then
            call fail("Denominator did not evaluate")
            return
        end if
        if (abs(v - 4.0_dp) > TOL) call fail("Denominator picked the wrong "// &
                                             "factors")
    end subroutine test_numerator_denominator

    subroutine test_float_refused()
        type(expr_t) :: x, e, r
        logical :: ok
        character(:), allocatable :: why

        x = sym(arena, "x")
        e = (real_expr(arena, 0.5_dp)*x + num(arena, 1))/(x + num(arena, 2))
        call poly_together(arena, e, r, ok, why)
        if (ok) call fail("Together accepted a floating-point coefficient as "// &
                          "though it were exact")
    end subroutine test_float_refused

end program test_fortsym_poly
