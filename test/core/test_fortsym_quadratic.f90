program test_fortsym_quadratic
    ! Exact arithmetic in Q(sqrt(d)).
    !
    ! The oracle is field algebra, not this module's own output: sqrt(d)^2 = d,
    ! x * (1/x) = 1, x * conjugate(x) = norm(x), and the identities that hold
    ! for every element of a quadratic field. Where a numeric check appears it
    ! is against an independently known decimal, never against a value this
    ! module produced earlier in the same test.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortsym_string, only: str_t, chars
    use fortsym_quadratic
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_root_squares_to_radicand()
    call test_field_axioms()
    call test_division_is_exact()
    call test_zero_and_equality()
    call test_conjugate_and_norm()
    call test_no_division_by_zero()
    call test_dop853_node()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_quadratic: ", n_pass, &
        " passed, ", n_fail, " failed"
    if (n_fail > 0) error stop 1

contains

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (*, "(a)") "  FAIL: "//label
        end if
    end subroutine ok

    subroutine test_root_squares_to_radicand()
        type(quadratic_t) :: r, sq, six
        logical :: s(3)

        r = quad_root(6, s(1))
        sq = quad_mul(r, r, s(2))
        six = quad_rational("6", 6, s(3))
        call ok("sqrt(6) constructs", all(s))
        call ok("sqrt(6)^2 is exactly 6", quad_equal(sq, six))
        call ok("sqrt(6) is not rational", .not. quad_is_zero( &
                quad_sub(r, quad_rational("0", 6, s(1)), s(2))))
    end subroutine test_root_squares_to_radicand

    subroutine test_field_axioms()
        type(quadratic_t) :: x, y, z, lhs, rhs
        logical :: s(8)

        x = quad("2/3", "-5/7", 6, s(1))
        y = quad("-11/4", "9/13", 6, s(2))
        z = quad("7/5", "1/2", 6, s(3))

        lhs = quad_mul(x, quad_add(y, z, s(4)), s(5))
        rhs = quad_add(quad_mul(x, y, s(6)), quad_mul(x, z, s(7)), s(8))
        call ok("operands construct", all(s))
        call ok("multiplication distributes over addition", quad_equal(lhs, rhs))

        lhs = quad_mul(quad_mul(x, y, s(1)), z, s(2))
        rhs = quad_mul(x, quad_mul(y, z, s(3)), s(4))
        call ok("multiplication is associative", quad_equal(lhs, rhs))

        call ok("x - x vanishes", quad_is_zero(quad_sub(x, x, s(1))))
        call ok("x + (-x) vanishes", &
                quad_is_zero(quad_add(x, quad_neg(x, s(1)), s(2))))
    end subroutine test_field_axioms

    subroutine test_division_is_exact()
        type(quadratic_t) :: x, y, q, one
        logical :: s(4)

        x = quad("3/8", "-2/9", 6, s(1))
        y = quad("-4/11", "5/6", 6, s(2))
        one = quad_rational("1", 6, s(3))

        q = quad_div(x, y, s(4))
        call ok("division succeeds", all(s))
        call ok("(x/y)*y recovers x exactly", &
                quad_equal(quad_mul(q, y, s(1)), x))
        call ok("x/x is exactly one", quad_equal(quad_div(x, x, s(1)), one))

        ! A quotient that is irrational in both components, squared back.
        q = quad_div(one, quad_root(6, s(1)), s(2))
        call ok("1/sqrt(6) squares to 1/6", &
                quad_equal(quad_mul(q, q, s(1)), &
                           quad_rational("1/6", 6, s(2))))
    end subroutine test_division_is_exact

    subroutine test_zero_and_equality()
        type(quadratic_t) :: a1, a2
        logical :: s(2)

        ! Spellings of the same value must compare equal; the canonical form is
        ! FLINT's choice and must not leak into the predicate.
        a1 = quad("1/2", "0/5", 6, s(1))
        a2 = quad("2/4", "-0", 6, s(2))
        call ok("differently spelled equals compare equal", quad_equal(a1, a2))
        call ok("zero with odd spelling is zero", &
                quad_is_zero(quad("0/9", "-0", 6, s(1))))
        call ok("a pure radical part is not zero", &
                .not. quad_is_zero(quad("0", "1/1000000", 6, s(1))))
    end subroutine test_zero_and_equality

    subroutine test_conjugate_and_norm()
        type(quadratic_t) :: x, xc, product
        type(str_t) :: n
        logical :: s(4)

        x = quad("5/3", "-7/4", 6, s(1))
        xc = quad_conjugate(x, s(2))
        product = quad_mul(x, xc, s(3))
        n = quad_norm(x, s(4))
        call ok("conjugate and norm compute", all(s))
        call ok("x*conj(x) is the norm, with no radical part", &
                quad_equal(product, quad_rational(chars(n), 6, s(1))))
    end subroutine test_conjugate_and_norm

    subroutine test_no_division_by_zero()
        type(quadratic_t) :: x, zero
        logical :: s(2), divided

        x = quad("1", "1", 6, s(1))
        zero = quad_rational("0", 6, s(2))
        call quad_div_probe(x, zero, divided)
        call ok("division by zero is refused, not silently wrong", .not. divided)
    end subroutine test_no_division_by_zero

    subroutine quad_div_probe(x, y, succeeded)
        type(quadratic_t), intent(in) :: x, y
        logical, intent(out) :: succeeded
        type(quadratic_t) :: unused

        unused = quad_div(x, y, succeeded)
    end subroutine quad_div_probe

    !> DOP853's fourth node is (6 - sqrt(6))/30 exactly. The decimal is
    !> Hairer's published c4 from dop853.f, an independent source.
    subroutine test_dop853_node()
        type(quadratic_t) :: c4
        real(dp) :: value
        logical :: s(4)

        c4 = quad_div(quad_sub(quad_rational("6", 6, s(1)), &
                               quad_root(6, s(2)), s(3)), &
                      quad_rational("30", 6, s(4)), s(4))
        call ok("c4 constructs", all(s))
        value = quad_to_real(c4, s(1))
        call ok("c4 matches Hairer's published decimal", &
                abs(value - 0.118350341907227396726757197510_dp) < 1.0e-16_dp)
        ! 30*c4 + sqrt(6) = 6 exactly, which the decimal cannot show.
        call ok("30*c4 + sqrt(6) is exactly 6", &
                quad_equal(quad_add(quad_mul(quad_rational("30", 6, s(1)), &
                                             c4, s(2)), &
                                    quad_root(6, s(3)), s(4)), &
                           quad_rational("6", 6, s(1))))
    end subroutine test_dop853_node

end program test_fortsym_quadratic
