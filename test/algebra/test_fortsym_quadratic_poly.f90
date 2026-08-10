program test_fortsym_quadratic_poly
    ! Univariate polynomials over Q(sqrt(d)).
    !
    ! The oracle is the defining property in each case: a remainder must
    ! satisfy p = q*quotient + r with deg r < deg q, a gcd must divide both
    ! inputs exactly and vanish at their shared root, and a recovered root must
    ! evaluate both original polynomials to exactly zero. None of the checks
    ! compares against a value this module produced earlier.
    use fortsym_quadratic, only: quadratic_t, quad, quad_rational, quad_root, &
        quad_is_zero, quad_equal, quad_mul, quad_sub
    use fortsym_quadratic_poly
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_degree_ignores_trailing_zeros()
    call test_arithmetic()
    call test_remainder_is_exact()
    call test_gcd_finds_the_shared_root()
    call test_irrational_shared_root()
    call test_ambiguous_gcd_is_refused()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_quadratic_poly: ", n_pass, &
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

    function rat(text) result(value)
        character(*), intent(in) :: text
        type(quadratic_t) :: value
        logical :: s

        value = quad_rational(text, 6, s)
    end function rat

    !> (x - r1)(x - r2) expanded, so the roots are known independently.
    function from_roots(r1, r2) result(p)
        type(quadratic_t), intent(in) :: r1, r2
        type(qpoly_t) :: p
        type(quadratic_t) :: c(3)
        logical :: s

        c(3) = rat("1")
        c(2) = quad_sub(rat("0"), quad_sub(rat("0"), &
                                           quad_sub(rat("0"), &
                                                    quad_sub(r1, &
                                                       quad_sub(rat("0"), r2, s), s), s), s), s)
        c(1) = quad_mul(r1, r2, s)
        p = qpoly_from(c)
    end function from_roots

    subroutine test_degree_ignores_trailing_zeros()
        type(qpoly_t) :: p
        type(quadratic_t) :: c(4)

        c(1) = rat("1"); c(2) = rat("2"); c(3) = rat("0"); c(4) = rat("0")
        p = qpoly_from(c)
        call ok("degree ignores trailing zeros", qpoly_degree(p) == 1)
        c = rat("0")
        p = qpoly_from(c)
        call ok("zero polynomial has degree -1", qpoly_degree(p) == -1)
        call ok("zero polynomial reports zero", qpoly_is_zero(p))
    end subroutine test_degree_ignores_trailing_zeros

    subroutine test_arithmetic()
        type(qpoly_t) :: p, q, r
        type(quadratic_t) :: cp(2), cq(2), x, expect, got
        logical :: s

        cp(1) = rat("1"); cp(2) = rat("1")          ! 1 + x
        cq(1) = rat("-1"); cq(2) = rat("1")         ! -1 + x
        p = qpoly_from(cp); q = qpoly_from(cq)
        r = qpoly_mul(p, q, s)                       ! x^2 - 1
        call ok("product has the right degree", qpoly_degree(r) == 2)
        x = quad_root(6, s)
        got = qpoly_eval(r, x, s)
        expect = rat("5")                            ! 6 - 1
        call ok("(x^2-1) at sqrt(6) is exactly 5", quad_equal(got, expect))
        call ok("p - p is the zero polynomial", qpoly_is_zero(qpoly_sub(p, p, s)))
    end subroutine test_arithmetic

    subroutine test_remainder_is_exact()
        type(qpoly_t) :: p, q, r
        type(quadratic_t) :: cp(3), cq(2)
        logical :: s

        cp(1) = rat("-6"); cp(2) = rat("1"); cp(3) = rat("1")   ! x^2 + x - 6
        cq(1) = rat("-2"); cq(2) = rat("1")                      ! x - 2
        p = qpoly_from(cp); q = qpoly_from(cq)
        r = qpoly_remainder(p, q, s)
        call ok("exact division leaves no remainder", qpoly_is_zero(r))

        cp(1) = rat("-5")                                        ! x^2 + x - 5
        p = qpoly_from(cp)
        r = qpoly_remainder(p, q, s)
        call ok("inexact division leaves a lower-degree remainder", &
                qpoly_degree(r) == 0)
        call ok("the remainder is p(2) = 1", quad_equal(r%coefficient(1), rat("1")))
    end subroutine test_remainder_is_exact

    !> Two quadratics sharing exactly one root: the shape the DOP853 closure
    !> produces. The gcd must be linear and its root must annihilate both.
    subroutine test_gcd_finds_the_shared_root()
        type(qpoly_t) :: p, q, g
        type(quadratic_t) :: shared, other1, other2, root, v1, v2
        logical :: s

        shared = rat("3/7"); other1 = rat("-2"); other2 = rat("5")
        p = from_roots(shared, other1)
        q = from_roots(shared, other2)
        g = qpoly_gcd(p, q, s)
        call ok("gcd computes", s)
        call ok("gcd of two quadratics with one shared root is linear", &
                qpoly_degree(g) == 1)
        root = qpoly_linear_root(g, s)
        call ok("linear root extracts", s)
        call ok("recovered root is the shared one", quad_equal(root, shared))
        v1 = qpoly_eval(p, root, s); v2 = qpoly_eval(q, root, s)
        call ok("root annihilates both polynomials exactly", &
                quad_is_zero(v1) .and. quad_is_zero(v2))
    end subroutine test_gcd_finds_the_shared_root

    !> The same, with a shared root that is irrational. A rational-only gcd
    !> could not represent this and would report no common factor.
    subroutine test_irrational_shared_root()
        type(qpoly_t) :: p, q, g
        type(quadratic_t) :: shared, root
        logical :: s

        shared = quad("1/2", "-1/3", 6, s)
        p = from_roots(shared, rat("7"))
        q = from_roots(shared, rat("-11/5"))
        g = qpoly_gcd(p, q, s)
        call ok("irrational shared root gives a linear gcd", &
                qpoly_degree(g) == 1)
        root = qpoly_linear_root(g, s)
        call ok("irrational root is recovered exactly", quad_equal(root, shared))
        call ok("it annihilates both", &
                quad_is_zero(qpoly_eval(p, root, s)) .and. &
                quad_is_zero(qpoly_eval(q, root, s)))
    end subroutine test_irrational_shared_root

    !> When two conditions share both roots the parameter is not determined.
    !> Returning either would be a guess, so the linear-root call must refuse.
    subroutine test_ambiguous_gcd_is_refused()
        type(qpoly_t) :: p, g
        type(quadratic_t) :: root
        logical :: s

        p = from_roots(rat("2"), rat("-3"))
        g = qpoly_gcd(p, p, s)
        call ok("gcd of a polynomial with itself keeps both roots", &
                qpoly_degree(g) == 2)
        root = qpoly_linear_root(g, s)
        call ok("an ambiguous gcd is refused, not guessed", .not. s)
    end subroutine test_ambiguous_gcd_is_refused

end program test_fortsym_quadratic_poly
