program test_fortsym_quadratic_multipoly
    ! Sparse multivariate polynomials over Q(sqrt(d)) and Sylvester elimination.
    !
    ! The oracles are algebraic identities and independently known factorisations,
    ! never this module's own earlier output. Canonical form is checked by
    ! building the same polynomial two different ways and requiring structural
    ! equality; elimination is checked against resultants whose value is known
    ! in closed form.
    use fortsym_quadratic, only: quadratic_t, quad, quad_rational, quad_root, &
        quad_is_zero, quad_equal, quad_mul, quad_sub, quad_add
    use fortsym_quadratic_poly, only: qpoly_t, qpoly_degree, qpoly_eval
    use fortsym_quadratic_multipoly
    implicit none

    integer :: n_pass, n_fail
    integer, parameter :: D = 6

    n_pass = 0
    n_fail = 0

    call test_canonical_form()
    call test_ring_axioms()
    call test_degrees_and_coefficients()
    call test_substitution()
    call test_resultant_of_shared_root()
    call test_resultant_detects_no_shared_root()
    call test_resultant_eliminates_a_variable()
    call test_to_univariate()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_quadratic_multipoly: ", n_pass, &
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

    function k(text) result(p)
        character(*), intent(in) :: text
        type(qmpoly_t) :: p
        logical :: s

        p = qmpoly_const(quad_rational(text, D, s), 2)
    end function k

    function x() result(p)
        type(qmpoly_t) :: p
        logical :: s
        p = qmpoly_var(1, 2, D, s)
    end function x

    function y() result(p)
        type(qmpoly_t) :: p
        logical :: s
        p = qmpoly_var(2, 2, D, s)
    end function y

    !> Terms that cancel must disappear, and the same polynomial written two
    !> ways must be structurally identical, or equality is not exact.
    subroutine test_canonical_form()
        type(qmpoly_t) :: p, q
        logical :: s

        p = qmpoly_sub(qmpoly_add(x(), y(), s), y(), s)
        call ok("x + y - y is exactly x", qmpoly_equal(p, x()))
        p = qmpoly_sub(x(), x(), s)
        call ok("x - x is the zero polynomial", qmpoly_is_zero(p))
        ! (x+y)^2 built by squaring versus by expansion.
        p = qmpoly_pow(qmpoly_add(x(), y(), s), 2, s)
        q = qmpoly_add(qmpoly_add(qmpoly_pow(x(), 2, s), &
                                  qmpoly_mul(k("2"), qmpoly_mul(x(), y(), s), s), s), &
                       qmpoly_pow(y(), 2, s), s)
        call ok("(x+y)^2 matches its expansion structurally", qmpoly_equal(p, q))
    end subroutine test_canonical_form

    subroutine test_ring_axioms()
        type(qmpoly_t) :: a, b, c, lhs, rhs
        logical :: s

        a = qmpoly_add(x(), k("3"), s)
        b = qmpoly_sub(y(), k("2/5"), s)
        c = qmpoly_add(qmpoly_mul(x(), y(), s), k("-7"), s)

        lhs = qmpoly_mul(a, qmpoly_add(b, c, s), s)
        rhs = qmpoly_add(qmpoly_mul(a, b, s), qmpoly_mul(a, c, s), s)
        call ok("multiplication distributes", qmpoly_equal(lhs, rhs))

        lhs = qmpoly_mul(qmpoly_mul(a, b, s), c, s)
        rhs = qmpoly_mul(a, qmpoly_mul(b, c, s), s)
        call ok("multiplication is associative", qmpoly_equal(lhs, rhs))

        lhs = qmpoly_mul(a, b, s)
        rhs = qmpoly_mul(b, a, s)
        call ok("multiplication commutes", qmpoly_equal(lhs, rhs))
    end subroutine test_ring_axioms

    subroutine test_degrees_and_coefficients()
        type(qmpoly_t) :: p, c1
        logical :: s

        ! p = 3 x^2 y + x^2 - y
        p = qmpoly_add(qmpoly_sub(qmpoly_mul(k("3"), &
                                             qmpoly_mul(qmpoly_pow(x(), 2, s), y(), s), s), &
                                  y(), s), qmpoly_pow(x(), 2, s), s)
        call ok("degree in x is 2", qmpoly_degree(p, 1) == 2)
        call ok("degree in y is 1", qmpoly_degree(p, 2) == 1)
        call ok("total degree is 3", qmpoly_total_degree(p) == 3)
        ! coefficient of y^1 is 3x^2 - 1
        c1 = qmpoly_coefficient_of(p, 2, 1, s)
        call ok("coefficient of y is 3x^2 - 1", &
                qmpoly_equal(c1, qmpoly_sub(qmpoly_mul(k("3"), &
                                                       qmpoly_pow(x(), 2, s), s), &
                                            k("1"), s)))
    end subroutine test_degrees_and_coefficients

    subroutine test_substitution()
        type(qmpoly_t) :: p, replaced
        type(quadratic_t) :: value
        logical :: s

        p = qmpoly_add(qmpoly_pow(x(), 2, s), y(), s)
        ! Substituting y := -x^2 must annihilate the polynomial identically.
        replaced = qmpoly_substitute(p, 2, qmpoly_neg(qmpoly_pow(x(), 2, s), s), s)
        call ok("substitution can make a polynomial vanish identically", &
                qmpoly_is_zero(replaced))
        ! Evaluating at an irrational point.
        value = qmpoly_eval_all(p, [quad_root(D, s), quad_rational("1", D, s)], s)
        call ok("x^2 + y at (sqrt 6, 1) is exactly 7", &
                quad_equal(value, quad_rational("7", D, s)))
    end subroutine test_substitution

    !> Two polynomials in x sharing the root x = y must have resultant zero
    !> identically, because the shared root exists for every y.
    subroutine test_resultant_of_shared_root()
        type(qmpoly_t) :: p, q, r
        logical :: s

        p = qmpoly_sub(x(), y(), s)                       ! x - y
        q = qmpoly_sub(qmpoly_pow(x(), 2, s), &
                       qmpoly_pow(y(), 2, s), s)          ! x^2 - y^2
        r = qmpoly_resultant(p, q, 1, s)
        call ok("resultant computes", s)
        call ok("a shared root for every y gives an identically zero resultant", &
                qmpoly_is_zero(r))
    end subroutine test_resultant_of_shared_root

    !> res_x(x - 1, x - 2) = -1, a classic closed form: no shared root, and the
    !> value does not depend on the other variable.
    subroutine test_resultant_detects_no_shared_root()
        type(qmpoly_t) :: p, q, r
        logical :: s

        p = qmpoly_sub(x(), k("1"), s)
        q = qmpoly_sub(x(), k("2"), s)
        r = qmpoly_resultant(p, q, 1, s)
        call ok("disjoint roots give a non-zero resultant", .not. qmpoly_is_zero(r))
        call ok("res(x-1, x-2) is exactly -1", qmpoly_equal(r, k("-1")))
    end subroutine test_resultant_detects_no_shared_root

    !> Eliminating x from a circle and a line leaves the quadratic in y whose
    !> roots are the intersection ordinates. Checked against the closed form
    !> res_x(x^2 + y^2 - 4, x - y) = 2y^2 - 4.
    subroutine test_resultant_eliminates_a_variable()
        type(qmpoly_t) :: circle, line, r, expected
        logical :: s

        circle = qmpoly_sub(qmpoly_add(qmpoly_pow(x(), 2, s), &
                                       qmpoly_pow(y(), 2, s), s), k("4"), s)
        line = qmpoly_sub(x(), y(), s)
        r = qmpoly_resultant(circle, line, 1, s)
        expected = qmpoly_sub(qmpoly_mul(k("2"), qmpoly_pow(y(), 2, s), s), &
                              k("4"), s)
        call ok("elimination removes the variable", qmpoly_degree(r, 1) <= 0)
        call ok("resultant matches the closed form 2y^2 - 4", &
                qmpoly_equal(r, expected))
    end subroutine test_resultant_eliminates_a_variable

    subroutine test_to_univariate()
        type(qmpoly_t) :: p
        type(qpoly_t) :: u
        type(quadratic_t) :: value
        logical :: s

        ! 2y^2 - 4, converted and evaluated at y = sqrt(6): 12 - 4 = 8.
        p = qmpoly_sub(qmpoly_mul(k("2"), qmpoly_pow(y(), 2, s), s), k("4"), s)
        u = qmpoly_to_univariate(p, 2, s)
        call ok("conversion succeeds for a single-variable polynomial", s)
        call ok("degree survives conversion", qpoly_degree(u) == 2)
        value = qpoly_eval(u, quad_root(D, s), s)
        call ok("converted polynomial evaluates to exactly 8 at sqrt(6)", &
                quad_equal(value, quad_rational("8", D, s)))
        ! A genuinely bivariate polynomial must be refused, not silently
        ! truncated to one of its slices.
        u = qmpoly_to_univariate(qmpoly_mul(x(), y(), s), 2, s)
        call ok("a two-variable polynomial is refused", .not. s)
    end subroutine test_to_univariate

end program test_fortsym_quadratic_multipoly
