program test_fortsym_quadratic_linalg
    ! Exact linear algebra over Q(sqrt(d)).
    !
    ! The oracle is substitution: a returned solution is fed back through the
    ! original matrix and the residual must be exactly zero, not small. Where a
    ! system has no solution or a family of them, the test asserts the reported
    ! status, because the caller's construction depends on telling those apart.
    use fortsym_quadratic, only: quadratic_t, quad, quad_rational, quad_root, &
        quad_add, quad_sub, quad_mul, quad_is_zero, quad_equal
    use fortsym_quadratic_linalg
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_square_system()
    call test_irrational_system()
    call test_dependent_extra_equation_is_ok()
    call test_contradictory_extra_equation_is_rejected()
    call test_underdetermined_reports_free_columns()
    call test_nullspace_spans_the_family()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_quadratic_linalg: ", n_pass, &
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

    !> True when A x - rhs vanishes exactly in every row.
    function residual_vanishes(matrix, x, rhs) result(exact)
        type(quadratic_t), intent(in) :: matrix(:, :), x(:), rhs(:)
        logical :: exact
        integer :: i, j
        type(quadratic_t) :: acc, term
        logical :: s

        exact = .true.
        do i = 1, size(matrix, 1)
            acc = rat("0")
            do j = 1, size(matrix, 2)
                term = quad_mul(matrix(i, j), x(j), s)
                acc = quad_add(acc, term, s)
            end do
            if (.not. quad_is_zero(quad_sub(acc, rhs(i), s))) exact = .false.
        end do
    end function residual_vanishes

    subroutine test_square_system()
        type(quadratic_t) :: m(2, 2), rhs(2)
        type(quadratic_t), allocatable :: x(:)
        integer :: status

        m(1, 1) = rat("2"); m(1, 2) = rat("1")
        m(2, 1) = rat("1"); m(2, 2) = rat("-3")
        rhs(1) = rat("5"); rhs(2) = rat("-1")
        call quad_solve(m, rhs, x, status)
        call ok("square system solves", status == SOLVE_OK)
        call ok("square residual is exactly zero", residual_vanishes(m, x, rhs))
        call ok("square solution is the expected rational", &
                quad_equal(x(1), rat("2")) .and. quad_equal(x(2), rat("1")))
    end subroutine test_square_system

    !> A system whose solution is genuinely irrational, so a rational-only
    !> solver would be unable to represent the answer at all.
    subroutine test_irrational_system()
        type(quadratic_t) :: m(2, 2), rhs(2)
        type(quadratic_t), allocatable :: x(:)
        integer :: status
        logical :: s

        m(1, 1) = quad_root(6, s); m(1, 2) = rat("0")
        m(2, 1) = rat("0"); m(2, 2) = quad_root(6, s)
        rhs(1) = rat("1"); rhs(2) = rat("6")
        call quad_solve(m, rhs, x, status)
        call ok("irrational system solves", status == SOLVE_OK)
        call ok("irrational residual is exactly zero", &
                residual_vanishes(m, x, rhs))
        ! x2 = 6/sqrt(6) = sqrt(6) exactly.
        call ok("6/sqrt(6) is exactly sqrt(6)", quad_equal(x(2), quad_root(6, s)))
    end subroutine test_irrational_system

    !> Three equations, two unknowns, the third a multiple of the first.
    !> This is the shape of a Runge-Kutta stage row whose simplifying
    !> assumptions are dependent for the chosen nodes, and it must succeed.
    subroutine test_dependent_extra_equation_is_ok()
        type(quadratic_t) :: m(3, 2), rhs(3)
        type(quadratic_t), allocatable :: x(:)
        integer :: status

        m(1, 1) = rat("1"); m(1, 2) = rat("1")
        m(2, 1) = rat("1"); m(2, 2) = rat("-1")
        m(3, 1) = rat("3"); m(3, 2) = rat("3")
        rhs(1) = rat("4"); rhs(2) = rat("0"); rhs(3) = rat("12")
        call quad_solve(m, rhs, x, status)
        call ok("dependent extra equation still solves", status == SOLVE_OK)
        call ok("overdetermined residual is exactly zero", &
                residual_vanishes(m, x, rhs))
    end subroutine test_dependent_extra_equation_is_ok

    !> The same shape with the extra equation contradicting. A solver that
    !> returned a plausible answer here would silently certify a wrong tableau.
    subroutine test_contradictory_extra_equation_is_rejected()
        type(quadratic_t) :: m(3, 2), rhs(3)
        type(quadratic_t), allocatable :: x(:)
        integer :: status

        m(1, 1) = rat("1"); m(1, 2) = rat("1")
        m(2, 1) = rat("1"); m(2, 2) = rat("-1")
        m(3, 1) = rat("3"); m(3, 2) = rat("3")
        rhs(1) = rat("4"); rhs(2) = rat("0"); rhs(3) = rat("13")
        call quad_solve(m, rhs, x, status)
        call ok("contradictory extra equation is rejected", &
                status == SOLVE_INCONSISTENT)
    end subroutine test_contradictory_extra_equation_is_rejected

    subroutine test_underdetermined_reports_free_columns()
        type(quadratic_t) :: m(1, 3), rhs(1)
        type(quadratic_t), allocatable :: x(:)
        logical, allocatable :: free(:)
        integer :: status

        m(1, 1) = rat("1"); m(1, 2) = rat("2"); m(1, 3) = rat("3")
        rhs(1) = rat("6")
        call quad_solve(m, rhs, x, status, free)
        call ok("underdetermined system is reported as such", &
                status == SOLVE_UNDERDETERMINED)
        call ok("two columns are free", count(free) == 2)
        call ok("particular solution satisfies the equation", &
                residual_vanishes(m, x, rhs))
    end subroutine test_underdetermined_reports_free_columns

    !> Every null-space vector must be annihilated exactly, and there must be
    !> one per free column, so a caller can parameterise the whole family.
    subroutine test_nullspace_spans_the_family()
        type(quadratic_t) :: m(1, 3), zero_rhs(1)
        type(quadratic_t), allocatable :: basis(:, :)
        integer :: status, k
        logical :: all_exact
        logical :: s

        m(1, 1) = rat("1"); m(1, 2) = rat("2"); m(1, 3) = quad_root(6, s)
        zero_rhs(1) = rat("0")
        call quad_nullspace(m, basis, status)
        call ok("null space computes", status == SOLVE_UNDERDETERMINED)
        call ok("one basis vector per free column", size(basis, 2) == 2)
        all_exact = .true.
        do k = 1, size(basis, 2)
            if (.not. residual_vanishes(m, basis(:, k), zero_rhs)) then
                all_exact = .false.
            end if
        end do
        call ok("every basis vector is annihilated exactly", all_exact)
    end subroutine test_nullspace_spans_the_family

end program test_fortsym_quadratic_linalg
