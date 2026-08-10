module fortsym_quadratic_linalg
    !> Exact linear algebra over Q(sqrt(d)).
    !>
    !> Every step of a reduced Runge-Kutta system is a linear solve, and the
    !> three shapes all occur in one construction:
    !>
    !>   square          the weights, from sum_i b_i c_i^(q-1) = 1/q
    !>   overdetermined  a stage row, where the simplifying assumptions supply
    !>                   more equations than the row has coefficients and the
    !>                   extra ones are dependent only for the right nodes
    !>   underdetermined a step that leaves free parameters to be fixed later
    !>
    !> So the solver reports which case it is in rather than assuming one.
    !> "More equations than unknowns" must come back as SOLVE_OK when the extra
    !> equations are dependent, and as SOLVE_INCONSISTENT when they are not:
    !> in Dormand and Prince's construction that distinction is precisely the
    !> condition that fixes a node, and collapsing the two would either hide a
    !> wrong tableau or reject a right one.
    !>
    !> Elimination is Gauss-Jordan with exact pivoting. A pivot is rejected
    !> only when it is exactly zero, never when it is small, because there is
    !> no such thing as small here.
    use fortsym_quadratic, only: quadratic_t, quad_rational, quad_add, &
        quad_sub, quad_mul, quad_div, quad_is_zero, quad_neg
    implicit none
    private

    public :: quad_solve, quad_nullspace
    public :: SOLVE_OK, SOLVE_INCONSISTENT, SOLVE_UNDERDETERMINED, SOLVE_FAILED

    !> Unique solution, possibly after discarding dependent equations.
    integer, parameter :: SOLVE_OK = 0
    !> No solution: an equation reduces to 0 = nonzero.
    integer, parameter :: SOLVE_INCONSISTENT = 1
    !> Consistent but with free columns; `solution` is the particular solution
    !> with free variables set to zero, and quad_nullspace gives the rest.
    integer, parameter :: SOLVE_UNDERDETERMINED = 2
    !> Arithmetic failure in the field.
    integer, parameter :: SOLVE_FAILED = 3

contains

    !> Solve A x = rhs exactly.
    !>
    !> `free_columns` is set true for each column without a pivot, so a caller
    !> that expects a unique solution can insist it is all false, and a caller
    !> parameterising a family knows exactly which unknowns it may choose.
    subroutine quad_solve(matrix, rhs, solution, status, free_columns)
        type(quadratic_t), intent(in) :: matrix(:, :), rhs(:)
        type(quadratic_t), allocatable, intent(out) :: solution(:)
        integer, intent(out) :: status
        logical, allocatable, intent(out), optional :: free_columns(:)

        type(quadratic_t), allocatable :: work(:, :)
        integer, allocatable :: pivot_of_row(:)
        logical, allocatable :: is_free(:)
        integer :: rows, cols, r, cc, row_used, k
        integer :: radicand
        logical :: ok
        type(quadratic_t) :: zero, factor, pivot

        rows = size(matrix, 1)
        cols = size(matrix, 2)
        radicand = matrix(1, 1)%radicand
        zero = quad_rational("0", radicand, ok)
        allocate (solution(cols), pivot_of_row(cols), is_free(cols))
        solution = zero
        pivot_of_row = 0
        is_free = .true.
        status = SOLVE_FAILED
        if (.not. ok) return
        if (size(rhs) /= rows) return

        allocate (work(rows, cols + 1))
        work(:, 1:cols) = matrix
        work(:, cols + 1) = rhs

        row_used = 0
        do cc = 1, cols
            r = 0
            do k = row_used + 1, rows
                if (.not. quad_is_zero(work(k, cc))) then
                    r = k
                    exit
                end if
            end do
            if (r == 0) cycle
            row_used = row_used + 1
            call swap_rows(work, row_used, r)
            ! Copy the pivot out first: scale_row divides the row in place, so
            ! passing the array element directly would alias it and every
            ! division after the pivot's own column would use a changed value.
            pivot = work(row_used, cc)
            call scale_row(work, row_used, pivot, ok)
            if (.not. ok) return
            do k = 1, rows
                if (k == row_used) cycle
                if (quad_is_zero(work(k, cc))) cycle
                factor = work(k, cc)
                call eliminate(work, k, row_used, factor, ok)
                if (.not. ok) return
            end do
            pivot_of_row(row_used) = cc
            is_free(cc) = .false.
        end do

        ! Any surviving row of the form 0 = nonzero makes the system unsolvable.
        do k = row_used + 1, rows
            if (.not. quad_is_zero(work(k, cols + 1))) then
                status = SOLVE_INCONSISTENT
                if (present(free_columns)) free_columns = is_free
                return
            end if
        end do

        do k = 1, row_used
            solution(pivot_of_row(k)) = work(k, cols + 1)
        end do

        if (any(is_free)) then
            status = SOLVE_UNDERDETERMINED
        else
            status = SOLVE_OK
        end if
        if (present(free_columns)) free_columns = is_free
    end subroutine quad_solve

    !> A basis of the null space, one column per free variable.
    !>
    !> Together with the particular solution from quad_solve this describes the
    !> whole solution set, which is what a construction needs when it defers
    !> some coefficients to a later condition.
    subroutine quad_nullspace(matrix, basis, status)
        type(quadratic_t), intent(in) :: matrix(:, :)
        type(quadratic_t), allocatable, intent(out) :: basis(:, :)
        integer, intent(out) :: status

        type(quadratic_t), allocatable :: rhs(:), particular(:), column(:)
        logical, allocatable :: is_free(:)
        integer :: rows, cols, n_free, f, j, radicand, local_status
        logical :: ok
        type(quadratic_t) :: zero, one

        rows = size(matrix, 1)
        cols = size(matrix, 2)
        radicand = matrix(1, 1)%radicand
        zero = quad_rational("0", radicand, ok)
        one = quad_rational("1", radicand, ok)
        allocate (rhs(rows))
        rhs = zero
        call quad_solve(matrix, rhs, particular, local_status, is_free)
        status = local_status
        if (local_status == SOLVE_FAILED) then
            allocate (basis(cols, 0))
            return
        end if

        n_free = count(is_free)
        allocate (basis(cols, n_free))
        if (n_free == 0) then
            status = SOLVE_OK
            return
        end if

        ! For each free column set it to one, the other free columns to zero,
        ! and read the pivot columns off the reduced system.
        f = 0
        do j = 1, cols
            if (.not. is_free(j)) cycle
            f = f + 1
            call solve_with_free(matrix, j, is_free, column, ok)
            if (.not. ok) then
                status = SOLVE_FAILED
                return
            end if
            basis(:, f) = column
        end do
        status = SOLVE_UNDERDETERMINED
    end subroutine quad_nullspace

    subroutine solve_with_free(matrix, chosen, is_free, column, ok)
        type(quadratic_t), intent(in) :: matrix(:, :)
        integer, intent(in) :: chosen
        logical, intent(in) :: is_free(:)
        type(quadratic_t), allocatable, intent(out) :: column(:)
        logical, intent(out) :: ok

        type(quadratic_t), allocatable :: reduced(:, :), rhs(:), part(:)
        integer :: rows, cols, j, kept, radicand, status
        integer, allocatable :: keep(:)
        type(quadratic_t) :: zero, one

        rows = size(matrix, 1)
        cols = size(matrix, 2)
        radicand = matrix(1, 1)%radicand
        zero = quad_rational("0", radicand, ok)
        one = quad_rational("1", radicand, ok)
        if (.not. ok) return

        allocate (keep(0))
        do j = 1, cols
            if (.not. is_free(j)) keep = [keep, j]
        end do
        kept = size(keep)
        allocate (reduced(rows, kept), rhs(rows), column(cols))
        do j = 1, kept
            reduced(:, j) = matrix(:, keep(j))
        end do
        ! Move the chosen free column to the right-hand side with value one.
        do j = 1, rows
            rhs(j) = quad_neg(matrix(j, chosen), ok)
            if (.not. ok) return
        end do
        call quad_solve(reduced, rhs, part, status)
        ok = status == SOLVE_OK .or. status == SOLVE_UNDERDETERMINED
        if (.not. ok) return
        column = zero
        column(chosen) = one
        do j = 1, kept
            column(keep(j)) = part(j)
        end do
    end subroutine solve_with_free

    subroutine swap_rows(work, i, j)
        type(quadratic_t), intent(inout) :: work(:, :)
        integer, intent(in) :: i, j
        type(quadratic_t), allocatable :: tmp(:)

        if (i == j) return
        allocate (tmp(size(work, 2)))
        tmp = work(i, :)
        work(i, :) = work(j, :)
        work(j, :) = tmp
    end subroutine swap_rows

    subroutine scale_row(work, i, pivot, ok)
        type(quadratic_t), intent(inout) :: work(:, :)
        integer, intent(in) :: i
        type(quadratic_t), intent(in) :: pivot
        logical, intent(out) :: ok
        integer :: j

        ok = .true.
        do j = 1, size(work, 2)
            work(i, j) = quad_div(work(i, j), pivot, ok)
            if (.not. ok) return
        end do
    end subroutine scale_row

    subroutine eliminate(work, target_row, pivot_row, factor, ok)
        type(quadratic_t), intent(inout) :: work(:, :)
        integer, intent(in) :: target_row, pivot_row
        type(quadratic_t), intent(in) :: factor
        logical, intent(out) :: ok
        integer :: j
        type(quadratic_t) :: product

        ok = .true.
        do j = 1, size(work, 2)
            product = quad_mul(factor, work(pivot_row, j), ok)
            if (.not. ok) return
            work(target_row, j) = quad_sub(work(target_row, j), product, ok)
            if (.not. ok) return
        end do
    end subroutine eliminate

end module fortsym_quadratic_linalg
