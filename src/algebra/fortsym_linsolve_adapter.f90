module fortsym_linsolve_adapter
    ! One owner for the bounded SymPy-shaped linsolve transport. The exact
    ! elimination itself remains in fortsym_linalg.
    use fortsym_engine, only: engine_t
    use fortsym_expr, only: expr_t
    use fortsym_linalg, only: exact_linear_system_result_t, &
        solve_exact_linear_system, parametric_linear_system_result_t, &
        solve_parametric_linear_system
    use fortsym_string, only: chars
    implicit none
    private

    public :: calculate_linsolve
    public :: calculate_parametric_linsolve

contains

    subroutine calculate_linsolve(engine, matrix, right_hand_side, values, ok, why)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: matrix(:, :), right_hand_side(:)
        type(expr_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), allocatable :: rhs_matrix(:, :)
        type(exact_linear_system_result_t) :: result
        integer :: equation_count, row

        allocate(values(0))
        ok = .false.
        why = ""
        equation_count = size(matrix, 1)
        if (equation_count < 1 .or. size(matrix, 2) /= equation_count) then
            why = "linsolve requires a square matrix"
            return
        end if
        if (size(right_hand_side) /= equation_count) then
            why = "linsolve requires a compatible right-hand side"
            return
        end if

        allocate(rhs_matrix(equation_count, 1))
        do row = 1, equation_count
            rhs_matrix(row, 1) = right_hand_side(row)
        end do
        result = solve_exact_linear_system(engine, matrix, rhs_matrix)
        if (.not. result%ok) then
            why = chars(result%message)
            return
        end if
        deallocate(values)
        allocate(values(equation_count))
        do row = 1, equation_count
            values(row) = result%values(row, 1)
        end do
        ok = .true.
    end subroutine calculate_linsolve

    subroutine calculate_parametric_linsolve( &
            engine, matrix, right_hand_side, variables, values, consistent, &
            ok, why)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: matrix(:, :), right_hand_side(:)
        type(expr_t), intent(in) :: variables(:)
        type(expr_t), allocatable, intent(out) :: values(:)
        logical, intent(out) :: consistent, ok
        character(:), allocatable, intent(out) :: why
        type(parametric_linear_system_result_t) :: result

        result = solve_parametric_linear_system( &
            engine, matrix, right_hand_side, variables)
        values = result%values
        consistent = result%consistent
        ok = result%ok
        why = chars(result%message)
    end subroutine calculate_parametric_linsolve

end module fortsym_linsolve_adapter
