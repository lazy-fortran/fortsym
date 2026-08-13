module fortsym_linalg
    use fortsym_arena, only: NK_INT, NK_RAT
    use fortsym_engine, only: engine_result_t, engine_t
    use fortsym_expr, only: &
        expr_t, is_valid, num, operator(+), operator(-), operator(*), &
        operator(/), same_arena
    use fortsym_matrix, only: matrix_rref_values
    use fortsym_string, only: str, str_t
    implicit none

    private

    public :: exact_linear_system_result_t
    public :: solve_exact_linear_system
    public :: parametric_linear_system_result_t
    public :: solve_parametric_linear_system

    type :: exact_linear_system_result_t
        logical :: ok = .false.
        type(expr_t), allocatable :: values(:, :)
        type(str_t) :: message
    end type exact_linear_system_result_t

    type :: parametric_linear_system_result_t
        logical :: ok = .false.
        logical :: consistent = .false.
        type(expr_t), allocatable :: values(:)
        type(str_t) :: message
    end type parametric_linear_system_result_t

contains

    function solve_exact_linear_system( &
            engine, matrix, right_hand_sides) result(result)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: matrix(:, :), right_hand_sides(:, :)
        type(exact_linear_system_result_t) :: result

        type(expr_t), allocatable :: exact_matrix(:, :), exact_rhs(:, :)
        type(expr_t), allocatable :: work(:, :)
        type(expr_t) :: factor, pivot_value, temporary
        integer :: column, equation_count, pivot, right_hand_side_count
        integer :: row, work_column
        logical :: exact

        equation_count = size(matrix, 1)
        right_hand_side_count = size(right_hand_sides, 2)
        if (equation_count < 1 .or. size(matrix, 2) /= equation_count) then
            call fail(result, "exact linear system requires a square matrix")
            return
        end if
        if (size(right_hand_sides, 1) /= equation_count .or. &
            right_hand_side_count < 1) then
            call fail( &
                result, "exact linear system requires compatible right-hand sides")
            return
        end if
        if (.not. valid_common_arena(matrix, right_hand_sides)) then
            call fail( &
                result, "exact linear system entries must share one arena")
            return
        end if

        allocate(exact_matrix(equation_count, equation_count))
        allocate(exact_rhs(equation_count, right_hand_side_count))
        call simplify_exact_array(engine, matrix, exact_matrix, exact)
        if (.not. exact) then
            call fail( &
                result, "exact linear system matrix must contain rational values")
            return
        end if
        call simplify_exact_array( &
            engine, right_hand_sides, exact_rhs, exact)
        if (.not. exact) then
            call fail( &
                result, "exact linear system right-hand sides must be rational")
            return
        end if

        allocate(work( &
            equation_count, equation_count + right_hand_side_count))
        work(:, :equation_count) = exact_matrix
        work(:, equation_count + 1:) = exact_rhs
        do column = 1, equation_count
            pivot = first_nonzero_row(work(:, column), column)
            if (pivot == 0) then
                call fail(result, "exact linear system is singular")
                return
            end if
            if (pivot /= column) then
                do work_column = 1, size(work, 2)
                    temporary = work(column, work_column)
                    work(column, work_column) = work(pivot, work_column)
                    work(pivot, work_column) = temporary
                end do
            end if

            pivot_value = work(column, column)
            do work_column = column, size(work, 2)
                call simplify_exact( &
                    engine, work(column, work_column) / pivot_value, &
                    work(column, work_column), exact)
                if (.not. exact) then
                    call fail( &
                        result, "exact linear elimination exceeded its domain")
                    return
                end if
            end do
            do row = 1, equation_count
                if (row == column) cycle
                factor = work(row, column)
                if (is_exact_zero(factor)) cycle
                do work_column = column, size(work, 2)
                    call simplify_exact( &
                        engine, work(row, work_column) - &
                        factor * work(column, work_column), &
                        work(row, work_column), exact)
                    if (.not. exact) then
                        call fail( &
                            result, &
                            "exact linear elimination exceeded its domain")
                        return
                    end if
                end do
            end do
        end do

        allocate(result%values(equation_count, right_hand_side_count))
        result%values = work(:, equation_count + 1:)
        call verify_residual( &
            engine, exact_matrix, exact_rhs, result%values, exact)
        if (.not. exact) then
            deallocate(result%values)
            call fail(result, "exact linear system residual verification failed")
            return
        end if
        result%ok = .true.
        result%message = str("")
    end function solve_exact_linear_system

    function solve_parametric_linear_system( &
            engine, matrix, right_hand_side, variables) result(result)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: matrix(:, :), right_hand_side(:)
        type(expr_t), intent(in) :: variables(:)
        type(parametric_linear_system_result_t) :: result

        type(expr_t), allocatable :: exact_matrix(:, :), exact_rhs(:, :)
        type(expr_t), allocatable :: augmented(:, :)
        type(expr_t), allocatable :: values(:)
        type(expr_t) :: expression, coefficient
        type(engine_result_t) :: simplified
        type(str_t) :: why
        integer, allocatable :: pivots(:)
        integer :: equation_count, variable_count, rank
        integer :: row, pivot_index, pivot_column, column
        logical :: exact, valid, zero_row

        allocate (result%values(0))
        result%message = str("")
        equation_count = size(matrix, 1)
        variable_count = size(matrix, 2)
        if (equation_count < 1 .or. variable_count < 1) then
            result%message = str( &
                "linsolve requires a nonempty coefficient matrix")
            return
        end if
        if (size(right_hand_side) /= equation_count) then
            result%message = str( &
                "linsolve requires a compatible right-hand side")
            return
        end if
        if (size(variables) /= variable_count) then
            result%message = str( &
                "linsolve requires one symbol per matrix column")
            return
        end if
        if (.not. valid_common_arena_vector(matrix, right_hand_side)) then
            result%message = str( &
                "linsolve entries must share one arena")
            return
        end if
        do column = 1, variable_count
            if (.not. is_valid(variables(column))) then
                result%message = str("linsolve received an invalid symbol")
                return
            end if
            if (.not. same_arena(matrix(1, 1), variables(column))) then
                result%message = str( &
                    "linsolve symbols must share the matrix arena")
                return
            end if
        end do

        allocate (exact_matrix(equation_count, variable_count))
        allocate (exact_rhs(equation_count, 1))
        call simplify_exact_array(engine, matrix, exact_matrix, exact)
        if (.not. exact) then
            result%message = str( &
                "linsolve coefficient matrix must contain rational values")
            return
        end if
        call simplify_exact_vector(engine, right_hand_side, exact_rhs(:, 1), exact)
        if (.not. exact) then
            result%message = str( &
                "linsolve right-hand side must contain rational values")
            return
        end if

        allocate (augmented(equation_count, variable_count + 1))
        augmented(:, :variable_count) = exact_matrix
        augmented(:, variable_count + 1) = exact_rhs(:, 1)
        call matrix_rref_values( &
            matrix(1, 1)%a, augmented, rank, pivots, valid, why, variable_count)
        if (.not. valid) then
            result%message = why
            return
        end if

        do row = rank + 1, equation_count
            zero_row = .true.
            do column = 1, variable_count
                if (.not. is_exact_zero(augmented(row, column))) then
                    zero_row = .false.
                    exit
                end if
            end do
            if (zero_row) then
                if (.not. is_exact_zero(augmented(row, variable_count + 1))) then
                    result%ok = .true.
                    result%message = str("")
                    return
                end if
            end if
        end do

        allocate (values(variable_count))
        do column = 1, variable_count
            values(column) = variables(column)
        end do
        do pivot_index = 1, rank
            pivot_column = pivots(pivot_index)
            expression = augmented(pivot_index, variable_count + 1)
            do column = 1, variable_count
                if (column == pivot_column) cycle
                coefficient = augmented(pivot_index, column)
                if (is_exact_zero(coefficient)) cycle
                expression = expression - coefficient*values(column)
                simplified = engine%simplify(expression)
                if (.not. simplified%ok) then
                    result%message = str( &
                        "linsolve could not simplify a free-parameter value")
                    return
                end if
                expression = simplified%value
            end do
            values(pivot_column) = expression
        end do

        deallocate (result%values)
        call move_alloc(values, result%values)
        result%ok = .true.
        result%consistent = .true.
        result%message = str("")
    end function solve_parametric_linear_system

    subroutine simplify_exact_array(engine, input, output, exact)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: input(:, :)
        type(expr_t), intent(out) :: output(:, :)
        logical, intent(out) :: exact

        integer :: column, row

        exact = .false.
        do column = 1, size(input, 2)
            do row = 1, size(input, 1)
                call simplify_exact( &
                    engine, input(row, column), output(row, column), exact)
                if (.not. exact) return
            end do
        end do
        exact = .true.
    end subroutine simplify_exact_array

    subroutine simplify_exact_vector(engine, input, output, exact)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: input(:)
        type(expr_t), intent(out) :: output(:)
        logical, intent(out) :: exact

        integer :: row

        exact = .false.
        if (size(output) /= size(input)) return
        do row = 1, size(input)
            call simplify_exact(engine, input(row), output(row), exact)
            if (.not. exact) return
        end do
        exact = .true.
    end subroutine simplify_exact_vector

    subroutine simplify_exact(engine, input, output, exact)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: input
        type(expr_t), intent(out) :: output
        logical, intent(out) :: exact

        type(engine_result_t) :: simplified

        simplified = engine%simplify(input)
        exact = .false.
        if (.not. simplified%ok) return
        select case (simplified%value%kind())
        case (NK_INT, NK_RAT)
            output = simplified%value
            exact = .true.
        end select
    end subroutine simplify_exact

    subroutine verify_residual(engine, matrix, right_hand_sides, values, exact)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: matrix(:, :), right_hand_sides(:, :)
        type(expr_t), intent(in) :: values(:, :)
        logical, intent(out) :: exact

        type(expr_t) :: accumulated, residual
        integer :: column, equation, variable

        exact = .false.
        do column = 1, size(right_hand_sides, 2)
            do equation = 1, size(matrix, 1)
                accumulated = num(matrix(1, 1)%a, 0)
                do variable = 1, size(matrix, 2)
                    call simplify_exact( &
                        engine, accumulated + &
                        matrix(equation, variable) * values(variable, column), &
                        accumulated, exact)
                    if (.not. exact) return
                end do
                call simplify_exact( &
                    engine, accumulated - right_hand_sides(equation, column), &
                    residual, exact)
                if (.not. exact) return
                if (.not. is_exact_zero(residual)) return
            end do
        end do
        exact = .true.
    end subroutine verify_residual

    function first_nonzero_row(column, first_row) result(row)
        type(expr_t), intent(in) :: column(:)
        integer, intent(in) :: first_row
        integer :: row

        do row = first_row, size(column)
            if (.not. is_exact_zero(column(row))) return
        end do
        row = 0
    end function first_nonzero_row

    function valid_common_arena(matrix, right_hand_sides) result(valid)
        type(expr_t), intent(in) :: matrix(:, :), right_hand_sides(:, :)
        logical :: valid

        integer :: column, row

        valid = .false.
        if (.not. is_valid(matrix(1, 1))) return
        do column = 1, size(matrix, 2)
            do row = 1, size(matrix, 1)
                if (.not. is_valid(matrix(row, column))) return
                if (.not. same_arena(matrix(1, 1), matrix(row, column))) return
            end do
        end do
        do column = 1, size(right_hand_sides, 2)
            do row = 1, size(right_hand_sides, 1)
                if (.not. is_valid(right_hand_sides(row, column))) return
                if (.not. same_arena( &
                    matrix(1, 1), right_hand_sides(row, column))) return
            end do
        end do
        valid = .true.
    end function valid_common_arena

    function valid_common_arena_vector(matrix, values) result(valid)
        type(expr_t), intent(in) :: matrix(:, :), values(:)
        logical :: valid
        integer :: column, row

        valid = .false.
        if (.not. is_valid(matrix(1, 1))) return
        do column = 1, size(matrix, 2)
            do row = 1, size(matrix, 1)
                if (.not. is_valid(matrix(row, column))) return
                if (.not. same_arena(matrix(1, 1), matrix(row, column))) then
                    return
                end if
            end do
        end do
        do row = 1, size(values)
            if (.not. is_valid(values(row))) return
            if (.not. same_arena(matrix(1, 1), values(row))) return
        end do
        valid = .true.
    end function valid_common_arena_vector

    function is_exact_zero(expression) result(zero)
        type(expr_t), intent(in) :: expression
        logical :: zero

        zero = expression%int_value() == 0
    end function is_exact_zero

    subroutine fail(result, message)
        type(exact_linear_system_result_t), intent(inout) :: result
        character(*), intent(in) :: message

        result%ok = .false.
        result%message = str(message)
    end subroutine fail

end module fortsym_linalg
