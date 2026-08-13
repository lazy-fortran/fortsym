module fortsym_matrix_adapter
    ! Transport adapter for the bounded exact dense matrix facade. Matrix
    ! construction remains an expression-level List owner; the determinant
    ! algorithm remains solely in fortsym_matrix.
    use fortsym_arena, only: arena_t
    use fortsym_engine, only: engine_t, engine_result_t
    use fortsym_expr, only: expr_t
    use fortsym_matrix, only: matrix_det, matrix_rank, matrix_inverse, &
        matrix_transpose
    use fortsym_string, only: str_t, chars
    implicit none
    private

    public :: calculate_matrix_det
    public :: calculate_matrix_rank
    public :: calculate_matrix_inverse
    public :: calculate_matrix_transpose

contains

    subroutine calculate_matrix_det(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message
        type(engine_result_t) :: simplified

        value = matrix_det(a, expression, ok, message)
        why = chars(message)
        if (.not. ok) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_det

    subroutine calculate_matrix_rank(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        value = matrix_rank(a, expression, ok, message)
        why = chars(message)
        if (.not. ok) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_rank

    subroutine calculate_matrix_inverse(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        value = matrix_inverse(a, expression, ok, message)
        why = chars(message)
        if (.not. ok) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_inverse

    subroutine calculate_matrix_transpose(a, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        value = matrix_transpose(a, expression, ok)
        if (ok) then
            why = ""
        else
            why = "matrix transpose requires a nonempty rectangular matrix"
        end if
    end subroutine calculate_matrix_transpose

    subroutine simplify_matrix_value(engine, value, ok, why)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(inout) :: value
        logical, intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(engine_result_t) :: simplified

        simplified = engine%simplify(value)
        if (.not. simplified%ok) then
            ok = .false.
            why = "matrix result simplification failed: "// &
                chars(simplified%message)
            return
        end if
        value = simplified%value
    end subroutine simplify_matrix_value

end module fortsym_matrix_adapter
