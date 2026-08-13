module fortsym_matrix_adapter
    ! Transport adapter for the bounded exact dense matrix facade. Matrix
    ! construction remains an expression-level List owner; the determinant
    ! algorithm remains solely in fortsym_matrix.
    use fortsym_arena, only: arena_t
    use fortsym_engine, only: engine_t, engine_result_t
    use fortsym_expr, only: expr_t
    use fortsym_matrix, only: matrix_det, matrix_trace, matrix_is_diagonal, matrix_is_zero_matrix, &
        matrix_is_upper, matrix_is_lower, matrix_is_upper_hessenberg, &
        matrix_is_lower_hessenberg, matrix_is_anti_symmetric, matrix_is_symbolic, &
        matrix_is_identity, matrix_is_echelon, matrix_is_hermitian, matrix_is_symmetric, &
        matrix_rank, matrix_inverse, &
        matrix_transpose, matrix_conjugate, matrix_adjoint, matrix_add, &
        matrix_multiply_elementwise, matrix_negate, matrix_divide, &
        matrix_null_space, &
        matrix_rref, matrix_dot
    use fortsym_string, only: str_t, chars
    implicit none
    private

    public :: calculate_matrix_det
    public :: calculate_matrix_trace
    public :: calculate_matrix_is_diagonal
    public :: calculate_matrix_is_zero_matrix
    public :: calculate_matrix_is_upper
    public :: calculate_matrix_is_lower
    public :: calculate_matrix_is_upper_hessenberg
    public :: calculate_matrix_is_lower_hessenberg
    public :: calculate_matrix_is_identity
    public :: calculate_matrix_is_echelon
    public :: calculate_matrix_is_hermitian
    public :: calculate_matrix_is_anti_symmetric
    public :: calculate_matrix_is_symbolic
    public :: calculate_matrix_is_symmetric
    public :: calculate_matrix_rank
    public :: calculate_matrix_inverse
    public :: calculate_matrix_transpose
    public :: calculate_matrix_conjugate
    public :: calculate_matrix_adjoint
    public :: calculate_matrix_multiply_elementwise
    public :: calculate_matrix_add
    public :: calculate_matrix_negate
    public :: calculate_matrix_divide
    public :: calculate_matrix_null_space
    public :: calculate_matrix_rref
    public :: calculate_matrix_multiply

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

    subroutine calculate_matrix_trace(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message
        logical :: canonical

        value = matrix_trace(a, expression, ok, message, canonical)
        why = chars(message)
        if (.not. ok .or. canonical) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_trace

    subroutine calculate_matrix_is_diagonal(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_diagonal(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_diagonal

    subroutine calculate_matrix_is_zero_matrix(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_zero_matrix(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_zero_matrix

    subroutine calculate_matrix_is_upper(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_upper(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_upper

    subroutine calculate_matrix_is_lower(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_lower(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_lower

    subroutine calculate_matrix_is_upper_hessenberg(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_upper_hessenberg( &
            a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_upper_hessenberg

    subroutine calculate_matrix_is_lower_hessenberg(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_lower_hessenberg( &
            a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_lower_hessenberg

    subroutine calculate_matrix_is_identity(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_identity(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_identity

    subroutine calculate_matrix_is_echelon(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_echelon(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_echelon

    subroutine calculate_matrix_is_hermitian(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_hermitian(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_hermitian

    subroutine calculate_matrix_is_anti_symmetric(a, engine, expression, simplify, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        logical, intent(in) :: simplify
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_anti_symmetric( &
            a, engine, expression, simplify, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_anti_symmetric

    subroutine calculate_matrix_is_symbolic(a, engine, expression, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_symbolic(a, engine, expression, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_symbolic

    subroutine calculate_matrix_is_symmetric(a, engine, expression, simplify, verdict, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        logical, intent(in) :: simplify
        integer, intent(out) :: verdict
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        call matrix_is_symmetric(a, engine, expression, simplify, verdict, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_is_symmetric

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

    subroutine calculate_matrix_conjugate(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        call matrix_conjugate(a, engine, expression, value, ok, why)
    end subroutine calculate_matrix_conjugate

    subroutine calculate_matrix_adjoint(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        call matrix_adjoint(a, engine, expression, value, ok, why)
    end subroutine calculate_matrix_adjoint

    subroutine calculate_matrix_multiply_elementwise( &
            a, engine, left, right, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: left, right
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message
        logical :: canonical

        value = matrix_multiply_elementwise( &
            a, left, right, ok, message, canonical)
        why = chars(message)
        if (.not. ok .or. canonical) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_multiply_elementwise

    subroutine calculate_matrix_add(a, engine, left, right, value, ok, why, subtract)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: left, right
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        logical, optional, intent(in) :: subtract
        type(str_t) :: message
        logical :: canonical

        value = matrix_add(a, left, right, ok, message, subtract, canonical)
        why = chars(message)
        if (.not. ok .or. canonical) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_add

    subroutine calculate_matrix_negate(a, engine, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message
        logical :: canonical

        value = matrix_negate(a, expression, ok, message, canonical)
        why = chars(message)
        if (.not. ok .or. canonical) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_negate

    subroutine calculate_matrix_divide(a, engine, matrix, scalar, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: matrix, scalar
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message
        logical :: canonical

        value = matrix_divide(a, matrix, scalar, ok, message, canonical)
        why = chars(message)
        if (.not. ok .or. canonical) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_divide

    subroutine calculate_matrix_null_space(a, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        value = matrix_null_space(a, expression, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_null_space

    subroutine calculate_matrix_rref(a, expression, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message

        value = matrix_rref(a, expression, ok, message)
        why = chars(message)
    end subroutine calculate_matrix_rref

    subroutine calculate_matrix_multiply(a, engine, left, right, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: left, right
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: message
        logical :: canonical

        value = matrix_dot(a, left, right, ok, message, canonical)
        why = chars(message)
        if (.not. ok .or. canonical) return
        call simplify_matrix_value(engine, value, ok, why)
    end subroutine calculate_matrix_multiply

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
