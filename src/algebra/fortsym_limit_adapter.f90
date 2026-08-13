module fortsym_limit_adapter
    ! Binding-safe subroutine boundary for the typed native limit result.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_limits, only: limit_point_t, limit_value_t, limit_of
    implicit none
    private

    public :: calculate_limit

contains

    subroutine calculate_limit(a, e, var, point, direction, result, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e, var
        type(limit_point_t),       intent(in)    :: point
        integer,                   intent(in)    :: direction
        type(limit_value_t),       intent(out)   :: result
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why

        result = limit_of(a, e, var, point, direction, ok, why)
    end subroutine calculate_limit

end module fortsym_limit_adapter
