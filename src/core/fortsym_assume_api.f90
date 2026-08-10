module fortsym_assume_api
    ! Stable internal bridge for callers whose procedure namespace also exposes
    ! the C symbol named `fortsym_assume`.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_assume, only: assumption_context_t, &
        init_impl => init_assumption_context, record_impl => record_assumption, &
        FACT_REAL, FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO
    implicit none
    private

    public :: assumption_context_t, init_assumption_context, record_assumption
    public :: FACT_REAL, FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO

contains

    subroutine init_assumption_context(context, home)
        type(assumption_context_t), intent(inout) :: context
        type(arena_t), target,       intent(inout) :: home

        call init_impl(context, home)
    end subroutine init_assumption_context

    subroutine record_assumption(context, expression, facts)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: expression
        integer,                     intent(in)    :: facts

        call record_impl(context, expression, facts)
    end subroutine record_assumption

end module fortsym_assume_api
