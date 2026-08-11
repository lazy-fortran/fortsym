module fortsym_assume_api
    ! Stable internal bridge for callers whose procedure namespace also exposes
    ! the C symbol named `fortsym_assume`.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_assume, only: assumption_context_t, &
        init_impl => init_assumption_context, record_impl => record_assumption, &
        relation_impl => record_relation, &
        FACT_REAL, FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO, &
        FACT_INTEGER, FACT_POSITIVE_INTEGER
    implicit none
    private

    public :: assumption_context_t, init_assumption_context, &
        record_assumption, record_relation, clone_assumption_context
    public :: FACT_REAL, FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO, &
        FACT_INTEGER, FACT_POSITIVE_INTEGER

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

    subroutine record_relation(context, relation, ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: relation
        logical,                     intent(out)   :: ok
        character(:), allocatable,   intent(out)   :: why

        call relation_impl(context, relation, ok, why)
    end subroutine record_relation

    subroutine clone_assumption_context(child, parent)
        type(assumption_context_t), intent(inout) :: child
        type(assumption_context_t), intent(in)    :: parent

        child = parent
    end subroutine clone_assumption_context

end module fortsym_assume_api
