module fortsym_assume
    ! Explicit domain facts used by guarded algebraic rewrites.
    !
    ! Facts belong to one arena and compare interned expression identities.
    ! Positive implies nonnegative, nonzero, and real. Nonnegative implies real.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    implicit none
    private

    public :: assumption_context_t, assumption_t
    public :: assume, positive, nonnegative, nonzero, real_valued
    public :: init_assumption_context, record_assumption
    public :: FACT_REAL, FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO

    integer, parameter :: FACT_REAL = 1
    integer, parameter :: FACT_POSITIVE = 2
    integer, parameter :: FACT_NONNEGATIVE = 4
    integer, parameter :: FACT_NONZERO = 8
    integer, parameter :: INITIAL_FACTS = 16

    type :: assumption_t
        type(expr_t) :: expression
        integer      :: facts = 0
    end type assumption_t

    type :: assumption_context_t
        type(arena_t), pointer :: home => null()
        integer, allocatable :: ids(:)
        integer, allocatable :: facts(:)
        integer :: n = 0
    contains
        procedure :: init => context_init
        procedure :: has => context_has
    end type assumption_context_t

contains

    subroutine init_assumption_context(context, home)
        type(assumption_context_t), intent(inout) :: context
        type(arena_t), target,       intent(inout) :: home

        call context_init(context, home)
    end subroutine init_assumption_context

    subroutine record_assumption(context, expression, facts)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: expression
        integer,                     intent(in)    :: facts
        type(assumption_t) :: fact

        fact%expression = expression
        fact%facts = facts
        call assume(context, fact)
    end subroutine record_assumption

    subroutine context_init(self, home)
        class(assumption_context_t), intent(inout) :: self
        type(arena_t), target,       intent(inout) :: home

        self%home => home
        if (allocated(self%ids)) deallocate (self%ids)
        if (allocated(self%facts)) deallocate (self%facts)
        allocate (self%ids(INITIAL_FACTS), source=0)
        allocate (self%facts(INITIAL_FACTS), source=0)
        self%n = 0
    end subroutine context_init

    subroutine assume(context, assumption)
        type(assumption_context_t), intent(inout) :: context
        type(assumption_t),         intent(in)    :: assumption
        integer, allocatable :: larger(:)
        integer :: k

        if (.not. associated(context%home)) return
        if (.not. associated(assumption%expression%a, context%home)) return

        do k = 1, context%n
            if (context%ids(k) /= assumption%expression%id) cycle
            context%facts(k) = ior(context%facts(k), implied(assumption%facts))
            return
        end do

        if (context%n >= size(context%ids)) then
            allocate (larger(2*size(context%ids)), source=0)
            larger(1:context%n) = context%ids(1:context%n)
            call move_alloc(larger, context%ids)
            allocate (larger(2*size(context%facts)), source=0)
            larger(1:context%n) = context%facts(1:context%n)
            call move_alloc(larger, context%facts)
        end if

        context%n = context%n + 1
        context%ids(context%n) = assumption%expression%id
        context%facts(context%n) = implied(assumption%facts)
    end subroutine assume

    function context_has(self, expression, fact) result(yes)
        class(assumption_context_t), intent(in) :: self
        type(expr_t),                intent(in) :: expression
        integer,                     intent(in) :: fact
        logical                                 :: yes
        integer :: k

        yes = .false.
        if (.not. associated(self%home)) return
        if (.not. associated(expression%a, self%home)) return
        do k = 1, self%n
            if (self%ids(k) /= expression%id) cycle
            yes = iand(self%facts(k), fact) == fact
            return
        end do
    end function context_has

    function positive(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_POSITIVE
    end function positive

    function nonnegative(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_NONNEGATIVE
    end function nonnegative

    function nonzero(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_NONZERO
    end function nonzero

    function real_valued(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_REAL
    end function real_valued

    function implied(facts) result(all_facts)
        integer, intent(in) :: facts
        integer             :: all_facts

        all_facts = facts
        if (iand(facts, FACT_POSITIVE) /= 0) then
            all_facts = ior(all_facts, FACT_NONNEGATIVE)
            all_facts = ior(all_facts, FACT_NONZERO)
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(facts, FACT_NONNEGATIVE) /= 0) then
            all_facts = ior(all_facts, FACT_REAL)
        end if
    end function implied

end module fortsym_assume
