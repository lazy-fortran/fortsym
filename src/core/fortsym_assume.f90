module fortsym_assume
    ! Explicit domain facts used by guarded algebraic rewrites.
    !
    ! Facts belong to one arena and compare interned expression identities.
    ! Positive implies nonnegative, nonzero, and real. Nonnegative implies real.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM, NK_INT, NK_RAT
    use fortsym_expr, only: expr_t, is_valid
    implicit none
    private

    public :: assumption_context_t, assumption_t
    public :: assume, positive, nonnegative, nonzero, real_valued
    public :: integer_valued, positive_integer, record_relation
    public :: assumption_has
    public :: init_assumption_context, record_assumption, clone_assumption_context
    public :: make_assumption_context, with_assumption
    public :: FACT_REAL, FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO, &
        FACT_INTEGER, FACT_POSITIVE_INTEGER

    integer, parameter :: FACT_REAL = 1
    integer, parameter :: FACT_POSITIVE = 2
    integer, parameter :: FACT_NONNEGATIVE = 4
    integer, parameter :: FACT_NONZERO = 8
    integer, parameter :: FACT_INTEGER = 16
    integer, parameter :: FACT_POSITIVE_INTEGER = 32
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
        procedure :: clone => context_clone
    end type assumption_context_t

    interface with_assumption
        module procedure with_fact_assumption
        module procedure with_relation_assumption
    end interface with_assumption

contains

    subroutine init_assumption_context(context, home)
        type(assumption_context_t), intent(inout) :: context
        type(arena_t), target,       intent(inout) :: home

        call context_init(context, home)
    end subroutine init_assumption_context

    subroutine clone_assumption_context(child, parent)
        type(assumption_context_t), intent(inout) :: child
        type(assumption_context_t), intent(in)    :: parent

        call context_clone(child, parent)
    end subroutine clone_assumption_context

    function make_assumption_context(home) result(context)
        type(arena_t), target, intent(inout) :: home
        type(assumption_context_t) :: context

        call init_assumption_context(context, home)
    end function make_assumption_context

    function with_fact_assumption(parent, assumption, ok) result(child)
        type(assumption_context_t), intent(in) :: parent
        type(assumption_t), intent(in) :: assumption
        logical, intent(out), optional :: ok
        type(assumption_context_t) :: child
        logical :: valid

        call clone_assumption_context(child, parent)
        valid = associated(parent%home)
        if (valid) valid = is_valid(assumption%expression)
        if (valid) valid = associated(assumption%expression%a, parent%home)
        if (valid) call assume(child, assumption)
        if (present(ok)) ok = valid
    end function with_fact_assumption

    function with_relation_assumption(parent, relation, ok) result(child)
        type(assumption_context_t), intent(in) :: parent
        type(expr_t), intent(in) :: relation
        logical, intent(out), optional :: ok
        type(assumption_context_t) :: child
        character(:), allocatable :: why
        logical :: valid, relation_ok

        call clone_assumption_context(child, parent)
        valid = associated(parent%home)
        if (valid) valid = is_valid(relation)
        if (valid) valid = associated(relation%a, parent%home)
        if (valid) then
            call record_relation(child, relation, relation_ok, why)
            valid = relation_ok
        end if
        if (present(ok)) ok = valid
    end function with_relation_assumption

    subroutine record_assumption(context, expression, facts)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: expression
        integer,                     intent(in)    :: facts
        type(assumption_t) :: fact

        fact%expression = expression
        fact%facts = facts
        call assume(context, fact)
    end subroutine record_assumption

    !> Record the bounded relation/domain fragment used by Assuming and
    !> Refine. Unsupported relations are refused by the caller; they are not
    !> treated as absent facts.
    recursive subroutine record_relation(context, relation, ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: relation
        logical,                     intent(out)   :: ok
        character(:), allocatable,   intent(out)   :: why
        character(:), allocatable :: name, domain
        type(expr_t) :: left, right
        integer(int64) :: n, d
        logical :: exact
        integer :: k

        ok = .false.
        why = ""
        if (.not. associated(context%home)) then
            why = "assumption context is not initialised"
            return
        end if
        if (.not. associated(relation%a, context%home)) then
            why = "assumption relation belongs to a different arena"
            return
        end if
        if (relation%kind() /= NK_FUNC) then
            why = "assumption must be a relation or domain membership"
            return
        end if
        name = chars(relation%name())

        if (name == "And") then
            if (relation%nargs() == 0) then
                why = "an empty conjunction is not an assumption"
                return
            end if
            ok = .true.
            do k = 1, relation%nargs()
                call record_relation(context, relation%arg(k), ok, why)
                if (.not. ok) return
            end do
            return
        end if

        if (name == "Element") then
            if (relation%nargs() /= 2) then
                why = "Element needs an expression and a named domain"
                return
            end if
            left = relation%arg(1)
            right = relation%arg(2)
            if (right%kind() /= NK_SYM) then
                why = "Element needs an expression and a named domain"
                return
            end if
            domain = chars(right%name())
            select case (domain)
            case ("Reals", "PositiveReals")
                call record_assumption(context, left, FACT_REAL)
                if (domain == "PositiveReals") then
                    call record_assumption(context, left, FACT_POSITIVE)
                end if
            case ("Integers")
                call record_assumption(context, left, FACT_INTEGER)
            case ("PositiveIntegers")
                call record_assumption(context, left, FACT_POSITIVE_INTEGER)
            case default
                why = "unsupported Element domain "//domain
                return
            end select
            ok = .true.
            return
        end if

        if (relation%nargs() /= 2) then
            why = "assumption relation needs two operands"
            return
        end if
        left = relation%arg(1)
        right = relation%arg(2)
        call exact_value(left, n, d, exact)
        if (exact) then
            call record_comparison(context, right, name, n, d, .true., ok, why)
            return
        end if
        call exact_value(right, n, d, exact)
        if (exact) then
            call record_comparison(context, left, name, n, d, .false., ok, why)
            return
        end if

        why = "relation needs an exact constant bound in this fragment"
    end subroutine record_relation

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

    subroutine context_clone(self, parent)
        class(assumption_context_t), intent(inout) :: self
        type(assumption_context_t),   intent(in)    :: parent

        self%home => parent%home
        if (allocated(self%ids)) deallocate (self%ids)
        if (allocated(self%facts)) deallocate (self%facts)
        if (allocated(parent%ids)) then
            allocate (self%ids, source=parent%ids)
        else
            allocate (self%ids(INITIAL_FACTS), source=0)
        end if
        if (allocated(parent%facts)) then
            allocate (self%facts, source=parent%facts)
        else
            allocate (self%facts(INITIAL_FACTS), source=0)
        end if
        self%n = parent%n
    end subroutine context_clone

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

    subroutine assumption_has(context, expression, fact, known)
        type(assumption_context_t), intent(in) :: context
        type(expr_t),                intent(in) :: expression
        integer,                     intent(in) :: fact
        logical,                     intent(out) :: known

        known = context_has(context, expression, fact)
    end subroutine assumption_has

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

    function integer_valued(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_INTEGER
    end function integer_valued

    function positive_integer(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_POSITIVE_INTEGER
    end function positive_integer

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
        if (iand(facts, FACT_NONZERO) /= 0) then
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(facts, FACT_INTEGER) /= 0) then
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(facts, FACT_POSITIVE_INTEGER) /= 0) then
            all_facts = ior(all_facts, FACT_INTEGER)
            all_facts = ior(all_facts, FACT_POSITIVE)
        end if
    end function implied

    subroutine record_comparison(context, expression, name, n, d, exact_left, &
            ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: expression
        character(*),                intent(in)    :: name
        integer(int64),              intent(in)    :: n, d
        logical,                     intent(in)    :: exact_left
        logical,                     intent(out)   :: ok
        character(:), allocatable,   intent(out)   :: why
        logical :: lower, strict

        ok = .false.
        why = ""
        if (.not. valid_denominator(d)) then
            why = "assumption bound has a zero denominator"
            return
        end if
        if (name == "Unequal") then
            if (n == 0_int64 .and. .not. exact_left) then
                call record_assumption(context, expression, FACT_NONZERO)
                ok = .true.
            else if (exact_left .and. n == 0_int64) then
                call record_assumption(context, expression, FACT_NONZERO)
                ok = .true.
            else
                why = "Unequal is supported only against exact zero"
            end if
            return
        end if

        lower = .false.
        strict = .false.
        if (.not. exact_left) then
            select case (name)
            case ("Greater")
                lower = .true.; strict = .true.
            case ("GreaterEqual")
                lower = .true.
            case ("Less", "LessEqual")
                ! Upper bounds are retained as a named, sound refusal until
                ! a consumer asks for a negative/nonpositive fact.
                why = "upper-bound inference is not needed by this fragment"
                return
            case default
                why = "unsupported assumption relation "//name
                return
            end select
        else
            select case (name)
            case ("Less")
                lower = .true.; strict = .true.
            case ("LessEqual")
                lower = .true.
            case ("Greater", "GreaterEqual")
                why = "upper-bound inference is not needed by this fragment"
                return
            case default
                why = "unsupported assumption relation "//name
                return
            end select
        end if

        if (lower) then
            if (n > 0_int64 .or. (n == 0_int64 .and. strict)) then
                call record_assumption(context, expression, FACT_POSITIVE)
            else if (n == 0_int64) then
                call record_assumption(context, expression, FACT_NONNEGATIVE)
            else
                why = "lower bound does not imply a supported sign fact"
                return
            end if
            ok = .true.
        end if
    end subroutine record_comparison

    subroutine exact_value(expression, n, d, ok)
        type(expr_t),   intent(in)  :: expression
        integer(int64), intent(out) :: n, d
        logical,        intent(out) :: ok

        n = 0_int64
        d = 1_int64
        ok = .false.
        select case (expression%kind())
        case (NK_INT)
            n = expression%int_value()
            ok = .true.
        case (NK_RAT)
            n = expression%int_value()
            d = expression%den_value()
            ok = valid_denominator(d)
        end select
    end subroutine exact_value

    pure function valid_denominator(d) result(ok)
        integer(int64), intent(in) :: d
        logical :: ok
        ok = d > 0_int64
    end function valid_denominator

end module fortsym_assume
