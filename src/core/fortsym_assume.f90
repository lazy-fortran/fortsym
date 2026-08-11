module fortsym_assume
    ! Explicit domain facts used by guarded algebraic rewrites.
    !
    ! Facts belong to one arena and compare interned expression identities.
    ! Sign facts are closed under their sound implications. Contradictory
    ! combinations are rejected before they reach a context.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM, NK_INT, NK_RAT
    use fortsym_cache, only: expr_cache_t, expr_pair_cache_t
    use fortsym_expr, only: expr_t, is_valid
    implicit none
    private

    public :: assumption_context_t, assumption_t
    public :: assume, zero, negative, nonpositive, positive, nonnegative, &
        nonzero, real_valued, rational_valued
    public :: integer_valued, positive_integer, algebraic_valued, record_relation
    public :: assumption_has
    public :: init_assumption_context, record_assumption, clone_assumption_context
    public :: make_assumption_context, with_assumption
    public :: FACT_REAL, FACT_ZERO, FACT_NEGATIVE, FACT_NONPOSITIVE, &
        FACT_POSITIVE, FACT_NONNEGATIVE, FACT_NONZERO, FACT_INTEGER, &
        FACT_POSITIVE_INTEGER, FACT_RATIONAL, FACT_ALGEBRAIC

    integer, parameter :: FACT_REAL = 1
    integer, parameter :: FACT_POSITIVE = 2
    integer, parameter :: FACT_NONNEGATIVE = 4
    integer, parameter :: FACT_NONZERO = 8
    integer, parameter :: FACT_INTEGER = 16
    integer, parameter :: FACT_POSITIVE_INTEGER = 32
    integer, parameter :: FACT_ZERO = 64
    integer, parameter :: FACT_NEGATIVE = 128
    integer, parameter :: FACT_NONPOSITIVE = 256
    integer, parameter :: FACT_RATIONAL = 512
    integer, parameter :: FACT_ALGEBRAIC = 1024
    integer, parameter :: FACT_ALL = 2047
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
        type(expr_pair_cache_t), allocatable :: complex_cache
        type(expr_cache_t), allocatable :: conjugate_cache
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
        character(:), allocatable :: why
        logical :: valid, assumption_ok

        call clone_assumption_context(child, parent)
        valid = associated(parent%home)
        if (valid) valid = is_valid(assumption%expression)
        if (valid) valid = associated(assumption%expression%a, parent%home)
        if (valid) then
            call assume(child, assumption, assumption_ok, why)
            valid = assumption_ok
        end if
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

    subroutine record_assumption(context, expression, facts, ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: expression
        integer,                     intent(in)    :: facts
        logical,                     intent(out), optional :: ok
        character(:), allocatable,   intent(out), optional :: why
        type(assumption_t) :: fact

        fact%expression = expression
        fact%facts = facts
        call assume(context, fact, ok, why)
    end subroutine record_assumption

    !> Record the bounded relation/domain fragment used by Assuming and
    !> Refine. The candidate context makes compound ingestion transactional:
    !> a refused or contradictory child never leaves a partial fact behind.
    subroutine record_relation(context, relation, ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: relation
        logical,                     intent(out)   :: ok
        character(:), allocatable,   intent(out)   :: why
        type(assumption_context_t) :: candidate

        call context_clone(candidate, context)
        call record_relation_impl(candidate, relation, ok, why)
        if (ok) call context_clone(context, candidate)
    end subroutine record_relation

    recursive subroutine record_relation_impl(context, relation, ok, why)
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
                call record_relation_impl(context, relation%arg(k), ok, why)
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
                call record_assumption(context, left, FACT_REAL, ok, why)
                if (.not. ok) return
                if (domain == "PositiveReals") then
                    call record_assumption(context, left, FACT_POSITIVE, ok, why)
                    if (.not. ok) return
                end if
            case ("Rationals")
                call record_assumption(context, left, FACT_RATIONAL, ok, why)
                if (.not. ok) return
            case ("Algebraics")
                call record_assumption(context, left, FACT_ALGEBRAIC, ok, why)
                if (.not. ok) return
            case ("Integers")
                call record_assumption(context, left, FACT_INTEGER, ok, why)
                if (.not. ok) return
            case ("PositiveIntegers")
                call record_assumption(context, left, FACT_POSITIVE_INTEGER, ok, why)
                if (.not. ok) return
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
    end subroutine record_relation_impl

    subroutine context_init(self, home)
        class(assumption_context_t), intent(inout) :: self
        type(arena_t), target,       intent(inout) :: home

        self%home => home
        if (allocated(self%ids)) deallocate (self%ids)
        if (allocated(self%facts)) deallocate (self%facts)
        allocate (self%ids(INITIAL_FACTS), source=0)
        allocate (self%facts(INITIAL_FACTS), source=0)
        if (.not. allocated(self%complex_cache)) allocate (self%complex_cache)
        call self%complex_cache%clear()
        if (.not. allocated(self%conjugate_cache)) allocate (self%conjugate_cache)
        call self%conjugate_cache%clear()
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
        if (.not. allocated(self%complex_cache)) allocate (self%complex_cache)
        call self%complex_cache%clear()
        if (.not. allocated(self%conjugate_cache)) allocate (self%conjugate_cache)
        call self%conjugate_cache%clear()
        self%n = parent%n
    end subroutine context_clone

    subroutine assume(context, assumption, ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(assumption_t),         intent(in)    :: assumption
        logical,                     intent(out), optional :: ok
        character(:), allocatable,   intent(out), optional :: why
        integer, allocatable :: larger(:)
        integer :: k, candidate_facts
        character(:), allocatable :: local_why
        logical :: accepted

        accepted = .false.
        local_why = ""
        if (.not. associated(context%home)) then
            local_why = "assumption context is not initialised"
        else if (.not. is_valid(assumption%expression)) then
            local_why = "assumption expression is invalid"
        else if (.not. associated(assumption%expression%a, context%home)) then
            local_why = "assumption belongs to a different arena"
        else if (iand(assumption%facts, not(FACT_ALL)) /= 0) then
            local_why = "unsupported assumption fact mask"
        else
            do k = 1, context%n
                if (context%ids(k) /= assumption%expression%id) cycle
                candidate_facts = implied(ior(context%facts(k), &
                    assumption%facts))
                if (.not. valid_facts(candidate_facts, local_why)) exit
                context%facts(k) = candidate_facts
                accepted = .true.
                exit
            end do

            if (.not. accepted .and. len(local_why) == 0) then
                candidate_facts = implied(assumption%facts)
                if (valid_facts(candidate_facts, local_why)) then
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
                    context%facts(context%n) = candidate_facts
                    accepted = .true.
                end if
            end if
        end if

        if (accepted .and. allocated(context%complex_cache)) then
            call context%complex_cache%clear()
        end if
        if (accepted .and. allocated(context%conjugate_cache)) then
            call context%conjugate_cache%clear()
        end if
        if (present(ok)) ok = accepted
        if (present(why)) why = local_why
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

    function zero(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_ZERO
    end function zero

    function negative(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_NEGATIVE
    end function negative

    function nonpositive(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_NONPOSITIVE
    end function nonpositive

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

    function rational_valued(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_RATIONAL
    end function rational_valued

    function algebraic_valued(expression) result(assumption)
        type(expr_t), intent(in) :: expression
        type(assumption_t)       :: assumption
        assumption%expression = expression
        assumption%facts = FACT_ALGEBRAIC
    end function algebraic_valued

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
        if (iand(facts, FACT_RATIONAL) /= 0) then
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(facts, FACT_INTEGER) /= 0) then
            all_facts = ior(all_facts, FACT_RATIONAL)
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(facts, FACT_POSITIVE_INTEGER) /= 0) then
            all_facts = ior(all_facts, FACT_INTEGER)
            all_facts = ior(all_facts, FACT_RATIONAL)
            all_facts = ior(all_facts, FACT_POSITIVE)
        end if
        if (iand(all_facts, FACT_RATIONAL) /= 0) then
            all_facts = ior(all_facts, FACT_ALGEBRAIC)
        end if
        if (iand(all_facts, FACT_ZERO) /= 0) then
            all_facts = ior(all_facts, FACT_NONNEGATIVE)
            all_facts = ior(all_facts, FACT_NONPOSITIVE)
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(all_facts, FACT_NEGATIVE) /= 0) then
            all_facts = ior(all_facts, FACT_NONPOSITIVE)
            all_facts = ior(all_facts, FACT_NONZERO)
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(all_facts, FACT_NONPOSITIVE) /= 0) then
            all_facts = ior(all_facts, FACT_REAL)
        end if
        if (iand(all_facts, FACT_NONNEGATIVE) /= 0 .and. &
            iand(all_facts, FACT_NONPOSITIVE) /= 0) then
            all_facts = ior(all_facts, FACT_ZERO)
        end if
    end function implied

    logical function valid_facts(facts, why)
        integer, intent(in) :: facts
        character(:), allocatable, intent(out) :: why

        valid_facts = .false.
        why = ""
        if (iand(facts, FACT_POSITIVE) /= 0 .and. &
            iand(facts, FACT_NONPOSITIVE) /= 0) then
            why = "contradictory assumptions: positive and nonpositive"
            return
        end if
        if (iand(facts, FACT_NEGATIVE) /= 0 .and. &
            iand(facts, FACT_NONNEGATIVE) /= 0) then
            why = "contradictory assumptions: negative and nonnegative"
            return
        end if
        if (iand(facts, FACT_ZERO) /= 0 .and. &
            iand(facts, FACT_NONZERO) /= 0) then
            why = "contradictory assumptions: zero and nonzero"
            return
        end if
        valid_facts = .true.
    end function valid_facts

    subroutine record_comparison(context, expression, name, n, d, exact_left, &
            ok, why)
        type(assumption_context_t), intent(inout) :: context
        type(expr_t),                intent(in)    :: expression
        character(*),                intent(in)    :: name
        integer(int64),              intent(in)    :: n, d
        logical,                     intent(in)    :: exact_left
        logical,                     intent(out)   :: ok
        character(:), allocatable,   intent(out)   :: why
        logical :: lower, upper, strict

        ok = .false.
        why = ""
        if (.not. valid_denominator(d)) then
            why = "assumption bound has a zero denominator"
            return
        end if
        if (name == "Equal") then
            if (n > 0_int64) then
                call record_assumption(context, expression, FACT_POSITIVE, ok, why)
            else if (n < 0_int64) then
                call record_assumption(context, expression, FACT_NEGATIVE, ok, why)
            else
                call record_assumption(context, expression, FACT_ZERO, ok, why)
            end if
            return
        end if
        if (name == "Unequal") then
            if (n == 0_int64) then
                call record_assumption(context, expression, FACT_NONZERO, ok, why)
            else
                why = "Unequal is supported only against exact zero"
            end if
            return
        end if

        lower = .false.
        upper = .false.
        strict = .false.
        select case (name)
        case ("Greater")
            if (exact_left) then
                upper = .true.
            else
                lower = .true.
            end if
            strict = .true.
        case ("GreaterEqual")
            if (exact_left) then
                upper = .true.
            else
                lower = .true.
            end if
        case ("Less")
            if (exact_left) then
                lower = .true.
            else
                upper = .true.
            end if
            strict = .true.
        case ("LessEqual")
            if (exact_left) then
                lower = .true.
            else
                upper = .true.
            end if
        case default
            why = "unsupported assumption relation "//name
            return
        end select

        if (lower) then
            if (n > 0_int64 .or. (n == 0_int64 .and. strict)) then
                call record_assumption(context, expression, FACT_POSITIVE, ok, why)
            else if (n == 0_int64) then
                call record_assumption(context, expression, FACT_NONNEGATIVE, ok, why)
            else
                why = "lower bound does not imply a supported sign fact"
                return
            end if
            return
        end if
        if (upper) then
            if (n < 0_int64 .or. (n == 0_int64 .and. strict)) then
                call record_assumption(context, expression, FACT_NEGATIVE, ok, why)
            else if (n == 0_int64) then
                call record_assumption(context, expression, FACT_NONPOSITIVE, ok, why)
            else
                why = "upper bound does not imply a supported sign fact"
                return
            end if
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
