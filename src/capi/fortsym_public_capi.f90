module fortsym_public_capi
    ! The public C ABI.  The older fortsym_capi module is an internal binding to
    ! SymEngine; this module is the ownership boundary for fortsym's own arena.
    use, intrinsic :: iso_c_binding, only: c_associated, c_char, c_double, &
        c_int, c_int64_t, c_null_char, c_null_ptr, c_ptr, c_size_t, c_loc, &
        c_f_pointer
    use fortsym_string, only: str_t
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, const, &
        func, func_in, is_valid, same_arena, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_print, only: print_expr
    use fortsym_subs, only: subs
    use fortsym_diff, only: diff
    use fortsym_assume_api, only: assumption_context_t, init_assumption_context, &
        clone_assumption_context, record_assumption, &
        record_relation, &
        assumption_has, &
        FACT_REAL, FACT_ZERO, FACT_NEGATIVE, FACT_NONPOSITIVE, FACT_POSITIVE, &
        FACT_NONNEGATIVE, FACT_NONZERO
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE
    implicit none
    private

    integer(c_int), parameter, public :: FORTSYM_OK = 0_c_int
    integer(c_int), parameter, public :: FORTSYM_INVALID_ARGUMENT = 1_c_int
    integer(c_int), parameter, public :: FORTSYM_INVALID_HANDLE = 2_c_int
    integer(c_int), parameter, public :: FORTSYM_FOREIGN_ARENA = 3_c_int
    integer(c_int), parameter, public :: FORTSYM_PARSE_ERROR = 4_c_int
    integer(c_int), parameter, public :: FORTSYM_UNSUPPORTED = 5_c_int
    integer(c_int), parameter, public :: FORTSYM_RESOURCE_LIMIT = 6_c_int
    integer(c_int), parameter, public :: FORTSYM_CONFLICT = 7_c_int

    type :: assumption_frame_t
        type(assumption_context_t), pointer :: context => null()
        type(assumption_frame_t), pointer :: previous => null()
    end type assumption_frame_t

    type :: arena_owner_t
        type(arena_t) :: value
        type(assumption_context_t), pointer :: assumptions => null()
        type(assumption_frame_t), pointer :: assumption_stack => null()
        type(native_engine_t) :: engine
        integer       :: references = 1
    end type arena_owner_t

    type :: expr_owner_t
        type(arena_owner_t), pointer :: arena => null()
        integer                       :: id = 0
    end type expr_owner_t

    public :: fortsym_abi_version, fortsym_arena_new, fortsym_arena_free
    public :: fortsym_int, fortsym_rational, fortsym_real, fortsym_exact
    public :: fortsym_symbol, c_fortsym_assume, fortsym_assume_relation, &
        fortsym_assumption_push, &
        fortsym_assumption_pop, fortsym_assumption_has, &
        fortsym_constant
    public :: fortsym_add, fortsym_subtract, fortsym_multiply, fortsym_divide
    public :: fortsym_power, fortsym_add_many, fortsym_function, fortsym_relation
    public :: fortsym_substitute, fortsym_differentiate, fortsym_expr_free
    public :: fortsym_expand, fortsym_simplify, fortsym_factor
    public :: fortsym_zero_test
    public :: fortsym_expr_kind, fortsym_expr_arity, fortsym_expr_argument
    public :: fortsym_expr_equal, fortsym_expr_node_count, fortsym_expr_text
    public :: fortsym_expr_name, fortsym_expr_exact_text
    public :: fortsym_expr_int_value, fortsym_expr_real_value

contains

    function fortsym_abi_version() bind(c, name="fortsym_abi_version") result(v)
        integer(c_int) :: v
        v = 7_c_int
    end function fortsym_abi_version

    function fortsym_arena_new(out, message, capacity) &
            bind(c, name="fortsym_arena_new") result(status)
        type(c_ptr), value :: out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a

        ! `out` is a pointer-to-pointer in C and therefore arrives as a C
        ! address, not as a Fortran VALUE result.  The helper writes it below.
        status = FORTSYM_INVALID_ARGUMENT
        call put_error(message, capacity, FORTSYM_INVALID_ARGUMENT)
        if (.not. c_associated(out)) then
            call put_error(message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        allocate (a)
        call a%value%init()
        allocate (a%assumptions)
        call init_assumption_context(a%assumptions, a%value)
        a%engine = make_native_engine(a%value)
        call c_store_pointer(out, c_loc(a))
        status = FORTSYM_OK
    end function fortsym_arena_new

    subroutine fortsym_arena_free(raw) bind(c, name="fortsym_arena_free")
        type(c_ptr), value :: raw
        type(arena_owner_t), pointer :: a

        if (.not. c_associated(raw)) return
        call c_f_pointer(raw, a)
        if (.not. associated(a)) return
        call release_arena(a)
    end subroutine fortsym_arena_free

    function fortsym_int(raw, value, out, message, capacity) &
            bind(c, name="fortsym_int") result(status)
        type(c_ptr), value :: raw, out
        integer(c_int64_t), value :: value
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_t) :: e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = num(a%value, int(value, kind=8))
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_int

    function fortsym_rational(raw, numerator, denominator, out, message, capacity) &
            bind(c, name="fortsym_rational") result(status)
        type(c_ptr), value :: raw, out
        integer(c_int64_t), value :: numerator, denominator
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_t) :: e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (denominator == 0_c_int64_t) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        e = rat(a%value, int(numerator, kind=8), int(denominator, kind=8))
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_rational

    function fortsym_real(raw, value, out, message, capacity) &
            bind(c, name="fortsym_real") result(status)
        type(c_ptr), value :: raw, out
        real(c_double), value :: value
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_t) :: e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = real_expr(a%value, value)
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_real

    function fortsym_exact(raw, value, out, message, capacity) &
            bind(c, name="fortsym_exact") result(status)
        type(c_ptr), value :: raw, out
        character(kind=c_char), intent(in) :: value(*)
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_t) :: e
        character(:), allocatable :: text
        logical :: ok

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        text = from_c_string(value)
        e = exact(a%value, text, ok)
        if (.not. ok) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_exact

    function fortsym_symbol(raw, name, out, message, capacity) &
            bind(c, name="fortsym_symbol") result(status)
        type(c_ptr), value :: raw, out
        character(kind=c_char), intent(in) :: name(*)
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_t) :: e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (len(from_c_string(name)) == 0) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        e = sym(a%value, from_c_string(name))
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_symbol

    function c_fortsym_assume(raw, expression_raw, fact, message, capacity) &
            bind(c, name="fortsym_assume") result(status)
        type(c_ptr), value :: raw, expression_raw
        integer(c_int), value :: fact
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, ep_arena
        type(expr_owner_t), pointer :: ep
        type(expr_t) :: expression
        character(:), allocatable :: why
        logical :: fact_ok

        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        ep_arena => ep%arena
        if (.not. associated(ep_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        if (fact /= FACT_REAL .and. fact /= FACT_ZERO .and. &
            fact /= FACT_NEGATIVE .and. fact /= FACT_NONPOSITIVE .and. &
            fact /= FACT_POSITIVE .and. fact /= FACT_NONNEGATIVE .and. &
            fact /= FACT_NONZERO) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        call record_assumption(a%assumptions, expression, int(fact), &
            fact_ok, why)
        if (.not. fact_ok) then
            call fail_reason(status, message, capacity, FORTSYM_CONFLICT, why)
            return
        end if
        call put_error(message, capacity, FORTSYM_OK)
        status = FORTSYM_OK
    end function c_fortsym_assume

    function fortsym_assume_relation(raw, relation_raw, message, capacity) &
            bind(c, name="fortsym_assume_relation") result(status)
        type(c_ptr), value :: raw, relation_raw
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, relation_arena
        type(expr_owner_t), pointer :: relation_owner
        type(expr_t) :: relation
        character(:), allocatable :: why
        logical :: ok

        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(relation_raw, relation_owner, relation, status, &
            message, capacity)
        if (status /= FORTSYM_OK) return
        relation_arena => relation_owner%arena
        if (.not. associated(relation_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        call record_relation(a%assumptions, relation, ok, why)
        if (.not. ok) then
            if (index(why, "contradictory assumptions:") == 1) then
                call fail_reason(status, message, capacity, FORTSYM_CONFLICT, why)
            else
                call fail_reason(status, message, capacity, FORTSYM_UNSUPPORTED, why)
            end if
            return
        end if
        call put_error(message, capacity, FORTSYM_OK)
        status = FORTSYM_OK
    end function fortsym_assume_relation

    function fortsym_assumption_push(raw, message, capacity) &
            bind(c, name="fortsym_assumption_push") result(status)
        type(c_ptr), value :: raw
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(assumption_frame_t), pointer :: frame

        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return

        allocate (frame)
        frame%context => a%assumptions
        frame%previous => a%assumption_stack
        allocate (a%assumptions)
        call clone_assumption_context(a%assumptions, frame%context)
        a%assumption_stack => frame
        call put_error(message, capacity, FORTSYM_OK)
        status = FORTSYM_OK
    end function fortsym_assumption_push

    function fortsym_assumption_pop(raw, message, capacity) &
            bind(c, name="fortsym_assumption_pop") result(status)
        type(c_ptr), value :: raw
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(assumption_frame_t), pointer :: frame
        type(assumption_context_t), pointer :: current

        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (.not. associated(a%assumption_stack)) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if

        frame => a%assumption_stack
        current => a%assumptions
        if (associated(current)) deallocate (current)
        a%assumptions => frame%context
        a%assumption_stack => frame%previous
        nullify (frame%context, frame%previous)
        deallocate (frame)
        call put_error(message, capacity, FORTSYM_OK)
        status = FORTSYM_OK
    end function fortsym_assumption_pop

    function fortsym_assumption_has(raw, expression_raw, fact, known, &
            message, capacity) bind(c, name="fortsym_assumption_has") &
            result(status)
        type(c_ptr), value :: raw, expression_raw
        integer(c_int), value :: fact
        integer(c_int), intent(out) :: known
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, ep_arena
        type(expr_owner_t), pointer :: ep
        type(expr_t) :: expression
        logical :: fact_known

        known = 0_c_int
        call put_error(message, capacity, FORTSYM_OK)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        ep_arena => ep%arena
        if (.not. associated(ep_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        if (fact /= FACT_REAL .and. fact /= FACT_ZERO .and. &
            fact /= FACT_NEGATIVE .and. fact /= FACT_NONPOSITIVE .and. &
            fact /= FACT_POSITIVE .and. fact /= FACT_NONNEGATIVE .and. &
            fact /= FACT_NONZERO) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        call assumption_has(a%assumptions, expression, int(fact), fact_known)
        if (fact_known) known = 1_c_int
        status = FORTSYM_OK
    end function fortsym_assumption_has

    function fortsym_constant(raw, name, out, message, capacity) &
            bind(c, name="fortsym_constant") result(status)
        type(c_ptr), value :: raw, out
        character(kind=c_char), intent(in) :: name(*)
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_t) :: e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (len(from_c_string(name)) == 0) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        e = const(a%value, from_c_string(name))
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_constant

    function fortsym_add(raw, left_raw, right_raw, out, message, capacity) &
            bind(c, name="fortsym_add") result(status)
        type(c_ptr), value :: raw, left_raw, right_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t) :: left, right, e

        call begin_output(out, message, capacity)
        call get_binary(raw, left_raw, right_raw, a, lp, rp, left, right, &
            status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = left + right
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_add

    function fortsym_subtract(raw, left_raw, right_raw, out, message, capacity) &
            bind(c, name="fortsym_subtract") result(status)
        type(c_ptr), value :: raw, left_raw, right_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t) :: left, right, e

        call begin_output(out, message, capacity)
        call get_binary(raw, left_raw, right_raw, a, lp, rp, left, right, &
            status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = left - right
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_subtract

    function fortsym_multiply(raw, left_raw, right_raw, out, message, capacity) &
            bind(c, name="fortsym_multiply") result(status)
        type(c_ptr), value :: raw, left_raw, right_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t) :: left, right, e

        call begin_output(out, message, capacity)
        call get_binary(raw, left_raw, right_raw, a, lp, rp, left, right, &
            status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = left*right
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_multiply

    function fortsym_divide(raw, left_raw, right_raw, out, message, capacity) &
            bind(c, name="fortsym_divide") result(status)
        type(c_ptr), value :: raw, left_raw, right_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t) :: left, right, e

        call begin_output(out, message, capacity)
        call get_binary(raw, left_raw, right_raw, a, lp, rp, left, right, &
            status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = left/right
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_divide

    function fortsym_power(raw, base_raw, exponent_raw, out, message, capacity) &
            bind(c, name="fortsym_power") result(status)
        type(c_ptr), value :: raw, base_raw, exponent_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: bp, ep
        type(expr_t) :: base, exponent, e

        call begin_output(out, message, capacity)
        call get_binary(raw, base_raw, exponent_raw, a, bp, ep, base, exponent, &
            status, message, capacity)
        if (status /= FORTSYM_OK) return
        e = base**exponent
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_power

    function fortsym_add_many(raw, args, count, out, message, capacity) &
            bind(c, name="fortsym_add_many") result(status)
        type(c_ptr), value :: raw, out
        type(c_ptr), intent(in) :: args(*)
        integer(c_size_t), value :: count, capacity
        character(kind=c_char), intent(out) :: message(*)
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: p
        type(expr_t), allocatable :: values(:)
        type(expr_t) :: e
        integer :: k, n

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (count == 0_c_size_t .or. count > int(huge(0), c_size_t)) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        n = int(count)
        allocate (values(n))
        do k = 1, n
            call get_expr(args(k), p, values(k), status, message, capacity)
            if (status /= FORTSYM_OK) return
            if (.not. associated(p%arena, a)) then
                call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
                return
            end if
        end do
        e = values(1)
        do k = 2, n
            e = e + values(k)
        end do
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_add_many

    function fortsym_function(raw, name, args, count, out, message, capacity) &
            bind(c, name="fortsym_function") result(status)
        type(c_ptr), value :: raw, out
        character(kind=c_char), intent(in) :: name(*)
        type(c_ptr), intent(in) :: args(*)
        integer(c_size_t), value :: count, capacity
        character(kind=c_char), intent(out) :: message(*)
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: p
        type(expr_t), allocatable :: values(:)
        type(expr_t) :: e
        character(:), allocatable :: head
        integer :: k, n

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        head = from_c_string(name)
        if (len(head) == 0) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        if (count > int(huge(0), c_size_t)) then
            call fail(status, message, capacity, FORTSYM_RESOURCE_LIMIT)
            return
        end if
        n = int(count)
        allocate (values(n))
        do k = 1, n
            call get_expr(args(k), p, values(k), status, message, capacity)
            if (status /= FORTSYM_OK) return
            if (.not. associated(p%arena, a)) then
                call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
                return
            end if
        end do
        if (n == 0) then
            e = func_in(a%value, head)
        else
            e = func(head, values)
        end if
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_function

    function fortsym_relation(raw, left_raw, right_raw, relation_kind, out, &
            message, capacity) bind(c, name="fortsym_relation") result(status)
        type(c_ptr), value :: raw, left_raw, right_raw, out
        integer(c_int), value :: relation_kind
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t) :: left, right, relation
        character(:), allocatable :: name
        integer :: ids(2)

        call begin_output(out, message, capacity)
        call get_binary(raw, left_raw, right_raw, a, lp, rp, left, right, &
            status, message, capacity)
        if (status /= FORTSYM_OK) return
        select case (relation_kind)
        case (1_c_int)
            name = "Equal"
        case (2_c_int)
            name = "Unequal"
        case (3_c_int)
            name = "Less"
        case (4_c_int)
            name = "LessEqual"
        case (5_c_int)
            name = "Greater"
        case (6_c_int)
            name = "GreaterEqual"
        case default
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end select
        ids(1) = left%id
        ids(2) = right%id
        relation%a => a%value
        relation%id = a%value%func(name, ids)
        relation%generation = left%generation
        call make_handle(a, relation, out, status, message, capacity)
    end function fortsym_relation

    function fortsym_substitute(raw, expression_raw, old_raw, new_raw, out, &
            message, capacity) bind(c, name="fortsym_substitute") result(status)
        type(c_ptr), value :: raw, expression_raw, old_raw, new_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: ep, op, np
        type(expr_t) :: expression, old_expression, new_expression, e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(old_raw, op, old_expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(new_raw, np, new_expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (.not. associated(ep%arena, a) .or. &
            .not. associated(op%arena, a) .or. .not. associated(np%arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        e = subs(expression, old_expression, new_expression)
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_substitute

    function fortsym_differentiate(raw, expression_raw, variable_raw, out, &
            message, capacity) bind(c, name="fortsym_differentiate") result(status)
        type(c_ptr), value :: raw, expression_raw, variable_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: ep, vp
        type(expr_t) :: expression, variable, e

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(variable_raw, vp, variable, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (.not. associated(ep%arena, a) .or. .not. associated(vp%arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        e = diff(expression, variable)
        call make_handle(a, e, out, status, message, capacity)
    end function fortsym_differentiate

    function fortsym_expand(raw, expression_raw, out, message, capacity) &
            bind(c, name="fortsym_expand") result(status)
        type(c_ptr), value :: raw, expression_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, ep_arena
        type(expr_owner_t), pointer :: ep
        type(expr_t) :: expression
        type(engine_result_t) :: result

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        ep_arena => ep%arena
        if (.not. associated(ep_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        call prepare_native_engine(a)
        result = a%engine%expand(expression)
        if (.not. result%ok) then
            call fail(status, message, capacity, FORTSYM_UNSUPPORTED)
            return
        end if
        call make_handle(a, result%value, out, status, message, capacity)
    end function fortsym_expand

    function fortsym_simplify(raw, expression_raw, out, message, capacity) &
            bind(c, name="fortsym_simplify") result(status)
        type(c_ptr), value :: raw, expression_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, ep_arena
        type(expr_owner_t), pointer :: ep
        type(expr_t) :: expression
        type(engine_result_t) :: result

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        ep_arena => ep%arena
        if (.not. associated(ep_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        call prepare_native_engine(a)
        result = a%engine%simplify(expression)
        if (.not. result%ok) then
            call fail(status, message, capacity, FORTSYM_UNSUPPORTED)
            return
        end if
        call make_handle(a, result%value, out, status, message, capacity)
    end function fortsym_simplify

    function fortsym_factor(raw, expression_raw, out, message, capacity) &
            bind(c, name="fortsym_factor") result(status)
        type(c_ptr), value :: raw, expression_raw, out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, ep_arena
        type(expr_owner_t), pointer :: ep
        type(expr_t) :: expression
        type(engine_result_t) :: result

        call begin_output(out, message, capacity)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        ep_arena => ep%arena
        if (.not. associated(ep_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        call prepare_native_engine(a)
        result = a%engine%factor(expression)
        if (.not. result%ok .or. result%conditional) then
            call fail(status, message, capacity, FORTSYM_UNSUPPORTED)
            return
        end if
        call make_handle(a, result%value, out, status, message, capacity)
    end function fortsym_factor

    function fortsym_zero_test(raw, expression_raw, verdict, message, capacity) &
            bind(c, name="fortsym_zero_test") result(status)
        type(c_ptr), value :: raw, expression_raw
        integer(c_int), intent(out) :: verdict
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(arena_owner_t), pointer :: a, ep_arena
        type(expr_owner_t), pointer :: ep
        type(expr_t) :: expression
        type(engine_result_t) :: result

        verdict = int(VERDICT_UNKNOWN, c_int)
        call put_error(message, capacity, FORTSYM_OK)
        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(expression_raw, ep, expression, status, message, capacity)
        if (status /= FORTSYM_OK) return
        ep_arena => ep%arena
        if (.not. associated(ep_arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        call prepare_native_engine(a)
        result = a%engine%zero_test(expression)
        if (.not. result%ok) then
            call fail(status, message, capacity, FORTSYM_UNSUPPORTED)
            return
        end if
        select case (result%verdict)
        case (VERDICT_TRUE)
            verdict = int(VERDICT_TRUE, c_int)
        case (VERDICT_FALSE)
            verdict = int(VERDICT_FALSE, c_int)
        case default
            verdict = int(VERDICT_UNKNOWN, c_int)
        end select
        status = FORTSYM_OK
    end function fortsym_zero_test

    subroutine fortsym_expr_free(raw) bind(c, name="fortsym_expr_free")
        type(c_ptr), value :: raw
        type(expr_owner_t), pointer :: e
        type(arena_owner_t), pointer :: a

        if (.not. c_associated(raw)) return
        call c_f_pointer(raw, e)
        if (.not. associated(e)) return
        a => e%arena
        deallocate (e)
        if (associated(a)) call release_arena(a)
    end subroutine fortsym_expr_free

    function fortsym_expr_kind(raw, kind, message, capacity) &
            bind(c, name="fortsym_expr_kind") result(status)
        type(c_ptr), value :: raw
        integer(c_int), intent(out) :: kind
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        kind = 0_c_int
        call get_expr(raw, p, e, status, message, capacity)
        if (status == FORTSYM_OK) kind = int(e%kind(), c_int)
    end function fortsym_expr_kind

    function fortsym_expr_arity(raw, arity, message, capacity) &
            bind(c, name="fortsym_expr_arity") result(status)
        type(c_ptr), value :: raw
        integer(c_size_t), intent(out) :: arity
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        arity = 0_c_size_t
        call get_expr(raw, p, e, status, message, capacity)
        if (status == FORTSYM_OK) arity = int(e%nargs(), c_size_t)
    end function fortsym_expr_arity

    function fortsym_expr_argument(raw, index, out, message, capacity) &
            bind(c, name="fortsym_expr_argument") result(status)
        type(c_ptr), value :: raw, out
        integer(c_size_t), value :: index
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        call begin_output(out, message, capacity)
        call get_expr(raw, p, e, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (index >= int(e%nargs(), c_size_t)) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        call make_handle(p%arena, e%arg(int(index) + 1), out, status, message, capacity)
    end function fortsym_expr_argument

    function fortsym_expr_equal(left_raw, right_raw, equal, message, capacity) &
            bind(c, name="fortsym_expr_equal") result(status)
        type(c_ptr), value :: left_raw, right_raw
        integer(c_int), intent(out) :: equal
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t) :: left, right

        equal = 0_c_int
        call get_expr(left_raw, lp, left, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(right_raw, rp, right, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (.not. associated(lp%arena, rp%arena)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
            return
        end if
        equal = merge(1_c_int, 0_c_int, left%id == right%id)
    end function fortsym_expr_equal

    function fortsym_expr_node_count(raw, count, message, capacity) &
            bind(c, name="fortsym_expr_node_count") result(status)
        type(c_ptr), value :: raw
        integer(c_size_t), intent(out) :: count
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        count = 0_c_size_t
        call get_expr(raw, p, e, status, message, capacity)
        if (status == FORTSYM_OK) count = int(e%node_count(), c_size_t)
    end function fortsym_expr_node_count

    function fortsym_expr_text(raw, buffer, capacity, required, message, &
            message_capacity) bind(c, name="fortsym_expr_text") result(status)
        type(c_ptr), value :: raw
        character(kind=c_char), intent(out) :: buffer(*)
        integer(c_size_t), value :: capacity
        integer(c_size_t), intent(out) :: required
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: message_capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        required = 0_c_size_t
        call get_expr(raw, p, e, status, message, message_capacity)
        if (status /= FORTSYM_OK) return
        call put_text(print_expr(e), buffer, capacity, required, status, &
            message, message_capacity)
    end function fortsym_expr_text

    function fortsym_expr_name(raw, buffer, capacity, required, message, &
            message_capacity) bind(c, name="fortsym_expr_name") result(status)
        type(c_ptr), value :: raw
        character(kind=c_char), intent(out) :: buffer(*)
        integer(c_size_t), value :: capacity
        integer(c_size_t), intent(out) :: required
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: message_capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        required = 0_c_size_t
        call get_expr(raw, p, e, status, message, message_capacity)
        if (status /= FORTSYM_OK) return
        call put_text(e%name(), buffer, capacity, required, status, &
            message, message_capacity)
    end function fortsym_expr_name

    function fortsym_expr_exact_text(raw, buffer, capacity, required, message, &
            message_capacity) bind(c, name="fortsym_expr_exact_text") result(status)
        type(c_ptr), value :: raw
        character(kind=c_char), intent(out) :: buffer(*)
        integer(c_size_t), value :: capacity
        integer(c_size_t), intent(out) :: required
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: message_capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        required = 0_c_size_t
        call get_expr(raw, p, e, status, message, message_capacity)
        if (status /= FORTSYM_OK) return
        call put_text(e%exact_text(), buffer, capacity, required, status, &
            message, message_capacity)
    end function fortsym_expr_exact_text

    function fortsym_expr_int_value(raw, value, message, capacity) &
            bind(c, name="fortsym_expr_int_value") result(status)
        type(c_ptr), value :: raw
        integer(c_int64_t), intent(out) :: value
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        value = 0_c_int64_t
        call get_expr(raw, p, e, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (e%kind() /= NK_INT .and. e%kind() /= NK_RAT) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        value = int(e%int_value(), c_int64_t)
    end function fortsym_expr_int_value

    function fortsym_expr_real_value(raw, value, message, capacity) &
            bind(c, name="fortsym_expr_real_value") result(status)
        type(c_ptr), value :: raw
        real(c_double), intent(out) :: value
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int) :: status
        type(expr_owner_t), pointer :: p
        type(expr_t) :: e

        value = 0.0_c_double
        call get_expr(raw, p, e, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (e%kind() /= NK_REAL) then
            call fail(status, message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        value = e%real_value()
    end function fortsym_expr_real_value

    subroutine begin_output(out, message, capacity)
        type(c_ptr), value :: out
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity

        if (c_associated(out)) call c_store_pointer(out, c_null_ptr)
        call put_error(message, capacity, FORTSYM_OK)
    end subroutine begin_output

    subroutine get_arena(raw, a, status, message, capacity)
        type(c_ptr), value :: raw
        type(arena_owner_t), pointer :: a
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity

        nullify(a)
        status = FORTSYM_INVALID_HANDLE
        if (.not. c_associated(raw)) then
            call put_error(message, capacity, FORTSYM_INVALID_HANDLE)
            return
        end if
        call c_f_pointer(raw, a)
        if (.not. associated(a) .or. .not. allocated(a%value%nodes)) then
            nullify(a)
            call put_error(message, capacity, FORTSYM_INVALID_HANDLE)
            return
        end if
        status = FORTSYM_OK
    end subroutine get_arena

    subroutine get_expr(raw, p, e, status, message, capacity)
        type(c_ptr), value :: raw
        type(expr_owner_t), pointer :: p
        type(expr_t), intent(out) :: e
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity

        nullify(p)
        nullify(e%a)
        e%id = 0
        status = FORTSYM_INVALID_HANDLE
        if (.not. c_associated(raw)) then
            call put_error(message, capacity, FORTSYM_INVALID_HANDLE)
            return
        end if
        call c_f_pointer(raw, p)
        if (.not. associated(p) .or. .not. associated(p%arena)) then
            nullify(p)
            call put_error(message, capacity, FORTSYM_INVALID_HANDLE)
            return
        end if
        e%a => p%arena%value
        e%id = p%id
        e%generation = e%a%generation_value()
        if (.not. is_valid(e)) then
            nullify(e%a)
            call put_error(message, capacity, FORTSYM_INVALID_HANDLE)
            return
        end if
        status = FORTSYM_OK
    end subroutine get_expr

    subroutine get_binary(raw, left_raw, right_raw, a, lp, rp, left, right, &
            status, message, capacity)
        type(c_ptr), value :: raw, left_raw, right_raw
        type(arena_owner_t), pointer :: a
        type(expr_owner_t), pointer :: lp, rp
        type(expr_t), intent(out) :: left, right
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity

        call get_arena(raw, a, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(left_raw, lp, left, status, message, capacity)
        if (status /= FORTSYM_OK) return
        call get_expr(right_raw, rp, right, status, message, capacity)
        if (status /= FORTSYM_OK) return
        if (.not. associated(lp%arena, a) .or. .not. associated(rp%arena, a)) then
            call fail(status, message, capacity, FORTSYM_FOREIGN_ARENA)
        end if
    end subroutine get_binary

    subroutine make_handle(a, e, out, status, message, capacity)
        type(arena_owner_t), pointer :: a
        type(expr_t), intent(in) :: e
        type(c_ptr), value :: out
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        type(expr_owner_t), pointer :: p

        status = FORTSYM_INVALID_ARGUMENT
        if (.not. c_associated(out)) then
            call put_error(message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        if (.not. associated(a) .or. .not. is_valid(e)) then
            call put_error(message, capacity, FORTSYM_INVALID_ARGUMENT)
            return
        end if
        allocate (p)
        p%arena => a
        p%id = e%id
        a%references = a%references + 1
        call c_store_pointer(out, c_loc(p))
        status = FORTSYM_OK
    end subroutine make_handle

    subroutine prepare_native_engine(a)
        type(arena_owner_t), pointer :: a

        a%engine%home => a%value
        nullify (a%engine%assumptions)
        if (associated(a%assumptions)) then
            if (a%assumptions%n > 0) then
                a%engine%assumptions => a%assumptions
            end if
        end if
    end subroutine prepare_native_engine

    subroutine release_arena(a)
        type(arena_owner_t), pointer :: a
        type(assumption_frame_t), pointer :: frame, previous

        if (.not. associated(a)) return
        a%references = a%references - 1
        if (a%references <= 0) then
            call a%value%clear()
            if (associated(a%assumptions)) deallocate (a%assumptions)
            frame => a%assumption_stack
            do while (associated(frame))
                previous => frame%previous
                if (associated(frame%context)) deallocate (frame%context)
                nullify (frame%context, frame%previous)
                deallocate (frame)
                frame => previous
            end do
            deallocate (a)
        end if
    end subroutine release_arena

    subroutine fail(status, message, capacity, code)
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int), value :: code

        status = code
        call put_error(message, capacity, code)
    end subroutine fail

    subroutine fail_reason(status, message, capacity, code, reason)
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: capacity
        integer(c_int), value :: code
        character(*), intent(in) :: reason

        status = code
        call put_reason(message, capacity, reason)
    end subroutine fail_reason

    function from_c_string(source) result(value)
        character(kind=c_char), intent(in) :: source(*)
        character(:), allocatable :: value
        integer :: n, k

        n = 0
        do while (source(n + 1) /= c_null_char)
            n = n + 1
        end do
        allocate (character(n) :: value)
        do k = 1, n
            value(k:k) = source(k)
        end do
    end function from_c_string

    subroutine put_error(buffer, capacity, code)
        character(kind=c_char), intent(out) :: buffer(*)
        integer(c_size_t), value :: capacity
        integer(c_int), value :: code
        character(len=64) :: error_text

        if (capacity == 0_c_size_t) return
        select case (code)
        case (FORTSYM_OK); error_text = ""
        case (FORTSYM_INVALID_ARGUMENT); error_text = "invalid argument"
        case (FORTSYM_INVALID_HANDLE); error_text = "invalid handle"
        case (FORTSYM_FOREIGN_ARENA); error_text = "foreign arena"
        case (FORTSYM_PARSE_ERROR); error_text = "parse error"
        case (FORTSYM_UNSUPPORTED); error_text = "unsupported operation"
        case (FORTSYM_RESOURCE_LIMIT); error_text = "resource limit"
        case (FORTSYM_CONFLICT); error_text = "contradictory assumptions"
        case default; error_text = "fortsym operation failed"
        end select
        call put_reason(buffer, capacity, error_text)
    end subroutine put_error

    subroutine put_reason(buffer, capacity, reason)
        character(kind=c_char), intent(out) :: buffer(*)
        integer(c_size_t), value :: capacity
        character(*), intent(in) :: reason
        integer(c_size_t) :: n, k

        if (capacity == 0_c_size_t) return
        n = min(int(len_trim(reason), c_size_t), capacity - 1_c_size_t)
        do k = 1, n
            buffer(k) = reason(int(k):int(k))
        end do
        buffer(n + 1) = c_null_char
        do k = n + 2, capacity
            buffer(k) = c_null_char
        end do
    end subroutine put_reason

    subroutine put_text(source, buffer, capacity, required, status, message, &
            message_capacity)
        type(str_t), intent(in) :: source
        character(kind=c_char), intent(out) :: buffer(*)
        integer(c_size_t), value :: capacity
        integer(c_size_t), intent(out) :: required
        integer(c_int), intent(out) :: status
        character(kind=c_char), intent(out) :: message(*)
        integer(c_size_t), value :: message_capacity
        integer(c_size_t) :: n, k
        character(:), allocatable :: source_text

        source_text = source%chars()
        required = int(len(source_text), c_size_t) + 1_c_size_t
        status = FORTSYM_OK
        if (capacity > 0_c_size_t) then
            n = min(int(len(source_text), c_size_t), capacity - 1_c_size_t)
            do k = 1, n
                buffer(k) = source_text(int(k):int(k))
            end do
            buffer(n + 1) = c_null_char
        end if
        if (capacity < required) then
            call fail(status, message, message_capacity, FORTSYM_RESOURCE_LIMIT)
        end if
    end subroutine put_text

    subroutine c_store_pointer(address, value)
        type(c_ptr), value :: address, value
        type(c_ptr), pointer :: slot

        call c_f_pointer(address, slot)
        slot = value
    end subroutine c_store_pointer

end module fortsym_public_capi
