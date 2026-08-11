module fortsym
    ! The short public Fortran surface. Explicit arenas and the lower-level
    ! modules remain available for callers that need independent state.
    use fortsym_arena, only: arena_t, node_kind_name, &
        NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, NK_POW, &
        NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL
    use fortsym_string, only: chars
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, &
        real_text_expr, const, func, func_in, partial, pi_expr, e_expr, &
        i_expr, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, &
        tanh, asinh, acosh, atanh, exp, log, sqrt, abs, erf, erfc, &
        gamma, besselj, legendrep, legendreq, &
        is_valid, same_arena, operator(+), operator(-), operator(*), &
        operator(/), operator(**), operator(==), operator(/=)
    use fortsym_subs, only: subs_impl => subs
    use fortsym_numeric, only: numeric_value, numeric_text, &
        numeric_precision_text, numeric_complex_text, &
        numeric_complex_text_t, numeric_callable_t
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE, verdict_name
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_backend, only: BACKEND_PROTOCOL_VERSION, EXPRESSION_SCHEMA, &
        BACKEND_PROVED, BACKEND_DISPROVED, BACKEND_UNKNOWN, &
        backend_evidence_t, backend_result_t, backend_status_name, &
        serialize_expression, deserialize_expression, assess_identity, &
        assess_equivalence, evidence_json, emit_backend_kernel
    use fortsym_ode, only: solve_ode
    implicit none
    private

    public :: arena_t, node_kind_name
    public :: NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, &
        NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL
    public :: expr_t, sym, num, rat, exact, real_expr, real_text_expr, const, &
        func, func_in, partial, pi_expr, e_expr, i_expr, is_valid, same_arena
    public :: subs, diff, simplify, expand, factor
    public :: zero_test, VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, verdict_name
    public :: numeric_value, numeric_text, numeric_precision_text, &
        numeric_complex_text, numeric_complex_text_t, numeric_callable_t
    public :: BACKEND_PROTOCOL_VERSION, EXPRESSION_SCHEMA, BACKEND_PROVED, &
        BACKEND_DISPROVED, BACKEND_UNKNOWN, backend_evidence_t, &
        backend_result_t, backend_status_name, serialize_expression, &
        deserialize_expression, assess_identity, assess_equivalence, &
        evidence_json, emit_backend_kernel
    public :: solve_ode
    public :: operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), operator(/=)
    public :: sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh, &
        asinh, acosh, atanh, exp, log, sqrt, abs, erf, erfc, gamma, &
        besselj, legendrep, legendreq
    public :: default_arena, reset, symbols
    public :: assignment(=)

    type(arena_t), target, save :: default_store
    logical, save :: default_ready = .false.

    interface assignment(=)
        module procedure assign_character
    end interface assignment(=)

contains

    !> Return the process-local arena used by character assignment and symbols.
    !> It is single-threaded state. Callers that need concurrency should create
    !> an arena_t and use the explicit constructors from the same module.
    function default_arena() result(a)
        type(arena_t), pointer :: a
        call ensure_default()
        a => default_store
    end function default_arena

    !> Clear the convenience arena. Handles made before this call become stale,
    !> including handles whose node index is reused after the next construction.
    subroutine reset()
        call default_store%clear()
        default_ready = .false.
    end subroutine reset

    !> Replace one expression structurally. The optional status arguments use
    !> the same convention as the native transformations below: a failed
    !> operation returns an invalid expression and explains why when requested.
    function subs(expression, old, new, ok, why) result(out)
        type(expr_t), intent(in) :: expression, old, new
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        type(expr_t) :: out

        out = expr_t()
        if (.not. is_valid(expression)) then
            call report_failure(ok, why, "subs: invalid expression")
            return
        end if
        if (.not. is_valid(old) .or. .not. is_valid(new)) then
            call report_failure(ok, why, "subs: invalid replacement")
            return
        end if
        if (.not. same_arena(expression, old) .or. &
            .not. same_arena(expression, new)) then
            call report_failure(ok, why, "subs: expressions belong to different arenas")
            return
        end if

        out = subs_impl(expression, old, new)
        if (.not. is_valid(out)) then
            call report_failure(ok, why, "subs: substitution failed")
        else
            call report_success(ok, why)
        end if
    end function subs

    !> Differentiate and simplify through the native engine in the expression's
    !> arena. The low-level fortsym_diff module remains available when callers
    !> specifically need the unsimplified derivative DAG.
    function diff(expression, variable, ok, why) result(out)
        type(expr_t), intent(in) :: expression, variable
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        type(expr_t) :: out
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        out = expr_t()
        if (.not. is_valid(expression) .or. .not. is_valid(variable)) then
            call report_failure(ok, why, "diff: invalid expression")
            return
        end if
        if (.not. same_arena(expression, variable)) then
            call report_failure(ok, why, "diff: expressions belong to different arenas")
            return
        end if

        engine = make_native_engine(expression%a)
        result = engine%diff(expression, variable)
        out = expression_from_engine(result, "diff", ok, why)
    end function diff

    !> Simplify an expression with the native engine in its owning arena.
    function simplify(expression, ok, why) result(out)
        type(expr_t), intent(in) :: expression
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        type(expr_t) :: out
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        out = expr_t()
        if (.not. is_valid(expression)) then
            call report_failure(ok, why, "simplify: invalid expression")
            return
        end if

        engine = make_native_engine(expression%a)
        result = engine%simplify(expression)
        out = expression_from_engine(result, "simplify", ok, why)
    end function simplify

    !> Expand an expression with the native engine in its owning arena.
    function expand(expression, ok, why) result(out)
        type(expr_t), intent(in) :: expression
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        type(expr_t) :: out
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        out = expr_t()
        if (.not. is_valid(expression)) then
            call report_failure(ok, why, "expand: invalid expression")
            return
        end if

        engine = make_native_engine(expression%a)
        result = engine%expand(expression)
        out = expression_from_engine(result, "expand", ok, why)
    end function expand

    !> Factor an expression with the native engine in its owning arena.
    function factor(expression, ok, why) result(out)
        type(expr_t), intent(in) :: expression
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        type(expr_t) :: out
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        out = expr_t()
        if (.not. is_valid(expression)) then
            call report_failure(ok, why, "factor: invalid expression")
            return
        end if

        engine = make_native_engine(expression%a)
        result = engine%factor(expression)
        out = expression_from_engine(result, "factor", ok, why)
    end function factor

    !> Return the native three-valued zero verdict for an expression.
    !> VERDICT_TRUE means proved zero, VERDICT_FALSE means proved nonzero, and
    !> VERDICT_UNKNOWN means the native engine declined to decide.
    function zero_test(expression) result(verdict)
        type(expr_t), intent(in) :: expression
        integer :: verdict
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        verdict = VERDICT_UNKNOWN
        if (.not. is_valid(expression)) return
        engine = make_native_engine(expression%a)
        result = engine%zero_test(expression)
        verdict = result%verdict
    end function zero_test

    !> Assigning text creates one symbol in the default arena. Text is never
    !> parsed as an expression.
    subroutine assign_character(lhs, rhs)
        type(expr_t), intent(out) :: lhs
        character(*), intent(in)   :: rhs
        call ensure_default()
        lhs = sym(default_store, trim(rhs))
    end subroutine assign_character

    function expression_from_engine(result, operation, ok, why) result(out)
        type(engine_result_t), intent(in) :: result
        character(*), intent(in) :: operation
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        type(expr_t) :: out
        character(:), allocatable :: message

        out = expr_t()
        if (result%ok .and. is_valid(result%value)) then
            out = result%value
            call report_success(ok, why)
            return
        end if

        message = chars(result%message)
        if (len(message) == 0) message = operation//": operation failed"
        call report_failure(ok, why, message)
    end function expression_from_engine

    subroutine report_success(ok, why)
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why

        if (present(ok)) ok = .true.
        if (present(why)) why = ""
    end subroutine report_success

    subroutine report_failure(ok, why, message)
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: why
        character(*), intent(in) :: message

        if (present(ok)) ok = .false.
        if (present(why)) why = message
    end subroutine report_failure

    !> Read up to eight whitespace- or comma-separated symbol names into scalar
    !> outputs. A missing output or an extra name sets ok=.false.; every output
    !> remains a valid symbol only when a corresponding name was present.
    subroutine symbols(names, first, second, third, fourth, fifth, sixth, &
            seventh, eighth, ok)
        character(*), intent(in) :: names
        type(expr_t), intent(out) :: first
        type(expr_t), intent(out), optional :: second, third, fourth, fifth
        type(expr_t), intent(out), optional :: sixth, seventh, eighth
        logical, intent(out), optional :: ok
        integer :: position
        logical :: good

        first = expr_t()
        if (present(second)) second = expr_t()
        if (present(third)) third = expr_t()
        if (present(fourth)) fourth = expr_t()
        if (present(fifth)) fifth = expr_t()
        if (present(sixth)) sixth = expr_t()
        if (present(seventh)) seventh = expr_t()
        if (present(eighth)) eighth = expr_t()

        call ensure_default()
        position = 1
        good = .true.
        call read_symbol(names, position, first, good)
        if (present(second)) call read_symbol(names, position, second, good)
        if (present(third)) call read_symbol(names, position, third, good)
        if (present(fourth)) call read_symbol(names, position, fourth, good)
        if (present(fifth)) call read_symbol(names, position, fifth, good)
        if (present(sixth)) call read_symbol(names, position, sixth, good)
        if (present(seventh)) call read_symbol(names, position, seventh, good)
        if (present(eighth)) call read_symbol(names, position, eighth, good)
        call skip_separators(names, position)
        if (position <= len_trim(names)) good = .false.
        if (present(ok)) ok = good
    end subroutine symbols

    subroutine ensure_default()
        if (default_ready) return
        call default_store%init()
        default_ready = .true.
    end subroutine ensure_default

    subroutine read_symbol(names, position, target, good)
        character(*), intent(in) :: names
        integer, intent(inout) :: position
        type(expr_t), intent(inout) :: target
        logical, intent(inout) :: good
        integer :: start, finish

        call skip_separators(names, position)
        if (position > len_trim(names)) then
            good = .false.
            return
        end if
        start = position
        do while (position <= len_trim(names))
            if (is_separator(names(position:position))) exit
            position = position + 1
        end do
        finish = position - 1
        target = sym(default_store, names(start:finish))
    end subroutine read_symbol

    subroutine skip_separators(names, position)
        character(*), intent(in) :: names
        integer, intent(inout) :: position
        do while (position <= len_trim(names))
            if (.not. is_separator(names(position:position))) return
            position = position + 1
        end do
    end subroutine skip_separators

    pure logical function is_separator(character)
        character, intent(in) :: character
        is_separator = character == ' ' .or. character == ',' .or. &
            iachar(character) == 9
    end function is_separator

end module fortsym
