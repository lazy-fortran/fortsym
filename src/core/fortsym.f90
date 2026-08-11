module fortsym
    ! The short public Fortran surface. Explicit arenas and the lower-level
    ! modules remain available for callers that need independent state.
    use fortsym_arena, only: arena_t, node_kind_name, &
        NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, NK_POW, &
        NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_string, only: str, chars
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, &
        real_text_expr, algebraic_expr, const, func, func_in, partial, pi_expr, e_expr, &
        i_expr, oo_expr, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, &
        tanh, asinh, acosh, atanh, exp, log, sqrt, abs, erf, erfc, &
        gamma, besselj, legendrep, legendreq, &
        is_valid, same_arena, operator(+), operator(-), operator(*), &
        operator(/), operator(**), operator(==), operator(/=)
    use fortsym_relation, only: equal, unequal, less, less_equal, greater, &
        greater_equal
    use fortsym_subs, only: subs_impl => subs
    use fortsym_assume, only: assumption_context_t, &
        make_assumption_context, with_assumption, zero, negative, nonpositive, &
        positive, nonnegative, nonzero, real_valued
    use fortsym_numeric, only: numeric_value, numeric_text, &
        numeric_precision_text, numeric_complex_text, &
        numeric_real_text_t, numeric_complex_text_t, numeric_callable_t
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE, verdict_name
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
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
        NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, NK_ALGEBRAIC
    public :: expr_t, sym, num, rat, exact, real_expr, real_text_expr, &
        algebraic_expr, const, &
        func, func_in, partial, equal, unequal, less, less_equal, greater, &
        greater_equal, pi_expr, e_expr, i_expr, oo_expr, is_valid, same_arena
    public :: assumption_context_t, make_assumption_context, with_assumption, &
        zero, negative, nonpositive, positive, nonnegative, nonzero, real_valued
    public :: str, chars
    public :: subs, diff, simplify, refine, expand, factor
    public :: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    public :: engine_result_t, zero_test, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE, verdict_name
    public :: numeric_value, numeric_text, numeric_precision_text, &
        numeric_real_text_t, numeric_complex_text, numeric_complex_text_t, &
        numeric_callable_t
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

    !> Replace one expression structurally.
    function subs(expression, old, new) result(result)
        type(expr_t), intent(in) :: expression, old, new
        type(engine_result_t) :: result

        if (.not. is_valid(expression)) then
            call report_failure(result, "subs: invalid expression")
            return
        end if
        if (.not. is_valid(old) .or. .not. is_valid(new)) then
            call report_failure(result, "subs: invalid replacement")
            return
        end if
        if (.not. same_arena(expression, old) .or. &
            .not. same_arena(expression, new)) then
            call report_failure(result, "subs: expressions belong to different arenas")
            return
        end if

        result%value = subs_impl(expression, old, new)
        if (.not. is_valid(result%value)) then
            call report_failure(result, "subs: substitution failed")
        else
            result%ok = .true.
        end if
    end function subs

    !> Differentiate and simplify through the native engine in the expression's
    !> arena. The low-level fortsym_diff module remains available when callers
    !> specifically need the unsimplified derivative DAG.
    function diff(expression, variable, assumptions) result(result)
        type(expr_t), intent(in) :: expression, variable
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression) .or. .not. is_valid(variable)) then
            call report_failure(result, "diff: invalid expression")
            return
        end if
        if (.not. same_arena(expression, variable)) then
            call report_failure(result, "diff: expressions belong to different arenas")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, "diff: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%diff(expression, variable)
    end function diff

    !> Simplify an expression with the native engine in its owning arena.
    function simplify(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "simplify: invalid expression")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "simplify: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%simplify(expression)
    end function simplify

    !> Refine an expression under an explicit assumption context. The native
    !> simplifier owns the guarded rewrite rules; refine is the named facade
    !> entry point for callers who are supplying domain facts.
    function refine(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        if (present(assumptions)) then
            result = simplify(expression, assumptions)
        else
            result = simplify(expression)
        end if
    end function refine

    !> Expand an expression with the native engine in its owning arena.
    function expand(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "expand: invalid expression")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "expand: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%expand(expression)
    end function expand

    !> Factor an expression with the native engine in its owning arena.
    function factor(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "factor: invalid expression")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "factor: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%factor(expression)
    end function factor

    !> Return the native three-valued zero verdict for an expression.
    !> VERDICT_TRUE means proved zero, VERDICT_FALSE means proved nonzero, and
    !> VERDICT_UNKNOWN means the native engine declined to decide.
    function zero_test(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "zero_test: invalid expression")
            return
        end if
        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "zero_test: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%zero_test(expression)
    end function zero_test

    !> Assigning text creates one symbol in the default arena. Text is never
    !> parsed as an expression.
    subroutine assign_character(lhs, rhs)
        type(expr_t), intent(out) :: lhs
        character(*), intent(in)   :: rhs
        call ensure_default()
        lhs = sym(default_store, trim(rhs))
    end subroutine assign_character

    subroutine report_failure(result, message)
        type(engine_result_t), intent(out) :: result
        character(*), intent(in) :: message

        result%ok = .false.
        result%value = expr_t()
        result%verdict = VERDICT_UNKNOWN
        result%message = str(message)
    end subroutine report_failure

    logical function context_matches(expression, assumptions) result(matches)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, intent(in) :: assumptions

        matches = .true.
        if (.not. present(assumptions)) return
        matches = associated(assumptions%home)
        if (matches) matches = associated(assumptions%home, expression%a)
    end function context_matches

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
