module fortsym_wl_f90
    ! Bounded source-to-source translation for a Wolfram assignment stream.
    !
    ! This is deliberately smaller than the native Wolfram interpreter: it
    ! accepts a short stream of top-level `name = expression` (or `:=`)
    ! statements, infers scalar inputs, expands references to earlier
    ! assignments, and delegates Fortran emission to the existing kernel
    ! generator. Unsupported source is refused explicitly.
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t, NK_CONST, NK_FUNC
    use fortsym_expr, only: expr_t, sym
    use fortsym_dialect, only: dialect, DIA_FORTRAN, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_eval, only: free_symbols_of
    use fortsym_subs, only: subs_many
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    use fortsym_print, only: print_expr_in, fortran_representable
    implicit none
    private

    public :: translate_wl_assignment
    public :: translate_wl_assignments

    integer, parameter :: MAX_ASSIGNMENTS = 128
    integer, parameter :: MAX_EXPRESSION_NODES = 100000

    character(*), parameter :: TEMP_PREFIX = "fsym_tmp"
    character(*), parameter :: GENERATED_NAME = "fortsym_generated_assignment"

contains

    !> Translate one bounded Wolfram assignment to a complete Fortran
    !> subroutine. Kept as a compatibility spelling; the implementation now
    !> accepts the same assignment stream as translate_wl_assignments.
    function translate_wl_assignment(source, ok, message) result(code)
        character(*),              intent(in)  :: source
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t)                            :: code

        code = translate_wl_assignments(source, ok, message)
    end function translate_wl_assignment

    !> Translate a bounded top-level assignment stream to a complete Fortran
    !> subroutine. Statements are separated by a semicolon or a newline at
    !> bracket depth zero. The returned source is empty when ok is false.
    function translate_wl_assignments(source, ok, message) result(code)
        character(*),              intent(in)  :: source
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t)                            :: code

        type(arena_t), target :: a
        type(expr_t), allocatable :: roots(:), old_symbols(:)
        type(kernel_spec_t) :: spec
        type(str_t), allocatable :: statements(:), targets(:), inputs(:)
        type(str_t), allocatable :: names(:)
        character(:), allocatable :: statement, lhs, rhs, parse_message
        character(:), allocatable :: cleaned_source
        integer :: nstatements, ninputs, k, j, eq, width
        integer :: first_statement
        integer :: nold
        logical :: parsed, representable, handled
        type(expr_t) :: root

        code = str("")
        ok = .false.
        message = ""

        call strip_leading_comments(source, cleaned_source, parsed, message)
        if (.not. parsed) return
        call split_stream(cleaned_source, statements, nstatements, parsed, message)
        if (.not. parsed) return
        first_statement = 1
        do while (first_statement <= nstatements)
            if (.not. safe_setup_statement(chars(statements(first_statement)))) exit
            first_statement = first_statement + 1
        end do
        if (first_statement > 1) then
            do k = first_statement, nstatements
                statements(k - first_statement + 1) = statements(k)
            end do
            nstatements = nstatements - first_statement + 1
        end if
        if (nstatements == 0) then
            message = "expected at least one top-level name = expression assignment"
            return
        end if

        ! Dynamic If needs statement-level control flow in the generated
        ! subroutine, so it is handled before the ordinary expression kernel.
        ! The bounded helper deliberately accepts only a scalar comparison and
        ! two assignments to one target; everything else stays on the normal
        ! refusal path below.
        if (nstatements == 1) then
            call translate_bounded_while_step_statement(chars(statements(1)), code, &
                handled, parsed, parse_message)
            if (.not. parsed) then
                message = parse_message
                return
            end if
            if (handled) then
                ok = .true.
                return
            end if
        end if
        if (nstatements == 1) then
            call translate_dynamic_if_statement(chars(statements(1)), code, handled, &
                parsed, parse_message)
            if (.not. parsed) then
                message = parse_message
                return
            end if
            if (handled) then
                ok = .true.
                return
            end if
        end if

        call a%init()
        call translate_bounded_scalar_reassignment(a, statements, nstatements, code, &
            handled, parsed, parse_message)
        if (.not. parsed) then
            message = parse_message
            return
        end if
        if (handled) then
            ok = .true.
            return
        end if
        allocate (roots(nstatements), old_symbols(nstatements))
        allocate (targets(nstatements))
        nold = 0
        do k = 1, nstatements
            statement = chars(statements(k))
            call lower_constant_if_statement(statement, cleaned_source, handled, &
                parsed, parse_message)
            if (.not. parsed) then
                message = parse_message
                return
            end if
            if (handled) statement = cleaned_source
            if (.not. handled) then
                call lower_bounded_while_statement(statement, cleaned_source, handled, &
                    parsed, parse_message)
                if (.not. parsed) then
                    message = parse_message
                    return
                end if
                if (handled) statement = cleaned_source
            end if
            if (.not. handled) then
                call lower_bounded_do_statement(statement, cleaned_source, handled, &
                    parsed, parse_message)
                if (.not. parsed) then
                    message = parse_message
                    return
                end if
                if (handled) statement = cleaned_source
            end if
            if (.not. handled) then
                call lower_bounded_for_statement(statement, cleaned_source, handled, &
                    parsed, parse_message)
                if (.not. parsed) then
                    message = parse_message
                    return
                end if
                if (handled) statement = cleaned_source
            end if

            eq = top_level_assignment(statement)
            if (eq == 0) then
                message = "expected a top-level name = expression assignment; "// &
                    "control flow and other statements are unsupported"
                return
            end if

            width = 1
            if (statement(eq:eq) == ":") width = 2
            lhs = trim(adjustl(statement(:eq - 1)))
            rhs = trim(adjustl(statement(eq + width:)))
            if (.not. valid_target_name(lhs)) then
                message = "assignment target is not a supported Fortran scalar name"
                return
            end if
            if (len(rhs) == 0) then
                message = "assignment has no right-hand side"
                return
            end if
            do j = 1, k - 1
                if (same_fortran_name(lhs, chars(targets(j)))) then
                    message = "assignment target is repeated; reassignment is unsupported"
                    return
                end if
            end do

            root = parse_expr_in(a, rhs, dialect(DIA_WOLFRAM), parsed, parse_message)
            if (.not. parsed) then
                message = "cannot parse right-hand side: "//parse_message
                return
            end if

            if (nold > 0) then
                root = subs_many(root, old_symbols(1:nold), roots(1:nold))
            end if
            if (root%node_count() > MAX_EXPRESSION_NODES) then
                message = "expanded expression exceeds the bounded node limit"
                return
            end if

            names = free_symbols_of(root)
            do j = 1, size(names)
                if (.not. valid_input_name(chars(names(j)))) then
                    message = "right-hand side contains a non-Fortran symbol: "// &
                        chars(names(j))
                    return
                end if
                if (same_fortran_name(chars(names(j)), lhs)) then
                    message = "recursive assignment target is unsupported"
                    return
                end if
            end do

            targets(k) = str(lhs)
            roots(k) = root
            nold = nold + 1
            old_symbols(nold) = sym(a, lhs)
        end do

        allocate (inputs(MAX_ASSIGNMENTS))
        ninputs = 0
        do k = 1, nstatements
            names = free_symbols_of(roots(k))
            do j = 1, size(names)
                if (name_in_list(chars(names(j)), targets, nstatements)) then
                    message = "right-hand side refers to an assignment target "// &
                        "before its definition: "//chars(names(j))
                    return
                end if
                if (.not. name_in_list(chars(names(j)), inputs, ninputs)) then
                    if (ninputs >= MAX_ASSIGNMENTS) then
                        message = "input symbol count exceeds the bounded limit"
                        return
                    end if
                    ninputs = ninputs + 1
                    inputs(ninputs) = names(j)
                end if
            end do
        end do

        spec%name = str("fortsym_generated_assignment")
        spec%mode = KERNEL_SUBROUTINE
        spec%generator = str("fortsym_wl_f90")
        spec%regenerate_command = str("fortsym_wl_to_f90 input.wl output.f90")
        spec%temp_prefix = str(TEMP_PREFIX)
        allocate (spec%args(ninputs), spec%outputs(nstatements))
        if (ninputs > 0) spec%args = inputs(1:ninputs)
        spec%outputs = targets

        representable = .true.
        do k = 1, nstatements
            if (.not. supported_fortran_expression(roots(k))) then
                representable = .false.
                exit
            end if
        end do
        if (representable) code = emit_kernel(roots, spec, representable)
        if (.not. representable .or. len(chars(code)) == 0) then
            message = "expression is outside the supported scalar Fortran grammar"
            code = str("")
            return
        end if
        ok = .true.
    end function translate_wl_assignments

    !> Lower one bounded scalar reassignment stream.
    !>
    !> For exactly two assignments to the same scalar target, the first value
    !> can be substituted into the second RHS because the accepted expression
    !> grammar is side-effect free.  The emitted kernel exposes only the final
    !> target, so its Fortran interface has no duplicate dummy arguments.  A
    !> longer stream, recursive first assignment, or non-scalar target remains
    !> on the ordinary refusal path.
    subroutine translate_bounded_scalar_reassignment(a, statements, nstatements, &
        code, handled, ok, message)
        type(arena_t), target, intent(inout) :: a
        type(str_t), intent(in) :: statements(:)
        integer, intent(in) :: nstatements
        type(str_t), intent(out) :: code
        logical, intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        type(expr_t) :: first_root, second_root, final_root, target_symbol
        type(kernel_spec_t) :: spec
        type(str_t), allocatable :: names(:), inputs(:)
        character(:), allocatable :: first, second, first_lhs, first_rhs
        character(:), allocatable :: second_lhs, second_rhs, parse_message
        integer :: first_eq, second_eq, first_width, second_width
        integer :: ninputs, j
        logical :: parsed, representable

        code = str("")
        handled = .false.
        ok = .true.
        message = ""
        if (nstatements /= 2) return

        first = chars(statements(1))
        second = chars(statements(2))
        first_eq = top_level_assignment(first)
        second_eq = top_level_assignment(second)
        if (first_eq == 0 .or. second_eq == 0) return

        first_width = 1
        if (first(first_eq:first_eq) == ":") first_width = 2
        second_width = 1
        if (second(second_eq:second_eq) == ":") second_width = 2
        first_lhs = trim(adjustl(first(:first_eq - 1)))
        first_rhs = trim(adjustl(first(first_eq + first_width:)))
        second_lhs = trim(adjustl(second(:second_eq - 1)))
        second_rhs = trim(adjustl(second(second_eq + second_width:)))
        if (.not. valid_target_name(first_lhs) .or. &
            .not. valid_target_name(second_lhs)) return
        if (first_lhs /= second_lhs) return
        handled = .true.
        if (len(first_rhs) == 0 .or. len(second_rhs) == 0) then
            ok = .false.
            message = "scalar reassignment needs two right-hand sides"
            return
        end if
        if (symbol_occurs(first_rhs, first_lhs)) then
            ok = .false.
            message = "first scalar assignment may not read its target"
            return
        end if

        first_root = parse_expr_in(a, first_rhs, dialect(DIA_WOLFRAM), parsed, &
            parse_message)
        if (.not. parsed) then
            ok = .false.
            message = "cannot parse first scalar assignment: "//parse_message
            return
        end if
        second_root = parse_expr_in(a, second_rhs, dialect(DIA_WOLFRAM), parsed, &
            parse_message)
        if (.not. parsed) then
            ok = .false.
            message = "cannot parse second scalar assignment: "//parse_message
            return
        end if
        target_symbol = sym(a, first_lhs)
        final_root = subs_many(second_root, [target_symbol], [first_root])
        if (final_root%node_count() > MAX_EXPRESSION_NODES) then
            ok = .false.
            message = "expanded scalar reassignment exceeds the bounded node limit"
            return
        end if
        representable = supported_fortran_expression(final_root)
        if (.not. representable) then
            ok = .false.
            message = "scalar reassignment is outside the supported scalar Fortran grammar"
            return
        end if

        allocate (inputs(MAX_ASSIGNMENTS))
        ninputs = 0
        names = free_symbols_of(final_root)
        do j = 1, size(names)
            if (.not. valid_input_name(chars(names(j)))) then
                ok = .false.
                message = "right-hand side contains a non-Fortran symbol: "// &
                    chars(names(j))
                return
            end if
            if (same_fortran_name(chars(names(j)), first_lhs)) then
                ok = .false.
                message = "recursive scalar reassignment is unsupported"
                return
            end if
            if (.not. name_in_list(chars(names(j)), inputs, ninputs)) then
                ninputs = ninputs + 1
                inputs(ninputs) = names(j)
            end if
        end do

        spec%name = str(GENERATED_NAME)
        spec%mode = KERNEL_SUBROUTINE
        spec%generator = str("fortsym_wl_f90")
        spec%regenerate_command = str("fortsym_wl_to_f90 input.wl output.f90")
        spec%temp_prefix = str(TEMP_PREFIX)
        allocate (spec%args(ninputs), spec%outputs(1))
        if (ninputs > 0) spec%args = inputs(1:ninputs)
        spec%outputs(1) = str(first_lhs)
        code = emit_kernel([final_root], spec, representable)
        if (len(chars(code)) == 0) then
            ok = .false.
            message = "scalar reassignment could not be emitted as Fortran"
        end if
    end subroutine translate_bounded_scalar_reassignment

    !> Lower the one-step, stateless While form that is always bounded.
    !>
    !> `While[x != y, x = y]` executes zero or one times and leaves x equal to
    !> y in either case.  Requiring the condition and assignment to have the
    !> same scalar target and right-hand side makes the lowering source-faithful
    !> without guessing about symbolic loop bounds or termination.
    subroutine lower_bounded_while_statement(text, lowered, handled, ok, message)
        character(*),              intent(in)  :: text
        character(:), allocatable, intent(out) :: lowered
        logical,                   intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: whole, args, condition, body
        character(:), allocatable :: condition_lhs, condition_rhs
        character(:), allocatable :: body_lhs, body_rhs
        integer :: open, close, comma, relation, eq, width

        lowered = ""
        handled = .false.
        ok = .true.
        message = ""
        whole = trim(adjustl(text))
        open = index(whole, "[")
        if (open <= 1) return
        if (whole(:open - 1) /= "While") return
        handled = .true.
        close = matching_close(whole, open, "[", "]")
        if (close /= len(whole)) then
            ok = .false.
            message = "bounded While must have one balanced argument list"
            return
        end if

        args = whole(open + 1:close - 1)
        call next_top_level_comma(args, 1, comma)
        if (comma == 0) then
            ok = .false.
            message = "bounded While needs a condition and a body"
            return
        end if
        condition = trim(adjustl(args(:comma - 1)))
        body = trim(adjustl(args(comma + 1:)))
        relation = index(condition, "!=")
        if (relation == 0) then
            ok = .false.
            message = "bounded While requires an unequal scalar condition"
            return
        end if
        condition_lhs = trim(adjustl(condition(:relation - 1)))
        condition_rhs = trim(adjustl(condition(relation + 2:)))
        if (.not. valid_target_name(condition_lhs) .or. len(condition_rhs) == 0) then
            ok = .false.
            message = "bounded While condition must compare a scalar target"
            return
        end if

        eq = top_level_assignment(body)
        if (eq == 0) then
            ok = .false.
            message = "bounded While body must be a scalar assignment"
            return
        end if
        width = 1
        if (body(eq:eq) == ":") width = 2
        body_lhs = trim(adjustl(body(:eq - 1)))
        body_rhs = trim(adjustl(body(eq + width:)))
        if (.not. valid_target_name(body_lhs) .or. len(body_rhs) == 0) then
            ok = .false.
            message = "bounded While body must assign a Fortran scalar"
            return
        end if
        if (body_lhs /= condition_lhs .or. body_rhs /= condition_rhs) then
            ok = .false.
            message = "bounded While body must assign the condition's value"
            return
        end if
        lowered = body_lhs//" = "//body_rhs
    end subroutine lower_bounded_while_statement

    !> Emit a terminating integer While loop with a unit step.
    !>
    !> Exact integer bounds and a matching direction are required.  Therefore
    !> `While[i < 4, i++]` and `While[i >= -2, i--]` terminate for every
    !> representable initial i, while arbitrary symbolic bounds and unrelated
    !> bodies remain refused.  Keeping the loop in the generated source also
    !> preserves the input value when the initial condition is false.
    subroutine translate_bounded_while_step_statement(text, code, handled, ok, message)
        character(*),              intent(in)  :: text
        type(str_t),               intent(out) :: code
        logical,                   intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        type(strbuf_t) :: b
        character(:), allocatable :: whole, args, condition, body
        character(:), allocatable :: left, operator, right
        integer :: open, close, comma, bound, ios
        logical :: good, increment

        code = str("")
        handled = .false.
        ok = .true.
        message = ""
        whole = trim(adjustl(text))
        open = index(whole, "[")
        if (open <= 1) return
        if (whole(:open - 1) /= "While") return
        handled = .true.
        close = matching_close(whole, open, "[", "]")
        if (close /= len(whole)) then
            ok = .false.
            message = "integer-step While must have one balanced argument list"
            return
        end if

        args = whole(open + 1:close - 1)
        call next_top_level_comma(args, 1, comma)
        if (comma == 0) then
            ok = .false.
            message = "integer-step While needs a condition and a body"
            return
        end if
        condition = trim(adjustl(args(:comma - 1)))
        body = trim(adjustl(args(comma + 1:)))
        call split_scalar_comparison(condition, left, operator, right, good)
        if (.not. good .or. .not. valid_target_name(left)) then
            ok = .false.
            message = "integer-step While requires one scalar comparison"
            return
        end if
        read (right, *, iostat=ios) bound
        if (ios /= 0) then
            handled = .false.
            return
        end if

        increment = body == left//"++" .or. body == "++"//left
        if (.not. increment) then
            if (body /= left//"--" .and. body /= "--"//left) then
                handled = .false.
                return
            end if
        end if
        if (increment) then
            if (operator /= "<" .and. operator /= "<=") then
                handled = .false.
                return
            end if
        else
            if (operator /= ">" .and. operator /= ">=") then
                handled = .false.
                return
            end if
        end if

        call b%append("! Generated by fortsym. Do not edit.")
        call b%newline()
        call b%append("! Generator: fortsym_wl_f90")
        call b%newline()
        call b%newline()
        call b%append("subroutine fortsym_generated_assignment("//left//")")
        call b%newline()
        call b%append("    implicit none")
        call b%newline()
        call b%append("    integer, intent(inout) :: "//left)
        call b%newline()
        call b%newline()
        call b%append("    do while ("//left//" "//operator//" "//trim(right)//")")
        call b%newline()
        if (increment) then
            call b%append("        "//left//" = "//left//" + 1")
        else
            call b%append("        "//left//" = "//left//" - 1")
        end if
        call b%newline()
        call b%append("    end do")
        call b%newline()
        call b%append("end subroutine fortsym_generated_assignment")
        call b%newline()
        code = b%to_str()
    end subroutine translate_bounded_while_step_statement

    !> Lower a stateless bounded Do assignment to its final iteration.
    !>
    !> A straight-line emitter cannot represent a runtime loop, but
    !> `Do[result = x + i, {i, 1, 3}]` has the same scalar result as
    !> `result = x + 3`: every iteration overwrites the target and the body
    !> does not read it. Keep recursive accumulations and symbolic ranges
    !> refused rather than silently changing their meaning.
    subroutine lower_bounded_do_statement(text, lowered, handled, ok, message)
        character(*),              intent(in)  :: text
        character(:), allocatable, intent(out) :: lowered
        logical,                   intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: whole, args, body, range
        character(:), allocatable :: iterator, lhs, rhs, replacement
        character(:), allocatable :: first_text, last_text
        character(32) :: replacement_buffer
        integer :: open, close, comma_one, range_open, range_close
        integer :: comma_two, extra_comma, eq, width
        integer :: first_value, last_value, ios
        logical :: body_assignment

        lowered = ""
        handled = .false.
        ok = .true.
        message = ""
        whole = trim(adjustl(text))
        open = index(whole, "[")
        if (open <= 1) return
        if (whole(:open - 1) /= "Do") return
        handled = .true.
        close = matching_close(whole, open, "[", "]")
        if (close /= len(whole)) then
            ok = .false.
            message = "bounded Do must have one balanced argument list"
            return
        end if

        args = whole(open + 1:close - 1)
        call next_top_level_comma(args, 1, comma_one)
        if (comma_one == 0) then
            ok = .false.
            message = "bounded Do needs a body and an iterator range"
            return
        end if
        body = trim(adjustl(args(:comma_one - 1)))
        range = trim(adjustl(args(comma_one + 1:)))
        eq = top_level_assignment(body)
        if (eq == 0) then
            ok = .false.
            message = "bounded Do body must be a scalar assignment"
            return
        end if
        width = 1
        if (body(eq:eq) == ":") width = 2
        lhs = trim(adjustl(body(:eq - 1)))
        rhs = trim(adjustl(body(eq + width:)))
        body_assignment = valid_target_name(lhs) .and. len(rhs) > 0
        if (.not. body_assignment) then
            ok = .false.
            message = "bounded Do body must assign a Fortran scalar"
            return
        end if

        range_open = index(range, "{")
        range_close = len(range)
        if (range_close < 2) then
            ok = .false.
            message = "bounded Do range must be a list"
            return
        end if
        if (range_open /= 1 .or. range(range_close:range_close) /= "}") then
            ok = .false.
            message = "bounded Do range must be a list"
            return
        end if
        range = trim(adjustl(range(range_open + 1:range_close - 1)))
        call next_top_level_comma(range, 1, comma_one)
        if (comma_one == 0) then
            ok = .false.
            message = "bounded Do range needs an iterator and an endpoint"
            return
        end if
        iterator = trim(adjustl(range(:comma_one - 1)))
        if (.not. valid_fortran_name(iterator)) then
            ok = .false.
            message = "bounded Do iterator must be a Fortran name"
            return
        end if
        call next_top_level_comma(range, comma_one + 1, comma_two)
        call next_top_level_comma(range, comma_two + 1, extra_comma)
        if (comma_two == 0) then
            first_text = "1"
            last_text = trim(adjustl(range(comma_one + 1:)))
        else
            if (extra_comma /= 0) then
                ok = .false.
                message = "bounded Do accepts only a two- or three-item range"
                return
            end if
            first_text = trim(adjustl(range(comma_one + 1:comma_two - 1)))
            last_text = trim(adjustl(range(comma_two + 1:)))
        end if
        read (first_text, *, iostat=ios) first_value
        if (ios /= 0) then
            ok = .false.
            message = "bounded Do lower bound must be an exact integer"
            return
        end if
        read (last_text, *, iostat=ios) last_value
        if (ios /= 0) then
            ok = .false.
            message = "bounded Do upper bound must be an exact integer"
            return
        end if
        if (last_value < first_value) then
            ok = .false.
            message = "bounded Do empty ranges are not representable"
            return
        end if
        if (symbol_occurs(rhs, lhs)) then
            ok = .false.
            message = "bounded Do body may not read its assignment target"
            return
        end if
        write (replacement_buffer, "(i0)") last_value
        replacement = trim(replacement_buffer)
        lowered = lhs//" = "//replace_symbol(rhs, iterator, replacement)
    end subroutine lower_bounded_do_statement

    !> Lower a stateless bounded For assignment to its final iteration.
    !>
    !> A straight-line emitter cannot represent a runtime loop, but a body that
    !> overwrites a different scalar target and never reads that target has the
    !> same result as its last iteration. Only exact integer bounds and unit
    !> increments are accepted; all other For forms remain refused. Both
    !> inclusive and nonempty strict bounds are supported.
    subroutine lower_bounded_for_statement(text, lowered, handled, ok, message)
        character(*),              intent(in)  :: text
        character(:), allocatable, intent(out) :: lowered
        logical,                   intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: whole, args, start, test, step, body
        character(:), allocatable :: iterator, start_name, start_text
        character(:), allocatable :: bound_name, lhs, rhs, test_operator
        character(32) :: replacement_buffer
        integer :: open, close, comma_one, comma_two, comma_three
        integer :: eq, width, relation, operator_width
        integer :: start_value, bound_value, final_value, ios
        logical :: descending

        lowered = ""
        handled = .false.
        ok = .true.
        message = ""
        whole = trim(adjustl(text))
        open = index(whole, "[")
        if (open <= 1) return
        if (whole(:open - 1) /= "For") return
        handled = .true.
        close = matching_close(whole, open, "[", "]")
        if (close /= len(whole)) then
            ok = .false.
            message = "bounded For must have one balanced argument list"
            return
        end if

        args = whole(open + 1:close - 1)
        call next_top_level_comma(args, 1, comma_one)
        if (comma_one == 0) then
            ok = .false.
            message = "bounded For needs start, test, increment, and body"
            return
        end if
        call next_top_level_comma(args, comma_one + 1, comma_two)
        call next_top_level_comma(args, comma_two + 1, comma_three)
        if (comma_two == 0 .or. comma_three == 0) then
            ok = .false.
            message = "bounded For needs start, test, increment, and body"
            return
        end if
        call next_top_level_comma(args, comma_three + 1, relation)
        if (relation /= 0) then
            ok = .false.
            message = "bounded For accepts exactly four arguments"
            return
        end if

        start = trim(adjustl(args(:comma_one - 1)))
        test = trim(adjustl(args(comma_one + 1:comma_two - 1)))
        step = trim(adjustl(args(comma_two + 1:comma_three - 1)))
        body = trim(adjustl(args(comma_three + 1:)))

        eq = top_level_assignment(start)
        if (eq == 0) then
            ok = .false.
            message = "bounded For start must assign an exact integer"
            return
        end if
        width = 1
        if (start(eq:eq) == ":") width = 2
        start_name = trim(adjustl(start(:eq - 1)))
        start_text = trim(adjustl(start(eq + width:)))
        if (.not. valid_fortran_name(start_name)) then
            ok = .false.
            message = "bounded For iterator must be a Fortran name"
            return
        end if
        read (start_text, *, iostat=ios) start_value
        if (ios /= 0) then
            ok = .false.
            message = "bounded For start must be an exact integer"
            return
        end if

        relation = index(test, "<=")
        descending = .false.
        test_operator = "<="
        operator_width = 2
        if (relation == 0) then
            relation = index(test, "<")
            test_operator = "<"
            operator_width = 1
        end if
        if (relation == 0) then
            relation = index(test, ">=")
            descending = .true.
            test_operator = ">="
            operator_width = 2
        end if
        if (relation == 0) then
            relation = index(test, ">")
            descending = .true.
            test_operator = ">"
            operator_width = 1
        end if
        if (relation == 0) then
            ok = .false.
            message = "bounded For test must use an integer bound"
            return
        end if
        iterator = trim(adjustl(test(:relation - 1)))
        bound_name = trim(adjustl(test(relation + operator_width:)))
        if (iterator /= start_name) then
            ok = .false.
            message = "bounded For test must use its iterator"
            return
        end if
        read (bound_name, *, iostat=ios) bound_value
        if (ios /= 0) then
            ok = .false.
            message = "bounded For bound must be an exact integer"
            return
        end if

        if (descending) then
            if (step /= start_name//"--") then
                ok = .false.
                message = "bounded For requires a unit descending step"
                return
            end if
            if (test_operator == ">=") then
                if (start_value < bound_value) then
                    ok = .false.
                    message = "bounded For requires a nonempty descending range"
                    return
                end if
                final_value = bound_value
            else
                if (start_value <= bound_value .or. bound_value == huge(bound_value)) then
                    ok = .false.
                    message = "bounded For strict descending range is empty or overflows"
                    return
                end if
                final_value = bound_value + 1
            end if
        else
            if (step /= start_name//"++") then
                ok = .false.
                message = "bounded For requires a unit ascending step"
                return
            end if
            if (test_operator == "<=") then
                if (start_value > bound_value) then
                    ok = .false.
                    message = "bounded For requires a nonempty ascending range"
                    return
                end if
                final_value = bound_value
            else
                if (start_value >= bound_value .or. bound_value == -huge(bound_value) - 1) then
                    ok = .false.
                    message = "bounded For strict ascending range is empty or overflows"
                    return
                end if
                final_value = bound_value - 1
            end if
        end if

        eq = top_level_assignment(body)
        if (eq == 0) then
            ok = .false.
            message = "bounded For body must be a scalar assignment"
            return
        end if
        width = 1
        if (body(eq:eq) == ":") width = 2
        lhs = trim(adjustl(body(:eq - 1)))
        rhs = trim(adjustl(body(eq + width:)))
        if (.not. valid_target_name(lhs) .or. len(rhs) == 0) then
            ok = .false.
            message = "bounded For body must assign a Fortran scalar"
            return
        end if
        if (lhs == start_name .or. symbol_occurs(rhs, lhs)) then
            ok = .false.
            message = "bounded For body may not read its assignment target"
            return
        end if
        write (replacement_buffer, "(i0)") final_value
        lowered = lhs//" = "//replace_symbol(rhs, start_name, &
            trim(replacement_buffer))
    end subroutine lower_bounded_for_statement

    !> Return the matching closing delimiter, or zero for an unbalanced form.
    pure function matching_close(text, open, opening, closing) result(close)
        character(*), intent(in) :: text, opening, closing
        integer,      intent(in) :: open
        integer                  :: close
        integer :: depth, k

        close = 0
        depth = 0
        do k = open, len(text)
            if (text(k:k) == opening) then
                depth = depth + 1
            else if (text(k:k) == closing) then
                depth = depth - 1
                if (depth == 0) then
                    close = k
                    return
                end if
            end if
        end do
    end function matching_close

    pure function symbol_occurs(text, symbol) result(found)
        character(*), intent(in) :: text, symbol
        logical                  :: found
        integer :: k, last

        found = .false.
        if (len(symbol) == 0) return
        last = len(text) - len(symbol) + 1
        if (last < 1) return
        do k = 1, last
            if (text(k:k + len(symbol) - 1) /= symbol) cycle
            if (k > 1) then
                if (is_symbol_character(text(k - 1:k - 1))) cycle
            end if
            if (k + len(symbol) <= len(text)) then
                if (is_symbol_character(text(k + len(symbol):k + len(symbol)))) cycle
            end if
            found = .true.
            return
        end do
    end function symbol_occurs

    function replace_symbol(text, symbol, replacement) result(result)
        character(*), intent(in) :: text, symbol, replacement
        character(:), allocatable :: result
        integer :: k, n

        result = ""
        k = 1
        n = len(symbol)
        do while (k <= len(text))
            if (k + n - 1 <= len(text)) then
                if (text(k:k + n - 1) == symbol) then
                    if (.not. symbol_occurs_at(text, k, symbol)) then
                        result = result//text(k:k)
                        k = k + 1
                        cycle
                    end if
                    result = result//replacement
                    k = k + n
                    cycle
                end if
            end if
            result = result//text(k:k)
            k = k + 1
        end do
    end function replace_symbol

    pure function symbol_occurs_at(text, position, symbol) result(found)
        character(*), intent(in) :: text, symbol
        integer,      intent(in) :: position
        logical                  :: found

        found = .true.
        if (position > 1) then
            if (is_symbol_character(text(position - 1:position - 1))) then
                found = .false.
                return
            end if
        end if
        if (position + len(symbol) <= len(text)) then
            if (is_symbol_character(text(position + len(symbol):position + len(symbol)))) then
                found = .false.
            end if
        end if
    end function symbol_occurs_at

    pure function is_symbol_character(c) result(yes)
        character, intent(in) :: c
        logical                :: yes

        yes = is_letter(c) .or. is_digit(c) .or. c == "_" .or. c == "$"
    end function is_symbol_character

    !> Lower the bounded static form If[True|False, assignment, assignment].
    !> Symbolic conditions and all other branch shapes remain refused instead
    !> of being guessed or emitted as non-compilable Fortran control flow.
    subroutine lower_constant_if_statement(text, lowered, handled, ok, message)
        character(*),              intent(in)  :: text
        character(:), allocatable, intent(out) :: lowered
        logical,                   intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: whole, args, condition, yes_branch
        character(:), allocatable :: no_branch
        integer :: open, close, comma_one, comma_two, extra_comma
        integer :: depth, k

        lowered = ""
        handled = .false.
        ok = .true.
        message = ""
        whole = trim(adjustl(text))
        open = index(whole, "[")
        if (open <= 1) return
        if (whole(:open - 1) /= "If") return
        handled = .true.

        depth = 0
        close = 0
        do k = open, len(whole)
            if (whole(k:k) == "[") then
                depth = depth + 1
            else if (whole(k:k) == "]") then
                depth = depth - 1
                if (depth == 0) then
                    close = k
                    exit
                end if
            end if
        end do
        if (close /= len(whole)) then
            ok = .false.
            message = "If statement must have one balanced argument list"
            return
        end if

        args = whole(open + 1:close - 1)
        call next_top_level_comma(args, 1, comma_one)
        if (comma_one == 0) then
            ok = .false.
            message = "bounded If needs a condition and two assignments"
            return
        end if
        call next_top_level_comma(args, comma_one + 1, comma_two)
        if (comma_two == 0) then
            ok = .false.
            message = "bounded If needs a condition and two assignments"
            return
        end if
        call next_top_level_comma(args, comma_two + 1, extra_comma)
        if (extra_comma /= 0) then
            ok = .false.
            message = "bounded If accepts exactly two assignment branches"
            return
        end if

        condition = trim(adjustl(args(:comma_one - 1)))
        yes_branch = trim(adjustl(args(comma_one + 1:comma_two - 1)))
        no_branch = trim(adjustl(args(comma_two + 1:)))
        if (condition == "True") then
            lowered = yes_branch
        else if (condition == "False") then
            lowered = no_branch
        else
            handled = .false.
            return
        end if
        if (top_level_assignment(lowered) == 0) then
            ok = .false.
            message = "bounded If branches must be assignments"
            return
        end if
    end subroutine lower_constant_if_statement

    !> Emit the bounded dynamic form If[comparison, assignment, assignment].
    !>
    !> This is intentionally a complete, small source-to-source path rather
    !> than a conditional expression rewrite: both Wolfram branches are
    !> evaluated only when selected, just as they are by Fortran IF.  The
    !> condition is restricted to one scalar comparison so the emitted code
    !> remains independently auditable and cannot accidentally accept a
    !> symbolic Wolfram predicate as Fortran syntax.
    subroutine translate_dynamic_if_statement(text, code, handled, ok, message)
        character(*),              intent(in)  :: text
        type(str_t),               intent(out) :: code
        logical,                   intent(out) :: handled, ok
        character(:), allocatable, intent(out) :: message

        type(arena_t), target :: a
        type(expr_t) :: condition_left, condition_right, yes_root, no_root
        type(strbuf_t) :: b
        type(str_t), allocatable :: inputs(:)
        type(str_t) :: rendered_left, rendered_right, rendered_yes, rendered_no
        character(:), allocatable :: whole, args, condition, yes_branch, no_branch
        character(:), allocatable :: left_text, right_text, operator_text
        character(:), allocatable :: yes_lhs, yes_rhs, no_lhs, no_rhs
        character(:), allocatable :: parse_message
        integer :: open, close, comma_one, comma_two, extra_comma
        integer :: eq, width, ninputs
        logical :: parsed, good

        code = str("")
        handled = .false.
        ok = .true.
        message = ""
        whole = trim(adjustl(text))
        open = index(whole, "[")
        if (open <= 1) return
        if (whole(:open - 1) /= "If") return
        handled = .true.

        close = matching_close(whole, open, "[", "]")
        if (close /= len(whole)) then
            ok = .false.
            message = "If statement must have one balanced argument list"
            return
        end if
        args = whole(open + 1:close - 1)
        call next_top_level_comma(args, 1, comma_one)
        if (comma_one == 0) then
            ok = .false.
            message = "bounded If needs a condition and two assignments"
            return
        end if
        call next_top_level_comma(args, comma_one + 1, comma_two)
        if (comma_two == 0) then
            ok = .false.
            message = "bounded If needs a condition and two assignments"
            return
        end if
        call next_top_level_comma(args, comma_two + 1, extra_comma)
        if (extra_comma /= 0) then
            ok = .false.
            message = "bounded If accepts exactly two assignment branches"
            return
        end if

        condition = trim(adjustl(args(:comma_one - 1)))
        yes_branch = trim(adjustl(args(comma_one + 1:comma_two - 1)))
        no_branch = trim(adjustl(args(comma_two + 1:)))
        if (condition == "True" .or. condition == "False") then
            handled = .false.
            return
        end if

        if (index(condition, "&&") > 0 .or. index(condition, "||") > 0) then
            ok = .false.
            message = "bounded If condition must be one scalar comparison"
            return
        end if

        call split_scalar_comparison(condition, left_text, operator_text, right_text, good)
        if (.not. good) then
            ok = .false.
            message = "bounded If condition must be one scalar comparison"
            return
        end if
        if (operator_text == "!=") operator_text = "/="

        eq = top_level_assignment(yes_branch)
        if (eq == 0) then
            ok = .false.
            message = "bounded If branches must be assignments"
            return
        end if
        width = 1
        if (yes_branch(eq:eq) == ":") width = 2
        yes_lhs = trim(adjustl(yes_branch(:eq - 1)))
        yes_rhs = trim(adjustl(yes_branch(eq + width:)))

        eq = top_level_assignment(no_branch)
        if (eq == 0) then
            ok = .false.
            message = "bounded If branches must be assignments"
            return
        end if
        width = 1
        if (no_branch(eq:eq) == ":") width = 2
        no_lhs = trim(adjustl(no_branch(:eq - 1)))
        no_rhs = trim(adjustl(no_branch(eq + width:)))
        if (.not. valid_target_name(yes_lhs) .or. &
            .not. valid_target_name(no_lhs) .or. &
            .not. same_fortran_name(yes_lhs, no_lhs)) then
            ok = .false.
            message = "bounded If branches must assign the same Fortran scalar"
            return
        end if
        if (len(yes_rhs) == 0 .or. len(no_rhs) == 0) then
            ok = .false.
            message = "bounded If branches must have right-hand sides"
            return
        end if

        call a%init()
        condition_left = parse_expr_in(a, left_text, dialect(DIA_WOLFRAM), &
            parsed, parse_message)
        if (.not. parsed) then
            ok = .false.
            message = "cannot parse bounded If condition: "//parse_message
            return
        end if
        condition_right = parse_expr_in(a, right_text, dialect(DIA_WOLFRAM), &
            parsed, parse_message)
        if (.not. parsed) then
            ok = .false.
            message = "cannot parse bounded If condition: "//parse_message
            return
        end if
        yes_root = parse_expr_in(a, yes_rhs, dialect(DIA_WOLFRAM), parsed, parse_message)
        if (.not. parsed) then
            ok = .false.
            message = "cannot parse bounded If branch: "//parse_message
            return
        end if
        no_root = parse_expr_in(a, no_rhs, dialect(DIA_WOLFRAM), parsed, parse_message)
        if (.not. parsed) then
            ok = .false.
            message = "cannot parse bounded If branch: "//parse_message
            return
        end if

        if (.not. fortran_representable(condition_left) .or. &
            .not. fortran_representable(condition_right) .or. &
            .not. fortran_representable(yes_root) .or. &
            .not. fortran_representable(no_root)) then
            ok = .false.
            message = "bounded If contains an expression outside scalar Fortran grammar"
            return
        end if

        allocate (inputs(MAX_ASSIGNMENTS))
        ninputs = 0
        call collect_dynamic_if_inputs(condition_left, yes_lhs, inputs, ninputs, ok, message)
        if (.not. ok) return
        call collect_dynamic_if_inputs(condition_right, yes_lhs, inputs, ninputs, ok, message)
        if (.not. ok) return
        call collect_dynamic_if_inputs(yes_root, yes_lhs, inputs, ninputs, ok, message)
        if (.not. ok) return
        call collect_dynamic_if_inputs(no_root, yes_lhs, inputs, ninputs, ok, message)
        if (.not. ok) return

        rendered_left = print_expr_in(condition_left, dialect(DIA_FORTRAN), good)
        rendered_right = print_expr_in(condition_right, dialect(DIA_FORTRAN), good)
        rendered_yes = print_expr_in(yes_root, dialect(DIA_FORTRAN), good)
        rendered_no = print_expr_in(no_root, dialect(DIA_FORTRAN), good)
        if (.not. good) then
            ok = .false.
            message = "bounded If expression could not be emitted as Fortran"
            return
        end if

        call b%append("! Generated by fortsym. Do not edit.")
        call b%newline()
        call b%append("! Generator: fortsym_wl_f90")
        call b%newline()
        call b%newline()
        call b%append("subroutine fortsym_generated_assignment(")
        call append_dynamic_if_names(b, inputs, ninputs, yes_lhs)
        call b%append(")")
        call b%newline()
        call b%append("    use, intrinsic :: iso_fortran_env, only: dp => real64")
        call b%newline()
        call b%append("    implicit none")
        call b%newline()
        if (ninputs > 0) then
            call b%append("    real(dp), intent(in) :: ")
            call append_dynamic_if_names(b, inputs, ninputs)
            call b%newline()
        end if
        call b%append("    real(dp), intent(out) :: "//yes_lhs)
        call b%newline()
        call b%newline()
        call b%append("    if ("//chars(rendered_left)//" "// &
            operator_text//" "//chars(rendered_right)//") then")
        call b%newline()
        call b%append("        "//yes_lhs//" = "//chars(rendered_yes))
        call b%newline()
        call b%append("    else")
        call b%newline()
        call b%append("        "//yes_lhs//" = "//chars(rendered_no))
        call b%newline()
        call b%append("    end if")
        call b%newline()
        call b%append("end subroutine fortsym_generated_assignment")
        call b%newline()
        code = b%to_str()
    end subroutine translate_dynamic_if_statement

    subroutine collect_dynamic_if_inputs(root, target, inputs, ninputs, ok, message)
        type(expr_t),               intent(in)    :: root
        character(*),               intent(in)    :: target
        type(str_t),                intent(inout) :: inputs(:)
        integer,                    intent(inout) :: ninputs
        logical,                    intent(out)   :: ok
        character(:), allocatable,  intent(out)   :: message
        type(str_t), allocatable :: names(:)
        integer :: k

        ok = .true.
        message = ""
        names = free_symbols_of(root)
        do k = 1, size(names)
            if (.not. valid_input_name(chars(names(k)))) then
                ok = .false.
                message = "bounded If contains a non-Fortran symbol: "//chars(names(k))
                return
            end if
            if (same_fortran_name(chars(names(k)), target)) then
                ok = .false.
                message = "bounded If right-hand side may not read its output target"
                return
            end if
            if (.not. name_in_list(chars(names(k)), inputs, ninputs)) then
                if (ninputs >= size(inputs)) then
                    ok = .false.
                    message = "bounded If input symbol count exceeds the limit"
                    return
                end if
                ninputs = ninputs + 1
                inputs(ninputs) = names(k)
            end if
        end do
    end subroutine collect_dynamic_if_inputs

    subroutine append_dynamic_if_names(b, names, n, final_name)
        type(strbuf_t), intent(inout) :: b
        type(str_t),    intent(in)    :: names(:)
        integer,        intent(in)    :: n
        character(*),   intent(in), optional :: final_name
        integer :: k

        do k = 1, n
            if (k > 1) call b%append(", ")
            call b%append(chars(names(k)))
        end do
        if (present(final_name)) then
            if (n > 0) call b%append(", ")
            call b%append(final_name)
        end if
    end subroutine append_dynamic_if_names

    subroutine split_scalar_comparison(text, left, operator, right, ok)
        character(*),              intent(in)  :: text
        character(:), allocatable, intent(out) :: left, operator, right
        logical,                   intent(out) :: ok
        integer :: depth, k, width
        character(2) :: candidate

        left = ""
        operator = ""
        right = ""
        ok = .false.
        depth = 0
        k = 1
        do while (k <= len(text))
            select case (text(k:k))
            case ("[", "{", "("); depth = depth + 1
            case ("]", "}", ")"); depth = depth - 1
            case default
                if (depth == 0) then
                    width = 1
                    if (k < len(text)) then
                        candidate = text(k:k + 1)
                        if (candidate == "<=" .or. candidate == ">=" .or. &
                            candidate == "!=" .or. candidate == "==") width = 2
                    end if
                    if (text(k:k) == "<" .or. text(k:k) == ">" .or. &
                        (text(k:k) == "=" .and. width == 2) .or. &
                        (text(k:k) == "!" .and. width == 2)) then
                        if (len_trim(text(:k - 1)) == 0 .or. &
                            len_trim(text(k + width:)) == 0) return
                        left = trim(adjustl(text(:k - 1)))
                        operator = text(k:k + width - 1)
                        right = trim(adjustl(text(k + width:)))
                        ok = .true.
                        return
                    end if
                    if (width == 2) k = k + 1
                end if
            end select
            k = k + 1
        end do
    end subroutine split_scalar_comparison

    pure subroutine next_top_level_comma(text, first, position)
        character(*), intent(in)  :: text
        integer,      intent(in)  :: first
        integer,      intent(out) :: position
        integer :: depth, k
        character :: c

        position = 0
        depth = 0
        do k = first, len(text)
            c = text(k:k)
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case (",")
                if (depth == 0) then
                    position = k
                    return
                end if
            end select
        end do
    end subroutine next_top_level_comma

    !> Remove only balanced Wolfram comments at the beginning of the source.
    !> A comment is a lexical no-op, but comments elsewhere remain part of the
    !> candidate statement so unsupported source cannot be silently discarded.
    subroutine strip_leading_comments(source, cleaned, ok, message)
        character(*),              intent(in)  :: source
        character(:), allocatable, intent(out) :: cleaned
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: first, k, depth
        character :: c

        first = 1
        ok = .true.
        message = ""
        do
            do while (first <= len(source))
                c = source(first:first)
                if (c == " " .or. c == char(9) .or. c == char(10) .or. &
                    c == char(13)) then
                    first = first + 1
                else
                    exit
                end if
            end do
            if (first > len(source)) exit
            if (first + 1 > len(source)) exit
            if (source(first:first + 1) /= "(*") exit

            depth = 1
            k = first + 2
            do while (k <= len(source))
                if (k + 1 <= len(source)) then
                    if (source(k:k + 1) == "(*") then
                        depth = depth + 1
                        k = k + 2
                        cycle
                    end if
                    if (source(k:k + 1) == "*)") then
                        depth = depth - 1
                        k = k + 2
                        if (depth == 0) exit
                        cycle
                    end if
                end if
                k = k + 1
            end do
            if (depth /= 0) then
                ok = .false.
                message = "unclosed Wolfram comment in source preamble"
                return
            end if
            first = k
        end do

        if (first <= len(source)) then
            cleaned = source(first:)
        else
            cleaned = ""
        end if
    end subroutine strip_leading_comments

    !> Recognize only stateless notebook reset statements at the stream head.
    !> The translator starts with a fresh arena and never consults a persistent
    !> Wolfram symbol table, so clearing ordinary user symbols cannot change
    !> the scalar assignment expressions emitted below. Definitions, options,
    !> attributes, imports, and value assignments are deliberately not included.
    pure function safe_setup_statement(text) result(ok)
        character(*), intent(in) :: text
        logical                   :: ok
        character(:), allocatable :: whole, head, args, item
        integer :: open, close, k, start

        whole = trim(adjustl(text))
        ok = .false.
        if (len(whole) == 0) return
        open = index(whole, "[")
        close = len(whole)
        if (open <= 1 .or. close <= open + 1) return
        if (whole(close:close) /= "]") return

        head = trim(adjustl(whole(:open - 1)))
        args = trim(adjustl(whole(open + 1:close - 1)))
        if (head == "ClearAll" .and. &
            args == '"Global`*"') then
            ok = .true.
            return
        end if
        if (.not. (head == "Clear" .or. head == "ClearAll")) return
        if (len(args) == 0) return

        start = 1
        do k = 1, len(args)
            if (args(k:k) == ",") then
                if (k == start) return
                item = trim(adjustl(args(start:k - 1)))
                if (.not. safe_setup_name(item)) return
                start = k + 1
            end if
        end do
        if (start > len(args)) return
        item = trim(adjustl(args(start:)))
        if (.not. safe_setup_name(item)) return
        ok = .true.
    end function safe_setup_statement

    pure function safe_setup_name(name) result(ok)
        character(*), intent(in) :: name
        logical                   :: ok

        ok = valid_fortran_name(name)
        if (.not. ok) return
        if (same_fortran_name(name, GENERATED_NAME) .or. &
            same_fortran_name(name, "dp") .or. &
            name_starts_with(name, TEMP_PREFIX)) then
            ok = .false.
            return
        end if
        if (same_fortran_name(name, "i") .or. same_fortran_name(name, "pi") .or. &
            same_fortran_name(name, "e") .or. same_fortran_name(name, "sin") .or. &
            same_fortran_name(name, "cos") .or. same_fortran_name(name, "tan") .or. &
            same_fortran_name(name, "asin") .or. same_fortran_name(name, "acos") .or. &
            same_fortran_name(name, "atan") .or. same_fortran_name(name, "sinh") .or. &
            same_fortran_name(name, "cosh") .or. same_fortran_name(name, "tanh") .or. &
            same_fortran_name(name, "asinh") .or. same_fortran_name(name, "acosh") .or. &
            same_fortran_name(name, "atanh") .or. same_fortran_name(name, "exp") .or. &
            same_fortran_name(name, "log") .or. same_fortran_name(name, "sqrt") .or. &
            same_fortran_name(name, "abs") .or. same_fortran_name(name, "erf") .or. &
            same_fortran_name(name, "erfc") .or. same_fortran_name(name, "gamma") .or. &
            same_fortran_name(name, "atan2") .or. same_fortran_name(name, "min") .or. &
            same_fortran_name(name, "max") .or. same_fortran_name(name, "clear") .or. &
            same_fortran_name(name, "clearall")) ok = .false.
    end function safe_setup_name

    !> Split only at top-level semicolons and newlines. Wolfram comments and
    !> nested brackets are skipped, so a multiline function call remains one
    !> candidate statement while a normal assignment stream remains readable.
    subroutine split_stream(source, statements, n, ok, message)
        character(*),              intent(in)  :: source
        type(str_t), allocatable,  intent(out) :: statements(:)
        integer,                   intent(out) :: n
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: k, start, depth, comment_depth
        character :: c

        allocate (statements(MAX_ASSIGNMENTS))
        n = 0
        ok = .true.
        message = ""
        start = 1
        depth = 0
        comment_depth = 0
        k = 1
        do while (k <= len(source))
            c = source(k:k)
            if (comment_depth > 0) then
                if (c == "(") then
                    if (k < len(source)) then
                        if (source(k + 1:k + 1) == "*") then
                            comment_depth = comment_depth + 1
                            k = k + 2
                            cycle
                        end if
                    end if
                end if
                if (c == "*") then
                    if (k < len(source)) then
                        if (source(k + 1:k + 1) == ")") then
                            comment_depth = comment_depth - 1
                            k = k + 2
                            cycle
                        end if
                    end if
                end if
                k = k + 1
                cycle
            end if

            if (c == "(") then
                if (k < len(source)) then
                    if (source(k + 1:k + 1) == "*") then
                        comment_depth = 1
                        k = k + 2
                        cycle
                    end if
                end if
            end if
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case (";", char(10))
                if (depth == 0) then
                    call append_statement(source, start, k - 1, statements, n, &
                        ok, message)
                    if (.not. ok) return
                    start = k + 1
                end if
            end select
            k = k + 1
        end do
        call append_statement(source, start, len(source), statements, n, ok, message)
    end subroutine split_stream

    subroutine append_statement(source, first, last, statements, n, ok, message)
        character(*),              intent(in)    :: source
        integer,                   intent(in)    :: first, last
        type(str_t),               intent(inout) :: statements(:)
        integer,                   intent(inout) :: n
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: message
        character(:), allocatable :: statement

        ok = .true.
        message = ""
        if (last < first) return
        statement = trim(adjustl(source(first:last)))
        if (len(statement) == 0) return
        if (n >= size(statements)) then
            ok = .false.
            message = "assignment stream exceeds the bounded statement limit"
            return
        end if
        n = n + 1
        statements(n) = str(statement)
    end subroutine append_statement

    !> Find one top-level `=` or `:=`, excluding equations and comparisons.
    pure function top_level_assignment(text) result(pos)
        character(*), intent(in) :: text
        integer                  :: pos
        integer :: k, depth, comment_depth
        character :: c

        pos = 0
        depth = 0
        comment_depth = 0
        k = 1
        do while (k <= len(text))
            c = text(k:k)
            if (comment_depth > 0) then
                if (c == "*") then
                    if (k < len(text)) then
                        if (text(k + 1:k + 1) == ")") then
                            comment_depth = comment_depth - 1
                            k = k + 2
                            cycle
                        end if
                    end if
                end if
                k = k + 1
                cycle
            end if
            if (c == "(") then
                if (k < len(text)) then
                    if (text(k + 1:k + 1) == "*") then
                        comment_depth = 1
                        k = k + 2
                        cycle
                    end if
                end if
            end if
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case ("=")
                if (depth /= 0) then
                    k = k + 1
                    cycle
                end if
                if (k > 1) then
                    if (index("=<>!+-*/", text(k - 1:k - 1)) > 0) then
                        k = k + 1
                        cycle
                    end if
                end if
                if (k < len(text)) then
                    if (text(k + 1:k + 1) == "=") then
                        k = k + 1
                        cycle
                    end if
                end if
                pos = k
                if (k > 1) then
                    if (text(k - 1:k - 1) == ":") pos = k - 1
                end if
                return
            end select
            k = k + 1
        end do
    end function top_level_assignment

    pure function valid_target_name(name) result(ok)
        character(*), intent(in) :: name
        logical                   :: ok
        ok = valid_fortran_name(name)
        if (.not. ok) return
        if (same_fortran_name(name, GENERATED_NAME) .or. &
            same_fortran_name(name, "dp") .or. name_starts_with(name, TEMP_PREFIX)) then
            ok = .false.
        end if
    end function valid_target_name

    pure function valid_input_name(name) result(ok)
        character(*), intent(in) :: name
        logical                   :: ok
        ok = valid_target_name(name)
    end function valid_input_name

    pure function same_fortran_name(left, right) result(yes)
        character(*), intent(in) :: left, right
        logical                   :: yes
        integer :: k

        yes = len(left) == len(right)
        if (.not. yes) return
        do k = 1, len(left)
            if (lower_ascii(left(k:k)) /= lower_ascii(right(k:k))) then
                yes = .false.
                return
            end if
        end do
    end function same_fortran_name

    pure function name_starts_with(name, prefix) result(yes)
        character(*), intent(in) :: name, prefix
        logical                   :: yes
        integer :: k

        yes = len(name) >= len(prefix)
        if (.not. yes) return
        do k = 1, len(prefix)
            if (lower_ascii(name(k:k)) /= lower_ascii(prefix(k:k))) then
                yes = .false.
                return
            end if
        end do
    end function name_starts_with

    pure function lower_ascii(c) result(lower)
        character, intent(in) :: c
        character             :: lower
        if (c >= "A" .and. c <= "Z") then
            lower = achar(iachar(c) + iachar("a") - iachar("A"))
        else
            lower = c
        end if
    end function lower_ascii

    pure function name_in_list(name, values, n) result(yes)
        character(*), intent(in) :: name
        type(str_t),  intent(in) :: values(:)
        integer,      intent(in) :: n
        logical                   :: yes
        integer :: k

        yes = .false.
        do k = 1, n
            if (same_fortran_name(name, chars(values(k)))) then
                yes = .true.
                return
            end if
        end do
    end function name_in_list

    !> The printer can render opaque heads, but that would not be compilable
    !> Fortran. Keep this translator's accepted expression grammar explicit.
    recursive function supported_fortran_expression(e) result(ok)
        type(expr_t), intent(in) :: e
        logical                  :: ok
        integer :: k
        character(:), allocatable :: name

        ok = .true.
        if (e%kind() == NK_CONST) then
            name = chars(e%name())
            if (.not. same_fortran_name(name, "pi") .and. &
                .not. same_fortran_name(name, "e")) then
                ok = .false.
                return
            end if
        else if (e%kind() == NK_FUNC) then
            name = chars(e%name())
            select case (name)
            case ("sin", "cos", "tan", "asin", "acos", "atan", "sinh", &
                    "cosh", "tanh", "asinh", "acosh", "atanh", "exp", "log", &
                    "sqrt", "abs", "erf", "erfc", "gamma")
                if (e%nargs() /= 1) then
                    ok = .false.
                    return
                end if
            case ("atan2")
                if (e%nargs() /= 2) then
                    ok = .false.
                    return
                end if
            case ("Min", "Max")
                ! Fortran's MIN/MAX intrinsics require at least two scalar
                ! arguments; keep unary Wolfram forms refused rather than
                ! emitting a call that will not compile.
                if (e%nargs() < 2) then
                    ok = .false.
                    return
                end if
            case default
                ok = .false.
                return
            end select
        end if

        do k = 1, e%nargs()
            if (.not. supported_fortran_expression(e%arg(k))) then
                ok = .false.
                return
            end if
        end do
    end function supported_fortran_expression

    !> Fortran identifiers are deliberately checked byte-wise: extended
    !> Wolfram names are not portable source identifiers for this emitter.
    pure function valid_fortran_name(name) result(ok)
        character(*), intent(in) :: name
        logical                   :: ok
        integer :: k
        character :: c

        ok = len(name) > 0 .and. len(name) <= 63
        if (.not. ok) return
        c = name(1:1)
        if (.not. is_letter(c)) then
            ok = .false.
            return
        end if
        do k = 2, len(name)
            c = name(k:k)
            if (.not. (is_letter(c) .or. is_digit(c) .or. c == "_")) then
                ok = .false.
                return
            end if
        end do
    end function valid_fortran_name

    pure function is_letter(c) result(ok)
        character, intent(in) :: c
        logical                :: ok
        ok = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z")
    end function is_letter

    pure function is_digit(c) result(ok)
        character, intent(in) :: c
        logical                :: ok
        ok = c >= "0" .and. c <= "9"
    end function is_digit

end module fortsym_wl_f90
