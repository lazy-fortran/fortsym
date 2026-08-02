module fortsym_wl_f90
    ! Bounded source-to-source translation for a Wolfram assignment stream.
    !
    ! This is deliberately smaller than the native Wolfram interpreter: it
    ! accepts a short stream of top-level `name = expression` (or `:=`)
    ! statements, infers scalar inputs, expands references to earlier
    ! assignments, and delegates Fortran emission to the existing kernel
    ! generator. Unsupported source is refused explicitly.
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t, NK_CONST, NK_FUNC
    use fortsym_expr, only: expr_t, sym
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_eval, only: free_symbols_of
    use fortsym_subs, only: subs_many
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
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

        call a%init()
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
                call lower_bounded_do_statement(statement, cleaned_source, handled, &
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
