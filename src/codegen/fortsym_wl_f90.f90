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
        integer :: nold
        logical :: parsed, representable
        type(expr_t) :: root

        code = str("")
        ok = .false.
        message = ""

        call strip_leading_comments(source, cleaned_source, parsed, message)
        if (.not. parsed) return
        call split_stream(cleaned_source, statements, nstatements, parsed, message)
        if (.not. parsed) return
        if (nstatements == 0) then
            message = "expected at least one top-level name = expression assignment"
            return
        end if

        call a%init()
        allocate (roots(nstatements), old_symbols(nstatements))
        allocate (targets(nstatements))
        nold = 0
        do k = 1, nstatements
            eq = top_level_assignment(chars(statements(k)))
            if (eq == 0) then
                message = "expected a top-level name = expression assignment; "// &
                    "control flow and other statements are unsupported"
                return
            end if

            statement = chars(statements(k))
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
