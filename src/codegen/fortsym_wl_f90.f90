module fortsym_wl_f90
    ! Bounded source-to-source translation for one Wolfram assignment.
    !
    ! This is deliberately smaller than the native Wolfram interpreter: it
    ! accepts one top-level `name = expression` (or `:=`) statement, infers
    ! scalar inputs from the expression, and delegates Fortran emission to the
    ! existing kernel generator. Unsupported source is refused explicitly.
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_eval, only: free_symbols_of
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    implicit none
    private

    public :: translate_wl_assignment

contains

    !> Translate one bounded Wolfram assignment to a complete Fortran
    !> subroutine. The returned source is empty when ok is false.
    function translate_wl_assignment(source, ok, message) result(code)
        character(*),              intent(in)  :: source
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t)                            :: code

        type(arena_t), target :: a
        type(expr_t) :: root
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        type(str_t), allocatable :: names(:)
        character(:), allocatable :: statement, lhs, rhs, parse_message
        integer :: eq, width, k
        logical :: parsed, representable, multiple

        code = str("")
        ok = .false.
        message = ""

        statement = trim(adjustl(source))
        if (len(statement) > 0) then
            if (statement(len(statement):len(statement)) == ";") then
                statement = trim(statement(:len(statement) - 1))
            end if
        end if
        if (len(statement) == 0) then
            message = "expected one assignment"
            return
        end if

        eq = top_level_assignment(statement)
        if (eq == 0) then
            message = "expected one top-level name = expression assignment"
            return
        end if
        width = 1
        if (statement(eq:eq) == ":") width = 2
        multiple = top_level_semicolon(statement, eq + width) /= 0
        if (.not. multiple .and. eq + width <= len(statement)) then
            multiple = top_level_assignment(statement(eq + width:)) /= 0
        end if
        if (multiple) then
            message = "multiple statements are outside the bounded translator"
            return
        end if

        lhs = trim(statement(:eq - 1))
        rhs = trim(statement(eq + width:))
        if (.not. valid_fortran_name(lhs)) then
            message = "assignment target is not a Fortran scalar name"
            return
        end if
        if (len(rhs) == 0) then
            message = "assignment has no right-hand side"
            return
        end if

        call a%init()
        root = parse_expr_in(a, rhs, dialect(DIA_WOLFRAM), parsed, parse_message)
        if (.not. parsed) then
            message = "cannot parse right-hand side: "//parse_message
            return
        end if

        names = free_symbols_of(root)
        do k = 1, size(names)
            if (.not. valid_fortran_name(chars(names(k)))) then
                message = "right-hand side contains a non-Fortran symbol: "// &
                    chars(names(k))
                return
            end if
            if (chars(names(k)) == lhs) then
                message = "recursive assignment target is unsupported"
                return
            end if
        end do

        roots(1) = root
        spec%name = str("fortsym_generated_assignment")
        spec%mode = KERNEL_SUBROUTINE
        spec%generator = str("fortsym_wl_f90")
        spec%regenerate_command = str("fortsym_wl_to_f90 input.wl output.f90")
        spec%temp_prefix = str("t")
        allocate (spec%args(size(names)), spec%outputs(1))
        if (size(names) > 0) spec%args = names
        spec%outputs(1) = str(lhs)

        representable = .true.
        code = emit_kernel(roots, spec, representable)
        if (.not. representable .or. len(chars(code)) == 0) then
            message = "expression is outside representable scalar Fortran"
            code = str("")
            return
        end if
        ok = .true.
    end function translate_wl_assignment

    !> Find one top-level `=` or `:=`, excluding equations and comparisons.
    pure function top_level_assignment(text) result(pos)
        character(*), intent(in) :: text
        integer                  :: pos
        integer :: k, depth
        character :: c

        pos = 0
        depth = 0
        do k = 1, len(text)
            c = text(k:k)
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case ("=")
                if (depth /= 0) cycle
                if (k > 1) then
                    if (index("=<>!+-*/", text(k - 1:k - 1)) > 0) cycle
                end if
                if (k < len(text)) then
                    if (text(k + 1:k + 1) == "=") cycle
                end if
                pos = k
                if (k > 1) then
                    if (text(k - 1:k - 1) == ":") pos = k - 1
                end if
                return
            end select
        end do
    end function top_level_assignment

    pure function top_level_semicolon(text, first) result(pos)
        character(*), intent(in) :: text
        integer,      intent(in)  :: first
        integer                   :: pos
        integer :: k, depth
        character :: c

        pos = 0
        depth = 0
        do k = first, len(text)
            c = text(k:k)
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case (";")
                if (depth == 0) then
                    pos = k
                    return
                end if
            end select
        end do
    end function top_level_semicolon

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
