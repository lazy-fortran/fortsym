module fortsym_fparse
    ! Reading expressions back out of Fortran source.
    !
    ! This closes the loop. Without it, checking a hand-written kernel against
    ! its symbolic definition means copying the Fortran into the checker by
    ! hand -- which is what libneo's verify_gs_derivatives.py does, keeping a
    ! dict of Fortran source as Python strings, replicated across some fifteen
    ! worktrees. The transcription is the fragile part: it silently goes stale
    ! the moment the real source changes, so the check passes while testing an
    ! expression nobody computes any more.
    !
    ! Here the real file is read. A check that passes has checked the code that
    ! actually runs.
    !
    ! Scope is still deliberately narrow: right-hand-side expressions and
    ! parameter array constructors. It does not parse statements, control flow,
    ! implied-do constructors or typed constructors; a full Fortran parser would
    ! be a far larger commitment than the job requires -- see the note on
    ! fortfront in issue #9.
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t, NK_FUNC
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect, DIA_FORTRAN
    use fortsym_parse, only: parse_expr_in
    implicit none
    private

    public :: parse_fortran_expr, parse_fortran_array, find_assignment, logical_lines

contains

    !> Read `file`, find the assignment to `symbol`, and parse its right-hand
    !> side.
    !>
    !> `ok` is false with a message when the file cannot be read, the symbol has
    !> no assignment, or the right-hand side is outside the supported subset.
    !> None of those is fatal: a caller checking a kernel wants to be told which
    !> expression it could not read, not to have the run stop.
    function parse_fortran_expr(a, file, symbol, ok, message) result(e)
        type(arena_t), target,     intent(inout) :: a
        character(*),              intent(in)    :: file, symbol
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: message
        type(expr_t)                             :: e

        type(str_t), allocatable :: lines(:)
        character(:), allocatable :: rhs
        integer :: n

        call logical_lines(file, lines, n, ok, message)
        if (.not. ok) return

        call find_assignment(lines, n, symbol, rhs, ok, message)
        if (.not. ok) return

        e = parse_expr_in(a, rhs, dialect(DIA_FORTRAN), ok, message)
        if (.not. ok) message = "in assignment to '"//symbol//"': "//message
    end function parse_fortran_expr

    !> Read a rank-one array constructor bound to `symbol` and return its
    !> elements as independent arena expressions. Implied-do, typed and nested
    !> constructors are refused rather than partially interpreted.
    subroutine parse_fortran_array(a, file, symbol, elements, n, ok, message)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: file, symbol
        type(expr_t), allocatable, intent(out) :: elements(:)
        integer, intent(out) :: n
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(expr_t) :: constructor
        type(str_t), allocatable :: elements_lines(:)
        character(:), allocatable :: rhs
        integer :: k, line_count

        allocate (elements(0))
        n = 0
        call logical_lines(file, elements_lines, line_count, ok, message)
        if (.not. ok) return
        call find_assignment(elements_lines, line_count, symbol, rhs, ok, message)
        if (.not. ok) return
        constructor = parse_expr_in(a, rhs, dialect(DIA_FORTRAN), ok, message)
        if (.not. ok) return
        if (constructor%kind() /= NK_FUNC .or. &
            chars(constructor%name()) /= "List") then
            ok = .false.
            message = "'"//symbol//"' is not a rank-one array constructor"
            return
        end if

        n = constructor%nargs()
        deallocate (elements)
        allocate (elements(n))
        do k = 1, n
            if (contains_array_constructor(constructor%arg(k))) then
                n = 0
                deallocate (elements)
                allocate (elements(0))
                ok = .false.
                message = "nested array constructors are unsupported"
                return
            end if
            elements(k) = constructor%arg(k)
        end do
        ok = .true.
    end subroutine parse_fortran_array

    recursive function contains_array_constructor(e) result(found)
        type(expr_t), intent(in) :: e
        type(expr_t) :: child
        logical :: found
        integer :: k

        found = .false.
        if (e%kind() == NK_FUNC .and. chars(e%name()) == "List") then
            found = .true.
            return
        end if
        do k = 1, e%nargs()
            child = e%arg(k)
            if (contains_array_constructor(child)) then
                found = .true.
                return
            end if
        end do
    end function contains_array_constructor

    !> Read a file into logical lines: comments removed and continuations
    !> joined, so each element is one complete statement's text.
    !>
    !> Both steps are necessary before anything can be found by name. A
    !> generated kernel wraps long expressions across continuations, and an
    !> assignment split over three lines would otherwise be invisible.
    subroutine logical_lines(file, lines, n, ok, message)
        character(*),              intent(in)  :: file
        type(str_t), allocatable,  intent(out) :: lines(:)
        integer,                   intent(out) :: n
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        type(str_t), allocatable :: buffer(:), bigger(:)
        type(strbuf_t) :: pending
        character(len=4096) :: raw
        character(:), allocatable :: text
        integer :: unit, ios
        logical :: continuing

        n = 0
        ok = .false.
        message = ""
        allocate (buffer(64))
        continuing = .false.

        open (newunit=unit, file=file, status="old", action="read", iostat=ios)
        if (ios /= 0) then
            allocate (lines(0))
            message = "cannot open '"//file//"'"
            return
        end if

        do
            read (unit, "(a)", iostat=ios) raw
            if (ios /= 0) exit

            text = strip_comment(rstrip(raw))
            if (len(text) == 0 .and. .not. continuing) cycle

            if (ends_with_ampersand(text)) then
                ! Drop the ampersand and keep accumulating.
                call pending%append(without_last(text))
                continuing = .true.
                cycle
            end if

            ! A continuation line may also *begin* with an ampersand, which is
            ! not part of the expression.
            call pending%append(strip_leading_ampersand(text, continuing))
            continuing = .false.

            if (n >= size(buffer)) then
                allocate (bigger(size(buffer)*2))
                bigger(1:n) = buffer(1:n)
                call move_alloc(bigger, buffer)
            end if
            n = n + 1
            buffer(n) = str(pending%chars())
            call pending%reset()
        end do

        close (unit)

        allocate (lines(n))
        if (n > 0) lines(1:n) = buffer(1:n)
        ok = .true.
    end subroutine logical_lines

    !> Right-hand side of the assignment or declaration initializer for `symbol`.
    !>
    !> The last assignment wins. A kernel may assign a temporary more than once,
    !> and the value that reaches the output is the final one.
    subroutine find_assignment(lines, n, symbol, rhs, ok, message)
        type(str_t),               intent(in)  :: lines(:)
        integer,                   intent(in)  :: n
        character(*),              intent(in)  :: symbol
        character(:), allocatable, intent(out) :: rhs
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: text, lhs, candidate
        logical :: found_declaration, pointer_declaration, saw_pointer
        integer :: k, eq

        ok = .false.
        rhs = ""
        message = ""
        saw_pointer = .false.

        do k = 1, n
            text = chars(lines(k))
            if (index(text, "::") > 0) then
                call declaration_binding(text, symbol, candidate, &
                    found_declaration, pointer_declaration)
                saw_pointer = saw_pointer .or. pointer_declaration
                if (found_declaration .and. len(candidate) > 0) then
                    rhs = candidate
                    ok = .true.
                end if
                cycle
            end if
            eq = assignment_position(text)
            if (eq == 0) cycle

            lhs = squeeze(text(1:eq - 1))
            if (.not. same_name(lhs, symbol)) cycle

            candidate = lstrip(text(eq + 1:))
            if (len(candidate) == 0) cycle

            rhs = candidate
            ok = .true.
        end do

        if (.not. ok) then
            if (saw_pointer) then
                message = "pointer initialisation for '"//symbol//"' is unsupported"
            else
                message = "no assignment to '"//symbol//"' found"
            end if
        end if
    end subroutine find_assignment

    subroutine declaration_binding(text, symbol, rhs, found, pointer)
        character(*), intent(in) :: text, symbol
        character(:), allocatable, intent(out) :: rhs
        logical, intent(out) :: found, pointer
        character(:), allocatable :: entities, entity
        integer :: separator, start, k, paren_depth, bracket_depth
        logical :: at_end, entity_found, entity_pointer

        rhs = ""
        found = .false.
        pointer = .false.
        separator = index(text, "::")
        if (separator == 0) return
        entities = lstrip(text(separator + 2:))
        start = 1
        paren_depth = 0
        bracket_depth = 0
        do k = 1, len(entities) + 1
            at_end = k > len(entities)
            if (.not. at_end) then
                if (entities(k:k) == "(") paren_depth = paren_depth + 1
                if (entities(k:k) == ")") paren_depth = paren_depth - 1
                if (entities(k:k) == "[") bracket_depth = bracket_depth + 1
                if (entities(k:k) == "]") bracket_depth = bracket_depth - 1
            end if
            if (at_end) then
                if (k > start) then
                    entity = squeeze(entities(start:k - 1))
                    call inspect_entity(entity, symbol, rhs, entity_found, entity_pointer)
                    pointer = pointer .or. entity_pointer
                    if (entity_found) then
                        found = .true.
                        return
                    end if
                end if
                exit
            else if (entities(k:k) == "," .and. paren_depth == 0 .and. &
                bracket_depth == 0) then
                if (k > start) then
                    entity = squeeze(entities(start:k - 1))
                    call inspect_entity(entity, symbol, rhs, entity_found, entity_pointer)
                    pointer = pointer .or. entity_pointer
                    if (entity_found) then
                        found = .true.
                        return
                    end if
                end if
                start = k + 1
            end if
        end do
    end subroutine declaration_binding

    subroutine inspect_entity(entity, symbol, rhs, found, pointer)
        character(*), intent(in) :: entity, symbol
        character(:), allocatable, intent(out) :: rhs
        logical, intent(out) :: found, pointer
        character(:), allocatable :: name
        integer :: eq, open

        rhs = ""
        found = .false.
        pointer = .false.
        eq = assignment_position(entity)
        if (eq == 0) then
            if (index(entity, "=>") > 0) then
                pointer = .true.
                name = squeeze(entity(:index(entity, "=>") - 1))
            else
                name = squeeze(entity)
            end if
        else
            name = squeeze(entity(:eq - 1))
        end if
        open = index(name, "(")
        if (open > 0) name = squeeze(name(:open - 1))
        if (.not. same_name(name, symbol)) return
        if (eq == 0 .or. pointer) return
        rhs = lstrip(entity(eq + 1:))
        found = len(rhs) > 0
    end subroutine inspect_entity

    !> Position of the `=` that makes this an assignment, or 0.
    !>
    !> Skips `==`, `/=`, `<=`, `>=` and any `=` inside parentheses, so a keyword
    !> argument or a comparison is never mistaken for an assignment. Fortran's
    !> pointer assignment `=>` is excluded too: it binds a name, it does not
    !> compute a value.
    function assignment_position(text) result(pos)
        character(*), intent(in) :: text
        integer                  :: pos
        integer :: k, depth
        character :: c

        pos = 0
        depth = 0

        do k = 1, len(text)
            c = text(k:k)
            if (c == "(") depth = depth + 1
            if (c == ")") depth = depth - 1
            if (depth /= 0) cycle
            if (c /= "=") cycle

            if (k > 1) then
                if (index("=/<>", text(k - 1:k - 1)) > 0) cycle
            end if
            if (k < len(text)) then
                if (text(k + 1:k + 1) == "=") cycle
                if (text(k + 1:k + 1) == ">") cycle
            end if

            pos = k
            return
        end do
    end function assignment_position

    !> Fortran is case-insensitive, so DPSI and dpsi are the same name. A
    !> case-sensitive comparison here would silently fail to find assignments in
    !> hand-written source that spells them differently from the caller.
    pure function same_name(a, b) result(yes)
        character(*), intent(in) :: a, b
        logical                  :: yes
        yes = .false.
        if (len(a) /= len(b)) return
        yes = lower(a) == lower(b)
    end function same_name

    pure function lower(s) result(r)
        character(*), intent(in)  :: s
        character(len=len(s))     :: r
        integer :: k, code
        r = s
        do k = 1, len(s)
            code = iachar(s(k:k))
            if (code >= iachar("A") .and. code <= iachar("Z")) &
                r(k:k) = achar(code + 32)
        end do
    end function lower

    ! ------------------------------------------------------------ text --

    !> Remove a trailing comment, respecting quoted strings so an exclamation
    !> mark inside a literal is not treated as one.
    pure function strip_comment(text) result(r)
        character(*), intent(in)  :: text
        character(:), allocatable :: r
        integer :: k
        character :: c, quote
        logical :: in_string

        in_string = .false.
        quote = " "

        do k = 1, len(text)
            c = text(k:k)
            if (in_string) then
                if (c == quote) in_string = .false.
            else if (c == '"' .or. c == "'") then
                in_string = .true.
                quote = c
            else if (c == "!") then
                r = rstrip(text(1:k - 1))
                return
            end if
        end do

        r = text
    end function strip_comment

    pure function ends_with_ampersand(text) result(yes)
        character(*), intent(in) :: text
        logical                  :: yes
        yes = .false.
        if (len(text) == 0) return
        yes = text(len(text):len(text)) == "&"
    end function ends_with_ampersand

    pure function without_last(text) result(r)
        character(*), intent(in)  :: text
        character(:), allocatable :: r
        r = rstrip(text(1:len(text) - 1))
    end function without_last

    pure function strip_leading_ampersand(text, continuing) result(r)
        character(*), intent(in)  :: text
        logical,      intent(in)  :: continuing
        character(:), allocatable :: r
        character(:), allocatable :: t
        t = lstrip(text)
        if (continuing .and. len(t) > 0) then
            if (t(1:1) == "&") then
                r = lstrip(t(2:))
                return
            end if
        end if
        r = t
    end function strip_leading_ampersand

    pure function rstrip(text) result(r)
        character(*), intent(in)  :: text
        character(:), allocatable :: r
        integer :: n
        n = len(text)
        do while (n > 0)
            if (text(n:n) /= " " .and. text(n:n) /= achar(9)) exit
            n = n - 1
        end do
        r = text(1:n)
    end function rstrip

    pure function lstrip(text) result(r)
        character(*), intent(in)  :: text
        character(:), allocatable :: r
        integer :: k
        k = 1
        do while (k <= len(text))
            if (text(k:k) /= " " .and. text(k:k) /= achar(9)) exit
            k = k + 1
        end do
        r = text(k:)
    end function lstrip

    !> Strip surrounding blanks. Used on the left-hand side, where any interior
    !> blank would mean this is not a simple name and the match should fail.
    pure function squeeze(text) result(r)
        character(*), intent(in)  :: text
        character(:), allocatable :: r
        r = lstrip(rstrip(text))
    end function squeeze

end module fortsym_fparse
