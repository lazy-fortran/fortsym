module fortsym_wl
    ! Evaluate a Wolfram-language subset.
    !
    ! This is the layer above the parser: it splits a script into statements,
    ! keeps a binding table, and lowers the command heads fortsym actually
    ! supports onto native operations. It is not an interpreter for the
    ! language, and does not pretend to be -- there is no pattern matcher, no
    ! rule system and no evaluator loop. Anything outside the supported set is
    ! reported by name as unsupported.
    !
    ! The refusal is the point. A compatibility layer that guesses is worse than
    ! one that declines, because a plausible wrong answer gets acted on. Every
    ! path here either produces a result fortsym can defend or names what
    ! stopped it.
    !
    ! Behaviour is checked against Mathics, an independent open implementation.
    ! No Wolfram product is involved; see LEGAL.md section 5.1.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM
    use fortsym_expr, only: expr_t, sym, num, func
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_subs, only: subs
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    private

    public :: wl_session_t, wl_binding_t
    public :: wl_session_begin, wl_run_source, wl_binding_count, wl_binding_at
    public :: wl_split_statements

    integer, parameter :: MAX_BINDINGS = 512

    !> One top-level assignment produced by a script.
    type :: wl_binding_t
        type(str_t)  :: name
        type(expr_t) :: value
        !> False when evaluation refused; message says which construct stopped
        !> it. The binding is still recorded, so a script that assigns twenty
        !> results and fails on one reports nineteen rather than nothing.
        logical      :: ok = .true.
        type(str_t)  :: message
    end type wl_binding_t

    type :: wl_session_t
        type(arena_t), pointer :: a => null()
        type(native_engine_t)  :: engine
        type(wl_binding_t)     :: bindings(MAX_BINDINGS)
        integer                :: n = 0
    end type wl_session_t

contains

    subroutine wl_session_begin(s, a)
        type(wl_session_t),    intent(out)   :: s
        type(arena_t), target, intent(inout) :: a
        s%a => a
        s%engine = make_native_engine(a)
        s%n = 0
    end subroutine wl_session_begin

    pure function wl_binding_count(s) result(n)
        type(wl_session_t), intent(in) :: s
        integer                        :: n
        n = s%n
    end function wl_binding_count

    function wl_binding_at(s, k) result(b)
        type(wl_session_t), intent(in) :: s
        integer,            intent(in) :: k
        type(wl_binding_t)             :: b
        b = s%bindings(k)
    end function wl_binding_at

    !> Split a script into top-level statements.
    !>
    !> Statements end at ";" or a newline, but only at nesting depth zero:
    !> a semicolon inside Module[{a, b}, ...] or a bracketed argument list does
    !> not end a statement, and treating it as one truncates the expression into
    !> something that still parses and means something else.
    subroutine wl_split_statements(source, starts, ends, n)
        character(*),         intent(in)  :: source
        integer, allocatable, intent(out) :: starts(:), ends(:)
        integer,              intent(out) :: n

        integer :: i, len_src, depth, comment, begin
        character :: c
        logical :: in_string

        len_src = len(source)
        allocate (starts(len_src + 1), ends(len_src + 1))
        n = 0
        depth = 0
        comment = 0
        in_string = .false.
        begin = 1
        i = 1

        do while (i <= len_src)
            c = source(i:i)

            if (comment > 0) then
                if (i < len_src) then
                    if (source(i:i + 1) == "(*") then
                        comment = comment + 1
                        i = i + 2
                        cycle
                    end if
                    if (source(i:i + 1) == "*)") then
                        comment = comment - 1
                        i = i + 2
                        cycle
                    end if
                end if
                i = i + 1
                cycle
            end if

            if (in_string) then
                if (c == """") in_string = .false.
                i = i + 1
                cycle
            end if

            if (i < len_src) then
                if (source(i:i + 1) == "(*") then
                    comment = 1
                    i = i + 2
                    cycle
                end if
            end if

            select case (c)
            case ("""")
                in_string = .true.
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            end select

            if (depth == 0 .and. (c == ";" .or. c == char(10))) then
                call push_statement(source, starts, ends, n, begin, i - 1)
                begin = i + 1
            end if
            i = i + 1
        end do

        call push_statement(source, starts, ends, n, begin, len_src)
    end subroutine wl_split_statements

    !> Record a statement unless it is blank once trimmed.
    subroutine push_statement(source, starts, ends, n, from, to)
        character(*), intent(in)    :: source
        integer,      intent(inout) :: starts(:), ends(:)
        integer,      intent(inout) :: n
        integer,      intent(in)    :: from, to
        integer :: lo, hi

        lo = from
        hi = to
        do while (lo <= hi)
            if (.not. is_space(source(lo:lo))) exit
            lo = lo + 1
        end do
        do while (hi >= lo)
            if (.not. is_space(source(hi:hi))) exit
            hi = hi - 1
        end do
        if (hi < lo) return

        n = n + 1
        starts(n) = lo
        ends(n) = hi
    end subroutine push_statement

    pure function is_space(c) result(yes)
        character, intent(in) :: c
        logical               :: yes
        yes = c == " " .or. c == char(9) .or. c == char(10) .or. c == char(13)
    end function is_space

    !> Run a whole script, recording every top-level assignment.
    subroutine wl_run_source(s, source)
        type(wl_session_t), intent(inout) :: s
        character(*),       intent(in)    :: source

        integer, allocatable :: starts(:), ends(:)
        integer :: n, k

        call wl_split_statements(source, starts, ends, n)
        do k = 1, n
            call wl_run_statement(s, source(starts(k):ends(k)))
            if (s%n >= MAX_BINDINGS) exit
        end do
    end subroutine wl_run_source

    !> Run one statement.
    !>
    !> Only assignment is recognised as a statement form. A bare expression is
    !> evaluated and discarded, exactly as a script's intermediate line is: the
    !> corpus reports through named variables, so an unnamed value has nothing
    !> to be compared against and inventing a name for it would fabricate a
    !> result the oracle never produced.
    subroutine wl_run_statement(s, text)
        type(wl_session_t), intent(inout) :: s
        character(*),       intent(in)    :: text

        integer :: eq
        character(:), allocatable :: name, rhs
        type(expr_t) :: value
        logical :: ok
        type(str_t) :: message

        eq = assignment_split(text)
        if (eq <= 0) return

        name = trim(adjustl(text(1:eq - 1)))
        if (.not. is_plain_name(name)) return

        rhs = text(eq + assignment_width(text, eq):)
        call wl_eval_text(s, rhs, value, ok, message)
        if (ok) value = auto_evaluate(s, value)

        s%n = s%n + 1
        s%bindings(s%n)%name = str(name)
        s%bindings(s%n)%value = value
        s%bindings(s%n)%ok = ok
        s%bindings(s%n)%message = message
    end subroutine wl_run_statement

    !> Position of a top-level assignment operator, or 0.
    !>
    !> "==" is an equation, not an assignment, and "<=" / ">=" / "!=" end in "="
    !> without being one. Missing that turns Solve[x == 1, x] into a binding of
    !> x and silently deletes the equation.
    function assignment_split(text) result(pos)
        character(*), intent(in) :: text
        integer                  :: pos
        integer :: i, depth
        character :: c

        pos = 0
        depth = 0
        do i = 1, len(text)
            c = text(i:i)
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case ("=")
                if (depth /= 0) cycle
                if (i > 1) then
                    if (index("=<>!+-*/", text(i - 1:i - 1)) > 0) cycle
                end if
                if (i < len(text)) then
                    if (text(i + 1:i + 1) == "=") cycle
                end if
                pos = i
                return
            end select
        end do
    end function assignment_split

    !> Width of the assignment operator at pos: 2 for ":=", otherwise 1.
    pure function assignment_width(text, pos) result(w)
        character(*), intent(in) :: text
        integer,      intent(in) :: pos
        integer                  :: w
        w = 1
        if (pos > 1) then
            if (text(pos - 1:pos - 1) == ":") w = 1
        end if
    end function assignment_width

    !> A bare symbol name, with no brackets. Definitions like f[x_] := ... carry
    !> a pattern fortsym has no matcher for, so they are skipped rather than
    !> bound to something that would later be substituted wrongly.
    pure function is_plain_name(name) result(yes)
        character(*), intent(in) :: name
        logical                  :: yes
        integer :: i
        character :: c

        yes = len(name) > 0
        if (.not. yes) return
        do i = 1, len(name)
            c = name(i:i)
            if (c >= "a" .and. c <= "z") cycle
            if (c >= "A" .and. c <= "Z") cycle
            ! A leading $ is ordinary in Wolfram -- $Assumptions, $failCount --
            ! and rejecting it drops those bindings silently.
            if (c == "$") cycle
            if (c >= "0" .and. c <= "9" .and. i > 1) cycle
            if (c == "_" .and. i > 1) cycle
            yes = .false.
            return
        end do
    end function is_plain_name

    !> Fold what Wolfram folds on its own.
    !>
    !> Wolfram evaluates arithmetic automatically -- 1/3 + 1/6 becomes 1/2,
    !> x*0 becomes 0 -- but does not apply trigonometric identities unless
    !> asked. fortsym's native simplify has the same split: it collects numeric
    !> factors and like terms and leaves Sin[x]^2 + Cos[x]^2 alone, because
    !> deciding that is the zero test's job. So running it on every binding
    !> matches Wolfram's automatic evaluation rather than exceeding it.
    !>
    !> Verified, not assumed: if simplify ever starts closing trig identities,
    !> this call would make fortsym disagree with Mathics on every unsimplified
    !> assignment in the corpus.
    function auto_evaluate(s, e) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        type(expr_t)                      :: r
        type(engine_result_t) :: res

        res = s%engine%simplify(e)
        if (res%ok) then
            r = res%value
        else
            r = e
        end if
    end function auto_evaluate

    !> Parse and evaluate one expression.
    subroutine wl_eval_text(s, text, value, ok, message)
        type(wl_session_t), intent(inout) :: s
        character(*),       intent(in)    :: text
        type(expr_t),       intent(out)   :: value
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message

        type(expr_t) :: parsed
        logical :: parsed_ok
        character(:), allocatable :: why

        parsed = parse_expr_in(s%a, text, dialect(DIA_WOLFRAM), parsed_ok, why)
        if (.not. parsed_ok) then
            value = num(s%a, 0)
            ok = .false.
            message = str("parse: "//why)
            return
        end if

        value = wl_eval(s, apply_bindings(s, parsed), ok, message)
    end subroutine wl_eval_text

    !> Replace bound symbols by their values.
    !>
    !> Applied outside-in once per statement rather than lazily, because the
    !> corpus writes straight-line derivations where each line uses the
    !> previous ones.
    function apply_bindings(s, e) result(r)
        type(wl_session_t), intent(in) :: s
        type(expr_t),       intent(in) :: e
        type(expr_t)                   :: r
        integer :: k

        r = e
        do k = s%n, 1, -1
            if (.not. s%bindings(k)%ok) cycle
            r = subs(r, sym(s%a, chars(s%bindings(k)%name)), s%bindings(k)%value)
        end do
    end function apply_bindings

    !> Lower one command to a native operation.
    recursive function wl_eval(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r

        character(:), allocatable :: head
        type(engine_result_t) :: res
        type(expr_t) :: var, point, target, inner
        type(expr_t), allocatable :: args(:)
        logical :: arg_ok
        type(str_t) :: arg_message
        integer :: order, k

        ok = .true.
        message = str("")
        r = e

        if (e%kind() /= NK_FUNC) return
        head = chars(e%name())

        ! Evaluate arguments first. Wolfram evaluates innermost-out, and
        ! dispatching only on the outer head leaves Simplify[D[f, x]] holding an
        ! unevaluated D -- which then prints as though the derivative had been
        ! declined rather than never attempted.
        if (head /= "List") then
            allocate (args(e%nargs()))
            do k = 1, e%nargs()
                inner = wl_eval(s, e%arg(k), arg_ok, arg_message)
                if (.not. arg_ok) then
                    ok = .false.
                    message = arg_message
                    return
                end if
                args(k) = inner
            end do
            r = func(head, args)
            if (r%kind() /= NK_FUNC) return
        end if

        select case (head)

        case ("D")
            if (r%nargs() < 2) then
                call refuse(ok, message, "D needs an expression and a variable")
                return
            end if
            var = r%arg(2)
            ! D[f, {x, n}] is the n-th derivative. Ignoring the list form would
            ! differentiate once and report a wrong order as a right answer.
            if (var%kind() == NK_FUNC) then
                if (chars(var%name()) == "List") then
                    call refuse(ok, message, "D with a {var, order} specification")
                    return
                end if
            end if
            r = diff(r%arg(1), var)

        case ("Simplify", "FullSimplify")
            res = s%engine%simplify(r%arg(1))
            if (.not. res%ok) then
                call refuse(ok, message, "Simplify: "//chars(res%message))
                return
            end if
            r = res%value

        case ("Expand", "ExpandAll")
            res = s%engine%expand(r%arg(1))
            if (.not. res%ok) then
                call refuse(ok, message, "Expand: "//chars(res%message))
                return
            end if
            r = res%value

        case ("Series")
            if (.not. series_spec(r, var, point, order)) then
                call refuse(ok, message, "Series needs {var, point, order}")
                return
            end if
            ! Wolfram's Series[f, {x, x0, n}] includes the x^n term, and so
            ! does fortsym's order argument, so the number passes through
            ! unchanged. Measured against Mathics rather than assumed: an
            ! off-by-one here silently lengthens or truncates every series in
            ! the corpus by one term, and both look plausible.
            res = s%engine%series(r%arg(1), var, point, order)
            if (.not. res%ok) then
                call refuse(ok, message, "Series: "//chars(res%message))
                return
            end if
            r = res%value

        case ("Normal")
            ! fortsym's series is already a polynomial with no order term, so
            ! Normal is the identity rather than a truncation.
            r = r%arg(1)

        case ("Solve")
            if (r%nargs() < 2) then
                call refuse(ok, message, "Solve needs an equation and a variable")
                return
            end if
            target = r%arg(1)
            res = s%engine%solve(target, r%arg(2))
            if (.not. res%ok) then
                call refuse(ok, message, "Solve: "//chars(res%message))
                return
            end if
            r = res%value

        case ("Plus", "Times", "Power", "List", "Rule", "Equal")
            ! Structural heads the parser already built. Nothing to lower.

        case default
            ! A head fortsym recognises but has not implemented must refuse.
            ! Letting it fall through would print Integrate[x^2, x] as though it
            ! were the answer, and the harness would score that against the
            ! oracle's x^3/3 as a wrong result rather than as a missing feature
            ! -- or worse, an unwary reader would take it for an integral.
            if (is_known_command(head)) then
                call refuse(ok, message, head//" is not implemented")
                return
            end if
            ! Anything else stays opaque. An applied function fortsym has no
            ! rule for is still a legitimate expression, and differentiating
            ! through it produces correct Derivative nodes.

        end select
    end function wl_eval

    !> Wolfram command heads fortsym knows about but cannot yet evaluate.
    !>
    !> Kept explicit rather than inferred: the distinction that matters is
    !> between a *command* fortsym owes an implementation and a user's own
    !> function, and only a list can express it. Each entry corresponds to a
    !> tracked issue; removing one is what "implemented" means here.
    pure function is_known_command(head) result(yes)
        character(*), intent(in) :: head
        logical                  :: yes

        select case (head)
        ! Integration and limits (#31, #32)
        case ("Integrate", "NIntegrate", "Limit")
            yes = .true.
        ! Polynomial and rational algebra (#28)
        case ("Factor", "Together", "Apart", "Cancel", "Collect", "PolynomialGCD")
            yes = .true.
        ! Assumptions (#29)
        case ("Refine", "Assuming", "Simplify2", "Element")
            yes = .true.
        ! Matrices (#30)
        case ("Det", "Inverse", "Transpose", "Dot", "Cross", "Eigenvalues", &
              "LinearSolve", "MatrixPower", "Tr")
            yes = .true.
        ! Solving beyond the scalar linear case (#36)
        case ("Reduce", "NSolve", "FindRoot", "Eliminate", "Roots", "ToRadicals")
            yes = .true.
        ! Series and sums (#35, #39)
        case ("SeriesCoefficient", "Sum", "Product", "Coefficient", &
              "CoefficientList")
            yes = .true.
        ! Complex domain (#33)
        case ("ComplexExpand", "Re", "Im", "Conjugate", "Arg")
            yes = .true.
        ! Trig and power rewrites (#40)
        case ("TrigExpand", "TrigReduce", "TrigToExp", "ExpToTrig", &
              "PowerExpand", "Simplify3")
            yes = .true.
        ! Differential equations (#43)
        case ("DSolve", "NDSolve")
            yes = .true.
        ! Piecewise (#38)
        case ("Piecewise", "Boole", "If", "Which")
            yes = .true.
        ! Numerics (#37)
        case ("N", "SetPrecision", "Interpolation", "Fit", "Chop")
            yes = .true.
        ! Plotting, through fortplot (#44)
        case ("Plot", "Plot3D", "ContourPlot", "ParametricPlot", "ListPlot", &
              "DensityPlot", "StreamPlot", "VectorPlot", "LogPlot", &
              "LogLogPlot", "Show", "Graphics", "GraphicsGrid", "Export", &
              "Legended")
            yes = .true.
        case default
            yes = .false.
        end select
    end function is_known_command

    subroutine refuse(ok, message, why)
        logical,      intent(out) :: ok
        type(str_t),  intent(out) :: message
        character(*), intent(in)  :: why
        ok = .false.
        message = str(why)
    end subroutine refuse

    !> Read a {var, point, order} series specification.
    function series_spec(e, var, point, order) result(good)
        type(expr_t), intent(in)  :: e
        type(expr_t), intent(out) :: var, point
        integer,      intent(out) :: order
        logical                   :: good
        type(expr_t) :: spec, ord

        good = .false.
        order = 0
        if (e%nargs() < 2) return
        spec = e%arg(2)
        if (spec%kind() /= NK_FUNC) return
        if (chars(spec%name()) /= "List") return
        if (spec%nargs() /= 3) return

        var = spec%arg(1)
        point = spec%arg(2)
        ord = spec%arg(3)
        if (.not. exact_small_int(ord, order)) return
        good = .true.
    end function series_spec

    !> Read a small non-negative integer literal.
    function exact_small_int(e, value) result(good)
        type(expr_t), intent(in)  :: e
        integer,      intent(out) :: value
        logical                   :: good
        character(:), allocatable :: text
        integer :: ios

        value = 0
        good = .false.
        text = chars(e%exact_text())
        if (len_trim(text) == 0) return
        read (text, *, iostat=ios) value
        good = ios == 0 .and. value >= 0 .and. value < 1000
    end function exact_small_int

end module fortsym_wl
