module fortsym_wl
    ! Evaluate a Wolfram-language subset.
    !
    ! This is the layer above the parser: it splits a script into statements,
    ! keeps a binding table, and lowers the command heads fortsym actually
    ! supports onto native operations. It is not an interpreter for the
    ! language, and does not pretend to be -- there is no general pattern
    ! matcher, rule system or evaluator loop. A bounded subset of ordinary
    ! named pattern definitions is supported because generated tables use it;
    ! anything outside that subset is reported by name as unsupported.
    !
    ! The refusal is the point. A compatibility layer that guesses is worse than
    ! one that declines, because a plausible wrong answer gets acted on. Every
    ! path here either produces a result fortsym can defend or names what
    ! stopped it.
    !
    ! Behaviour is checked against Mathics, an independent open implementation.
    ! No Wolfram product is involved; see LEGAL.md section 5.1.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_FUNC, NK_SYM, NK_ADD, NK_MUL, NK_POW
    use fortsym_expr, only: expr_t, sym, num, func, func_in, real_expr, &
        operator(+), operator(-), operator(*), operator(**)
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_assume, only: assumption_context_t
    use fortsym_subs, only: subs
    use fortsym_diff, only: diff
    use fortsym_eval, only: free_symbols_of
    use fortsym_engine, only: engine_result_t, wall_seconds
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_matrix, only: matrix_transpose, matrix_dot, matrix_det, &
        matrix_inverse, is_matrix, is_list
    use fortsym_plot, only: plot_expression, plot_spec_t, read_plot_range, &
        plot_constant, curve_t, figure_data_t, CURVE_LINE, CURVE_POINTS, &
        sample_curve, sample_parametric_curve, render_figure, render_panels, &
        render_surface, render_contour, render_density, render_stream, &
        render_vector, set_grid_samples, set_arrow_samples
    use fortsym_numeric, only: numeric_value
    use fortsym_wl_solve, only: wl_solve
    use fortsym_wl_num, only: wl_n, wl_chop, wl_identity_matrix, wl_cross, &
        wl_trace, CHOP_DEFAULT
    use fortsym_integrate, only: integrate
    use fortsym_defint, only: definite_integral
    use fortsym_poly, only: poly_together, poly_cancel, poly_apart, &
        poly_factor, poly_coefficient, poly_coefficient_list, poly_collect, &
        poly_exponent, poly_gcd_expr, poly_divide, poly_numerator, &
        poly_denominator
    use fortsym_polysolve, only: solve_polynomial
    use fortsym_complexdom, only: re_part, im_part, conjugate, arg_of, &
        complex_expand
    use fortsym_sums, only: sum_closed_form, product_closed_form
    use fortsym_trigrewrite, only: trig_expand, trig_reduce, trig_to_exp, &
        exp_to_trig, power_expand
    use fortsym_limits, only: limit_of, limit_value_t, finite_point, &
        plus_infinity, minus_infinity, TWO_SIDED, LIMIT_FINITE, &
        LIMIT_PLUS_INF
    implicit none
    private

    public :: wl_session_t, wl_binding_t
    public :: wl_session_begin, wl_run_source, wl_binding_count, wl_binding_at
    public :: wl_split_statements

    integer, parameter :: MAX_BINDINGS = 512
    integer, parameter :: MAX_FUNCTIONS = 256
    integer, parameter :: MAX_FUNCTION_ARGS = 16
    integer, parameter :: MAX_TABLE_ITEMS = 10000
    integer, parameter :: MAX_PLOTS = 256
    integer, parameter :: dp = real64

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

    !> One simple f[x_, y_] := body definition.
    !>
    !> This is intentionally not a general DownValues implementation. The
    !> corpus uses named positional patterns for reusable algebraic bodies;
    !> storing those directly lets Table substitute concrete arguments without
    !> pretending to support conditions, defaults or sequence patterns.
    type :: wl_function_t
        type(str_t)  :: name
        type(str_t)  :: params(MAX_FUNCTION_ARGS)
        type(expr_t) :: body
        integer      :: nparams = 0
        logical      :: defined = .false.
    end type wl_function_t

    !> The curves behind one written plot file.
    !>
    !> Plot returns the file name, not this record: a graphics object that
    !> compares equal to another would let a script compare two plots as though
    !> they were expressions. The record is kept beside the session so Show
    !> can redraw the curves behind handles produced earlier in the script.
    type :: plot_record_t
        type(str_t)         :: file
        !> Surfaces and field plots have no curve list to merge. Show refuses
        !> them rather than silently showing only one argument.
        logical             :: overlayable = .false.
        type(figure_data_t) :: data
    end type plot_record_t

    type :: wl_session_t
        type(arena_t), pointer :: a => null()
        type(native_engine_t)  :: engine
        type(wl_binding_t)     :: bindings(MAX_BINDINGS)
        type(wl_function_t)    :: functions(MAX_FUNCTIONS)
        type(plot_record_t)    :: plots(MAX_PLOTS)
        integer                :: n = 0
        integer                :: nfunctions = 0
        integer                :: n_plots = 0
        !> Wall-clock budget for the whole script, in seconds. A script that
        !> exceeds it stops and says so. Hanging is the one outcome a
        !> verification tool must never have: a refusal names the construct
        !> that is missing, while a hang looks identical to hard work and gets
        !> killed by a harness timeout that records nothing.
        real(dp)               :: budget_seconds = 20.0_dp
        real(dp)               :: started = 0.0_dp
        !> Plots written so far, used to name the output files. A script
        !> plotting twenty curves must not overwrite one file nineteen times.
        integer                :: plot_count = 0
    end type wl_session_t

contains

    subroutine wl_session_begin(s, a)
        type(wl_session_t),    intent(out)   :: s
        type(arena_t), target, intent(inout) :: a
        s%a => a
        s%engine = make_native_engine(a)
        s%n = 0
        s%nfunctions = 0
        s%functions(:)%defined = .false.
        s%n_plots = 0
        s%started = wall_seconds()
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
                ! Associations are bracketed by <| and |>, but neither
                ! delimiter is a single bracket character. Count them here so
                ! commas in an association do not look like notebook cell
                ! separators.
                if (source(i:i + 1) == "<|") then
                    depth = depth + 1
                    i = i + 2
                    cycle
                end if
                if (source(i:i + 1) == "|>") then
                    depth = depth - 1
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

            if (depth == 0) then
                ! Notebook exports sometimes join cells with `, Null,` at
                ! top level. Commas inside brackets remain argument
                ! separators; only a depth-zero comma can delimit cells.
                if (c == ";" .or. c == ",") then
                    call push_statement(source, starts, ends, n, begin, i - 1)
                    begin = i + 1
                else if (c == char(10)) then
                    ! A newline ends a statement only when what precedes it can
                    ! stand alone. Wolfram continues a line that is still
                    ! waiting for a right operand, and the corpus is written
                    ! that way: long derivations are broken after a trailing
                    ! "+" or ",". Splitting unconditionally truncated those
                    ! into a prefix that still parsed as something else, so the
                    ! reported failure was "unexpected end of input" on a
                    ! script that was perfectly well formed.
                    if (.not. awaits_operand(source, begin, i - 1)) then
                        call push_statement(source, starts, ends, n, begin, &
                            i - 1)
                        begin = i + 1
                    end if
                end if
            end if
            i = i + 1
        end do

        call push_statement(source, starts, ends, n, begin, len_src)
    end subroutine wl_split_statements

    !> True when the text so far ends with something that needs a right
    !> operand, so the following newline continues the statement.
    !>
    !> Decided from the last significant character rather than by trial
    !> parsing. That is the conservative direction for this particular choice:
    !> a character that cannot end an expression is unambiguous evidence of
    !> continuation, whereas guessing from a failed parse would join two
    !> genuinely separate statements into one and change what the script means.
    !> A line ending in a closing bracket, a name or a number therefore always
    !> terminates, which is the common case.
    function awaits_operand(source, from, to) result(yes)
        character(*), intent(in) :: source
        integer,      intent(in) :: from, to
        logical                  :: yes
        integer :: k
        character :: c

        yes = .false.
        k = to
        do while (k >= from)
            c = source(k:k)
            if (c /= " " .and. c /= char(9) .and. c /= char(13)) exit
            k = k - 1
        end do
        if (k < from) return

        c = source(k:k)
        select case (c)
            ! Binary and prefix operators, an open bracket, and a comma: each one
            ! has a right-hand side that has not arrived yet.
            ! "&" and "!" are deliberately absent: both are postfix in Wolfram --
            ! "&" closes a pure function and "!" is factorial -- so a line ending
            ! in either is complete, and continuing it would glue the next
            ! statement on.
        case ("+", "-", "*", "/", "^", ",", "=", "<", ">", "|", &
                "@", ":", "~", "(", "[", "{")
            yes = .true.
        end select
    end function awaits_operand

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
        s%started = wall_seconds()
        do k = 1, n
            if (wall_seconds() - s%started > s%budget_seconds) then
                if (s%n < MAX_BINDINGS) then
                    s%n = s%n + 1
                    s%bindings(s%n)%name = str("<budget>")
                    s%bindings(s%n)%ok = .false.
                    s%bindings(s%n)%message = str("time budget exhausted; " // &
                        "remaining statements not evaluated")
                end if
                exit
            end if
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
    recursive subroutine wl_run_statement(s, text)
        type(wl_session_t), intent(inout) :: s
        character(*),       intent(in)    :: text

        integer :: comma, eq, lhs_end
        character(:), allocatable :: name, rhs, function_name
        type(expr_t) :: value
        logical :: ok, function_lhs_ok
        type(str_t) :: message

        ! A top-level comma is Wolfram's compound-expression separator. Commas
        ! inside calls and lists are protected by the same nesting depth that
        ! protects statement newlines, so recursively processing the pieces
        ! preserves left-to-right side effects such as Clear[..., Null, x=1].
        comma = top_level_comma(text)
        if (comma > 0) then
            call wl_run_statement(s, text(1:comma - 1))
            call wl_run_statement(s, text(comma + 1:))
            return
        end if

        eq = assignment_split(text)
        if (eq <= 0) then
            call wl_run_command(s, text)
            return
        end if

        lhs_end = eq - 1
        if (lhs_end > 0) then
            if (text(lhs_end:lhs_end) == ":") lhs_end = lhs_end - 1
        end if
        name = trim(adjustl(text(1:lhs_end)))

        rhs = text(eq + assignment_width(text, eq):)

        if (.not. is_plain_name(name)) then
            function_lhs_ok = function_lhs_name(name, function_name)
            if (.not. function_lhs_ok) return
            call define_function(s, name, rhs, ok, message)
            return
        end if

        call wl_eval_text(s, rhs, value, ok, message)
        if (ok) value = auto_evaluate(s, value)

        s%n = s%n + 1
        s%bindings(s%n)%name = str(name)
        s%bindings(s%n)%value = value
        s%bindings(s%n)%ok = ok
        s%bindings(s%n)%message = message
    end subroutine wl_run_statement

    !> Run the small set of bare commands with session-level side effects.
    subroutine wl_run_command(s, text)
        type(wl_session_t), intent(inout) :: s
        character(*),       intent(in)    :: text
        type(expr_t) :: parsed
        logical :: ok
        character(:), allocatable :: why

        parsed = parse_expr_in(s%a, text, dialect(DIA_WOLFRAM), ok, why)
        if (.not. ok) return
        if (parsed%kind() /= NK_FUNC) return

        select case (chars(parsed%name()))
        case ("Clear", "Remove")
            call clear_names(s, parsed)
        case default
            ! Bare expressions are deliberately discarded. Only named
            ! assignments are part of the benchmark output protocol.
        end select
    end subroutine wl_run_command

    !> Remove value and positional-function definitions for each named symbol.
    subroutine clear_names(s, command)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: command
        type(expr_t) :: item
        character(:), allocatable :: name
        integer :: k, j

        do k = 1, command%nargs()
            item = command%arg(k)
            if (item%kind() /= NK_SYM) cycle
            name = chars(item%name())

            j = 1
            do while (j <= s%n)
                if (chars(s%bindings(j)%name) == name) then
                    if (j < s%n) s%bindings(j:s%n - 1) = s%bindings(j + 1:s%n)
                    s%n = s%n - 1
                else
                    j = j + 1
                end if
            end do

            do j = 1, s%nfunctions
                if (s%functions(j)%defined) then
                    if (chars(s%functions(j)%name) == name) then
                        s%functions(j)%defined = .false.
                    end if
                end if
            end do
        end do
    end subroutine clear_names

    !> Position of the first comma outside brackets, braces and parentheses.
    function top_level_comma(text) result(pos)
        character(*), intent(in) :: text
        integer                  :: pos
        integer :: i, depth
        logical :: in_string
        character :: c

        pos = 0
        depth = 0
        in_string = .false.
        do i = 1, len(text)
            c = text(i:i)
            if (in_string) then
                if (c == """") in_string = .false.
                cycle
            end if
            if (c == """") then
                in_string = .true.
                cycle
            end if
            if (i < len(text)) then
                if (text(i:i + 1) == "<|") then
                    depth = depth + 1
                    cycle
                end if
                if (text(i:i + 1) == "|>") then
                    depth = depth - 1
                    cycle
                end if
            end if
            select case (c)
            case ("[", "{", "(")
                depth = depth + 1
            case ("]", "}", ")")
                depth = depth - 1
            case (",")
                if (depth == 0) then
                    pos = i
                    return
                end if
            end select
        end do
    end function top_level_comma

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

    !> A bare symbol name, with no brackets. Pattern definitions are parsed by
    !> the separate bounded positional-definition path below; this predicate
    !> remains strict so a function definition is never mistaken for a value
    !> binding.
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

    !> Validate f[x_, y_] and return its bare function name.
    function function_lhs_name(lhs, base) result(yes)
        character(*), intent(in) :: lhs
        character(:), allocatable, intent(out) :: base
        logical :: yes
        integer :: open, close, start, i
        character(:), allocatable :: pattern

        yes = .false.
        base = ""
        open = index(lhs, "[")
        close = len_trim(lhs)
        if (open <= 1 .or. close <= open) return
        if (lhs(close:close) /= "]") return
        if (scan(lhs(open + 1:close - 1), "[]") /= 0) return

        base = trim(adjustl(lhs(1:open - 1)))
        if (.not. is_plain_name(base)) then
            base = ""
            return
        end if

        start = open + 1
        do i = open + 1, close - 1
            if (lhs(i:i) /= ",") cycle
            if (.not. pattern_name(lhs, start, i - 1, pattern)) then
                base = ""
                return
            end if
            start = i + 1
        end do
        if (.not. pattern_name(lhs, start, close - 1, pattern)) then
            base = ""
            return
        end if
        yes = .true.
    end function function_lhs_name

    !> Extract the name from one ordinary name-pattern, such as x_.
    function pattern_name(text, from, to, name) result(yes)
        character(*), intent(in) :: text
        integer,      intent(in) :: from, to
        character(:), allocatable, intent(out) :: name
        logical :: yes
        character(:), allocatable :: piece
        integer :: n

        yes = .false.
        name = ""
        if (to < from) return
        piece = trim(adjustl(text(from:to)))
        n = len(piece)
        if (n < 2) return
        if (piece(n:n) /= "_") return
        if (n > 2) then
            if (piece(n - 1:n - 1) == "_") return
        end if
        name = piece(1:n - 1)
        if (.not. is_plain_name(name)) then
            name = ""
            return
        end if
        yes = .true.
    end function pattern_name

    !> Store a bounded positional pattern definition without adding a fake
    !> value binding to the result stream.
    subroutine define_function(s, lhs, rhs, ok, message)
        type(wl_session_t), intent(inout) :: s
        character(*),        intent(in)    :: lhs, rhs
        logical,             intent(out)   :: ok
        type(str_t),         intent(out)   :: message

        character(:), allocatable :: base, pattern
        type(expr_t) :: body
        logical :: parsed_ok, lhs_ok
        character(:), allocatable :: why
        integer :: open, close, start, i, nparams, slot, k

        ok = .false.
        message = str("")
        lhs_ok = function_lhs_name(lhs, base)
        if (.not. lhs_ok) then
            call refuse(ok, message, "unsupported function definition")
            return
        end if

        open = index(lhs, "[")
        close = len_trim(lhs)
        nparams = 1
        do i = open + 1, close - 1
            if (lhs(i:i) == ",") nparams = nparams + 1
        end do
        if (nparams > MAX_FUNCTION_ARGS) then
            call refuse(ok, message, "function definition has too many patterns")
            return
        end if

        body = parse_expr_in(s%a, rhs, dialect(DIA_WOLFRAM), parsed_ok, why)
        if (.not. parsed_ok) then
            call refuse(ok, message, "function definition: "//why)
            return
        end if

        slot = 0
        do k = s%nfunctions, 1, -1
            if (.not. s%functions(k)%defined) cycle
            if (chars(s%functions(k)%name) /= base) cycle
            if (s%functions(k)%nparams /= nparams) cycle
            slot = k
            exit
        end do
        if (slot == 0) then
            if (s%nfunctions >= MAX_FUNCTIONS) then
                call refuse(ok, message, "too many function definitions")
                return
            end if
            s%nfunctions = s%nfunctions + 1
            slot = s%nfunctions
        end if

        s%functions(slot)%name = str(base)
        s%functions(slot)%body = body
        s%functions(slot)%nparams = nparams
        s%functions(slot)%defined = .true.
        start = open + 1
        k = 1
        do i = open + 1, close - 1
            if (lhs(i:i) /= ",") cycle
            lhs_ok = pattern_name(lhs, start, i - 1, pattern)
            s%functions(slot)%params(k) = str(pattern)
            k = k + 1
            start = i + 1
        end do
        lhs_ok = pattern_name(lhs, start, close - 1, pattern)
        s%functions(slot)%params(k) = str(pattern)

        ok = .true.
    end subroutine define_function

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

        ! Table owns its iterator symbols. Applying global bindings first would
        ! turn Table[i, {i, 3}] into Table[99, {99, 3}] when a script happened
        ! to use i earlier; resolve other globals after each local index has
        ! been substituted instead.
        if (parsed%kind() == NK_FUNC .and. chars(parsed%name()) == "Table") then
            value = wl_eval(s, parsed, ok, message)
        else
            value = wl_eval(s, apply_bindings(s, parsed), ok, message)
        end if
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
        type(expr_t) :: var, point, target, inner, placeholder
        type(expr_t), allocatable :: args(:)
        logical :: arg_ok
        type(str_t) :: arg_message
        integer :: order, k, j, digits
        real(dp) :: value, tol
        character(:), allocatable :: why

        ok = .true.
        message = str("")
        r = e

        ! Checked inside the recursion, not only between statements: one
        ! Series-then-FullSimplify line in the KiLCA corpus ran for a minute on
        ! its own, so a per-statement check alone still leaves the process
        ! looking hung for as long as any single statement takes.
        if (wall_seconds() - s%started > s%budget_seconds) then
            call refuse(ok, message, "time budget exhausted")
            return
        end if

        ! Walk arithmetic nodes too. Dispatching only on NK_FUNC let an
        ! unimplemented head hide inside a sum or product: -Inverse[g] . r has
        ! Times at the top, so Dot was never examined and its unevaluated form
        ! was reported as though it were the answer.
        select case (e%kind())
        case (NK_ADD, NK_MUL, NK_POW)
            r = eval_children(s, e, ok, message)
            return
        case (NK_FUNC)
            ! fall through to the command table
        case default
            return
        end select
        head = chars(e%name())

        ! Table needs its iterator variables substituted before the body is
        ! evaluated. Running the generic innermost-out walk first would make
        ! Table[If[...], {i, 3}] refuse on If before it ever sees a value for
        ! i. The lowering routine is deliberately bounded: a verification
        ! frontend must decline a huge expansion rather than turn one source
        ! line into an accidental memory/time bomb.
        if (head == "Table") then
            r = lower_table(s, e, ok, message)
            return
        end if

        ! If is control flow, not an ordinary function: evaluating both
        ! branches before choosing one would recurse forever on definitions
        ! such as f[n_] := If[n <= 0, 1, n*f[n - 1]]. Decide the condition
        ! first and evaluate only the selected branch.
        if (head == "If") then
            r = lower_if(s, e, ok, message)
            return
        end if

        ! Evaluate arguments first. Wolfram evaluates innermost-out, and
        ! dispatching only on the outer head leaves Simplify[D[f, x]] holding an
        ! unevaluated D -- which then prints as though the derivative had been
        ! declined rather than never attempted.
        ! A zero-argument application such as Directory[] has nothing to
        ! evaluate, and rebuilding it through func() would take the arena from
        ! an argument that does not exist.
        if (head /= "List" .and. e%nargs() > 0) then
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
            inner = r%arg(1)
            ! Every argument after the first is a differentiation
            ! specification: D[f, x, y] is a mixed partial and D[f, {x, n}] is
            ! the n-th derivative. Taking only the second argument would
            ! differentiate once and report a lower order as the right answer.
            do k = 2, r%nargs()
                var = r%arg(k)
                if (var%kind() == NK_FUNC) then
                    if (chars(var%name()) /= "List") then
                        ! An opaque application such as q[t] is an independent
                        ! coordinate for a partial derivative. Rename it to a
                        ! fresh symbol, differentiate, then restore the node.
                        if (.not. is_opaque_application(var)) then
                            call refuse(ok, message, "D with a computed "// &
                                "variable: "//chars(var%name())// &
                                " is not an independent coordinate")
                            return
                        end if
                        placeholder = fresh_symbol(s%a, r)
                        inner = subs(inner, var, placeholder)
                        inner = diff(inner, placeholder)
                        inner = subs(inner, placeholder, var)
                        cycle
                    end if
                    if (var%nargs() /= 2) then
                        call refuse(ok, message, &
                            "D with a {var, order} of unexpected shape")
                        return
                    end if
                    if (.not. exact_small_int(var%arg(2), order)) then
                        call refuse(ok, message, "D with a symbolic order")
                        return
                    end if
                    do j = 1, order
                        inner = diff(inner, var%arg(1))
                    end do
                    cycle
                end if
                inner = diff(inner, var)
            end do
            r = inner

        case ("Module", "Block", "With")
            if (r%nargs() < 2) then
                call refuse(ok, message, head//" needs locals and a body")
                return
            end if
            r = scoped_body(s, r, ok, message)
            if (.not. ok) return

        case ("Simplify", "FullSimplify")
            res = s%engine%simplify(r%arg(1))
            if (.not. res%ok) then
                call refuse(ok, message, "Simplify: "//chars(res%message))
                return
            end if
            r = res%value
            ! The native arithmetic simplifier intentionally does not apply
            ! trigonometric identities during automatic evaluation. An
            ! explicit Wolfram Simplify is allowed to close this exact,
            ! assumption-free identity, matching the independent oracle.
            if (is_pythagorean_identity(r)) r = num(s%a, 1)

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
            inner = wl_solve(s%a, s%engine, r, arg_ok, why)
            if (.not. arg_ok) then
                call refuse(ok, message, "Solve: "//why)
                return
            end if
            r = inner

            ! Matrix heads on operands that are not concrete matrices stay
            ! unevaluated rather than refusing. That is not a guess: Transpose[jac]
            ! where jac is a symbol is exactly what Mathics returns too, so
            ! refusing would disagree with the oracle on a correct answer. A
            ! genuine shape error -- ragged rows, mismatched dimensions -- still
            ! refuses, because that is a mistake in the source rather than a
            ! symbol standing in for a matrix.
        case ("N")
            if (r%nargs() < 1 .or. r%nargs() > 2) then
                call refuse(ok, message, "N takes an expression and an "// &
                    "optional precision")
                return
            end if
            digits = 0
            if (r%nargs() == 2) then
                if (.not. exact_small_int(r%arg(2), digits)) then
                    call refuse(ok, message, "N with a symbolic precision")
                    return
                end if
                if (digits < 1) then
                    call refuse(ok, message, "N with a non-positive precision")
                    return
                end if
            end if
            inner = wl_n(s%a, r%arg(1), digits, ok, why)
            if (.not. ok) then
                call refuse(ok, message, "N: "//why)
                return
            end if
            r = inner

        case ("Chop")
            if (r%nargs() < 1 .or. r%nargs() > 2) then
                call refuse(ok, message, "Chop takes an expression and an "// &
                    "optional tolerance")
                return
            end if
            tol = CHOP_DEFAULT
            if (r%nargs() == 2) then
                call numeric_value(r%arg(2), tol, ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "Chop: "//why)
                    return
                end if
                if (tol < 0.0_dp) then
                    call refuse(ok, message, "Chop with a negative tolerance")
                    return
                end if
            end if
            r = wl_chop(s%a, r%arg(1), tol)

        case ("Cross")
            if (r%nargs() /= 2) then
                call refuse(ok, message, "Cross needs two vectors")
                return
            end if
            if (is_list(r%arg(1)) .and. is_list(r%arg(2))) then
                inner = wl_cross(s%a, r%arg(1), r%arg(2), ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "Cross: "//why)
                    return
                end if
                r = inner
            end if

        case ("Tr")
            if (r%nargs() /= 1) then
                call refuse(ok, message, "Tr with options is not implemented")
                return
            end if
            if (is_matrix(r%arg(1))) then
                inner = wl_trace(s%a, r%arg(1), ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "Tr: "//why)
                    return
                end if
                r = inner
            end if

        case ("Plot", "LogPlot", "LogLogPlot")
            if (r%nargs() < 2) then
                call refuse(ok, message, head//" needs an expression and a range")
                return
            end if
            r = render_plot(s, r, head, ok, message)
            if (.not. ok) return

        case ("ParametricPlot")
            if (r%nargs() < 2) then
                call refuse(ok, message, &
                    "ParametricPlot needs components and a range")
                return
            end if
            r = render_parametric(s, r, ok, message)
            if (.not. ok) return

        case ("ListPlot", "ListLinePlot")
            if (r%nargs() < 1) then
                call refuse(ok, message, head//" needs data")
                return
            end if
            r = render_list_plot(s, r, head == "ListLinePlot", ok, message)
            if (.not. ok) return

        case ("Plot3D", "ContourPlot", "DensityPlot", "StreamPlot", &
                "VectorPlot")
            if (r%nargs() < 3) then
                call refuse(ok, message, head//" needs an x and a y range")
                return
            end if
            r = render_field(s, r, head, ok, message)
            if (.not. ok) return

        case ("Show")
            r = render_overlay(s, r, ok, message)
            if (.not. ok) return

        case ("GraphicsRow", "GraphicsGrid", "GraphicsArray")
            r = render_grid(s, r, head, ok, message)
            if (.not. ok) return

        case ("Graphics", "Graphics3D")
            call refuse(ok, message, &
                head//" draws primitives (Line, Point, Text, ...), which "// &
                "fortsym does not represent")
            return

        case ("Legended")
            call refuse(ok, message, "Legended: legends are not rendered")
            return

        case ("Transpose")
            if (is_matrix(r%arg(1))) then
                r = matrix_transpose(s%a, r%arg(1), ok)
                if (.not. ok) then
                    call refuse(ok, message, "Transpose of a ragged matrix")
                    return
                end if
            end if

        case ("Dot")
            if (r%nargs() < 2) then
                call refuse(ok, message, "Dot needs two operands")
                return
            end if
            inner = r%arg(1)
            do k = 2, r%nargs()
                if (.not. dottable(inner) .or. .not. dottable(r%arg(k))) then
                    ! One operand is symbolic, so the product cannot be formed
                    ! and the whole application stays as written.
                    return
                end if
                inner = matrix_dot(s%a, inner, r%arg(k), ok, message)
                if (.not. ok) return
            end do
            r = inner

        case ("Det")
            if (is_matrix(r%arg(1))) then
                r = matrix_det(s%a, r%arg(1), ok, message)
                if (.not. ok) return
            end if

        case ("Inverse")
            if (is_matrix(r%arg(1))) then
                r = matrix_inverse(s%a, r%arg(1), ok, message)
                if (.not. ok) return
            end if

        case ("IdentityMatrix")
            if (r%nargs() /= 1) then
                call refuse(ok, message, "IdentityMatrix needs one size")
                return
            end if
            if (exact_small_int(r%arg(1), order)) then
                inner = wl_identity_matrix(s%a, order, ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "IdentityMatrix: "//why)
                    return
                end if
                r = inner
            end if

        case ("Integrate")
            r = lower_integrate(s, r, ok, message)
            if (.not. ok) return

        case ("Limit")
            r = lower_limit(s, r, ok, message)
            if (.not. ok) return

        case ("Sum", "Product")
            r = lower_sum(s, r, head == "Sum", ok, message)
            if (.not. ok) return

        case ("Map")
            r = lower_map(s, r, ok, message)
            if (.not. ok) return

            ! Lower case: the parser canonicalises Wolfram spellings to fortsym's
            ! own names, and the printer spells them back. Matching "Re" here fired
            ! never, and because the printer restored the Wolfram spelling the
            ! unevaluated result read exactly like a correct one.
        case ("re", "im", "conjugate", "arg", "ComplexExpand")
            r = lower_complex(s, r, head, ok, message)
            if (.not. ok) return

        case ("TrigExpand", "TrigReduce", "TrigToExp", "ExpToTrig", &
                "PowerExpand")
            r = lower_rewrite(s, r, head, ok, message)
            if (.not. ok) return

        case ("Part")
            r = lower_part(s, r, ok, message)
            if (.not. ok) return

        case ("Together", "Cancel", "Factor", "Apart", "Collect", &
                "Coefficient", "CoefficientList", "Exponent", &
                "PolynomialGCD", "PolynomialQuotient", "PolynomialRemainder")
            r = lower_polynomial(s, r, head, ok, message)
            if (.not. ok) return

        case ("Numerator")
            if (r%nargs() /= 1) then
                call refuse(ok, message, "Numerator takes one argument")
                return
            end if
            r = poly_numerator(s%a, r%arg(1))

        case ("Denominator")
            if (r%nargs() /= 1) then
                call refuse(ok, message, "Denominator takes one argument")
                return
            end if
            r = poly_denominator(s%a, r%arg(1))

        case ("Plus", "Times", "Power", "List", "Rule", "Equal")
            ! Structural heads the parser already built. Nothing to lower.

        case default
            if (has_function_definition(s, head, r%nargs())) then
                r = lower_user_function(s, r, ok, message)
                if (.not. ok) return
                return
            end if
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

    !> Integrate[f, spec, ...]. A spec is a bare variable x, a {x} that means
    !> the same, or a {x, a, b} that asks for a definite integral.
    !>
    !> Multiple specs are nested, with the last spec innermost. This preserves
    !> limits that refer to an outer integration variable.
    function lower_integrate(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t) :: spec, var, inner, stage
        character(:), allocatable :: why
        integer :: k

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() < 2) then
            call refuse(ok, message, "Integrate needs an expression and a "// &
                "variable")
            return
        end if

        inner = e%arg(1)
        do k = e%nargs(), 2, -1
            spec = e%arg(k)

            if (spec%kind() == NK_SYM) then
                inner = integrate(s%a, inner, spec, ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "Integrate: "//why)
                    return
                end if
                cycle
            end if

            if (spec%kind() /= NK_FUNC .or. chars(spec%name()) /= "List") then
                call refuse(ok, message, "Integrate needs a variable or a "// &
                    "{var, a, b} range")
                return
            end if

            var = spec%arg(1)
            if (var%kind() /= NK_SYM) then
                call refuse(ok, message, "Integrate needs a plain variable")
                return
            end if

            select case (spec%nargs())
            case (1)
                inner = integrate(s%a, inner, var, ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "Integrate: "//why)
                    return
                end if
            case (3)
                ! Use a temporary because the definite-integral routine has an
                ! intent(out) result and must read the integrand first.
                call definite_integral(s%a, inner, var, spec%arg(2), &
                    spec%arg(3), stage, ok, why)
                if (.not. ok) then
                    call refuse(ok, message, "definite Integrate: "//why)
                    return
                end if
                inner = stage
            case default
                call refuse(ok, message, "Integrate range must be {var} or "// &
                    "{var, a, b}")
                return
            end select
        end do

        r = inner
    end function lower_integrate

    !> Evaluate the decidable subset of Wolfram If[condition, yes, no].
    !>
    !> A symbolic condition is refused rather than guessed. Numeric relations
    !> are enough for the bounded recursive and piecewise definitions in the
    !> corpus, and keeping this helper narrow prevents assumptions from leaking
    !> into the ordinary expression evaluator.
    recursive function lower_if(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r, condition, branch
        logical :: arg_ok, decided, truth
        type(str_t) :: arg_message

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() < 2 .or. e%nargs() > 3) then
            call refuse(ok, message, "If needs a condition and two branches")
            return
        end if

        condition = wl_eval(s, e%arg(1), arg_ok, arg_message)
        if (.not. arg_ok) then
            call refuse(ok, message, chars(arg_message))
            return
        end if
        call condition_value(condition, decided, truth)
        if (.not. decided) then
            call refuse(ok, message, "If condition is not decidable")
            return
        end if

        if (truth) then
            branch = e%arg(2)
        else
            if (e%nargs() /= 3) then
                call refuse(ok, message, "If false branch is missing")
                return
            end if
            branch = e%arg(3)
        end if

        r = wl_eval(s, branch, ok, message)
    end function lower_if

    !> Map[f, {x1, x2, ...}] for a named positional function.
    !>
    !> The corpus uses this form for bounded data preparation. Pure functions,
    !> levels other than one, and non-list expressions remain explicit
    !> refusals rather than being approximated.
    recursive function lower_map(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r, mapper, data, mapped
        type(expr_t), allocatable         :: values(:)
        logical :: item_ok
        type(str_t) :: item_message
        integer :: k

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() /= 2) then
            call refuse(ok, message, "Map needs a function and a list")
            return
        end if
        mapper = e%arg(1)
        data = e%arg(2)
        if (mapper%kind() /= NK_SYM) then
            call refuse(ok, message, "Map needs a named function")
            return
        end if
        if (data%kind() /= NK_FUNC) then
            call refuse(ok, message, "Map needs a list")
            return
        end if
        if (chars(data%name()) /= "List") then
            call refuse(ok, message, "Map needs a list")
            return
        end if

        allocate (values(data%nargs()))
        do k = 1, data%nargs()
            mapped = wl_eval(s, func(chars(mapper%name()), [data%arg(k)]), &
                item_ok, item_message)
            if (.not. item_ok) then
                call refuse(ok, message, chars(item_message))
                return
            end if
            values(k) = mapped
        end do
        r = func("List", values)
    end function lower_map

    !> True when a positional pattern definition matches this call shape.
    function has_function_definition(s, name, nargs) result(yes)
        type(wl_session_t), intent(in) :: s
        character(*),        intent(in) :: name
        integer,             intent(in) :: nargs
        logical :: yes
        integer :: k

        yes = .false.
        do k = s%nfunctions, 1, -1
            if (.not. s%functions(k)%defined) cycle
            if (chars(s%functions(k)%name) /= name) cycle
            if (s%functions(k)%nparams /= nargs) cycle
            yes = .true.
            return
        end do
    end function has_function_definition

    !> Substitute a matched call into the stored body and evaluate it.
    recursive function lower_user_function(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r, body
        integer :: slot, k

        ok = .false.
        message = str("")
        r = e
        slot = 0
        do k = s%nfunctions, 1, -1
            if (.not. s%functions(k)%defined) cycle
            if (chars(s%functions(k)%name) /= chars(e%name())) cycle
            if (s%functions(k)%nparams /= e%nargs()) cycle
            slot = k
            exit
        end do
        if (slot == 0) then
            call refuse(ok, message, "no matching function definition")
            return
        end if

        body = s%functions(slot)%body
        do k = 1, e%nargs()
            body = subs(body, sym(s%a, chars(s%functions(slot)%params(k))), &
                e%arg(k))
        end do
        body = apply_bindings(s, body)
        r = wl_eval(s, body, ok, message)
        if (ok) r = auto_evaluate(s, r)
    end function lower_user_function

    !> Table[body, {i, n}], Table[body, {i, lo, hi}] and nested ranges.
    !>
    !> This is explicit enumeration, not a symbolic summation engine. It is
    !> the useful and defensible subset for a reference runner: every index
    !> value is substituted into the body and then sent through the same
    !> evaluator as a top-level expression. Unsupported bounds or a requested
    !> expansion beyond MAX_TABLE_ITEMS are refused rather than approximated.
    recursive function lower_table(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() < 2) then
            call refuse(ok, message, "Table needs a body and an iterator")
            return
        end if

        r = lower_table_level(s, e%arg(1), e, 2, ok, message)
    end function lower_table

    !> Lower one iterator and recurse through the remaining iterator specs.
    recursive function lower_table_level(s, body, table, level, ok, message) &
            result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: body, table
        integer,            intent(in)    :: level
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t), allocatable :: values(:), entries(:)
        type(expr_t) :: var, next_body, cell
        integer :: k

        ok = .true.
        message = str("")
        r = body

        if (level > table%nargs()) then
            r = wl_eval(s, apply_bindings(s, body), ok, message)
            if (ok) r = auto_evaluate(s, r)
            return
        end if

        call table_values(s, table%arg(level), var, values, ok, message)
        if (.not. ok) return

        allocate (entries(size(values)))
        do k = 1, size(values)
            next_body = subs(body, var, values(k))
            if (level == table%nargs()) then
                cell = wl_eval(s, apply_bindings(s, next_body), ok, message)
                if (ok) cell = auto_evaluate(s, cell)
            else
                cell = lower_table_level(s, next_body, table, level + 1, &
                    ok, message)
            end if
            if (.not. ok) then
                r = body
                return
            end if
            entries(k) = cell
        end do

        if (size(entries) == 0) then
            r = func_in(s%a, "List")
        else
            r = func("List", entries)
        end if
    end function lower_table_level

    !> Turn one Table iterator specification into its concrete values.
    subroutine table_values(s, spec, var, values, ok, message)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: spec
        type(expr_t),       intent(out)   :: var
        type(expr_t), allocatable, intent(out) :: values(:)
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message

        type(expr_t) :: explicit_values, item
        type(expr_t) :: bound
        integer(int64) :: lo, hi, step, count64, k64
        integer :: k
        type(str_t) :: why

        ok = .false.
        message = str("")
        var = spec

        if (spec%kind() /= NK_FUNC .or. chars(spec%name()) /= "List") then
            call refuse(ok, message, "Table needs a {var, range} iterator")
            return
        end if
        if (spec%nargs() < 2 .or. spec%nargs() > 4) then
            call refuse(ok, message, "Table iterator has an unsupported shape")
            return
        end if

        var = spec%arg(1)
        if (var%kind() /= NK_SYM) then
            call refuse(ok, message, "Table needs a plain index variable")
            return
        end if

        ! The explicit-value form, {i, {a, b, c}}, is common in generated
        ! scripts and costs no symbolic inference: evaluate each supplied
        ! value independently, then substitute it exactly as a range value.
        if (spec%nargs() == 2) then
            bound = apply_bindings(s, spec%arg(2))
            explicit_values = bound
            if (explicit_values%kind() == NK_FUNC .and. &
                chars(explicit_values%name()) == "List") then
                if (explicit_values%nargs() > MAX_TABLE_ITEMS) then
                    call refuse(ok, message, "Table expansion exceeds its bound")
                    return
                end if
                allocate (values(explicit_values%nargs()))
                do k = 1, explicit_values%nargs()
                    item = wl_eval(s, explicit_values%arg(k), ok, why)
                    if (.not. ok) then
                        call refuse(ok, message, "Table value: "//chars(why))
                        return
                    end if
                    values(k) = item
                end do
                ok = .true.
                return
            end if

            if (.not. exact_integer(bound, hi)) then
                call refuse(ok, message, "Table needs an integer upper bound")
                return
            end if
            lo = 1_int64
            step = 1_int64
        else
            bound = apply_bindings(s, spec%arg(2))
            if (.not. exact_integer(bound, lo)) then
                call refuse(ok, message, "Table needs integer range bounds")
                return
            end if
            bound = apply_bindings(s, spec%arg(3))
            if (.not. exact_integer(bound, hi)) then
                call refuse(ok, message, "Table needs integer range bounds")
                return
            end if
            step = 1_int64
            if (spec%nargs() == 4) then
                bound = apply_bindings(s, spec%arg(4))
                if (.not. exact_integer(bound, step)) then
                    call refuse(ok, message, "Table needs an integer step")
                    return
                end if
            end if
        end if

        if (step == 0_int64) then
            call refuse(ok, message, "Table step cannot be zero")
            return
        end if

        if ((step > 0_int64 .and. lo > hi) .or. &
            (step < 0_int64 .and. lo < hi)) then
            count64 = 0_int64
        else if (step > 0_int64) then
            count64 = (hi - lo)/step + 1_int64
        else
            count64 = (lo - hi)/(-step) + 1_int64
        end if

        if (count64 < 0_int64 .or. count64 > int(MAX_TABLE_ITEMS, int64)) then
            call refuse(ok, message, "Table expansion exceeds its bound")
            return
        end if

        allocate (values(int(count64)))
        do k = 1, size(values)
            k64 = int(k - 1, int64)
            values(k) = num(s%a, lo + k64*step)
        end do
        ok = .true.
    end subroutine table_values

    !> Read an exact integer literal without accepting a floating approximation.
    function exact_integer(e, value) result(good)
        type(expr_t), intent(in) :: e
        integer(int64), intent(out) :: value
        logical :: good
        character(:), allocatable :: text
        integer :: ios

        value = 0_int64
        good = .false.
        text = chars(e%exact_text())
        if (len_trim(text) == 0) return
        read (text, *, iostat=ios) value
        good = ios == 0
    end function exact_integer

    !> Limit[f, x -> a]. Only the two-sided form: Wolfram's Direction option
    !> selects a side, and answering a one-sided request with the two-sided
    !> limit is right only when both agree, which is the very thing a
    !> one-sided request suspects. So the option refuses.
    function lower_limit(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(limit_value_t) :: lv
        type(expr_t) :: rule, var, point
        character(:), allocatable :: why

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() /= 2) then
            call refuse(ok, message, "Limit with options such as Direction")
            return
        end if
        rule = e%arg(2)
        if (rule%kind() /= NK_FUNC) then
            call refuse(ok, message, "Limit needs a x -> a rule")
            return
        end if
        if (chars(rule%name()) /= "Rule") then
            call refuse(ok, message, "Limit needs a x -> a rule")
            return
        end if
        var = rule%arg(1)
        point = rule%arg(2)
        if (var%kind() /= NK_SYM) then
            call refuse(ok, message, "Limit needs a plain variable")
            return
        end if

        if (is_named(point, "Infinity")) then
            lv = limit_of(s%a, e%arg(1), var, plus_infinity(), TWO_SIDED, &
                ok, why)
        else if (is_negative_infinity(point)) then
            lv = limit_of(s%a, e%arg(1), var, minus_infinity(), TWO_SIDED, &
                ok, why)
        else
            lv = limit_of(s%a, e%arg(1), var, finite_point(point), TWO_SIDED, &
                ok, why)
        end if

        if (.not. ok) then
            call refuse(ok, message, "Limit: "//why)
            return
        end if

        ! An infinite limit is reported as Wolfram spells it, not as a large
        ! number: a caller that cannot tell Infinity from a float will use it
        ! in arithmetic.
        select case (lv%kind)
        case (LIMIT_FINITE)
            r = lv%value
        case (LIMIT_PLUS_INF)
            r = func_in(s%a, "Infinity")
        case default
            r = -func_in(s%a, "Infinity")
        end select
    end function lower_limit

    !> Sum[body, {k, lo, hi}] and Product likewise.
    function lower_sum(s, e, is_sum, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(in)    :: is_sum
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t) :: spec, var
        character(:), allocatable :: why

        ok = .true.
        message = str("")
        r = e

        ! A multi-index Sum has more than one iterator spec; nesting them here
        ! would need each inner closed form to survive the outer summation,
        ! which is not established, so it refuses.
        if (e%nargs() /= 2) then
            call refuse(ok, message, "Sum or Product over several indices")
            return
        end if
        spec = e%arg(2)
        if (spec%kind() /= NK_FUNC) then
            call refuse(ok, message, "Sum needs a {k, lo, hi} range")
            return
        end if
        if (chars(spec%name()) /= "List" .or. spec%nargs() /= 3) then
            call refuse(ok, message, "Sum needs a {k, lo, hi} range")
            return
        end if
        var = spec%arg(1)
        if (var%kind() /= NK_SYM) then
            call refuse(ok, message, "Sum needs a plain index variable")
            return
        end if

        if (is_sum) then
            r = sum_closed_form(s%a, e%arg(1), var, spec%arg(2), spec%arg(3), &
                ok, why)
        else
            r = product_closed_form(s%a, e%arg(1), var, spec%arg(2), &
                spec%arg(3), ok, why)
        end if
        if (.not. ok) then
            call refuse(ok, message, "Sum: "//why)
            return
        end if
    end function lower_sum

    !> Re, Im, Conjugate, Arg and ComplexExpand.
    !>
    !> The assumption context is empty, which is the whole difficulty: a bare
    !> symbol has unknown reality, so Re[x] refuses rather than returning x.
    !> Wolfram's ComplexExpand assumes every symbol real by default; copying
    !> that would make fortsym agree with the oracle by sharing its unsoundness,
    !> and the disagreement is the honest outcome until #29 carries assumptions
    !> in from the script.
    function lower_complex(s, e, head, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        character(*),       intent(in)    :: head
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(assumption_context_t) :: facts
        type(expr_t) :: out
        character(:), allocatable :: why

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() /= 1) then
            call refuse(ok, message, head//" takes one argument here")
            return
        end if

        select case (head)
        case ("re");           call re_part(e%arg(1), facts, out, ok, why)
        case ("im");           call im_part(e%arg(1), facts, out, ok, why)
        case ("conjugate");    call conjugate(e%arg(1), facts, out, ok, why)
        case ("arg");          call arg_of(e%arg(1), facts, out, ok, why)
        case default;          call complex_expand(e%arg(1), facts, out, ok, why)
        end select

        if (.not. ok) then
            call refuse(ok, message, head//": "//why)
            return
        end if
        r = out
    end function lower_complex

    !> The trig and power rewrite family.
    function lower_rewrite(s, e, head, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        character(*),       intent(in)    :: head
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t) :: out
        character(:), allocatable :: why

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() /= 1) then
            call refuse(ok, message, head//" takes one argument here")
            return
        end if

        select case (head)
        case ("TrigExpand");  call trig_expand(e%arg(1), out, ok, why)
        case ("TrigReduce");  call trig_reduce(e%arg(1), out, ok, why)
        case ("TrigToExp");   call trig_to_exp(e%arg(1), out, ok, why)
        case ("ExpToTrig");   call exp_to_trig(e%arg(1), out, ok, why)
        case default;         call power_expand(e%arg(1), out, ok, why)
        end select

        if (.not. ok) then
            call refuse(ok, message, head//": "//why)
            return
        end if
        r = out
    end function lower_rewrite

    !> Part[expr, i, j, ...] on explicit lists only. Indexing a literal list
    !> by positive integers is unambiguous; unsupported spans and non-lists
    !> refuse instead of being printed as a computed value.
    function lower_part(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        integer :: k, index

        ok = .true.
        message = str("")
        r = e

        if (e%nargs() < 2) then
            call refuse(ok, message, "Part without an index")
            return
        end if

        r = e%arg(1)
        do k = 2, e%nargs()
            if (r%kind() /= NK_FUNC .or. chars(r%name()) /= "List") then
                call refuse(ok, message, "Part of a non-list")
                return
            end if
            if (.not. exact_small_int(e%arg(k), index)) then
                call refuse(ok, message, "Part with a non-literal index")
                return
            end if
            if (index < 1 .or. index > r%nargs()) then
                call refuse(ok, message, "Part index out of range")
                return
            end if
            r = r%arg(index)
        end do
    end function lower_part

    !> Polynomial and rational-function heads, over exact rationals.
    !>
    !> Every one of these either produces a result fortsym_poly has checked --
    !> a cancelled fraction, a factorisation that multiplies back, a partial
    !> fraction expansion that recombines to the input -- or refuses with the
    !> reason. Nothing here falls through to the unevaluated form, which would
    !> print Factor[x^2 - 1] as though that were the factorisation.
    function lower_polynomial(s, e, head, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        character(*),        intent(in)   :: head
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t), allocatable :: list(:)
        type(expr_t) :: var, out
        character(:), allocatable :: why
        integer :: order

        r = e
        ok = .true.
        message = str("")

        select case (head)

        case ("Together")
            if (e%nargs() /= 1) then
                call refuse(ok, message, "Together takes one argument")
                return
            end if
            call poly_together(s%a, e%arg(1), out, ok, why)

        case ("Cancel")
            if (e%nargs() /= 1) then
                call refuse(ok, message, "Cancel takes one argument")
                return
            end if
            call poly_cancel(s%a, e%arg(1), out, ok, why)

        case ("Factor")
            if (e%nargs() /= 1) then
                call refuse(ok, message, "Factor takes one argument")
                return
            end if
            call poly_factor(s%a, e%arg(1), out, ok, why)

        case ("Apart")
            if (e%nargs() < 1 .or. e%nargs() > 2) then
                call refuse(ok, message, "Apart takes one or two arguments")
                return
            end if
            if (e%nargs() == 2) then
                call poly_apart(s%a, e%arg(1), e%arg(2), .true., out, ok, why)
            else
                var = e%arg(1)
                call poly_apart(s%a, e%arg(1), var, .false., out, ok, why)
            end if

        case ("Collect")
            if (e%nargs() /= 2) then
                call refuse(ok, message, &
                    "Collect here takes an expression and one variable")
                return
            end if
            call poly_collect(s%a, e%arg(1), e%arg(2), out, ok, why)

        case ("Exponent")
            if (e%nargs() /= 2) then
                call refuse(ok, message, "Exponent takes two arguments")
                return
            end if
            call poly_exponent(s%a, e%arg(1), e%arg(2), out, ok, why)

        case ("Coefficient")
            if (e%nargs() < 2 .or. e%nargs() > 3) then
                call refuse(ok, message, "Coefficient takes two or three "// &
                    "arguments")
                return
            end if
            order = 1
            if (e%nargs() == 3) then
                if (.not. exact_small_int(e%arg(3), order)) then
                    call refuse(ok, message, &
                        "Coefficient with a symbolic or negative power")
                    return
                end if
            end if
            var = e%arg(2)
            ! Coefficient[e, x^n] names the power in the form itself.
            if (var%kind() == NK_POW) then
                if (.not. exact_small_int(var%arg(2), order)) then
                    call refuse(ok, message, &
                        "Coefficient with a symbolic power")
                    return
                end if
                var = var%arg(1)
            end if
            call poly_coefficient(s%a, e%arg(1), var, order, out, ok, why)

        case ("CoefficientList")
            if (e%nargs() /= 2) then
                call refuse(ok, message, &
                    "CoefficientList here takes an expression and one "// &
                    "variable")
                return
            end if
            call poly_coefficient_list(s%a, e%arg(1), e%arg(2), list, ok, why)
            if (.not. ok) then
                call refuse(ok, message, head//": "//why)
                return
            end if
            r = func("List", list)
            return

        case ("PolynomialGCD")
            if (e%nargs() /= 2) then
                call refuse(ok, message, &
                    "PolynomialGCD here takes two polynomials")
                return
            end if
            call poly_gcd_expr(s%a, e%arg(1), e%arg(2), out, ok, why)

        case ("PolynomialQuotient", "PolynomialRemainder")
            if (e%nargs() /= 3) then
                call refuse(ok, message, &
                    head//" takes two polynomials and a variable")
                return
            end if
            call poly_divide(s%a, e%arg(1), e%arg(2), e%arg(3), &
                head == "PolynomialQuotient", out, ok, why)

        case default
            call refuse(ok, message, head//" is not implemented")
            return
        end select

        if (.not. ok) then
            call refuse(ok, message, head//": "//why)
            return
        end if
        r = out
    end function lower_polynomial

    !> A zero-argument application of the given name, such as Infinity.
    function is_named(e, name) result(yes)
        type(expr_t), intent(in) :: e
        character(*), intent(in) :: name
        logical                  :: yes

        yes = .false.
        if (e%kind() /= NK_FUNC) return
        yes = chars(e%name()) == name
    end function is_named

    !> Decide a boolean literal or a closed numeric relation.
    subroutine condition_value(e, decided, truth)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: decided, truth
        character(:), allocatable :: name, why
        real(dp) :: left, right
        logical :: left_ok, right_ok

        decided = .false.
        truth = .false.

        if (e%kind() == NK_SYM) then
            name = chars(e%name())
            if (name == "True") then
                decided = .true.
                truth = .true.
            else if (name == "False") then
                decided = .true.
                truth = .false.
            end if
            return
        end if

        if (e%kind() /= NK_FUNC) return
        name = chars(e%name())
        if (e%nargs() /= 2) return
        call numeric_value(e%arg(1), left, left_ok, why)
        if (.not. left_ok) return
        call numeric_value(e%arg(2), right, right_ok, why)
        if (.not. right_ok) return

        select case (name)
        case ("Less")
            truth = left < right
        case ("LessEqual")
            truth = left <= right
        case ("Greater")
            truth = left > right
        case ("GreaterEqual")
            truth = left >= right
        case ("Equal")
            truth = left == right
        case ("Unequal")
            truth = left /= right
        case default
            return
        end select
        decided = .true.
    end subroutine condition_value

    !> -Infinity, which the parser builds as a negation rather than a head.
    function is_negative_infinity(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        integer :: k

        yes = .false.
        if (e%kind() /= NK_MUL) return
        do k = 1, e%nargs()
            if (is_named(e%arg(k), "Infinity")) then
                yes = .true.
                return
            end if
        end do
    end function is_negative_infinity

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
        case ("NIntegrate")
            yes = .true.
            ! Assumptions (#29)
        case ("Refine", "Assuming", "Simplify2", "Element")
            yes = .true.
            ! Matrices (#30)
        case ("Eigenvalues", "LinearSolve", "MatrixPower", "MatrixForm")
            yes = .true.
            ! Solving beyond the scalar linear case (#36)
        case ("Reduce", "NSolve", "FindRoot", "Eliminate", "Roots", "ToRadicals")
            yes = .true.
            ! Series and sums (#35, #39)
        case ("SeriesCoefficient")
            yes = .true.
            ! Trig and power rewrites (#40)
        case ("Simplify3")
            yes = .true.
            ! Differential equations (#43)
        case ("DSolve", "NDSolve")
            yes = .true.
            ! Piecewise (#38)
        case ("Piecewise", "Boole", "If", "Which")
            yes = .true.
            ! Numerics (#37)
        case ("SetPrecision", "Interpolation", "Fit")
            yes = .true.
            ! Plotting, through fortplot (#44)
        case ("Plot3D", "ContourPlot", "ParametricPlot", "ListPlot", &
                "ListLinePlot", "DensityPlot", "StreamPlot", "VectorPlot", &
                "LogPlot", "LogLogPlot", "Show", "Graphics", "Graphics3D", &
                "GraphicsRow", "GraphicsGrid", "GraphicsArray", "Export", &
                "Legended")
            yes = .true.
            ! Constructs the parser can represent but the evaluator does not
            ! implement. They must refuse rather than print an unevaluated form as
            ! though it were a result.
        case ("Apply", "MapApply", "Function", "Slot", "SlotSequence", &
                "Span", "SetDelayed", "CompoundExpression", "StringJoin", &
                "ReplaceRepeated", "Condition", "DerivativeOperator")
            yes = .true.
        case default
            yes = .false.
        end select
    end function is_known_command

    !> Is `e` an application fortsym has no meaning for, and can therefore
    !> treat as an independent coordinate?
    function is_opaque_application(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        character(:), allocatable :: name
        integer :: k
        character :: c

        yes = .false.
        if (e%kind() /= NK_FUNC) return
        if (e%nargs() < 1) return

        name = chars(e%name())
        if (len(name) == 0) return
        c = name(1:1)
        if (.not. is_letter(c)) return
        do k = 2, len(name)
            c = name(k:k)
            if (is_letter(c)) cycle
            if (c >= "0" .and. c <= "9") cycle
            return
        end do

        if (is_known_command(name)) return
        if (has_differentiation_rule(name)) return
        select case (name)
        case ("List", "Rule", "Equal", "Plus", "Times", "Power", "Derivative", &
                "Part", "Slot", "Function", "D", "Integrate", "Sum", "Product", &
                "Limit", "Series", "Solve", "Simplify", "FullSimplify")
            return
        end select

        yes = .true.
    end function is_opaque_application

    pure function is_letter(c) result(yes)
        character, intent(in) :: c
        logical               :: yes

        yes = (c >= "a" .and. c <= "z")
        if (yes) return
        yes = (c >= "A" .and. c <= "Z")
    end function is_letter

    !> The canonical names fortsym differentiates through. An application with
    !> one of these heads is a function of its argument, not a coordinate.
    pure function has_differentiation_rule(name) result(yes)
        character(*), intent(in) :: name
        logical                  :: yes

        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", &
                "exp", "log", "sqrt", "abs", "erf", "erfc", "gamma", &
                "loggamma", "polygamma", "besselj", "legendrep", "legendreq", &
                "re", "im", "conjugate", "arg")
            yes = .true.
        case default
            yes = .false.
        end select
    end function has_differentiation_rule

    !> A symbol name that occurs nowhere in `e`, for use as a stand-in during
    !> a substitute-differentiate-substitute round trip.
    function fresh_symbol(a, e) result(v)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        type(expr_t)                         :: v
        type(str_t), allocatable :: names(:)
        character(:), allocatable :: candidate
        integer :: attempt, k
        logical :: taken

        names = free_symbols_of(e)
        do attempt = 0, 999
            candidate = "fortsymCoordinate"//itoa(attempt)
            taken = .false.
            do k = 1, size(names)
                if (chars(names(k)) == candidate) then
                    taken = .true.
                    exit
                end if
            end do
            if (.not. taken) exit
        end do
        v = sym(a, candidate)
    end function fresh_symbol

    function itoa(n) result(text)
        integer, intent(in) :: n
        character(:), allocatable :: text
        character(len=16) :: buf

        write (buf, '(i0)') n
        text = trim(buf)
    end function itoa

    !> Name the next output file. A script plotting twenty curves must not
    !> overwrite one file nineteen times.
    function next_plot_file(s) result(file)
        type(wl_session_t), intent(inout) :: s
        type(str_t)                       :: file
        character(16) :: tag

        s%plot_count = s%plot_count + 1
        write (tag, "(i0)") s%plot_count
        file = str("fortsym-plot-"//trim(tag)//".png")
    end function next_plot_file

    !> Remember the curves behind a file so Show can redraw them.
    subroutine keep_plot(s, file, fd, overlayable)
        type(wl_session_t),  intent(inout) :: s
        type(str_t),         intent(in)    :: file
        type(figure_data_t), intent(in)    :: fd
        logical,             intent(in)    :: overlayable

        if (s%n_plots >= MAX_PLOTS) return
        s%n_plots = s%n_plots + 1
        s%plots(s%n_plots)%file = file
        s%plots(s%n_plots)%data = fd
        s%plots(s%n_plots)%overlayable = overlayable
    end subroutine keep_plot

    !> Find the record a plot handle refers to. Handles are the file names Plot
    !> returned, which no source symbol can collide with.
    function plot_index(s, e) result(k)
        type(wl_session_t), intent(in) :: s
        type(expr_t),       intent(in) :: e
        integer                        :: k
        integer :: j

        k = 0
        if (e%kind() /= NK_SYM) return
        do j = 1, s%n_plots
            if (chars(s%plots(j)%file) == chars(e%name())) then
                k = j
                return
            end if
        end do
    end function plot_index

    !> Render Plot[f, {x, a, b}] and return the file it wrote.
    !>
    !> The returned value is the filename rather than a graphics object.
    !> fortsym has no graphics representation and inventing one would let a
    !> script compare two plots as though they were expressions.
    function render_plot(s, e, head, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        character(*),       intent(in)    :: head
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(plot_spec_t) :: spec
        type(figure_data_t) :: fd
        type(curve_t) :: c
        type(expr_t) :: body
        type(str_t) :: file, why
        integer :: k, n

        r = e
        ok = .true.
        message = str("")
        if (.not. read_plot_range(e%arg(2), spec)) then
            call refuse(ok, message, head//" needs a {var, lower, upper} range")
            return
        end if
        ! A non-positive sample has no place on a logarithmic axis, so it is
        ! dropped exactly like an undefined one rather than clamped.
        spec%positive_y = head == "LogPlot" .or. head == "LogLogPlot"
        spec%positive_x = head == "LogLogPlot"

        body = e%arg(1)
        n = 1
        if (is_list(body)) n = body%nargs()
        if (n < 1) then
            call refuse(ok, message, head//" needs at least one expression")
            return
        end if

        allocate (fd%curves(n))
        do k = 1, n
            if (is_list(body)) then
                ok = sample_curve(body%arg(k), spec, c, why)
            else
                ok = sample_curve(body, spec, c, why)
            end if
            if (.not. ok) then
                call refuse(ok, message, head//": "//chars(why))
                return
            end if
            fd%curves(k) = c
        end do
        fd%xname = spec%variable
        fd%xlog = spec%positive_x
        fd%ylog = spec%positive_y

        file = next_plot_file(s)
        call render_figure(fd, chars(file))
        call keep_plot(s, file, fd, .true.)
        r = sym(s%a, chars(file))
    end function render_plot

    !> ParametricPlot[{fx, fy}, {t, a, b}], one curve or a list of them.
    function render_parametric(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(plot_spec_t) :: spec
        type(figure_data_t) :: fd
        type(curve_t) :: c
        type(expr_t) :: body, item
        type(str_t) :: file, why
        integer :: k, n

        r = e
        ok = .true.
        message = str("")
        if (.not. read_plot_range(e%arg(2), spec)) then
            call refuse(ok, message, &
                "ParametricPlot needs a {var, lower, upper} range")
            return
        end if

        body = e%arg(1)
        if (.not. is_list(body)) then
            call refuse(ok, message, "ParametricPlot needs {x(t), y(t)}")
            return
        end if
        ! {x, y} is one curve; {{x1, y1}, {x2, y2}} is several. Telling them
        ! apart by the first element rather than by the count, because a pair
        ! of curves and a pair of components both have two arguments.
        item = body%arg(1)
        if (is_list(item)) then
            n = body%nargs()
        else
            n = 1
        end if

        allocate (fd%curves(n))
        do k = 1, n
            if (n == 1) then
                item = body
            else
                item = body%arg(k)
            end if
            if (item%nargs() /= 2) then
                call refuse(ok, message, &
                    "ParametricPlot needs exactly two components per curve")
                return
            end if
            ok = sample_parametric_curve(item%arg(1), item%arg(2), spec, c, why)
            if (.not. ok) then
                call refuse(ok, message, "ParametricPlot: "//chars(why))
                return
            end if
            fd%curves(k) = c
        end do

        file = next_plot_file(s)
        call render_figure(fd, chars(file))
        call keep_plot(s, file, fd, .true.)
        r = sym(s%a, chars(file))
    end function render_parametric

    !> ListPlot and ListLinePlot over explicit data.
    function render_list_plot(s, e, joined, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(in)    :: joined
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(figure_data_t) :: fd
        type(expr_t) :: data, item
        type(str_t) :: file
        logical :: nested
        integer :: k, n

        r = e
        ok = .true.
        message = str("")
        data = e%arg(1)
        if (.not. is_list(data)) then
            call refuse(ok, message, "ListPlot needs an explicit list of data")
            return
        end if
        if (data%nargs() < 1) then
            call refuse(ok, message, "ListPlot needs at least one point")
            return
        end if

        ! {{x, y}, ...} is one dataset; {{{x, y}, ...}, ...} is several. The
        ! difference shows in the first element's first element.
        nested = .false.
        item = data%arg(1)
        if (is_list(item)) then
            if (item%nargs() > 0) then
                if (is_list(item%arg(1))) nested = .true.
            end if
        end if

        if (nested) then
            n = data%nargs()
        else
            n = 1
        end if
        allocate (fd%curves(n))
        do k = 1, n
            if (nested) then
                item = data%arg(k)
            else
                item = data
            end if
            if (.not. dataset_curve(item, joined, fd%curves(k), message)) then
                ok = .false.
                return
            end if
        end do

        file = next_plot_file(s)
        call render_figure(fd, chars(file))
        call keep_plot(s, file, fd, .true.)
        r = sym(s%a, chars(file))
    end function render_list_plot

    !> Turn one explicit dataset into a curve.
    !>
    !> Either every element is a number, in which case the index is the
    !> abscissa exactly as Wolfram does, or every element is an {x, y} pair. A
    !> mixture, or an entry that is not a number, refuses: guessing what a
    !> symbolic entry meant would put a made-up point on the axes.
    function dataset_curve(data, joined, c, message) result(ok)
        type(expr_t),  intent(in)    :: data
        logical,       intent(in)    :: joined
        type(curve_t), intent(out)   :: c
        type(str_t),   intent(inout) :: message
        logical                      :: ok

        type(expr_t) :: item
        real(dp), allocatable :: xs(:), ys(:)
        real(dp) :: a, b
        logical :: got, pairs
        integer :: k, n

        ok = .false.
        n = data%nargs()
        if (n < 1) then
            message = str("ListPlot: empty dataset")
            return
        end if
        item = data%arg(1)
        pairs = is_list(item)

        allocate (xs(n), ys(n))
        do k = 1, n
            item = data%arg(k)
            if (is_list(item) .neqv. pairs) then
                message = str("ListPlot: dataset mixes numbers and pairs")
                return
            end if
            if (pairs) then
                if (item%nargs() /= 2) then
                    message = str("ListPlot: a point is not an {x, y} pair")
                    return
                end if
                a = plot_constant(item%arg(1), got)
                if (got) b = plot_constant(item%arg(2), got)
            else
                a = real(k, dp)
                b = plot_constant(item, got)
            end if
            if (.not. got) then
                message = str("ListPlot: a data entry is not a number")
                return
            end if
            xs(k) = a
            ys(k) = b
        end do

        c%x = xs
        c%y = ys
        if (joined) then
            c%style = CURVE_LINE
        else
            c%style = CURVE_POINTS
        end if
        ok = .true.
    end function dataset_curve

    !> The two-range family: Plot3D, ContourPlot, DensityPlot, StreamPlot and
    !> VectorPlot. None of these produce curves, so the record they leave is
    !> not overlayable and Show refuses to combine them.
    function render_field(s, e, head, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        character(*),       intent(in)    :: head
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(plot_spec_t) :: sx, sy
        type(figure_data_t) :: empty
        type(expr_t) :: body
        type(str_t) :: file, why

        r = e
        ok = .true.
        message = str("")
        if (.not. read_plot_range(e%arg(2), sx)) then
            call refuse(ok, message, head//" needs a {var, lower, upper} x range")
            return
        end if
        if (.not. read_plot_range(e%arg(3), sy)) then
            call refuse(ok, message, head//" needs a {var, lower, upper} y range")
            return
        end if
        if (chars(sx%variable) == chars(sy%variable)) then
            call refuse(ok, message, head//" needs two different variables")
            return
        end if

        body = e%arg(1)
        file = next_plot_file(s)

        select case (head)
        case ("Plot3D")
            call set_grid_samples(sx, sy)
            ok = render_surface(body, sx, sy, chars(file), why)
        case ("ContourPlot")
            if (body%kind() == NK_FUNC) then
                if (chars(body%name()) == "Equal") then
                    ! ContourPlot[f == g] wants the single curve where the two
                    ! sides agree. That needs a line contour at one level, and
                    ! fortplot's line-contour tracer indexes the field the
                    ! other way round from its band tracer, so it would draw
                    ! that curve transposed. Shading the difference instead
                    ! would answer a different question.
                    call refuse(ok, message, &
                        "ContourPlot of an equation needs single-level line "// &
                        "contours, which fortplot currently traces transposed")
                    return
                end if
            end if
            call set_grid_samples(sx, sy)
            ok = render_contour(body, sx, sy, chars(file), why)
        case ("DensityPlot")
            call set_grid_samples(sx, sy)
            ok = render_density(body, sx, sy, chars(file), why)
        case ("StreamPlot", "VectorPlot")
            if (.not. is_list(body)) then
                call refuse(ok, message, head//" needs {u, v} components")
                return
            end if
            if (body%nargs() /= 2) then
                call refuse(ok, message, head//" needs exactly two components")
                return
            end if
            if (head == "StreamPlot") then
                call set_grid_samples(sx, sy)
                ok = render_stream(body%arg(1), body%arg(2), sx, sy, &
                    chars(file), why)
            else
                call set_arrow_samples(sx, sy)
                ok = render_vector(body%arg(1), body%arg(2), sx, sy, &
                    chars(file), why)
            end if
        end select

        if (.not. ok) then
            call refuse(ok, message, head//": "//chars(why))
            return
        end if
        call keep_plot(s, file, empty, .false.)
        r = sym(s%a, chars(file))
    end function render_field

    !> Show[p1, p2, ...] draws every curve of every argument into one figure.
    !>
    !> An argument that is not a plot this script produced refuses. Returning
    !> the first argument instead would call one curve the overlay of two.
    function render_overlay(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(figure_data_t) :: fd
        type(str_t) :: file
        integer :: k, j, idx, total, filled, first

        r = e
        ok = .true.
        message = str("")

        total = 0
        first = 0
        do k = 1, e%nargs()
            if (is_option(e%arg(k))) cycle
            idx = plot_index(s, e%arg(k))
            if (idx == 0) then
                call refuse(ok, message, &
                    "Show: an argument is not a plot made earlier in this script")
                return
            end if
            if (.not. s%plots(idx)%overlayable) then
                call refuse(ok, message, &
                    "Show: a surface, contour, density or field plot has no "// &
                    "curves to overlay")
                return
            end if
            if (first == 0) first = idx
            if (s%plots(idx)%data%xlog .neqv. s%plots(first)%data%xlog) then
                call refuse(ok, message, &
                    "Show: cannot overlay a logarithmic axis with a linear one")
                return
            end if
            if (s%plots(idx)%data%ylog .neqv. s%plots(first)%data%ylog) then
                call refuse(ok, message, &
                    "Show: cannot overlay a logarithmic axis with a linear one")
                return
            end if
            total = total + size(s%plots(idx)%data%curves)
        end do

        if (first == 0) then
            call refuse(ok, message, "Show needs at least one plot")
            return
        end if

        allocate (fd%curves(total))
        filled = 0
        do k = 1, e%nargs()
            if (is_option(e%arg(k))) cycle
            idx = plot_index(s, e%arg(k))
            do j = 1, size(s%plots(idx)%data%curves)
                filled = filled + 1
                fd%curves(filled) = s%plots(idx)%data%curves(j)
            end do
        end do
        fd%xname = s%plots(first)%data%xname
        fd%yname = s%plots(first)%data%yname
        fd%xlog = s%plots(first)%data%xlog
        fd%ylog = s%plots(first)%data%ylog

        file = next_plot_file(s)
        call render_figure(fd, chars(file))
        call keep_plot(s, file, fd, .true.)
        r = sym(s%a, chars(file))
    end function render_overlay

    !> GraphicsRow, GraphicsGrid and GraphicsArray: earlier plots as panels.
    recursive function render_grid(s, e, head, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        character(*),       intent(in)    :: head
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(figure_data_t), allocatable :: panels(:)
        type(figure_data_t) :: empty
        type(expr_t) :: data, row, entry
        type(str_t) :: file
        integer :: rows, cols, k, j, idx, filled

        r = e
        ok = .true.
        message = str("")
        if (e%nargs() < 1) then
            call refuse(ok, message, head//" needs a list of plots")
            return
        end if
        data = e%arg(1)
        if (.not. is_list(data)) then
            call refuse(ok, message, head//" needs a list of plots")
            return
        end if
        if (data%nargs() < 1) then
            call refuse(ok, message, head//" needs at least one plot")
            return
        end if

        row = data%arg(1)
        if (is_list(row)) then
            rows = data%nargs()
            cols = row%nargs()
        else
            rows = 1
            cols = data%nargs()
        end if
        if (cols < 1) then
            call refuse(ok, message, head//" needs at least one plot")
            return
        end if

        allocate (panels(rows*cols))
        filled = 0
        do k = 1, rows
            if (rows == 1) then
                row = data
            else
                row = data%arg(k)
                if (.not. is_list(row)) then
                    call refuse(ok, message, head//" needs a rectangular grid")
                    return
                end if
                ! A ragged grid would silently shift panels into the wrong
                ! cells, so it refuses rather than being padded.
                if (row%nargs() /= cols) then
                    call refuse(ok, message, head//" needs a rectangular grid")
                    return
                end if
            end if
            do j = 1, cols
                ! Entries inside a list are not evaluated on the way in, so a
                ! GraphicsRow[{Plot[...], Plot[...]}] written inline has to be
                ! drawn here before there is a handle to look up.
                entry = wl_eval(s, row%arg(j), ok, message)
                if (.not. ok) return
                idx = plot_index(s, entry)
                if (idx == 0) then
                    call refuse(ok, message, head// &
                        ": an entry is not a plot made earlier in this script")
                    return
                end if
                if (.not. s%plots(idx)%overlayable) then
                    call refuse(ok, message, head// &
                        ": a surface, contour, density or field plot cannot "// &
                        "be redrawn as a panel")
                    return
                end if
                filled = filled + 1
                panels(filled) = s%plots(idx)%data
            end do
        end do

        file = next_plot_file(s)
        call render_panels(panels, rows, cols, chars(file))
        ! Kept as a non-overlayable record: a panel figure has no single curve
        ! list, so a later Show naming it says so instead of failing to find it.
        call keep_plot(s, file, empty, .false.)
        r = sym(s%a, chars(file))
    end function render_grid

    !> True for an option such as PlotRange -> All. Options change how a figure
    !> looks, never which curves are in it, so they are skipped.
    function is_option(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes

        yes = .false.
        if (e%kind() /= NK_FUNC) return
        yes = chars(e%name()) == "Rule"
    end function is_option
    !> True when Dot can actually form a product with this operand.
    function dottable(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        yes = is_list(e)
    end function dottable

    !> Substitute a scoping construct's local initialisers into its body.
    !>
    !> Returning the body untouched would be a wrong answer, not a refusal:
    !> Module[{ok = 1}, ok + 2] would report "ok + 2" as though that were the
    !> value. An uninitialised local is refused instead, because it names a
    !> fresh variable in a scope fortsym does not model, and treating it as the
    !> outer symbol of the same name silently resolves a shadow the wrong way.
    function scoped_body(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t) :: locals, item
        integer :: k

        ok = .true.
        message = str("")
        r = e%arg(e%nargs())
        locals = e%arg(1)

        if (locals%kind() /= NK_FUNC) return
        if (chars(locals%name()) /= "List") return

        do k = 1, locals%nargs()
            item = locals%arg(k)
            if (item%kind() == NK_FUNC) then
                if (chars(item%name()) == "Set") then
                    r = subs(r, item%arg(1), item%arg(2))
                    cycle
                end if
            end if
            call refuse(ok, message, &
                "scoping construct with an uninitialised local")
            return
        end do
    end function scoped_body

    !> Rebuild an arithmetic node from evaluated children.
    !>
    !> Folded with the arena's own operators rather than by index surgery, so
    !> the result is interned exactly as if it had been built directly and the
    !> structural comparison in the tests stays meaningful.
    recursive function eval_children(s, e, ok, message) result(r)
        type(wl_session_t), intent(inout) :: s
        type(expr_t),       intent(in)    :: e
        logical,            intent(out)   :: ok
        type(str_t),        intent(out)   :: message
        type(expr_t)                      :: r
        type(expr_t) :: child
        integer :: k

        ok = .true.
        message = str("")

        do k = 1, e%nargs()
            child = wl_eval(s, e%arg(k), ok, message)
            if (.not. ok) then
                r = e
                return
            end if
            if (k == 1) then
                r = child
                cycle
            end if
            select case (e%kind())
            case (NK_ADD); r = r + child
            case (NK_MUL); r = r*child
            case (NK_POW); r = r**child
            end select
        end do
    end function eval_children

    !> Recognise only the assumption-free identity sin(u)^2 + cos(u)^2 = 1.
    !>
    !> This deliberately does not call a broad trig rewrite: identities that
    !> introduce denominators or depend on branch assumptions belong to a
    !> separate, guarded operation. The two terms are tested in both orders
    !> because the arena canonicalises sums but the predicate should not rely
    !> on that implementation detail.
    function is_pythagorean_identity(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(expr_t) :: sin_angle, cos_angle

        yes = .false.
        if (e%kind() /= NK_ADD .or. e%nargs() /= 2) return
        if (is_trig_square(e%arg(1), "sin", sin_angle) .and. &
            is_trig_square(e%arg(2), "cos", cos_angle)) then
            yes = sin_angle%id == cos_angle%id
            return
        end if
        if (is_trig_square(e%arg(1), "cos", cos_angle) .and. &
            is_trig_square(e%arg(2), "sin", sin_angle)) then
            yes = sin_angle%id == cos_angle%id
        end if
    end function is_pythagorean_identity

    function is_trig_square(e, expected, angle) result(yes)
        type(expr_t), intent(in)  :: e
        character(*), intent(in)  :: expected
        type(expr_t), intent(out) :: angle
        logical                   :: yes
        type(expr_t) :: base, exponent

        yes = .false.
        angle = e
        if (e%kind() /= NK_POW) return
        exponent = e%arg(2)
        if (exponent%kind() /= NK_INT) return
        if (exponent%int_value() /= 2_int64) return
        base = e%arg(1)
        if (base%kind() /= NK_FUNC .or. base%nargs() /= 1) return
        if (chars(base%name()) /= expected) return
        angle = base%arg(1)
        yes = .true.
    end function is_trig_square

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
