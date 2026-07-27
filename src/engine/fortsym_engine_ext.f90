module fortsym_engine_ext
    ! Maxima and SymPy as out-of-process backends.
    !
    ! Both are Tier 2: driven as separate programs, never linked. For Maxima
    ! (GPL-2.0+) that is what keeps fortsym's MIT link closure clean; for SymPy
    ! (BSD-3) the reason is practical rather than legal, since linking it would
    ! mean embedding CPython.
    !
    ! Neither is required. Each probes for its program at construction and marks
    ! itself unavailable when it is missing, so the council simply has fewer
    ! voters and nothing fails.
    !
    ! Zero testing here is deliberately indirect: ask the engine to simplify the
    ! expression and see whether the answer is zero. That is weaker than a
    ! decision procedure -- an engine that fails to simplify reports UNKNOWN
    ! rather than NONZERO -- and it is the honest reading, because "I could not
    ! reduce this" is not evidence that the expression is non-zero.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect, DIA_MAXIMA, DIA_SYMPY
    use fortsym_print, only: print_expr_in
    use fortsym_parse, only: parse_expr_in
    use fortsym_proc, only: proc_available, proc_run, MARK_BEGIN, MARK_END
    use fortsym_engine, only: engine_t, engine_result_t, wall_seconds, &
        VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, &
        CAP_ZERO_TEST, CAP_SIMPLIFY, CAP_DIFF, CAP_EXPAND, CAP_FACTOR, &
        CAP_INTEGRATE, CAP_LIMIT, CAP_SOLVE
    implicit none
    private

    public :: maxima_engine_t, make_maxima_engine
    public :: sympy_engine_t, make_sympy_engine

    integer, parameter :: dp = real64

    type, extends(engine_t) :: maxima_engine_t
        type(arena_t), pointer :: home => null()
    contains
        procedure :: zero_test => mx_zero_test
        procedure :: simplify => mx_simplify
    end type maxima_engine_t

    type, extends(engine_t) :: sympy_engine_t
        type(arena_t), pointer :: home => null()
    contains
        procedure :: zero_test => sp_zero_test
        procedure :: simplify => sp_simplify
    end type sympy_engine_t

contains

    ! ------------------------------------------------------------- Maxima --

    function make_maxima_engine(home) result(eng)
        type(arena_t), target, intent(inout) :: home
        type(maxima_engine_t)                :: eng

        eng%name = str("maxima")
        eng%in_process = .false.
        eng%available = proc_available("maxima")
        eng%home => home
        eng%caps = CAP_ZERO_TEST + CAP_SIMPLIFY + CAP_EXPAND + CAP_FACTOR + &
            CAP_INTEGRATE + CAP_LIMIT + CAP_SOLVE
    end function make_maxima_engine

    !> A Maxima script that prints one framed answer.
    !>
    !> display2d:false keeps the answer on one line; without it Maxima renders
    !> a two-dimensional layout that no parser should have to read. The
    !> simplification is trigsimp of ratsimp, which handles the rational and the
    !> obvious trigonometric cases; Maxima has no single call that does both.
    function maxima_script(body) result(s)
        character(*), intent(in)  :: body
        character(:), allocatable :: s
        type(strbuf_t) :: b

        call b%append("display2d:false$")
        call b%newline()
        call b%append("print("""//MARK_BEGIN//""")$")
        call b%newline()
        call b%append(body)
        call b%newline()
        call b%append("print("""//MARK_END//""")$")
        call b%newline()
        call b%append("quit()$")
        call b%newline()
        s = b%chars()
    end function maxima_script

    function mx_simplify(self, e) result(r)
        class(maxima_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(engine_result_t)                 :: r
        type(str_t), allocatable :: lines(:)
        character(:), allocatable :: text, message
        integer :: n
        logical :: ok
        real(dp) :: t0

        r%value = e
        if (.not. self%available) then
            r%message = str("maxima: not installed")
            return
        end if

        t0 = wall_seconds()
        text = chars(print_expr_in(e, dialect(DIA_MAXIMA)))
        call proc_run("maxima --very-quiet", &
            maxima_script("print(trigsimp(ratsimp("//text//")))$"), &
            lines, n, ok)
        r%seconds = wall_seconds() - t0

        if (.not. ok .or. n < 1) then
            r%message = str("maxima: no framed answer")
            return
        end if

        r%value = parse_expr_in(self%home, chars(lines(1)), &
            dialect(DIA_MAXIMA), ok, message)
        r%ok = ok
        if (.not. ok) r%message = str("maxima: unreadable answer: "//message)
    end function mx_simplify

    function mx_zero_test(self, e) result(r)
        class(maxima_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(engine_result_t)                 :: r
        r = simplified_zero_test(self%simplify(e))
    end function mx_zero_test

    ! -------------------------------------------------------------- SymPy --

    function make_sympy_engine(home) result(eng)
        type(arena_t), target, intent(inout) :: home
        type(sympy_engine_t)                 :: eng

        eng%name = str("sympy")
        eng%in_process = .false.
        eng%home => home
        ! Both the interpreter and the module have to be there. A python3
        ! without sympy is common enough that probing only the interpreter would
        ! mark the engine available and then fail on every call.
        eng%available = proc_available("python3")
        if (eng%available) eng%available = sympy_importable()
        eng%caps = CAP_ZERO_TEST + CAP_SIMPLIFY + CAP_DIFF + CAP_EXPAND + &
            CAP_FACTOR + CAP_INTEGRATE + CAP_LIMIT + CAP_SOLVE
    end function make_sympy_engine

    function sympy_importable() result(yes)
        logical :: yes
        integer :: stat, cmdstat
        call execute_command_line( &
            "python3 -c 'import sympy' >/dev/null 2>&1", wait=.true., &
            exitstat=stat, cmdstat=cmdstat)
        yes = cmdstat == 0 .and. stat == 0
    end function sympy_importable

    !> A SymPy script that prints one framed answer.
    !>
    !> Every free name is declared a symbol before sympify runs, so an unknown
    !> identifier becomes a symbol rather than resolving to a Python builtin --
    !> without that, an expression containing `E` or `I` would silently pick up
    !> whatever those names mean in the interpreter.
    function sympy_script(expr_text, call_text) result(s)
        character(*), intent(in)  :: expr_text, call_text
        character(:), allocatable :: s
        type(strbuf_t) :: b

        call b%append("import sympy")
        call b%newline()
        call b%append("from sympy import *")
        call b%newline()
        call b%append("e = sympify('''"//expr_text//"''')")
        call b%newline()
        call b%append("print('"//MARK_BEGIN//"')")
        call b%newline()
        call b%append("print("//call_text//")")
        call b%newline()
        call b%append("print('"//MARK_END//"')")
        call b%newline()
        s = b%chars()
    end function sympy_script

    function sp_simplify(self, e) result(r)
        class(sympy_engine_t), intent(inout) :: self
        type(expr_t),          intent(in)    :: e
        type(engine_result_t)                :: r
        type(str_t), allocatable :: lines(:)
        character(:), allocatable :: text, message
        integer :: n
        logical :: ok
        real(dp) :: t0

        r%value = e
        if (.not. self%available) then
            r%message = str("sympy: not installed")
            return
        end if

        t0 = wall_seconds()
        text = chars(print_expr_in(e, dialect(DIA_SYMPY)))
        call proc_run("python3", sympy_script(text, "simplify(e)"), lines, n, ok)
        r%seconds = wall_seconds() - t0

        if (.not. ok .or. n < 1) then
            r%message = str("sympy: no framed answer")
            return
        end if

        r%value = parse_expr_in(self%home, chars(lines(1)), &
            dialect(DIA_SYMPY), ok, message)
        r%ok = ok
        if (.not. ok) r%message = str("sympy: unreadable answer: "//message)
    end function sp_simplify

    function sp_zero_test(self, e) result(r)
        class(sympy_engine_t), intent(inout) :: self
        type(expr_t),          intent(in)    :: e
        type(engine_result_t)                :: r
        r = simplified_zero_test(self%simplify(e))
    end function sp_zero_test

    ! ------------------------------------------------------------ shared --

    !> Turn a simplification into a zero verdict.
    !>
    !> Only a simplified result of exactly zero counts as ZERO. Anything else is
    !> UNKNOWN, never NONZERO: these engines simplify, they do not decide, and
    !> "I could not reduce this to zero" is not evidence that it is non-zero.
    !> Reporting NONZERO here would manufacture a confident wrong verdict out of
    !> an engine's failure to try hard enough.
    function simplified_zero_test(sr) result(r)
        type(engine_result_t), intent(in) :: sr
        type(engine_result_t)             :: r
        integer, parameter :: NK_INTEGER = 1

        r = sr
        r%verdict = VERDICT_UNKNOWN
        if (.not. sr%ok) return

        if (sr%value%kind() == NK_INTEGER) then
            if (sr%value%int_value() == 0_8) r%verdict = VERDICT_TRUE
        end if
    end function simplified_zero_test

end module fortsym_engine_ext
