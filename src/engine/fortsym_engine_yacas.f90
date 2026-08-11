module fortsym_engine_yacas
    ! Yacas as a Tier 1 backend.
    !
    ! Yacas is linked in-process, so it costs no subprocess per call, and it
    ! brings exactly the capabilities SymEngine lacks: symbolic integration,
    ! limits, solve, factorization and multivariate rational cancellation.
    !
    ! It also lacks what SymEngine has. Measured here, Yacas's Simplify leaves
    ! Sin(x)^2+Cos(x)^2 untouched, so this engine deliberately does NOT declare
    ! CAP_ZERO_TEST for trigonometry: the exponential normal form in fsym_shim is
    ! strictly better at that, and an engine that claimed the capability would
    ! draw work it answers worse. Declaring capabilities honestly is what makes
    ! the council's fallback meaningful.
    !
    ! Availability is a run-time property. Yacas needs its .ys script library --
    ! without it the engine cannot even parse `x^2`, because operator precedence
    ! itself is defined in those scripts -- so construction loads the library and
    ! verifies it took, and reports unavailable rather than returning a handle
    ! that silently misparses everything.
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_size_t, &
        c_null_char, c_null_ptr, c_associated
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect, DIA_YACAS
    use fortsym_print, only: print_expr_in
    use fortsym_parse, only: parse_expr_in
    use fortsym_engine, only: engine_t, engine_result_t, wall_seconds, &
        VERDICT_UNKNOWN, VERDICT_TRUE, &
        CAP_SIMPLIFY, CAP_EXPAND, CAP_FACTOR, CAP_INTEGRATE, CAP_LIMIT, &
        CAP_SOLVE, CAP_DIFF, resource_limit_t
    implicit none
    private

    public :: yacas_engine_t, make_yacas_engine, yacas_scripts_default

    integer, parameter :: dp = real64

    type, extends(engine_t) :: yacas_engine_t
        type(arena_t), pointer :: home => null()
        type(c_ptr)            :: handle = c_null_ptr
    contains
        procedure :: simplify => yc_simplify
        procedure :: factor => yc_factor
        procedure :: integrate => yc_integrate
        procedure :: shutdown => yc_shutdown
    end type yacas_engine_t

    interface

        function fsym_yacas_new(scripts_dir) bind(c, name="fsym_yacas_new") &
                result(h)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: scripts_dir(*)
            type(c_ptr)                        :: h
        end function fsym_yacas_new

        subroutine fsym_yacas_free(self) bind(c, name="fsym_yacas_free")
            import :: c_ptr
            type(c_ptr), value :: self
        end subroutine fsym_yacas_free

        function fsym_yacas_eval(self, expression) &
                bind(c, name="fsym_yacas_eval") result(ok)
            import :: c_ptr, c_char, c_int
            type(c_ptr),            value      :: self
            character(kind=c_char), intent(in) :: expression(*)
            integer(c_int)                     :: ok
        end function fsym_yacas_eval

        function fsym_yacas_result_len(self) &
                bind(c, name="fsym_yacas_result_len") result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: self
            integer(c_size_t)  :: n
        end function fsym_yacas_result_len

        function fsym_yacas_result_fetch(self, buf, n) &
                bind(c, name="fsym_yacas_result_fetch") result(m)
            import :: c_ptr, c_char, c_size_t
            type(c_ptr),            value         :: self
            character(kind=c_char), intent(inout) :: buf(*)
            integer(c_size_t),      value         :: n
            integer(c_size_t)                     :: m
        end function fsym_yacas_result_fetch

        function fsym_yacas_error_len(self) &
                bind(c, name="fsym_yacas_error_len") result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: self
            integer(c_size_t)  :: n
        end function fsym_yacas_error_len

        function fsym_yacas_error_fetch(self, buf, n) &
                bind(c, name="fsym_yacas_error_fetch") result(m)
            import :: c_ptr, c_char, c_size_t
            type(c_ptr),            value         :: self
            character(kind=c_char), intent(inout) :: buf(*)
            integer(c_size_t),      value         :: n
            integer(c_size_t)                     :: m
        end function fsym_yacas_error_fetch

    end interface

contains

    !> Where to find the .ys script library.
    !>
    !> The environment variable first, then the usual install locations. The
    !> path is deliberately not compiled in: doing that would need the Fortran
    !> preprocessor, and turning -cpp on across the codebase risks the C
    !> preprocessor misreading Fortran's // string concatenation. The build
    !> sets the variable for its own tests, and a packaged install can point
    !> somewhere else without a rebuild.
    function yacas_scripts_default() result(path)
        character(:), allocatable :: path
        character(len=4096) :: buf
        integer :: n, stat, k

        character(*), parameter :: CANDIDATES(3) = [character(len=32) :: &
            "/usr/share/yacas/scripts", &
            "/usr/local/share/yacas/scripts", &
            "/usr/lib/yacas/scripts"]

        call get_environment_variable("FORTSYM_YACAS_SCRIPTS", buf, n, stat)
        if (stat == 0 .and. n > 0) then
            path = buf(1:n)
            return
        end if

        do k = 1, size(CANDIDATES)
            if (has_init_script(trim_right(CANDIDATES(k)))) then
                path = trim_right(CANDIDATES(k))
                return
            end if
        end do

        path = ""
    end function yacas_scripts_default

    !> A scripts directory is only usable if the entry point is actually there.
    function has_init_script(dir) result(yes)
        character(*), intent(in) :: dir
        logical                  :: yes
        inquire (file=dir//"/yacasinit.ys", exist=yes)
    end function has_init_script

    pure function trim_right(s) result(r)
        character(*), intent(in)  :: s
        character(:), allocatable :: r
        integer :: n
        n = len(s)
        do while (n > 0)
            if (s(n:n) /= " ") exit
            n = n - 1
        end do
        r = s(1:n)
    end function trim_right

    function make_yacas_engine(home, scripts_dir) result(eng)
        type(arena_t), target,  intent(inout) :: home
        character(*), optional, intent(in)    :: scripts_dir
        type(yacas_engine_t)                  :: eng
        character(:), allocatable :: path

        eng%name = str("yacas")
        eng%in_process = .true.
        eng%home => home
        eng%available = .false.
        eng%handle = c_null_ptr

        ! Capabilities Yacas actually has. CAP_ZERO_TEST is absent on purpose:
        ! its Simplify does not close trigonometric identities, and claiming the
        ! capability would draw work another engine answers better.
        eng%caps = CAP_SIMPLIFY + CAP_EXPAND + CAP_FACTOR + CAP_INTEGRATE + &
            CAP_LIMIT + CAP_SOLVE + CAP_DIFF

        if (present(scripts_dir)) then
            path = scripts_dir
        else
            path = yacas_scripts_default()
        end if

        if (len(path) == 0) return

        eng%handle = fsym_yacas_new(path//c_null_char)
        eng%available = c_associated(eng%handle)
    end function make_yacas_engine

    subroutine yc_shutdown(self)
        class(yacas_engine_t), intent(inout) :: self
        if (c_associated(self%handle)) then
            call fsym_yacas_free(self%handle)
            self%handle = c_null_ptr
        end if
        self%available = .false.
    end subroutine yc_shutdown

    !> Evaluate a Yacas expression and read the answer back as an expression.
    function evaluate(self, source, r)
        class(yacas_engine_t), intent(inout) :: self
        character(*),          intent(in)    :: source
        type(engine_result_t), intent(inout) :: r
        logical                              :: evaluate

        character(:), allocatable :: text, message

        evaluate = .false.

        if (fsym_yacas_eval(self%handle, source//c_null_char) == 0_c_int) then
            r%message = str("yacas: "//fetch_error(self))
            return
        end if

        text = fetch_result(self)
        if (len(text) == 0) then
            r%message = str("yacas: empty result")
            return
        end if

        r%value = parse_expr_in(self%home, text, dialect(DIA_YACAS), &
            evaluate, message)
        if (.not. evaluate) then
            r%message = str("yacas: unreadable answer '"//text//"': "//message)
        end if
    end function evaluate

    function yc_simplify(self, e, limit) result(r)
        class(yacas_engine_t), intent(inout) :: self
        type(expr_t),          intent(in)    :: e
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                :: r
        character(:), allocatable :: text
        real(dp) :: t0

        r%value = e
        if (.not. self%available) then
            r%message = str("yacas: not available")
            return
        end if

        t0 = wall_seconds()
        text = chars(print_expr_in(e, dialect(DIA_YACAS)))
        ! Simplify after ratsimp-equivalent normalisation: Yacas's Simplify
        ! handles rational structure, including the multivariate cancellation
        ! SymEngine cannot do.
        r%ok = evaluate(self, "Simplify("//text//");", r)
        r%seconds = wall_seconds() - t0
        if (.not. r%ok) r%value = e
    end function yc_simplify

    function yc_factor(self, e, limit) result(r)
        class(yacas_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                :: r
        character(:), allocatable :: text
        real(dp) :: t0

        r%value = e
        if (.not. self%available) then
            r%message = str("yacas: not available")
            return
        end if

        t0 = wall_seconds()
        text = chars(print_expr_in(e, dialect(DIA_YACAS)))
        r%ok = evaluate(self, "Factor("//text//");", r)
        r%seconds = wall_seconds() - t0
        if (.not. r%ok) r%value = e
    end function yc_factor

    !> Symbolic integration, which no other linked engine here provides.
    function yc_integrate(self, e, v) result(r)
        class(yacas_engine_t), intent(inout) :: self
        type(expr_t),          intent(in)    :: e, v
        type(engine_result_t)                :: r
        character(:), allocatable :: text, var
        real(dp) :: t0

        r%value = e
        if (.not. self%available) then
            r%message = str("yacas: not available")
            return
        end if

        t0 = wall_seconds()
        text = chars(print_expr_in(e, dialect(DIA_YACAS)))
        var = chars(print_expr_in(v, dialect(DIA_YACAS)))
        r%ok = evaluate(self, "Integrate("//var//") "//text//";", r)
        r%seconds = wall_seconds() - t0
        if (.not. r%ok) r%value = e
    end function yc_integrate

    function fetch_result(self) result(text)
        class(yacas_engine_t), intent(inout) :: self
        character(:), allocatable            :: text
        integer(c_size_t) :: n, got
        character(kind=c_char), allocatable :: buf(:)
        integer :: k

        n = fsym_yacas_result_len(self%handle)
        if (n == 0_c_size_t) then
            text = ""
            return
        end if
        allocate (buf(n))
        got = fsym_yacas_result_fetch(self%handle, buf, n)
        allocate (character(len=int(got)) :: text)
        do k = 1, int(got)
            text(k:k) = buf(k)
        end do
    end function fetch_result

    function fetch_error(self) result(text)
        class(yacas_engine_t), intent(inout) :: self
        character(:), allocatable            :: text
        integer(c_size_t) :: n, got
        character(kind=c_char), allocatable :: buf(:)
        integer :: k

        n = fsym_yacas_error_len(self%handle)
        if (n == 0_c_size_t) then
            text = "unspecified failure"
            return
        end if
        allocate (buf(n))
        got = fsym_yacas_error_fetch(self%handle, buf, n)
        allocate (character(len=int(got)) :: text)
        do k = 1, int(got)
            text(k:k) = buf(k)
        end do
    end function fetch_error

end module fortsym_engine_yacas
