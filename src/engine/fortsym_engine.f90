module fortsym_engine
    ! What a computer algebra backend has to provide, and what it may decline.
    !
    ! Engines differ enormously in what they can do. SymEngine decides
    ! trigonometric identities and cannot integrate; Yacas integrates and factors
    ! but cannot simplify trigonometry; SymPy and Maxima do most things slowly.
    ! Rather than pretend to a common denominator, each engine declares its
    ! capabilities and callers ask before dispatching.
    !
    ! Two rules keep the abstraction honest:
    !
    !   - A verdict is three-valued. UNKNOWN is a first-class answer meaning
    !     "outside what I decide", not an error and not a disguised NO. An engine
    !     that guesses instead of returning UNKNOWN is worse than useless in a
    !     verification tool, because a confident wrong answer is acted on.
    !   - Every call is timed. The council compares engines on speed as well as
    !     verdicts, and benchmark tables fall out of ordinary use rather than
    !     needing a separate harness.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_expr, only: expr_t
    implicit none
    private

    public :: engine_t, engine_result_t
    public :: VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, verdict_name
    public :: CAP_ZERO_TEST, CAP_SIMPLIFY, CAP_DIFF, CAP_EXPAND, CAP_FACTOR, &
        CAP_INTEGRATE, CAP_LIMIT, CAP_SOLVE, CAP_CSE, CAP_EVAL, CAP_SERIES
    public :: has_cap, wall_seconds

    integer, parameter :: dp = real64

    !> Three-valued verdict. The distinction between FALSE and UNKNOWN is the
    !> whole point: FALSE is a decision, UNKNOWN is a refusal to guess.
    integer, parameter :: VERDICT_UNKNOWN = 0
    integer, parameter :: VERDICT_TRUE = 1
    integer, parameter :: VERDICT_FALSE = 2

    ! Capability bits. Powers of two so an engine declares a bitmask.
    integer, parameter :: CAP_ZERO_TEST = 1
    integer, parameter :: CAP_SIMPLIFY = 2
    integer, parameter :: CAP_DIFF = 4
    integer, parameter :: CAP_EXPAND = 8
    integer, parameter :: CAP_FACTOR = 16
    integer, parameter :: CAP_INTEGRATE = 32
    integer, parameter :: CAP_LIMIT = 64
    integer, parameter :: CAP_SOLVE = 128
    integer, parameter :: CAP_CSE = 256
    integer, parameter :: CAP_EVAL = 512
    integer, parameter :: CAP_SERIES = 1024

    !> Outcome of one engine call: what it answered, whether it answered at all,
    !> and how long it took.
    type :: engine_result_t
        logical      :: ok = .false. !< the engine produced an answer
        integer      :: verdict = VERDICT_UNKNOWN
        type(expr_t) :: value !< result expression, when applicable
        real(dp)     :: seconds = 0.0_dp
        type(str_t)  :: message !< why, when ok is false
        logical      :: conditional = .false.
        type(str_t)  :: condition !< validity condition when conditional
    end type engine_result_t

    !> A computer algebra backend.
    !>
    !> Every operation has a default that reports "not supported", so a backend
    !> implements only what it can actually do and adding an operation to this
    !> type does not break existing engines.
    type, abstract :: engine_t
        type(str_t) :: name
        !> False when the tool is not installed. Callers skip it; nothing fails.
        logical     :: available = .false.
        integer     :: caps = 0
        !> True when the engine runs in this process. Out-of-process engines pay
        !> a spawn cost per call, which the council weighs when choosing.
        logical     :: in_process = .false.
    contains
        procedure :: zero_test => engine_zero_test_default
        procedure :: simplify => engine_simplify_default
        procedure :: diff => engine_diff_default
        procedure :: expand => engine_expand_default
        procedure :: series => engine_series_default
        procedure :: series_coeff => engine_series_coeff_default
        procedure :: solve => engine_solve_default
        procedure :: shutdown => engine_shutdown_default
    end type engine_t

contains

    pure function verdict_name(v) result(s)
        integer, intent(in) :: v
        type(str_t)         :: s
        select case (v)
        case (VERDICT_TRUE);  s = str("ZERO")
        case (VERDICT_FALSE); s = str("NONZERO")
        case default;         s = str("UNKNOWN")
        end select
    end function verdict_name

    pure function has_cap(self, cap) result(yes)
        class(engine_t), intent(in) :: self
        integer,         intent(in) :: cap
        logical                     :: yes
        yes = self%available .and. iand(self%caps, cap) == cap
    end function has_cap

    !> Monotonic wall clock in seconds. system_clock rather than cpu_time,
    !> because an out-of-process engine spends most of its time not running on
    !> this CPU and cpu_time would report it as free.
    function wall_seconds() result(t)
        real(dp)       :: t
        integer(int64) :: count, rate
        call system_clock(count, rate)
        if (rate > 0_int64) then
            t = real(count, dp)/real(rate, dp)
        else
            t = 0.0_dp
        end if
    end function wall_seconds

    ! Defaults. Each reports honestly that it did nothing, so an engine that
    ! does not implement an operation can never be mistaken for one that
    ! answered.

    function engine_zero_test_default(self, e) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e
        type(engine_result_t)          :: r
        r%ok = .false.
        r%verdict = VERDICT_UNKNOWN
        r%message = str(chars(self%name)//": zero_test not supported")
    end function engine_zero_test_default

    function engine_simplify_default(self, e) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e
        type(engine_result_t)          :: r
        r%ok = .false.
        r%value = e
        r%message = str(chars(self%name)//": simplify not supported")
    end function engine_simplify_default

    function engine_diff_default(self, e, v) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e, v
        type(engine_result_t)          :: r
        r%ok = .false.
        r%value = e
        r%message = str(chars(self%name)//": diff not supported")
    end function engine_diff_default

    function engine_expand_default(self, e) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e
        type(engine_result_t)          :: r
        r%ok = .false.
        r%value = e
        r%message = str(chars(self%name)//": expand not supported")
    end function engine_expand_default

    function engine_series_default(self, e, v, point, order) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e, v, point
        integer,         intent(in)    :: order
        type(engine_result_t)          :: r
        r%ok = .false.
        r%value = e
        r%message = str(chars(self%name)//": series not supported")
        associate (unused_v => v, unused_point => point, unused_order => order)
        end associate
    end function engine_series_default

    function engine_series_coeff_default(self, e, v, point, order) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e, v, point
        integer,         intent(in)    :: order
        type(engine_result_t)          :: r
        r%ok = .false.
        r%value = e
        r%message = str(chars(self%name)//": series_coeff not supported")
        associate (unused_v => v, unused_point => point, unused_order => order)
        end associate
    end function engine_series_coeff_default

    function engine_solve_default(self, e, v) result(r)
        class(engine_t), intent(inout) :: self
        type(expr_t),    intent(in)    :: e, v
        type(engine_result_t)          :: r
        r%ok = .false.
        r%value = e
        r%message = str(chars(self%name)//": solve not supported")
        associate (unused_v => v)
        end associate
    end function engine_solve_default

    subroutine engine_shutdown_default(self)
        class(engine_t), intent(inout) :: self
        ! In-process engines have nothing to release; subprocess backends
        ! override this to close pipes and reap the child.
        self%available = self%available
    end subroutine engine_shutdown_default

end module fortsym_engine
