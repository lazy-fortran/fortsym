module fortsym_check
    ! The assertion harness: what replaces the notebook PASS/FAIL scripts.
    !
    ! The output contract is deliberately the one those scripts already use --
    ! `PASS  <label>`, `FAIL  <label> -> <residual>`, a summary, and a nonzero
    ! exit -- so a suite can be ported without changing how anything reads it,
    ! and so the failure detail is the residual rather than a bare boolean.
    !
    ! What is new is that a verdict carries its strength. A symbolic decision
    ! and a numeric probe are both useful and are not the same thing, and the
    ! report says which one was used:
    !
    !   PASS         decided symbolically. A proof.
    !   PASS(probe)  no counterexample in N random high-precision evaluations.
    !                Strong evidence, not a proof.
    !
    ! `check_identity` is the strict variant that refuses probe evidence, for
    ! suites where only a proof will do. Conflating the two is how a
    ! verification tool starts quietly asserting things it has not established.
    use, intrinsic :: iso_fortran_env, only: int64, real64, output_unit
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_expr, only: expr_t, operator(-)
    use fortsym_engine, only: engine_t, engine_result_t, &
        VERDICT_TRUE, VERDICT_FALSE
    use fortsym_eval, only: binding_t, eval_expr, collect_free_symbols
    use fortsym_print, only: print_expr
    implicit none
    private

    public :: suite_t
    public :: suite_begin, suite_end, check_zero, check_identity, probe_zero

    integer, parameter :: dp = real64

    !> Probe points per check. Enough that an accidental vanishing at every one
    !> of them is not a realistic outcome for a non-identity, and cheap enough
    !> that a suite of hundreds of checks stays fast.
    integer, parameter :: PROBE_POINTS = 200

    !> Relative tolerance for calling a probe residual zero.
    real(dp), parameter :: PROBE_TOL = 1.0e-11_dp

    type :: suite_t
        type(str_t) :: name
        integer     :: total = 0
        integer     :: passed = 0
        integer     :: proved = 0
        integer     :: by_probe = 0
        integer     :: failed = 0
        type(strbuf_t) :: json
    end type suite_t

contains

    subroutine suite_begin(s, name)
        type(suite_t), intent(out) :: s
        character(*),  intent(in)  :: name
        s%name = str(name)
        s%total = 0
        s%passed = 0
        s%proved = 0
        s%by_probe = 0
        s%failed = 0
        write (output_unit, "(a)") "suite: "//name
    end subroutine suite_begin

    !> Assert that an expression is identically zero.
    !>
    !> The engine decides where it can. Where it returns UNKNOWN -- gamma,
    !> Bessel, anything outside the decidable fragment -- a numeric probe takes
    !> over and the pass is reported as probe evidence rather than proof.
    subroutine check_zero(s, eng, label, e)
        type(suite_t),   intent(inout) :: s
        class(engine_t), intent(inout) :: eng
        character(*),    intent(in)    :: label
        type(expr_t),    intent(in)    :: e

        type(engine_result_t) :: r
        integer :: tried, agreed
        logical :: probe_clean

        s%total = s%total + 1
        r = eng%zero_test(e)

        if (r%verdict == VERDICT_TRUE) then
            call record_pass(s, label, .true., 0, 0)
            return
        end if

        if (r%verdict == VERDICT_FALSE) then
            call record_fail(s, label, e, "engine decided it is not zero")
            return
        end if

        call probe_zero(e, probe_clean, tried, agreed)

        if (tried == 0) then
            call record_fail(s, label, e, &
                "undecided, and no probe point was evaluable")
        else if (probe_clean) then
            call record_pass(s, label, .false., tried, agreed)
        else
            call record_fail(s, label, e, "a probe point gave a non-zero value")
        end if
    end subroutine check_zero

    !> Assert that an expression is identically zero, and require a proof.
    !>
    !> Probe evidence is rejected here. For a suite that must not accept
    !> anything short of a symbolic decision, this is the call to use.
    subroutine check_identity(s, eng, label, e)
        type(suite_t),   intent(inout) :: s
        class(engine_t), intent(inout) :: eng
        character(*),    intent(in)    :: label
        type(expr_t),    intent(in)    :: e

        type(engine_result_t) :: r

        s%total = s%total + 1
        r = eng%zero_test(e)

        if (r%verdict == VERDICT_TRUE) then
            call record_pass(s, label, .true., 0, 0)
        else if (r%verdict == VERDICT_FALSE) then
            call record_fail(s, label, e, "engine decided it is not zero")
        else
            call record_fail(s, label, e, &
                "undecided; check_identity does not accept probe evidence")
        end if
    end subroutine check_identity

    !> Evaluate at many pseudo-random points and report whether the expression
    !> vanished at every evaluable one.
    !>
    !> This is a Schwartz-Zippel style test: passing means no counterexample was
    !> found, which is strong evidence and not a proof. Points where the
    !> expression is undefined -- poles, branch cuts -- are skipped rather than
    !> counted as failures, because a probe deliberately samples arbitrary
    !> places and hitting one is expected.
    subroutine probe_zero(e, clean, tried, agreed)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: clean
        integer,      intent(out) :: tried, agreed

        type(binding_t) :: b
        type(str_t), allocatable :: names(:)
        real(dp) :: v, scale
        integer  :: k, j
        integer(int64) :: seed
        logical  :: defined

        clean = .true.
        tried = 0
        agreed = 0

        call collect_free_symbols(e, names)
        b%n = size(names)
        allocate (b%names(b%n), b%values(b%n))
        if (b%n > 0) b%names = names

        ! A fixed seed, so a failing check reproduces exactly. A probe that
        ! passed yesterday and fails today for want of the same points would be
        ! useless as a regression test.
        seed = 20260727_int64

        do k = 1, PROBE_POINTS
            do j = 1, b%n
                seed = next_seed(seed)
                ! Spread over a few orders of magnitude and both signs, so the
                ! points are not all in one comfortable region.
                b%values(j) = unit_from(seed)*3.0_dp - 1.5_dp
            end do

            v = eval_expr(e, b, defined)
            if (.not. defined) cycle

            tried = tried + 1
            scale = max(1.0_dp, abs(v))
            if (abs(v) <= PROBE_TOL*scale) then
                agreed = agreed + 1
            else
                clean = .false.
                return
            end if
        end do
    end subroutine probe_zero

    !> Linear congruential step. Deterministic and self-contained: the standard
    !> random_number would depend on the compiler's seeding and make a failure
    !> unreproducible.
    pure function next_seed(seed) result(nxt)
        integer(int64), intent(in) :: seed
        integer(int64)             :: nxt
        integer(int64), parameter :: A = 6364136223846793005_int64
        integer(int64), parameter :: C = 1442695040888963407_int64
        nxt = A*seed + C
    end function next_seed

    pure function unit_from(seed) result(u)
        integer(int64), intent(in) :: seed
        real(dp)                   :: u
        integer(int64) :: bits
        ! Top bits only: the low bits of a linear congruential generator have
        ! short periods.
        bits = ishft(abs(seed), -20)
        u = real(mod(bits, 1000000_int64), dp)/1000000.0_dp
    end function unit_from

    subroutine record_pass(s, label, proved, tried, agreed)
        type(suite_t), intent(inout) :: s
        character(*),  intent(in)    :: label
        logical,       intent(in)    :: proved
        integer,       intent(in)    :: tried, agreed

        s%passed = s%passed + 1
        if (proved) then
            s%proved = s%proved + 1
            write (output_unit, "(a)") "PASS         "//label
        else
            s%by_probe = s%by_probe + 1
            write (output_unit, "(a)") "PASS(probe)  "//label// &
                "   [no counterexample in "//chars(str(tried))//" points]"
        end if
    end subroutine record_pass

    subroutine record_fail(s, label, e, why)
        type(suite_t), intent(inout) :: s
        character(*),  intent(in)    :: label
        type(expr_t),  intent(in)    :: e
        character(*),  intent(in)    :: why

        s%failed = s%failed + 1
        ! The residual, not just a boolean. Every notebook harness this replaces
        ! prints the unsimplified residual on failure, because that is what a
        ! human needs in order to see which term is wrong.
        write (output_unit, "(a)") "FAIL         "//label//"  -> "// &
            chars(print_expr(e))
        write (output_unit, "(a)") "               ("//why//")"
    end subroutine record_fail

    !> Summary, optional JSON, and the exit status.
    !>
    !> The exit status is what makes a suite usable from a build: a runner needs
    !> to know a check failed without parsing output.
    subroutine suite_end(s, json_path)
        type(suite_t),          intent(inout) :: s
        character(*), optional, intent(in)    :: json_path
        integer :: unit, ios

        write (output_unit, "(a)") ""
        write (output_unit, "(a)") "suite "//chars(s%name)//": "// &
            chars(str(s%passed))//"/"//chars(str(s%total))//" passed  ("// &
            chars(str(s%proved))//" proved, "// &
            chars(str(s%by_probe))//" by probe)"

        if (present(json_path)) then
            open (newunit=unit, file=json_path, status="replace", &
                action="write", iostat=ios)
            if (ios == 0) then
                write (unit, "(a)") "{"
                write (unit, "(a)") '  "suite": "'//chars(s%name)//'",'
                write (unit, "(a)") '  "total": '//chars(str(s%total))//","
                write (unit, "(a)") '  "passed": '//chars(str(s%passed))//","
                write (unit, "(a)") '  "proved": '//chars(str(s%proved))//","
                write (unit, "(a)") '  "by_probe": '//chars(str(s%by_probe))//","
                write (unit, "(a)") '  "failed": '//chars(str(s%failed))
                write (unit, "(a)") "}"
                close (unit)
            end if
        end if

        if (s%failed /= 0) error stop 1
    end subroutine suite_end

end module fortsym_check
