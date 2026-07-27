program test_fortsym_check
    ! The harness itself, checked on its own behaviour.
    !
    ! suite_end stops the program when anything failed, which is exactly what a
    ! ported notebook suite needs and exactly what makes the harness awkward to
    ! test. So the failure paths are exercised through the pieces underneath --
    ! probe_zero and the evaluator -- and only the passing paths go through the
    ! full suite.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_parse, only: parse_expr
    use fortsym_eval, only: binding_t, eval_expr, free_symbols_of
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_check
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    type(suite_t)            :: s
    integer :: nfail = 0

    call arena%init()
    eng = make_symengine_engine(arena)

    call test_evaluator()
    call test_undefined_points()
    call test_free_symbols()
    call test_probe_separates_identities()

    ! The passing paths, through the real harness. A true identity inside the
    ! decidable fragment must be proved; one outside it must pass by probe. The
    ! distinction being visible in the output is the point.
    call suite_begin(s, "harness self-check")
    call check_zero(s, eng, "pythagorean (decidable)", &
        parsed("sin(x)**2 + cos(x)**2 - 1"))
    call check_zero(s, eng, "rational cancellation (decidable)", &
        parsed("(x**2 - 1)/(x - 1) - (x + 1)"))
    call check_zero(s, eng, "gamma recurrence (probe only)", &
        parsed("gamma(x + 1) - x*gamma(x)"))
    call check_identity(s, eng, "strict variant accepts a proof", &
        parsed("cosh(x)**2 - sinh(x)**2 - 1"))

    call check_counts()

    if (nfail /= 0) then
        print *, "test_fortsym_check: ", nfail, " check(s) FAILED"
        error stop 1
    end if

    ! suite_end stops on failure, so reaching past it is itself an assertion.
    call suite_end(s, "/tmp/fortsym_selfcheck.json")
    print *, "test_fortsym_check: all checks passed"

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    function parsed(text) result(e)
        character(*), intent(in) :: text
        type(expr_t)             :: e
        character(:), allocatable :: message
        logical :: good
        e = parse_expr(arena, text, good, message)
        if (.not. good) then
            nfail = nfail + 1
            print *, "PARSE-FAIL ", text, " : ", message
        end if
    end function parsed

    function bind1(name, value) result(b)
        character(*), intent(in) :: name
        real(dp),     intent(in) :: value
        type(binding_t)          :: b
        b%n = 1
        allocate (b%names(1), b%values(1))
        b%names(1) = str(name)
        b%values(1) = value
    end function bind1

    subroutine test_evaluator()
        type(binding_t) :: b
        real(dp) :: v
        logical :: defined

        b = bind1("x", 2.0_dp)

        v = eval_expr(parsed("x + 1"), b, defined)
        call ok("evaluates a sum", defined .and. abs(v - 3.0_dp) < 1.0e-14_dp)

        v = eval_expr(parsed("x**3"), b, defined)
        call ok("evaluates a power", defined .and. abs(v - 8.0_dp) < 1.0e-14_dp)

        ! An exact rational must not have been truncated by integer division on
        ! its way through the arena.
        v = eval_expr(parsed("1/4"), b, defined)
        call ok("rational is exact", defined .and. abs(v - 0.25_dp) < 1.0e-14_dp)

        v = eval_expr(parsed("sin(x)**2 + cos(x)**2"), b, defined)
        call ok("evaluates functions", defined .and. abs(v - 1.0_dp) < 1.0e-14_dp)

        v = eval_expr(parsed("pi"), b, defined)
        call ok("knows pi", defined .and. abs(v - 3.14159265358979_dp) < 1.0e-12_dp)

        ! atan2 argument order must match the convention fixed at construction.
        v = eval_expr(parsed("atan2(1, 0)"), b, defined)
        call ok("atan2 order", defined .and. &
            abs(v - 1.5707963267948966_dp) < 1.0e-12_dp)
    end subroutine test_evaluator

    !> A probe samples arbitrary points, so hitting a pole or a branch cut is an
    !> ordinary event that must be reported rather than signalled.
    subroutine test_undefined_points()
        type(binding_t) :: b
        real(dp) :: v
        logical :: defined

        b = bind1("x", -1.0_dp)
        v = eval_expr(parsed("log(x)"), b, defined)
        call ok("log of a negative is undefined", .not. defined)

        v = eval_expr(parsed("sqrt(x)"), b, defined)
        call ok("sqrt of a negative is undefined", .not. defined)

        ! A negative base with an integer exponent is perfectly well defined,
        ! and must not be lumped in with the above.
        v = eval_expr(parsed("x**2"), b, defined)
        call ok("negative base, integer power is defined", &
            defined .and. abs(v - 1.0_dp) < 1.0e-14_dp)

        b = bind1("x", 0.0_dp)
        v = eval_expr(parsed("1/x"), b, defined)
        call ok("pole is undefined", .not. defined)

        ! An unbound symbol has no value; inventing one would make the probe
        ! agree with anything.
        b%n = 0
        v = eval_expr(parsed("y + 1"), b, defined)
        call ok("unbound symbol is undefined", .not. defined)

        ! Likewise an unknown function head.
        b = bind1("x", 1.0_dp)
        v = eval_expr(parsed("besselj(x)"), b, defined)
        call ok("unknown function is undefined", .not. defined)
    end subroutine test_undefined_points

    subroutine test_free_symbols()
        type(str_t), allocatable :: names(:)

        names = free_symbols_of(parsed("a*b + sin(a)"))
        call ok("finds both symbols", size(names) == 2)

        names = free_symbols_of(parsed("pi + 1"))
        call ok("constants are not free symbols", size(names) == 0)
    end subroutine test_free_symbols

    !> The probe must accept true identities and reject false ones, including
    !> ones the symbolic engine cannot decide -- that is the whole reason it
    !> exists as a fallback.
    subroutine test_probe_separates_identities()
        logical :: clean
        integer :: tried, agreed

        call probe_zero(parsed("gamma(x + 1) - x*gamma(x)"), clean, tried, agreed)
        call ok("probe accepts the gamma recurrence", clean .and. tried > 0)

        call probe_zero(parsed("sin(x)**2 + cos(x)**2 - 1"), clean, tried, agreed)
        call ok("probe accepts a true identity", clean .and. tried > 0)

        call probe_zero(parsed("gamma(x + 1) - x*gamma(x) + 1"), clean, tried, agreed)
        call ok("probe rejects a near miss", .not. clean)

        call probe_zero(parsed("x - y"), clean, tried, agreed)
        call ok("probe rejects a non-identity", .not. clean)

        ! Not every point is evaluable, and that must not be mistaken for
        ! agreement: an expression defined nowhere yields no tried points.
        call probe_zero(parsed("besselj(x) - besselj(x) + 1"), clean, tried, agreed)
        call ok("undefined everywhere yields no probe points", tried == 0)
    end subroutine test_probe_separates_identities

    subroutine check_counts()
        call ok("all four suite checks passed", s%passed == 4)
        call ok("three were proved", s%proved == 3)
        call ok("one passed by probe only", s%by_probe == 1)
        call ok("none failed", s%failed == 0)
    end subroutine check_counts

end program test_fortsym_check
