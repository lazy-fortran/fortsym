program test_fortsym_identities
    ! The gate that decides whether fortsym behaves like a computer algebra
    ! system rather than a pattern matcher.
    !
    ! Three classes of assertion, and the second two matter more than the first:
    !
    !   MUST_ZERO     a true identity the engine has to decide.
    !   MUST_NOT_ZERO an expression that must never be *claimed* zero. Both
    !                 NONZERO and UNKNOWN satisfy this, because UNKNOWN makes no
    !                 claim. Several entries here vanish on part of the domain
    !                 only -- an engine that "proved" them would be wrong.
    !   MUST_UNKNOWN  a true identity that lies outside the decidable fragment.
    !                 The engine has to admit that rather than guess, because a
    !                 confident wrong verdict is acted on and a refusal is not.
    !
    ! Expressions are written in fortsym's own notation and parsed, so the test
    ! reads like the mathematics it checks and exercises the parser at the same
    ! time.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_parse, only: parse_expr
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE, verdict_name
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    implicit none

    integer, parameter :: dp = real64

    integer, parameter :: MUST_ZERO = 1
    integer, parameter :: MUST_NOT_ZERO = 2
    integer, parameter :: MUST_UNKNOWN = 3

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    integer :: nfail = 0, nrun = 0
    real(dp) :: total_seconds = 0.0_dp

    call arena%init()
    eng = make_symengine_engine(arena)

    print *, "engine: ", chars(eng%name)
    print *, ""
    print *, "-- true identities, must be decided ZERO --"
    call check("sin(x)**2 + cos(x)**2 - 1", MUST_ZERO)
    call check("cosh(x)**2 - sinh(x)**2 - 1", MUST_ZERO)
    call check("sin(x + y) - sin(x)*cos(y) - cos(x)*sin(y)", MUST_ZERO)
    call check("cos(x + y) - cos(x)*cos(y) + sin(x)*sin(y)", MUST_ZERO)
    call check("tan(x + y) - (tan(x) + tan(y))/(1 - tan(x)*tan(y))", MUST_ZERO)
    call check("sin(2*x) - 2*sin(x)*cos(x)", MUST_ZERO)
    call check("cos(2*x) - 1 + 2*sin(x)**2", MUST_ZERO)
    call check("sin(3*x) - 3*sin(x) + 4*sin(x)**3", MUST_ZERO)
    call check("tanh(x) - sinh(x)/cosh(x)", MUST_ZERO)
    call check("exp(log(x)) - x", MUST_ZERO)
    call check("exp(2*log(x)) - x**2", MUST_ZERO)
    call check("exp(x + y) - exp(x)*exp(y)", MUST_ZERO)
    call check("exp(x) - exp(x/2)**2", MUST_ZERO)
    call check("sin(x) - 2*sin(x/2)*cos(x/2)", MUST_ZERO)
    call check("(x**2 - 1)/(x - 1) - (x + 1)", MUST_ZERO)
    call check("1/(x - 1) + 1/(x + 1) - 2*x/(x**2 - 1)", MUST_ZERO)
    call check("(x**3 - y**3)/(x - y) - (x**2 + x*y + y**2)", MUST_ZERO)
    call check("sin(x)*cos(y) - (sin(x + y) + sin(x - y))/2", MUST_ZERO)

    print *, ""
    print *, "-- must never be claimed ZERO --"
    call check("sin(x)**2 + cos(x)**2 - 2", MUST_NOT_ZERO)
    call check("sin(x + y) - sin(x)*cos(y) - cos(x)*sin(y) + 1", MUST_NOT_ZERO)
    call check("x - y", MUST_NOT_ZERO)
    call check("exp(x) - exp(2*x)", MUST_NOT_ZERO)
    ! The next three hold only on part of the domain. Claiming them as
    ! identities would be a correctness bug, not a missed simplification.
    call check("sqrt(x**2) - x", MUST_NOT_ZERO)
    call check("log(exp(x)) - x", MUST_NOT_ZERO)
    call check("atan(x) + atan(1/x) - pi/2", MUST_NOT_ZERO)
    ! Regression. An unknown function head used to collapse to a bare symbol,
    ! discarding its arguments, so every application of the same head became
    ! identical and these decided to ZERO. A wrong ZERO is the one failure this
    ! design exists to prevent, so both directions are pinned here.
    call check("f(x) - f(y)", MUST_NOT_ZERO)
    call check("besselj(0, x) - besselj(1, x)", MUST_NOT_ZERO)

    print *, ""
    print *, "-- outside the decidable fragment, must be UNKNOWN --"
    ! rewrite_as_exp silently passes gamma and its relatives through unchanged,
    ! so without a fragment guard these were reported as non-identities even
    ! though they are true. Admitting ignorance is the correct answer.
    call check("gamma(x + 1) - x*gamma(x)", MUST_UNKNOWN)
    call check("loggamma(x + 1) - loggamma(x) - log(x)", MUST_UNKNOWN)
    ! An opaque head must keep its arguments well enough that a genuine
    ! cancellation still works.
    call check("f(x) - f(x)", MUST_ZERO)

    print *, ""
    print *, "checks:", nrun, " failed:", nfail
    write (*, '(a,f8.4,a)') " total engine time: ", total_seconds, " s"

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_identities: all checks passed"

contains

    subroutine check(text, expectation)
        character(*), intent(in) :: text
        integer,      intent(in) :: expectation
        type(expr_t) :: e
        type(engine_result_t) :: r
        character(:), allocatable :: message
        logical :: good, passed

        nrun = nrun + 1

        e = parse_expr(arena, text, good, message)
        if (.not. good) then
            nfail = nfail + 1
            print *, "  PARSE-FAIL ", text, " : ", message
            return
        end if

        r = eng%zero_test(e)
        total_seconds = total_seconds + r%seconds

        if (.not. r%ok) then
            nfail = nfail + 1
            print *, "  ENGINE-FAIL ", text
            return
        end if

        select case (expectation)
        case (MUST_ZERO)
            passed = r%verdict == VERDICT_TRUE
        case (MUST_NOT_ZERO)
            ! UNKNOWN is acceptable: it makes no claim.
            passed = r%verdict /= VERDICT_TRUE
        case (MUST_UNKNOWN)
            passed = r%verdict == VERDICT_UNKNOWN
        case default
            passed = .false.
        end select

        if (passed) then
            print *, "  ok   ", chars(verdict_name(r%verdict)), "  ", text
        else
            nfail = nfail + 1
            print *, "  FAIL ", chars(verdict_name(r%verdict)), "  ", text
        end if
    end subroutine check

end program test_fortsym_identities
