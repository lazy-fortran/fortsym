program test_fortsym_council
    ! Multi-engine behaviour, and the benchmark table that falls out of it.
    !
    ! Which engines are present varies by machine, so this test asserts the
    ! properties that must hold whatever is installed, and prints the rest as
    ! information. The one thing it will not do is fail because Maxima or SymPy
    ! is missing -- Tier 2 engines are optional by design.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, operator(-), operator(==)
    use fortsym_parse, only: parse_expr
    use fortsym_engine, only: VERDICT_TRUE, VERDICT_FALSE, &
        verdict_name, engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_engine_ext, only: make_maxima_engine, make_sympy_engine
    use fortsym_council
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target    :: arena
    type(council_t)          :: council
    type(symengine_engine_t) :: se
    integer :: nfail = 0

    call arena%init()

    se = make_symengine_engine(arena)
    call council_add(council, se)
    call council_add(council, make_maxima_engine(arena))
    call council_add(council, make_sympy_engine(arena))

    print *, "council members:", council%n
    call list_members()

    call test_true_identities()
    call test_non_identities()
    call test_tournament_is_verified()

    print *, ""
    print *, "=== benchmark ==="
    write (*, "(a)") chars(council_benchmark_table(council))

    if (council%n_disagreements > 0) then
        print *, "=== findings ==="
        write (*, "(a)") council%findings%chars()
    end if

    if (nfail /= 0) then
        print *, "test_fortsym_council: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_council: all checks passed"

contains

    subroutine list_members()
        integer :: k
        do k = 1, council%n
            print *, "   - ", chars(council%members(k)%eng%name)
        end do
    end subroutine list_members

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

    !> A true identity must never come back NONZERO from the council, whichever
    !> engines are present. ZERO is the goal; UNKNOWN is acceptable when no
    !> available engine can decide it.
    subroutine test_true_identities()
        print *, ""
        print *, "-- true identities --"
        call vote_check("sin(x)**2 + cos(x)**2 - 1", .true.)
        call vote_check("(x**2 - 1)/(x - 1) - (x + 1)", .true.)
        call vote_check("exp(x + y) - exp(x)*exp(y)", .true.)
        call vote_check("cosh(x)**2 - sinh(x)**2 - 1", .true.)
    end subroutine test_true_identities

    !> A non-identity must never come back ZERO.
    subroutine test_non_identities()
        print *, ""
        print *, "-- non-identities --"
        call vote_check("x - y", .false.)
        call vote_check("sin(x)**2 + cos(x)**2 - 2", .false.)
        call vote_check("sqrt(x**2) - x", .false.)
    end subroutine test_non_identities

    subroutine vote_check(text, is_identity)
        character(*), intent(in) :: text
        logical,      intent(in) :: is_identity
        type(council_decision_t) :: d
        integer :: k

        d = council_zero_test(council, parsed(text), text)

        if (is_identity) then
            call ok("true identity not reported NONZERO: "//text, &
                d%verdict /= VERDICT_FALSE)
        else
            call ok("non-identity not reported ZERO: "//text, &
                d%verdict /= VERDICT_TRUE)
        end if

        write (*, "(a)", advance="no") "   "//chars(verdict_name(d%verdict))// &
            "  "//text//"   ["
        do k = 1, d%n_votes
            if (k > 1) write (*, "(a)", advance="no") " "
            write (*, "(a)", advance="no") chars(d%votes(k)%engine)//"="// &
                chars(verdict_name(d%votes(k)%verdict))
        end do
        write (*, "(a)") "]"

        ! A disagreement is a finding, not a test failure: it means one engine
        ! is wrong, which is exactly what the council exists to surface.
        if (d%disagreement) print *, "      ^ engines disagree"
    end subroutine vote_check

    !> The tournament must never return a form that is not the same function.
    !>
    !> This is the property that makes ranking by size safe. It is checked by
    !> confirming the winner is equivalent to the input, which is the same
    !> condition the tournament itself enforces -- so a regression that skipped
    !> verification would be caught here.
    subroutine test_tournament_is_verified()
        type(expr_t) :: e, best
        integer :: before, after

        print *, ""
        print *, "-- tournament --"

        e = parsed("sin(x)**2 + cos(x)**2")
        before = e%node_count()
        best = council_best_form(council, e, se)
        after = best%node_count()

        call ok("winner is equivalent to input", &
            verdict_of(best - e) == VERDICT_TRUE)
        call ok("winner is no larger than input", after <= before)

        print *, "   nodes before:", before, " after:", after

        ! A form that cannot be improved must come back unchanged rather than
        ! being replaced by something merely different.
        e = parsed("x + y")
        best = council_best_form(council, e, se)
        call ok("irreducible form is unchanged", best == e)
    end subroutine test_tournament_is_verified

    function verdict_of(e) result(v)
        type(expr_t), intent(in) :: e
        integer                  :: v
        type(engine_result_t) :: r
        r = se%zero_test(e)
        v = r%verdict
    end function verdict_of

end program test_fortsym_council
