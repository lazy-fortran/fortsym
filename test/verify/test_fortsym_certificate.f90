!> Certificate derivations: ladder skewness by integration by parts, the
!> level-0 constraint, the proof-obligation ledger (decided, probed, failed),
!> and the gap decomposition into nonnegative indicators. Negative cases
!> check that a wrong operator or a wrong integrating factor is caught.
program test_fortsym_certificate
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym, only: arena_t, expr_t, sym, num, rat, func, gamma, sin, cos, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine, only: engine_result_t
    use fortsym_subs, only: subs_many
    use fortsym_obligation, only: obligation_ledger_t, prove_zero, ledger_holds, &
        OBLIGATION_PROVED, OBLIGATION_PROBED, OBLIGATION_FAILED
    use fortsym_certificate, only: derivation_t, first_order_t, operator_split_t, &
        split_bounds_t, ladder_obligations, constraint_obligations, &
        first_order_adjoint, apply_derivation, split_bounds, split_indicators
    implicit none

    type(arena_t), target :: arena
    integer :: failures = 0
    type(expr_t) :: x, c, l, nu, rho, one, zero

    x = sym(arena, "x")
    c = sym(arena, "c")
    l = sym(arena, "l")
    nu = sym(arena, "nu")
    one = num(arena, 1_int64)
    zero = num(arena, 0_int64)
    rho = func("rho", [x])

    call test_ladder()
    call test_constraint()
    call test_ledger_verdicts()
    call test_indicators()
    if (failures > 0) then
        print '(a,i0)', "FAIL  certificate checks: ", failures
        error stop 1
    end if
    print '(a)', "PASS  certificate derivations"

contains

    subroutine check(label, cond)
        character(*), intent(in) :: label
        logical, intent(in) :: cond

        if (.not. cond) then
            failures = failures + 1
            print '(a)', "FAIL  "//label
        else
            print '(a)', "PASS  "//label
        end if
    end subroutine check

    function derivation() result(d)
        type(derivation_t) :: d

        d%coords = [x]
        d%coeffs = [c]
        d%wavenumbers = [sym(arena, "k")]
    end function derivation

    function drho() result(e)
        type(expr_t) :: e
        type(derivation_t) :: d

        d = derivation()
        e = apply_derivation(d, rho)
    end function drho

    subroutine test_ladder()
        type(obligation_ledger_t) :: good, bad
        type(first_order_t) :: up, dn, wrong
        type(expr_t) :: weight

        good%verbose = .false.
        bad%verbose = .false.
        weight = 1/rho**2
        up%a = rho*(l + 1)
        up%b = l/2*drho()
        dn%a = rho*(l + 1)
        dn%b = -(l/2 + l + 1)*drho()
        call ladder_obligations(good, "toy ladder", derivation(), weight, &
            nu*l*(l + 1), up, dn, l)
        call check("skew ladder discharges every obligation", ledger_holds(good))
        wrong = dn
        wrong%b = -(l/2)*drho()
        call ladder_obligations(bad, "toy ladder", derivation(), weight, &
            nu*l*(l + 1), up, wrong, l)
        call check("missing weight derivative in the partner is caught", &
            .not. ledger_holds(bad))
        call check("the failure is the multiplier part", &
            bad%items(bad%n)%status == OBLIGATION_FAILED)
    end subroutine test_ladder

    subroutine test_constraint()
        type(obligation_ledger_t) :: good, bad
        type(first_order_t) :: up0
        type(expr_t) :: g, s0

        good%verbose = .false.
        bad%verbose = .false.
        up0%a = rho
        up0%b = zero
        s0 = drho()/rho
        g = constraint_obligations(good, "toy constraint", derivation(), up0, s0, &
            one, [c/rho])
        call check("constraint with integrating factor 1 holds", ledger_holds(good))
        g = constraint_obligations(bad, "toy constraint", derivation(), up0, s0, &
            rho, [c/rho**2])
        call check("wrong integrating factor is caught", &
            bad%items(1)%status == OBLIGATION_FAILED)
    end subroutine test_constraint

    subroutine test_ledger_verdicts()
        type(obligation_ledger_t) :: led

        led%verbose = .false.
        call prove_zero(led, "pythagoras", sin(x)**2 + cos(x)**2 - 1)
        call check("decidable identity is PROVED", led%items(1)%status == OBLIGATION_PROVED)
        call prove_zero(led, "gamma recurrence", gamma(x + 1) - x*gamma(x))
        call check("undecided identity is PROBED", led%items(2)%status == OBLIGATION_PROBED)
        call prove_zero(led, "false identity", gamma(x + 1) - gamma(x))
        call check("false identity FAILS", led%items(3)%status == OBLIGATION_FAILED)
        call prove_zero(led, "jet identity", apply_derivation(derivation(), 1/rho) + &
            drho()/rho**2)
        call check("jet identity is PROVED", led%items(4)%status == OBLIGATION_PROVED)
        led%allow_probe = .false.
        call check("probe evidence rejected when probes are disallowed", &
            .not. ledger_holds(led))
    end subroutine test_ledger_verdicts

    subroutine test_indicators()
        type(obligation_ledger_t) :: led
        type(operator_split_t) :: p
        type(split_bounds_t) :: b
        type(expr_t), allocatable :: eta(:)
        type(expr_t) :: s(3)

        led%verbose = .false.
        p%parity = [0, 1, 0]
        p%sigma = [zero, rat(arena, 3_int64, 2_int64), nu]
        allocate (p%a(3, 3))
        p%a = zero
        p%a(1, 2) = c
        p%a(2, 1) = -c
        p%a(3, 2) = l
        p%a(2, 3) = -l
        s = [one, zero, 2*one]
        b = split_bounds(p, s, "t")
        eta = split_indicators(p, s, b, led, "indicators")
        call check("gap equals the sum of indicators", ledger_holds(led))
        call check("one constraint row for the null level", size(b%constraints) == 1)
        call check("null level carries no indicator", eta(1)%id == zero%id)
    end subroutine test_indicators

end program test_fortsym_certificate
