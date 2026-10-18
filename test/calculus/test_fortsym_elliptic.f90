program test_fortsym_elliptic
    ! Complete elliptic integrals K(m), E(m) in the parameter m = k**2.
    ! Independent oracles:
    !   * DLMF 19.4.1 written in the modulus k, dK/dk = (E - k'**2 K)/(k k'**2)
    !     and dE/dk = (E - K)/k, proved against the chain rule through m = k**2;
    !   * the Legendre relation (DLMF 19.7.1) at m = 1/2, which ties the
    !     numerical evaluator to pi without using the AGM iteration's own form;
    !   * published values K(1/2), E(1/2) (DLMF 19.20 / A&S table 17.1) and
    !     centred finite differences of the numerical evaluator.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, elliptic_k, elliptic_e, pi_expr, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==)
    use fortsym_diff, only: diff
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_print, only: print_expr_in
    use fortsym_dialect, only: dialect, DIA_WOLFRAM, DIA_MAXIMA
    implicit none

    integer, parameter :: dp = real64
    real(dp), parameter :: k_half = 1.854074677301371918433850347195260_dp
    real(dp), parameter :: e_half = 1.350643881047675502520175001612783_dp
    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(suite_t) :: suite
    type(expr_t) :: k, m, kk, ee, dkdk, dedk, got
    type(binding_t) :: bindings
    real(dp) :: point, step, symbolic_value, fd_value, kv, ev
    logical :: defined
    integer :: nfail, i

    nfail = 0
    call arena%init()
    engine = make_native_engine(arena)
    k = sym(arena, "k")
    m = sym(arena, "m")

    call suite_begin(suite, "complete elliptic integrals")
    kk = elliptic_k(k**2)
    ee = elliptic_e(k**2)
    dkdk = diff(kk, k)
    dedk = diff(ee, k)
    call check_identity(suite, engine, "dK/dk = (E - (1-k^2) K)/(k (1-k^2))", &
        dkdk - (ee - (1 - k**2)*kk)/(k*(1 - k**2)))
    call check_identity(suite, engine, "dE/dk = (E - K)/k", &
        dedk - (ee - kk)/k)
    ! Legendre's ODE for K in the modulus (DLMF 19.4.6 rearranged):
    ! d/dk (k k'^2 dK/dk) = k K.
    call check_identity(suite, engine, "Legendre ODE for K", &
        diff(k*(1 - k**2)*dkdk, k) - k*kk)
    call check_identity(suite, engine, "K(0) = pi/2", &
        elliptic_k(num(arena, 0)) - pi_expr(arena)/2)
    call check_identity(suite, engine, "E(0) = pi/2", &
        elliptic_e(num(arena, 0)) - pi_expr(arena)/2)
    call check_identity(suite, engine, "E(1) = 1", &
        elliptic_e(num(arena, 1)) - 1)
    call suite_end(suite)

    allocate (bindings%names(1), bindings%values(1))
    bindings%names(1) = str("m")
    bindings%n = 1
    bindings%values(1) = 0.5_dp
    kv = eval_expr(elliptic_k(m), bindings, defined)
    call check("K(1/2) evaluates", defined)
    call check("K(1/2) matches the tabulated value", abs(kv - k_half) < 1.0e-14_dp)
    ev = eval_expr(elliptic_e(m), bindings, defined)
    call check("E(1/2) matches the tabulated value", abs(ev - e_half) < 1.0e-14_dp)
    ! Legendre relation at m = 1/2: 2 E K - K**2 = pi/2.
    call check("Legendre relation at m = 1/2", &
        abs(2*ev*kv - kv**2 - 2*atan(1.0_dp)) < 1.0e-13_dp)
    bindings%values(1) = 1.0_dp
    kv = eval_expr(elliptic_k(m), bindings, defined)
    call check("K(1) has no real value", .not. defined)

    step = 1.0e-5_dp
    do i = 0, 5
        point = -0.7_dp + 0.33_dp*i
        call fd_check("dK/dm", elliptic_k(m), point)
        call fd_check("dE/dm", elliptic_e(m), point)
    end do

    got = diff(elliptic_k(m), sym(arena, "x"))
    call check("K(m) is constant in an unrelated symbol", got == num(arena, 0))
    call check("Wolfram spelling EllipticK", &
        chars(print_expr_in(elliptic_k(m), dialect(DIA_WOLFRAM))) == "EllipticK[m]")
    call check("Maxima spelling elliptic_ec", &
        chars(print_expr_in(elliptic_e(m), dialect(DIA_MAXIMA))) == "elliptic_ec(m)")

    if (nfail /= 0) error stop 1
    write (*, "(a)") "elliptic checks passed"

contains

    subroutine fd_check(label, f, at)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp), intent(in) :: at
        real(dp) :: plus, minus
        logical :: d1, d2, d3

        bindings%values(1) = at
        symbolic_value = eval_expr(diff(f, m), bindings, d1)
        bindings%values(1) = at + step
        plus = eval_expr(f, bindings, d2)
        bindings%values(1) = at - step
        minus = eval_expr(f, bindings, d3)
        fd_value = (plus - minus)/(2*step)
        call check(label//" agrees with a centred finite difference", &
            d1 .and. d2 .and. d3 .and. &
            abs(symbolic_value - fd_value) < 1.0e-7_dp*max(1.0_dp, abs(fd_value)))
    end subroutine fd_check

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (condition) then
            write (*, "(a)") "PASS "//label
        else
            write (*, "(a)") "FAIL "//label
            nfail = nfail + 1
        end if
    end subroutine check

end program test_fortsym_elliptic
