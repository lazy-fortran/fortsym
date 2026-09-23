!> Goal-oriented certificates for periodic advection-diffusion,
!>   -kappa f'' + v f' = s  on the circle, mean-zero functions,
!> in the orthonormal Fourier basis cos(k x), sin(k x), k = 1, 2.
!>
!> S = -kappa d^2/dx^2 is symmetric and preserves the cos/sin parity;
!> A = v d/dx is skew and flips it. The example runs the loop
!> FORWARD -> ADJOINT -> CERTIFY -> INDICATE:
!>
!> 1. the symmetric output q = <s, K^{-1} s> with the Ritz/complementary
!>    pair, checked against the textbook Fourier value;
!> 2. the non-symmetric output J = <a, K^{-1} b> (a = sin x, b = cos x) with
!>    the residual-pairing (DWR) estimate and its rigorous radius, checked
!>    against J = v/(kappa**2 + v**2);
!> 3. per-mode indicators of the gap and of both residual norms;
!> 4. dJ/dv from the same primal and dual solutions;
!> 5. the skew-symmetrised variable-velocity advection v(x) d/dx + v'(x)/2,
!>    whose adjoint fortsym derives by integration by parts.
program example_certificate_advection_diffusion
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym, only: arena_t, expr_t, sym, num, rat, func, operator(+), &
        operator(-), operator(*), operator(/), operator(**), numeric_value, &
        str, chars
    use fortsym_subs, only: subs_many
    use fortsym_obligation, only: obligation_ledger_t, ledger_holds, ledger_report, &
        prove_zero
    use fortsym_certificate, only: operator_split_t, split_bounds_t, dwr_t, &
        derivation_t, first_order_t, split_obligations, split_bounds, split_exact, &
        split_indicators, split_dwr, dwr_exact, dwr_gradient, first_order_adjoint
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine, only: engine_result_t
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, ball_runtime, &
        emit_rigorous_kernel
    implicit none

    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(operator_split_t) :: split
    type(split_bounds_t) :: b
    type(dwr_t) :: d
    type(obligation_ledger_t) :: ledger
    type(expr_t) :: kappa, v, zero, one, s(4), asrc(4), bsrc(4), q, jx, dj
    type(expr_t), allocatable :: u_opt(:), w_opt(:), f_opt(:), g_opt(:), eta(:)
    type(expr_t) :: olds(2), vals(2)
    real(dp) :: jn, est, rad, dval
    integer :: failures, k

    failures = 0
    kappa = sym(arena, "kappa")
    v = sym(arena, "v")
    zero = num(arena, 0_int64)
    one = num(arena, 1_int64)
    ! Unknowns: cos x, sin x, cos 2x, sin 2x. d/dx cos kx = -k sin kx.
    split%parity = [0, 1, 0, 1]
    split%sigma = [kappa, kappa, 4*kappa, 4*kappa]
    allocate (split%a(4, 4))
    split%a = zero
    do k = 1, 2
        split%a(2*k, 2*k - 1) = -k*v
        split%a(2*k - 1, 2*k) = k*v
    end do
    call split_obligations(split, ledger, "advection-diffusion")

    ! 1. symmetric output for s = cos x + 2 cos 2x
    s = [one, zero, 2*one, zero]
    b = split_bounds(split, s, "t")
    call split_exact(split, s, b, ledger, "advection-diffusion", q, u_opt, w_opt)
    call prove_zero(ledger, "q equals the Fourier value", q - &
        (kappa/(kappa**2 + v**2) + 4*4*kappa/(16*kappa**2 + 4*v**2)))

    ! 3. per-mode gap indicators
    eta = split_indicators(split, s, b, ledger, "advection-diffusion")

    ! 2. non-symmetric output J = <sin x, K^{-1} cos x>
    asrc = [zero, one, zero, zero]
    bsrc = [one, zero, zero, zero]
    d = split_dwr(split, asrc, bsrc, "t")
    call dwr_exact(split, asrc, bsrc, d, ledger, "advection-diffusion", jx, f_opt, g_opt)
    call prove_zero(ledger, "J equals v/(kappa**2 + v**2)", jx - v/(kappa**2 + v**2))

    ! 4. gradient from the dual information
    dj = dwr_gradient(split, asrc, bsrc, d, v)
    call prove_zero(ledger, "dJ/dv from primal and dual solutions", &
        subs_many(dj, [d%f, d%g], [f_opt, g_opt]) - derivative(jx, v))

    ! 5. adjoint of the skew-symmetrised variable-velocity advection
    call variable_velocity_adjoint()

    call ledger_report(ledger)
    if (.not. ledger_holds(ledger)) failures = failures + 1

    ! Numeric DWR check at kappa = 1/2, v = 3/4 with perturbed trials.
    olds = [kappa, v]
    vals = [r(1, 2), r(3, 4)]
    jn = value(subs_many(jx, olds, vals))
    call perturbed_dwr(est, rad)
    print '(a,es23.15,a,es23.15,a,es10.3)', "J ", jn, "  estimate ", est, &
        "  radius ", rad
    if (abs(jn - est) > rad) failures = failures + 1
    dval = value(subs_many(derivative(jx, v), olds, vals))
    print '(a,es23.15)', "dJ/dv ", dval

    call emit_dwr_kernel()
    if (failures > 0) error stop 1
    print '(a)', "example_certificate_advection_diffusion: all checks passed"

contains

    function r(a, c) result(e)
        integer, intent(in) :: a, c
        type(expr_t) :: e

        e = rat(arena, int(a, int64), int(c, int64))
    end function r

    function derivative(e, x) result(de)
        type(expr_t), intent(in) :: e, x
        type(expr_t) :: de
        type(native_engine_t) :: eng
        type(engine_result_t) :: res

        eng = make_native_engine(arena)
        res = eng%diff(e, x)
        de = res%value
    end function derivative

    real(dp) function value(e)
        type(expr_t), intent(in) :: e
        logical :: ok
        character(:), allocatable :: why

        call numeric_value(e, value, ok, why)
        if (.not. ok) error stop "example_certificate_advection_diffusion: no value"
    end function value

    !> Trials f, g = exact solutions plus dyadic perturbations; the
    !> constraint set is empty here (no null level on mean-zero functions).
    subroutine perturbed_dwr(est, rad)
        real(dp), intent(out) :: est, rad
        type(expr_t) :: ft(4), gt(4)
        type(expr_t), allocatable :: all_old(:), all_new(:)
        integer :: i

        do i = 1, 4
            ft(i) = subs_many(f_opt(i), olds, vals) + r(i, 64)
            gt(i) = subs_many(g_opt(i), olds, vals) - r(1, 32*i)
        end do
        all_old = [d%f, d%g, olds]
        all_new = [ft, gt, vals]
        est = value(subs_many(d%estimate, all_old, all_new))
        rad = sqrt(value(subs_many(d%primal_norm2, all_old, all_new))* &
            value(subs_many(d%dual_norm2, all_old, all_new)))
    end subroutine perturbed_dwr

    subroutine variable_velocity_adjoint()
        type(derivation_t) :: dx
        type(first_order_t) :: adv, adj
        type(expr_t) :: x, vel
        type(native_engine_t) :: eng
        type(engine_result_t) :: res

        x = sym(arena, "x")
        vel = func("vel", [x])
        dx%coords = [x]
        dx%coeffs = [one]
        dx%wavenumbers = [sym(arena, "kx")]
        eng = make_native_engine(arena)
        res = eng%diff(vel, x)
        adv%a = vel
        adv%b = res%value/2
        adj = first_order_adjoint(adv, dx, one)
        call prove_zero(ledger, "v d/dx + v'/2 is skew: derivative part", adj%a + adv%a)
        call prove_zero(ledger, "v d/dx + v'/2 is skew: multiplier part", adj%b + adv%b)
    end subroutine variable_velocity_adjoint

    subroutine emit_dwr_kernel()
        type(rigorous_kernel_spec_t) :: spec
        logical :: ok
        character(:), allocatable :: why
        integer :: i

        spec%name = str("dwr_estimate")
        allocate (spec%args(10))
        do i = 1, 4
            spec%args(i) = str("tf"//achar(48 + i))
            spec%args(4 + i) = str("tg"//achar(48 + i))
        end do
        spec%args(9) = str("kappa")
        spec%args(10) = str("v")
        spec%outputs = [str("estimate"), str("primal2"), str("dual2")]
        spec%runtime = ball_runtime()
        print '(a)', chars(emit_rigorous_kernel([d%estimate, d%primal_norm2, &
            d%dual_norm2], spec, ok, why))
        if (.not. ok) failures = failures + 1
    end subroutine emit_dwr_kernel

end program example_certificate_advection_diffusion
