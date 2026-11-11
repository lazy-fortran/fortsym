program test_fortsym_zero_soundness
    ! Exact zeros that consumers once saw refuted. A zero test may leave them
    ! undecided, but it must never return NONZERO, and it must never return
    ! ZERO for the same residual shifted by one.
    !
    ! Weighted product rule: with w = s**(-alpha) on s > 0, or its real
    ! definition exp(-alpha*log(s)), d(w*N)/ds = w*(N_s - alpha*N/s).
    !
    ! Screw pinch: the ideal-MHD energy density of one Fourier harmonic is a
    ! quadratic in the tangential amplitude Ctan and the divergence D. It
    ! equals k2*(Ctan - center)**2 + gamma_p*D**2 + qmin by completing the
    ! square, so its value at Ctan = center, D = 0 is qmin.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t
    use fortsym_assume, only: assumption_context_t, assume, positive, nonzero, &
        real_valued
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_expr, only: expr_t, sym, rat, exp, log, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_subs, only: subs_many
    implicit none

    type(arena_t), target :: arena
    type(assumption_context_t), target :: assumptions
    type(native_engine_t) :: native
    type(symengine_engine_t) :: symengine
    integer :: failures

    failures = 0
    call arena%init()
    call assumptions%init(arena)
    call weighted_product_rule()
    call screw_pinch_minimum()
    if (failures /= 0) error stop 'zero-test soundness regression failed'
    print *, 'PASS zero-test soundness'

contains

    subroutine weighted_product_rule()
        type(expr_t) :: s, alpha, n, ns, m, flux, xs, eta, half, weight

        s = sym(arena, 's'); alpha = sym(arena, 'alpha'); n = sym(arena, 'N')
        ns = sym(arena, 'N_s'); m = sym(arena, 'm'); flux = sym(arena, 'T')
        xs = sym(arena, 'x_s')
        call assume(assumptions, positive(s))
        call assume(assumptions, real_valued(alpha))
        call assume(assumptions, real_valued(n))
        call assume(assumptions, real_valued(ns))
        call assume(assumptions, real_valued(xs))
        call assume(assumptions, positive(m))
        call assume(assumptions, positive(flux))
        native = make_native_engine(arena, assumptions)
        symengine = make_symengine_engine(arena)
        half = rat(arena, 1_int64, 2_int64)

        call product_rule('symbolic power', s**(-alpha), alpha, s, n, ns)
        call product_rule('exponential of logarithm', exp(-alpha*log(s)), alpha, s, n, ns)
        call product_rule('half power', s**(-half), half, s, n, ns)
        call product_rule('mode-dependent power', s**(-(1 - m*half)), 1 - m*half, s, n, ns)
        call product_rule('mode-dependent exponential', &
            exp(-(1 - m*half)*log(s)), 1 - m*half, s, n, ns)
        ! eta = -T*x_s/(2*m) cancels x_s + 2*m*eta/T under any common weight.
        eta = -flux*xs/(2*m)
        weight = s**(-alpha)
        call sound('weighted cancellation, power', &
            weight*xs + 2*m*(weight*eta)/flux)
        weight = exp(-alpha*log(s))
        call sound('weighted cancellation, exponential', &
            weight*xs + 2*m*(weight*eta)/flux)

    end subroutine weighted_product_rule

    subroutine screw_pinch_minimum()
        type(expr_t) :: r, t, tp, z, zp, m, k, gammap, f, fp, g, h, d, tangent
        type(expr_t) :: mr, k2, ff, gg, jj, qr, qt, qz, divergence, lorentz, raw
        type(expr_t) :: e, center, qmin, old(4), new(4)

        r = real_symbol('r'); t = real_symbol('Btheta'); tp = real_symbol('Btheta_r')
        z = real_symbol('Bz'); zp = real_symbol('Bz_r'); m = real_symbol('m')
        k = real_symbol('k'); gammap = real_symbol('gamma_p'); f = real_symbol('f')
        fp = real_symbol('f_r'); g = real_symbol('g'); h = real_symbol('h')
        d = real_symbol('D'); tangent = real_symbol('Ctan')
        call assume(assumptions, positive(r))
        call assume(assumptions, positive(z))
        call assume(assumptions, positive(gammap))
        call assume(assumptions, nonzero(m))
        mr = m/r
        k2 = mr**2 + k**2
        ff = mr*t + k*z
        gg = mr*z - k*t
        jj = mr*z + k*t
        call assume(assumptions, nonzero(ff))
        native = make_native_engine(arena, assumptions)
        symengine = make_symengine_engine(arena)

        ! Cylindrical curl of xi x B for xi = (f, i*g, i*h), B = (0, Btheta, Bz).
        qr = ff*f
        qt = -k*tangent - tp*f - t*fp
        qz = mr*tangent - z*fp - (zp + z/r)*f
        divergence = fp + f/r - mr*g - k*h
        lorentz = zp*(qz*f - qr*h) - (tp + t/r)*(qr*g - qt*f)
        raw = qr**2 + qt**2 + qz**2 + lorentz &
            - (t*(tp + t/r) + z*zp)*f*divergence + gammap*divergence**2
        center = (gg*fp + jj*f/r)/k2
        qmin = ff**2/k2*fp**2 + 2*(k**2*z**2 - mr**2*t**2)/(r*k2)*f*fp &
            + (ff**2 + (z**2 - t**2)/r**2 - 2*t*tp/r - jj**2/(r**2*k2))*f**2
        ! Eliminate g and h for Ctan = Bz*g - Btheta*h and D, then minimize.
        e = fp + f/r
        old = [g, h, tangent, d]
        new(1) = (t*e + k*center)/ff
        new(2) = (z*e - mr*center)/ff
        new(3) = center
        new(4) = 0*d
        call sound('screw-pinch energy at its minimum', &
            subs_many(raw, old, new) - qmin)
    end subroutine screw_pinch_minimum

    subroutine product_rule(label, w, power, s, n, ns)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: w, power, s, n, ns
        type(expr_t) :: product

        product = w*n
        call sound('product rule, '//label, diff(product, s) &
            + diff(product, n)*ns - w*(ns - power*n/s))
    end subroutine product_rule

    function real_symbol(name) result(expression)
        character(*), intent(in) :: name
        type(expr_t) :: expression

        expression = sym(arena, name)
        call assume(assumptions, real_valued(expression))
    end function real_symbol

    subroutine sound(label, zero)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: zero
        type(engine_result_t) :: result

        result = native%zero_test(zero)
        call check(label//' [native]', result, VERDICT_FALSE)
        result = symengine%zero_test(zero)
        call check(label//' [SymEngine]', result, VERDICT_FALSE)
        result = native%zero_test(zero + 1)
        call check(label//' shifted [native]', result, VERDICT_TRUE)
        result = symengine%zero_test(zero + 1)
        call check(label//' shifted [SymEngine]', result, VERDICT_TRUE)
    end subroutine sound

    subroutine check(label, result, forbidden)
        character(*), intent(in) :: label
        type(engine_result_t), intent(in) :: result
        integer, intent(in) :: forbidden

        if (result%ok .and. result%verdict /= forbidden) then
            print *, 'PASS ', label
        else
            print *, 'FAIL ', label
            failures = failures + 1
        end if
    end subroutine check

end program test_fortsym_zero_soundness
