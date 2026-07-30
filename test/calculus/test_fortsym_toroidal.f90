program test_fortsym_toroidal
    ! Independent oracles: standard toroidal-coordinate scale factors and the
    ! associated-Legendre radial equation obtained by separation of 3D Laplace.
    use, intrinsic :: iso_fortran_env, only: dp => real64, int64
    use fortsym_string, only: str
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, rat, cos, sinh, cosh, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_diff, only: diff
    use fortsym_chart, only: chart_t, metric_covariant, jacobian, laplacian
    use fortsym_toroidal, only: make_toroidal_chart, toroidal_scalar_ansatz
    use fortsym_eval, only: binding_t, eval_expr
    implicit none

    type(arena_t), target :: arena
    type(chart_t) :: chart
    type(binding_t) :: bindings
    type(expr_t) :: eta, theta, phi, scale, n, m, denominator
    type(expr_t) :: metric(3, 3), radial, field, expected, radial_residual
    logical :: defined
    integer :: i, j, nfail

    nfail = 0
    call arena%init()
    eta = sym(arena, "eta")
    theta = sym(arena, "theta")
    phi = sym(arena, "phi")
    scale = sym(arena, "scale")
    n = sym(arena, "n")
    m = sym(arena, "m")
    bindings%names = [str("eta"), str("theta"), str("phi"), &
        str("scale"), str("n"), str("m")]
    bindings%values = [1.1_dp, 0.7_dp, 0.4_dp, 1.3_dp, 2.0_dp, 1.0_dp]
    bindings%n = 6

    denominator = cosh(eta) - cos(theta)
    chart = make_toroidal_chart(arena, eta, theta, phi, scale)
    metric = metric_covariant(chart)
    expected = scale**2/denominator**2
    call check("g_eta_eta", metric(1, 1) - expected, 2.0e-13_dp)
    call check("g_theta_theta", metric(2, 2) - expected, 2.0e-13_dp)
    call check("g_phi_phi", &
        metric(3, 3) - expected*sinh(eta)**2, 2.0e-13_dp)
    do i = 1, 3
        do j = i + 1, 3
            call check("off-diagonal metric", metric(i, j), 2.0e-13_dp)
        end do
    end do
    call check("positive Jacobian", &
        jacobian(chart) - scale**3*sinh(eta)/denominator**3, 2.0e-13_dp)

    ! A polynomial radial function keeps this oracle independent of any
    ! associated-Legendre differentiation rule while exercising every
    ! coefficient in the separated operator.
    radial = eta**3 + 2*eta + 1
    field = toroidal_scalar_ansatz(radial, eta, theta, phi, n, m)
    radial_residual = diff(diff(radial, eta), eta) + &
        cosh(eta)*diff(radial, eta)/sinh(eta) - &
        (n**2 - rat(arena, 1_int64, 4_int64) + &
        m**2/sinh(eta)**2)*radial
    expected = denominator**rat(arena, 5_int64, 2_int64)* &
        cos(n*theta)*cos(m*phi)* &
        radial_residual/scale**2
    call check("Laplace separates to Legendre ODE", &
        laplacian(chart, field) - expected, 2.0e-11_dp)

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_toroidal: all checks passed"

contains

    subroutine check(label, expression, tolerance)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        real(dp), intent(in) :: tolerance
        real(dp) :: value

        value = eval_expr(expression, bindings, defined)
        if (.not. defined .or. abs(value) > tolerance) then
            nfail = nfail + 1
            print *, "FAIL ", label, " residual=", value
        end if
    end subroutine check

end program test_fortsym_toroidal
