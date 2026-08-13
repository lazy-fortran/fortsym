program example_nonorthogonal_flux_coordinates
    ! A periodic non-orthogonal flux chart.  The position map is deliberately
    ! simple so the component identities remain visible:
    !
    !   x = psi,  y = theta + kappa sin(phi),  z = phi.
    !
    ! The flux surfaces are psi=constant, while g_theta_phi is nonzero.  A =
    ! psi dphi gives B^i=(0,-1,0), so the example exercises B^i, B_i,
    ! sqrt(g) B^i, Clebsch coordinates, and the magnetic two-form together.
    use fortsym
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: chart
    type(flux_coordinate_t) :: clebsch
    type(magnetic_chart_t) :: magnetic_owner
    type(expr_t) :: coordinates(DIM), position(DIM), potential(DIM)
    type(expr_t) :: psi, theta, phi, kappa
    type(expr_t) :: metric(DIM, DIM), b(DIM), b_lower(DIM), b_density_values(DIM)
    type(expr_t) :: recovered(DIM), clebsch_values(CLEBSCH_RESIDUAL_COUNT)
    type(tensor_t) :: b_upper, b_covariant, b_roundtrip, b_density_tensor
    type(form_t) :: volume_value, flux_form, flux_from_b, closed_form
    type(engine_result_t) :: checked

    call reset()
    arena => default_arena()
    psi = "psi"
    theta = "theta"
    phi = "phi"
    kappa = "kappa"
    coordinates = coords(psi, theta, phi)
    position(1) = psi
    position(2) = theta + kappa*sin(phi)
    position(3) = phi
    chart = make_chart(coordinates, position)

    metric = metric_covariant(chart)
    call assert_zero(metric(1, 1) - 1, "g_psi_psi = 1")
    call assert_zero(metric(2, 2) - 1, "g_theta_theta = 1")
    call assert_zero(metric(2, 3) - kappa*cos(phi), &
        "g_theta_phi is off-diagonal")
    call assert_zero(metric(3, 2) - kappa*cos(phi), &
        "g_phi_theta is off-diagonal")
    call assert_zero(metric(3, 3) - (1 + kappa**2*cos(phi)**2), &
        "g_phi_phi")
    call assert_zero(jacobian(chart) - 1, "signed flux-chart Jacobian")
    call assert_zero(sqrtg(chart) - 1, "positive flux-chart volume density")

    potential = num(arena, 0)
    potential(3) = psi
    b = b_con(chart, potential)
    b_lower = b_cov(chart, b)
    b_density_values = b_density(chart, b)
    call assert_zero(b(1), "B^psi = 0")
    call assert_zero(b(2) + 1, "B^theta = -1")
    call assert_zero(b(3), "B^phi = 0")
    call assert_zero(b_lower(1), "B_psi = 0")
    call assert_zero(b_lower(2) + 1, "B_theta = -1")
    call assert_zero(b_lower(3) + kappa*cos(phi), &
        "off-diagonal lowering contributes B_phi")
    call assert_zero(b_density_values(1) - b(1), "B density component 1")
    call assert_zero(b_density_values(2) - b(2), "B density component 2")
    call assert_zero(b_density_values(3) - b(3), "B density component 3")

    b_upper = tensor_vector(chart, b)
    b_covariant = lower(chart, b_upper, 1)
    b_roundtrip = raise(chart, b_covariant, 1)
    call assert_zero(tensor_component(b_roundtrip, [1]) - b(1), &
        "raise(lower(B)) component 1")
    call assert_zero(tensor_component(b_roundtrip, [2]) - b(2), &
        "raise(lower(B)) component 2")
    call assert_zero(tensor_component(b_roundtrip, [3]) - b(3), &
        "raise(lower(B)) component 3")
    b_density_tensor = density(b_upper, 1)
    if (tensor_density_weight(b_density_tensor) /= 1) then
        error stop "magnetic density weight is not one"
    end if

    magnetic_owner = magnetic_chart(chart, potential, 1)
    flux_form = magnetic_chart_flux_form(magnetic_owner)
    volume_value = volume_form(chart)
    flux_from_b = b_flux_form(chart, b)
    closed_form = d(chart, flux_form)
    call assert_zero(form_component(volume_value, 7) - 1, &
        "oriented volume form")
    call assert_zero(form_component(flux_form, 5) - 1, &
        "beta = d(psi dphi)")
    call assert_zero(form_component(flux_from_b, 5) - &
        form_component(flux_form, 5), "beta = i_B(Omega)")
    call assert_zero(form_component(closed_form, 7), "d(beta) = 0")
    recovered = b_con_form(chart, flux_form)
    call assert_zero(recovered(1) - b(1), "form/vector round trip 1")
    call assert_zero(recovered(2) - b(2), "form/vector round trip 2")
    call assert_zero(recovered(3) - b(3), "form/vector round trip 3")

    clebsch = flux_coordinates(chart, 1, FLUX_CLEBSCH)
    clebsch_values = clebsch_residuals(clebsch, b, psi, phi)
    call assert_zero(clebsch_values(1), "Clebsch residual 1")
    call assert_zero(clebsch_values(2), "Clebsch residual 2")
    call assert_zero(clebsch_values(3), "Clebsch residual 3")
    call assert_zero(div_density(chart, b_density_values), &
        "density divergence of B")
    call assert_zero(field_line_derivative(chart, b, psi), &
        "B dot grad(psi) = 0")

    print '(a)', "non-orthogonal flux coordinates"
    print '(a)', "  x = (psi, theta + kappa sin(phi), phi)"
    print '(a)', "  g_theta_phi = kappa cos(phi), J = sqrt(g) = 1"
    print '(a)', "  B^i = (0, -1, 0), beta = i_B(Omega) = d(psi dphi)"

contains

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = zero_test(expression)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            error stop 1
        end if
    end subroutine assert_zero

end program example_nonorthogonal_flux_coordinates
