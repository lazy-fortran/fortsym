program example_magnetic_flux_coordinates
    ! Nested toroidal flux surfaces labelled by psi. The potential A=psi dphi
    ! gives a field tangent to every psi=constant surface and demonstrates the
    ! native B^i, flux-form, volume, and divergence owners together.
    use fortsym
    use fortsym_metric, only: metric_t, metric_create, metric_valid
    use fortsym_string, only: chars
    use fortsym_print, only: print_expr
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: flux_chart
    type(metric_t) :: flux_metric
    type(magnetic_chart_t) :: magnetic_owner
    type(flux_coordinate_t) :: clebsch
    type(expr_t) :: coordinates(DIM), position(DIM), potential(DIM)
    type(expr_t) :: psi, theta, phi, major_radius, minor_radius
    type(expr_t) :: radial, b(DIM), b_lower(DIM), flux_density(DIM)
    type(expr_t) :: volume_density, expected
    type(expr_t) :: clebsch_values(CLEBSCH_RESIDUAL_COUNT)
    type(expr_t) :: metric_components(DIM, DIM)
    type(expr_t) :: reluctivity(DIM, DIM), h_lower(DIM), h_upper(DIM)
    type(form_t) :: potential_form, flux_form, closed_form
    type(engine_result_t) :: checked
    integer :: mask

    call reset()
    arena => default_arena()
    psi = "psi"
    theta = "theta"
    phi = "phi"
    major_radius = "R0"
    minor_radius = "a"
    radial = sqrt(psi)
    coordinates(1) = psi
    coordinates(2) = theta
    coordinates(3) = phi
    position(1) = (major_radius + minor_radius*radial*cos(theta))*cos(phi)
    position(2) = (major_radius + minor_radius*radial*cos(theta))*sin(phi)
    position(3) = minor_radius*radial*sin(theta)
    flux_chart = chart_create(arena, coordinates, position)

    potential = num(arena, 0)
    potential(3) = psi
    magnetic_owner = magnetic_chart(flux_chart, potential, 1)
    if (.not. magnetic_chart_valid(magnetic_owner)) then
        error stop "magnetic chart owner invalid"
    end if
    b = b_con(flux_chart, potential)
    flux_density = curl_density(flux_chart, potential)
    clebsch = flux_coordinates(flux_chart, 1, FLUX_CLEBSCH)
    clebsch_values = clebsch_residuals(clebsch, b, psi, phi)
    b_lower = b_cov(flux_chart, b)
    ! Keep the expensive constitutive raise on the compact analytic metric
    ! owner. The chart-derived metric remains tested above, while the owner
    ! avoids carrying the raw Cartesian chain rule through g^ij g_jk B^k.
    metric_components = num(arena, 0)
    metric_components(1, 1) = num(arena, 1)
    metric_components(2, 2) = num(arena, 1)
    metric_components(2, 3) = minor_radius*cos(phi)
    metric_components(3, 2) = metric_components(2, 3)
    metric_components(3, 3) = 1 + minor_radius**2*cos(phi)**2
    flux_metric = metric_create(metric_components, coordinates=coordinates)
    if (.not. metric_valid(flux_metric)) error stop "flux metric invalid"
    reluctivity = metric_components
    h_lower = h_cov(flux_chart, reluctivity, b)
    h_upper = h_con(flux_metric, h_lower)
    volume_density = jacobian(flux_chart)
    expected = -minor_radius**2*(major_radius + minor_radius*radial*cos(theta))/2
    call assert_zero(volume_density - expected, "toroidal flux-coordinate Jacobian")
    call assert_zero(b(1), "B^psi = 0: field stays on flux surfaces")
    call assert_zero(b(2)*volume_density + 1, &
        "A=psi dphi gives B^theta=-1/J")
    call assert_zero(div_density(flux_chart, flux_density), &
        "d(i_B volume) = 0")
    call assert_zero(field_line_derivative(flux_chart, b, psi), &
        "B dot grad(psi) = 0")
    call assert_zero(clebsch_values(1), "Clebsch residual component 1")
    call assert_zero(clebsch_values(2), "Clebsch residual component 2")
    call assert_zero(clebsch_values(3), "Clebsch residual component 3")
    call assert_zero(h_lower(1) - b(1), "H_i = g_ij B^j, component 1")
    call assert_zero(h_lower(2) - (b(2) + minor_radius*cos(phi)*b(3)), &
        "H_i = g_ij B^j, component 2")
    call assert_zero(h_lower(3) - (minor_radius*cos(phi)*b(2) + &
        (1 + minor_radius**2*cos(phi)**2)*b(3)), &
        "H_i = g_ij B^j, component 3")
    call assert_zero(h_upper(1) - b(1), "H^i raises back to B^i, component 1")
    call assert_zero(h_upper(2) - b(2), "H^i raises back to B^i, component 2")
    call assert_zero(h_upper(3) - b(3), "H^i raises back to B^i, component 3")

    potential_form = magnetic_chart_potential_form(magnetic_owner)
    flux_form = magnetic_chart_flux_form(magnetic_owner)
    closed_form = d(flux_chart, flux_form)
    do mask = 0, 2**DIM - 1
        if (mask == 3 .or. mask == 5 .or. mask == 6 .or. mask == 7) then
            call assert_zero(form_component(closed_form, mask), &
                "d flux form = 0")
        end if
    end do

    print '(a)', "magnetic flux coordinates"
    print '(a)', "  surfaces: psi = constant, B^psi = 0"
    print '(a,a)', "  signed J = ", chars(print_expr(expected))
    print '(a)', "  A = psi dphi,  i_B(volume) = dA,  d(dA) = 0"
    print '(a)', "  nu_ij = g_ij: H_i = B_i and H^i = B^i"

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

end program example_magnetic_flux_coordinates
