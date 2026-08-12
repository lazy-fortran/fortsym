program example_boozer_coordinates
    ! An analytic Boozer-coordinate fixture. It is deliberately coordinate
    ! owned: it demonstrates the representation and its identities without
    ! pretending to solve the magnetic-equilibrium construction problem.
    !
    ! With q=(psi,theta,phi), choose
    !   g_ij = diag(1, h^2, h^2),  sqrt(g)=h^2,
    !   B_i = (0, I(psi), G(psi)).
    ! Then B^i=(0, I/h^2, G/h^2), so the angular covariant components are
    ! flux functions while the contravariant components carry the Jacobian.
    use fortsym
    use fortsym_string, only: chars
    use fortsym_print, only: print_expr
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: boozer_chart
    type(metric_t) :: metric_owner
    type(expr_t) :: coordinates(DIM), position(DIM)
    type(expr_t) :: psi, theta, phi, h0, epsilon, h
    type(expr_t) :: i_flux, g_flux
    type(expr_t) :: metric_components(DIM, DIM), b_covariant_values(DIM)
    type(expr_t) :: b_contravariant_values(DIM), volume_density
    type(expr_t) :: boozer_residual(BOOZER_RESIDUAL_COUNT)
    type(expr_t) :: grad_psi(DIM), b_dot_grad_psi
    type(flux_coordinate_t) :: flux_owner
    integer :: indices_empty(0)
    type(tensor_t) :: b_covariant, b_contravariant, divergence_value
    type(form_t) :: metric_volume_value, volume_value, flux_form, closed_form
    type(engine_result_t) :: checked
    integer :: indices(1)

    call reset()
    arena => default_arena()
    psi = "psi"
    theta = "theta"
    phi = "phi"
    h0 = "h0"
    epsilon = "epsilon"
    h = h0*(1 + epsilon*cos(theta))
    i_flux = sym(arena, "I0") + sym(arena, "I1")*psi
    g_flux = sym(arena, "G0") + sym(arena, "G1")*psi

    coordinates(1) = psi
    coordinates(2) = theta
    coordinates(3) = phi
    ! The coordinate-aware metric owner needs coordinates; position is only
    ! a harmless identity chart here, not a Cartesian equilibrium embedding.
    position = coordinates
    boozer_chart = chart_create(arena, coordinates, position)
    flux_owner = flux_coordinates(boozer_chart, 1, FLUX_BOOZER)
    if (.not. flux_coordinate_valid(flux_owner)) then
        error stop "Boozer flux-coordinate owner invalid"
    end if

    metric_components = num(arena, 0)
    metric_components(1, 1) = num(arena, 1)
    metric_components(2, 2) = h**2
    metric_components(3, 3) = h**2
    metric_owner = metric_create(metric_components, orientation=1, &
        coordinates=coordinates)
    if (.not. metric_valid(metric_owner)) error stop "Boozer metric invalid"

    b_covariant_values = num(arena, 0)
    b_covariant_values(2) = i_flux
    b_covariant_values(3) = g_flux
    b_contravariant_values(1) = num(arena, 0)
    b_contravariant_values(2) = i_flux/h**2
    b_contravariant_values(3) = g_flux/h**2
    b_covariant = tensor_covector(boozer_chart, b_covariant_values)
    b_contravariant = raise(metric_owner, b_covariant, 1)
    divergence_value = covariant_divergence(metric_owner, b_contravariant)
    metric_volume_value = volume_form(metric_owner)
    ! The analytic fixture is on the positive regular branch h > 0, so use
    ! that branch explicitly for the displayed oriented volume form. The
    ! metric-owned volume remains the branch-safe sqrt(abs(det(g))) result.
    volume_value = form_three(boozer_chart, h**2)
    flux_form = interior(boozer_chart, b_contravariant_values, volume_value)
    closed_form = d(boozer_chart, flux_form)
    volume_density = metric_sqrtg(metric_owner)
    grad_psi = metric_grad(metric_owner, psi)
    b_dot_grad_psi = b_contravariant_values(1)*grad_psi(1) + &
        b_contravariant_values(2)*grad_psi(2) + &
        b_contravariant_values(3)*grad_psi(3)

    call assert_zero(metric_det(metric_owner) - h**4, &
        "Boozer metric determinant")
    call assert_zero(volume_density**2 - abs(metric_det(metric_owner)), &
        "Boozer positive metric volume density")
    call assert_zero(form_component(metric_volume_value, 7)**2 - &
        abs(metric_det(metric_owner)), "Boozer oriented volume form")
    boozer_residual = boozer_residuals(flux_owner, b_covariant_values)
    call assert_zero(boozer_residual(1), "B_psi = 0")
    call assert_zero(boozer_residual(2), "B_theta is independent of theta")
    call assert_zero(boozer_residual(3), "B_theta is independent of phi")
    call assert_zero(boozer_residual(4), "B_phi is independent of theta")
    call assert_zero(boozer_residual(5), "B_phi is independent of phi")

    indices(1) = 1
    call assert_zero(tensor_component(b_contravariant, indices), &
        "B^psi = 0")
    indices(1) = 2
    call assert_zero(tensor_component(b_contravariant, indices) - i_flux/h**2, &
        "raised B^theta")
    indices(1) = 3
    call assert_zero(tensor_component(b_contravariant, indices) - g_flux/h**2, &
        "raised B^phi")
    call assert_zero(tensor_component(divergence_value, indices_empty), &
        "div B = 0")
    call assert_zero(b_dot_grad_psi, "B dot grad(psi) = 0")
    call assert_zero(form_component(flux_form, 3) - g_flux, &
        "i_B(Omega) psi-theta component")
    call assert_zero(form_component(flux_form, 5) + i_flux, &
        "i_B(Omega) psi-phi component")
    call assert_zero(form_component(flux_form, 6), &
        "i_B(Omega) theta-phi component")
    call assert_zero(form_component(closed_form, 7), "d(i_B(Omega)) = 0")

    print '(a)', "Boozer coordinates (analytic representation)"
    print '(a)', "  B_psi = 0"
    print '(a,a,a,a)', "  B_theta = ", chars(print_expr(i_flux)), &
        ", B_phi = ", chars(print_expr(g_flux))
    print '(a)', "  B_theta and B_phi depend only on psi (flux functions)"
    print '(a,a)', "  g_ij = diag(1, h^2, h^2), h = ", chars(print_expr(h))
    print '(a)', "  sqrt(g) = h^2; Omega = h^2 dpsi wedge dtheta wedge dphi"
    print '(a)', "  B^theta = B_theta/h^2; B^phi = B_phi/h^2"
    print '(a)', "  i_B(Omega) = B_phi dpsi wedge dtheta - B_theta dpsi wedge dphi"
    print '(a)', "  B dot grad(psi) = 0; div(B) = 0; d(i_B(Omega)) = 0"

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

end program example_boozer_coordinates
