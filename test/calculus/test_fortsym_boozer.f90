program test_fortsym_boozer
    ! Boozer-coordinate representation checks.
    !
    ! The analytic fixture isolates the representation contract from the
    ! equilibrium construction: B_theta and B_phi are flux functions, while
    ! raising the index introduces the metric volume factor. The contraction
    ! with the oriented volume form checks the corresponding flux two-form.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, cos, abs, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_diff, only: diff
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_metric, only: metric_t, metric_create, metric_covariant, &
        metric_det, metric_sqrtg, metric_grad, metric_valid
    use fortsym_tensor, only: tensor_t, tensor_covector, tensor_component, &
        raise, lower
    use fortsym_connection, only: covariant_divergence
    use fortsym_form, only: form_t, form_three, volume_form, interior, d, &
        form_component
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: chart
    type(metric_t) :: metric
    type(expr_t) :: coordinates(DIM), metric_values(DIM, DIM)
    type(expr_t) :: psi, theta, phi, h, i_flux, g_flux
    type(expr_t) :: b_covariant_values(DIM), b_contravariant_values(DIM)
    type(expr_t) :: metric_view(DIM, DIM), volume_density, grad_psi(DIM)
    type(expr_t) :: b_dot_grad_psi
    type(tensor_t) :: b_covariant, b_contravariant, lowered, divergence_value
    type(form_t) :: metric_volume_value, volume_value, flux_form, closed_form
    integer :: empty_indices(0)

    call arena%init()
    engine = make_symengine_engine(arena)
    psi = sym(arena, "psi")
    theta = sym(arena, "theta")
    phi = sym(arena, "phi")
    h = sym(arena, "h0")*(1 + sym(arena, "epsilon")*cos(theta))
    i_flux = sym(arena, "I0") + sym(arena, "I1")*psi
    g_flux = sym(arena, "G0") + sym(arena, "G1")*psi
    coordinates(1) = psi
    coordinates(2) = theta
    coordinates(3) = phi
    chart = chart_create(arena, coordinates, coordinates)

    metric_values = num(arena, 0)
    metric_values(1, 1) = num(arena, 1)
    metric_values(2, 2) = h**2
    metric_values(3, 3) = h**2
    metric = metric_create(metric_values, orientation=1, coordinates=coordinates)
    if (.not. metric_valid(metric)) error stop "Boozer metric is invalid"

    b_covariant_values = num(arena, 0)
    b_covariant_values(2) = i_flux
    b_covariant_values(3) = g_flux
    b_contravariant_values = num(arena, 0)
    b_contravariant_values(2) = i_flux/h**2
    b_contravariant_values(3) = g_flux/h**2

    b_covariant = tensor_covector(chart, b_covariant_values)
    b_contravariant = raise(metric, b_covariant, 1)
    lowered = lower(metric, b_contravariant, 1)
    divergence_value = covariant_divergence(metric, b_contravariant)
    metric_volume_value = volume_form(metric)
    ! On the regular analytic branch h > 0, the oriented form is h**2 times
    ! the coordinate volume basis. Keep the branch-safe metric-owned form
    ! separate from this explicit representation used for the flux identity.
    volume_value = form_three(chart, h**2)
    flux_form = interior(chart, b_contravariant_values, volume_value)
    closed_form = d(chart, flux_form)
    metric_view = metric_covariant(metric)
    volume_density = metric_sqrtg(metric)
    grad_psi = metric_grad(metric, psi)
    b_dot_grad_psi = b_contravariant_values(1)*grad_psi(1) + &
        b_contravariant_values(2)*grad_psi(2) + &
        b_contravariant_values(3)*grad_psi(3)

    call suite_begin(suite, "Boozer coordinate representation")
    call check_identity(suite, engine, "g_psi psi = 1", &
        metric_view(1, 1) - 1)
    call check_identity(suite, engine, "g_theta theta = h**2", &
        metric_view(2, 2) - h**2)
    call check_identity(suite, engine, "g_phi phi = h**2", &
        metric_view(3, 3) - h**2)
    call check_identity(suite, engine, "off-diagonal metric components vanish", &
        metric_view(1, 2) + metric_view(1, 3) + metric_view(2, 3))
    call check_identity(suite, engine, "det(g) = h**4", metric_det(metric) - h**4)
    call check_identity(suite, engine, "sqrt(abs(det(g))) is the volume density", &
        volume_density**2 - abs(metric_det(metric)))
    call check_identity(suite, engine, "metric volume form has sqrt(abs(det(g)))", &
        form_component(metric_volume_value, 7)**2 - abs(metric_det(metric)))
    call check_identity(suite, engine, "B_psi = 0", b_covariant_values(1))
    call check_identity(suite, engine, "B_theta is a flux function", &
        b_covariant_values(2) - i_flux)
    call check_identity(suite, engine, "B_phi is a flux function", &
        b_covariant_values(3) - g_flux)
    call check_identity(suite, engine, "d_theta B_theta = 0", &
        diff(b_covariant_values(2), theta))
    call check_identity(suite, engine, "d_phi B_theta = 0", &
        diff(b_covariant_values(2), phi))
    call check_identity(suite, engine, "d_theta B_phi = 0", &
        diff(b_covariant_values(3), theta))
    call check_identity(suite, engine, "d_phi B_phi = 0", &
        diff(b_covariant_values(3), phi))
    call check_identity(suite, engine, "B^theta is raised with g", &
        tensor_component(b_contravariant, [2]) - b_contravariant_values(2))
    call check_identity(suite, engine, "B^phi is raised with g", &
        tensor_component(b_contravariant, [3]) - b_contravariant_values(3))
    call check_identity(suite, engine, "lower(raise(B)) returns B", &
        tensor_component(lowered, [2]) - i_flux)
    call check_identity(suite, engine, "div(B) = 0", &
        tensor_component(divergence_value, empty_indices))
    call check_identity(suite, engine, "B dot grad(psi) = 0", b_dot_grad_psi)
    call check_identity(suite, engine, "i_B(Omega)_psi theta = G(psi)", &
        form_component(flux_form, 3) - g_flux)
    call check_identity(suite, engine, "i_B(Omega)_psi phi = -I(psi)", &
        form_component(flux_form, 5) + i_flux)
    call check_identity(suite, engine, "i_B(Omega)_theta phi = 0", &
        form_component(flux_form, 6))
    call check_identity(suite, engine, "d(i_B(Omega)) = 0", &
        form_component(closed_form, 7))

    if (suite%failed /= 0) then
        print *, "test_fortsym_boozer: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_boozer.json")
    print *, "test_fortsym_boozer: all checks passed"

end program test_fortsym_boozer
