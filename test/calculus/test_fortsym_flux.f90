program test_fortsym_flux
    ! Native flux-coordinate residuals checked by an independent SymEngine
    ! identity oracle. The descriptor itself is metadata; these tests verify
    ! the residual formulas and their label/angle ordering.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, is_valid, operator(+), &
        operator(-), operator(*)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_flux, only: flux_coordinate_t, flux_coordinates, &
        flux_coordinate_valid, flux_coordinate_label, flux_coordinate_kind, &
        flux_coordinate_angles, flux_normal_residual, &
        straight_field_line_residual, clebsch_residuals, boozer_residuals, &
        FLUX_GENERIC, hamada_residuals, FLUX_CLEBSCH, FLUX_BOOZER, &
        FLUX_HAMADA, CLEBSCH_RESIDUAL_COUNT, HAMADA_RESIDUAL_COUNT
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: chart
    type(flux_coordinate_t) :: boozer, clebsch, hamada, generic, relabelled
    type(expr_t) :: coordinates(DIM), b_up(DIM), b_cov(DIM)
    type(expr_t) :: psi, theta, phi, iota, i_flux, g_flux
    type(expr_t) :: i_relabel, g_relabel
    type(expr_t) :: residuals(5), residual, expected
    type(expr_t) :: clebsch_values(CLEBSCH_RESIDUAL_COUNT)
    type(expr_t) :: alpha, beta
    integer :: angles(2)

    call arena%init()
    engine = make_symengine_engine(arena)
    psi = sym(arena, "psi_flux")
    theta = sym(arena, "theta_flux")
    phi = sym(arena, "phi_flux")
    iota = sym(arena, "iota_flux")
    i_flux = sym(arena, "I_flux") + sym(arena, "I_prime")*psi
    g_flux = sym(arena, "G_flux") + sym(arena, "G_prime")*psi
    i_relabel = sym(arena, "I_relabel") + sym(arena, "I_relabel_prime")*theta
    g_relabel = sym(arena, "G_relabel") + sym(arena, "G_relabel_prime")*theta
    coordinates(1) = psi
    coordinates(2) = theta
    coordinates(3) = phi
    chart = chart_create(arena, coordinates, coordinates)
    clebsch = flux_coordinates(chart, 1, FLUX_CLEBSCH)
    boozer = flux_coordinates(chart, 1, FLUX_BOOZER)
    hamada = flux_coordinates(chart, 1, FLUX_HAMADA)
    generic = flux_coordinates(chart, 1, FLUX_GENERIC)
    relabelled = flux_coordinates(chart, 2, FLUX_BOOZER)

    b_cov = num(arena, 0)
    b_cov(2) = i_flux
    b_cov(3) = g_flux
    b_up = num(arena, 0)
    b_up(2) = iota
    b_up(3) = num(arena, 1)

    call suite_begin(suite, "native flux-coordinate residuals")
    if (.not. flux_coordinate_valid(boozer)) then
        error stop "Boozer flux-coordinate descriptor is invalid"
    end if
    if (flux_coordinate_kind(boozer) /= FLUX_BOOZER) then
        error stop "flux-coordinate kind metadata failed"
    end if
    angles = flux_coordinate_angles(boozer)
    if (any(angles /= [2, 3])) error stop "flux-coordinate angle ordering failed"
    call check_identity(suite, engine, "flux label retained by descriptor", &
        flux_coordinate_label(boozer) - psi)

    residuals = boozer_residuals(boozer, b_cov)
    call check_identity(suite, engine, "B_psi residual", residuals(1))
    call check_identity(suite, engine, "d_theta B_theta residual", residuals(2))
    call check_identity(suite, engine, "d_phi B_theta residual", residuals(3))
    call check_identity(suite, engine, "d_theta B_phi residual", residuals(4))
    call check_identity(suite, engine, "d_phi B_phi residual", residuals(5))

    b_cov(2) = i_flux + theta
    residuals = boozer_residuals(boozer, b_cov)
    call check_identity(suite, engine, "non-flux B_theta is detected", &
        residuals(2) - 1)
    call check_identity(suite, engine, "non-flux B_theta phi derivative", &
        residuals(3))

    b_up(2) = i_flux
    b_up(3) = g_flux
    residuals = hamada_residuals(hamada, b_up)
    if (size(residuals) /= HAMADA_RESIDUAL_COUNT) then
        error stop "Hamada residual count changed unexpectedly"
    end if
    call check_identity(suite, engine, "Hamada B_psi residual", residuals(1))
    call check_identity(suite, engine, "d_theta B^theta residual", residuals(2))
    call check_identity(suite, engine, "d_phi B^theta residual", residuals(3))
    call check_identity(suite, engine, "d_theta B^phi residual", residuals(4))
    call check_identity(suite, engine, "d_phi B^phi residual", residuals(5))

    b_up(2) = i_flux + theta
    residuals = hamada_residuals(hamada, b_up)
    call check_identity(suite, engine, "non-flux B^theta is detected", &
        residuals(2) - 1)
    residuals = hamada_residuals(boozer, b_up)
    if (is_valid(residuals(1))) error stop "Boozer descriptor accepted Hamada residuals"

    b_up(2) = iota
    b_up(3) = num(arena, 1)
    alpha = psi
    beta = theta
    clebsch_values = clebsch_residuals(clebsch, b_up, alpha, beta)
    if (size(clebsch_values) /= CLEBSCH_RESIDUAL_COUNT) then
        error stop "Clebsch residual count changed unexpectedly"
    end if
    call check_identity(suite, engine, "Clebsch residual 1", &
        clebsch_values(1))
    call check_identity(suite, engine, "Clebsch residual 2", &
        clebsch_values(2) - iota)
    call check_identity(suite, engine, "Clebsch residual 3", &
        clebsch_values(3))
    b_up(3) = num(arena, 1) + theta
    clebsch_values = clebsch_residuals(clebsch, b_up, alpha, beta)
    call check_identity(suite, engine, "non-Clebsch field is detected", &
        clebsch_values(3) - theta)
    b_up(3) = num(arena, 1)
    residual = flux_normal_residual(boozer, b_up)
    call check_identity(suite, engine, "normal field residual", residual)
    residual = straight_field_line_residual(boozer, b_up, iota)
    call check_identity(suite, engine, "straight-field-line residual", residual)
    b_up(2) = iota + theta
    residual = straight_field_line_residual(boozer, b_up, iota)
    expected = theta
    call check_identity(suite, engine, "bad straight-field-line residual", &
        residual - expected)

    residuals = boozer_residuals(generic, b_cov)
    if (is_valid(residuals(1))) error stop "generic descriptor accepted Boozer residuals"

    angles = flux_coordinate_angles(relabelled)
    if (any(angles /= [1, 3])) error stop "relabelled angle ordering failed"
    b_cov = num(arena, 0)
    b_cov(1) = i_relabel
    b_cov(3) = g_relabel
    residuals = boozer_residuals(relabelled, b_cov)
    call check_identity(suite, engine, "relabelled B_psi residual", residuals(1))
    call check_identity(suite, engine, "relabelled B_theta residual", residuals(2))
    call check_identity(suite, engine, "relabelled B_phi residual", residuals(4))

    if (suite%failed /= 0) then
        print *, "test_fortsym_flux: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_flux.json")
    print *, "test_fortsym_flux: all checks passed"

end program test_fortsym_flux
