program test_fortsym_magnetic_weak
    ! Independent symbolic checks for the two paper Fourier weak-form branches.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, i_expr, operator(+), &
        operator(-), operator(*), operator(**)
    use fortsym_diff, only: diff
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_magnetic_weak, only: fourier_constitutive, &
        fourier_constitutive_t, fourier_constitutive_valid, fourier_weak_form, &
        fourier_weak_form_t, fourier_weak_form_valid, current_compatibility, &
        fourier_longitudinal_residual, fourier_transverse_residual, &
        fourier_longitudinal_flux, fourier_transverse_flux, &
        FOURIER_LONGITUDINAL, FOURIER_TRANSVERSE, SPACE_NODAL, &
        SPACE_EDGE, TRACE_NORMAL, TRACE_TANGENTIAL
    use fortsym_magnetic, only: j_fourier
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: chart
    type(fourier_constitutive_t) :: material
    type(fourier_weak_form_t) :: longitudinal, transverse
    type(expr_t) :: u(DIM), position(DIM), nu(DIM, DIM)
    type(expr_t) :: current(DIM), residual, expected
    type(expr_t) :: full_potential(DIM), full_current(DIM), full_j(DIM)
    type(expr_t) :: scalar_potential, transverse_potential(2)
    type(expr_t) :: transverse_current(2), transverse_residual(2)
    type(expr_t) :: curl_scalar, expected_one, expected_two
    type(expr_t) :: longitudinal_flux_one, longitudinal_flux_two
    type(expr_t) :: transverse_flux
    type(expr_t) :: mode_expression
    type(engine_result_t) :: reduced
    call arena%init()
    engine = make_symengine_engine(arena)
    u(1) = sym(arena, "u1")
    u(2) = sym(arena, "u2")
    u(3) = sym(arena, "u3")
    position(1) = u(1)
    position(2) = u(2)
    position(3) = u(3)
    chart = chart_create(arena, u, position)
    call suite_begin(suite, "native Fourier weak forms")

    nu(1, 1) = num(arena, 2)
    nu(1, 2) = num(arena, 3)
    nu(1, 3) = num(arena, 0)
    nu(2, 1) = num(arena, 5)
    nu(2, 2) = num(arena, 7)
    nu(2, 3) = num(arena, 0)
    nu(3, 1) = num(arena, 0)
    nu(3, 2) = num(arena, 0)
    nu(3, 3) = sym(arena, "nu33")
    material = fourier_constitutive(chart, nu)
    if (.not. fourier_constitutive_valid(material)) then
        error stop "block constitutive owner was rejected"
    end if

    call check_identity(suite, engine, "nubar(1,1)", &
        material%nubar_t(1, 1) - 7)
    call check_identity(suite, engine, "nubar(1,2)", &
        material%nubar_t(1, 2) + 5)
    call check_identity(suite, engine, "nubar(2,1)", &
        material%nubar_t(2, 1) + 3)
    call check_identity(suite, engine, "nubar(2,2)", &
        material%nubar_t(2, 2) - 2)

    longitudinal = fourier_weak_form(chart, material, 0)
    if (.not. fourier_weak_form_valid(longitudinal)) error stop &
        "longitudinal weak form is invalid"
    if (longitudinal%branch /= FOURIER_LONGITUDINAL) error stop &
        "longitudinal branch metadata failed"
    if (longitudinal%trial_space /= SPACE_NODAL .or. &
        longitudinal%test_space /= SPACE_NODAL) error stop &
        "longitudinal space metadata failed"
    if (longitudinal%boundary_trace /= TRACE_NORMAL) error stop &
        "longitudinal trace metadata failed"
    call check_identity(suite, engine, "longitudinal diffusion(1,1)", &
        longitudinal%longitudinal_diffusion(1, 1) - nu(2, 2))
    call check_identity(suite, engine, "longitudinal diffusion(1,2)", &
        longitudinal%longitudinal_diffusion(1, 2) + nu(2, 1))
    call check_identity(suite, engine, "longitudinal diffusion(2,1)", &
        longitudinal%longitudinal_diffusion(2, 1) + nu(1, 2))
    call check_identity(suite, engine, "longitudinal diffusion(2,2)", &
        longitudinal%longitudinal_diffusion(2, 2) - nu(1, 1))

    transverse = fourier_weak_form(chart, material, 2)
    if (.not. fourier_weak_form_valid(transverse)) error stop &
        "transverse weak form is invalid"
    if (transverse%branch /= FOURIER_TRANSVERSE) error stop &
        "transverse branch metadata failed"
    if (transverse%trial_space /= SPACE_EDGE .or. &
        transverse%test_space /= SPACE_EDGE) error stop &
        "transverse space metadata failed"
    if (transverse%boundary_trace /= TRACE_TANGENTIAL) error stop &
        "transverse trace metadata failed"
    call check_identity(suite, engine, "transverse curl coefficient", &
        transverse%transverse_curl_coefficient - nu(3, 3))
    call check_identity(suite, engine, "transverse mass(1,1)", &
        transverse%transverse_mass(1, 1) - 28)
    call check_identity(suite, engine, "transverse mass(1,2)", &
        transverse%transverse_mass(1, 2) + 20)
    call check_identity(suite, engine, "transverse mass(2,1)", &
        transverse%transverse_mass(2, 1) + 12)
    call check_identity(suite, engine, "transverse mass(2,2)", &
        transverse%transverse_mass(2, 2) - 8)

    scalar_potential = u(1)**2 + u(1)*u(2)
    longitudinal_flux_one = fourier_longitudinal_flux(chart, material, &
        scalar_potential, 1)
    longitudinal_flux_two = fourier_longitudinal_flux(chart, material, &
        scalar_potential, 2)
    call check_identity(suite, engine, "longitudinal boundary flux 1", &
        longitudinal_flux_one - (7*diff(scalar_potential, u(1)) - &
        5*diff(scalar_potential, u(2))))
    call check_identity(suite, engine, "longitudinal boundary flux 2", &
        longitudinal_flux_two - (-3*diff(scalar_potential, u(1)) + &
        2*diff(scalar_potential, u(2))))
    full_potential = num(arena, 0)
    full_potential(3) = scalar_potential
    full_current = num(arena, 0)
    full_current(3) = u(1) - 2*u(2)
    full_j = j_fourier(chart, nu, full_potential, 0)
    residual = fourier_longitudinal_residual(chart, material, scalar_potential, &
        full_current(3))
    call check_identity(suite, engine, "longitudinal residual/full curl-curl", &
        residual - (full_j(3) - full_current(3)))

    transverse_potential(1) = u(1)**2 + u(2)
    transverse_potential(2) = u(1)*u(2)
    transverse_current(1) = u(1) - u(2)
    transverse_current(2) = u(1) + 2*u(2)
    transverse_flux = fourier_transverse_flux(chart, material, &
        transverse_potential)
    curl_scalar = diff(transverse_potential(2), u(1)) - &
        diff(transverse_potential(1), u(2))
    call check_identity(suite, engine, "transverse boundary flux", &
        transverse_flux - nu(3, 3)*curl_scalar)
    transverse_residual = fourier_transverse_residual(chart, material, &
        transverse_potential, transverse_current, 2)
    mode_expression = num(arena, 2)
    transverse_residual = fourier_transverse_residual(chart, material, &
        transverse_potential, transverse_current, mode_expression)
    curl_scalar = diff(transverse_potential(2), u(1)) - &
        diff(transverse_potential(1), u(2))
    expected_one = diff(nu(3, 3)*curl_scalar, u(2)) + &
        4*(7*transverse_potential(1) - 5*transverse_potential(2)) - &
        transverse_current(1)
    expected_two = -diff(nu(3, 3)*curl_scalar, u(1)) + &
        4*(-3*transverse_potential(1) + 2*transverse_potential(2)) - &
        transverse_current(2)
    call check_identity(suite, engine, "transverse residual component 1", &
        transverse_residual(1) - expected_one)
    call check_identity(suite, engine, "transverse residual component 2", &
        transverse_residual(2) - expected_two)
    full_potential(1) = transverse_potential(1)
    full_potential(2) = transverse_potential(2)
    full_potential(3) = num(arena, 0)
    full_current(1) = transverse_current(1)
    full_current(2) = transverse_current(2)
    full_current(3) = num(arena, 0)
    full_j = j_fourier(chart, nu, full_potential, 2)
    call check_identity(suite, engine, "transverse residual/full curl-curl 1", &
        transverse_residual(1) - (full_j(1) - full_current(1)))
    call check_identity(suite, engine, "transverse residual/full curl-curl 2", &
        transverse_residual(2) - (full_j(2) - full_current(2)))

    current(1) = u(1)**2
    current(2) = u(2)**2
    current(3) = u(3)
    residual = current_compatibility(chart, current, 3)
    expected = 2*u(1) + 2*u(2) + 3*i_expr(arena)*u(3)
    reduced = engine%simplify(residual - expected)
    if (reduced%ok) then
        call check_identity(suite, engine, "Fourier current compatibility", &
            reduced%value)
    else
        call check_identity(suite, engine, "Fourier current compatibility", &
            residual - expected)
    end if

    ! A nonzero cross-block entry must not be silently dropped.
    nu(1, 3) = num(arena, 1)
    material = fourier_constitutive(chart, nu)
    if (fourier_constitutive_valid(material)) error stop &
        "non-block constitutive owner was accepted"

    if (suite%failed /= 0) then
        print *, "test_fortsym_magnetic_weak: ", suite%failed, &
            " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_magnetic_weak.json")
    print *, "test_fortsym_magnetic_weak: all checks passed"
end program test_fortsym_magnetic_weak
