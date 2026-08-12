program test_fortsym_magnetic_weak
    ! Independent symbolic checks for the two paper Fourier weak-form branches.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, i_expr, operator(+), &
        operator(-), operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_magnetic_weak, only: fourier_constitutive, &
        fourier_constitutive_t, fourier_constitutive_valid, fourier_weak_form, &
        fourier_weak_form_t, fourier_weak_form_valid, current_compatibility, &
        FOURIER_LONGITUDINAL, FOURIER_TRANSVERSE, SPACE_NODAL, &
        SPACE_EDGE, TRACE_NORMAL, TRACE_TANGENTIAL
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: chart
    type(fourier_constitutive_t) :: material
    type(fourier_weak_form_t) :: longitudinal, transverse
    type(expr_t) :: u(DIM), position(DIM), nu(DIM, DIM)
    type(expr_t) :: current(DIM), residual, expected
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
    call check_identity(suite, engine, "longitudinal coefficient", &
        longitudinal%scalar_coefficient - nu(3, 3))

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
