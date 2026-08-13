program test_fortsym_spacetime_form
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_valid, spacetime_metric_sqrtg
    use fortsym_spacetime_form, only: spacetime_form_t, spacetime_form_one, &
        spacetime_form_two, spacetime_form_component, spacetime_d, &
        spacetime_wedge, spacetime_hodge, spacetime_form_four, &
        spacetime_form_scalar, spacetime_codifferential, spacetime_interior, &
        spacetime_lie, spacetime_laplace_de_rham, spacetime_volume_form
    use fortsym_maxwell, only: maxwell_field_strength, maxwell_gauge_transform, &
        maxwell_residual
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(spacetime_metric_t) :: metric, metric_2d
    type(expr_t) :: u(SPACETIME_DIM), components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: potential_components(SPACETIME_DIM), vector(SPACETIME_DIM)
    type(expr_t) :: two_components(6), residual
    type(spacetime_form_t) :: potential, field, closed, hodge, hodge_hodge, codiff
    type(spacetime_form_t) :: contraction, lie_field, cartan
    type(spacetime_form_t) :: volume, negative_volume, volume_2d, negative_volume_2d
    type(spacetime_form_t) :: scalar_form, laplace, gauge, gauge_field, current
    type(spacetime_form_t) :: maxwell_error
    type(spacetime_form_t) :: alpha_2d, beta_2d, alpha_star_2d
    type(spacetime_form_t) :: alpha_star_star_2d, scalar_2d, scalar_star_2d
    type(spacetime_form_t) :: laplace_2d
    type(expr_t) :: values_2d(SPACETIME_DIM), components_2d(SPACETIME_DIM, &
        SPACETIME_DIM)
    integer :: signature(SPACETIME_DIM), mask

    call arena%init()
    engine = make_symengine_engine(arena)
    u(1) = sym(arena, "t")
    u(2) = sym(arena, "x")
    u(3) = sym(arena, "y")
    u(4) = sym(arena, "z")
    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = num(arena, 1)
    components(3, 3) = num(arena, 1)
    components(4, 4) = num(arena, 1)
    signature = [-1, 1, 1, 1]
    metric = spacetime_metric_create(components, 4, u, signature, 1)
    call suite_begin(suite, "spacetime differential forms")

    volume = spacetime_volume_form(metric)
    negative_volume = spacetime_volume_form(metric, -1)
    call check_identity(suite, engine, "oriented spacetime volume", &
        spacetime_form_component(volume, 15) - 1)
    call check_identity(suite, engine, "reversed spacetime volume", &
        spacetime_form_component(negative_volume, 15) + 1)

    potential_components(1) = u(2)*u(3)
    potential_components(2) = u(1)*u(3)
    potential_components(3) = u(1)*u(2)
    potential_components(4) = u(1) + u(2)
    potential = spacetime_form_one(metric, potential_components)
    field = spacetime_d(metric, potential)
    closed = spacetime_d(metric, field)
    do mask = 0, 15
        if (mask /= 15) cycle
        call check_identity(suite, engine, "d(dA)=0", &
            spacetime_form_component(closed, mask))
    end do

    vector = num(arena, 0)
    vector(1) = num(arena, 1)
    two_components = num(arena, 0)
    two_components(1) = u(4) - u(3)
    two_components(3) = u(1)
    field = spacetime_form_two(metric, two_components)
    contraction = spacetime_interior(vector, field)
    lie_field = spacetime_lie(metric, vector, field)
    cartan = spacetime_d(metric, contraction)
    call check_identity(suite, engine, "interior contraction", &
        spacetime_form_component(contraction, 2) - (u(4) - u(3)))
    call check_identity(suite, engine, "Cartan Lie derivative", &
        spacetime_form_component(lie_field, 6) - 1)
    call check_identity(suite, engine, "Cartan identity", &
        spacetime_form_component(lie_field, 6) - &
        spacetime_form_component(cartan, 6))

    potential = spacetime_form_one(metric, u)
    codiff = spacetime_codifferential(metric, potential)
    call check_identity(suite, engine, "codifferential of radial one-form", &
        spacetime_form_component(codiff, 0) + 2)

    scalar_form = spacetime_form_scalar(u(1)**2 + u(2)**2 + u(3)**2 + u(4)**2)
    laplace = spacetime_laplace_de_rham(metric, scalar_form)
    call check_identity(suite, engine, "Lorentzian scalar Laplace-de Rham", &
        spacetime_form_component(laplace, 0) + 4)

    potential = spacetime_form_one(metric, potential_components)
    field = maxwell_field_strength(metric, potential)
    gauge = maxwell_gauge_transform(metric, potential, u(1)*u(2))
    gauge_field = maxwell_field_strength(metric, gauge)
    do mask = 0, 15
        if (popcnt(mask) /= 2) cycle
        call check_identity(suite, engine, "gauge transformation leaves F", &
            spacetime_form_component(gauge_field, mask) - &
            spacetime_form_component(field, mask))
    end do
    current = spacetime_d(metric, spacetime_hodge(metric, field))
    maxwell_error = maxwell_residual(metric, potential, current)
    do mask = 0, 15
        if (popcnt(mask) /= 3) cycle
        call check_identity(suite, engine, "Maxwell source residual", &
            spacetime_form_component(maxwell_error, mask))
    end do

    two_components(1) = num(arena, 1)
    two_components(2) = num(arena, 2)
    two_components(3) = num(arena, 3)
    two_components(4) = num(arena, 4)
    two_components(5) = num(arena, 5)
    two_components(6) = num(arena, 6)
    field = spacetime_form_two(metric, two_components)
    hodge = spacetime_hodge(metric, field)
    hodge_hodge = spacetime_hodge(metric, hodge)
    do mask = 0, 15
        if (popcnt(mask) /= 2) cycle
        residual = spacetime_form_component(hodge_hodge, mask) + &
            spacetime_form_component(field, mask)
        call check_identity(suite, engine, "Lorentzian star squared on two-form", &
            residual)
    end do

    field = spacetime_wedge(potential, potential)
    do mask = 0, 15
        if (popcnt(mask) /= 2) cycle
        call check_identity(suite, engine, "one-form wedge itself", &
            spacetime_form_component(field, mask))
    end do

    field = spacetime_form_four(metric, num(arena, 1))
    hodge = spacetime_hodge(metric, field)
    call check_identity(suite, engine, "star volume is signed scalar", &
        spacetime_form_component(hodge, 0) + 1)
    vector = num(arena, 0)
    vector(1) = num(arena, 1)
    contraction = spacetime_interior(vector, volume)
    call check_identity(suite, engine, "interior of spacetime volume", &
        spacetime_form_component(contraction, 14) - 1)

    ! The same owner must honor a lower runtime dimension.  This is a flat
    ! two-dimensional Riemannian metric, so the Hodge degree and
    ! Laplace--de Rham sign are independently visible.
    components_2d = num(arena, 0)
    components_2d(1, 1) = num(arena, 1)
    components_2d(2, 2) = num(arena, 1)
    signature = 1
    metric_2d = spacetime_metric_create(components_2d, 2, u, signature, 1)
    if (.not. spacetime_metric_valid(metric_2d)) error stop &
        "invalid two-dimensional form metric"

    volume_2d = spacetime_volume_form(metric_2d)
    negative_volume_2d = spacetime_volume_form(metric_2d, -1)
    call check_identity(suite, engine, "2D spacetime volume", &
        spacetime_form_component(volume_2d, 3) - 1)
    call check_identity(suite, engine, "reversed 2D spacetime volume", &
        spacetime_form_component(negative_volume_2d, 3) + 1)

    values_2d = num(arena, 0)
    values_2d(1) = u(2)
    values_2d(2) = u(1)**2
    alpha_2d = spacetime_form_one(metric_2d, values_2d)
    beta_2d = spacetime_d(metric_2d, alpha_2d)
    call check_identity(suite, engine, "2D exterior derivative", &
        spacetime_form_component(beta_2d, 3) - (2*u(1) - 1))
    call check_identity(suite, engine, "2D d(dA)=0", &
        spacetime_form_component(spacetime_d(metric_2d, beta_2d), 0))

    alpha_star_2d = spacetime_hodge(metric_2d, alpha_2d)
    alpha_star_star_2d = spacetime_hodge(metric_2d, alpha_star_2d)
    do mask = 1, 2
        call check_identity(suite, engine, "2D Hodge involution on one-form", &
            spacetime_form_component(alpha_star_star_2d, 2**(mask - 1)) + &
            spacetime_form_component(alpha_2d, 2**(mask - 1)))
    end do

    scalar_2d = spacetime_form_scalar(metric_2d, u(1)**2)
    scalar_star_2d = spacetime_hodge(metric_2d, scalar_2d)
    call check_identity(suite, engine, "2D Hodge scalar volume factor", &
        spacetime_form_component(scalar_star_2d, 3) - &
        spacetime_metric_sqrtg(metric_2d)*u(1)**2)
    laplace_2d = spacetime_laplace_de_rham(metric_2d, scalar_2d)
    call check_identity(suite, engine, "2D Laplace-de Rham", &
        spacetime_form_component(laplace_2d, 0) + 2)

    if (suite%failed /= 0) then
        print *, "test_fortsym_spacetime_form: ", suite%failed, &
            " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_spacetime_form.json")
    print *, "test_fortsym_spacetime_form: all checks passed"
end program test_fortsym_spacetime_form
