program test_fortsym_chart_map
    ! Coordinate-transition identities are the independent oracle here. The
    ! map is a non-unit shear so density weight and covariant slots remain
    ! visible in the transformed expressions.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(/)
    use fortsym_subs, only: subs_many
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_chart_map, only: chart_map_t, chart_map_create, compose_maps, &
        map_jacobian, inverse_jacobian, transform_tensor, transform_form
    use fortsym, only: facade_chart_map_t => chart_map_t, &
        facade_chart_map_create => chart_map_create, &
        facade_transform_tensor => transform_tensor
    use fortsym_tensor, only: tensor_t, tensor_vector, tensor_covector, &
        density
    use fortsym_form, only: form_t, form_scalar, form_one, form_two, form_three, &
        form_component
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: source, target, middle, final
    type(chart_map_t) :: transition, second_transition, composed
    type(facade_chart_map_t) :: facade_transition
    type(expr_t) :: source_u(DIM), target_u(DIM), final_u(DIM)
    type(expr_t) :: source_position(DIM), target_position(DIM), final_position(DIM)
    type(expr_t) :: forward(DIM), inverse(DIM)
    type(expr_t) :: second_forward(DIM), second_inverse(DIM)
    type(expr_t) :: jacobian(DIM, DIM), inverse_map(DIM, DIM)
    type(expr_t) :: composed_jacobian(DIM, DIM), composed_inverse(DIM, DIM)
    type(expr_t) :: source_values(DIM), covector_values(DIM)
    type(expr_t) :: expected, composition
    type(tensor_t) :: vector, vector_target, vector_density, composed_vector
    type(tensor_t) :: covector, covector_target
    type(form_t) :: scalar_form, one_form, two_form, three_form
    type(form_t) :: scalar_target, one_target, two_target, three_target
    type(form_t) :: sequential_form, composed_form
    integer :: i, j, mask

    call arena%init()
    engine = make_symengine_engine(arena)
    call make_symbols()
    source_position = source_u
    target_position(1) = (target_u(1) - target_u(2))/2
    target_position(2) = target_u(2)
    target_position(3) = target_u(3)
    source = chart_create(arena, source_u, source_position)
    target = chart_create(arena, target_u, target_position)

    forward(1) = 2*source_u(1) + source_u(2)
    forward(2) = source_u(2)
    forward(3) = source_u(3)
    inverse(1) = (target_u(1) - target_u(2))/2
    inverse(2) = target_u(2)
    inverse(3) = target_u(3)
    transition = chart_map_create(source, target, forward, inverse)

    middle = chart_create(arena, target_u, target_position)
    final = chart_create(arena, final_u, final_position)
    second_forward(1) = target_u(1) + target_u(2)
    second_forward(2) = target_u(2)
    second_forward(3) = target_u(3)
    second_inverse(1) = final_u(1) - final_u(2)
    second_inverse(2) = final_u(2)
    second_inverse(3) = final_u(3)
    second_transition = chart_map_create(middle, final, second_forward, &
        second_inverse)
    composed = compose_maps(transition, second_transition)

    call suite_begin(suite, "chart map transformations")
    jacobian = map_jacobian(transition)
    inverse_map = inverse_jacobian(transition)
    call check_identity(suite, engine, "forward map K11", jacobian(1, 1) - 2)
    call check_identity(suite, engine, "forward map K12", jacobian(1, 2) - 1)
    call check_identity(suite, engine, "inverse map L11", &
        inverse_map(1, 1) - num(arena, 1)/2)
    call check_identity(suite, engine, "inverse map L12", &
        inverse_map(1, 2) + num(arena, 1)/2)

    composed_jacobian = map_jacobian(composed)
    composed_inverse = inverse_jacobian(composed)
    call check_identity(suite, engine, "composed map K12", &
        composed_jacobian(1, 2) - 2)
    call check_identity(suite, engine, "composed map L11", &
        composed_inverse(1, 1) - num(arena, 1)/2)
    call check_identity(suite, engine, "composed map forward", &
        composed%forward(1) - (2*source_u(1) + 2*source_u(2)))
    call check_identity(suite, engine, "composed map inverse", &
        composed%inverse(1) - (final_u(1) - 2*final_u(2))/2)

    do i = 1, DIM
        composition = subs_many(forward(i), source_u, inverse)
        call check_identity(suite, engine, "forward/inverse composition", &
            composition - target_u(i))
        composition = subs_many(inverse(i), target_u, forward)
        call check_identity(suite, engine, "inverse/forward composition", &
            composition - source_u(i))
    end do

    source_values = source_u
    vector = tensor_vector(source, source_values)
    vector_target = transform_tensor(transition, vector)
    call check_identity(suite, engine, "contravariant p component", &
        vector_target%component(0) - target_u(1))
    call check_identity(suite, engine, "contravariant q component", &
        vector_target%component(1) - target_u(2))
    call check_identity(suite, engine, "contravariant s component", &
        vector_target%component(2) - target_u(3))

    covector_values(1) = 2*source_u(1)
    covector_values(2) = source_u(2)
    covector_values(3) = source_u(3)
    covector = tensor_covector(source, covector_values)
    covector_target = transform_tensor(transition, covector)
    call check_identity(suite, engine, "covariant p component", &
        covector_target%component(0) - (target_u(1) - target_u(2))/2)
    call check_identity(suite, engine, "covariant q component", &
        covector_target%component(1) - (-target_u(1) + 3*target_u(2))/2)
    call check_identity(suite, engine, "covariant s component", &
        covector_target%component(2) - target_u(3))

    vector_density = transform_tensor(transition, density(vector, 1))
    call check_identity(suite, engine, "density p component", &
        vector_density%component(0) - 2*target_u(1))
    call check_identity(suite, engine, "density q component", &
        vector_density%component(1) - 2*target_u(2))
    call check_identity(suite, engine, "density s component", &
        vector_density%component(2) - 2*target_u(3))

    facade_transition = facade_chart_map_create(source, target, forward, inverse)
    vector_target = facade_transform_tensor(facade_transition, vector)
    call check_identity(suite, engine, "facade contravariant p component", &
        vector_target%component(0) - target_u(1))

    scalar_form = form_scalar(2*source_u(1) + source_u(2))
    scalar_target = transform_form(transition, scalar_form)
    call check_identity(suite, engine, "scalar form transport", &
        form_component(scalar_target, 0) - target_u(1))

    one_form = form_one(source, source_values)
    one_target = transform_form(transition, one_form)
    call check_identity(suite, engine, "one-form p component", &
        form_component(one_target, 1) - (target_u(1) - target_u(2))/4)
    call check_identity(suite, engine, "one-form q component", &
        form_component(one_target, 2) - (-target_u(1) + 5*target_u(2))/4)
    call check_identity(suite, engine, "one-form s component", &
        form_component(one_target, 4) - target_u(3))

    two_form = form_two(source, source_values)
    two_target = transform_form(transition, two_form)
    call check_identity(suite, engine, "two-form pq component", &
        form_component(two_target, 3) - (target_u(1) - target_u(2))/4)
    call check_identity(suite, engine, "two-form ps component", &
        form_component(two_target, 5) - target_u(2)/2)
    call check_identity(suite, engine, "two-form qs component", &
        form_component(two_target, 6) - (-target_u(2)/2 + &
        target_u(3)))

    three_form = form_three(source, num(arena, 1))
    three_target = transform_form(transition, three_form)
    call check_identity(suite, engine, "oriented three-form transport", &
        form_component(three_target, 7) - num(arena, 1)/2)

    sequential_form = transform_form(second_transition, one_target)
    composed_form = transform_form(composed, one_form)
    do mask = 1, 4
        if (mask == 3) cycle
        call check_identity(suite, engine, "form composition", &
            form_component(sequential_form, mask) - &
            form_component(composed_form, mask))
    end do

    vector_target = transform_tensor(second_transition, vector_target)
    composed_vector = transform_tensor(composed, vector)
    do i = 0, DIM - 1
        call check_identity(suite, engine, "tensor composition", &
            vector_target%component(i) - composed_vector%component(i))
    end do

    if (suite%failed /= 0) then
        print *, "test_fortsym_chart_map: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_chart_map.json")
    print *, "test_fortsym_chart_map: all checks passed"

contains

    subroutine make_symbols()
        source_u(1) = sym(arena, "map_x")
        source_u(2) = sym(arena, "map_y")
        source_u(3) = sym(arena, "map_z")
        target_u(1) = sym(arena, "map_p")
        target_u(2) = sym(arena, "map_q")
        target_u(3) = sym(arena, "map_s")
        final_u(1) = sym(arena, "map_r")
        final_u(2) = sym(arena, "map_t")
        final_u(3) = sym(arena, "map_v")
        final_position(1) = (final_u(1) - final_u(2))/2
        final_position(2) = final_u(2)
        final_position(3) = final_u(3)
    end subroutine make_symbols

end program test_fortsym_chart_map
