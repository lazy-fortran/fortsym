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
    use fortsym_chart_map, only: chart_map_t, chart_map_create, map_jacobian, &
        inverse_jacobian, transform_tensor
    use fortsym, only: facade_chart_map_t => chart_map_t, &
        facade_chart_map_create => chart_map_create, &
        facade_transform_tensor => transform_tensor
    use fortsym_tensor, only: tensor_t, tensor_vector, tensor_covector, &
        density
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: source, target
    type(chart_map_t) :: transition
    type(facade_chart_map_t) :: facade_transition
    type(expr_t) :: source_u(DIM), target_u(DIM)
    type(expr_t) :: source_position(DIM), target_position(DIM)
    type(expr_t) :: forward(DIM), inverse(DIM)
    type(expr_t) :: jacobian(DIM, DIM), inverse_map(DIM, DIM)
    type(expr_t) :: source_values(DIM), covector_values(DIM)
    type(expr_t) :: expected, composition
    type(tensor_t) :: vector, vector_target, vector_density
    type(tensor_t) :: covector, covector_target
    integer :: i, j

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

    call suite_begin(suite, "chart map transformations")
    jacobian = map_jacobian(transition)
    inverse_map = inverse_jacobian(transition)
    call check_identity(suite, engine, "forward map K11", jacobian(1, 1) - 2)
    call check_identity(suite, engine, "forward map K12", jacobian(1, 2) - 1)
    call check_identity(suite, engine, "inverse map L11", &
        inverse_map(1, 1) - num(arena, 1)/2)
    call check_identity(suite, engine, "inverse map L12", &
        inverse_map(1, 2) + num(arena, 1)/2)

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
    end subroutine make_symbols

end program test_fortsym_chart_map
