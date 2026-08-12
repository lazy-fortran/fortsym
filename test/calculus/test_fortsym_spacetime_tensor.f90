program test_fortsym_spacetime_tensor
    ! Independent component checks for runtime-dimension variance and density
    ! metadata.  The metric has det(g)=1, so the lower/raise round trip has a
    ! compact expected result without relying on a numerical probe.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, sqrt, exp, operator(+), operator(-), &
        operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_valid
    use fortsym_spacetime_tensor, only: spacetime_tensor_t, &
        spacetime_tensor_scalar, spacetime_tensor_vector, &
        spacetime_tensor_component, &
        spacetime_tensor_rank, spacetime_tensor_dimension, &
        spacetime_tensor_variance, spacetime_tensor_density_weight, &
        spacetime_tensor_valid, spacetime_tensor_density_factor, &
        spacetime_tensor_raise, spacetime_tensor_lower, &
        spacetime_tensor_product, spacetime_tensor_contract, &
        spacetime_tensor_permute, spacetime_metric_covariant_tensor, &
        spacetime_metric_contravariant_tensor, spacetime_tensor_covariant_diff, &
        spacetime_tensor_covariant_divergence, &
        spacetime_tensor_lie_derivative, &
        spacetime_killing, &
        SPACETIME_UPPER, SPACETIME_LOWER
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(spacetime_metric_t) :: metric
    type(spacetime_tensor_t) :: upper, lower, roundtrip, covariant, contravariant
    type(spacetime_tensor_t) :: product, scalar, permuted, density_value
    type(spacetime_metric_t) :: curved_metric
    type(spacetime_tensor_t) :: curved_vector, derivative, metric_derivative
    type(spacetime_tensor_t) :: lie_result, density_scalar, dilation_vector
    type(spacetime_tensor_t) :: divergence_result
    type(spacetime_tensor_t) :: curved_density, density_derivative, scalar_tensor
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: values(SPACETIME_DIM), factor
    type(expr_t) :: curved_components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: density_values(SPACETIME_DIM)
    integer :: signature(SPACETIME_DIM), indices(2), scalar_indices(1), &
        derivative_indices(3), permutation(2), empty(0)

    call arena%init()
    engine = make_symengine_engine(arena)
    coordinates(1) = sym(arena, "st_t")
    coordinates(2) = sym(arena, "st_x")
    coordinates(3) = sym(arena, "st_y")
    coordinates(4) = sym(arena, "st_z")
    components = num(arena, 0)
    components(1, 1) = num(arena, 2)
    components(1, 2) = num(arena, 1)
    components(2, 1) = num(arena, 1)
    components(2, 2) = num(arena, 1)
    signature = 1
    metric = spacetime_metric_create(components, 2, coordinates, signature, 1)
    if (.not. spacetime_metric_valid(metric)) error stop "invalid 2D tensor metric"
    call suite_begin(suite, "runtime-dimension spacetime tensors")

    values = num(arena, 0)
    values(1) = coordinates(1)
    values(2) = coordinates(2)
    upper = spacetime_tensor_vector(metric, values)
    call check(suite, spacetime_tensor_valid(upper), "upper vector is valid")
    call check(suite, spacetime_tensor_dimension(upper) == 2, &
        "runtime tensor dimension is retained")
    call check(suite, spacetime_tensor_variance(upper, 1) == SPACETIME_UPPER, &
        "upper variance metadata")

    lower = spacetime_tensor_lower(metric, upper, 1)
    roundtrip = spacetime_tensor_raise(metric, lower, 1)
    scalar_indices(1) = 1
    call check_identity(suite, engine, "flat/sharp round trip component 1", &
        spacetime_tensor_component(roundtrip, scalar_indices) - values(1))
    scalar_indices(1) = 2
    call check_identity(suite, engine, "flat/sharp round trip component 2", &
        spacetime_tensor_component(roundtrip, scalar_indices) - values(2))
    call check(suite, spacetime_tensor_variance(lower, 1) == SPACETIME_LOWER, &
        "lower variance metadata")

    covariant = spacetime_metric_covariant_tensor(metric)
    contravariant = spacetime_metric_contravariant_tensor(metric)
    indices(1) = 1
    indices(2) = 2
    call check_identity(suite, engine, "covariant metric component", &
        spacetime_tensor_component(covariant, indices) - 1)
    call check_identity(suite, engine, "contravariant metric component", &
        spacetime_tensor_component(contravariant, indices) + 1)

    factor = sqrt(num(arena, 1))
    density_value = spacetime_tensor_density_factor(upper, factor)
    call check(suite, spacetime_tensor_density_weight(density_value) == 1, &
        "vector density weight")
    scalar_indices(1) = 1
    call check_identity(suite, engine, "density factor preserves components", &
        spacetime_tensor_component(density_value, scalar_indices) - &
        factor*spacetime_tensor_component(upper, scalar_indices))

    product = spacetime_tensor_product(upper, lower)
    scalar = spacetime_tensor_contract(product, 1, 2)
    call check(suite, spacetime_tensor_rank(scalar) == 0, &
        "opposite-variance product contracts to scalar")
    call check_identity(suite, engine, "metric contraction", &
        spacetime_tensor_component(scalar, empty) - &
        (2*coordinates(1)**2 + 2*coordinates(1)*coordinates(2) + &
        coordinates(2)**2))
    permutation(1) = 2
    permutation(2) = 1
    permuted = spacetime_tensor_permute(covariant, permutation)
    indices(1) = 1
    indices(2) = 2
    call check_identity(suite, engine, "permutation preserves tensor meaning", &
        spacetime_tensor_component(permuted, indices) - &
        spacetime_tensor_component(covariant, permutation))

    curved_components = num(arena, 0)
    curved_components(1, 1) = num(arena, 1)
    curved_components(2, 2) = exp(2*coordinates(1))
    curved_metric = spacetime_metric_create(curved_components, 2, coordinates, &
        signature, 1)
    values = num(arena, 0)
    values(2) = num(arena, 1)
    curved_vector = spacetime_tensor_vector(curved_metric, values)
    derivative = spacetime_tensor_covariant_diff(curved_metric, curved_vector)
    indices(1) = 2
    indices(2) = 1
    call check_identity(suite, engine, "nabla_1 V^2", &
        spacetime_tensor_component(derivative, indices) - 1)
    indices(1) = 1
    indices(2) = 2
    call check_identity(suite, engine, "nabla_2 V^1", &
        spacetime_tensor_component(derivative, indices) + exp(2*coordinates(1)))
    indices(1) = 2
    indices(2) = 2
    call check_identity(suite, engine, "nabla_2 V^2", &
        spacetime_tensor_component(derivative, indices))
    covariant = spacetime_metric_covariant_tensor(curved_metric)
    metric_derivative = spacetime_tensor_covariant_diff(curved_metric, covariant)
    derivative_indices(1) = 2
    derivative_indices(2) = 2
    derivative_indices(3) = 1
    call check_identity(suite, engine, "nabla_1 g_22", &
        spacetime_tensor_component(metric_derivative, derivative_indices))
    lie_result = spacetime_killing(curved_metric, curved_vector)
    indices(1) = 2
    indices(2) = 2
    call check_identity(suite, engine, "L_d/dx g_22", &
        spacetime_tensor_component(lie_result, indices))
    values = num(arena, 0)
    values(1) = coordinates(1)
    dilation_vector = spacetime_tensor_vector(curved_metric, values)
    lie_result = spacetime_tensor_lie_derivative(curved_metric, &
        dilation_vector, covariant)
    indices(1) = 1
    indices(2) = 1
    call check_identity(suite, engine, "L_(t d/dt) g_11", &
        spacetime_tensor_component(lie_result, indices) - 2)
    indices(1) = 2
    indices(2) = 2
    call check_identity(suite, engine, "L_(t d/dt) g_22", &
        spacetime_tensor_component(lie_result, indices) - &
        2*coordinates(1)*exp(2*coordinates(1)))
    scalar_tensor = spacetime_tensor_scalar(curved_metric, coordinates(1))
    lie_result = spacetime_tensor_lie_derivative(curved_metric, &
        dilation_vector, scalar_tensor)
    call check_identity(suite, engine, "L_(t d/dt) t", &
        spacetime_tensor_component(lie_result, empty) - coordinates(1))
    density_scalar = spacetime_tensor_scalar(curved_metric, num(arena, 1), 1)
    lie_result = spacetime_tensor_lie_derivative(curved_metric, &
        dilation_vector, density_scalar)
    call check_identity(suite, engine, "L_(t d/dt) density", &
        spacetime_tensor_component(lie_result, empty) - 1)
    divergence_result = spacetime_tensor_covariant_divergence(curved_metric, &
        curved_vector)
    call check_identity(suite, engine, "div constant x vector", &
        spacetime_tensor_component(divergence_result, empty))
    divergence_result = spacetime_tensor_covariant_divergence(curved_metric, &
        dilation_vector)
    call check_identity(suite, engine, "div t d/dt", &
        spacetime_tensor_component(divergence_result, empty) - &
        (1 + coordinates(1)))
    density_values = num(arena, 0)
    density_values(1) = coordinates(1)
    curved_density = spacetime_tensor_vector(curved_metric, density_values, 1)
    divergence_result = spacetime_tensor_covariant_divergence(curved_metric, &
        curved_density)
    call check_identity(suite, engine, "div weight-one t density", &
        spacetime_tensor_component(divergence_result, empty) - 1)
    contravariant = spacetime_metric_contravariant_tensor(curved_metric)
    divergence_result = spacetime_tensor_covariant_divergence(curved_metric, &
        contravariant)
    scalar_indices(1) = 2
    call check_identity(suite, engine, "div contravariant metric", &
        spacetime_tensor_component(divergence_result, scalar_indices))
    density_values = num(arena, 0)
    density_values(2) = exp(coordinates(1))
    curved_density = spacetime_tensor_vector(curved_metric, density_values, 1)
    density_derivative = spacetime_tensor_covariant_diff(curved_metric, &
        curved_density)
    indices(1) = 2
    indices(2) = 1
    call check_identity(suite, engine, "density nabla_1 W^2", &
        spacetime_tensor_component(density_derivative, indices) - &
        exp(coordinates(1)))
    scalar_tensor = spacetime_tensor_scalar(curved_metric, coordinates(1))
    derivative = spacetime_tensor_covariant_diff(curved_metric, scalar_tensor)
    scalar_indices(1) = 1
    call check_identity(suite, engine, "scalar derivative", &
        spacetime_tensor_component(derivative, scalar_indices) - 1)

    if (suite%failed /= 0) error stop "spacetime tensor checks failed"
    call suite_end(suite, "/tmp/fortsym_spacetime_tensor.json")
    print *, "test_fortsym_spacetime_tensor: all checks passed"

contains

    subroutine check(s, condition, label)
        type(suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        s%total = s%total + 1
        if (condition) then
            s%passed = s%passed + 1
            print '(a,a)', "PASS ", label
        else
            s%failed = s%failed + 1
            print '(a,a)', "FAIL ", label
        end if
    end subroutine check

end program test_fortsym_spacetime_tensor
