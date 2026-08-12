program test_fortsym_spacetime_tensor
    ! Independent component checks for runtime-dimension variance and density
    ! metadata.  The metric has det(g)=1, so the lower/raise round trip has a
    ! compact expected result without relying on a numerical probe.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, sqrt, operator(+), operator(-), &
        operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_valid
    use fortsym_spacetime_tensor, only: spacetime_tensor_t, &
        spacetime_tensor_vector, spacetime_tensor_component, &
        spacetime_tensor_rank, spacetime_tensor_dimension, &
        spacetime_tensor_variance, spacetime_tensor_density_weight, &
        spacetime_tensor_valid, spacetime_tensor_density_factor, &
        spacetime_tensor_raise, spacetime_tensor_lower, &
        spacetime_tensor_product, spacetime_tensor_contract, &
        spacetime_tensor_permute, spacetime_metric_covariant_tensor, &
        spacetime_metric_contravariant_tensor, SPACETIME_UPPER, SPACETIME_LOWER
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(spacetime_metric_t) :: metric
    type(spacetime_tensor_t) :: upper, lower, roundtrip, covariant, contravariant
    type(spacetime_tensor_t) :: product, scalar, permuted, density_value
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: values(SPACETIME_DIM), factor
    integer :: signature(SPACETIME_DIM), indices(2), empty(0)

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
    indices(1) = 1
    call check_identity(suite, engine, "flat/sharp round trip component 1", &
        spacetime_tensor_component(roundtrip, indices(1:1)) - values(1))
    indices(1) = 2
    call check_identity(suite, engine, "flat/sharp round trip component 2", &
        spacetime_tensor_component(roundtrip, indices(1:1)) - values(2))
    call check(suite, spacetime_tensor_variance(lower, 1) == SPACETIME_LOWER, &
        "lower variance metadata")

    covariant = spacetime_metric_covariant_tensor(metric)
    contravariant = spacetime_metric_contravariant_tensor(metric)
    indices = [1, 2]
    call check_identity(suite, engine, "covariant metric component", &
        spacetime_tensor_component(covariant, indices) - 1)
    call check_identity(suite, engine, "contravariant metric component", &
        spacetime_tensor_component(contravariant, indices) + 1)

    factor = sqrt(num(arena, 1))
    density_value = spacetime_tensor_density_factor(upper, factor)
    call check(suite, spacetime_tensor_density_weight(density_value) == 1, &
        "vector density weight")
    indices(1) = 1
    call check_identity(suite, engine, "density factor preserves components", &
        spacetime_tensor_component(density_value, indices(1:1)) - &
        factor*spacetime_tensor_component(upper, indices(1:1)))

    product = spacetime_tensor_product(upper, lower)
    scalar = spacetime_tensor_contract(product, 1, 2)
    call check(suite, spacetime_tensor_rank(scalar) == 0, &
        "opposite-variance product contracts to scalar")
    call check_identity(suite, engine, "metric contraction", &
        spacetime_tensor_component(scalar, empty) - &
        (2*coordinates(1)**2 + 2*coordinates(1)*coordinates(2) + &
        coordinates(2)**2))
    permuted = spacetime_tensor_permute(covariant, [2, 1])
    indices = [1, 2]
    call check_identity(suite, engine, "permutation preserves tensor meaning", &
        spacetime_tensor_component(permuted, indices) - &
        spacetime_tensor_component(covariant, [2, 1]))

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
