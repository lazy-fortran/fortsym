program test_fortsym_tensor
    ! Independent tensor oracles: raising/lowering must be inverse metric
    ! actions, mixed metric contraction is the dimension, and contraction of
    ! an upper/lower product is the ordinary component dot product.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym, only: lie
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_metric, only: metric_t, metric_from_chart
    use fortsym_tensor, only: tensor_t, tensor_scalar, tensor_vector, tensor_covector, &
        tensor_from_components, tensor_component, tensor_rank, tensor_variance, &
        tensor_density_weight, tensor_valid, density, raise, lower, &
        tensor_product, contract, trace, permute, symmetrize, antisymmetrize, &
        metric_covariant_tensor, tensor_lie_derivative, tensor_symmetry, &
        tensor_chart_compatible, tensor_same_chart, &
        killing, &
        declare_symmetry, SYMMETRY_NONE, SYMMETRIC, ANTISYMMETRIC, UPPER, &
        LOWER_VARIANCE
    use fortsym_index, only: index_type_t, index_t, index_type, make_index, &
        index_valid, compatible_indices, INDEX_TANGENT, INDEX_INTERNAL
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: shear, other_chart
    type(metric_t) :: metric_owner
    type(expr_t) :: u(DIM), position(DIM), values(DIM), expected
    type(expr_t) :: vector_values(DIM), tensor_values(DIM), covector_values(DIM)
    type(expr_t) :: translation_values(DIM)
    type(expr_t) :: components(27), matrix_components(9)
    type(tensor_t) :: vup, vcov, vdown, roundtrip, weighted, scaled_density, outer, dot
    type(tensor_t) :: other_vector, mismatched_product, mismatched_lower
    type(tensor_t) :: metric_down, mixed, rank_three
    type(tensor_t) :: permuted, matrix, symmetric_value, antisymmetric_value
    type(tensor_t) :: scalar, vector_field, vector_lie, covector_field, &
        covector_lie, density_scalar, density_lie, generic_lie, killing_value, &
        metric_killing
    type(index_type_t) :: tangent_space, internal_space
    type(index_t) :: upper_i, lower_i, lower_j, internal_i
    integer :: indices(4), empty(0), rank_three_variance(3), i, j, k

    call arena%init()
    engine = make_symengine_engine(arena)
    shear = make_shear_chart()
    metric_owner = metric_from_chart(shear)
    call suite_begin(suite, "typed tensors")

    values(1) = u(1)
    values(2) = u(2)
    values(3) = u(3)
    vup = tensor_vector(shear, values)
    call check_metadata(suite, vup, 1, UPPER, 0, &
        "contravariant vector metadata")
    vcov = tensor_covector(shear, values)
    call check_metadata(suite, vcov, 1, LOWER_VARIANCE, 0, &
        "covariant vector metadata")

    other_chart = make_other_chart()
    other_vector = tensor_vector(other_chart, values)
    call check(suite, tensor_chart_compatible(vup, shear), &
        "tensor keeps its chart owner")
    call check(suite, .not. tensor_chart_compatible(vup, other_chart), &
        "tensor rejects a different position map")
    call check(suite, .not. tensor_same_chart(vup, other_vector), &
        "tensor comparison rejects different chart owners")
    mismatched_product = tensor_product(vup, other_vector)
    call check(suite, .not. tensor_valid(mismatched_product), &
        "tensor product rejects different chart owners")
    mismatched_lower = lower(other_chart, vup, 1)
    call check(suite, .not. tensor_valid(mismatched_lower), &
        "metric operation rejects a different chart owner")

    vector_values = num(arena, 0)
    vector_values(1) = u(1)
    vector_values(2) = u(2)
    vector_field = tensor_vector(shear, vector_values)
    scalar = tensor_scalar(u(1)**2 + u(3))
    scalar = tensor_lie_derivative(shear, vector_field, scalar)
    call check_identity(suite, engine, "Lie derivative of a scalar", &
        tensor_component(scalar, empty) - 2*u(1)**2)

    tensor_values(1) = u(1)**2
    tensor_values(2) = u(1)*u(2)
    tensor_values(3) = u(3)
    vector_lie = tensor_lie_derivative(shear, vector_field, &
        tensor_vector(shear, tensor_values))
    indices(1) = 1
    call check_identity(suite, engine, "Lie derivative upper component 1", &
        tensor_component(vector_lie, indices(1:1)) - u(1)**2)
    indices(1) = 2
    call check_identity(suite, engine, "Lie derivative upper component 2", &
        tensor_component(vector_lie, indices(1:1)) - u(1)*u(2))
    indices(1) = 3
    call check_identity(suite, engine, "Lie derivative upper component 3", &
        tensor_component(vector_lie, indices(1:1)))

    covector_values(1) = u(1)*u(2)
    covector_values(2) = u(3)
    covector_values(3) = num(arena, 0)
    covector_field = tensor_covector(shear, covector_values)
    covector_lie = tensor_lie_derivative(shear, vector_field, covector_field)
    indices(1) = 1
    call check_identity(suite, engine, "Lie derivative lower component 1", &
        tensor_component(covector_lie, indices(1:1)) - 3*u(1)*u(2))
    indices(1) = 2
    call check_identity(suite, engine, "Lie derivative lower component 2", &
        tensor_component(covector_lie, indices(1:1)) - u(3))
    indices(1) = 3
    call check_identity(suite, engine, "Lie derivative lower component 3", &
        tensor_component(covector_lie, indices(1:1)))

    density_scalar = density(tensor_scalar(u(1)), 1)
    density_lie = tensor_lie_derivative(shear, vector_field, density_scalar)
    call check_identity(suite, engine, "Lie derivative density weight term", &
        tensor_component(density_lie, empty) - 3*u(1))
    call check_metadata(suite, density_lie, 0, 0, 1, &
        "Lie derivative preserves density metadata")
    generic_lie = lie(shear, vector_field, density_scalar)
    call check_identity(suite, engine, "facade generic tensor Lie derivative", &
        tensor_component(generic_lie, empty) - tensor_component(density_lie, empty))

    ! The third coordinate is an isometry of the nonorthogonal shear metric:
    ! its coefficients depend on u_1 and u_2 but not u_3.  The Killing
    ! residual must therefore vanish in both chart and explicit-metric owners.
    translation_values = num(arena, 0)
    translation_values(3) = num(arena, 1)
    killing_value = killing(shear, tensor_vector(shear, translation_values))
    metric_killing = killing(metric_owner, &
        tensor_vector(shear, translation_values))
    call check(suite, tensor_valid(killing_value), &
        "chart Killing residual is valid")
    call check(suite, tensor_symmetry(killing_value, 1, 2) == SYMMETRIC, &
        "chart Killing residual is symmetric")
    call check(suite, tensor_valid(metric_killing), &
        "metric Killing residual is valid")
    do i = 1, DIM
        do j = 1, DIM
            indices(1) = i
            indices(2) = j
            call check_identity(suite, engine, "translation Killing residual", &
                tensor_component(killing_value, indices(1:2)))
            call check_identity(suite, engine, &
                "metric translation Killing residual", &
                tensor_component(metric_killing, indices(1:2)))
        end do
    end do

    vdown = lower(shear, vup, 1)
    call check_metadata(suite, vdown, 1, LOWER_VARIANCE, 0, &
        "lowered vector metadata")
    roundtrip = raise(shear, vdown, 1)
    do i = 1, DIM
        indices(1) = i
        call check_identity(suite, engine, "raise(lower(v)) returns v", &
            tensor_component(roundtrip, indices(1:1)) - values(i))
    end do

    weighted = density(vup, 2)
    call check_metadata(suite, weighted, 1, UPPER, 2, &
        "vector density metadata")

    scaled_density = density(vup, u(1) + 2)
    call check_metadata(suite, scaled_density, 1, UPPER, 1, &
        "component density metadata")
    indices(1) = 1
    call check_identity(suite, engine, "component density factor", &
        tensor_component(scaled_density, indices(1:1)) - (u(1) + 2)*values(1))

    outer = tensor_product(weighted, vdown)
    call check_metadata(suite, outer, 2, UPPER, 2, &
        "tensor product metadata")
    dot = contract(outer, 1, 2)
    indices(1) = 1
    expected = values(1)*tensor_component(vdown, indices(1:1))
    indices(1) = 2
    expected = expected + values(2)*tensor_component(vdown, indices(1:1))
    indices(1) = 3
    expected = expected + values(3)*tensor_component(vdown, indices(1:1))
    call check_identity(suite, engine, "upper/lower contraction", &
        tensor_component(dot, empty) - expected)

    tangent_space = index_type("tangent", DIM, INDEX_TANGENT)
    internal_space = index_type("internal", DIM, INDEX_INTERNAL)
    upper_i = make_index(tangent_space, 1, UPPER, "i", .true.)
    lower_i = make_index(tangent_space, 2, LOWER_VARIANCE, "i", .true.)
    lower_j = make_index(tangent_space, 2, LOWER_VARIANCE, "j", .true.)
    internal_i = make_index(internal_space, 2, LOWER_VARIANCE, "i", .true.)
    if (.not. index_valid(upper_i)) error stop "upper index invalid"
    if (.not. compatible_indices(upper_i, lower_i)) then
        error stop "compatible indices rejected"
    end if
    if (compatible_indices(upper_i, lower_j)) error stop "label mismatch accepted"
    if (compatible_indices(upper_i, internal_i)) error stop "space mismatch accepted"
    dot = contract(outer, upper_i, lower_i)
    call check_identity(suite, engine, "typed upper/lower contraction", &
        tensor_component(dot, empty) - expected)
    dot = contract(outer, upper_i, lower_j)
    if (tensor_valid(dot)) error stop "mismatched typed labels contracted"

    metric_down = metric_covariant_tensor(metric_owner)
    call check_metadata(suite, metric_down, 2, LOWER_VARIANCE, 0, &
        "covariant metric metadata")
    mixed = raise(metric_owner, metric_down, 1)
    call check(suite, tensor_symmetry(metric_down, 1, 2) == SYMMETRIC, &
        "metric declares covariant symmetry")
    call check(suite, tensor_symmetry(mixed, 1, 2) == SYMMETRY_NONE, &
        "raising one slot clears mixed symmetry declaration")
    do i = 1, DIM
        do j = 1, DIM
            indices(1) = i
            indices(2) = j
            expected = num(arena, 0)
            if (i == j) expected = num(arena, 1)
            call check_identity(suite, engine, "metric raise gives Kronecker delta", &
                tensor_component(mixed, indices(1:2)) - expected)
        end do
    end do
    dot = trace(mixed, 1, 2)
    call check_identity(suite, engine, "metric trace is dimension", &
        tensor_component(dot, empty) - 3)

    do k = 1, 27
        components(k) = num(arena, k)
    end do
    rank_three_variance(1) = UPPER
    rank_three_variance(2) = LOWER_VARIANCE
    rank_three_variance(3) = UPPER
    rank_three = tensor_from_components(shear, 3, components, &
        rank_three_variance, -1)
    call check_metadata(suite, rank_three, 3, UPPER, -1, &
        "rank-three tensor metadata")
    call check_slot_variances(suite, rank_three, LOWER_VARIANCE, UPPER, &
        "rank-three slot variance metadata")
    indices(1) = 2
    indices(2) = 3
    indices(3) = 1
    call check_identity(suite, engine, "flat component ordering", &
        tensor_component(rank_three, indices(1:3)) - 8)

    rank_three_variance(1) = UPPER
    rank_three_variance(2) = UPPER
    rank_three_variance(3) = UPPER
    rank_three = tensor_from_components(shear, 3, components, &
        rank_three_variance, 0)
    if (tensor_rank(rank_three) /= 3) error stop "rank metadata failed"
    if (.not. tensor_valid(rank_three)) error stop "tensor validity failed"

    permuted = permute(rank_three, [2, 1, 3])
    call check_slot_variances(suite, permuted, UPPER, UPPER, &
        "permuted tensor slot variance metadata")
    indices(1) = 1
    indices(2) = 2
    indices(3) = 3
    call check_identity(suite, engine, "tensor permutation", &
        tensor_component(permuted, indices(1:3)) - 20)

    do i = 1, 9
        matrix_components(i) = num(arena, i)
    end do
    matrix = tensor_from_components(shear, 2, matrix_components, [UPPER, UPPER])
    symmetric_value = symmetrize(matrix, 1, 2)
    antisymmetric_value = antisymmetrize(matrix, 1, 2)
    call check(suite, tensor_symmetry(symmetric_value, 1, 2) == SYMMETRIC, &
        "symmetrize retains symmetric declaration")
    call check(suite, tensor_symmetry(antisymmetric_value, 1, 2) == ANTISYMMETRIC, &
        "antisymmetrize retains antisymmetric declaration")
    matrix = declare_symmetry(symmetric_value, 1, 2, SYMMETRIC)
    call check(suite, tensor_valid(matrix), &
        "matching symmetry declaration is accepted")
    matrix = declare_symmetry(antisymmetric_value, 1, 2, SYMMETRIC)
    call check(suite, .not. tensor_valid(matrix), &
        "false symmetry declaration is refused")
    indices(1) = 1
    indices(2) = 2
    call check_identity(suite, engine, "tensor symmetrization", &
        tensor_component(symmetric_value, indices(1:2)) - 3)
    call check_identity(suite, engine, "tensor antisymmetrization", &
        tensor_component(antisymmetric_value, indices(1:2)) - 1)

    if (suite%failed /= 0) then
        print *, "test_fortsym_tensor: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_tensor.json")
    print *, "test_fortsym_tensor: all checks passed"

contains

    subroutine check_metadata(s, value, expected_rank, expected_variance, &
            expected_weight, label)
        type(suite_t), intent(inout) :: s
        type(tensor_t), intent(in) :: value
        integer, intent(in) :: expected_rank, expected_variance, expected_weight
        character(*), intent(in) :: label
        logical :: okay

        s%total = s%total + 1
        okay = tensor_valid(value)
        if (tensor_rank(value) /= expected_rank) okay = .false.
        if (tensor_variance(value, 1) /= expected_variance) okay = .false.
        if (tensor_density_weight(value) /= expected_weight) okay = .false.
        if (.not. okay) then
            s%failed = s%failed + 1
            print *, "FAIL ", label
        else
            s%passed = s%passed + 1
            print *, "PASS ", label
        end if
    end subroutine check_metadata

    subroutine check(s, condition, label)
        type(suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        s%total = s%total + 1
        if (condition) then
            s%passed = s%passed + 1
            print *, "PASS ", label
        else
            s%failed = s%failed + 1
            print *, "FAIL ", label
        end if
    end subroutine check

    subroutine check_slot_variances(s, value, expected_second, expected_third, &
            label)
        type(suite_t), intent(inout) :: s
        type(tensor_t), intent(in) :: value
        integer, intent(in) :: expected_second, expected_third
        character(*), intent(in) :: label
        logical :: okay

        s%total = s%total + 1
        okay = tensor_variance(value, 2) == expected_second .and. &
            tensor_variance(value, 3) == expected_third
        if (.not. okay) then
            s%failed = s%failed + 1
            print *, "FAIL ", label
        else
            s%passed = s%passed + 1
            print *, "PASS ", label
        end if
    end subroutine check_slot_variances

    function make_shear_chart() result(c)
        type(chart_t) :: c

        u(1) = sym(arena, "u1")
        u(2) = sym(arena, "u2")
        u(3) = sym(arena, "u3")
        position(1) = u(1) + u(2)
        position(2) = u(2)
        position(3) = u(3)
        c = chart_create(arena, u, position)
    end function make_shear_chart

    function make_other_chart() result(c)
        type(chart_t) :: c
        type(expr_t) :: other_position(DIM)

        other_position = position
        other_position(1) = position(1) + 1
        c = chart_create(arena, u, other_position)
    end function make_other_chart

end program test_fortsym_tensor
