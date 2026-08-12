program example_spacetime_tensor_calculus
    ! Physicist's component views on one runtime-dimension metric:
    ! V^i, V_i = g_ij V^j, and the weight-one density sqrt(g) V^i.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(spacetime_metric_t) :: metric
    type(spacetime_metric_t) :: curved_metric
    type(spacetime_tensor_t) :: v_upper, v_lower, roundtrip, density_value
    type(spacetime_tensor_t) :: dyad, norm
    type(spacetime_tensor_t) :: curved_vector, derivative_tensor
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: vector_values(SPACETIME_DIM), volume_factor
    type(expr_t) :: lower_display, density_display, norm_display
    type(expr_t) :: derivative_display
    type(engine_result_t) :: checked
    integer :: signature(SPACETIME_DIM), empty(0), single_index(1), &
        derivative_indices(2), i

    call reset()
    arena => default_arena()
    do i = 1, SPACETIME_DIM
        coordinates(i) = sym(arena, "q_"//chars(str(i)))
    end do
    components = num(arena, 0)
    components(1, 1) = num(arena, 2)
    components(2, 2) = num(arena, 1)
    signature = 1
    metric = spacetime_metric_create(components, 2, coordinates, signature, 1)
    if (.not. spacetime_metric_valid(metric)) error stop "invalid tensor metric"

    vector_values = num(arena, 0)
    vector_values(1) = coordinates(1)
    vector_values(2) = coordinates(2)
    v_upper = spacetime_tensor_vector(metric, vector_values)
    v_lower = spacetime_tensor_lower(metric, v_upper, 1)
    roundtrip = spacetime_tensor_raise(metric, v_lower, 1)
    single_index(1) = 1
    call assert_zero(spacetime_tensor_component(roundtrip, single_index) - &
        vector_values(1), &
        "raise(lower(V)) component 1")
    single_index(1) = 2
    call assert_zero(spacetime_tensor_component(roundtrip, single_index) - &
        vector_values(2), &
        "raise(lower(V)) component 2")

    volume_factor = spacetime_metric_sqrtg(metric)
    density_value = spacetime_tensor_density_factor(v_upper, volume_factor)
    single_index(1) = 1
    checked = simplify(spacetime_tensor_component(v_lower, single_index))
    if (.not. checked%ok) error stop "failed to simplify lowered component"
    lower_display = checked%value
    checked = simplify(spacetime_tensor_component(density_value, single_index))
    if (.not. checked%ok) error stop "failed to simplify density component"
    density_display = checked%value
    dyad = spacetime_tensor_product(v_upper, v_lower)
    norm = spacetime_tensor_contract(dyad, 1, 2)
    checked = simplify(spacetime_tensor_component(norm, empty))
    if (.not. checked%ok) error stop "failed to simplify tensor contraction"
    norm_display = checked%value

    print '(a)', "runtime-dimension tensor calculus"
    print '(a,a)', "  V^1 = ", &
        chars(print_expr(spacetime_tensor_component(v_upper, single_index)))
    print '(a,a)', "  V_1 = ", chars(print_expr(lower_display))
    print '(a,a)', "  sqrtg V^1 = ", &
        chars(print_expr(density_display))
    print '(a,a)', "  V^i V_i = ", chars(print_expr(norm_display))
    print '(a,i0)', "  density weight = ", &
        spacetime_tensor_density_weight(density_value)

    components = num(arena, 0)
    components(1, 1) = num(arena, 1)
    components(2, 2) = exp(2*coordinates(1))
    curved_metric = spacetime_metric_create(components, 2, coordinates, &
        signature, 1)
    vector_values = num(arena, 0)
    vector_values(2) = num(arena, 1)
    curved_vector = spacetime_tensor_vector(curved_metric, vector_values)
    derivative_tensor = spacetime_tensor_covariant_diff(curved_metric, &
        curved_vector)
    derivative_indices(1) = 2
    derivative_indices(2) = 1
    checked = simplify(spacetime_tensor_component(derivative_tensor, &
        derivative_indices))
    if (.not. checked%ok) error stop "failed to simplify covariant derivative"
    derivative_display = checked%value
    print '(a,a)', "  nabla_1 V^2 = ", chars(print_expr(derivative_display))
    derivative_indices(1) = 1
    derivative_indices(2) = 2
    checked = simplify(spacetime_tensor_component(derivative_tensor, &
        derivative_indices))
    if (.not. checked%ok) error stop "failed to simplify covariant derivative"
    derivative_display = checked%value
    print '(a,a)', "  nabla_2 V^1 = ", chars(print_expr(derivative_display))

contains

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = zero_test(expression)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            error stop 1
        end if
    end subroutine assert_zero

end program example_spacetime_tensor_calculus
