program connection_native
    ! Short native example: covariant differentiation, tensor density weight,
    ! and curvature on a nonlinear nonorthogonal flat coordinate chart.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: chart
    type(expr_t) :: u(DIM), position(DIM), value
    type(expr_t) :: component, scalar
    type(tensor_t) :: christoffel_value, metric_value, metric_derivative_value
    type(engine_result_t) :: simplified, checked
    integer :: indices(3)

    call reset()
    arena => default_arena()
    u(1) = "u"
    u(2) = "v"
    u(3) = "w"
    position(1) = u(1)
    position(2) = u(1)*u(2)
    position(3) = u(3)
    chart = chart_create(arena, u, position)

    christoffel_value = christoffel_tensor(chart)
    indices(1) = 2
    indices(2) = 1
    indices(3) = 2
    component = tensor_component(christoffel_value, indices)
    simplified = simplify(component)
    if (.not. simplified%ok) error stop "Christoffel simplification failed"
    print '(a,a)', "Gamma^v_uv = ", chars(print_expr(simplified%value))

    metric_value = metric_covariant_tensor(chart)
    metric_derivative_value = covariant_diff(chart, metric_value)
    indices(1) = 1
    indices(2) = 2
    indices(3) = 2
    checked = zero_test(tensor_component(metric_derivative_value, indices))
    if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
        error stop "metric compatibility failed"
    end if

    value = jacobian(chart)
    scalar = scalar_curvature(chart)
    checked = zero_test(scalar)
    if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
        error stop "flat chart has nonzero scalar curvature"
    end if

    print '(a,a)', "weight-one density source = ", chars(print_expr(value))
    print '(a)', "checked covariant metric derivative and scalar curvature = 0"

end program connection_native
