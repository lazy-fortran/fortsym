program test_fortsym_connection
    ! Independent geometric oracles: a nonlinear polynomial coordinate map is
    ! flat, nonorthogonal, and has a nonconstant Jacobian, so compatibility,
    ! density, and curvature identities stay in the exact rational fragment.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_chart, only: DIM, chart_t, chart_create, christoffel, jacobian
    use fortsym_metric, only: metric_t, metric_from_chart
    use fortsym_tensor, only: tensor_t, tensor_scalar, tensor_component, &
        tensor_rank, tensor_variance, tensor_valid, metric_covariant_tensor, &
        UPPER, LOWER_VARIANCE
    use fortsym_connection, only: covariant_diff, christoffel_tensor, &
        riemann_tensor, ricci_tensor, scalar_curvature, einstein_tensor
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(native_engine_t) :: native
    type(suite_t) :: suite
    type(chart_t) :: polynomial
    type(metric_t) :: metric_owner
    type(expr_t) :: u(DIM), position(DIM), scalar_value
    type(expr_t) :: gamma(DIM, DIM, DIM), expected
    type(tensor_t) :: scalar, gradient, metric, metric_derivative
    type(tensor_t) :: metric_gradient, metric_owner_value
    type(tensor_t) :: density_value, density_derivative, gamma_value
    type(tensor_t) :: metric_gamma_value
    type(tensor_t) :: riemann, ricci, einstein
    type(tensor_t) :: metric_riemann, metric_ricci, metric_einstein
    type(expr_t) :: metric_scalar
    integer :: indices(4)

    call arena%init()
    engine = make_symengine_engine(arena)
    native = make_native_engine(arena)
    polynomial = make_polynomial_chart()
    metric_owner = metric_from_chart(polynomial)
    call suite_begin(suite, "metric connection and curvature")

    gamma = christoffel(polynomial)
    gamma_value = christoffel_tensor(polynomial)
    metric_gamma_value = christoffel_tensor(metric_owner)
    if (.not. tensor_valid(gamma_value)) error stop "Christoffel tensor invalid"
    if (.not. tensor_valid(metric_gamma_value)) then
        error stop "metric-owner Christoffel tensor invalid"
    end if
    if (tensor_rank(gamma_value) /= 3) error stop "Christoffel rank failed"
    if (tensor_variance(gamma_value, 1) /= UPPER) then
        error stop "Christoffel upper slot failed"
    end if
    if (tensor_variance(gamma_value, 2) /= LOWER_VARIANCE .or. &
        tensor_variance(gamma_value, 3) /= LOWER_VARIANCE) then
        error stop "Christoffel lower slots failed"
    end if
    indices(1) = 1
    indices(2) = 1
    indices(3) = 1
    call check_identity(suite, engine, "Christoffel tensor matches chart", &
        tensor_component(gamma_value, indices(1:3)) - gamma(1, 1, 1))
    indices(1) = 2
    indices(2) = 1
    indices(3) = 2
    call check_identity(suite, engine, "Gamma^v_uv = 1/u", &
        tensor_component(gamma_value, indices(1:3)) - 1/u(1))
    call check_identity(suite, engine, "metric-owner Christoffel matches chart", &
        tensor_component(metric_gamma_value, indices(1:3)) - &
        tensor_component(gamma_value, indices(1:3)))

    scalar_value = u(2)
    scalar = tensor_scalar(scalar_value)
    gradient = covariant_diff(polynomial, scalar)
    metric_gradient = covariant_diff(metric_owner, scalar)
    indices(1) = 1
    call check_identity(suite, engine, "nabla_Z R = 0", &
        tensor_component(gradient, indices(1:1)))
    indices(1) = 2
    call check_identity(suite, engine, "nabla_R R = 1", &
        tensor_component(gradient, indices(1:1)) - 1)
    indices(1) = 3
    call check_identity(suite, engine, "nabla_phi R = 0", &
        tensor_component(gradient, indices(1:1)))
    indices(1) = 2
    call check_identity(suite, engine, "metric-owner nabla_R R = 1", &
        tensor_component(metric_gradient, indices(1:1)) - 1)

    metric = metric_covariant_tensor(polynomial)
    metric_derivative = covariant_diff(polynomial, metric)
    call check_tensor_zero(suite, engine, native, metric_derivative, &
        "metric compatibility")
    metric_owner_value = metric_covariant_tensor(metric_owner)
    metric_derivative = covariant_diff(metric_owner, metric_owner_value)
    call check_tensor_zero(suite, engine, native, metric_derivative, &
        "metric-owner compatibility")

    density_value = tensor_scalar(jacobian(polynomial), 1)
    density_derivative = covariant_diff(polynomial, density_value)
    call check_tensor_zero(suite, engine, native, density_derivative, &
        "weight-one density compatibility")

    riemann = riemann_tensor(polynomial)
    call check_tensor_zero(suite, engine, native, riemann, &
        "polynomial chart is flat")
    ricci = ricci_tensor(polynomial)
    call check_tensor_zero(suite, engine, native, ricci, "Ricci tensor is zero")
    expected = scalar_curvature(polynomial)
    call check_identity(suite, engine, "scalar curvature is zero", expected)
    einstein = einstein_tensor(polynomial)
    call check_tensor_zero(suite, engine, native, einstein, "Einstein tensor is zero")

    metric_riemann = riemann_tensor(metric_owner)
    call check_tensor_zero(suite, engine, native, metric_riemann, &
        "metric-owner Riemann tensor is zero")
    metric_ricci = ricci_tensor(metric_owner)
    call check_tensor_zero(suite, engine, native, metric_ricci, &
        "metric-owner Ricci tensor is zero")
    metric_scalar = scalar_curvature(metric_owner)
    call check_identity(suite, engine, "metric-owner scalar curvature is zero", &
        metric_scalar)
    metric_einstein = einstein_tensor(metric_owner)
    call check_tensor_zero(suite, engine, native, metric_einstein, &
        "metric-owner Einstein tensor is zero")

    if (suite%failed /= 0) then
        print *, "test_fortsym_connection: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_connection.json")
    print *, "test_fortsym_connection: all checks passed"

contains

    function make_polynomial_chart() result(c)
        type(chart_t) :: c

        u(1) = sym(arena, "Z")
        u(2) = sym(arena, "R")
        u(3) = sym(arena, "phi")
        position(1) = u(1)
        position(2) = u(1)*u(2)
        position(3) = u(3)
        c = chart_create(arena, u, position)
    end function make_polynomial_chart

    subroutine check_tensor_zero(s, e, n, value, label)
        type(suite_t), intent(inout) :: s
        type(symengine_engine_t), intent(inout) :: e
        type(native_engine_t), intent(inout) :: n
        type(tensor_t), intent(in) :: value
        character(*), intent(in) :: label
        integer :: rank, count, flat, k

        if (.not. tensor_valid(value)) error stop "zero-check tensor invalid"
        rank = tensor_rank(value)
        count = 1
        do k = 1, rank
            count = count*DIM
        end do
        do flat = 0, count - 1
            call decode_index(flat, rank, indices)
            call check_component(s, e, n, value, indices, rank, label)
        end do
    end subroutine check_tensor_zero

    subroutine check_component(s, e, n, value, component_indices, rank, label)
        type(suite_t), intent(inout) :: s
        type(symengine_engine_t), intent(inout) :: e
        type(native_engine_t), intent(inout) :: n
        type(tensor_t), intent(in) :: value
        integer, intent(in) :: component_indices(:), rank
        character(*), intent(in) :: label
        type(expr_t) :: residual
        type(engine_result_t) :: reduced

        residual = tensor_component(value, component_indices(1:rank))
        reduced = n%simplify(residual)
        if (reduced%ok) then
            call check_identity(s, e, label, reduced%value)
        else
            call check_identity(s, e, label, residual)
        end if
    end subroutine check_component

    subroutine decode_index(flat, rank, component_indices)
        integer, intent(in) :: flat, rank
        integer, intent(out) :: component_indices(4)
        integer :: value, k

        component_indices = 1
        value = flat
        do k = 1, rank
            component_indices(k) = mod(value, DIM) + 1
            value = value/DIM
        end do
    end subroutine decode_index

end program test_fortsym_connection
