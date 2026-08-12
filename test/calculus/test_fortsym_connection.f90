program test_fortsym_connection
    ! Independent geometric oracles: a nonlinear polynomial coordinate map is
    ! flat, nonorthogonal, and has a nonconstant Jacobian, so compatibility,
    ! density, and curvature identities stay in the exact rational fragment.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, sym, cos, sin, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t, VERDICT_FALSE
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_chart, only: DIM, chart_t, chart_create, christoffel, jacobian, &
        div_density
    use fortsym_metric, only: metric_t, metric_create, metric_from_chart, &
        metric_valid
    use fortsym_tensor, only: tensor_t, tensor_scalar, tensor_component, &
        tensor_rank, tensor_variance, tensor_valid, metric_covariant_tensor, &
        UPPER, LOWER_VARIANCE, MAX_RANK, tensor_vector, density
    use fortsym_connection, only: connection_t, connection_create, &
        connection_from_chart, connection_from_metric, connection_valid, &
        connection_convention, CONNECTION_STANDARD, CONNECTION_OPPOSITE, &
        covariant_diff, &
        covariant_divergence, torsion, nonmetricity, geodesic_residual, &
        christoffel_tensor, &
        riemann_tensor, first_bianchi_residual, ricci_tensor, &
        second_bianchi_residual, scalar_curvature, einstein_tensor
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(native_engine_t) :: native
    type(suite_t) :: suite
    type(chart_t) :: polynomial
    type(chart_t) :: cylindrical
    type(metric_t) :: metric_owner
    type(metric_t) :: cylindrical_metric
    type(metric_t) :: curved_metric
    type(metric_t) :: cartesian_metric
    type(connection_t) :: chart_connection, metric_connection, supplied_connection
    type(connection_t) :: opposite_connection
    type(expr_t) :: u(DIM), position(DIM), scalar_value
    type(expr_t) :: gamma(DIM, DIM, DIM), expected
    type(tensor_t) :: scalar, gradient, metric, metric_derivative
    type(tensor_t) :: metric_gradient, metric_owner_value
    type(tensor_t) :: density_value, density_derivative, density_vector, &
        density_divergence, gamma_value
    type(tensor_t) :: metric_gamma_value
    type(tensor_t) :: riemann, ricci, einstein
    type(tensor_t) :: metric_riemann, metric_ricci, metric_einstein
    type(tensor_t) :: bianchi, metric_bianchi, second_bianchi
    type(tensor_t) :: metric_second_bianchi, curved_riemann, curved_bianchi
    type(tensor_t) :: curved_second_bianchi
    type(tensor_t) :: chart_torsion, metric_torsion, supplied_torsion
    type(tensor_t) :: chart_nonmetricity, metric_nonmetricity
    type(tensor_t) :: supplied_nonmetricity, supplied_derivative, supplied_divergence
    type(tensor_t) :: supplied_riemann
    type(expr_t) :: metric_scalar
    type(expr_t) :: curved_components(DIM, DIM)
    type(expr_t) :: cartesian_components(DIM, DIM)
    type(expr_t) :: supplied_gamma(DIM, DIM, DIM), supplied_vector(DIM)
    type(expr_t) :: cylindrical_u(DIM), cylindrical_position(DIM)
    type(expr_t) :: geodesic_curve(DIM), geodesic_parameter
    type(expr_t) :: geodesic_value(DIM), metric_geodesic_value(DIM)
    type(expr_t) :: supplied_geodesic_value(DIM)
    type(engine_result_t) :: nonzero_result
    integer :: indices(MAX_RANK), four_indices(4)

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
    density_vector = density(tensor_vector(polynomial, u), 1)
    density_divergence = covariant_divergence(polynomial, density_vector)
    call check_identity(suite, engine, "density divergence is componentwise", &
        tensor_component(density_divergence, indices(1:0)) - &
        div_density(polynomial, u))

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

    chart_connection = connection_from_chart(polynomial)
    metric_connection = connection_from_metric(metric_owner)
    if (.not. connection_valid(chart_connection)) error stop "chart connection invalid"
    if (.not. connection_valid(metric_connection)) error stop "metric connection invalid"
    if (connection_convention(chart_connection) /= CONNECTION_STANDARD) then
        error stop "connection convention failed"
    end if
    chart_torsion = torsion(chart_connection)
    metric_torsion = torsion(metric_connection)
    indices(1) = 1
    indices(2) = 1
    indices(3) = 2
    call check_identity(suite, engine, "chart connection torsion is zero", &
        tensor_component(chart_torsion, indices(1:3)))
    call check_identity(suite, engine, "metric connection torsion is zero", &
        tensor_component(metric_torsion, indices(1:3)))
    chart_nonmetricity = nonmetricity(chart_connection, metric_owner)
    metric_nonmetricity = nonmetricity(metric_connection, metric_owner)
    call check_identity(suite, engine, "chart connection nonmetricity is zero", &
        tensor_component(chart_nonmetricity, indices(1:3)))
    call check_identity(suite, engine, "metric connection nonmetricity is zero", &
        tensor_component(metric_nonmetricity, indices(1:3)))

    cartesian_components = num(arena, 0)
    cartesian_components(1, 1) = num(arena, 1)
    cartesian_components(2, 2) = num(arena, 1)
    cartesian_components(3, 3) = num(arena, 1)
    cartesian_metric = metric_create(cartesian_components, orientation=1, &
        coordinates=u)
    supplied_gamma = num(arena, 0)
    supplied_gamma(1, 1, 2) = u(1)
    supplied_gamma(1, 2, 1) = 2*u(2)
    supplied_connection = connection_create(supplied_gamma, u)
    if (.not. connection_valid(supplied_connection)) then
        error stop "supplied connection invalid"
    end if
    supplied_torsion = torsion(supplied_connection)
    indices(1) = 1
    indices(2) = 1
    indices(3) = 2
    call check_identity(suite, engine, "supplied torsion T^1_12", &
        tensor_component(supplied_torsion, indices(1:3)) - (u(1) - 2*u(2)))
    supplied_nonmetricity = nonmetricity(supplied_connection, cartesian_metric)
    call check_identity(suite, engine, "supplied Q_112 = 4*R", &
        tensor_component(supplied_nonmetricity, indices(1:3)) - 4*u(2))
    supplied_riemann = riemann_tensor(supplied_connection)
    call check_identity(suite, engine, "supplied R^1_212 = -2*R*Z", &
        tensor_component(supplied_riemann, [1, 2, 1, 2]) + 2*u(1)*u(2))
    opposite_connection = connection_create(supplied_gamma, u, CONNECTION_OPPOSITE)
    if (.not. connection_valid(opposite_connection)) then
        error stop "opposite supplied connection invalid"
    end if
    if (connection_convention(opposite_connection) /= CONNECTION_OPPOSITE) then
        error stop "opposite connection convention failed"
    end if
    supplied_riemann = riemann_tensor(opposite_connection)
    call check_identity(suite, engine, "opposite R^1_212 = 2*R*Z", &
        tensor_component(supplied_riemann, [1, 2, 1, 2]) - 2*u(1)*u(2))
    supplied_vector = u
    supplied_derivative = covariant_diff(supplied_connection, &
        tensor_vector(polynomial, supplied_vector))
    indices(1) = 1
    indices(2) = 2
    call check_identity(suite, engine, "supplied nabla_2 V^1", &
        tensor_component(supplied_derivative, indices(1:2)) - 2*u(1)*u(2))
    supplied_divergence = covariant_divergence(supplied_connection, &
        tensor_vector(polynomial, supplied_vector))
    call check_identity(suite, engine, "supplied divergence", &
        tensor_component(supplied_divergence, indices(1:0)) - (3 + u(1)*u(2)))

    geodesic_parameter = sym(arena, "connection_lambda")
    geodesic_curve(1) = geodesic_parameter
    geodesic_curve(2) = geodesic_parameter
    geodesic_curve(3) = num(arena, 0)
    supplied_geodesic_value = geodesic_residual(supplied_connection, &
        geodesic_curve, geodesic_parameter)
    call check_identity(suite, engine, "supplied connection geodesic", &
        supplied_geodesic_value(1) - 3*geodesic_parameter)
    call check_identity(suite, engine, "supplied connection geodesic y", &
        supplied_geodesic_value(2))
    call check_identity(suite, engine, "supplied connection geodesic z", &
        supplied_geodesic_value(3))

    cylindrical = make_cylindrical_chart()
    cylindrical_metric = metric_from_chart(cylindrical)
    geodesic_curve(1) = sym(arena, "R0")
    geodesic_parameter = sym(arena, "lambda")
    geodesic_curve(2) = geodesic_parameter
    geodesic_curve(3) = num(arena, 0)
    geodesic_value = geodesic_residual(cylindrical, geodesic_curve, &
        geodesic_parameter)
    call check_identity(suite, engine, "cylindrical radial geodesic residual", &
        geodesic_value(1) + geodesic_curve(1))
    call check_identity(suite, engine, "cylindrical angular geodesic residual", &
        geodesic_value(2))
    call check_identity(suite, engine, "cylindrical axial geodesic residual", &
        geodesic_value(3))
    metric_geodesic_value = geodesic_residual(cylindrical_metric, geodesic_curve, &
        geodesic_parameter)
    call check_identity(suite, engine, "metric-owner cylindrical radial geodesic", &
        metric_geodesic_value(1) + geodesic_curve(1))
    call check_identity(suite, engine, "metric-owner cylindrical angular geodesic", &
        metric_geodesic_value(2))
    call check_identity(suite, engine, "metric-owner cylindrical axial geodesic", &
        metric_geodesic_value(3))

    bianchi = first_bianchi_residual(polynomial)
    call check_tensor_zero(suite, engine, native, bianchi, &
        "flat chart first Bianchi residual is zero")
    metric_bianchi = first_bianchi_residual(metric_owner)
    call check_tensor_zero(suite, engine, native, metric_bianchi, &
        "metric-owner first Bianchi residual is zero")

    second_bianchi = second_bianchi_residual(polynomial)
    call check_tensor_zero(suite, engine, native, second_bianchi, &
        "flat chart second Bianchi residual is zero")
    metric_second_bianchi = second_bianchi_residual(metric_owner)
    call check_tensor_zero(suite, engine, native, metric_second_bianchi, &
        "metric-owner second Bianchi residual is zero")

    curved_components = num(arena, 0)
    curved_components(1, 1) = num(arena, 1)
    curved_components(2, 2) = (1 + u(1)**2)**2
    curved_components(3, 3) = num(arena, 1)
    curved_metric = metric_create(curved_components, orientation=1, &
        coordinates=u)
    if (.not. metric_valid(curved_metric)) error stop "curved metric invalid"
    curved_riemann = riemann_tensor(curved_metric)
    curved_bianchi = first_bianchi_residual(curved_metric)
    call check_tensor_zero(suite, engine, native, curved_bianchi, &
        "non-flat metric first Bianchi residual is zero")
    curved_second_bianchi = second_bianchi_residual(curved_metric)
    call check_tensor_zero(suite, engine, native, curved_second_bianchi, &
        "non-flat metric second Bianchi residual is zero")
    four_indices(1) = 1
    four_indices(2) = 2
    four_indices(3) = 1
    four_indices(4) = 2
    nonzero_result = engine%zero_test(tensor_component(curved_riemann, four_indices))
    if (.not. nonzero_result%ok .or. nonzero_result%verdict /= VERDICT_FALSE) then
        error stop "non-flat metric did not produce nonzero curvature"
    end if

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

    function make_cylindrical_chart() result(c)
        type(chart_t) :: c

        cylindrical_u(1) = sym(arena, "rho")
        cylindrical_u(2) = sym(arena, "theta")
        cylindrical_u(3) = sym(arena, "zeta")
        cylindrical_position(1) = cylindrical_u(1)*cos(cylindrical_u(2))
        cylindrical_position(2) = cylindrical_u(1)*sin(cylindrical_u(2))
        cylindrical_position(3) = cylindrical_u(3)
        c = chart_create(arena, cylindrical_u, cylindrical_position)
    end function make_cylindrical_chart

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
        integer, intent(out) :: component_indices(MAX_RANK)
        integer :: value, k

        component_indices = 1
        value = flat
        do k = 1, rank
            component_indices(k) = mod(value, DIM) + 1
            value = value/DIM
        end do
    end subroutine decode_index

end program test_fortsym_connection
