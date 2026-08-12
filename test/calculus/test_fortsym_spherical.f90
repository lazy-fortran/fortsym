program test_fortsym_spherical
    ! Independent component checks for the regular spherical chart.
    ! The chart path supplies the basis, Jacobian, and curl; the compact
    ! coordinate metric path supplies the connection and Laplace--Beltrami
    ! operators without carrying Cartesian chain-rule expansion into every
    ! component.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, sin, cos, operator(+), &
        operator(-), operator(*), operator(/), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create, covariant_basis, &
        reciprocal_basis, metric_covariant, jacobian, surface_measure, curl
    use fortsym_metric, only: metric_t, metric_create, &
        metric_contravariant, metric_valid, metric_grad, metric_divergence, &
        metric_laplacian
    use fortsym_connection, only: christoffel_tensor
    use fortsym_tensor, only: tensor_t, tensor_component
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: spherical
    type(metric_t) :: spherical_metric
    type(expr_t) :: coordinates(DIM), position(DIM)
    type(expr_t) :: radius, theta, phi
    type(expr_t) :: basis(DIM, DIM), reciprocal(DIM, DIM)
    type(expr_t) :: chart_metric(DIM, DIM), metric_values(DIM, DIM)
    type(expr_t) :: inverse(DIM, DIM), residual
    type(expr_t) :: gradient_value(DIM), vector_value(DIM)
    type(expr_t) :: covector_value(DIM), curl_value(DIM)
    type(expr_t) :: laplacian_value
    type(tensor_t) :: christoffel_value
    integer :: i, j, k, indices(3)

    call arena%init()
    engine = make_symengine_engine(arena)
    radius = sym(arena, "spherical_r")
    theta = sym(arena, "spherical_theta")
    phi = sym(arena, "spherical_phi")
    coordinates(1) = radius
    coordinates(2) = theta
    coordinates(3) = phi
    position(1) = radius*sin(theta)*cos(phi)
    position(2) = radius*sin(theta)*sin(phi)
    position(3) = radius*cos(theta)
    spherical = chart_create(arena, coordinates, position)

    basis = covariant_basis(spherical)
    reciprocal = reciprocal_basis(spherical)
    chart_metric = metric_covariant(spherical)
    call suite_begin(suite, "spherical coordinate geometry")

    do i = 1, DIM
        do j = 1, DIM
            residual = reciprocal(1, i)*basis(1, j)
            do k = 2, DIM
                residual = residual + reciprocal(k, i)*basis(k, j)
            end do
            if (i == j) residual = residual - 1
            call check_identity(suite, engine, "reciprocal basis identity", residual)
        end do
    end do

    call check_identity(suite, engine, "g_rr = 1", chart_metric(1, 1) - 1)
    call check_identity(suite, engine, "g_theta theta = r**2", &
        chart_metric(2, 2) - radius**2)
    call check_identity(suite, engine, "g_phi phi = r**2 sin(theta)**2", &
        chart_metric(3, 3) - radius**2*sin(theta)**2)
    call check_identity(suite, engine, "signed spherical Jacobian", &
        jacobian(spherical) - radius**2*sin(theta))
    call check_identity(suite, engine, "spherical surface measure squared", &
        surface_measure(spherical, 1)**2 - radius**4*sin(theta)**2)

    metric_values = num(arena, 0)
    metric_values(1, 1) = num(arena, 1)
    metric_values(2, 2) = radius**2
    metric_values(3, 3) = radius**2*sin(theta)**2
    spherical_metric = metric_create(metric_values, coordinates=coordinates)
    if (.not. metric_valid(spherical_metric)) then
        error stop "spherical metric is invalid"
    end if
    inverse = metric_contravariant(spherical_metric)
    call check_identity(suite, engine, "g^rr = 1", inverse(1, 1) - 1)
    call check_identity(suite, engine, "g^theta theta = 1/r**2", &
        inverse(2, 2) - 1/radius**2)
    call check_identity(suite, engine, &
        "g^phi phi = 1/(r**2 sin(theta)**2)", &
        inverse(3, 3) - 1/(radius**2*sin(theta)**2))

    christoffel_value = christoffel_tensor(spherical_metric)
    indices(1) = 1
    indices(2) = 2
    indices(3) = 2
    call check_identity(suite, engine, "Gamma^r_theta theta = -r", &
        tensor_component(christoffel_value, indices) + radius)
    indices(1) = 1
    indices(2) = 3
    indices(3) = 3
    call check_identity(suite, engine, "Gamma^r_phi phi", &
        tensor_component(christoffel_value, indices) + &
        radius*sin(theta)**2)
    indices(1) = 2
    indices(2) = 1
    indices(3) = 2
    call check_identity(suite, engine, "Gamma^theta_r theta = 1/r", &
        tensor_component(christoffel_value, indices) - 1/radius)
    indices(1) = 2
    indices(2) = 3
    indices(3) = 3
    call check_identity(suite, engine, "Gamma^theta_phi phi", &
        tensor_component(christoffel_value, indices) + sin(theta)*cos(theta))
    indices(1) = 3
    indices(2) = 1
    indices(3) = 3
    call check_identity(suite, engine, "Gamma^phi_r phi = 1/r", &
        tensor_component(christoffel_value, indices) - 1/radius)
    indices(1) = 3
    indices(2) = 2
    indices(3) = 3
    call check_identity(suite, engine, "Gamma^phi_theta phi", &
        tensor_component(christoffel_value, indices) - cos(theta)/sin(theta))

    gradient_value = metric_grad(spherical_metric, radius)
    call check_identity(suite, engine, "grad(r)^r = 1", gradient_value(1) - 1)
    call check_identity(suite, engine, "grad(r)^theta = 0", gradient_value(2))
    call check_identity(suite, engine, "grad(r)^phi = 0", gradient_value(3))

    vector_value = num(arena, 0)
    vector_value(1) = radius
    call check_identity(suite, engine, "div(r e_r) = 3", &
        metric_divergence(spherical_metric, vector_value) - 3)

    covector_value = num(arena, 0)
    covector_value(3) = radius**2*sin(theta)**2
    curl_value = curl(spherical, covector_value)
    call check_identity(suite, engine, "curl radial component", &
        curl_value(1) - 2*cos(theta))
    call check_identity(suite, engine, "curl theta component", &
        curl_value(2) + 2*sin(theta)/radius)
    call check_identity(suite, engine, "curl phi component", curl_value(3))

    laplacian_value = metric_laplacian(spherical_metric, radius**2)
    call check_identity(suite, engine, "Laplace-Beltrami(r**2) = 6", &
        laplacian_value - 6)

    if (suite%failed /= 0) then
        print *, "test_fortsym_spherical: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_spherical.json")
    print *, "test_fortsym_spherical: all checks passed"

end program test_fortsym_spherical
