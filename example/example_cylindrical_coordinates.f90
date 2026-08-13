program example_cylindrical_coordinates
    ! Cylindrical coordinates on the regular patch rho > 0.
    ! The chart owns the coordinate map and bases; the compact explicit metric
    ! owns the connection and vector-calculus kernels.
    use fortsym
    use fortsym_connection, only: christoffel_tensor
    use fortsym_metric, only: metric_create, &
        metric_contravariant_owner => metric_contravariant
    use fortsym_string, only: chars
    use fortsym_print, only: print_expr
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: cylindrical
    type(metric_t) :: cylindrical_metric
    type(expr_t) :: coordinates(DIM), position(DIM)
    type(expr_t) :: rho, theta, zeta
    type(expr_t) :: basis(DIM, DIM), reciprocal(DIM, DIM)
    type(expr_t) :: metric(DIM, DIM), inverse(DIM, DIM)
    type(expr_t) :: metric_components(DIM, DIM)
    type(expr_t) :: jacobian_value, sqrtg_value
    type(expr_t) :: gradient(DIM), vector_value(DIM), curl_value(DIM)
    type(expr_t) :: scalar_value, covector_value(DIM), laplacian_value
    type(tensor_t) :: christoffel_value
    type(engine_result_t) :: checked
    integer :: i, j, k, indices(3)

    call reset()
    arena => default_arena()
    rho = "rho"
    theta = "theta"
    zeta = "zeta"
    coordinates = coords(rho, theta, zeta)
    position(1) = rho*cos(theta)
    position(2) = rho*sin(theta)
    position(3) = zeta
    cylindrical = chart_create(arena, coordinates, position)

    basis = covariant_basis(cylindrical)
    reciprocal = reciprocal_basis(cylindrical)
    metric = metric_covariant(cylindrical)
    jacobian_value = jacobian(cylindrical)
    sqrtg_value = sqrtg(cylindrical)

    ! Keep the connection path on the compact cylindrical metric rather than
    ! repeatedly expanding the Cartesian chain rule through every component.
    metric_components = num(arena, 0)
    metric_components(1, 1) = num(arena, 1)
    metric_components(2, 2) = rho**2
    metric_components(3, 3) = num(arena, 1)
    cylindrical_metric = metric_create(metric_components, coordinates=coordinates)
    inverse = metric_contravariant_owner(cylindrical_metric)
    christoffel_value = christoffel_tensor(cylindrical_metric)

    do i = 1, DIM
        do j = 1, DIM
            scalar_value = reciprocal(1, i)*basis(1, j)
            do k = 2, DIM
                scalar_value = scalar_value + reciprocal(k, i)*basis(k, j)
            end do
            if (i == j) scalar_value = scalar_value - 1
            call assert_zero(scalar_value, "reciprocal basis identity")
        end do
    end do

    call assert_zero(metric(1, 1) - 1, "cylindrical g_rho rho")
    call assert_zero(metric(2, 2) - rho**2, "cylindrical g_theta theta")
    call assert_zero(metric(3, 3) - 1, "cylindrical g_z z")
    call assert_zero(metric(1, 2) + metric(1, 3) + metric(2, 3), &
        "cylindrical off-diagonal metric")
    call assert_zero(inverse(1, 1) - 1, "cylindrical g^rho rho")
    call assert_zero(inverse(2, 2) - 1/rho**2, "cylindrical g^theta theta")
    call assert_zero(inverse(3, 3) - 1, "cylindrical g^z z")
    call assert_zero(jacobian_value - rho, "cylindrical signed Jacobian")
    call assert_zero(sqrtg_value**2 - rho**2, "cylindrical sqrtg square")

    indices = [1, 2, 2]
    call assert_zero(tensor_component(christoffel_value, indices) + rho, &
        "Gamma^rho_theta theta")
    indices = [2, 1, 2]
    call assert_zero(tensor_component(christoffel_value, indices) - 1/rho, &
        "Gamma^theta_rho theta")
    indices = [2, 2, 1]
    call assert_zero(tensor_component(christoffel_value, indices) - 1/rho, &
        "Gamma^theta_theta rho")

    gradient = grad(cylindrical_metric, rho)
    call assert_zero(gradient(1) - 1, "gradient of rho, radial component")
    call assert_zero(gradient(2), "gradient of rho, angular component")
    call assert_zero(gradient(3), "gradient of rho, axial component")

    vector_value = num(arena, 0)
    vector_value(1) = rho
    call assert_zero(divergence(cylindrical_metric, vector_value) - 2, &
        "divergence of rho e_rho")

    covector_value = num(arena, 0)
    covector_value(2) = rho**2
    curl_value = curl(cylindrical, covector_value)
    call assert_zero(curl_value(1), "cylindrical curl radial component")
    call assert_zero(curl_value(2), "cylindrical curl angular component")
    call assert_zero(curl_value(3) - 2, "cylindrical curl axial component")

    scalar_value = rho**2
    laplacian_value = laplacian(cylindrical_metric, scalar_value)
    call assert_zero(laplacian_value - 4, &
        "cylindrical Laplace-Beltrami of rho**2")

    print '(a)', "cylindrical coordinates (rho, theta, zeta)"
    print '(a,a)', "  J = rho; sqrt(g)^2 = ", chars(print_expr(rho**2))
    print '(a)', "  g_ij = diag(1, rho^2, 1)"
    print '(a)', "  grad(rho) = (1, 0, 0); div(rho e_rho) = 2"
    print '(a)', "  curl(rho^2 dtheta) = (0, 0, 2)"
    print '(a)', "  Laplace-Beltrami(rho^2) = 4"

contains

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = simplify(expression)
        if (.not. checked%ok) then
            write (*, '(a)') "FAILED: simplification: "//label
            error stop 1
        end if
        checked = zero_test(checked%value)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            error stop 1
        end if
    end subroutine assert_zero

end program example_cylindrical_coordinates
