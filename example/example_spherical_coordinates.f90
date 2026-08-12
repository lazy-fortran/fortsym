program example_spherical_coordinates
    ! Spherical coordinates on the regular patch r > 0, 0 < theta < pi.
    ! The chart owner derives the basis, metric, connection, and vector
    ! calculus; the explicit branch note only resolves sqrt(r**4 sin(theta)**2)
    ! to the positive volume coefficient r**2 sin(theta) for the displayed
    ! oriented form.
    use fortsym
    use fortsym_connection, only: christoffel_tensor
    use fortsym_metric, only: metric_create, &
        metric_contravariant_owner => metric_contravariant
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: spherical
    type(metric_t) :: spherical_metric
    type(expr_t) :: coordinates(DIM), position(DIM)
    type(expr_t) :: radius, theta, phi
    type(expr_t) :: basis(DIM, DIM), reciprocal(DIM, DIM)
    type(expr_t) :: metric(DIM, DIM), inverse(DIM, DIM)
    type(expr_t) :: metric_components(DIM, DIM)
    type(expr_t) :: jacobian_value, sqrtg_value
    type(expr_t) :: gradient(DIM), vector_value(DIM), curl_value(DIM)
    type(expr_t) :: scalar_value, covector_value(DIM), laplacian_value
    type(expr_t) :: expected_volume
    type(tensor_t) :: christoffel_value
    type(engine_result_t) :: checked
    integer :: i, j, k, indices(3)

    call reset()
    arena => default_arena()
    radius = "r"
    theta = "theta"
    phi = "phi"
    coordinates(1) = radius
    coordinates(2) = theta
    coordinates(3) = phi
    position(1) = radius*sin(theta)*cos(phi)
    position(2) = radius*sin(theta)*sin(phi)
    position(3) = radius*cos(theta)
    spherical = chart_create(arena, coordinates, position)

    basis = covariant_basis(spherical)
    reciprocal = reciprocal_basis(spherical)
    metric = metric_covariant(spherical)
    jacobian_value = jacobian(spherical)
    sqrtg_value = sqrtg(spherical)
    expected_volume = radius**2*sin(theta)

    ! Use the compact component form for the expensive connection and vector
    ! calculus owners. This is the same metric derived above, with the
    ! regular spherical formulas made explicit instead of carrying the raw
    ! Cartesian chain-rule expansion through every Christoffel component.
    metric_components = num(arena, 0)
    metric_components(1, 1) = num(arena, 1)
    metric_components(2, 2) = radius**2
    metric_components(3, 3) = radius**2*sin(theta)**2
    spherical_metric = metric_create(metric_components, coordinates=coordinates)
    inverse = metric_contravariant_owner(spherical_metric)
    christoffel_value = christoffel_tensor(spherical_metric)

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

    call assert_zero(metric(1, 1) - 1, "spherical g_rr")
    call assert_zero(metric(2, 2) - radius**2, "spherical g_theta theta")
    call assert_zero(metric(3, 3) - radius**2*sin(theta)**2, &
        "spherical g_phi phi")
    call assert_zero(metric(1, 2) + metric(1, 3) + metric(2, 3), &
        "spherical off-diagonal metric")
    call assert_zero(inverse(1, 1) - 1, "spherical g^rr")
    call assert_zero(inverse(2, 2) - 1/radius**2, "spherical g^theta theta")
    call assert_zero(inverse(3, 3) - 1/(radius**2*sin(theta)**2), &
        "spherical g^phi phi")
    call assert_zero(jacobian_value - expected_volume, &
        "spherical signed Jacobian")
    call assert_zero(sqrtg_value**2 - expected_volume**2, &
        "spherical sqrtg square")
    indices(1) = 1
    indices(2) = 2
    indices(3) = 2
    call assert_zero(tensor_component(christoffel_value, indices) + radius, &
        "Gamma^r_theta theta")
    indices(1) = 1
    indices(2) = 3
    indices(3) = 3
    call assert_zero(tensor_component(christoffel_value, indices) + &
        radius*sin(theta)**2, &
        "Gamma^r_phi phi")
    indices(1) = 2
    indices(2) = 1
    indices(3) = 2
    call assert_zero(tensor_component(christoffel_value, indices) - 1/radius, &
        "Gamma^theta_r theta")
    indices(1) = 2
    indices(2) = 3
    indices(3) = 3
    call assert_zero(tensor_component(christoffel_value, indices) + &
        sin(theta)*cos(theta), &
        "Gamma^theta_phi phi")
    indices(1) = 3
    indices(2) = 1
    indices(3) = 3
    call assert_zero(tensor_component(christoffel_value, indices) - 1/radius, &
        "Gamma^phi_r phi")
    indices(1) = 3
    indices(2) = 2
    indices(3) = 3
    call assert_zero(tensor_component(christoffel_value, indices) - &
        cos(theta)/sin(theta), &
        "Gamma^phi_theta phi")

    gradient = grad(spherical_metric, radius)
    call assert_zero(gradient(1) - 1, "gradient of r, radial component")
    call assert_zero(gradient(2), "gradient of r, theta component")
    call assert_zero(gradient(3), "gradient of r, phi component")

    vector_value = num(arena, 0)
    vector_value(1) = radius
    call assert_zero(divergence(spherical_metric, vector_value) - 3, &
        "divergence of r e_r")

    ! This covector is r**2 sin(theta)**2 dphi. Its curl is a compact field
    ! with both radial and polar components, exercising the Jacobian factor
    ! without introducing an unevaluated branch choice.
    covector_value = num(arena, 0)
    covector_value(3) = radius**2*sin(theta)**2
    curl_value = curl(spherical, covector_value)
    call assert_zero(curl_value(1) - 2*cos(theta), &
        "spherical curl radial component")
    call assert_zero(curl_value(2) + 2*sin(theta)/radius, &
        "spherical curl theta component")
    call assert_zero(curl_value(3), "spherical curl phi component")

    scalar_value = radius**2
    laplacian_value = laplacian(spherical_metric, scalar_value)
    call assert_zero(laplacian_value - 6, "spherical Laplace-Beltrami of r**2")

    print '(a)', "spherical coordinates (r, theta, phi)"
    print '(a,a)', "  sqrt(g)^2 = ", chars(print_expr(expected_volume**2))
    print '(a,a)', "  J = sqrt(g) = ", chars(print_expr(expected_volume))
    print '(a)', "  g_ij = diag(1, r^2, r^2 sin(theta)^2)"
    print '(a)', "  grad(r) = (1, 0, 0); div(r e_r) = 3"
    print '(a)', "  curl(r^2 sin(theta)^2 dphi) = (2 cos(theta), -2 sin(theta)/r, 0)"
    print '(a)', "  Laplace-Beltrami(r^2) = 6"

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

end program example_spherical_coordinates
