program test_fortsym_relativity
    ! Spherical Minkowski coordinates are a flat Lorentzian metric with
    ! nonzero Christoffel symbols. This catches dimension, signature, and
    ! curvature-index errors simultaneously.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, sin, cos, exp, &
        expr_abs => abs, operator(*), &
        operator(/), operator(+), operator(-), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_det, spacetime_metric_sqrtg, &
        spacetime_metric_contravariant, spacetime_christoffel, &
        spacetime_ricci, spacetime_scalar_curvature, spacetime_einstein, &
        spacetime_geodesic_residual, spacetime_metric_flat, &
        spacetime_metric_sharp, spacetime_metric_grad, &
        spacetime_metric_divergence, spacetime_metric_laplacian
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(spacetime_metric_t) :: metric
    type(spacetime_metric_t) :: metric_2d
    type(spacetime_metric_t) :: metric_cartesian
    type(expr_t) :: u(SPACETIME_DIM), components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: inverse(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: gamma(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: ricci(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: einstein(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: scalar, determinant, volume
    type(expr_t) :: components_2d(SPACETIME_DIM, SPACETIME_DIM), scalar_2d
    type(expr_t) :: parameter, curve(SPACETIME_DIM), residual(SPACETIME_DIM)
    type(expr_t) :: components_cartesian(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: vector(SPACETIME_DIM), flat_vector(SPACETIME_DIM)
    type(expr_t) :: sharp_vector(SPACETIME_DIM), gradient(SPACETIME_DIM)
    type(expr_t) :: divergence_value, laplacian_value
    integer :: signature(SPACETIME_DIM), i, j

    call arena%init()
    engine = make_symengine_engine(arena)
    u(1) = sym(arena, "t")
    u(2) = sym(arena, "r")
    u(3) = sym(arena, "theta")
    u(4) = sym(arena, "phi")
    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = num(arena, 1)
    components(3, 3) = u(2)**2
    components(4, 4) = u(2)**2*sin(u(3))**2
    signature = [-1, 1, 1, 1]
    metric = spacetime_metric_create(components, SPACETIME_DIM, u, &
        signature, 1)
    call suite_begin(suite, "spherical Minkowski relativity")

    parameter = sym(arena, "lambda")
    curve = num(arena, 0)
    curve(1) = parameter
    curve(2) = num(arena, 1)
    curve(3) = parameter
    residual = spacetime_geodesic_residual(metric, curve, parameter)
    call check_identity(suite, engine, "spherical geodesic radial residual", &
        residual(2) + 1)
    call check_identity(suite, engine, "spherical geodesic time residual", &
        residual(1))

    determinant = spacetime_metric_det(metric)
    volume = spacetime_metric_sqrtg(metric)
    call check_identity(suite, engine, "spherical metric determinant", &
        determinant + u(2)**4*sin(u(3))**2)
    call check_identity(suite, engine, "spherical metric volume square", &
        volume**2 - expr_abs(determinant))

    inverse = spacetime_metric_contravariant(metric)
    call check_identity(suite, engine, "g^tt", inverse(1, 1) + 1)
    call check_identity(suite, engine, "g^rr", inverse(2, 2) - 1)
    call check_identity(suite, engine, "g^theta theta", &
        inverse(3, 3) - 1/u(2)**2)
    call check_identity(suite, engine, "g^phi phi", &
        inverse(4, 4) - 1/(u(2)**2*sin(u(3))**2))

    gamma = spacetime_christoffel(metric)
    call check_identity(suite, engine, "Gamma r theta theta", &
        gamma(2, 3, 3) + u(2))
    call check_identity(suite, engine, "Gamma theta r theta", &
        gamma(3, 2, 3) - 1/u(2))
    call check_identity(suite, engine, "Gamma phi theta phi", &
        gamma(4, 3, 4) - cos(u(3))/sin(u(3)))

    ricci = spacetime_ricci(metric)
    do i = 1, SPACETIME_DIM
        do j = 1, SPACETIME_DIM
            call check_identity(suite, engine, "flat Ricci component", ricci(i, j))
        end do
    end do
    scalar = spacetime_scalar_curvature(metric)
    call check_identity(suite, engine, "flat scalar curvature", scalar)
    einstein = spacetime_einstein(metric)
    do i = 1, SPACETIME_DIM
        do j = 1, SPACETIME_DIM
            call check_identity(suite, engine, "flat Einstein component", einstein(i, j))
        end do
    end do

    components_cartesian = num(arena, 0)
    components_cartesian(1, 1) = num(arena, -1)
    components_cartesian(2, 2) = num(arena, 1)
    components_cartesian(3, 3) = num(arena, 1)
    components_cartesian(4, 4) = num(arena, 1)
    metric_cartesian = spacetime_metric_create(components_cartesian, &
        SPACETIME_DIM, u, signature, 1)
    vector = u
    flat_vector = spacetime_metric_flat(metric_cartesian, vector)
    call check_identity(suite, engine, "Minkowski flat time component", &
        flat_vector(1) + u(1))
    call check_identity(suite, engine, "Minkowski flat space component", &
        flat_vector(2) - u(2))
    sharp_vector = spacetime_metric_sharp(metric_cartesian, flat_vector)
    do i = 1, SPACETIME_DIM
        call check_identity(suite, engine, "Minkowski sharp-flat round trip", &
            sharp_vector(i) - vector(i))
    end do
    scalar = u(1) + u(2) + u(3) + u(4)
    gradient = spacetime_metric_grad(metric_cartesian, scalar)
    call check_identity(suite, engine, "Minkowski wave gradient time", &
        gradient(1) + 1)
    call check_identity(suite, engine, "Minkowski wave gradient space", &
        gradient(2) - 1)
    divergence_value = spacetime_metric_divergence(metric_cartesian, vector)
    call check_identity(suite, engine, "Minkowski vector divergence", &
        divergence_value - 4)
    laplacian_value = spacetime_metric_laplacian(metric_cartesian, scalar)
    call check_identity(suite, engine, "Minkowski wave operator on linear scalar", &
        laplacian_value)

    components_2d = num(arena, 0)
    components_2d(1, 1) = num(arena, 1)
    components_2d(2, 2) = exp(2*u(1))
    metric_2d = spacetime_metric_create(components_2d, 2, u, &
        [1, 1, 1, 1], 1)
    scalar_2d = spacetime_scalar_curvature(metric_2d)
    call check_identity(suite, engine, "curved two-dimensional scalar curvature", &
        scalar_2d + 2)
    inverse = spacetime_metric_contravariant(metric_2d)
    call check_identity(suite, engine, "unused two-dimensional inverse slot", &
        inverse(3, 3))

    if (suite%failed /= 0) then
        print *, "test_fortsym_relativity: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_relativity.json")
    print *, "test_fortsym_relativity: all checks passed"
end program test_fortsym_relativity
