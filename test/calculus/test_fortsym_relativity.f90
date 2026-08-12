program test_fortsym_relativity
    ! Spherical Minkowski coordinates are a flat Lorentzian metric with
    ! nonzero Christoffel symbols. This catches dimension, signature, and
    ! curvature-index errors simultaneously.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, sin, cos, &
        expr_abs => abs, operator(*), &
        operator(/), operator(+), operator(-), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_det, spacetime_metric_sqrtg, &
        spacetime_metric_contravariant, spacetime_christoffel, &
        spacetime_ricci, spacetime_scalar_curvature, spacetime_einstein, &
        spacetime_geodesic_residual
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(spacetime_metric_t) :: metric
    type(expr_t) :: u(SPACETIME_DIM), components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: inverse(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: gamma(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: ricci(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: einstein(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: scalar, determinant, volume
    type(expr_t) :: parameter, curve(SPACETIME_DIM), residual(SPACETIME_DIM)
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

    if (suite%failed /= 0) then
        print *, "test_fortsym_relativity: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_relativity.json")
    print *, "test_fortsym_relativity: all checks passed"
end program test_fortsym_relativity
