program test_fortsym_metric
    ! Independent metric-owner checks: inverse components multiply to the
    ! identity, sqrtg is positive for a Lorentzian determinant, and signature
    ! and orientation survive as separate metadata.
    use fortsym_arena, only: arena_t
    use fortsym, only: grad, divergence, laplacian
    use fortsym_expr, only: expr_t, num, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_metric, only: metric_t, metric_create, metric_from_chart, &
        metric_covariant, metric_contravariant, metric_det, metric_sqrtg, &
        metric_signature, metric_orientation, metric_valid, metric_grad, &
        metric_divergence, metric_laplacian, metric_inner
    use fortsym_volume, only: metric_volume_density, levi_civita_symbol, &
        metric_levi_civita
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: cartesian
    type(metric_t) :: euclidean, shear_metric, lorentzian, invalid, degenerate
    type(expr_t) :: components(DIM, DIM), covariant(DIM, DIM)
    type(expr_t) :: inverse(DIM, DIM), product, determinant, root
    type(expr_t) :: scalar, gradient(DIM), vector(DIM), divergence_value
    type(expr_t) :: laplacian_value, facade_gradient(DIM), facade_laplacian
    type(expr_t) :: inner_left(DIM), inner_right(DIM), inner_value
    type(expr_t) :: epsilon_lower(DIM, DIM, DIM), epsilon_upper(DIM, DIM, DIM)
    integer :: signature(DIM), returned_signature(DIM)
    integer :: i, j, k

    call arena%init()
    engine = make_symengine_engine(arena)
    cartesian = make_cartesian()
    call suite_begin(suite, "explicit metric owner")

    euclidean = metric_from_chart(cartesian)
    if (.not. metric_valid(euclidean)) error stop "chart metric is invalid"
    returned_signature = metric_signature(euclidean)
    if (any(returned_signature /= 1)) error stop "Euclidean signature failed"
    if (metric_orientation(euclidean) /= 1) error stop "default orientation failed"
    call check_identity(suite, engine, "chart metric has unit positive sqrtg", &
        metric_sqrtg(euclidean) - 1)
    call check_identity(suite, engine, "metric volume density is positive", &
        metric_volume_density(euclidean) - 1)
    if (levi_civita_symbol(1, 2, 3) /= 1 .or. &
        levi_civita_symbol(1, 3, 2) /= -1 .or. &
        levi_civita_symbol(1, 1, 2) /= 0) then
        error stop "Levi-Civita symbol convention failed"
    end if
    epsilon_lower = metric_levi_civita(euclidean, -1)
    epsilon_upper = metric_levi_civita(euclidean, 1)
    call check_identity(suite, engine, "covariant Levi-Civita tensor", &
        epsilon_lower(1, 2, 3) - 1)
    call check_identity(suite, engine, "contravariant Levi-Civita tensor", &
        epsilon_upper(1, 3, 2) + 1)

    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = num(arena, 1)
    components(3, 3) = num(arena, 1)
    signature = [-1, 1, 1]
    lorentzian = metric_create(components, signature, -1, cartesian%u)
    if (.not. metric_valid(lorentzian)) error stop "Lorentzian metric is invalid"
    returned_signature = metric_signature(lorentzian)
    if (any(returned_signature /= signature)) error stop "signature metadata failed"
    if (metric_orientation(lorentzian) /= -1) error stop "orientation metadata failed"

    determinant = metric_det(lorentzian)
    root = metric_sqrtg(lorentzian)
    call check_identity(suite, engine, "Lorentzian determinant", determinant + 1)
    call check_identity(suite, engine, "sqrtg uses absolute determinant", root - 1)
    call check_identity(suite, engine, "Lorentzian volume density is positive", &
        metric_volume_density(lorentzian) - 1)
    epsilon_lower = metric_levi_civita(lorentzian, -1)
    epsilon_upper = metric_levi_civita(lorentzian, 1)
    call check_identity(suite, engine, "oriented covariant Levi-Civita tensor", &
        epsilon_lower(1, 2, 3) + 1)
    call check_identity(suite, engine, "raised Levi-Civita tensor signature", &
        epsilon_upper(1, 2, 3) - 1)

    scalar = cartesian%u(1) + cartesian%u(2) + cartesian%u(3)
    gradient = metric_grad(euclidean, scalar)
    call check_identity(suite, engine, "Euclidean metric gradient x", &
        gradient(1) - 1)
    call check_identity(suite, engine, "Euclidean metric gradient y", &
        gradient(2) - 1)
    call check_identity(suite, engine, "Euclidean metric gradient z", &
        gradient(3) - 1)
    vector = cartesian%u
    divergence_value = metric_divergence(euclidean, vector)
    call check_identity(suite, engine, "Euclidean metric divergence", &
        divergence_value - 3)
    laplacian_value = metric_laplacian(euclidean, scalar)
    call check_identity(suite, engine, "Euclidean metric Laplace-Beltrami", &
        laplacian_value)
    facade_gradient = grad(euclidean, scalar)
    call check_identity(suite, engine, "facade metric gradient", &
        facade_gradient(1) - gradient(1))
    facade_laplacian = laplacian(euclidean, scalar)
    call check_identity(suite, engine, "facade metric Laplace-Beltrami", &
        facade_laplacian - laplacian_value)
    facade_laplacian = divergence(euclidean, gradient)
    call check_identity(suite, engine, "facade metric divergence of gradient", &
        facade_laplacian - laplacian_value)
    facade_laplacian = divergence(euclidean, vector)
    call check_identity(suite, engine, "facade metric divergence", &
        facade_laplacian - divergence_value)

    components = num(arena, 0)
    components(1, 1) = num(arena, 2)
    components(1, 2) = num(arena, 1)
    components(2, 1) = num(arena, 1)
    components(2, 2) = num(arena, 3)
    components(3, 3) = num(arena, 4)
    shear_metric = metric_create(components)
    inner_left = cartesian%u
    inner_right = num(arena, 0)
    inner_right(1) = num(arena, 1)
    inner_right(2) = num(arena, 2)
    inner_right(3) = num(arena, 3)
    inner_value = metric_inner(shear_metric, inner_left, inner_right)
    call check_identity(suite, engine, "nonorthogonal metric inner product", &
        inner_value - (4*cartesian%u(1) + 7*cartesian%u(2) + &
        12*cartesian%u(3)))

    gradient = metric_grad(lorentzian, cartesian%u(1))
    call check_identity(suite, engine, "Lorentzian metric gradient sign", &
        gradient(1) + 1)

    covariant = metric_covariant(lorentzian)
    inverse = metric_contravariant(lorentzian)
    do i = 1, DIM
        do j = 1, DIM
            product = inverse(i, 1)*covariant(1, j)
            do k = 2, DIM
                product = product + inverse(i, k)*covariant(k, j)
            end do
            if (i == j) product = product - 1
            call check_identity(suite, engine, "metric inverse identity", product)
        end do
    end do

    signature(1) = 0
    invalid = metric_create(components, signature, -1)
    if (metric_valid(invalid)) error stop "invalid signature was accepted"

    components = num(arena, 0)
    degenerate = metric_create(components, [1, 1, 1], 1)
    if (metric_valid(degenerate)) error stop "degenerate metric was accepted"

    if (suite%failed /= 0) then
        print *, "test_fortsym_metric: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_metric.json")
    print *, "test_fortsym_metric: all checks passed"

contains

    function make_cartesian() result(c)
        type(chart_t) :: c
        type(expr_t) :: u(DIM), position(DIM)

        u(1) = sym(arena, "metric_x")
        u(2) = sym(arena, "metric_y")
        u(3) = sym(arena, "metric_z")
        position = u
        c = chart_create(arena, u, position)
    end function make_cartesian

end program test_fortsym_metric
