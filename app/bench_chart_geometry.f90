program bench_chart_geometry
    ! Repeated native chart-view workload. It intentionally calls the same
    ! immutable views that geometry consumers use, while keeping the result
    ! handles alive so the loop cannot be reduced to an empty computation.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym
    use fortsym_engine, only: wall_seconds
    implicit none

    integer, parameter :: repetitions = 2000
    integer :: k
    real(real64) :: started, elapsed
    type(arena_t), pointer :: arena
    type(expr_t) :: radius, phi, z, coordinates(DIM), position(DIM)
    type(expr_t) :: basis(DIM, DIM), inverse_basis(DIM, DIM)
    type(expr_t) :: metric_values(DIM, DIM), inverse_metric(DIM, DIM)
    type(expr_t) :: last_jacobian, last_sqrtg
    type(chart_t) :: chart_value

    call reset()
    arena => default_arena()
    radius = "bench_r"
    phi = "bench_phi"
    z = "bench_z"
    coordinates = coords(radius, phi, z)
    position(1) = radius*cos(phi)
    position(2) = radius*sin(phi)
    position(3) = z
    chart_value = make_chart(coordinates, position)

    started = wall_seconds()
    do k = 1, repetitions
        basis = covariant_basis(chart_value)
        inverse_basis = reciprocal_basis(chart_value)
        metric_values = metric_covariant(chart_value)
        inverse_metric = metric_contravariant(chart_value)
        last_jacobian = jacobian(chart_value)
        last_sqrtg = sqrtg(chart_value)
    end do
    elapsed = wall_seconds() - started

    if (.not. is_valid(basis(1, 1)) .or. .not. is_valid(inverse_basis(1, 1)) .or. &
        .not. is_valid(metric_values(1, 1)) .or. &
        .not. is_valid(inverse_metric(1, 1)) .or. &
        .not. is_valid(last_jacobian) .or. .not. is_valid(last_sqrtg)) then
        error stop "chart benchmark produced an invalid view"
    end if
    print '(a,i0)', "repetitions=", repetitions
    print '(a,es16.8)', "seconds=", elapsed
    print '(a,es16.8)', "microseconds_per_bundle=", &
        elapsed*1.0e6_real64/real(repetitions, real64)
end program bench_chart_geometry
