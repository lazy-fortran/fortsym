program test_fortsym_easy_geometry
    ! The concise facade must construct the same native chart and metric as
    ! the explicit owners. The identities below are checked by SymEngine,
    ! independently of the constructor implementation.
    use fortsym
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    implicit none

    type(arena_t), target :: explicit_arena
    type(arena_t), pointer :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(expr_t) :: z, radius, phi, coordinates(DIM), position(DIM)
    type(expr_t) :: explicit_coordinates(DIM)
    type(expr_t) :: covariant(DIM, DIM), contravariant(DIM, DIM)
    type(chart_t) :: cylindrical, explicit_chart
    type(metric_t) :: chart_metric, explicit_metric, supplied_metric
    integer :: i, j

    call reset()
    arena => default_arena()
    engine = make_symengine_engine(arena)
    call suite_begin(suite, "concise geometry facade")

    z = "z"
    radius = "r"
    phi = "phi"
    coordinates = coords(radius, phi, z)
    position(1) = radius*cos(phi)
    position(2) = radius*sin(phi)
    position(3) = z
    cylindrical = make_chart(coordinates, position)
    chart_metric = make_metric(cylindrical)
    covariant = metric_covariant(chart_metric)
    contravariant = metric_contravariant(chart_metric)

    if (.not. chart_valid(cylindrical)) error stop "concise chart invalid"
    if (.not. metric_valid(chart_metric)) error stop "concise metric invalid"
    call check_identity(suite, engine, "cylindrical g_rr", covariant(1, 1) - 1)
    call check_identity(suite, engine, "cylindrical g_phiphi", &
        covariant(2, 2) - radius**2)
    call check_identity(suite, engine, "cylindrical g_zz", covariant(3, 3) - 1)
    call check_identity(suite, engine, "cylindrical inverse g^phiphi", &
        contravariant(2, 2) - 1/radius**2)
    call check_identity(suite, engine, "cylindrical signed Jacobian", &
        jacobian(cylindrical) - radius)
    call check_identity(suite, engine, "cylindrical sqrtg square", &
        sqrtg(cylindrical)**2 - radius**2)
    do i = 1, DIM
        do j = 1, DIM
            if (i /= j) then
                call check_identity(suite, engine, "cylindrical off-diagonal metric", &
                    covariant(i, j))
            end if
        end do
    end do

    ! The chart type exposes its source handles for low-level callers. A
    ! mutation therefore has to invalidate the cached geometric views rather
    ! than silently returning the old metric.
    cylindrical%x(1) = radius + z
    chart_metric = make_metric(cylindrical)
    covariant = metric_covariant(chart_metric)
    call check_identity(suite, engine, "edited chart invalidates metric cache", &
        covariant(1, 1) - (1 + sin(phi)**2))

    ! Explicit construction remains the same vocabulary with an explicit
    ! arena, and make_metric(array, ...) remains the supplied-metric owner.
    call explicit_arena%init()
    explicit_coordinates = coords(sym(explicit_arena, "r2"), &
        sym(explicit_arena, "phi2"), sym(explicit_arena, "z2"))
    explicit_chart = make_chart(explicit_arena, explicit_coordinates, &
        explicit_coordinates)
    explicit_metric = make_metric(explicit_chart)
    if (.not. chart_valid(explicit_chart)) error stop "explicit chart invalid"
    if (.not. metric_valid(explicit_metric)) error stop "explicit metric invalid"
    supplied_metric = make_metric(covariant, coordinates=coordinates)
    if (.not. metric_valid(supplied_metric)) then
        error stop "supplied metric constructor invalid"
    end if

    if (suite%failed /= 0) then
        print *, "test_fortsym_easy_geometry: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_easy_geometry.json")
    print *, "test_fortsym_easy_geometry: all checks passed"
end program test_fortsym_easy_geometry
