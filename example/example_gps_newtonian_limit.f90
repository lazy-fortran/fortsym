program example_gps_newtonian_limit
    ! Weak, static field limit used by the GPS clock/orbit model.
    ! With c=1 and g_00=-(1+2 Phi), Gamma^r_tt = d_r Phi and the slow
    ! geodesic equation becomes r'' = -d_r Phi.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(spacetime_metric_t) :: metric
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: potential, mu, radius, speed
    type(expr_t) :: christoffel_value(SPACETIME_DIM, SPACETIME_DIM, &
        SPACETIME_DIM)
    type(expr_t) :: radial_acceleration, clock_correction
    type(engine_result_t) :: derivative, checked
    integer :: signature(SPACETIME_DIM)

    call reset()
    arena => default_arena()
    coordinates(1) = "t"
    coordinates(2) = "r"
    coordinates(3) = "theta"
    coordinates(4) = "phi"
    radius = coordinates(2)
    mu = "mu"
    speed = "v"
    potential = -mu/radius
    components = num(arena, 0)
    components(1, 1) = -(1 + 2*potential)
    components(2, 2) = num(arena, 1)
    components(3, 3) = num(arena, 1)
    components(4, 4) = num(arena, 1)
    signature = 1
    signature(1) = -1
    metric = spacetime_metric_create(components, SPACETIME_DIM, coordinates, &
        signature, 1)
    if (.not. spacetime_metric_valid(metric)) error stop "invalid weak-field metric"

    christoffel_value = spacetime_christoffel(metric)
    derivative = diff(potential, radius)
    if (.not. derivative%ok) error stop "potential derivative failed"
    call assert_zero(christoffel_value(2, 1, 1) - derivative%value, &
        "radial Christoffel is d_r Phi")
    radial_acceleration = -derivative%value
    clock_correction = potential - speed**2/2

    print '(a)', "GPS weak-field/Newtonian limit (c=1; restore c in SI units)"
    print '(a,a)', "  Phi(r) = ", chars(print_expr(potential))
    print '(a,a)', "  r'' = ", chars(print_expr(radial_acceleration))
    print '(a,a)', "  d tau/d t - 1 = ", chars(print_expr(clock_correction))
    print '(a)', "  first term: gravitational shift; second: second-order Doppler"

contains

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = zero_test(expression)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            error stop 1
        end if
    end subroutine assert_zero

end program example_gps_newtonian_limit
