program example_magnetic_flux_coordinates
    ! Nested toroidal flux surfaces labelled by psi. The potential A=psi dphi
    ! gives a field tangent to every psi=constant surface and demonstrates the
    ! native B^i, flux-form, volume, and divergence owners together.
    use fortsym
    use fortsym_string, only: chars
    use fortsym_print, only: print_expr
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: flux_chart
    type(expr_t) :: coordinates(DIM), position(DIM), potential(DIM)
    type(expr_t) :: psi, theta, phi, major_radius, minor_radius
    type(expr_t) :: radial, b(DIM), volume_density, expected
    type(form_t) :: potential_form, flux_form, closed_form
    type(engine_result_t) :: checked
    integer :: mask

    call reset()
    arena => default_arena()
    psi = "psi"
    theta = "theta"
    phi = "phi"
    major_radius = "R0"
    minor_radius = "a"
    radial = sqrt(psi)
    coordinates(1) = psi
    coordinates(2) = theta
    coordinates(3) = phi
    position(1) = (major_radius + minor_radius*radial*cos(theta))*cos(phi)
    position(2) = (major_radius + minor_radius*radial*cos(theta))*sin(phi)
    position(3) = minor_radius*radial*sin(theta)
    flux_chart = chart_create(arena, coordinates, position)

    potential = num(arena, 0)
    potential(3) = psi
    b = b_con(flux_chart, potential)
    volume_density = jacobian(flux_chart)
    expected = -minor_radius**2*(major_radius + minor_radius*radial*cos(theta))/2
    call assert_zero(volume_density - expected, "toroidal flux-coordinate Jacobian")
    call assert_zero(b(1), "B^psi = 0: field stays on flux surfaces")
    call assert_zero(b(2)*volume_density + 1, &
        "A=psi dphi gives B^theta=-1/J")
    call assert_zero(divergence(flux_chart, b), "div B = 0")

    potential_form = form_one(flux_chart, potential)
    flux_form = d(flux_chart, potential_form)
    closed_form = d(flux_chart, flux_form)
    do mask = 0, 2**DIM - 1
        if (mask == 3 .or. mask == 5 .or. mask == 6 .or. mask == 7) then
            call assert_zero(form_component(closed_form, mask), &
                "d flux form = 0")
        end if
    end do

    print '(a)', "magnetic flux coordinates"
    print '(a)', "  surfaces: psi = constant, B^psi = 0"
    print '(a,a)', "  signed J = ", chars(print_expr(expected))
    print '(a)', "  A = psi dphi,  i_B(volume) = dA,  d(dA) = 0"

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

end program example_magnetic_flux_coordinates
