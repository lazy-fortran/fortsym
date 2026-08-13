program example_fourier_weak_form
    ! Executable Albert--Bíro--Lainer strong-form derivation record.
    ! The native owner keeps the density convention and the two Fourier
    ! branches explicit; this example only supplies a readable fixture.
    use fortsym
    use fortsym_expr, only: expr_t, operator(+), operator(*)
    use fortsym_diff, only: diff_native => diff
    use fortsym_string, only: chars
    use fortsym_print, only: print_expr
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: chart
    type(expr_t) :: coordinates(DIM), position(DIM), nu(DIM, DIM)
    type(expr_t) :: z, radius, phi, mode, nu33
    type(expr_t) :: potential(DIM), current(DIM), transverse_potential(2)
    type(expr_t) :: transverse_current(2), residual, residual_pair(2)
    type(expr_t) :: longitudinal_flux_one, longitudinal_flux_two
    type(expr_t) :: longitudinal_boundary, normal(2)
    type(expr_t) :: transverse_flux, transverse_boundary(2), edge_test(2)
    type(expr_t) :: edge_contraction
    type(expr_t) :: full_current(DIM), full_potential(DIM), full_j(DIM)
    type(fourier_constitutive_t) :: material
    type(fourier_weak_form_t) :: longitudinal, transverse
    type(engine_result_t) :: checked

    call reset()
    arena => default_arena()
    z = "Z"
    radius = "R"
    phi = "phi"
    mode = "n"
    nu33 = "nu33"
    coordinates = coords(z, radius, phi)
    position = coordinates
    chart = make_chart(coordinates, position)

    nu = num(arena, 0)
    nu(1, 1) = num(arena, 2)
    nu(1, 2) = num(arena, 3)
    nu(2, 1) = num(arena, 5)
    nu(2, 2) = num(arena, 7)
    nu(3, 3) = nu33
    material = fourier_constitutive(chart, nu)
    if (.not. fourier_constitutive_valid(material)) error stop &
        "Fourier constitutive owner is invalid"

    longitudinal = fourier_weak_form(chart, material, 0)
    transverse = fourier_weak_form(chart, material, 2)
    if (.not. fourier_weak_form_valid(longitudinal)) error stop &
        "longitudinal Fourier form is invalid"
    if (.not. fourier_weak_form_valid(transverse)) error stop &
        "transverse Fourier form is invalid"

    potential = num(arena, 0)
    potential(3) = z**2 + z*radius
    current = num(arena, 0)
    current(3) = z - radius
    longitudinal_flux_one = fourier_longitudinal_flux(chart, material, &
        potential(3), 1)
    longitudinal_flux_two = fourier_longitudinal_flux(chart, material, &
        potential(3), 2)
    call assert_zero(longitudinal_flux_one - &
        (7*diff_native(potential(3), z) - &
        5*diff_native(potential(3), radius)), &
        "n=0 boundary flux component 1")
    call assert_zero(longitudinal_flux_two - &
        (-3*diff_native(potential(3), z) + &
        2*diff_native(potential(3), radius)), &
        "n=0 boundary flux component 2")
    normal(1) = num(arena, 2)
    normal(2) = num(arena, 3)
    longitudinal_boundary = fourier_longitudinal_boundary_flux( &
        chart, material, potential(3), normal)
    call assert_zero(longitudinal_boundary - (normal(1)* &
        longitudinal_flux_one + normal(2)*longitudinal_flux_two), &
        "n=0 normal boundary contraction")
    residual = fourier_longitudinal_residual(chart, material, potential(3), &
        current(3))
    call assert_zero(residual - (-divergence_block(material, potential(3)) - &
        current(3)), "n=0 longitudinal residual")

    transverse_potential(1) = z**2 + radius
    transverse_potential(2) = z*radius
    transverse_current(1) = z - radius
    transverse_current(2) = z + 2*radius
    transverse_flux = fourier_transverse_flux(chart, material, &
        transverse_potential)
    call assert_zero(transverse_flux - nu33*( &
        diff_native(transverse_potential(2), z) - &
        diff_native(transverse_potential(1), radius)), &
        "n/=0 boundary flux")
    call fourier_transverse_boundary_flux(chart, material, &
        transverse_potential, normal, transverse_boundary)
    call assert_zero(transverse_boundary(1) + normal(2)*transverse_flux, &
        "n/=0 edge boundary coefficient 1")
    call assert_zero(transverse_boundary(2) - normal(1)*transverse_flux, &
        "n/=0 edge boundary coefficient 2")
    edge_test(1) = z
    edge_test(2) = radius
    edge_contraction = fourier_transverse_boundary_contraction(chart, material, &
        transverse_potential, normal, edge_test)
    call assert_zero(edge_contraction - (edge_test(1)*transverse_boundary(1) + &
        edge_test(2)*transverse_boundary(2)), &
        "n/=0 edge boundary contraction")
    residual_pair = fourier_transverse_residual(chart, material, &
        transverse_potential, transverse_current, mode)
    full_potential = num(arena, 0)
    full_potential(1) = transverse_potential(1)
    full_potential(2) = transverse_potential(2)
    full_current = num(arena, 0)
    full_current(1) = transverse_current(1)
    full_current(2) = transverse_current(2)
    full_j = j_fourier(chart, nu, full_potential, mode)
    call assert_zero(residual_pair(1) - (full_j(1) - full_current(1)), &
        "symbolic transverse residual component 1")
    call assert_zero(residual_pair(2) - (full_j(2) - full_current(2)), &
        "symbolic transverse residual component 2")

    print '(a)', "Albert--Bíro--Lainer Fourier strong residuals"
    print '(a)', "  n=0: scalar nodal branch; diffusion block = nubar_t"
    print '(a)', "       boundary flux = nubar_t grad_t(A_3)"
    print '(a)', "  n/=0: transverse edge branch; mass = n**2*nubar_t"
    print '(a)', "       boundary flux = nu33 curl_t(a)"
    checked = simplify(residual_pair(1))
    if (.not. checked%ok) error stop "transverse residual display failed"
    print '(a,a)', "  residual_1(n) = ", chars(print_expr(checked%value))

contains

    function divergence_block(material, scalar) result(value)
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: scalar
        type(expr_t) :: value

        value = diff_native(material%nubar_t(1, 1)*diff_native(scalar, z) + &
            material%nubar_t(1, 2)*diff_native(scalar, radius), z) + &
            diff_native(material%nubar_t(2, 1)*diff_native(scalar, z) + &
            material%nubar_t(2, 2)*diff_native(scalar, radius), radius)
    end function divergence_block

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = zero_test(expression)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            error stop 1
        end if
    end subroutine assert_zero

end program example_fourier_weak_form
