program example_fourier_constitutive_density
    ! Physical Cartesian reluctivity -> Albert--Bíro--Lainer coordinate density.
    use fortsym
    use fortsym_string, only: chars
    use fortsym_print, only: print_expr
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: cylindrical
    type(expr_t) :: coordinates(DIM), position(DIM)
    type(expr_t) :: z, radius, phi, physical_scalar
    type(expr_t) :: physical_matrix(DIM, DIM), density_matrix(DIM, DIM)
    type(expr_t) :: volume
    type(fourier_constitutive_t) :: material
    type(engine_result_t) :: checked

    call reset()
    arena => default_arena()
    z = "Z"
    radius = "R"
    phi = "phi"
    coordinates = coords(z, radius, phi)
    position(1) = radius*cos(phi)
    position(2) = radius*sin(phi)
    position(3) = z
    cylindrical = make_chart(coordinates, position)
    volume = sqrtg(cylindrical)

    physical_scalar = "nu_phys"
    call reluctivity_density(cylindrical, physical_scalar, density_matrix)
    call assert_zero(density_matrix(1, 1) - physical_scalar/volume, &
        "isotropic axial density")
    call assert_zero(density_matrix(2, 2) - physical_scalar/volume, &
        "isotropic radial density")
    call assert_zero(density_matrix(3, 3) - &
        physical_scalar*radius**2/volume, "isotropic angular density")

    physical_matrix = num(arena, 0)
    physical_matrix(1, 1) = num(arena, 2)
    physical_matrix(2, 2) = num(arena, 2)
    physical_matrix(3, 3) = num(arena, 5)
    call reluctivity_density(cylindrical, physical_matrix, density_matrix)
    call assert_zero(density_matrix(1, 1) - num(arena, 5)/volume, &
        "Cartesian axial density")
    call assert_zero(density_matrix(2, 2) - &
        (num(arena, 2)*cos(phi)**2 + num(arena, 2)*sin(phi)**2)/volume, &
        "Cartesian radial density")
    call assert_zero(density_matrix(3, 3) - radius**2 * &
        (num(arena, 2)*sin(phi)**2 + num(arena, 2)*cos(phi)**2)/volume, &
        "Cartesian angular density")

    material = fourier_constitutive(cylindrical, density_matrix)
    if (.not. fourier_constitutive_valid(material)) error stop &
        "converted reluctivity was rejected by Fourier owner"
    checked = simplify(density_matrix(2, 2))
    if (.not. checked%ok) error stop "density display simplification failed"
    print '(a)', "physical Cartesian reluctivity -> Fourier density"
    print '(a,a)', "  nu_RR = ", chars(print_expr(checked%value))

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

end program example_fourier_constitutive_density
