program example_cylindrical_fourier
    ! Albert--Bíro--Lainer cylindrical ordering: (Z, R, phi).
    ! The symmetry direction is represented by exp(i*n*phi), so the native
    ! density kernel returns sqrt(g) B^i without introducing a branch choice
    ! for sqrt(R**2). This is the natural finite-element representation.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: cylindrical
    type(expr_t) :: coordinates(DIM), position(DIM), potential(DIM)
    type(expr_t) :: z, radius, phi, mode, density_value(DIM)
    type(expr_t) :: expected(DIM)
    type(engine_result_t) :: checked, displayed

    call reset()
    arena => default_arena()
    z = "Z"
    radius = "R"
    phi = "phi"
    mode = "n"
    coordinates(1) = z
    coordinates(2) = radius
    coordinates(3) = phi
    position(1) = radius*cos(phi)
    position(2) = radius*sin(phi)
    position(3) = z
    cylindrical = chart_create(arena, coordinates, position)

    ! A = A_1 dZ + A_2 dR, with A_1 = Z R and A_2 = R**2.
    potential = num(arena, 0)
    potential(1) = z*radius
    potential(2) = radius**2
    density_value = b_fourier_density(cylindrical, potential, mode)
    expected(1) = -i_expr(arena)*mode*radius**2
    expected(2) = i_expr(arena)*mode*z*radius
    expected(3) = -z
    call assert_zero(density_value(1) - expected(1), &
        "sqrtg B^Z = -i n A_R")
    call assert_zero(density_value(2) - expected(2), &
        "sqrtg B^R = i n A_Z")
    call assert_zero(density_value(3) - expected(3), &
        "sqrtg B^phi = d_Z A_R - d_R A_Z")

    print '(a)', "cylindrical Fourier magnetic density (Z, R, phi)"
    print '(a)', "  A = Z R dZ + R**2 dR"
    print '(a,a)', "  sqrt(g) = |R|, signed J = R; mode n = ", &
        chars(print_expr(mode))
    displayed = simplify(density_value(1))
    if (.not. displayed%ok) error stop "display simplification failed"
    print '(a,a)', "  sqrt(g) B^Z = ", chars(print_expr(displayed%value))
    displayed = simplify(density_value(2))
    if (.not. displayed%ok) error stop "display simplification failed"
    print '(a,a)', "  sqrt(g) B^R = ", chars(print_expr(displayed%value))
    displayed = simplify(density_value(3))
    if (.not. displayed%ok) error stop "display simplification failed"
    print '(a,a)', "  sqrt(g) B^phi = ", chars(print_expr(displayed%value))

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

end program example_cylindrical_fourier
