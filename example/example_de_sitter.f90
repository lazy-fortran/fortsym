program example_de_sitter
    ! de Sitter in flat slicing: ds^2 = -dt^2 + exp(2 H t) d x^i d x^i.
    ! The executable checks the vacuum Einstein equation with Lambda=3 H^2.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(spacetime_metric_t) :: metric
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: scale, h, einstein(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: covariant(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: scalar, cosmological_constant
    type(expr_t) :: vector_value(SPACETIME_DIM), covector_value(SPACETIME_DIM)
    type(expr_t) :: raised_value(SPACETIME_DIM), wave_time
    type(engine_result_t) :: checked, wave_checked
    integer :: signature(SPACETIME_DIM), i, j

    call reset()
    arena => default_arena()
    coordinates(1) = "t"
    coordinates(2) = "x"
    coordinates(3) = "y"
    coordinates(4) = "z"
    h = "H"
    scale = exp(h*coordinates(1))
    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = scale**2
    components(3, 3) = scale**2
    components(4, 4) = scale**2
    signature = 1
    signature(1) = -1
    metric = spacetime_metric_create(components, SPACETIME_DIM, coordinates, &
        signature, 1)
    if (.not. spacetime_metric_valid(metric)) error stop "invalid de Sitter metric"

    covariant = spacetime_metric_covariant(metric)
    einstein = spacetime_einstein(metric)
    cosmological_constant = 3*h**2
    do i = 1, SPACETIME_DIM
        do j = 1, SPACETIME_DIM
            call assert_zero(einstein(i, j) + cosmological_constant* &
                covariant(i, j), "Einstein equation")
        end do
    end do
    scalar = spacetime_scalar_curvature(metric)
    call assert_zero(scalar - 12*h**2, "de Sitter scalar curvature")

    vector_value = num(arena, 0)
    vector_value(1) = coordinates(1)
    covector_value = spacetime_metric_flat(metric, vector_value)
    raised_value = spacetime_metric_sharp(metric, covector_value)
    do i = 1, SPACETIME_DIM
        call assert_zero(raised_value(i) - vector_value(i), &
            "de Sitter sharp-flat round trip")
    end do
    wave_time = spacetime_metric_laplacian(metric, coordinates(1))
    wave_checked = simplify(wave_time + 3*h)
    if (.not. wave_checked%ok) error stop "de Sitter wave simplification failed"
    call assert_zero(wave_checked%value, "de Sitter wave operator on time")
    print '(a)', "de Sitter flat slicing"
    checked = simplify(scalar)
    if (.not. checked%ok) error stop "de Sitter scalar simplification failed"
    print '(a,a)', "  R = ", chars(print_expr(checked%value))
    print '(a,a)', "  Lambda = ", chars(print_expr(cosmological_constant))
    print '(a)', "  G_ab + Lambda g_ab = 0"
    print '(a)', "  sharp(flat(partial_t)) = partial_t"
    print '(a,a)', "  Box(t) + 3 H = ", chars(print_expr(wave_checked%value))

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

end program example_de_sitter
