program example_killing_vectors
    ! Killing residuals are Lie derivatives of the metric:
    !   K_ij[X] = (L_X g)_ij.
    ! Translation in z is an isometry of the displayed metrics, while the
    ! x-translation of g_yy = 1 + x**2 has K_yy = 2*x.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: chart
    type(metric_t) :: metric
    type(tensor_t) :: residual
    type(expr_t) :: coordinates(DIM), position(DIM), values(DIM)
    type(expr_t) :: components(DIM, DIM), x
    type(engine_result_t) :: checked
    integer :: i, j, indices(2)

    call reset()
    arena => default_arena()
    coordinates(1) = "x"
    coordinates(2) = "y"
    coordinates(3) = "z"
    position = coordinates
    chart = chart_create(arena, coordinates, position)

    values = num(arena, 0)
    values(3) = num(arena, 1)
    residual = killing(chart, tensor_vector(chart, values))
    if (.not. tensor_valid(residual)) error stop "chart Killing residual invalid"
    do i = 1, DIM
        do j = 1, DIM
            indices(1) = i
            indices(2) = j
            call assert_zero(tensor_component(residual, indices), &
                "z translation Killing residual")
        end do
    end do

    x = coordinates(1)
    components = num(arena, 0)
    components(1, 1) = num(arena, 1)
    components(2, 2) = num(arena, 1) + x**2
    components(3, 3) = num(arena, 1)
    metric = metric_create(components, coordinates=coordinates)
    values = num(arena, 0)
    values(1) = num(arena, 1)
    residual = killing(metric, tensor_vector(chart, values))
    indices(1) = 2
    indices(2) = 2
    call assert_zero(tensor_component(residual, indices) - 2*x, &
        "x translation Killing residual")

    print '(a)', "Killing-vector residuals"
    print '(a)', "  K_ij[partial_z] = 0"
    checked = simplify(tensor_component(residual, indices))
    if (.not. checked%ok) error stop "Killing residual simplification failed"
    print '(a,a)', "  K_yy[partial_x] = ", chars(print_expr(checked%value))

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

end program example_killing_vectors
