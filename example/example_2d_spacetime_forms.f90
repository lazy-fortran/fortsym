program example_2d_spacetime_forms
    ! The dimension-aware spacetime form owner also serves lower-dimensional
    ! metrics.  This is a two-dimensional Riemannian de Rham calculation.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(spacetime_metric_t) :: metric
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: one_form_values(SPACETIME_DIM), scalar, laplace_value
    type(spacetime_form_t) :: alpha, field, scalar_form, laplace
    type(engine_result_t) :: checked
    integer :: signature(SPACETIME_DIM)

    call reset()
    arena => default_arena()
    coordinates(1) = "t"
    coordinates(2) = "x"
    coordinates(3) = "unused_y"
    coordinates(4) = "unused_z"
    components = num(arena, 0)
    components(1, 1) = num(arena, 1)
    components(2, 2) = num(arena, 1)
    signature = 1
    metric = spacetime_metric_create(components, 2, coordinates, signature, 1)
    if (.not. spacetime_metric_valid(metric)) error stop "invalid 2D metric"

    one_form_values = num(arena, 0)
    one_form_values(1) = coordinates(2)
    one_form_values(2) = coordinates(1)**2
    alpha = spacetime_form_one(metric, one_form_values)
    field = spacetime_d(metric, alpha)
    call assert_zero(spacetime_form_component(field, 3) - &
        (2*coordinates(1) - 1), "dA")
    call assert_zero(spacetime_form_component(spacetime_d(metric, field), 0), &
        "d(dA)")

    scalar = coordinates(1)**2
    scalar_form = spacetime_form_scalar(metric, scalar)
    laplace = spacetime_laplace_de_rham(metric, scalar_form)
    checked = simplify(spacetime_form_component(laplace, 0))
    if (.not. checked%ok) error stop "failed to simplify Laplace-de Rham result"
    laplace_value = checked%value
    call assert_zero(laplace_value + 2, &
        "Laplace-de Rham scalar")

    print '(a)', "two-dimensional spacetime differential forms"
    print '(a,a)', "  dA_tx = ", chars(print_expr(spacetime_form_component(field, 3)))
    print '(a)', "  d(dA) = 0"
    print '(a,a)', "  Delta_dR(t^2) = ", &
        chars(print_expr(laplace_value))

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

end program example_2d_spacetime_forms
