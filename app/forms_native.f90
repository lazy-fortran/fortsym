program forms_native
    ! Short native example: a covector potential, its magnetic two-form, and
    ! the coordinate identity i_B(volume) = dA.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: chart
    type(expr_t) :: u(DIM), position(DIM), potential(DIM), vector(DIM)
    type(expr_t) :: args(2)
    type(form_t) :: potential_form, magnetic_form, volume, flux_form
    type(engine_result_t) :: checked
    integer :: mask

    call reset()
    arena => default_arena()
    u(1) = "Z"
    u(2) = "R"
    u(3) = "phi"
    position(1) = u(1) + u(2)
    position(2) = u(2)
    position(3) = u(3)
    chart = chart_create(arena, u, position)

    args(1) = u(1)
    args(2) = u(2)
    potential(1) = func("A1", args)
    potential(2) = func("A2", args)
    potential(3) = num(arena, 0)
    potential_form = form_one(chart, potential)
    magnetic_form = d(chart, potential_form)
    vector = b_con(chart, potential)
    volume = star(chart, form(num(arena, 1)))
    flux_form = interior(chart, vector, volume)

    do mask = 3, 6
        if (mask == 4) cycle
        checked = zero_test(form_component(flux_form, mask) - &
            form_component(magnetic_form, mask))
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            error stop "i_B(volume) /= dA"
        end if
    end do
    checked = zero_test(form_component(d(chart, flux_form), 7))
    if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
        error stop "magnetic flux form is not closed"
    end if

    print '(a)', "native differential-form magnetic derivation"
    print '(a,a)', "dA_12 = ", chars(print_expr(form_component(magnetic_form, 3)))
    print '(a,a)', "dA_13 = ", chars(print_expr(form_component(magnetic_form, 5)))
    print '(a,a)', "dA_23 = ", chars(print_expr(form_component(magnetic_form, 6)))
    print '(a)', "checked i_B(volume) = dA and d(dA) = 0"

end program forms_native
