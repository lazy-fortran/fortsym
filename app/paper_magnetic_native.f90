program paper_magnetic_native
    ! Native translation of the first paper_magnetic Fourier derivation.
    !
    ! The example intentionally uses the short facade surface. The generic
    ! chart owns the metric and volume factor, while fortsym_magnetic owns the
    ! variance and Fourier-mode views. The Wolfram and Python frontends can
    ! therefore compare against the same native expression tree.
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: cylindrical
    type(expr_t) :: z, r, phi, mode
    type(expr_t) :: coordinates(DIM), position(DIM), args(2)
    type(expr_t) :: potential(DIM), b_up(DIM), b_down(DIM)
    type(expr_t) :: metric(DIM, DIM), volume, expected
    type(assumption_context_t) :: facts
    type(engine_result_t) :: checked, derivative_r, derivative_z
    logical :: context_ok
    integer :: i

    call reset()
    arena => default_arena()

    z = "Z"
    r = "R"
    phi = "phi"
    mode = "n"
    facts = make_assumption_context(arena)
    facts = with_assumption(facts, positive(r), context_ok)
    if (.not. context_ok) error stop "failed to record R > 0"
    coordinates(1) = z
    coordinates(2) = r
    coordinates(3) = phi
    position(1) = r*cos(phi)
    position(2) = r*sin(phi)
    position(3) = z
    cylindrical = chart_create(arena, coordinates, position)

    args(1) = z
    args(2) = r
    potential(1) = func("A1", args)
    potential(2) = func("A2", args)
    potential(3) = num(arena, 0)

    metric = metric_covariant(cylindrical)
    volume = sqrtg(cylindrical)
    b_up = b_fourier(cylindrical, potential, mode)
    b_down = b_cov(cylindrical, b_up)

    call assert_zero(volume**2 - r**2, "sqrtg^2 = R^2")
    call assert_zero(b_up(1) - (-i_expr(arena)*mode*potential(2)/volume), &
        "B^1 = -i n A_2 / sqrtg")
    call assert_zero(b_up(2) - (i_expr(arena)*mode*potential(1)/volume), &
        "B^2 = i n A_1 / sqrtg")
    derivative_z = diff(potential(2), z)
    derivative_r = diff(potential(1), r)
    if (.not. derivative_z%ok .or. .not. derivative_r%ok) then
        error stop "native derivative failed"
    end if
    expected = (derivative_z%value - derivative_r%value)/volume
    call assert_zero(b_up(3) - expected, &
        "B^3 = (partial_1 A_2 - partial_2 A_1) / sqrtg")
    call assert_zero(b_down(1) - metric(1, 1)*b_up(1) - &
        metric(1, 2)*b_up(2) - metric(1, 3)*b_up(3), "B_1 = g_1j B^j")
    call assert_zero(b_down(2) - metric(2, 1)*b_up(1) - &
        metric(2, 2)*b_up(2) - metric(2, 3)*b_up(3), "B_2 = g_2j B^j")
    call assert_zero(b_down(3) - metric(3, 1)*b_up(1) - &
        metric(3, 2)*b_up(2) - metric(3, 3)*b_up(3), "B_3 = g_3j B^j")

    print *, "paper_magnetic native derivation"
    call show("sqrtg", volume, facts)
    do i = 1, DIM
        call show("B^"//char(iachar("0") + i), b_up(i), facts)
        call show("B_"//char(iachar("0") + i), b_down(i), facts)
    end do

contains

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = zero_test(expression)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            if (checked%ok) then
                write (*, '(a)') "  verdict: "//chars(verdict_name(checked%verdict))
            else
                write (*, '(a)') "  error: "//chars(checked%message)
            end if
            error stop 1
        end if
    end subroutine assert_zero

    subroutine show(label, expression, assumptions)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), intent(in), target :: assumptions
        type(engine_result_t) :: result

        result = simplify(expression, assumptions=assumptions)
        if (result%ok) then
            print '(a,a)', trim(label)//" = ", chars(print_expr(result%value))
        else
            print '(a,a)', trim(label)//" = ", chars(print_expr(expression))
        end if
    end subroutine show

end program paper_magnetic_native
