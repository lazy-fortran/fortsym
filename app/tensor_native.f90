program tensor_native
    ! Short native example: explicit slot variance, metric raise/lower, and
    ! an upper/lower contraction on a nonorthogonal coordinate chart.
    use fortsym
    implicit none

    type(arena_t), pointer :: arena
    type(chart_t) :: chart
    type(expr_t) :: u(DIM), position(DIM), values(DIM), expected
    type(tensor_t) :: vup, vdown, roundtrip, outer, dot
    type(engine_result_t) :: checked
    integer :: indices(1), empty(0), i

    call reset()
    arena => default_arena()
    u(1) = "u1"
    u(2) = "u2"
    u(3) = "u3"
    position(1) = u(1) + u(2)
    position(2) = u(2)
    position(3) = u(3)
    chart = chart_create(arena, u, position)

    values(1) = u(1)
    values(2) = u(2)
    values(3) = u(3)
    vup = vector(chart, values)
    vdown = lower(chart, vup, 1)
    roundtrip = raise(chart, vdown, 1)
    do i = 1, DIM
        indices(1) = i
        checked = zero_test(tensor_component(roundtrip, indices) - values(i))
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            error stop "raise(lower(v)) /= v"
        end if
    end do

    outer = tensor_product(vup, vdown)
    dot = contract(outer, 1, 2)
    indices(1) = 1
    expected = values(1)*tensor_component(vdown, indices)
    indices(1) = 2
    expected = expected + values(2)*tensor_component(vdown, indices)
    indices(1) = 3
    expected = expected + values(3)*tensor_component(vdown, indices)
    checked = zero_test(tensor_component(dot, empty) - expected)
    if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
        error stop "upper/lower contraction failed"
    end if

    print '(a)', "native typed tensor derivation"
    print '(a,i0)', "vector rank = ", tensor_rank(vup)
    print '(a,i0)', "vector variance = ", tensor_variance(vup, 1)
    print '(a,i0)', "lowered density weight = ", tensor_density_weight(vdown)
    print '(a)', "checked raise(lower(v)) = v and upper/lower contraction"

end program tensor_native
