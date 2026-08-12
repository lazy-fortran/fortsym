program test_fortsym_form
    ! Differential-form identities are the independent oracle for this module:
    ! d^2 = 0, the graded Leibniz rule, Cartan's identity, and the metric
    ! Hodge involution. The magnetic test also checks that i_B(volume)=dA.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create
    use fortsym_magnetic, only: b_con
    use fortsym_form, only: form_t, form, form_one, form_component, &
        wedge, d, star, interior, lie, flat, sharp, scale_form
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: shear
    type(expr_t) :: u(DIM), position(DIM), vector(DIM), potential(DIM)
    type(expr_t) :: scalar, expected, left, right
    type(expr_t) :: raised(DIM)
    type(form_t) :: scalar_form, one_form
    type(form_t) :: derivative, second_derivative, product, hodge, hodge_hodge
    type(form_t) :: volume, flux, lie_form, cartan_first, cartan_second
    integer :: mask, i

    call arena%init()
    engine = make_symengine_engine(arena)
    shear = make_shear_chart()
    call suite_begin(suite, "differential forms")

    u(1) = sym(arena, "u1")
    u(2) = sym(arena, "u2")
    u(3) = sym(arena, "u3")
    scalar = u(1)*u(2) + u(3)**2
    scalar_form = form(scalar)
    derivative = d(shear, scalar_form)
    call check_identity(suite, engine, "d scalar, component 1", &
        form_component(derivative, 1) - u(2))
    call check_identity(suite, engine, "d scalar, component 2", &
        form_component(derivative, 2) - u(1))
    call check_identity(suite, engine, "d scalar, component 3", &
        form_component(derivative, 4) - 2*u(3))

    second_derivative = d(shear, derivative)
    do mask = 3, 6
        if (mask == 4) cycle
        call check_identity(suite, engine, "d(d scalar) component", &
            form_component(second_derivative, mask))
    end do

    vector(1) = u(1)
    vector(2) = u(2)
    vector(3) = u(3)
    one_form = form_one(shear, vector)
    product = wedge(one_form, scalar_form)
    do mask = 3, 6
        if (mask == 4) cycle
        left = form_component(d(shear, product), mask)
        right = form_component(scale_form(d(shear, one_form), scalar), mask) - &
            form_component(wedge(one_form, d(shear, scalar_form)), mask)
        call check_identity(suite, engine, "graded Leibniz rule", left - right)
    end do

    product = wedge(one_form, one_form)
    do mask = 3, 6
        if (mask == 4) cycle
        call check_identity(suite, engine, "one-form wedge itself vanishes", &
            form_component(product, mask))
    end do

    hodge = star(shear, one_form)
    hodge_hodge = star(shear, hodge)
    do i = 1, DIM
        mask = 2**(i - 1)
        call check_identity(suite, engine, "Hodge star squared on one-form", &
            form_component(hodge_hodge, mask) - form_component(one_form, mask))
    end do

    vector(1) = u(1)
    vector(2) = u(2)
    vector(3) = u(3)
    one_form = flat(shear, vector)
    raised = sharp(shear, one_form)
    do i = 1, DIM
        call check_identity(suite, engine, "sharp(flat(v)) returns v", &
            raised(i) - vector(i))
    end do

    potential(1) = u(2)*u(3)
    potential(2) = u(1)**2
    potential(3) = u(2) + u(3)**2
    one_form = form_one(shear, potential)
    vector = b_con(shear, potential)
    volume = star(shear, form(num(arena, 1)))
    flux = interior(shear, vector, volume)
    derivative = d(shear, one_form)
    do mask = 3, 6
        if (mask == 4) cycle
        call check_identity(suite, engine, "magnetic flux two-form equals dA", &
            form_component(flux, mask) - form_component(derivative, mask))
    end do
    second_derivative = d(shear, flux)
    call check_identity(suite, engine, "magnetic flux is closed", &
        form_component(second_derivative, 7))

    lie_form = lie(shear, vector, one_form)
    cartan_first = interior(shear, vector, d(shear, one_form))
    cartan_second = d(shear, interior(shear, vector, one_form))
    do i = 1, DIM
        mask = 2**(i - 1)
        expected = form_component(lie_form, mask)
        left = form_component(cartan_first, mask) + &
            form_component(cartan_second, mask)
        call check_identity(suite, engine, "Cartan identity", expected - left)
    end do

    if (suite%failed /= 0) then
        print *, "test_fortsym_form: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_form.json")
    print *, "test_fortsym_form: all checks passed"

contains

    function make_shear_chart() result(c)
        type(chart_t) :: c

        u(1) = sym(arena, "u1")
        u(2) = sym(arena, "u2")
        u(3) = sym(arena, "u3")
        position(1) = u(1) + u(2)
        position(2) = u(2)
        position(3) = u(3)
        c = chart_create(arena, u, position)
    end function make_shear_chart

end program test_fortsym_form
