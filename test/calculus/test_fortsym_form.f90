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
    use fortsym_metric, only: metric_t, metric_create, metric_from_chart
    use fortsym_magnetic, only: b_con
    use fortsym_form, only: form_t, form, form_one, form_zero, form_valid, &
        form_component, form_degree, wedge, d, star, interior, lie, flat, &
        sharp, scale_form, volume_form
    use fortsym_form_tensor, only: form_from_tensor, tensor_from_form
    use fortsym_tensor, only: tensor_t, tensor_from_matrix, tensor_component, &
        tensor_variance, tensor_valid, density, LOWER_VARIANCE
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: shear
    type(metric_t) :: metric_owner
    type(metric_t) :: lorentz_metric
    type(expr_t) :: u(DIM), position(DIM), vector(DIM), potential(DIM)
    type(expr_t) :: scalar, expected, left, right
    type(expr_t) :: lorentz_components(DIM, DIM)
    type(expr_t) :: raised(DIM)
    type(form_t) :: scalar_form, one_form
    type(form_t) :: derivative, second_derivative, product, hodge, hodge_hodge
    type(form_t) :: metric_hodge, metric_hodge_hodge
    type(form_t) :: volume, reversed_volume, flux, lie_form
    type(form_t) :: cartan_first, cartan_second
    type(form_t) :: top_zero, top_from_d
    type(form_t) :: two_form, round_form
    type(tensor_t) :: form_tensor, density_tensor, nonsymmetric
    type(expr_t) :: tensor_matrix(DIM, DIM)
    integer :: pair(2)
    integer :: mask, i, lorentz_signature(DIM)

    call arena%init()
    engine = make_symengine_engine(arena)
    shear = make_shear_chart()
    metric_owner = metric_from_chart(shear, orientation=-1)
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
    two_form = wedge(one_form, derivative)

    ! Forms and tensors share one conversion owner. A two-form becomes a
    ! fully antisymmetric lower tensor, including the sign of reversed slots,
    ! and converts back without a second component representation.
    form_tensor = tensor_from_form(shear, two_form)
    round_form = form_from_tensor(form_tensor)
    if (.not. tensor_valid(form_tensor) .or. .not. form_valid(round_form)) then
        suite%total = suite%total + 1
        suite%failed = suite%failed + 1
        print *, "FAIL  form/tensor conversion validity"
    end if
    if (tensor_variance(form_tensor, 1) /= LOWER_VARIANCE .or. &
        tensor_variance(form_tensor, 2) /= LOWER_VARIANCE) then
        suite%total = suite%total + 1
        suite%failed = suite%failed + 1
        print *, "FAIL  form-to-tensor variance"
    end if
    do mask = 3, 6
        if (mask == 4) cycle
        call check_identity(suite, engine, "form/tensor round trip", &
            form_component(round_form, mask) - form_component(two_form, mask))
    end do
    pair = [2, 1]
    call check_identity(suite, engine, "form tensor antisymmetry", &
        tensor_component(form_tensor, pair) + form_component(two_form, 3))

    density_tensor = density(form_tensor, 1)
    if (form_valid(form_from_tensor(density_tensor))) then
        suite%total = suite%total + 1
        suite%failed = suite%failed + 1
        print *, "FAIL  density tensor conversion refusal"
    end if
    tensor_matrix = num(arena, 0)
    tensor_matrix(1, 1) = num(arena, 1)
    nonsymmetric = tensor_from_matrix(shear, tensor_matrix, LOWER_VARIANCE, &
        LOWER_VARIANCE)
    if (form_valid(form_from_tensor(nonsymmetric))) then
        suite%total = suite%total + 1
        suite%failed = suite%failed + 1
        print *, "FAIL  nonsymmetric tensor conversion refusal"
    end if

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

    lorentz_components(1, 1) = num(arena, -1)
    lorentz_components(1, 2) = num(arena, 0)
    lorentz_components(1, 3) = num(arena, 0)
    lorentz_components(2, 1) = num(arena, 0)
    lorentz_components(2, 2) = num(arena, 1)
    lorentz_components(2, 3) = num(arena, 0)
    lorentz_components(3, 1) = num(arena, 0)
    lorentz_components(3, 2) = num(arena, 0)
    lorentz_components(3, 3) = num(arena, 1)
    lorentz_signature = [-1, 1, 1]
    lorentz_metric = metric_create(lorentz_components, &
        signature=lorentz_signature, orientation=1, coordinates=u)
    metric_hodge = star(lorentz_metric, one_form)
    metric_hodge_hodge = star(lorentz_metric, metric_hodge)
    do i = 1, DIM
        mask = 2**(i - 1)
        call check_identity(suite, engine, "Lorentzian Hodge star squared", &
            form_component(metric_hodge_hodge, mask) + &
            form_component(one_form, mask))
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
    reversed_volume = volume_form(shear, -1)
    call check_identity(suite, engine, "volume form matches Hodge volume", &
        form_component(volume, 7) - form_component(volume_form(shear), 7))
    call check_identity(suite, engine, "reversed volume form orientation", &
        form_component(reversed_volume, 7) + form_component(volume, 7))
    call check_identity(suite, engine, "metric volume uses stored orientation", &
        form_component(volume_form(metric_owner), 7) + &
        form_component(volume, 7))
    call check_identity(suite, engine, "metric volume orientation override", &
        form_component(volume_form(metric_owner, 1), 7) - &
        form_component(volume, 7))
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

    top_from_d = d(shear, volume)
    top_zero = form_zero(shear, DIM + 1)
    call check_identity(suite, engine, "d of a top form is the degree-four zero form", &
        form_component(top_from_d, 0))
    call check_identity(suite, engine, "explicit degree-four zero form", &
        form_component(top_zero, 0))
    if (form_degree(top_from_d) /= DIM + 1 .or. &
        form_degree(top_zero) /= DIM + 1) then
        suite%total = suite%total + 1
        suite%failed = suite%failed + 1
        print *, "FAIL  degree-four form metadata"
    end if

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
