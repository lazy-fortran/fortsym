program test_fortsym_notation
    ! The notation gate keeps the physicist's component views distinct:
    ! B^i, B_i, and sqrtg B^i carry different variance or density metadata.
    ! The same test includes the mathematician's metric-free form identities
    ! and explicit orientation/signature boundaries.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create, jacobian, sqrtg
    use fortsym_metric, only: metric_t, metric_create
    use fortsym_form, only: form_t, form_one, form_component, form_valid, &
        volume_form, d, wedge, star
    use fortsym_tensor, only: tensor_t, vector, tensor_component, &
        tensor_variance, tensor_density_weight, tensor_valid, density, lower, &
        tensor_from_matrix, LOWER_VARIANCE, UPPER
    use fortsym_form_tensor, only: form_from_tensor, tensor_from_form
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: chart, left_chart
    type(metric_t) :: lorentz_metric
    type(expr_t) :: u(DIM), position(DIM), field(DIM)
    type(expr_t) :: metric_components(DIM, DIM)
    type(expr_t) :: signed_j, positive_volume
    type(expr_t) :: alpha_component(DIM)
    type(form_t) :: alpha, beta, beta_beta, oriented_volume
    type(form_t) :: round_form
    type(tensor_t) :: b_upper, b_lower, b_density, form_tensor
    type(tensor_t) :: nonsymmetric
    type(expr_t) :: nonsymmetric_components(DIM, DIM)
    integer :: signature(DIM), indices(1), mask

    call arena%init()
    engine = make_symengine_engine(arena)
    u(1) = sym(arena, "notation_u1")
    u(2) = sym(arena, "notation_u2")
    u(3) = sym(arena, "notation_u3")
    position = u
    chart = chart_create(arena, u, position)
    position(3) = -u(3)
    left_chart = chart_create(arena, u, position)
    call suite_begin(suite, "physicist and mathematician notation")

    ! A left-handed chart changes the signed Jacobian only. The metric volume
    ! remains positive, while the oriented top form receives an explicit sign.
    signed_j = jacobian(left_chart)
    positive_volume = sqrtg(left_chart)
    oriented_volume = volume_form(left_chart, -1)
    call check_identity(suite, engine, "left-handed signed Jacobian", &
        signed_j + 1)
    call check_identity(suite, engine, "left-handed positive sqrtg", &
        positive_volume - 1)
    call check_identity(suite, engine, "left-handed oriented volume", &
        form_component(oriented_volume, 7) + 1)

    ! The same numerical components have different geometric meanings when
    ! their variance or density metadata changes.
    field(1) = u(1)
    field(2) = u(2)
    field(3) = u(3)
    b_upper = vector(left_chart, field)
    b_density = density(b_upper, 1)
    indices(1) = 1
    if (.not. tensor_valid(b_upper)) error stop "B^i tensor invalid"
    if (.not. tensor_valid(b_density)) error stop "sqrtg B^i tensor invalid"
    if (tensor_variance(b_upper, 1) /= UPPER) error stop "B^i variance lost"
    if (tensor_density_weight(b_upper) /= 0) error stop "B^i weight changed"
    if (tensor_variance(b_density, 1) /= UPPER) then
        error stop "sqrtg B^i variance lost"
    end if
    if (tensor_density_weight(b_density) /= 1) then
        error stop "sqrtg B^i density weight lost"
    end if
    call check_identity(suite, engine, "B^i component is unchanged by metadata", &
        tensor_component(b_density, indices) - tensor_component(b_upper, indices))

    signature = [1, 1, 1]
    metric_components = num(arena, 0)
    metric_components(1, 1) = num(arena, -1)
    metric_components(2, 2) = num(arena, 1)
    metric_components(3, 3) = num(arena, 1)
    signature(1) = -1
    lorentz_metric = metric_create(metric_components, signature=signature, &
        orientation=1, coordinates=u)
    b_lower = lower(lorentz_metric, b_upper, 1)
    if (.not. tensor_valid(b_lower)) error stop "B_i tensor invalid"
    if (tensor_variance(b_lower, 1) /= LOWER_VARIANCE) then
        error stop "B_i variance lost"
    end if
    call check_identity(suite, engine, "Lorentzian B_1 = -B^1", &
        tensor_component(b_lower, indices) + tensor_component(b_upper, indices))

    ! Exterior calculus remains metric-free. This is the same chart-independent
    ! identity used for magnetic flux forms and Maxwell's Bianchi identity.
    alpha_component(1) = u(2)*u(3)
    alpha_component(2) = u(1)**2
    alpha_component(3) = u(2) + u(3)**2
    alpha = form_one(left_chart, alpha_component)
    beta = d(left_chart, alpha)
    beta_beta = d(left_chart, beta)
    do mask = 3, 6
        if (mask == 4) cycle
        call check_identity(suite, engine, "metric-free d(d(alpha))", &
            form_component(beta_beta, mask))
    end do
    call check_identity(suite, engine, "metric-free alpha wedge alpha", &
        form_component(wedge(alpha, alpha), 3))

    ! The Lorentzian Hodge involution is metric-dependent and has the opposite
    ! sign from the Euclidean three-dimensional chart involution.
    beta_beta = star(lorentz_metric, alpha)
    beta = star(lorentz_metric, beta_beta)
    do mask = 1, 4
        if (mask == 3) cycle
        call check_identity(suite, engine, "Lorentzian Hodge sign", &
            form_component(beta, mask) + form_component(alpha, mask))
    end do

    ! Conversion refuses a density or a tensor that is not exactly alternating.
    form_tensor = tensor_from_form(left_chart, beta_beta)
    round_form = form_from_tensor(form_tensor)
    if (.not. form_valid(round_form)) then
        error stop "valid form/tensor round trip was refused"
    end if
    if (form_valid(form_from_tensor(b_density))) then
        error stop "density tensor was silently converted to a form"
    end if
    nonsymmetric_components = num(arena, 0)
    nonsymmetric_components(1, 1) = num(arena, 1)
    nonsymmetric = tensor_from_matrix(left_chart, nonsymmetric_components, &
        LOWER_VARIANCE, LOWER_VARIANCE)
    if (form_valid(form_from_tensor(nonsymmetric))) then
        error stop "non-antisymmetric tensor was silently converted"
    end if

    if (suite%failed /= 0) then
        print *, "test_fortsym_notation: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_notation.json")
    print *, "test_fortsym_notation: all checks passed"
end program test_fortsym_notation
