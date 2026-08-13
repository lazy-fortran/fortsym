program test_fortsym_spacetime_form_tensor
    ! A form is a compact owner for an exact lower antisymmetric tensor.  The
    ! bridge is checked against explicit components and rejects tensors whose
    ! metadata or components do not satisfy that definition.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_valid
    use fortsym_spacetime_form, only: spacetime_form_t, spacetime_form_two, &
        spacetime_form_component, spacetime_form_valid
    use fortsym_spacetime_form_tensor, only: spacetime_form_from_tensor, &
        spacetime_tensor_from_form
    use fortsym_spacetime_tensor, only: spacetime_tensor_t, &
        spacetime_tensor_from_components, spacetime_tensor_component, &
        spacetime_tensor_density_factor, spacetime_tensor_dimension, &
        spacetime_tensor_rank, spacetime_tensor_valid, &
        spacetime_tensor_symmetry, &
        spacetime_tensor_variance, SPACETIME_LOWER
    use fortsym_spacetime_tensor, only: SPACETIME_ANTISYMMETRIC
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(spacetime_metric_t) :: metric, metric_2d
    type(spacetime_form_t) :: form, round_form, form_2d
    type(spacetime_form_t) :: refused_density, refused_nonsymmetric
    type(spacetime_tensor_t) :: tensor, tensor_2d, density_tensor, nonsymmetric
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: components_2d(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: form_values(6), tensor_values(SPACETIME_DIM**2)
    type(expr_t) :: form_values_2d(6)
    integer :: signature(SPACETIME_DIM), lower_variance(2), pair(2)

    call arena%init()
    engine = make_symengine_engine(arena)
    coordinates(1) = sym(arena, "bridge_t")
    coordinates(2) = sym(arena, "bridge_x")
    coordinates(3) = sym(arena, "bridge_y")
    coordinates(4) = sym(arena, "bridge_z")
    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = num(arena, 1)
    components(3, 3) = num(arena, 1)
    components(4, 4) = num(arena, 1)
    signature(1) = -1
    signature(2) = 1
    signature(3) = 1
    signature(4) = 1
    metric = spacetime_metric_create(components, 4, coordinates, signature, 1)
    if (.not. spacetime_metric_valid(metric)) error stop "invalid bridge metric"
    call suite_begin(suite, "spacetime form/tensor bridge")

    form_values = num(arena, 0)
    form_values(1) = coordinates(1)
    form_values(2) = coordinates(2)
    form_values(3) = coordinates(3)
    form_values(4) = coordinates(4)
    form_values(5) = coordinates(1) + coordinates(2)
    form_values(6) = coordinates(3) + coordinates(4)
    form = spacetime_form_two(metric, form_values)
    tensor = spacetime_tensor_from_form(metric, form)
    round_form = spacetime_form_from_tensor(metric, tensor)
    call check(suite, spacetime_form_valid(form), "source two-form is valid")
    call check(suite, spacetime_tensor_valid(tensor), "form tensor is valid")
    call check(suite, spacetime_tensor_rank(tensor) == 2, "bridge preserves rank")
    call check(suite, spacetime_tensor_dimension(tensor) == 4, &
        "bridge preserves dimension")
    call check(suite, spacetime_tensor_variance(tensor, 1) == SPACETIME_LOWER, &
        "bridge emits lower first slot")
    call check(suite, spacetime_tensor_variance(tensor, 2) == SPACETIME_LOWER, &
        "bridge emits lower second slot")
    call check(suite, spacetime_tensor_symmetry(tensor, 1, 2) == &
        SPACETIME_ANTISYMMETRIC, "bridge retains antisymmetry metadata")
    call check(suite, spacetime_form_valid(round_form), &
        "tensor form round trip is valid")
    pair(1) = 1
    pair(2) = 2
    call check_identity(suite, engine, "form/tensor canonical component", &
        spacetime_tensor_component(tensor, pair) - form_values(1))
    pair(1) = 2
    pair(2) = 1
    call check_identity(suite, engine, "form/tensor reversed component", &
        spacetime_tensor_component(tensor, pair) + form_values(1))
    call check_identity(suite, engine, "form/tensor round trip", &
        spacetime_form_component(round_form, 12) - form_values(6))

    density_tensor = spacetime_tensor_density_factor(tensor, num(arena, 2))
    refused_density = spacetime_form_from_tensor(metric, density_tensor)
    call check(suite, .not. spacetime_form_valid(refused_density), &
        "density tensor conversion is refused")

    tensor_values = num(arena, 0)
    tensor_values(5) = coordinates(1)
    tensor_values(2) = coordinates(2)
    lower_variance = SPACETIME_LOWER
    nonsymmetric = spacetime_tensor_from_components(metric, 2, tensor_values, &
        lower_variance)
    refused_nonsymmetric = spacetime_form_from_tensor(metric, nonsymmetric)
    call check(suite, .not. spacetime_form_valid(refused_nonsymmetric), &
        "non-antisymmetric tensor conversion is refused")

    components_2d = num(arena, 0)
    components_2d(1, 1) = num(arena, 1)
    components_2d(2, 2) = num(arena, 1)
    signature = 1
    metric_2d = spacetime_metric_create(components_2d, 2, coordinates, &
        signature, 1)
    form_values_2d = num(arena, 0)
    form_values_2d(1) = coordinates(1) + coordinates(2)
    form_2d = spacetime_form_two(metric_2d, form_values_2d)
    tensor_2d = spacetime_tensor_from_form(metric_2d, form_2d)
    call check(suite, spacetime_tensor_valid(tensor_2d), &
        "two-dimensional form tensor is valid")
    pair(1) = 1
    pair(2) = 2
    call check_identity(suite, engine, "2D canonical bridge component", &
        spacetime_tensor_component(tensor_2d, pair) - form_values_2d(1))
    pair(1) = 2
    pair(2) = 1
    call check_identity(suite, engine, "2D reversed bridge component", &
        spacetime_tensor_component(tensor_2d, pair) + form_values_2d(1))

    if (suite%failed /= 0) error stop "spacetime form/tensor checks failed"
    call suite_end(suite, "/tmp/fortsym_spacetime_form_tensor.json")
    print *, "test_fortsym_spacetime_form_tensor: all checks passed"

contains

    subroutine check(s, condition, label)
        type(suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        s%total = s%total + 1
        if (condition) then
            s%passed = s%passed + 1
            s%proved = s%proved + 1
            print *, "PASS         ", label
        else
            s%failed = s%failed + 1
            print *, "FAIL         ", label
        end if
    end subroutine check

end program test_fortsym_spacetime_form_tensor
