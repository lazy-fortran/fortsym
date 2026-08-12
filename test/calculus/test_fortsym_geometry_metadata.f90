program test_fortsym_geometry_metadata
    ! The metadata owner is a declaration contract. Its independent oracle is
    ! the signed-count and validity behavior, followed by metric round trips.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, sym
    use fortsym_geometry_metadata, only: orientation_t, signature_t, &
        orientation_create, orientation_valid, orientation_value, &
        orientation_is_positive, orientation_is_negative, signature_create, &
        signature_valid, signature_dimension, signature_component, &
        signature_positive_count, signature_negative_count, &
        signature_is_lorentzian
    use fortsym_chart, only: DIM
    use fortsym_metric, only: metric_t, metric_create_metadata, metric_valid, &
        metric_signature, metric_orientation, metric_signature_type, &
        metric_orientation_type
    implicit none

    type(arena_t), target :: arena
    type(orientation_t) :: positive, negative, invalid_orientation
    type(signature_t) :: lorentzian, euclidean, invalid_signature
    type(metric_t) :: metric
    type(metric_t) :: invalid_metric
    type(expr_t) :: coordinates(DIM), components(DIM, DIM)
    integer :: i, returned_signature(DIM)

    call arena%init()
    positive = orientation_create(1)
    negative = orientation_create(-1)
    invalid_orientation = orientation_create(0)
    if (.not. orientation_valid(positive)) error stop "positive orientation invalid"
    if (.not. orientation_valid(negative)) error stop "negative orientation invalid"
    if (orientation_value(positive) /= 1) error stop "positive orientation value lost"
    if (orientation_value(negative) /= -1) error stop "negative orientation value lost"
    if (.not. orientation_is_positive(positive)) error stop "positive predicate failed"
    if (.not. orientation_is_negative(negative)) error stop "negative predicate failed"
    if (orientation_valid(invalid_orientation)) error stop "invalid orientation accepted"

    lorentzian = signature_create([-1, 1, 1, 1])
    euclidean = signature_create([1, 1, 1])
    invalid_signature = signature_create([1, 0, 1])
    if (.not. signature_valid(lorentzian)) error stop "Lorentzian signature invalid"
    if (signature_dimension(lorentzian) /= 4) error stop "signature dimension lost"
    if (signature_component(lorentzian, 1) /= -1) error stop "signature component lost"
    if (signature_positive_count(lorentzian) /= 3) error stop "positive count failed"
    if (signature_negative_count(lorentzian) /= 1) error stop "negative count failed"
    if (.not. signature_is_lorentzian(lorentzian)) error stop "Lorentzian predicate failed"
    if (signature_is_lorentzian(euclidean)) error stop "Euclidean predicate failed"
    if (signature_valid(invalid_signature)) error stop "invalid signature accepted"
    if (signature_component(lorentzian, 5) /= 0) error stop "out-of-range component accepted"

    do i = 1, DIM
        coordinates(i) = sym(arena, "metadata_x"//char(ichar("0") + i))
    end do
    components = num(arena, 0)
    do i = 1, DIM
        components(i, i) = num(arena, 1)
    end do
    metric = metric_create_metadata(components, euclidean, negative, coordinates)
    if (.not. metric_valid(metric)) error stop "typed metric invalid"
    returned_signature = metric_signature(metric)
    do i = 1, DIM
        if (returned_signature(i) /= 1) error stop "typed metric signature lost"
    end do
    if (metric_orientation(metric) /= -1) error stop "typed metric orientation lost"
    if (signature_dimension(metric_signature_type(metric)) /= DIM) then
        error stop "metric signature type lost"
    end if
    if (orientation_value(metric_orientation_type(metric)) /= -1) then
        error stop "metric orientation type lost"
    end if
    invalid_metric = metric_create_metadata(components, lorentzian, positive, coordinates)
    if (metric_valid(invalid_metric)) error stop "wrong metric dimension accepted"
    print *, "test_fortsym_geometry_metadata: all checks passed"
end program test_fortsym_geometry_metadata
