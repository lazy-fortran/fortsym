module fortsym_chart_map
    ! Coordinate transitions between chart-bound tensor representations.
    !
    ! A transition stores both directions. Components are first transformed
    ! in the source variables and then expressed in target variables through
    ! the supplied inverse map. This makes the coordinate dependence explicit
    ! and prevents a target tensor from carrying source-coordinate expressions.
    use fortsym_chart, only: chart_t, DIM
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, num, abs, is_valid, operator(+), &
        operator(-), operator(*), operator(/), operator(**), operator(==)
    use fortsym_subs, only: subs_many
    use fortsym_form, only: form_t, form_scalar, form_one, form_two, form_three, &
        form_component, form_degree, form_valid
    use fortsym_tensor, only: tensor_t, MAX_RANK, UPPER, LOWER_VARIANCE, &
        tensor_valid, tensor_from_storage
    implicit none
    private

    integer, parameter :: MAX_COMPONENTS = DIM**MAX_RANK

    public :: chart_map_t, chart_map_create, compose_maps
    public :: map_jacobian, inverse_jacobian, transform_tensor, transform_form

    type :: chart_map_t
        type(chart_t) :: source
        type(chart_t) :: target
        type(expr_t) :: forward(DIM)
        type(expr_t) :: inverse(DIM)
    end type chart_map_t

contains

    !> Create a transition with target coordinates as functions of source
    !> coordinates and source coordinates as functions of target coordinates.
    function chart_map_create(source, target, forward, inverse) result(result)
        type(chart_t), intent(in) :: source, target
        type(expr_t), intent(in) :: forward(DIM), inverse(DIM)
        type(chart_map_t) :: result
        integer :: k

        if (.not. associated(source%a)) return
        if (.not. associated(target%a)) return
        if (.not. associated(source%a, target%a)) return
        do k = 1, DIM
            if (.not. is_valid(forward(k))) return
            if (.not. is_valid(inverse(k))) return
            if (.not. associated(forward(k)%a, source%a)) return
            if (.not. associated(inverse(k)%a, target%a)) return
        end do
        result%source = source
        result%target = target
        result%forward = forward
        result%inverse = inverse
    end function chart_map_create

    !> Compose two transitions. `following` is applied after `first`.
    function compose_maps(first, following) result(result)
        type(chart_map_t), intent(in) :: first, following
        type(chart_map_t) :: result
        type(expr_t) :: forward(DIM), inverse(DIM)
        integer :: k

        if (.not. map_valid(first)) return
        if (.not. map_valid(following)) return
        if (.not. same_chart(first%target, following%source)) return
        do k = 1, DIM
            forward(k) = subs_many(following%forward(k), &
                following%source%u, first%forward)
            inverse(k) = subs_many(first%inverse(k), first%target%u, &
                following%inverse)
        end do
        result = chart_map_create(first%source, following%target, forward, inverse)
    end function compose_maps

    !> K(i,j) = partial(target coordinate i)/partial(source coordinate j).
    function map_jacobian(map) result(result)
        type(chart_map_t), intent(in) :: map
        type(expr_t) :: result(DIM, DIM)
        integer :: i, j

        do i = 1, DIM
            do j = 1, DIM
                result(i, j) = diff(map%forward(i), map%source%u(j))
            end do
        end do
    end function map_jacobian

    !> L(i,j) = partial(source coordinate i)/partial(target coordinate j).
    function inverse_jacobian(map) result(result)
        type(chart_map_t), intent(in) :: map
        type(expr_t) :: result(DIM, DIM)
        integer :: i, j

        do i = 1, DIM
            do j = 1, DIM
                result(i, j) = diff(map%inverse(i), map%target%u(j))
            end do
        end do
    end function inverse_jacobian

    !> Transform every tensor slot and retain its density weight.
    !>
    !> Upper slots use K. Lower slots use L. A density of weight w receives
    !> abs(det(K))**w. The result is expressed in the target coordinates.
    function transform_tensor(map, source_tensor) result(result)
        type(chart_map_t), intent(in) :: map
        type(tensor_t), intent(in) :: source_tensor
        type(tensor_t) :: result
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        type(expr_t) :: k_target(DIM, DIM), lower_map(DIM, DIM)
        type(expr_t) :: jacobian_det, density_factor, transformed
        type(expr_t) :: term
        integer :: rank, count, output_flat, source_flat, slot
        integer :: output_indices(MAX_RANK), source_indices(MAX_RANK)
        integer :: variance(MAX_RANK)

        if (.not. map_valid(map)) return
        if (.not. tensor_valid(source_tensor)) return
        if (.not. associated(source_tensor%a, map%source%a)) return

        rank = source_tensor%rank
        count = component_count(rank)
        variance = source_tensor%variance
        k_target = target_jacobian(map)
        lower_map = inverse_jacobian(map)
        jacobian_det = det3(k_target)
        density_factor = num(map%target%a, 1)
        if (source_tensor%density_weight /= 0) then
            density_factor = abs(jacobian_det)**source_tensor%density_weight
        end if

        do output_flat = 0, count - 1
            call decode_index(output_flat, rank, output_indices)
            values(output_flat) = num(map%target%a, 0)
            do source_flat = 0, count - 1
                call decode_index(source_flat, rank, source_indices)
                term = density_factor
                do slot = 1, rank
                    if (variance(slot) == UPPER) then
                        term = term*k_target(output_indices(slot), &
                            source_indices(slot))
                    else if (variance(slot) == LOWER_VARIANCE) then
                        term = term*lower_map(source_indices(slot), &
                            output_indices(slot))
                    end if
                end do
                transformed = subs_many(source_tensor%component(source_flat), &
                    map%source%u, map%inverse)
                values(output_flat) = values(output_flat) + term*transformed
            end do
        end do
        result = tensor_from_storage(map%target, rank, values, variance, &
            source_tensor%density_weight)
    end function transform_tensor

    !> Express a source differential form in the target coordinate coframe.
    !>
    !> This is the pullback along the supplied inverse coordinate map: every
    !> covector factor uses L(i,j) = partial(source i)/partial(target j),
    !> while the coefficient is simultaneously expressed in target symbols.
    !> The top-form rule is signed, so orientation reversal is preserved.
    function transform_form(map, source_form) result(result)
        type(chart_map_t), intent(in) :: map
        type(form_t), intent(in) :: source_form
        type(form_t) :: result
        type(expr_t) :: lower_map(DIM, DIM)
        type(expr_t) :: one_values(DIM), two_values(3), coefficient
        type(expr_t) :: source_12, source_13, source_23
        integer :: degree, i, j

        if (.not. map_valid(map)) return
        if (.not. form_valid(source_form)) return
        if (.not. associated(source_form%a, map%source%a)) return
        degree = form_degree(source_form)
        if (degree < 0 .or. degree > DIM) return

        lower_map = inverse_jacobian(map)
        select case (degree)
        case (0)
            coefficient = subs_many(form_component(source_form, 0), &
                map%source%u, map%inverse)
            result = form_scalar(coefficient)
        case (1)
            one_values = num(map%target%a, 0)
            do j = 1, DIM
                do i = 1, DIM
                    coefficient = subs_many(form_component(source_form, &
                        2**(i - 1)), map%source%u, map%inverse)
                    one_values(j) = one_values(j) + lower_map(i, j)*coefficient
                end do
            end do
            result = form_one(map%target, one_values)
        case (2)
            two_values = num(map%target%a, 0)
            source_12 = subs_many(form_component(source_form, 3), &
                map%source%u, map%inverse)
            source_13 = subs_many(form_component(source_form, 5), &
                map%source%u, map%inverse)
            source_23 = subs_many(form_component(source_form, 6), &
                map%source%u, map%inverse)
            do j = 1, DIM
                do i = j + 1, DIM
                    two_values(pair_index(j, i)) = &
                        source_12*(lower_map(1, j)*lower_map(2, i) - &
                        lower_map(1, i)*lower_map(2, j)) + &
                        source_13*(lower_map(1, j)*lower_map(3, i) - &
                        lower_map(1, i)*lower_map(3, j)) + &
                        source_23*(lower_map(2, j)*lower_map(3, i) - &
                        lower_map(2, i)*lower_map(3, j))
                end do
            end do
            result = form_two(map%target, two_values)
        case (3)
            coefficient = subs_many(form_component(source_form, 7), &
                map%source%u, map%inverse)
            result = form_three(map%target, coefficient*det3(lower_map))
        end select
    end function transform_form

    function target_jacobian(map) result(result)
        type(chart_map_t), intent(in) :: map
        type(expr_t) :: result(DIM, DIM)
        type(expr_t) :: source_jacobian(DIM, DIM)
        integer :: i, j

        source_jacobian = map_jacobian(map)
        do i = 1, DIM
            do j = 1, DIM
                result(i, j) = subs_many(source_jacobian(i, j), &
                    map%source%u, map%inverse)
            end do
        end do
    end function target_jacobian

    function map_valid(map) result(valid)
        type(chart_map_t), intent(in) :: map
        logical :: valid
        integer :: k

        valid = .false.
        if (.not. associated(map%source%a)) return
        if (.not. associated(map%target%a)) return
        if (.not. associated(map%source%a, map%target%a)) return
        do k = 1, DIM
            if (.not. is_valid(map%forward(k))) return
            if (.not. is_valid(map%inverse(k))) return
            if (.not. associated(map%forward(k)%a, map%source%a)) return
            if (.not. associated(map%inverse(k)%a, map%target%a)) return
        end do
        valid = .true.
    end function map_valid

    function same_chart(left, right) result(same)
        type(chart_t), intent(in) :: left, right
        logical :: same
        integer :: k

        same = .false.
        if (.not. associated(left%a)) return
        if (.not. associated(right%a)) return
        if (.not. associated(left%a, right%a)) return
        do k = 1, DIM
            if (.not. (left%u(k) == right%u(k))) return
            if (.not. (left%x(k) == right%x(k))) return
        end do
        same = .true.
    end function same_chart

    function det3(matrix) result(result)
        type(expr_t), intent(in) :: matrix(DIM, DIM)
        type(expr_t) :: result

        result = matrix(1, 1)*(matrix(2, 2)*matrix(3, 3) - &
            matrix(2, 3)*matrix(3, 2)) - matrix(1, 2)*(&
            matrix(2, 1)*matrix(3, 3) - matrix(2, 3)*matrix(3, 1)) + &
            matrix(1, 3)*(matrix(2, 1)*matrix(3, 2) - &
            matrix(2, 2)*matrix(3, 1))
    end function det3

    pure function pair_index(first, second) result(index)
        integer, intent(in) :: first, second
        integer :: index

        if (first == 1 .and. second == 2) then
            index = 1
        else if (first == 1 .and. second == 3) then
            index = 2
        else
            index = 3
        end if
    end function pair_index

    pure function component_count(rank) result(count)
        integer, intent(in) :: rank
        integer :: count, k

        count = 1
        do k = 1, rank
            count = count*DIM
        end do
    end function component_count

    subroutine decode_index(flat, rank, indices)
        integer, intent(in) :: flat, rank
        integer, intent(out) :: indices(MAX_RANK)
        integer :: value, k

        indices = 1
        value = flat
        do k = 1, rank
            indices(k) = mod(value, DIM) + 1
            value = value/DIM
        end do
    end subroutine decode_index

end module fortsym_chart_map
