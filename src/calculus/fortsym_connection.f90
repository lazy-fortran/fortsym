module fortsym_connection
    ! Metric connection and curvature for the typed coordinate tensor layer.
    !
    ! The chart remains the owner of the coordinate map and Christoffel
    ! symbols. This module owns only indexed covariant calculus, so it can be
    ! replaced by a supplied connection later without coupling the expression
    ! arena to a relativity or continuum-mechanics package.
    !
    ! Convention:
    !   (nabla_k T)^..._... appends k as a lower slot.
    !   A density of weight w contributes -w Gamma^m_mk T.
    !   R^a_bcd = d_c Gamma^a_db - d_d Gamma^a_cb
    !             + Gamma^a_cm Gamma^m_db - Gamma^a_dm Gamma^m_cb.
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: chart_t, DIM, &
        chart_christoffel => christoffel, metric_covariant, metric_contravariant
    use fortsym_metric, only: metric_t, metric_valid, metric_has_coordinates, &
        metric_same_arena, metric_arena, metric_coordinates, &
        owner_metric_covariant => metric_covariant, &
        owner_metric_contravariant => metric_contravariant
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, num, operator(+), operator(-), &
        operator(*), operator(/)
    use fortsym_tensor, only: tensor_t, MAX_RANK, UPPER, LOWER_VARIANCE, &
        tensor_from_components, tensor_from_matrix, tensor_from_arena, tensor_component, &
        tensor_rank, tensor_variance, tensor_density_weight, tensor_valid, &
        tensor_same_arena
    implicit none
    private

    integer, parameter :: MAX_COMPONENTS = DIM**MAX_RANK

    public :: covariant_diff, covariant_derivative
    public :: christoffel_tensor, riemann_tensor, ricci_tensor
    public :: scalar_curvature, einstein_tensor

    interface covariant_diff
        module procedure covariant_diff_chart
        module procedure covariant_diff_metric
    end interface covariant_diff

    interface covariant_derivative
        module procedure covariant_derivative_chart
        module procedure covariant_derivative_metric
    end interface covariant_derivative

    interface christoffel_tensor
        module procedure christoffel_tensor_chart
        module procedure christoffel_tensor_metric
    end interface christoffel_tensor

    interface riemann_tensor
        module procedure riemann_tensor_chart
        module procedure riemann_tensor_metric
    end interface riemann_tensor

    interface ricci_tensor
        module procedure ricci_tensor_chart
        module procedure ricci_tensor_metric
    end interface ricci_tensor

    interface scalar_curvature
        module procedure scalar_curvature_chart
        module procedure scalar_curvature_metric
    end interface scalar_curvature

    interface einstein_tensor
        module procedure einstein_tensor_chart
        module procedure einstein_tensor_metric
    end interface einstein_tensor

contains

    !> Full covariant derivative, with the derivative index as the last slot.
    function covariant_diff_chart(c, tensor_value) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result
        type(expr_t) :: gamma(DIM, DIM, DIM)

        if (.not. associated(c%a)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. tensor_same_arena(tensor_value, c)) return
        gamma = chart_christoffel(c)
        result = covariant_diff_components(c%a, c%u, &
            gamma, tensor_value)
    end function covariant_diff_chart

    !> Full covariant derivative using a supplied coordinate-aware metric.
    function covariant_diff_metric(g, tensor_value) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result
        type(expr_t) :: coordinates(DIM), gamma(DIM, DIM, DIM)

        if (.not. metric_same_arena(g, tensor_value%a)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. metric_has_coordinates(g)) return
        coordinates = metric_coordinates(g)
        gamma = metric_christoffel_components(g)
        result = covariant_diff_components(metric_arena(g), coordinates, &
            gamma, tensor_value)
    end function covariant_diff_metric

    !> Descriptive alias for callers who prefer the full operation name.
    function covariant_derivative_chart(c, tensor_value) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result

        result = covariant_diff_chart(c, tensor_value)
    end function covariant_derivative_chart

    !> Readable alias for the metric-owner covariant derivative.
    function covariant_derivative_metric(g, tensor_value) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result

        result = covariant_diff_metric(g, tensor_value)
    end function covariant_derivative_metric

    function covariant_diff_components(a, coordinates, gamma, tensor_value) &
            result(result)
        type(arena_t), pointer, intent(in) :: a
        type(expr_t), intent(in) :: coordinates(DIM), gamma(DIM, DIM, DIM)
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        type(expr_t) :: base, term, trace_gamma
        integer :: rank, output_rank, count, output_index
        integer :: indices(MAX_RANK), old_indices(MAX_RANK)
        integer :: variances(MAX_RANK), empty(0)
        integer :: k, i, m, slot, weight

        rank = tensor_rank(tensor_value)
        if (rank >= MAX_RANK) return
        output_rank = rank + 1
        count = component_count(output_rank)
        variances = 0
        do slot = 1, rank
            variances(slot) = tensor_variance(tensor_value, slot)
        end do
        variances(output_rank) = LOWER_VARIANCE
        weight = tensor_density_weight(tensor_value)

        do output_index = 0, count - 1
            call decode_index(output_index, output_rank, indices)
            k = indices(output_rank)
            if (rank == 0) then
                base = tensor_component(tensor_value, empty)
            else
                base = tensor_component(tensor_value, indices(1:rank))
            end if
            term = diff(base, coordinates(k))

            if (weight /= 0) then
                trace_gamma = num(a, 0)
                do m = 1, DIM
                    trace_gamma = trace_gamma + gamma(m, m, k)
                end do
                term = term - num(a, weight)*trace_gamma*base
            end if

            do slot = 1, rank
                i = indices(slot)
                do m = 1, DIM
                    old_indices = indices
                    old_indices(slot) = m
                    if (variances(slot) == UPPER) then
                        term = term + gamma(i, k, m)* &
                            tensor_component(tensor_value, old_indices(1:rank))
                    else
                        term = term - gamma(m, k, i)* &
                            tensor_component(tensor_value, old_indices(1:rank))
                    end if
                end do
            end do
            values(output_index) = term
        end do

        result = tensor_from_arena(a, output_rank, values, variances, weight)
    end function covariant_diff_components

    function metric_christoffel_components(g) result(gamma)
        type(metric_t), intent(in) :: g
        type(expr_t) :: gamma(DIM, DIM, DIM)
        type(expr_t) :: metric(DIM, DIM), inverse(DIM, DIM)
        type(expr_t) :: coordinates(DIM), term
        integer :: k, i, j, l

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        metric = owner_metric_covariant(g)
        inverse = owner_metric_contravariant(g)
        coordinates = metric_coordinates(g)
        do k = 1, DIM
            do i = 1, DIM
                do j = 1, DIM
                    gamma(k, i, j) = num(metric_arena(g), 0)
                    do l = 1, DIM
                        term = diff(metric(l, j), coordinates(i)) + &
                            diff(metric(l, i), coordinates(j)) - &
                            diff(metric(i, j), coordinates(l))
                        gamma(k, i, j) = gamma(k, i, j) + &
                            inverse(k, l)*term/2
                    end do
                end do
            end do
        end do
    end function metric_christoffel_components

    !> Christoffel symbols as Gamma^a_bc with explicit slot variance.
    function christoffel_tensor_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(expr_t) :: gamma(DIM, DIM, DIM), values(MAX_COMPONENTS)
        integer :: indices(MAX_RANK), variances(MAX_RANK)
        integer :: a, b, d

        if (.not. associated(c%a)) return
        gamma = chart_christoffel(c)
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        do a = 1, DIM
            do b = 1, DIM
                do d = 1, DIM
                    indices(1) = a
                    indices(2) = b
                    indices(3) = d
                    values(encode_index(indices, 3) + 1) = gamma(a, b, d)
                end do
            end do
        end do
        result = tensor_from_components(c, 3, values(1:component_count(3)), &
            variances(1:3))
    end function christoffel_tensor_chart

    !> Christoffel tensor for a supplied metric with explicit coordinates.
    !>
    !> A metric without coordinates remains useful for algebraic operations,
    !> but connection construction refuses it instead of guessing derivative
    !> variables from free-symbol order.
    function christoffel_tensor_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result
        type(expr_t) :: gamma(DIM, DIM, DIM)
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        integer :: variances(MAX_RANK), indices(MAX_RANK), k, i, j

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        gamma = metric_christoffel_components(g)
        do k = 1, DIM
            do i = 1, DIM
                do j = 1, DIM
                    indices(1) = k
                    indices(2) = i
                    indices(3) = j
                    values(encode_index(indices, 3)) = gamma(k, i, j)
                end do
            end do
        end do
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        result = tensor_from_arena(metric_arena(g), 3, values, variances)
    end function christoffel_tensor_metric

    !> Riemann tensor R^a_bcd in the convention declared above.
    function riemann_tensor_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(expr_t) :: gamma(DIM, DIM, DIM), values(MAX_COMPONENTS), value
        integer :: indices(MAX_RANK), variances(MAX_RANK)
        integer :: a, b, cindex, d, m

        if (.not. associated(c%a)) return
        gamma = chart_christoffel(c)
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        variances(4) = LOWER_VARIANCE
        do a = 1, DIM
            do b = 1, DIM
                do cindex = 1, DIM
                    do d = 1, DIM
                        value = diff(gamma(a, d, b), c%u(cindex)) - &
                            diff(gamma(a, cindex, b), c%u(d))
                        do m = 1, DIM
                            value = value + gamma(a, cindex, m)*gamma(m, d, b) - &
                                gamma(a, d, m)*gamma(m, cindex, b)
                        end do
                        indices(1) = a
                        indices(2) = b
                        indices(3) = cindex
                        indices(4) = d
                        values(encode_index(indices, 4) + 1) = value
                    end do
                end do
            end do
        end do
        result = tensor_from_components(c, 4, values(1:component_count(4)), &
            variances(1:4))
    end function riemann_tensor_chart

    !> Riemann tensor for a supplied coordinate-aware metric.
    function riemann_tensor_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result
        type(expr_t) :: gamma(DIM, DIM, DIM), coordinates(DIM)
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        integer :: variances(MAX_RANK)

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        gamma = metric_christoffel_components(g)
        coordinates = metric_coordinates(g)
        call riemann_components(gamma, coordinates, values)
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        variances(4) = LOWER_VARIANCE
        result = tensor_from_arena(metric_arena(g), 4, values, variances)
    end function riemann_tensor_metric

    !> Ricci tensor R_bd = R^a_bad, returned covariant in both slots.
    function ricci_tensor_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(tensor_t) :: riemann
        type(expr_t) :: values(DIM, DIM), value
        integer :: indices(MAX_RANK)
        integer :: a, b, d

        if (.not. associated(c%a)) return
        riemann = riemann_tensor_chart(c)
        if (.not. tensor_valid(riemann)) return
        do b = 1, DIM
            do d = 1, DIM
                value = num(c%a, 0)
                do a = 1, DIM
                    indices(1) = a
                    indices(2) = b
                    indices(3) = a
                    indices(4) = d
                    value = value + tensor_component(riemann, indices(1:4))
                end do
                values(b, d) = value
            end do
        end do
        result = tensor_from_matrix(c, values, LOWER_VARIANCE, LOWER_VARIANCE)
    end function ricci_tensor_chart

    !> Ricci tensor obtained by contracting the metric-owner Riemann tensor.
    function ricci_tensor_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result, riemann
        type(expr_t) :: values(0:MAX_COMPONENTS - 1), value
        integer :: indices(MAX_RANK), a, b, d
        integer :: variances(MAX_RANK)

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        riemann = riemann_tensor_metric(g)
        if (.not. tensor_valid(riemann)) return
        do b = 1, DIM
            do d = 1, DIM
                value = num(metric_arena(g), 0)
                do a = 1, DIM
                    indices(1) = a
                    indices(2) = b
                    indices(3) = a
                    indices(4) = d
                    value = value + tensor_component(riemann, indices(1:4))
                end do
                values(encode_pair(b, d)) = value
            end do
        end do
        variances = 0
        variances(1) = LOWER_VARIANCE
        variances(2) = LOWER_VARIANCE
        result = tensor_from_arena(metric_arena(g), 2, values, variances)
    end function ricci_tensor_metric

    !> Scalar curvature R = g^bd R_bd.
    function scalar_curvature_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t) :: result
        type(tensor_t) :: ricci
        type(expr_t) :: inverse_metric(DIM, DIM)
        integer :: indices(MAX_RANK)
        integer :: i, j

        if (.not. associated(c%a)) return
        result = num(c%a, 0)
        ricci = ricci_tensor_chart(c)
        if (.not. tensor_valid(ricci)) return
        inverse_metric = metric_contravariant(c)
        do i = 1, DIM
            do j = 1, DIM
                indices(1) = i
                indices(2) = j
                result = result + inverse_metric(i, j)* &
                    tensor_component(ricci, indices(1:2))
            end do
        end do
    end function scalar_curvature_chart

    !> Scalar curvature obtained from a metric-owner Ricci tensor.
    function scalar_curvature_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(expr_t) :: result
        type(tensor_t) :: ricci
        type(expr_t) :: inverse_metric(DIM, DIM)
        integer :: indices(MAX_RANK), i, j

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        result = num(metric_arena(g), 0)
        ricci = ricci_tensor_metric(g)
        if (.not. tensor_valid(ricci)) return
        inverse_metric = owner_metric_contravariant(g)
        do i = 1, DIM
            do j = 1, DIM
                indices(1) = i
                indices(2) = j
                result = result + inverse_metric(i, j)* &
                    tensor_component(ricci, indices(1:2))
            end do
        end do
    end function scalar_curvature_metric

    !> Einstein tensor G_bd = R_bd - 1/2 R g_bd.
    function einstein_tensor_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(tensor_t) :: ricci
        type(expr_t) :: values(DIM, DIM), metric(DIM, DIM), scalar, half
        integer :: indices(MAX_RANK)
        integer :: i, j

        if (.not. associated(c%a)) return
        ricci = ricci_tensor_chart(c)
        if (.not. tensor_valid(ricci)) return
        metric = metric_covariant(c)
        scalar = scalar_curvature_chart(c)
        half = num(c%a, 1)/num(c%a, 2)
        do i = 1, DIM
            do j = 1, DIM
                indices(1) = i
                indices(2) = j
                values(i, j) = tensor_component(ricci, indices(1:2)) - &
                    half*scalar*metric(i, j)
            end do
        end do
        result = tensor_from_matrix(c, values, LOWER_VARIANCE, LOWER_VARIANCE)
    end function einstein_tensor_chart

    !> Einstein tensor for a supplied coordinate-aware metric.
    function einstein_tensor_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result, ricci
        type(expr_t) :: values(0:MAX_COMPONENTS - 1), metric(DIM, DIM)
        type(expr_t) :: scalar, half, value
        integer :: indices(MAX_RANK), variances(MAX_RANK), i, j

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        ricci = ricci_tensor_metric(g)
        if (.not. tensor_valid(ricci)) return
        metric = owner_metric_covariant(g)
        scalar = scalar_curvature_metric(g)
        half = num(metric_arena(g), 1)/num(metric_arena(g), 2)
        do i = 1, DIM
            do j = 1, DIM
                indices(1) = i
                indices(2) = j
                value = tensor_component(ricci, indices(1:2)) - &
                    half*scalar*metric(i, j)
                values(encode_pair(i, j)) = value
            end do
        end do
        variances = 0
        variances(1) = LOWER_VARIANCE
        variances(2) = LOWER_VARIANCE
        result = tensor_from_arena(metric_arena(g), 2, values, variances)
    end function einstein_tensor_metric

    subroutine riemann_components(gamma, coordinates, values)
        type(expr_t), intent(in) :: gamma(DIM, DIM, DIM), coordinates(DIM)
        type(expr_t), intent(out) :: values(0:MAX_COMPONENTS - 1)
        type(expr_t) :: value
        integer :: indices(MAX_RANK)
        integer :: a, b, cindex, d, m

        do a = 1, DIM
            do b = 1, DIM
                do cindex = 1, DIM
                    do d = 1, DIM
                        value = diff(gamma(a, d, b), coordinates(cindex)) - &
                            diff(gamma(a, cindex, b), coordinates(d))
                        do m = 1, DIM
                            value = value + gamma(a, cindex, m)*gamma(m, d, b) - &
                                gamma(a, d, m)*gamma(m, cindex, b)
                        end do
                        indices(1) = a
                        indices(2) = b
                        indices(3) = cindex
                        indices(4) = d
                        values(encode_index(indices, 4)) = value
                    end do
                end do
            end do
        end do
    end subroutine riemann_components

    pure function component_count(rank) result(count)
        integer, intent(in) :: rank
        integer :: count, k

        count = 1
        do k = 1, rank
            count = count*DIM
        end do
    end function component_count

    pure function encode_pair(first, second) result(index)
        integer, intent(in) :: first, second
        integer :: index

        index = (first - 1) + DIM*(second - 1)
    end function encode_pair

    pure function encode_index(indices, rank) result(index)
        integer, intent(in) :: indices(:), rank
        integer :: index, scale, k

        index = 0
        scale = 1
        do k = 1, rank
            index = index + (indices(k) - 1)*scale
            scale = scale*DIM
        end do
    end function encode_index

    subroutine decode_index(index, rank, indices)
        integer, intent(in) :: index, rank
        integer, intent(out) :: indices(MAX_RANK)
        integer :: value, k

        indices = 1
        value = index
        do k = 1, rank
            indices(k) = mod(value, DIM) + 1
            value = value/DIM
        end do
    end subroutine decode_index

end module fortsym_connection
