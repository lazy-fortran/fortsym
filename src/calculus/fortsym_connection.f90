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
    use fortsym_subs, only: subs
    use fortsym_expr, only: expr_t, num, is_valid, same_arena, operator(+), operator(-), &
        operator(*), operator(/), operator(/=)
    use fortsym_tensor, only: tensor_t, MAX_RANK, UPPER, LOWER_VARIANCE, &
        tensor_from_components, tensor_from_matrix, tensor_from_arena, tensor_component, &
        tensor_rank, tensor_variance, tensor_density_weight, tensor_valid, &
        tensor_same_arena, contract_slots
    implicit none
    private

    integer, parameter :: MAX_COMPONENTS = DIM**MAX_RANK
    integer, parameter, public :: CONNECTION_STANDARD = 1
    integer, parameter, public :: CONNECTION_OPPOSITE = -1

    type :: connection_t
        private
        type(arena_t), pointer :: a => null()
        type(expr_t) :: coordinate(DIM)
        type(expr_t) :: component(DIM, DIM, DIM)
        integer :: convention = CONNECTION_STANDARD
        logical :: valid = .false.
    end type connection_t

    public :: connection_t, connection_create, connection_from_chart, &
        connection_from_metric, connection_valid, connection_arena, &
        connection_same_arena, connection_coordinates, connection_christoffel, &
        connection_convention
    public :: covariant_diff, covariant_derivative, covariant_divergence
    public :: torsion, nonmetricity
    public :: geodesic_residual
    public :: christoffel_tensor, riemann_tensor, first_bianchi_residual, &
        second_bianchi_residual, ricci_tensor
    public :: scalar_curvature, einstein_tensor

    interface torsion
        module procedure torsion_connection
    end interface torsion

    interface nonmetricity
        module procedure nonmetricity_connection_metric
    end interface nonmetricity

    interface covariant_diff
        module procedure covariant_diff_chart
        module procedure covariant_diff_metric
        module procedure covariant_diff_connection
    end interface covariant_diff

    interface covariant_derivative
        module procedure covariant_derivative_chart
        module procedure covariant_derivative_metric
        module procedure covariant_derivative_connection
    end interface covariant_derivative

    interface covariant_divergence
        module procedure covariant_divergence_chart
        module procedure covariant_divergence_metric
        module procedure covariant_divergence_connection
    end interface covariant_divergence

    interface geodesic_residual
        module procedure geodesic_residual_chart
        module procedure geodesic_residual_metric
        module procedure geodesic_residual_connection
    end interface geodesic_residual

    interface christoffel_tensor
        module procedure christoffel_tensor_chart
        module procedure christoffel_tensor_metric
    end interface christoffel_tensor

    interface riemann_tensor
        module procedure riemann_tensor_chart
        module procedure riemann_tensor_metric
        module procedure riemann_tensor_connection
    end interface riemann_tensor

    interface first_bianchi_residual
        module procedure first_bianchi_residual_chart
        module procedure first_bianchi_residual_metric
    end interface first_bianchi_residual

    interface second_bianchi_residual
        module procedure second_bianchi_residual_chart
        module procedure second_bianchi_residual_metric
    end interface second_bianchi_residual

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

    !> Construct an affine connection Gamma^a_bc in explicit coordinates.
    !>
    !> The standard convention is the same one used by the existing curvature
    !> owners: R^a_bcd = d_c Gamma^a_db - d_d Gamma^a_cb + ....  The
    !> opposite convention is its exact negative. The coefficients remain the
    !> supplied affine connection in either case; only curvature views use the
    !> stored sign convention.
    function connection_create(components, coordinates, convention) result(result)
        type(expr_t), intent(in) :: components(DIM, DIM, DIM), coordinates(DIM)
        integer, optional, intent(in) :: convention
        type(connection_t) :: result
        integer :: aindex, b, cindex, k

        if (.not. is_valid(components(1, 1, 1))) return
        result%a => components(1, 1, 1)%a
        do aindex = 1, DIM
            do b = 1, DIM
                do cindex = 1, DIM
                    if (.not. is_valid(components(aindex, b, cindex))) return
                    if (.not. associated(components(aindex, b, cindex)%a, result%a)) return
                end do
            end do
        end do
        do k = 1, DIM
            if (.not. is_valid(coordinates(k))) return
            if (.not. associated(coordinates(k)%a, result%a)) return
        end do

        result%convention = CONNECTION_STANDARD
        if (present(convention)) result%convention = convention
        if (result%convention /= CONNECTION_STANDARD .and. &
            result%convention /= CONNECTION_OPPOSITE) return
        result%component = components
        result%coordinate = coordinates
        result%valid = .true.
    end function connection_create

    !> Construct the Levi-Civita connection induced by a chart.
    function connection_from_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(connection_t) :: result
        type(expr_t) :: gamma(DIM, DIM, DIM)

        if (.not. associated(c%a)) return
        gamma = chart_christoffel(c)
        result = connection_create(gamma, c%u)
    end function connection_from_chart

    !> Construct the coordinate Levi-Civita connection induced by a metric.
    function connection_from_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(connection_t) :: result
        type(expr_t) :: coordinates(DIM), gamma(DIM, DIM, DIM)

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        coordinates = metric_coordinates(g)
        gamma = metric_christoffel_components(g)
        result = connection_create(gamma, coordinates)
    end function connection_from_metric

    function connection_valid(connection) result(valid)
        type(connection_t), intent(in) :: connection
        logical :: valid

        valid = connection%valid
        if (.not. valid) return
        if (.not. associated(connection%a)) then
            valid = .false.
            return
        end if
        if (connection%convention /= CONNECTION_STANDARD .and. &
            connection%convention /= CONNECTION_OPPOSITE) valid = .false.
    end function connection_valid

    function connection_arena(connection) result(a)
        type(connection_t), intent(in) :: connection
        type(arena_t), pointer :: a

        nullify(a)
        if (.not. connection_valid(connection)) return
        a => connection%a
    end function connection_arena

    function connection_same_arena(connection, a) result(same)
        type(connection_t), intent(in) :: connection
        type(arena_t), pointer, intent(in) :: a
        logical :: same

        same = .false.
        if (.not. connection_valid(connection)) return
        same = associated(connection%a, a)
    end function connection_same_arena

    function connection_coordinates(connection) result(coordinates)
        type(connection_t), intent(in) :: connection
        type(expr_t) :: coordinates(DIM)

        if (.not. connection_valid(connection)) return
        coordinates = connection%coordinate
    end function connection_coordinates

    function connection_christoffel(connection) result(components)
        type(connection_t), intent(in) :: connection
        type(expr_t) :: components(DIM, DIM, DIM)

        if (.not. connection_valid(connection)) return
        components = connection%component
    end function connection_christoffel

    function connection_convention(connection) result(convention)
        type(connection_t), intent(in) :: connection
        integer :: convention

        convention = 0
        if (.not. connection_valid(connection)) return
        convention = connection%convention
    end function connection_convention

    !> T^a_bc = Gamma^a_bc - Gamma^a_cb.
    function torsion_connection(connection) result(result)
        type(connection_t), intent(in) :: connection
        type(tensor_t) :: result
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        integer :: variances(MAX_RANK), indices(MAX_RANK)
        integer :: aindex, b, cindex

        if (.not. connection_valid(connection)) return
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        do aindex = 1, DIM
            do b = 1, DIM
                do cindex = 1, DIM
                    indices(1) = aindex
                    indices(2) = b
                    indices(3) = cindex
                    values(encode_index(indices, 3)) = &
                        connection%component(aindex, b, cindex) - &
                        connection%component(aindex, cindex, b)
                end do
            end do
        end do
        result = tensor_from_arena(connection%a, 3, values, variances)
    end function torsion_connection

    !> Q_ijk = -nabla_k g_ij, with the derivative slot last.
    function nonmetricity_connection_metric(connection, g) result(result)
        type(connection_t), intent(in) :: connection
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM), coordinates(DIM), metric_coordinates_value(DIM)
        type(expr_t) :: values(0:MAX_COMPONENTS - 1), value
        integer :: variances(MAX_RANK), indices(MAX_RANK)
        integer :: i, j, k, m

        if (.not. connection_valid(connection)) return
        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. metric_same_arena(g, connection%a)) return
        metric_coordinates_value = metric_coordinates(g)
        do k = 1, DIM
            if (metric_coordinates_value(k) /= connection%coordinate(k)) return
        end do

        metric = owner_metric_covariant(g)
        coordinates = connection%coordinate
        variances = 0
        variances(1) = LOWER_VARIANCE
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        do i = 1, DIM
            do j = 1, DIM
                do k = 1, DIM
                    value = -diff(metric(i, j), coordinates(k))
                    do m = 1, DIM
                        value = value + connection%component(m, k, i)*metric(m, j) + &
                            connection%component(m, k, j)*metric(i, m)
                    end do
                    indices(1) = i
                    indices(2) = j
                    indices(3) = k
                    values(encode_index(indices, 3)) = value
                end do
            end do
        end do
        result = tensor_from_arena(connection%a, 3, values, variances)
    end function nonmetricity_connection_metric

    !> Coordinate geodesic residual x''^a + Gamma^a_bc x'^b x'^c.
    !>
    !> The curve is expressed in the chart's coordinate variables.  The
    !> Christoffel symbols are evaluated on that curve before the residual is
    !> returned, matching the spacetime owner without coupling this module to
    !> a particular physics domain.
    function geodesic_residual_chart(c, curve, parameter) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: curve(DIM), parameter
        type(expr_t) :: value(DIM)
        type(expr_t) :: gamma(DIM, DIM, DIM), velocity(DIM), term
        integer :: a, b, k

        if (.not. associated(c%a)) return
        if (.not. is_valid(parameter)) return
        if (.not. same_arena(parameter, c%u(1))) return
        do k = 1, DIM
            if (.not. is_valid(curve(k))) return
            if (.not. same_arena(curve(k), c%u(1))) return
        end do

        gamma = chart_christoffel(c)
        do k = 1, DIM
            velocity(k) = diff(curve(k), parameter)
        end do
        do a = 1, DIM
            value(a) = diff(diff(curve(a), parameter), parameter)
            do b = 1, DIM
                do k = 1, DIM
                    term = substitute_curve(gamma(a, b, k), c%u, curve)
                    value(a) = value(a) + term*velocity(b)*velocity(k)
                end do
            end do
        end do
    end function geodesic_residual_chart

    !> Metric-owner overload of the coordinate geodesic residual.
    function geodesic_residual_metric(g, curve, parameter) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t), intent(in) :: curve(DIM), parameter
        type(expr_t) :: value(DIM)
        type(expr_t) :: coordinates(DIM), gamma(DIM, DIM, DIM)
        type(expr_t) :: velocity(DIM), term
        integer :: a, b, k

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        coordinates = metric_coordinates(g)
        if (.not. is_valid(parameter)) return
        if (.not. same_arena(parameter, coordinates(1))) return
        do k = 1, DIM
            if (.not. is_valid(curve(k))) return
            if (.not. same_arena(curve(k), coordinates(1))) return
        end do

        gamma = metric_christoffel_components(g)
        do k = 1, DIM
            velocity(k) = diff(curve(k), parameter)
        end do
        do a = 1, DIM
            value(a) = diff(diff(curve(a), parameter), parameter)
            do b = 1, DIM
                do k = 1, DIM
                    term = substitute_curve(gamma(a, b, k), coordinates, curve)
                    value(a) = value(a) + term*velocity(b)*velocity(k)
                end do
            end do
        end do
    end function geodesic_residual_metric

    !> Affine-connection overload of the coordinate geodesic residual.
    !>
    !> The supplied connection owns its coordinate tuple, so this view also
    !> supports torsionful or nonmetric connections without requiring a metric.
    function geodesic_residual_connection(connection, curve, parameter) result(value)
        type(connection_t), intent(in) :: connection
        type(expr_t), intent(in) :: curve(DIM), parameter
        type(expr_t) :: value(DIM)
        type(expr_t) :: velocity(DIM), term
        integer :: a, b, k

        if (.not. connection_valid(connection)) return
        if (.not. is_valid(parameter)) return
        if (.not. same_arena(parameter, connection%coordinate(1))) return
        do k = 1, DIM
            if (.not. is_valid(curve(k))) return
            if (.not. same_arena(curve(k), connection%coordinate(1))) return
        end do

        do k = 1, DIM
            velocity(k) = diff(curve(k), parameter)
        end do
        do a = 1, DIM
            value(a) = diff(diff(curve(a), parameter), parameter)
            do b = 1, DIM
                do k = 1, DIM
                    term = substitute_curve(connection%component(a, b, k), &
                        connection%coordinate, curve)
                    value(a) = value(a) + term*velocity(b)*velocity(k)
                end do
            end do
        end do
    end function geodesic_residual_connection

    function substitute_curve(expression, coordinates, curve) result(value)
        type(expr_t), intent(in) :: expression, coordinates(DIM), curve(DIM)
        type(expr_t) :: value
        integer :: k

        value = expression
        do k = 1, DIM
            value = subs(value, coordinates(k), curve(k))
        end do
    end function substitute_curve

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

    !> Full covariant derivative using a supplied affine connection.
    function covariant_diff_connection(connection, tensor_value) result(result)
        type(connection_t), intent(in) :: connection
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result

        if (.not. connection_valid(connection)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. associated(tensor_value%a, connection%a)) return
        result = covariant_diff_components(connection%a, connection%coordinate, &
            connection%component, tensor_value)
    end function covariant_diff_connection

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

    !> Readable alias for the supplied-connection covariant derivative.
    function covariant_derivative_connection(connection, tensor_value) result(result)
        type(connection_t), intent(in) :: connection
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result

        result = covariant_diff_connection(connection, tensor_value)
    end function covariant_derivative_connection

    !> Contract the first contravariant slot with the appended derivative
    !> slot. This is the physicists' divergence convention for a typed tensor.
    function covariant_divergence_chart(c, tensor_value) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result, differentiated
        integer :: rank

        if (.not. associated(c%a)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. tensor_same_arena(tensor_value, c)) return
        rank = tensor_rank(tensor_value)
        if (rank < 1) return
        if (tensor_variance(tensor_value, 1) /= UPPER) return
        differentiated = covariant_diff_chart(c, tensor_value)
        result = contract_slots(differentiated, 1, rank + 1)
    end function covariant_divergence_chart

    !> Metric-owner overload of the same first-slot divergence convention.
    function covariant_divergence_metric(g, tensor_value) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result, differentiated
        integer :: rank

        if (.not. metric_same_arena(g, tensor_value%a)) return
        if (.not. tensor_valid(tensor_value)) return
        rank = tensor_rank(tensor_value)
        if (rank < 1) return
        if (tensor_variance(tensor_value, 1) /= UPPER) return
        differentiated = covariant_diff_metric(g, tensor_value)
        result = contract_slots(differentiated, 1, rank + 1)
    end function covariant_divergence_metric

    !> Metric-free first-slot divergence for a supplied affine connection.
    function covariant_divergence_connection(connection, tensor_value) result(result)
        type(connection_t), intent(in) :: connection
        type(tensor_t), intent(in) :: tensor_value
        type(tensor_t) :: result, differentiated
        integer :: rank

        if (.not. connection_valid(connection)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. associated(tensor_value%a, connection%a)) return
        rank = tensor_rank(tensor_value)
        if (rank < 1) return
        if (tensor_variance(tensor_value, 1) /= UPPER) return
        differentiated = covariant_diff_connection(connection, tensor_value)
        result = contract_slots(differentiated, 1, rank + 1)
    end function covariant_divergence_connection

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
        do slot = 1, rank
            do m = 1, rank
                result%symmetry(slot, m) = tensor_value%symmetry(slot, m)
            end do
        end do
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

    !> Riemann tensor for a supplied affine connection.
    !>
    !> No metric is inferred here. The connection owns both the coefficients
    !> and the coordinate tuple, so torsionful or nonmetric connections can be
    !> differentiated without coupling this view to a metric owner.
    function riemann_tensor_connection(connection) result(result)
        type(connection_t), intent(in) :: connection
        type(tensor_t) :: result
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        integer :: variances(MAX_RANK)
        integer :: component

        if (.not. connection_valid(connection)) return
        call riemann_components(connection%component, connection%coordinate, values)
        if (connection%convention == CONNECTION_OPPOSITE) then
            do component = 0, DIM**4 - 1
                values(component) = -values(component)
            end do
        end if
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        variances(4) = LOWER_VARIANCE
        result = tensor_from_arena(connection%a, 4, values, variances)
    end function riemann_tensor_connection

    !> First Bianchi residual R^a_bcd + R^a_cdb + R^a_dbc.
    !>
    !> The residual is zero for a torsion-free connection. This owner is
    !> deliberately an identity residual rather than a Boolean so callers can
    !> inspect, simplify, or pass it to an independent verifier.
    function first_bianchi_residual_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result, riemann

        if (.not. associated(c%a)) return
        riemann = riemann_tensor_chart(c)
        result = first_bianchi_from_riemann(c%a, riemann)
    end function first_bianchi_residual_chart

    !> First Bianchi residual for a supplied coordinate-aware metric.
    function first_bianchi_residual_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result, riemann

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        riemann = riemann_tensor_metric(g)
        result = first_bianchi_from_riemann(metric_arena(g), riemann)
    end function first_bianchi_residual_metric

    !> Second differential Bianchi residual for a torsion-free metric
    !> connection:
    !>   nabla_e R^a_bcd + nabla_c R^a_bde + nabla_d R^a_bec = 0.
    function second_bianchi_residual_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result, riemann, differentiated

        if (.not. associated(c%a)) return
        riemann = riemann_tensor_chart(c)
        differentiated = covariant_diff_chart(c, riemann)
        result = second_bianchi_from_derivative(c%a, differentiated)
    end function second_bianchi_residual_chart

    !> Metric-owner overload of the second differential Bianchi residual.
    function second_bianchi_residual_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result, riemann, differentiated

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        riemann = riemann_tensor_metric(g)
        differentiated = covariant_diff_metric(g, riemann)
        result = second_bianchi_from_derivative(metric_arena(g), differentiated)
    end function second_bianchi_residual_metric

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

    function first_bianchi_from_riemann(owner, riemann) result(result)
        type(arena_t), pointer, intent(in) :: owner
        type(tensor_t), intent(in) :: riemann
        type(tensor_t) :: result
        type(expr_t) :: values(0:MAX_COMPONENTS - 1), value
        integer :: indices(MAX_RANK), four_indices(4), variances(MAX_RANK)
        integer :: a, b, cindex, d

        if (.not. tensor_valid(riemann)) return
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        variances(4) = LOWER_VARIANCE
        do a = 1, DIM
            do b = 1, DIM
                do cindex = 1, DIM
                    do d = 1, DIM
                        indices(1) = a
                        indices(2) = b
                        indices(3) = cindex
                        indices(4) = d
                        four_indices(1) = a
                        four_indices(2) = b
                        four_indices(3) = cindex
                        four_indices(4) = d
                        value = tensor_component(riemann, four_indices)
                        indices(2) = cindex
                        indices(3) = d
                        indices(4) = b
                        four_indices(2) = cindex
                        four_indices(3) = d
                        four_indices(4) = b
                        value = value + tensor_component(riemann, four_indices)
                        indices(2) = d
                        indices(3) = b
                        indices(4) = cindex
                        four_indices(2) = d
                        four_indices(3) = b
                        four_indices(4) = cindex
                        value = value + tensor_component(riemann, four_indices)
                        indices(2) = b
                        indices(3) = cindex
                        indices(4) = d
                        values(encode_index(indices, 4)) = value
                    end do
                end do
            end do
        end do
        result = tensor_from_arena(owner, 4, values, variances)
    end function first_bianchi_from_riemann

    function second_bianchi_from_derivative(owner, differentiated) result(result)
        type(arena_t), pointer, intent(in) :: owner
        type(tensor_t), intent(in) :: differentiated
        type(tensor_t) :: result
        type(expr_t) :: values(0:MAX_COMPONENTS - 1), value
        integer :: indices(5), variances(5)
        integer :: a, b, cindex, d, e

        if (.not. tensor_valid(differentiated)) return
        if (tensor_rank(differentiated) /= 5) return
        variances = 0
        variances(1) = UPPER
        variances(2) = LOWER_VARIANCE
        variances(3) = LOWER_VARIANCE
        variances(4) = LOWER_VARIANCE
        variances(5) = LOWER_VARIANCE
        do a = 1, DIM
            do b = 1, DIM
                do cindex = 1, DIM
                    do d = 1, DIM
                        do e = 1, DIM
                            ! The derivative slot is last:
                            ! differentiated(a,b,c,d,e) = nabla_e R^a_bcd.
                            indices(1) = a
                            indices(2) = b
                            indices(3) = cindex
                            indices(4) = d
                            indices(5) = e
                            value = tensor_component(differentiated, indices)

                            ! nabla_c R^a_bde.
                            indices(3) = d
                            indices(4) = e
                            indices(5) = cindex
                            value = value + tensor_component(differentiated, indices)

                            ! nabla_d R^a_bec.
                            indices(3) = e
                            indices(4) = cindex
                            indices(5) = d
                            value = value + tensor_component(differentiated, indices)

                            indices(3) = cindex
                            indices(4) = d
                            indices(5) = e
                            values(encode_index(indices, 5)) = value
                        end do
                    end do
                end do
            end do
        end do
        result = tensor_from_arena(owner, 5, values, variances)
    end function second_bianchi_from_derivative

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
