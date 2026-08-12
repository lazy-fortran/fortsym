module fortsym_tensor
    ! Typed coordinate tensors for the geometry toolkit.
    !
    ! Components live in one flat, fixed-capacity store, while rank, slot
    ! variance, and density weight remain explicit metadata. The cap is a
    ! deliberate native boundary: rank four is enough for the usual metric,
    ! stress-energy, connection, and curvature objects, and larger objects
    ! return an invalid tensor instead of silently losing index information.
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: chart_t, DIM, metric_covariant, &
        metric_contravariant
    use fortsym_expr, only: expr_t, num, is_valid, operator(+), operator(*)
    implicit none
    private

    integer, parameter, public :: UPPER = 1
    integer, parameter, public :: LOWER_VARIANCE = -1
    integer, parameter, public :: MAX_RANK = 4
    integer, parameter :: MAX_COMPONENTS = DIM**MAX_RANK

    public :: tensor_t, tensor, tensor_scalar, tensor_vector, tensor_covector
    public :: tensor_from_components, tensor_from_matrix
    public :: tensor_from_storage
    public :: tensor_component, tensor_rank, tensor_variance
    public :: tensor_density_weight, tensor_valid, tensor_same_arena, density
    public :: vector, covector, raise, lower
    public :: tensor_product, contract, trace
    public :: metric_covariant_tensor, metric_contravariant_tensor

    type :: tensor_t
        type(arena_t), pointer :: a => null()
        integer :: rank = -1
        integer :: variance(MAX_RANK) = 0
        integer :: density_weight = 0
        type(expr_t) :: component(0:MAX_COMPONENTS - 1)
    end type tensor_t

    interface tensor
        module procedure tensor_scalar
        module procedure tensor_from_components
    end interface tensor

    interface vector
        module procedure tensor_vector
    end interface vector

    interface covector
        module procedure tensor_covector
    end interface covector

contains

    !> Scalar tensor constructor. A scalar may itself carry density weight.
    function tensor_scalar(value, density_weight) result(result)
        type(expr_t), intent(in) :: value
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), weight

        if (.not. is_valid(value)) return
        metadata = 0
        weight = optional_weight(density_weight)
        result = zero_tensor(value%a, 0, metadata, weight)
        result%component(0) = value
    end function tensor_scalar

    !> Contravariant coordinate vector constructor.
    function tensor_vector(c, values, density_weight) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: values(DIM)
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK)
        integer :: i

        if (.not. associated(c%a)) return
        do i = 1, DIM
            if (.not. is_valid(values(i))) return
            if (.not. associated(values(i)%a, c%a)) return
        end do
        metadata = 0
        metadata(1) = UPPER
        result = zero_tensor(c%a, 1, metadata, optional_weight(density_weight))
        do i = 1, DIM
            result%component(i - 1) = values(i)
        end do
    end function tensor_vector

    !> Covariant coordinate one-form constructor.
    function tensor_covector(c, values, density_weight) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: values(DIM)
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK)
        integer :: i

        if (.not. associated(c%a)) return
        do i = 1, DIM
            if (.not. is_valid(values(i))) return
            if (.not. associated(values(i)%a, c%a)) return
        end do
        metadata = 0
        metadata(1) = LOWER_VARIANCE
        result = zero_tensor(c%a, 1, metadata, optional_weight(density_weight))
        do i = 1, DIM
            result%component(i - 1) = values(i)
        end do
    end function tensor_covector

    !> General flat component constructor. Values use the first-slot-fastest
    !> ordering produced by tensor_component indices.
    function tensor_from_components(c, rank, values, variance, density_weight) &
            result(result)
        type(chart_t), intent(in) :: c
        integer, intent(in) :: rank
        type(expr_t), intent(in) :: values(:)
        integer, intent(in) :: variance(:)
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), weight, k, count

        if (.not. associated(c%a)) return
        if (rank < 0 .or. rank > MAX_RANK) return
        if (size(variance) /= rank) return
        count = component_count(rank)
        if (size(values) /= count) return
        metadata = 0
        do k = 1, rank
            if (variance(k) /= UPPER .and. variance(k) /= LOWER_VARIANCE) return
            metadata(k) = variance(k)
        end do
        do k = 1, count
            if (.not. is_valid(values(k))) return
            if (.not. associated(values(k)%a, c%a)) return
        end do
        weight = optional_weight(density_weight)
        result = zero_tensor(c%a, rank, metadata, weight)
        do k = 1, count
            result%component(k - 1) = values(k)
        end do
    end function tensor_from_components

    !> Construct from the fixed native component store without passing an
    !> array section across a module boundary. This is the ownership-safe path
    !> for generated tensor transformations and keeps the component buffer
    !> explicit for compilers that warn about hidden temporaries.
    function tensor_from_storage(c, rank, values, variance, density_weight) &
            result(result)
        type(chart_t), intent(in) :: c
        integer, intent(in) :: rank
        type(expr_t), intent(in) :: values(0:MAX_COMPONENTS - 1)
        integer, intent(in) :: variance(MAX_RANK)
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), weight, k, count

        if (.not. associated(c%a)) return
        if (rank < 0 .or. rank > MAX_RANK) return
        count = component_count(rank)
        metadata = 0
        do k = 1, rank
            if (variance(k) /= UPPER .and. variance(k) /= LOWER_VARIANCE) return
            metadata(k) = variance(k)
        end do
        do k = 0, count - 1
            if (.not. is_valid(values(k))) return
            if (.not. associated(values(k)%a, c%a)) return
        end do
        weight = optional_weight(density_weight)
        result = zero_tensor(c%a, rank, metadata, weight)
        do k = 0, count - 1
            result%component(k) = values(k)
        end do
    end function tensor_from_storage

    !> Rank-two constructor with ordinary Fortran matrix indexing.
    function tensor_from_matrix(c, values, first_variance, second_variance, &
            density_weight) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: values(DIM, DIM)
        integer, intent(in) :: first_variance, second_variance
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), i, j, weight

        if (.not. associated(c%a)) return
        if (.not. valid_variance(first_variance)) return
        if (.not. valid_variance(second_variance)) return
        do i = 1, DIM
            do j = 1, DIM
                if (.not. is_valid(values(i, j))) return
                if (.not. associated(values(i, j)%a, c%a)) return
            end do
        end do
        metadata = 0
        metadata(1) = first_variance
        metadata(2) = second_variance
        weight = optional_weight(density_weight)
        result = zero_tensor(c%a, 2, metadata, weight)
        do i = 1, DIM
            do j = 1, DIM
                result%component(encode_pair(i, j)) = values(i, j)
            end do
        end do
    end function tensor_from_matrix

    !> Read a component by its ordered index tuple.
    function tensor_component(tensor_value, indices) result(value)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: indices(:)
        type(expr_t) :: value
        integer :: k

        if (.not. tensor_valid(tensor_value)) return
        if (size(indices) /= tensor_value%rank) return
        do k = 1, size(indices)
            if (indices(k) < 1 .or. indices(k) > DIM) return
        end do
        value = tensor_value%component(encode_index(indices, tensor_value%rank))
    end function tensor_component

    function tensor_rank(tensor_value) result(rank)
        type(tensor_t), intent(in) :: tensor_value
        integer :: rank

        rank = tensor_value%rank
    end function tensor_rank

    function tensor_variance(tensor_value, slot) result(variance)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        integer :: variance

        variance = 0
        if (.not. tensor_valid(tensor_value)) return
        if (slot < 1 .or. slot > tensor_value%rank) return
        variance = tensor_value%variance(slot)
    end function tensor_variance

    function tensor_density_weight(tensor_value) result(weight)
        type(tensor_t), intent(in) :: tensor_value
        integer :: weight

        weight = tensor_value%density_weight
    end function tensor_density_weight

    function tensor_same_arena(tensor_value, c) result(same)
        type(tensor_t), intent(in) :: tensor_value
        type(chart_t), intent(in) :: c
        logical :: same

        same = associated(tensor_value%a, c%a)
    end function tensor_same_arena

    function tensor_valid(tensor_value) result(valid)
        type(tensor_t), intent(in) :: tensor_value
        logical :: valid
        integer :: k

        valid = .false.
        if (.not. associated(tensor_value%a)) return
        if (tensor_value%rank < 0 .or. tensor_value%rank > MAX_RANK) return
        do k = 1, tensor_value%rank
            if (.not. valid_variance(tensor_value%variance(k))) return
        end do
        do k = 0, component_count(tensor_value%rank) - 1
            if (.not. is_valid(tensor_value%component(k))) return
        end do
        valid = .true.
    end function tensor_valid

    !> Change only the density metadata, preserving all components.
    function density(tensor_value, weight) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: weight
        type(tensor_t) :: result

        if (.not. tensor_valid(tensor_value)) return
        result = tensor_value
        result%density_weight = weight
    end function density

    !> Metric raise of one selected covariant slot.
    function raise(c, tensor_value, slot) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM)
        integer :: metadata(MAX_RANK), indices(MAX_RANK), old_indices(MAX_RANK)
        integer :: output_index, old_index, i, j

        if (.not. valid_metric_input(c, tensor_value, slot, LOWER_VARIANCE)) return
        metric = metric_contravariant(c)
        metadata = tensor_value%variance
        metadata(slot) = UPPER
        result = zero_tensor(c%a, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        do output_index = 0, component_count(tensor_value%rank) - 1
            call decode_index(output_index, tensor_value%rank, indices)
            result%component(output_index) = num(c%a, 0)
            do j = 1, DIM
                old_indices = indices
                old_indices(slot) = j
                old_index = encode_index(old_indices, tensor_value%rank)
                i = indices(slot)
                result%component(output_index) = result%component(output_index) + &
                    metric(i, j)*tensor_value%component(old_index)
            end do
        end do
    end function raise

    !> Metric lower of one selected contravariant slot.
    function lower(c, tensor_value, slot) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM)
        integer :: metadata(MAX_RANK), indices(MAX_RANK), old_indices(MAX_RANK)
        integer :: output_index, old_index, i, j

        if (.not. valid_metric_input(c, tensor_value, slot, UPPER)) return
        metric = metric_covariant(c)
        metadata = tensor_value%variance
        metadata(slot) = LOWER_VARIANCE
        result = zero_tensor(c%a, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        do output_index = 0, component_count(tensor_value%rank) - 1
            call decode_index(output_index, tensor_value%rank, indices)
            result%component(output_index) = num(c%a, 0)
            do j = 1, DIM
                old_indices = indices
                old_indices(slot) = j
                old_index = encode_index(old_indices, tensor_value%rank)
                i = indices(slot)
                result%component(output_index) = result%component(output_index) + &
                    metric(i, j)*tensor_value%component(old_index)
            end do
        end do
    end function lower

    !> Outer/tensor product. Slot order is left slots followed by right slots.
    function tensor_product(left, right) result(result)
        type(tensor_t), intent(in) :: left, right
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), left_count, right_count, i, j, k

        if (.not. tensor_valid(left)) return
        if (.not. tensor_valid(right)) return
        if (.not. associated(left%a, right%a)) return
        if (left%rank + right%rank > MAX_RANK) return
        metadata = 0
        do k = 1, left%rank
            metadata(k) = left%variance(k)
        end do
        do k = 1, right%rank
            metadata(left%rank + k) = right%variance(k)
        end do
        result = zero_tensor(left%a, left%rank + right%rank, metadata, &
            left%density_weight + right%density_weight)
        left_count = component_count(left%rank)
        right_count = component_count(right%rank)
        do i = 0, left_count - 1
            do j = 0, right_count - 1
                result%component(i + left_count*j) = &
                    left%component(i)*right%component(j)
            end do
        end do
    end function tensor_product

    !> Contract two opposite-variance slots, preserving all free slot order.
    function contract(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), free_indices(MAX_RANK)
        integer :: old_indices(MAX_RANK), free_slot, old_slot
        integer :: output_index, old_index, i

        if (.not. tensor_valid(tensor_value)) return
        if (tensor_value%rank < 2) return
        if (first < 1 .or. first > tensor_value%rank) return
        if (second < 1 .or. second > tensor_value%rank) return
        if (first == second) return
        if (tensor_value%variance(first) == tensor_value%variance(second)) return
        metadata = 0
        free_slot = 0
        do old_slot = 1, tensor_value%rank
            if (old_slot == first .or. old_slot == second) cycle
            free_slot = free_slot + 1
            metadata(free_slot) = tensor_value%variance(old_slot)
        end do
        result = zero_tensor(tensor_value%a, tensor_value%rank - 2, metadata, &
            tensor_value%density_weight)
        do output_index = 0, component_count(tensor_value%rank - 2) - 1
            call decode_index(output_index, tensor_value%rank - 2, free_indices)
            result%component(output_index) = num(tensor_value%a, 0)
            do i = 1, DIM
                free_slot = 0
                do old_slot = 1, tensor_value%rank
                    if (old_slot == first .or. old_slot == second) cycle
                    free_slot = free_slot + 1
                    old_indices(old_slot) = free_indices(free_slot)
                end do
                old_indices(first) = i
                old_indices(second) = i
                old_index = encode_index(old_indices, tensor_value%rank)
                result%component(output_index) = result%component(output_index) + &
                    tensor_value%component(old_index)
            end do
        end do
    end function contract

    function trace(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(tensor_t) :: result

        result = contract(tensor_value, first, second)
    end function trace

    function metric_covariant_tensor(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(expr_t) :: values(DIM, DIM)

        if (.not. associated(c%a)) return
        values = metric_covariant(c)
        result = tensor_from_matrix(c, values, LOWER_VARIANCE, LOWER_VARIANCE)
    end function metric_covariant_tensor

    function metric_contravariant_tensor(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(expr_t) :: values(DIM, DIM)

        if (.not. associated(c%a)) return
        values = metric_contravariant(c)
        result = tensor_from_matrix(c, values, UPPER, UPPER)
    end function metric_contravariant_tensor

    function zero_tensor(a, rank, metadata, weight) result(result)
        type(arena_t), pointer, intent(in) :: a
        integer, intent(in) :: rank, metadata(MAX_RANK), weight
        type(tensor_t) :: result
        integer :: k

        if (.not. associated(a)) return
        if (rank < 0 .or. rank > MAX_RANK) return
        result%a => a
        result%rank = rank
        result%variance = metadata
        result%density_weight = weight
        do k = 0, MAX_COMPONENTS - 1
            result%component(k) = num(a, 0)
        end do
    end function zero_tensor

    function valid_metric_input(c, tensor_value, slot, expected) result(valid)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot, expected
        logical :: valid

        valid = .false.
        if (.not. associated(c%a)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. associated(tensor_value%a, c%a)) return
        if (slot < 1 .or. slot > tensor_value%rank) return
        if (tensor_value%variance(slot) /= expected) return
        valid = .true.
    end function valid_metric_input

    pure function valid_variance(value) result(valid)
        integer, intent(in) :: value
        logical :: valid

        valid = value == UPPER .or. value == LOWER_VARIANCE
    end function valid_variance

    pure function optional_weight(value) result(weight)
        integer, optional, intent(in) :: value
        integer :: weight

        weight = 0
        if (present(value)) weight = value
    end function optional_weight

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

end module fortsym_tensor
