module fortsym_spacetime_tensor
    ! Runtime-dimension typed tensors for the relativity geometry owner.
    !
    ! The chart tensor owner deliberately remains a compact fixed-3D type.
    ! This owner has the same metadata contract but is dimensioned by the
    ! spacetime metric (1..4), so four-dimensional index operations do not
    ! silently pass through a three-dimensional representation.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, is_valid, same_arena, &
        operator(+), operator(-), operator(*)
    use fortsym_diff, only: diff
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_valid, spacetime_metric_arena, &
        spacetime_metric_dimension, spacetime_metric_covariant, &
        spacetime_metric_contravariant, spacetime_metric_has_coordinates, &
        spacetime_metric_coordinates, spacetime_christoffel
    implicit none
    private

    integer, parameter, public :: SPACETIME_TENSOR_MAX_RANK = 4
    integer, parameter :: MAX_COMPONENTS = SPACETIME_DIM**SPACETIME_TENSOR_MAX_RANK
    integer, parameter, public :: SPACETIME_UPPER = 1
    integer, parameter, public :: SPACETIME_LOWER = -1

    type, public :: spacetime_tensor_t
        private
        type(arena_t), pointer :: a => null()
        integer :: rank = -1
        integer :: dimension = 0
        integer :: variance(SPACETIME_TENSOR_MAX_RANK) = 0
        integer :: density_weight = 0
        type(expr_t) :: component(0:MAX_COMPONENTS - 1)
        logical :: valid = .false.
    end type spacetime_tensor_t

    public :: spacetime_tensor_scalar, spacetime_tensor_vector
    public :: spacetime_tensor_covector, spacetime_tensor_from_components
    public :: spacetime_tensor_from_storage
    public :: spacetime_metric_covariant_tensor
    public :: spacetime_metric_contravariant_tensor
    public :: spacetime_tensor_component, spacetime_tensor_component_flat
    public :: spacetime_tensor_rank, spacetime_tensor_dimension
    public :: spacetime_tensor_variance, spacetime_tensor_density_weight
    public :: spacetime_tensor_valid, spacetime_tensor_same_arena
    public :: spacetime_tensor_density, spacetime_tensor_density_factor
    public :: spacetime_tensor_raise, spacetime_tensor_lower
    public :: spacetime_tensor_product, spacetime_tensor_contract
    public :: spacetime_tensor_permute
    public :: spacetime_tensor_covariant_diff
    public :: spacetime_tensor_covariant_divergence
    public :: spacetime_tensor_lie_derivative
    public :: spacetime_killing

contains

    function spacetime_tensor_scalar(g, value, density_weight) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: value
        integer, optional, intent(in) :: density_weight
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)

        if (.not. spacetime_metric_valid(g)) return
        if (.not. is_valid(value)) return
        if (.not. same_arena(value, spacetime_metric_component(g))) return
        metadata = 0
        result = zero_tensor(spacetime_metric_arena(g), &
            spacetime_metric_dimension(g), 0, metadata, optional_weight(density_weight))
        result%component(0) = value
    end function spacetime_tensor_scalar

    function spacetime_tensor_vector(g, values, density_weight) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: values(SPACETIME_DIM)
        integer, optional, intent(in) :: density_weight
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), i, dimension

        if (.not. spacetime_metric_valid(g)) return
        dimension = spacetime_metric_dimension(g)
        do i = 1, dimension
            if (.not. is_valid(values(i))) return
            if (.not. same_arena(values(i), spacetime_metric_component(g))) return
        end do
        metadata = 0
        metadata(1) = SPACETIME_UPPER
        result = zero_tensor(spacetime_metric_arena(g), dimension, 1, metadata, &
            optional_weight(density_weight))
        do i = 1, dimension
            result%component(i - 1) = values(i)
        end do
    end function spacetime_tensor_vector

    function spacetime_tensor_covector(g, values, density_weight) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: values(SPACETIME_DIM)
        integer, optional, intent(in) :: density_weight
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), i, dimension

        if (.not. spacetime_metric_valid(g)) return
        dimension = spacetime_metric_dimension(g)
        do i = 1, dimension
            if (.not. is_valid(values(i))) return
            if (.not. same_arena(values(i), spacetime_metric_component(g))) return
        end do
        metadata = 0
        metadata(1) = SPACETIME_LOWER
        result = zero_tensor(spacetime_metric_arena(g), dimension, 1, metadata, &
            optional_weight(density_weight))
        do i = 1, dimension
            result%component(i - 1) = values(i)
        end do
    end function spacetime_tensor_covector

    function spacetime_tensor_from_components(g, rank, values, variance, &
            density_weight) result(result)
        type(spacetime_metric_t), intent(in) :: g
        integer, intent(in) :: rank
        type(expr_t), intent(in) :: values(:)
        integer, intent(in) :: variance(:)
        integer, optional, intent(in) :: density_weight
        type(spacetime_tensor_t) :: result
        type(expr_t) :: storage(0:MAX_COMPONENTS - 1)
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), count, k

        if (.not. spacetime_metric_valid(g)) return
        if (rank < 0 .or. rank > SPACETIME_TENSOR_MAX_RANK) return
        if (size(variance) /= rank) return
        count = component_count(rank, spacetime_metric_dimension(g))
        if (size(values) /= count) return
        storage = num(spacetime_metric_arena(g), 0)
        metadata = 0
        do k = 1, rank
            if (.not. valid_variance(variance(k))) return
            metadata(k) = variance(k)
        end do
        do k = 1, count
            if (.not. is_valid(values(k))) return
            if (.not. same_arena(values(k), spacetime_metric_component(g))) return
            storage(k - 1) = values(k)
        end do
        result = spacetime_tensor_from_storage(g, rank, storage, metadata, &
            density_weight)
    end function spacetime_tensor_from_components

    function spacetime_tensor_from_storage(g, rank, values, variance, &
            density_weight) result(result)
        type(spacetime_metric_t), intent(in) :: g
        integer, intent(in) :: rank
        type(expr_t), intent(in) :: values(0:MAX_COMPONENTS - 1)
        integer, intent(in) :: variance(SPACETIME_TENSOR_MAX_RANK)
        integer, optional, intent(in) :: density_weight
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), count, k

        if (.not. spacetime_metric_valid(g)) return
        if (rank < 0 .or. rank > SPACETIME_TENSOR_MAX_RANK) return
        count = component_count(rank, spacetime_metric_dimension(g))
        metadata = 0
        do k = 1, rank
            if (.not. valid_variance(variance(k))) return
            metadata(k) = variance(k)
        end do
        do k = 0, count - 1
            if (.not. is_valid(values(k))) return
            if (.not. same_arena(values(k), spacetime_metric_component(g))) return
        end do
        result = zero_tensor(spacetime_metric_arena(g), &
            spacetime_metric_dimension(g), rank, metadata, optional_weight(density_weight))
        do k = 0, count - 1
            result%component(k) = values(k)
        end do
    end function spacetime_tensor_from_storage

    function spacetime_metric_covariant_tensor(g) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), i, j, dimension
        type(expr_t) :: metric(SPACETIME_DIM, SPACETIME_DIM)

        if (.not. spacetime_metric_valid(g)) return
        dimension = spacetime_metric_dimension(g)
        metric = spacetime_metric_covariant(g)
        metadata = 0
        metadata(1:2) = SPACETIME_LOWER
        result = zero_tensor(spacetime_metric_arena(g), dimension, 2, metadata, 0)
        do j = 1, dimension
            do i = 1, dimension
                result%component(encode_pair(i, j, dimension)) = metric(i, j)
            end do
        end do
    end function spacetime_metric_covariant_tensor

    function spacetime_metric_contravariant_tensor(g) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), i, j, dimension
        type(expr_t) :: metric(SPACETIME_DIM, SPACETIME_DIM)

        if (.not. spacetime_metric_valid(g)) return
        dimension = spacetime_metric_dimension(g)
        metric = spacetime_metric_contravariant(g)
        metadata = 0
        metadata(1:2) = SPACETIME_UPPER
        result = zero_tensor(spacetime_metric_arena(g), dimension, 2, metadata, 0)
        do j = 1, dimension
            do i = 1, dimension
                result%component(encode_pair(i, j, dimension)) = metric(i, j)
            end do
        end do
    end function spacetime_metric_contravariant_tensor

    function spacetime_tensor_component(tensor_value, indices) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: indices(:)
        type(expr_t) :: value
        integer :: k, flat

        if (.not. spacetime_tensor_valid(tensor_value)) return
        if (size(indices) /= tensor_value%rank) return
        flat = 0
        do k = 1, tensor_value%rank
            if (indices(k) < 1 .or. indices(k) > tensor_value%dimension) return
            flat = flat + (indices(k) - 1)*tensor_value%dimension**(k - 1)
        end do
        value = tensor_value%component(flat)
    end function spacetime_tensor_component

    function spacetime_tensor_component_flat(tensor_value, indices) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: indices(SPACETIME_TENSOR_MAX_RANK)
        type(expr_t) :: value
        integer :: k, flat

        if (.not. spacetime_tensor_valid(tensor_value)) return
        flat = 0
        do k = 1, tensor_value%rank
            if (indices(k) < 1 .or. indices(k) > tensor_value%dimension) return
            flat = flat + (indices(k) - 1)*tensor_value%dimension**(k - 1)
        end do
        value = tensor_value%component(flat)
    end function spacetime_tensor_component_flat

    function spacetime_tensor_rank(tensor_value) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer :: value

        value = tensor_value%rank
    end function spacetime_tensor_rank

    function spacetime_tensor_dimension(tensor_value) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer :: value

        value = tensor_value%dimension
    end function spacetime_tensor_dimension

    function spacetime_tensor_variance(tensor_value, slot) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        integer :: value

        value = 0
        if (.not. spacetime_tensor_valid(tensor_value)) return
        if (slot < 1 .or. slot > tensor_value%rank) return
        value = tensor_value%variance(slot)
    end function spacetime_tensor_variance

    function spacetime_tensor_density_weight(tensor_value) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer :: value

        value = tensor_value%density_weight
    end function spacetime_tensor_density_weight

    function spacetime_tensor_valid(tensor_value) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        logical :: value
        integer :: k, count

        value = tensor_value%valid .and. associated(tensor_value%a)
        if (.not. value) return
        if (tensor_value%dimension < 1 .or. &
            tensor_value%dimension > SPACETIME_DIM) then
            value = .false.
            return
        end if
        if (tensor_value%rank < 0 .or. &
            tensor_value%rank > SPACETIME_TENSOR_MAX_RANK) then
            value = .false.
            return
        end if
        do k = 1, tensor_value%rank
            if (.not. valid_variance(tensor_value%variance(k))) then
                value = .false.
                return
            end if
        end do
        count = component_count(tensor_value%rank, tensor_value%dimension)
        do k = 0, count - 1
            if (.not. is_valid(tensor_value%component(k))) then
                value = .false.
                return
            end if
            if (.not. associated(tensor_value%component(k)%a, tensor_value%a)) then
                value = .false.
                return
            end if
        end do
    end function spacetime_tensor_valid

    function spacetime_tensor_same_arena(tensor_value, g) result(value)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        type(spacetime_metric_t), intent(in) :: g
        logical :: value

        value = spacetime_tensor_valid(tensor_value)
        if (.not. value) return
        value = spacetime_metric_valid(g)
        if (.not. value) return
        value = associated(tensor_value%a, spacetime_metric_arena(g))
        if (.not. value) return
        value = tensor_value%dimension == spacetime_metric_dimension(g)
    end function spacetime_tensor_same_arena

    function spacetime_tensor_density(tensor_value, weight) result(result)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: weight
        type(spacetime_tensor_t) :: result

        if (.not. spacetime_tensor_valid(tensor_value)) return
        result = tensor_value
        result%density_weight = weight
    end function spacetime_tensor_density

    function spacetime_tensor_density_factor(tensor_value, factor) result(result)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        type(expr_t), intent(in) :: factor
        type(spacetime_tensor_t) :: result
        integer :: k, count

        if (.not. spacetime_tensor_valid(tensor_value)) return
        if (.not. is_valid(factor)) return
        if (.not. associated(factor%a, tensor_value%a)) return
        result = tensor_value
        result%density_weight = tensor_value%density_weight + 1
        count = component_count(tensor_value%rank, tensor_value%dimension)
        do k = 0, count - 1
            result%component(k) = factor*tensor_value%component(k)
        end do
    end function spacetime_tensor_density_factor

    function spacetime_tensor_raise(g, tensor_value, slot) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(spacetime_tensor_t) :: result
        type(expr_t) :: metric(SPACETIME_DIM, SPACETIME_DIM), term
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: old_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: count, output_index, j, dimension

        if (.not. spacetime_tensor_same_arena(tensor_value, g)) return
        if (slot < 1 .or. slot > tensor_value%rank) return
        if (tensor_value%variance(slot) /= SPACETIME_LOWER) return
        dimension = tensor_value%dimension
        metric = spacetime_metric_contravariant(g)
        metadata = tensor_value%variance
        metadata(slot) = SPACETIME_UPPER
        result = zero_tensor(tensor_value%a, dimension, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        count = component_count(tensor_value%rank, dimension)
        do output_index = 0, count - 1
            call decode_index(output_index, tensor_value%rank, indices, dimension)
            term = num(tensor_value%a, 0)
            do j = 1, dimension
                old_indices = indices
                old_indices(slot) = j
                term = term + metric(indices(slot), j)* &
                    tensor_value%component(encode_index(old_indices, tensor_value%rank, dimension))
            end do
            result%component(output_index) = term
        end do
    end function spacetime_tensor_raise

    function spacetime_tensor_lower(g, tensor_value, slot) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(spacetime_tensor_t) :: result
        type(expr_t) :: metric(SPACETIME_DIM, SPACETIME_DIM), term
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: old_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: count, output_index, j, dimension

        if (.not. spacetime_tensor_same_arena(tensor_value, g)) return
        if (slot < 1 .or. slot > tensor_value%rank) return
        if (tensor_value%variance(slot) /= SPACETIME_UPPER) return
        dimension = tensor_value%dimension
        metric = spacetime_metric_covariant(g)
        metadata = tensor_value%variance
        metadata(slot) = SPACETIME_LOWER
        result = zero_tensor(tensor_value%a, dimension, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        count = component_count(tensor_value%rank, dimension)
        do output_index = 0, count - 1
            call decode_index(output_index, tensor_value%rank, indices, dimension)
            term = num(tensor_value%a, 0)
            do j = 1, dimension
                old_indices = indices
                old_indices(slot) = j
                term = term + metric(indices(slot), j)* &
                    tensor_value%component(encode_index(old_indices, tensor_value%rank, dimension))
            end do
            result%component(output_index) = term
        end do
    end function spacetime_tensor_lower

    function spacetime_tensor_product(left, right) result(result)
        type(spacetime_tensor_t), intent(in) :: left, right
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK), left_count, right_count
        integer :: k, i, j

        if (.not. spacetime_tensor_valid(left)) return
        if (.not. spacetime_tensor_valid(right)) return
        if (.not. associated(left%a, right%a)) return
        if (left%dimension /= right%dimension) return
        if (left%rank + right%rank > SPACETIME_TENSOR_MAX_RANK) return
        metadata = 0
        do k = 1, left%rank
            metadata(k) = left%variance(k)
        end do
        do k = 1, right%rank
            metadata(left%rank + k) = right%variance(k)
        end do
        result = zero_tensor(left%a, left%dimension, left%rank + right%rank, &
            metadata, left%density_weight + right%density_weight)
        left_count = component_count(left%rank, left%dimension)
        right_count = component_count(right%rank, right%dimension)
        do j = 0, right_count - 1
            do i = 0, left_count - 1
                result%component(i + left_count*j) = left%component(i)* &
                    right%component(j)
            end do
        end do
    end function spacetime_tensor_product

    function spacetime_tensor_contract(tensor_value, first, second) result(result)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)
        integer :: free_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: old_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: free_slot, old_slot, output_index, old_index, i, dimension

        if (.not. spacetime_tensor_valid(tensor_value)) return
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
        dimension = tensor_value%dimension
        result = zero_tensor(tensor_value%a, dimension, tensor_value%rank - 2, &
            metadata, tensor_value%density_weight)
        do output_index = 0, component_count(tensor_value%rank - 2, dimension) - 1
            call decode_index(output_index, tensor_value%rank - 2, free_indices, dimension)
            result%component(output_index) = num(tensor_value%a, 0)
            do i = 1, dimension
                free_slot = 0
                old_indices = 1
                do old_slot = 1, tensor_value%rank
                    if (old_slot == first .or. old_slot == second) cycle
                    free_slot = free_slot + 1
                    old_indices(old_slot) = free_indices(free_slot)
                end do
                old_indices(first) = i
                old_indices(second) = i
                old_index = encode_index(old_indices, tensor_value%rank, dimension)
                result%component(output_index) = result%component(output_index) + &
                    tensor_value%component(old_index)
            end do
        end do
    end function spacetime_tensor_contract

    function spacetime_tensor_permute(tensor_value, order) result(result)
        type(spacetime_tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: order(:)
        type(spacetime_tensor_t) :: result
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)
        integer :: output_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: source_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: slot, output_index, source_index, dimension

        if (.not. spacetime_tensor_valid(tensor_value)) return
        if (.not. valid_permutation(order, tensor_value%rank)) return
        dimension = tensor_value%dimension
        metadata = 0
        do slot = 1, tensor_value%rank
            metadata(slot) = tensor_value%variance(order(slot))
        end do
        result = zero_tensor(tensor_value%a, dimension, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        do output_index = 0, component_count(tensor_value%rank, dimension) - 1
            call decode_index(output_index, tensor_value%rank, output_indices, dimension)
            source_indices = 1
            do slot = 1, tensor_value%rank
                source_indices(order(slot)) = output_indices(slot)
            end do
            source_index = encode_index(source_indices, tensor_value%rank, dimension)
            result%component(output_index) = tensor_value%component(source_index)
        end do
    end function spacetime_tensor_permute

    !> Full metric covariant derivative with the derivative slot last.
    !>
    !> A tensor density of weight w receives the physicists' transport term
    !> -w Gamma^m_mk T. The native rank bound is deliberate: a rank-r input
    !> produces rank r+1, and the owner stores at most four slots.
    function spacetime_tensor_covariant_diff(g, tensor_value) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: tensor_value
        type(spacetime_tensor_t) :: result
        type(expr_t) :: coordinates(SPACETIME_DIM)
        type(expr_t) :: gamma(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        type(expr_t) :: base, term, trace_gamma(SPACETIME_DIM)
        integer :: rank, output_rank, count, output_index, dimension
        integer :: indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: old_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)
        integer :: k, i, m, slot, weight

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        if (.not. spacetime_tensor_same_arena(tensor_value, g)) return
        rank = tensor_value%rank
        if (rank < 0 .or. rank >= SPACETIME_TENSOR_MAX_RANK) return
        dimension = spacetime_metric_dimension(g)
        output_rank = rank + 1
        count = component_count(output_rank, dimension)
        coordinates = spacetime_metric_coordinates(g)
        gamma = spacetime_christoffel(g)
        metadata = 0
        do slot = 1, rank
            metadata(slot) = tensor_value%variance(slot)
        end do
        metadata(output_rank) = SPACETIME_LOWER
        weight = tensor_value%density_weight
        values = num(tensor_value%a, 0)
        if (weight /= 0) then
            do k = 1, dimension
                trace_gamma(k) = num(tensor_value%a, 0)
                do m = 1, dimension
                    trace_gamma(k) = trace_gamma(k) + gamma(m, m, k)
                end do
            end do
        end if

        do output_index = 0, count - 1
            call decode_index(output_index, output_rank, indices, dimension)
            k = indices(output_rank)
            base = tensor_value%component(encode_index(indices, rank, dimension))
            term = diff(base, coordinates(k))
            if (weight /= 0) then
                term = term - num(tensor_value%a, weight)*trace_gamma(k)*base
            end if
            do slot = 1, rank
                i = indices(slot)
                do m = 1, dimension
                    old_indices = indices
                    old_indices(slot) = m
                    if (tensor_value%variance(slot) == SPACETIME_UPPER) then
                        term = term + gamma(i, k, m)* &
                            tensor_value%component(encode_index(old_indices, rank, dimension))
                    else
                        term = term - gamma(m, k, i)* &
                            tensor_value%component(encode_index(old_indices, rank, dimension))
                    end if
                end do
            end do
            values(output_index) = term
        end do

        result = spacetime_tensor_from_storage(g, output_rank, values, metadata, weight)
    end function spacetime_tensor_covariant_diff

    !> Contract the first upper tensor slot with the derivative slot.
    !>
    !> This is evaluated directly instead of constructing the full covariant
    !> derivative and then contracting it, which keeps the symbolic DAG and
    !> intermediate component storage proportional to the result rank.
    function spacetime_tensor_covariant_divergence(g, tensor_value) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: tensor_value
        type(spacetime_tensor_t) :: result
        type(expr_t) :: coordinates(SPACETIME_DIM)
        type(expr_t) :: gamma(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: base, term, trace_gamma(SPACETIME_DIM)
        integer :: rank, output_rank, count, output_index, dimension
        integer :: output_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: old_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)
        integer :: i, k, m, slot, weight

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        if (.not. spacetime_tensor_same_arena(tensor_value, g)) return
        rank = tensor_value%rank
        if (rank < 1) return
        if (tensor_value%variance(1) /= SPACETIME_UPPER) return
        dimension = spacetime_metric_dimension(g)
        output_rank = rank - 1
        count = component_count(output_rank, dimension)
        coordinates = spacetime_metric_coordinates(g)
        gamma = spacetime_christoffel(g)
        metadata = 0
        do slot = 2, rank
            metadata(slot - 1) = tensor_value%variance(slot)
        end do
        weight = tensor_value%density_weight
        if (weight /= 0) then
            do i = 1, dimension
                trace_gamma(i) = num(tensor_value%a, 0)
                do m = 1, dimension
                    trace_gamma(i) = trace_gamma(i) + gamma(m, m, i)
                end do
            end do
        end if
        result = zero_tensor(tensor_value%a, dimension, output_rank, metadata, weight)

        do output_index = 0, count - 1
            call decode_index(output_index, output_rank, output_indices, dimension)
            result%component(output_index) = num(tensor_value%a, 0)
            do i = 1, dimension
                indices(1) = i
                do slot = 2, rank
                    indices(slot) = output_indices(slot - 1)
                end do
                base = tensor_value%component(encode_index(indices, rank, dimension))
                term = diff(base, coordinates(i))
                do m = 1, dimension
                    old_indices = indices
                    old_indices(1) = m
                    term = term + gamma(i, i, m)* &
                        tensor_value%component(encode_index(old_indices, rank, dimension))
                end do
                do slot = 2, rank
                    k = indices(slot)
                    do m = 1, dimension
                        old_indices = indices
                        old_indices(slot) = m
                        if (tensor_value%variance(slot) == SPACETIME_UPPER) then
                            term = term + gamma(k, i, m)* &
                                tensor_value%component(encode_index(old_indices, rank, dimension))
                        else
                            term = term - gamma(m, i, k)* &
                                tensor_value%component(encode_index(old_indices, rank, dimension))
                        end if
                    end do
                end do
                if (weight /= 0) then
                    term = term - num(tensor_value%a, weight)*trace_gamma(i)*base
                end if
                result%component(output_index) = result%component(output_index) + term
            end do
        end do
    end function spacetime_tensor_covariant_divergence

    !> Coordinate Lie derivative of a typed spacetime tensor.
    !>
    !> The vector is an ordinary contravariant, weight-zero field.  A tensor
    !> density of weight w receives +w T d_k X^k, while upper slots receive
    !> -T^k d_k X^a and lower slots receive +T_k d_b X^k.
    function spacetime_tensor_lie_derivative(g, vector_value, tensor_value) &
            result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: vector_value, tensor_value
        type(spacetime_tensor_t) :: result
        type(expr_t) :: coordinates(SPACETIME_DIM)
        type(expr_t) :: values(0:MAX_COMPONENTS - 1)
        type(expr_t) :: base, term, divergence
        integer :: rank, count, output_index, dimension
        integer :: indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: old_indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)
        integer :: i, k, slot, weight

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        if (.not. spacetime_tensor_same_arena(vector_value, g)) return
        if (.not. spacetime_tensor_same_arena(tensor_value, g)) return
        if (vector_value%rank /= 1) return
        if (vector_value%variance(1) /= SPACETIME_UPPER) return
        if (vector_value%density_weight /= 0) return
        dimension = spacetime_metric_dimension(g)
        rank = tensor_value%rank
        count = component_count(rank, dimension)
        coordinates = spacetime_metric_coordinates(g)
        metadata = tensor_value%variance
        weight = tensor_value%density_weight
        values = num(tensor_value%a, 0)
        divergence = num(tensor_value%a, 0)
        do k = 1, dimension
            divergence = divergence + diff(vector_value%component(k - 1), &
                coordinates(k))
        end do

        do output_index = 0, count - 1
            call decode_index(output_index, rank, indices, dimension)
            base = tensor_value%component(encode_index(indices, rank, dimension))
            term = num(tensor_value%a, 0)
            do k = 1, dimension
                term = term + vector_value%component(k - 1)* &
                    diff(base, coordinates(k))
            end do
            do slot = 1, rank
                i = indices(slot)
                do k = 1, dimension
                    old_indices = indices
                    old_indices(slot) = k
                    if (tensor_value%variance(slot) == SPACETIME_UPPER) then
                        term = term - tensor_value%component( &
                            encode_index(old_indices, rank, dimension))* &
                            diff(vector_value%component(i - 1), coordinates(k))
                    else
                        term = term + tensor_value%component( &
                            encode_index(old_indices, rank, dimension))* &
                            diff(vector_value%component(k - 1), coordinates(i))
                    end if
                end do
            end do
            if (weight /= 0) then
                term = term + num(tensor_value%a, weight)*base*divergence
            end if
            values(output_index) = term
        end do

        result = spacetime_tensor_from_storage(g, rank, values, metadata, weight)
    end function spacetime_tensor_lie_derivative

    function spacetime_killing(g, vector_value) result(result)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: vector_value
        type(spacetime_tensor_t) :: result
        type(spacetime_tensor_t) :: metric_value

        metric_value = spacetime_metric_covariant_tensor(g)
        result = spacetime_tensor_lie_derivative(g, vector_value, metric_value)
    end function spacetime_killing

    function zero_tensor(a, dimension, rank, metadata, weight) result(result)
        type(arena_t), pointer, intent(in) :: a
        integer, intent(in) :: dimension, rank
        integer, intent(in) :: metadata(SPACETIME_TENSOR_MAX_RANK), weight
        type(spacetime_tensor_t) :: result
        integer :: k

        if (.not. associated(a)) return
        if (dimension < 1 .or. dimension > SPACETIME_DIM) return
        if (rank < 0 .or. rank > SPACETIME_TENSOR_MAX_RANK) return
        result%a => a
        result%dimension = dimension
        result%rank = rank
        result%variance = metadata
        result%density_weight = weight
        do k = 0, MAX_COMPONENTS - 1
            result%component(k) = num(a, 0)
        end do
        result%valid = .true.
    end function zero_tensor

    function spacetime_metric_component(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value
        type(expr_t) :: metric(SPACETIME_DIM, SPACETIME_DIM)

        metric = spacetime_metric_covariant(g)
        value = metric(1, 1)
    end function spacetime_metric_component

    pure function component_count(rank, dimension) result(count)
        integer, intent(in) :: rank, dimension
        integer :: count, k

        count = 1
        do k = 1, rank
            count = count*dimension
        end do
    end function component_count

    pure integer function encode_pair(first, second, dimension)
        integer, intent(in) :: first, second, dimension

        encode_pair = (first - 1) + dimension*(second - 1)
    end function encode_pair

    pure integer function encode_index(indices, rank, dimension) result(index)
        integer, intent(in) :: indices(SPACETIME_TENSOR_MAX_RANK)
        integer, intent(in) :: rank, dimension
        integer :: scale, k

        index = 0
        scale = 1
        do k = 1, rank
            index = index + (indices(k) - 1)*scale
            scale = scale*dimension
        end do
    end function encode_index

    pure subroutine decode_index(index, rank, indices, dimension)
        integer, intent(in) :: index, rank, dimension
        integer, intent(out) :: indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: value, k

        indices = 1
        value = index
        do k = 1, rank
            indices(k) = mod(value, dimension) + 1
            value = value/dimension
        end do
    end subroutine decode_index

    pure logical function valid_variance(value)
        integer, intent(in) :: value

        valid_variance = value == SPACETIME_UPPER .or. value == SPACETIME_LOWER
    end function valid_variance

    pure integer function optional_weight(value)
        integer, optional, intent(in) :: value

        optional_weight = 0
        if (present(value)) optional_weight = value
    end function optional_weight

    pure logical function valid_permutation(order, rank)
        integer, intent(in) :: order(:), rank
        integer :: i, j

        valid_permutation = size(order) == rank
        if (.not. valid_permutation) return
        do i = 1, rank
            if (order(i) < 1 .or. order(i) > rank) then
                valid_permutation = .false.
                return
            end if
            do j = i + 1, rank
                if (order(i) == order(j)) then
                    valid_permutation = .false.
                    return
                end if
            end do
        end do
    end function valid_permutation

end module fortsym_spacetime_tensor
