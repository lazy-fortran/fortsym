module fortsym_tensor
    ! Typed coordinate tensors for the geometry toolkit.
    !
    ! Components live in one flat, fixed-capacity store, while rank, slot
    ! variance, and density weight remain explicit metadata. The cap is a
    ! deliberate native boundary: rank five is enough for the usual metric,
    ! stress-energy, connection, curvature, and second-Bianchi objects, and
    ! larger objects
    ! return an invalid tensor instead of silently losing index information.
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: chart_t, DIM, metric_covariant, &
        metric_contravariant
    use fortsym_metric, only: metric_t, metric_valid, metric_same_arena, &
        metric_arena, owner_metric_covariant => metric_covariant, &
        owner_metric_contravariant => metric_contravariant
    use fortsym_diff, only: diff
    use fortsym_index, only: index_t, index_valid, index_dimension, index_slot, &
        index_variance, compatible_indices
    use fortsym_expr, only: expr_t, num, is_valid, operator(+), operator(-), &
        operator(*), &
        operator(/), operator(==)
    implicit none
    private

    integer, parameter, public :: UPPER = 1
    integer, parameter, public :: LOWER_VARIANCE = -1
    integer, parameter, public :: SYMMETRY_NONE = 0
    integer, parameter, public :: SYMMETRIC = 1
    integer, parameter, public :: ANTISYMMETRIC = -1
    integer, parameter, public :: MAX_RANK = 5
    integer, parameter :: MAX_COMPONENTS = DIM**MAX_RANK

    public :: tensor_t, tensor, tensor_scalar, tensor_vector, tensor_covector
    public :: tensor_from_components, tensor_from_matrix
    public :: tensor_from_storage, tensor_from_arena
    public :: tensor_component, tensor_rank, tensor_variance
    public :: tensor_symmetry, declare_symmetry
    public :: tensor_density_weight, tensor_valid, tensor_same_arena, density, &
        density_factor
    public :: vector, covector, raise, lower
    public :: tensor_product, contract, contract_slots, trace
    public :: permute, symmetrize, antisymmetrize
    public :: tensor_lie_derivative
    public :: metric_covariant_tensor, metric_contravariant_tensor

    type :: tensor_t
        type(arena_t), pointer :: a => null()
        integer :: rank = -1
        integer :: variance(MAX_RANK) = 0
        integer :: symmetry(MAX_RANK, MAX_RANK) = 0
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

    interface raise
        module procedure raise_chart
        module procedure raise_metric
    end interface raise

    interface lower
        module procedure lower_chart
        module procedure lower_metric
    end interface lower

    interface contract
        module procedure contract_slots
        module procedure contract_indices
    end interface contract

    interface density
        module procedure density_weight
        module procedure density_factor
    end interface density

    interface metric_covariant_tensor
        module procedure metric_covariant_tensor_chart
        module procedure metric_covariant_tensor_metric
    end interface metric_covariant_tensor

    interface metric_contravariant_tensor
        module procedure metric_contravariant_tensor_chart
        module procedure metric_contravariant_tensor_metric
    end interface metric_contravariant_tensor

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

    !> Construct a typed tensor from an arena-owned component store.
    !>
    !> This is the owner-safe path for objects such as a supplied metric or
    !> connection that have coordinates and an arena but no chart embedding.
    function tensor_from_arena(a, rank, values, variance, density_weight) &
            result(result)
        type(arena_t), pointer, intent(in) :: a
        integer, intent(in) :: rank
        type(expr_t), intent(in) :: values(0:MAX_COMPONENTS - 1)
        integer, intent(in) :: variance(MAX_RANK)
        integer, optional, intent(in) :: density_weight
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), weight, k, count

        if (.not. associated(a)) return
        if (rank < 0 .or. rank > MAX_RANK) return
        count = component_count(rank)
        metadata = 0
        do k = 1, rank
            if (variance(k) /= UPPER .and. variance(k) /= LOWER_VARIANCE) return
            metadata(k) = variance(k)
        end do
        do k = 0, count - 1
            if (.not. is_valid(values(k))) return
            if (.not. associated(values(k)%a, a)) return
        end do
        weight = optional_weight(density_weight)
        result = zero_tensor(a, rank, metadata, weight)
        do k = 0, count - 1
            result%component(k) = values(k)
        end do
    end function tensor_from_arena

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

    !> Return the declared pair symmetry of two tensor slots.
    function tensor_symmetry(tensor_value, first, second) result(kind)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        integer :: kind

        kind = SYMMETRY_NONE
        if (.not. tensor_valid(tensor_value)) return
        if (first < 1 .or. first > tensor_value%rank) return
        if (second < 1 .or. second > tensor_value%rank) return
        if (first == second) return
        kind = tensor_value%symmetry(first, second)
    end function tensor_symmetry

    !> Declare a pair symmetry after checking every stored component.
    !>
    !> A declaration is a mathematical promise, so arbitrary components are
    !> refused rather than projected. Symmetric and antisymmetric slots must
    !> have equal variance, matching the ordinary indexed-tensor convention.
    function declare_symmetry(tensor_value, first, second, kind) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second, kind
        type(tensor_t) :: result
        integer :: indices(MAX_RANK), swapped(MAX_RANK), flat, count
        type(expr_t) :: left, right, zero

        if (.not. tensor_valid(tensor_value)) return
        if (first < 1 .or. first > tensor_value%rank) return
        if (second < 1 .or. second > tensor_value%rank) return
        if (first == second) return
        if (kind /= SYMMETRIC .and. kind /= ANTISYMMETRIC) return
        if (tensor_value%variance(first) /= tensor_value%variance(second)) return

        count = component_count(tensor_value%rank)
        zero = num(tensor_value%a, 0)
        do flat = 0, count - 1
            call decode_index(flat, tensor_value%rank, indices)
            swapped = indices
            swapped(first) = indices(second)
            swapped(second) = indices(first)
            left = tensor_value%component(flat)
            right = tensor_value%component(encode_index(swapped, tensor_value%rank))
            if (kind == SYMMETRIC) then
                if (.not. (left == right)) return
            else
                if (indices(first) == indices(second)) then
                    if (.not. (left == zero)) return
                else if (.not. (left == -right)) then
                    return
                end if
            end if
        end do

        result = tensor_value
        result%symmetry(first, second) = kind
        result%symmetry(second, first) = kind
    end function declare_symmetry

    function tensor_same_arena(tensor_value, c) result(same)
        type(tensor_t), intent(in) :: tensor_value
        type(chart_t), intent(in) :: c
        logical :: same

        same = associated(tensor_value%a, c%a)
    end function tensor_same_arena

    function tensor_valid(tensor_value) result(valid)
        type(tensor_t), intent(in) :: tensor_value
        logical :: valid
        integer :: k, j

        valid = .false.
        if (.not. associated(tensor_value%a)) return
        if (tensor_value%rank < 0 .or. tensor_value%rank > MAX_RANK) return
        do k = 1, tensor_value%rank
            if (.not. valid_variance(tensor_value%variance(k))) return
        end do
        do k = 1, tensor_value%rank
            if (tensor_value%symmetry(k, k) /= SYMMETRY_NONE) return
            do j = 1, tensor_value%rank
                if (.not. valid_symmetry_value(tensor_value%symmetry(k, j))) return
                if (tensor_value%symmetry(k, j) /= tensor_value%symmetry(j, k)) return
                if (tensor_value%symmetry(k, j) /= SYMMETRY_NONE) then
                    if (tensor_value%variance(k) /= tensor_value%variance(j)) return
                end if
            end do
        end do
        do k = 0, component_count(tensor_value%rank) - 1
            if (.not. is_valid(tensor_value%component(k))) return
        end do
        valid = .true.
    end function tensor_valid

    !> Change only the density metadata, preserving all components.
    function density_weight(tensor_value, weight) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: weight
        type(tensor_t) :: result

        if (.not. tensor_valid(tensor_value)) return
        result = tensor_value
        result%density_weight = weight
    end function density_weight

    !> Multiply components by one scalar density factor and increment weight.
    !>
    !> This is the generic owner for objects such as `sqrtg B^i`. The factor
    !> is an expression in the tensor's arena, so the operation preserves the
    !> existing component representation and adds no parallel density store.
    function density_factor(tensor_value, factor) result(result)
        type(tensor_t), intent(in) :: tensor_value
        type(expr_t), intent(in) :: factor
        type(tensor_t) :: result
        integer :: k

        if (.not. tensor_valid(tensor_value)) return
        if (.not. is_valid(factor)) return
        if (.not. associated(factor%a, tensor_value%a)) return
        result = tensor_value
        result%density_weight = tensor_value%density_weight + 1
        do k = 0, component_count(tensor_value%rank) - 1
            result%component(k) = factor*tensor_value%component(k)
        end do
    end function density_factor

    !> Coordinate Lie derivative of a typed tensor along an ordinary vector.
    !>
    !> For a tensor density of weight w the coordinate expression is
    !>   (L_X T)^a..._b... = X^k d_k T^a..._b...
    !>       - T^k... d_k X^a + T^...k d_b X^k
    !>       + w T^a..._b... d_k X^k.
    !> The vector is deliberately required to have zero density weight: a
    !> density is not a vector field and silently accepting one would make
    !> the transport law ambiguous. Components are written directly into the
    !> fixed tensor store; no component arrays or rank-dependent temporaries
    !> are formed in the contraction loops.
    function tensor_lie_derivative(c, vector_value, tensor_value) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: vector_value, tensor_value
        type(tensor_t) :: result
        type(expr_t) :: base, term, divergence
        integer :: rank, count, output_index, indices(MAX_RANK)
        integer :: old_indices(MAX_RANK), slot, i, k, variance

        if (.not. associated(c%a)) return
        if (.not. tensor_valid(vector_value)) return
        if (.not. tensor_valid(tensor_value)) return
        if (.not. associated(vector_value%a, c%a)) return
        if (.not. associated(tensor_value%a, c%a)) return
        if (tensor_rank(vector_value) /= 1) return
        if (tensor_variance(vector_value, 1) /= UPPER) return
        if (tensor_density_weight(vector_value) /= 0) return

        rank = tensor_rank(tensor_value)
        count = component_count(rank)
        result = zero_tensor(c%a, rank, tensor_value%variance, &
            tensor_density_weight(tensor_value))
        result%symmetry = tensor_value%symmetry
        divergence = num(c%a, 0)
        do k = 1, DIM
            divergence = divergence + diff(vector_value%component(k - 1), c%u(k))
        end do

        do output_index = 0, count - 1
            call decode_index(output_index, rank, indices)
            if (rank == 0) then
                base = tensor_value%component(0)
            else
                base = tensor_value%component(encode_index(indices, rank))
            end if
            term = num(c%a, 0)
            do k = 1, DIM
                term = term + vector_value%component(k - 1)* &
                    diff(base, c%u(k))
            end do
            do slot = 1, rank
                i = indices(slot)
                variance = tensor_variance(tensor_value, slot)
                do k = 1, DIM
                    old_indices = indices
                    old_indices(slot) = k
                    if (variance == UPPER) then
                        term = term - tensor_value%component( &
                            encode_index(old_indices, rank))* &
                            diff(vector_value%component(i - 1), c%u(k))
                    else
                        term = term + tensor_value%component( &
                            encode_index(old_indices, rank))* &
                            diff(vector_value%component(k - 1), c%u(i))
                    end if
                end do
            end do
            if (tensor_density_weight(tensor_value) /= 0) then
                term = term + num(c%a, tensor_density_weight(tensor_value))* &
                    base*divergence
            end if
            result%component(output_index) = term
        end do
    end function tensor_lie_derivative

    !> Metric raise of one selected covariant slot.
    function raise_chart(c, tensor_value, slot) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM)

        if (.not. valid_metric_input(c, tensor_value, slot, LOWER_VARIANCE)) return
        metric = metric_contravariant(c)
        result = raise_components(tensor_value, metric, slot)
    end function raise_chart

    !> Metric-owner form of raise; the tensor and metric must share an arena.
    function raise_metric(g, tensor_value, slot) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM)

        if (.not. metric_same_arena(g, tensor_value%a)) return
        if (.not. valid_tensor_slot(tensor_value, slot, LOWER_VARIANCE)) return
        metric = owner_metric_contravariant(g)
        result = raise_components(tensor_value, metric, slot)
    end function raise_metric

    !> Metric lower of one selected contravariant slot.
    function lower_chart(c, tensor_value, slot) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM)

        if (.not. valid_metric_input(c, tensor_value, slot, UPPER)) return
        metric = metric_covariant(c)
        result = lower_components(tensor_value, metric, slot)
    end function lower_chart

    !> Metric-owner form of lower; the tensor and metric must share an arena.
    function lower_metric(g, tensor_value, slot) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot
        type(tensor_t) :: result
        type(expr_t) :: metric(DIM, DIM)

        if (.not. metric_same_arena(g, tensor_value%a)) return
        if (.not. valid_tensor_slot(tensor_value, slot, UPPER)) return
        metric = owner_metric_covariant(g)
        result = lower_components(tensor_value, metric, slot)
    end function lower_metric

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
        do i = 1, left%rank
            do j = 1, left%rank
                result%symmetry(i, j) = left%symmetry(i, j)
            end do
        end do
        do i = 1, right%rank
            do j = 1, right%rank
                result%symmetry(left%rank + i, left%rank + j) = &
                    right%symmetry(i, j)
            end do
        end do
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
    function contract_slots(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), free_indices(MAX_RANK)
        integer :: old_indices(MAX_RANK), free_slot, old_slot
        integer :: output_index, old_index, i, free_first, free_second

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
        do old_slot = 1, tensor_value%rank
            if (old_slot == first .or. old_slot == second) cycle
            free_first = 0
            do i = 1, tensor_value%rank
                if (i == first .or. i == second) cycle
                free_first = free_first + 1
                if (i == old_slot) exit
            end do
            if (free_first == 0) cycle
            do i = 1, tensor_value%rank
                if (i == first .or. i == second) cycle
                if (tensor_value%symmetry(old_slot, i) == SYMMETRY_NONE) cycle
                free_second = 0
                do old_index = 1, tensor_value%rank
                    if (old_index == first .or. old_index == second) cycle
                    free_second = free_second + 1
                    if (old_index == i) exit
                end do
                if (free_second == 0) cycle
                result%symmetry(free_first, free_second) = &
                    tensor_value%symmetry(old_slot, i)
                result%symmetry(free_second, free_first) = &
                    tensor_value%symmetry(old_slot, i)
            end do
        end do
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
    end function contract_slots

    !> Checked contraction using two typed index labels. The labels must name
    !> the same index space with opposite variance; nonempty labels must match.
    function contract_indices(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        type(index_t), intent(in) :: first, second
        type(tensor_t) :: result
        integer :: first_slot, second_slot

        if (.not. tensor_valid(tensor_value)) return
        if (.not. index_valid(first)) return
        if (.not. index_valid(second)) return
        if (.not. compatible_indices(first, second)) return
        if (index_dimension(first) /= DIM) return
        if (index_dimension(second) /= DIM) return
        first_slot = index_slot(first)
        second_slot = index_slot(second)
        if (first_slot < 1 .or. first_slot > tensor_value%rank) return
        if (second_slot < 1 .or. second_slot > tensor_value%rank) return
        if (tensor_value%variance(first_slot) /= index_variance(first)) return
        if (tensor_value%variance(second_slot) /= index_variance(second)) return
        result = contract_slots(tensor_value, first_slot, second_slot)
    end function contract_indices

    function trace(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(tensor_t) :: result

        result = contract_slots(tensor_value, first, second)
    end function trace

    !> Reorder tensor slots. `order(k)` gives the old slot at new slot k.
    function permute(tensor_value, order) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: order(:)
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), output_indices(MAX_RANK)
        integer :: source_indices(MAX_RANK), slot, output_index, source_index, i

        if (.not. tensor_valid(tensor_value)) return
        if (.not. valid_permutation(order, tensor_value%rank)) return
        metadata = 0
        do slot = 1, tensor_value%rank
            metadata(slot) = tensor_value%variance(order(slot))
        end do
        result = zero_tensor(tensor_value%a, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        do slot = 1, tensor_value%rank
            do i = 1, tensor_value%rank
                result%symmetry(slot, i) = &
                    tensor_value%symmetry(order(slot), order(i))
            end do
        end do
        do output_index = 0, component_count(tensor_value%rank) - 1
            call decode_index(output_index, tensor_value%rank, output_indices)
            source_indices = 1
            do slot = 1, tensor_value%rank
                source_indices(order(slot)) = output_indices(slot)
            end do
            source_index = encode_index(source_indices, tensor_value%rank)
            result%component(output_index) = tensor_value%component(source_index)
        end do
    end function permute

    !> Symmetrize two slots with equal variance, preserving density weight.
    function symmetrize(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(tensor_t) :: result

        result = symmetrize_pair(tensor_value, first, second, .false.)
    end function symmetrize

    !> Antisymmetrize two slots with equal variance, preserving density weight.
    function antisymmetrize(tensor_value, first, second) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        type(tensor_t) :: result

        result = symmetrize_pair(tensor_value, first, second, .true.)
    end function antisymmetrize

    function metric_covariant_tensor_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(expr_t) :: values(DIM, DIM)

        if (.not. associated(c%a)) return
        values = metric_covariant(c)
        result = tensor_from_matrix(c, values, LOWER_VARIANCE, LOWER_VARIANCE)
        result = declare_symmetry(result, 1, 2, SYMMETRIC)
    end function metric_covariant_tensor_chart

    function metric_covariant_tensor_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result
        type(expr_t) :: values(DIM, DIM)
        integer :: metadata(MAX_RANK), i, j

        if (.not. metric_valid(g)) return
        values = owner_metric_covariant(g)
        metadata = 0
        metadata(1) = LOWER_VARIANCE
        metadata(2) = LOWER_VARIANCE
        result = zero_tensor(metric_arena(g), 2, metadata, 0)
        do j = 1, DIM
            do i = 1, DIM
                result%component(encode_pair(i, j)) = values(i, j)
            end do
        end do
        result = declare_symmetry(result, 1, 2, SYMMETRIC)
    end function metric_covariant_tensor_metric

    function metric_contravariant_tensor_chart(c) result(result)
        type(chart_t), intent(in) :: c
        type(tensor_t) :: result
        type(expr_t) :: values(DIM, DIM)

        if (.not. associated(c%a)) return
        values = metric_contravariant(c)
        result = tensor_from_matrix(c, values, UPPER, UPPER)
        result = declare_symmetry(result, 1, 2, SYMMETRIC)
    end function metric_contravariant_tensor_chart

    function metric_contravariant_tensor_metric(g) result(result)
        type(metric_t), intent(in) :: g
        type(tensor_t) :: result
        type(expr_t) :: values(DIM, DIM)
        integer :: metadata(MAX_RANK), i, j

        if (.not. metric_valid(g)) return
        values = owner_metric_contravariant(g)
        metadata = 0
        metadata(1) = UPPER
        metadata(2) = UPPER
        result = zero_tensor(metric_arena(g), 2, metadata, 0)
        do j = 1, DIM
            do i = 1, DIM
                result%component(encode_pair(i, j)) = values(i, j)
            end do
        end do
        result = declare_symmetry(result, 1, 2, SYMMETRIC)
    end function metric_contravariant_tensor_metric

    function raise_components(tensor_value, metric, slot) result(result)
        type(tensor_t), intent(in) :: tensor_value
        type(expr_t), intent(in) :: metric(DIM, DIM)
        integer, intent(in) :: slot
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), indices(MAX_RANK), old_indices(MAX_RANK)
        integer :: output_index, old_index, i, j

        metadata = tensor_value%variance
        metadata(slot) = UPPER
        result = zero_tensor(tensor_value%a, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        result%symmetry = tensor_value%symmetry
        do i = 1, tensor_value%rank
            result%symmetry(slot, i) = SYMMETRY_NONE
            result%symmetry(i, slot) = SYMMETRY_NONE
        end do
        do output_index = 0, component_count(tensor_value%rank) - 1
            call decode_index(output_index, tensor_value%rank, indices)
            result%component(output_index) = num(tensor_value%a, 0)
            do j = 1, DIM
                old_indices = indices
                old_indices(slot) = j
                old_index = encode_index(old_indices, tensor_value%rank)
                i = indices(slot)
                result%component(output_index) = result%component(output_index) + &
                    metric(i, j)*tensor_value%component(old_index)
            end do
        end do
    end function raise_components

    function lower_components(tensor_value, metric, slot) result(result)
        type(tensor_t), intent(in) :: tensor_value
        type(expr_t), intent(in) :: metric(DIM, DIM)
        integer, intent(in) :: slot
        type(tensor_t) :: result
        integer :: metadata(MAX_RANK), indices(MAX_RANK), old_indices(MAX_RANK)
        integer :: output_index, old_index, i, j

        metadata = tensor_value%variance
        metadata(slot) = LOWER_VARIANCE
        result = zero_tensor(tensor_value%a, tensor_value%rank, metadata, &
            tensor_value%density_weight)
        result%symmetry = tensor_value%symmetry
        do i = 1, tensor_value%rank
            result%symmetry(slot, i) = SYMMETRY_NONE
            result%symmetry(i, slot) = SYMMETRY_NONE
        end do
        do output_index = 0, component_count(tensor_value%rank) - 1
            call decode_index(output_index, tensor_value%rank, indices)
            result%component(output_index) = num(tensor_value%a, 0)
            do j = 1, DIM
                old_indices = indices
                old_indices(slot) = j
                old_index = encode_index(old_indices, tensor_value%rank)
                i = indices(slot)
                result%component(output_index) = result%component(output_index) + &
                    metric(i, j)*tensor_value%component(old_index)
            end do
        end do
    end function lower_components

    function symmetrize_pair(tensor_value, first, second, alternating) result(result)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: first, second
        logical, intent(in) :: alternating
        type(tensor_t) :: result
        integer :: indices(MAX_RANK), swapped(MAX_RANK), output_index
        type(expr_t) :: value

        if (.not. tensor_valid(tensor_value)) return
        if (first < 1 .or. first > tensor_value%rank) return
        if (second < 1 .or. second > tensor_value%rank) return
        if (first == second) return
        if (tensor_value%variance(first) /= tensor_value%variance(second)) return
        result = zero_tensor(tensor_value%a, tensor_value%rank, &
            tensor_value%variance, tensor_value%density_weight)
        result%symmetry = 0
        do output_index = 0, component_count(tensor_value%rank) - 1
            call decode_index(output_index, tensor_value%rank, indices)
            swapped = indices
            swapped(first) = indices(second)
            swapped(second) = indices(first)
            value = tensor_value%component(encode_index(indices, tensor_value%rank)) + &
                tensor_value%component(encode_index(swapped, tensor_value%rank))
            if (alternating) value = tensor_value%component(encode_index(indices, &
                tensor_value%rank)) - tensor_value%component(encode_index(swapped, &
                tensor_value%rank))
            result%component(output_index) = value/num(tensor_value%a, 2)
        end do
        if (alternating) then
            result%symmetry(first, second) = ANTISYMMETRIC
            result%symmetry(second, first) = ANTISYMMETRIC
        else
            result%symmetry(first, second) = SYMMETRIC
            result%symmetry(second, first) = SYMMETRIC
        end if
    end function symmetrize_pair

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
        result%symmetry = 0
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

    function valid_tensor_slot(tensor_value, slot, expected) result(valid)
        type(tensor_t), intent(in) :: tensor_value
        integer, intent(in) :: slot, expected
        logical :: valid

        valid = tensor_valid(tensor_value)
        if (.not. valid) return
        if (slot < 1 .or. slot > tensor_value%rank) then
            valid = .false.
            return
        end if
        valid = tensor_value%variance(slot) == expected
    end function valid_tensor_slot

    pure function valid_variance(value) result(valid)
        integer, intent(in) :: value
        logical :: valid

        valid = value == UPPER .or. value == LOWER_VARIANCE
    end function valid_variance

    pure function valid_symmetry_value(value) result(valid)
        integer, intent(in) :: value
        logical :: valid

        valid = value == SYMMETRY_NONE .or. value == SYMMETRIC .or. &
            value == ANTISYMMETRIC
    end function valid_symmetry_value

    pure function valid_permutation(order, rank) result(valid)
        integer, intent(in) :: order(:), rank
        logical :: valid
        integer :: i, j

        valid = size(order) == rank
        if (.not. valid) return
        do i = 1, rank
            if (order(i) < 1 .or. order(i) > rank) then
                valid = .false.
                return
            end if
            do j = i + 1, rank
                if (order(i) == order(j)) then
                    valid = .false.
                    return
                end if
            end do
        end do
    end function valid_permutation

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
