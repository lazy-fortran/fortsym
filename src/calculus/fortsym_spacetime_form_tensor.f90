module fortsym_spacetime_form_tensor
    ! Bridge the runtime spacetime tensor and differential-form owners.
    !
    ! A k-form is an exact lower, weight-zero, fully antisymmetric tensor.  The
    ! two owners keep their own compact storage; this module is the only place
    ! that translates between the mask and first-slot-fastest representations.
    use fortsym_arena, only: arena_t
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_expr, only: expr_t, num, operator(-), operator(==)
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_valid, &
        spacetime_metric_arena, spacetime_metric_dimension
    use fortsym_spacetime_form, only: spacetime_form_t, &
        spacetime_form_scalar, spacetime_form_one, spacetime_form_two, &
        spacetime_form_three, spacetime_form_four, spacetime_form_component, &
        spacetime_form_degree, spacetime_form_dimension, spacetime_form_valid, &
        spacetime_form_same_arena
    use fortsym_spacetime_tensor, only: spacetime_tensor_t, &
        SPACETIME_TENSOR_MAX_RANK, &
        spacetime_tensor_from_storage, spacetime_tensor_valid, &
        spacetime_tensor_declare_symmetry, &
        spacetime_tensor_rank, spacetime_tensor_dimension, &
        spacetime_tensor_variance, spacetime_tensor_density_weight, &
        spacetime_tensor_component_flat, spacetime_tensor_same_arena, &
        SPACETIME_LOWER, SPACETIME_ANTISYMMETRIC
    implicit none
    private

    integer, parameter :: MAX_COMPONENTS = SPACETIME_DIM**SPACETIME_TENSOR_MAX_RANK

    public :: spacetime_form_from_tensor, spacetime_tensor_from_form

contains

    !> Convert an exact lower antisymmetric tensor to a runtime spacetime form.
    !>
    !> The conversion is deliberately strict: density-valued tensors, upper
    !> slots, repeated-index nonzeros, and mismatched metric owners are refused.
    function spacetime_form_from_tensor(g, tensor_value) result(form)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_tensor_t), intent(in) :: tensor_value
        type(spacetime_form_t) :: form
        type(expr_t) :: coefficients4(SPACETIME_DIM), coefficients6(6)
        type(expr_t) :: actual, expected, zero
        type(arena_t), pointer :: a
        type(native_engine_t) :: engine
        integer :: degree, dimension, count, flat, mask, slot, sign
        integer :: indices(SPACETIME_TENSOR_MAX_RANK)

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_tensor_valid(tensor_value)) return
        if (.not. spacetime_tensor_same_arena(tensor_value, g)) return
        degree = spacetime_tensor_rank(tensor_value)
        dimension = spacetime_metric_dimension(g)
        if (degree > dimension) return
        if (spacetime_tensor_dimension(tensor_value) /= dimension) return
        if (spacetime_tensor_density_weight(tensor_value) /= 0) return
        do slot = 1, degree
            if (spacetime_tensor_variance(tensor_value, slot) /= SPACETIME_LOWER) return
        end do

        a => spacetime_metric_arena(g)
        engine = make_native_engine(a)
        zero = num(spacetime_metric_arena(g), 0)
        coefficients4 = zero
        coefficients6 = zero
        count = component_count(degree, dimension)
        do flat = 0, count - 1
            call decode_index(flat, degree, indices, dimension)
            actual = spacetime_tensor_component_flat(tensor_value, indices)
            if (has_duplicate(indices, degree)) then
                if (.not. expression_equal(actual, zero, engine)) return
                cycle
            end if
            mask = indices_mask(indices, degree)
            sign = permutation_sign(indices, degree)
            if (sign < 0) actual = -actual
            select case (degree)
            case (0)
                coefficients4(1) = actual
            case (1)
                coefficients4(coefficient_index(mask, degree)) = actual
            case (2)
                coefficients6(coefficient_index(mask, degree)) = actual
            case (3)
                coefficients4(coefficient_index(mask, degree)) = actual
            case (4)
                coefficients4(1) = actual
            end select
        end do

        select case (degree)
        case (0)
            form = spacetime_form_scalar(g, coefficients4(1))
        case (1)
            form = spacetime_form_one(g, coefficients4)
        case (2)
            form = spacetime_form_two(g, coefficients6)
        case (3)
            form = spacetime_form_three(g, coefficients4)
        case (4)
            form = spacetime_form_four(g, coefficients4(1))
        case default
            return
        end select
        if (.not. spacetime_form_valid(form)) return

        ! Check every ordered component against the constructed form.  This
        ! catches a non-antisymmetric tensor even when its canonical entries
        ! happen to look valid.
        do flat = 0, count - 1
            call decode_index(flat, degree, indices, dimension)
            actual = spacetime_tensor_component_flat(tensor_value, indices)
            expected = zero
            if (.not. has_duplicate(indices, degree)) then
                mask = indices_mask(indices, degree)
                expected = spacetime_form_component(form, mask)
                if (permutation_sign(indices, degree) < 0) expected = -expected
            end if
            if (.not. expression_equal(actual, expected, engine)) then
                form = spacetime_form_t()
                return
            end if
        end do
    end function spacetime_form_from_tensor

    !> Convert a runtime spacetime form to its fully antisymmetric tensor view.
    function spacetime_tensor_from_form(g, form) result(tensor_value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_tensor_t) :: tensor_value
        type(expr_t) :: values(0:MAX_COMPONENTS - 1), zero
        integer :: metadata(SPACETIME_TENSOR_MAX_RANK)
        integer :: degree, dimension, count, flat, mask, slot, sign
        integer :: indices(SPACETIME_TENSOR_MAX_RANK)

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_form_valid(form)) return
        if (.not. spacetime_form_same_arena(form, spacetime_metric_arena(g))) return
        degree = spacetime_form_degree(form)
        dimension = spacetime_metric_dimension(g)
        if (spacetime_form_dimension(form) /= dimension) return
        if (degree > dimension) return

        zero = num(spacetime_metric_arena(g), 0)
        values = zero
        metadata = 0
        do slot = 1, degree
            metadata(slot) = SPACETIME_LOWER
        end do
        count = component_count(degree, dimension)
        do flat = 0, count - 1
            call decode_index(flat, degree, indices, dimension)
            if (has_duplicate(indices, degree)) cycle
            mask = indices_mask(indices, degree)
            values(flat) = spacetime_form_component(form, mask)
            sign = permutation_sign(indices, degree)
            if (sign < 0) values(flat) = -values(flat)
        end do
        tensor_value = spacetime_tensor_from_storage(g, degree, values, metadata, 0)
        do slot = 1, degree
            do mask = slot + 1, degree
                tensor_value = spacetime_tensor_declare_symmetry(tensor_value, &
                    slot, mask, SPACETIME_ANTISYMMETRIC)
            end do
        end do
    end function spacetime_tensor_from_form

    pure function component_count(rank, dimension) result(count)
        integer, intent(in) :: rank, dimension
        integer :: count, slot

        count = 1
        do slot = 1, rank
            count = count*dimension
        end do
    end function component_count

    pure subroutine decode_index(flat, rank, indices, dimension)
        integer, intent(in) :: flat, rank, dimension
        integer, intent(out) :: indices(SPACETIME_TENSOR_MAX_RANK)
        integer :: value, slot

        indices = 1
        value = flat
        do slot = 1, rank
            indices(slot) = mod(value, dimension) + 1
            value = value/dimension
        end do
    end subroutine decode_index

    pure function indices_mask(indices, degree) result(mask)
        integer, intent(in) :: indices(SPACETIME_TENSOR_MAX_RANK), degree
        integer :: mask, slot

        mask = 0
        do slot = 1, degree
            mask = ibset(mask, indices(slot) - 1)
        end do
    end function indices_mask

    pure function has_duplicate(indices, degree) result(found)
        integer, intent(in) :: indices(SPACETIME_TENSOR_MAX_RANK), degree
        logical :: found
        integer :: left, right

        found = .false.
        do left = 1, degree
            do right = left + 1, degree
                if (indices(left) == indices(right)) found = .true.
            end do
        end do
    end function has_duplicate

    pure function permutation_sign(indices, degree) result(sign)
        integer, intent(in) :: indices(SPACETIME_TENSOR_MAX_RANK), degree
        integer :: sign, left, right, inversions

        inversions = 0
        do left = 1, degree
            do right = left + 1, degree
                if (indices(left) > indices(right)) inversions = inversions + 1
            end do
        end do
        sign = 1
        if (mod(inversions, 2) == 1) sign = -1
    end function permutation_sign

    pure function coefficient_index(mask, degree) result(index)
        integer, intent(in) :: mask, degree
        integer :: index, candidate

        index = 0
        do candidate = 0, 2**SPACETIME_DIM - 1
            if (popcnt(candidate) /= degree) cycle
            index = index + 1
            if (candidate == mask) return
        end do
        index = 1
    end function coefficient_index

    function expression_equal(left, right, engine) result(same)
        type(expr_t), intent(in) :: left, right
        type(native_engine_t), intent(inout) :: engine
        logical :: same
        type(engine_result_t) :: decision

        same = left == right
        if (same) return
        decision = engine%zero_test(left - right)
        same = decision%verdict == VERDICT_TRUE
    end function expression_equal

end module fortsym_spacetime_form_tensor
