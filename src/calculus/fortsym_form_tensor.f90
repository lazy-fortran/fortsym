module fortsym_form_tensor
    ! The single bridge between the typed tensor and differential-form owners.
    !
    ! A k-form is a fully antisymmetric covariant rank-k tensor.  The form
    ! owner keeps the compact wedge-mask representation, while the tensor
    ! owner keeps variance and flat component metadata.  This module performs
    ! the conversion without giving either owner a second representation of
    ! the other.  Conversion is deliberately exact at this boundary: a tensor
    ! with non-antisymmetric or nonzero-density components is refused rather
    ! than silently projected into a different mathematical object.
    use fortsym_chart, only: chart_t, DIM
    use fortsym_form, only: form_t, form_valid
    use fortsym_tensor, only: tensor_t, MAX_RANK, LOWER_VARIANCE, tensor_valid, &
        tensor_rank, tensor_variance, tensor_density_weight, tensor_from_arena, &
        ANTISYMMETRIC
    use fortsym_expr, only: expr_t, num, operator(-), operator(==)
    implicit none
    private

    integer, parameter :: COMPONENT_CAPACITY = DIM**MAX_RANK

    public :: form_from_tensor, tensor_from_form

contains

    !> Convert an exact antisymmetric lower tensor into a k-form.
    !>
    !> Forms have no implicit density slot, so only weight-zero tensors are
    !> accepted.  Every ordered tensor component is checked against the
    !> corresponding wedge-mask component, including repeated-index zeros.
    function form_from_tensor(tensor_value) result(alpha)
        type(tensor_t), intent(in) :: tensor_value
        type(form_t) :: alpha
        type(expr_t) :: actual, expected, zero
        integer :: degree, mask, flat, indices(MAX_RANK), sign

        if (.not. tensor_valid(tensor_value)) return
        degree = tensor_rank(tensor_value)
        if (degree > DIM) return
        if (tensor_density_weight(tensor_value) /= 0) return
        do mask = 1, degree
            if (tensor_variance(tensor_value, mask) /= LOWER_VARIANCE) return
        end do

        alpha%a => tensor_value%a
        alpha%degree = degree
        alpha%zero_extension = .false.
        zero = num(tensor_value%a, 0)
        do mask = 0, 2**DIM - 1
            alpha%component(mask) = zero
        end do
        do mask = 0, 2**DIM - 1
            if (mask_degree(mask) /= degree) cycle
            call mask_indices(mask, degree, indices)
            alpha%component(mask) = tensor_value%component( &
                tensor_offset(indices, degree))
        end do

        do flat = 0, component_count(degree) - 1
            call decode_index(flat, degree, indices)
            actual = tensor_value%component(tensor_offset(indices, degree))
            if (has_duplicate(indices, degree)) then
                if (.not. (actual == zero)) then
                    alpha = form_t()
                    return
                end if
                cycle
            end if
            mask = indices_mask(indices, degree)
            expected = alpha%component(mask)
            sign = permutation_sign(indices, degree)
            if (sign < 0) expected = -expected
            if (.not. (actual == expected)) then
                alpha = form_t()
                return
            end if
        end do
    end function form_from_tensor

    !> Convert a k-form into its fully antisymmetric lower tensor view.
    !>
    !> The returned tensor uses first-slot-fastest order and carries no density
    !> weight.  Repeated indices are structural zeros and all other components
    !> receive the permutation sign of the ordered coframe.
    function tensor_from_form(c, alpha) result(tensor_value)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(tensor_t) :: tensor_value
        type(expr_t) :: values(0:COMPONENT_CAPACITY - 1)
        integer :: variance(MAX_RANK), degree, flat, indices(MAX_RANK)
        integer :: mask, sign, k

        if (.not. associated(c%a)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, c%a)) return
        degree = alpha%degree
        if (degree > DIM) return

        do k = 0, COMPONENT_CAPACITY - 1
            values(k) = num(c%a, 0)
        end do
        variance = 0
        do k = 1, degree
            variance(k) = LOWER_VARIANCE
        end do
        do flat = 0, component_count(degree) - 1
            call decode_index(flat, degree, indices)
            if (has_duplicate(indices, degree)) cycle
            mask = indices_mask(indices, degree)
            sign = permutation_sign(indices, degree)
            values(flat) = alpha%component(mask)
            if (sign < 0) values(flat) = -values(flat)
        end do

        tensor_value = tensor_from_arena(c%a, degree, values, variance, 0)
        do k = 1, degree
            do flat = k + 1, degree
                tensor_value%symmetry(k, flat) = ANTISYMMETRIC
                tensor_value%symmetry(flat, k) = ANTISYMMETRIC
            end do
        end do
    end function tensor_from_form

    pure function component_count(rank) result(count)
        integer, intent(in) :: rank
        integer :: count, k

        count = 1
        do k = 1, rank
            count = count*DIM
        end do
    end function component_count

    pure function mask_degree(mask) result(degree)
        integer, intent(in) :: mask
        integer :: degree

        degree = popcnt(mask)
    end function mask_degree

    subroutine mask_indices(mask, degree, indices)
        integer, intent(in) :: mask, degree
        integer, intent(out) :: indices(MAX_RANK)
        integer :: i, found

        indices = 1
        found = 0
        do i = 1, DIM
            if (.not. btest(mask, i - 1)) cycle
            found = found + 1
            if (found <= degree) indices(found) = i
        end do
    end subroutine mask_indices

    subroutine decode_index(flat, rank, indices)
        integer, intent(in) :: flat, rank
        integer, intent(out) :: indices(MAX_RANK)
        integer :: work, slot

        indices = 1
        work = flat
        do slot = 1, rank
            indices(slot) = mod(work, DIM) + 1
            work = work/DIM
        end do
    end subroutine decode_index

    pure function tensor_offset(indices, rank) result(offset)
        integer, intent(in) :: indices(MAX_RANK), rank
        integer :: offset, stride, slot

        offset = 0
        stride = 1
        do slot = 1, rank
            offset = offset + (indices(slot) - 1)*stride
            stride = stride*DIM
        end do
    end function tensor_offset

    pure function indices_mask(indices, rank) result(mask)
        integer, intent(in) :: indices(MAX_RANK), rank
        integer :: mask, slot

        mask = 0
        do slot = 1, rank
            mask = ior(mask, 2**(indices(slot) - 1))
        end do
    end function indices_mask

    pure function has_duplicate(indices, rank) result(duplicate)
        integer, intent(in) :: indices(MAX_RANK), rank
        logical :: duplicate
        integer :: i, j

        duplicate = .false.
        do i = 1, rank
            do j = i + 1, rank
                if (indices(i) == indices(j)) duplicate = .true.
            end do
        end do
    end function has_duplicate

    pure function permutation_sign(indices, rank) result(sign)
        integer, intent(in) :: indices(MAX_RANK), rank
        integer :: sign, i, j, inversions

        inversions = 0
        do i = 1, rank
            do j = i + 1, rank
                if (indices(i) > indices(j)) inversions = inversions + 1
            end do
        end do
        sign = 1
        if (mod(inversions, 2) == 1) sign = -1
    end function permutation_sign

end module fortsym_form_tensor
