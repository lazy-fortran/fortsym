module fortsym_geometry_metadata
    ! Explicit orientation and signature metadata for geometric owners.
    !
    ! These values are declarations. They are never inferred from a metric
    ! determinant or from a coordinate expression. Keeping them in one small
    ! owner lets metric, form, and relativity toolkits share the same meaning
    ! without sharing their component storage.
    implicit none
    private

    integer, parameter, public :: MAX_GEOMETRY_DIM = 16

    type, public :: orientation_t
        private
        integer :: value = 0
        logical :: valid = .false.
    end type orientation_t

    type, public :: signature_t
        private
        integer :: dimension = 0
        integer :: value(MAX_GEOMETRY_DIM) = 0
        logical :: valid = .false.
    end type signature_t

    public :: orientation_create, orientation_valid, orientation_value
    public :: orientation_is_positive, orientation_is_negative
    public :: signature_create, signature_valid, signature_dimension
    public :: signature_component, signature_positive_count
    public :: signature_negative_count, signature_is_lorentzian

contains

    function orientation_create(sign) result(value)
        integer, intent(in) :: sign
        type(orientation_t) :: value

        if (sign /= 1 .and. sign /= -1) return
        value%value = sign
        value%valid = .true.
    end function orientation_create

    pure function orientation_valid(value) result(valid)
        type(orientation_t), intent(in) :: value
        logical :: valid

        valid = value%valid .and. (value%value == 1 .or. value%value == -1)
    end function orientation_valid

    pure function orientation_value(value) result(sign)
        type(orientation_t), intent(in) :: value
        integer :: sign

        sign = 0
        if (orientation_valid(value)) sign = value%value
    end function orientation_value

    pure function orientation_is_positive(value) result(positive)
        type(orientation_t), intent(in) :: value
        logical :: positive

        positive = orientation_valid(value) .and. value%value == 1
    end function orientation_is_positive

    pure function orientation_is_negative(value) result(negative)
        type(orientation_t), intent(in) :: value
        logical :: negative

        negative = orientation_valid(value) .and. value%value == -1
    end function orientation_is_negative

    function signature_create(values) result(value)
        integer, intent(in) :: values(:)
        type(signature_t) :: value
        integer :: i

        if (size(values) < 1 .or. size(values) > MAX_GEOMETRY_DIM) return
        do i = 1, size(values)
            if (values(i) /= 1 .and. values(i) /= -1) return
        end do
        value%dimension = size(values)
        do i = 1, value%dimension
            value%value(i) = values(i)
        end do
        value%valid = .true.
    end function signature_create

    pure function signature_valid(value) result(valid)
        type(signature_t), intent(in) :: value
        logical :: valid
        integer :: i

        valid = value%valid
        if (.not. valid) return
        if (value%dimension < 1 .or. value%dimension > MAX_GEOMETRY_DIM) then
            valid = .false.
            return
        end if
        do i = 1, value%dimension
            if (value%value(i) /= 1 .and. value%value(i) /= -1) then
                valid = .false.
                return
            end if
        end do
    end function signature_valid

    pure function signature_dimension(value) result(dimension)
        type(signature_t), intent(in) :: value
        integer :: dimension

        dimension = 0
        if (signature_valid(value)) dimension = value%dimension
    end function signature_dimension

    pure function signature_component(value, index) result(sign)
        type(signature_t), intent(in) :: value
        integer, intent(in) :: index
        integer :: sign

        sign = 0
        if (.not. signature_valid(value)) return
        if (index < 1 .or. index > value%dimension) return
        sign = value%value(index)
    end function signature_component

    pure function signature_positive_count(value) result(count)
        type(signature_t), intent(in) :: value
        integer :: count, i

        count = 0
        if (.not. signature_valid(value)) return
        do i = 1, value%dimension
            if (value%value(i) == 1) count = count + 1
        end do
    end function signature_positive_count

    pure function signature_negative_count(value) result(count)
        type(signature_t), intent(in) :: value
        integer :: count, i

        count = 0
        if (.not. signature_valid(value)) return
        do i = 1, value%dimension
            if (value%value(i) == -1) count = count + 1
        end do
    end function signature_negative_count

    pure function signature_is_lorentzian(value) result(lorentzian)
        type(signature_t), intent(in) :: value
        logical :: lorentzian
        integer :: positive, negative

        lorentzian = .false.
        if (.not. signature_valid(value)) return
        if (value%dimension < 2) return
        positive = signature_positive_count(value)
        negative = signature_negative_count(value)
        lorentzian = positive == 1 .or. negative == 1
    end function signature_is_lorentzian

end module fortsym_geometry_metadata
