module fortsym_index
    ! Small, value-semantic index labels for checked tensor contractions.
    ! Component storage remains owned by fortsym_tensor; this module only
    ! describes an index space, slot, variance, and optional printed label.
    implicit none
    private

    integer, parameter, public :: INDEX_TANGENT = 1
    integer, parameter, public :: INDEX_COTANGENT = 2
    integer, parameter, public :: INDEX_SPACETIME = 3
    integer, parameter, public :: INDEX_INTERNAL = 4
    integer, parameter, public :: INDEX_USER = 5

    type, public :: index_type_t
        private
        integer :: dimension = 0
        integer :: category = 0
        character(len=64) :: name = ""
    end type index_type_t

    type, public :: index_t
        private
        type(index_type_t) :: space
        integer :: slot = 0
        integer :: variance = 0
        logical :: dummy = .false.
        character(len=64) :: name = ""
    end type index_t

    public :: index_type, make_index
    public :: index_valid, index_space_valid, index_dimension
    public :: index_category, index_space_name, index_label
    public :: index_slot, index_variance, index_is_dummy
    public :: same_index_space, compatible_indices

contains

    function index_type(name, dimension, category) result(value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension
        integer, optional, intent(in) :: category
        type(index_type_t) :: value
        integer :: selected_category

        selected_category = INDEX_USER
        if (present(category)) selected_category = category
        if (len_trim(name) == 0) return
        if (dimension < 1) return
        if (.not. valid_category(selected_category)) return
        if (len_trim(name) > len(value%name)) return
        value%name = ""
        value%name(1:len_trim(name)) = name(1:len_trim(name))
        value%dimension = dimension
        value%category = selected_category
    end function index_type

    function make_index(space, slot, variance, label, dummy) result(value)
        type(index_type_t), intent(in) :: space
        integer, intent(in) :: slot, variance
        character(len=*), optional, intent(in) :: label
        logical, optional, intent(in) :: dummy
        type(index_t) :: value
        character(len=64) :: selected_label

        if (.not. index_space_valid(space)) return
        if (slot < 1 .or. slot > space%dimension) return
        if (variance /= 1 .and. variance /= -1) return
        selected_label = ""
        if (present(label)) then
            if (len_trim(label) > len(selected_label)) return
            selected_label(1:len_trim(label)) = label(1:len_trim(label))
        end if
        value%space = space
        value%slot = slot
        value%variance = variance
        value%name = selected_label
        if (present(dummy)) value%dummy = dummy
    end function make_index

    pure function index_space_valid(space) result(value)
        type(index_type_t), intent(in) :: space
        logical :: value

        value = space%dimension > 0 .and. valid_category(space%category) .and. &
            len_trim(space%name) > 0
    end function index_space_valid

    pure function index_valid(value) result(valid)
        type(index_t), intent(in) :: value
        logical :: valid

        valid = index_space_valid(value%space)
        if (.not. valid) return
        valid = value%slot >= 1 .and. value%slot <= value%space%dimension
        if (.not. valid) return
        valid = value%variance == 1 .or. value%variance == -1
    end function index_valid

    pure function index_dimension(value) result(dimension)
        type(index_t), intent(in) :: value
        integer :: dimension

        dimension = 0
        if (index_valid(value)) dimension = value%space%dimension
    end function index_dimension

    pure function index_category(value) result(category)
        type(index_type_t), intent(in) :: value
        integer :: category

        category = 0
        if (index_space_valid(value)) category = value%category
    end function index_category

    pure function index_space_name(value) result(name)
        type(index_t), intent(in) :: value
        character(len=64) :: name

        name = ""
        if (index_valid(value)) name = value%space%name
    end function index_space_name

    pure function index_label(value) result(name)
        type(index_t), intent(in) :: value
        character(len=64) :: name

        name = ""
        if (index_valid(value)) name = value%name
    end function index_label

    pure function index_slot(value) result(slot)
        type(index_t), intent(in) :: value
        integer :: slot

        slot = 0
        if (index_valid(value)) slot = value%slot
    end function index_slot

    pure function index_variance(value) result(variance)
        type(index_t), intent(in) :: value
        integer :: variance

        variance = 0
        if (index_valid(value)) variance = value%variance
    end function index_variance

    pure function index_is_dummy(value) result(dummy)
        type(index_t), intent(in) :: value
        logical :: dummy

        dummy = index_valid(value) .and. value%dummy
    end function index_is_dummy

    pure function same_index_space(left, right) result(same)
        type(index_t), intent(in) :: left, right
        logical :: same

        same = index_valid(left)
        if (.not. same) return
        same = index_valid(right)
        if (.not. same) return
        same = left%space%dimension == right%space%dimension .and. &
            left%space%category == right%space%category .and. &
            trim(left%space%name) == trim(right%space%name)
    end function same_index_space

    pure function compatible_indices(left, right) result(compatible)
        type(index_t), intent(in) :: left, right
        logical :: compatible
        character(len=64) :: left_label, right_label

        compatible = same_index_space(left, right)
        if (.not. compatible) return
        compatible = left%variance == -right%variance
        if (.not. compatible) return
        left_label = index_label(left)
        right_label = index_label(right)
        if (len_trim(left_label) > 0 .and. len_trim(right_label) > 0) then
            compatible = trim(left_label) == trim(right_label)
        end if
    end function compatible_indices

    pure function valid_category(category) result(valid)
        integer, intent(in) :: category
        logical :: valid

        valid = category >= INDEX_TANGENT .and. category <= INDEX_USER
    end function valid_category

end module fortsym_index
