module fortsym_domain
    ! Topological metadata for the differential-geometry toolkit.
    !
    ! This owner deliberately stores declarations only. It does not infer
    ! topology from coordinate expressions: exactness and global statements
    ! must name the manifold and patch assumptions supplied by the caller.
    implicit none
    private

    integer, parameter :: NAME_LEN = 64

    type, public :: manifold_t
        private
        integer :: dimension = 0
        character(len=NAME_LEN) :: name = ""
        logical :: has_boundary = .false.
        logical :: simply_connected = .false.
        logical :: valid = .false.
    end type manifold_t

    type, public :: patch_t
        private
        type(manifold_t) :: parent
        character(len=NAME_LEN) :: name = ""
        logical :: open_domain = .true.
        logical :: has_boundary = .false.
        logical :: simply_connected = .false.
        logical :: valid = .false.
    end type patch_t

    public :: manifold_create, patch_create
    public :: manifold_valid, manifold_dimension, manifold_name
    public :: manifold_has_boundary, manifold_simply_connected
    public :: patch_valid, patch_dimension, patch_name, patch_manifold
    public :: patch_is_open, patch_has_boundary, patch_simply_connected
    public :: same_manifold, same_patch_parent

contains

    function manifold_create(name, dimension, has_boundary, simply_connected) &
            result(value)
        character(len=*), intent(in) :: name
        integer, intent(in) :: dimension
        logical, optional, intent(in) :: has_boundary, simply_connected
        type(manifold_t) :: value
        integer :: length

        length = len_trim(name)
        if (length < 1 .or. length > NAME_LEN) return
        if (dimension < 1) return
        value%name(1:length) = name(1:length)
        value%dimension = dimension
        if (present(has_boundary)) value%has_boundary = has_boundary
        if (present(simply_connected)) then
            value%simply_connected = simply_connected
        end if
        value%valid = .true.
    end function manifold_create

    function patch_create(parent, name, open_domain, has_boundary, &
            simply_connected) result(value)
        type(manifold_t), intent(in) :: parent
        character(len=*), intent(in) :: name
        logical, optional, intent(in) :: open_domain, has_boundary, &
            simply_connected
        type(patch_t) :: value
        integer :: length

        if (.not. manifold_valid(parent)) return
        length = len_trim(name)
        if (length < 1 .or. length > NAME_LEN) return
        value%parent = parent
        value%name(1:length) = name(1:length)
        if (present(open_domain)) value%open_domain = open_domain
        if (present(has_boundary)) value%has_boundary = has_boundary
        if (present(simply_connected)) then
            value%simply_connected = simply_connected
        end if
        value%valid = .true.
    end function patch_create

    pure function manifold_valid(value) result(valid)
        type(manifold_t), intent(in) :: value
        logical :: valid

        valid = value%valid .and. value%dimension > 0 .and. &
            len_trim(value%name) > 0
    end function manifold_valid

    pure function manifold_dimension(value) result(dimension)
        type(manifold_t), intent(in) :: value
        integer :: dimension

        dimension = 0
        if (manifold_valid(value)) dimension = value%dimension
    end function manifold_dimension

    pure function manifold_name(value) result(name)
        type(manifold_t), intent(in) :: value
        character(len=NAME_LEN) :: name

        name = ""
        if (manifold_valid(value)) name = value%name
    end function manifold_name

    pure function manifold_has_boundary(value) result(has_boundary)
        type(manifold_t), intent(in) :: value
        logical :: has_boundary

        has_boundary = manifold_valid(value) .and. value%has_boundary
    end function manifold_has_boundary

    pure function manifold_simply_connected(value) result(simply_connected)
        type(manifold_t), intent(in) :: value
        logical :: simply_connected

        simply_connected = manifold_valid(value) .and. value%simply_connected
    end function manifold_simply_connected

    pure function patch_valid(value) result(valid)
        type(patch_t), intent(in) :: value
        logical :: valid

        valid = value%valid .and. manifold_valid(value%parent) .and. &
            len_trim(value%name) > 0
    end function patch_valid

    pure function patch_dimension(value) result(dimension)
        type(patch_t), intent(in) :: value
        integer :: dimension

        dimension = 0
        if (patch_valid(value)) dimension = manifold_dimension(value%parent)
    end function patch_dimension

    pure function patch_name(value) result(name)
        type(patch_t), intent(in) :: value
        character(len=NAME_LEN) :: name

        name = ""
        if (patch_valid(value)) name = value%name
    end function patch_name

    pure function patch_manifold(value) result(parent)
        type(patch_t), intent(in) :: value
        type(manifold_t) :: parent

        if (patch_valid(value)) parent = value%parent
    end function patch_manifold

    pure function patch_is_open(value) result(open_domain)
        type(patch_t), intent(in) :: value
        logical :: open_domain

        open_domain = patch_valid(value) .and. value%open_domain
    end function patch_is_open

    pure function patch_has_boundary(value) result(has_boundary)
        type(patch_t), intent(in) :: value
        logical :: has_boundary

        has_boundary = patch_valid(value) .and. value%has_boundary
    end function patch_has_boundary

    pure function patch_simply_connected(value) result(simply_connected)
        type(patch_t), intent(in) :: value
        logical :: simply_connected

        simply_connected = patch_valid(value) .and. value%simply_connected
    end function patch_simply_connected

    pure function same_manifold(left, right) result(same)
        type(manifold_t), intent(in) :: left, right
        logical :: same

        same = manifold_valid(left) .and. manifold_valid(right) .and. &
            left%dimension == right%dimension .and. &
            trim(left%name) == trim(right%name)
    end function same_manifold

    pure function same_patch_parent(patch, parent) result(same)
        type(patch_t), intent(in) :: patch
        type(manifold_t), intent(in) :: parent
        logical :: same

        same = patch_valid(patch) .and. same_manifold(patch%parent, parent)
    end function same_patch_parent

end module fortsym_domain
