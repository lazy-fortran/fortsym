module fortsym_jet
    ! Value, gradient and Hessian of a quantity as data.
    !
    ! A generator states which partial derivatives it needs as a
    ! derivative_set_t (shared with FortGen's Horner leaves) and how each is
    ! spelled in Fortran as a jet_names_t, once per quantity:
    !
    !     B = func('Bmod', x)
    !     call bind(B, members('f%Bmod', 'f%dBmod', 'f%d2Bmod'))
    !     dB = jet(B, partials(3, 2), x)
    !
    ! bind makes the applied function and its partials kernel inputs that
    ! read those names; the same names serve as output references of a
    ! kernel_t (fortsym_emit). No reference is assembled from strings.
    use fortsym_arena, only: NK_FUNC
    use fortsym_expr, only: expr_t
    use fortsym_diff, only: diff
    use fortsym_chain, only: chain_t
    use fortsym_context, only: set_binding
    use fortsym_string, only: str_t, str, chars, indexed
    use fortgen_derivative_set, only: derivative_set_t, partials, radial_mixed, &
        multi_indices
    implicit none
    private

    public :: derivative_set_t, partials, radial_mixed, multi_indices
    public :: jet_names_t, members, suffixed, listed
    public :: bind, jet, derivative_indices

    integer, parameter :: STYLE_MEMBERS = 1, STYLE_SUFFIXED = 2, STYLE_LISTED = 3

    interface listed
        module procedure listed_chars, listed_strings
    end interface listed

    !> How the value and the partial derivatives of one quantity are named.
    type :: jet_names_t
        integer :: style = 0
        character(:), allocatable :: value, gradient, hessian, separator
        type(str_t), allocatable :: axes(:), list(:)
        integer, allocatable :: packing(:, :)
    contains
        procedure :: of => names_of
        procedure :: all => names_all
    end type jet_names_t

contains

    !> Value `value`, gradient elements gradient(i) and Hessian elements
    !> hessian(packing(i, j)). The default packing numbers the upper
    !> triangle row by row: 11, 12, 13, 22, 23, 33 are 1..6.
    function members(value, gradient, hessian, packing) result(names)
        character(*), intent(in) :: value
        character(*), intent(in), optional :: gradient, hessian
        integer, intent(in), optional :: packing(:, :)
        type(jet_names_t) :: names

        names%style = STYLE_MEMBERS
        names%value = value
        if (present(gradient)) names%gradient = gradient
        if (present(hessian)) names%hessian = hessian
        if (present(packing)) names%packing = packing
    end function members

    !> Value `value` and partials value//separator//axis letters, such as
    !> r, r_r, r_t, r_rr, r_rt for suffixed('r', ['r', 't']).
    function suffixed(value, axes, separator) result(names)
        character(*), intent(in) :: value, axes(:)
        character(*), intent(in), optional :: separator
        type(jet_names_t) :: names
        integer :: j

        names%style = STYLE_SUFFIXED
        names%value = value
        names%separator = '_'
        if (present(separator)) names%separator = separator
        allocate (names%axes(size(axes)))
        do j = 1, size(axes)
            names%axes(j) = str(trim(axes(j)))
        end do
    end function suffixed

    !> Explicit names in canonical order: value, gradient, then the upper
    !> Hessian row by row. An empty name leaves that partial unnamed.
    function listed_chars(list) result(names)
        character(*), intent(in) :: list(:)
        type(jet_names_t) :: names
        integer :: k

        names%style = STYLE_LISTED
        allocate (names%list(size(list)))
        do k = 1, size(list)
            names%list(k) = str(trim(list(k)))
        end do
    end function listed_chars

    !> The same from strings, such as listed(words('B dB d2B')).
    function listed_strings(list) result(names)
        type(str_t), intent(in) :: list(:)
        type(jet_names_t) :: names

        names%style = STYLE_LISTED
        names%list = list
    end function listed_strings

    !> The name of the partial with multi-index alpha, or '' when the
    !> scheme names none.
    function names_of(self, alpha) result(name)
        class(jet_names_t), intent(in) :: self
        integer, intent(in) :: alpha(:)
        character(:), allocatable :: name
        integer, allocatable :: idx(:)
        integer :: j, k

        name = ''
        idx = derivative_indices(alpha)
        select case (self%style)
        case (STYLE_MEMBERS)
            select case (size(idx))
            case (0)
                name = self%value
            case (1)
                if (allocated(self%gradient)) name = indexed(self%gradient, idx(1))
            case (2)
                if (.not. allocated(self%hessian)) return
                if (allocated(self%packing)) then
                    name = indexed(self%hessian, self%packing(idx(1), idx(2)))
                else
                    name = indexed(self%hessian, rank_of(alpha))
                end if
            end select
        case (STYLE_SUFFIXED)
            name = self%value
            if (size(idx) == 0) return
            name = name//self%separator
            do k = 1, size(idx)
                name = name//chars(self%axes(idx(k)))
            end do
        case (STYLE_LISTED)
            k = 0
            do j = 0, sum(alpha) - 1
                k = k + count_of_order(size(alpha), j)
            end do
            k = k + rank_of(alpha)
            if (k <= size(self%list)) name = chars(self%list(k))
        end select
    end function names_of

    !> The names of every member of `set`, in set order.
    function names_all(self, set) result(list)
        class(jet_names_t), intent(in) :: self
        type(derivative_set_t), intent(in) :: set
        type(str_t), allocatable :: list(:)
        integer :: k

        allocate (list(set%count()))
        do k = 1, set%count()
            list(k) = str(self%of(set%orders(:, k)))
        end do
    end function names_all

    !> Bind the applied function q and its partials in `set` (default: up to
    !> second order) to the names of `names` in the current context.
    subroutine bind(q, names, set)
        type(expr_t), intent(in) :: q
        type(jet_names_t), intent(in) :: names
        type(derivative_set_t), intent(in), optional :: set
        type(derivative_set_t) :: which
        character(:), allocatable :: head, name
        integer :: k

        if (q%kind() /= NK_FUNC) error stop "bind: not an applied function"
        head = chars(q%name())
        if (q%nargs() == 0) then
            call set_binding(head, [integer ::], names%of([integer ::]))
            return
        end if
        if (present(set)) then
            which = set
        else
            which = partials(q%nargs(), 2)
        end if
        do k = 1, which%count()
            name = names%of(which%orders(:, k))
            if (len(name) == 0) cycle
            call set_binding(head, derivative_indices(which%orders(:, k)), name)
        end do
    end subroutine bind

    !> The partials of q in `set`, differentiated by the symbols x or along
    !> the variables of chain c, in increasing variable order: the second
    !> partial (i, j), i <= j, is d_j(d_i q).
    function jet(q, set, x, chain) result(d)
        type(expr_t), intent(in) :: q
        type(derivative_set_t), intent(in) :: set
        type(expr_t), intent(in), optional :: x(:)
        type(chain_t), intent(inout), optional :: chain
        type(expr_t), allocatable :: d(:)
        integer, allocatable :: idx(:)
        integer :: k, m

        allocate (d(set%count()))
        do k = 1, set%count()
            d(k) = q
            idx = derivative_indices(set%orders(:, k))
            do m = 1, size(idx)
                if (present(chain)) then
                    d(k) = chain%d(d(k), idx(m))
                else if (present(x)) then
                    d(k) = diff(d(k), x(idx(m)))
                else
                    error stop "jet: neither variables nor chain"
                end if
            end do
        end do
    end function jet

    !> Argument indices of a multi-index, ascending: (1, 0, 1) is [1, 3].
    pure function derivative_indices(alpha) result(idx)
        integer, intent(in) :: alpha(:)
        integer, allocatable :: idx(:)
        integer :: j

        allocate (idx(0))
        do j = 1, size(alpha)
            idx = [idx, spread(j, 1, alpha(j))]
        end do
    end function derivative_indices

    !> Position of alpha among the multi-indices of its total order, in
    !> descending lexicographic order.
    integer function rank_of(alpha)
        integer, intent(in) :: alpha(:)
        type(derivative_set_t) :: same
        integer :: k

        same = partials(size(alpha), sum(alpha))
        same = same%of_order(sum(alpha))
        do k = 1, same%count()
            if (all(same%orders(:, k) == alpha)) then
                rank_of = k
                return
            end if
        end do
        rank_of = 0
    end function rank_of

    integer function count_of_order(n, m)
        integer, intent(in) :: n, m
        type(derivative_set_t) :: same

        same = partials(n, m)
        same = same%of_order(m)
        count_of_order = same%count()
    end function count_of_order

end module fortsym_jet
