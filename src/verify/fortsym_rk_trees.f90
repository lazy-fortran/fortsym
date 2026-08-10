module fortsym_rk_trees
    !> Rooted trees, and the two functions on them that Runge-Kutta order
    !> theory is written in.
    !>
    !> A tree is a root carrying a multiset of subtrees. Butcher's order
    !> conditions say a method attains order p exactly when, for every rooted
    !> tree t with order(t) <= p,
    !>
    !>     sum_i b_i * Phi_i(t) = 1 / gamma(t)
    !>
    !> with
    !>
    !>     Phi_i(bullet) = 1
    !>     Phi_i(t)      = product over children tk of ( sum_j a_ij Phi_j(tk) )
    !>     gamma(bullet) = 1
    !>     gamma(t)      = order(t) * product over children tk of gamma(tk)
    !>
    !> This module owns the trees and gamma. fortsym_rk owns Phi, because Phi
    !> needs a tableau.
    !>
    !> Trees are held in one flat table. A tree is an index into it, and its
    !> children are indices of strictly earlier entries, so the table can be
    !> built one order at a time and read without recursion into storage.
    !> Children are kept in non-decreasing index order, which makes the
    !> multiset canonical and stops the same tree being enumerated twice.
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    public :: tree_table_t, build_rooted_trees, tree_gamma, tree_count_of_order

    !> Largest number of children a single node may carry. A node of a tree of
    !> order p has at most p-1 children, so this bounds the supported order at
    !> 17, far above the order 8 that DOP853 needs.
    integer, parameter, public :: RK_MAX_CHILDREN = 16

    type :: tree_table_t
        !> Number of trees held.
        integer :: n = 0
        !> order(k) is the node count of tree k.
        integer, allocatable :: order(:)
        !> nchild(k) is the number of subtrees hanging off tree k's root.
        integer, allocatable :: nchild(:)
        !> child(j, k) is the table index of tree k's j-th subtree. Children
        !> are non-decreasing in j.
        integer, allocatable :: child(:, :)
    end type tree_table_t

contains

    !> Every rooted tree of order 1 through max_order, in non-decreasing order.
    !>
    !> Tree 1 is the single node. A tree of order r is a root plus a multiset
    !> of already-built trees whose orders sum to r-1, so the table is complete
    !> at each order before the next one starts.
    subroutine build_rooted_trees(max_order, table, ok)
        integer, intent(in) :: max_order
        type(tree_table_t), intent(out) :: table
        logical, intent(out) :: ok

        integer :: capacity, r
        integer :: children(RK_MAX_CHILDREN)

        ok = .false.
        if (max_order < 1) return

        capacity = tree_capacity(max_order)
        allocate (table%order(capacity), table%nchild(capacity))
        allocate (table%child(RK_MAX_CHILDREN, capacity))
        table%order = 0
        table%nchild = 0
        table%child = 0
        table%n = 0

        ! The single node. It has no children, and every other tree is built
        ! from it upwards.
        call append_tree(table, 1, 0, children, ok)
        if (.not. ok) return

        do r = 2, max_order
            call extend_with_order(table, r, ok)
            if (.not. ok) return
        end do

        ok = .true.
    end subroutine build_rooted_trees

    !> Append every tree of order r, built from the trees already present.
    subroutine extend_with_order(table, r, ok)
        type(tree_table_t), intent(inout) :: table
        integer, intent(in) :: r
        logical, intent(out) :: ok

        integer :: children(RK_MAX_CHILDREN)
        integer :: available

        ! Only trees of order < r can be children, and every one of those is
        ! already in the table because orders are built in sequence.
        available = table%n
        children = 0
        call emit_child_multisets(table, r - 1, 1, available, 0, children, ok)
    end subroutine extend_with_order

    !> Walk multisets of existing trees whose orders sum to `remaining`, and
    !> append one tree of order remaining+1 per multiset.
    !>
    !> `first` is the smallest child index still allowed. Requiring children to
    !> be non-decreasing makes each multiset appear exactly once.
    recursive subroutine emit_child_multisets(table, remaining, first, &
                                              available, depth, children, ok)
        type(tree_table_t), intent(inout) :: table
        integer, intent(in) :: remaining, first, available, depth
        integer, intent(inout) :: children(:)
        logical, intent(out) :: ok

        integer :: candidate, child_order

        ok = .true.
        if (remaining == 0) then
            call append_tree(table, depth_order(table, depth, children), &
                             depth, children, ok)
            return
        end if

        do candidate = first, available
            child_order = table%order(candidate)
            if (child_order > remaining) cycle
            if (depth + 1 > RK_MAX_CHILDREN) then
                ok = .false.
                return
            end if
            children(depth + 1) = candidate
            call emit_child_multisets(table, remaining - child_order, &
                                      candidate, available, depth + 1, &
                                      children, ok)
            if (.not. ok) return
        end do
    end subroutine emit_child_multisets

    !> Node count of a root carrying the first `depth` entries of `children`.
    pure function depth_order(table, depth, children) result(order)
        type(tree_table_t), intent(in) :: table
        integer, intent(in) :: depth
        integer, intent(in) :: children(:)
        integer :: order
        integer :: j

        order = 1
        do j = 1, depth
            order = order + table%order(children(j))
        end do
    end function depth_order

    subroutine append_tree(table, order, nchild, children, ok)
        type(tree_table_t), intent(inout) :: table
        integer, intent(in) :: order, nchild
        integer, intent(in) :: children(:)
        logical, intent(out) :: ok

        integer :: j

        ok = table%n < size(table%order)
        if (.not. ok) return

        table%n = table%n + 1
        table%order(table%n) = order
        table%nchild(table%n) = nchild
        do j = 1, nchild
            table%child(j, table%n) = children(j)
        end do
    end subroutine append_tree

    !> gamma(t): order(t) times the product of gamma over its subtrees.
    !>
    !> The reciprocal of this is the exact value each order condition has to
    !> reproduce, so it is returned as an integer and inverted by the caller
    !> in exact arithmetic rather than in floating point.
    recursive function tree_gamma(table, k) result(value)
        type(tree_table_t), intent(in) :: table
        integer, intent(in) :: k
        integer(int64) :: value
        integer :: j

        value = int(table%order(k), int64)
        do j = 1, table%nchild(k)
            value = value*tree_gamma(table, table%child(j, k))
        end do
    end function tree_gamma

    !> How many trees the table holds at exactly this order.
    pure function tree_count_of_order(table, order) result(n)
        type(tree_table_t), intent(in) :: table
        integer, intent(in) :: order
        integer :: n
        integer :: k

        n = 0
        do k = 1, table%n
            if (table%order(k) == order) n = n + 1
        end do
    end function tree_count_of_order

    !> Table size to allocate. The number of rooted trees grows quickly and
    !> has no simple closed form, so this uses the known counts and a generous
    !> geometric continuation beyond them rather than a tight bound.
    pure function tree_capacity(max_order) result(capacity)
        integer, intent(in) :: max_order
        integer :: capacity
        integer, parameter :: known(10) = &
            [1, 1, 2, 4, 9, 20, 48, 115, 286, 719]
        integer :: r

        capacity = 0
        do r = 1, max_order
            if (r <= size(known)) then
                capacity = capacity + known(r)
            else
                capacity = capacity*3
            end if
        end do
        capacity = capacity + 8
    end function tree_capacity

end module fortsym_rk_trees
