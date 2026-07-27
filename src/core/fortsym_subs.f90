module fortsym_subs
    ! Structural substitution over the native expression DAG.
    !
    ! Matches are tested before descending. Replacement expressions are not
    ! visited, giving simultaneous semantics for swaps and coupled ansatzes.
    use fortsym_arena, only: NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, is_valid, same_arena
    use fortsym_string, only: chars
    implicit none
    private

    public :: subs, subs_many

contains

    function subs(e, old, new) result(r)
        type(expr_t), intent(in) :: e, old, new
        type(expr_t) :: r

        r = subs_many(e, [old], [new])
    end function subs

    function subs_many(e, old, new) result(r)
        type(expr_t), intent(in) :: e
        type(expr_t), intent(in) :: old(:), new(:)
        type(expr_t) :: r
        integer, allocatable :: memo(:)
        integer :: i

        if (.not. is_valid(e)) return
        if (size(old) /= size(new)) return
        do i = 1, size(old)
            if (.not. is_valid(old(i))) return
            if (.not. is_valid(new(i))) return
            if (.not. same_arena(e, old(i))) return
            if (.not. same_arena(e, new(i))) return
        end do

        allocate (memo(e%a%size()), source=0)
        r%a => e%a
        r%id = substitute_node(e%id, e, old, new, memo)
    end function subs_many

    recursive function substitute_node(id, root, old, new, memo) result(result_id)
        integer, intent(in) :: id
        type(expr_t), intent(in) :: root
        type(expr_t), intent(in) :: old(:), new(:)
        integer, intent(inout) :: memo(:)
        integer :: result_id
        integer, allocatable :: args(:)
        integer :: i, kind, nargs

        if (memo(id) /= 0) then
            result_id = memo(id)
            return
        end if
        do i = 1, size(old)
            if (id == old(i)%id) then
                result_id = new(i)%id
                memo(id) = result_id
                return
            end if
        end do

        kind = root%a%kind_of(id)
        nargs = root%a%nargs_of(id)
        select case (kind)
        case (NK_ADD, NK_MUL, NK_POW, NK_FUNC)
            allocate (args(nargs))
            do i = 1, nargs
                args(i) = substitute_node(root%a%arg_of(id, i), root, old, new, memo)
            end do
            select case (kind)
            case (NK_ADD)
                result_id = root%a%add(args)
            case (NK_MUL)
                result_id = root%a%mul(args)
            case (NK_POW)
                result_id = root%a%pow(args(1), args(2))
            case (NK_FUNC)
                result_id = root%a%func(chars(root%a%name_of(id)), args)
            end select
        case default
            result_id = id
        end select
        memo(id) = result_id
    end function substitute_node

end module fortsym_subs
