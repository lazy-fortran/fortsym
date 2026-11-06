module fortsym_context
    ! The arena and kernel bindings that arena-free constructors and the
    ! emitters use.
    !
    ! A generator states its mathematics with sym('phi') and func('Bmod', x)
    ! instead of threading an arena through every call. Those constructors
    ! intern into the current context. Without an open context that is a
    ! process-wide base arena; `call ctx%open()` starts a fresh arena with
    ! empty bindings, and `call ctx%close()` clears it and returns to the
    ! enclosing one. Contexts nest. Closing clears the arena rather than
    ! freeing it, so a handle that outlives its context fails the generation
    ! check instead of pointing at freed memory.
    !
    ! This is single-threaded process state. Concurrent or independent
    ! derivations keep using explicit arenas with sym(arena, name).
    use fortsym_arena, only: arena_t
    use fortsym_string, only: str_t, str, chars
    implicit none
    private

    public :: context_t, current_arena
    public :: kernel_binding_t, set_binding, active_bindings

    !> Replacement for an applied function in generated code. An empty
    !> derivative_indices list binds the function itself; a nonempty list
    !> binds the canonical partial derivative with those argument indices.
    !> The optional `%args%` marker in replacement is expanded to the
    !> original call arguments.
    type :: kernel_binding_t
        type(str_t)              :: head
        integer, allocatable     :: derivative_indices(:)
        type(str_t)              :: replacement
    end type kernel_binding_t

    type :: frame_t
        type(arena_t), pointer :: arena => null()
        type(kernel_binding_t), allocatable :: bindings(:)
    end type frame_t

    !> A handle on one level of the context stack.
    type :: context_t
        integer :: level = 0
    contains
        procedure :: open => context_open
        procedure :: close => context_close
    end type context_t

    ! frames(0) is the base context; frames(1:depth) are open contexts. The
    ! arenas of closed levels stay allocated for reuse.
    type(frame_t), allocatable, save :: frames(:)
    integer, save :: depth = 0

contains

    !> The arena that arena-free constructors intern into.
    function current_arena() result(a)
        type(arena_t), pointer :: a

        call ensure_base()
        a => frames(depth)%arena
    end function current_arena

    subroutine context_open(self)
        class(context_t), intent(inout) :: self
        type(frame_t), allocatable :: grown(:)

        if (self%level /= 0) error stop "context: already open"
        call ensure_base()
        if (depth + 1 > ubound(frames, 1)) then
            allocate (grown(0:2*ubound(frames, 1) + 1))
            grown(0:ubound(frames, 1)) = frames
            call move_alloc(grown, frames)
        end if
        depth = depth + 1
        if (.not. associated(frames(depth)%arena)) allocate (frames(depth)%arena)
        call frames(depth)%arena%init()
        allocate (frames(depth)%bindings(0))
        self%level = depth
    end subroutine context_open

    subroutine context_close(self)
        class(context_t), intent(inout) :: self

        if (self%level == 0) return
        if (self%level /= depth) error stop "context: closed out of order"
        call frames(depth)%arena%clear()
        deallocate (frames(depth)%bindings)
        depth = depth - 1
        self%level = 0
    end subroutine context_close

    !> Bind `head` (or its partial derivative by argument indices) to
    !> `replacement` in the current context, replacing an earlier binding of
    !> the same head and indices.
    subroutine set_binding(head, indices, replacement)
        character(*), intent(in) :: head, replacement
        integer, intent(in) :: indices(:)
        type(kernel_binding_t) :: b
        integer :: k

        call ensure_base()
        associate (list => frames(depth)%bindings)
            do k = 1, size(list)
                if (chars(list(k)%head) /= head) cycle
                if (size(list(k)%derivative_indices) /= size(indices)) cycle
                if (any(list(k)%derivative_indices /= indices)) cycle
                list(k)%replacement = str(replacement)
                return
            end do
        end associate
        b%head = str(head)
        b%derivative_indices = indices
        b%replacement = str(replacement)
        frames(depth)%bindings = [frames(depth)%bindings, b]
    end subroutine set_binding

    !> The bindings of the current context; unallocated when there are none,
    !> so that an emitter sees them as absent.
    subroutine active_bindings(bindings)
        type(kernel_binding_t), allocatable, intent(out) :: bindings(:)

        call ensure_base()
        if (size(frames(depth)%bindings) > 0) bindings = frames(depth)%bindings
    end subroutine active_bindings

    subroutine ensure_base()
        if (allocated(frames)) return
        allocate (frames(0:3))
        allocate (frames(0)%arena)
        call frames(0)%arena%init()
        allocate (frames(0)%bindings(0))
    end subroutine ensure_base

end module fortsym_context
