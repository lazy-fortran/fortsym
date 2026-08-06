module fortsym_kernel_ir
    ! Backend-neutral lowering for scalar expression kernels.
    !
    ! The expression arena is an excellent symbolic representation but it is
    ! not a code-generator interface: it exposes Fortran-owned names, exact
    ! arithmetic, and engine-facing node kinds. This module freezes a reachable
    ! expression DAG into a compact, typed IR. Fortran, CUDA, HIP, and SYCL
    ! emitters can consume the same node and operand arrays without adding
    ! backend syntax to the symbolic printer.
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL
    use fortsym_exact, only: exact_to_real
    use fortsym_expr, only: expr_t
    use fortsym_string, only: chars, str_t
    implicit none
    private

    public :: kernel_ir_t, kernel_ir_node_t, lower_kernel_ir
    public :: IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, &
        IR_FUNCTION

    integer, parameter :: dp = real64

    integer, parameter :: IR_LITERAL = 1
    integer, parameter :: IR_SYMBOL = 2
    integer, parameter :: IR_CONSTANT = 3
    integer, parameter :: IR_ADD = 4
    integer, parameter :: IR_MUL = 5
    integer, parameter :: IR_POW = 6
    integer, parameter :: IR_FUNCTION = 7

    type :: kernel_ir_node_t
        integer :: operation = 0
        integer :: first_operand = 0
        integer :: n_operands = 0
        real(dp) :: value = 0.0_dp
        type(str_t) :: name
    end type kernel_ir_node_t

    type :: kernel_ir_t
        type(kernel_ir_node_t), allocatable :: nodes(:)
        integer, allocatable :: operands(:)
        integer, allocatable :: outputs(:)
        integer :: n_nodes = 0
        integer :: n_operands = 0
    contains
        procedure :: clear => kernel_ir_clear
    end type kernel_ir_t

contains

    subroutine kernel_ir_clear(self)
        class(kernel_ir_t), intent(inout) :: self

        if (allocated(self%nodes)) deallocate(self%nodes)
        if (allocated(self%operands)) deallocate(self%operands)
        if (allocated(self%outputs)) deallocate(self%outputs)
        self%n_nodes = 0
        self%n_operands = 0
    end subroutine kernel_ir_clear

    subroutine lower_kernel_ir(roots, ir, ok, message)
        type(expr_t), intent(in) :: roots(:)
        type(kernel_ir_t), intent(inout) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        type(arena_t), pointer :: arena
        logical, allocatable :: seen(:)
        integer, allocatable :: mapping(:)
        integer :: node_count, operand_count, node_cursor, operand_cursor
        integer :: k, output_node

        call ir%clear()
        ok = .false.
        message = ""
        if (size(roots) == 0) then
            allocate(ir%nodes(0), ir%operands(0), ir%outputs(0))
            ok = .true.
            return
        end if
        if (.not. associated(roots(1)%a)) then
            message = "kernel IR: first root is invalid"
            return
        end if
        arena => roots(1)%a
        do k = 1, size(roots)
            if (.not. associated(roots(k)%a)) then
                message = "kernel IR: root is invalid"
                return
            end if
            if (.not. associated(roots(k)%a, arena)) then
                message = "kernel IR: roots belong to different arenas"
                return
            end if
            if (roots(k)%id < 1 .or. roots(k)%id > arena%size()) then
                message = "kernel IR: root node index is invalid"
                return
            end if
        end do

        allocate(seen(arena%size()), source=.false.)
        node_count = 0
        operand_count = 0
        do k = 1, size(roots)
            call count_reachable(arena, roots(k)%id, seen, node_count, &
                operand_count, ok, message)
            if (.not. ok) return
        end do

        allocate(ir%nodes(node_count), ir%operands(operand_count), &
            ir%outputs(size(roots)))
        allocate(mapping(arena%size()), source=0)
        node_cursor = 0
        operand_cursor = 0
        ok = .true.
        do k = 1, size(roots)
            call lower_node(arena, roots(k)%id, mapping, ir, node_cursor, &
                operand_cursor, output_node, ok, message)
            if (.not. ok) return
            ir%outputs(k) = output_node
        end do
        ir%n_nodes = node_cursor
        ir%n_operands = operand_cursor
    end subroutine lower_kernel_ir

    recursive subroutine count_reachable(arena, id, seen, node_count, &
            operand_count, ok, message)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        logical, intent(inout) :: seen(:)
        integer, intent(inout) :: node_count, operand_count
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: k, child

        ok = .false.
        message = ""
        if (id < 1 .or. id > arena%size()) then
            message = "kernel IR: child node index is invalid"
            return
        end if
        if (seen(id)) then
            ok = .true.
            return
        end if
        seen(id) = .true.
        node_count = node_count + 1
        operand_count = operand_count + arena%nargs_of(id)
        do k = 1, arena%nargs_of(id)
            child = arena%arg_of(id, k)
            call count_reachable(arena, child, seen, node_count, operand_count, &
                ok, message)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine count_reachable

    recursive subroutine lower_node(arena, id, mapping, ir, node_cursor, &
            operand_cursor, node_index, ok, message)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        integer, intent(inout) :: mapping(:)
        type(kernel_ir_t), intent(inout) :: ir
        integer, intent(inout) :: node_cursor, operand_cursor
        integer, intent(out) :: node_index
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: k, child, child_index, kind

        ok = .false.
        message = ""
        node_index = 0
        if (id < 1 .or. id > arena%size()) then
            message = "kernel IR: node index is invalid"
            return
        end if
        if (mapping(id) > 0) then
            node_index = mapping(id)
            ok = .true.
            return
        end if

        do k = 1, arena%nargs_of(id)
            child = arena%arg_of(id, k)
            call lower_node(arena, child, mapping, ir, node_cursor, &
                operand_cursor, child_index, ok, message)
            if (.not. ok) return
        end do

        node_cursor = node_cursor + 1
        node_index = node_cursor
        mapping(id) = node_index
        ir%nodes(node_index)%first_operand = operand_cursor + 1
        ir%nodes(node_index)%n_operands = arena%nargs_of(id)
        do k = 1, arena%nargs_of(id)
            child = arena%arg_of(id, k)
            child_index = mapping(child)
            operand_cursor = operand_cursor + 1
            ir%operands(operand_cursor) = child_index
        end do

        kind = arena%kind_of(id)
        select case (kind)
        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL)
            ir%nodes(node_index)%operation = IR_LITERAL
            call literal_value(arena, id, ir%nodes(node_index)%value, ok, &
                message)
            if (.not. ok) return
        case (NK_SYM)
            ir%nodes(node_index)%operation = IR_SYMBOL
            ir%nodes(node_index)%name = arena%name_of(id)
        case (NK_CONST)
            ir%nodes(node_index)%operation = IR_CONSTANT
            ir%nodes(node_index)%name = arena%name_of(id)
        case (NK_ADD)
            ir%nodes(node_index)%operation = IR_ADD
        case (NK_MUL)
            ir%nodes(node_index)%operation = IR_MUL
        case (NK_POW)
            ir%nodes(node_index)%operation = IR_POW
        case (NK_FUNC)
            ir%nodes(node_index)%operation = IR_FUNCTION
            ir%nodes(node_index)%name = arena%name_of(id)
        case default
            message = "kernel IR: unsupported arena node kind"
            return
        end select
        ok = .true.
    end subroutine lower_node

    subroutine literal_value(arena, id, value, ok, message)
        type(arena_t), intent(in) :: arena
        integer, intent(in) :: id
        real(dp), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        character(:), allocatable :: text
        integer :: ios

        ok = .true.
        message = ""
        select case (arena%kind_of(id))
        case (NK_INT)
            value = real(arena%num_of(id), dp)
        case (NK_RAT)
            value = real(arena%num_of(id), dp)/real(arena%den_of(id), dp)
        case (NK_REAL)
            value = arena%real_of(id)
        case (NK_BIG_INT, NK_BIG_RAT)
            value = exact_to_real(chars(arena%exact_text_of(id)), ok)
            if (.not. ok) message = "kernel IR: exact literal is not finite"
        case (NK_BIG_REAL)
            text = chars(arena%real_text_of(id))
            read(text, *, iostat=ios) value
            if (ios /= 0) then
                ok = .false.
                message = "kernel IR: decimal literal cannot be parsed"
            end if
        end select
        if (ok) then
            if (.not. ieee_is_finite(value)) then
                ok = .false.
                message = "kernel IR: literal is not finite"
            end if
        end if
    end subroutine literal_value

end module fortsym_kernel_ir
