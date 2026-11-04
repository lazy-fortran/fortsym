module fortsym_fortgen_adapter
    !! Compatibility bridge from FortSym's established scalar kernel IR to the
    !! shared FortGen IR. FortSym keeps ownership of symbolic lowering; FortGen
    !! owns backend-neutral scalar source generation.
    use fortsym_kernel_ir, only: fs_kernel_ir_t => kernel_ir_t, &
        FS_IR_LITERAL => IR_LITERAL, FS_IR_SYMBOL => IR_SYMBOL, &
        FS_IR_CONSTANT => IR_CONSTANT, FS_IR_ADD => IR_ADD, FS_IR_MUL => IR_MUL, &
        FS_IR_POW => IR_POW, FS_IR_FUNCTION => IR_FUNCTION
    use, intrinsic :: iso_fortran_env, only: real32, real64
    use fortsym_expr, only: expr_t
    use fortsym_kernel_ir, only: lower_kernel_ir
    use fortsym_string, only: chars, str_t
    use fortgen_kernel_ir, only: fg_kernel_ir_t => kernel_ir_t, &
        FG_IR_LITERAL => IR_LITERAL, FG_IR_SYMBOL => IR_SYMBOL, &
        FG_IR_CONSTANT => IR_CONSTANT, FG_IR_ADD => IR_ADD, FG_IR_MUL => IR_MUL, &
        FG_IR_POW => IR_POW, FG_IR_FUNCTION => IR_FUNCTION
    use fortgen_kernel_ir, only: kernel_ir_validate_types, TYPE_REAL32, TYPE_REAL64
    use fortgen_string, only: fg_str => str
    implicit none
    private

    public :: to_fortgen_kernel_ir, real_stage_binding_t, append_real_stage

    type :: real_stage_binding_t
        type(str_t) :: name
        integer :: node = 0
    end type real_stage_binding_t

contains

    subroutine to_fortgen_kernel_ir(source, target, ok, message)
        type(fs_kernel_ir_t), intent(in) :: source
        type(fg_kernel_ir_t), intent(out) :: target
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: i

        ok = .false.
        message = ""
        if (.not. allocated(source%nodes) .or. .not. allocated(source%operands) .or. &
            .not. allocated(source%outputs)) then
            message = "FortSym/FortGen adapter: source IR storage is not allocated"
            return
        end if
        if (source%n_nodes /= size(source%nodes) .or. &
            source%n_operands /= size(source%operands)) then
            message = "FortSym/FortGen adapter: source IR counts do not match storage"
            return
        end if

        allocate(target%nodes(source%n_nodes))
        allocate(target%operands(source%n_operands))
        allocate(target%outputs(size(source%outputs)))
        target%n_nodes = source%n_nodes
        target%n_operands = source%n_operands
        if (source%n_operands > 0) target%operands = source%operands
        if (size(source%outputs) > 0) target%outputs = source%outputs

        do i = 1, source%n_nodes
            target%nodes(i)%first_operand = source%nodes(i)%first_operand
            target%nodes(i)%n_operands = source%nodes(i)%n_operands
            target%nodes(i)%value = source%nodes(i)%value
            select case(source%nodes(i)%operation)
            case (FS_IR_LITERAL)
                target%nodes(i)%operation = FG_IR_LITERAL
            case (FS_IR_SYMBOL)
                target%nodes(i)%operation = FG_IR_SYMBOL
                target%nodes(i)%name = fg_str(chars(source%nodes(i)%name))
            case (FS_IR_CONSTANT)
                target%nodes(i)%operation = FG_IR_CONSTANT
                target%nodes(i)%name = fg_str(chars(source%nodes(i)%name))
            case (FS_IR_ADD)
                target%nodes(i)%operation = FG_IR_ADD
            case (FS_IR_MUL)
                target%nodes(i)%operation = FG_IR_MUL
            case (FS_IR_POW)
                target%nodes(i)%operation = FG_IR_POW
            case (FS_IR_FUNCTION)
                target%nodes(i)%operation = FG_IR_FUNCTION
                target%nodes(i)%name = fg_str(chars(source%nodes(i)%name))
            case default
                message = "FortSym/FortGen adapter: unknown source IR operation"
                return
            end select
        end do
        ok = .true.
    end subroutine to_fortgen_kernel_ir

    ! Stage symbols bind computed values without substituting their exact formulas.
    ! Failure leaves the destination graph intact; successful roots become outputs.
    subroutine append_real_stage(roots, scalar_type, bindings, target, outputs, ok, message)
        type(expr_t), intent(in) :: roots(:)
        integer, intent(in) :: scalar_type
        type(real_stage_binding_t), intent(in) :: bindings(:)
        type(fg_kernel_ir_t), intent(inout) :: target
        integer, allocatable, intent(out) :: outputs(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(fs_kernel_ir_t) :: source
        type(fg_kernel_ir_t) :: work
        integer, allocatable :: mapping(:)
        integer :: i, j, k, first, n, left, right, old_nodes, old_operands, id
        character(:), allocatable :: name

        ok = .false.
        message = "real stage: compute type must be real32 or real64"
        if (scalar_type /= TYPE_REAL32 .and. scalar_type /= TYPE_REAL64) return
        message = "real stage: at least one root is required"
        if (size(roots) == 0) return
        work = target
        if (.not. allocated(work%nodes) .and. .not. allocated(work%operands) .and. &
            .not. allocated(work%outputs)) then
            allocate(work%nodes(0), work%operands(0), work%outputs(0))
            work%n_nodes = 0
            work%n_operands = 0
        end if
        call kernel_ir_validate_types(work, ok, message)
        if (.not. ok) return
        ok = .false.
        message = "real stage: destination must contain only typed real nodes"
        if (any(work%nodes%scalar_type /= TYPE_REAL32 .and. &
            work%nodes%scalar_type /= TYPE_REAL64)) return
        do i = 1, size(bindings)
            message = "real stage: invalid binding name"
            name = chars(bindings(i)%name)
            if (.not. valid_stage_name(name)) return
            do j = 1, i - 1
                message = "real stage: duplicate binding name"
                if (name == chars(bindings(j)%name)) return
            end do
            message = "real stage: binding node is outside destination graph"
            id = bindings(i)%node
            if (id < 1 .or. id > work%n_nodes) return
            message = "real stage: binding type mismatch; insert an explicit cast"
            if (work%nodes(id)%scalar_type /= scalar_type) return
        end do
        call lower_kernel_ir(roots, source, ok, message)
        if (.not. ok) return
        ok = .false.
        allocate(mapping(source%n_nodes))
        old_nodes = work%n_nodes
        old_operands = work%n_operands
        ! A binary left fold needs at most one node per original operand.
        deallocate(work%nodes, work%operands)
        allocate(work%nodes(old_nodes + source%n_nodes + source%n_operands))
        allocate(work%operands(old_operands + 2*source%n_operands))
        if (old_nodes > 0) work%nodes(:old_nodes) = target%nodes
        if (old_operands > 0) work%operands(:old_operands) = target%operands
        do i = 1, source%n_nodes
            first = source%nodes(i)%first_operand
            n = source%nodes(i)%n_operands
            select case (source%nodes(i)%operation)
            case (FS_IR_SYMBOL)
                name = chars(source%nodes(i)%name)
                message = "real stage: every symbol needs a binding: "//name
                mapping(i) = 0
                do j = 1, size(bindings)
                    if (name == chars(bindings(j)%name)) mapping(i) = bindings(j)%node
                end do
                if (mapping(i) == 0) return
            case (FS_IR_LITERAL)
                message = "real stage: literal exceeds real32 range"
                if (scalar_type == TYPE_REAL32) then
                    if (abs(source%nodes(i)%value) > real(huge(0.0_real32), real64)) return
                end if
                mapping(i) = append_node(FG_IR_LITERAL)
                work%nodes(mapping(i))%value = source%nodes(i)%value
            case (FS_IR_ADD, FS_IR_MUL)
                message = "real stage: arithmetic needs at least two operands"
                if (n < 2) return
                left = mapping(source%operands(first))
                do k = first + 1, first + n - 1
                    right = mapping(source%operands(k))
                    if (source%nodes(i)%operation == FS_IR_ADD) then
                        left = append_node(FG_IR_ADD, left, right)
                    else
                        left = append_node(FG_IR_MUL, left, right)
                    end if
                end do
                mapping(i) = left
            case default
                message = "real stage: unsupported operation (only literals, symbols, sums and products)"
                return
            end select
        end do
        work%nodes = work%nodes(:work%n_nodes)
        work%operands = work%operands(:work%n_operands)
        work%outputs = mapping(source%outputs)
        call kernel_ir_validate_types(work, ok, message)
        if (.not. ok) return
        outputs = work%outputs
        target = work
    contains
        integer function append_node(operation, a, b) result(node)
            integer, intent(in) :: operation
            integer, intent(in), optional :: a, b
            work%n_nodes = work%n_nodes + 1
            node = work%n_nodes
            work%nodes(node)%operation = operation
            work%nodes(node)%scalar_type = scalar_type
            work%nodes(node)%first_operand = work%n_operands + 1
            if (present(a) .and. present(b)) then
                work%nodes(node)%n_operands = 2
                work%operands(work%n_operands + 1:work%n_operands + 2) = [a, b]
                work%n_operands = work%n_operands + 2
            end if
        end function append_node
    end subroutine append_real_stage

    logical function valid_stage_name(name) result(valid)
        character(*), intent(in) :: name
        integer :: i, c
        valid = .false.
        if (len(name) == 0) return
        do i = 1, len(name)
            c = iachar(name(i:i))
            if ((c >= iachar('a') .and. c <= iachar('z')) .or. &
                (c >= iachar('A') .and. c <= iachar('Z'))) cycle
            if (i > 1 .and. (name(i:i) == '_' .or. &
                (c >= iachar('0') .and. c <= iachar('9')))) cycle
            return
        end do
        valid = .true.
    end function valid_stage_name

end module fortsym_fortgen_adapter
