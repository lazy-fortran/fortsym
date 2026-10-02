module fortsym_fortgen_adapter
    !! Compatibility bridge from FortSym's established scalar kernel IR to the
    !! shared FortGen IR. FortSym keeps ownership of symbolic lowering; FortGen
    !! owns backend-neutral scalar source generation.
    use fortsym_kernel_ir, only: fs_kernel_ir_t => kernel_ir_t, &
        FS_IR_LITERAL => IR_LITERAL, FS_IR_SYMBOL => IR_SYMBOL, &
        FS_IR_CONSTANT => IR_CONSTANT, FS_IR_ADD => IR_ADD, FS_IR_MUL => IR_MUL, &
        FS_IR_POW => IR_POW, FS_IR_FUNCTION => IR_FUNCTION
    use fortsym_string, only: chars
    use fortgen_kernel_ir, only: fg_kernel_ir_t => kernel_ir_t, &
        FG_IR_LITERAL => IR_LITERAL, FG_IR_SYMBOL => IR_SYMBOL, &
        FG_IR_CONSTANT => IR_CONSTANT, FG_IR_ADD => IR_ADD, FG_IR_MUL => IR_MUL, &
        FG_IR_POW => IR_POW, FG_IR_FUNCTION => IR_FUNCTION
    use fortgen_string, only: fg_str => str
    implicit none
    private

    public :: to_fortgen_kernel_ir

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

end module fortsym_fortgen_adapter
