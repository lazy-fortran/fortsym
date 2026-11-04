program test_fortsym_real_stages
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), operator(*), sin, pi_expr, real_expr, num, operator(**)
    use fortsym_string, only: str
    use fortsym_fortgen_adapter, only: append_real_stage, real_stage_binding_t
    use fortgen_kernel_ir
    use fortgen_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, emit_metal_device_ir
    use fortgen_string, only: fg_str => str, str_t, chars
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: a, b, rounded, value, z
    type(kernel_ir_t) :: ir, before, empty
    type(kernel_emit_spec_t) :: spec
    type(real_stage_binding_t) :: bindings(3)
    type(str_t) :: source
    integer, allocatable :: outputs(:), bulk(:), residual(:)
    character(:), allocatable :: message
    character(2048) :: path
    logical :: ok
    integer :: unit, mode, n, high_product, late_bulk, late_wide, late_residual

    call arena%init()
    a = sym(arena, 'a'); b = sym(arena, 'b'); rounded = sym(arena, 'rounded')
    call kernel_ir_allocate(ir, 4, 2, 0)
    ir%nodes(1)%operation = IR_SYMBOL; ir%nodes(1)%name = fg_str('x')
    ir%nodes(2)%operation = IR_SYMBOL; ir%nodes(2)%name = fg_str('y')
    ir%nodes(1:2)%scalar_type = TYPE_REAL64
    ir%nodes(3:4)%operation = IR_CAST
    ir%nodes(3:4)%scalar_type = TYPE_REAL32
    ir%nodes(3)%first_operand = 1; ir%nodes(4)%first_operand = 2
    ir%nodes(3:4)%n_operands = 1
    ir%operands = [1, 2]
    bindings(1) = real_stage_binding_t(str('a'), 3)
    bindings(2) = real_stage_binding_t(str('b'), 4)
    value = a*b
    call append_real_stage([value, value], TYPE_REAL32, bindings(:2), ir, bulk, ok, message)
    call check(ok, message)
    call check(bulk(1) == bulk(2), 'shared math-stage expression computed once')
    n = ir%n_nodes
    call add_node(IR_CAST, TYPE_REAL64, [bulk(1)])
    bindings(1)%node = 1; bindings(2)%node = 2
    bindings(3) = real_stage_binding_t(str('rounded'), ir%n_nodes)
    call append_real_stage([a*b-rounded], TYPE_REAL64, bindings, ir, residual, ok, message)
    call check(ok, message)
    call add_node(IR_ADD, TYPE_REAL64, [n+1, residual(1)])
    ir%outputs = [bulk(1), residual(1), ir%n_nodes]
    spec%name = fg_str('staged_product')
    spec%args = [fg_str('x'), fg_str('y')]
    spec%arg_types = [TYPE_REAL64, TYPE_REAL64]
    spec%outputs = [fg_str('bulk'), fg_str('residual'), fg_str('corrected')]
    spec%output_types = [TYPE_REAL32, TYPE_REAL64, TYPE_REAL64]
    spec%policy%small_power_limit = 0
    spec%policy%fold_exact_constants = .false.
    spec%policy%eliminate_constant_divisions = .false.
    spec%policy%shape_fma = .false.
    source = emit_fortran_kernel_ir(ir, spec, ok, message)
    call check(ok, message)
    source = emit_cuda_device_ir(ir, spec, ok, message)
    call check(ok, message)
    source = emit_metal_device_ir(ir, spec, ok, message)
    call check(.not. ok, 'Metal refuses native real64 stage graph')
    before = ir
    call append_real_stage([a], TYPE_BOOL, bindings, ir, outputs, ok, message)
    call refused('unsupported compute type')
    bindings(1)%node = 3
    call append_real_stage([a], TYPE_REAL64, bindings, ir, outputs, ok, message)
    call refused('missing widening cast')
    bindings(1)%node = 1
    bindings(3)%name = str('a')
    call append_real_stage([a], TYPE_REAL64, bindings, ir, outputs, ok, message)
    call refused('duplicate binding')
    bindings(3)%name = str('bad-name')
    call append_real_stage([a], TYPE_REAL64, bindings, ir, outputs, ok, message)
    call refused('invalid formal name')
    z = sym(arena, 'unbound')
    call append_real_stage([z], TYPE_REAL64, bindings(:2), ir, outputs, ok, message)
    call refused('unbound symbol')
    call append_real_stage([a**2], TYPE_REAL64, bindings(:2), ir, outputs, ok, message)
    call refused('unsupported power')
    call append_real_stage([real_expr(arena,1.0e100_real64)], TYPE_REAL32, &
        bindings(:0), ir, outputs, ok, message)
    call refused('out-of-range real32 literal')
    call append_real_stage([num(arena,2)], TYPE_REAL64, &
        bindings(:0), empty, outputs, ok, message)
    call check(ok .and. empty%n_nodes == 1, 'literal-only stage initializes graph')
    call append_real_stage([pi_expr(arena)], TYPE_REAL64, bindings(:2), ir, outputs, ok, message)
    call refused('unsupported named constant')
    call append_real_stage([sin(a)], TYPE_REAL64, bindings(:2), ir, outputs, ok, message)
    call refused('unsupported function')
    call append_real_stage([a+b+z], TYPE_REAL64, bindings(:2), ir, outputs, ok, message)
    call refused('transactional late failure')
    bindings(3) = real_stage_binding_t(str('unbound'), 1)
    call append_real_stage([a+b+z], TYPE_REAL64, bindings, ir, outputs, ok, message)
    call check(ok, 'n-ary sum lowers to binary: '//message)
    call check(ir%n_nodes == before%n_nodes + 2, 'three-term sum requires two additions')
    ir = before
    do mode = 1, min(2, command_argument_count())
        call get_command_argument(mode, path)
        if (mode == 1) source = emit_fortran_kernel_ir(ir, spec, ok, message)
        if (mode == 2) source = emit_cuda_device_ir(ir, spec, ok, message)
        call check(ok, message)
        open(newunit=unit, file=trim(path), status='replace')
        write(unit,'(a)') chars(source)
        spec%name = fg_str('staged_product_widened')
        ir%outputs(3) = n+1
        if (mode == 1) source = emit_fortran_kernel_ir(ir, spec, ok, message)
        if (mode == 2) source = emit_cuda_device_ir(ir, spec, ok, message)
        call check(ok, message)
        write(unit,'(a)') chars(source)
        ir = before
        call add_node(IR_LITERAL, TYPE_REAL64, [integer ::])
        ir%outputs(2) = ir%n_nodes
        ir%outputs(3) = n+1
        spec%name = fg_str('staged_product_cse')
        if (mode == 1) source = emit_fortran_kernel_ir(ir, spec, ok, message)
        if (mode == 2) source = emit_cuda_device_ir(ir, spec, ok, message)
        call check(ok, message)
        write(unit,'(a)') chars(source)
        ir = before
        call add_node(IR_MUL, TYPE_REAL64, [1, 2])
        high_product = ir%n_nodes
        call add_node(IR_CAST, TYPE_REAL32, [high_product])
        late_bulk = ir%n_nodes
        call add_node(IR_CAST, TYPE_REAL64, [late_bulk])
        late_wide = ir%n_nodes
        call add_node(IR_SUB, TYPE_REAL64, [high_product, late_wide])
        late_residual = ir%n_nodes
        call add_node(IR_ADD, TYPE_REAL64, [late_wide, late_residual])
        ir%outputs = [late_bulk, late_residual, ir%n_nodes]
        spec%name = fg_str('staged_product_late_cast')
        if (mode == 1) source = emit_fortran_kernel_ir(ir, spec, ok, message)
        if (mode == 2) source = emit_cuda_device_ir(ir, spec, ok, message)
        call check(ok, message)
        write(unit,'(a)') chars(source)
        close(unit)
        ir = before
        spec%name = fg_str('staged_product')
    end do
contains
    subroutine add_node(op, scalar, operands)
        integer, intent(in) :: op, scalar, operands(:)
        type(kernel_ir_node_t) :: node
        node%operation = op; node%scalar_type = scalar
        node%first_operand = ir%n_operands + 1
        node%n_operands = size(operands)
        ir%nodes = [ir%nodes, node]
        ir%operands = [ir%operands, operands]
        ir%n_nodes = size(ir%nodes); ir%n_operands = size(ir%operands)
    end subroutine add_node
    subroutine refused(label)
        character(*), intent(in) :: label
        call check(.not. ok .and. len(message) > 0, label)
        call check(ir%n_nodes == before%n_nodes .and. ir%n_operands == before%n_operands, &
            'failure preserves destination counts')
        call check(all(ir%outputs == before%outputs), 'failure preserves outputs')
        call check(all(ir%operands == before%operands), 'failure preserves operands')
    end subroutine refused
    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label
        if (condition) return
        print *, 'FAIL: ', label
        error stop 1
    end subroutine check
end program test_fortsym_real_stages
