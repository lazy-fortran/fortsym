program gen_rbf_derivatives
    ! Generate the RBF value and natural-parameter derivative leaf consumed by
    ! FortML.  Native simplification is deliberate: SymEngine currently
    ! refuses this derivative DAG, so no unproved external rewrite is used.
    use, intrinsic :: iso_fortran_env, only: output_unit, real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, exp_expr => exp, operator(*), &
        operator(-), operator(**)
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir, IR_ADD, IR_MUL, &
        IR_POW, IR_FUNCTION
    use fortsym_string, only: chars, str, str_t
    implicit none
    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(native_engine_t) :: eng
    type(expr_t) :: variance, distance, lengthscale, value, roots(4)
    type(engine_result_t) :: simplified
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(len=2048) :: output, revision
    character(:), allocatable :: message
    integer :: output_length, revision_length, status, unit, ios, k
    logical :: good

    call get_command_argument(1, output, length=output_length, status=status)
    call get_command_argument(2, revision, length=revision_length, status=status)
    if (output_length == 0 .or. revision_length == 0) then
        write (output_unit, '(a)') &
            'usage: gen_rbf_derivatives OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    eng = make_native_engine(arena)
    variance = sym(arena, 'variance')
    distance = sym(arena, 'distance')
    lengthscale = sym(arena, 'lengthscale')
    value = variance*exp_expr(-0.5_dp*distance*lengthscale**(-2))
    roots(1) = value
    roots(2) = diff(value, variance)
    roots(3) = diff(value, distance)
    roots(4) = diff(value, lengthscale)
    do k = 1, size(roots)
        simplified = eng%simplify(roots(k))
        if (.not. simplified%ok) then
            write (output_unit, '(a)') 'simplification failed: '// &
                chars(simplified%message)
            error stop 1
        end if
        roots(k) = simplified%value
    end do

    call lower_kernel_ir(roots, ir, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'lowering failed: '//message
        error stop 1
    end if
    spec%name = str('fortml_generated_rbf_leaf_derivatives')
    spec%args(1) = str('variance')
    spec%args(2) = str('distance')
    spec%args(3) = str('lengthscale')
    spec%outputs(1) = str('value')
    spec%outputs(2) = str('dvariance')
    spec%outputs(3) = str('ddistance')
    spec%outputs(4) = str('dlengthscale')
    spec%temp_prefix = str('t')
    spec%generator = str('gen_rbf_derivatives')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_rbf_derivatives OUTPUT_PATH FORTSYM_REVISION')
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'emission failed: '//message
        error stop 1
    end if
    open (newunit=unit, file=trim(output(:output_length)), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) error stop 1
    write (unit, '(a,i0)') '! FortSym IR nodes: ', ir%n_nodes
    write (unit, '(a,i0)') '! FortSym compound operations: ', compound_count(ir)
    write (unit, '(a)') chars(source)
    close (unit)
    write (output_unit, '(a)') 'wrote '//trim(output(:output_length))
    write (output_unit, '(a,i0)') 'IR nodes: ', ir%n_nodes
    write (output_unit, '(a,i0)') 'compound operations: ', compound_count(ir)

contains

    integer function compound_count(kernel_ir) result(count)
        type(kernel_ir_t), intent(in) :: kernel_ir
        integer :: node
        count = 0
        do node = 1, kernel_ir%n_nodes
            if (kernel_ir%nodes(node)%operation == IR_ADD .or. &
                kernel_ir%nodes(node)%operation == IR_MUL .or. &
                kernel_ir%nodes(node)%operation == IR_POW .or. &
                kernel_ir%nodes(node)%operation == IR_FUNCTION) count = count + 1
        end do
    end function compound_count

end program gen_rbf_derivatives
