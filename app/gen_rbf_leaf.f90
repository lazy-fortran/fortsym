program gen_rbf_leaf
    ! Generate the scalar RBF primal leaf used by FortML's CPU path.
    use, intrinsic :: iso_fortran_env, only: output_unit, real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, exp_expr => exp, operator(*), &
        operator(-), operator(**)
    use fortsym_kernel_emit, only: kernel_emit_spec_t, &
        emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_string, only: chars, str, str_t
    implicit none

    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(expr_t) :: variance, distance, lengthscale, root(1)
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(len=2048) :: output, revision
    character(:), allocatable :: message
    integer :: output_length, revision_length, status, unit, ios
    logical :: good

    call get_command_argument(1, output, length=output_length, status=status)
    call get_command_argument(2, revision, length=revision_length, status=status)
    if (output_length == 0 .or. revision_length == 0) then
        write (output_unit, '(a)') &
            'usage: gen_rbf_leaf OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    variance = sym(arena, 'variance')
    distance = sym(arena, 'distance')
    lengthscale = sym(arena, 'lengthscale')
    root(1) = variance*exp_expr(-0.5_dp*distance*lengthscale**(-2))

    call lower_kernel_ir(root, ir, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_rbf_leaf: '//message
        error stop 1
    end if

    spec%name = str('fortml_generated_rbf_leaf_fortran')
    spec%args = [str('variance'), str('distance'), str('lengthscale')]
    spec%outputs = [str('output')]
    spec%temp_prefix = str('t')
    spec%generator = str('gen_rbf_leaf')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_rbf_leaf OUTPUT_PATH FORTSYM_REVISION')
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_rbf_leaf: '//message
        error stop 1
    end if

    open (newunit=unit, file=trim(output(:output_length)), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        write (output_unit, '(a)') 'gen_rbf_leaf: cannot write output'
        error stop 1
    end if
    write (unit, '(a)') chars(source)
    close (unit)
    write (output_unit, '(a)') 'gen_rbf_leaf: wrote '//trim(output(:output_length))
end program gen_rbf_leaf
