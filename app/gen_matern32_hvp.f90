program gen_matern32_hvp
    ! Generate the complete value/JVP/VJP/HVP leaf for the Matérn-3/2
    ! covariance.  FortSym owns this small closed form so the derivative
    ! product is proof-producing and independent of the generic FortAD leaf.
    use, intrinsic :: iso_fortran_env, only: output_unit, real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, exp_expr => exp, operator(+), &
        operator(*), operator(-)
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_kernel_emit, only: kernel_emit_spec_t, &
        emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir, IR_ADD, &
        IR_MUL, IR_POW, IR_FUNCTION
    use fortsym_string, only: chars, str, str_t
    implicit none

    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(native_engine_t) :: eng
    type(expr_t) :: distance, distance_d, lv, lv_d, ll, ll_d, y_b
    type(expr_t) :: value, value_d, distance_b, distance_b_d
    type(expr_t) :: lv_b, lv_b_d, ll_b, ll_b_d, roots(8)
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
            'usage: gen_matern32_hvp OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    eng = make_native_engine(arena)
    distance = sym(arena, 'distance')
    distance_d = sym(arena, 'distance_d')
    lv = sym(arena, 'lv')
    lv_d = sym(arena, 'lv_d')
    ll = sym(arena, 'll')
    ll_d = sym(arena, 'll_d')
    y_b = sym(arena, 'y_b')

    value = exp_expr(lv)*(1.0_dp + sqrt(3.0_dp)*distance*exp_expr(-ll))* &
        exp_expr(-sqrt(3.0_dp)*distance*exp_expr(-ll))
    value_d = diff(value, distance)*distance_d + diff(value, lv)*lv_d + &
        diff(value, ll)*ll_d
    distance_b = y_b*diff(value, distance)
    lv_b = y_b*diff(value, lv)
    ll_b = y_b*diff(value, ll)
    distance_b_d = y_b*diff(value_d, distance)
    lv_b_d = y_b*diff(value_d, lv)
    ll_b_d = y_b*diff(value_d, ll)
    roots = [value, value_d, distance_b, distance_b_d, lv_b, lv_b_d, &
        ll_b, ll_b_d]

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
    spec%name = str('fortml_generated_matern32_hvp_core')
    spec%args = [str('distance'), str('distance_d'), str('lv'), &
        str('lv_d'), str('ll'), str('ll_d'), str('y_b')]
    spec%outputs = [str('y'), str('y_d'), str('distance_b'), &
        str('distance_b_d'), str('lv_b'), str('lv_b_d'), str('ll_b'), &
        str('ll_b_d')]
    spec%temp_prefix = str('fs_t')
    spec%generator = str('gen_matern32_hvp')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_matern32_hvp OUTPUT_PATH FORTSYM_REVISION')
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
    write (unit, '(a)') 'module fortml_generated_matern32_products'
    write (unit, '(a)') '    implicit none'
    write (unit, '(a)') 'contains'
    write (unit, '(a)') chars(source)
    write (unit, '(a)') &
        '    subroutine fortml_matern32_hvp(distance, distance_d, lv, lv_d, ll, ll_d, y, y_d, &'
    write (unit, '(a)') &
        '            y_b, distance_b, distance_b_d, lv_b, lv_b_d, ll_b, ll_b_d)'
    write (unit, '(a)') '        use, intrinsic :: iso_fortran_env, only: real64'
    write (unit, '(a)') '        implicit none'
    write (unit, '(a)') '        real(real64), intent(in) :: distance, distance_d, lv, lv_d, ll, ll_d'
    write (unit, '(a)') '        real(real64), intent(out) :: y, y_d'
    write (unit, '(a)') '        real(real64), intent(in) :: y_b'
    write (unit, '(a)') '        real(real64), intent(out) :: distance_b, distance_b_d, lv_b, lv_b_d, ll_b, ll_b_d'
    write (unit, '(a)') '        call fortml_generated_matern32_hvp_core(distance, distance_d, lv, lv_d, ll, ll_d, y_b, &'
    write (unit, '(a)') '            y, y_d, distance_b, distance_b_d, lv_b, lv_b_d, ll_b, ll_b_d)'
    write (unit, '(a)') '    end subroutine fortml_matern32_hvp'
    write (unit, '(a)') 'end module fortml_generated_matern32_products'
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

end program gen_matern32_hvp
