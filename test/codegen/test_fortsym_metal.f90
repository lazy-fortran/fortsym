program test_fortsym_metal
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(-), operator(*)
    use fortsym_diff, only: diff
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    use fortsym_kernel_emit
    use fortsym_kernel_target, only: target_name, target_is_valid
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, y, value, roots(2)
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(kernel_spec_t) :: legacy
    type(str_t) :: source
    character(:), allocatable :: message
    character(1024) :: output_path
    logical :: good
    integer :: unit

    call arena%init()
    x = sym(arena, 'x')
    y = sym(arena, 'y')
    value = y*(2 - x*y)
    roots = [value, diff(value, x)]
    call lower_kernel_ir(roots, ir, good, message)
    call check(good, 'Newton value and derivative lower: '//message)
    spec%name = str('newton_leaf')
    spec%args = [str('x'), str('y')]
    spec%outputs = [str('value'), str('derivative')]
    spec%target = TARGET_METAL
    call check(TARGET_FORTRAN_OPENMP_SIMD == 5 .and. TARGET_METAL == 6, 'stable identities')
    call check(target_is_valid(TARGET_METAL), 'Metal target valid')
    call check(target_name(TARGET_METAL) == 'metal', 'Metal target name')
    source = emit_metal_device_ir(ir, spec, good, message)
    call check(.not. good .and. len(chars(source)) == 0, 'default real64 rejected')
    call check(index(message, 'real32') > 0, 'precision refusal diagnostic')
    spec%precision = PRECISION_MIXED
    source = emit_metal_device_ir(ir, spec, good, message)
    call check(.not. good .and. len(chars(source)) == 0, 'mixed precision rejected')
    legacy%name = str('newton_fortran')
    legacy%args = spec%args
    legacy%outputs = spec%outputs
    legacy%target = TARGET_METAL
    source = emit_kernel(roots, legacy, ok=good, message=message)
    call check(.not. good, 'legacy Fortran emitter rejects Metal target')
    call check(index(message, 'Metal') > 0, 'legacy refusal identifies Metal target')
    spec%precision = PRECISION_REAL32
    source = emit_metal_device_ir(ir, spec, good, message)
    call check(good, 'real32 Metal emits: '//message)
    output_path = 'fortsym_newton.metal'
    if (command_argument_count() > 0) call get_command_argument(1, output_path)
    open(newunit=unit, file=trim(output_path), status='replace')
    write(unit, '(a)') chars(source)
    close(unit)
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    call check(.not. good, 'Fortran IR emitter rejects Metal target')
    spec%target = TARGET_FORTRAN_CPU
    source = emit_metal_device_ir(ir, spec, good, message)
    call check(.not. good, 'Metal emitter rejects CPU target')
    spec%target = TARGET_FORTRAN_OPENMP_SIMD
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    call check(good, 'existing SIMD target emits: '//message)
    call check(index(chars(source), '!$omp declare simd') > 0, 'SIMD directive retained')
contains
    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label
        if (condition) return
        print *, 'FAIL: ', label
        error stop 1
    end subroutine check
end program test_fortsym_metal
