! Optional runtime calls are compiled and executed against exact special values.
! The fixture encloses only x=0 tightly, returning the entire line elsewhere;
! it is deliberately independent of any production transcendental algorithm.
program test_fortsym_rigorous_functions
    use fortsym, only: arena_t, expr_t, sym, exp, sin, cos
    use fortsym_string, only: str_t, str, chars
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, interval_runtime, &
        emit_rigorous_kernel, emit_float_kernel
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, roots(3)
    type(rigorous_kernel_spec_t) :: spec
    type(str_t) :: source
    logical :: ok
    character(:), allocatable :: why
    integer :: unit, status
    character, parameter :: nl = new_line('a')

    x = sym(arena, 'x')
    roots = [exp(x), sin(x), cos(x)]
    spec%name = str('enclose_functions')
    spec%args = [str('x')]
    spec%outputs = [str('e'), str('s'), str('c')]
    spec%runtime = interval_runtime('fixture', 'interval_t')
    source = emit_rigorous_kernel(roots, spec, ok, why)
    if (ok) error stop 'runtime without transcendental procedures accepted'
    spec%runtime%exp = str('rexponential')
    spec%runtime%sin = str('rsine')
    spec%runtime%cos = str('rcosine')
    source = emit_rigorous_kernel(roots, spec, ok, why)
    if (.not. ok) error stop why
    open (newunit=unit, file='rigorous_functions_fixture.f90', status='replace')
    write (unit, '(a)') 'module fixture'//nl// &
        'use iso_fortran_env, only: real64'//nl// &
        'implicit none'//nl// &
        'type interval_t'//nl// &
        'real(real64) :: lo, hi'//nl// &
        'end type'//nl//'contains'
    call write_function(unit, 'rexponential', '1.0_real64')
    call write_function(unit, 'rsine', '0.0_real64')
    call write_function(unit, 'rcosine', '1.0_real64')
    write (unit, '(a)') 'end module fixture'//nl// &
        'module leaves'//nl//'contains'//nl//chars(source)
    spec%name = str('float_functions')
    source = emit_float_kernel(roots, spec, ok, why)
    if (.not. ok) error stop why
    write (unit, '(a)') chars(source)//nl//'end module leaves'
    write (unit, '(a)') 'program check_calls'//nl// &
        'use iso_fortran_env, only: real64, real128'//nl// &
        'use fixture'//nl//'use leaves'//nl//'implicit none'//nl// &
        'type(interval_t) :: e,s,c'//nl// &
        'real(real64) :: ef,sf,cf'//nl// &
        'call enclose_functions(interval_t(0d0,0d0),e,s,c)'//nl// &
        'if (e%lo/=1d0.or.e%hi/=1d0) error stop 1'//nl// &
        'if (s%lo/=0d0.or.s%hi/=0d0) error stop 2'//nl// &
        'if (c%lo/=1d0.or.c%hi/=1d0) error stop 3'//nl// &
        'call float_functions(0.5_real64,ef,sf,cf)'//nl// &
        'if(abs(real(ef,real128)-exp(0.5_real128))>1e-15_real128) error stop 4'//nl// &
        'if(abs(real(sf,real128)-sin(0.5_real128))>1e-15_real128) error stop 5'//nl// &
        'if(abs(real(cf,real128)-cos(0.5_real128))>1e-15_real128) error stop 6'//nl// &
        'end program'
    close (unit)
    call execute_command_line('gfortran -std=f2018 -fcheck=all '// &
        'rigorous_functions_fixture.f90 -o rigorous_functions_fixture', &
        exitstat=status)
    if (status /= 0) error stop 'generated function leaves did not compile'
    call execute_command_line('./rigorous_functions_fixture', exitstat=status)
    if (status /= 0) error stop 'independent special-value/real128 oracle failed'
    print '(a)', 'PASS optional rigorous exp/sin/cos runtime and floating leaves'
contains
    subroutine write_function(u, name, value)
        integer, intent(in) :: u
        character(*), intent(in) :: name, value
        write (u, '(a)') 'pure function '//name//'(x) result(y)'//nl// &
            'type(interval_t), intent(in) :: x'//nl// &
            'type(interval_t) :: y'//nl// &
            'y=interval_t(-huge(1d0),huge(1d0))'//nl// &
            'if(x%lo==0d0.and.x%hi==0d0) y=interval_t('//value//','//value//')'//nl// &
            'end function'
    end subroutine
end program
