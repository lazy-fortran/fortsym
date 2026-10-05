! Raw derivative products retain constant expressions such as 2-1 in
! exponents. Compile their emitted leaves and check independent rational
! Coulomb-force directional derivatives, without native simplification.
program test_fortsym_rigorous_raw_products
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym, only: arena_t, expr_t, sym, sqrt, real_expr, num, &
                       operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_products, only: jvp
    use fortsym_string, only: str_t, str, chars
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, interval_runtime, &
                                     emit_rigorous_kernel, emit_float_kernel
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, y, u, v, radius, force(2), tangent(2), z, minimum
    type(rigorous_kernel_spec_t) :: spec
    type(str_t) :: source
    character(4096) :: runtime
    character(:), allocatable :: why, text, command
    character, parameter :: nl = new_line('a')
    character(*), parameter :: work = '/tmp/fortsym_rigorous_raw_products'
    logical :: ok
    integer :: status, unit, length

    x = sym(arena, 'x'); y = sym(arena, 'y')
    u = sym(arena, 'u'); v = sym(arena, 'v')
    z = sym(arena, 'z')
    radius = sqrt(x**2 + y**2)
    force = [x/(radius*(x**2 + y**2)), y/(radius*(x**2 + y**2))]
    tangent = jvp(force, [x, y], [u, v])
    spec%args = [str('x'), str('y'), str('u'), str('v')]
    spec%outputs = [str('a'), str('b')]
    spec%runtime = interval_runtime()
    spec%name = str('raw_interval')
    text = 'module raw_kernels'//nl//'contains'//nl
    source = emit_rigorous_kernel(tangent, spec, ok, why)
    if (.not. ok) error stop why
    text = text//chars(source)
    spec%name = str('raw_float')
    source = emit_float_kernel(tangent, spec, ok, why)
    if (.not. ok) error stop why
    text = text//chars(source)
    minimum = num(arena, -huge(0_int64) - 1_int64)
    spec%args = [str('x')]
    spec%name = str('raw_minimum')
    source = emit_rigorous_kernel([minimum + x, minimum*x], spec, ok, why)
    if (.not. ok) error stop why
    text = text//chars(source)//'end module raw_kernels'//nl
    call execute_command_line('mkdir -p '//work, exitstat=status)
    if (status /= 0) error stop 'cannot create raw-products workspace'
    call write_text(work//'/kernels.f90', text)

    ! Unsupported variable or decimal exponents must continue to fail closed.
    spec%args = [str('x'), str('z')]
    spec%outputs = [str('a')]
    source = emit_rigorous_kernel([x**(z - 1)], spec, ok, why)
    if (ok) error stop 'symbolic exponent accepted'
    source = emit_rigorous_kernel([x**(real_expr(arena, 0.5_real64) + 1)], &
                                  spec, ok, why)
    if (ok) error stop 'decimal exponent accepted'
    call get_environment_variable('FORTSYM_TEST_RUNTIME_DIR', runtime, length, status)
    if (status /= 0 .or. length == 0) then
        runtime = 'test/codegen/runtime'
    end if
    text = 'program drive'//nl// &
           'use, intrinsic :: iso_fortran_env, only: dp=>real64, qp=>real128'//nl// &
           'use fortsym_interval_runtime, only: interval_t, ipoint'//nl// &
           'use raw_kernels'//nl//'implicit none'//nl// &
           'type(interval_t)::a,b'//nl//'real(dp)::fa,fb'//nl// &
           'call raw_interval(ipoint(3._dp),ipoint(4._dp),'// &
           'ipoint(-.5_dp),ipoint(.25_dp),a,b)'//nl// &
           'call raw_float(3._dp,4._dp,-.5_dp,.25_dp,fa,fb)'//nl// &
           'call contains(a,-8._qp/3125);call contains(b,49._qp/12500)'//nl// &
           'if(abs(real(fa,qp)+8._qp/3125)>1.e-15_qp)error stop 1'//nl// &
           'if(abs(real(fb,qp)-49._qp/12500)>1.e-15_qp)error stop 2'//nl// &
           'call raw_interval(ipoint(3._dp),ipoint(4._dp),'// &
           'ipoint(2._dp),ipoint(-1._dp),a,b)'//nl// &
           'call contains(a,32._qp/3125);call contains(b,-49._qp/3125)'//nl// &
           'call raw_minimum(ipoint(.25_dp),a,b)'//nl// &
           'call contains(a,-9223372036854775808._qp+.25_qp)'//nl// &
           'call contains(b,-2305843009213693952._qp)'//nl// &
           'contains'//nl//'subroutine contains(a,exact)'//nl// &
           'type(interval_t),intent(in)::a'//nl//'real(qp),intent(in)::exact'//nl// &
           'if(real(a%lo,qp)>exact.or.real(a%hi,qp)<exact)error stop 3'//nl// &
           'if(real(a%hi-a%lo,qp)>1.e-12_qp*(1+abs(exact)))error stop 4'//nl// &
           'end subroutine'//nl//'end program'//nl
    call write_text(work//'/drive.f90', text)
    command = 'gfortran -O1 -fno-fast-math -ffp-contract=off -J'//work// &
              ' -I'//work//' '//trim(runtime)//'/fortsym_interval_runtime.f90 '// &
              work//'/kernels.f90 '//work//'/drive.f90 -o '//work//'/drive'
    call execute_command_line(command, exitstat=status)
    if (status /= 0) error stop 'raw-products emitted kernel compile failed'
    call execute_command_line(work//'/drive', exitstat=status)
    if (status /= 0) error stop 'raw-products independent runtime oracle failed'
    print '(a)', 'PASS raw derivative-products emitted rational oracles'

contains
    subroutine write_text(path, content)
        character(*), intent(in) :: path, content

        open (newunit=unit, file=path, status='replace', action='write', &
              access='stream', form='unformatted')
        write (unit) content
        close (unit)
    end subroutine write_text
end program test_fortsym_rigorous_raw_products
