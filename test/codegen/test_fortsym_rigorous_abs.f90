! Independent exact sign cases and real128 interior values exercise the
! emitted absolute-value image. Derivative certificates exclude the cusp.
program test_fortsym_rigorous_abs
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, abs, operator(+), operator(*), operator(**)
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_string, only: str_t, str, chars
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, interval_runtime, &
        emit_rigorous_kernel, emit_float_kernel
    implicit none
    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(engine_result_t) :: reduced
    type(expr_t) :: x, a, b, k, acc
    type(rigorous_kernel_spec_t) :: spec
    type(str_t) :: source
    logical :: ok
    character(:), allocatable :: why
    integer :: u, status
    character, parameter :: nl = new_line('a')

    call arena%init()
    engine = make_native_engine(arena)
    x = sym(arena, 'x'); a = sym(arena, 'a'); b = sym(arena, 'b')
    k = sym(arena, 'k'); acc = sym(arena, 'acc')
    spec%args = [str('x')]; spec%outputs = [str('value')]
    spec%runtime = interval_runtime('fixture', 'interval_t')
    spec%elemental_procedure = .true.
    spec%name = str('absolute_image')
    source = emit_rigorous_kernel([abs(x)], spec, ok, why)
    if (ok) error stop 'default runtime accepted absolute value'
    if (index(why, 'abs') == 0) error stop 'missing absolute-value diagnostic'
    spec%runtime%abs = str('rabs')
    source = emit_rigorous_kernel([abs(x)], spec, ok, why)
    if (.not. ok) error stop why
    open (newunit=u, file='rigorous_abs_fixture.f90', status='replace')
    call write_runtime(u)
    write (u, '(a)') 'module leaves'//nl//'contains'//nl//chars(source)
    reduced = engine%simplify(diff(abs(x), x))
    if (.not. reduced%ok) error stop chars(reduced%message)
    spec%name = str('absolute_derivative')
    source = emit_rigorous_kernel([reduced%value], spec, ok, why)
    if (.not. ok) error stop why
    write (u, '(a)') chars(source)
    spec%name = str('curvature')
    spec%args = [str('acc'), str('k'), str('a'), str('b')]
    source = emit_rigorous_kernel([acc + k**2*(abs(a) + abs(b))], spec, ok, why)
    if (.not. ok) error stop why
    write (u, '(a)') chars(source)
    spec%name = str('complex_magnitude')
    spec%args = [str('x')]; spec%complex_args = [.true.]
    source = emit_float_kernel([abs(x)], spec, ok, why)
    if (.not. ok) error stop why
    write (u, '(a)') chars(source)//nl//'end module leaves'
    call write_checker(u)
    close (u)
    spec%runtime%abs = str('not-valid')
    source = emit_rigorous_kernel([abs(x)], spec, ok, why)
    if (ok) error stop 'invalid absolute-value runtime name accepted'
    call execute_command_line('gfortran -std=f2018 -fcheck=all '// &
        '-fno-fast-math -ffp-contract=off rigorous_abs_fixture.f90 '// &
        '-o rigorous_abs_fixture', exitstat=status)
    if (status /= 0) error stop 'absolute-value leaves did not compile'
    call execute_command_line('./rigorous_abs_fixture', exitstat=status)
    if (status /= 0) error stop 'independent absolute-value oracle failed'
    print '(a)', 'PASS optional rigorous absolute value and nonzero derivative bounds'
contains
    subroutine write_runtime(unit)
        integer, intent(in) :: unit
        ! Test operands are small dyadics: add/mul/powi below are exact.
        ! Derivative boxes have endpoints +/-1 and +/-2: inversion is exact.
        ! Outside these finite fixtures this is not a production runtime.
        write (unit, '(a)') 'module fixture'//nl// &
            'use iso_fortran_env, only: real64'//nl// &
            'use ieee_arithmetic, only: ieee_value,ieee_positive_inf'//nl// &
            'implicit none'//nl//'type interval_t'//nl// &
            'real(real64)::lo,hi'//nl//'end type'//nl//'contains'
        write (unit, '(a)') 'pure elemental function rabs(x) result(y)'//nl// &
            'type(interval_t),intent(in)::x'//nl//'type(interval_t)::y'//nl// &
            'if(x%lo>=0)then'//nl//'y=x'//nl//'else if(x%hi<=0)then'//nl// &
            'y=interval_t(-x%hi,-x%lo)'//nl//'else'//nl// &
            'y=interval_t(0d0,max(-x%lo,x%hi))'//nl//'end if'//nl//'end function'
        write (unit, '(a)') 'pure elemental function iadd(x,z) result(y)'//nl// &
            'type(interval_t),intent(in)::x,z'//nl//'type(interval_t)::y'//nl// &
            'y=interval_t(x%lo+z%lo,x%hi+z%hi)'//nl//'end function'
        write (unit, '(a)') 'pure elemental function imul(x,z) result(y)'//nl// &
            'type(interval_t),intent(in)::x,z'//nl//'type(interval_t)::y'//nl// &
            'real(real64)::p(4)'//nl// &
            'p=[x%lo*z%lo,x%lo*z%hi,x%hi*z%lo,x%hi*z%hi]'//nl// &
            'y=interval_t(minval(p),maxval(p))'//nl//'end function'
        write (unit, '(a)') 'pure elemental function ipowi(x,n) result(y)'//nl// &
            'type(interval_t),intent(in)::x'//nl//'integer,intent(in)::n'//nl// &
            'type(interval_t)::y'//nl// &
            'y=interval_t(min(x%lo**n,x%hi**n),max(x%lo**n,x%hi**n))'//nl// &
            'if(mod(n,2)==0.and.x%lo<=0.and.x%hi>=0)y%lo=0'//nl//'end function'
        write (unit, '(a)') 'pure elemental function idiv(x,z) result(y)'//nl// &
            'type(interval_t),intent(in)::x,z'//nl//'type(interval_t)::y'//nl// &
            'real(real64)::p(4),inf'//nl// &
            'inf=ieee_value(1d0,ieee_positive_inf)'//nl// &
            'if(z%lo<=0.and.z%hi>=0)then'//nl//'y=interval_t(-inf,inf)'//nl// &
            'else'//nl//'p=[x%lo/z%lo,x%lo/z%hi,x%hi/z%lo,x%hi/z%hi]'//nl// &
            'y=interval_t(minval(p),maxval(p))'//nl// &
            'end if'//nl//'end function'//nl// &
            'end module fixture'
    end subroutine write_runtime

    subroutine write_checker(unit)
        integer, intent(in) :: unit
        write (unit, '(a)') 'program check'//nl// &
            'use iso_fortran_env, only: real64,real128'//nl// &
            'use ieee_arithmetic, only: ieee_is_finite'//nl// &
            'use fixture'//nl//'use leaves'//nl//'implicit none'//nl// &
            'type(interval_t)::value,derivative'//nl// &
            'real(real64)::magnitude'//nl// &
            'real(real128)::a,b,z'//nl//'integer::j'//nl// &
            'call absolute_image(interval_t(-2d0,-1d0),value)'//nl// &
            'if(value%lo/=1d0.or.value%hi/=2d0)error stop 1'//nl// &
            'call absolute_image(interval_t(1d0,2d0),value)'//nl// &
            'if(value%lo/=1d0.or.value%hi/=2d0)error stop 2'//nl// &
            'call absolute_image(interval_t(0d0,0d0),value)'//nl// &
            'if(value%lo/=0d0.or.value%hi/=0d0)error stop 3'//nl// &
            'call absolute_image(interval_t(-2d0,.5d0),value)'//nl// &
            'if(value%lo/=0d0.or.value%hi/=2d0)error stop 4'//nl// &
            'do j=-64,16'//nl//'z=real(j,real128)/32'//nl// &
            'call require(value,abs(z))'//nl//'end do'//nl// &
            'call absolute_derivative(interval_t(-2d0,-1d0),derivative)'//nl// &
            'call require(derivative,-1.0_real128)'//nl// &
            'call absolute_derivative(interval_t(1d0,2d0),derivative)'//nl// &
            'call require(derivative,1.0_real128)'//nl// &
            'call absolute_derivative(interval_t(-1d0,1d0),derivative)'//nl// &
            'if(ieee_is_finite(derivative%lo).or.ieee_is_finite(derivative%hi)) '// &
            'error stop 5'
        write (unit, '(a)') 'call curvature(interval_t(.125d0,.125d0), '// &
            'interval_t(3d0,3d0),interval_t(-2d0,.5d0),'// &
            'interval_t(-1d0,2d0),value)'//nl// &
            'do j=0,80'//nl//'a=-2.0_real128+real(j,real128)/32'//nl// &
            'b=2.0_real128-real(j,real128)*3/80'//nl// &
            'call require(value,.125_real128+9*(abs(a)+abs(b)))'//nl//'end do'//nl// &
            'call complex_magnitude(cmplx(3d0,4d0,real64),magnitude)'//nl// &
            'if(magnitude/=5d0) error stop 6'//nl//'contains'//nl// &
            'subroutine require(a,value)'//nl//'type(interval_t),intent(in)::a'//nl// &
            'real(real128),intent(in)::value'//nl// &
            'if(value<real(a%lo,real128).or.value>real(a%hi,real128)) '// &
            'error stop 7'//nl//'end subroutine'//nl//'end program'
    end subroutine write_checker
end program test_fortsym_rigorous_abs
