! The runtime fixture uses elementary, exact dyadic bounds on [-1/2,1/2].
! This tests emitted dispatch and derivative containment, independently of
! any production transcendental approximation or argument reduction.
program test_fortsym_rigorous_sincos
    use fortsym, only: arena_t, expr_t, sym, sin, cos, &
        operator(*), operator(-)
    use fortsym_diff, only: diff
    use fortsym_string, only: str_t, str, chars
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, interval_runtime, &
        emit_rigorous_kernel, emit_float_kernel
    implicit none
    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(engine_result_t) :: reduced
    type(expr_t) :: x, roots(2), derivatives(2)
    type(rigorous_kernel_spec_t) :: spec
    type(str_t) :: source
    logical :: ok
    character(:), allocatable :: why
    integer :: u, status, j
    character, parameter :: nl = new_line('a')

    call arena%init()
    engine = make_native_engine(arena)
    x = sym(arena, 'x')
    roots = [sin(2*x), cos(2*x)]
    do j = 1, 2
        reduced = engine%simplify(diff(roots(j), x))
        if (.not. reduced%ok) error stop chars(reduced%message)
        derivatives(j) = reduced%value
    end do
    spec%args = [str('x')]
    spec%outputs = [str('s'), str('c')]
    spec%runtime = interval_runtime('fixture', 'interval_t')
    spec%runtime%sin = str('rsine')
    spec%runtime%cos = str('rcosine')
    spec%elemental_procedure = .true.
    open (newunit=u, file='rigorous_sincos_fixture.f90', status='replace')
    call write_runtime(u)
    write (u, '(a)') 'module leaves'//nl//'contains'
    call write_leaf('scalar_pair', roots)
    spec%runtime%sincos = str('rsincos')
    ! Complete pairs need only the paired procedure, not scalar fallbacks.
    spec%runtime%sin = str('')
    spec%runtime%cos = str('')
    call write_leaf('paired', roots)
    call write_leaf('paired_reverse', [roots(2), roots(1)])
    call write_leaf('paired_derivative', derivatives)
    spec%name = str('floating')
    source = emit_float_kernel(roots, spec, ok, why)
    if (.not. ok) error stop why
    write (u, '(a)') chars(source)

    ! A pair procedure cannot silently stand in for an unmatched function.
    source = emit_rigorous_kernel([sin(x), cos(2*x)], spec, ok, why)
    if (ok) error stop 'different trig arguments accepted without scalar procedures'
    spec%runtime%sin = str('rsine')
    spec%runtime%cos = str('rcosine')
    call write_leaf('unpaired', [sin(x), cos(2*x)])
    call write_leaf('nested_pair', [sin(sin(x)), cos(sin(x))])
    ! A mixed leaf must import both paired and remaining scalar procedures.
    call write_leaf('mixed_pair', [sin(x), sin(2*x)*cos(2*x)])
    spec%runtime%sincos = str('invalid-name')
    source = emit_rigorous_kernel(roots, spec, ok, why)
    if (ok) error stop 'invalid paired runtime name accepted'
    write (u, '(a)') 'end module leaves'
    call write_checker(u)
    close (u)
    call execute_command_line('gfortran -std=f2018 -fcheck=all '// &
        '-fno-fast-math -ffp-contract=off rigorous_sincos_fixture.f90 '// &
        '-o rigorous_sincos_fixture', exitstat=status)
    if (status /= 0) error stop 'generated paired leaves did not compile'
    call execute_command_line('./rigorous_sincos_fixture', exitstat=status)
    if (status /= 0) error stop 'independent paired value/derivative containment oracle'
    print '(a)', 'PASS paired rigorous sine/cosine and derivative containment'
contains
    subroutine write_leaf(name, values)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: values(:)
        character(:), allocatable :: rendered
        integer :: at
        spec%name = str(name)
        source = emit_rigorous_kernel(values, spec, ok, why)
        if (.not. ok) error stop why
        ! Supplementary schedule assertion; compiled mathematical checks follow.
        if (name == 'paired') then
            rendered = chars(source)
            at = index(rendered, 'call rsincos(')
            if (at == 0) error stop 'pair not scheduled'
            if (index(rendered(at + 1:), 'call rsincos(') /= 0) &
                error stop 'pair scheduled twice'
        end if
        write (u, '(a)') chars(source)
    end subroutine write_leaf

    subroutine write_runtime(unit)
        integer, intent(in) :: unit
        write (unit, '(a)') 'module fixture'//nl// &
            'use iso_fortran_env, only: real64'//nl// &
            'implicit none'//nl//'type interval_t'//nl// &
            'real(real64) :: lo,hi'//nl//'end type'//nl//'contains'
        ! |sin(x)-x| <= |x|**3/6 < 1/32 and 7/8 <= cos(x) <= 1
        ! on [-1/2,1/2]. Restrict endpoints to dyadics on the 1/32 grid;
        ! then the shifts below are exact. The entire-line fallback is safe.
        write (unit, '(a)') 'pure elemental subroutine rsincos(x,s,c)'//nl// &
            'type(interval_t),intent(in)::x'//nl// &
            'type(interval_t),intent(out)::s,c'//nl// &
            's=interval_t(-huge(1d0),huge(1d0));c=s'//nl// &
            'if(x%lo==0d0.and.x%hi==0d0)then'//nl// &
            's=interval_t(0d0,0d0);c=interval_t(1d0,1d0)'//nl// &
            'else if(x%lo>=-.5d0.and.x%hi<=.5d0)then'//nl// &
            'if(modulo(x%lo*32d0,1d0)==0d0.and.modulo(x%hi*32d0,1d0)==0d0)then'//nl// &
            's=interval_t(x%lo-1d0/32d0,x%hi+1d0/32d0)'//nl// &
            'c=interval_t(7d0/8d0,1d0)'//nl// &
            'end if'//nl//'end if'//nl//'end subroutine'
        write (unit, '(a)') 'pure elemental function rsine(x) result(y)'//nl// &
            'type(interval_t),intent(in)::x'//nl//'type(interval_t)::y,c'//nl// &
            'call rsincos(x,y,c)'//nl//'end function'//nl// &
            'pure elemental function rcosine(x) result(y)'//nl// &
            'type(interval_t),intent(in)::x'//nl//'type(interval_t)::y,s'//nl// &
            'call rsincos(x,s,y)'//nl//'end function'
        write (unit, '(a)') 'pure elemental function iscale(x,a) result(y)'//nl// &
            'type(interval_t),intent(in)::x'//nl//'real(real64),intent(in)::a'//nl// &
            'type(interval_t)::y'//nl// &
            'y=interval_t(min(a*x%lo,a*x%hi),max(a*x%lo,a*x%hi))'//nl//'end function'
        write (unit, '(a)') 'pure elemental function ineg(x) result(y)'//nl// &
            'type(interval_t),intent(in)::x'//nl//'type(interval_t)::y'//nl// &
            'y=interval_t(-x%hi,-x%lo)'//nl//'end function'
        write (unit, '(a)') 'pure elemental function imul(x,z) result(y)'//nl// &
            'type(interval_t),intent(in)::x,z'//nl//'type(interval_t)::y'//nl// &
            'real(real64)::p(4)'//nl// &
            'p=[x%lo*z%lo,x%lo*z%hi,x%hi*z%lo,x%hi*z%hi]'//nl// &
            'y=interval_t(minval(p),maxval(p))'//nl// &
            'end function'//nl//'end module fixture'
    end subroutine write_runtime

    subroutine write_checker(unit)
        integer, intent(in) :: unit
        write (unit, '(a)') 'program check'//nl// &
            'use iso_fortran_env, only: real64,real128'//nl// &
            'use fixture'//nl//'use leaves'//nl//'implicit none'//nl// &
            'type(interval_t)::s,c,ds,dc,ss,cc,rc,rs'//nl// &
            'real(real64)::x,sf,cf'//nl// &
            'real(real128)::z,sv,cv'//nl//'integer::j'//nl// &
            'do j=-8,8'//nl//'x=real(j,real64)/32;z=real(j,real128)/32'//nl// &
            'sv=sin(2*z);cv=cos(2*z)'//nl// &
            'call paired(interval_t(x,x),s,c)'//nl// &
            'call scalar_pair(interval_t(x,x),ss,cc)'//nl// &
            'if(s%lo/=ss%lo.or.s%hi/=ss%hi) error stop 1'//nl// &
            'if(c%lo/=cc%lo.or.c%hi/=cc%hi) error stop 2'//nl// &
            'call require(s,sv);call require(c,cv)'//nl// &
            'call paired_reverse(interval_t(x,x),rc,rs)'//nl// &
            'call require(rs,sv);call require(rc,cv)'//nl// &
            'call paired_derivative(interval_t(x,x),ds,dc)'//nl// &
            'call require(ds,2*cv);call require(dc,-2*sv)'//nl// &
            'call unpaired(interval_t(x,x),ss,cc)'//nl// &
            'call require(ss,sin(z));call require(cc,cv)'//nl// &
            'call nested_pair(interval_t(x,x),ss,cc)'//nl// &
            'call require(ss,sin(sin(z)));call require(cc,cos(sin(z)))'//nl// &
            'call mixed_pair(interval_t(x,x),ss,cc)'//nl// &
            'call require(ss,sin(z));call require(cc,sv*cv)'//nl// &
            'call floating(x,sf,cf)'//nl// &
            'if(abs(real(sf,real128)-sv)>1e-15_real128) error stop 3'//nl// &
            'if(abs(real(cf,real128)-cv)>1e-15_real128) error stop 4'//nl//'end do'
        write (unit, '(a)') 'call paired(interval_t(-.25d0,.25d0),s,c)'//nl// &
            'call paired_derivative(interval_t(-.25d0,.25d0),ds,dc)'//nl// &
            'do j=-32,32'//nl//'z=real(j,real128)/128'//nl// &
            'call require(s,sin(2*z));call require(c,cos(2*z))'//nl// &
            'call require(ds,2*cos(2*z));call require(dc,-2*sin(2*z))'//nl// &
            'end do'//nl//'contains'//nl// &
            'subroutine require(a,value)'//nl//'type(interval_t),intent(in)::a'//nl// &
            'real(real128),intent(in)::value'//nl// &
            'if(value<real(a%lo,real128).or.value>real(a%hi,real128)) '// &
            'error stop 5'//nl// &
            'end subroutine'//nl//'end program'
    end subroutine write_checker
end program test_fortsym_rigorous_sincos
