! Independent binary128 factorial and elementary-polynomial oracles check
! runtime enclosures. No binary64 conversion of an exact coefficient is an oracle.
program test_fortsym_rigorous_rationals
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, exact, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_string, only: str_t, str, chars
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, interval_runtime, &
        emit_rigorous_kernel
    implicit none
    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(engine_result_t) :: reduced
    type(expr_t) :: x, sixth, tiny64, tinybig, nearone, biginteger, roots(12)
    type(rigorous_kernel_spec_t) :: spec
    type(str_t) :: source
    character(:), allocatable :: why, command
    character(len=4096) :: fortnum_source, compiler
    character(len=32) :: mode
    logical :: ok
    integer :: j, unit, status
    character, parameter :: nl = new_line('a')
    character(*), parameter :: factorial_text = '10888869450418352160768000000'

    call arena%init()
    engine = make_native_engine(arena)
    x = sym(arena,'x')
    sixth = num(arena,1)/num(arena,6)
    tiny64 = exact(arena,'1/9007199254740993')
    tinybig = exact(arena,'1/'//factorial_text)
    nearone = exact(arena,'10888869450418352160768000001/'//factorial_text)
    biginteger = exact(arena,factorial_text)
    roots(1) = sixth
    roots(2) = -sixth
    roots(3) = tiny64
    roots(4) = tinybig
    roots(5) = tinybig*x - sixth*x**2 + num(arena,5)/num(arena,7)
    roots(6) = nearone
    roots(7) = nearone*x + (1+x)*sixth
    roots(8) = biginteger
    roots(9) = -tinybig*x - sixth
    roots(10) = exact(arena,'9007199254740993')*x + sixth
    roots(11) = exact(arena,'-9223372036854775808') + x
    roots(12) = exact(arena,'-9223372036854775808')*x
    do j = 1,size(roots)
        reduced = engine%simplify(roots(j))
        if (.not. reduced%ok) error stop chars(reduced%message)
        roots(j) = reduced%value
    end do
    spec%name = str('rational_leaves')
    allocate(spec%args(1),spec%outputs(size(roots)))
    spec%args(1) = str('x')
    do j = 1,size(roots)
        block
            character(len=8) :: output_name
            write(output_name,'("r",i0)') j
            spec%outputs(j) = str(trim(output_name))
        end block
    end do
    spec%runtime = interval_runtime('fortnum_interval','interval_t')
    source = emit_rigorous_kernel(roots,spec,ok,why)
    if (.not. ok) then
        print '(a)', 'FAIL simplified exact coefficient generation: '//why
        error stop 1
    end if
    mode = '--generation-only'
    if (command_argument_count() > 0) call get_command_argument(1,mode)
    if (trim(mode) == '--generation-only') then
        print '(a)', 'PASS simplified small/large exact coefficient generation'
        stop
    end if
    if (trim(mode) /= '--runtime') error stop 'expected --generation-only or --runtime'
    call get_environment_variable('FORTSYM_TEST_FORTNUM_SOURCE_DIR', &
        fortnum_source, status=status)
    if (status /= 0 .or. len_trim(fortnum_source) == 0) &
        error stop 'FORTSYM_TEST_FORTNUM_SOURCE_DIR is required for the runtime oracle'
    compiler = 'gfortran'
    call get_environment_variable('FORTSYM_TEST_COMPILER',compiler,status=status)
    if (status /= 0 .or. len_trim(compiler) == 0) compiler = 'gfortran'
    open(newunit=unit,file='rigorous_rational_fixture.f90',status='replace')
    write(unit,'(a)') 'module rational_fixture_leaves'//nl//'contains'//nl// &
        chars(source)//nl//'end module rational_fixture_leaves'
    call write_checker(unit)
    close(unit)
    command = shell_quote(trim(compiler))//' -std=f2018 -fcheck=all '// &
        '-fno-fast-math -ffp-contract=off '// &
        shell_quote(trim(fortnum_source)//'/src/verified/fortnum_rounding.f90')//' '// &
        shell_quote(trim(fortnum_source)//'/src/verified/fortnum_interval.f90')//' '// &
        'rigorous_rational_fixture.f90 -o rigorous_rational_fixture'
    call execute_command_line(command,exitstat=status)
    if (status /= 0) &
        error stop 'ordinary FortNum emitted rational oracle did not compile'
    call execute_command_line('./rigorous_rational_fixture', exitstat=status)
    if (status /= 0) &
        error stop 'independent binary128 exact coefficient oracle failed'
    print '(a)', 'PASS compiled ordinary FortNum exact rational coefficient containment'
contains
    function shell_quote(value) result(quoted)
        character(*), intent(in) :: value
        character(:), allocatable :: quoted
        integer :: i
        character, parameter :: quote = achar(39)

        quoted = quote
        do i = 1,len(value)
            if (value(i:i) == quote) then
                quoted = quoted//quote//achar(34)//quote//achar(34)//quote
            else
                quoted = quoted//value(i:i)
            end if
        end do
        quoted = quoted//quote
    end function

    subroutine write_checker(unit)
        integer, intent(in) :: unit
        write(unit,'(a)') 'program check_rationals'//nl// &
            'use iso_fortran_env, only: real64,real128'//nl// &
            'use ieee_arithmetic, only: ieee_is_finite'//nl// &
            'use fortnum_interval, only: interval_t,interval'//nl// &
            'use rational_fixture_leaves'//nl//'implicit none'//nl// &
            'type(interval_t)::box,r(12)'//nl// &
            'real(real128)::f,x,values(12),unitden,nearone'//nl// &
            'integer::i,j,k'//nl// &
            'f=1.0_real128'//nl//'do i=1,27'//nl// &
            'f=f*real(i,real128)'//nl//'end do'//nl// &
            'unitden=9007199254740993.0_real128'//nl// &
            'nearone=(f+1)/f'//nl// &
            'if(encloses(interval(1.0_real64),nearone)) error stop 1'//nl// &
            'if(encloses(interval(1.0_real64/6),1.0_real128/6)) error stop 2'
        write(unit,'(a)') 'do k=1,4'//nl//'select case(k)'//nl// &
            'case(1);box=interval(-2.0_real64,-.25_real64)'//nl// &
            'case(2);box=interval(-.125_real64,.75_real64)'//nl// &
            'case(3);box=interval(1.125_real64,2.5_real64)'//nl// &
            'case(4);box=interval(.375_real64)'//nl//'end select'//nl// &
            'call rational_leaves(box,r(1),r(2),r(3),r(4),r(5), &'//nl// &
            'r(6),r(7),r(8),r(9),r(10),r(11),r(12))'//nl// &
            'if(.not.all(ieee_is_finite(r%lo)))error stop 3'//nl// &
            'if(.not.all(ieee_is_finite(r%hi)))error stop 4'//nl// &
            'if(any(r%lo>r%hi))error stop 5'//nl// &
            'do j=0,64'//nl// &
            'x=real(box%lo,real128)+(real(box%hi,real128)- &'//nl// &
            'real(box%lo,real128))*real(j,real128)/64'//nl// &
            'values(1)=1.0_real128/6'//nl//'values(2)=-values(1)'//nl// &
            'values(3)=1.0_real128/unitden'//nl//'values(4)=1.0_real128/f'//nl// &
            'values(5)=x/f-x*x/6+5.0_real128/7'//nl// &
            'values(6)=nearone'//nl//'values(7)=nearone*x+(1+x)/6'//nl// &
            'values(8)=f'//nl//'values(9)=-x/f-1.0_real128/6'//nl// &
            'values(10)=unitden*x+1.0_real128/6'//nl// &
            'values(11)=-(2.0_real128**63)+x'//nl// &
            'values(12)=-(2.0_real128**63)*x'//nl// &
            'do i=1,12'//nl//'if(.not.encloses(r(i),values(i)))then'//nl// &
            'print *,"MISSED",i,k,j,r(i)%lo,values(i),r(i)%hi'//nl// &
            'error stop 6'//nl//'end if'//nl//'end do'//nl//'end do'
        write(unit,'(a)') '! Reject useless widening on exact constant outputs.'//nl// &
            'do i=1,8'//nl//'if(i==5.or.i==7)cycle'//nl// &
            'if(real(r(i)%hi,real128)-real(r(i)%lo,real128)> &'//nl// &
            'abs(values(i))*1.0e-10_real128)error stop 7'//nl//'end do'//nl// &
            'if(k==4)then'//nl// &
            'do i=11,12'//nl// &
            'if(real(r(i)%hi,real128)-real(r(i)%lo,real128)> &'//nl// &
            'abs(values(i))*1.0e-10_real128)error stop 8'//nl// &
            'end do'//nl//'end if'//nl//'end do'//nl// &
            'print *,"PASS independent factorial and polynomial containment"'//nl// &
            'contains'//nl//'logical function encloses(a,value)'//nl// &
            'type(interval_t),intent(in)::a'//nl// &
            'real(real128),intent(in)::value'//nl// &
            'encloses=real(a%lo,real128)<=value.and.value<=real(a%hi,real128)'//nl// &
            'end function'//nl//'end program check_rationals'
    end subroutine
end program test_fortsym_rigorous_rationals
