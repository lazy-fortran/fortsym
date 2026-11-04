program test_fortsym_cse_reciprocal
    ! A denominator shared by several roots is named once. The printer emits a
    ! reciprocal power as a division by its base, and full CSE never names
    ! the reciprocal itself, so the base must be named when the reciprocal is
    ! shared; otherwise the denominator is recomputed at every use.
    !
    ! With reciprocal_temporaries, 1/x is named once and every reciprocal
    ! power of x becomes a product. The compiled kernel is checked against
    ! the quotient written directly in Fortran.
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    use fortsym_string, only: str, chars
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, y, z, d, roots(2), shared(1)
    type(kernel_spec_t) :: spec
    character(:), allocatable :: source

    call arena%init()
    x = sym(arena, 'x')
    y = sym(arena, 'y')
    d = x*y + x + y
    roots = [x/d, y/d]
    spec%name = str('shared_denominator')
    allocate (spec%args(2), spec%outputs(2))
    spec%args(1) = str('x')
    spec%args(2) = str('y')
    spec%outputs(1) = str('a')
    spec%outputs(2) = str('b')
    source = chars(emit_kernel(roots, spec))
    if (occurrences(source, "x*y") /= 1) then
        print '(a)', source
        error stop 'shared denominator was not named once'
    end if
    print '(a)', 'PASS: shared reciprocal denominator named once'

    z = sym(arena, 'z')
    shared(1) = y/x + z/x**2 + y*z/x**3
    spec%name = str('shared_reciprocal')
    deallocate (spec%args, spec%outputs)
    allocate (spec%args(3), spec%outputs(1))
    spec%args(1) = str('x')
    spec%args(2) = str('y')
    spec%args(3) = str('z')
    spec%outputs(1) = str('r')
    source = chars(emit_kernel(shared, spec))
    if (divisions(source) /= 3) then
        print '(a)', source
        error stop 'default output no longer divides at every use'
    end if
    spec%reciprocal_temporaries = .true.
    source = chars(emit_kernel(shared, spec))
    if (divisions(source) /= 1) then
        print '(a)', source
        error stop 'reciprocal 1/x was not named once'
    end if
    call check_compiled(source)
    print '(a)', 'PASS: shared reciprocal 1/x emits one division'

    ! A reciprocal built as 1/x and raised to powers keeps its unit factor in
    ! the DAG; constant divisions such as /2 are folded and stay as they are.
    roots(1) = (1/x)**3*y/2 + 1/x
    roots(2) = 3*(1/x)**2/2 - y/x
    spec%name = str('unit_reciprocal')
    deallocate (spec%args, spec%outputs)
    allocate (spec%args(2), spec%outputs(2))
    spec%args(1) = str('x')
    spec%args(2) = str('y')
    spec%outputs(1) = str('a')
    spec%outputs(2) = str('b')
    source = chars(emit_kernel(roots, spec))
    if (occurrences(source, '/x') /= 1 .or. occurrences(source, '= t1' &
        //new_line('a')) /= 0 .or. occurrences(source, '= 1.0_dp/real') /= 0) then
        print '(a)', source
        error stop 'unit reciprocal was copied or a constant was named'
    end if
    print '(a)', 'PASS: unit reciprocal named once, constants left alone'

contains

    !> Divisions in the statements, outside the comment banner.
    integer function divisions(text)
        character(*), intent(in) :: text
        integer :: start, finish

        divisions = 0
        start = 1
        do while (start <= len(text))
            finish = index(text(start:), new_line('a'))
            if (finish == 0) then
                finish = len(text) + 1
            else
                finish = start + finish - 1
            end if
            if (index(adjustl(text(start:finish - 1)), '!') /= 1) &
                divisions = divisions + occurrences(text(start:finish - 1), '/')
            start = finish + 1
        end do
    end function divisions

    !> Compile the kernel with gfortran and compare it with the quotient.
    subroutine check_compiled(kernel)
        character(*), intent(in) :: kernel
        integer :: unit, stat, k
        real(dp) :: got, want, xv, yv, zv

        open (newunit=unit, file='./fortsym_shared_reciprocal.f90', &
            status='replace', action='write')
        write (unit, '(a)') kernel
        write (unit, '(a)') 'program drive'
        write (unit, '(a)') '    use, intrinsic :: iso_fortran_env, only: dp => real64'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    real(dp) :: r, x, y, z'
        write (unit, '(a)') '    integer :: k'
        write (unit, '(a)') '    do k = 1, 9'
        write (unit, '(a)') '        x = 0.3_dp*k - 1.6_dp; y = 0.7_dp*k'
        write (unit, '(a)') '        z = 1.1_dp - 0.2_dp*k'
        write (unit, '(a)') '        call shared_reciprocal(x, y, z, r)'
        write (unit, '(a)') "        write (*, '(4es25.17)') r, x, y, z"
        write (unit, '(a)') '    end do'
        write (unit, '(a)') 'end program drive'
        close (unit)
        call execute_command_line('gfortran -o ./fortsym_shared_reciprocal '// &
            './fortsym_shared_reciprocal.f90 && ./fortsym_shared_reciprocal '// &
            '> ./fortsym_shared_reciprocal.out', wait=.true., exitstat=stat)
        if (stat /= 0) error stop 'shared reciprocal kernel did not compile or run'
        open (newunit=unit, file='./fortsym_shared_reciprocal.out', &
            status='old', action='read')
        do k = 1, 9
            ! Different optimization levels can contract the input expressions differently.
            read (unit, *) got, xv, yv, zv
            want = yv/xv + zv/xv**2 + yv*zv/xv**3
            if (abs(got - want) > 8*epsilon(1.0_dp)*(abs(yv/xv) + &
                abs(zv/xv**2) + abs(yv*zv/xv**3))) then
                print *, 'x =', xv, ' got', got, ' want', want
                error stop 'shared reciprocal kernel changed the value'
            end if
        end do
        close (unit)
    end subroutine check_compiled

    integer function occurrences(text, pattern)
        character(*), intent(in) :: text, pattern
        integer :: start, hit

        occurrences = 0
        start = 1
        do
            hit = index(text(start:), pattern)
            if (hit == 0) return
            occurrences = occurrences + 1
            start = start + hit + len(pattern) - 1
        end do
    end function occurrences

end program test_fortsym_cse_reciprocal
