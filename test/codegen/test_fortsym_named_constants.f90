program test_fortsym_named_constants
    use, intrinsic :: iso_fortran_env, only: real64, real128
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, const, pi_expr, e_expr, &
        operator(*), operator(+)
    use fortsym_string, only: str_t, str, chars
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    use fortsym_kernel_typed, only: typed_kernel_spec_t, emit_typed_kernel
    use fortsym_kernel_taylor, only: taylor_emit_spec_t, emit_taylor_step
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, emit_python_kernel_ir
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, roots(4)
    integer :: failures = 0, checks = 0, kind_index, wrapped
    character(7), parameter :: kinds(*) = [character(7) :: &
        'real32', 'real64', 'real128']

    call arena%init()
    x = sym(arena, 'x')
    roots = [pi_expr(arena)*x, e_expr(arena)*x, &
        pi_expr(arena) + x, e_expr(arena) + x]
    do kind_index = 1, size(kinds)
        do wrapped = 0, 1
            call compiled_constants(trim(kinds(kind_index)), wrapped == 1)
        end do
    end do
    call refusals()
    print *, 'named constant checks:', checks - failures, 'passed;', failures, 'failed'
    if (failures /= 0) error stop 1
contains
    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        checks = checks + 1
        if (condition) return
        failures = failures + 1
        print *, 'FAIL ', label
    end subroutine check

    subroutine compiled_constants(kind_name, wrap)
        character(*), intent(in) :: kind_name
        logical, intent(in) :: wrap
        type(typed_kernel_spec_t) :: typed
        type(taylor_emit_spec_t) :: taylor
        type(kernel_spec_t) :: scalar
        type(str_t) :: typed_source, taylor_source, scalar_source
        type(str_t), allocatable :: params(:)
        type(binding_t) :: binding
        character(:), allocatable :: message, header, names, declaration
        character(2048) :: compiler
        integer :: unit, stat, j, k, ios, start, comma
        logical :: good
        real(real128) :: values(4), scalar_values(4), typed_values(4), expected(4)
        real(real128) :: xv, tol
        real(real64) :: direct

        typed%name = str('typed_constants')
        typed%args = [str('x')]
        typed%outputs = [str('p'), str('e'), str('pa'), str('ea')]
        typed%type_name = str('box_t')
        if (wrap) typed%literal_constructor = str('box_t')
        typed_source = emit_typed_kernel(roots, typed, good, message)
        call check('typed pi/e emit', good)
        if (.not. good) return
        taylor%name = str('taylor_constants')
        taylor%args = typed%args
        taylor%outputs = typed%outputs
        if (wrap) then
            taylor%type_name = str('box_t')
            taylor%literal_constructor = str('box_t')
        end if
        taylor_source = emit_taylor_step(roots, taylor, good, message)
        call check('Taylor pi/e emit', good)
        if (.not. good) return
        scalar%name = str('scalar_constants')
        scalar%module_name = str('scalar_mod')
        scalar%args = typed%args
        scalar%outputs = typed%outputs
        scalar_source = emit_kernel(roots, scalar, good, message=message)
        call check('scalar pi/e emit', good)
        if (.not. good) return

        header = chars(taylor_source)
        header = header(index(header, ',') + 1:index(header, ')') - 1)
        names = trim(adjustl(header))
        allocate (params(0))
        start = 1
        do
            comma = index(names(start:), ',')
            if (comma == 0) then
                params = [params, str(trim(adjustl(names(start:))))]
                exit
            end if
            params = [params, str(trim(adjustl(names(start:start + comma - 2))))]
            start = start + comma
        end do
        open (newunit=unit, file='named_constants.f90', status='replace')
        write (unit, '(a)') chars(scalar_source)
        call write_box(unit, kind_name)
        write (unit, '(a)') 'module constants_mod', 'use box_mod', 'implicit none', &
            'contains', chars(typed_source), chars(taylor_source), 'end module'
        write (unit, '(a)') 'program run_constants', &
            'use constants_mod', 'use scalar_mod, only: scalar_constants', &
            'use, intrinsic :: iso_fortran_env, only: real64', 'implicit none', &
            'type(box_t) :: bx, bp, be, bpa, bea', &
            'real(real64) :: sp, se, spa, sea', 'integer :: k'
        declaration = 'real(dp)'
        if (wrap) declaration = 'type(box_t)'
        do j = 1, size(params)
            write (unit, '(a)') declaration//' :: '//chars(params(j))//'(0:2)'
        end do
        if (wrap) then
            write (unit, '(a)') 'x = box_t(0.0_dp)', &
                'x(0) = box_t(0.75_dp)', 'x(1) = box_t(-0.5_dp)', &
                'x(2) = box_t(0.25_dp)'
        else
            write (unit, '(a)') 'x = [0.75_dp, -0.5_dp, 0.25_dp]'
        end if
        write (unit, '(a)') 'do k = 0, 2'
        if (wrap) then
            write (unit, '(a)') 'bx = x(k)'
        else
            write (unit, '(a)') 'bx = box_t(x(k))'
        end if
        write (unit, '(a)') 'call typed_constants(bx, bp, be, bpa, bea)', &
            'call scalar_constants(real(bx%v, real64), sp, se, spa, sea)', &
            'call taylor_constants(k, '//names//')', &
            "write (*, '(4(es44.35e4,1x))') bp%v, be%v, bpa%v, bea%v", &
            "write (*, '(4(es44.35e4,1x))') sp, se, spa, sea"
        if (wrap) then
            write (unit, '(a)') &
                "write (*, '(4(es44.35e4,1x))') p(k)%v, e(k)%v, pa(k)%v, ea(k)%v"
        else
            write (unit, '(a)') &
                "write (*, '(4(es44.35e4,1x))') p(k), e(k), pa(k), ea(k)"
        end if
        write (unit, '(a)') 'end do', 'end program'
        close (unit)
        call get_environment_variable('FORTSYM_TEST_COMPILER', compiler, status=stat)
        if (stat /= 0) compiler = 'gfortran'
        call execute_command_line(trim(compiler)// &
            ' -o named_constants named_constants.f90 > named_constants.log 2>&1', &
            exitstat=stat)
        call check('pi/e fixture compiles: '//kind_name, stat == 0)
        if (stat /= 0) return
        call execute_command_line('./named_constants > named_constants.out', &
            exitstat=stat)
        call check('pi/e fixture runs', stat == 0)
        if (stat /= 0) return
        tol = 8.0_real128*epsilon(1.0_real128)
        if (kind_name == 'real32') tol = 8.0_real128*2.0_real128**(-23)
        if (kind_name == 'real64') tol = 8.0_real128*epsilon(1.0_real64)
        binding%names = [str('x')]
        binding%values = [0.0_real64]
        binding%n = 1
        open (newunit=unit, file='named_constants.out', status='old')
        do k = 0, 2
            xv = 0.75_real128
            if (k == 1) xv = -0.5_real128
            if (k == 2) xv = 0.25_real128
            read (unit, *, iostat=ios) typed_values, scalar_values, values
            call check('compiled values readable', ios == 0)
            if (ios /= 0) exit
            expected = [acos(-1.0_real128)*xv, exp(1.0_real128)*xv, &
                acos(-1.0_real128) + xv, exp(1.0_real128) + xv]
            binding%values(1) = real(xv, real64)
            do j = 1, 4
                direct = eval_expr(roots(j), binding, good)
                call check('direct evaluation defined', good)
                call check('scalar matches direct evaluation', &
                    abs(scalar_values(j) - direct) < 2.e-15_real128)
                call check('typed matches scalar', &
                    abs(typed_values(j) - scalar_values(j)) < max(tol, 2.e-15_real128))
                call check('typed matches kind-precision oracle', &
                    abs(typed_values(j) - expected(j)) < tol)
                if (k > 0 .and. j > 2) expected(j) = xv
                call check('Taylor coefficient matches independent oracle', &
                    abs(values(j) - expected(j)) < tol)
                if (k == 0 .or. j <= 2) call check('Taylor matches scalar', &
                    abs(values(j) - scalar_values(j)) < max(tol, 2.e-15_real128))
            end do
        end do
        close (unit)
    end subroutine compiled_constants

    subroutine write_box(unit, kind_name)
        integer, intent(in) :: unit
        character(*), intent(in) :: kind_name
        write (unit, '(a)') 'module box_mod', &
            'use, intrinsic :: iso_fortran_env, only: dp => '//kind_name, &
            'implicit none', 'type box_t', 'real(dp) :: v', 'end type', &
            'interface operator(*)', 'module procedure mul, scale', 'end interface', &
            'interface operator(+)', 'module procedure add, shift', 'end interface', &
            'contains', 'elemental function mul(a,b) result(c)', &
            'type(box_t), intent(in) :: a,b', 'type(box_t) :: c', &
            'c%v = a%v*b%v', 'end function', &
            'elemental function scale(a,b) result(c)', &
            'type(box_t), intent(in) :: a', 'real(dp), intent(in) :: b', &
            'type(box_t) :: c', 'c%v = a%v*b', 'end function', &
            'elemental function add(a,b) result(c)', &
            'type(box_t), intent(in) :: a,b', 'type(box_t) :: c', &
            'c%v = a%v+b%v', 'end function', &
            'elemental function shift(a,b) result(c)', &
            'type(box_t), intent(in) :: a', 'real(dp), intent(in) :: b', &
            'type(box_t) :: c', 'c%v = a%v+b', 'end function', 'end module'
    end subroutine write_box

    subroutine refusals()
        type(typed_kernel_spec_t) :: typed
        type(taylor_emit_spec_t) :: taylor
        type(kernel_emit_spec_t) :: spec
        type(kernel_ir_t) :: ir
        type(str_t) :: source
        character(:), allocatable :: message
        character(7), parameter :: names(*) = ['i      ', 'Catalan', 'unknown']
        logical :: good
        integer :: j
        typed%name = str('typed_refusal')
        typed%args = [str('x')]
        typed%outputs = [str('f')]
        typed%type_name = str('box_t')
        taylor%name = str('taylor_refusal')
        taylor%args = typed%args
        taylor%outputs = typed%outputs
        spec%name = str('ir_refusal')
        spec%args = typed%args
        spec%outputs = typed%outputs
        do j = 1, size(names)
            source = emit_typed_kernel([const(arena, trim(names(j)))*x], &
                typed, good, message)
            call check('typed explicitly refuses '//trim(names(j)), &
                .not. good .and. index(message, trim(names(j))) > 0 .and. &
                len(chars(source)) == 0)
            if (j == 1) call check('typed preserves i diagnostic', &
                message == 'emit_typed_kernel: unsupported complex constant i')
            source = emit_taylor_step([const(arena, trim(names(j)))*x], &
                taylor, good, message)
            call check('Taylor explicitly refuses '//trim(names(j)), &
                .not. good .and. index(message, trim(names(j))) > 0 .and. &
                len(chars(source)) == 0)
            if (j == 1) call check('Taylor preserves i diagnostic', &
                message == 'emit_taylor_step: unsupported complex constant i')
            call lower_kernel_ir([const(arena, trim(names(j)))*x], ir, good, message)
            call check('named constant lowers', good)
            source = emit_fortran_kernel_ir(ir, spec, good, message)
            call check('IR Fortran explicitly refuses '//trim(names(j)), &
                .not. good .and. index(message, trim(names(j))) > 0)
            source = emit_cuda_device_ir(ir, spec, good, message)
            call check('CUDA explicitly refuses '//trim(names(j)), &
                .not. good .and. index(message, trim(names(j))) > 0)
            source = emit_python_kernel_ir(ir, spec, good, message)
            if (j == 1) then
                call check('Python retains complex i support', good)
            else
                call check('Python explicitly refuses '//trim(names(j)), &
                    .not. good .and. index(message, trim(names(j))) > 0)
            end if
        end do
    end subroutine refusals
end program test_fortsym_named_constants
