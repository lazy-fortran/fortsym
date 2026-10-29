program test_fortsym_complex_i
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, const, operator(*), operator(==)
    use fortsym_diff, only: diff
    use fortsym_products, only: jvp, vjp
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, emit_statements, &
        cse_analyse, cse_result_t
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir
    use fortsym_kernel_typed, only: typed_kernel_spec_t, emit_typed_kernel
    use fortsym_kernel_taylor, only: taylor_emit_spec_t, emit_taylor_step
    use fortsym_print, only: fortran_representable
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: x, y, ii, f, roots(8), forward(1), reverse(2), one, zero
    type(kernel_spec_t) :: spec
    type(kernel_emit_spec_t) :: ir_spec
    type(kernel_ir_t) :: ir
    type(typed_kernel_spec_t) :: typed_spec
    type(taylor_emit_spec_t) :: taylor_spec
    type(cse_result_t) :: temporaries
    type(native_engine_t) :: eng
    type(engine_result_t) :: simplified
    type(str_t) :: source
    character(:), allocatable :: message
    integer :: unit, stat, nfail = 0, ncheck = 0
    logical :: good

    call arena%init()
    x = sym(arena, "x")
    y = sym(arena, "y")
    ii = const(arena, "i")
    one = num(arena, 1)
    zero = num(arena, 0)
    f = ii*x*y
    roots(1) = f
    forward = jvp([f], [x, y], [one, zero])
    roots(2) = forward(1)
    forward = jvp([f], [x, y], [zero, one])
    roots(3) = forward(1)
    forward = jvp([f], [x, y], [sym(arena, "dx"), sym(arena, "dy")])
    roots(4) = forward(1)
    reverse = vjp([f], [x, y], [sym(arena, "u")])
    roots(5:6) = reverse
    forward = jvp([ii], [x], [one])
    roots(7) = forward(1)
    reverse = vjp([ii], [x, y], [one])
    roots(8) = reverse(1)
    call check("diff i is zero", diff(ii, x) == zero)
    call check("VJP i has zero y component", reverse(2) == zero)
    eng = make_native_engine(arena)
    simplified = eng%simplify(ii*ii)
    call check("native i*i simplifies", simplified%ok)
    call check("native i*i = -1", simplified%value == num(arena, -1))
    simplified = eng%simplify(ii*ii*ii)
    call check("native i*i*i = -i", simplified%value == num(arena, -1)*ii)
    simplified = eng%simplify(ii*ii*ii*ii)
    call check("native i*i*i*i = 1", simplified%value == one)
    call check("real expression printer refuses i", .not. fortran_representable(f))

    spec%name = str("complex_i")
    spec%module_name = str("complex_i_fixture")
    spec%args = [str("x"), str("y"), str("dx"), str("dy"), str("u")]
    spec%outputs = [str("f"), str("jx"), str("jy"), str("jw"), &
        str("vx"), str("vy"), str("ji"), str("vi")]
    source = emit_kernel(roots, spec, good, message=message)
    call check("real kernel refuses i", .not. good)
    call check("real refusal explains representability", &
        index(message, "not representable in Fortran") > 0)
    call check("real refusal emits no source", len(chars(source)) == 0)
    call lower_kernel_ir([f], ir, good, message)
    call check("i lowers into scalar IR", good)
    ir_spec%name = str("real_i")
    ir_spec%args = [str("x"), str("y")]
    ir_spec%outputs = [str("f")]
    source = emit_fortran_kernel_ir(ir, ir_spec, good, message)
    call check("real IR Fortran refuses i explicitly", &
        .not. good .and. index(message, "constant i") > 0 .and. len(chars(source)) == 0)
    source = emit_cuda_device_ir(ir, ir_spec, good, message)
    call check("real CUDA refuses i explicitly", &
        .not. good .and. index(message, "constant i") > 0 .and. len(chars(source)) == 0)


    typed_spec%name = str("typed_i")
    typed_spec%args = ir_spec%args
    typed_spec%outputs = ir_spec%outputs
    typed_spec%type_name = str("dual_t")
    source = emit_typed_kernel([f], typed_spec, good, message)
    call check("typed emitter refuses i explicitly", &
        .not. good .and. index(message, "constant i") > 0 .and. len(chars(source)) == 0)
    taylor_spec%name = str("taylor_i")
    taylor_spec%args = ir_spec%args
    taylor_spec%outputs = ir_spec%outputs
    source = emit_taylor_step([f], taylor_spec, good, message)
    call check("Taylor emitter refuses i explicitly", &
        .not. good .and. index(message, "constant i") > 0 .and. len(chars(source)) == 0)

    spec%scalar_type = str("complex(dp)")
    source = emit_kernel(roots, spec, good, message=message)
    call check("complex kernel accepts i", good)
    if (good) then
        call compile_and_run(chars(source))
        temporaries = cse_analyse(roots, "t")
        source = emit_statements(roots, spec, temporaries, good)
        call check("complex statements accept i", good)
        ! A colliding input forces the literal and declaration to share an alias.
        spec%args(3) = str("dp")
        spec%outputs = [str("f")]
        spec%scalar_type = str("COMPLEX(kind=DP)")
        source = emit_kernel([f], spec, good, message=message)
        call check("complex kernel accepts kind-name collision", good)
        if (good) call compile_alias(chars(source))
    else
        print *, message
    end if
    print *, "complex i checks:", ncheck - nfail, "passed;", nfail, "failed"
    if (nfail /= 0) error stop 1

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        ncheck = ncheck + 1
        if (condition) return
        nfail = nfail + 1
        print *, "FAIL ", label
    end subroutine check

    subroutine compile_alias(code)
        character(*), intent(in) :: code
        open (newunit=unit, file="complex_i_alias.f90", status="replace")
        write (unit, '(a)') code
        close (unit)
        call execute_command_line("gfortran -std=f2018 -c complex_i_alias.f90 "// &
            "> complex_i_alias.log 2>&1", exitstat=stat)
        call check("complex literal kind alias compiles", stat == 0)
    end subroutine compile_alias

    subroutine compile_and_run(code)
        character(*), intent(in) :: code
        open (newunit=unit, file="complex_i_run.f90", status="replace")
        write (unit, '(a)') code
        write (unit, '(a)') 'program drive_complex_i'
        write (unit, '(a)') '    use, intrinsic :: iso_fortran_env, only: dp => real64'
        write (unit, '(a)') '    use complex_i_fixture, only: complex_i'
        write (unit, '(a)') '    implicit none'
        write (unit, '(a)') '    complex(dp), parameter :: ii = (0.0_dp, 1.0_dp)'
        write (unit, '(a)') '    complex(dp) :: x, y, dx, dy, u, r(8), p(8), m(8)'
        write (unit, '(a)') '    complex(dp) :: expected(8), fd(3)'
        write (unit, '(a)') '    real(dp), parameter :: h = 1.0e-5_dp'
        write (unit, '(a)') '    integer :: k'
        write (unit, '(a)') '    dx = (0.5_dp, -0.25_dp)'
        write (unit, '(a)') '    dy = (-0.75_dp, 0.5_dp)'
        write (unit, '(a)') '    u = (0.25_dp, 1.5_dp)'
        write (unit, '(a)') '    do k = 1, 4'
        write (unit, '(a)') '        x = cmplx(k*0.25_dp, -0.5_dp, dp)'
        write (unit, '(a)') '        y = cmplx(-0.75_dp, k*0.5_dp, dp)'
        write (unit, '(a)') &
            '        call complex_i(x,y,dx,dy,u,r(1),r(2),r(3),r(4), &'
        write (unit, '(a)') '            r(5),r(6),r(7),r(8))'
        write (unit, '(a)') '        expected = [ii*x*y, ii*y, ii*x, &'
        write (unit, '(a)') &
            '            ii*(y*dx+x*dy), u*ii*y, u*ii*x, (0.0_dp,0.0_dp), &'
        write (unit, '(a)') '            (0.0_dp,0.0_dp)]'
        write (unit, '(a)') &
            '        if (any(abs(r-expected)>1.0e-12_dp)) error stop 1'
        write (unit, '(a)') &
            '        call complex_i(x+h,y,dx,dy,u,p(1),p(2),p(3),p(4), &'
        write (unit, '(a)') '            p(5),p(6),p(7),p(8))'
        write (unit, '(a)') &
            '        call complex_i(x-h,y,dx,dy,u,m(1),m(2),m(3),m(4), &'
        write (unit, '(a)') '            m(5),m(6),m(7),m(8))'
        write (unit, '(a)') '        fd(1) = (p(1)-m(1))/(2*h)'
        write (unit, '(a)') &
            '        call complex_i(x,y+h,dx,dy,u,p(1),p(2),p(3),p(4), &'
        write (unit, '(a)') '            p(5),p(6),p(7),p(8))'
        write (unit, '(a)') &
            '        call complex_i(x,y-h,dx,dy,u,m(1),m(2),m(3),m(4), &'
        write (unit, '(a)') '            m(5),m(6),m(7),m(8))'
        write (unit, '(a)') '        fd(2) = (p(1)-m(1))/(2*h)'
        write (unit, '(a)') &
            '        call complex_i(x+h*dx,y+h*dy,dx,dy,u,p(1),p(2),p(3), &'
        write (unit, '(a)') '            p(4),p(5),p(6),p(7),p(8))'
        write (unit, '(a)') &
            '        call complex_i(x-h*dx,y-h*dy,dx,dy,u,m(1),m(2),m(3), &'
        write (unit, '(a)') '            m(4),m(5),m(6),m(7),m(8))'
        write (unit, '(a)') '        fd(3) = (p(1)-m(1))/(2*h)'
        write (unit, '(a)') &
            '        if (any(abs(r(2:4)-fd)>1.0e-9_dp)) error stop 2'
        write (unit, '(a)') &
            '        if (any(abs(r(5:6)-u*fd(1:2))>1.0e-9_dp)) error stop 3'
        write (unit, '(a)') '    end do'
        write (unit, '(a)') '    print *, "compiled complex oracle: 52 checks passed"'
        write (unit, '(a)') 'end program drive_complex_i'
        close (unit)
        call execute_command_line("gfortran -std=f2018 -o complex_i_run "// &
            "complex_i_run.f90 > complex_i_run.log 2>&1", exitstat=stat)
        call check("complex i and derivative kernels compile", stat == 0)
        if (stat /= 0) return
        call execute_command_line("./complex_i_run", exitstat=stat)
        call check("complex value, hand derivatives and finite differences", stat == 0)
    end subroutine compile_and_run

end program test_fortsym_complex_i
