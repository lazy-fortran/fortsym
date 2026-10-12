program gen_kernel_batch_bench
    ! Writes <dir>/kbench_modules.f90 (batch modules for a transcendental
    ! expression, deterministic and SIMD variants) and <dir>/kbench_main.f90
    ! (strong-scaling driver). See run_kernel_batch_scaling.sh.
    use fortsym, only: arena_t, expr_t, sym, num, sin, exp, &
        operator(+), operator(*), operator(/)
    use fortsym_string, only: str, chars
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_batch_ir
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: x, y, a, roots(1)
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    character(:), allocatable :: message, dir
    character(1024) :: arg
    logical :: good
    integer :: unit, v
    character(8), parameter :: names(2) = [character(8) :: "kb_det", "kb_simd"]

    if (command_argument_count() < 1) error stop "usage: gen_kernel_batch_bench DIR"
    call get_command_argument(1, arg)
    dir = trim(arg)

    call arena%init()
    x = sym(arena, "x")
    y = sym(arena, "y")
    a = sym(arena, "a")
    roots(1) = sin(x)*exp(a*y)/(num(arena, 2) + sin(y)*sin(y)) &
        + exp(x*y)/(num(arena, 1) + x*x)
    call lower_kernel_ir(roots, ir, good, message)
    if (.not. good) error stop "lowering failed"

    open (newunit=unit, file=dir // "/kbench_modules.f90", status="replace", &
        action="write")
    do v = 1, 2
        spec%name = str(trim(names(v)))
        if (allocated(spec%args)) deallocate (spec%args, spec%outputs, spec%uniform)
        allocate (spec%args(3), spec%outputs(1), spec%uniform(1))
        spec%args = [str("x"), str("y"), str("a")]
        spec%outputs(1) = str("r")
        spec%uniform(1) = str("a")
        spec%batch_deterministic = (v == 1)
        write (unit, "(a)") chars(emit_fortran_batch_ir(ir, spec, good, message))
        if (.not. good) error stop "emission failed"
    end do
    close (unit)

    open (newunit=unit, file=dir // "/kbench_main.f90", status="replace", &
        action="write")
    write (unit, "(a)") "program kbench_main"
    write (unit, "(a)") "    use, intrinsic :: iso_fortran_env, only: real64"
    write (unit, "(a)") "    use omp_lib, only: omp_set_num_threads, omp_get_wtime"
    write (unit, "(a)") "    use kb_det_batch_mod"
    write (unit, "(a)") "    use kb_simd_batch_mod"
    write (unit, "(a)") "    implicit none"
    write (unit, "(a)") "    integer, parameter :: n = 8000000, nrep = 7"
    write (unit, "(a)") "    real(real64), allocatable :: x(:), y(:), r(:)"
    write (unit, "(a)") "    real(real64) :: a, t0, best(2), t1(2)"
    write (unit, "(a)") "    integer :: i, v, rep, nt, narg"
    write (unit, "(a)") "    character(32) :: arg"
    write (unit, "(a)") "    allocate (x(n), y(n), r(n))"
    write (unit, "(a)") "    a = 0.375_real64"
    write (unit, "(a)") "    !$omp parallel do schedule(static)"
    write (unit, "(a)") "    do i = 1, n"
    write (unit, "(a)") "        x(i) = -1.5_real64 + 3.0_real64*real(i, real64)/real(n, real64)"
    write (unit, "(a)") "        y(i) = 0.25_real64 + 2.0_real64*real(mod(7*i, n), real64)/real(n, real64)"
    write (unit, "(a)") "    end do"
    write (unit, "(a)") "    r = 0.0_real64"
    write (unit, "(a)") "    narg = command_argument_count()"
    write (unit, "(a)") "    print '(a)', '# threads  t_det[s]  speedup_det  t_simd[s]  speedup_simd'"
    write (unit, "(a)") "    do i = 1, narg"
    write (unit, "(a)") "        call get_command_argument(i, arg)"
    write (unit, "(a)") "        read (arg, *) nt"
    write (unit, "(a)") "        call omp_set_num_threads(nt)"
    write (unit, "(a)") "        best = huge(1.0_real64)"
    write (unit, "(a)") "        do rep = 1, nrep"
    write (unit, "(a)") "            do v = 1, 2"
    write (unit, "(a)") "                t0 = omp_get_wtime()"
    write (unit, "(a)") "                if (v == 1) call kb_det_batch(n, x, y, a, r)"
    write (unit, "(a)") "                if (v == 2) call kb_simd_batch(n, x, y, a, r)"
    write (unit, "(a)") "                best(v) = min(best(v), omp_get_wtime() - t0)"
    write (unit, "(a)") "            end do"
    write (unit, "(a)") "        end do"
    write (unit, "(a)") "        if (i == 1) t1 = best"
    write (unit, "(a)") "        print '(i8,4es13.4)', nt, best(1), t1(1)/best(1), best(2), t1(1)/best(2)"
    write (unit, "(a)") "    end do"
    write (unit, "(a)") "    print '(a,es12.4)', '# checksum ', sum(r)"
    write (unit, "(a)") "end program kbench_main"
    close (unit)
end program gen_kernel_batch_bench
