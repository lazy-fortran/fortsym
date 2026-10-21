program test_fortsym_kernel_batch
    ! M1/M2 emission: the declare-simd leaf and the batched OpenMP host driver.
    ! Generic expressions (polynomial, rational, transcendental and a symbolic
    ! derivative) are emitted, compiled with gfortran -fopenmp -Wall -Wextra
    ! -Werror and run on 10^6 points. The batch result must equal the scalar
    ! leaf bitwise at 1, 2 and 8 threads, with FMA contraction off.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym, only: arena_t, expr_t, sym, num, sin, exp, diff, engine_result_t, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_string, only: str_t, str, chars
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_fortran_batch_ir, TARGET_FORTRAN_OPENMP_SIMD, TARGET_FORTRAN_CPU, &
        TARGET_CUDA, target_name
    implicit none

    integer :: nfail = 0
    ! Kernels 1-4 are deterministic (scalar leaf per element); 5-8 are the SIMD
    ! variants of the same expressions; 9 is a SIMD batch with a private leaf.
    ! kref is the scalar leaf each batch is compared with. Expressions 1-2 use
    ! only + - * /, so their SIMD variants are exactly equal to the scalar leaf.
    integer, parameter :: nk = 10
    character(8), parameter :: kname(nk) = [character(8) :: "kd_poly", "kd_rat", &
        "kd_trans", "kd_dsin", "kf_poly", "kf_rat", "kf_trans", "kf_dsin", "ki_poly", &
        "ks_poly"]
    integer, parameter :: kexpr(nk) = [1, 2, 3, 4, 1, 2, 3, 4, 1, 1]
    integer, parameter :: kref(nk) = [1, 2, 3, 4, 1, 2, 3, 4, 1, 1]
    logical, parameter :: kdet(nk) = [.true., .true., .true., .true., .false., &
        .false., .false., .false., .false., .false.]
    ! ks_poly: SIMD driver without a thread team of its own (batch_threaded off).
    logical, parameter :: kthreaded(nk) = [.true., .true., .true., .true., .true., &
        .true., .true., .true., .true., .false.]
    logical, parameter :: kinline(nk) = [.false., .false., .false., .false., &
        .false., .false., .false., .false., .true., .false.]
    logical, parameter :: kexact(nk) = [.true., .true., .true., .true., .true., &
        .true., .false., .false., .true., .true.]

    call test_leaf_directive()
    call test_batch_rejections()
    call test_long_batch()
    call test_compiled_batches()

    if (nfail == 0) then
        print *, "test_fortsym_kernel_batch: all checks passed"
    else
        print *, "test_fortsym_kernel_batch: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    subroutine make_ir(which, ir, good)
        integer, intent(in) :: which
        type(kernel_ir_t), intent(out) :: ir
        logical, intent(out) :: good
        type(arena_t), target :: arena
        type(expr_t) :: x, y, a, roots(1)
        type(engine_result_t) :: d
        character(:), allocatable :: message

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        a = sym(arena, "a")
        select case (which)
        case (1)
            roots(1) = x**3 - num(arena, 2)*x*y**2 + a*y + num(arena, 1)
        case (2)
            roots(1) = (x**2 + a)/(x*y + num(arena, 3)) + y/(x**2 + y**2 + num(arena, 1))
        case (3)
            roots(1) = sin(x)*exp(-y*y) + a*exp(x)/(num(arena, 2) + sin(y))
        case default
            d = diff(sin(x*y)*exp(a*x)/(num(arena, 1) + x**2), x)
            roots(1) = d%value
        end select
        call lower_kernel_ir(roots, ir, good, message)
    end subroutine make_ir

    subroutine fill_spec(spec, name)
        type(kernel_emit_spec_t), intent(out) :: spec
        character(*), intent(in) :: name

        spec%name = str(name)
        allocate (spec%args(3), spec%outputs(1), spec%uniform(1))
        spec%args(1) = str("x")
        spec%args(2) = str("y")
        spec%args(3) = str("a")
        spec%outputs(1) = str("r")
        spec%uniform(1) = str("a")
    end subroutine fill_spec

    subroutine test_leaf_directive()
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        character(:), allocatable :: message, src, cpu
        logical :: good
        integer :: p, q

        call make_ir(1, ir, good)
        call ok("leaf IR lowers", good)
        call fill_spec(spec, "simd_leaf")
        spec%target = TARGET_FORTRAN_OPENMP_SIMD
        spec%pure_procedure = .true.
        src = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("simd target is accepted", good)
        call ok("simd target name is stable", &
            target_name(TARGET_FORTRAN_OPENMP_SIMD) == "fortran_openmp_simd")
        p = index(src, "!$omp declare simd uniform(a)")
        q = index(src, "subroutine simd_leaf(")
        call ok("declare simd follows the subroutine statement", &
            p > q .and. q > 0 .and. index(src, "!$acc") == 0 .and. &
            index(src, "declare target") == 0)
        spec%target = TARGET_FORTRAN_CPU
        cpu = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("simd leaf body equals the CPU leaf body", good .and. &
            index(src, cpu(index(cpu, "implicit none"):)) > 0)
        spec%target = TARGET_FORTRAN_OPENMP_SIMD
        spec%uniform(1) = str("missing")
        src = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("unknown uniform name is refused", .not. good)
        ! A 63-character name continues the subroutine statement; the
        ! directive must follow the whole statement, not its first line.
        call fill_spec(spec, repeat("long_leaf_", 6)//"abc")
        spec%target = TARGET_FORTRAN_OPENMP_SIMD
        spec%pure_procedure = .true.
        src = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        p = index(src, "!$omp declare simd uniform(a)")
        call ok("declare simd follows a continued subroutine statement", &
            good .and. p > 2 .and. index(src, " &"//new_line("a")) > 0 .and. &
            src(max(1, p - 6):max(1, p - 1)) == ")"//new_line("a")//"    ")
    end subroutine test_leaf_directive

    subroutine test_long_batch()
        type(arena_t), target :: arena
        type(expr_t) :: x, y, a
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        character(:), allocatable :: src, message
        character(*), parameter :: xn = "argument_x_"//repeat("x", 35)
        character(*), parameter :: yn = "argument_y_"//repeat("y", 35)
        character(*), parameter :: an = "uniform_a_"//repeat("a", 35)
        logical :: good
        integer :: unit, mode, first, last, stat

        call arena%init()
        x = sym(arena, xn)
        y = sym(arena, yn)
        a = sym(arena, an)
        call lower_kernel_ir([x + num(arena, 2)*y + a], ir, good, message)
        call ok("wide batch lowers", good)
        spec%name = str("wide_sum")
        spec%args = [str(xn), str(yn), str(an)]
        spec%outputs = [str("r")]
        spec%uniform = [str(an)]
        spec%batch_threaded = .false.
        do mode = 1, 2
            spec%batch_deterministic = mode == 2
            src = chars(emit_fortran_batch_ir(ir, spec, good, message))
            call ok("wide batch emits "//message, good)
            if (.not. good) cycle
            first = 1
            do while (first <= len(src))
                last = index(src(first:), new_line("a"))
                if (last == 0) last = len(src) - first + 2
                call ok("every wide batch line fits 88 columns", last - 1 <= 88)
                first = first + last
            end do
            open (newunit=unit, file="wide_batch.f90", status="replace")
            call write_text(unit, src)
            call write_text(unit, "program wide_batch_test")
            call write_text(unit, "use wide_sum_batch_mod")
            call write_text(unit, "use iso_fortran_env, only: real64, int64")
            call write_text(unit, "implicit none")
            call write_text(unit, "real(real64) :: r(2), reference")
            call write_text(unit, "call wide_sum_batch(2, [1.0_real64, 2.0_real64], &")
            call write_text(unit, "    [3.0_real64, 4.0_real64], 5.0_real64, r)")
            call write_text(unit, "call wide_sum(1.0_real64, 3.0_real64, &")
            call write_text(unit, "    5.0_real64, reference)")
            call write_text(unit, "if (transfer(r(1), 0_int64) /= &")
            call write_text(unit, "    transfer(reference, 0_int64)) error stop 1")
            call write_text(unit, "if (any(abs(r - [12.0_real64, 15.0_real64]) > &")
            call write_text(unit, "    0.0_real64)) error stop 2")
            call write_text(unit, "end program wide_batch_test")
            close(unit)
            call execute_command_line("gfortran -std=f2018 -fopenmp "// &
                "-ffree-line-length-132 -Werror=line-truncation "// &
                "wide_batch.f90 -o wide_batch_run > wide_batch_build.log 2>&1", &
                exitstat=stat)
            call ok("wide batch compiles at standard line length", stat == 0)
            if (stat /= 0) cycle
            call execute_command_line("./wide_batch_run", exitstat=stat)
            call ok("wide scalar/batch and independent affine oracle agree", stat == 0)
        end do
    end subroutine test_long_batch

    subroutine test_batch_rejections()
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        character(:), allocatable :: message, src
        logical :: good

        call make_ir(1, ir, good)
        call fill_spec(spec, "rej")
        spec%target = TARGET_CUDA
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("batch refuses a CUDA target", .not. good)
        call fill_spec(spec, "rej")
        spec%args(1) = str("batch_n")
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("batch refuses a reserved argument name", .not. good)
        call fill_spec(spec, "rej")
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("batch emits", good)
        call ok("batch defaults to the SIMD driver", &
            index(src, "!$omp parallel do simd schedule(static)") > 0)
        spec%batch_deterministic = .true.
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("batch module has the driver", &
            index(src, "module rej_batch_mod") > 0 .and. &
            index(src, "subroutine rej_batch(batch_n, x, y, a, r)") > 0 .and. &
            index(src, "real(real64), intent(in) :: a" // new_line("a")) > 0 .and. &
            index(src, "!$omp parallel do simd safelen(1) schedule(static)") > 0 .and. &
            index(src, "!GCC$") == 0 .and. &
            index(src, "declare simd") == 0)
        spec%batch_deterministic = .false.
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("simd batch emits declare simd and a simd loop", good .and. &
            index(src, "!$omp parallel do simd schedule(static)") > 0 .and. &
            index(src, "!$omp declare simd uniform(a)") > 0 .and. &
            index(src, "noinline") == 0)
        spec%batch_threaded = .false.
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("unthreaded SIMD batch has a simd loop and no thread team", good .and. &
            index(src, "!$omp simd") > 0 .and. index(src, "parallel") == 0)
        spec%batch_deterministic = .true.
        src = chars(emit_fortran_batch_ir(ir, spec, good, message))
        call ok("unthreaded deterministic batch restricts SIMD to width one", &
            good .and. index(src, "!$omp simd safelen(1)") > 0 .and. &
            index(src, "parallel") == 0)
    end subroutine test_batch_rejections

    subroutine write_text(unit, text)
        integer, intent(in) :: unit
        character(*), intent(in) :: text

        write (unit, "(a)") text
    end subroutine write_text

    subroutine test_compiled_batches()
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        character(:), allocatable :: message, src
        logical :: good
        integer :: k, unit, ios, stat, nvec
        character(256) :: line
        character(*), parameter :: flags = "-O3 -fopenmp -ffp-contract=off "// &
            "-Wall -Wextra -Werror -fopt-info-vec"

        ! One module per kernel, written to the current directory.
        open (newunit=unit, file="kbatch_modules.f90", status="replace", &
            action="write", iostat=ios)
        call ok("fixture opens", ios == 0)
        if (ios /= 0) return
        do k = 1, nk
            call make_ir(kexpr(k), ir, good)
            call ok("kernel IR lowers: " // trim(kname(k)), good)
            if (.not. good) cycle
            call fill_spec(spec, trim(kname(k)))
            spec%batch_deterministic = kdet(k)
            spec%batch_inline = kinline(k)
            spec%batch_threaded = kthreaded(k)
            spec%batch_parallel_min = 1000
            src = chars(emit_fortran_batch_ir(ir, spec, good, message))
            call ok("batch emits: " // trim(kname(k)) // " " // message, good)
            if (good) call write_text(unit, src)
        end do
        close (unit)

        open (newunit=unit, file="kbatch_main.f90", status="replace", &
            action="write", iostat=ios)
        if (ios /= 0) return
        call write_driver(unit)
        close (unit)

        call execute_command_line("gfortran " // flags // " -o kbatch_run "// &
            "kbatch_modules.f90 kbatch_main.f90 > kbatch_build.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("generated code compiles -Wall -Wextra -Werror under -fopenmp", &
            stat == 0)
        if (stat /= 0) return
        nvec = 0
        open (newunit=unit, file="kbatch_build.log", status="old", action="read", &
            iostat=ios)
        do while (ios == 0)
            read (unit, "(a)", iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "kbatch_modules.f90") > 0 .and. &
                index(line, "loop vectorized") > 0) nvec = nvec + 1
        end do
        close (unit)
        call ok("vectorisation report lists a vectorised generated loop", nvec > 0)
        call execute_command_line("./kbatch_run", wait=.true., exitstat=stat)
        call ok("batch equals scalar leaf at 1, 2 and 8 threads", stat == 0)
    end subroutine test_compiled_batches

    subroutine write_driver(unit)
        integer, intent(in) :: unit
        integer :: k
        character(:), allocatable :: nm, ref

        call write_text(unit, "program kbatch_main")
        call write_text(unit, "    use, intrinsic :: iso_fortran_env, only: real64, int64")
        call write_text(unit, "    use omp_lib, only: omp_set_num_threads")
        do k = 1, nk
            call write_text(unit, "    use " // trim(kname(k)) // "_batch_mod")
        end do
        call write_text(unit, "    implicit none")
        call write_text(unit, "    integer, parameter :: n = 1000000")
        call write_text(unit, "    integer, parameter :: threads(3) = [1, 2, 8]")
        call write_text(unit, "    real(real64), allocatable :: x(:), y(:)")
        call write_text(unit, "    real(real64), allocatable :: ref(:), out(:), first(:)")
        call write_text(unit, "    real(real64) :: a, worst")
        call write_text(unit, "    integer :: i, t")
        call write_text(unit, "    allocate (x(n), y(n), ref(n), out(n), first(n))")
        call write_text(unit, "    a = 0.375_real64")
        call write_text(unit, "    do i = 1, n")
        call write_text(unit, "        x(i) = -1.5_real64 + 3.0_real64*real(i, real64)/real(n, real64)")
        call write_text(unit, "        y(i) = 0.25_real64 + 2.0_real64*real(mod(7*i, n), real64)/real(n, real64)")
        call write_text(unit, "    end do")
        do k = 1, nk
            nm = trim(kname(k))
            ref = trim(kname(kref(k)))
            call write_text(unit, "    do i = 1, n")
            call write_text(unit, "        call " // ref // "(x(i), y(i), a, ref(i))")
            call write_text(unit, "    end do")
            call write_text(unit, "    worst = 0.0_real64")
            call write_text(unit, "    do t = 1, size(threads)")
            call write_text(unit, "        call omp_set_num_threads(threads(t))")
            call write_text(unit, "        out = -huge(1.0_real64)")
            call write_text(unit, "        call " // nm // "_batch(n, x, y, a, out)")
            call write_text(unit, "        worst = max(worst, maxval(abs(out - ref)/max(1.0_real64, abs(ref))))")
            if (kexact(k)) then
                call write_text(unit, "        if (any(transfer(out, [0_int64]) /= transfer(ref, [0_int64]))) then")
                call write_text(unit, "            print *, 'MISMATCH " // nm // " threads', threads(t)")
                call write_text(unit, "            error stop 1")
                call write_text(unit, "        end if")
            else
                call write_text(unit, "        if (worst > 1.0e-12_real64) then")
                call write_text(unit, "            print *, 'TOLERANCE " // nm // " threads', threads(t), worst")
                call write_text(unit, "            error stop 3")
                call write_text(unit, "        end if")
            end if
            call write_text(unit, "        if (t == 1) first = out")
            if (kexact(k)) call write_text(unit, &
                "        if (any(transfer(out, [0_int64]) /= transfer(first, [0_int64]))) error stop 2")
            call write_text(unit, "    end do")
            call write_text(unit, "    print '(a,es10.2)', 'max rel dev " // nm // ": ', worst")
        end do
        call write_text(unit, "end program kbatch_main")
    end subroutine write_driver

end program test_fortsym_kernel_batch
