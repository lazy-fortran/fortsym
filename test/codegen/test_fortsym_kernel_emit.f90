program test_fortsym_kernel_emit
    ! Both emitters consume one IR instance. The Fortran result is compiled and
    ! run here; when nvcc and a CUDA device are available, the CUDA leaf is
    ! launched through a tiny independent wrapper and checked against the same
    ! host formula.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sin_expr => sin, exp_expr => exp, &
        operator(+), operator(*), operator(/), operator(**), num, rat, real_expr, &
        operator(==), func, erf_expr => erf, erfc_expr => erfc, gamma_expr => gamma, &
        besselj_expr => besselj
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit
    use fortsym_fparse, only: parse_fortran_array
    use fortsym_print, only: fortran_representable
    use fortsym_quadratic, only: quadratic_t, quad
    use fortsym_proc, only: proc_available
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_emitters_share_ir()
    call test_emission_policies()
    call test_target_driven_emission()
    call test_special_function_emission()
    call test_emit_tables()
    call test_fortran_symbol_name_boundary()

    if (nfail == 0) then
        print *, "test_fortsym_kernel_emit: all checks passed"
    else
        print *, "test_fortsym_kernel_emit: ", nfail, " check(s) FAILED"
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

    function named1(name, x) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: x
        type(expr_t) :: e, args(1)

        args(1) = x
        e = func(name, args)
    end function named1

    function named2(name, x, y) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: x, y
        type(expr_t) :: e, args(2)

        args(1) = x
        args(2) = y
        e = func(name, args)
    end function named2

    subroutine test_special_function_emission()
        type(arena_t), target :: arena
        type(expr_t) :: x, one, zero, root, roots(1)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        type(str_t) :: source
        logical :: good
        character(:), allocatable :: message
        integer :: unit, ios, stat

        call arena%init()
        x = sym(arena, "x")
        one = num(arena, 1)
        zero = num(arena, 0)
        root = erf_expr(x) + erfc_expr(x) + gamma_expr(x + one) + &
            named1("loggamma", x + one) + besselj_expr(zero, x)
        roots(1) = root
        call lower_kernel_ir(roots, ir, good, message)
        call ok("special-function IR lowers", good)
        if (.not. good) return

        spec%name = str("fortsym_special_leaf")
        allocate (spec%args(1), spec%outputs(1))
        spec%args(1) = str("x")
        spec%outputs(1) = str("r")
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("standard special functions emit", good)
        call ok("standard specials need no fortnum use", &
            index(chars(source), "use fortnum_special") == 0)
        call ok("loggamma maps to log_gamma", &
            index(chars(source), "log_gamma(") > 0)
        call ok("BesselJ maps to bessel_jn with integer order", &
            index(chars(source), "bessel_jn(0,") > 0)

        open (newunit=unit, file="/tmp/fortsym_special_fortran.f90", &
            status="replace", action="write", iostat=ios)
        call ok("special-function fixture opens", ios == 0)
        if (ios /= 0) return
        write (unit, "(a)") chars(source)
        write (unit, "(a)") "program drive_fortsym_special"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(real64) :: r"
        write (unit, "(a)") "  call fortsym_special_leaf(0.0_real64, r)"
        write (unit, "(a)") "  if (abs(r - 3.0_real64) > 1.0e-14_real64) error stop 1"
        write (unit, "(a)") "end program drive_fortsym_special"
        close (unit)
        call execute_command_line( &
            "gfortran -o /tmp/fortsym_special_fortran "// &
            "/tmp/fortsym_special_fortran.f90 > /tmp/fortsym_special_fortran.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("standard special kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_special_fortran", wait=.true., &
                exitstat=stat)
            call ok("standard special kernel matches exact-value oracle", stat == 0)
        end if

        root = named2("besseli", zero, x) + named2("besselk", zero, x)
        roots(1) = root
        call lower_kernel_ir(roots, ir, good, message)
        call ok("fortnum special IR lowers", good)
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("fortnum special functions emit", good)
        call ok("fortnum special module is declared", &
            index(chars(source), "use fortnum_special, only: bessel_in, bessel_kn") > 0)
        call ok("besseli maps to bessel_in", index(chars(source), "bessel_in(0,") > 0)
        call ok("besselk maps to bessel_kn", index(chars(source), "bessel_kn(0,") > 0)

        root = named1("elliptic_k", x) + named1("elliptic_e", x)
        roots(1) = root
        call lower_kernel_ir(roots, ir, good, message)
        call ok("elliptic IR lowers", good)
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("elliptic integrals emit", good)
        call ok("elliptic integrals import fortnum_special", &
            index(chars(source), "use fortnum_special, only: ellip_k, ellip_e") > 0)
        call ok("elliptic_k maps to ellip_k", index(chars(source), "ellip_k(x)") > 0)
        call ok("elliptic_e maps to ellip_e", index(chars(source), "ellip_e(x)") > 0)
        call ok("elliptic integrals are representable", fortran_representable(root))

        root = named1("zeta", x)
        roots(1) = root
        call lower_kernel_ir(roots, ir, good, message)
        call ok("unmapped special IR lowers for refusal", good)
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("unmapped special is refused at emission", .not. good)
        call ok("unmapped special names the head", index(message, "zeta") > 0)
        call ok("unmapped special is refused by representability", &
            .not. fortran_representable(root))
    end subroutine test_special_function_emission

    subroutine test_emitters_share_ir()
        type(arena_t), target :: arena
        type(expr_t) :: x, y, shared, root, roots(2)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        type(str_t) :: fortran_source, cuda_source
        logical :: good
        character(:), allocatable :: message
        integer :: unit, ios, stat
        character(32) :: cuda_arch

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        shared = x*y
        root = sin_expr(shared) + exp_expr(x)/(1 + y*y) + (x + 1)**2
        roots(1) = root
        roots(2) = shared
        call lower_kernel_ir(roots, ir, good, message)
        call ok("shared IR lowers for the emitter test", good)
        if (.not. good) return

        spec%name = str("fortsym_ir_leaf")
        allocate (spec%args(2), spec%outputs(2))
        spec%args(1) = str("x")
        spec%args(2) = str("y")
        spec%outputs(1) = str("r")
        spec%outputs(2) = str("s")
        spec%generator = str("test_fortsym_kernel_emit")
        spec%regenerate_command = str("ctest -R test_fortsym_kernel_emit")

        fortran_source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("Fortran emitter accepts the shared IR", good)
        call ok("Fortran emitter has a diagnostic-free result", &
            len(message) == 0)
        call ok("Fortran emitter declares real64", &
            index(chars(fortran_source), "real(real64)") > 0)
        call ok("Fortran emitter writes both roots", &
            index(chars(fortran_source), "s = t3") > 0)

        open (newunit=unit, file="/tmp/fortsym_ir_fortran.f90", &
            status="replace", action="write", iostat=ios)
        call ok("Fortran fixture opens", ios == 0)
        if (ios /= 0) return
        write (unit, "(a)") chars(fortran_source)
        write (unit, "(a)") "program drive_fortsym_ir"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(real64) :: r, s"
        write (unit, "(a)") "  real(real64), parameter :: x = 0.37_real64"
        write (unit, "(a)") "  real(real64), parameter :: y = -0.81_real64"
        write (unit, "(a)") "  call fortsym_ir_leaf(x, y, r, s)"
        write (unit, "(a)") "  if (abs(r - (sin(x*y) + exp(x)/(1.0_real64 + y*y) + "// &
            "(x + 1.0_real64)**2)) > 1.0e-14_real64) error stop 1"
        write (unit, "(a)") "  if (abs(s - x*y) > 1.0e-14_real64) error stop 2"
        write (unit, "(a)") "end program drive_fortsym_ir"
        close (unit)
        call execute_command_line( &
            "gfortran -o /tmp/fortsym_ir_fortran /tmp/fortsym_ir_fortran.f90 "// &
            "> /tmp/fortsym_ir_fortran.log 2>&1", wait=.true., exitstat=stat)
        call ok("generated Fortran compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_ir_fortran", wait=.true., &
                exitstat=stat)
            call ok("generated Fortran agrees with independent oracle", stat == 0)
        end if

        cuda_source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("CUDA emitter accepts the same IR", good)
        call ok("CUDA emitter has a diagnostic-free result", len(message) == 0)
        call ok("CUDA emitter uses a device leaf", &
            index(chars(cuda_source), "__device__") > 0)
        call ok("CUDA emitter expands these exact small powers", &
            index(chars(cuda_source), "pow(") == 0 .and. &
            index(chars(cuda_source), " / ") > 0)

        spec%args(1) = str("x")
        spec%args(2) = str("bad-name")
        fortran_source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("invalid identifiers are refused", .not. good)

        spec%args(1) = str("x")
        spec%args(2) = str("x")
        fortran_source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("duplicate arguments are refused", .not. good)

        spec%args(1) = str("x")
        spec%args(2) = str("y")
        spec%outputs(1) = str("r")
        spec%outputs(2) = str("r")
        cuda_source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("duplicate outputs are refused", .not. good)

        spec%outputs(1) = str("r")
        spec%outputs(2) = str("s")
        spec%args(1) = str("x")
        spec%args(2) = str("y")
        cuda_source = emit_cuda_batch_ir(ir, spec, good, message)
        call ok("CUDA batch emitter restores the valid specification", good)
        spec%name = str(repeat('x', 58))
        fortran_source = emit_cuda_batch_ir(ir, spec, good, message)
        call ok("CUDA batch refuses an overlong entry point", .not. good)
        spec%name = str("fortsym_ir_leaf")
        call get_environment_variable('FORTSYM_TEST_CUDA_ARCH', cuda_arch)
        if (len_trim(cuda_arch) == 0) then
            print *, 'CUDA execution opt-in: set FORTSYM_TEST_CUDA_ARCH (e.g. sm_120)'
            return
        end if
        call ok('CUDA architecture is explicit', &
            verify(trim(cuda_arch), 'sm_compute0123456789_') == 0)
        if (verify(trim(cuda_arch), 'sm_compute0123456789_') /= 0) return

        ! proc_available rather than a hand-rolled probe: `command -v` on a
        ! missing program exits 1 under bash but 127 under dash, and libgfortran
        ! reports 127 as an invalid command, which aborts the program unless
        ! cmdstat is present to absorb it. That made this test pass on a
        ! bash-as-/bin/sh distribution and abort on a Debian-derived one.
        if (.not. proc_available("nvcc")) then
            print *, "CUDA compiler unavailable; CUDA compile/run oracle skipped"
            return
        end if

        open (newunit=unit, file="/tmp/fortsym_ir_cuda.cu", &
            status="replace", action="write", iostat=ios)
        call ok("CUDA fixture opens", ios == 0)
        if (ios /= 0) return
        write (unit, "(a)") chars(cuda_source)
        write (unit, "(a)") "#include <cuda_runtime.h>"
        write (unit, "(a)") "#include <cmath>"
        write (unit, "(a)") "int main() {"
        write (unit, "(a)") "  double hx[2] = {0.37, -0.81}, hy[2] = {0., 0.};"
        write (unit, "(a)") "  double *dx, *dy; int device_count = 0;"
        write (unit, "(a)") "  if (cudaGetDeviceCount(&device_count) != cudaSuccess || device_count == 0) return 77;"
        write (unit, "(a)") "  if (cudaMalloc(&dx, sizeof(hx)) || cudaMalloc(&dy, sizeof(hy))) return 10;"
        write (unit, "(a)") "  if (cudaMemcpy(dx, hx, sizeof(hx), cudaMemcpyHostToDevice)) return 11;"
        write (unit, "(a)") "  fortsym_ir_leaf_batch<<<1, 32>>>(1, dx, dy);"
        write (unit, "(a)") "  if (cudaDeviceSynchronize() != cudaSuccess) return 12;"
        write (unit, "(a)") "  if (cudaMemcpy(hy, dy, sizeof(hy), cudaMemcpyDeviceToHost)) return 13;"
        write (unit, "(a)") "  double expected = sin(hx[0]*hx[1]) + exp(hx[0])/(1.0 + hx[1]*hx[1]) + pow(hx[0] + 1.0, 2.0);"
        write (unit, "(a)") "  cudaFree(dx); cudaFree(dy);"
        write (unit, "(a)") "  return fabs(hy[0] - expected) > 1e-13 || fabs(hy[1] - hx[0]*hx[1]) > 1e-13;"
        write (unit, "(a)") "}"
        close (unit)
        call execute_command_line( &
            "nvcc_path=$(command -v nvcc) && "// &
            "nvcc_dir=$(dirname ""$nvcc_path"") && "// &
            "env -i PATH=""$nvcc_dir:/usr/bin:/bin"" HOME=/tmp "// &
            """$nvcc_path"" -arch="//trim(cuda_arch)// &
            " -O2 -std=c++17 -o /tmp/fortsym_ir_cuda "// &
            "/tmp/fortsym_ir_cuda.cu "// &
            "> /tmp/fortsym_ir_cuda.log 2>&1", wait=.true., exitstat=stat)
        call ok("generated CUDA compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line( &
                "nvcc_path=$(command -v nvcc) && "// &
                "nvcc_dir=$(dirname ""$nvcc_path"") && "// &
                "env -i PATH=""$nvcc_dir:/usr/bin:/bin"" HOME=/tmp "// &
                "/tmp/fortsym_ir_cuda", wait=.true., &
                exitstat=stat)
            call ok("generated CUDA agrees with independent oracle", stat == 0)
        end if

    end subroutine test_emitters_share_ir

    subroutine test_emission_policies()
        type(arena_t), target :: arena
        type(expr_t) :: x, y, z, zero, one, two, root, roots(1)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        type(str_t) :: source
        character(:), allocatable :: default_source, conservative_source, message
        logical :: good
        integer :: unit, ios, stat

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        z = sym(arena, "z")
        zero = num(arena, 0_int64)
        one = num(arena, 1_int64)
        two = num(arena, 2_int64)
        root = x**two + x*y + z + y/two + zero*x + one*x + zero
        roots(1) = root
        call lower_kernel_ir(roots, ir, good, message)
        call ok("policy test lowers its IR", good)
        if (.not. good) return

        spec%name = str("policy_leaf")
        allocate (spec%args(3), spec%outputs(1))
        spec%args(1) = str("x")
        spec%args(2) = str("y")
        spec%args(3) = str("z")
        spec%outputs(1) = str("r")
        spec%generator = str("test_fortsym_kernel_emit")
        spec%regenerate_command = str("ctest -R test_fortsym_kernel_emit")

        source = emit_fortran_kernel_ir(ir, spec, good, message)
        default_source = chars(source)
        call ok("default emission policy is accepted", good)
        call ok("default policy expands small powers", &
            index(default_source, " ** ") == 0)
        call ok("default policy eliminates the constant division", &
            index(default_source, "5.0000000000000000E-001_real64") > 0)
        call ok("default policy folds exact zero and one elements", &
            index(default_source, "0.0000000000000000E+000_real64") == 0 .and. &
            index(default_source, "1.0000000000000000E+000_real64") == 0)
        call ok("default policy shapes a one-use product", &
            index(default_source, " + (x * y)") > 0)

        open (newunit=unit, file="/tmp/fortsym_policy.f90", status="replace", &
            action="write", iostat=ios)
        call ok("policy fixture opens", ios == 0)
        if (ios == 0) then
            write (unit, "(a)") default_source
            write (unit, "(a)") "program drive_fortsym_policy"
            write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: real64"
            write (unit, "(a)") "  implicit none"
            write (unit, "(a)") "  real(real64) :: r"
            write (unit, "(a)") "  real(real64), parameter :: x = 0.37_real64"
            write (unit, "(a)") "  real(real64), parameter :: y = -0.81_real64"
            write (unit, "(a)") "  real(real64), parameter :: z = 1.23_real64"
            write (unit, "(a)") "  call policy_leaf(x, y, z, r)"
            write (unit, "(a)") "  if (abs(r - (x*x + x*y + z + y/2.0_real64 + x)) > 1.0e-14_real64) error stop 1"
            write (unit, "(a)") "end program drive_fortsym_policy"
            close (unit)
            call execute_command_line( &
                "gfortran -o /tmp/fortsym_policy /tmp/fortsym_policy.f90 "// &
                "> /tmp/fortsym_policy.log 2>&1", wait=.true., exitstat=stat)
            call ok("policy-generated Fortran compiles", stat == 0)
            if (stat == 0) then
                call execute_command_line("/tmp/fortsym_policy", wait=.true., &
                    exitstat=stat)
                call ok("policy-generated Fortran matches independent oracle", &
                    stat == 0)
            end if
        end if

        spec%policy%small_power_limit = 0
        spec%policy%fold_exact_constants = .false.
        spec%policy%eliminate_constant_divisions = .false.
        spec%policy%shape_fma = .false.
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        conservative_source = chars(source)
        call ok("conservative emission policy is accepted", good)
        call ok("conservative policy retains powers", &
            index(conservative_source, " ** ") > 0)
        call ok("conservative policy retains a product temporary", &
            index(conservative_source, " = (x * y)") > 0)
    end subroutine test_emission_policies

    subroutine test_target_driven_emission()
        type(arena_t), target :: arena
        type(expr_t) :: x, root, roots(1)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        type(str_t) :: source
        character(:), allocatable :: default_source, cpu_source
        character(:), allocatable :: openmp_source, openacc_source, both_source
        logical :: good
        character(:), allocatable :: message

        call arena%init()
        x = sym(arena, "x")
        root = x*x + 1
        roots(1) = root
        call lower_kernel_ir(roots, ir, good, message)
        call ok("target test lowers its IR", good)
        if (.not. good) return

        spec%name = str("target_leaf")
        allocate (spec%args(1), spec%outputs(1))
        spec%args(1) = str("x")
        spec%outputs(1) = str("r")

        default_source = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("default target remains a CPU leaf", good)
        call ok("default target emits no directives", &
            index(default_source, "!$omp") == 0 .and. &
            index(default_source, "!$acc") == 0)

        spec%target = TARGET_FORTRAN_CPU
        cpu_source = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("explicit CPU target is accepted", good)
        call ok("explicit CPU target emits no directives", &
            index(cpu_source, "!$omp") == 0 .and. index(cpu_source, "!$acc") == 0)
        call ok("default and explicit CPU output agree", &
            default_source == cpu_source)

        spec%precision = PRECISION_REAL32
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("IR real32 precision is accepted", good)
        call ok("IR real32 emits explicit scalar kinds", &
            index(chars(source), "real(real32), intent(in)") > 0 .and. &
            index(chars(source), "real(real32), intent(out)") > 0 .and. &
            index(chars(source), "_real32") > 0)

        spec%precision = PRECISION_MIXED
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("IR mixed precision is accepted", good)
        call ok("IR mixed precision has a double output boundary", &
            index(chars(source), "real(real32), intent(in)") > 0 .and. &
            index(chars(source), "real(real64), intent(out)") > 0)

        spec%target = TARGET_FORTRAN_OPENMP_TARGET
        openmp_source = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("OpenMP target is accepted", good)
        call ok("OpenMP target emits only OpenMP decoration", &
            index(openmp_source, "!$omp declare target") > 0 .and. &
            index(openmp_source, "!$acc") == 0)

        spec%target = TARGET_FORTRAN_OPENACC
        openacc_source = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("OpenACC target is accepted", good)
        call ok("OpenACC target emits only OpenACC decoration", &
            index(openacc_source, "!$acc routine seq") > 0 .and. &
            index(openacc_source, "!$omp") == 0)

        spec%target = TARGET_FORTRAN_OPENMP_TARGET_AND_OPENACC
        both_source = chars(emit_fortran_kernel_ir(ir, spec, good, message))
        call ok("combined target is accepted", good)
        call ok("combined target emits both historical directives", &
            index(both_source, "!$omp declare target") > 0 .and. &
            index(both_source, "!$acc routine seq") > 0)

        spec%target = TARGET_CUDA
        spec%precision = PRECISION_REAL64
        source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("CUDA target is accepted by the CUDA emitter", good)
        call ok("CUDA target emits no Fortran directives", &
            index(chars(source), "!$omp") == 0 .and. &
            index(chars(source), "!$acc") == 0)
        spec%precision = PRECISION_REAL32
        source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("CUDA real32 precision is accepted", good)
        call ok("CUDA real32 emits float arguments", &
            index(chars(source), "const float x") > 0 .and. &
            index(chars(source), "float* r") > 0)
        spec%precision = PRECISION_MIXED
        source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("CUDA mixed precision has a double output", good .and. &
            index(chars(source), "const float x") > 0 .and. &
            index(chars(source), "double* r") > 0)
        call ok("target identities have stable serialised names", &
            target_name(TARGET_FORTRAN_CPU) == "fortran_cpu" .and. &
            target_name(TARGET_FORTRAN_OPENMP_TARGET) == "fortran_openmp_target" .and. &
            target_name(TARGET_FORTRAN_OPENACC) == "fortran_openacc" .and. &
            target_name(TARGET_CUDA) == "cuda")
    end subroutine test_target_driven_emission

    subroutine test_emit_tables()
        type(arena_t), target :: arena
        type(expr_t) :: values(4), matrix(2, 2)
        type(quadratic_t) :: quadratic(2)
        type(expr_t), allocatable :: parsed(:)
        type(str_t) :: source, matrix_source, quadratic_source
        character(8) :: comments(4)
        character(:), allocatable :: message
        integer :: unit, ios, stat, n
        logical :: good, quadratic_ok

        call arena%init()
        values(1) = rat(arena, 1_int64, 3_int64)
        values(2) = real_expr(arena, -1.25_dp)
        values(3) = num(arena, 4_int64)
        values(4) = rat(arena, -2_int64, 3_int64)
        comments = ["c0      ", "negative", "integer ", "rational"]
        source = emit_table("values", values, "real(dp)", comments, good, message)
        call ok("rank-one table emitter accepts exact values", good)
        call ok("table emitter uses one typed declaration", &
            index(chars(source), "real(dp), parameter :: values(4)") > 0)
        call ok("table emitter preserves comments", &
            index(chars(source), "! negative") > 0)
        call ok("table emitter avoids narrow overflow fields", &
            index(chars(source), "****") == 0)

        open (newunit=unit, file="/tmp/fortsym_table.f90", status="replace", &
            action="write", iostat=ios)
        call ok("table fixture opens", ios == 0)
        if (ios /= 0) return
        write (unit, "(a)") "program drive_fortsym_table"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") chars(source)
        write (unit, "(a)") "  if (abs(values(1) - 1.0_dp/3.0_dp) > 1.0e-15_dp) error stop 1"
        write (unit, "(a)") "  if (abs(values(2) + 1.25_dp) > 1.0e-15_dp) error stop 2"
        write (unit, "(a)") "  if (values(3) /= 4.0_dp) error stop 3"
        write (unit, "(a)") "  if (abs(values(4) + 2.0_dp/3.0_dp) > 1.0e-15_dp) error stop 4"
        write (unit, "(a)") "end program drive_fortsym_table"
        close (unit)
        call execute_command_line( &
            "gfortran -o /tmp/fortsym_table /tmp/fortsym_table.f90 > /tmp/fortsym_table.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("generated rank-one table compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_table", wait=.true., exitstat=stat)
            call ok("generated rank-one table matches independent values", stat == 0)
        end if

        call parse_fortran_array(arena, "/tmp/fortsym_table.f90", "values", &
            parsed, n, good, message)
        call ok("emitted table reads back through fparse", good .and. n == 4)
        if (good) then
            call ok("readback preserves first exact value", parsed(1) == values(1))
            call ok("readback preserves last exact value", parsed(4) == values(4))
        end if

        matrix(1, 1) = num(arena, 1_int64)
        matrix(2, 1) = num(arena, 2_int64)
        matrix(1, 2) = num(arena, 3_int64)
        matrix(2, 2) = num(arena, 4_int64)
        matrix_source = emit_table("matrix", matrix, "real(dp)", ok=good, &
            message=message)
        call ok("rank-two table emitter accepts a matrix", good)
        call ok("rank-two table uses reshape", index(chars(matrix_source), &
            "reshape([") > 0)

        matrix_source = emit_table("matrix", matrix, "real(dp)", ok=good, &
            message=message)
        open (newunit=unit, file="/tmp/fortsym_matrix.f90", status="replace", &
            action="write", iostat=ios)
        call ok("rank-two fixture opens", ios == 0)
        if (ios == 0) then
            write (unit, "(a)") "program drive_fortsym_matrix"
            write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
            write (unit, "(a)") "  implicit none"
            write (unit, "(a)") chars(matrix_source)
            write (unit, "(a)") "  if (matrix(1, 1) /= 1.0_dp) error stop 1"
            write (unit, "(a)") "  if (matrix(2, 2) /= 4.0_dp) error stop 2"
            write (unit, "(a)") "end program drive_fortsym_matrix"
            close (unit)
            call execute_command_line( &
                "gfortran -o /tmp/fortsym_matrix /tmp/fortsym_matrix.f90 > /tmp/fortsym_matrix.log 2>&1", &
                wait=.true., exitstat=stat)
            call ok("generated rank-two table compiles", stat == 0)
            if (stat == 0) then
                call execute_command_line("/tmp/fortsym_matrix", wait=.true., exitstat=stat)
                call ok("generated rank-two table matches independent values", stat == 0)
            end if
        end if

        quadratic(1) = quad("1/2", "1/3", 6, quadratic_ok)
        quadratic(2) = quad("0", "-1", 6, quadratic_ok)
        quadratic_source = emit_table("quadratic", quadratic, "real(dp)", ok=good, &
            message=message)
        call ok("quadratic table emitter accepts exact values", good)
        call ok("quadratic table preserves radicals", &
            index(chars(quadratic_source), "sqrt(6.0d0)") > 0)
        open (newunit=unit, file="/tmp/fortsym_quadratic_table.f90", status="replace", &
            action="write", iostat=ios)
        call ok("quadratic fixture opens", ios == 0)
        if (ios == 0) then
            write (unit, "(a)") "program drive_fortsym_quadratic"
            write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
            write (unit, "(a)") "  implicit none"
            write (unit, "(a)") chars(quadratic_source)
            write (unit, "(a)") "  if (abs(quadratic(1) - (0.5_dp + sqrt(6.0_dp)/3.0_dp)) > 1.0e-15_dp) error stop 1"
            write (unit, "(a)") "  if (abs(quadratic(2) + sqrt(6.0_dp)) > 1.0e-15_dp) error stop 2"
            write (unit, "(a)") "end program drive_fortsym_quadratic"
            close (unit)
            call execute_command_line( &
                "gfortran -o /tmp/fortsym_quadratic_table /tmp/fortsym_quadratic_table.f90 > /tmp/fortsym_quadratic_table.log 2>&1", &
                wait=.true., exitstat=stat)
            call ok("generated quadratic table compiles", stat == 0)
            if (stat == 0) then
                call execute_command_line("/tmp/fortsym_quadratic_table", wait=.true., exitstat=stat)
                call ok("generated quadratic table matches independent values", stat == 0)
            end if
        end if
    end subroutine test_emit_tables

    subroutine test_fortran_symbol_name_boundary()
        type(arena_t), target :: arena
        type(expr_t) :: roots(1)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        type(str_t) :: source
        logical :: good
        character(:), allocatable :: message

        call arena%init()
        roots(1) = sym(arena, "\[Alpha]")
        call lower_kernel_ir(roots, ir, good, message)
        call ok("invalid IR symbol lowers before Fortran validation", good)
        spec%name = str("invalid_ir_name")
        allocate (spec%args(1), spec%outputs(1))
        spec%args(1) = str("x")
        spec%outputs(1) = str("r")
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("IR emitter refuses invalid Wolfram character names", .not. good)
        call ok("IR emitter names the invalid symbol", index(message, "\[Alpha]") > 0)
        call ok("IR emitter returns no invalid source", len(chars(source)) == 0)
        source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("CUDA emitter refuses invalid Wolfram character names", .not. good)
        call ok("CUDA emitter names the invalid symbol", index(message, "\[Alpha]") > 0)

        call arena%clear()
        call arena%init()
        roots(1) = sym(arena, "Gamma") + sym(arena, "gamma")
        call lower_kernel_ir(roots, ir, good, message)
        call ok("case-collision IR lowers", good)
        deallocate (spec%args, spec%outputs)
        spec%name = str("ir_case_collision")
        allocate (spec%args(2), spec%outputs(1))
        spec%args(1) = str("Gamma")
        spec%args(2) = str("gamma")
        spec%outputs(1) = str("r")
        source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("case-collision IR is accepted", good)
        call ok("case-collision IR emits mapping", &
            index(chars(source), "Gamma -> gamma__1") > 0)
    end subroutine test_fortran_symbol_name_boundary

end program test_fortsym_kernel_emit
