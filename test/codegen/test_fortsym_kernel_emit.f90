program test_fortsym_kernel_emit
    ! Both emitters consume one IR instance. The Fortran result is compiled and
    ! run here; when nvcc and a CUDA device are available, the CUDA leaf is
    ! launched through a tiny independent wrapper and checked against the same
    ! host formula.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sin_expr => sin, exp_expr => exp, &
        operator(+), operator(*), operator(/), operator(**)
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit
    use fortsym_proc, only: proc_available
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_emitters_share_ir()

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

    subroutine test_emitters_share_ir()
        type(arena_t), target :: arena
        type(expr_t) :: x, y, shared, root, roots(2)
        type(kernel_ir_t) :: ir
        type(kernel_emit_spec_t) :: spec
        type(str_t) :: fortran_source, cuda_source
        logical :: good
        character(:), allocatable :: message
        integer :: unit, ios, stat

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        shared = x*y
        root = sin_expr(shared) + exp_expr(x)/(1 + y*y) + (x + 1)**2
        roots = [root, shared]
        call lower_kernel_ir(roots, ir, good, message)
        call ok("shared IR lowers for the emitter test", good)
        if (.not. good) return

        spec%name = str("fortsym_ir_leaf")
        spec%args = [str("x"), str("y")]
        spec%outputs = [str("r"), str("s")]
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
        call ok("CUDA emitter maps powers to CUDA math", &
            index(chars(cuda_source), "pow(") > 0)

        spec%args = [str("x"), str("bad-name")]
        fortran_source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("invalid identifiers are refused", .not. good)

        spec%args = [str("x"), str("x")]
        fortran_source = emit_fortran_kernel_ir(ir, spec, good, message)
        call ok("duplicate arguments are refused", .not. good)

        spec%args = [str("x"), str("y")]
        spec%outputs = [str("r"), str("r")]
        cuda_source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("duplicate outputs are refused", .not. good)

        spec%outputs = [str("r"), str("s")]
        spec%args = [str("x"), str("y")]
        cuda_source = emit_cuda_device_ir(ir, spec, good, message)
        call ok("CUDA emitter restores the valid specification", good)

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
        write (unit, "(a)") "__global__ void invoke(const double* x, const double* y, "// &
            "double* r, double* s) {"
        write (unit, "(a)") "  int i = blockIdx.x * blockDim.x + threadIdx.x;"
        write (unit, "(a)") "  if (i == 0) fortsym_ir_leaf(x[0], y[0], &r[0], &s[0]);"
        write (unit, "(a)") "}"
        write (unit, "(a)") "int main() {"
        write (unit, "(a)") "  double hx[1] = {0.37}, hy[1] = {-0.81};"
        write (unit, "(a)") "  double hr[1] = {0.0}, hs[1] = {0.0};"
        write (unit, "(a)") "  double *dx, *dy, *dr, *ds;"
        write (unit, "(a)") "  if (cudaMalloc(&dx, sizeof(hx)) || cudaMalloc(&dy, sizeof(hy)) || "// &
            "cudaMalloc(&dr, sizeof(hr)) || cudaMalloc(&ds, sizeof(hs))) return 10;"
        write (unit, "(a)") "  cudaMemcpy(dx, hx, sizeof(hx), cudaMemcpyHostToDevice);"
        write (unit, "(a)") "  cudaMemcpy(dy, hy, sizeof(hy), cudaMemcpyHostToDevice);"
        write (unit, "(a)") "  invoke<<<1, 1>>>(dx, dy, dr, ds);"
        write (unit, "(a)") "  if (cudaDeviceSynchronize() != cudaSuccess) return 11;"
        write (unit, "(a)") "  cudaMemcpy(hr, dr, sizeof(hr), cudaMemcpyDeviceToHost);"
        write (unit, "(a)") "  cudaMemcpy(hs, ds, sizeof(hs), cudaMemcpyDeviceToHost);"
        write (unit, "(a)") "  double expected = sin(hx[0]*hy[0]) + exp(hx[0])/(1.0 + hy[0]*hy[0]) + pow(hx[0] + 1.0, 2.0);"
        write (unit, "(a)") "  cudaFree(dx); cudaFree(dy); cudaFree(dr); cudaFree(ds);"
        write (unit, "(a)") "  return fabs(hr[0] - expected) > 1e-13 || fabs(hs[0] - hx[0]*hy[0]) > 1e-13;"
        write (unit, "(a)") "}"
        close (unit)
        call execute_command_line( &
            "nvcc -O2 -std=c++17 -o /tmp/fortsym_ir_cuda /tmp/fortsym_ir_cuda.cu "// &
            "> /tmp/fortsym_ir_cuda.log 2>&1", wait=.true., exitstat=stat)
        call ok("generated CUDA compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_ir_cuda", wait=.true., &
                exitstat=stat)
            call ok("generated CUDA agrees with independent oracle", stat == 0)
        end if

    end subroutine test_emitters_share_ir

end program test_fortsym_kernel_emit
