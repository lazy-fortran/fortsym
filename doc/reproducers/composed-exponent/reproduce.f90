program reproduce_composed_exponent
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(**)
    use fortsym_diff, only: diff_n
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    use fortsym_string, only: str, chars
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: x, roots(2)
    type(kernel_spec_t) :: spec
    character(:), allocatable :: source
    integer :: unit, status
    logical :: valid

    call arena%init()
    x = sym(arena, "x")
    roots(1) = diff_n(x**(num(arena, 2) + num(arena, 1)), x, 4)
    roots(2) = diff_n(x**(num(arena, 1) + num(arena, 0)), x, 2)
    spec%name = str("composed_exponents")
    allocate (spec%args(1), spec%outputs(2))
    spec%args(1) = str("x")
    spec%outputs(1) = str("r1")
    spec%outputs(2) = str("r2")
    spec%openmp_declare_target = .false.
    spec%openacc_routine_seq = .false.
    source = chars(emit_kernel(roots, spec, valid))
    if (.not. valid) error stop "emission refused"
    open (newunit=unit, file="composed_exponents.f90", status="replace")
    write (unit, '(a)') source
    write (unit, '(a)') "program check_composed_exponents"
    write (unit, '(a)') "use, intrinsic :: iso_fortran_env, only: real64"
    write (unit, '(a)') "use, intrinsic :: ieee_arithmetic, only: ieee_is_finite"
    write (unit, '(a)') "implicit none"
    write (unit, '(a)') "real(real64) :: r1,r2"
    write (unit, '(a)') "call composed_exponents(0.0_real64,r1,r2)"
    write (unit, '(a)') "print *, 'expected two finite zeros; observed:',r1,r2"
    write (unit, '(a)') "if (.not. ieee_is_finite(r1)) error stop 1"
    write (unit, '(a)') "if (.not. ieee_is_finite(r2)) error stop 2"
    write (unit, '(a)') "if (abs(r1)+abs(r2)>0.0_real64) error stop 3"
    write (unit, '(a)') "end program check_composed_exponents"
    close (unit)
    call execute_command_line("gfortran -O2 -fno-fast-math -ffp-contract=off "// &
        "composed_exponents.f90 -o composed_exponents", &
        wait=.true., exitstat=status)
    if (status /= 0) error stop "compiled oracle failed to build"
    call execute_command_line("./composed_exponents", wait=.true., exitstat=status)
    if (status /= 0) error stop "composed-exponent limitation reproduced"
end program reproduce_composed_exponent
