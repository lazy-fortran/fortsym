program gen_legendre_recurrence
    ! Generate the integer-degree associated-Legendre recurrence and derivative.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/)
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    use fortsym_string, only: chars, str
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: degree, order, x, previous, current
    type(expr_t) :: roots(2)
    type(kernel_spec_t) :: spec
    character(1024) :: output
    integer :: length, status, unit, ios

    call get_command_argument(1, output, length=length, status=status)
    if (status /= 0 .or. length == 0) then
        write (output_unit, "(a)") &
            "usage: gen_legendre_recurrence OUTPUT_PATH"
        error stop 2
    end if
    output = output(:length)

    call arena%init()
    degree = sym(arena, "degree")
    order = sym(arena, "order")
    x = sym(arena, "x")
    previous = sym(arena, "previous")
    current = sym(arena, "current")

    ! DLMF 14.10.3 solved for P_(degree+1)^order.
    roots(1) = ((2*degree + 1)*x*current - &
        (degree + order)*previous)/(degree - order + 1)
    ! DLMF 14.10.5 solved for d P_degree^order / dx.
    roots(2) = (degree*x*current - &
        (degree + order)*previous)/(x*x - 1)

    spec%name = str("legendre_recurrence_kernel")
    spec%mode = KERNEL_SUBROUTINE
    spec%module_name = str("fortnum_legendre_recurrence_kernel")
    spec%generator = str("gen_legendre_recurrence")
    spec%regenerate_command = str( &
        "fo exec gen_legendre_recurrence -- OUTPUT_PATH")
    spec%pure_procedure = .true.
    allocate (spec%args(5), spec%outputs(2))
    spec%args = [str("degree"), str("order"), str("x"), &
        str("previous"), str("current")]
    spec%outputs = [str("next"), str("derivative")]

    open (newunit=unit, file=trim(output), status="replace", action="write", &
        iostat=ios)
    if (ios /= 0) then
        write (output_unit, "(a)") &
            "gen_legendre_recurrence: cannot write "//trim(output)
        error stop 1
    end if
    write (unit, "(a)") chars(emit_kernel(roots, spec))
    close (unit)
    write (output_unit, "(a)") &
        "gen_legendre_recurrence: wrote "//trim(output)

end program gen_legendre_recurrence
