program gen_toroidal_kernels
    ! Generate hypergeometric-series and associated-order recurrence kernels.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), sqrt
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    use fortsym_string, only: chars, str
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: a, b, c, k, z, term
    type(expr_t) :: degree, order, x, current, next_order
    type(expr_t) :: root(1)
    type(kernel_spec_t) :: spec
    character(1024) :: output_directory, revision
    integer :: length, revision_length, status

    call get_command_argument(1, output_directory, length=length, status=status)
    if (status /= 0 .or. length == 0) call usage()
    output_directory = output_directory(:length)
    call get_command_argument(2, revision, length=revision_length, status=status)
    if (status /= 0 .or. revision_length == 0) call usage()
    revision = revision(:revision_length)

    call arena%init()
    a = sym(arena, "a")
    b = sym(arena, "b")
    c = sym(arena, "c")
    k = sym(arena, "k")
    z = sym(arena, "z")
    term = sym(arena, "term")
    root(1) = term*(a + k)*(b + k)*z/((c + k)*(k + 1))

    spec = base_spec("hypergeom_2f1_term_kernel", &
        "fortnum_hypergeom_2f1_term_kernel")
    allocate (spec%args(6), spec%outputs(1))
    spec%args(1) = str("a")
    spec%args(2) = str("b")
    spec%args(3) = str("c")
    spec%args(4) = str("k")
    spec%args(5) = str("z")
    spec%args(6) = str("term")
    spec%outputs(1) = str("next_term")
    call write_kernel("fortnum_hypergeom_2f1_term_kernel.f90", root, spec)

    degree = sym(arena, "degree")
    order = sym(arena, "order")
    x = sym(arena, "x")
    current = sym(arena, "current")
    next_order = sym(arena, "next_order")
    root(1) = -2*(order + 1)*x*next_order/sqrt(x*x - 1) + &
        (degree - order)*(degree + order + 1)*current

    spec = base_spec("toroidal_order_kernel", &
        "fortnum_toroidal_order_kernel")
    allocate (spec%args(5), spec%outputs(1))
    spec%args(1) = str("degree")
    spec%args(2) = str("order")
    spec%args(3) = str("x")
    spec%args(4) = str("current")
    spec%args(5) = str("next_order")
    spec%outputs(1) = str("following_order")
    call write_kernel("fortnum_toroidal_order_kernel.f90", root, spec)

contains

    function base_spec(name, module_name) result(result_spec)
        character(*), intent(in) :: name, module_name
        type(kernel_spec_t) :: result_spec

        result_spec%name = str(name)
        result_spec%mode = KERNEL_SUBROUTINE
        result_spec%module_name = str(module_name)
        result_spec%generator = str("gen_toroidal_kernels")
        result_spec%generator_revision = str(trim(revision))
        result_spec%regenerate_command = str( &
            "fo exec gen_toroidal_kernels OUTPUT_DIR FORTSYM_REVISION")
        result_spec%pure_procedure = .true.
    end function base_spec

    subroutine write_kernel(filename, roots, kernel_spec)
        character(*), intent(in) :: filename
        type(expr_t), intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: kernel_spec
        character(2048) :: path
        integer :: unit, ios

        path = trim(output_directory)//"/"//filename
        open (newunit=unit, file=trim(path), status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            write (output_unit, "(a)") &
                "gen_toroidal_kernels: cannot write "//trim(path)
            error stop 1
        end if
        write (unit, "(a)") chars(emit_kernel(roots, kernel_spec))
        close (unit)
        write (output_unit, "(a)") &
            "gen_toroidal_kernels: wrote "//trim(path)
    end subroutine write_kernel

    subroutine usage()
        write (output_unit, "(a)") &
            "usage: gen_toroidal_kernels OUTPUT_DIR FORTSYM_REVISION"
        error stop 2
    end subroutine usage

end program gen_toroidal_kernels
