program fortsym_latex_paper_pilot
    ! Reproduce the scalar left and right sides of paper_magnetic.py Eq. (40).
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), operator(*), &
        operator(**)
    use fortsym_latex, only: latex_t
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: derivative, n, nu22, a1, nu21, a2, j1, lhs
    type(latex_t) :: tex
    character(:), allocatable :: output, message
    logical :: ok
    integer :: output_length

    if (command_argument_count() /= 1) then
        write (error_unit, "(a)") &
            "usage: fortsym_latex_paper_pilot output.tex"
        error stop 2
    end if
    call get_command_argument(1, length=output_length)
    allocate (character(output_length) :: output)
    call get_command_argument(1, output)

    call arena%init()
    derivative = sym(arena, "d2_nu33_curl_a")
    n = sym(arena, "n")
    nu22 = sym(arena, "nu22")
    a1 = sym(arena, "A1")
    nu21 = sym(arena, "nu21")
    a2 = sym(arena, "A2")
    j1 = sym(arena, "J1")
    lhs = derivative + n**2*(nu22*a1 - nu21*a2)

    call tex%source("paper_magnetic.py, Eq. (40)")
    call register("d2_nu33_curl_a", &
        "\partial_{2}\left(\nu_{33}\,\operatorname{curl}_{\mathrm{t}}\boldsymbol{a}\right)")
    call register("n", "n")
    call register("nu22", "\nu_{22}")
    call register("A1", "A_{1}")
    call register("nu21", "\nu_{21}")
    call register("A2", "A_{2}")
    call register("J1", "\mathcal{J}^{1}")
    call tex%eq("AmpereOne", lhs, ok=ok, message=message)
    if (.not. ok) call fail(message)
    call tex%eq("AmpereOneRhs", j1, ok=ok, message=message)
    if (.not. ok) call fail(message)
    call tex%write(output, ok, message)
    if (.not. ok) call fail(message)

contains

    subroutine register(name, value)
        character(*), intent(in) :: name, value
        call tex%name(name, value, ok, message)
        if (.not. ok) call fail(message)
    end subroutine register

    subroutine fail(detail)
        character(*), intent(in) :: detail
        write (error_unit, "(a)") trim(detail)
        error stop 1
    end subroutine fail

end program fortsym_latex_paper_pilot
