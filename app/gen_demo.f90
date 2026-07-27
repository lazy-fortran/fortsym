program gen_demo
    ! A worked generator, and the reference for writing others.
    !
    ! It exists to make the whole loop real rather than described: derive
    ! symbolically here, emit Fortran into src/generated/, commit that output,
    ! and let CI re-run this program and fail on any difference. That gate is
    ! what stops a generated kernel from outliving its generator -- the failure
    ! mode that left this ecosystem with optimised kernels nobody can reproduce.
    !
    ! Committing the output rather than generating at build time is deliberate:
    ! the emitted code is reviewable in a diff, and a consumer needs no computer
    ! algebra system installed to build against it.
    use, intrinsic :: iso_fortran_env, only: real64, output_unit
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**), sin, cos, exp
    use fortsym_diff, only: diff
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, cse_analyse, &
        cse_result_t, KERNEL_SUBROUTINE
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    implicit none

    integer, parameter :: dp = real64
    character(*), parameter :: OUTPUT = "src/generated/fortsym_demo_kernel.f90"

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    type(expr_t) :: x, y, f, roots(3)
    type(kernel_spec_t) :: spec
    type(cse_result_t) :: cse
    integer :: unit, ios

    call arena%init()
    eng = make_symengine_engine(arena)

    x = sym(arena, "x")
    y = sym(arena, "y")

    ! A function with genuine shared structure, so the generated kernel shows
    ! common subexpression elimination doing something.
    f = sin(x*y)**2 + exp(x*y)/(1 + cos(x*y))

    ! The value and both first derivatives are emitted together. Generating them
    ! as one kernel is the point: they share subexpressions, so computing all
    ! three costs far less than three separate kernels would.
    roots(1) = f
    roots(2) = diff(f, x)
    roots(3) = diff(f, y)

    ! Simplify before emitting. Differentiation deliberately does not simplify
    ! -- d(x*y)/dx comes back as 1*y + x*0 -- because that belongs to the
    ! engines. A generator is where it has to happen: without this the emitted
    ! kernel carries every 0 + and *1 the chain rule produced, and no compiler
    ! is obliged to fold them.
    call simplify_all(roots)

    spec%name = str("fortsym_demo_kernel")
    spec%mode = KERNEL_SUBROUTINE
    spec%temp_prefix = str("t")
    spec%generator = str("gen_demo")
    allocate (spec%args(2), spec%outputs(3))
    spec%args(1) = str("x")
    spec%args(2) = str("y")
    spec%outputs(1) = str("f")
    spec%outputs(2) = str("dfdx")
    spec%outputs(3) = str("dfdy")

    cse = cse_analyse(roots, "t")

    open (newunit=unit, file=OUTPUT, status="replace", action="write", &
        iostat=ios)
    if (ios /= 0) then
        write (output_unit, "(a)") "gen_demo: cannot write "//OUTPUT
        error stop 1
    end if
    write (unit, "(a)") chars(emit_kernel(roots, spec))
    close (unit)

    write (output_unit, "(a)") "gen_demo: wrote "//OUTPUT
    write (output_unit, "(a)") "  shared subexpressions named: "// &
        chars(str(cse%n))
    write (output_unit, "(a)") "  nodes, value only:  "// &
        chars(str(roots(1)%node_count()))
    write (output_unit, "(a)") "  nodes, all three:   "// &
        chars(str(total_nodes(roots)))

contains

    !> Replace each expression by the engine's simplified form, keeping the
    !> original whenever the engine declines. A generator must never emit
    !> something it could not verify came back.
    subroutine simplify_all(r)
        type(expr_t), intent(inout) :: r(:)
        type(engine_result_t) :: res
        integer :: k
        do k = 1, size(r)
            res = eng%simplify(r(k))
            if (res%ok) r(k) = res%value
        end do
    end subroutine simplify_all

    !> Distinct nodes across all outputs. Because the arena is hash-consed, a
    !> subexpression shared between the value and its derivatives is counted
    !> once -- so this measures what the kernel actually computes rather than
    !> what the three expressions look like separately.
    function total_nodes(r) result(n)
        type(expr_t), intent(in) :: r(:)
        integer                  :: n
        type(expr_t) :: combined
        integer :: k
        combined = r(1)
        do k = 2, size(r)
            combined = combined + r(k)
        end do
        n = combined%node_count()
    end function total_nodes

end program gen_demo
