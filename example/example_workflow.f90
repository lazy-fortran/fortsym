program example_workflow
    ! A tour of fortsym in one file: build an expression, differentiate it,
    ! prove an identity about it, ask several engines to agree, and emit a
    ! kernel.
    !
    ! Run it with:  fo exec example_workflow
    use, intrinsic :: iso_fortran_env, only: real64, output_unit
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**), sin, cos, exp
    use fortsym_parse, only: parse_expr
    use fortsym_print, only: print_expr
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t, verdict_name
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_engine_ext, only: make_maxima_engine, make_sympy_engine
    use fortsym_council
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_zero
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    use fortsym_products, only: gradient, hvp
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    type(council_t)          :: council
    type(suite_t)            :: s

    type(expr_t) :: x, y, f, dfdx
    type(expr_t) :: vars(2), dirs(2), g(2), h(2)
    type(expr_t) :: roots(3)
    type(kernel_spec_t) :: spec
    type(council_decision_t) :: d
    character(:), allocatable :: msg
    logical :: ok

    call arena%init()
    eng = make_symengine_engine(arena)

    ! ---------------------------------------------------------------- 1 ----
    ! Build expressions. Operators are overloaded, so this reads as maths.
    x = sym(arena, "x")
    y = sym(arena, "y")
    f = sin(x*y)**2 + exp(x*y)/(1 + cos(x*y))

    call say("f(x,y)      = "//chars(print_expr(f)))

    ! Or parse from text, in fortsym's notation or any engine's dialect.
    f = parse_expr(arena, "sin(x*y)**2 + exp(x*y)/(1 + cos(x*y))", ok, msg)
    call say("parsed back = "//chars(print_expr(f)))

    ! ---------------------------------------------------------------- 2 ----
    ! Differentiate. Native, so a chart with 27 metric derivatives costs no
    ! engine round-trips. Deliberately unsimplified -- tidying is the engines'
    ! job, and hash-consing keeps the redundant pieces to one node each.
    dfdx = diff(f, x)
    call say("")
    call say("df/dx (raw)      nodes = "//chars(str(dfdx%node_count())))
    call say("df/dx (simplified) = "//chars(print_expr(simplified(dfdx))))

    ! ---------------------------------------------------------------- 3 ----
    ! Prove identities. A suite prints PASS/FAIL, exports JSON and sets the
    ! exit status, so it drops straight into a build.
    call say("")
    call suite_begin(s, "example identities")
    call check_zero(s, eng, "pythagorean", sin(x)**2 + cos(x)**2 - 1)
    call check_zero(s, eng, "angle sum", &
        sin(x + y) - sin(x)*cos(y) - cos(x)*sin(y))
    ! ---------------------------------------------------------------- 4 ----
    ! Ask several engines. Whichever are installed take part; the rest are
    ! skipped. Disagreement would be reported as a finding.
    call council_add(council, eng)
    call council_add(council, make_maxima_engine(arena))
    call council_add(council, make_sympy_engine(arena))

    d = council_zero_test(council, &
        parse_expr(arena, "(x**2 - 1)/(x - 1) - (x + 1)", &
        ok, msg), "rational cancellation")
    call say("")
    call say("(x**2-1)/(x-1) - (x+1)   -> "//chars(verdict_name(d%verdict)))
    call show_votes(d)

    ! Not an identity: it holds only for x >= 0. fortsym must refuse to claim
    ! it, and that refusal is the property worth demonstrating.
    d = council_zero_test(council, sqrt_x_minus_x(x), "sqrt(x**2) - x")
    call say("")
    call say("sqrt(x**2) - x           -> "//chars(verdict_name(d%verdict))// &
        "   (correct: only true for x >= 0)")
    call show_votes(d)

    ! ---------------------------------------------------------------- 5 ----
    ! Derivative products, built contracted -- no Jacobian, no Hessian.
    vars(1) = x; vars(2) = y
    dirs(1) = sym(arena, "wx"); dirs(2) = sym(arena, "wy")
    g = gradient(f, vars)
    h = hvp(f, vars, dirs)
    call say("")
    call say("gradient and Hessian-vector product built without forming either")
    call say("   grad nodes = "//chars(str(g(1)%node_count()))// &
        ",  H*w nodes = "//chars(str(h(1)%node_count())))

    ! ---------------------------------------------------------------- 6 ----
    ! Emit a kernel: value and both derivatives together, so they share work.
    roots(1) = simplified(f)
    roots(2) = simplified(diff(f, x))
    roots(3) = simplified(diff(f, y))

    spec%name = str("example_kernel")
    spec%mode = KERNEL_SUBROUTINE
    spec%temp_prefix = str("t")
    spec%generator = str("example_workflow")
    allocate (spec%args(2), spec%outputs(3))
    spec%args(1) = str("x"); spec%args(2) = str("y")
    spec%outputs(1) = str("f")
    spec%outputs(2) = str("dfdx")
    spec%outputs(3) = str("dfdy")

    call say("")
    call say("generated kernel:")
    call say("")
    write (output_unit, "(a)") chars(emit_kernel(roots, spec))

    call say(chars(council_benchmark_table(council)))

    call council_shutdown(council)
    call suite_end(s)

contains

    subroutine say(text)
        character(*), intent(in) :: text
        write (output_unit, "(a)") text
    end subroutine say

    !> Ask the engine for a tidier form, keeping the original if it declines.
    function simplified(e) result(r)
        type(expr_t), intent(in) :: e
        type(expr_t)             :: r
        type(engine_result_t) :: res
        res = eng%simplify(e)
        r = e
        if (res%ok) r = res%value
    end function simplified

    function sqrt_x_minus_x(v) result(e)
        type(expr_t), intent(in) :: v
        type(expr_t)             :: e
        character(:), allocatable :: m
        logical :: good
        e = parse_expr(arena, "sqrt(x**2) - x", good, m)
    end function sqrt_x_minus_x

    subroutine show_votes(dd)
        type(council_decision_t), intent(in) :: dd
        integer :: k
        do k = 1, dd%n_votes
            call say("   "//chars(dd%votes(k)%engine)//" -> "// &
                chars(verdict_name(dd%votes(k)%verdict)))
        end do
    end subroutine show_votes

end program example_workflow
