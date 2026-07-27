program test_fortsym_yacas
    ! The Yacas backend, checked on what it is actually for.
    !
    ! Yacas earns its place by covering what SymEngine cannot: symbolic
    ! integration, limits, solve, factorization and multivariate rational
    ! cancellation. So the checks here are complementarity checks, and the
    ! oracle is differentiation -- an integral is verified by differentiating it
    ! back, which is independent of whatever route Yacas took to find it.
    !
    ! The suite skips cleanly when Yacas is unavailable. It is fetched and built
    ! by the CMake path but not by the fpm path, and a test that failed for want
    ! of an optional engine would make the whole build depend on it.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(-), operator(*), operator(+), &
        operator(/), operator(**), sin, cos
    use fortsym_diff, only: diff
    use fortsym_parse, only: parse_expr
    use fortsym_print, only: print_expr
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, &
        has_cap, CAP_INTEGRATE, CAP_ZERO_TEST
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_engine_yacas, only: yacas_engine_t, make_yacas_engine
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: se
    type(yacas_engine_t)     :: yc
    integer :: nfail = 0

    call arena%init()
    se = make_symengine_engine(arena)
    yc = make_yacas_engine(arena)

    if (.not. yc%available) then
        print *, "test_fortsym_yacas: SKIP (Yacas not available in this build)"
        stop 0
    end if

    print *, "yacas available"

    call test_capabilities_are_honest()
    call test_integration()
    call test_multivariate_cancellation()
    call test_shutdown_is_safe()

    call yc%shutdown()

    if (nfail == 0) then
        print *, "test_fortsym_yacas: all checks passed"
    else
        print *, "test_fortsym_yacas: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        else
            print *, "  ok  ", label
        end if
    end subroutine ok

    function parsed(text) result(e)
        character(*), intent(in) :: text
        type(expr_t)             :: e
        character(:), allocatable :: message
        logical :: good
        e = parse_expr(arena, text, good, message)
        if (.not. good) then
            nfail = nfail + 1
            print *, "PARSE-FAIL ", text, " : ", message
        end if
    end function parsed

    function is_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(engine_result_t) :: r
        r = se%zero_test(e)
        yes = r%verdict == VERDICT_TRUE
    end function is_zero

    !> An engine must declare only what it can do. Yacas cannot close
    !> trigonometric identities, so claiming CAP_ZERO_TEST would draw work it
    !> answers worse than the engine that already handles it.
    subroutine test_capabilities_are_honest()
        call ok("declares integration", has_cap(yc, CAP_INTEGRATE))
        call ok("does not claim zero testing", .not. has_cap(yc, CAP_ZERO_TEST))
        call ok("symengine does claim zero testing", has_cap(se, CAP_ZERO_TEST))
    end subroutine test_capabilities_are_honest

    !> Integration, verified by differentiating the result back.
    !>
    !> That is a genuinely independent oracle: fortsym's differentiation is
    !> native and knows nothing about how Yacas found the antiderivative.
    subroutine test_integration()
        type(expr_t) :: x, f, back
        type(engine_result_t) :: r

        x = sym(arena, "x")

        ! A polynomial, then something needing integration by parts.
        f = x**2
        r = yc%integrate(f, x)
        call ok("integrates x**2", r%ok)
        if (r%ok) then
            back = diff(r%value, x) - f
            call ok("d/dx of the antiderivative recovers x**2", is_zero(back))
        end if

        f = x*sin(x)
        r = yc%integrate(f, x)
        call ok("integrates x*sin(x)", r%ok)
        if (r%ok) then
            print *, "      antiderivative: ", chars(print_expr(r%value))
            back = diff(r%value, x) - f
            call ok("d/dx recovers x*sin(x)", is_zero(back))
        end if

        ! SymEngine cannot do this at all, which is the point of having Yacas.
        call ok("symengine has no integration", .not. has_cap(se, CAP_INTEGRATE))
    end subroutine test_integration

    !> Multivariate rational cancellation: SymEngine's FLINT path is univariate
    !> only, so (x**2 - y**2)/(x - y) stays uncancelled there.
    subroutine test_multivariate_cancellation()
        type(expr_t) :: e, want
        type(engine_result_t) :: r

        e = parsed("(x**2 - y**2)/(x - y)")
        want = parsed("x + y")

        r = yc%simplify(e)
        call ok("simplifies a multivariate quotient", r%ok)
        if (r%ok) then
            print *, "      simplified: ", chars(print_expr(r%value))
            call ok("result equals x + y", is_zero(r%value - want))
            ! And it is genuinely smaller, which is what the tournament ranks on.
            call ok("result is smaller than the input", &
                r%value%node_count() < e%node_count())
        end if
    end subroutine test_multivariate_cancellation

    !> Shutting down twice must be safe. An engine is released when a council is
    !> torn down, and a double free would be a crash rather than a test failure.
    subroutine test_shutdown_is_safe()
        type(yacas_engine_t) :: tmp
        tmp = make_yacas_engine(arena)
        call tmp%shutdown()
        call tmp%shutdown()
        call ok("double shutdown is safe", .not. tmp%available)
    end subroutine test_shutdown_is_safe

end program test_fortsym_yacas
