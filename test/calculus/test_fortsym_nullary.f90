program test_fortsym_nullary
    use fortsym_arena, only: arena_t, NK_INT
    use fortsym_expr, only: expr_t, sym, num, func, func_in, &
        operator(*), operator(==)
    use fortsym_diff, only: diff, diff_n
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_products, only: jvp, vjp, hvp, gradient, jacobian_matrix, &
        directional_derivative
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: x, c, one(1), value, forward(1), reverse(1), matrix(1, 1)
    type(native_engine_t) :: eng
    type(engine_result_t) :: simplified
    character(16) :: mode
    integer :: nfail = 0, ncheck = 0

    call arena%init()
    eng = make_native_engine(arena)
    x = sym(arena, "x")
    one = [num(arena, 1)]
    ! Leave an unrelated live argument after the empty application's slot.
    c = func_in(arena, "c")
    value = func("unrelated", [x])
    call get_command_argument(1, mode)

    if (mode == "" .or. mode == "diff") then
        call zero("diff c()", diff(c, x))
        call zero("higher diff c()", diff_n(c, x, 3))
        forward = gradient(c, [x])
        call zero("gradient c()", forward(1))
        matrix = jacobian_matrix([c], [x])
        call zero("Jacobian c()", matrix(1, 1))
        simplified = eng%simplify(directional_derivative(c, [x], one))
        call check("directional simplification succeeds", simplified%ok)
        call zero("directional c()", simplified%value)
        forward = hvp(c, [x], one)
        call zero("Hessian product c()", forward(1))
        call check("diff preserves unknown unary derivative", &
            diff(func("c", [x]), x) == &
            func("Derivative1", [sym(arena, "c"), num(arena, 1), x]))
    end if
    if (mode == "" .or. mode == "jvp") then
        forward = jvp([c], [x], one)
        call zero("JVP c()", forward(1))
        forward = jvp([c*x], [x], one)
        call check("JVP c()*x", forward(1) == one(1)*c)
        forward = jvp([func("c", [x])], [x], one)
        value = func("Derivative_c", [x])*one(1)
        call check("JVP preserves unknown unary derivative", forward(1) == value)
    end if
    if (mode == "" .or. mode == "vjp") then
        reverse = vjp([c], [x], one)
        call zero("VJP c()", reverse(1))
        reverse = vjp([c*x], [x], one)
        call check("VJP c()*x", reverse(1) == one(1)*c)
        reverse = vjp([func("c", [x])], [x], one)
        value = one(1)*(func("Derivative_c", [x])*one(1))
        call check("VJP preserves unknown unary derivative", reverse(1) == value)
    end if
    print *, "nullary checks:", ncheck - nfail, "passed;", nfail, "failed"
    if (nfail /= 0) error stop 1

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        ncheck = ncheck + 1
        if (condition) return
        nfail = nfail + 1
        print *, "FAIL ", label
    end subroutine check

    subroutine zero(label, expression)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        logical :: is_zero
        is_zero = .false.
        if (expression%kind() == NK_INT) is_zero = expression%int_value() == 0
        call check(label, is_zero)
    end subroutine zero

end program test_fortsym_nullary
