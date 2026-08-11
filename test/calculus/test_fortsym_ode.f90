program test_fortsym_ode
    ! Independent checks for the bounded DSolve slice.
    !
    ! The oracle here is not a stored answer string: each returned function is
    ! substituted into a separately parsed original equation and the residual
    ! is zero-tested. Initial data and unsupported higher-order equations are
    ! checked separately.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC
    use fortsym_expr, only: expr_t, operator(-)
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_subs, only: subs, subs_many
    use fortsym_diff, only: diff
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    type(arena_t), target :: a
    type(native_engine_t) :: engine
    type(wl_session_t) :: session
    type(wl_binding_t) :: binding
    type(expr_t) :: equation, initial_problem, result, solution
    type(expr_t) :: dependent
    logical :: ok, found
    character(:), allocatable :: message

    call a%init()
    engine = make_native_engine(a)

    call run_source(session, a, "answer = DSolve[y'[x] == a*y[x], y, x]"// &
        char(10), result, ok, message)
    call require(ok, "homogeneous DSolve refused: "//message)
    call extract_solution(result, solution, ok)
    call require(ok, "homogeneous result has the wrong rule shape")
    equation = parse_wolfram(a, "y'[x] == a*y[x]", ok, message)
    call require(ok, "could not parse homogeneous oracle equation")
    call require_residual(engine, equation, solution, "homogeneous")
    call require(contains_name(solution, "C"), &
        "homogeneous solution omitted the explicit integration constant")

    call run_source(session, a, &
        "answer = DSolve[y'[x] - 2*y[x] == d*Exp[3*x], y, x]"//char(10), &
        result, ok, message)
    call require(ok, "exponential forcing DSolve refused: "//message)
    call extract_solution(result, solution, ok)
    call require(ok, "forced result has the wrong rule shape")
    equation = parse_wolfram(a, "y'[x] - 2*y[x] == d*Exp[3*x]", ok, message)
    call require(ok, "could not parse forced oracle equation")
    call require_residual(engine, equation, solution, "forced")

    call run_source(session, a, &
        "answer = DSolve[{y'[x] == a*y[x], y[0] == y0}, y, x]"//char(10), &
        result, ok, message)
    call require(ok, "initial-value DSolve refused: "//message)
    call extract_solution(result, solution, ok)
    call require(ok, "initial-value result has the wrong rule shape")
    initial_problem = parse_wolfram(a, "{y'[x] == a*y[x], y[0] == y0}", &
        ok, message)
    call require(ok, "could not parse initial-value oracle problem")
    call require_residual(engine, initial_problem%arg(1), solution, "initial")
    equation = initial_problem%arg(1)
    dependent = find_function(equation%arg(1))
    if (.not. valid_function(dependent)) dependent = find_function(equation%arg(2))
    call require_initial(engine, solution, initial_problem%arg(2), &
        dependent%arg(1))

    call run_source(session, a, "answer = DSolve[y''[x] == y[x], y, x]"// &
        char(10), result, ok, message)
    call require(.not. ok, "unsupported second-order DSolve was accepted")

    print *, "PASS test_fortsym_ode"

contains

    subroutine run_source(s, arena, source, value, ok, message)
        type(wl_session_t), intent(out) :: s
        type(arena_t), target, intent(inout) :: arena
        character(*), intent(in) :: source
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(wl_binding_t) :: item
        integer :: k

        value = expr_t()
        ok = .false.
        message = "missing binding"
        call wl_session_begin(s, arena)
        call wl_run_source(s, source)
        do k = 1, wl_binding_count(s)
            item = wl_binding_at(s, k)
            if (chars(item%name) /= "answer") cycle
            value = item%value
            ok = item%ok
            message = chars(item%message)
            return
        end do
    end subroutine run_source

    function parse_wolfram(arena, source, ok, message) result(value)
        type(arena_t), target, intent(inout) :: arena
        character(*), intent(in) :: source
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(expr_t) :: value

        value = parse_expr_in(arena, source, dialect(DIA_WOLFRAM), ok, message)
    end function parse_wolfram

    subroutine extract_solution(value, solution, ok)
        type(expr_t), intent(in) :: value
        type(expr_t), intent(out) :: solution
        logical, intent(out) :: ok
        type(expr_t) :: outer, rule

        solution = value
        ok = .false.
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
            value%nargs() /= 1) return
        outer = value%arg(1)
        if (outer%kind() /= NK_FUNC .or. chars(outer%name()) /= "List" .or. &
            outer%nargs() /= 1) return
        rule = outer%arg(1)
        if (rule%kind() /= NK_FUNC .or. chars(rule%name()) /= "Rule" .or. &
            rule%nargs() /= 2) return
        solution = rule%arg(2)
        ok = .true.
    end subroutine extract_solution

    subroutine require_residual(engine, equation, solution, label)
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: equation, solution
        character(*), intent(in) :: label
        type(expr_t) :: dependent, derivative, residual, checked, variable
        type(expr_t) :: old_nodes(2), new_nodes(2)
        type(engine_result_t) :: verdict, expanded, simplified

        dependent = equation%arg(1)
        if (dependent%kind() == NK_FUNC .and. &
            chars(dependent%name()) == "Equal") then
            dependent = equation%arg(2)
        end if
        if (dependent%kind() /= NK_FUNC) then
            ! Find y[x] in the equation's left side in the common test forms.
            dependent = equation%arg(1)
            if (dependent%kind() == NK_FUNC .and. &
                chars(dependent%name()) == "Equal") dependent = dependent%arg(1)
        end if
        if (equation%kind() == NK_FUNC .and. chars(equation%name()) == "Equal") then
            dependent = find_function(equation%arg(1))
            if (.not. valid_function(dependent)) then
                dependent = find_function(equation%arg(2))
            end if
            variable = dependent%arg(1)
            derivative = diff(dependent, variable)
            residual = equation%arg(1) - equation%arg(2)
        else
            dependent = find_function(equation%arg(1))
            variable = dependent%arg(1)
            derivative = diff(dependent, variable)
            residual = equation%arg(1) - equation%arg(2)
        end if
        old_nodes(1) = derivative
        old_nodes(2) = dependent
        new_nodes(1) = diff(solution, variable)
        new_nodes(2) = solution
        checked = subs_many(residual, old_nodes, new_nodes)
        expanded = engine%expand(checked)
        if (expanded%ok) checked = expanded%value
        simplified = engine%simplify(checked)
        if (simplified%ok) checked = simplified%value
        verdict = engine%zero_test(checked)
        call require(verdict%ok .and. verdict%verdict == VERDICT_TRUE, &
            label//" solution failed independent residual check")
    end subroutine require_residual

    recursive function find_function(e) result(found)
        type(expr_t), intent(in) :: e
        type(expr_t) :: found
        integer :: k

        found = e
        if (e%kind() == NK_FUNC .and. e%nargs() == 1) return
        do k = 1, e%nargs()
            found = find_function(e%arg(k))
            if (valid_function(found)) return
        end do
        found = e
    end function find_function

    logical function valid_function(e)
        type(expr_t), intent(in) :: e
        valid_function = e%kind() == NK_FUNC .and. e%nargs() == 1 .and. &
            chars(e%name()) /= "Equal"
    end function valid_function

    subroutine require_initial(engine, solution, condition, variable)
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: solution, condition, variable
        type(expr_t) :: point, target, residual, left_side
        type(engine_result_t) :: verdict

        left_side = condition%arg(1)
        point = left_side%arg(1)
        target = condition%arg(2)
        residual = subs(solution, variable, point)
        verdict = engine%zero_test(residual - target)
        call require(verdict%ok .and. verdict%verdict == VERDICT_TRUE, &
            "initial condition failed independent check")
    end subroutine require_initial

    recursive logical function contains_name(e, wanted) result(found)
        type(expr_t), intent(in) :: e
        character(*), intent(in) :: wanted
        integer :: k

        found = .false.
        if (e%kind() == NK_FUNC .and. chars(e%name()) == wanted) then
            found = .true.
            return
        end if
        do k = 1, e%nargs()
            if (contains_name(e%arg(k), wanted)) then
                found = .true.
                return
            end if
        end do
    end function contains_name

    subroutine require(condition, message)
        logical, intent(in) :: condition
        character(*), intent(in) :: message
        if (.not. condition) then
            print *, "FAIL test_fortsym_ode: ", message
            error stop 1
        end if
    end subroutine require

end program test_fortsym_ode
