module fortsym_ode
    ! A verified, deliberately bounded ordinary differential-equation solver.
    !
    ! The first useful slice is one first-order linear equation in one unknown:
    !
    !     y' = a(x) y + q(x)
    !
    ! The integrating-factor construction proposes the answer, while the
    ! original equation -- including its derivative node -- is substituted
    ! back and zero-tested before anything is returned. Unsupported equations
    ! are refusals, not unevaluated answers that could be mistaken for success.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_ADD, NK_MUL, NK_FUNC, NK_SYM
    use fortsym_expr, only: expr_t, num, func, is_valid, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        exp
    use fortsym_subs, only: subs, subs_many
    use fortsym_diff, only: diff
    use fortsym_integrate, only: integrate
    use fortsym_print, only: print_expr
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t
    implicit none
    private

    public :: solve_ode

contains

    !> Solve one bounded first-order linear ODE.
    !>
    !> problem is the first argument of Wolfram's DSolve: one equation or a
    !> list containing one equation and an optional value condition. unknown
    !> is either y or y[x], and variable must be a symbol.
    function solve_ode(a, engine, problem, unknown, variable, ok, why) result(r)
        type(arena_t), target, intent(inout) :: a
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: problem, unknown, variable
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: r
        type(expr_t) :: dependent, derivative, equation
        type(expr_t) :: initial_point, initial_value
        type(expr_t) :: coefficient, forcing, antiderivative
        type(expr_t) :: homogeneous, integrating_factor
        type(expr_t) :: particular, constant, solution
        type(expr_t) :: checked, at_point, initial_residual
        type(expr_t) :: rule_args(2), constant_args(1), inner_list(1)
        type(expr_t) :: outer_list(1), old_nodes(2), new_nodes(2)
        type(engine_result_t) :: expanded, simplified
        logical :: good, have_equation, have_initial, particular_is_direct
        logical :: decided
        character(:), allocatable :: reason

        ok = .false.
        why = ""
        r = problem

        if (variable%kind() /= NK_SYM) then
            why = "DSolve needs a symbolic independent variable"
            return
        end if
        call dependent_from_spec(a, unknown, variable, dependent, good, reason)
        if (.not. good) then
            why = "DSolve: "//reason
            return
        end if
        derivative = diff(dependent, variable)

        have_equation = .false.
        have_initial = .false.
        initial_point = variable
        initial_value = num(a, 0_int64)
        if (is_list(problem)) then
            call scan_problem_list(problem, derivative, dependent, equation, &
                have_equation, initial_point, initial_value, have_initial, &
                good, reason)
        else
            call scan_one_equation(problem, derivative, equation, &
                have_equation, good, reason)
        end if
        if (.not. good) then
            why = "DSolve: "//reason
            return
        end if
        if (.not. have_equation) then
            why = "DSolve needs one differential equation"
            return
        end if

        call linear_parts(equation%arg(1) - equation%arg(2), derivative, &
            dependent, coefficient, forcing, engine, good, reason)
        if (.not. good) then
            why = "DSolve: "//reason
            return
        end if

        ! y' = a*y + q, after the derivative coefficient has been normalized.
        antiderivative = integrate_or_refuse(a, coefficient, variable, engine, &
            good, reason)
        if (.not. good) then
            why = "DSolve integrating factor: "//reason
            return
        end if
        integrating_factor = exp(-antiderivative)
        homogeneous = exp(antiderivative)
        particular = num(a, 0_int64)
        particular_is_direct = .false.
        if (.not. expression_is_zero(engine, forcing, decided)) then
            call exponential_particular(a, antiderivative, forcing, variable, &
                engine, particular, good, reason)
            if (.not. good) then
                particular = integrate_or_refuse(a, integrating_factor*forcing, &
                    variable, engine, good, reason)
            else
                particular_is_direct = .true.
            end if
            if (.not. good) then
                why = "DSolve particular solution: "//reason
                return
            end if
        end if

        constant_args(1) = num(a, 1_int64)
        constant = func("C", constant_args)
        if (particular_is_direct) then
            solution = homogeneous*constant + particular
        else
            solution = exp(antiderivative)*(constant + particular)
        end if

        if (have_initial) then
            at_point = subs(antiderivative, variable, initial_point)
            at_point = exp(at_point)
            if (particular_is_direct) then
                solution = homogeneous*((initial_value - &
                    subs(particular, variable, initial_point))/at_point) + &
                    particular
            else
                solution = exp(antiderivative)*(initial_value/at_point - &
                    subs(particular, variable, initial_point) + particular)
            end if
        end if

        ! Substitute both nodes simultaneously. Sequential substitution would
        ! let a replacement derivative be traversed by the second substitution
        ! and would no longer check the original equation.
        old_nodes(1) = derivative
        old_nodes(2) = dependent
        new_nodes(1) = diff(solution, variable)
        new_nodes(2) = solution
        checked = subs_many(equation%arg(1) - equation%arg(2), old_nodes, &
            new_nodes)
        expanded = engine%expand(checked)
        if (expanded%ok) checked = expanded%value
        simplified = engine%simplify(checked)
        if (simplified%ok) checked = simplified%value
        if (.not. expression_is_zero(engine, checked, decided)) then
            if (decided) then
                why = "candidate does not satisfy the differential equation"
            else
                why = "differential-equation residual is not decidably zero: "// &
                    chars(print_expr(checked))
            end if
            return
        end if

        if (have_initial) then
            initial_residual = subs(solution, variable, initial_point) - &
                initial_value
            if (.not. expression_is_zero(engine, initial_residual, decided)) then
                why = "initial-condition residual is not decidably zero"
                return
            end if
        end if

        rule_args(1) = dependent
        rule_args(2) = solution
        inner_list(1) = func("Rule", rule_args)
        outer_list(1) = func("List", inner_list)
        r = func("List", outer_list)
        ok = .true.
    end function solve_ode

    subroutine dependent_from_spec(a, specification, variable, dependent, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: specification, variable
        type(expr_t), intent(out) :: dependent
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: args(1)
        type(expr_t) :: argument

        ok = .false.
        why = ""
        dependent = specification
        if (specification%kind() == NK_SYM) then
            args(1) = variable
            dependent = func(chars(specification%name()), args)
            ok = .true.
            return
        end if
        if (specification%kind() /= NK_FUNC .or. specification%nargs() /= 1) then
            why = "unknown function must be y or y[x]"
            return
        end if
        argument = specification%arg(1)
        if (argument%id /= variable%id) then
            why = "unknown function must use the independent variable"
            return
        end if
        dependent = specification
        ok = .true.
    end subroutine dependent_from_spec

    subroutine scan_problem_list(problem, derivative, dependent, equation, &
            have_equation, initial_point, initial_value, have_initial, ok, why)
        type(expr_t), intent(in) :: problem, derivative, dependent
        type(expr_t), intent(out) :: equation, initial_point, initial_value
        logical, intent(out) :: have_equation, have_initial, ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: item_point, item_value
        integer :: k
        logical :: item_ok, item_is_ode, item_has_initial

        have_equation = .false.
        have_initial = .false.
        ok = .true.
        why = ""
        equation = problem
        initial_point = derivative
        initial_value = derivative
        do k = 1, problem%nargs()
            item_is_ode = contains_node(problem%arg(k), derivative)
            if (item_is_ode) then
                if (have_equation) then
                    ok = .false.
                    why = "only one first-order differential equation is supported"
                    return
                end if
                call scan_one_equation(problem%arg(k), derivative, equation, &
                    have_equation, item_ok, why)
            else
                call scan_initial(problem%arg(k), dependent, item_point, &
                    item_value, item_has_initial, item_ok, why)
                if (item_ok .and. item_has_initial) then
                    if (have_initial) then
                        item_ok = .false.
                        why = "only one value initial condition is supported"
                    else
                        initial_point = item_point
                        initial_value = item_value
                        have_initial = .true.
                    end if
                end if
            end if
            if (.not. item_ok) then
                ok = .false.
                return
            end if
        end do
    end subroutine scan_problem_list

    subroutine scan_one_equation(candidate, derivative, equation, have_equation, &
            ok, why)
        type(expr_t), intent(in) :: candidate, derivative
        type(expr_t), intent(out) :: equation
        logical, intent(out) :: have_equation, ok
        character(:), allocatable, intent(out) :: why

        equation = candidate
        have_equation = .false.
        ok = .false.
        why = ""
        if (.not. is_equation(candidate)) then
            why = "DSolve needs equations of the form lhs == rhs"
            return
        end if
        if (.not. contains_node(candidate, derivative)) then
            why = "DSolve needs one differential equation"
            return
        end if
        have_equation = .true.
        ok = .true.
    end subroutine scan_one_equation

    subroutine scan_initial(candidate, dependent, point, value, have_initial, &
            ok, why)
        type(expr_t), intent(in) :: candidate, dependent
        type(expr_t), intent(out) :: point, value
        logical, intent(out) :: have_initial, ok
        character(:), allocatable, intent(out) :: why

        have_initial = .false.
        ok = .false.
        why = ""
        point = candidate
        value = candidate
        if (.not. is_equation(candidate)) then
            why = "non-differential list entries must be initial conditions"
            return
        end if
        if (is_dependent_value(candidate%arg(1), dependent, point)) then
            value = candidate%arg(2)
        else if (is_dependent_value(candidate%arg(2), dependent, point)) then
            value = candidate%arg(1)
        else
            why = "initial condition must have the unknown function on one side"
            return
        end if
        if (contains_node(value, dependent)) then
            why = "initial value cannot depend on the unknown function"
            return
        end if
        have_initial = .true.
        ok = .true.
    end subroutine scan_initial

    subroutine linear_parts(residual, derivative, dependent, coefficient, &
            forcing, engine, ok, why)
        type(expr_t), intent(in) :: residual, derivative, dependent
        type(expr_t), intent(out) :: coefficient, forcing
        type(native_engine_t), intent(inout) :: engine
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: term, term_coefficient
        type(expr_t) :: dcoefficient, ycoefficient
        type(engine_result_t) :: simplified
        logical :: has_derivative, term_ok, decided
        integer :: k, term_kind

        dcoefficient = num(residual%a, 0_int64)
        ycoefficient = num(residual%a, 0_int64)
        forcing = num(residual%a, 0_int64)
        coefficient = num(residual%a, 0_int64)
        ok = .false.
        why = ""
        has_derivative = .false.

        if (residual%kind() == NK_ADD) then
            do k = 1, residual%nargs()
                term = residual%arg(k)
                call classify_term(term, derivative, dependent, term_kind, &
                    term_coefficient, term_ok)
                if (.not. term_ok) then
                    why = "equation is nonlinear in the unknown function"
                    return
                end if
                call collect_term(term_kind, term_coefficient, dcoefficient, &
                    ycoefficient, forcing, has_derivative)
            end do
        else
            call classify_term(residual, derivative, dependent, term_kind, &
                term_coefficient, term_ok)
            if (.not. term_ok) then
                why = "equation is nonlinear in the unknown function"
                return
            end if
            call collect_term(term_kind, term_coefficient, dcoefficient, &
                ycoefficient, forcing, has_derivative)
        end if

        if (.not. has_derivative) then
            why = "equation does not contain a first derivative"
            return
        end if
        simplified = engine%simplify(dcoefficient)
        if (.not. simplified%ok) then
            why = "could not simplify the derivative coefficient"
            return
        end if
        dcoefficient = simplified%value
        simplified = engine%simplify(ycoefficient)
        if (.not. simplified%ok) then
            why = "could not simplify the function coefficient"
            return
        end if
        ycoefficient = simplified%value
        simplified = engine%simplify(forcing)
        if (.not. simplified%ok) then
            why = "could not simplify the forcing term"
            return
        end if
        forcing = simplified%value
        if (expression_is_zero(engine, dcoefficient, decided)) then
            why = "the derivative coefficient is zero"
            return
        end if
        if (.not. decided) then
            why = "the derivative coefficient is not decidably nonzero"
            return
        end if

        ! The residual is d*y' + c*y + rest == 0. Normalize to y' = a*y + q.
        if (expression_is_zero(engine, dcoefficient - &
            num(residual%a, 1_int64), decided)) then
            coefficient = -ycoefficient
            forcing = -forcing
        else
            coefficient = -ycoefficient/dcoefficient
            forcing = -forcing/dcoefficient
        end if
        ok = .true.
    end subroutine linear_parts

    subroutine classify_term(term, derivative, dependent, kind, coefficient, ok)
        type(expr_t), intent(in) :: term, derivative, dependent
        integer, intent(out) :: kind
        type(expr_t), intent(out) :: coefficient
        logical, intent(out) :: ok
        integer :: k, factor_kind
        type(expr_t) :: factor

        ! 0 = forcing term, 1 = derivative term, 2 = function term.
        kind = 0
        coefficient = num(term%a, 1_int64)
        ok = .true.
        if (term%kind() == NK_MUL) then
            do k = 1, term%nargs()
                factor = term%arg(k)
                factor_kind = factor_match(factor, derivative, dependent)
                if (factor_kind < 0) then
                    ok = .false.
                    return
                end if
                if (factor_kind > 0) then
                    if (kind /= 0) then
                        ok = .false.
                        return
                    end if
                    kind = factor_kind
                else
                    coefficient = coefficient*factor
                end if
            end do
        else
            factor_kind = factor_match(term, derivative, dependent)
            if (factor_kind < 0) then
                ok = .false.
                return
            end if
            if (factor_kind > 0) then
                kind = factor_kind
            else
                coefficient = term
            end if
        end if
    end subroutine classify_term

    function factor_match(factor, derivative, dependent) result(kind)
        type(expr_t), intent(in) :: factor, derivative, dependent
        integer :: kind

        kind = 0
        if (factor%id == derivative%id) then
            kind = 1
        else if (factor%id == dependent%id) then
            kind = 2
        else if (contains_node(factor, derivative) .or. &
                contains_node(factor, dependent)) then
            kind = -1
        end if
    end function factor_match

    subroutine collect_term(kind, term_coefficient, dcoefficient, &
            ycoefficient, forcing, has_derivative)
        integer, intent(in) :: kind
        type(expr_t), intent(in) :: term_coefficient
        type(expr_t), intent(inout) :: dcoefficient, ycoefficient, forcing
        logical, intent(inout) :: has_derivative

        if (kind == 1) then
            dcoefficient = dcoefficient + term_coefficient
            has_derivative = .true.
        else if (kind == 2) then
            ycoefficient = ycoefficient + term_coefficient
        else
            forcing = forcing + term_coefficient
        end if
    end subroutine collect_term

    function integrate_or_refuse(a, expression, variable, engine, ok, why) &
            result(result)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: expression, variable
        type(native_engine_t), intent(inout) :: engine
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: result
        logical :: decided

        result = integrate(a, expression, variable, ok, why)
        if (.not. ok) return
        if (.not. expression_is_zero(engine, diff(result, variable) - &
            expression, decided)) then
            ok = .false.
            why = "antiderivative did not pass the residual check"
        end if
    end function integrate_or_refuse

    subroutine exponential_particular(a, antiderivative, forcing, variable, &
            engine, result, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: antiderivative, forcing, variable
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(out) :: result
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: coefficient, exponent, slope, ode_coefficient
        type(expr_t) :: candidate, factor, checked
        type(engine_result_t) :: simplified, expanded
        logical :: found, zero_slope, slope_decided, coefficient_decided
        integer :: k

        result = num(a, 0_int64)
        ok = .false.
        why = "forcing has no supported exponential form: "//chars(print_expr(forcing))
        coefficient = num(a, 1_int64)
        exponent = num(a, 0_int64)
        found = .false.
        if (forcing%kind() == NK_MUL) then
            do k = 1, forcing%nargs()
                factor = forcing%arg(k)
                if (factor%kind() == NK_FUNC .and. &
                    chars(factor%name()) == "exp" .and. factor%nargs() == 1) then
                    if (found) return
                    exponent = factor%arg(1)
                    found = .true.
                else if (contains_node(factor, variable)) then
                    return
                else
                    coefficient = coefficient*factor
                end if
            end do
        else if (forcing%kind() == NK_FUNC .and. &
                chars(forcing%name()) == "exp" .and. forcing%nargs() == 1) then
            exponent = forcing%arg(1)
            found = .true.
        else
            return
        end if
        if (.not. found) return

        ode_coefficient = diff(antiderivative, variable)
        simplified = engine%simplify(ode_coefficient)
        if (.not. simplified%ok) return
        ode_coefficient = simplified%value
        if (.not. expression_is_zero(engine, diff(ode_coefficient, variable), &
            coefficient_decided)) return
        if (.not. coefficient_decided) return
        slope = diff(exponent, variable) - ode_coefficient
        simplified = engine%simplify(slope)
        if (.not. simplified%ok) return
        slope = simplified%value
        zero_slope = expression_is_zero(engine, slope, slope_decided)
        if (.not. slope_decided) then
            why = "exponential slope is undecidable: "//chars(print_expr(slope))
            return
        end if
        if (zero_slope) then
            candidate = coefficient*exp(exponent)*variable
        else
            candidate = coefficient*exp(exponent)/slope
        end if
        checked = diff(candidate, variable) - ode_coefficient*candidate - &
            coefficient*exp(exponent)
        expanded = engine%expand(checked)
        if (expanded%ok) checked = expanded%value
        simplified = engine%simplify(checked)
        if (simplified%ok) checked = simplified%value
        if (.not. expression_is_zero(engine, checked, slope_decided)) then
            why = "exponential particular solution failed verification: "// &
                chars(print_expr(checked))
            return
        end if
        result = candidate
        ok = .true.
        why = ""
    end subroutine exponential_particular

    function expression_is_zero(engine, expression, decided) result(zero)
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: expression
        logical, intent(out) :: decided
        type(engine_result_t) :: check
        logical :: zero

        check = engine%zero_test(expression)
        decided = check%ok .and. (check%verdict == VERDICT_TRUE .or. &
            check%verdict == VERDICT_FALSE)
        zero = decided .and. check%verdict == VERDICT_TRUE
    end function expression_is_zero

    logical function is_equation(e)
        type(expr_t), intent(in) :: e
        is_equation = e%kind() == NK_FUNC .and. chars(e%name()) == "Equal" &
            .and. e%nargs() == 2
    end function is_equation

    logical function is_list(e)
        type(expr_t), intent(in) :: e
        is_list = e%kind() == NK_FUNC .and. chars(e%name()) == "List"
    end function is_list

    logical function is_dependent_value(e, dependent, point)
        type(expr_t), intent(in) :: e, dependent
        type(expr_t), intent(out) :: point
        is_dependent_value = .false.
        point = e
        if (e%kind() /= NK_FUNC .or. e%nargs() /= 1) return
        if (chars(e%name()) /= chars(dependent%name())) return
        point = e%arg(1)
        is_dependent_value = .true.
    end function is_dependent_value

    recursive logical function contains_node(e, target) result(found)
        type(expr_t), intent(in) :: e, target
        integer :: k

        found = .false.
        if (.not. is_valid(e)) return
        if (e%id == target%id) then
            found = .true.
            return
        end if
        do k = 1, e%nargs()
            if (contains_node(e%arg(k), target)) then
                found = .true.
                return
            end if
        end do
    end function contains_node

end module fortsym_ode
