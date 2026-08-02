module fortsym_wl_solve
    ! Solve[...] for the Wolfram subset.
    !
    ! Three routes, tried in this order and never blended:
    !
    !   * one equation, one variable, polynomial in that variable ->
    !     fortsym_polysolve, which returns *every* root exactly and has already
    !     substituted each one back into the untouched coefficient vector.
    !   * one equation, one variable, not a polynomial -> the engine's scalar
    !     solver, which handles symbolic coefficients (x + a == 0) and verifies
    !     its candidate with a zero test.
    !   * n equations, n variables, linear with rational coefficients ->
    !     fortsym_linalg's exact elimination, with the linearity itself proved
    !     by a zero test rather than assumed from the syntax.
    !   * a bounded 2x2 linear system with symbolic coefficients ->
    !     successive scalar elimination with the same reconstruction proof.
    !
    ! Everything else refuses. In particular a transcendental equation is never
    ! answered: Solve[Cos[x] == 0, x] has infinitely many roots and any finite
    ! list fortsym could print would be a different statement than the one the
    ! oracle makes.
    !
    ! The result shape is Wolfram's list of rule lists, {{x -> r1}, {x -> r2}}.
    ! That is not cosmetic: a bare root where the oracle returns a rule list is
    ! a disagreement, and downstream code indexing [[1, 1, 2]] needs the nesting
    ! to be real.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM, NK_INT, NK_RAT
    use fortsym_expr, only: expr_t, sym, num, func, func_in, &
        operator(+), operator(-), operator(*), operator(/)
    use fortsym_engine, only: engine_t, engine_result_t, VERDICT_TRUE, wall_seconds
    use fortsym_polysolve, only: solve_polynomial
    use fortsym_linalg, only: solve_exact_linear_system, &
        exact_linear_system_result_t
    use fortsym_subs, only: subs
    use fortsym_diff, only: diff
    use fortsym_eval, only: free_symbols_of
    implicit none
    private

    public :: wl_solve

    integer, parameter :: dp = real64

    ! Elimination is cubic in the number of unknowns and the entries are exact,
    ! so an unbounded system is an unbounded amount of work driven by input.
    integer, parameter :: MAX_UNKNOWNS = 12

contains

    !> Evaluate a Solve application. `e` is the whole Solve[...] node with its
    !> arguments already evaluated. On refusal `why` names what stopped it.
    function wl_solve(a, engine, e, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        class(engine_t),           intent(inout) :: engine
        type(expr_t),              intent(in)    :: e
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: eqns(:), vars(:)
        integer :: k

        r = e
        ok = .false.
        why = ""

        if (e%nargs() < 1) then
            why = "Solve needs an equation"
            return
        end if
        if (e%nargs() > 2) then
            ! Solve[eqns, vars, domain] restricts the solution set, and
            ! ignoring the third argument would answer a different question.
            why = "Solve with a domain or elimination specification"
            return
        end if

        call operand_list(e%arg(1), eqns)
        if (e%nargs() == 2) then
            call operand_list(e%arg(2), vars)
        else
            call inferred_variable(a, e%arg(1), vars, ok, why)
            if (.not. ok) return
            ok = .false.
        end if

        if (size(eqns) < 1 .or. size(vars) < 1) then
            why = "Solve needs at least one equation and one variable"
            return
        end if
        do k = 1, size(vars)
            if (vars(k)%kind() /= NK_SYM) then
                why = "Solve needs plain symbols as the unknowns"
                return
            end if
        end do

        if (size(eqns) == 1 .and. size(vars) == 1) then
            r = solve_scalar(a, engine, eqns(1), vars(1), ok, why)
            return
        end if
        if (size(eqns) /= size(vars)) then
            why = "Solve with a non-square system is not implemented"
            return
        end if
        if (size(vars) > MAX_UNKNOWNS) then
            why = "Solve with more unknowns than the exact solver bound"
            return
        end if
        r = solve_linear_system(a, engine, eqns, vars, ok, why)
    end function wl_solve

    ! ------------------------------------------------------------- scalar --

    !> One equation in one unknown: exact polynomial roots first, the engine's
    !> scalar solver second.
    function solve_scalar(a, engine, eqn, var, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        class(engine_t),           intent(inout) :: engine
        type(expr_t),              intent(in)    :: eqn, var
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(expr_t), allocatable :: roots(:), rules(:)
        type(expr_t)          :: resid
        type(engine_result_t) :: res
        character(:), allocatable :: poly_why
        logical :: good
        integer :: k

        r = eqn
        call residual(eqn, resid, good)
        if (.not. good) then
            ok = .false.
            why = "Solve needs an equation of the form lhs == rhs"
            return
        end if

        call solve_polynomial(a, resid, var, roots, ok, poly_why)
        if (ok) then
            ! Every root here was already substituted back into the original
            ! coefficients by the solver; an unverified one never reaches this
            ! point.
            allocate (rules(size(roots)))
            do k = 1, size(roots)
                rules(k) = single_rule(var, roots(k))
            end do
            r = rule_set(a, rules)
            why = ""
            return
        end if

        ! Not a polynomial in the unknown, or of a degree with no exact root
        ! formula here. The engine's scalar solver still covers the linear case
        ! with symbolic coefficients, and refuses rather than guesses otherwise.
        res = engine%solve(resid, var)
        if (.not. res%ok) then
            ok = .false.
            why = poly_why//"; scalar solver: "//chars(res%message)
            return
        end if
        allocate (rules(1))
        rules(1) = single_rule(var, res%value)
        r = rule_set(a, rules)
        ok = .true.
        why = ""
    end function solve_scalar

    ! ------------------------------------------------------------- systems --

    !> n linear equations in n unknowns, with a bounded symbolic 2x2 fallback.
    !>
    !> Linearity is proved, not assumed: the extracted coefficients are used to
    !> rebuild the equation and the difference must zero-test true. A quadratic
    !> term would otherwise be silently dropped by the derivative extraction and
    !> the wrong root reported as the answer.
    function solve_linear_system(a, engine, eqns, vars, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        class(engine_t),           intent(inout) :: engine
        type(expr_t),              intent(in)    :: eqns(:), vars(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: r
        type(exact_linear_system_result_t) :: sol
        type(expr_t), allocatable :: m(:, :), rhs(:, :), rules(:)
        type(expr_t) :: resid, konst, coef, recon, probe
        type(engine_result_t) :: simplified, verdict
        integer :: n, i, j
        logical :: all_exact, good
        character(8) :: tag

        r = eqns(1)
        ok = .false.
        why = ""
        n = size(vars)
        all_exact = .true.
        allocate (m(n, n))
        allocate (rhs(n, 1))

        do i = 1, n
            call residual(eqns(i), resid, good)
            if (.not. good) then
                why = "Solve needs every entry to be an equation lhs == rhs"
                return
            end if

            konst = resid
            do j = 1, n
                konst = subs(konst, vars(j), num(a, 0))
            end do
            simplified = engine%simplify(konst)
            if (.not. simplified%ok) then
                why = "Solve: "//chars(simplified%message)
                return
            end if
            konst = simplified%value
            if (.not. is_exact_rational(konst)) then
                all_exact = .false.
            end if
            rhs(i, 1) = num(a, 0) - konst

            recon = konst
            do j = 1, n
                simplified = engine%simplify(diff(resid, vars(j)))
                if (.not. simplified%ok) then
                    why = "Solve: "//chars(simplified%message)
                    return
                end if
                coef = simplified%value
                if (.not. is_exact_rational(coef)) then
                    all_exact = .false.
                end if
                m(i, j) = coef
                recon = recon + coef*vars(j)
            end do

            ! Expanded before the zero test: the native simplifier does not
            ! distribute a leading minus over a sum, so a genuinely zero
            ! difference tests UNKNOWN and a correct linear system would be
            ! refused as nonlinear.
            verdict = zero_after_expansion(engine, resid - recon)
            if (verdict%verdict /= VERDICT_TRUE) then
                write (tag, "(i0)") i
                why = "equation "//trim(tag)// &
                    " is not linear in the unknowns"
                return
            end if
        end do

        if (.not. all_exact .and. n == 2) then
            r = solve_symbolic_two_by_two(engine, eqns, vars, ok, why)
            if (ok) return
            return
        end if
        if (.not. all_exact) then
            write (tag, "(i0)") 1
            why = "equation "//trim(tag)// &
                " has symbolic coefficients outside the bounded 2x2 solver"
            return
        end if

        sol = solve_exact_linear_system(engine, m, rhs)
        if (.not. sol%ok) then
            why = "Solve: "//chars(sol%message)
            return
        end if

        ! Independent of how elimination produced them, the values must
        ! annihilate every original equation.
        do i = 1, n
            call residual(eqns(i), resid, good)
            probe = resid
            do j = 1, n
                probe = subs(probe, vars(j), sol%values(j, 1))
            end do
            verdict = zero_after_expansion(engine, probe)
            if (verdict%verdict /= VERDICT_TRUE) then
                write (tag, "(i0)") i
                why = "the computed solution could not be verified in "// &
                    "equation "//trim(tag)
                return
            end if
        end do

        allocate (rules(n))
        do j = 1, n
            rules(j) = single_rule(vars(j), sol%values(j, 1))
        end do
        ! One solution, so one inner rule list holding every unknown.
        r = func("List", [func("List", rules)])
        ok = .true.
    end function solve_linear_system

    !> Solve a bounded symbolic 2x2 system by successive scalar elimination.
    !> Each elimination proves its linear reconstruction before division. This
    !> intentionally does not generalise to arbitrary symbolic Gaussian
    !> elimination: a conditional pivot or a rapidly growing expression must
    !> remain visible as a refusal.
    function solve_symbolic_two_by_two(engine, eqns, vars, ok, why) result(r)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: eqns(:), vars(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: r
        type(expr_t) :: first, second
        logical :: good

        r = eqns(1)
        ok = .false.
        why = ""
        if (size(eqns) /= 2 .or. size(vars) /= 2) then
            why = "symbolic Solve needs a 2x2 system"
            return
        end if
        call residual(eqns(1), first, good)
        if (.not. good) then
            why = "Solve needs every entry to be an equation lhs == rhs"
            return
        end if
        call residual(eqns(2), second, good)
        if (.not. good) then
            why = "Solve needs every entry to be an equation lhs == rhs"
            return
        end if

        r = solve_symbolic_order(engine, first, second, vars(1), vars(2), ok, why)
        if (ok) return
        r = solve_symbolic_order(engine, second, first, vars(1), vars(2), ok, why)
    end function solve_symbolic_two_by_two

    function solve_symbolic_order( &
            engine, first, second, x, y, ok, why) result(r)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: first, second, x, y
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: r
        type(expr_t) :: x_value, y_value, reduced, rules(2)
        type(engine_result_t) :: x_solution, y_solution

        r = first
        ok = .false.
        why = ""
        x_solution = solve_symbolic_linear_equation(engine, first, x)
        if (.not. x_solution%ok) then
            why = "symbolic Solve first elimination: "// &
                chars(x_solution%message)
            return
        end if
        reduced = subs(second, x, x_solution%value)
        y_solution = solve_symbolic_linear_equation(engine, reduced, y)
        if (.not. y_solution%ok) then
            why = "symbolic Solve second elimination: "// &
                chars(y_solution%message)
            return
        end if
        y_value = y_solution%value
        x_value = subs(x_solution%value, y, y_value)

        rules(1) = single_rule(x, x_value)
        rules(2) = single_rule(y, y_value)
        r = func("List", [func("List", rules)])
        ok = .true.
    end function solve_symbolic_order

    function solve_symbolic_linear_equation(engine, equation, variable) result(r)
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: equation, variable
        type(engine_result_t) :: r
        type(engine_result_t) :: coefficient, constant, linearity, candidate
        type(engine_result_t) :: coefficient_zero
        type(expr_t) :: derivative
        real(dp) :: started

        started = wall_seconds()
        r%value = equation
        derivative = diff(equation, variable)
        coefficient = engine%simplify(derivative)
        if (.not. coefficient%ok) then
            r = coefficient
            r%seconds = wall_seconds() - started
            return
        end if
        linearity = zero_after_expansion( &
            engine, diff(coefficient%value, variable))
        if (linearity%verdict /= VERDICT_TRUE) then
            r%message = str("symbolic equation is not linear in the variable")
            r%seconds = wall_seconds() - started
            return
        end if
        coefficient_zero = engine%zero_test(coefficient%value)
        if (coefficient_zero%verdict == VERDICT_TRUE) then
            r%message = str("symbolic linear coefficient is zero")
            r%seconds = wall_seconds() - started
            return
        end if

        constant = engine%simplify( &
            subs(equation, variable, variable - variable))
        if (.not. constant%ok) then
            r = constant
            r%seconds = wall_seconds() - started
            return
        end if
        linearity = zero_after_expansion( &
            engine, equation - (constant%value + coefficient%value*variable))
        if (linearity%verdict /= VERDICT_TRUE) then
            r%message = str("symbolic linear reconstruction could not be verified")
            r%seconds = wall_seconds() - started
            return
        end if
        candidate = engine%simplify(-constant%value/coefficient%value)
        if (.not. candidate%ok) then
            r = candidate
            r%seconds = wall_seconds() - started
            return
        end if
        r = candidate
        r%ok = .true.
        r%seconds = wall_seconds() - started
    end function solve_symbolic_linear_equation

    ! ------------------------------------------------------------- helpers --

    !> lhs == rhs becomes lhs - rhs; a bare expression is already a residual,
    !> which is how Wolfram reads Solve[expr, x] too.
    subroutine residual(eqn, resid, good)
        type(expr_t), intent(in)  :: eqn
        type(expr_t), intent(out) :: resid
        logical,      intent(out) :: good

        good = .true.
        resid = eqn
        if (eqn%kind() /= NK_FUNC) return
        if (chars(eqn%name()) /= "Equal") return
        if (eqn%nargs() /= 2) then
            ! a == b == c is a chained statement, not one equation.
            good = .false.
            return
        end if
        resid = eqn%arg(1) - eqn%arg(2)
    end subroutine residual

    !> Flatten a List argument into its elements; anything else is a single
    !> element.
    subroutine operand_list(e, items)
        type(expr_t),              intent(in)  :: e
        type(expr_t), allocatable, intent(out) :: items(:)
        integer :: k

        if (e%kind() == NK_FUNC) then
            if (chars(e%name()) == "List") then
                allocate (items(e%nargs()))
                do k = 1, e%nargs()
                    items(k) = e%arg(k)
                end do
                return
            end if
        end if
        allocate (items(1))
        items(1) = e
    end subroutine operand_list

    !> Solve[eq] with no variable named. Wolfram solves for the free symbols it
    !> finds; fortsym only accepts the unambiguous case of exactly one, because
    !> picking one of several would answer a question nobody asked.
    subroutine inferred_variable(a, e, vars, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t), allocatable, intent(out)   :: vars(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(str_t), allocatable :: names(:)

        ok = .false.
        why = ""
        allocate (vars(0))
        names = free_symbols_of(e)
        if (size(names) /= 1) then
            why = "Solve without a variable needs exactly one free symbol"
            return
        end if
        deallocate (vars)
        allocate (vars(1))
        vars(1) = sym(a, chars(names(1)))
        ok = .true.
    end subroutine inferred_variable

    !> Zero test after expansion. Expansion can only fail to prove a zero, it
    !> can never turn a nonzero difference into a zero one, so nothing is
    !> loosened here -- an UNKNOWN still refuses.
    function zero_after_expansion(engine, e) result(verdict)
        class(engine_t), intent(inout) :: engine
        type(expr_t),    intent(in)    :: e
        type(engine_result_t)          :: verdict
        type(engine_result_t) :: expanded

        expanded = engine%expand(e)
        if (.not. expanded%ok) then
            verdict = engine%zero_test(e)
            return
        end if
        verdict = engine%zero_test(expanded%value)
    end function zero_after_expansion

    function single_rule(var, value) result(r)
        type(expr_t), intent(in) :: var, value
        type(expr_t)             :: r
        r = func("Rule", [var, value])
    end function single_rule

    !> {{x -> r1}, {x -> r2}}: one outer element per solution.
    function rule_set(a, rules) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: rules(:)
        type(expr_t)                         :: r
        type(expr_t), allocatable :: outer(:)
        integer :: k

        if (size(rules) == 0) then
            ! No solutions is a real answer and Wolfram writes it {}.
            r = func_in(a, "List")
            return
        end if
        allocate (outer(size(rules)))
        do k = 1, size(rules)
            outer(k) = func("List", [rules(k)])
        end do
        r = func("List", outer)
    end function rule_set

    !> An exact rational literal. Reals are excluded on purpose: the exact
    !> solver's guarantees are arithmetic guarantees, and feeding a rounded
    !> double in would return an exact-looking answer to an inexact question.
    function is_exact_rational(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        yes = e%kind() == NK_INT .or. e%kind() == NK_RAT
    end function is_exact_rational

end module fortsym_wl_solve
