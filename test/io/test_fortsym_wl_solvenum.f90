program test_fortsym_wl_solvenum
    ! Solve, N, Chop, IdentityMatrix, Cross, Tr, Range and DiagonalMatrix in
    ! the Wolfram subset.
    !
    ! Nothing here compares against a string this program's author copied out
    ! of fortsym's own output. Each check is a property the implementation
    ! cannot satisfy by accident:
    !
    !   * a root is substituted back into the equation -- reparsed from source
    !     text, not taken from the solver -- and the residual must zero-test.
    !   * the result *shape* is walked structurally: List of List of Rule, with
    !     the rule's left side the unknown that was asked for. A bare root
    !     fails here even when its value is right.
    !   * N is checked against decimal digits of pi that are a fact about pi,
    !     not about fortsym.
    !   * Cross is checked by perpendicularity to both of its symbolic
    !     operands, which fixes all three components up to a scale no wrong
    !     formula survives together with the unit-vector case.
    !   * Tr is checked by Tr[A.B] == Tr[B.A], which no diagonal-picking bug
    !     satisfies for a generic pair.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_INT, NK_RAT, NK_REAL
    use fortsym_expr, only: expr_t, sym, num, operator(-), operator(*), &
        operator(+)
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_parse, only: parse_expr_in
    use fortsym_subs, only: subs
    use fortsym_assume, only: assumption_context_t
    use fortsym_complexdom, only: complex_expand
    use fortsym_numeric, only: numeric_value
    use fortsym_print, only: print_expr
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_solve_roots_verify()
    call test_solve_shape_is_rule_lists()
    call test_solve_linear_system()
    call test_linear_solve()
    call test_solve_refuses_what_it_cannot_verify()
    call test_n_precision()
    call test_chop()
    call test_identity_cross_trace()
    call test_array_collections()
    call test_flatten()
    call test_range_and_diagonal_matrix()

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_solvenum"
    else
        print *, "FAIL test_fortsym_wl_solvenum:", nfail
        error stop 1
    end if

contains

    !> Run a script in `a` and return the value bound to `name`.
    subroutine run_one(a, source, name, value, ok, message)
        type(arena_t), target,     intent(inout) :: a
        character(*),              intent(in)    :: source, name
        type(expr_t),              intent(out)   :: value
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: message
        type(wl_session_t) :: s
        type(wl_binding_t) :: b
        integer :: k

        ok = .false.
        message = "no binding named "//name
        call wl_session_begin(s, a)
        call wl_run_source(s, source)
        do k = 1, wl_binding_count(s)
            b = wl_binding_at(s, k)
            if (chars(b%name) /= name) cycle
            value = b%value
            ok = b%ok
            message = chars(b%message)
            return
        end do
    end subroutine run_one

    subroutine fail(label, detail)
        character(*), intent(in) :: label, detail
        print *, "FAIL ", label, ": ", detail
        nfail = nfail + 1
    end subroutine fail

    !> True when `e` is zero, by any route this test can run independently of
    !> the code under test.
    !>
    !> Three routes because no single one covers the root shapes Solve
    !> produces: the native simplifier proves rational zeros, ComplexExpand
    !> proves the ones written with the imaginary unit, and a floating-point
    !> evaluation covers surds, which fortsym's exact simplifier does not fold.
    !> The numeric route is a genuinely separate arithmetic and is the reason a
    !> surd root cannot pass by construction.
    function proves_zero(a, e) result(yes)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        logical                              :: yes
        type(native_engine_t) :: engine
        type(engine_result_t) :: expanded, verdict
        type(assumption_context_t) :: facts
        type(expr_t) :: split
        real(dp) :: value
        logical :: ok
        character(:), allocatable :: why

        engine = make_native_engine(a)
        expanded = engine%expand(e)
        if (expanded%ok) then
            verdict = engine%zero_test(expanded%value)
        else
            verdict = engine%zero_test(e)
        end if
        yes = verdict%verdict == VERDICT_TRUE
        if (yes) return

        call complex_expand(e, facts, split, ok, why)
        if (ok) then
            verdict = engine%zero_test(split)
            yes = verdict%verdict == VERDICT_TRUE
            if (yes) return
        end if

        call numeric_value(e, value, ok, why)
        if (ok) yes = abs(value) < 1.0e-10_dp
    end function proves_zero

    !> Structural check on {{v -> r1}, {v -> r2}, ...}, returning the roots.
    subroutine rule_list_roots(label, value, var_name, roots, good)
        character(*),              intent(in)  :: label, var_name
        type(expr_t),              intent(in)  :: value
        type(expr_t), allocatable, intent(out) :: roots(:)
        logical,                   intent(out) :: good
        type(expr_t) :: inner, rule, lhs
        integer :: k

        good = .false.
        allocate (roots(0))
        if (value%kind() /= NK_FUNC) then
            call fail(label, "result is not a List")
            return
        end if
        if (chars(value%name()) /= "List") then
            call fail(label, "result head is "//chars(value%name()))
            return
        end if

        deallocate (roots)
        allocate (roots(value%nargs()))
        do k = 1, value%nargs()
            inner = value%arg(k)
            if (inner%kind() /= NK_FUNC) then
                call fail(label, "solution is not a List of rules")
                return
            end if
            if (chars(inner%name()) /= "List") then
                call fail(label, "solution head is "//chars(inner%name()))
                return
            end if
            if (inner%nargs() /= 1) then
                call fail(label, "one unknown expected in the solution")
                return
            end if
            rule = inner%arg(1)
            if (rule%kind() /= NK_FUNC) then
                call fail(label, "solution entry is not a Rule")
                return
            end if
            if (chars(rule%name()) /= "Rule") then
                call fail(label, "solution entry head is "//chars(rule%name()))
                return
            end if
            if (rule%nargs() /= 2) then
                call fail(label, "Rule with the wrong arity")
                return
            end if
            lhs = rule%arg(1)
            if (chars(lhs%name()) /= var_name) then
                call fail(label, "rule solves for "//chars(lhs%name()))
                return
            end if
            roots(k) = rule%arg(2)
        end do
        good = .true.
    end subroutine rule_list_roots

    !> Every returned root, substituted into the equation as reparsed from its
    !> own source text, must annihilate it -- and there must be as many roots
    !> as the degree, so a solver that drops one fails too.
    subroutine check_roots(label, script, name, residual_text, var_name, count)
        character(*), intent(in) :: label, script, name, residual_text
        character(*), intent(in) :: var_name
        integer,      intent(in) :: count
        type(arena_t), target :: a
        type(expr_t) :: value, resid, probe, v
        type(expr_t), allocatable :: roots(:)
        logical :: ok, good
        character(:), allocatable :: message
        integer :: k

        call a%init()
        call run_one(a, script, name, value, ok, message)
        if (.not. ok) then
            call fail(label, "refused: "//message)
            return
        end if
        call rule_list_roots(label, value, var_name, roots, good)
        if (.not. good) return
        if (size(roots) /= count) then
            call fail(label, "wrong number of roots")
            return
        end if

        resid = parse_expr_in(a, residual_text, dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            call fail(label, "oracle text did not parse: "//message)
            return
        end if
        v = sym(a, var_name)
        do k = 1, size(roots)
            probe = subs(resid, v, roots(k))
            if (.not. proves_zero(a, probe)) then
                call fail(label, "root "//chars(print_expr(roots(k)))// &
                          " does not satisfy the equation")
            end if
        end do
    end subroutine check_roots

    subroutine test_solve_roots_verify()
        ! Two distinct rational roots.
        call check_roots("quadratic", "q = Solve[x^2 - 5*x + 6 == 0, x]"//nl(), &
                         "q", "x^2 - 5*x + 6", "x", 2)
        ! lhs == rhs rather than == 0: the residual is formed, not assumed.
        call check_roots("moved rhs", "q = Solve[x^2 == 9, x]"//nl(), &
                         "q", "x^2 - 9", "x", 2)
        ! A repeated root is listed with its multiplicity, as Wolfram does.
        call check_roots("double root", "q = Solve[x^2 - 4*x + 4 == 0, x]"//nl(), &
                         "q", "x^2 - 4*x + 4", "x", 2)
        ! Quartic with two real and two imaginary roots, and no variable named:
        ! the single free symbol is inferred.
        call check_roots("quartic", "q = Solve[x^4 - 1 == 0]"//nl(), &
                         "q", "x^4 - 1", "x", 4)
        ! Irrational roots: exact surds, still verified by substitution.
        call check_roots("surd", "q = Solve[x^2 - 2 == 0, x]"//nl(), &
                         "q", "x^2 - 2", "x", 2)
        ! Symbolic coefficient, handled by the scalar solver rather than the
        ! polynomial one, and it must come back in the same shape.
        call check_roots("symbolic", "q = Solve[x + c0 == 0, x]"//nl(), &
                         "q", "x + c0", "x", 1)
    end subroutine test_solve_roots_verify

    !> A bare root is the failure mode this guards: the value would be right
    !> and the answer still wrong.
    subroutine test_solve_shape_is_rule_lists()
        type(arena_t), target :: a
        type(expr_t) :: value
        type(expr_t), allocatable :: roots(:)
        logical :: ok, good
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "q = Solve[2*x - 6 == 0, x]"//nl(), "q", value, ok, message)
        if (.not. ok) then
            call fail("shape", "refused: "//message)
            return
        end if
        call rule_list_roots("shape", value, "x", roots, good)
        if (.not. good) return
        if (size(roots) /= 1) then
            call fail("shape", "expected one root")
            return
        end if
        ! 3 is a fact about 2x = 6, not about the solver.
        if (roots(1)%kind() /= NK_INT) then
            call fail("shape", "root is not an integer")
            return
        end if
        if (roots(1)%int_value() /= 3) call fail("shape", "root is not 3")
    end subroutine test_solve_shape_is_rule_lists

    !> The system's solution is substituted back into both equations.
    subroutine test_solve_linear_system()
        type(arena_t), target :: a
        type(expr_t) :: value, inner, rule, probe, e1, e2, lhs
        type(expr_t) :: xs, ys
        logical :: ok
        character(:), allocatable :: message
        integer :: k

        call a%init()
        call run_one(a, "q = Solve[{5*x + y == 3, 4*x - 3*y == 2}, {x, y}]"//nl(), &
                     "q", value, ok, message)
        if (.not. ok) then
            call fail("system", "refused: "//message)
            return
        end if
        if (chars(value%name()) /= "List" .or. value%nargs() /= 1) then
            call fail("system", "expected one solution in a List")
            return
        end if
        inner = value%arg(1)
        if (chars(inner%name()) /= "List" .or. inner%nargs() /= 2) then
            call fail("system", "expected two rules in the solution")
            return
        end if
        xs = num(a, 0)
        ys = num(a, 0)
        do k = 1, 2
            rule = inner%arg(k)
            if (chars(rule%name()) /= "Rule") then
                call fail("system", "solution entry is not a Rule")
                return
            end if
            lhs = rule%arg(1)
            if (chars(lhs%name()) == "x") xs = rule%arg(2)
            if (chars(lhs%name()) == "y") ys = rule%arg(2)
        end do

        e1 = parse_expr_in(a, "5*x + y - 3", dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            call fail("system", "oracle text did not parse")
            return
        end if
        e2 = parse_expr_in(a, "4*x - 3*y - 2", dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            call fail("system", "oracle text did not parse")
            return
        end if
        probe = subs(subs(e1, sym(a, "x"), xs), sym(a, "y"), ys)
        if (.not. proves_zero(a, probe)) &
            call fail("system", "first equation not satisfied")
        probe = subs(subs(e2, sym(a, "x"), xs), sym(a, "y"), ys)
        if (.not. proves_zero(a, probe)) &
            call fail("system", "second equation not satisfied")
    end subroutine test_solve_linear_system

    !> LinearSolve returns the unique exact solution and preserves the shape of
    !> a matrix right-hand side. The values below follow from ordinary
    !> elimination on paper, independently of fortsym's linalg routine.
    subroutine test_linear_solve()
        type(arena_t), target :: a
        type(expr_t) :: value, row, item
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "v = LinearSolve[{{2, 1}, {1, -1}}, {5, 1}]"//nl(), &
                     "v", value, ok, message)
        if (.not. ok) then
            call fail("LinearSolve vector", "refused: "//message)
            return
        end if
        if (chars(value%name()) /= "List" .or. value%nargs() /= 2) then
            call fail("LinearSolve vector", "expected {2, 1}")
        else
            item = value%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 2) &
                call fail("LinearSolve vector", "wrong first value")
            item = value%arg(2)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("LinearSolve vector", "wrong second value")
        end if

        call run_one(a, "m = LinearSolve[{{2, 1}, {1, -1}}, "// &
                     "{{5, 3}, {1, 0}}]"//nl(), "m", value, ok, message)
        if (.not. ok) then
            call fail("LinearSolve matrix RHS", "refused: "//message)
            return
        end if
        if (chars(value%name()) /= "List" .or. value%nargs() /= 2) then
            call fail("LinearSolve matrix RHS", "expected a 2x2 result")
            return
        end if
        row = value%arg(1)
        if (chars(row%name()) /= "List" .or. row%nargs() /= 2) then
            call fail("LinearSolve matrix RHS", "wrong first result row")
        else
            item = row%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 2) &
                call fail("LinearSolve matrix RHS", "wrong first row value")
            item = row%arg(2)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("LinearSolve matrix RHS", "wrong second row value")
        end if
        row = value%arg(2)
        if (chars(row%name()) /= "List" .or. row%nargs() /= 2) then
            call fail("LinearSolve matrix RHS", "wrong second result row")
        else
            item = row%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("LinearSolve matrix RHS", "wrong first value in row")
            item = row%arg(2)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("LinearSolve matrix RHS", "wrong second value in row")
        end if

        call expect_refusal("singular LinearSolve", &
                            "v = LinearSolve[{{1, 2}, {2, 4}}, {1, 2}]"//nl(), "v")
    end subroutine test_linear_solve

    !> Refusals, each naming a case where a printed answer would be a claim
    !> fortsym cannot back.
    subroutine test_solve_refuses_what_it_cannot_verify()
        call expect_refusal("transcendental", &
                            "q = Solve[Cos[x] == 0, x]"//nl(), "q")
        call expect_refusal("quintic", &
                            "q = Solve[x^5 - x - 1 == 0, x]"//nl(), "q")
        call expect_refusal("nonlinear system", &
                            "q = Solve[{x*y == 1, x + y == 3}, {x, y}]"//nl(), "q")
        ! Two free symbols and no variable named: which one is meant is not
        ! decidable, and picking one would answer a different question.
        call expect_refusal("ambiguous unknown", &
                            "q = Solve[x + y == 1]"//nl(), "q")
    end subroutine test_solve_refuses_what_it_cannot_verify

    subroutine expect_refusal(label, script, name)
        character(*), intent(in) :: label, script, name
        type(arena_t), target :: a
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, script, name, value, ok, message)
        if (ok) call fail(label, "answered "//chars(print_expr(value))// &
                          " instead of refusing")
    end subroutine expect_refusal

    !> The digits of pi are the oracle. 3.14159 and 3.14 are facts about pi.
    subroutine test_n_precision()
        call expect_real("N six digits", "v = N[Pi, 6]"//nl(), "v", &
                         3.14159_dp, 1.0e-13_dp)
        call expect_real("N three digits", "v = N[Pi, 3]"//nl(), "v", &
                         3.14_dp, 1.0e-13_dp)
        ! Rounding, not truncation: to 4 digits pi is 3.142, not 3.141.
        call expect_real("N rounds", "v = N[Pi, 4]"//nl(), "v", &
                         3.142_dp, 1.0e-13_dp)
        ! Full double precision when no digit count is asked for.
        call expect_real("N plain", "v = N[Pi]"//nl(), "v", &
                         3.141592653589793_dp, 1.0e-15_dp)
        ! Above what real64 carries, N refuses rather than pads.
        call expect_refusal("N thirty digits", "v = N[Pi, 30]"//nl(), "v")
        call expect_refusal("N seventeen digits", "v = N[Pi, 17]"//nl(), "v")
        ! Elementwise over a list.
        call expect_list_real("N over a list", "v = N[{Pi, 2*Pi}, 5]"//nl(), &
                              "v", [3.1416_dp, 6.2832_dp], 1.0e-12_dp)
    end subroutine test_n_precision

    subroutine expect_real(label, script, name, expected, tol)
        character(*), intent(in) :: label, script, name
        real(dp),     intent(in) :: expected, tol
        type(arena_t), target :: a
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, script, name, value, ok, message)
        if (.not. ok) then
            call fail(label, "refused: "//message)
            return
        end if
        if (value%kind() /= NK_REAL) then
            call fail(label, "result is not a real number")
            return
        end if
        if (abs(value%real_value() - expected) > tol) &
            call fail(label, "value disagrees with the independent decimal")
    end subroutine expect_real

    subroutine expect_list_real(label, script, name, expected, tol)
        character(*), intent(in) :: label, script, name
        real(dp),     intent(in) :: expected(:), tol
        type(arena_t), target :: a
        type(expr_t) :: value, item
        logical :: ok
        character(:), allocatable :: message
        integer :: k

        call a%init()
        call run_one(a, script, name, value, ok, message)
        if (.not. ok) then
            call fail(label, "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC) then
            call fail(label, "result is not a List")
            return
        end if
        if (chars(value%name()) /= "List" .or. value%nargs() /= size(expected)) then
            call fail(label, "result is not a List of the right length")
            return
        end if
        do k = 1, size(expected)
            item = value%arg(k)
            if (item%kind() /= NK_REAL) then
                call fail(label, "element is not a real number")
                cycle
            end if
            if (abs(item%real_value() - expected(k)) > tol) &
                call fail(label, "element disagrees with the decimal")
        end do
    end subroutine expect_list_real

    !> A value below the threshold becomes exact zero; one above it survives.
    subroutine test_chop()
        type(arena_t), target :: a
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "v = Chop[N[10^(-15)]]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("chop small", "refused: "//message)
        else if (value%kind() /= NK_INT) then
            call fail("chop small", "negligible value was not chopped to zero")
        else if (value%int_value() /= 0) then
            call fail("chop small", "chopped value is not zero")
        end if

        ! 1e-6 is above the documented 1e-10 threshold and must survive: a
        ! Chop that zeroes everything would pass the first check alone.
        call expect_real("chop keeps", "v = Chop[N[10^(-6)]]"//nl(), "v", &
                         1.0e-6_dp, 1.0e-18_dp)
        ! An exact rational is not a floating-point artefact and is not chopped.
        call a%init()
        call run_one(a, "v = Chop[1/1000000000000000]"//nl(), "v", value, ok, &
                     message)
        if (.not. ok) then
            call fail("chop exact", "refused: "//message)
        else if (value%kind() == NK_INT) then
            call fail("chop exact", "an exact rational was chopped away")
        end if
    end subroutine test_chop

    subroutine test_identity_cross_trace()
        type(arena_t), target :: a
        type(expr_t) :: value, cross, dot1, dot2, x, y
        type(expr_t) :: t1, t2, item
        logical :: ok
        character(:), allocatable :: message
        integer :: k

        ! IdentityMatrix through Det and Tr, both of which existed before.
        call a%init()
        call run_one(a, "v = Det[IdentityMatrix[4]]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("identity det", "refused: "//message)
        else if (value%kind() /= NK_INT) then
            call fail("identity det", "determinant is not an integer")
        else if (value%int_value() /= 1) then
            call fail("identity det", "determinant of the identity is not 1")
        end if

        call a%init()
        call run_one(a, "v = Tr[IdentityMatrix[5]]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("identity tr", "refused: "//message)
        else if (value%kind() /= NK_INT) then
            call fail("identity tr", "trace is not an integer")
        else if (value%int_value() /= 5) then
            call fail("identity tr", "trace of the 5x5 identity is not 5")
        end if

        ! Cross: perpendicular to both operands, for symbolic components.
        call a%init()
        call run_one(a, "v = Cross[{a1, a2, a3}, {b1, b2, b3}]"//nl(), "v", &
                     value, ok, message)
        if (.not. ok) then
            call fail("cross", "refused: "//message)
            return
        end if
        if (chars(value%name()) /= "List" .or. value%nargs() /= 3) then
            call fail("cross", "result is not a three-component vector")
            return
        end if
        cross = value
        dot1 = num(a, 0)
        dot2 = num(a, 0)
        do k = 1, 3
            x = sym(a, "a"//achar(48 + k))
            y = sym(a, "b"//achar(48 + k))
            dot1 = dot1 + cross%arg(k)*x
            dot2 = dot2 + cross%arg(k)*y
        end do
        if (.not. proves_zero(a, dot1)) &
            call fail("cross", "result is not perpendicular to the first operand")
        if (.not. proves_zero(a, dot2)) &
            call fail("cross", "result is not perpendicular to the second operand")

        ! A perpendicular vector is only fixed up to sign and scale by the two
        ! dot products, so one concrete case pins the orientation.
        call a%init()
        call run_one(a, "v = Cross[{1, 0, 0}, {0, 1, 0}]"//nl(), "v", value, ok, &
                     message)
        if (.not. ok) then
            call fail("cross orientation", "refused: "//message)
        else
            do k = 1, 3
                item = value%arg(k)
                if (item%kind() /= NK_INT) then
                    call fail("cross orientation", "component is not an integer")
                    cycle
                end if
                if (k < 3 .and. item%int_value() /= 0) &
                    call fail("cross orientation", "e1 x e2 has a nonzero "// &
                              "component off the third axis")
                if (k == 3 .and. item%int_value() /= 1) &
                    call fail("cross orientation", "e1 x e2 is not e3")
            end do
        end if

        ! Tr[A.B] == Tr[B.A] for a generic pair: a trace that read the wrong
        ! diagonal, or summed the whole matrix, would not satisfy it.
        call a%init()
        call run_one(a, "v = Tr[Dot[{{1, 2}, {3, 5}}, {{7, 11}, {13, 17}}]]"//nl(), &
                     "v", t1, ok, message)
        if (.not. ok) then
            call fail("trace cyclic", "refused: "//message)
            return
        end if
        call run_one(a, "v = Tr[Dot[{{7, 11}, {13, 17}}, {{1, 2}, {3, 5}}]]"//nl(), &
                     "v", t2, ok, message)
        if (.not. ok) then
            call fail("trace cyclic", "refused: "//message)
            return
        end if
        if (t1%id /= t2%id) call fail("trace cyclic", "Tr[A.B] /= Tr[B.A]")
        ! And the value itself: 1*7+2*13 + 3*11+5*17 = 33 + 118 = 151, by hand.
        if (t1%kind() /= NK_INT) then
            call fail("trace value", "trace is not an integer")
        else if (t1%int_value() /= 151) then
            call fail("trace value", "trace disagrees with the hand calculation")
        end if
    end subroutine test_identity_cross_trace

    subroutine test_array_collections()
        type(arena_t), target :: a
        type(expr_t) :: value, item, row
        logical :: ok
        character(:), allocatable :: message
        integer :: i, j
        integer, parameter :: expected_outer(2, 2) = reshape([3, 6, 4, 8], [2, 2])

        ! Array applies a named definition to one-based indices. The expected
        ! squares are computed here independently of the evaluator's expansion.
        call a%init()
        call run_one(a, "g[i_] := i^2"//nl()//"v = Array[g, 3]"//nl(), "v", &
                     value, ok, message)
        if (.not. ok) then
            call fail("array", "refused: "//message)
        else if (chars(value%name()) /= "List" .or. value%nargs() /= 3) then
            call fail("array", "result is not a three-element list")
        else
            do i = 1, 3
                item = value%arg(i)
                if (item%kind() /= NK_INT .or. item%int_value() /= int(i*i)) &
                    call fail("array", "generated index does not produce its square")
            end do
        end if

        ! ConstantArray preserves the requested nested shape and value.
        call a%init()
        call run_one(a, "v = ConstantArray[7, {2, 2}]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("constant array", "refused: "//message)
        else if (chars(value%name()) /= "List" .or. value%nargs() /= 2) then
            call fail("constant array", "result is not a 2x2 list")
        else
            do i = 1, 2
                row = value%arg(i)
                if (row%kind() /= NK_FUNC .or. &
                    chars(row%name()) /= "List" .or. row%nargs() /= 2) then
                    call fail("constant array", "nested shape is not 2x2")
                    cycle
                end if
                do j = 1, 2
                    item = row%arg(j)
                    if (item%kind() /= NK_INT .or. item%int_value() /= 7) &
                        call fail("constant array", "entry is not the repeated value")
                end do
            end do
        end if

        ! Dimension expressions are evaluated before the constructor expands;
        ! this exercises the same bound-resolution path used by a corpus script.
        call a%init()
        call run_one(a, "n = 3"//nl()// &
                     "v = ConstantArray[0, {2*n, 2*n}]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("dynamic constant array", "refused: "//message)
        else if (chars(value%name()) /= "List" .or. value%nargs() /= 6) then
            call fail("dynamic constant array", "result does not have six rows")
        else
            do i = 1, 6
                row = value%arg(i)
                if (row%kind() /= NK_FUNC .or. chars(row%name()) /= "List" .or. &
                    row%nargs() /= 6) then
                    call fail("dynamic constant array", "result does not have six columns")
                end if
            end do
        end if

        ! Unsupported dimensions remain an unevaluated constructor, matching
        ! the oracle's boundary without dropping the named result.
        call a%init()
        call run_one(a, "v = Array[f, n]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("opaque array", "refused instead of preserving the constructor")
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "Array") then
            call fail("opaque array", "unsupported dimensions were not preserved")
        end if

        call a%init()
        call run_one(a, "v = Array[term[1], {2, 2}]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("computed array head", "refused instead of preserving the constructor")
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "Array") then
            call fail("computed array head", "unsupported head was not preserved")
        end if

        ! Outer[Times] is the Cartesian product, i.e. the outer product of
        ! the two vectors. These four products are an independent oracle.
        call a%init()
        call run_one(a, "v = Outer[Times, {1, 2}, {3, 4}]"//nl(), "v", &
                     value, ok, message)
        if (.not. ok) then
            call fail("outer", "refused: "//message)
        else if (chars(value%name()) /= "List" .or. value%nargs() /= 2) then
            call fail("outer", "result is not a 2x2 list")
        else
            do i = 1, 2
                row = value%arg(i)
                do j = 1, 2
                    item = row%arg(j)
                    if (item%kind() /= NK_INT .or. &
                        item%int_value() /= expected_outer(i, j)) &
                        call fail("outer", "outer product entry is incorrect")
                end do
            end do
        end if
    end subroutine test_array_collections

    subroutine test_flatten()
        type(arena_t), target :: a
        type(expr_t) :: value, item
        logical :: ok
        character(:), allocatable :: message
        integer :: i

        ! The expected sequence follows Flatten's definition: recurse through
        ! lists left-to-right and preserve every non-list leaf.
        call a%init()
        call run_one(a, "v = Flatten[{{1, 2}, {3, {4}}}]"//nl(), &
                     "v", value, ok, message)
        if (.not. ok) then
            call fail("flatten", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                value%nargs() /= 4) then
            call fail("flatten", "wrong flattened length")
        else
            do i = 1, 4
                item = value%arg(i)
                if (item%kind() /= NK_INT .or. item%int_value() /= i) &
                    call fail("flatten", "leaf order or value is wrong")
            end do
        end if

        call a%init()
        call run_one(a, "v = Flatten[{}]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. value%nargs() /= 0) &
            call fail("flatten empty", "empty input did not remain an empty List")

        call expect_refusal("flatten levels", "v = Flatten[{{1}}, 1]"//nl(), "v")
    end subroutine test_flatten

    !> Range is an arithmetic progression and DiagonalMatrix is a structural
    !> embedding. These checks derive the expected entries from those
    !> definitions, rather than copying the native printer's output.
    subroutine test_range_and_diagonal_matrix()
        type(arena_t), target :: a
        type(expr_t) :: value, item, row, x
        logical :: ok
        character(:), allocatable :: message
        integer :: i, j

        call a%init()
        call run_one(a, "v = Range[2, 8, 2]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("range integers", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                value%nargs() /= 4) then
            call fail("range integers", "wrong length or result head")
        else
            do i = 1, value%nargs()
                item = value%arg(i)
                if (item%kind() /= NK_INT .or. item%int_value() /= int(2*i)) &
                    call fail("range integers", "wrong arithmetic-progression entry")
            end do
        end if

        call a%init()
        call run_one(a, "v = Range[1, 1/2, -1/4]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("range rationals", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. value%nargs() /= 3) then
            call fail("range rationals", "wrong length")
        else
            item = value%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("range rationals", "wrong first entry")
            item = value%arg(2)
            if (item%kind() /= NK_RAT .or. item%int_value() /= 3 .or. &
                    item%den_value() /= 4) &
                call fail("range rationals", "wrong middle entry")
            item = value%arg(3)
            if (item%kind() /= NK_RAT .or. item%int_value() /= 1 .or. &
                    item%den_value() /= 2) &
                call fail("range rationals", "wrong final entry")
        end if

        call a%init()
        call run_one(a, "v = Range[5, 1]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. value%nargs() /= 0) &
            call fail("range empty", "descending unit range is not empty")

        call a%init()
        call run_one(a, "v = Range[1, 65]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. &
                chars(value%name()) /= "Range") &
            call fail("range bound", "large valid range was expanded past the safety bound")

        call a%init()
        call run_one(a, "v = Range[0, 2*Pi, Pi/2]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. &
                chars(value%name()) /= "Range") &
            call fail("range exact symbolic", "exact symbolic range was approximated")
        call expect_refusal("range zero step", "v = Range[1, 3, 0]"//nl(), "v")

        call a%init()
        call run_one(a, "v = DiagonalMatrix[{2, x, 0}]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("diagonal matrix", "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                value%nargs() /= 3) then
            call fail("diagonal matrix", "wrong matrix shape")
            return
        end if
        x = sym(a, "x")
        do i = 1, 3
            row = value%arg(i)
            if (row%kind() /= NK_FUNC .or. chars(row%name()) /= "List" .or. &
                    row%nargs() /= 3) then
                call fail("diagonal matrix", "row has wrong shape")
                cycle
            end if
            do j = 1, 3
                item = row%arg(j)
                if (i == j) then
                    if (i == 1) then
                        if (item%kind() /= NK_INT .or. item%int_value() /= 2) &
                            call fail("diagonal matrix", "first diagonal entry is wrong")
                    else if (i == 2) then
                        if (item%id /= x%id) &
                            call fail("diagonal matrix", "symbolic diagonal entry is wrong")
                    else if (item%kind() /= NK_INT .or. item%int_value() /= 0) then
                        call fail("diagonal matrix", "zero diagonal entry is wrong")
                    end if
                else if (item%kind() /= NK_INT .or. item%int_value() /= 0) then
                    call fail("diagonal matrix", "off-diagonal entry is not zero")
                end if
            end do
        end do
        call expect_refusal("diagonal matrix non-list", "v = DiagonalMatrix[x]"//nl(), "v")
    end subroutine test_range_and_diagonal_matrix

    pure function nl() result(c)
        character(1) :: c
        c = achar(10)
    end function nl

end program test_fortsym_wl_solvenum
