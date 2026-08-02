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
    !     not about fortsym; high precision must remain a long decimal result.
    !   * Cross is checked by perpendicularity to both of its symbolic
    !     operands, which fixes all three components up to a scale no wrong
    !     formula survives together with the unit-vector case.
    !   * Tr is checked by Tr[A.B] == Tr[B.A], which no diagonal-picking bug
    !     satisfies for a generic pair.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM, NK_INT, NK_RAT, NK_REAL, &
        NK_BIG_REAL
    use fortsym_expr, only: expr_t, sym, num, real_expr, operator(-), operator(*), &
        operator(+), operator(**)
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
    call test_solve_rule_replacement()
    call test_fractional_exponent()
    call test_solve_linear_system()
    call test_solve_symbolic_linear_system()
    call test_linear_solve()
    call test_solve_refuses_what_it_cannot_verify()
    call test_n_precision()
    call test_implicit_scientific_power()
    call test_find_root()
    call test_chop()
    call test_identity_cross_trace()
    call test_array_collections()
    call test_flatten()
    call test_append_and_join()
    call test_range_and_diagonal_matrix()
    call test_legendre_wolfram()
    call test_characteristic_polynomial()
    call test_multivariate_coefficient_list()
    call test_fold_list()
    call test_total()
    call test_pseudoinverse()
    call test_singular_value_list()
    call test_extrema()
    call test_array_flatten()
    call test_matrix_power()
    call test_minors_dispatch()

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

    subroutine test_solve_rule_replacement()
        type(arena_t), target :: a
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "hpnReturn = r /. First[Solve[2*r == (1-r)/2, r]]"//nl(), &
            "hpnReturn", value, ok, message)
        if (.not. ok) then
            call fail("Solve rule replacement", "refused: "//message)
            return
        end if
        ! The independent oracle is the exact rearrangement
        ! 2 r = (1-r)/2, hence 5 r = 1.
        if (value%kind() /= NK_RAT .or. value%int_value() /= 1 .or. &
            value%den_value() /= 5) then
            call fail("Solve rule replacement", "expected exact 1/5")
        end if
    end subroutine test_solve_rule_replacement

    subroutine test_fractional_exponent()
        type(arena_t), target :: a
        type(expr_t) :: value, shifted
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "e = Exponent[(u - v)*x^(2/5), x]"//nl()// &
            "f = Exponent[z + (u - z)*x^(2/5), x]"//nl(), &
            "e", value, ok, message)
        if (.not. ok) then
            call fail("fractional Exponent", "refused: "//message)
            return
        end if
        ! The coefficient (u-v) is independent of x, so the exact exponent is
        ! the rational power 2/5, not a floating approximation.
        if (value%kind() /= NK_RAT .or. value%int_value() /= 2 .or. &
            value%den_value() /= 5) then
            call fail("fractional Exponent", "expected exact 2/5")
        end if
        call run_one(a, "f = Exponent[z + (u - z)*x^(2/5), x]"//nl(), &
            "f", shifted, ok, message)
        if (.not. ok .or. shifted%kind() /= NK_RAT .or. &
            shifted%int_value() /= 2 .or. shifted%den_value() /= 5) then
            call fail("shifted fractional Exponent", "expected exact 2/5")
        end if
    end subroutine test_fractional_exponent

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

    subroutine test_solve_symbolic_linear_system()
        type(arena_t), target :: a
        type(expr_t) :: value, inner, rule, lhs, q0_value, d2_value
        type(expr_t) :: e1, e2, probe
        logical :: ok
        logical :: numeric_defined
        character(:), allocatable :: message
        real(dp) :: numeric_residual
        integer :: k

        call a%init()
        call run_one(a, "q = Solve[{"// &
            "(qsrc - q0) + kap D2 == 0, "// &
            "gam1 (1 - q0) - gam2 D2 == 0}, {q0, D2}]"//nl(), &
            "q", value, ok, message)
        if (.not. ok) then
            call fail("symbolic system", "refused: "//message)
            return
        end if
        if (chars(value%name()) /= "List" .or. value%nargs() /= 1) then
            call fail("symbolic system", "expected one solution")
            return
        end if
        inner = value%arg(1)
        if (chars(inner%name()) /= "List" .or. inner%nargs() /= 2) then
            call fail("symbolic system", "expected two rules")
            return
        end if
        q0_value = num(a, 0)
        d2_value = num(a, 0)
        do k = 1, 2
            rule = inner%arg(k)
            lhs = rule%arg(1)
            if (chars(lhs%name()) == "q0") q0_value = rule%arg(2)
            if (chars(lhs%name()) == "D2") d2_value = rule%arg(2)
        end do

        e1 = parse_expr_in(a, "(qsrc - q0) + kap D2", &
            dialect(DIA_WOLFRAM), ok, message)
        e2 = parse_expr_in(a, "gam1 (1 - q0) - gam2 D2", &
            dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            call fail("symbolic system", "oracle text did not parse")
            return
        end if
        probe = subs(subs(e1, sym(a, "q0"), q0_value), &
            sym(a, "D2"), d2_value)
        probe = subs(probe, sym(a, "qsrc"), num(a, 3))
        probe = subs(probe, sym(a, "kap"), num(a, 7))
        probe = subs(probe, sym(a, "gam1"), num(a, 2))
        probe = subs(probe, sym(a, "gam2"), num(a, 5))
        call numeric_value(probe, numeric_residual, numeric_defined, message)
        if (.not. numeric_defined .or. abs(numeric_residual) > 1.0e-10_dp) then
            call fail("symbolic system", "first numeric instance failed")
            return
        end if
        probe = subs(subs(e2, sym(a, "q0"), q0_value), &
            sym(a, "D2"), d2_value)
        probe = subs(probe, sym(a, "qsrc"), num(a, 3))
        probe = subs(probe, sym(a, "kap"), num(a, 7))
        probe = subs(probe, sym(a, "gam1"), num(a, 2))
        probe = subs(probe, sym(a, "gam2"), num(a, 5))
        call numeric_value(probe, numeric_residual, numeric_defined, message)
        if (.not. numeric_defined .or. abs(numeric_residual) > 1.0e-10_dp) then
            call fail("symbolic system", "second numeric instance failed")
        end if
    end subroutine test_solve_symbolic_linear_system

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
        call expect_big_real("N thirty digits", "v = N[Pi, 30]"//nl(), "v", &
            "3.1415926535897932384626")
        call expect_big_real("N seventeen digits", "v = N[Pi, 17]"//nl(), "v", &
            "3.1415926535897932")
        ! The explicit bound is a refusal, not an allocation proportional to an
        ! untrusted digit count.
        call expect_refusal("N above MPFR bound", "v = N[Pi, 600]"//nl(), "v")
        ! Elementwise over a list.
        call expect_list_real("N over a list", "v = N[{Pi, 2*Pi}, 5]"//nl(), &
            "v", [3.1416_dp, 6.2832_dp], 1.0e-12_dp)
    end subroutine test_n_precision

    !> An implicit scientific literal must not absorb the next factor into its
    !> negative exponent.  The exact source algebra reduces to
    !> (165/100*15)/(241/100) = 2475/241 before the 2/3 power; the decimal
    !> prefix below is an independent high-precision oracle for that value.
    subroutine test_implicit_scientific_power()
        call expect_big_real("implicit scientific power", &
            "eta0 = 241/100 10^-9"//nl()// &
            "v = N[(165/100 10^-9 15/eta0)^(2/3), 20]"//nl(), "v", &
            "4.7246768213475920150")
    end subroutine test_implicit_scientific_power

    !> The decimal is an independent high-precision oracle for Log[2], not a
    !> check that the implementation agrees with its own Newton iteration.
    subroutine test_find_root()
        call expect_real("numeric FindRoot", &
            "root = FindRoot[Exp[-x] == 1/2, {x, 1/2}]"//nl()// &
            "v = x /. root"//nl(), "v", 0.69314718055994530942_dp, 1.0e-14_dp)
    end subroutine test_find_root

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

    subroutine expect_big_real(label, script, name, prefix)
        character(*), intent(in) :: label, script, name, prefix
        type(arena_t), target :: a
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: message, text

        call a%init()
        call run_one(a, script, name, value, ok, message)
        if (.not. ok) then
            call fail(label, "refused: "//message)
            return
        end if
        if (value%kind() /= NK_BIG_REAL) then
            call fail(label, "result discarded requested precision")
            return
        end if
        text = chars(value%real_text())
        if (index(text, prefix) /= 1 .or. len(text) <= 16) then
            call fail(label, "decimal result does not retain the independent pi digits")
        end if
    end subroutine expect_big_real

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

        ! A row vector times a matrix is the first operation in the chained
        ! metric products used by the large-step corpus. Check its columns
        ! independently from the implementation, then check that its vector
        ! result can feed the existing vector dot product.
        call a%init()
        call run_one(a, "v = {2, 3} . {{5, 7, 11}, {13, 17, 19}}"//nl(), &
            "v", value, ok, message)
        if (.not. ok) then
            call fail("vector-matrix dot", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                 value%nargs() /= 3) then
            call fail("vector-matrix dot", "result is not a three-component vector")
        else
            item = value%arg(1)
            if (item%int_value() /= 49) &
                call fail("vector-matrix dot", "first column is not 2*5 + 3*13")
            item = value%arg(2)
            if (item%int_value() /= 65) &
                call fail("vector-matrix dot", "second column is not 2*7 + 3*17")
            item = value%arg(3)
            if (item%int_value() /= 79) &
                call fail("vector-matrix dot", "third column is not 2*11 + 3*19")
        end if

        call a%init()
        call run_one(a, "v = {2, 3} . {{5, 7, 11}, {13, 17, 19}} . {23, 29, 31}"//nl(), &
            "v", value, ok, message)
        if (.not. ok) then
            call fail("chained vector-matrix dot", "refused: "//message)
        else if (value%kind() /= NK_INT .or. value%int_value() /= 5461) then
            call fail("chained vector-matrix dot", "chain disagrees with 49*23 + 65*29 + 79*31")
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

    !> Append and Join construct lists without changing their element order.
    !> The expected entries are derived from the list definitions, not copied
    !> from the native printer.
    subroutine test_append_and_join()
        type(arena_t), target :: a
        type(expr_t) :: value, item
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "v = Append[{1, 2}, x]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("append", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                value%nargs() /= 3) then
            call fail("append", "wrong result shape")
        else
            item = value%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("append", "first entry changed")
            item = value%arg(2)
            if (item%kind() /= NK_INT .or. item%int_value() /= 2) &
                call fail("append", "second entry changed")
            item = value%arg(3)
            if (item%kind() /= NK_SYM .or. chars(item%name()) /= "x") &
                call fail("append", "appended entry is wrong")
        end if

        call a%init()
        call run_one(a, "v = Join[{1, 2}, {x, 3}, {}]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("join", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                value%nargs() /= 4) then
            call fail("join", "wrong result shape")
        else
            item = value%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("join", "first entry changed")
            item = value%arg(2)
            if (item%kind() /= NK_INT .or. item%int_value() /= 2) &
                call fail("join", "second entry changed")
            item = value%arg(3)
            if (item%kind() /= NK_SYM .or. chars(item%name()) /= "x") &
                call fail("join", "third entry changed")
            item = value%arg(4)
            if (item%kind() /= NK_INT .or. item%int_value() /= 3) &
                call fail("join", "fourth entry changed")
        end if

        call expect_refusal("append non-list", "v = Append[x, 1]"//nl(), "v")
        call expect_refusal("join non-list", "v = Join[{1}, x]"//nl(), "v")
    end subroutine test_append_and_join

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

        call a%init()
        call run_one(a, "v = Diagonal[{{1, 2, 3}, {x, 5, 6}}]"//nl(), "v", &
            value, ok, message)
        if (.not. ok) then
            call fail("diagonal extract", "refused: "//message)
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
                value%nargs() /= 2) then
            call fail("diagonal extract", "wrong rectangular result shape")
        else
            item = value%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 1) &
                call fail("diagonal extract", "first diagonal entry is wrong")
            item = value%arg(2)
            if (item%kind() /= NK_INT .or. item%int_value() /= 5) &
                call fail("diagonal extract", "second diagonal entry is wrong")
        end if
        call a%init()
        call run_one(a, "v = Diagonal[x]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("diagonal non-matrix", "refused instead of preserving the constructor")
        else if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "Diagonal") then
            call fail("diagonal non-matrix", "unsupported argument was not preserved")
        end if
    end subroutine test_range_and_diagonal_matrix

    !> LegendreP is checked at an independently chosen numeric point against
    !> the defining Rodrigues polynomial for degree three.
    subroutine test_legendre_wolfram()
        type(arena_t), target :: a
        type(expr_t) :: value, point
        logical :: ok, defined
        character(:), allocatable :: message
        real(dp) :: got, expected

        call a%init()
        call run_one(a, "v = LegendreP[3, x]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("LegendreP dispatch", "refused: "//message)
            return
        end if
        point = subs(value, sym(a, "x"), real_expr(a, 0.3_dp))
        call numeric_value(point, got, defined, message)
        expected = (5.0_dp*0.3_dp**3 - 3.0_dp*0.3_dp)/2.0_dp
        if (.not. defined) then
            call fail("LegendreP dispatch", "result is not numerically defined")
        else if (abs(got - expected) > 1.0e-13_dp) then
            call fail("LegendreP dispatch", "Rodrigues polynomial value is wrong")
        end if
    end subroutine test_legendre_wolfram

    !> The characteristic polynomial is checked at a point against its
    !> defining determinant, not against the native expression's spelling.
    subroutine test_characteristic_polynomial()
        type(arena_t), target :: a
        type(expr_t) :: value, point
        logical :: ok, defined
        character(:), allocatable :: message
        real(dp) :: got

        call a%init()
        call run_one(a, "v = CharacteristicPolynomial[{{1, 2}, {3, 4}}, z]"//nl(), &
            "v", value, ok, message)
        if (.not. ok) then
            call fail("CharacteristicPolynomial", "refused: "//message)
            return
        end if
        point = subs(value, sym(a, "z"), real_expr(a, 5.0_dp))
        call numeric_value(point, got, defined, message)
        if (.not. defined) then
            call fail("CharacteristicPolynomial", "result is not numerically defined")
        else if (abs(got - (-2.0_dp)) > 1.0e-13_dp) then
            call fail("CharacteristicPolynomial", &
                "determinant at z=5 does not match det(5 I - A)")
        end if

        call a%init()
        call run_one(a, "v = CharacteristicPolynomial[s, z]"//nl(), "v", &
            value, ok, message)
        if (.not. ok) then
            call fail("CharacteristicPolynomial opaque", &
                "symbolic matrix was refused")
        else if (value%kind() /= NK_FUNC .or. &
                chars(value%name()) /= "CharacteristicPolynomial") then
            call fail("CharacteristicPolynomial opaque", &
                "symbolic matrix was not preserved")
        end if
    end subroutine test_characteristic_polynomial

    !> A multivariate coefficient list is checked by reconstructing the
    !> polynomial from its nested entries. The reconstruction is independent
    !> of the order in which the native evaluator extracts coefficients.
    subroutine test_multivariate_coefficient_list()
        type(arena_t), target :: a
        type(expr_t) :: value, x, y, reconstructed, row, coefficient
        logical :: ok
        character(:), allocatable :: message
        integer :: i, j

        call a%init()
        call run_one(a, "v = CoefficientList[1 + 2*x + 3*y + 4*x*y, {x, y}]"//nl(), &
            "v", value, ok, message)
        if (.not. ok) then
            call fail("CoefficientList", "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
            value%nargs() /= 2) then
            call fail("CoefficientList", "wrong outer list shape")
            return
        end if

        x = sym(a, "x")
        y = sym(a, "y")
        reconstructed = num(a, 0)
        do i = 1, value%nargs()
            row = value%arg(i)
            if (row%kind() /= NK_FUNC .or. chars(row%name()) /= "List") then
                call fail("CoefficientList", "wrong inner list shape")
                return
            end if
            do j = 1, row%nargs()
                coefficient = row%arg(j)
                reconstructed = reconstructed + coefficient * &
                    x**(i - 1) * y**(j - 1)
            end do
        end do
        if (.not. proves_zero(a, reconstructed - (1 + 2*x + 3*y + 4*x*y))) then
            call fail("CoefficientList", "nested coefficients do not reconstruct")
        end if
    end subroutine test_multivariate_coefficient_list

    !> FoldList[Plus, ...] is checked against its defining prefix-sum
    !> recurrence, including the initial value and the empty-list boundary.
    subroutine test_fold_list()
        type(arena_t), target :: a
        type(expr_t) :: value, empty, item
        logical :: ok
        character(:), allocatable :: message
        integer, parameter :: expected(4) = [1, 3, 6, 10]
        integer :: k

        call a%init()
        call run_one(a, "v = FoldList[Plus, 1, {2, 3, 4}]"//nl(), "v", &
            value, ok, message)
        if (.not. ok) then
            call fail("FoldList", "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
            value%nargs() /= size(expected)) then
            call fail("FoldList", "wrong prefix-list shape")
            return
        end if
        do k = 1, size(expected)
            item = value%arg(k)
            if (item%kind() /= NK_INT .or. item%int_value() /= expected(k)) then
                call fail("FoldList", "prefix recurrence is wrong")
                return
            end if
        end do

        call run_one(a, "v = FoldList[Plus, 7, {}]"//nl(), "v", &
            empty, ok, message)
        item = empty
        if (.not. ok .or. item%nargs() /= 1) then
            call fail("FoldList empty", "initial value was not retained")
        else
            item = item%arg(1)
            if (item%kind() /= NK_INT .or. item%int_value() /= 7) then
                call fail("FoldList empty", "initial value was not retained")
            end if
        end if
    end subroutine test_fold_list

    !> Total is checked against independent scalar and vector sums, including
    !> Wolfram's empty-list identity.
    subroutine test_total()
        type(arena_t), target :: a
        type(expr_t) :: value, row, item
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "v = Total[{1, 2, 3, 4}]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_INT .or. value%int_value() /= 10) then
            call fail("Total scalar", "wrong scalar sum or refusal: "//message)
            return
        end if

        call run_one(a, "v = Total[{{1, 2}, {3, 4}}]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. value%nargs() /= 2) then
            call fail("Total vector", "wrong vector sum shape or refusal: "//message)
            return
        end if
        row = value
        item = row%arg(1)
        if (item%kind() /= NK_INT .or. item%int_value() /= 4) then
            call fail("Total vector", "wrong first component")
        end if
        item = row%arg(2)
        if (item%kind() /= NK_INT .or. item%int_value() /= 6) then
            call fail("Total vector", "wrong second component")
        end if

        call run_one(a, "v = Total[{}]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_INT .or. value%int_value() /= 0) then
            call fail("Total empty", "empty-list identity is not zero")
        end if
    end subroutine test_total

    !> The full-column-rank Moore-Penrose identity is checked on a rectangular
    !> rational matrix. The expected entries come from (A^T A)^-1 A^T, not from
    !> the implementation's intermediate nodes.
    subroutine test_pseudoinverse()
        type(arena_t), target :: a
        type(expr_t) :: value, row, item
        logical :: ok
        character(:), allocatable :: message
        integer(int64), parameter :: numerators(2, 3) = reshape([ &
            2_int64, -1_int64, -1_int64, 2_int64, 1_int64, 1_int64], [2, 3])
        integer(int64), parameter :: denominators(2, 3) = reshape([ &
            3_int64, 3_int64, 3_int64, 3_int64, 3_int64, 3_int64], [2, 3])
        integer :: i, j

        call a%init()
        call run_one(a, "p = PseudoInverse[{{1, 0}, {0, 1}, {1, 1}}]"//nl(), &
            "p", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. value%nargs() /= 2) then
            call fail("PseudoInverse", "wrong result shape or refusal: "//message)
            return
        end if
        do i = 1, 2
            row = value%arg(i)
            if (row%kind() /= NK_FUNC .or. row%nargs() /= 3) then
                call fail("PseudoInverse", "wrong row shape")
                return
            end if
            do j = 1, 3
                item = row%arg(j)
                if (item%kind() /= NK_RAT .or. &
                    item%int_value() /= numerators(i, j) .or. &
                    item%den_value() /= denominators(i, j)) then
                    call fail("PseudoInverse", "normal-equation result is wrong")
                    return
                end if
            end do
        end do
    end subroutine test_pseudoinverse

    !> A diagonal matrix has singular values equal to sorted absolute diagonal
    !> entries; this test also fixes the zero boundary.
    subroutine test_singular_value_list()
        type(arena_t), target :: a
        type(expr_t) :: value, item
        logical :: ok
        character(:), allocatable :: message
        real(dp), parameter :: expected(2) = [3.0_dp, 0.0_dp]
        integer :: k

        call a%init()
        call run_one(a, "v = SingularValueList[{{0.0, 0.0}, {0.0, -3.0}}]"//nl(), &
            "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_FUNC .or. value%nargs() /= 2) then
            call fail("SingularValueList", "wrong result shape or refusal: "//message)
            return
        end if
        do k = 1, 2
            item = value%arg(k)
            if (item%kind() /= NK_REAL .or. abs(item%real_value() - expected(k)) > &
                1.0e-12_dp) then
                call fail("SingularValueList", "wrong sorted singular value")
                return
            end if
        end do
    end subroutine test_singular_value_list

    !> Numeric extrema reduce the explicit singular-value list to its operator
    !> norm, while the opposite extremum fixes the ordering boundary.
    subroutine test_extrema()
        type(arena_t), target :: a
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        call run_one(a, "v = Max[{1.0, 3.0, 2.0}]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_REAL .or. &
            abs(value%real_value() - 3.0_dp) > 1.0e-12_dp) then
            call fail("Max", "wrong numeric maximum or refusal: "//message)
        end if
        call run_one(a, "v = Min[{1.0, 3.0, 2.0}]"//nl(), "v", value, ok, message)
        if (.not. ok .or. value%kind() /= NK_REAL .or. &
            abs(value%real_value() - 1.0_dp) > 1.0e-12_dp) then
            call fail("Min", "wrong numeric minimum or refusal: "//message)
        end if
    end subroutine test_extrema

    !> ArrayFlatten is checked by its block-concatenation definition, including
    !> a non-square result so swapping row and column offsets cannot pass.
    subroutine test_array_flatten()
        type(arena_t), target :: a
        type(expr_t) :: value, row, item
        logical :: ok
        character(:), allocatable :: message
        integer, parameter :: expected(3, 3) = &
            reshape([1, 3, 7, 2, 4, 8, 5, 6, 9], [3, 3])
        integer :: i, j

        call a%init()
        call run_one(a, "v = ArrayFlatten[{{{{1, 2}, {3, 4}}, {{5}, {6}}}, "// &
            "{{{7, 8}}, {{9}}}}]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("ArrayFlatten", "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
            value%nargs() /= 3) then
            call fail("ArrayFlatten", "wrong output shape")
            return
        end if
        do i = 1, 3
            row = value%arg(i)
            if (row%kind() /= NK_FUNC .or. chars(row%name()) /= "List" .or. &
                row%nargs() /= 3) then
                call fail("ArrayFlatten", "wrong output shape")
                return
            end if
            do j = 1, 3
                item = row%arg(j)
                if (item%kind() /= NK_INT .or. item%int_value() /= expected(i, j)) then
                    call fail("ArrayFlatten", "block concatenation is wrong")
                    return
                end if
            end do
        end do
    end subroutine test_array_flatten

    !> MatrixPower is checked against the independently multiplied entries of
    !> a concrete 2x2 matrix, including the exponent-zero identity boundary.
    subroutine test_matrix_power()
        type(arena_t), target :: a
        type(expr_t) :: value, row, item
        logical :: ok
        character(:), allocatable :: message
        integer, parameter :: expected(2, 2) = reshape([7, 15, 10, 22], [2, 2])
        integer :: i, j

        call a%init()
        call run_one(a, "v = MatrixPower[{{1, 2}, {3, 4}}, 2]"//nl(), "v", &
            value, ok, message)
        if (.not. ok) then
            call fail("MatrixPower", "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
            value%nargs() /= 2) then
            call fail("MatrixPower", "wrong matrix shape")
            return
        end if
        do i = 1, 2
            row = value%arg(i)
            if (row%kind() /= NK_FUNC .or. chars(row%name()) /= "List" .or. &
                row%nargs() /= 2) then
                call fail("MatrixPower", "wrong row shape")
                cycle
            end if
            do j = 1, 2
                item = row%arg(j)
                if (item%kind() /= NK_INT .or. &
                    item%int_value() /= expected(i, j)) then
                    call fail("MatrixPower", "wrong independently multiplied entry")
                end if
            end do
        end do

        call a%init()
        call run_one(a, "v = MatrixPower[{{1, 2}, {3, 4}}, 0]"//nl(), "v", &
            value, ok, message)
        if (.not. ok) then
            call fail("MatrixPower identity", "zero exponent is not the identity")
        else if (value%kind() /= NK_FUNC .or. value%nargs() /= 2) then
            call fail("MatrixPower identity", "zero exponent is not a 2x2 matrix")
        else
            row = value%arg(1)
            item = row%arg(1)
            if (item%int_value() /= 1) call fail("MatrixPower identity", &
                "identity has wrong (1,1) entry")
            item = row%arg(2)
            if (item%int_value() /= 0) call fail("MatrixPower identity", &
                "identity has wrong (1,2) entry")
            row = value%arg(2)
            item = row%arg(1)
            if (item%int_value() /= 0) call fail("MatrixPower identity", &
                "identity has wrong (2,1) entry")
            item = row%arg(2)
            if (item%int_value() /= 1) call fail("MatrixPower identity", &
                "identity has wrong (2,2) entry")
        end if
        call expect_refusal("matrix power negative exponent", &
            "v = MatrixPower[{{1, 2}, {3, 4}}, -1]"//nl(), "v")
    end subroutine test_matrix_power

    !> Exercise the public Wolfram dispatch as well as the matrix primitive.
    !> The four values are independent 3x3 determinants of the same 3x4
    !> matrix used by the algebra-level test.
    subroutine test_minors_dispatch()
        type(arena_t), target :: a
        type(expr_t) :: value, row, item
        logical :: ok
        character(:), allocatable :: message
        integer :: k
        integer, parameter :: expected(4) = [11, 3, -10, 15]

        call a%init()
        call run_one(a, "v = Minors[{{1, 2, 3, 4}, {0, 1, 4, 2}, "// &
            "{2, 0, 1, 3}}, 3]"//nl(), "v", value, ok, message)
        if (.not. ok) then
            call fail("Minors dispatch", "refused: "//message)
            return
        end if
        if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
            value%nargs() /= 1) then
            call fail("Minors dispatch", "wrong row-combination shape")
            return
        end if
        row = value%arg(1)
        if (row%kind() /= NK_FUNC .or. chars(row%name()) /= "List" .or. &
            row%nargs() /= 4) then
            call fail("Minors dispatch", "wrong column-combination shape")
            return
        end if
        do k = 1, 4
            item = row%arg(k)
            if (item%kind() /= NK_INT .or. item%int_value() /= expected(k)) &
                call fail("Minors dispatch", "wrong determinant value")
        end do
    end subroutine test_minors_dispatch

    pure function nl() result(c)
        character(1) :: c
        c = achar(10)
    end function nl

end program test_fortsym_wl_solvenum
