program test_fortsym_wl_statements
    ! Splitting a Wolfram script into statements.
    !
    ! The oracle is evaluation, not the split itself: a script is written so
    ! that a correct split produces a value an independent hand calculation
    ! agrees with, and a wrong split produces either a refusal or a different
    ! number. Asserting on statement offsets would only prove the splitter
    ! agrees with whoever wrote the expectation down.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_print, only: print_expr
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    integer :: nfail = 0

    call test_continuation_lines()
    call test_leading_continuation_operators()
    call test_complete_lines_still_separate()
    call test_postfix_ends_a_line()
    call test_table_evaluation()
    call test_clear_and_compound_expression()
    call test_empty_script()
    call test_explicit_trig_simplify()
    call test_recursive_function()
    call test_pure_map_apply_replace()
    call test_list_threading()
    call test_list_selectors()
    call test_list_child_evaluation()
    call test_parenthesized_wolfram_multiplication()

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_statements"
    else
        print *, "FAIL test_fortsym_wl_statements:", nfail
        error stop 1
    end if

contains

    !> Run a script and check the value bound to `name`.
    subroutine expect(label, source, name, expected)
        character(*), intent(in) :: label, source, name, expected
        type(arena_t), target :: a
        type(wl_session_t) :: s
        type(wl_binding_t) :: b
        integer :: k
        logical :: found

        call a%init()
        call wl_session_begin(s, a)
        call wl_run_source(s, source)

        found = .false.
        do k = 1, wl_binding_count(s)
            b = wl_binding_at(s, k)
            if (chars(b%name) /= name) cycle
            found = .true.
            if (.not. b%ok) then
                print *, "FAIL ", label, ": refused: ", chars(b%message)
                nfail = nfail + 1
                return
            end if
            if (chars(print_expr(b%value)) /= expected) then
                print *, "FAIL ", label, ": got ", &
                    chars(print_expr(b%value)), " want ", expected
                nfail = nfail + 1
            end if
        end do

        if (.not. found) then
            print *, "FAIL ", label, ": no binding named ", name
            nfail = nfail + 1
        end if
    end subroutine expect

    !> A line ending in an operator continues onto the next.
    !>
    !> This is how the corpus writes long derivations, and splitting at the
    !> newline truncated them into a prefix that still parsed -- so the script
    !> reported "unexpected end of input" while being perfectly well formed.
    subroutine test_continuation_lines()
        call expect("trailing plus", "a = 1 +"//char(10)//"    2"//char(10), &
            "a", "3")
        ! A trailing "*" rather than a comma: a comma is only ever reached
        ! inside brackets, where the depth counter already suppresses the
        ! split, so it would not exercise this rule at all.
        call expect("trailing times", "b = 2 *"//char(10)//"  3"//char(10), &
            "b", "6")
        call expect("trailing dot", "c = {1, 2} ."//char(10)// &
            "  {x, y}"//char(10), "c", "x + y*2")
    end subroutine test_continuation_lines

    !> A continuation operator may be written at the start of the next line.
    subroutine test_leading_continuation_operators()
        call expect("leading plus", "a = 1"//char(10)//"  + 2"//char(10), &
            "a", "3")
        call expect("leading times", "b = 2"//char(10)//"  * 3"//char(10), &
            "b", "6")
        call expect("leading dot", "c = {1, 2}"//char(10)// &
            "  . {x, y}"//char(10), "c", "x + y*2")
    end subroutine test_leading_continuation_operators

    !> Two complete lines stay two statements. The continuation rule must not
    !> glue independent assignments together.
    subroutine test_complete_lines_still_separate()
        call expect("second of two", &
            "p = 2"//char(10)//"q = 5"//char(10), "q", "5")
        call expect("first of two", &
            "p = 2"//char(10)//"q = 5"//char(10), "p", "2")
    end subroutine test_complete_lines_still_separate

    !> "&" closes a pure function and "!" is factorial: both are postfix, so a
    !> line ending in either is complete. Treating them as operators awaiting a
    !> right operand would swallow the next statement.
    subroutine test_postfix_ends_a_line()
        call expect("factorial then assignment", &
            "u = 3!"//char(10)//"v = 7"//char(10), "v", "7")
    end subroutine test_postfix_ends_a_line

    !> A Table is checked against hand-evaluated values, including nested
    !> iterators. This is a behavioral oracle: it does not inspect the source
    !> or the evaluator's internal nodes.
    subroutine test_table_evaluation()
        call expect("table squares", "values = Table[i^2, {i, 3}]"//char(10), &
            "values", "List(1, 4, 9)")
        call expect("nested table", &
            "grid = Table[i + j, {i, 2}, {j, 3}]"//char(10), &
            "grid", "List(List(2, 3, 4), List(3, 4, 5))")
        call expect("nested table expression", &
            "grid = Table[Table[i + j, {j, 2}], {i, 2}]"//char(10), &
            "grid", "List(List(2, 3), List(3, 4))")
        call expect("map named function", &
            "f[x_] := x + 1"//char(10)// &
            "values = Map[f, {1, 2, 3}]"//char(10), &
            "values", "List(2, 3, 4)")
        call expect("pattern function in table", &
            "f[i_] := i^2"//char(10)// &
            "values = Table[f[i], {i, 3}]"//char(10), &
            "values", "List(1, 4, 9)")
        call expect("two-parameter function", &
            "f[i_, j_] = i/j"//char(10)// &
            "value = f[2, 4]"//char(10), "value", "1/2")
        call expect("table local shadows global", &
            "i = 99"//char(10)//"n = 3"//char(10)// &
            "values = Table[i + n, {i, n}]"//char(10), &
            "values", "List(4, 5, 6)")
    end subroutine test_table_evaluation

    !> Clear removes an earlier value, and a top-level comma keeps compound
    !> expressions in their written order. The expected result must therefore
    !> retain the symbol rather than the stale numeric binding.
    subroutine test_clear_and_compound_expression()
        call expect("clear binding", &
            "a = 2"//char(10)//"Clear[a]"//char(10)// &
            "value = a + 1"//char(10), "value", "a + 1")
        call expect("compound clear", &
            "a = 2"//char(10)//"Clear[a], Null, a = 3"//char(10), &
            "a", "3")
    end subroutine test_clear_and_compound_expression

    subroutine test_empty_script()
        type(arena_t), target :: a
        type(wl_session_t) :: s

        call a%init()
        call wl_session_begin(s, a)
        call wl_run_source(s, "Null"//char(10))
        if (wl_binding_count(s) /= 0) then
            print *, "FAIL empty script: produced ", wl_binding_count(s), &
                " bindings"
            nfail = nfail + 1
        end if
    end subroutine test_empty_script

    subroutine test_explicit_trig_simplify()
        call expect("pythagorean identity", &
            "value = Simplify[Sin[x]^2 + Cos[x]^2]"//char(10), &
            "value", "1")
    end subroutine test_explicit_trig_simplify

    subroutine test_recursive_function()
        call expect("recursive function", &
            "f[n_] := If[n <= 0, 1, n*f[n - 1]]"//char(10)// &
            "value = f[4]"//char(10), "value", "24")
    end subroutine test_recursive_function

    !> Pure-function application and replacement are checked through their
    !> observable values. A parser-only implementation would accept these
    !> forms but either refuse or leak Slot/Rule nodes into the result.
    subroutine test_pure_map_apply_replace()
        call expect("pure function Map", &
            "values = Map[#^2 &, {1, 2, 3}]"//char(10), &
            "values", "List(1, 4, 9)")
        call expect("Apply Plus", &
            "value = Apply[Plus, {1, 2, 3}]"//char(10), &
            "value", "6")
        call expect("ReplaceAll rules", &
            "value = {x, y} /. {x -> 2, y -> 3}"//char(10), &
            "value", "List(2, 3)")
    end subroutine test_pure_map_apply_replace

    !> Arithmetic on lists is threaded element by element in Wolfram.
    !> These are independent hand calculations, not assertions about the
    !> evaluator's internal list representation.
    subroutine test_list_threading()
        call expect("list Times", "value = {1, 2}*{3, 4}"//char(10), &
            "value", "List(3, 8)")
        call expect("scalar list Plus", "value = {1, 2} + 3"//char(10), &
            "value", "List(4, 5)")
        call expect("list Power", "value = {1, 2}^2"//char(10), &
            "value", "List(1, 4)")
        call expect("Thread equal", &
            "value = Thread[Equal[{x, y}, {a, b}]]"//char(10), &
            "value", "List(Equal(x, a), Equal(y, b))")
        call expect("Thread bound equal", &
            "lhs = {x, y}"//char(10)//"rhs = {a, b}"//char(10)// &
            "value = Thread[Equal[lhs, rhs]]"//char(10), &
            "value", "List(Equal(x, a), Equal(y, b))")
    end subroutine test_list_threading

    !> Selectors are checked against the hand-calculated item order, including
    !> negative counts and one-based inclusive ranges.
    subroutine test_list_selectors()
        call expect("First list item", "value = First[{a, b, c}]"//char(10), &
            "value", "a")
        call expect("Last list item", "value = Last[{a, b, c}]"//char(10), &
            "value", "c")
        call expect("Rest list items", "value = Rest[{a, b, c}]"//char(10), &
            "value", "List(b, c)")
        call expect("Most list items", "value = Most[{a, b, c}]"//char(10), &
            "value", "List(a, b)")
        call expect("Reverse list items", &
            "value = Reverse[{a, b, c}]"//char(10), "value", "List(c, b, a)")
        call expect("Take positive count", &
            "value = Take[{a, b, c, d}, 2]"//char(10), "value", "List(a, b)")
        call expect("Take negative count", &
            "value = Take[{a, b, c, d}, -2]"//char(10), "value", "List(c, d)")
        call expect("Take inclusive range", &
            "value = Take[{a, b, c, d}, {2, 3}]"//char(10), &
            "value", "List(b, c)")
        call expect("Drop positive count", &
            "value = Drop[{a, b, c, d}, 1]"//char(10), "value", "List(b, c, d)")
        call expect("Drop negative count", &
            "value = Drop[{a, b, c, d}, -2]"//char(10), "value", "List(a, b)")
        call expect("Drop inclusive range", &
            "value = Drop[{a, b, c, d}, {2, 3}]"//char(10), &
            "value", "List(a, d)")
    end subroutine test_list_selectors

    !> List elements are evaluated independently. These hand-derived values
    !> catch the tempting but incorrect optimization of leaving List children
    !> untouched.
    subroutine test_list_child_evaluation()
        call expect("derivatives inside list", &
            "values = {D[x^2, x], D[Sin[x], x]}"//char(10), &
            "values", "List(x*2, cos(x))")
        call expect("derivative of list", &
            "values = D[{x^2, Sin[x]}, x]"//char(10), &
            "values", "List(x*2, cos(x))")
        call expect("replace after derivative", &
            "values = D[{x^2, x*y}, x] /. x -> 0"//char(10), &
            "values", "List(0, y)")
    end subroutine test_list_child_evaluation

    !> Wolfram calls use square brackets. Parentheses after a symbol are
    !> implicit multiplication, so rho (x + 1) is rho*(x + 1), not rho[x+1].
    subroutine test_parenthesized_wolfram_multiplication()
        call expect("parenthesized multiplication", &
            "value = rho (x + 1)"//char(10), &
            "value", "rho*(x + 1)")
    end subroutine test_parenthesized_wolfram_multiplication

end program test_fortsym_wl_statements
