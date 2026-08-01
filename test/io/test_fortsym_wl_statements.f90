program test_fortsym_wl_statements
    ! Splitting a Wolfram script into statements.
    !
    ! The oracle is evaluation, not the split itself: a script is written so
    ! that a correct split produces a value an independent hand calculation
    ! agrees with, and a wrong split produces either a refusal or a different
    ! number. Asserting on statement offsets would only prove the splitter
    ! agrees with whoever wrote the expectation down.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_print, only: print_expr
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    integer :: nfail = 0

    call test_continuation_lines()
    call test_complete_lines_still_separate()
    call test_postfix_ends_a_line()
    call test_table_evaluation()

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
    end subroutine test_continuation_lines

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
        call expect("pattern function in table", &
                    "f[i_] := i^2"//char(10)// &
                    "values = Table[f[i], {i, 3}]"//char(10), &
                    "values", "List(1, 4, 9)")
        call expect("two-parameter function", &
                    "f[i_, j_] = i/j"//char(10)// &
                    "value = f[2, 4]"//char(10), "value", "1/2")
    end subroutine test_table_evaluation

end program test_fortsym_wl_statements
