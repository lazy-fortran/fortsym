program test_fortsym_wl_filenamejoin
    ! Independent behavioral checks for the bounded FileNameJoin subset.
    ! Expected paths are hand-calculated POSIX join results, rather than
    ! values obtained from the evaluator or its printer.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_print, only: print_expr
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    integer :: nfail = 0

    call expect_value("literal components", &
        "value = FileNameJoin[{""root"", ""figures"", ""plot.pdf""}]"// &
        char(10), '"root/figures/plot.pdf"')
    call expect_value("absolute component resets path", &
        "value = FileNameJoin[{""root/"", ""/figures"", ""plot.pdf""}]"// &
        char(10), '"/figures/plot.pdf"')
    call expect_value("trailing separator is preserved", &
        "value = FileNameJoin[{""root"", ""figures/""}]"//char(10), &
        '"root/figures/"')
    call expect_refusal("symbolic component", &
        "value = FileNameJoin[{root, ""figures""}]"//char(10), &
        "FileNameJoin needs non-empty literal string components")
    call expect_refusal("dynamic component", &
        "dir = ""root"""//char(10)// &
        "value = FileNameJoin[{dir, ""figures""}]"//char(10), &
        "FileNameJoin needs non-empty literal string components")
    call expect_refusal("empty component", &
        "value = FileNameJoin[{""root"", """"}]"//char(10), &
        "FileNameJoin needs non-empty literal string components")

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_filenamejoin"
    else
        print *, "FAIL test_fortsym_wl_filenamejoin:", nfail
        error stop 1
    end if

contains

    subroutine expect_value(label, source, expected)
        character(*), intent(in) :: label, source, expected
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
            if (chars(b%name) /= "value") cycle
            found = .true.
            if (.not. b%ok) then
                print *, "FAIL ", label, ": refused: ", chars(b%message)
                nfail = nfail + 1
            else if (chars(print_expr(b%value)) /= expected) then
                print *, "FAIL ", label, ": got ", &
                    chars(print_expr(b%value)), " want ", expected
                nfail = nfail + 1
            end if
        end do
        if (.not. found) then
            print *, "FAIL ", label, ": no value binding"
            nfail = nfail + 1
        end if
    end subroutine expect_value

    subroutine expect_refusal(label, source, expected)
        character(*), intent(in) :: label, source, expected
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
            if (chars(b%name) /= "value") cycle
            found = .true.
            if (b%ok) then
                print *, "FAIL ", label, ": unexpectedly evaluated"
                nfail = nfail + 1
            else if (chars(b%message) /= expected) then
                print *, "FAIL ", label, ": refused with ", &
                    chars(b%message), " want ", expected
                nfail = nfail + 1
            end if
        end do
        if (.not. found) then
            print *, "FAIL ", label, ": no value binding"
            nfail = nfail + 1
        end if
    end subroutine expect_refusal

end program test_fortsym_wl_filenamejoin
