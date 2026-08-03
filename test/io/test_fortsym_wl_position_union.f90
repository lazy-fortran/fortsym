program test_fortsym_wl_position_union
    ! Independent behavioral checks for the bounded explicit-list slice.
    ! The expected paths and sorted set values are hand-evaluated Wolfram
    ! results; no evaluator printer or internal node identity is used to make
    ! the oracle agree with itself.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_print, only: print_expr
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    integer :: nfail = 0

    call expect("nested positions", &
        "value = Position[{{1, 2}, {2, 1, 2}, 2}, 2]"//char(10), &
        "List(List(1, 2), List(2, 1), List(2, 3), List(3))")
    call expect("root list position is empty path", &
        "value = Position[{1, 2}, {1, 2}]"//char(10), &
        "List(List())")
    call expect("list position", &
        "value = Position[{{1, 2}}, {1, 2}]"//char(10), &
        "List(List(1))")
    call expect("position no match", &
        "value = Position[{1, 2}, 9]"//char(10), "List()")
    call expect("union numeric and symbols", &
        "value = Union[{b, 2, a, b}, {3, a, 2}]"//char(10), &
        "List(2, 3, a, b)")
    call expect("union empty input", &
        "value = Union[{}, {b, a}, {a}]"//char(10), &
        "List(a, b)")
    call expect_refusal("position non-list", &
        "value = Position[x, 1]"//char(10), &
        "Position: the expression must be an explicit list")
    call expect_refusal("union non-list", &
        "value = Union[x, {1}]"//char(10), &
        "Union: all arguments must be explicit lists")

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_position_union"
    else
        print *, "FAIL test_fortsym_wl_position_union:", nfail
        error stop 1
    end if

contains

    subroutine expect(label, source, expected)
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
    end subroutine expect

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

end program test_fortsym_wl_position_union
