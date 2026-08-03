program test_fortsym_wl_levicivita
    ! Independent behavioral regression for the bounded rank-three tensor.
    ! Antisymmetry and the six nonzero unit entries are the oracle; the test
    ! does not compare against a tensor serialized by the evaluator.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_INT
    use fortsym_expr, only: expr_t
    use fortsym_wl, only: wl_binding_at, wl_binding_count, wl_binding_t, &
        wl_run_source, wl_session_begin, wl_session_t
    implicit none

    type(arena_t), target :: a
    type(wl_session_t) :: s
    type(expr_t) :: epsilon, slice, reverse, values
    integer :: i, j, k, nonzero

    call a%init()
    call wl_session_begin(s, a)
    call wl_run_source(s, "epsilon = LeviCivitaTensor[3]"//char(10)// &
        "slice = epsilon[[1, 2, 3]]"//char(10)// &
        "reverse = epsilon[[1, 3, 2]]"//char(10)// &
        "values = Table[epsilon[[i, j, k]], {i, 3}, {j, 3}, {k, 3}]"//char(10))
    epsilon = value_for(s, "epsilon")
    slice = value_for(s, "slice")
    reverse = value_for(s, "reverse")
    values = value_for(s, "values")

    if (epsilon%kind() /= NK_FUNC) then
        print *, "FAIL test_fortsym_wl_levicivita: generic tensor changed"
        error stop 1
    end if
    if (chars(epsilon%name()) /= "LeviCivitaTensor") then
        print *, "FAIL test_fortsym_wl_levicivita: generic tensor changed"
        error stop 1
    end if
    if (epsilon%nargs() /= 1) then
        print *, "FAIL test_fortsym_wl_levicivita: generic tensor changed"
        error stop 1
    end if
    if (slice%kind() /= NK_INT .or. slice%int_value() /= 1) then
        print *, "FAIL test_fortsym_wl_levicivita: epsilon123"
        error stop 1
    end if
    if (reverse%kind() /= NK_INT .or. reverse%int_value() /= -1) then
        print *, "FAIL test_fortsym_wl_levicivita: epsilon132"
        error stop 1
    end if
    epsilon = values
    nonzero = 0
    do i = 1, 3
        do j = 1, 3
            do k = 1, 3
                if (integer_at(epsilon, i, j, k) /= 0) then
                    nonzero = nonzero + 1
                end if
                if (i == j .or. i == k .or. j == k) then
                    if (integer_at(epsilon, i, j, k) /= 0) then
                        print *, "FAIL test_fortsym_wl_levicivita: repeated index"
                        error stop 1
                    end if
                else if (integer_at(epsilon, i, j, k)**2 /= 1) then
                    print *, "FAIL test_fortsym_wl_levicivita: permutation magnitude"
                    error stop 1
                end if
            end do
        end do
    end do
    if (nonzero /= 6 .or. integer_at(epsilon, 1, 2, 3) /= 1 .or. &
            integer_at(epsilon, 1, 3, 2) /= -1) then
        print *, "FAIL test_fortsym_wl_levicivita: antisymmetry invariant"
        error stop 1
    end if
    print *, "PASS test_fortsym_wl_levicivita"

contains

    function value_for(session, name) result(value)
        type(wl_session_t), intent(in) :: session
        character(*), intent(in) :: name
        type(expr_t) :: value
        type(wl_binding_t) :: binding
        integer :: n

        value%id = 0
        do n = 1, wl_binding_count(session)
            binding = wl_binding_at(session, n)
            if (chars(binding%name) /= name) cycle
            if (.not. binding%ok) then
                print *, "FAIL test_fortsym_wl_levicivita: ", &
                    chars(binding%message)
                error stop 1
            end if
            value = binding%value
            return
        end do
        print *, "FAIL test_fortsym_wl_levicivita: missing binding ", name
        error stop 1
    end function value_for

    function integer_at(expression, i, j, k) result(value)
        type(expr_t), intent(in) :: expression
        integer, intent(in) :: i, j, k
        integer :: value
        type(expr_t) :: plane, row, entry

        plane = expression%arg(i)
        row = plane%arg(j)
        entry = row%arg(k)
        if (entry%kind() /= NK_INT) then
            print *, "FAIL test_fortsym_wl_levicivita: non-integer entry"
            error stop 1
        end if
        value = int(entry%int_value())
    end function integer_at

end program test_fortsym_wl_levicivita
