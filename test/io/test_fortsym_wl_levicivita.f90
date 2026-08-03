program test_fortsym_wl_levicivita
    ! Independent behavioral regression for the rank-three Levi-Civita tensor.
    ! The checks use antisymmetry and the invariant sum epsilon^2 = 3! rather
    ! than comparing against a serialized tensor produced by fortsym itself.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_INT
    use fortsym_expr, only: expr_t
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    type(arena_t), target :: a
    type(wl_session_t) :: s
    type(wl_binding_t) :: binding
    type(expr_t) :: value, plane, row, entry, slice, table_value, scoped
    integer :: i, j, k, total, failures

    failures = 0
    call a%init()
    call wl_session_begin(s, a)
    call wl_run_source(s, "epsilon = LeviCivitaTensor[3]"//char(10))
    value = value_for(s, "epsilon")
    call wl_run_source(s, "slice = LeviCivitaTensor[3][[1, 2, 3]]"//char(10))
    slice = value_for(s, "slice")
    if (slice%kind() /= NK_INT .or. slice%int_value() /= 1) then
        print *, "FAIL LeviCivitaTensor: direct Part did not select epsilon123"
        error stop 1
    end if
    call wl_run_source(s, "tableValue = Table[LeviCivitaTensor[3][[i, j, k]], "// &
        "{i, 3}, {j, 3}, {k, 3}]"//char(10))
    table_value = value_for(s, "tableValue")
    if (table_value%kind() /= NK_FUNC .or. chars(table_value%name()) /= "List" .or. &
        table_value%nargs() /= 3) then
        print *, "FAIL LeviCivitaTensor: Table did not retain tensor shape"
        error stop 1
    end if
    call wl_run_source(s, "scoped = Module[{local = {4, 5, 6}}, "// &
        "local[[2]]]"//char(10))
    scoped = value_for(s, "scoped")
    if (scoped%kind() /= NK_INT .or. scoped%int_value() /= 5) then
        print *, "FAIL LeviCivitaTensor: Module local Part failed"
        error stop 1
    end if

    if (value%kind() /= NK_FUNC .or. chars(value%name()) /= "List" .or. &
        value%nargs() /= 3) then
        print *, "FAIL LeviCivitaTensor: wrong rank-three shape"
        error stop 1
    end if

    total = 0
    do i = 1, 3
        do j = 1, 3
            do k = 1, 3
                plane = value%arg(i)
                row = plane%arg(j)
                entry = row%arg(k)
                if (.not. integer_value(entry, total)) then
                    print *, "FAIL LeviCivitaTensor: non-integer entry"
                    error stop 1
                end if
                if (i == j .or. i == k .or. j == k) then
                    if (total /= 0) failures = failures + 1
                else
                    total = total*total
                    if (total /= 1) failures = failures + 1
                    total = 1
                end if
            end do
        end do
    end do

    if (integer_at(value, 1, 2, 3) /= 1) failures = failures + 1
    if (integer_at(value, 1, 3, 2) /= -1) failures = failures + 1
    total = 0
    do i = 1, 3
        do j = 1, 3
            do k = 1, 3
                total = total + integer_at(value, i, j, k)**2
            end do
        end do
    end do
    if (total /= 6) failures = failures + 1

    if (failures /= 0) then
        print *, "FAIL LeviCivitaTensor:", failures, " invariant checks"
        error stop 1
    end if
    print *, "PASS test_fortsym_wl_levicivita"

contains

    function value_for(session, name) result(result_value)
        type(wl_session_t), intent(in) :: session
        character(*),        intent(in) :: name
        type(expr_t)                    :: result_value
        integer                         :: n

        result_value%id = 0
        do n = 1, wl_binding_count(session)
            binding = wl_binding_at(session, n)
            if (chars(binding%name) == name) then
                if (.not. binding%ok) then
                    print *, "FAIL LeviCivitaTensor: ", chars(binding%message)
                    error stop 1
                end if
                result_value = binding%value
                return
            end if
        end do
        print *, "FAIL LeviCivitaTensor: missing binding"
        error stop 1
    end function value_for

    function integer_value(expression, result_value) result(ok)
        type(expr_t), intent(in)  :: expression
        integer,      intent(out) :: result_value
        logical                   :: ok

        ok = expression%kind() == NK_INT
        if (ok) then
            result_value = int(expression%int_value(), kind(result_value))
        else
            result_value = 0
        end if
    end function integer_value

    function integer_at(expression, i, j, k) result(result_value)
        type(expr_t), intent(in) :: expression
        integer,      intent(in) :: i, j, k
        integer                  :: result_value

        block
            type(expr_t) :: plane, row, entry
            plane = expression%arg(i)
            row = plane%arg(j)
            entry = row%arg(k)
            result_value = int(entry%int_value(), kind(result_value))
        end block
    end function integer_at

end program test_fortsym_wl_levicivita
