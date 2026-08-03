program test_fortsym_wl_radial_derivative
    ! Independent regression for second derivatives inside a mapped pure
    ! function.  The expected expression is hand-derived from
    ! d^2(a0 + a1 t + a2 t^2 + a3 t^3)/dt^2 = 2 a2 + 6 a3 t.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_print, only: print_expr
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    type(arena_t), target :: a
    type(wl_session_t) :: s
    type(wl_binding_t) :: b
    character(:), allocatable :: actual
    integer :: k
    logical :: found

    call a%init()
    call wl_session_begin(s, a)
    call wl_run_source(s, &
        "polynomial[t_] = a0 + a1*t + a2*t^2 + a3*t^3"//char(10)// &
        "points = {0, h1, h1+h2, h1+h2+h3}"//char(10)// &
        "second = (D[polynomial[t], {t, 2}] /. t -> #) & /@ points"// &
        char(10))

    found = .false.
    do k = 1, wl_binding_count(s)
        b = wl_binding_at(s, k)
        if (chars(b%name) /= "second") cycle
        found = .true.
        if (.not. b%ok) then
            print *, "FAIL radial derivative: refused: ", chars(b%message)
            error stop 1
        end if
        actual = chars(print_expr(b%value))
        if (actual /= &
                "List(a2*2, a2*2 + a3*h1*6, "// &
                "a2*2 + a3*(h1 + h2)*6, "// &
                "a2*2 + a3*(h1 + h2 + h3)*6)") then
            print *, "FAIL radial derivative: got ", actual
            error stop 1
        end if
    end do
    if (.not. found) then
        print *, "FAIL radial derivative: no second binding"
        error stop 1
    end if
    print *, "PASS test_fortsym_wl_radial_derivative"
end program test_fortsym_wl_radial_derivative
