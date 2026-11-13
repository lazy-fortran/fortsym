program test_fortsym_wl_delegates
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use fortsym_arena, only: arena_t
    use fortsym_capi, only: fsym_delegate_counts, FSYM_DELEGATE_FLINT_EXACT, &
        FSYM_DELEGATE_FLINT_ALGEBRAIC, FSYM_DELEGATE_SYMENGINE_EVAL
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_print, only: print_expr_in
    use fortsym_string, only: chars
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_t, wl_binding_count, wl_binding_at
    implicit none
    type(arena_t), target :: arena
    type(wl_session_t) :: session
    character(32) :: mode
    integer(c_int64_t) :: attempts, successes
    integer(c_int) :: rc
    logical :: counted

    call get_command_argument(1, mode)
    if (len_trim(mode) == 0) mode = "default"
    counted = trim(mode) /= "default"
    call arena%init()
    call wl_session_begin(session, arena)
    call wl_run_source(session, "small = 2 + 3"//achar(10))
    call expect("small", "5", .true.)
    call counts(FSYM_DELEGATE_FLINT_EXACT, .false.)
    call counts(FSYM_DELEGATE_FLINT_ALGEBRAIC, .false.)
    call counts(FSYM_DELEGATE_SYMENGINE_EVAL, .false.)

    call wl_run_source(session, "large = 2^80"//achar(10))
    if (trim(mode) == "exact-off") then
        call expect("large", "2^80", .true.)
        call counts(FSYM_DELEGATE_FLINT_EXACT, .true., .true.)
    else
        call expect("large", "1208925819614629174706176", .true.)
        call counts(FSYM_DELEGATE_FLINT_EXACT, counted)
    end if
    call wl_run_source(session, "precision = N[Pi,50]"//achar(10))
    if (trim(mode) == "eval-off") then
        call expect("precision", "", .false.)
        call counts(FSYM_DELEGATE_SYMENGINE_EVAL, .true., .true.)
    else
        ! Requested precision is a lower bound; the MPFR printer retains guard digits.
        call expect("precision", "3.1415926535897932384626433832795028841971693993751", .true., .true.)
        call counts(FSYM_DELEGATE_SYMENGINE_EVAL, counted)
    end if

contains

    subroutine expect(name, text, good, prefix)
        character(*), intent(in) :: name, text
        logical, intent(in) :: good
        logical, optional, intent(in) :: prefix
        type(wl_binding_t) :: binding
        character(:), allocatable :: rendered
        integer :: k
        logical :: matches
        do k = 1, wl_binding_count(session)
            binding = wl_binding_at(session, k)
            if (chars(binding%name) /= name) cycle
            if (binding%ok .neqv. good) error stop "unexpected binding status"
            if (good) then
                rendered = chars(print_expr_in(binding%value, dialect(DIA_WOLFRAM)))
                matches = rendered == text
                if (present(prefix)) then
                    if (prefix) matches = index(rendered, text) == 1
                end if
                if (.not. matches) then
                    print *, name, ": ", rendered
                    error stop "unexpected binding value"
                end if
                print '(a)', name//"="//rendered
            else
                if (index(chars(binding%message), "disabled by experiment policy") == 0) &
                    error stop "disabled evaluation must give an explicit refusal"
            end if
            return
        end do
        error stop "missing binding"
    end subroutine expect

    subroutine counts(group, nonzero, refused)
        integer(c_int), intent(in) :: group
        logical, intent(in) :: nonzero
        logical, optional, intent(in) :: refused
        rc = fsym_delegate_counts(group, attempts, successes)
        if (rc /= 1_c_int) error stop "counter query failed"
        if (nonzero .neqv. (attempts > 0_c_int64_t)) error stop "unexpected attempt count"
        if (present(refused)) then
            if (refused .and. successes /= 0_c_int64_t) error stop "refused call counted as success"
        else
            if (nonzero .neqv. (successes > 0_c_int64_t)) error stop "unexpected success count"
        end if
    end subroutine counts
end program test_fortsym_wl_delegates
