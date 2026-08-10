program test_fortsym_exact_padding
    ! Surrounding blanks must not change an exact value.
    !
    ! Fortran callers build these strings in fixed-length arrays, so "0" often
    ! arrives as "0" followed by spaces. Whether FLINT's parser accepts that is
    ! a property of the build: on one it works, on another the call fails and
    ! the wrapper hands back an empty string that every later call rejects as
    ! "not an exact rational". A value's meaning must not depend on how the
    ! caller declared its buffer, so the oracle here is the trimmed form.
    use fortsym_string, only: str_t, chars
    use fortsym_exact
    implicit none

    integer :: n_pass, n_fail
    character(len=16) :: padded(3)

    n_pass = 0
    n_fail = 0
    padded = [character(len=16) :: "0", "3/40", "-9/10"]

    call same_as_trimmed(padded(1), "0")
    call same_as_trimmed(padded(2), "3/40")
    call same_as_trimmed(padded(3), "-9/10")
    call test_arithmetic_on_padded()
    call test_blank_is_rejected()

    write (*, "(a,i0,a,i0,a)") "test_fortsym_exact_padding: ", n_pass, &
        " passed, ", n_fail, " failed"
    if (n_fail > 0) error stop 1

contains

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (condition) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (*, "(a)") "  FAIL: "//label
        end if
    end subroutine ok

    subroutine same_as_trimmed(padded_text, tight)
        character(*), intent(in) :: padded_text, tight
        type(str_t) :: a, b
        logical :: sa, sb

        a = exact_normalize(padded_text, sa)
        b = exact_normalize(tight, sb)
        call ok("padded '"//tight//"' normalises", sa)
        call ok("tight '"//tight//"' normalises", sb)
        call ok("padded and tight '"//tight//"' agree", &
                sa .and. sb .and. chars(a) == chars(b))
    end subroutine same_as_trimmed

    !> The failure that reached CI: subtracting two padded zeros produced a
    !> value later calls could not read back.
    subroutine test_arithmetic_on_padded()
        type(str_t) :: difference, canonical
        logical :: s1, s2
        real(kind(1.0d0)) :: value
        logical :: s3

        difference = exact_sub(padded(1), padded(1), s1)
        call ok("padded zero minus padded zero succeeds", s1)
        canonical = exact_normalize(chars(difference), s2)
        call ok("the difference normalises", s2)
        call ok("the difference is zero", &
                s2 .and. chars(canonical) == chars(exact_normalize("0", s1)))
        value = exact_to_real(chars(difference), s3)
        call ok("the difference converts to a real", s3)
        call ok("and that real is zero", s3 .and. value == 0.0d0)
    end subroutine test_arithmetic_on_padded

    subroutine test_blank_is_rejected()
        type(str_t) :: value
        logical :: s

        value = exact_normalize("     ", s)
        call ok("an all-blank string is rejected, not sent as an empty token", &
                .not. s)
    end subroutine test_blank_is_rejected

end program test_fortsym_exact_padding
