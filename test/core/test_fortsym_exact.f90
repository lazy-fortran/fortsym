program test_fortsym_exact
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, chars
    use fortsym_exact, only: exact_normalize, exact_add, exact_sub, exact_mul, &
        exact_div, exact_pow, exact_uses_shared_flint
    use fortsym_exact, only: exact_uses_shared_mpfr
    use fortsym_exact, only: exact_to_real
    implicit none

    integer :: nfail = 0
    logical :: ok
    real(real64) :: projected

    call check_logical("FLINT is dynamically linked", exact_uses_shared_flint())
    call check_logical("MPFR is dynamically linked", exact_uses_shared_mpfr())

    projected = exact_to_real("0", ok)
    call check_logical("exact zero projects exactly", &
        ok .and. projected == 0.0_real64)
    projected = exact_to_real("1"//repeat("0", 400)//"/"//&
        ("1"//repeat("0", 399)//"1"), ok)
    call check_logical("scaled exact ratio projects nearest to one", &
        ok .and. projected == 1.0_real64)
    projected = exact_to_real("1/1"//repeat("0", 400), ok)
    call check_logical("subnormal exact projection is refused", .not. ok)

    block
        type(str_t) :: got
        character(:), allocatable :: oversized
        got = exact_add( &
            "100000000000000000000000000000000000000000000000000", &
            "99999999999999999999999999999999999999999999999999", ok)
        call check_result("large integer addition", got, ok, &
            "199999999999999999999999999999999999999999999999999")

        got = exact_mul("100000000000000000001", &
            "99999999999999999999", ok)
        call check_result("difference-of-squares multiplication", got, ok, &
            "9999999999999999999999999999999999999999")

        got = exact_normalize( &
            "600000000000000000000000000000000000000000000000000/"// &
            "800000000000000000000000000000000000000000000000000", ok)
        call check_result("large rational canonicalization", got, ok, "3/4")
        got = exact_normalize("6/-8", ok)
        call check_result("rational sign canonicalization", got, ok, "-3/4")

        got = exact_add("1/100000000000000000000", &
            "3/100000000000000000000", ok)
        call check_result("large rational addition", got, ok, &
            "1/25000000000000000000")

        got = exact_div("9999999999999999999999999999999999999999", &
            "99999999999999999999", ok)
        call check_result("large exact division", got, ok, &
            "100000000000000000001")

        got = exact_pow("100000000000000000001", 2_int64, ok)
        call check_result("large square", got, ok, &
            "10000000000000000000200000000000000000001")

        got = exact_pow("2", -3_int64, ok)
        call check_result("negative exact power", got, ok, "1/8")

        got = exact_sub("100000000000000000000", "1", ok)
        call check_result("exact subtraction", got, ok, &
            "99999999999999999999")

        got = exact_div("1", "0", ok)
        call expect_failure("division by zero is refused", got, ok)
        got = exact_normalize("1/0", ok)
        call expect_failure("zero denominator is refused", got, ok)
        got = exact_pow("0", -1_int64, ok)
        call expect_failure("negative power of zero is refused", got, ok)
        got = exact_pow("2", 1000001_int64, ok)
        call expect_failure("oversized exact power is refused", got, ok)
        got = exact_pow("10000000000000000000", 1000000_int64, ok)
        call expect_failure("oversized exact output is preflighted", got, ok)
        oversized = repeat("1", 1024*1024 + 1)
        got = exact_normalize(oversized, ok)
        call expect_failure("oversized exact input is refused", got, ok)
        got = exact_normalize("1"//achar(0)//"garbage", ok)
        call expect_failure("embedded NUL exact input is refused", got, ok)
        got = exact_normalize("12x", ok)
        call expect_failure("malformed exact value is refused", got, ok)
    end block

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_exact: all checks passed"

contains

    subroutine check_logical(label, condition)
        character(*), intent(in) :: label
        logical,      intent(in) :: condition

        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check_logical

    subroutine check_result(label, got, succeeded, want)
        character(*), intent(in) :: label, want
        type(str_t),   intent(in) :: got
        logical,       intent(in) :: succeeded

        if (.not. succeeded .or. chars(got) /= want) then
            nfail = nfail + 1
            print *, "FAIL ", label
            print *, "  got:  ", chars(got)
            print *, "  want: ", want
        end if
    end subroutine check_result

    subroutine expect_failure(label, got, succeeded)
        character(*), intent(in) :: label
        type(str_t),   intent(in) :: got
        logical,       intent(in) :: succeeded

        if (succeeded .or. len(chars(got)) /= 0) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine expect_failure

end program test_fortsym_exact
