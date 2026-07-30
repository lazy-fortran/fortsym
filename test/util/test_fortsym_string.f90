program test_fortsym_string
    ! Behavioural checks for str_t and strbuf_t. The oracle is what the string
    ! should contain, stated independently of how the type stores it.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: str_t, strbuf_t, str, chars, len_str, compare_str, &
        operator(//), operator(==), operator(/=), assignment(=)
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_construction()
    call test_concat()
    call test_compare()
    call test_query()
    call test_number_formatting()
    call test_real_roundtrip()
    call test_buffer_basics()
    call test_buffer_growth()
    call test_buffer_reset()

    if (nfail == 0) then
        print *, "test_fortsym_string: all checks passed"
    else
        print *, "test_fortsym_string: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    ! Compares length first. Fortran's intrinsic CHARACTER comparison blank-pads
    ! the shorter operand, so a bare got /= want would rate " " equal to "" --
    ! the exact confusion these tests exist to catch. The oracle must not share
    ! the defect it is testing for.
    subroutine check(label, got, want)
        character(*), intent(in) :: label, got, want
        logical :: same
        same = len(got) == len(want)
        if (same .and. len(got) > 0) same = got == want
        if (.not. same) then
            nfail = nfail + 1
            print *, "FAIL ", label
            print *, "      got  [", got, "] len", len(got)
            print *, "      want [", want, "] len", len(want)
        end if
    end subroutine check

    subroutine check_true(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check_true

    subroutine check_int(label, got, want)
        character(*), intent(in) :: label
        integer,      intent(in) :: got, want
        if (got /= want) then
            nfail = nfail + 1
            print *, "FAIL ", label, " got", got, " want", want
        end if
    end subroutine check_int

    subroutine test_construction()
        type(str_t) :: a, b

        a = str("hello")
        call check("construct from chars", chars(a), "hello")

        b = "assigned"
        call check("assign from chars", chars(b), "assigned")

        ! An untouched str_t reads as empty rather than tripping an allocation
        ! check, so callers never need an allocated() guard.
        block
            type(str_t) :: fresh
            call check("default is empty", chars(fresh), "")
            call check_int("default length", len_str(fresh), 0)
            call check_true("default is_empty", fresh%is_empty())
        end block

        ! Length is the content length exactly: no padding, so no TRIM.
        a = str("ab ")
        call check_int("trailing space is content", len_str(a), 3)
    end subroutine test_construction

    subroutine test_concat()
        type(str_t) :: a, b, c

        a = str("foo")
        b = str("bar")

        c = a//b
        call check("str // str", chars(c), "foobar")

        c = a//"baz"
        call check("str // chars", chars(c), "foobaz")

        c = "qux"//b
        call check("chars // str", chars(c), "quxbar")

        ! Concatenating empties must not introduce padding or drop content.
        c = str("")//a//str("")
        call check("concat with empties", chars(c), "foo")
    end subroutine test_concat

    subroutine test_compare()
        type(str_t) :: a, b

        a = str("same")
        b = str("same")
        call check_true("equal strings compare equal", a == b)
        call check_true("equal to chars", a == "same")
        call check_true("chars equal to str", "same" == a)

        b = str("different")
        call check_true("unequal strings differ", a /= b)
        call check_true("unequal to chars", a /= "different")

        ! The distinction fixed-length CHARACTER cannot make: padding is content.
        a = str("x")
        b = str("x ")
        call check_true("trailing space is significant", a /= b)

        call check_int("lexical equal", compare_str(str("x"), str("x")), 0)
        call check_int("lexical prefix first", &
            compare_str(str("x"), str("xx")), -1)
        call check_int("lexical longer prefix second", &
            compare_str(str("xx"), str("x")), 1)
        call check_int("lexical differing byte", &
            compare_str(str("alpha"), str("beta")), -1)
        block
            type(str_t) :: fresh
            call check_int("unallocated empty sorts before content", &
                compare_str(fresh, str("x")), -1)
            call check_int("two unallocated empties compare equal", &
                compare_str(fresh, fresh), 0)
        end block
    end subroutine test_compare

    subroutine test_query()
        type(str_t) :: a

        a = str("alpha_beta")

        call check_true("starts_with hit", a%starts_with("alpha"))
        call check_true("starts_with miss", .not. a%starts_with("beta"))
        call check_true("ends_with hit", a%ends_with("beta"))
        call check_true("ends_with miss", .not. a%ends_with("alpha"))

        ! A prefix longer than the string is a miss, not an out-of-bounds read.
        call check_true("starts_with overlong", &
            .not. a%starts_with("alpha_beta_gamma"))
        call check_true("ends_with overlong", &
            .not. a%ends_with("alpha_beta_gamma"))

        call check_int("index_of hit", a%index_of("_"), 6)
        call check_int("index_of miss", a%index_of("zz"), 0)

        call check("sub interior", chars(a%sub(1, 5)), "alpha")
        call check("sub to end", chars(a%sub(7, 10)), "beta")

        ! Out-of-range slices clamp rather than crash.
        call check("sub past end clamps", chars(a%sub(7, 999)), "beta")
        call check("sub before start clamps", chars(a%sub(-5, 5)), "alpha")
        call check("sub inverted is empty", chars(a%sub(8, 3)), "")
    end subroutine test_query

    subroutine test_number_formatting()
        ! i0 formatting, so no leading blanks survive into the value.
        call check("int zero", chars(str(0)), "0")
        call check("int positive", chars(str(42)), "42")
        call check("int negative", chars(str(-7)), "-7")
        call check("int large", chars(str(2147483647)), "2147483647")
        call check("int64 large", chars(str(9223372036854775807_int64)), &
            "9223372036854775807")
    end subroutine test_number_formatting

    subroutine test_real_roundtrip()
        ! The default real format must recover the exact double. Generated
        ! kernels have to reproduce the constants the CAS computed, so a lossy
        ! default would silently perturb every emitted literal.
        real(dp), parameter :: cases(*) = [ &
            0.0_dp, 1.0_dp, -1.0_dp, &
            0.1_dp, &
            3.14159265358979323846_dp, &
            2.718281828459045_dp, &
            1.0e-300_dp, 1.0e300_dp, &
            1.2345678901234567e-5_dp]
        real(dp) :: got
        integer  :: i, ios
        character(:), allocatable :: text

        do i = 1, size(cases)
            text = chars(str(cases(i)))
            read (text, *, iostat=ios) got
            if (ios /= 0) then
                nfail = nfail + 1
                print *, "FAIL real reparse failed for [", text, "]"
            else if (got /= cases(i)) then
                nfail = nfail + 1
                print *, "FAIL real round-trip [", text, "]"
                print *, "      got ", got, " want ", cases(i)
            end if
        end do

        ! An explicit format overrides the default.
        call check("explicit real format", chars(str(1.5_dp, '(f4.1)')), "1.5")
    end subroutine test_real_roundtrip

    subroutine test_buffer_basics()
        type(strbuf_t) :: b

        call check("empty buffer", b%chars(), "")
        call check_int("empty buffer length", b%len(), 0)

        call b%append("abc")
        call b%append(str("def"))
        call b%append(42)
        call check("buffer mixed appends", b%chars(), "abcdef42")
        call check_int("buffer length", b%len(), 8)

        call check("to_str matches chars", chars(b%to_str()), b%chars())

        ! Appending nothing must not disturb the contents.
        call b%append("")
        call check("append empty is a no-op", b%chars(), "abcdef42")
    end subroutine test_buffer_basics

    subroutine test_buffer_growth()
        type(strbuf_t) :: b
        character(:), allocatable :: want
        integer :: i

        ! Cross the initial capacity many times over. This is the case that
        ! quadratic concatenation would make painful and that must stay correct
        ! across every reallocation.
        want = ""
        do i = 1, 500
            call b%append("0123456789")
            want = want//"0123456789"
        end do

        call check_int("grown buffer length", b%len(), 5000)
        call check_true("grown buffer content", b%chars() == want)

        ! Spare capacity must never leak into the result.
        call check_int("no padding in result", len(b%chars()), 5000)
    end subroutine test_buffer_growth

    subroutine test_buffer_reset()
        type(strbuf_t) :: b

        call b%append("discarded")
        call b%reset()
        call check("reset empties", b%chars(), "")
        call check_int("reset zeroes length", b%len(), 0)

        ! Reusing after reset must produce only the new content.
        call b%append("fresh")
        call check("reuse after reset", b%chars(), "fresh")
    end subroutine test_buffer_reset

end program test_fortsym_string
