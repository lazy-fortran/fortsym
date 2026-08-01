program test_fortsym_numeric
    ! N[...] checked against values that are known without running fortsym.
    !
    ! The oracles here are independent of the implementation: sin(pi/6) is one
    ! half, exp(0) is one, 1/3 + 1/6 is one half by hand, and 2**40 is a number
    ! written out in full below. The text form is checked by the only property
    ! that matters for it -- reading the string back must reproduce the double
    ! bit for bit -- which is verified with a plain READ, not by comparing
    ! against a string the test author typed.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, rat, pi_expr, &
                            sin, exp, log, sqrt, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**)
    use fortsym_numeric, only: numeric_value, numeric_text
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target :: arena
    integer :: nfail = 0

    call arena%init()

    call test_exact_integer()
    call test_exact_rational()
    call test_transcendental_oracles()
    call test_free_symbol_refused()
    call test_pole_refused()
    call test_unknown_head_refused()
    call test_text_round_trips()
    call test_text_is_short()
    call test_text_refuses_when_value_does()

    if (nfail /= 0) then
        print *, "test_fortsym_numeric: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_numeric: all checks passed"

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (cond) then
            print *, "  ok   ", label
        else
            print *, "  FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine ok

    !> Integers below 2**53 are exactly representable, so any rounding at all
    !> would show up as inequality here.
    subroutine test_exact_integer()
        type(expr_t) :: e
        real(dp) :: v
        logical  :: good
        character(:), allocatable :: why

        e = num(arena, 1099511627776_int64)   ! 2**40
        call numeric_value(e, v, good, why)
        call ok("2**40 evaluates", good)
        call ok("2**40 is exact", v == 1099511627776.0_dp)

        e = num(arena, -7) * num(arena, 6)
        call numeric_value(e, v, good, why)
        call ok("-7*6 is exact", good .and. v == -42.0_dp)
    end subroutine test_exact_integer

    !> 1/3 + 1/6 = 1/2 by hand, and one half is exact in binary, so the sum must
    !> land on it exactly despite neither summand being representable.
    subroutine test_exact_rational()
        type(expr_t) :: e
        real(dp) :: v
        logical  :: good
        character(:), allocatable :: why

        e = rat(arena, 1_int64, 3_int64) + rat(arena, 1_int64, 6_int64)
        call numeric_value(e, v, good, why)
        call ok("1/3 + 1/6 evaluates", good)
        call ok("1/3 + 1/6 is exactly 1/2", v == 0.5_dp)

        e = rat(arena, 3_int64, 4_int64)
        call numeric_value(e, v, good, why)
        call ok("3/4 is exactly 0.75", good .and. v == 0.75_dp)
    end subroutine test_exact_rational

    !> exp(0) is exactly one. sin(pi/6) is one half in exact arithmetic; the
    !> double it must land near is set by the rounding of pi itself, so it is
    !> compared to within a few ulp rather than exactly.
    subroutine test_transcendental_oracles()
        type(expr_t) :: e
        real(dp) :: v
        logical  :: good
        character(:), allocatable :: why

        e = exp(num(arena, 0))
        call numeric_value(e, v, good, why)
        call ok("exp(0) is exactly 1", good .and. v == 1.0_dp)

        e = sin(pi_expr(arena)/num(arena, 6))
        call numeric_value(e, v, good, why)
        call ok("sin(pi/6) evaluates", good)
        call ok("sin(pi/6) is 1/2", abs(v - 0.5_dp) < 1.0e-15_dp)

        e = log(exp(num(arena, 3)))
        call numeric_value(e, v, good, why)
        call ok("log(exp(3)) is 3", good .and. abs(v - 3.0_dp) < 1.0e-14_dp)

        e = sqrt(num(arena, 2))**num(arena, 2)
        call numeric_value(e, v, good, why)
        call ok("sqrt(2)**2 is 2", good .and. abs(v - 2.0_dp) < 1.0e-15_dp)
    end subroutine test_transcendental_oracles

    subroutine test_free_symbol_refused()
        type(expr_t) :: e
        real(dp) :: v
        logical  :: good
        character(:), allocatable :: why

        e = sym(arena, "x") + num(arena, 1)
        call numeric_value(e, v, good, why)
        call ok("x + 1 is refused", .not. good)
        call ok("refusal names the symbol", index(why, "symbol x") > 0)
        call ok("refused value is not passed off as 0", .not. good)

        e = sym(arena, "alpha")*num(arena, 0)
        call numeric_value(e, v, good, why)
        call ok("alpha*0 is refused rather than assumed 0", .not. good)
        call ok("refusal names alpha", index(why, "symbol alpha") > 0)
    end subroutine test_free_symbol_refused

    !> 0**(-1) is a pole. The failure mode this guards is returning huge() or an
    !> IEEE infinity, either of which reads as an ordinary finite answer.
    subroutine test_pole_refused()
        type(expr_t) :: e
        real(dp) :: v
        logical  :: good
        character(:), allocatable :: why

        e = num(arena, 0)**num(arena, -1)
        call numeric_value(e, v, good, why)
        call ok("0**(-1) is refused", .not. good)
        call ok("pole refusal explains itself", len(why) > 0)

        e = log(num(arena, 0))
        call numeric_value(e, v, good, why)
        call ok("log(0) is refused", .not. good)

        e = sqrt(num(arena, -1))
        call numeric_value(e, v, good, why)
        call ok("sqrt(-1) is refused by a real N", .not. good)
    end subroutine test_pole_refused

    subroutine test_unknown_head_refused()
        use fortsym_expr, only: func
        type(expr_t) :: e
        real(dp) :: v
        logical  :: good
        character(:), allocatable :: why

        e = func("zeta", [num(arena, 2)])
        call numeric_value(e, v, good, why)
        call ok("unknown head zeta is refused", .not. good)
    end subroutine test_unknown_head_refused

    !> The defining property of numeric_text: the string reads back as the same
    !> double. Checked by an independent READ, on values of very different size.
    subroutine test_text_round_trips()
        type(expr_t) :: e
        real(dp) :: v, back
        logical  :: good, tgood
        integer  :: ios, k
        character(:), allocatable :: why, text
        integer(int64), parameter :: numer(6) = [1_int64, 2_int64, &
            22_int64, -355_int64, 1_int64, 987654321_int64]
        integer(int64), parameter :: denom(6) = [3_int64, 1_int64, &
            7_int64, 113_int64, 1000000_int64, 1000_int64]

        do k = 1, 6
            e = rat(arena, numer(k), denom(k))
            call numeric_value(e, v, good, why)
            text = numeric_text(e, tgood, why)
            call ok("text produced", good .and. tgood)
            back = 0.0_dp
            read (text, *, iostat=ios) back
            call ok("text parses: "//text, ios == 0)
            call ok("text round-trips: "//text, back == v)
        end do

        e = sin(num(arena, 1))
        call numeric_value(e, v, good, why)
        text = numeric_text(e, tgood, why)
        back = 0.0_dp
        read (text, *, iostat=ios) back
        call ok("sin(1) text round-trips", tgood .and. ios == 0 .and. back == v)
    end subroutine test_text_round_trips

    !> Shortest means no fewer significant digits would also round-trip. Checked
    !> against an independently formatted candidate with one digit fewer.
    subroutine test_text_is_short()
        type(expr_t) :: e
        real(dp) :: v, back
        logical  :: good, tgood
        integer  :: ios, d
        character(:), allocatable :: why, text
        character(len=64) :: buf
        character(len=32) :: fmt

        e = rat(arena, 1_int64, 2_int64)
        call numeric_value(e, v, good, why)
        text = numeric_text(e, tgood, why)
        call ok("1/2 has a text form", good .and. tgood)

        ! One significant digit suffices for 0.5, so anything longer than a
        ! single-digit mantissa would mean the search is not minimal.
        call ok("1/2 text is minimal length", count_digits(text) == 1)

        ! For an awkward value, every strictly shorter form must fail to read
        ! back -- that is what minimality means, tested without trusting the
        ! implementation's own search.
        e = rat(arena, 1_int64, 3_int64)
        call numeric_value(e, v, good, why)
        text = numeric_text(e, tgood, why)
        d = count_digits(text) - 1
        if (d >= 1) then
            write (fmt, '("(es40.",i0,"e3)")') d - 1
            write (buf, fmt) v
            back = 0.0_dp
            read (buf, *, iostat=ios) back
            call ok("no shorter form of 1/3 round-trips", &
                    ios /= 0 .or. back /= v)
        else
            call ok("1/3 text has at least two digits", .false.)
        end if
    end subroutine test_text_is_short

    subroutine test_text_refuses_when_value_does()
        type(expr_t) :: e
        logical  :: tgood
        character(:), allocatable :: why, text

        e = sym(arena, "y")
        text = numeric_text(e, tgood, why)
        call ok("text refuses a free symbol", .not. tgood)
        call ok("text refusal names y", index(why, "symbol y") > 0)
        call ok("refused text is empty", len(text) == 0)
    end subroutine test_text_refuses_when_value_does

    !> Significant digits in a scientific-form string: mantissa digits before
    !> the exponent marker.
    function count_digits(text) result(n)
        character(*), intent(in) :: text
        integer :: n
        integer :: k, stop_at

        stop_at = index(text, "E")
        if (stop_at == 0) stop_at = len(text) + 1
        n = 0
        do k = 1, stop_at - 1
            if (text(k:k) >= "0" .and. text(k:k) <= "9") n = n + 1
        end do
    end function count_digits

end program test_fortsym_numeric
