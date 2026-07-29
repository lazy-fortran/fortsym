program test_fortsym_roundtrip
    ! Printer and parser must agree, in every dialect.
    !
    ! The oracle is the arena itself: print an expression, parse the text back,
    ! and require the same node index. Hash-consing makes that a complete
    ! structural check -- equal indices mean equal trees -- so this catches a
    ! dropped parenthesis, a mis-mapped function name or a lost sign without
    ! anyone having to write down the expected string.
    !
    ! Fortran is excluded from the round trip on purpose: it prints pi as a
    ! literal and rationals as typed quotients, both of which are deliberately
    ! lossy. Its output is checked separately, against what a compiler must see.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_dialect, only: dialect_t, dialect, DIA_NATIVE, DIA_SYMENGINE, &
        DIA_YACAS, DIA_SYMPY, DIA_MAXIMA, DIA_FORTRAN
    use fortsym_print, only: print_expr, print_expr_in
    use fortsym_parse, only: parse_expr_in
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_printing_shape()
    call test_roundtrip_all_dialects()
    call test_dialect_spellings()
    call test_fortran_emission()
    call test_parse_errors()

    if (nfail == 0) then
        print *, "test_fortsym_roundtrip: all checks passed"
    else
        print *, "test_fortsym_roundtrip: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    subroutine eq_text(label, got, want)
        character(*), intent(in) :: label, got, want
        logical :: same
        same = len(got) == len(want)
        if (same .and. len(got) > 0) same = got == want
        if (.not. same) then
            nfail = nfail + 1
            print *, "FAIL ", label
            print *, "      got  [", got, "]"
            print *, "      want [", want, "]"
        end if
    end subroutine eq_text

    !> The printer has to undo the arena's normal form. These pin the cases
    !> where a naive printer would emit "+ -1*" or "x*y**(-1)".
    subroutine test_printing_shape()
        type(arena_t), target :: a
        type(expr_t) :: x, y

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")

        call eq_check("symbol", print_text(x), "x")
        call eq_check("sum", print_text(x + y), "x + y")
        call eq_check("product", print_text(x*y), "x*y")

        ! Subtraction is stored as addition of a negation; it must read back as
        ! a subtraction rather than "x + -1*y".
        call eq_check("difference", print_text(x - y), "x - y")

        ! Division is stored as a reciprocal power; it must read back as "/".
        call eq_check("quotient", print_text(x/y), "x/y")

        call eq_check("power", print_text(x**2), "x**2")

        ! Precedence: a sum inside a product needs parentheses, a product inside
        ! a sum does not.
        call eq_check("sum in product", print_text((x + y)*x), "x*(x + y)")
        call eq_check("product in sum", print_text(x*y + x), "x + x*y")

        ! Unary minus, not "-1*x".
        call eq_check("negation", print_text(-x), "-x")
        call eq_check("multiple numeric signs", &
            print_text((-2)*(-1)*x), "x*2")

        ! A compound base keeps its parentheses under a power.
        call eq_check("compound base", print_text((x + y)**2), "(x + y)**2")

        ! Exponentiation is right-associative, so no parentheses are needed and
        ! adding them would change nothing but adding them on the left would.
        call eq_check("right assoc power", print_text(x**y**x), "x**y**x")

        call eq_check("function", print_text(sin(x)), "sin(x)")
        call eq_check("two-arg function", print_text(atan2(y, x)), "atan2(y, x)")
        call eq_check("Bessel function", print_text(besselj(1, x)), &
            "besselj(1, x)")
    end subroutine test_printing_shape

    subroutine eq_check(label, got, want)
        character(*), intent(in) :: label, got, want
        call eq_text(label, got, want)
    end subroutine eq_check

    function print_text(e) result(s)
        type(expr_t), intent(in)  :: e
        character(:), allocatable :: s
        s = chars(print_expr(e))
    end function print_text

    !> Print then parse then compare node indices, for every dialect that is
    !> meant to be lossless.
    subroutine test_roundtrip_all_dialects()
        integer, parameter :: dialects(*) = [DIA_NATIVE, DIA_SYMENGINE, &
            DIA_YACAS, DIA_SYMPY, DIA_MAXIMA]
        integer :: k

        do k = 1, size(dialects)
            call roundtrip_corpus(dialects(k))
        end do
    end subroutine test_roundtrip_all_dialects

    subroutine roundtrip_corpus(dia)
        integer, intent(in) :: dia
        type(arena_t), target :: a
        type(dialect_t) :: d
        type(expr_t) :: x, y, z, cases(20)
        integer :: k, n

        call a%init()
        d = dialect(dia)
        x = sym(a, "x")
        y = sym(a, "y")
        z = sym(a, "zeta")

        n = 0
        n = n + 1; cases(n) = x
        n = n + 1; cases(n) = num(a, 42)
        n = n + 1; cases(n) = num(a, -7)
        n = n + 1; cases(n) = rat(a, 3_int64, 4_int64)
        n = n + 1; cases(n) = real_expr(a, 1.5_dp)
        n = n + 1; cases(n) = x + y
        n = n + 1; cases(n) = x - y
        n = n + 1; cases(n) = x*y
        n = n + 1; cases(n) = x/y
        n = n + 1; cases(n) = x**2
        n = n + 1; cases(n) = x**y
        n = n + 1; cases(n) = -x
        n = n + 1; cases(n) = (x + y)*z
        n = n + 1; cases(n) = (x + y)**2
        n = n + 1; cases(n) = sin(x*y)**2 + cos(x*y)
        n = n + 1; cases(n) = atan2(y, x)
        n = n + 1; cases(n) = exp(x)/(1 + log(y))
        n = n + 1; cases(n) = sqrt(x**2 + y**2)
        n = n + 1; cases(n) = pi_expr(a)*x
        n = n + 1; cases(n) = x*y*z - 3*x/(y + 1)

        do k = 1, n
            call check_roundtrip(a, d, cases(k), k)
        end do
    end subroutine roundtrip_corpus

    subroutine check_roundtrip(a, d, e, k)
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t),          intent(in)    :: e
        integer,               intent(in)    :: k
        character(:), allocatable :: text, message
        type(expr_t) :: back
        logical :: good
        character(len=8) :: kbuf

        text = chars(print_expr_in(e, d))
        back = parse_expr_in(a, text, d, good, message)

        write (kbuf, '(i0)') k

        if (.not. good) then
            nfail = nfail + 1
            print *, "FAIL parse ", chars(d%name), " case ", kbuf(1:len_of(k))
            print *, "      text [", text, "]"
            print *, "      why  ", message
            return
        end if

        ! Structural identity, courtesy of hash-consing.
        if (.not. (back == e)) then
            nfail = nfail + 1
            print *, "FAIL roundtrip ", chars(d%name), " case ", kbuf(1:len_of(k))
            print *, "      text [", text, "]"
            print *, "      reprinted [", chars(print_expr_in(back, d)), "]"
        end if
    end subroutine check_roundtrip

    pure function len_of(k) result(n)
        integer, intent(in) :: k
        integer             :: n
        n = 1
        do while (10**n <= k)
            n = n + 1
        end do
    end function len_of

    !> The spellings that actually differ between engines. These are the ones a
    !> mistake makes plausible-looking but wrong.
    subroutine test_dialect_spellings()
        type(arena_t), target :: a
        type(expr_t) :: x

        call a%init()
        x = sym(a, "x")

        ! Power operator.
        call eq_text("yacas power", &
            chars(print_expr_in(x**2, dialect(DIA_YACAS))), "x^2")
        call eq_text("maxima power", &
            chars(print_expr_in(x**2, dialect(DIA_MAXIMA))), "x^2")
        call eq_text("sympy power", &
            chars(print_expr_in(x**2, dialect(DIA_SYMPY))), "x**2")

        ! Yacas capitalises, and its natural logarithm is Ln -- Log would be a
        ! different function.
        call eq_text("yacas sin", &
            chars(print_expr_in(sin(x), dialect(DIA_YACAS))), "Sin(x)")
        call eq_text("yacas log is Ln", &
            chars(print_expr_in(log(x), dialect(DIA_YACAS))), "Ln(x)")
        call eq_text("yacas asin is ArcSin", &
            chars(print_expr_in(asin(x), dialect(DIA_YACAS))), &
            "ArcSin(x)")

        ! SymPy's absolute value is a class name.
        call eq_text("sympy Abs", &
            chars(print_expr_in(abs(x), dialect(DIA_SYMPY))), "Abs(x)")
        call eq_text("Fortran Bessel intrinsic", &
            chars(print_expr_in(besselj(2, x), dialect(DIA_FORTRAN))), &
            "bessel_jn(2, x)")

        ! Maxima prefixes constants; a bare e there is an ordinary symbol.
        call eq_text("maxima pi", &
            chars(print_expr_in(pi_expr(a), dialect(DIA_MAXIMA))), "%pi")
        call eq_text("symengine pi", &
            chars(print_expr_in(pi_expr(a), dialect(DIA_SYMENGINE))), &
            "pi")
        call eq_text("yacas pi", &
            chars(print_expr_in(pi_expr(a), dialect(DIA_YACAS))), "Pi")
    end subroutine test_dialect_spellings

    !> Fortran emission is deliberately not round-trippable, so it is checked
    !> against what a compiler must be given.
    subroutine test_fortran_emission()
        type(arena_t), target :: a
        type(dialect_t) :: d
        type(expr_t) :: x
        character(:), allocatable :: text

        call a%init()
        d = dialect(DIA_FORTRAN)
        x = sym(a, "x")

        ! An exact rational must not become integer division, which would
        ! evaluate to zero.
        text = chars(print_expr_in(rat(a, 1_int64, 3_int64), d))
        call ok("fortran rational is not integer division", &
            index(text, ".0_dp/") > 0)

        ! Real literals must carry a kind, or they are single precision.
        text = chars(print_expr_in(real_expr(a, 1.5_dp), d))
        call ok("fortran real carries kind", index(text, "_dp") > 0)

        ! pi has no Fortran spelling, so it must be emitted at full precision
        ! rather than left as a free variable named pi.
        text = chars(print_expr_in(pi_expr(a), d))
        call ok("fortran pi is a literal", index(text, "3.14159") > 0)
        call ok("fortran pi carries kind", index(text, "_dp") > 0)

        ! An integer exponent must stay an integer: x**2.0_dp is a much slower
        ! and less accurate operation than x**2.
        text = chars(print_expr_in(x**2, d))
        call eq_text("fortran integer exponent stays integer", text, "x**2")
    end subroutine test_fortran_emission

    !> Malformed input must be reported, never accepted and never fatal: this
    !> parser reads replies from external programs.
    subroutine test_parse_errors()
        type(arena_t), target :: a
        type(dialect_t) :: d
        type(expr_t) :: e
        character(:), allocatable :: message
        logical :: good

        call a%init()
        d = dialect(DIA_NATIVE)

        e = parse_expr_in(a, "x +", d, good, message)
        call ok("trailing operator rejected", .not. good)

        e = parse_expr_in(a, "(x + y", d, good, message)
        call ok("unclosed paren rejected", .not. good)

        e = parse_expr_in(a, "", d, good, message)
        call ok("empty input rejected", .not. good)

        e = parse_expr_in(a, "x @ y", d, good, message)
        call ok("bad character rejected", .not. good)

        e = parse_expr_in(a, "x y", d, good, message)
        call ok("juxtaposition rejected", .not. good)

        ! A well-formed input must still succeed after all that.
        e = parse_expr_in(a, "sin(x) + 1", d, good, message)
        call ok("valid input accepted", good)
    end subroutine test_parse_errors

end program test_fortsym_roundtrip
