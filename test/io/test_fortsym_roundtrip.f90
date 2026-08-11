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
    use fortsym_arena, only: arena_t, NK_MUL, NK_FUNC, NK_ALGEBRAIC
    use fortsym_expr
    use fortsym_algebraic, only: algebraic_from_re_im, algebraic_sqrt
    use fortsym_dialect, only: dialect_t, dialect, DIA_NATIVE, DIA_SYMENGINE, &
        DIA_YACAS, DIA_SYMPY, DIA_MAXIMA, DIA_FORTRAN, DIA_WOLFRAM
    use fortsym_print, only: print_expr, print_expr_in
    use fortsym_parse, only: parse_expr_in
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_printing_shape()
    call test_construction_history_independence()
    call test_roundtrip_all_dialects()
    call test_algebraic_roundtrip()
    call test_dialect_spellings()
    call test_infinity_sentinel()
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
        call eq_check("large rational coefficient sign", &
            print_text(x*exact(a, &
            "-18446744073709551617/18446744073709551616")), &
            "-(x*(18446744073709551617/18446744073709551616))")

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

    subroutine test_construction_history_independence()
        type(arena_t), target :: a, b
        type(expr_t) :: ax, ay, bx, by, left, right
        type(expr_t) :: abig_i, abig_q, bbig_i, bbig_q
        character(:), allocatable :: left_text, right_text

        call a%init()
        call b%init()

        ! Reverse symbol insertion and operand construction. Sorting by arena
        ! indices makes these render differently even though they are the same
        ! commutative tree; stable structural ordering must erase that history.
        ax = sym(a, "x")
        ay = sym(a, "y")
        abig_i = exact(a, "18446744073709551616")
        abig_q = exact(a, &
            "18446744073709551617/18446744073709551616")
        bbig_q = exact(b, &
            "18446744073709551617/18446744073709551616")
        bbig_i = exact(b, "18446744073709551616")
        by = sym(b, "y")
        bx = sym(b, "x")
        left = ax + ay + pi_expr(a) + sin(ax) + ax**2 + ax*ay + 1 + &
            abig_i + abig_q
        right = bbig_q + bbig_i + 1 + by*bx + bx**2 + sin(bx) + &
            pi_expr(b) + by + bx

        left_text = print_text(left)
        right_text = print_text(right)
        call eq_text("construction histories print identically", left_text, &
            right_text)
        call eq_text("semantic ordering has a documented exact form", &
            left_text, "x + y + pi + sin(x) + x**2 + x*y + 1 + "//&
            "18446744073709551616 + "//&
            "18446744073709551617/18446744073709551616")
    end subroutine test_construction_history_independence

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
        type(expr_t) :: x, y, z, cases(33)
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
        ! Independent decimal goldens: 2^64 and the adjacent-over-power ratio.
        n = n + 1; cases(n) = exact(a, "18446744073709551616")
        n = n + 1; cases(n) = exact(a, "-18446744073709551616")
        n = n + 1; cases(n) = exact(a, &
            "18446744073709551617/18446744073709551616")
        n = n + 1; cases(n) = exact(a, &
            "-18446744073709551617/18446744073709551616")
        n = n + 1; cases(n) = x + exact(a, "-18446744073709551616")
        n = n + 1; cases(n) = x*exact(a, &
            "-18446744073709551617/18446744073709551616")
        ! INT64_MIN cannot be negated in int64; decimal exact arithmetic must
        ! carry its sign through both atom and sum printing.
        n = n + 1; cases(n) = exact(a, "-9223372036854775808")
        n = n + 1; cases(n) = x + exact(a, "-9223372036854775808")
        n = n + 1; cases(n) = x + exact(a, "-9223372036854775808/3")
        ! A syntactic subtraction stores a distinct -1 factor; its spelling
        ! must not collide with a genuinely negative large coefficient.
        n = n + 1; cases(n) = x - y*exact(a, "18446744073709551616")
        n = n + 1; cases(n) = x + 2*exact(a, "-18446744073709551616")
        n = n + 1; cases(n) = x*exact(a, "-18446744073709551616")*&
            exact(a, "-18446744073709551617/18446744073709551616")
        n = n + 1; cases(n) = exact(a, "-18446744073709551617")*&
            exact(a, "18446744073709551616")

        do k = 1, n
            call check_roundtrip(a, d, cases(k), k)
        end do
    end subroutine roundtrip_corpus

    !> Canonical qqbar1 text is the lossless internal representation of an
    !> algebraic atom. The known minimal polynomial for sqrt(2) is an
    !> independent mathematical oracle; the bridge supplies the root ordering
    !> and the parser must retain the resulting atom without treating its
    !> punctuation as expression syntax.
    subroutine test_algebraic_roundtrip()
        integer, parameter :: dialects(*) = [DIA_NATIVE, DIA_SYMENGINE, &
            DIA_YACAS, DIA_SYMPY, DIA_MAXIMA]
        type(arena_t), target :: a
        type(dialect_t) :: d
        type(expr_t) :: root, back, x, mixed
        character(:), allocatable :: root_text, text, message
        logical :: bridge_ok, valid, good
        integer :: k

        call a%init()
        root_text = chars(algebraic_from_re_im("2", "0", bridge_ok))
        call ok("algebraic round-trip bridge construction succeeds", bridge_ok)
        if (.not. bridge_ok) return
        root_text = chars(algebraic_sqrt(root_text, bridge_ok))
        call ok("algebraic round-trip sqrt succeeds", bridge_ok)
        if (.not. bridge_ok) return
        call eq_text("sqrt(2) has the known qqbar1 root", root_text, &
            "qqbar1:0:-2,0,1")

        root = algebraic_expr(a, root_text, valid)
        call ok("canonical algebraic atom enters the arena", valid)
        if (.not. valid) return
        call ok("canonical algebraic atom has its node kind", &
            root%kind() == NK_ALGEBRAIC)

        do k = 1, size(dialects)
            d = dialect(dialects(k))
            text = chars(print_expr_in(root, d, valid))
            call ok("algebraic printer accepts a lossless dialect", valid)
            if (.not. valid) cycle
            call eq_text("algebraic printer retains canonical text", text, root_text)
            back = parse_expr_in(a, text, d, good, message)
            call ok("algebraic parser accepts printer output", good)
            if (good) then
                call ok("algebraic parser restores the atom", back == root)
                call eq_text("algebraic parser retains canonical payload", &
                    chars(back%algebraic_text()), root_text)
            end if
        end do

        x = sym(a, "x")
        mixed = root + x
        text = chars(print_expr(mixed))
        back = parse_expr_in(a, text, dialect(DIA_NATIVE), good, message)
        call ok("algebraic token works inside an expression", good)
        if (good) call ok("mixed algebraic expression round-trips", back == mixed)

        back = parse_expr_in(a, root_text//"+x", dialect(DIA_NATIVE), good, message)
        call ok("algebraic token touches an operator without whitespace", good)
        if (good) call ok("unspaced algebraic expression round-trips", back == mixed)

        back = parse_expr_in(a, "f("//root_text//",x)", &
            dialect(DIA_NATIVE), good, message)
        call ok("algebraic token works in a multi-argument call", good)
        if (good) call ok("multi-argument algebraic call keeps both arguments", &
            back%kind() == NK_FUNC .and. back%nargs() == 2)

        back = parse_expr_in(a, "qqbar1:0:not-an-integer,1", &
            dialect(DIA_NATIVE), good, message)
        call ok("malformed qqbar1 text is refused", .not. good)
    end subroutine test_algebraic_roundtrip

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
        call eq_text("wolfram positive infinity", &
            chars(print_expr_in(oo_expr(a), dialect(DIA_WOLFRAM))), "Infinity")
        call eq_text("wolfram complex infinity", &
            chars(print_expr_in(zoo_expr(a), dialect(DIA_WOLFRAM))), &
            "ComplexInfinity")
        call eq_text("wolfram undefined value", &
            chars(print_expr_in(nan_expr(a), dialect(DIA_WOLFRAM))), &
            "Indeterminate")
    end subroutine test_dialect_spellings

    subroutine test_infinity_sentinel()
        type(arena_t), target :: a
        type(expr_t) :: infinity, complex_infinity, undefined, parsed
        character(:), allocatable :: message
        logical :: good

        call a%init()
        infinity = oo_expr(a)
        call eq_text("native positive infinity spelling", print_text(infinity), "oo")
        complex_infinity = zoo_expr(a)
        undefined = nan_expr(a)
        call eq_text("native complex infinity spelling", print_text(complex_infinity), "zoo")
        call eq_text("native undefined spelling", print_text(undefined), "nan")

        parsed = parse_expr_in(a, "oo", dialect(DIA_NATIVE), good, message)
        call ok("native positive infinity parses", good)
        if (good) call ok("parsed infinity retains its sentinel node", parsed == infinity)

        parsed = parse_expr_in(a, "zoo", dialect(DIA_NATIVE), good, message)
        call ok("native complex infinity parses", good)
        if (good) call ok("parsed complex infinity retains its sentinel node", &
            parsed == complex_infinity)

        parsed = parse_expr_in(a, "nan", dialect(DIA_NATIVE), good, message)
        call ok("native undefined value parses", good)
        if (good) call ok("parsed undefined value retains its sentinel node", &
            parsed == undefined)

        parsed = parse_expr_in(a, "Infinity", dialect(DIA_WOLFRAM), good, message)
        call ok("wolfram positive infinity parses", good)
        if (good) call ok("parsed wolfram infinity retains its sentinel node", &
            parsed == infinity)

        parsed = parse_expr_in(a, "ComplexInfinity", dialect(DIA_WOLFRAM), good, message)
        call ok("wolfram complex infinity parses", good)
        if (good) call ok("parsed wolfram complex infinity retains its sentinel node", &
            parsed == complex_infinity)

        parsed = parse_expr_in(a, "Indeterminate", dialect(DIA_WOLFRAM), good, message)
        call ok("wolfram undefined value parses", good)
        if (good) call ok("parsed wolfram undefined value retains its sentinel node", &
            parsed == undefined)
    end subroutine test_infinity_sentinel

    !> Fortran emission is deliberately not round-trippable, so it is checked
    !> against what a compiler must be given.
    subroutine test_fortran_emission()
        type(arena_t), target :: a
        type(dialect_t) :: d
        type(expr_t) :: x
        character(:), allocatable :: text
        logical :: good

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

        text = chars(print_expr_in(exact(a, "18446744073709551616"), d))
        call eq_text("fortran large integer projects to binary64", text, &
            "1.8446744073709552E+019_dp")

        text = chars(print_expr_in(exact(a, &
            "18446744073709551617/18446744073709551616"), d))
        call eq_text("fortran large rational avoids integer division", text, &
            "1.0000000000000000E+000_dp")

        text = chars(print_expr_in(exact(a, "1"//repeat("0", 400)), d, good))
        call ok("out-of-range exact Fortran emission is refused", &
            .not. good .and. len(text) == 0)

        text = chars(print_expr_in(oo_expr(a), d, good))
        call ok("infinity is refused by finite Fortran emission", &
            .not. good .and. len(text) == 0)

        text = chars(print_expr_in(zoo_expr(a), d, good))
        call ok("complex infinity is refused by finite Fortran emission", &
            .not. good .and. len(text) == 0)

        text = chars(print_expr_in(nan_expr(a), d, good))
        call ok("undefined value is refused by finite Fortran emission", &
            .not. good .and. len(text) == 0)
    end subroutine test_fortran_emission

    !> Malformed input must be reported, never accepted and never fatal: this
    !> parser reads replies from external programs.
    subroutine test_parse_errors()
        type(arena_t), target :: a
        type(dialect_t) :: d
        type(expr_t) :: e
        character(:), allocatable :: message
        character(:), allocatable :: huge_left, huge_right
        logical :: good, valid_expr

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

        ! Each operand is below the 1 MiB input budget, while the canonical
        ! coprime quotient is above it. The parser must retain a structural
        ! division instead of returning an invalid expression after the FLINT
        ! operation produces a result too large for one arena scalar.
        huge_left = "1"//repeat("0", 525000)//"1"
        huge_right = "1"//repeat("0", 525000)//"2"
        e = parse_expr_in(a, huge_left//"/"//huge_right, d, good, message)
        valid_expr = is_valid(e)
        call ok("oversize exact quotient remains a valid expression", &
            good .and. valid_expr)
        if (good) call ok("oversize exact quotient remains structural", &
            e%kind() == NK_MUL)
    end subroutine test_parse_errors

end program test_fortsym_roundtrip
