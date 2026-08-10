program test_fortsym_wolfram
    ! The Wolfram-language subset reader.
    !
    ! The oracle is the arena: parse Wolfram text and require the same node
    ! index as the expression built directly. Hash-consing makes that a complete
    ! structural check, so a mis-mapped name, a lost bracket or a swapped
    ! argument order fails here without anyone writing down an expected string.
    !
    ! fortsym reads a documented *subset*. It is not a Wolfram Language
    ! implementation, and every construct here is one the consumer corpus
    ! actually contains. Behaviour is verified against Mathics, never against a
    ! Wolfram product -- see LEGAL.md section 5.1.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_print, only: print_expr_in
    use fortsym_parse, only: parse_expr_in
    implicit none

    integer :: nfail = 0

    call test_bracket_application()
    call test_constants_and_exactness()
    call test_comments_and_whitespace()
    call test_arctan_arity()
    call test_lists_survive()
    call test_roundtrip()
    call test_failure_is_reported_not_crashed()
    call test_juxtaposition()
    call test_postfix_and_application()
    call test_associations()
    call test_named_character_suffixes()
    call test_pattern_operators_and_precision()

    if (nfail == 0) then
        print *, "PASS test_fortsym_wolfram"
    else
        print *, "FAIL test_fortsym_wolfram:", nfail
        error stop 1
    end if

contains

    !> Parse Wolfram text and compare against an expression built directly.
    subroutine same(label, a, text, expected)
        character(*),          intent(in)    :: label, text
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: expected
        type(expr_t) :: got
        logical :: ok
        character(:), allocatable :: message

        got = parse_expr_in(a, text, dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            print *, "FAIL ", label, ": ", message
            nfail = nfail + 1
            return
        end if
        if (got%id /= expected%id) then
            print *, "FAIL ", label, ": got ", &
                chars(print_expr_in(got, dialect(DIA_WOLFRAM))), &
                " expected ", &
                chars(print_expr_in(expected, dialect(DIA_WOLFRAM)))
            nfail = nfail + 1
        end if
    end subroutine same

    function make_func1(name, first) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: first
        type(expr_t) :: e, arguments(1)

        arguments(1) = first
        e = func(name, arguments)
    end function make_func1

    function make_func2(name, first, second) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: first, second
        type(expr_t) :: e, arguments(2)

        arguments(1) = first
        arguments(2) = second
        e = func(name, arguments)
    end function make_func2

    function make_func3(name, first, second, third) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: first, second, third
        type(expr_t) :: e, arguments(3)

        arguments(1) = first
        arguments(2) = second
        arguments(3) = third
        e = func(name, arguments)
    end function make_func3

    subroutine test_bracket_application()
        type(arena_t), target :: a
        type(expr_t) :: x, y

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")

        call same("sin", a, "Sin[x]", sin(x))
        call same("nested", a, "Sin[Cos[x]]", sin(cos(x)))
        call same("power", a, "Sin[x]^2", sin(x)**2)
        call same("sum", a, "Sin[x]^2 + Cos[x]^2", sin(x)**2 + cos(x)**2)
        call same("product", a, "Exp[x*y]", exp(x*y))
        call same("sqrt", a, "Sqrt[x]", sqrt(x))
        call same("multiarg", a, "BesselJ[0, x]", &
            make_func2("besselj", num(a, 0), x))
        ! An unknown head stays an opaque application rather than becoming a
        ! parse error: most of the corpus calls functions fortsym has no
        ! wrapper for, and losing them at the door would lose the derivation.
        call same("opaque", a, "Bt0[x]", make_func1("Bt0", x))
    end subroutine test_bracket_application

    subroutine test_constants_and_exactness()
        type(arena_t), target :: a
        type(expr_t) :: x

        call a%init()
        x = sym(a, "x")

        call same("pi", a, "Pi", const(a, "pi"))
        call same("imaginary", a, "I", const(a, "i"))
        call same("lowercase i is a symbol", a, "i", sym(a, "i"))
        ! 1/3 must stay an exact rational node. A float here would silently
        ! change every downstream comparison from exact to approximate.
        call same("rational", a, "1/3", rat(a, 1_int64, 3_int64))
        call same("mixed", a, "x + 1/3", x + rat(a, 1_int64, 3_int64))
        call same("negative", a, "-7", num(a, -7))
    end subroutine test_constants_and_exactness

    subroutine test_comments_and_whitespace()
        type(arena_t), target :: a
        type(expr_t) :: x

        call a%init()
        x = sym(a, "x")

        call same("comment", a, "(* leading *) Sin[x]", sin(x))
        call same("trailing", a, "Sin[x] (* trailing *)", sin(x))
        ! Wolfram comments nest. Scanning for the first "*)" would end this one
        ! early and feed " x]" back as code.
        call same("nested comment", a, "(* a (* b *) c *) Sin[x]", sin(x))
        call same("newlines", a, "Sin["//char(10)//"  x"//char(10)//"]", sin(x))
        call same("tabs", a, char(9)//"Sin[x]"//char(9), sin(x))
    end subroutine test_comments_and_whitespace

    subroutine test_arctan_arity()
        type(arena_t), target :: a
        type(expr_t) :: x, y

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")

        ! Wolfram overloads ArcTan on arity. Mapping the name without checking
        ! it would turn ArcTan[y, x] into a one-argument atan and lose the
        ! quadrant, which is how a generated kernel gets a sign wrong.
        call same("arctan1", a, "ArcTan[x]", atan(x))
        call same("arctan2", a, "ArcTan[y, x]", make_func2("atan2", y, x))
    end subroutine test_arctan_arity

    subroutine test_lists_survive()
        type(arena_t), target :: a
        type(expr_t) :: x
        type(expr_t) :: got
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        x = sym(a, "x")

        ! A series specification is a list, and the lowering layer has to read
        ! it back. Flattening it into its elements would lose which number is
        ! the point and which is the order.
        call same("list", a, "{x, 0, 5}", &
            make_func3("List", x, num(a, 0), num(a, 5)))

        got = parse_expr_in(a, "Series[Exp[x], {x, 0, 3}]", &
            dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            print *, "FAIL series spec: ", message
            nfail = nfail + 1
        else if (chars(got%name()) /= "Series" .or. got%nargs() /= 2) then
            print *, "FAIL series spec: head ", chars(got%name()), &
                " nargs ", got%nargs()
            nfail = nfail + 1
        end if
    end subroutine test_lists_survive

    !> A malformed operand must produce an error, never a crash.
    !>
    !> Regression: "-Inverse[g] . c" segfaulted. The parser negated a term it
    !> had already failed to build, walking a node whose arena pointer was
    !> never set. A parser that dies on bad input cannot report which corpus
    !> construct it lacks, which is the only thing the refusal path is for.
    subroutine test_failure_is_reported_not_crashed()
        type(arena_t), target :: a
        type(expr_t) :: got
        logical :: ok
        character(:), allocatable :: message
        character(*), parameter :: bad(*) = [character(24) :: &
            "Sin[x                   ", &
            "*                       ", &
            "1 + + *                 ", &
            "x #name                 ", &
            "Sin[x]]]                ", &
            "{1, 2                   "]
        integer :: k

        call a%init()
        do k = 1, size(bad)
            got = parse_expr_in(a, trim(bad(k)), dialect(DIA_WOLFRAM), ok, message)
            if (ok) then
                print *, "FAIL malformed accepted: ", trim(bad(k))
                nfail = nfail + 1
            end if
        end do
        if (index(message, "position") == 0) then
            print *, "FAIL malformed diagnostic has no source position: ", message
            nfail = nfail + 1
        end if

        ! The construct that triggered the crash. It is well formed -- Dot is a
        ! real operator -- so it must parse; refusing Inverse is the evaluator's
        ! job, not the parser's.
        got = parse_expr_in(a, "-Inverse[g] . c", dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            print *, "FAIL dot after unary minus: ", message
            nfail = nfail + 1
        end if
    end subroutine test_failure_is_reported_not_crashed

    subroutine test_juxtaposition()
        type(arena_t), target :: a
        type(expr_t) :: x, y, r

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")
        r = sym(a, "r")

        ! Wolfram writes products with nothing between the factors.
        call same("juxtaposed", a, "x y", x*y)
        call same("coefficient", a, "2 x", num(a, 2)*x)
        call same("mixed juxtaposition", a, "2 Sin[x] Cos[x]", &
            num(a, 2)*sin(x)*cos(x))
        call same("juxtaposed call", a, "(x/y) Sin[r]", (x/y)*sin(r))
        ! A binary minus must survive: treating the next token as the start of
        ! a factor would turn this into a product and swallow the operator.
        call same("minus not product", a, "x - y", x - y)
        call same("plus not product", a, "x + y", x + y)
    end subroutine test_juxtaposition

    subroutine test_postfix_and_application()
        type(arena_t), target :: a
        type(expr_t) :: x

        call a%init()
        x = sym(a, "x")

        ! Part, curried application and prefix @ all appear in the corpus. Each
        ! was previously "unexpected token" and cost the whole statement.
        call same("part", a, "x[[1]]", make_func2("Part", x, num(a, 1)))
        call same("prefix at", a, "Sin @ x", sin(x))
        ! An empty list is legal and common: a script builds one before a loop
        ! fills it. Rejecting it lost every such statement.
        call same("empty list", a, "{}", func_in(a, "List"))
        ! Regression: a zero-argument call crashed the evaluator, which rebuilt
        ! it through func() and took the arena from an argument that does not
        ! exist. Nine corpus scripts died on Directory[].
        call same("empty call", a, "Directory[]", func_in(a, "Directory"))
        call same("empty parenthesised call", a, "Out()", func_in(a, "Out"))
        ! A named character is one identifier. Splitting it scatters brackets
        ! into the token stream and derails the rest of the expression.
        call same("named character", a, "Sin[\[Alpha]]", &
            sin(sym(a, "\[Alpha]")))
    end subroutine test_postfix_and_application

    subroutine test_named_character_suffixes()
        type(arena_t), target :: a

        call a%init()
        call same("named character with digit suffix", a, "\[Mu]0", &
            sym(a, "\[Mu]0"))
        call same("named character with letter suffix", a, "\[Gamma]se", &
            sym(a, "\[Gamma]se"))
    end subroutine test_named_character_suffixes

    subroutine test_pattern_operators_and_precision()
        type(arena_t), target :: a
        type(expr_t) :: x, test, pattern

        call a%init()
        x = sym(a, "x")
        pattern = make_func2("Pattern", x, func_in(a, "Blank"))
        test = make_func2("PatternTest", pattern, sym(a, "NumericQ"))
        call same("pattern test", a, "x_?NumericQ", test)
        call same("blank sequence", a, "__", func_in(a, "BlankSequence"))
        call same("blank null sequence", a, "___", &
            func_in(a, "BlankNullSequence"))
        call same("alternatives", a, "x_|y_", make_func2("Alternatives", &
            make_func2("Pattern", x, func_in(a, "Blank")), &
            make_func2("Pattern", sym(a, "y"), func_in(a, "Blank"))))
        call same("pattern test precedence", a, "x_?NumericQ|y_?StringQ", &
            make_func2("Alternatives", &
                make_func2("PatternTest", &
                    make_func2("Pattern", x, func_in(a, "Blank")), &
                    sym(a, "NumericQ")), &
                make_func2("PatternTest", &
                    make_func2("Pattern", sym(a, "y"), func_in(a, "Blank")), &
                    sym(a, "StringQ"))))
        call same("set delayed", a, "f[x_] := x", &
            make_func2("SetDelayed", make_func1("f", pattern), x))
        call same("up set", a, "x ^= 2", make_func2("UpSet", x, num(a, 2)))
        call same("up set delayed", a, "x ^:= 2", &
            make_func2("UpSetDelayed", x, num(a, 2)))
        call same("context name", a, "Global`x", sym(a, "Global`x"))
        call same("precision annotation", a, "1.25``20", real_expr(a, 1.25_real64))
    end subroutine test_pattern_operators_and_precision

    subroutine test_associations()
        type(arena_t), target :: a
        type(expr_t) :: x, y

        call a%init()
        x = sym(a, '"x"')
        y = sym(a, '"y"')

        call same("association", a, "<|""x"" -> 1, ""y"" -> 2|>", &
            make_func2("Association", make_func2("Rule", x, num(a, 1)), &
                make_func2("Rule", y, num(a, 2))))
        call same("empty association", a, "<||>", func_in(a, "Association"))
    end subroutine test_associations

    subroutine test_roundtrip()
        type(arena_t), target :: a
        type(expr_t) :: x, y, e, back
        logical :: ok
        character(:), allocatable :: message

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")
        e = sin(x*y)**2 + cos(x*y) - exp(x)/sqrt(y) + rat(a, 2_int64, 3_int64)

        back = parse_expr_in(a, chars(print_expr_in(e, dialect(DIA_WOLFRAM))), &
            dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok) then
            print *, "FAIL roundtrip: ", message
            nfail = nfail + 1
        else if (back%id /= e%id) then
            print *, "FAIL roundtrip: ", &
                chars(print_expr_in(e, dialect(DIA_WOLFRAM))), " became ", &
                chars(print_expr_in(back, dialect(DIA_WOLFRAM)))
            nfail = nfail + 1
        end if

        e = func_in(a, "List")
        if (chars(print_expr_in(e, dialect(DIA_WOLFRAM))) /= "{}") then
            print *, "FAIL empty-list printer"
            nfail = nfail + 1
        end if
        back = parse_expr_in(a, chars(print_expr_in(e, dialect(DIA_WOLFRAM))), &
            dialect(DIA_WOLFRAM), ok, message)
        if (.not. ok .or. back%id /= e%id) then
            print *, "FAIL empty-list roundtrip"
            nfail = nfail + 1
        end if
    end subroutine test_roundtrip

end program test_fortsym_wolfram
