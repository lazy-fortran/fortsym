module fortsym_parse
    ! Text to expression, by precedence climbing.
    !
    ! The parser is the other half of the dialect contract: it reads back what
    ! the printer emits, in the same dialect, so an expression can make a round
    ! trip through any engine without changing meaning. The round-trip test is
    ! what keeps the two honest, and it is the reason both consult one table in
    ! fortsym_dialect rather than each carrying its own idea of the syntax.
    !
    ! Precedence climbing rather than a recursive-descent cascade: one loop with
    ! a binding-power table handles all the binary operators, so adding one is a
    ! table entry rather than a new level of functions.
    !
    ! Errors never abort. A malformed input yields an invalid expression and a
    ! message, because this parser reads text produced by external programs,
    ! where a truncated or unexpected reply is a normal event to be reported and
    ! not a reason to stop the build.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, &
        NK_BIG_RAT, NK_MUL, NK_SYM
    use fortsym_expr, only: expr_t, sym, num, exact, real_expr, const, func, &
        func_in, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_exact, only: exact_sub, exact_div
    use fortsym_dialect, only: dialect_t, dialect, fn_canonical, DIA_WOLFRAM, &
        const_canonical, DIA_NATIVE
    implicit none
    private

    public :: parse_expr, parse_expr_in

    integer, parameter :: dp = real64

    ! Token kinds.
    integer, parameter :: T_END = 0
    integer, parameter :: T_NUMBER = 1
    integer, parameter :: T_NAME = 2
    integer, parameter :: T_OP = 3
    integer, parameter :: T_LPAREN = 4
    integer, parameter :: T_RPAREN = 5
    integer, parameter :: T_COMMA = 6
    integer, parameter :: T_ERROR = 7
    ! Wolfram applies functions with brackets and writes lists with braces.
    integer, parameter :: T_LBRACKET = 8
    integer, parameter :: T_RBRACKET = 9
    integer, parameter :: T_LBRACE = 10
    integer, parameter :: T_RBRACE = 11

    !> Parser state. Kept in one object so the recursive descent needs no
    !> module-level variables and two parses can never interfere.
    type :: parser_t
        character(:), allocatable :: src
        integer                   :: pos = 1
        ! Current token.
        integer                   :: tok = T_END
        character(:), allocatable :: text
        logical                   :: is_real = .false.
        logical                   :: integer_fits = .true.
        integer(int64)            :: ivalue = 0_int64
        real(dp)                  :: rvalue = 0.0_dp
        !> Cached dialect flag: the lexer needs it and takes no dialect
        !> argument on every path.
        logical                   :: wolfram = .false.
        ! First error encountered, if any.
        logical                   :: failed = .false.
        character(:), allocatable :: message
    end type parser_t

contains

    !> Parse in fortsym's own notation.
    function parse_expr(a, text, ok, message) result(e)
        type(arena_t), target,     intent(inout) :: a
        character(*),              intent(in)    :: text
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: message
        type(expr_t)                             :: e
        e = parse_expr_in(a, text, dialect(DIA_NATIVE), ok, message)
    end function parse_expr

    !> Parse in a given dialect.
    function parse_expr_in(a, text, d, ok, message) result(e)
        type(arena_t), target,     intent(inout) :: a
        character(*),              intent(in)    :: text
        type(dialect_t),           intent(in)    :: d
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: message
        type(expr_t)                             :: e
        type(parser_t) :: p

        p%src = text
        p%pos = 1
        p%wolfram = d%wolfram_syntax
        p%failed = .false.
        p%message = ""
        call advance(p, d)

        e = parse_binary(p, a, d, 0)

        if (.not. p%failed .and. p%tok /= T_END) then
            call fail(p, "unexpected trailing input")
        end if

        ok = .not. p%failed
        message = p%message
    end function parse_expr_in

    subroutine fail(p, why)
        type(parser_t), intent(inout) :: p
        character(*),   intent(in)    :: why
        ! Keep the first error: later ones are usually consequences of it.
        if (.not. p%failed) then
            p%failed = .true.
            p%message = why
        end if
    end subroutine fail

    ! ------------------------------------------------------------ lexer --

    pure function is_digit(c) result(yes)
        character, intent(in) :: c
        logical               :: yes
        yes = c >= "0" .and. c <= "9"
    end function is_digit

    pure function is_alpha(c) result(yes)
        character, intent(in) :: c
        logical               :: yes
        ! Underscore and percent are name characters: Fortran identifiers use
        ! the first and Maxima spells its constants %pi.
        yes = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z") &
            .or. c == "_" .or. c == "%"
    end function is_alpha

    pure function is_name_char(c) result(yes)
        character, intent(in) :: c
        logical               :: yes
        ! $ is an ordinary symbol character in Wolfram -- $Assumptions,
        ! $InputFileName -- and " ` " separates contexts. Rejecting either
        ! turns a legitimate name into a lexical error mid-expression.
        yes = is_alpha(c) .or. is_digit(c) .or. c == "$" .or. c == "`" &
              .or. is_extended(c)
    end function is_name_char

    !> Any byte above ASCII, treated as part of an identifier.
    !>
    !> The corpus is full of Greek variable names -- alpha, omega, psi -- stored
    !> as multi-byte UTF-8. Working byte-wise is enough because every character
    !> fortsym gives syntactic meaning to is ASCII, so a continuation byte can
    !> never be mistaken for an operator.
    pure function is_extended(c) result(yes)
        character, intent(in) :: c
        logical               :: yes
        yes = iachar(c) > 127
    end function is_extended

    !> Read the next token into the parser state.
    subroutine advance(p, d)
        type(parser_t),  intent(inout) :: p
        type(dialect_t), intent(in)    :: d
        integer :: n, start
        character :: c

        n = len(p%src)

        call skip_trivia(p, n)

        if (p%pos > n) then
            p%tok = T_END
            p%text = ""
            return
        end if

        c = p%src(p%pos:p%pos)

        if (is_digit(c)) then
            call lex_number(p)
            return
        end if

        if (c == "." .and. p%pos < n) then
            ! A dot followed by a digit starts a number; otherwise it is the
            ! Dot product. Treating every dot as a number turns "a . b" into a
            ! malformed literal and loses the operator entirely.
            if (is_digit(p%src(p%pos + 1:p%pos + 1))) then
                call lex_number(p)
            else
                p%tok = T_OP
                p%text = "."
                p%pos = p%pos + 1
            end if
            return
        end if

        if (is_alpha(c) .or. c == "$" .or. is_extended(c)) then
            start = p%pos
            do while (p%pos <= n)
                if (.not. is_name_char(p%src(p%pos:p%pos))) exit
                p%pos = p%pos + 1
            end do
            p%tok = T_NAME
            p%text = p%src(start:p%pos - 1)
            return
        end if

        select case (c)
        case ("[")
            p%tok = T_LBRACKET
            p%text = "["
            p%pos = p%pos + 1
        case ("]")
            p%tok = T_RBRACKET
            p%text = "]"
            p%pos = p%pos + 1
        case ("{")
            p%tok = T_LBRACE
            p%text = "{"
            p%pos = p%pos + 1
        case ("}")
            p%tok = T_RBRACE
            p%text = "}"
            p%pos = p%pos + 1
        case ("(")
            p%tok = T_LPAREN
            p%text = "("
            p%pos = p%pos + 1
        case (")")
            p%tok = T_RPAREN
            p%text = ")"
            p%pos = p%pos + 1
        case (",")
            p%tok = T_COMMA
            p%text = ","
            p%pos = p%pos + 1
        case ("*")
            ! ** is one token; a dialect that spells power as ^ still has * for
            ! multiplication, so both are recognised regardless of dialect and
            ! only the printer's choice depends on d.
            if (p%pos < n) then
                if (p%src(p%pos + 1:p%pos + 1) == "*") then
                    p%tok = T_OP
                    p%text = "**"
                    p%pos = p%pos + 2
                    return
                end if
            end if
            p%tok = T_OP
            p%text = "*"
            p%pos = p%pos + 1
        case ("'")
            ! Wolfram's derivative postfix: f'[r]. Handled by parse_postfix.
            p%tok = T_OP
            p%text = "'"
            p%pos = p%pos + 1
        case ("=", "<", ">", "!", "&", "|", ":")
            call lex_relational(p, n, c)
        case ("-")
            ! -> is a rule, not a subtraction of something negative. Lexing it
            ! as two tokens turns {x -> 1} into a subtraction and a stray >.
            if (p%pos < n) then
                if (p%src(p%pos + 1:p%pos + 1) == ">") then
                    p%tok = T_OP
                    p%text = "->"
                    p%pos = p%pos + 2
                    return
                end if
            end if
            p%tok = T_OP
            p%text = "-"
            p%pos = p%pos + 1
        case ("/")
            if (p%pos < n) then
                select case (p%src(p%pos:p%pos + 1))
                case ("/.", "/;")
                    p%tok = T_OP
                    p%text = p%src(p%pos:p%pos + 1)
                    p%pos = p%pos + 2
                    return
                end select
            end if
            p%tok = T_OP
            p%text = "/"
            p%pos = p%pos + 1
        case ("@")
            if (.not. d%wolfram_syntax) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
            p%tok = T_OP
            p%text = "@"
            p%pos = p%pos + 1
        case ("\")
            if (.not. d%wolfram_syntax) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
            ! \[Alpha] and friends are single named characters. Lexed whole so
            ! the name survives; splitting them scatters brackets into the
            ! token stream and derails the rest of the expression.
            call lex_named_character(p, n)
        case ("+", "^")
            p%tok = T_OP
            p%text = c
            p%pos = p%pos + 1
        case ("""")
            call lex_string(p, n)
        case default
            p%tok = T_ERROR
            p%text = c
            p%pos = p%pos + 1
            call fail(p, "unexpected character '"//c//"'")
        end select
    end subroutine advance

    !> A Wolfram named character such as \[Alpha], lexed as one identifier.
    subroutine lex_named_character(p, n)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n
        integer :: start

        start = p%pos
        p%pos = p%pos + 1
        if (p%pos <= n) then
            if (p%src(p%pos:p%pos) == "[") then
                do while (p%pos <= n)
                    if (p%src(p%pos:p%pos) == "]") then
                        p%pos = p%pos + 1
                        p%tok = T_NAME
                        p%text = p%src(start:p%pos - 1)
                        return
                    end if
                    p%pos = p%pos + 1
                end do
            end if
        end if
        p%tok = T_ERROR
        p%text = "\"
        call fail(p, "unterminated named character")
    end subroutine lex_named_character

    !> A string literal, kept whole and opaque.
    !>
    !> Strings in this corpus are labels and file paths, never operands, so the
    !> subset carries them as inert names rather than adding a string type the
    !> algebra would then have to reason about. The quotes stay part of the
    !> name so printing reproduces the literal exactly.
    subroutine lex_string(p, n)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n
        integer :: start

        start = p%pos
        p%pos = p%pos + 1
        do while (p%pos <= n)
            if (p%src(p%pos:p%pos) == "\\") then
                p%pos = p%pos + 2
                cycle
            end if
            if (p%src(p%pos:p%pos) == """") then
                p%pos = p%pos + 1
                p%tok = T_NAME
                p%text = p%src(start:p%pos - 1)
                return
            end if
            p%pos = p%pos + 1
        end do
        p%tok = T_ERROR
        p%text = ""
        call fail(p, "unterminated string")
    end subroutine lex_string

    !> Relational, logical and rule operators.
    !>
    !> Two characters where there are two: reading "==" as two "=" tokens turns
    !> an equation into an assignment, which is how Solve[x == 1, x] silently
    !> becomes a binding of x with the equation deleted.
    subroutine lex_relational(p, n, c)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n
        character,      intent(in)    :: c
        character(2) :: pair

        pair = "  "
        if (p%pos < n) pair = p%src(p%pos:p%pos + 1)

        select case (pair)
        case ("==", "!=", "<=", ">=", "&&", "||", ":>")
            p%tok = T_OP
            p%text = pair
            p%pos = p%pos + 2
            return
        end select

        select case (c)
        case ("<", ">")
            p%tok = T_OP
            p%text = c
            p%pos = p%pos + 1
        case ("=")
            ! A nested "=" is a local initialiser: Module[{ok = 1}, body]. The
            ! statement splitter has already taken any top-level assignment, so
            ! one reaching here is inside brackets and is structural.
            if (p%wolfram) then
                p%tok = T_OP
                p%text = "="
                p%pos = p%pos + 1
                return
            end if
            p%tok = T_ERROR
            p%text = c
            p%pos = p%pos + 1
            call fail(p, "assignment inside an expression")
        case default
            p%tok = T_ERROR
            p%text = c
            p%pos = p%pos + 1
            call fail(p, "unexpected character '"//c//"'")
        end select
    end subroutine lex_relational

    !> Whitespace and comments, skipped together.
    !>
    !> Wolfram nests (* ... *) comments, so a depth counter is required: a
    !> single scan for the first "*)" would end the comment early and dump the
    !> remainder of the inner comment into the token stream as code.
    subroutine skip_trivia(p, n)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n
        integer :: depth
        character :: c

        do while (p%pos <= n)
            c = p%src(p%pos:p%pos)
            if (c == " " .or. c == char(9) .or. c == char(10) &
                .or. c == char(13)) then
                p%pos = p%pos + 1
                cycle
            end if
            if (c == "(" .and. p%pos < n) then
                if (p%src(p%pos + 1:p%pos + 1) == "*") then
                    depth = 0
                    do while (p%pos <= n)
                        if (p%pos < n) then
                            if (p%src(p%pos:p%pos + 1) == "(*") then
                                depth = depth + 1
                                p%pos = p%pos + 2
                                cycle
                            end if
                            if (p%src(p%pos:p%pos + 1) == "*)") then
                                depth = depth - 1
                                p%pos = p%pos + 2
                                if (depth <= 0) exit
                                cycle
                            end if
                        end if
                        p%pos = p%pos + 1
                    end do
                    cycle
                end if
            end if
            exit
        end do
    end subroutine skip_trivia

    !> A numeric literal. Anything with a point or an exponent is a real;
    !> everything else stays an exact integer, because turning 3 into 3.0 would
    !> lose the exactness the arena depends on for rational arithmetic.
    subroutine lex_number(p)
        type(parser_t), intent(inout) :: p
        integer :: n, start, ios
        logical :: seen_point, seen_exp
        character :: c

        n = len(p%src)
        start = p%pos
        seen_point = .false.
        seen_exp = .false.

        do while (p%pos <= n)
            c = p%src(p%pos:p%pos)
            if (is_digit(c)) then
                p%pos = p%pos + 1
            else if (c == "." .and. .not. seen_point .and. .not. seen_exp) then
                seen_point = .true.
                p%pos = p%pos + 1
            else if ((c == "e" .or. c == "E" .or. c == "d" .or. c == "D") &
                    .and. .not. seen_exp .and. p%pos < n) then
                ! Only an exponent if digits or a sign follow; otherwise this is
                ! the start of an adjacent name.
                if (is_digit(p%src(p%pos + 1:p%pos + 1)) .or. &
                    ((p%src(p%pos + 1:p%pos + 1) == "+" .or. &
                    p%src(p%pos + 1:p%pos + 1) == "-") .and. p%pos + 1 < n)) then
                    seen_exp = .true.
                    p%pos = p%pos + 2
                else
                    exit
                end if
            else
                exit
            end if
        end do

        p%tok = T_NUMBER
        p%text = p%src(start:p%pos - 1)
        p%is_real = seen_point .or. seen_exp
        p%integer_fits = .true.

        if (p%is_real) then
            read (p%text, *, iostat=ios) p%rvalue
            if (ios /= 0) call fail(p, "malformed number '"//p%text//"'")
        else
            read (p%text, *, iostat=ios) p%ivalue
            ! The lexer has already proved this token consists only of decimal
            ! digits. A failed int64 conversion therefore means arbitrary
            ! precision, not malformed syntax.
            p%integer_fits = ios == 0
        end if

        ! A Fortran kind suffix belongs to the literal, not to the expression.
        call skip_kind_suffix(p)
    end subroutine lex_number

    !> Consume a trailing _dp, _real64 or similar so generated Fortran reads
    !> back as the number it denotes.
    subroutine skip_kind_suffix(p)
        type(parser_t), intent(inout) :: p
        integer :: n
        n = len(p%src)
        if (p%pos > n) return
        if (p%src(p%pos:p%pos) /= "_") return
        p%pos = p%pos + 1
        do while (p%pos <= n)
            if (.not. is_name_char(p%src(p%pos:p%pos))) exit
            p%pos = p%pos + 1
        end do
        ! A kind-suffixed literal is a real even when written without a point.
        p%is_real = .true.
        if (.not. p%is_real) return
        read (p%text, *, iostat=n) p%rvalue
        if (n /= 0) call fail(p, "malformed number '"//p%text//"'")
    end subroutine skip_kind_suffix

    ! ----------------------------------------------------------- parser --

    !> Binding power of a binary operator, or -1 if it is not one.
    !> True when the current token could begin a primary expression.
    !>
    !> Deliberately excludes operators and closing brackets: treating those as
    !> the start of a factor would turn "a - b" into a product and swallow the
    !> rest of the expression.
    pure function starts_primary(p) result(yes)
        type(parser_t), intent(in) :: p
        logical                    :: yes
        yes = p%tok == T_NUMBER .or. p%tok == T_NAME .or. &
              p%tok == T_LPAREN .or. p%tok == T_LBRACE
    end function starts_primary

    pure function binding_power(op) result(bp)
        character(*), intent(in) :: op
        integer                  :: bp
        select case (op)
        case ("="); bp = 1
        case ("@"); bp = 2
        case ("/.", "/;"); bp = 3
        case ("->", ":>"); bp = 4
        case ("||"); bp = 4
        case ("&&"); bp = 4
        case ("."); bp = 9
        case ("==", "!=", "<", ">", "<=", ">="); bp = 5
        case ("+", "-"); bp = 6
        case ("*", "/"); bp = 7
        case ("**", "^"); bp = 8
        case default; bp = -1
        end select
    end function binding_power

    pure function is_right_assoc(op) result(yes)
        character(*), intent(in) :: op
        logical                  :: yes
        ! Only exponentiation associates to the right: a**b**c is a**(b**c).
        yes = op == "**" .or. op == "^" .or. op == "@"
    end function is_right_assoc

    !> Precedence climbing. Parses a unary/primary term, then folds in binary
    !> operators whose binding power is at least min_bp.
    recursive function parse_binary(p, a, d, min_bp) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        integer,               intent(in)    :: min_bp
        type(expr_t)                         :: e
        type(expr_t) :: rhs
        character(:), allocatable :: op
        integer :: bp, next_bp

        e = parse_unary(p, a, d)
        if (p%failed) return

        do
            ! Stop at the first failure. Without this the loop keeps folding
            ! operators onto an operand that was never built, and an expr_t
            ! with a null arena pointer segfaults rather than reporting the
            ! parse error that already happened.
            if (p%failed) return
            ! Juxtaposition is multiplication in Wolfram: "a1 r" and
            ! "(c/(4 Pi)) Bz'[r]" are products with no operator between them.
            ! Handled before the operator test because there is no token to
            ! test -- the absence of one is the operator.
            if (d%implicit_multiplication .and. starts_primary(p)) then
                if (binding_power("*") < min_bp) exit
                rhs = parse_binary(p, a, d, binding_power("*") + 1)
                if (p%failed) return
                e = e*rhs
                cycle
            end if
            if (p%tok /= T_OP) exit
            op = p%text
            bp = binding_power(op)
            if (bp < min_bp .or. bp < 0) exit

            call advance(p, d)

            if (is_right_assoc(op)) then
                next_bp = bp
            else
                next_bp = bp + 1
            end if

            rhs = parse_binary(p, a, d, next_bp)
            if (p%failed) return

            select case (op)
            case ("+"); e = e + rhs
            case ("-")
                if (is_exact_kind(rhs%kind())) then
                    e = e + negate(a, rhs)
                else if (rhs%kind() == NK_MUL) then
                    if (has_big_exact_factor(rhs)) then
                        e = e + negate(a, rhs)
                    else
                        e = e - rhs
                    end if
                else
                    e = e - rhs
                end if
            case ("*"); e = e*rhs
            case ("/"); e = divide(a, e, rhs)
            case ("**", "^"); e = e**rhs
            ! Relations stay structural. Deciding x > 0 needs an assumption
            ! context, and folding it to a boolean here would answer a question
            ! nobody asked with information nobody supplied.
            case ("=="); e = func("Equal", [e, rhs])
            case ("!="); e = func("Unequal", [e, rhs])
            case ("<"); e = func("Less", [e, rhs])
            case (">"); e = func("Greater", [e, rhs])
            case ("<="); e = func("LessEqual", [e, rhs])
            case (">="); e = func("GreaterEqual", [e, rhs])
            case ("&&"); e = func("And", [e, rhs])
            case ("||"); e = func("Or", [e, rhs])
            case ("->"); e = func("Rule", [e, rhs])
            case (":>"); e = func("RuleDelayed", [e, rhs])
            case ("/."); e = func("ReplaceAll", [e, rhs])
            case ("/;"); e = func("Condition", [e, rhs])
            ! Dot is non-commutative matrix product, so it stays a structural
            ! head rather than becoming Times: folding it would let the
            ! simplifier reorder factors and silently transpose the result.
            case ("."); e = func("Dot", [e, rhs])
            ! f @ x is f[x]. Applying the head rather than building an operator
            ! node keeps one representation for application, so Simplify @ e
            ! reaches the same lowering as Simplify[e].
            case ("@"); e = apply_head(a, d, e, rhs)
            case ("="); e = func("Set", [e, rhs])
            case default
                call fail(p, "unknown operator '"//op//"'")
                return
            end select
        end do
    end function parse_binary

    recursive function parse_unary(p, a, d) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t)                         :: e

        if (p%tok == T_OP) then
            if (p%text == "-") then
                call advance(p, d)
                ! Unary minus binds tighter than +/- but looser than **, so -x**2
                ! is -(x**2), matching Fortran and every engine here.
                e = parse_binary(p, a, d, 7)
                ! Negating a failed parse walks a partially built node whose
                ! arena pointer was never set, which segfaults instead of
                ! reporting the error that already happened.
                if (p%failed) return
                e = negate(a, e)
                return
            else if (p%text == "+") then
                call advance(p, d)
                e = parse_binary(p, a, d, 7)
                return
            end if
        end if

        e = parse_primary(p, a, d)
    end function parse_unary

    !> Negation, folded into the literal when the operand is one.
    !>
    !> Without this, reading back the text "-7" would build (-1)*7 rather than
    !> the integer -7, and printing that gives "-7" again -- so the round trip
    !> would appear to work while quietly changing the structure. Every literal
    !> must parse to the same node the arena would have stored.
    function negate(a, e) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        type(expr_t)                         :: r
        type(expr_t) :: child
        type(str_t) :: value
        integer, allocatable :: factors(:)
        integer :: k, target
        logical :: good, inserted

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT)
            value = exact_sub("0", chars(e%exact_text()), good)
            if (good) r = exact(a, chars(value), inserted)
            if (.not. good) r = -e
            if (good) then
                if (.not. inserted) r = -e
            end if
        case (NK_REAL)
            r = real_expr(a, -e%real_value())
        case (NK_MUL)
            allocate (factors(e%nargs()))
            do k = 1, e%nargs()
                child = e%arg(k)
                factors(k) = child%id
            end do
            target = 0
            do k = 1, e%nargs()
                child = e%arg(k)
                if (child%kind() /= NK_BIG_INT .and. &
                    child%kind() /= NK_BIG_RAT) cycle
                target = k
                exit
            end do
            if (target == 0) then
                do k = 1, e%nargs()
                    child = e%arg(k)
                    if (.not. is_exact_kind(child%kind())) cycle
                    target = k
                    exit
                end do
            end if
            if (target == 0) then
                r = -e
                return
            end if
            child = e%arg(target)
            value = exact_sub("0", chars(child%exact_text()), good)
            if (.not. good) then
                r = -e
                return
            end if
            r = exact(a, chars(value), inserted)
            if (.not. inserted) then
                r = -e
                return
            end if
            factors(target) = r%id
            r%a => a
            r%id = a%mul(factors)
            return
        case default
            r = -e
        end select
    end function negate

    !> Division, folded into an exact rational when both operands are integers.
    !>
    !> The arena stores 3/4 as one rational node, so parsing "3/4" as a division
    !> of two integers would produce a different structure from the one that was
    !> printed. Folding here keeps the two spellings on the same node, and keeps
    !> the value exact rather than letting it become a float.
    function divide(a, x, y) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: x, y
        type(expr_t)                         :: r
        type(str_t) :: value
        logical :: good, inserted

        if (is_exact_kind(x%kind()) .and. is_exact_kind(y%kind())) then
            value = exact_div(chars(x%exact_text()), chars(y%exact_text()), good)
            if (good) then
                r = exact(a, chars(value), inserted)
                if (inserted) return
            end if
        end if
        r = x/y
    end function divide

    pure function is_exact_kind(kind) result(yes)
        integer, intent(in) :: kind
        logical             :: yes
        yes = kind == NK_INT .or. kind == NK_RAT .or. &
            kind == NK_BIG_INT .or. kind == NK_BIG_RAT
    end function is_exact_kind

    function has_big_exact_factor(e) result(yes)
        type(expr_t), intent(in) :: e
        type(expr_t)             :: child
        logical                  :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, e%nargs()
            child = e%arg(k)
            kind = child%kind()
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            yes = .true.
            return
        end do
    end function has_big_exact_factor

    recursive function parse_primary(p, a, d) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t)                         :: e
        character(:), allocatable :: name, canon
        type(expr_t), allocatable :: fargs(:)
        integer :: nargs, nprimes

        select case (p%tok)

        case (T_NUMBER)
            if (p%is_real) then
                e = real_expr(a, p%rvalue)
            else if (p%integer_fits) then
                e = num(a, p%ivalue)
            else
                e = exact(a, p%text, p%integer_fits)
                if (.not. p%integer_fits) then
                    call fail(p, "invalid exact integer '"//p%text//"'")
                    return
                end if
            end if
            call advance(p, d)

        case (T_NAME)
            name = p%text
            call advance(p, d)
            ! Wolfram writes the derivative of an unspecified function as a
            ! postfix prime: Bz'[r]. Counting the primes and emitting the same
            ! Derivative<n> head that fortsym's own differentiation produces
            ! keeps one representation for the concept -- otherwise a corpus
            ! script's Bz'[r] and fortsym's D[Bz[r], r] would be different
            ! expressions that print identically.
            nprimes = 0
            do while (p%tok == T_OP)
                if (p%text /= "'") exit
                nprimes = nprimes + 1
                call advance(p, d)
            end do
            if (nprimes > 0) then
                if (p%tok /= T_LBRACKET) then
                    call fail(p, "prime must be followed by an argument list")
                    return
                end if
                call advance(p, d)
                call parse_arg_list(p, a, d, fargs, nargs, T_RBRACKET)
                if (p%failed) return
                if (nargs /= 1) then
                    call fail(p, "prime on a function of several variables")
                    return
                end if
                e = prime_derivative(a, name, fargs(1), nprimes)
                return
            end if
            if (p%tok == T_LBRACKET) then
                call advance(p, d)
                ! [[ ... ]] is Part, not a nested application.
                if (p%tok == T_LBRACKET) then
                    call advance(p, d)
                    call parse_arg_list(p, a, d, fargs, nargs, T_RBRACKET)
                    if (p%failed) return
                    if (p%tok /= T_RBRACKET) then
                        call fail(p, "expected ']]' closing a Part")
                        return
                    end if
                    call advance(p, d)
                    e = part_expression(a, sym(a, name), fargs, nargs)
                    e = parse_postfix(p, a, d, e)
                    return
                end if
                call parse_arg_list(p, a, d, fargs, nargs, T_RBRACKET)
                if (p%failed) return
                canon = chars(fn_canonical(d, name))
                ! Wolfram overloads ArcTan on arity, so the parser resolves it
                ! rather than the name table: two arguments is atan2, and its
                ! argument order is what decides the quadrant.
                if (name == "ArcTan") then
                    if (nargs == 2) then
                        canon = "atan2"
                    else
                        canon = "atan"
                    end if
                end if
                if (nargs == 0) then
                    e = func_in(a, canon)
                else
                    e = func(canon, fargs(1:nargs))
                end if
                ! f[a][b] applies the result again. Without this the second
                ! bracket group is left in the stream and the statement ends as
                ! "unexpected trailing input".
                e = parse_postfix(p, a, d, e)
                return
            else if (p%tok == T_LPAREN) then
                call advance(p, d)
                call parse_arg_list(p, a, d, fargs, nargs, T_RPAREN)
                if (p%failed) return
                canon = chars(fn_canonical(d, name))
                e = func(canon, fargs(1:nargs))
            else
                ! A bare name is a constant if the dialect knows it as one, and
                ! an ordinary symbol otherwise. Getting this wrong is how
                ! Euler's number becomes a free variable.
                canon = chars(const_canonical(d, name))
                ! Wolfram names are case-sensitive: lowercase i is a common
                ! summation/index variable, while only uppercase I is the
                ! imaginary unit. Testing the canonical spelling alone would
                ! turn that ordinary symbol into I before evaluation.
                if (d%id == DIA_WOLFRAM) then
                    if (name == "Pi" .or. name == "E" .or. name == "I") then
                        e = const(a, canon)
                    else
                        e = sym(a, name)
                    end if
                else if (canon == "pi" .or. canon == "e" .or. canon == "i") then
                    e = const(a, canon)
                else
                    e = sym(a, name)
                end if
            end if

        case (T_LPAREN)
            call advance(p, d)
            e = parse_binary(p, a, d, 0)
            if (p%failed) return
            if (p%tok /= T_RPAREN) then
                call fail(p, "expected ')'")
                return
            end if
            call advance(p, d)

        case (T_LBRACE)
            ! A list becomes an opaque List application, so {x, 0, 5} survives
            ! as a structured argument for Series and friends instead of being
            ! flattened into something the lowering layer cannot read back.
            call advance(p, d)
            call parse_arg_list(p, a, d, fargs, nargs, T_RBRACE)
            if (p%failed) return
            if (nargs == 0) then
                e = func_in(a, "List")
            else
                e = func("List", fargs(1:nargs))
            end if

        case (T_END)
            call fail(p, "unexpected end of input")

        case default
            call fail(p, "unexpected token '"//p%text//"'")
        end select
    end function parse_primary

    !> Trailing [[...]] and [...] groups after a primary.
    recursive function parse_postfix(p, a, d, base) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t),          intent(in)    :: base
        type(expr_t)                         :: e
        type(expr_t), allocatable :: fargs(:)
        integer :: nargs

        e = base
        do
            if (p%tok /= T_LBRACKET) return
            call advance(p, d)
            if (p%tok == T_LBRACKET) then
                call advance(p, d)
                call parse_arg_list(p, a, d, fargs, nargs, T_RBRACKET)
                if (p%failed) return
                if (p%tok /= T_RBRACKET) then
                    call fail(p, "expected ']]' closing a Part")
                    return
                end if
                call advance(p, d)
                e = part_expression(a, e, fargs, nargs)
                cycle
            end if
            call parse_arg_list(p, a, d, fargs, nargs, T_RBRACKET)
            if (p%failed) return
            if (nargs == 0) then
                e = func("Apply", [e])
            else
                e = build_apply(a, e, fargs, nargs)
            end if
        end do
    end function parse_postfix

    !> Part[expr, indices...] kept structural.
    function part_expression(a, base, indices, nargs) result(e)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: base
        type(expr_t),          intent(in)    :: indices(:)
        integer,               intent(in)    :: nargs
        type(expr_t)                         :: e
        type(expr_t), allocatable :: parts(:)
        integer :: k

        allocate (parts(nargs + 1))
        parts(1) = base
        do k = 1, nargs
            parts(k + 1) = indices(k)
        end do
        e = func("Part", parts)
    end function part_expression

    !> Apply an already-built expression to further arguments.
    function build_apply(a, head, args, nargs) result(e)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: head
        type(expr_t),          intent(in)    :: args(:)
        integer,               intent(in)    :: nargs
        type(expr_t)                         :: e
        type(expr_t), allocatable :: parts(:)
        integer :: k

        if (head%kind() == NK_SYM) then
            e = func(chars(head%name()), args(1:nargs))
            return
        end if
        allocate (parts(nargs + 1))
        parts(1) = head
        do k = 1, nargs
            parts(k + 1) = args(k)
        end do
        e = func("Apply", parts)
    end function build_apply

    !> Apply a head to one argument, as f @ x does.
    function apply_head(a, d, head, argument) result(e)
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t),          intent(in)    :: head, argument
        type(expr_t)                         :: e
        type(expr_t) :: parts(1)

        parts(1) = argument
        if (head%kind() == NK_SYM) then
            ! Canonicalised like a bracketed call, so Sin @ x and Sin[x] are
            ! the same interned node rather than two heads that print alike.
            e = func(chars(fn_canonical(d, chars(head%name()))), parts)
        else
            ! A computed head is not something this subset can apply, so it
            ! stays structural rather than being guessed at.
            e = func("Apply", [head, argument])
        end if
    end function apply_head

    !> f'[x] as the Derivative<n> node fortsym's own diff would produce.
    !>
    !> The multi-index is all-in-the-first-argument because a primed function
    !> has exactly one argument; a several-variable function has no prime form
    !> in Wolfram either.
    function prime_derivative(a, name, argument, order) result(e)
        type(arena_t), target, intent(inout) :: a
        character(*),          intent(in)    :: name
        type(expr_t),          intent(in)    :: argument
        integer,               intent(in)    :: order
        type(expr_t)                         :: e
        type(expr_t) :: parts(3)

        parts(1) = sym(a, name)
        parts(2) = num(a, int(order, int64))
        parts(3) = argument
        e = func("Derivative1", parts)
    end function prime_derivative

    recursive subroutine parse_arg_list(p, a, d, fargs, nargs, closer)
        type(parser_t),            intent(inout) :: p
        type(arena_t), target,     intent(inout) :: a
        type(dialect_t),           intent(in)    :: d
        type(expr_t), allocatable, intent(out)   :: fargs(:)
        integer,                   intent(out)   :: nargs
        integer,                   intent(in)    :: closer
        integer, parameter :: MAX_ARGS = 16

        allocate (fargs(MAX_ARGS))
        nargs = 0

        if (p%tok == closer) then
            ! {} and f[] are legal. Failing here rejected every empty list in
            ! the corpus, and an empty list is how a script says "no results
            ! yet" before a loop fills it.
            call advance(p, d)
            return
        end if

        do
            if (nargs >= MAX_ARGS) then
                call fail(p, "too many function arguments")
                return
            end if
            nargs = nargs + 1
            fargs(nargs) = parse_binary(p, a, d, 0)
            if (p%failed) return

            if (p%tok == T_COMMA) then
                call advance(p, d)
                cycle
            end if
            exit
        end do

        if (p%tok /= closer) then
            call fail(p, "expected closing bracket on argument list")
            return
        end if
        call advance(p, d)
    end subroutine parse_arg_list

end module fortsym_parse
