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
        NK_BIG_RAT, NK_MUL, NK_SYM, NK_FUNC
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, &
        algebraic_expr, const, func, func_in, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_exact, only: exact_sub, exact_div
    use fortsym_dialect, only: dialect_t, dialect, fn_canonical, DIA_WOLFRAM, &
        DIA_FORTRAN, DIA_LATEX, const_canonical, DIA_NATIVE
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
    !> A pure-function slot: #, #1, ## or ##1. The index rides in ivalue and
    !> the head ("Slot" or "SlotSequence") in text.
    integer, parameter :: T_SLOT = 12
    !> Association delimiters: <| key -> value |>.
    integer, parameter :: T_LASSOC = 13
    integer, parameter :: T_RASSOC = 14
    !> Wolfram pattern blanks: _, __ and ___
    integer, parameter :: T_BLANK = 15
    !> Canonical FLINT qqbar1 atoms are one lossless internal token. Keeping
    !> the payload opaque prevents its commas and colons from becoming parser
    !> punctuation.
    integer, parameter :: T_ALGEBRAIC = 16

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

        if (d%id == DIA_LATEX) then
            ok = .false.
            message = "LaTeX is write-only and has no parser"
            return
        end if
        if (d%id == DIA_FORTRAN) then
            p%src = normalize_fortran_arrays(text)
        else
            p%src = text
        end if
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

    ! Fortran's alternate array-constructor spelling is `(/ ... /)`. The
    ! expression parser uses brackets for the same structural list node; this
    ! lexical normalization keeps both spellings on one parser path.
    function normalize_fortran_arrays(text) result(normalized)
        character(*), intent(in) :: text
        character(:), allocatable :: normalized
        integer :: k

        normalized = ""
        k = 1
        do while (k <= len(text))
            if (k < len(text) .and. text(k:k + 1) == "(/") then
                normalized = normalized//"["
                k = k + 2
            else if (k < len(text) .and. text(k:k + 1) == "/)") then
                normalized = normalized//"]"
                k = k + 2
            else
                normalized = normalized//text(k:k)
                k = k + 1
            end if
        end do
    end function normalize_fortran_arrays

    subroutine fail(p, why)
        type(parser_t), intent(inout) :: p
        character(*),   intent(in)    :: why
        ! Keep the first error: later ones are usually consequences of it.
        if (.not. p%failed) then
            p%failed = .true.
            p%message = trim(why)//" at position "//itoa(max(1, p%pos))
        end if
    end subroutine fail

    function itoa(n) result(text)
        integer, intent(in) :: n
        character(:), allocatable :: text
        character(len=16) :: buffer

        write (buffer, '(i0)') n
        text = trim(buffer)
    end function itoa

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

        if (d%wolfram_syntax .and. c == "_") then
            call lex_blank(p, n)
            return
        end if

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

        if (algebraic_prefix(p, n)) then
            call lex_algebraic(p, n)
            return
        end if

        if (is_alpha(c) .or. c == "$" .or. is_extended(c)) then
            start = p%pos
            do while (p%pos <= n)
                if (d%wolfram_syntax) then
                    if (p%src(p%pos:p%pos) == "_") exit
                end if
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
            ! Longest match first. "//." read as "//" then "." would turn a
            ! ReplaceRepeated into a postfix application of a Dot product.
            if (p%pos + 2 <= n) then
                if (p%src(p%pos:p%pos + 2) == "//.") then
                    p%tok = T_OP
                    p%text = "//."
                    p%pos = p%pos + 3
                    return
                end if
            end if
            if (p%pos < n) then
                select case (p%src(p%pos:p%pos + 1))
                case ("/.", "/;", "/@", "//")
                    p%tok = T_OP
                    p%text = p%src(p%pos:p%pos + 1)
                    p%pos = p%pos + 2
                    return
                end select
            end if
            p%tok = T_OP
            p%text = "/"
            p%pos = p%pos + 1
        case ("#")
            if (.not. d%wolfram_syntax) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
            call lex_slot(p, n)
        case (";")
            if (.not. d%wolfram_syntax) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
            p%tok = T_OP
            p%text = ";"
            p%pos = p%pos + 1
            if (p%pos <= n) then
                if (p%src(p%pos:p%pos) == ";") then
                    p%text = ";;"
                    p%pos = p%pos + 1
                end if
            end if
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
            if (p%pos <= n) then
                if (p%src(p%pos:p%pos) == "@") then
                    p%text = "@@"
                    p%pos = p%pos + 1
                    if (p%pos <= n) then
                        if (p%src(p%pos:p%pos) == "@") then
                            p%text = "@@@"
                            p%pos = p%pos + 1
                        end if
                    end if
                end if
            end if
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
        case ("+")
            p%tok = T_OP
            p%text = c
            p%pos = p%pos + 1
        case ("^")
            if (d%wolfram_syntax .and. p%pos < n) then
                if (p%src(p%pos + 1:p%pos + 1) == "=") then
                    p%tok = T_OP
                    p%text = "^="
                    p%pos = p%pos + 2
                    return
                end if
                if (p%pos + 2 <= n) then
                    if (p%src(p%pos + 1:p%pos + 2) == ":=") then
                        p%tok = T_OP
                        p%text = "^:="
                        p%pos = p%pos + 3
                        return
                    end if
                end if
            end if
            p%tok = T_OP
            p%text = c
            p%pos = p%pos + 1
        case ("?")
            if (.not. d%wolfram_syntax) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
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

    pure function algebraic_prefix(p, n) result(yes)
        type(parser_t), intent(in) :: p
        integer,        intent(in) :: n
        logical                    :: yes
        integer, parameter :: prefix_length = 7

        yes = .false.
        if (p%pos > n) return
        if (n - p%pos + 1 < prefix_length) return
        yes = p%src(p%pos:p%pos + prefix_length - 1) == "qqbar1:"
    end function algebraic_prefix

    pure function algebraic_coefficient_after_comma(p, n) result(yes)
        type(parser_t), intent(in) :: p
        integer,        intent(in) :: n
        logical                    :: yes
        integer                    :: next
        character                   :: c

        yes = .false.
        next = p%pos + 1
        if (next > n) return
        c = p%src(next:next)
        if (c == "+" .or. c == "-") then
            next = next + 1
            if (next > n) return
        end if
        yes = is_digit(p%src(next:next))
    end function algebraic_coefficient_after_comma

    !> Consume a canonical qqbar1 payload without exposing its internal
    !> punctuation to the ordinary expression grammar. The arena constructor
    !> performs the full FLINT validation and canonicalization after this token
    !> is handed to parse_primary.
    subroutine lex_algebraic(p, n)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n
        integer :: start
        logical :: have_digits

        start = p%pos
        p%pos = p%pos + 7

        ! The root index is unsigned and ends at the first colon.
        have_digits = .false.
        do while (p%pos <= n)
            if (.not. is_digit(p%src(p%pos:p%pos))) exit
            have_digits = .true.
            p%pos = p%pos + 1
        end do
        if (.not. have_digits) then
            p%tok = T_ALGEBRAIC
            p%text = p%src(start:p%pos - 1)
            return
        end if
        if (p%pos > n) then
            p%tok = T_ALGEBRAIC
            p%text = p%src(start:p%pos - 1)
            return
        end if
        if (p%src(p%pos:p%pos) /= ":") then
            p%tok = T_ALGEBRAIC
            p%text = p%src(start:p%pos - 1)
            return
        end if
        p%pos = p%pos + 1

        ! Each coefficient is an optional sign followed by decimal digits.
        ! A plus or minus after a completed coefficient is an expression
        ! operator, so it remains for the precedence parser when no comma
        ! introduces another coefficient.
        do
            if (p%pos > n) exit
            if (p%src(p%pos:p%pos) == "+" .or. &
                p%src(p%pos:p%pos) == "-") then
                p%pos = p%pos + 1
            end if
            have_digits = .false.
            do while (p%pos <= n)
                if (.not. is_digit(p%src(p%pos:p%pos))) exit
                have_digits = .true.
                p%pos = p%pos + 1
            end do
            if (.not. have_digits) exit
            if (p%pos > n) exit
            if (p%src(p%pos:p%pos) /= ",") exit
            if (.not. algebraic_coefficient_after_comma(p, n)) exit
            p%pos = p%pos + 1
        end do

        p%tok = T_ALGEBRAIC
        p%text = p%src(start:p%pos - 1)
    end subroutine lex_algebraic

    !> A pure-function slot: #, #1, ## or ##2.
    !>
    !> A named slot such as #name belongs to an Association and denotes a
    !> lookup by key, not a positional argument. There is no key here to look
    !> up, so it is rejected by name rather than silently read as slot one.
    subroutine lex_slot(p, n)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n
        integer :: start, ios

        p%pos = p%pos + 1
        p%text = "Slot"
        if (p%pos <= n) then
            if (p%src(p%pos:p%pos) == "#") then
                p%text = "SlotSequence"
                p%pos = p%pos + 1
            end if
        end if

        start = p%pos
        do while (p%pos <= n)
            if (.not. is_digit(p%src(p%pos:p%pos))) exit
            p%pos = p%pos + 1
        end do

        if (p%pos == start) then
            if (p%pos <= n) then
                if (is_alpha(p%src(p%pos:p%pos))) then
                    p%tok = T_ERROR
                    call fail(p, "named slot #name")
                    return
                end if
            end if
            p%ivalue = 1_int64
        else
            read (p%src(start:p%pos - 1), *, iostat=ios) p%ivalue
            if (ios /= 0) then
                p%tok = T_ERROR
                call fail(p, "malformed slot number")
                return
            end if
        end if
        p%tok = T_SLOT
    end subroutine lex_slot

    !> A Wolfram pattern blank: _, __ or ___.
    subroutine lex_blank(p, n)
        type(parser_t), intent(inout) :: p
        integer,        intent(in)    :: n

        p%tok = T_BLANK
        p%text = "_"
        p%pos = p%pos + 1
        if (p%pos <= n) then
            if (p%src(p%pos:p%pos) == "_") then
                p%text = "__"
                p%pos = p%pos + 1
                if (p%pos <= n) then
                    if (p%src(p%pos:p%pos) == "_") then
                        p%text = "___"
                        p%pos = p%pos + 1
                    end if
                end if
            end if
        end if
    end subroutine lex_blank

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
                        do while (p%pos <= n)
                            if (.not. is_name_char(p%src(p%pos:p%pos))) exit
                            p%pos = p%pos + 1
                        end do
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
        case ("==", "!=", "<=", ">=", "&&", "||", ":>", ":=", "<>")
            p%tok = T_OP
            p%text = pair
            p%pos = p%pos + 2
            return
        end select

        if (pair == "<|") then
            p%tok = T_LASSOC
            p%text = pair
            p%pos = p%pos + 2
            return
        else if (pair == "|>") then
            p%tok = T_RASSOC
            p%text = pair
            p%pos = p%pos + 2
            return
        end if

        select case (c)
        case ("&")
            ! A lone "&" is the postfix that closes a pure function. Only
            ! Wolfram spells it that way; in the native dialect it stays an
            ! error rather than becoming a silent no-op.
            if (.not. p%wolfram) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
            p%tok = T_OP
            p%text = "&"
            p%pos = p%pos + 1
        case ("<", ">")
            p%tok = T_OP
            p%text = c
            p%pos = p%pos + 1
        case ("|")
            if (.not. p%wolfram) then
                p%tok = T_ERROR
                p%text = c
                p%pos = p%pos + 1
                call fail(p, "unexpected character '"//c//"'")
                return
            end if
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
        call skip_precision(p)
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

    !> Consume Wolfram approximate-number precision annotations.
    !> The bounded arena keeps the numeric value as parsed binary64 while
    !> accepting the precision annotation instead of reporting a lexical error.
    subroutine skip_precision(p)
        type(parser_t), intent(inout) :: p
        integer :: n, start, ios

        n = len(p%src)
        if (p%pos > n) return
        if (iachar(p%src(p%pos:p%pos)) /= 96) return
        p%pos = p%pos + 1
        if (p%pos <= n) then
            if (iachar(p%src(p%pos:p%pos)) == 96) p%pos = p%pos + 1
        end if
        start = p%pos
        do while (p%pos <= n)
            if (.not. is_digit(p%src(p%pos:p%pos))) exit
            p%pos = p%pos + 1
        end do
        if (p%pos == start) then
            call fail(p, "missing numeric precision")
            return
        end if
        p%is_real = .true.
        read (p%text, *, iostat=ios) p%rvalue
        if (ios /= 0) call fail(p, "malformed number '"//p%text//"'")
    end subroutine skip_precision

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
            p%tok == T_ALGEBRAIC .or. &
            p%tok == T_LPAREN .or. p%tok == T_LBRACE .or. &
            p%tok == T_LASSOC .or. p%tok == T_SLOT .or. p%tok == T_BLANK
    end function starts_primary

    !> True when the current token could begin a whole operand, including a
    !> signed one. Used where an operand is optional -- a[[2 ;; -1]] has one
    !> and a[[2 ;;]] does not -- and a bare starts_primary would read the
    !> minus of the first as the start of a subtraction that is not there.
    pure function starts_expression(p) result(yes)
        type(parser_t), intent(in) :: p
        logical                    :: yes
        yes = starts_primary(p)
        if (yes) return
        if (p%tok /= T_OP) return
        yes = p%text == "-" .or. p%text == "+"
    end function starts_expression

    !> Binding powers in Wolfram's own order.
    !>
    !> The relative order is what decides meaning, and getting one pair wrong
    !> reassociates an expression into a different one that still parses --
    !> the worst kind of error here, because nothing reports it. So the table
    !> mirrors Wolfram's documented precedence rather than convenience:
    !> application forms (@, /@, @@) bind tighter than Power, Dot sits between
    !> Times and Power, Span sits just under Plus, and the pure-function "&"
    !> binds looser than everything except the postfix //, assignment and ";".
    pure function binding_power(op) result(bp)
        character(*), intent(in) :: op
        integer                  :: bp
        select case (op)
        case (";"); bp = 1
        case ("=", ":=", "^=", "^:="); bp = 2
        case ("//"); bp = 3
        case ("&"); bp = 4
        case ("/.", "//."); bp = 5
        case ("->", ":>"); bp = 6
        case ("/;"); bp = 7
        case ("|"); bp = 8
        case ("?"); bp = 9
        case ("||"); bp = 8
        case ("&&"); bp = 9
        case ("==", "!=", "<", ">", "<=", ">="); bp = 10
        case ("<>"); bp = 11
        case (";;"); bp = 12
        case ("+", "-"); bp = 13
        case ("*", "/"); bp = 14
        case ("."); bp = 15
        case ("**", "^"); bp = 16
        case ("@", "/@", "@@", "@@@"); bp = 17
        case default; bp = -1
        end select
    end function binding_power

    !> Binding power of the factor level, used by unary minus and by the
    !> implicit multiplication of juxtaposed operands.
    pure function bp_times() result(bp)
        integer :: bp
        bp = binding_power("*")
    end function bp_times

    pure function is_right_assoc(op) result(yes)
        character(*), intent(in) :: op
        logical                  :: yes
        ! Exponentiation and the application forms associate to the right:
        ! a**b**c is a**(b**c) and f /@ g /@ x is f /@ (g /@ x).
        yes = op == "**" .or. op == "^" .or. op == "@" .or. op == "/@" &
            .or. op == "@@" .or. op == "@@@" .or. op == "->" .or. op == ":>" &
            .or. op == "^=" .or. op == "^:="
    end function is_right_assoc

    !> Precedence climbing. Parses a unary/primary term, then folds in binary
    !> operators whose binding power is at least min_bp.
    recursive function parse_binary(p, a, d, min_bp, power_rhs) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        integer,               intent(in)    :: min_bp
        logical, optional,     intent(in)    :: power_rhs
        type(expr_t)                         :: e
        type(expr_t) :: rhs
        type(expr_t) :: one(1), pair(2)
        character(:), allocatable :: op
        integer :: bp, next_bp
        logical :: rhs_is_power

        rhs_is_power = .false.
        if (present(power_rhs)) rhs_is_power = power_rhs
        e = parse_unary(p, a, d, rhs_is_power)
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
                ! An implicit product at the factor boundary must stay outside
                ! a unary sign used as a power's right operand.  Otherwise
                ! `10^-9 15` is read as `10^(-(9*15))` instead of
                ! `(10^-9)*15`; explicit multiplication still follows the
                ! ordinary precedence-climbing path below.
                if (bp_times() < min_bp) exit
                rhs = parse_binary(p, a, d, bp_times() + 1)
                if (p%failed) return
                e = e*rhs
                cycle
            end if
            if (p%tok /= T_OP) exit
            op = p%text
            bp = binding_power(op)
            if (bp < min_bp .or. bp < 0) exit

            ! Postfix "&" closes a pure function: it takes the whole
            ! expression to its left as the body and has no right operand.
            if (op == "&") then
                call advance(p, d)
                one(1) = e
                e = func("Function", one)
                cycle
            end if

            ! Span has an optional right end -- a[[2 ;;]] is "from 2 to the
            ! end" -- so the right operand is read only when one is there.
            if (op == ";;") then
                call advance(p, d)
                if (.not. starts_expression(p)) then
                    one(1) = e
                    e = func("Span", one)
                    cycle
                end if
                rhs = parse_binary(p, a, d, bp + 1)
                if (p%failed) return
                pair(1) = e
                pair(2) = rhs
                e = func("Span", pair)
                cycle
            end if

            ! A trailing ";" inside brackets -- Module[{}, a; b;] -- ends the
            ! sequence rather than introducing another expression.
            if (op == ";") then
                call advance(p, d)
                if (.not. starts_expression(p)) then
                    one(1) = e
                    e = func("CompoundExpression", one)
                    cycle
                end if
                rhs = parse_binary(p, a, d, bp + 1)
                if (p%failed) return
                pair(1) = e
                pair(2) = rhs
                e = func("CompoundExpression", pair)
                cycle
            end if

            call advance(p, d)

            if (is_right_assoc(op)) then
                next_bp = bp
            else
                next_bp = bp + 1
            end if

            rhs = parse_binary(p, a, d, next_bp, op == "**" .or. op == "^")
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
            case ("/")
                if (d%id == DIA_FORTRAN) then
                    e = fortran_real_ratio(a, e, rhs)
                else
                    e = divide(a, e, rhs)
                end if
            case ("**", "^"); e = e**rhs
                ! Relations stay structural. Deciding x > 0 needs an assumption
                ! context, and folding it to a boolean here would answer a question
                ! nobody asked with information nobody supplied.
            case ("=="); pair(1) = e; pair(2) = rhs
                e = func("Equal", pair)
            case ("!="); pair(1) = e; pair(2) = rhs
                e = func("Unequal", pair)
            case ("<"); pair(1) = e; pair(2) = rhs
                e = func("Less", pair)
            case (">"); pair(1) = e; pair(2) = rhs
                e = func("Greater", pair)
            case ("<="); pair(1) = e; pair(2) = rhs
                e = func("LessEqual", pair)
            case (">="); pair(1) = e; pair(2) = rhs
                e = func("GreaterEqual", pair)
            case ("&&"); pair(1) = e; pair(2) = rhs
                e = func("And", pair)
            case ("||"); pair(1) = e; pair(2) = rhs
                e = func("Or", pair)
            case ("->"); pair(1) = e; pair(2) = rhs
                e = func("Rule", pair)
            case (":>"); pair(1) = e; pair(2) = rhs
                e = func("RuleDelayed", pair)
            case ("|"); pair(1) = e; pair(2) = rhs
                e = func("Alternatives", pair)
            case ("?"); pair(1) = e; pair(2) = rhs
                e = func("PatternTest", pair)
            case ("/."); pair(1) = e; pair(2) = rhs
                e = func("ReplaceAll", pair)
            case ("/;"); pair(1) = e; pair(2) = rhs
                e = func("Condition", pair)
                ! Dot is non-commutative matrix product, so it stays a structural
                ! head rather than becoming Times: folding it would let the
                ! simplifier reorder factors and silently transpose the result.
            case ("."); pair(1) = e; pair(2) = rhs
                e = func("Dot", pair)
                ! f @ x is f[x]. Applying the head rather than building an operator
                ! node keeps one representation for application, so Simplify @ e
                ! reaches the same lowering as Simplify[e].
            case ("@"); e = apply_head(a, d, e, rhs)
                ! x // f is f[x]: the same application, written the other way
                ! round, so it lowers through the same path as f @ x.
            case ("//"); e = apply_head(a, d, rhs, e)
                ! Map, Apply and MapApply stay structural. Each one needs the
                ! argument's list structure at evaluation time, and this subset has
                ! no evaluator for them, so they are heads that refuse by name
                ! rather than results.
            case ("/@"); pair(1) = e; pair(2) = rhs
                e = func("Map", pair)
            case ("@@"); pair(1) = e; pair(2) = rhs
                e = func("Apply", pair)
            case ("@@@"); pair(1) = e; pair(2) = rhs
                e = func("MapApply", pair)
            case ("//."); pair(1) = e; pair(2) = rhs
                e = func("ReplaceRepeated", pair)
            case ("<>"); pair(1) = e; pair(2) = rhs
                e = func("StringJoin", pair)
            case ("="); pair(1) = e; pair(2) = rhs
                e = func("Set", pair)
            case (":="); pair(1) = e; pair(2) = rhs
                e = func("SetDelayed", pair)
            case ("^="); pair(1) = e; pair(2) = rhs
                e = func("UpSet", pair)
            case ("^:="); pair(1) = e; pair(2) = rhs
                e = func("UpSetDelayed", pair)
            case default
                call fail(p, "unknown operator '"//op//"'")
                return
            end select
        end do
    end function parse_binary

    ! The Fortran printer writes an exact rational as a quotient of typed real
    ! literals so a compiler cannot perform integer division. Recover that
    ! exact node when both operands are integral real literals; ordinary real
    ! quotients remain real expressions.
    function fortran_real_ratio(a, left, right) result(e)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: left, right
        type(expr_t) :: e
        integer(int64) :: numerator, denominator
        logical :: left_ok, right_ok

        call integral_real_value(left, numerator, left_ok)
        call integral_real_value(right, denominator, right_ok)
        if (left_ok .and. right_ok .and. denominator /= 0_int64) then
            e = rat(a, numerator, denominator)
        else
            e = divide(a, left, right)
        end if
    end function fortran_real_ratio

    recursive subroutine integral_real_value(e, value, ok)
        type(expr_t), intent(in) :: e
        type(expr_t) :: factor
        integer(int64), intent(out) :: value
        logical, intent(out) :: ok
        real(dp) :: projected

        value = 0_int64
        ok = .false.
        select case (e%kind())
        case (NK_REAL)
            projected = e%real_value()
            if (projected /= projected) return
            if (abs(projected) >= real(huge(0_int64), dp)) return
            value = nint(projected, kind=int64)
            ok = real(value, dp) == projected
        case (NK_MUL)
            if (e%nargs() /= 2) return
            factor = e%arg(1)
            if (factor%kind() /= NK_INT) return
            if (factor%int_value() /= -1_int64) return
            factor = e%arg(2)
            call integral_real_value(factor, value, ok)
            if (ok) value = -value
        end select
    end subroutine integral_real_value

    recursive function parse_unary(p, a, d, power_rhs) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        logical,               intent(in)    :: power_rhs
        type(expr_t)                         :: e
        type(expr_t) :: one(1), pair(2)

        if (p%tok == T_OP) then
            if (p%text == ";;") then
                ! Leading Span: a[[;; 3]] runs from the first part to three.
                call advance(p, d)
                if (.not. starts_expression(p)) then
                    e = func_in(a, "Span")
                    return
                end if
                e = parse_binary(p, a, d, binding_power(";;") + 1)
                if (p%failed) return
                pair(1) = num(a, 1_int64)
                pair(2) = e
                e = func("Span", pair)
                return
            else if (p%text == "-") then
                call advance(p, d)
                ! Unary minus binds tighter than +/- but looser than **, so -x**2
                ! is -(x**2), matching Fortran and every engine here.
                ! In a power's right operand, the sign belongs to the next
                ! factor only: `10^-9 15` means `(10^-9)*15`. At the top
                ! level the looser unary-minus binding is retained, so
                ! `-x**2` remains `-(x**2)`.
                if (power_rhs) then
                    e = parse_binary(p, a, d, bp_times() + 1)
                else
                    e = parse_binary(p, a, d, bp_times())
                end if
                ! Negating a failed parse walks a partially built node whose
                ! arena pointer was never set, which segfaults instead of
                ! reporting the error that already happened.
                if (p%failed) return
                e = negate(a, e)
                return
            else if (p%text == "+") then
                call advance(p, d)
                e = parse_binary(p, a, d, bp_times())
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
            r%generation = a%generation_value()
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
        type(expr_t) :: one(1), pair(2)
        integer :: nargs, nprimes
        logical :: valid

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

        case (T_ALGEBRAIC)
            e = algebraic_expr(a, p%text, valid)
            if (.not. valid) then
                call fail(p, "invalid qqbar1 algebraic literal '"//p%text//"'")
                return
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
            else if (p%tok == T_LPAREN .and. d%wolfram_syntax .and. &
                    empty_parentheses(p)) then
                ! Empty parenthesised calls are retained for compatibility
                ! with the corpus' Out() spelling. Non-empty parentheses in
                ! Wolfram syntax are handled below as implicit multiplication.
                call advance(p, d)
                if (p%failed) return
                if (p%tok /= T_RPAREN) then
                    call fail(p, "expected ')' closing an empty call")
                    return
                end if
                call advance(p, d)
                canon = chars(fn_canonical(d, name))
                e = func_in(a, canon)
            else if (p%tok == T_LPAREN .and. .not. d%wolfram_syntax) then
                call advance(p, d)
                call parse_arg_list(p, a, d, fargs, nargs, T_RPAREN)
                if (p%failed) return
                canon = chars(fn_canonical(d, name))
                if (nargs == 0) then
                    e = func_in(a, canon)
                else
                    e = func(canon, fargs(1:nargs))
                end if
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
            e = parse_postfix(p, a, d, e)

        case (T_SLOT)
            ! Slot[n] stays structural: it only means something inside the
            ! Function that binds it, and this subset has no applier for one.
            one(1) = num(a, p%ivalue)
            e = func("Slot", one)
            if (p%text == "SlotSequence") e = func("SlotSequence", one)
            call advance(p, d)
            e = parse_postfix(p, a, d, e)
            return

        case (T_BLANK)
            select case (p%text)
            case ("_"); e = func_in(a, "Blank")
            case ("__"); e = func_in(a, "BlankSequence")
            case ("___"); e = func_in(a, "BlankNullSequence")
            end select
            call advance(p, d)
            e = parse_postfix(p, a, d, e)
            return

        case (T_LASSOC)
            ! Associations use the same Rule nodes as ordinary replacement
            ! rules, but need a distinct outer head so the native runner can
            ! flatten a results association into one benchmark binding per
            ! key.
            call advance(p, d)
            call parse_arg_list(p, a, d, fargs, nargs, T_RASSOC)
            if (p%failed) return
            if (nargs == 0) then
                e = func_in(a, "Association")
            else
                e = func("Association", fargs(1:nargs))
            end if
            e = parse_postfix(p, a, d, e)
            return

        case (T_LPAREN)
            call advance(p, d)
            e = parse_binary(p, a, d, 0)
            if (p%failed) return
            if (p%tok /= T_RPAREN) then
                call fail(p, "expected ')'")
                return
            end if
            call advance(p, d)
            ! (expr)[[k]] and (f)[x] apply to the parenthesised group.
            e = parse_postfix(p, a, d, e)
            return

        case (T_LBRACKET)
            if (d%id /= DIA_FORTRAN) then
                call fail(p, "array constructors are only supported in Fortran syntax")
                return
            end if
            call advance(p, d)
            call parse_arg_list(p, a, d, fargs, nargs, T_RBRACKET)
            if (p%failed) return
            if (nargs == 0) then
                e = func_in(a, "List")
            else
                e = func("List", fargs(1:nargs))
            end if
            e = parse_postfix(p, a, d, e)
            return

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
            ! {a, b, c}[[k]] indexes the list. Without this the [[k]] group is
            ! left in the stream and the enclosing argument list reports a
            ! missing closing bracket.
            e = parse_postfix(p, a, d, e)
            return

        case (T_END)
            call fail(p, "unexpected end of input")

        case default
            call fail(p, "unexpected token '"//p%text//"'")
        end select
    end function parse_primary

    !> Look ahead from just after an opening parenthesis without consuming it.
    function empty_parentheses(p) result(yes)
        type(parser_t), intent(in) :: p
        logical :: yes
        integer :: index, n

        n = len(p%src)
        index = p%pos
        do while (index <= n)
            if (p%src(index:index) /= " " .and. &
                p%src(index:index) /= char(9) .and. &
                p%src(index:index) /= char(10) .and. &
                p%src(index:index) /= char(13)) exit
            index = index + 1
        end do
        yes = index <= n .and. p%src(index:index) == ")"
    end function empty_parentheses

    !> Trailing [[...]] and [...] groups after a primary.
    recursive function parse_postfix(p, a, d, base) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t),          intent(in)    :: base
        type(expr_t)                         :: e
        type(expr_t), allocatable :: fargs(:)
        type(expr_t) :: one(1), pair(2)
        integer :: nargs

        e = base
        do
            if (p%tok == T_BLANK) then
                select case (p%text)
                case ("_"); pair(1) = e; pair(2) = func_in(a, "Blank")
                    e = func("Pattern", pair)
                case ("__"); pair(1) = e; pair(2) = func_in(a, "BlankSequence")
                    e = func("Pattern", pair)
                case ("___"); pair(1) = e; pair(2) = func_in(a, "BlankNullSequence")
                    e = func("Pattern", pair)
                end select
                call advance(p, d)
                cycle
            end if
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
                one(1) = e
                e = func("Apply", one)
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
        logical :: matched

        if (head%kind() == NK_SYM) then
            e = func(chars(head%name()), args(1:nargs))
            return
        end if
        ! Derivative[n][f][x] is the same thing as f'[x], written the long
        ! way. Left as a chain of applications it printed back as
        ! Apply[Apply[Derivative[1], f], x], which reads like an answer and is
        ! not one. Only the exact shape is recognised: one order, one function
        ! symbol, one argument. Anything else stays an unevaluated Apply.
        if (nargs == 1) then
            e = derivative_operator(a, head, args(1), matched)
            if (matched) return
        end if
        allocate (parts(nargs + 1))
        parts(1) = head
        do k = 1, nargs
            parts(k + 1) = args(k)
        end do
        e = func("Apply", parts)
    end function build_apply

    !> The two stages of Derivative[n][f][x].
    !>
    !> Stage one turns Derivative[n] applied to a symbol into a marker node;
    !> stage two turns that marker applied to one argument into the same
    !> Derivative1 node fortsym's own differentiation and the f'[x] postfix
    !> both produce, so all three spellings are one expression.
    function derivative_operator(a, head, argument, matched) result(e)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: head
        type(expr_t),          intent(in)    :: argument
        logical,               intent(out)   :: matched
        type(expr_t)                         :: e
        type(expr_t) :: order, target_fn
        type(expr_t) :: parts(3)
        type(expr_t) :: pair(2)

        matched = .false.
        e = head
        if (head%kind() /= NK_FUNC) return

        if (chars(head%name()) == "Derivative") then
            if (head%nargs() /= 1) return
            order = head%arg(1)
            ! A symbolic or fractional order is a different concept, and a
            ! multi-index Derivative[m, n] differentiates a function of
            ! several variables. Neither is what Derivative1 records.
            if (order%kind() /= NK_INT) return
            if (argument%kind() /= NK_SYM) return
            matched = .true.
            pair(1) = argument
            pair(2) = order
            e = func("DerivativeOperator", pair)
            return
        end if

        if (chars(head%name()) == "DerivativeOperator") then
            if (head%nargs() /= 2) return
            target_fn = head%arg(1)
            order = head%arg(2)
            matched = .true.
            parts(1) = target_fn
            parts(2) = order
            parts(3) = argument
            e = func("Derivative1", parts)
        end if
    end function derivative_operator

    !> Apply a head to one argument, as f @ x does.
    function apply_head(a, d, head, argument) result(e)
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t),          intent(in)    :: head, argument
        type(expr_t)                         :: e
        type(expr_t) :: parts(1), pair(2)

        parts(1) = argument
        if (head%kind() == NK_SYM) then
            ! Canonicalised like a bracketed call, so Sin @ x and Sin[x] are
            ! the same interned node rather than two heads that print alike.
            e = func(chars(fn_canonical(d, chars(head%name()))), parts)
        else
            ! A computed head is not something this subset can apply, so it
            ! stays structural rather than being guessed at.
            pair(1) = head
            pair(2) = argument
            e = func("Apply", pair)
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
        ! Corpus scripts write literal tables of coefficients; eighteen entries
        ! is an ordinary list, not a sign of a runaway parse.
        integer, parameter :: MAX_ARGS = 256

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
            call fail(p, "expected closing bracket on argument list, got '"// &
                p%text//"'")
            return
        end if
        call advance(p, d)
    end subroutine parse_arg_list

end module fortsym_parse
