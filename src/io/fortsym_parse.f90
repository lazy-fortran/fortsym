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
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL
    use fortsym_expr, only: expr_t, sym, num, rat, real_expr, const, func, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_dialect, only: dialect_t, dialect, fn_canonical, &
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

    !> Parser state. Kept in one object so the recursive descent needs no
    !> module-level variables and two parses can never interfere.
    type :: parser_t
        character(:), allocatable :: src
        integer                   :: pos = 1
        ! Current token.
        integer                   :: tok = T_END
        character(:), allocatable :: text
        logical                   :: is_real = .false.
        integer(int64)            :: ivalue = 0_int64
        real(dp)                  :: rvalue = 0.0_dp
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
        yes = is_alpha(c) .or. is_digit(c)
    end function is_name_char

    !> Read the next token into the parser state.
    subroutine advance(p, d)
        type(parser_t),  intent(inout) :: p
        type(dialect_t), intent(in)    :: d
        integer :: n, start
        character :: c

        n = len(p%src)

        do while (p%pos <= n)
            if (p%src(p%pos:p%pos) /= " ") exit
            p%pos = p%pos + 1
        end do

        if (p%pos > n) then
            p%tok = T_END
            p%text = ""
            return
        end if

        c = p%src(p%pos:p%pos)

        if (is_digit(c) .or. (c == "." .and. p%pos < n)) then
            call lex_number(p)
            return
        end if

        if (is_alpha(c)) then
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
        case ("+", "-", "/", "^")
            p%tok = T_OP
            p%text = c
            p%pos = p%pos + 1
        case default
            p%tok = T_ERROR
            p%text = c
            p%pos = p%pos + 1
            call fail(p, "unexpected character '"//c//"'")
        end select
    end subroutine advance

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

        if (p%is_real) then
            read (p%text, *, iostat=ios) p%rvalue
        else
            read (p%text, *, iostat=ios) p%ivalue
        end if
        if (ios /= 0) call fail(p, "malformed number '"//p%text//"'")

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
        read (p%text, *) p%rvalue
    end subroutine skip_kind_suffix

    ! ----------------------------------------------------------- parser --

    !> Binding power of a binary operator, or -1 if it is not one.
    pure function binding_power(op) result(bp)
        character(*), intent(in) :: op
        integer                  :: bp
        select case (op)
        case ("+", "-"); bp = 1
        case ("*", "/"); bp = 2
        case ("**", "^"); bp = 3
        case default; bp = -1
        end select
    end function binding_power

    pure function is_right_assoc(op) result(yes)
        character(*), intent(in) :: op
        logical                  :: yes
        ! Only exponentiation associates to the right: a**b**c is a**(b**c).
        yes = op == "**" .or. op == "^"
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
            case ("-"); e = e - rhs
            case ("*"); e = e*rhs
            case ("/"); e = divide(a, e, rhs)
            case ("**", "^"); e = e**rhs
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
                e = negate(a, parse_binary(p, a, d, 2))
                return
            else if (p%text == "+") then
                call advance(p, d)
                e = parse_binary(p, a, d, 2)
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

        select case (e%kind())
        case (NK_INT)
            r = num(a, -e%int_value())
        case (NK_RAT)
            r = rat(a, -e%int_value(), e%den_value())
        case (NK_REAL)
            r = real_expr(a, -e%real_value())
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

        if (x%kind() == NK_INT .and. y%kind() == NK_INT) then
            if (y%int_value() /= 0_int64) then
                r = rat(a, x%int_value(), y%int_value())
                return
            end if
        end if
        r = x/y
    end function divide

    recursive function parse_primary(p, a, d) result(e)
        type(parser_t),        intent(inout) :: p
        type(arena_t), target, intent(inout) :: a
        type(dialect_t),       intent(in)    :: d
        type(expr_t)                         :: e
        character(:), allocatable :: name, canon
        type(expr_t), allocatable :: fargs(:)
        integer :: nargs

        select case (p%tok)

        case (T_NUMBER)
            if (p%is_real) then
                e = real_expr(a, p%rvalue)
            else
                e = num(a, p%ivalue)
            end if
            call advance(p, d)

        case (T_NAME)
            name = p%text
            call advance(p, d)
            if (p%tok == T_LPAREN) then
                call advance(p, d)
                call parse_arg_list(p, a, d, fargs, nargs)
                if (p%failed) return
                canon = chars(fn_canonical(d, name))
                e = func(canon, fargs(1:nargs))
            else
                ! A bare name is a constant if the dialect knows it as one, and
                ! an ordinary symbol otherwise. Getting this wrong is how
                ! Euler's number becomes a free variable.
                canon = chars(const_canonical(d, name))
                if (canon == "pi" .or. canon == "e" .or. canon == "i") then
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

        case (T_END)
            call fail(p, "unexpected end of input")

        case default
            call fail(p, "unexpected token '"//p%text//"'")
        end select
    end function parse_primary

    recursive subroutine parse_arg_list(p, a, d, fargs, nargs)
        type(parser_t),            intent(inout) :: p
        type(arena_t), target,     intent(inout) :: a
        type(dialect_t),           intent(in)    :: d
        type(expr_t), allocatable, intent(out)   :: fargs(:)
        integer,                   intent(out)   :: nargs
        integer, parameter :: MAX_ARGS = 16

        allocate (fargs(MAX_ARGS))
        nargs = 0

        if (p%tok == T_RPAREN) then
            call advance(p, d)
            call fail(p, "function call with no arguments")
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

        if (p%tok /= T_RPAREN) then
            call fail(p, "expected ')' closing argument list")
            return
        end if
        call advance(p, d)
    end subroutine parse_arg_list

end module fortsym_parse
