module fortsym_print
    ! Rendering an expression as text in a chosen dialect.
    !
    ! The arena stores a normalised structure: subtraction is addition of a
    ! negation, division is multiplication by a reciprocal power, and operands
    ! of sums and products are sorted. That is right for interning and wrong for
    ! reading, so the printer undoes it -- a product whose factors include
    ! x**(-1) prints as a quotient, a term with a negative coefficient prints
    ! with a minus sign instead of "+ -1*", and a leading -1 factor prints as
    ! unary minus.
    !
    ! Parenthesisation is driven by operator precedence, not by wrapping
    ! everything. Redundant parentheses in generated Fortran are not merely
    ! ugly: they defeat the round-trip test that keeps printer and parser in
    ! agreement, and they make emitted kernels hard to review against the
    ! mathematics they came from.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL
    use fortsym_expr, only: expr_t
    use fortsym_exact, only: exact_to_real
    use fortsym_dialect, only: dialect_t, dialect, fn_spelling, const_spelling, &
        DIA_NATIVE, DIA_FORTRAN, DIA_WOLFRAM
    implicit none
    private

    public :: print_expr, print_expr_in, print_expr_sub, fortran_representable
    public :: fortran_roots_representable

    integer, parameter :: dp = real64

    ! Binding powers. A child is parenthesised when its own precedence is lower
    ! than the context it appears in.
    integer, parameter :: PREC_ADD = 1
    integer, parameter :: PREC_MUL = 2
    integer, parameter :: PREC_POW = 3
    integer, parameter :: PREC_ATOM = 4

    !> pi to full real64 precision, for dialects with no symbolic constant.
    real(dp), parameter :: PI_VALUE = &
        3.141592653589793238462643383279502884_dp
    real(dp), parameter :: E_VALUE = &
        2.718281828459045235360287471352662498_dp

contains

    !> Render in fortsym's own notation.
    function print_expr(e) result(s)
        type(expr_t), intent(in) :: e
        type(str_t)              :: s
        s = print_expr_in(e, dialect(DIA_NATIVE))
    end function print_expr

    !> Render in a given dialect.
    function print_expr_in(e, d, ok) result(s)
        type(expr_t),    intent(in) :: e
        type(dialect_t), intent(in) :: d
        logical, intent(out), optional :: ok
        type(str_t)                 :: s
        type(strbuf_t) :: b
        integer :: no_ids(0)
        type(str_t) :: no_names(0)
        logical :: valid
        valid = .true.
        if (d%id == DIA_FORTRAN) valid = fortran_representable(e)
        if (present(ok)) ok = valid
        if (.not. valid) then
            s = str("")
            return
        end if
        call emit(b, e%a, e%id, d, PREC_ADD, no_ids, no_names)
        s = b%to_str()
    end function print_expr_in

    !> Render with some nodes replaced by names.
    !>
    !> This is what lets common subexpression elimination reuse the printer: a
    !> node that has been assigned to a temporary is emitted as that temporary's
    !> name instead of being expanded again. Keeping it here rather than in a
    !> second printer means generated kernels inherit every parenthesisation and
    !> literal-formatting rule automatically.
    function print_expr_sub(e, d, ids, names, ok, prechecked) result(s)
        type(expr_t),    intent(in) :: e
        type(dialect_t), intent(in) :: d
        integer,         intent(in) :: ids(:)
        type(str_t),     intent(in) :: names(:)
        logical, intent(out), optional :: ok
        logical, intent(in), optional :: prechecked
        type(str_t)                 :: s
        type(strbuf_t) :: b
        logical :: valid
        valid = .true.
        if (d%id == DIA_FORTRAN) then
            if (present(prechecked)) then
                valid = prechecked
            else
                valid = fortran_representable(e)
            end if
        end if
        if (present(ok)) ok = valid
        if (.not. valid) then
            s = str("")
            return
        end if
        call emit(b, e%a, e%id, d, PREC_ADD, ids, names)
        s = b%to_str()
    end function print_expr_sub

    !> Whether every arbitrary exact node reachable from e has a finite normal
    !> nearest-even binary64 projection for a real(dp) Fortran kernel.
    function fortran_representable(e) result(ok)
        type(expr_t), intent(in) :: e
        logical                  :: ok
        logical, allocatable :: visited(:)

        allocate (visited(e%a%size()), source=.false.)
        ok = fortran_node_representable(e%a, e%id, visited)
    end function fortran_representable

    function fortran_roots_representable(roots) result(ok)
        type(expr_t), intent(in) :: roots(:)
        logical                  :: ok
        logical, allocatable :: visited(:)
        integer :: k

        ok = .true.
        if (size(roots) == 0) return
        allocate (visited(roots(1)%a%size()), source=.false.)
        do k = 1, size(roots)
            ok = fortran_node_representable(roots(k)%a, roots(k)%id, visited)
            if (.not. ok) return
        end do
    end function fortran_roots_representable

    recursive function fortran_node_representable(a, id, visited) result(ok)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical,       intent(inout) :: visited(:)
        logical                  :: ok
        real(dp) :: projected
        integer :: k
        character(:), allocatable :: text

        ok = .true.
        if (visited(id)) return
        visited(id) = .true.
        select case (a%kind_of(id))
        case (NK_BIG_INT, NK_BIG_RAT)
            projected = exact_to_real(chars(a%exact_text_of(id)), ok)
            if (ok) ok = ieee_is_finite(projected)
            return
        case (NK_BIG_REAL)
            text = chars(a%real_text_of(id))
            read (text, *, iostat=k) projected
            if (k /= 0) then
                ok = .false.
            else
                ok = ieee_is_finite(projected)
            end if
            return
        end select
        do k = 1, a%nargs_of(id)
            ok = fortran_node_representable(a, a%arg_of(id, k), visited)
            if (.not. ok) return
        end do
    end function fortran_node_representable

    !> Index of `id` in the substitution table, or 0.
    pure function subst_slot(id, ids) result(k)
        integer, intent(in) :: id, ids(:)
        integer             :: k
        integer :: j
        k = 0
        do j = 1, size(ids)
            if (ids(j) == id) then
                k = j
                return
            end if
        end do
    end function subst_slot

    !> Append the rendering of node `id`, parenthesising if its precedence is
    !> below `context`.
    recursive subroutine emit(b, a, id, d, context, ids, names)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        integer :: slot

        ! A node standing in for a temporary is emitted as its name, and its
        ! subtree is not walked -- that is the whole saving of CSE.
        slot = subst_slot(id, ids)
        if (slot > 0) then
            call b%append(chars(names(slot)))
            return
        end if

        select case (a%kind_of(id))
        case (NK_INT)
            call emit_integer(b, a, id, d, context)
        case (NK_RAT)
            call emit_rational(b, a, id, d, context)
        case (NK_BIG_INT, NK_BIG_RAT)
            call emit_big_exact(b, a, id, d, context)
        case (NK_BIG_REAL)
            call emit_big_real(b, a, id, d, context)
        case (NK_REAL)
            call emit_real(b, a, id, d, context)
        case (NK_SYM)
            call b%append(chars(a%name_of(id)))
        case (NK_CONST)
            call emit_constant(b, a, id, d)
        case (NK_ADD)
            call emit_sum(b, a, id, d, context, ids, names)
        case (NK_MUL)
            call emit_product(b, a, id, d, context, ids, names)
        case (NK_POW)
            call emit_power(b, a, id, d, context, ids, names)
        case (NK_FUNC)
            call emit_function(b, a, id, d, ids, names)
        case default
            call b%append("<?>")
        end select
    end subroutine emit

    ! ------------------------------------------------------------- atoms --

    !> A negative integer binds like a product, because -2**2 would otherwise
    !> read as (-2)**2 in a power context.
    subroutine emit_integer(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer(int64) :: v
        logical :: wrap

        v = a%num_of(id)
        ! Parenthesise a negative literal from multiplicative context upward.
        ! Without this a term following a minus sign emits as "2 - -1", which no
        ! Fortran compiler accepts: two arithmetic operators cannot be adjacent.
        wrap = v < 0_int64 .and. context >= PREC_MUL
        if (wrap) call b%append("(")
        call b%append(chars(str(v)))
        if (wrap) call b%append(")")
    end subroutine emit_integer

    !> An exact rational. Where the dialect allows it this is a/b; in Fortran it
    !> must become a quotient of typed reals, because 1/3 in Fortran is integer
    !> division and evaluates to zero.
    subroutine emit_rational(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        logical :: wrap

        wrap = context > PREC_MUL
        if (wrap) call b%append("(")
        call b%append(chars(str(a%num_of(id))))
        if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
        call b%append("/")
        call b%append(chars(str(a%den_of(id))))
        if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
        if (wrap) call b%append(")")
    end subroutine emit_rational

    !> An arbitrary-precision exact scalar. Native and CAS dialects accept the
    !> canonical decimal spelling directly. Fortran receives real literals so
    !> the source remains valid even though it has no unbounded integer kind.
    subroutine emit_big_exact(b, a, id, d, context, magnitude_only)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        logical, intent(in), optional :: magnitude_only
        character(:), allocatable :: text, numerator, denominator
        integer :: slash, first
        logical :: negative, magnitude, wrap
        real(dp) :: projected
        logical :: converted

        text = chars(a%exact_text_of(id))
        magnitude = .false.
        if (present(magnitude_only)) magnitude = magnitude_only
        if (d%id == DIA_FORTRAN) then
            projected = exact_to_real(text, converted)
            if (.not. converted) then
                return
            end if
            converted = ieee_is_finite(projected)
            if (.not. converted) then
                return
            end if
            if (magnitude) projected = abs(projected)
            wrap = projected < 0.0_dp .and. context >= PREC_MUL
            if (wrap) call b%append("(")
            call b%append(chars(str(projected)))
            call b%append(chars(d%real_suffix))
            if (wrap) call b%append(")")
            return
        end if
        negative = text(1:1) == "-"
        first = 1
        if (negative .and. magnitude) first = 2
        slash = index(text, "/")
        wrap = (negative .and. .not. magnitude .and. context >= PREC_MUL) .or. &
            (slash > 0 .and. context >= PREC_MUL)
        if (wrap) call b%append("(")

        if (slash == 0) then
            call b%append(text(first:))
        else
            numerator = text(first:slash - 1)
            denominator = text(slash + 1:)
            call b%append(numerator)
            call b%append("/")
            call b%append(denominator)
        end if
        if (wrap) call b%append(")")
    end subroutine emit_big_exact

    subroutine emit_real(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        real(dp) :: v
        logical :: wrap

        v = a%real_of(id)
        ! Same reason as emit_integer: "- -1.0_dp" is not valid Fortran.
        wrap = v < 0.0_dp .and. context >= PREC_MUL
        if (wrap) call b%append("(")
        if (d%compact_reals) then
            call b%append(compact_real(v))
        else
            call b%append(chars(str(v)))
        end if
        call b%append(chars(d%real_suffix))
        if (wrap) call b%append(")")
    end subroutine emit_real

    !> Emit a retained MPFR decimal without reducing it to real64 in CAS
    !> dialects. Fortran has only the real64 kernel path, so it receives the
    !> same checked projection used by fortran_representable.
    subroutine emit_big_real(b, a, id, d, context, magnitude_only)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        logical, intent(in), optional  :: magnitude_only
        character(:), allocatable :: text
        real(dp) :: projected
        logical :: magnitude, negative, wrap
        integer :: ios

        text = chars(a%real_text_of(id))
        magnitude = .false.
        if (present(magnitude_only)) magnitude = magnitude_only
        negative = .false.
        if (len(text) > 0) negative = text(1:1) == "-"

        if (d%id == DIA_FORTRAN) then
            read (text, *, iostat=ios) projected
            if (ios /= 0 .or. .not. ieee_is_finite(projected)) return
            if (magnitude) projected = abs(projected)
            wrap = projected < 0.0_dp .and. context >= PREC_MUL
            if (wrap) call b%append("(")
            call b%append(chars(str(projected)))
            call b%append(chars(d%real_suffix))
            if (wrap) call b%append(")")
            return
        end if

        if (magnitude .and. negative) then
            text = text(2:)
            negative = .false.
        end if
        if (d%id == DIA_WOLFRAM) text = wolfram_exponent(text)
        wrap = negative .and. context >= PREC_MUL
        if (wrap) call b%append("(")
        call b%append(text)
        if (wrap) call b%append(")")
    end subroutine emit_big_real

    !> Shortest decimal form that still round-trips through real64.
    !>
    !> The default 17-significant-digit form is right for generated Fortran,
    !> where losing a digit changes the compiled constant. It is wrong for
    !> comparing against a CAS: Mathics prints 2.5, and 2.5000000000000000E+000
    !> is structurally different text for the same number, so every real in the
    !> corpus would be scored as a disagreement.
    !>
    !> Tries increasing precision and stops at the first one that reads back
    !> exactly, so the shortening can never change the value.
    function compact_real(v) result(text)
        real(dp), intent(in)      :: v
        character(:), allocatable :: text
        character(32) :: buf
        real(dp) :: back
        integer :: digits, ios

        ! Fixed point first, within the range where it stays short. g0 switches
        ! to an exponent below 1e-3 and prints 0.001 as 0.1E-002, which is the
        ! same number spelled differently from every CAS this is compared with.
        if (v == 0.0_dp .or. (abs(v) >= 1.0e-4_dp .and. abs(v) < 1.0e16_dp)) then
            do digits = 1, 17
                write (buf, "(f0." // int_text(digits) // ")") v
                read (buf, *, iostat=ios) back
                if (ios == 0) then
                    if (back == v) then
                        text = with_leading_zero(trim(adjustl(buf)))
                        return
                    end if
                end if
            end do
        end if

        ! Outside that range, Wolfram's InputForm writes 1.*^-9 rather than an
        ! E exponent, and that is what the comparator parses back.
        do digits = 1, 17
            write (buf, "(es0." // int_text(digits) // ")") v
            read (buf, *, iostat=ios) back
            if (ios == 0) then
                if (back == v) then
                    text = wolfram_exponent(trim(adjustl(buf)))
                    return
                end if
            end if
        end do
        text = chars(str(v))
    end function compact_real

    !> ".001" is not a numeral in most readers; "0.001" is.
    function with_leading_zero(raw) result(text)
        character(*), intent(in)  :: raw
        character(:), allocatable :: text
        if (len(raw) > 0 .and. raw(1:1) == ".") then
            text = "0"//raw
        else if (len(raw) > 1 .and. raw(1:2) == "-.") then
            text = "-0."//raw(3:)
        else
            text = raw
        end if
    end function with_leading_zero

    !> Turn Fortran's 1.0E-09 into Wolfram's 1.0*^-9.
    function wolfram_exponent(raw) result(text)
        character(*), intent(in)  :: raw
        character(:), allocatable :: text, mantissa, expo
        integer :: epos, value, ios

        epos = scan(raw, "EeDd")
        if (epos == 0) then
            text = with_leading_zero(raw)
            return
        end if
        mantissa = with_leading_zero(raw(1:epos - 1))
        read (raw(epos + 1:), *, iostat=ios) value
        if (ios /= 0) then
            text = with_leading_zero(raw)
            return
        end if
        expo = int_text(value)
        if (value >= 0) then
            text = mantissa//"*^"//expo
        else
            text = mantissa//"*^-"//int_text(-value)
        end if
    end function wolfram_exponent

    function int_text(k) result(text)
        integer, intent(in)       :: k
        character(:), allocatable :: text
        character(8) :: buf
        write (buf, "(i0)") k
        text = trim(buf)
    end function int_text

    subroutine emit_constant(b, a, id, d)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        character(:), allocatable :: name

        name = chars(a%name_of(id))

        if (d%numeric_constants) then
            ! No symbolic constants in this dialect, so emit the value at full
            ! precision rather than letting the name escape as a free variable.
            select case (name)
            case ("pi")
                call b%append(chars(str(PI_VALUE)))
                call b%append(chars(d%real_suffix))
            case ("e")
                call b%append(chars(str(E_VALUE)))
                call b%append(chars(d%real_suffix))
            case default
                call b%append(name)
            end select
        else
            call b%append(chars(const_spelling(d, name)))
        end if
    end subroutine emit_constant

    ! --------------------------------------------------------- operators --

    !> A sum. Terms carrying a negative coefficient are printed with a minus
    !> sign and their coefficient negated, so the output reads a - b rather than
    !> a + (-1)*b.
    recursive subroutine emit_sum(b, a, id, d, context, ids, names)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        integer :: k, child
        logical :: wrap

        wrap = context > PREC_ADD
        if (wrap) call b%append("(")

        do k = 1, a%nargs_of(id)
            child = a%arg_of(id, k)
            if (k == 1) then
                call emit(b, a, child, d, PREC_ADD, ids, names)
            else if (is_negative_term(a, child)) then
                call b%append(" - ")
                call emit_negated(b, a, child, d, PREC_ADD, ids, names)
            else
                call b%append(" + ")
                call emit(b, a, child, d, PREC_ADD, ids, names)
            end if
        end do

        if (wrap) call b%append(")")
    end subroutine emit_sum

    !> Does this term carry an explicitly negative numeric coefficient?
    function is_negative_term(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k, child

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT)
            yes = a%num_of(id) < 0_int64
        case (NK_RAT)
            yes = a%num_of(id) < 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            yes = exact_is_negative(a, id)
        case (NK_REAL)
            yes = a%real_of(id) < 0.0_dp
        case (NK_BIG_REAL)
            yes = big_real_is_negative(a, id)
        case (NK_MUL)
            if (count_big_exact_factor(a, id) > 1) return
            do k = 1, a%nargs_of(id)
                child = a%arg_of(id, k)
                if (numeric_is_negative(a, child)) yes = .not. yes
            end do
            if (has_big_exact_factor(a, id)) then
                if (yes) yes = has_negative_big_exact_factor(a, id)
            end if
        end select
    end function is_negative_term

    !> Print a term with its sign removed; the caller has already emitted "-".
    recursive subroutine emit_negated(b, a, id, d, context, ids, names)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(arena_t), pointer :: ap
        integer :: k, n, negated
        integer, allocatable :: factors(:)

        select case (a%kind_of(id))
        case (NK_INT)
            ! No kind suffix: an integer stays an integer, exactly as in
            ! emit_integer. Suffixing here turned a negated -1 into 1.0_dp and
            ! put a real literal where an integer exponent belonged.
            call emit_compact_exact_magnitude(b, a, id, d, context)
        case (NK_RAT)
            call emit_compact_exact_magnitude(b, a, id, d, context)
        case (NK_BIG_INT, NK_BIG_RAT)
            call emit_big_exact(b, a, id, d, context, magnitude_only=.true.)
        case (NK_BIG_REAL)
            call emit_big_real(b, a, id, d, context, magnitude_only=.true.)
        case (NK_REAL)
            call b%append(chars(str(-a%real_of(id))))
            call b%append(chars(d%real_suffix))
        case (NK_MUL)
            ! Rebuild the factor list with the negative coefficient flipped. A
            ! coefficient of exactly -1 disappears, so -1*x prints as x rather
            ! than 1*x.
            n = a%nargs_of(id)
            allocate (factors(n))
            negated = 0
            do k = 1, n
                factors(k) = a%arg_of(id, k)
            end do
            call emit_product_factors(b, a, factors, d, context, ids, names, negate=.true.)
            deallocate (factors)
        case default
            call emit(b, a, id, d, context, ids, names)
        end select
    end subroutine emit_negated

    !> Emit the magnitude of a compact exact value without negating int64 in
    !> Fortran. Decimal slicing is defined even for INT64_MIN.
    subroutine emit_compact_exact_magnitude(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        character(:), allocatable :: text
        integer :: slash, first
        logical :: wrap

        text = chars(a%exact_text_of(id))
        first = 1
        if (text(1:1) == "-") first = 2
        slash = index(text, "/")
        wrap = slash > 0 .and. context > PREC_MUL
        if (wrap) call b%append("(")
        if (slash == 0) then
            call b%append(text(first:))
        else
            call b%append(text(first:slash - 1))
            if (d%id == DIA_FORTRAN) &
                call b%append(chars(d%int_real_suffix))
            call b%append("/")
            call b%append(text(slash + 1:))
            if (d%id == DIA_FORTRAN) &
                call b%append(chars(d%int_real_suffix))
        end if
        if (wrap) call b%append(")")
    end subroutine emit_compact_exact_magnitude

    !> A product, split into numerator and denominator. Factors that are powers
    !> with a negative integer exponent move to the denominator with the sign of
    !> the exponent flipped, so x*y**(-1) prints as x/y.
    recursive subroutine emit_product(b, a, id, d, context, ids, names)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        integer, allocatable :: factors(:)
        integer :: k, n

        n = a%nargs_of(id)
        allocate (factors(n))
        do k = 1, n
            factors(k) = a%arg_of(id, k)
        end do
        call emit_product_factors(b, a, factors, d, context, ids, names, negate=.false.)
        deallocate (factors)
    end subroutine emit_product

    !> True when a product has an odd number of negative numeric factors.
    !> Engine results may retain several numeric factors, so testing only
    !> whether any factor is negative emits doubled signs for expressions such
    !> as (-2)*(-1)*x.
    function product_is_negative(a, factors) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: factors(:)
        logical                   :: yes
        integer :: k
        yes = .false.
        do k = 1, size(factors)
            if (numeric_is_negative(a, factors(k))) yes = .not. yes
        end do
    end function product_is_negative

    recursive subroutine emit_product_factors(b, a, factors, d, context, ids, &
            names, negate)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: factors(:)
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        logical,         intent(in)    :: negate
        integer :: k, nnum, nden, base, expo
        logical :: wrap, first, negative, structural_sign, preserve_signs
        integer, allocatable :: numer(:), denom(:)

        allocate (numer(size(factors)), denom(size(factors)))
        nnum = 0
        nden = 0

        do k = 1, size(factors)
            if (is_reciprocal(a, factors(k), base, expo)) then
                nden = nden + 1
                denom(nden) = factors(k)
            else
                nnum = nnum + 1
                numer(nnum) = factors(k)
            end if
        end do

        wrap = context > PREC_MUL
        if (wrap) call b%append("(")

        ! Emit one leading sign for the parity of every numeric sign. The
        ! caller's negate flag removes the sign already emitted by a sum.
        first = .true.
        negative = product_is_negative(a, factors)
        if (negate) negative = .not. negative
        structural_sign = negative .and. .not. negate .and. &
            has_negative_big_exact_in(factors, a) .and. &
            count_big_exact_in(factors, a) == 1
        preserve_signs = .not. negate .and. .not. structural_sign .and. &
            has_big_exact_in(factors, a) .and. &
            has_negative_numeric_in(factors, a)
        if (structural_sign) then
            call b%append("-(")
        else if (negative .and. .not. preserve_signs) then
            call b%append("-")
        end if

        if (nnum == 0) then
            ! Everything moved to the denominator, so the numerator is 1.
            call b%append("1")
            if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
            first = .false.
        else
            do k = 1, nnum
                if (preserve_signs) then
                    if (.not. first) call b%append("*")
                    call emit(b, a, numer(k), d, PREC_MUL, ids, names)
                    first = .false.
                    cycle
                end if
                if (numeric_is_negative(a, numer(k))) then
                    ! The common leading sign already represents this factor's
                    ! sign. Emit its magnitude and drop unit factors.
                    if (is_minus_one(a, numer(k))) then
                        cycle
                    end if
                    if (.not. first) call b%append("*")
                    call emit_negated(b, a, numer(k), d, PREC_MUL, ids, names)
                    first = .false.
                    cycle
                end if
                if (.not. first) call b%append("*")
                call emit(b, a, numer(k), d, PREC_MUL, ids, names)
                first = .false.
            end do
            if (first) then
                call b%append("1")
                if (d%id == DIA_FORTRAN) &
                    call b%append(chars(d%int_real_suffix))
                first = .false.
            end if
        end if

        do k = 1, nden
            call b%append("/")
            if (is_reciprocal(a, denom(k), base, expo)) then
                if (expo == -1) then
                    ! Plain reciprocal: print the base at power precedence so a
                    ! compound base keeps its parentheses.
                    call emit(b, a, base, d, PREC_POW, ids, names)
                else
                    ! A higher reciprocal power: print base**|expo|.
                    call emit(b, a, base, d, PREC_POW, ids, names)
                    call b%append(chars(d%power))
                    call b%append(chars(str(-expo)))
                end if
            end if
        end do

        if (structural_sign) call b%append(")")
        if (wrap) call b%append(")")
        deallocate (numer, denom)
    end subroutine emit_product_factors

    function has_big_exact_in(factors, a) result(yes)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        logical                   :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, size(factors)
            kind = a%kind_of(factors(k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            yes = .true.
            return
        end do
    end function has_big_exact_in

    function count_big_exact_in(factors, a) result(count)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        integer                   :: count
        integer :: k, kind

        count = 0
        do k = 1, size(factors)
            kind = a%kind_of(factors(k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            count = count + 1
        end do
    end function count_big_exact_in

    function has_negative_big_exact_in(factors, a) result(yes)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        logical                   :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, size(factors)
            kind = a%kind_of(factors(k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            if (.not. exact_is_negative(a, factors(k))) cycle
            yes = .true.
            return
        end do
    end function has_negative_big_exact_in

    function has_negative_numeric_in(factors, a) result(yes)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        logical                   :: yes
        integer :: k

        yes = .false.
        do k = 1, size(factors)
            if (.not. numeric_is_negative(a, factors(k))) cycle
            yes = .true.
            return
        end do
    end function has_negative_numeric_in

    function has_big_exact_factor(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, a%nargs_of(id)
            kind = a%kind_of(a%arg_of(id, k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            yes = .true.
            return
        end do
    end function has_big_exact_factor

    function count_big_exact_factor(a, id) result(count)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer                   :: count
        integer :: k, kind

        count = 0
        do k = 1, a%nargs_of(id)
            kind = a%kind_of(a%arg_of(id, k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            count = count + 1
        end do
    end function count_big_exact_factor

    function has_negative_big_exact_factor(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k, child, kind

        yes = .false.
        do k = 1, a%nargs_of(id)
            child = a%arg_of(id, k)
            kind = a%kind_of(child)
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            if (.not. exact_is_negative(a, child)) cycle
            yes = .true.
            return
        end do
    end function has_negative_big_exact_factor

    !> True when the node is a power with a negative integer exponent, in which
    !> case it belongs in a denominator. Returns the base and the exponent.
    function is_reciprocal(a, id, base, expo) result(yes)
        type(arena_t), intent(in)  :: a
        integer,       intent(in)  :: id
        integer,       intent(out) :: base, expo
        logical                    :: yes
        integer :: e_id

        yes = .false.
        base = 0
        expo = 0
        if (a%kind_of(id) /= NK_POW) return
        e_id = a%arg_of(id, 2)
        if (a%kind_of(e_id) /= NK_INT) return
        if (a%num_of(e_id) >= 0_int64) return
        base = a%arg_of(id, 1)
        expo = int(a%num_of(e_id))
        yes = .true.
    end function is_reciprocal

    function is_numeric(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        yes = a%kind_of(id) == NK_INT .or. a%kind_of(id) == NK_RAT .or. &
            a%kind_of(id) == NK_BIG_INT .or. a%kind_of(id) == NK_BIG_RAT .or. &
            a%kind_of(id) == NK_REAL .or. a%kind_of(id) == NK_BIG_REAL
    end function is_numeric

    function numeric_is_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            yes = a%num_of(id) < 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            yes = exact_is_negative(a, id)
        case (NK_REAL)
            yes = a%real_of(id) < 0.0_dp
        case (NK_BIG_REAL)
            yes = big_real_is_negative(a, id)
        end select
    end function numeric_is_negative

    pure function big_real_is_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        character(:), allocatable :: text

        text = chars(a%real_text_of(id))
        yes = .false.
        if (len(text) > 0) yes = text(1:1) == "-"
    end function big_real_is_negative

    function exact_is_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        character(:), allocatable :: text

        text = chars(a%exact_text_of(id))
        yes = len(text) > 0
        if (yes) yes = text(1:1) == "-"
    end function exact_is_negative

    function is_minus_one(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        yes = .false.
        if (a%kind_of(id) == NK_INT) yes = a%num_of(id) == -1_int64
    end function is_minus_one

    !> Power. Exponentiation is right-associative, so the base is printed at a
    !> higher precedence than the exponent: a**b**c must not become (a**b)**c.
    recursive subroutine emit_power(b, a, id, d, context, ids, names)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        logical :: wrap

        wrap = context > PREC_POW
        if (wrap) call b%append("(")
        call emit(b, a, a%arg_of(id, 1), d, PREC_POW + 1, ids, names)
        call b%append(chars(d%power))
        call emit(b, a, a%arg_of(id, 2), d, PREC_POW, ids, names)
        if (wrap) call b%append(")")
    end subroutine emit_power

    recursive subroutine emit_function(b, a, id, d, ids, names)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        integer :: k

        ! List[] is an internal spelling for the empty list. Wolfram's
        ! InputForm writes that value as {}, and preserving the surface form is
        ! necessary for an independent parser to distinguish it from an empty
        ! call such as Directory[].
        if (d%id == DIA_WOLFRAM .and. chars(a%name_of(id)) == "List" .and. &
            a%nargs_of(id) == 0) then
            call b%append("{}")
            return
        end if

        call b%append(chars(fn_spelling(d, chars(a%name_of(id)))))
        ! Wolfram applies with brackets. Emitting parentheses here would produce
        ! text this dialect's own parser reads as a product, so the round trip
        ! would silently change the expression instead of failing.
        if (d%bracket_application) then
            call b%append("[")
        else
            call b%append("(")
        end if
        do k = 1, a%nargs_of(id)
            if (k > 1) call b%append(", ")
            ! Arguments sit inside brackets already, so they need none of
            ! their own whatever their precedence.
            if (d%id == DIA_FORTRAN .and. &
                a%kind_of(a%arg_of(id, k)) == NK_INT .and. &
                fortran_real_intrinsic_argument(a, id, k)) then
                ! Fortran does not convert integer actual arguments for generic
                ! intrinsics such as max/min/atan2, even when another argument
                ! is real, so an integer literal has to be written as a real.
                call b%append(chars(str(a%num_of(a%arg_of(id, k)))))
                call b%append(chars(d%int_real_suffix))
            else
                call emit(b, a, a%arg_of(id, k), d, PREC_ADD, ids, names)
            end if
        end do
        if (d%bracket_application) then
            call b%append("]")
        else
            call b%append(")")
        end if
    end subroutine emit_function

    !> True when this Fortran intrinsic takes a real actual argument in this
    !> position, so an integer literal there has to be written as a real.
    !>
    !> The test is a whitelist of intrinsics rather than a test for what is not
    !> one. Indexing a declared array reaches emit_function with exactly the
    !> shape of a call -- x(1) is indistinguishable from a one-argument
    !> application -- and a subscript must stay an integer. Treating every
    !> unrecognised head as an intrinsic emitted x(1.0_dp), which is not valid
    !> Fortran.
    pure function fortran_real_intrinsic_argument(a, id, position) &
            result(is_real)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, position
        logical :: is_real

        select case (chars(a%name_of(id)))
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
              "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", &
              "exp", "log", "log10", "sqrt", "abs", &
              "erf", "erfc", "gamma", "max", "min")
            is_real = .true.
        case ("besselj")
            ! bessel_jn takes an integer order followed by a real value.
            is_real = position > 1
        case default
            ! A declared kernel array being indexed, or any other head fortsym
            ! does not know to be an intrinsic. Its integers stay integers.
            is_real = .false.
        end select
    end function fortran_real_intrinsic_argument

end module fortsym_print
