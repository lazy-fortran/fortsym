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
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t
    use fortsym_dialect, only: dialect_t, dialect, fn_spelling, const_spelling, &
        DIA_NATIVE, DIA_FORTRAN
    implicit none
    private

    public :: print_expr, print_expr_in, print_expr_sub

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
    function print_expr_in(e, d) result(s)
        type(expr_t),    intent(in) :: e
        type(dialect_t), intent(in) :: d
        type(str_t)                 :: s
        type(strbuf_t) :: b
        integer :: no_ids(0)
        type(str_t) :: no_names(0)
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
    function print_expr_sub(e, d, ids, names) result(s)
        type(expr_t),    intent(in) :: e
        type(dialect_t), intent(in) :: d
        integer,         intent(in) :: ids(:)
        type(str_t),     intent(in) :: names(:)
        type(str_t)                 :: s
        type(strbuf_t) :: b
        call emit(b, e%a, e%id, d, PREC_ADD, ids, names)
        s = b%to_str()
    end function print_expr_sub

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
        call b%append(chars(str(v)))
        call b%append(chars(d%real_suffix))
        if (wrap) call b%append(")")
    end subroutine emit_real

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
        case (NK_REAL)
            yes = a%real_of(id) < 0.0_dp
        case (NK_MUL)
            do k = 1, a%nargs_of(id)
                child = a%arg_of(id, k)
                if (a%kind_of(child) == NK_INT) then
                    if (a%num_of(child) < 0_int64) yes = .true.
                else if (a%kind_of(child) == NK_RAT) then
                    if (a%num_of(child) < 0_int64) yes = .true.
                end if
            end do
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
            call b%append(chars(str(-a%num_of(id))))
        case (NK_RAT)
            call b%append(chars(str(-a%num_of(id))))
            if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
            call b%append("/")
            call b%append(chars(str(a%den_of(id))))
            if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
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

    !> True when a product carries a negative numeric coefficient anywhere in
    !> its factor list. Operands are sorted by node index, so the coefficient is
    !> not necessarily first and cannot be found by looking at one position.
    function product_is_negative(a, factors) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: factors(:)
        logical                   :: yes
        integer :: k
        yes = .false.
        do k = 1, size(factors)
            if (a%kind_of(factors(k)) == NK_INT) then
                if (a%num_of(factors(k)) < 0_int64) yes = .true.
            else if (a%kind_of(factors(k)) == NK_RAT) then
                if (a%num_of(factors(k)) < 0_int64) yes = .true.
            else if (a%kind_of(factors(k)) == NK_REAL) then
                if (a%real_of(factors(k)) < 0.0_dp) yes = .true.
            end if
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
        logical :: wrap, first, coeff_done
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

        ! A product with a negative coefficient reads as a negation. When the
        ! caller has not already emitted the sign (a sum does that for its
        ! non-first terms), emit it here -- otherwise -x prints as "x*-1".
        first = .true.
        coeff_done = .not. negate
        if (.not. negate) then
            if (product_is_negative(a, factors)) then
                call b%append("-")
                coeff_done = .false.
            end if
        end if

        if (nnum == 0) then
            ! Everything moved to the denominator, so the numerator is 1.
            call b%append("1")
            if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
            first = .false.
        else
            do k = 1, nnum
                if (.not. coeff_done .and. is_numeric(a, numer(k))) then
                    ! This is the factor carrying the sign. Negate it, and drop
                    ! it entirely when it is exactly -1.
                    coeff_done = .true.
                    if (is_minus_one(a, numer(k))) then
                        if (nnum == 1) then
                            call b%append("1")
                            if (d%id == DIA_FORTRAN) &
                                call b%append(chars(d%int_real_suffix))
                            first = .false.
                        end if
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

        if (wrap) call b%append(")")
        deallocate (numer, denom)
    end subroutine emit_product_factors

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
            a%kind_of(id) == NK_REAL
    end function is_numeric

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

        call b%append(chars(fn_spelling(d, chars(a%name_of(id)))))
        call b%append("(")
        do k = 1, a%nargs_of(id)
            if (k > 1) call b%append(", ")
            ! Arguments sit inside parentheses already, so they need none of
            ! their own whatever their precedence.
            call emit(b, a, a%arg_of(id, k), d, PREC_ADD, ids, names)
        end do
        call b%append(")")
    end subroutine emit_function

end module fortsym_print
