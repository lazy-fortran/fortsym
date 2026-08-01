module fortsym_trigrewrite
    ! Directed rewrites between the equivalent forms of trigonometric and
    ! power expressions: angle expansion, product linearisation, Euler's
    ! formulae in both directions, and the exponent laws for products.
    !
    ! Every rule implemented here is an *identity*, true wherever both sides
    ! are defined, and each rewrite is applied only in the shape where that is
    ! literally so. The family is small on purpose. The classical way to get
    ! this wrong is `power_expand`: `(a*b)^p -> a^p b^p` and `(a^m)^n ->
    ! a^(m n)` are false for non-integer exponents once the base can be
    ! negative or complex -- `sqrt((-1)*(-1))` is 1 while `sqrt(-1)*sqrt(-1)`
    ! is -1, and `((-2)^2)^(1/2)` is 2 while `(-2)^(2*(1/2))` is -2. Systems
    ! that apply those laws unconditionally hand back a sign error dressed as
    ! a simplification. Here the rewrite happens when the outer exponent is an
    ! integer, or when the assumption context establishes that the base
    ! factors are positive, and otherwise the whole call is refused with the
    ! reason. A refusal costs the caller a rewrite; the alternative costs them
    ! a sign.
    !
    ! Two policies worth stating, because they are choices rather than
    ! consequences:
    !
    !   * a head with no rule is traversed, not refused, wherever leaving it
    !     alone is itself correct -- `trig_expand` of `besselj(0, x) + sin(a
    !     + b)` expands the sine and copies the Bessel term. A head is refused
    !     only where there was visibly something to do and no sound rule for
    !     it: `tan` of a compound angle, and the power cases above.
    !   * expansion is bounded. `cos(n*x)` costs 2^n products before any
    !     collection of like terms, so a total angle multiplicity above
    !     MAX_MULTIPLICITY, or more than MAX_TRIG_FACTORS trig factors in one
    !     product, is refused as too large rather than attempted. A refusal
    !     that names the bound is recoverable; an expression that eats the
    !     arena is not.
    !
    ! Results are not simplified. `sin(x + 0)` expands to a sum containing
    ! `sin(x)*cos(0)`; deciding `cos(0)` is one is the engines' job, and the
    ! numeric probe in the tests checks values rather than shapes for exactly
    ! that reason.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, num, rat, func, func_in, &
                            i_expr, is_valid, &
                            sin, cos, exp, sqrt, sinh, cosh, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**), operator(==), &
                            operator(/=)
    use fortsym_assume, only: assumption_context_t, FACT_POSITIVE, FACT_REAL
    implicit none
    private

    public :: trig_expand, trig_reduce, trig_to_exp, exp_to_trig, power_expand

    integer, parameter :: dp = real64

    ! Sum of |coefficient| over the atoms of one angle. sin(n*x) expands into
    ! 2^n signed products, so this is a hard size bound, not a preference.
    integer, parameter :: MAX_MULTIPLICITY = 10

    ! Trig factors in one product handed to the linearisation. Each factor
    ! doubles the term count, so this bounds a product-to-sum at 2^7 terms.
    integer, parameter :: MAX_TRIG_FACTORS = 8

    ! Working room for the linearised term list, sized to the bound above.
    integer, parameter :: MAX_TERMS = 256

    ! Largest magnitude accepted as an integer literal. Anything bigger is
    ! reported as "not an integer" rather than silently wrapping, which would
    ! turn a huge exponent into a small one and license a rewrite.
    integer, parameter :: INT_LIMIT = 1000000

    ! Trig classification used inside the linearisation. NOT_TRIG marks the
    ! bare coefficient carried before any trig factor has been absorbed.
    integer, parameter :: NOT_TRIG = 0
    integer, parameter :: IS_SIN = 1
    integer, parameter :: IS_COS = 2

contains

    ! ------------------------------------------------------- trig_expand --

    !> Angle-sum and multiple-angle expansion of every sine and cosine in the
    !> expression. `sin(a+b)` becomes the two-term sum, `cos(3*x)` the
    !> Chebyshev polynomial in `cos(x)` and `sin(x)`.
    subroutine trig_expand(e, expanded, ok, why)
        type(expr_t),              intent(in)  :: e
        type(expr_t),              intent(out) :: expanded
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "trig_expand: the expression is not valid"
            return
        end if

        ok = .true.
        expanded = expand_node(e, ok, why)
        if (.not. ok) expanded = e
    end subroutine trig_expand

    recursive function expand_node(e, ok, why) result(r)
        type(expr_t),              intent(in)    :: e
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t), allocatable :: parts(:)
        character(:), allocatable :: head
        integer :: k, n

        r = e
        if (.not. ok) return

        select case (e%kind())

        case (NK_ADD, NK_MUL)
            n = e%nargs()
            allocate (parts(n))
            do k = 1, n
                parts(k) = expand_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            r = combine(e%a, parts, e%kind() == NK_ADD)

        case (NK_POW)
            r = expand_node(e%arg(1), ok, why)
            if (.not. ok) return
            r = r**expand_node(e%arg(2), ok, why)

        case (NK_FUNC)
            head = chars(e%name())
            n = e%nargs()
            if (n == 0) then
                r = func_in(e%a, head)
                return
            end if
            allocate (parts(n))
            do k = 1, n
                parts(k) = expand_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do

            if (n == 1) then
                if (head == "sin" .or. head == "cos") then
                    r = expand_trig(parts(1), head == "sin", ok, why)
                    return
                end if
                ! tan(a+b) has an addition formula, but its two-sided form
                ! introduces a denominator that vanishes exactly where the
                ! left side is finite. Rather than hand back a rewrite with
                ! new removable poles, the caller is told to go through
                ! sin/cos, where every rule here is unconditional.
                if (head == "tan" .and. is_compound_angle(parts(1))) then
                    ok = .false.
                    why = "trig_expand: no expansion rule for tan of a "// &
                          "compound angle; rewrite it as sin/cos first"
                    return
                end if
            end if
            r = func(head, parts)

        case default
            r = e
        end select
    end function expand_node

    !> True when an angle is a sum or carries an integer multiplier, i.e.
    !> when an expansion was expected and its absence is a real gap.
    function is_compound_angle(u) result(yes)
        type(expr_t), intent(in) :: u
        logical                  :: yes
        integer :: m
        type(expr_t) :: base
        logical :: split

        yes = .false.
        if (u%kind() == NK_ADD) then
            yes = .true.
            return
        end if
        call split_multiple(u, m, base, split)
        if (split) yes = abs(m) > 1
    end function is_compound_angle

    !> sin or cos of an arbitrary angle, expanded by repeated application of
    !> the addition formula.
    !>
    !> The whole family -- sin(a+b), cos(a+b), sin(2x), cos(n x) -- is one
    !> recurrence. Writing the angle as a sum of integer multiples of atoms
    !> and tracking the pair (cos U, sin U) as each atom is absorbed,
    !>
    !>     C' = C cos a - S sin a,   S' = S cos a + C sin a,
    !>
    !> gives the angle-sum formulae directly and, when the same atom is
    !> absorbed n times, exactly the Chebyshev expansion of cos(n a) and
    !> sin(n a). One recurrence rather than a table of special cases means
    !> there is no n for which the table happens to be wrong.
    function expand_trig(u, want_sin, ok, why) result(r)
        type(expr_t),              intent(in)    :: u
        logical,                   intent(in)    :: want_sin
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t), allocatable :: terms(:)
        type(expr_t) :: base, c_acc, s_acc, c_step, s_step
        integer, allocatable :: mult(:)
        integer :: k, j, n, total
        logical :: split, started

        r = u
        call angle_terms(u, terms, n)
        allocate (mult(n))
        total = 0
        do k = 1, n
            call split_multiple(terms(k), mult(k), base, split)
            if (.not. split) mult(k) = 1
            if (split) terms(k) = base
            total = total + abs(mult(k))
        end do

        if (total > MAX_MULTIPLICITY) then
            ok = .false.
            why = "trig_expand: angle multiplicity exceeds the expansion "// &
                  "bound; the expanded form would have 2**n terms"
            return
        end if

        started = .false.
        c_acc = num(u%a, 1)
        s_acc = num(u%a, 0)
        do k = 1, n
            if (mult(k) == 0) cycle
            ! A negative multiplier is absorbed as the reflected atom:
            ! cos(-a) = cos(a) and sin(-a) = -sin(a), so no separate rule is
            ! needed for a subtracted angle.
            c_step = cos(terms(k))
            if (mult(k) > 0) then
                s_step = sin(terms(k))
            else
                s_step = num(u%a, -1)*sin(terms(k))
            end if
            do j = 1, abs(mult(k))
                if (.not. started) then
                    c_acc = c_step
                    s_acc = s_step
                    started = .true.
                else
                    call step_pair(c_acc, s_acc, c_step, s_step)
                end if
            end do
        end do

        ! An angle with no atoms left is identically zero: every term was a
        ! zero multiple. sin(0) = 0 and cos(0) = 1 are the accumulator's
        ! initial values, so nothing special is needed here.
        if (want_sin) then
            r = s_acc
        else
            r = c_acc
        end if
    end function expand_trig

    !> One application of the addition formula to the running (cos, sin) pair.
    subroutine step_pair(c_acc, s_acc, c_step, s_step)
        type(expr_t), intent(inout) :: c_acc, s_acc
        type(expr_t), intent(in)    :: c_step, s_step
        type(expr_t) :: c_new, s_new

        c_new = c_acc*c_step - s_acc*s_step
        s_new = s_acc*c_step + c_acc*s_step
        c_acc = c_new
        s_acc = s_new
    end subroutine step_pair

    !> The summands of an angle, or the angle itself when it is not a sum.
    subroutine angle_terms(u, terms, n)
        type(expr_t),              intent(in)  :: u
        type(expr_t), allocatable, intent(out) :: terms(:)
        integer,                   intent(out) :: n
        integer :: k

        if (u%kind() == NK_ADD) then
            n = u%nargs()
            allocate (terms(n))
            do k = 1, n
                terms(k) = u%arg(k)
            end do
        else
            n = 1
            allocate (terms(1))
            terms(1) = u
        end if
    end subroutine angle_terms

    !> Split `n * base` out of a product. `split` is false when the term has
    !> no integer factor, or when it is nothing but integers -- `sin(3)` is
    !> not three copies of `sin(1)`.
    subroutine split_multiple(t, n, base, split)
        type(expr_t), intent(in)  :: t
        integer,      intent(out) :: n
        type(expr_t), intent(out) :: base
        logical,      intent(out) :: split
        type(expr_t), allocatable :: rest(:)
        integer :: k, m, nrest, acc
        logical :: good

        n = 1
        base = t
        split = .false.
        if (t%kind() /= NK_MUL) return

        allocate (rest(t%nargs()))
        nrest = 0
        acc = 1
        do k = 1, t%nargs()
            call integer_value(t%arg(k), m, good)
            if (good) then
                if (abs(real(acc, dp)*real(m, dp)) > real(INT_LIMIT, dp)) &
                    return
                acc = acc*m
            else
                nrest = nrest + 1
                rest(nrest) = t%arg(k)
            end if
        end do

        if (nrest == 0) return
        if (acc == 1) return
        n = acc
        base = combine(t%a, rest(1:nrest), .false.)
        split = .true.
    end subroutine split_multiple

    ! ------------------------------------------------------- trig_reduce --

    !> The opposite direction: products and integer powers of sines and
    !> cosines rewritten as sums of single trig terms of combined angles.
    subroutine trig_reduce(e, reduced, ok, why)
        type(expr_t),              intent(in)  :: e
        type(expr_t),              intent(out) :: reduced
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "trig_reduce: the expression is not valid"
            return
        end if

        ok = .true.
        reduced = reduce_node(e, ok, why)
        if (.not. ok) reduced = e
    end subroutine trig_reduce

    recursive function reduce_node(e, ok, why) result(r)
        type(expr_t),              intent(in)    :: e
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t), allocatable :: parts(:)
        character(:), allocatable :: head
        integer :: k, n

        r = e
        if (.not. ok) return

        select case (e%kind())

        case (NK_ADD)
            n = e%nargs()
            allocate (parts(n))
            do k = 1, n
                parts(k) = reduce_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            r = combine(e%a, parts, .true.)

        case (NK_MUL)
            r = reduce_product(e, ok, why)

        case (NK_POW)
            ! A power is only routed through the linearisation when it is a
            ! positive integer power of a trig function; anything else is
            ! rebuilt from reduced parts, which also keeps this case from
            ! handing itself back to reduce_product forever.
            if (is_trig_power(e)) then
                r = reduce_product(e, ok, why)
                return
            end if
            r = reduce_node(e%arg(1), ok, why)
            if (.not. ok) return
            r = r**reduce_node(e%arg(2), ok, why)

        case (NK_FUNC)
            head = chars(e%name())
            n = e%nargs()
            if (n == 0) then
                r = func_in(e%a, head)
                return
            end if
            ! Angles are left alone. Linearising inside an angle is sound but
            ! pointless, and it makes the combined angles unrecognisable.
            if (n == 1) then
                if (head == "sin" .or. head == "cos") return
            end if
            allocate (parts(n))
            do k = 1, n
                parts(k) = reduce_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            r = func(head, parts)

        case default
            r = e
        end select
    end function reduce_node

    !> Collect the trig factors of a product (or of a single power), linearise
    !> them, and multiply the result back by whatever else was there.
    recursive function reduce_product(e, ok, why) result(r)
        type(expr_t),              intent(in)    :: e
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t) :: trig(MAX_TRIG_FACTORS + 1)
        type(expr_t), allocatable :: other(:)
        type(expr_t), allocatable :: factors(:)
        type(expr_t) :: linear
        integer :: ntrig, nother, k, nf

        r = e
        call product_factors(e, factors, nf)
        allocate (other(nf))
        ntrig = 0
        nother = 0

        do k = 1, nf
            call take_trig(factors(k), trig, ntrig, other, nother, ok, why)
            if (.not. ok) return
        end do

        do k = 1, nother
            other(k) = reduce_node(other(k), ok, why)
            if (.not. ok) return
        end do

        ! Nothing to pair up: a lone trig factor is already linear.
        if (ntrig <= 1) then
            r = combine(e%a, other(1:nother), .false.)
            do k = 1, ntrig
                if (nother == 0) then
                    r = trig(k)
                else
                    r = r*trig(k)
                end if
            end do
            return
        end if

        linear = linearise(e%a, trig, ntrig, ok, why)
        if (.not. ok) return
        if (nother == 0) then
            r = linear
        else
            r = combine(e%a, other(1:nother), .false.)*linear
        end if
    end function reduce_product

    !> A positive integer power of a sine or cosine, the one power shape the
    !> linearisation knows how to unfold.
    function is_trig_power(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        integer :: n
        logical :: good

        yes = .false.
        if (e%kind() /= NK_POW) return
        if (.not. is_trig_call(e%arg(1))) return
        call integer_value(e%arg(2), n, good)
        if (good) yes = n >= 1
    end function is_trig_power

    !> Sort one factor into the trig list or the remainder, unfolding a small
    !> positive integer power of a trig function into repeated factors so that
    !> cos(x)**2 linearises like cos(x)*cos(x).
    subroutine take_trig(f, trig, ntrig, other, nother, ok, why)
        type(expr_t),              intent(in)    :: f
        type(expr_t),              intent(inout) :: trig(:)
        integer,                   intent(inout) :: ntrig
        type(expr_t), allocatable, intent(inout) :: other(:)
        integer,                   intent(inout) :: nother
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t), allocatable :: bigger(:)
        integer :: n, k
        logical :: good

        if (is_trig_call(f)) then
            if (ntrig >= MAX_TRIG_FACTORS) then
                ok = .false.
                why = "trig_reduce: more than the supported number of trig "// &
                      "factors in one product; the linearised form doubles "// &
                      "with each factor"
                return
            end if
            ntrig = ntrig + 1
            trig(ntrig) = f
            return
        end if

        if (f%kind() == NK_POW) then
            if (is_trig_call(f%arg(1))) then
                call integer_value(f%arg(2), n, good)
                if (good) then
                    if (n >= 1) then
                        if (ntrig + n > MAX_TRIG_FACTORS) then
                            ok = .false.
                            why = "trig_reduce: trig power too high to "// &
                                  "linearise within the supported bound"
                            return
                        end if
                        do k = 1, n
                            ntrig = ntrig + 1
                            trig(ntrig) = f%arg(1)
                        end do
                        return
                    end if
                end if
            end if
        end if

        if (nother >= size(other)) then
            allocate (bigger(2*size(other) + 1))
            bigger(1:nother) = other(1:nother)
            call move_alloc(bigger, other)
        end if
        nother = nother + 1
        other(nother) = f
    end subroutine take_trig

    !> Product-to-sum applied left to right.
    !>
    !> The running result is a list of terms, each a coefficient times a
    !> single sine or cosine of one angle. Absorbing another factor uses only
    !> the four textbook identities
    !>
    !>     sin A sin B = (cos(A-B) - cos(A+B))/2
    !>     cos A cos B = (cos(A-B) + cos(A+B))/2
    !>     sin A cos B = (sin(A+B) + sin(A-B))/2
    !>     cos A sin B = (sin(A+B) - sin(A-B))/2
    !>
    !> each of which turns one term into two, hence the term bound.
    function linearise(a, trig, ntrig, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: trig(:)
        integer,                   intent(in)    :: ntrig
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t) :: coeff(MAX_TERMS), ang(MAX_TERMS)
        type(expr_t) :: new_coeff(MAX_TERMS), new_ang(MAX_TERMS)
        type(expr_t) :: half, minus_half, fang, piece
        integer :: form(MAX_TERMS), new_form(MAX_TERMS)
        integer :: nterm, nnew, j, t, fkind

        r = num(a, 0)
        half = rat(a, 1_int64, 2_int64)
        minus_half = rat(a, -1_int64, 2_int64)

        nterm = 1
        coeff(1) = num(a, 1)
        form(1) = NOT_TRIG
        ang(1) = num(a, 0)

        do j = 1, ntrig
            fkind = trig_kind(trig(j))
            fang = trig(j)%arg(1)
            nnew = 0
            do t = 1, nterm
                if (nnew + 2 > MAX_TERMS) then
                    ok = .false.
                    why = "trig_reduce: linearised form exceeds the term "// &
                          "bound"
                    return
                end if

                if (form(t) == NOT_TRIG) then
                    nnew = nnew + 1
                    new_coeff(nnew) = coeff(t)
                    new_form(nnew) = fkind
                    new_ang(nnew) = fang
                    cycle
                end if

                if (form(t) == IS_SIN .and. fkind == IS_SIN) then
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*half, IS_COS, ang(t) - fang)
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*minus_half, IS_COS, ang(t) + fang)
                else if (form(t) == IS_COS .and. fkind == IS_COS) then
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*half, IS_COS, ang(t) - fang)
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*half, IS_COS, ang(t) + fang)
                else if (form(t) == IS_SIN .and. fkind == IS_COS) then
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*half, IS_SIN, ang(t) + fang)
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*half, IS_SIN, ang(t) - fang)
                else
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*half, IS_SIN, ang(t) + fang)
                    call emit(new_coeff, new_form, new_ang, nnew, &
                              coeff(t)*minus_half, IS_SIN, ang(t) - fang)
                end if
            end do

            nterm = nnew
            coeff(1:nterm) = new_coeff(1:nterm)
            form(1:nterm) = new_form(1:nterm)
            ang(1:nterm) = new_ang(1:nterm)
        end do

        do t = 1, nterm
            select case (form(t))
            case (IS_SIN)
                piece = coeff(t)*sin(ang(t))
            case (IS_COS)
                piece = coeff(t)*cos(ang(t))
            case default
                piece = coeff(t)
            end select
            if (t == 1) then
                r = piece
            else
                r = r + piece
            end if
        end do
    end function linearise

    subroutine emit(coeff, form, ang, n, c, k, u)
        type(expr_t), intent(inout) :: coeff(:), ang(:)
        integer,      intent(inout) :: form(:)
        integer,      intent(inout) :: n
        type(expr_t), intent(in)    :: c, u
        integer,      intent(in)    :: k

        n = n + 1
        coeff(n) = c
        form(n) = k
        ang(n) = u
    end subroutine emit

    function is_trig_call(f) result(yes)
        type(expr_t), intent(in) :: f
        logical                  :: yes
        yes = trig_kind(f) /= NOT_TRIG
    end function is_trig_call

    function trig_kind(f) result(k)
        type(expr_t), intent(in) :: f
        integer                  :: k
        character(:), allocatable :: head

        k = NOT_TRIG
        if (f%kind() /= NK_FUNC) return
        if (f%nargs() /= 1) return
        head = chars(f%name())
        if (head == "sin") k = IS_SIN
        if (head == "cos") k = IS_COS
    end function trig_kind

    ! -------------------------------------------------------- Euler pair --

    !> Every circular and hyperbolic function replaced by exponentials.
    subroutine trig_to_exp(e, rewritten, ok, why)
        type(expr_t),              intent(in)  :: e
        type(expr_t),              intent(out) :: rewritten
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "trig_to_exp: the expression is not valid"
            return
        end if

        ok = .true.
        rewritten = to_exp_node(e, ok, why)
        if (.not. ok) rewritten = e
    end subroutine trig_to_exp

    recursive function to_exp_node(e, ok, why) result(r)
        type(expr_t),              intent(in)    :: e
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t), allocatable :: parts(:)
        type(expr_t) :: u, ep, em, iu
        character(:), allocatable :: head
        integer :: k, n

        r = e
        if (.not. ok) return

        select case (e%kind())

        case (NK_ADD, NK_MUL)
            n = e%nargs()
            allocate (parts(n))
            do k = 1, n
                parts(k) = to_exp_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            r = combine(e%a, parts, e%kind() == NK_ADD)

        case (NK_POW)
            r = to_exp_node(e%arg(1), ok, why)
            if (.not. ok) return
            r = r**to_exp_node(e%arg(2), ok, why)

        case (NK_FUNC)
            head = chars(e%name())
            n = e%nargs()
            if (n == 0) then
                r = func_in(e%a, head)
                return
            end if
            allocate (parts(n))
            do k = 1, n
                parts(k) = to_exp_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            if (n /= 1) then
                r = func(head, parts)
                return
            end if

            u = parts(1)
            iu = i_expr(e%a)*u
            select case (head)
            case ("sin")
                r = (exp(iu) - exp(num(e%a, -1)*iu))/(num(e%a, 2)*i_expr(e%a))
            case ("cos")
                r = (exp(iu) + exp(num(e%a, -1)*iu))/num(e%a, 2)
            case ("tan")
                ep = (exp(iu) - exp(num(e%a, -1)*iu))/ &
                     (num(e%a, 2)*i_expr(e%a))
                em = (exp(iu) + exp(num(e%a, -1)*iu))/num(e%a, 2)
                r = ep/em
            case ("sinh")
                r = (exp(u) - exp(num(e%a, -1)*u))/num(e%a, 2)
            case ("cosh")
                r = (exp(u) + exp(num(e%a, -1)*u))/num(e%a, 2)
            case ("tanh")
                ep = exp(u) - exp(num(e%a, -1)*u)
                em = exp(u) + exp(num(e%a, -1)*u)
                r = ep/em
            case default
                r = func(head, parts)
            end select

        case default
            r = e
        end select
    end function to_exp_node

    !> The inverse direction: exponentials rewritten as trig functions.
    !>
    !> `exp(i*w)` becomes `cos(w) + i sin(w)` and any other exponent `z`
    !> becomes `cosh(z) + sinh(z)`. Both are identities for every complex
    !> argument, so the rewrite never needs to know whether the exponent is
    !> really imaginary -- it only picks the form that reads better.
    subroutine exp_to_trig(e, rewritten, ok, why)
        type(expr_t),              intent(in)  :: e
        type(expr_t),              intent(out) :: rewritten
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "exp_to_trig: the expression is not valid"
            return
        end if

        ok = .true.
        rewritten = to_trig_node(e, ok, why)
        if (.not. ok) rewritten = e
    end subroutine exp_to_trig

    recursive function to_trig_node(e, ok, why) result(r)
        type(expr_t),              intent(in)    :: e
        logical,                   intent(inout) :: ok
        character(:), allocatable, intent(inout) :: why
        type(expr_t) :: r
        type(expr_t), allocatable :: parts(:)
        type(expr_t), allocatable :: pieces(:)
        character(:), allocatable :: head
        integer :: k, n

        r = e
        if (.not. ok) return

        select case (e%kind())

        case (NK_ADD, NK_MUL)
            n = e%nargs()
            allocate (parts(n))
            do k = 1, n
                parts(k) = to_trig_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            r = combine(e%a, parts, e%kind() == NK_ADD)

        case (NK_POW)
            r = to_trig_node(e%arg(1), ok, why)
            if (.not. ok) return
            r = r**to_trig_node(e%arg(2), ok, why)

        case (NK_FUNC)
            head = chars(e%name())
            n = e%nargs()
            if (n == 0) then
                r = func_in(e%a, head)
                return
            end if
            allocate (parts(n))
            do k = 1, n
                parts(k) = to_trig_node(e%arg(k), ok, why)
                if (.not. ok) return
            end do
            if (head == "exp" .and. n == 1) then
                ! exp(a+b) = exp(a)*exp(b) holds for all complex a, b, so a
                ! sum in the exponent is split first and each piece gets the
                ! form that suits it.
                if (parts(1)%kind() == NK_ADD) then
                    allocate (pieces(parts(1)%nargs()))
                    do k = 1, parts(1)%nargs()
                        pieces(k) = euler_of(parts(1)%arg(k))
                    end do
                    r = combine(e%a, pieces, .false.)
                else
                    r = euler_of(parts(1))
                end if
                return
            end if
            r = func(head, parts)

        case default
            r = e
        end select
    end function to_trig_node

    !> exp(z) written with trig functions.
    function euler_of(z) result(r)
        type(expr_t), intent(in) :: z
        type(expr_t)             :: r
        type(expr_t) :: w
        logical :: imaginary

        call strip_imaginary_unit(z, w, imaginary)
        if (imaginary) then
            r = cos(w) + i_expr(z%a)*sin(w)
        else
            r = cosh(z) + sinh(z)
        end if
    end function euler_of

    !> Peel a single factor of the imaginary unit off a term. `imaginary` is
    !> false when there is none, or when there is more than one -- i*i*x is a
    !> real multiple in disguise and the cosh/sinh form is the honest one.
    subroutine strip_imaginary_unit(z, w, imaginary)
        type(expr_t), intent(in)  :: z
        type(expr_t), intent(out) :: w
        logical,      intent(out) :: imaginary
        type(expr_t), allocatable :: rest(:)
        type(expr_t) :: unit
        integer :: k, nrest, count_i

        w = z
        imaginary = .false.
        unit = i_expr(z%a)

        if (z == unit) then
            w = num(z%a, 1)
            imaginary = .true.
            return
        end if

        if (z%kind() /= NK_MUL) return

        allocate (rest(z%nargs()))
        nrest = 0
        count_i = 0
        do k = 1, z%nargs()
            if (z%arg(k) == unit) then
                count_i = count_i + 1
            else
                nrest = nrest + 1
                rest(nrest) = z%arg(k)
            end if
        end do

        if (count_i /= 1) return
        imaginary = .true.
        if (nrest == 0) then
            w = num(z%a, 1)
        else
            w = combine(z%a, rest(1:nrest), .false.)
        end if
    end subroutine strip_imaginary_unit

    ! ------------------------------------------------------ power_expand --

    !> `(a*b)^n -> a^n b^n` and `(a^m)^n -> a^(m n)`, applied only where they
    !> are true.
    !>
    !> The exponent must be an integer, or the base factors must be known
    !> positive from `context`. Anything else is refused, naming the exponent
    !> that stopped it. This is the whole point of the routine: `sqrt(x*y)`
    !> is not `sqrt(x) sqrt(y)`, and a system that says otherwise is wrong at
    !> x = y = -1 without any warning to the caller.
    subroutine power_expand(e, expanded, ok, why, context)
        type(expr_t),               intent(in)  :: e
        type(expr_t),               intent(out) :: expanded
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(assumption_context_t), intent(in), optional :: context
        type(assumption_context_t) :: local

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "power_expand: the expression is not valid"
            return
        end if

        ! An absent context is an empty one: no expression is known positive,
        ! so only the integer-exponent rules apply.
        if (present(context)) local = context

        ok = .true.
        expanded = power_node(e, local, ok, why)
        if (.not. ok) expanded = e
    end subroutine power_expand

    recursive function power_node(e, context, ok, why) result(r)
        type(expr_t),               intent(in)    :: e
        type(assumption_context_t), intent(in)    :: context
        logical,                    intent(inout) :: ok
        character(:), allocatable,  intent(inout) :: why
        type(expr_t) :: r
        type(expr_t), allocatable :: parts(:)
        type(expr_t), allocatable :: pieces(:)
        type(expr_t), allocatable :: factors(:)
        type(expr_t) :: b, p
        character(:), allocatable :: head
        integer :: k, n, nf, expo
        logical :: int_expo

        r = e
        if (.not. ok) return

        select case (e%kind())

        case (NK_ADD, NK_MUL)
            n = e%nargs()
            allocate (parts(n))
            do k = 1, n
                parts(k) = power_node(e%arg(k), context, ok, why)
                if (.not. ok) return
            end do
            r = combine(e%a, parts, e%kind() == NK_ADD)

        case (NK_POW)
            b = power_node(e%arg(1), context, ok, why)
            if (.not. ok) return
            p = power_node(e%arg(2), context, ok, why)
            if (.not. ok) return
            call integer_value(p, expo, int_expo)

            if (b%kind() == NK_MUL) then
                call product_factors(b, factors, nf)
                if (.not. int_expo) then
                    if (.not. all_positive(factors, nf, context)) then
                        ok = .false.
                        why = "power_expand: (a*b)**p splits only for an "// &
                              "integer p or positive factors; p here is "// &
                              "neither, and the split is false for "// &
                              "negative or complex factors"
                        return
                    end if
                end if
                allocate (pieces(nf))
                do k = 1, nf
                    pieces(k) = factors(k)**p
                end do
                r = combine(e%a, pieces, .false.)
                return
            end if

            if (b%kind() == NK_POW) then
                if (.not. int_expo) then
                    if (.not. is_positive(b%arg(1), context)) then
                        ok = .false.
                        why = "power_expand: (a**m)**p collapses only for "// &
                              "an integer p or a positive a; ((-2)**2)"// &
                              "**(1/2) is 2 while (-2)**(2*(1/2)) is -2"
                        return
                    end if
                    if (.not. is_real_expr(b%arg(2), context)) then
                        ok = .false.
                        why = "power_expand: (a**m)**p collapses only for "// &
                              "an integer p or a positive a and a real m; "// &
                              "m is not known real here, and (2**(5*i))"// &
                              "**(1/2) is not 2**(5*i*(1/2))"
                        return
                    end if
                end if
                r = b%arg(1)**(b%arg(2)*p)
                return
            end if

            r = b**p

        case (NK_FUNC)
            head = chars(e%name())
            n = e%nargs()
            if (n == 0) then
                r = func_in(e%a, head)
                return
            end if
            allocate (parts(n))
            do k = 1, n
                parts(k) = power_node(e%arg(k), context, ok, why)
                if (.not. ok) return
            end do

            ! sqrt is the exponent 1/2 wearing a name, and it is where the
            ! unsound rewrite is most often reached for.
            if (head == "sqrt" .and. n == 1) then
                if (parts(1)%kind() == NK_MUL) then
                    call product_factors(parts(1), factors, nf)
                    if (.not. all_positive(factors, nf, context)) then
                        ok = .false.
                        why = "power_expand: sqrt(a*b) is not "// &
                              "sqrt(a)*sqrt(b) unless the factors are "// &
                              "positive; at a = b = -1 the left side is 1 "// &
                              "and the right side is -1"
                        return
                    end if
                    allocate (pieces(nf))
                    do k = 1, nf
                        pieces(k) = sqrt(factors(k))
                    end do
                    r = combine(e%a, pieces, .false.)
                    return
                end if
                if (parts(1)%kind() == NK_POW) then
                    if (.not. is_positive(parts(1)%arg(1), context)) then
                        ok = .false.
                        why = "power_expand: sqrt(a**m) is not a**(m/2) "// &
                              "unless a is positive; sqrt(a**2) is abs(a)"
                        return
                    end if
                    if (.not. is_real_expr(parts(1)%arg(2), context)) then
                        ok = .false.
                        why = "power_expand: sqrt(a**m) is not a**(m/2) "// &
                              "unless m is real as well; m is not known "// &
                              "real here, and sqrt(2**(5*i)) is not "// &
                              "2**(5*i/2)"
                        return
                    end if
                    r = parts(1)%arg(1)**(parts(1)%arg(2)* &
                                          rat(e%a, 1_int64, 2_int64))
                    return
                end if
            end if
            r = func(head, parts)

        case default
            r = e
        end select
    end function power_node

    function all_positive(factors, nf, context) result(yes)
        type(expr_t),               intent(in) :: factors(:)
        integer,                    intent(in) :: nf
        type(assumption_context_t), intent(in) :: context
        logical                                :: yes
        integer :: k

        yes = .true.
        do k = 1, nf
            if (.not. is_positive(factors(k), context)) then
                yes = .false.
                return
            end if
        end do
    end function all_positive

    !> Positivity, from an explicit assumption or from the expression being
    !> visibly a positive literal or a product/power of positives. Unknown
    !> means not positive: this predicate is only ever used to license a
    !> rewrite, so its safe answer is "no".
    recursive function is_positive(e, context) result(yes)
        type(expr_t),               intent(in) :: e
        type(assumption_context_t), intent(in) :: context
        logical                                :: yes
        character(:), allocatable :: head
        integer :: k

        yes = .false.
        if (context%has(e, FACT_POSITIVE)) then
            yes = .true.
            return
        end if

        select case (e%kind())
        case (NK_INT)
            yes = e%int_value() > 0_int64
        case (NK_RAT)
            ! Denominators are normalised positive by the arena, so the sign
            ! lives entirely in the numerator.
            yes = e%int_value() > 0_int64 .and. e%den_value() > 0_int64
        case (NK_REAL)
            yes = e%real_value() > 0.0_dp
        case (NK_CONST)
            head = chars(e%name())
            yes = head == "pi" .or. head == "e"
        case (NK_MUL)
            yes = .true.
            do k = 1, e%nargs()
                if (.not. is_positive(e%arg(k), context)) then
                    yes = .false.
                    return
                end if
            end do
        case (NK_POW)
            ! A positive base raised to a *real* exponent is positive. The
            ! exponent has to be established real: 2**(5*i) has modulus one
            ! but is not positive, and treating it as positive licenses a
            ! sign error in the splitting rules below.
            if (is_positive(e%arg(1), context)) then
                yes = is_real_expr(e%arg(2), context)
            end if
        case (NK_FUNC)
            head = chars(e%name())
            if (head == "exp") then
                if (e%nargs() == 1) yes = is_real_expr(e%arg(1), context)
            end if
        end select
    end function is_positive

    !> Realness, by the same conservative contract as is_positive: an
    !> explicit assumption, a numeric literal, or a shape built only from
    !> those. Unknown means "not known real". Symbols are not assumed real:
    !> the module manufactures exp(i*...) nodes itself, so a default-real
    !> reading of an unconstrained atom would readmit the sign errors this
    !> predicate exists to exclude.
    recursive function is_real_expr(e, context) result(yes)
        type(expr_t),               intent(in) :: e
        type(assumption_context_t), intent(in) :: context
        logical                                :: yes
        character(:), allocatable :: head
        integer :: k, expo
        logical :: int_expo

        yes = .false.
        ! Positivity implies realness in the assumption context, so this one
        ! query covers both.
        if (context%has(e, FACT_REAL)) then
            yes = .true.
            return
        end if

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL)
            yes = .true.
        case (NK_CONST)
            head = chars(e%name())
            yes = head == "pi" .or. head == "e"
        case (NK_ADD, NK_MUL)
            yes = .true.
            do k = 1, e%nargs()
                if (.not. is_real_expr(e%arg(k), context)) then
                    yes = .false.
                    return
                end if
            end do
        case (NK_POW)
            ! A real base with an integer exponent stays real; a positive
            ! base with a real exponent does too. Anything else -- notably a
            ! negative base with a fractional exponent -- is left unknown.
            call integer_value(e%arg(2), expo, int_expo)
            if (int_expo) then
                yes = is_real_expr(e%arg(1), context)
                return
            end if
            if (is_positive(e%arg(1), context)) then
                yes = is_real_expr(e%arg(2), context)
            end if
        case (NK_FUNC)
            head = chars(e%name())
            if (head == "exp" .or. head == "sin" .or. head == "cos") then
                if (e%nargs() == 1) yes = is_real_expr(e%arg(1), context)
            end if
        end select
    end function is_real_expr

    ! ---------------------------------------------------------- utilities --

    !> The factors of a product, or the expression itself when it is not one.
    subroutine product_factors(e, factors, nf)
        type(expr_t),              intent(in)  :: e
        type(expr_t), allocatable, intent(out) :: factors(:)
        integer,                   intent(out) :: nf
        integer :: k

        if (e%kind() == NK_MUL) then
            nf = e%nargs()
            allocate (factors(nf))
            do k = 1, nf
                factors(k) = e%arg(k)
            end do
        else
            nf = 1
            allocate (factors(1))
            factors(1) = e
        end if
    end subroutine product_factors

    !> Rebuild a sum or a product from its parts. An empty list is the
    !> identity of the operation, which is what makes the callers above able
    !> to drop factors without a special case.
    function combine(a, parts, as_sum) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: parts(:)
        logical,               intent(in)    :: as_sum
        type(expr_t)                         :: r
        integer :: k

        if (size(parts) == 0) then
            if (as_sum) then
                r = num(a, 0)
            else
                r = num(a, 1)
            end if
            return
        end if

        r = parts(1)
        do k = 2, size(parts)
            if (as_sum) then
                r = r + parts(k)
            else
                r = r*parts(k)
            end if
        end do
    end function combine

    !> The integer an expression denotes, when it plainly denotes one.
    !>
    !> Sums and products of integers are folded because an exponent often
    !> arrives that way, but nothing is evaluated: a real literal that happens
    !> to be whole is not accepted, since 2.0 as an exponent carries no
    !> promise that the caller meant the integer power. Magnitudes past
    !> INT_LIMIT report failure rather than overflowing into a small integer.
    recursive subroutine integer_value(e, n, ok)
        type(expr_t), intent(in)  :: e
        integer,      intent(out) :: n
        logical,      intent(out) :: ok
        integer(int64) :: v
        integer :: k, m
        logical :: good

        n = 0
        ok = .false.

        select case (e%kind())

        case (NK_INT)
            v = e%int_value()
            if (abs(v) > int(INT_LIMIT, int64)) return
            n = int(v)
            ok = .true.

        case (NK_RAT)
            if (e%den_value() /= 1_int64) return
            v = e%int_value()
            if (abs(v) > int(INT_LIMIT, int64)) return
            n = int(v)
            ok = .true.

        case (NK_MUL)
            n = 1
            do k = 1, e%nargs()
                call integer_value(e%arg(k), m, good)
                if (.not. good) then
                    n = 0
                    return
                end if
                if (abs(real(n, dp)*real(m, dp)) > real(INT_LIMIT, dp)) then
                    n = 0
                    return
                end if
                n = n*m
            end do
            ok = .true.

        case (NK_ADD)
            n = 0
            do k = 1, e%nargs()
                call integer_value(e%arg(k), m, good)
                if (.not. good) then
                    n = 0
                    return
                end if
                if (abs(real(n, dp) + real(m, dp)) > real(INT_LIMIT, dp)) then
                    n = 0
                    return
                end if
                n = n + m
            end do
            ok = .true.

        end select
    end subroutine integer_value

end module fortsym_trigrewrite
