module fortsym_sums
    ! Closed forms for indexed sums and products.
    !
    ! Everything here rests on one identity. If S is an antidifference of the
    ! body f, meaning S(k+1) - S(k) = f(k) for every k, then
    !
    !     sum_{k=lower}^{upper} f(k) = S(upper + 1) - S(lower)
    !
    ! by telescoping. Writing every case that way -- polynomials, geometric
    ! terms, and a body that is literally a difference -- is what keeps the
    ! bounds honest: the empty range upper = lower - 1 collapses to zero on its
    ! own rather than through a special case someone has to remember, and there
    ! is no place left for the off-by-one that dominates this subject.
    !
    ! The closed form is asserted for upper >= lower - 1. Below that the usual
    ! convention makes the sum zero while the formula does not, and with
    ! symbolic bounds there is no way to tell which side we are on. Callers who
    ! substitute a smaller upper bound are outside the stated range; concrete
    ! bounds are checked and answered directly, so the gap only exists where
    ! nothing could have been decided anyway.
    !
    ! Refused rather than answered:
    !
    !   * a body outside the implemented classes. Guessing an antidifference is
    !     the one failure mode that produces a formula which looks right and
    !     disagrees with the sum; a refusal costs the caller a message, a wrong
    !     closed form costs them the result they built on it.
    !   * a geometric sum whose ratio cannot be decided against 1. r = 1 is a
    !     removable singularity of (r^(u+1) - r^l)/(r - 1), so the closed form
    !     is 0/0 exactly there and correct everywhere else. With a numeric
    !     ratio the branch is decided here. With a symbolic ratio it cannot be,
    !     and the caller must assert r /= 1 through assume_ratio_ne_one before
    !     the formula is issued.
    !   * an infinite bound. No convergence test is implemented, so the only
    !     defensible answer is that this operation does not do limits.
    !   * an expansion larger than MAX_EXPAND_TERMS whose body has no closed
    !     form. Producing a million-node expression, or looping for minutes, is
    !     not a better outcome than saying the range is too large.
    !
    ! Gosper's algorithm is not implemented. Hypergeometric terms outside the
    ! classes above are refused and say so, rather than being answered by a
    ! partial imitation of it.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
                             NK_CONST, NK_ADD, NK_MUL, NK_POW
    use fortsym_string, only: chars
    use fortsym_expr, only: expr_t, num, rat, is_valid, same_arena, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**), operator(==), &
                            operator(/=)
    use fortsym_subs, only: subs
    implicit none
    private

    public :: sum_closed_form, product_closed_form
    public :: MAX_EXPAND_TERMS, MAX_POLY_DEGREE

    integer, parameter :: dp = real64

    !> Largest number of terms written out by direct expansion. Chosen so that
    !> the worst case is a few thousand interned nodes rather than a hang.
    integer(int64), parameter :: MAX_EXPAND_TERMS = 1024_int64

    !> Largest exponent p for which sum k^p is issued. The Faulhaber
    !> coefficients are exact int64 rationals; past this degree the Bernoulli
    !> recurrence can overflow, and an overflowed coefficient is a wrong
    !> formula, so the degree is capped where the arithmetic is still provable.
    integer, parameter :: MAX_POLY_DEGREE = 16

    ! Term shapes recognised inside a body, after var-free factors are pulled
    ! out as a coefficient.
    integer, parameter :: TERM_CONST = 0     ! coefficient only
    integer, parameter :: TERM_POWER = 1     ! coefficient * var**p, p >= 1
    integer, parameter :: TERM_GEOM = 2      ! coefficient * r**var

contains

    ! ------------------------------------------------------------- sums --

    !> The closed form of sum_{var=lower}^{upper} body, or a refusal saying
    !> what stopped it.
    !>
    !> assume_ratio_ne_one is the caller taking responsibility for the r /= 1
    !> branch of a geometric sum with a symbolic ratio. Absent or false, that
    !> case is refused.
    function sum_closed_form(a, body, var, lower, upper, ok, why, &
                             assume_ratio_ne_one) result(s)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: body, var, lower, upper
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        logical, optional,         intent(in)    :: assume_ratio_ne_one
        type(expr_t) :: s
        integer(int64) :: lo, up, count
        logical :: concrete, assume_ne_one

        s = num(a, 0)
        assume_ne_one = .false.
        if (present(assume_ratio_ne_one)) assume_ne_one = assume_ratio_ne_one

        call check_inputs(a, body, var, lower, upper, ok, why)
        if (.not. ok) return
        ok = .false.

        call concrete_bounds(lower, upper, lo, up, concrete)
        if (concrete) then
            if (up < lo) then
                ! The empty range. Answered before anything else so that no
                ! closed form is ever consulted about a range that has no
                ! terms.
                s = num(a, 0)
                ok = .true.
                return
            end if
            count = up - lo + 1_int64
            if (count <= MAX_EXPAND_TERMS) then
                s = expand_sum(a, body, var, lo, up)
                ok = .true.
                return
            end if
        end if

        s = closed_sum(a, body, var, lower, upper, assume_ne_one, ok, why)
        if (ok) return
        if (concrete) then
            why = why//"; direct expansion of "//itoa(count)// &
                  " terms exceeds the cap of "//itoa(MAX_EXPAND_TERMS)
        end if
    end function sum_closed_form

    !> Sum written out term by term. Exact by construction: it is the
    !> definition, not a formula about it.
    function expand_sum(a, body, var, lo, up) result(s)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: body, var
        integer(int64),        intent(in)    :: lo, up
        type(expr_t) :: s
        integer(int64) :: k

        s = subs(body, var, num(a, lo))
        do k = lo + 1_int64, up
            s = s + subs(body, var, num(a, k))
        end do
    end function expand_sum

    !> Linearity, then one antidifference per term, then telescoping on the
    !> whole body if the term walk found something it does not know.
    function closed_sum(a, body, var, lower, upper, assume_ne_one, ok, why) &
        result(s)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: body, var, lower, upper
        logical,                   intent(in)    :: assume_ne_one
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: s, term, part, hi
        integer :: nterms, k
        logical :: started

        s = num(a, 0)
        hi = upper + 1
        nterms = term_count(body)
        started = .false.

        do k = 1, nterms
            term = term_at(body, k)
            part = term_antidifference(a, term, var, lower, hi, &
                                       assume_ne_one, ok, why)
            if (.not. ok) then
                s = telescoping_sum(a, body, var, lower, hi, ok)
                if (ok) then
                    why = ""
                else
                    why = "no closed form: "//why// &
                          " (and the body is not a first difference)"
                end if
                return
            end if
            if (started) then
                s = s + part
            else
                s = part
                started = .true.
            end if
        end do

        ok = .true.
    end function closed_sum

    !> S(hi) - S(lower) for one term, where S is that term's antidifference.
    function term_antidifference(a, term, var, lower, hi, assume_ne_one, &
                                 ok, why) result(part)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: term, var, lower, hi
        logical,                   intent(in)    :: assume_ne_one
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: part, coeff, ratio, core, lo_val, hi_val
        integer :: tclass, degree
        logical :: has_coeff, decided, is_one

        part = num(a, 0)
        call analyse_term(a, term, var, tclass, degree, ratio, coeff, &
                          has_coeff, ok, why)
        if (.not. ok) return
        ok = .false.

        select case (tclass)
        case (TERM_CONST)
            ! S(m) = m, so the sum is the number of terms.
            core = hi - lower
        case (TERM_POWER)
            ! S(m) = sum_{k=0}^{m-1} k^p, the Faulhaber polynomial.
            hi_val = faulhaber_at(a, degree, hi, ok)
            if (.not. ok) then
                why = "Faulhaber coefficients for degree "//itoa(int(degree, int64))// &
                      " overflow exact int64 rationals"
                return
            end if
            lo_val = faulhaber_at(a, degree, lower, ok)
            if (.not. ok) then
                why = "Faulhaber coefficients for degree "//itoa(int(degree, int64))// &
                      " overflow exact int64 rationals"
                return
            end if
            core = hi_val - lo_val
        case (TERM_GEOM)
            call decide_unit_ratio(ratio, decided, is_one)
            if (decided) then
                if (is_one) then
                    ! 1**k is the constant 1: the closed form (r^(u+1) -
                    ! r^l)/(r - 1) is 0/0 here, and this is the branch it
                    ! stands for.
                    core = hi - lower
                else if (numeric_is_zero(ratio)) then
                    why = "geometric ratio is zero: 0**0 makes the term at "// &
                          "k = 0 ambiguous"
                    return
                else
                    core = (ratio**hi - ratio**lower)/(ratio - 1)
                end if
            else if (assume_ne_one) then
                core = (ratio**hi - ratio**lower)/(ratio - 1)
            else
                why = "geometric ratio is symbolic and the r = 1 branch "// &
                      "cannot be decided; pass assume_ratio_ne_one to "// &
                      "assert r /= 1"
                return
            end if
        case default
            why = "unrecognised term shape"
            return
        end select

        if (has_coeff) then
            part = coeff*core
        else
            part = core
        end if
        ok = .true.
    end function term_antidifference

    ! ---------------------------------------------------------- products --

    !> The closed form of product_{var=lower}^{upper} body, or a refusal.
    !>
    !> An empty range is 1, the empty product, for the same reason an empty sum
    !> is 0.
    function product_closed_form(a, body, var, lower, upper, ok, why) &
        result(p)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: body, var, lower, upper
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: p
        integer(int64) :: lo, up, count
        logical :: concrete

        p = num(a, 1)
        call check_inputs(a, body, var, lower, upper, ok, why)
        if (.not. ok) return
        ok = .false.

        call concrete_bounds(lower, upper, lo, up, concrete)
        if (concrete) then
            if (up < lo) then
                p = num(a, 1)
                ok = .true.
                return
            end if
            count = up - lo + 1_int64
            if (count <= MAX_EXPAND_TERMS) then
                p = expand_product(a, body, var, lo, up)
                ok = .true.
                return
            end if
        end if

        p = closed_product(a, body, var, lower, upper, ok, why)
        if (ok) return
        if (concrete) then
            why = why//"; direct expansion of "//itoa(count)// &
                  " factors exceeds the cap of "//itoa(MAX_EXPAND_TERMS)
        end if
    end function product_closed_form

    function expand_product(a, body, var, lo, up) result(p)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: body, var
        integer(int64),        intent(in)    :: lo, up
        type(expr_t) :: p
        integer(int64) :: k

        p = subs(body, var, num(a, lo))
        do k = lo + 1_int64, up
            p = p*subs(body, var, num(a, k))
        end do
    end function expand_product

    !> Two classes only: c * r**var, where the exponents add up under the
    !> product, and a body that is a ratio f(k+1)/f(k), which telescopes.
    !>
    !> product k^p is deliberately absent. Its closed form needs the gamma
    !> function and a positivity argument about the lower bound that nothing
    !> here can supply, so it is refused instead of guessed.
    function closed_product(a, body, var, lower, upper, ok, why) result(p)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: body, var, lower, upper
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: p, coeff, ratio, count, expo
        integer :: tclass, degree
        logical :: has_coeff

        p = num(a, 1)
        count = upper - lower + 1

        if (term_count(body) == 1) then
            call analyse_term(a, body, var, tclass, degree, ratio, coeff, &
                              has_coeff, ok, why)
        else
            ok = .false.
            why = "body is a sum, which no implemented product form covers"
        end if

        if (ok) then
            ok = .false.
            select case (tclass)
            case (TERM_CONST)
                p = coeff**count
                ok = .true.
            case (TERM_GEOM)
                ! The exponents form an arithmetic series:
                ! sum_{k=l}^{u} k = (u(u+1) - l(l-1))/2.
                expo = rat(a, 1_int64, 2_int64)* &
                       (upper*(upper + 1) - lower*(lower - 1))
                if (has_coeff) then
                    p = coeff**count*ratio**expo
                else
                    p = ratio**expo
                end if
                ok = .true.
            case (TERM_POWER)
                why = "product of var**"//itoa(int(degree, int64))// &
                      " needs gamma and a positivity argument for the "// &
                      "lower bound: refused"
            case default
                why = "unrecognised term shape"
            end select
            if (ok) return
        end if

        p = telescoping_product(a, body, var, lower, upper + 1, ok)
        if (ok) then
            why = ""
        else
            why = "no closed form: "//why// &
                  " (and the body is not a ratio f(k+1)/f(k))"
        end if
    end function closed_product

    ! ------------------------------------------------------ telescoping --

    !> If body = f(var+1) - f(var) then the sum is f(hi) - f(lower), with
    !> hi = upper + 1. The match is structural on hash-consed nodes, so it is
    !> exact: a false positive is impossible, and a body written in a shape
    !> this does not recognise is missed and refused rather than mis-summed.
    function telescoping_sum(a, body, var, lower, hi, ok) result(s)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: body, var, lower, hi
        logical,               intent(out)   :: ok
        type(expr_t) :: s, f, shifted
        integer :: k, other

        s = num(a, 0)
        ok = .false.
        if (body%kind() /= NK_ADD) return
        if (body%nargs() /= 2) return

        do k = 1, 2
            other = 3 - k
            f = strip_negation(body%arg(k), ok)
            if (.not. ok) cycle
            shifted = subs(f, var, var + 1)
            if (shifted == body%arg(other)) then
                s = subs(f, var, hi) - subs(f, var, lower)
                ok = .true.
                return
            end if
        end do
        ok = .false.
    end function telescoping_sum

    !> If body = f(var+1)/f(var) then the product is f(hi)/f(lower).
    function telescoping_product(a, body, var, lower, hi, ok) result(p)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: body, var, lower, hi
        logical,               intent(out)   :: ok
        type(expr_t) :: p, f, shifted
        integer :: k, other

        p = num(a, 1)
        ok = .false.
        if (body%kind() /= NK_MUL) return
        if (body%nargs() /= 2) return

        do k = 1, 2
            other = 3 - k
            f = strip_reciprocal(body%arg(k), ok)
            if (.not. ok) cycle
            shifted = subs(f, var, var + 1)
            if (shifted == body%arg(other)) then
                p = subs(f, var, hi)/subs(f, var, lower)
                ok = .true.
                return
            end if
        end do
        ok = .false.
    end function telescoping_product

    !> Peel a reciprocal, giving g from g**(-1). Anything that is not literally
    !> a power with exponent -1 is declined.
    function strip_reciprocal(e, ok) result(g)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: ok
        type(expr_t) :: g, expo

        g = e
        ok = .false.
        if (e%kind() /= NK_POW) return
        expo = e%arg(2)
        if (expo%kind() /= NK_INT) return
        if (expo%int_value() /= -1_int64) return
        g = e%arg(1)
        ok = .true.
    end function strip_reciprocal

    ! -------------------------------------------------- term inspection --

    !> Number of additive terms in a body.
    function term_count(e) result(n)
        type(expr_t), intent(in) :: e
        integer :: n
        n = 1
        if (e%kind() == NK_ADD) n = e%nargs()
    end function term_count

    function term_at(e, k) result(t)
        type(expr_t), intent(in) :: e
        integer,      intent(in) :: k
        type(expr_t) :: t
        t = e
        if (e%kind() == NK_ADD) t = e%arg(k)
    end function term_at

    !> Split a term into var-free factors (the coefficient) and at most one
    !> var-dependent factor, and classify that factor.
    subroutine analyse_term(a, term, var, tclass, degree, ratio, coeff, &
                            has_coeff, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: term, var
        integer,                   intent(out)   :: tclass, degree
        type(expr_t),              intent(out)   :: ratio, coeff
        logical,                   intent(out)   :: has_coeff, ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: f, vfactor, base, expo
        integer :: nf, k
        integer(int64) :: p
        logical :: seen

        tclass = TERM_CONST
        degree = 0
        ratio = num(a, 0)
        coeff = num(a, 1)
        has_coeff = .false.
        seen = .false.
        ok = .false.
        why = ""

        nf = 1
        if (term%kind() == NK_MUL) nf = term%nargs()

        do k = 1, nf
            if (term%kind() == NK_MUL) then
                f = term%arg(k)
            else
                f = term
            end if
            if (depends_on(f, var)) then
                if (seen) then
                    why = "term has two factors depending on the summation "// &
                          "variable, which no implemented class covers"
                    return
                end if
                seen = .true.
                vfactor = f
            else if (has_coeff) then
                coeff = coeff*f
            else
                coeff = f
                has_coeff = .true.
            end if
        end do

        if (.not. seen) then
            tclass = TERM_CONST
            ok = .true.
            return
        end if

        if (vfactor == var) then
            tclass = TERM_POWER
            degree = 1
            ok = .true.
            return
        end if

        if (vfactor%kind() /= NK_POW) then
            why = "body factor is neither var**p nor r**var"
            return
        end if

        base = vfactor%arg(1)
        expo = vfactor%arg(2)

        if (base == var) then
            ! var**p. Only a non-negative integer exponent is a polynomial;
            ! var**(-1) is a harmonic number and has no elementary closed form.
            if (expo%kind() /= NK_INT) then
                why = "power of the summation variable has a non-integer "// &
                      "exponent"
                return
            end if
            p = expo%int_value()
            if (p < 0_int64) then
                why = "negative power of the summation variable gives a "// &
                      "harmonic-type sum, which has no elementary closed form"
                return
            end if
            if (p > int(MAX_POLY_DEGREE, int64)) then
                why = "polynomial degree "//itoa(p)//" exceeds the cap of "// &
                      itoa(int(MAX_POLY_DEGREE, int64))
                return
            end if
            if (p == 0_int64) then
                tclass = TERM_CONST
            else
                tclass = TERM_POWER
                degree = int(p)
            end if
            ok = .true.
            return
        end if

        if (expo == var) then
            ! r**var, with r independent of var: the depends_on test above
            ! already established that the whole factor depends on var, and the
            ! exponent is var itself, so a var-free base is all that is left to
            ! check.
            if (depends_on(base, var)) then
                why = "geometric base still depends on the summation variable"
                return
            end if
            tclass = TERM_GEOM
            ratio = base
            ok = .true.
            return
        end if

        why = "power with the summation variable buried in base and exponent"
    end subroutine analyse_term

    !> Does this expression contain the summation variable?
    recursive function depends_on(e, var) result(yes)
        type(expr_t), intent(in) :: e, var
        logical :: yes
        integer :: k

        yes = .true.
        if (e == var) return
        do k = 1, e%nargs()
            if (depends_on(e%arg(k), var)) return
        end do
        yes = .false.
    end function depends_on

    !> Peel a -1 out of a product, giving g from -g. Returns ok = false for
    !> anything that does not literally have -1 as one of its factors.
    function strip_negation(e, ok) result(g)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: ok
        type(expr_t) :: g, factor
        integer :: k, neg_at, nf
        logical :: started

        g = e
        ok = .false.
        if (e%kind() /= NK_MUL) return
        nf = e%nargs()

        neg_at = 0
        do k = 1, nf
            factor = e%arg(k)
            if (factor%kind() == NK_INT) then
                if (factor%int_value() == -1_int64) neg_at = k
            end if
        end do
        if (neg_at == 0) return

        ! Rebuilding from the remaining factors rather than requiring exactly
        ! two: the arena flattens nested products, so -(1*x) arrives as a
        ! three-factor node and a two-factor test would miss it.
        started = .false.
        do k = 1, nf
            if (k == neg_at) cycle
            factor = e%arg(k)
            if (started) then
                g = g*factor
            else
                g = factor
                started = .true.
            end if
        end do
        if (.not. started) return
        ok = .true.
    end function strip_negation

    ! ------------------------------------------------------- Faulhaber --

    !> S(m) = sum_{k=0}^{m-1} k^p, the unique polynomial antidifference of k^p,
    !> built from Bernoulli numbers in the B_1 = -1/2 convention:
    !>
    !>     S(m) = 1/(p+1) * sum_{j=0}^{p} binom(p+1, j) B_j m^(p+1-j)
    !>
    !> Every coefficient is an exact int64 rational. Any overflow along the way
    !> is reported rather than wrapped, because a wrapped coefficient is a
    !> plausible-looking wrong polynomial.
    function faulhaber_at(a, p, m, ok) result(s)
        type(arena_t), target, intent(inout) :: a
        integer,               intent(in)    :: p
        type(expr_t),          intent(in)    :: m
        logical,               intent(out)   :: ok
        type(expr_t) :: s
        integer(int64), allocatable :: bn(:), bd(:)
        integer(int64) :: cn, cd, bin
        integer :: j
        logical :: started

        s = num(a, 0)
        started = .false.
        call bernoulli_numbers(p, bn, bd, ok)
        if (.not. ok) return

        do j = 0, p
            if (bn(j) == 0_int64) cycle
            bin = binomial(p + 1, j, ok)
            if (.not. ok) return
            call rat_mul(bin, 1_int64, bn(j), bd(j), cn, cd, ok)
            if (.not. ok) return
            call rat_mul(cn, cd, 1_int64, int(p + 1, int64), cn, cd, ok)
            if (.not. ok) return
            if (started) then
                s = s + rat(a, cn, cd)*m**(p + 1 - j)
            else
                s = rat(a, cn, cd)*m**(p + 1 - j)
                started = .true.
            end if
        end do

        ! p >= 1 always leaves the j = 0 term, whose coefficient is 1/(p+1).
        ok = started
    end function faulhaber_at

    !> B_0 .. B_p from sum_{j=0}^{m} binom(m+1, j) B_j = 0, which fixes
    !> B_1 = -1/2 -- the convention that makes the Faulhaber polynomial above
    !> the sum over k = 0 .. m-1 rather than 0 .. m.
    subroutine bernoulli_numbers(p, bn, bd, ok)
        integer,                     intent(in)  :: p
        integer(int64), allocatable, intent(out) :: bn(:), bd(:)
        logical,                     intent(out) :: ok
        integer(int64) :: accn, accd, tn, td, bin
        integer :: m, j

        allocate (bn(0:p), bd(0:p))
        bn = 0_int64
        bd = 1_int64
        bn(0) = 1_int64
        ok = .true.

        do m = 1, p
            accn = 0_int64
            accd = 1_int64
            do j = 0, m - 1
                bin = binomial(m + 1, j, ok)
                if (.not. ok) return
                call rat_mul(bin, 1_int64, bn(j), bd(j), tn, td, ok)
                if (.not. ok) return
                call rat_add(accn, accd, tn, td, accn, accd, ok)
                if (.not. ok) return
            end do
            call rat_mul(accn, accd, -1_int64, int(m + 1, int64), &
                         bn(m), bd(m), ok)
            if (.not. ok) return
        end do
    end subroutine bernoulli_numbers

    ! -------------------------------------------------- exact rationals --

    !> Products and sums of small exact rationals, refusing on overflow rather
    !> than wrapping.
    subroutine rat_mul(an, ad, bn, bd, rn, rd, ok)
        integer(int64), intent(in)  :: an, ad, bn, bd
        integer(int64), intent(out) :: rn, rd
        logical,        intent(out) :: ok
        integer(int64) :: n, d

        call imul(an, bn, n, ok)
        if (.not. ok) return
        call imul(ad, bd, d, ok)
        if (.not. ok) return
        call rat_norm(n, d, rn, rd, ok)
    end subroutine rat_mul

    subroutine rat_add(an, ad, bn, bd, rn, rd, ok)
        integer(int64), intent(in)  :: an, ad, bn, bd
        integer(int64), intent(out) :: rn, rd
        logical,        intent(out) :: ok
        integer(int64) :: x, y, n, d

        call imul(an, bd, x, ok)
        if (.not. ok) return
        call imul(bn, ad, y, ok)
        if (.not. ok) return
        ! Nested rather than combined: `huge - y` itself overflows for the
        ! wrong sign of y, so the guard may not share an expression with the
        ! quantity it guards.
        if (y > 0_int64) then
            if (x > huge(0_int64) - y) then
                ok = .false.
                return
            end if
        else if (y < 0_int64) then
            if (x < -huge(0_int64) - y) then
                ok = .false.
                return
            end if
        end if
        n = x + y
        call imul(ad, bd, d, ok)
        if (.not. ok) return
        call rat_norm(n, d, rn, rd, ok)
    end subroutine rat_add

    subroutine rat_norm(n, d, rn, rd, ok)
        integer(int64), intent(in)  :: n, d
        integer(int64), intent(out) :: rn, rd
        logical,        intent(out) :: ok
        integer(int64) :: g

        ok = .false.
        rn = 0_int64
        rd = 1_int64
        if (d == 0_int64) return
        rn = n
        rd = d
        if (rd < 0_int64) then
            rn = -rn
            rd = -rd
        end if
        g = gcd64(abs(rn), rd)
        if (g > 1_int64) then
            rn = rn/g
            rd = rd/g
        end if
        ok = .true.
    end subroutine rat_norm

    subroutine imul(x, y, z, ok)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: z
        logical,        intent(out) :: ok

        z = 0_int64
        ok = .true.
        if (x == 0_int64 .or. y == 0_int64) return
        ! abs(-huge-1) is not representable, so that operand is declined
        ! before any absolute value is taken.
        if (x < -huge(0_int64) .or. y < -huge(0_int64)) then
            ok = .false.
            return
        end if
        if (abs(x) > huge(0_int64)/abs(y)) then
            ok = .false.
            return
        end if
        z = x*y
    end subroutine imul

    function gcd64(x, y) result(g)
        integer(int64), intent(in) :: x, y
        integer(int64) :: g, u, v, t

        u = abs(x)
        v = abs(y)
        do while (v /= 0_int64)
            t = mod(u, v)
            u = v
            v = t
        end do
        g = u
        if (g == 0_int64) g = 1_int64
    end function gcd64

    !> binom(n, k) built multiplicatively, with the division taken at every
    !> step so the intermediate stays as small as the result allows.
    function binomial(n, k, ok) result(c)
        integer, intent(in)  :: n, k
        logical, intent(out) :: ok
        integer(int64) :: c, t
        integer :: j, kk

        c = 0_int64
        ok = .true.
        if (k < 0 .or. k > n) return
        kk = min(k, n - k)
        c = 1_int64
        do j = 1, kk
            call imul(c, int(n - kk + j, int64), t, ok)
            if (.not. ok) return
            c = t/int(j, int64)
        end do
    end function binomial

    ! ------------------------------------------------------- validation --

    !> Everything that has to hold before any classification is attempted.
    subroutine check_inputs(a, body, var, lower, upper, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: body, var, lower, upper
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why

        ok = .false.
        why = ""

        if (.not. is_valid(body)) then
            why = "body is not a valid expression"
            return
        end if
        if (.not. is_valid(var)) then
            why = "summation variable is not a valid expression"
            return
        end if
        if (.not. is_valid(lower)) then
            why = "lower bound is not a valid expression"
            return
        end if
        if (.not. is_valid(upper)) then
            why = "upper bound is not a valid expression"
            return
        end if
        if (var%kind() /= NK_SYM) then
            why = "summation variable must be a symbol"
            return
        end if
        if (.not. associated(body%a, a)) then
            why = "body belongs to a different arena"
            return
        end if
        if (.not. same_arena(body, var)) then
            why = "summation variable belongs to a different arena"
            return
        end if
        if (.not. same_arena(body, lower)) then
            why = "lower bound belongs to a different arena"
            return
        end if
        if (.not. same_arena(body, upper)) then
            why = "upper bound belongs to a different arena"
            return
        end if

        call check_bound(lower, "lower", ok, why)
        if (.not. ok) return
        call check_bound(upper, "upper", ok, why)
        if (.not. ok) return

        if (depends_on(lower, var)) then
            ok = .false.
            why = "lower bound depends on the summation variable"
            return
        end if
        if (depends_on(upper, var)) then
            ok = .false.
            why = "upper bound depends on the summation variable"
            return
        end if
    end subroutine check_inputs

    subroutine check_bound(b, side, ok, why)
        type(expr_t),              intent(in)  :: b
        character(*),              intent(in)  :: side
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        ok = .false.
        why = ""

        if (is_infinite_symbol(b)) then
            why = side//" bound is infinite and no convergence test is "// &
                  "implemented"
            return
        end if
        if (b%kind() == NK_RAT) then
            why = side//" bound is a non-integer rational"
            return
        end if
        if (b%kind() == NK_REAL) then
            why = side//" bound is a floating-point value, not an integer "// &
                  "index"
            return
        end if
        ok = .true.
    end subroutine check_bound

    !> The names an infinite bound arrives under. Matched by name because
    !> there is no infinity node kind; anything else symbolic is treated as an
    !> ordinary finite unknown, which is what a symbolic bound means here.
    function is_infinite_symbol(b) result(yes)
        type(expr_t), intent(in) :: b
        logical :: yes
        character(:), allocatable :: n

        yes = .false.
        if (b%kind() /= NK_SYM .and. b%kind() /= NK_CONST) return
        n = lower_case(chars(b%name()))
        yes = n == "inf" .or. n == "infinity" .or. n == "oo"
    end function is_infinite_symbol

    function lower_case(s) result(t)
        character(*), intent(in) :: s
        character(len(s)) :: t
        integer :: k, c

        t = s
        do k = 1, len(s)
            c = iachar(t(k:k))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                t(k:k) = achar(c + 32)
            end if
        end do
    end function lower_case

    !> Both bounds as machine integers, when both are integer literals.
    subroutine concrete_bounds(lower, upper, lo, up, concrete)
        type(expr_t),   intent(in)  :: lower, upper
        integer(int64), intent(out) :: lo, up
        logical,        intent(out) :: concrete

        lo = 0_int64
        up = -1_int64
        concrete = .false.
        if (lower%kind() /= NK_INT) return
        if (upper%kind() /= NK_INT) return
        lo = lower%int_value()
        up = upper%int_value()
        concrete = .true.
    end subroutine concrete_bounds

    !> Can this ratio be compared with 1 without an assumption, and is it 1?
    !> Only literal numbers are decided; a symbol may or may not be 1 and
    !> saying otherwise is exactly the error this module exists to avoid.
    subroutine decide_unit_ratio(r, decided, is_one)
        type(expr_t), intent(in)  :: r
        logical,      intent(out) :: decided, is_one

        decided = .false.
        is_one = .false.
        select case (r%kind())
        case (NK_INT)
            decided = .true.
            is_one = r%int_value() == 1_int64
        case (NK_RAT)
            ! A rational node is always in lowest terms with denominator > 1,
            ! so it is never 1, but it is decided.
            decided = .true.
            is_one = .false.
        case (NK_REAL)
            decided = .true.
            is_one = r%real_value() == 1.0_dp
        end select
    end subroutine decide_unit_ratio

    function numeric_is_zero(r) result(yes)
        type(expr_t), intent(in) :: r
        logical :: yes

        yes = .false.
        select case (r%kind())
        case (NK_INT)
            yes = r%int_value() == 0_int64
        case (NK_REAL)
            yes = r%real_value() == 0.0_dp
        end select
    end function numeric_is_zero

    function itoa(v) result(s)
        integer(int64), intent(in) :: v
        character(:), allocatable :: s
        character(len=32) :: buf

        write (buf, '(i0)') v
        s = trim(buf)
    end function itoa

end module fortsym_sums
