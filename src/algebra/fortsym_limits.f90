module fortsym_limits
    ! Limits of a univariate expression at a finite point or at +/- infinity.
    !
    ! A limit is the operation where a plausible answer is cheapest to produce
    ! and most expensive to trust, so every path here ends in either a theorem
    ! or a refusal. Four theorems are implemented, and nothing else:
    !
    !   * continuity. If every node of `e` sits in the interior of its domain
    !     at the point, the composition is continuous there and the limit is the
    !     substituted value. The check walks the whole tree; it does not sample
    !     and conclude. It walks the tree the caller wrote and substitutes only
    !     where a number is needed, so a domain rule is never chosen from a node
    !     kind that substitution manufactured -- `x**x` at 0 is refused rather
    !     than read off the accidental `0**0`.
    !   * L'Hopital, for 0/0 at a finite point. The numerator and denominator
    !     are split structurally, their vanishing is decided by the engine's
    !     zero test (a decision, never a numeric guess), and the rule is applied
    !     under a hard iteration cap. Hitting the cap is a refusal: a loop that
    !     has not terminated is not an answer, and reporting the last quotient
    !     would be reporting an indeterminate form as a limit.
    !   * degree comparison, for a ratio of polynomials at +/- infinity. Exact,
    !     cheap, and it is where the sign of an infinite limit comes from.
    !   * the exponential/power/logarithm growth ordering at +infinity, for a
    !     single exp-log monomial K*exp(c*x)*x^p*log(x)^m. Restricted to one
    !     monomial on purpose: for a monomial the ordering (c, p, m) is the
    !     whole answer, with no lower-order terms that a heuristic would have to
    !     guess about.
    !
    ! What is deliberately absent is Gruntz's algorithm over most-rapidly-varying
    ! subexpressions (Gruntz 1996), which is the correct general method. It is
    ! not implemented here, and the cases it exists for -- nested exponentials,
    ! cancelling sums of comparable growth -- are refused rather than answered by
    ! an ordering heuristic, because that is precisely the family where a naive
    ! method is confidently wrong.
    !
    ! ONE-SIDEDNESS. Every theorem above proves a two-sided statement: the
    ! continuity check demands an interior point of the domain, and L'Hopital is
    ! applied to functions continuous on a full neighbourhood. So an answer for
    ! a finite point is always the two-sided limit, and a one-sided request is
    ! answered with it only because a two-sided limit forces both one-sided
    ! limits to exist and agree. There is no path in which one side is computed
    ! and passed off as the two-sided value. The consequence is that a function
    ! whose two one-sided limits differ -- 1/x at 0 -- is refused, never
    ! reported as infinite, and a genuinely one-sided limit such as sqrt(x) at 0
    ! from above is refused as well. That is the conservative direction.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr, only: expr_t, num, exp, is_valid, same_arena, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==)
    use fortsym_subs, only: subs
    use fortsym_diff, only: diff
    use fortsym_eval, only: free_symbols_of
    use fortsym_numeric, only: numeric_value
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    private

    public :: limit_of
    public :: limit_value_t, limit_point_t
    public :: finite_point, plus_infinity, minus_infinity
    public :: LIMIT_FINITE, LIMIT_PLUS_INF, LIMIT_MINUS_INF
    public :: AT_FINITE, AT_PLUS_INF, AT_MINUS_INF
    public :: FROM_BELOW, TWO_SIDED, FROM_ABOVE

    integer, parameter :: dp = real64

    !> What kind of value the limit is. An infinite limit is a distinct kind
    !> rather than a huge number, so a caller cannot mistake one for a value.
    integer, parameter :: LIMIT_FINITE = 0
    integer, parameter :: LIMIT_PLUS_INF = 1
    integer, parameter :: LIMIT_MINUS_INF = 2

    !> Where the limit is taken.
    integer, parameter :: AT_FINITE = 0
    integer, parameter :: AT_PLUS_INF = 1
    integer, parameter :: AT_MINUS_INF = 2

    !> From which side. See the one-sidedness note above.
    integer, parameter :: FROM_BELOW = -1
    integer, parameter :: TWO_SIDED = 0
    integer, parameter :: FROM_ABOVE = 1

    !> How many times L'Hopital may be applied before the attempt is abandoned.
    integer, parameter :: MAX_LHOPITAL = 8

    !> Largest polynomial degree the rational path will build.
    integer, parameter :: MAX_DEGREE = 24

    !> A closed subexpression whose magnitude is below this is treated as
    !> undecided rather than as nonzero. Sign decisions taken from a floating
    !> point probe are only safe with slack, and refusing inside the slack is
    !> the direction that cannot produce a wrong limit.
    real(dp), parameter :: MARGIN = 1.0e-9_dp

    !> The value of a limit.
    type :: limit_value_t
        integer      :: kind = LIMIT_FINITE
        !> Meaningful only when kind is LIMIT_FINITE.
        type(expr_t) :: value
    end type limit_value_t

    !> The point the variable approaches.
    type :: limit_point_t
        integer      :: at = AT_FINITE
        !> Meaningful only when at is AT_FINITE. Must be a closed expression.
        type(expr_t) :: value
    end type limit_point_t

contains

    ! ---------------------------------------------------- point constructors --

    function finite_point(value) result(p)
        type(expr_t), intent(in) :: value
        type(limit_point_t)      :: p
        p%at = AT_FINITE
        p%value = value
    end function finite_point

    function plus_infinity() result(p)
        type(limit_point_t) :: p
        p%at = AT_PLUS_INF
    end function plus_infinity

    function minus_infinity() result(p)
        type(limit_point_t) :: p
        p%at = AT_MINUS_INF
    end function minus_infinity

    ! ------------------------------------------------------------ entry point --

    !> The limit of `e` as `var` approaches `point` from `direction`, or a
    !> refusal naming what could not be established.
    function limit_of(a, e, var, point, direction, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        type(limit_point_t),       intent(in)    :: point
        integer,                   intent(in)    :: direction
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(limit_value_t) :: r
        type(native_engine_t) :: eng

        ok = .false.
        why = ""
        r%kind = LIMIT_FINITE
        r%value = e

        call validate(a, e, var, point, direction, ok, why)
        if (.not. ok) return
        ok = .false.

        eng = make_native_engine(a)

        ! A subexpression free of the variable is its own limit everywhere,
        ! which also disposes of the constant case before any machinery runs.
        if (.not. depends_on(e, var)) then
            r%value = simplified(eng, e, ok)
            if (.not. ok) then
                why = "the expression does not involve the variable but "// &
                      "could not be simplified"
                return
            end if
            ok = .true.
            return
        end if

        select case (point%at)
        case (AT_FINITE)
            r = limit_at_finite(a, eng, e, var, point%value, ok, why)
        case default
            r = limit_at_infinity(a, eng, e, var, point%at, ok, why)
        end select
    end function limit_of

    !> Everything that must hold before any theorem is worth trying. A limit of
    !> a two-variable expression, or towards a point that is itself symbolic,
    !> has no meaning here and is stopped at the door.
    subroutine validate(a, e, var, point, direction, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e, var
        type(limit_point_t),       intent(in)    :: point
        integer,                   intent(in)    :: direction
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(str_t), allocatable :: names(:)
        real(dp) :: pv
        character(:), allocatable :: inner
        logical :: good
        integer :: k

        ok = .false.
        why = ""

        if (.not. is_valid(e) .or. .not. is_valid(var)) then
            why = "expression or variable is not a valid expression"
            return
        end if
        if (.not. same_arena(e, var)) then
            why = "expression and variable live in different arenas"
            return
        end if
        if (.not. associated(e%a, a)) then
            why = "expression does not belong to the arena passed in"
            return
        end if
        if (var%kind() /= NK_SYM) then
            why = "the limit variable must be a symbol"
            return
        end if

        names = free_symbols_of(e)
        do k = 1, size(names)
            if (chars(names(k)) /= chars(var%name())) then
                why = "expression contains the free symbol "// &
                      chars(names(k))//" besides the limit variable"
                return
            end if
        end do

        select case (direction)
        case (FROM_BELOW, TWO_SIDED, FROM_ABOVE)
        case default
            why = "direction must be FROM_BELOW, TWO_SIDED or FROM_ABOVE"
            return
        end select

        select case (point%at)
        case (AT_FINITE)
            if (.not. is_valid(point%value)) then
                why = "the finite point is not a valid expression"
                return
            end if
            if (.not. same_arena(e, point%value)) then
                why = "the point lives in a different arena"
                return
            end if
            ! The point has to be an actual number: substituting a symbol into
            ! the expression would leave every later decision undecidable.
            call numeric_value(point%value, pv, good, inner)
            if (.not. good) then
                why = "the point is not a closed finite value: "//inner
                return
            end if
        case (AT_PLUS_INF)
            if (direction == FROM_ABOVE) then
                why = "+infinity can only be approached from below"
                return
            end if
        case (AT_MINUS_INF)
            if (direction == FROM_BELOW) then
                why = "-infinity can only be approached from above"
                return
            end if
        case default
            why = "unknown point kind"
            return
        end select

        ok = .true.
    end subroutine validate

    ! ------------------------------------------------------- finite point --

    !> Continuity first, then L'Hopital on 0/0. Both conclusions are two-sided.
    function limit_at_finite(a, eng, e, var, p, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, var, p
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(limit_value_t) :: r
        type(expr_t) :: at_point, numer, denom, ns, ds
        character(:), allocatable :: inner
        logical :: good, dgood
        integer :: step, vn, vd

        ok = .false.
        why = ""
        r%kind = LIMIT_FINITE
        r%value = e

        ! Continuity: the ORIGINAL tree is checked node by node, with the point
        ! substituted only where a numeric value is needed. Walking the
        ! original tree is what makes the domain rule for a power depend on the
        ! exponent the user wrote rather than on whatever the exponent collapses
        ! to at this particular point.
        call require_continuous(e, var, p, good, inner)
        if (good) then
            at_point = subs(e, var, p)
            r%value = simplified(eng, at_point, ok)
            if (.not. ok) then
                why = "the expression is continuous at the point but the "// &
                      "substituted value could not be simplified"
                return
            end if
            ok = .true.
            return
        end if

        call split_quotient(e, numer, denom)
        if (denom == num(a, 1)) then
            why = "not continuous at the point ("//inner// &
                  ") and not a quotient, so no rule applies"
            return
        end if

        do step = 1, MAX_LHOPITAL
            ns = subs(numer, var, p)
            ds = subs(denom, var, p)

            ! L'Hopital needs both parts defined and continuous at the point;
            ! without that the quotient of derivatives says nothing.
            call require_continuous(numer, var, p, good, inner)
            if (.not. good) then
                why = "numerator is not continuous at the point: "//inner
                return
            end if
            call require_continuous(denom, var, p, good, inner)
            if (.not. good) then
                why = "denominator is not continuous at the point: "//inner
                return
            end if

            vd = zero_verdict(eng, ds)
            if (vd == VERDICT_FALSE) then
                r%value = simplified(eng, ns/ds, ok)
                if (.not. ok) then
                    why = "the limiting quotient could not be simplified"
                    return
                end if
                ok = .true.
                return
            end if
            if (vd /= VERDICT_TRUE) then
                why = "cannot decide whether the denominator vanishes at "// &
                      "the point"
                return
            end if

            vn = zero_verdict(eng, ns)
            if (vn == VERDICT_FALSE) then
                ! A nonzero numerator over a vanishing denominator is a pole.
                ! The two sides generally have opposite signs, so there is no
                ! two-sided limit to report -- and reporting one side as the
                ! answer is exactly the mistake this module refuses to make.
                why = "denominator vanishes and numerator does not: this is "// &
                      "a pole, and the one-sided limits need not agree"
                return
            end if
            if (vn /= VERDICT_TRUE) then
                why = "cannot decide whether the numerator vanishes at the "// &
                      "point"
                return
            end if

            ! 0/0 confirmed on both sides: differentiate and go round again.
            numer = eng_diff(eng, numer, var, good)
            denom = eng_diff(eng, denom, var, dgood)
            if (.not. good .or. .not. dgood) then
                why = "could not differentiate for L'Hopital's rule"
                return
            end if
        end do

        why = "L'Hopital's rule reached its iteration cap without resolving "// &
              "the indeterminate form"
    end function limit_at_finite

    ! ------------------------------------------------------ infinite point --

    !> Degree comparison first, then the growth ordering for a single monomial.
    function limit_at_infinity(a, eng, e, var, at, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, var
        integer,                   intent(in)    :: at
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(limit_value_t) :: r
        character(:), allocatable :: inner

        ok = .false.
        r%kind = LIMIT_FINITE
        r%value = e

        r = rational_at_infinity(a, eng, e, var, at, ok, inner)
        if (ok) return

        if (at == AT_PLUS_INF) then
            r = monomial_at_infinity(a, eng, e, var, ok, why)
            if (ok) return
            why = "no rule applies: "//inner//"; "//why
            return
        end if

        why = "no rule applies at -infinity: "//inner// &
              "; the growth ordering is only implemented at +infinity"
    end function limit_at_infinity

    !> A ratio of polynomials settles at infinity by leading degree alone. The
    !> leading coefficients are stripped of provable zeros first, because a
    !> coefficient that is only syntactically present would falsify the degree.
    function rational_at_infinity(a, eng, e, var, at, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, var
        integer,                   intent(in)    :: at
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(limit_value_t) :: r
        type(expr_t) :: numer, denom, ratio
        type(expr_t), allocatable :: cn(:), cd(:)
        real(dp) :: rv
        logical :: good
        integer :: dn, dd, gap
        character(:), allocatable :: inner

        ok = .false.
        why = ""
        r%kind = LIMIT_FINITE
        r%value = e

        call split_quotient(e, numer, denom)
        call poly_of(numer, var, cn, good)
        if (.not. good) then
            why = "the numerator is not a polynomial in the variable"
            return
        end if
        call poly_of(denom, var, cd, good)
        if (.not. good) then
            why = "the denominator is not a polynomial in the variable"
            return
        end if

        call trim_leading(eng, cn, dn, good)
        if (.not. good) then
            why = "cannot decide the degree of the numerator"
            return
        end if
        call trim_leading(eng, cd, dd, good)
        if (.not. good) then
            why = "cannot decide the degree of the denominator"
            return
        end if
        if (dd < 0) then
            why = "the denominator is identically zero"
            return
        end if
        if (dn < 0) then
            ! The numerator is the zero polynomial, so the function is zero.
            r%value = num(a, 0)
            ok = .true.
            return
        end if

        if (dn < dd) then
            r%value = num(a, 0)
            ok = .true.
            return
        end if

        ratio = simplified(eng, cn(dn + 1)/cd(dd + 1), good)
        if (.not. good) then
            why = "the ratio of leading coefficients could not be simplified"
            return
        end if

        if (dn == dd) then
            r%value = ratio
            ok = .true.
            return
        end if

        ! Degree of the numerator wins, so the magnitude diverges. The sign
        ! needs the sign of the leading ratio, and at -infinity also the parity
        ! of the degree gap.
        call numeric_value(ratio, rv, good, inner)
        if (.not. good) then
            why = "cannot evaluate the ratio of leading coefficients: "//inner
            return
        end if
        ! The two leading coefficients were each PROVED nonzero by the exact
        ! three-valued zero test in trim_leading, so their ratio is exactly
        ! nonzero and the float probe is needed only for its sign. Discarding
        ! small-but-exact ratios here would refuse limits such as x/10**10 at
        ! +infinity, which is decidable. The only unusable probe is one that
        ! underflowed all the way to zero and so carries no sign at all.
        if (rv == 0.0_dp) then
            why = "the ratio of leading coefficients is nonzero but "// &
                  "underflows to zero in double precision, so its sign "// &
                  "cannot be read"
            return
        end if
        gap = dn - dd
        if (at == AT_MINUS_INF .and. mod(gap, 2) == 1) rv = -rv

        if (rv > 0.0_dp) then
            r%kind = LIMIT_PLUS_INF
        else
            r%kind = LIMIT_MINUS_INF
        end if
        r%value = num(a, 0)
        ok = .true.
    end function rational_at_infinity

    !> The growth ordering, for expressions that are exactly one exp-log
    !> monomial K*exp(c*x)*x^p*log(x)^m. For a single monomial the triple
    !> (c, p, m) compared lexicographically against zero is the whole answer:
    !> exp beats any power, any power beats any power of the logarithm. Sums are
    !> refused, because there the ordering only gives the dominant term and the
    !> cancellations are exactly what a heuristic gets wrong.
    function monomial_at_infinity(a, eng, e, var, ok, why) result(r)
        type(arena_t), target,     intent(inout) :: a
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, var
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(limit_value_t) :: r
        type(expr_t) :: konst
        real(dp) :: c, p, m, kv
        logical :: good
        integer :: order, verdict
        character(:), allocatable :: inner

        ok = .false.
        why = ""
        r%kind = LIMIT_FINITE
        r%value = e

        c = 0.0_dp
        p = 0.0_dp
        m = 0.0_dp
        konst = num(a, 1)
        call growth_of(e, var, c, p, m, konst, good, why)
        if (.not. good) then
            why = "not an exp-log monomial in the variable: "//why
            return
        end if

        call decide_order(c, p, m, order, good, why)
        if (.not. good) return

        konst = simplified(eng, konst, good)
        if (.not. good) then
            why = "the constant factor could not be simplified"
            return
        end if

        if (order < 0) then
            ! Every factor decays, so the monomial decays to zero.
            r%value = num(a, 0)
            ok = .true.
            return
        end if

        ! The constant factor has not been zero-tested yet, so decide it
        ! exactly first. An exactly zero factor makes the whole monomial the
        ! zero function; a proved-nonzero factor only needs the float probe for
        ! its sign, and a small exact constant is no reason to refuse.
        verdict = zero_verdict(eng, konst)
        if (verdict == VERDICT_TRUE) then
            r%value = num(a, 0)
            ok = .true.
            return
        end if
        if (verdict /= VERDICT_FALSE) then
            why = "cannot decide whether the constant factor vanishes"
            return
        end if

        call numeric_value(konst, kv, good, inner)
        if (.not. good) then
            why = "cannot evaluate the constant factor: "//inner
            return
        end if
        if (kv == 0.0_dp) then
            why = "the constant factor is nonzero but underflows to zero in "// &
                  "double precision, so its sign cannot be read"
            return
        end if

        if (order == 0) then
            r%value = konst
            ok = .true.
            return
        end if

        if (kv > 0.0_dp) then
            r%kind = LIMIT_PLUS_INF
        else
            r%kind = LIMIT_MINUS_INF
        end if
        r%value = num(a, 0)
        ok = .true.
    end function monomial_at_infinity

    !> Lexicographic comparison of the growth triple against zero: +1 diverges,
    !> 0 tends to the constant factor, -1 tends to zero. An exponent that is
    !> nonzero but within the numeric margin is undecided rather than equal,
    !> since treating a tiny nonzero power as zero swaps a divergence for a
    !> finite value.
    subroutine decide_order(c, p, m, order, ok, why)
        real(dp),                  intent(in)  :: c, p, m
        integer,                   intent(out) :: order
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        real(dp) :: parts(3)
        integer :: k

        order = 0
        ok = .false.
        why = ""
        parts = [c, p, m]

        do k = 1, 3
            if (parts(k) /= 0.0_dp .and. abs(parts(k)) < MARGIN) then
                why = "a growth exponent is too small to tell from zero"
                return
            end if
            if (parts(k) > 0.0_dp) then
                order = 1
                ok = .true.
                return
            end if
            if (parts(k) < 0.0_dp) then
                order = -1
                ok = .true.
                return
            end if
        end do

        order = 0
        ok = .true.
    end subroutine decide_order

    !> Accumulate the growth triple of an exp-log monomial. `konst` collects the
    !> factors that do not involve the variable.
    recursive subroutine growth_of(e, var, c, p, m, konst, ok, why)
        type(expr_t),              intent(in)    :: e, var
        real(dp),                  intent(inout) :: c, p, m
        type(expr_t),              intent(inout) :: konst
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: base, expo, arg, sub_konst
        type(expr_t), allocatable :: coeffs(:)
        real(dp) :: q, cb, pb, mb, kv, slope
        logical :: good
        integer :: k, deg
        character(:), allocatable :: inner

        ok = .false.
        why = ""

        if (.not. depends_on(e, var)) then
            konst = konst*e
            ok = .true.
            return
        end if

        if (e == var) then
            p = p + 1.0_dp
            ok = .true.
            return
        end if

        select case (e%kind())

        case (NK_MUL)
            do k = 1, e%nargs()
                call growth_of(e%arg(k), var, c, p, m, konst, good, inner)
                if (.not. good) then
                    why = inner
                    return
                end if
            end do
            ok = .true.

        case (NK_POW)
            base = e%arg(1)
            expo = e%arg(2)
            if (depends_on(expo, var)) then
                why = "an exponent depends on the variable"
                return
            end if
            call numeric_value(expo, q, good, inner)
            if (.not. good) then
                why = "an exponent has no numeric value: "//inner
                return
            end if
            cb = 0.0_dp
            pb = 0.0_dp
            mb = 0.0_dp
            sub_konst = num_like(e, 1)
            call growth_of(base, var, cb, pb, mb, sub_konst, good, inner)
            if (.not. good) then
                why = inner
                return
            end if
            ! Raising to a non-integer power needs a positive base, otherwise
            ! the factor is not even real for large x.
            call numeric_value(sub_konst, kv, good, inner)
            if (.not. good) then
                why = "the constant part of a power base has no value: "//inner
                return
            end if
            if (expo%kind() /= NK_INT .and. kv <= MARGIN) then
                why = "a non-integer power of a base that is not provably "// &
                      "positive"
                return
            end if
            c = c + q*cb
            p = p + q*pb
            m = m + q*mb
            konst = konst*(sub_konst**expo)
            ok = .true.

        case (NK_FUNC)
            arg = e%arg(1)
            select case (chars(e%name()))
            case ("exp")
                ! exp(u) contributes only when u is linear in the variable; a
                ! higher-degree exponent is outside the ordering used here.
                call poly_of(arg, var, coeffs, good)
                if (.not. good) then
                    why = "the argument of exp is not a polynomial"
                    return
                end if
                deg = size(coeffs) - 1
                if (deg /= 1) then
                    why = "the argument of exp is not linear in the variable"
                    return
                end if
                call numeric_value(coeffs(2), slope, good, inner)
                if (.not. good) then
                    why = "the slope inside exp has no numeric value: "//inner
                    return
                end if
                c = c + slope
                konst = konst*exp(coeffs(1))
                ok = .true.
            case ("log")
                if (.not. (arg == var)) then
                    why = "only log of the bare variable is handled"
                    return
                end if
                m = m + 1.0_dp
                ok = .true.
            case default
                why = "function "//chars(e%name())// &
                      " is outside the growth ordering"
            end select

        case default
            why = "a sum or unsupported node appears in the monomial"
        end select
    end subroutine growth_of

    ! -------------------------------------------------------- continuity --

    !> True when every node of `e`, with `var` set to the point `p`, sits in the
    !> interior of its domain, which makes the whole composition continuous
    !> there. `why` names the first node that fails.
    !>
    !> The walk is over the ORIGINAL tree and substitution happens only inside
    !> `point_value`, where a number is actually needed. That ordering matters:
    !> the domain rule for `base**expo` depends on whether the exponent is a
    !> genuine constant integer, and substituting first would let an exponent
    !> that depends on the variable masquerade as one whenever it happens to
    !> collapse to an integer at this particular point.
    recursive subroutine require_continuous(e, var, p, ok, why)
        type(expr_t),              intent(in)  :: e, var, p
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: base, expo, arg
        real(dp) :: v
        integer :: k
        logical :: good
        character(:), allocatable :: inner, name

        ok = .false.
        why = ""

        select case (e%kind())

        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT)
            ok = .true.

        case (NK_CONST)
            select case (chars(e%name()))
            case ("pi", "e")
                ok = .true.
            case default
                ! The imaginary unit has no place in a real limit.
                why = "constant "//chars(e%name())//" has no real value"
            end select

        case (NK_SYM)
            ! The limit variable is the one symbol allowed here; it stands for
            ! the point. Anything else means the caller slipped a second free
            ! symbol past validation.
            if (e == var) then
                ok = .true.
            else
                why = "symbol "//chars(e%name())//" is free besides the "// &
                      "limit variable"
            end if

        case (NK_ADD, NK_MUL)
            do k = 1, e%nargs()
                call require_continuous(e%arg(k), var, p, good, inner)
                if (.not. good) then
                    why = inner
                    return
                end if
            end do
            ok = .true.

        case (NK_POW)
            base = e%arg(1)
            expo = e%arg(2)
            call require_continuous(base, var, p, good, inner)
            if (.not. good) then
                why = inner
                return
            end if
            call require_continuous(expo, var, p, good, inner)
            if (.not. good) then
                why = inner
                return
            end if
            call point_value(base, var, p, v, good)
            if (.not. good) then
                why = "cannot decide the value of a power base"
                return
            end if
            if (depends_on(expo, var)) then
                ! a**b is continuous at (v, w) with v <= 0 only when the
                ! exponent is a genuine constant integer. A variable exponent
                ! is not, however integral its value at this one point turns
                ! out to be, so the base must be provably positive.
                if (v <= MARGIN) then
                    why = "a power whose exponent depends on the variable "// &
                          "needs a base that is provably positive"
                    return
                end if
                ok = .true.
            else if (expo%kind() == NK_INT) then
                if (expo%int_value() < 0_int64) then
                    if (abs(v) <= MARGIN) then
                        why = "a negative power of something that vanishes "// &
                              "at the point"
                        return
                    end if
                end if
                ok = .true.
            else
                ! A non-integer exponent needs a strictly positive base to stay
                ! real and continuous on both sides.
                if (v <= MARGIN) then
                    why = "a non-integer power of a base that is not "// &
                          "provably positive"
                    return
                end if
                ok = .true.
            end if

        case (NK_FUNC)
            do k = 1, e%nargs()
                call require_continuous(e%arg(k), var, p, good, inner)
                if (.not. good) then
                    why = inner
                    return
                end if
            end do
            name = chars(e%name())
            arg = e%arg(1)
            select case (name)

            ! Entire on the real line: no domain condition to check.
            case ("sin", "cos", "atan", "sinh", "cosh", "tanh", "asinh", &
                  "exp", "erf", "erfc", "abs")
                ok = .true.

            case ("tan")
                call point_value(arg, var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the argument of tan"
                    return
                end if
                if (abs(cos(v)) <= MARGIN) then
                    why = "tan has a pole at the point"
                    return
                end if
                ok = .true.

            case ("log", "gamma", "loggamma")
                call point_value(arg, var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the argument of "//name
                    return
                end if
                if (v <= MARGIN) then
                    why = name//" needs an argument that is provably positive"
                    return
                end if
                ok = .true.

            case ("sqrt")
                call point_value(arg, var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the argument of sqrt"
                    return
                end if
                ! Zero is on the boundary of the domain: sqrt is continuous
                ! there only from one side, so it is refused rather than
                ! answered with a two-sided claim.
                if (v <= MARGIN) then
                    why = "sqrt at or below zero is a domain boundary, not "// &
                          "an interior point"
                    return
                end if
                ok = .true.

            case ("asin", "acos")
                call point_value(arg, var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the argument of "//name
                    return
                end if
                if (abs(v) >= 1.0_dp - MARGIN) then
                    why = name//" is at the edge of its domain"
                    return
                end if
                ok = .true.

            case ("atanh")
                call point_value(arg, var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the argument of atanh"
                    return
                end if
                if (abs(v) >= 1.0_dp - MARGIN) then
                    why = "atanh is at the edge of its domain"
                    return
                end if
                ok = .true.

            case ("acosh")
                call point_value(arg, var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the argument of acosh"
                    return
                end if
                if (v <= 1.0_dp + MARGIN) then
                    why = "acosh is at the edge of its domain"
                    return
                end if
                ok = .true.

            case ("atan2")
                if (e%nargs() /= 2) then
                    why = "atan2 needs two arguments"
                    return
                end if
                call point_value(e%arg(1), var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the first argument of atan2"
                    return
                end if
                call point_value(e%arg(2), var, p, v, good)
                if (.not. good) then
                    why = "cannot decide the second argument of atan2"
                    return
                end if
                ! The branch cut runs along the negative second argument, so
                ! only the right half plane is accepted.
                if (v <= MARGIN) then
                    why = "atan2 near its branch cut"
                    return
                end if
                ok = .true.

            case default
                why = "function "//name//" has no continuity rule here"
            end select

        case default
            why = "unsupported node kind in the expression"
        end select
    end subroutine require_continuous

    !> The value of `e` at var = p. Substitution happens here and nowhere else
    !> in the continuity walk, so the walk keeps seeing the node kinds the user
    !> wrote. Refused outright when the evaluator declines -- a pole, a branch
    !> cut, an unknown head -- so a domain test never runs on an invented
    !> number. A value inside the margin is still reported: callers that need
    !> the magnitude and callers that need the sign both see the same number
    !> and apply their own threshold.
    subroutine point_value(e, var, p, v, ok)
        type(expr_t), intent(in)  :: e, var, p
        real(dp),     intent(out) :: v
        logical,      intent(out) :: ok
        character(:), allocatable :: why

        call numeric_value(subs(e, var, p), v, ok, why)
        if (.not. ok) v = 0.0_dp
    end subroutine point_value

    ! ---------------------------------------------------------- structure --

    !> Split into numerator and denominator by pulling out factors that are
    !> powers with a negative integer exponent. This is how division is
    !> represented in the arena, so the split is exact rather than a guess at
    !> what the user meant.
    subroutine split_quotient(e, numer, denom)
        type(expr_t), intent(in)  :: e
        type(expr_t), intent(out) :: numer, denom
        type(expr_t) :: factor, base, expo
        type(arena_t), pointer :: a
        integer :: k

        a => e%a
        numer = num(a, 1)
        denom = num(a, 1)

        select case (e%kind())
        case (NK_MUL)
            do k = 1, e%nargs()
                factor = e%arg(k)
                if (is_negative_power(factor)) then
                    base = factor%arg(1)
                    expo = factor%arg(2)
                    denom = denom*(base**num(a, -expo%int_value()))
                else
                    numer = numer*factor
                end if
            end do
        case (NK_POW)
            if (is_negative_power(e)) then
                base = e%arg(1)
                expo = e%arg(2)
                denom = base**num(a, -expo%int_value())
            else
                numer = e
            end if
        case default
            numer = e
        end select
    end subroutine split_quotient

    function is_negative_power(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(expr_t) :: expo

        yes = .false.
        if (e%kind() /= NK_POW) return
        expo = e%arg(2)
        if (expo%kind() /= NK_INT) return
        yes = expo%int_value() < 0_int64
    end function is_negative_power

    !> Coefficients of a univariate polynomial, index k holding the coefficient
    !> of degree k-1. Fails for anything that is not a polynomial in `var`.
    recursive subroutine poly_of(e, var, coeffs, ok)
        type(expr_t),              intent(in)  :: e, var
        type(expr_t), allocatable, intent(out) :: coeffs(:)
        logical,                   intent(out) :: ok
        type(expr_t), allocatable :: left(:), right(:), acc(:)
        type(expr_t) :: expo
        type(arena_t), pointer :: a
        integer(int64) :: n
        integer :: k

        a => e%a
        ok = .false.
        allocate (coeffs(1))
        coeffs(1) = num(a, 0)

        if (.not. depends_on(e, var)) then
            coeffs(1) = e
            ok = .true.
            return
        end if

        if (e == var) then
            deallocate (coeffs)
            allocate (coeffs(2))
            coeffs(1) = num(a, 0)
            coeffs(2) = num(a, 1)
            ok = .true.
            return
        end if

        select case (e%kind())

        case (NK_ADD)
            call poly_of(e%arg(1), var, acc, ok)
            if (.not. ok) return
            do k = 2, e%nargs()
                call poly_of(e%arg(k), var, right, ok)
                if (.not. ok) return
                acc = poly_add(acc, right)
            end do
            call move_alloc(acc, coeffs)
            ok = size(coeffs) - 1 <= MAX_DEGREE

        case (NK_MUL)
            call poly_of(e%arg(1), var, acc, ok)
            if (.not. ok) return
            do k = 2, e%nargs()
                call poly_of(e%arg(k), var, right, ok)
                if (.not. ok) return
                if (size(acc) + size(right) - 2 > MAX_DEGREE) then
                    ok = .false.
                    return
                end if
                acc = poly_mul(acc, right)
            end do
            call move_alloc(acc, coeffs)
            ok = .true.

        case (NK_POW)
            expo = e%arg(2)
            if (expo%kind() /= NK_INT) return
            n = expo%int_value()
            if (n < 0_int64 .or. n > int(MAX_DEGREE, int64)) return
            call poly_of(e%arg(1), var, left, ok)
            if (.not. ok) return
            if ((size(left) - 1)*int(n) > MAX_DEGREE) then
                ok = .false.
                return
            end if
            allocate (acc(1))
            acc(1) = num(a, 1)
            do k = 1, int(n)
                acc = poly_mul(acc, left)
            end do
            call move_alloc(acc, coeffs)
            ok = .true.

        case default
            ok = .false.
        end select
    end subroutine poly_of

    function poly_add(x, y) result(s)
        type(expr_t), intent(in)  :: x(:), y(:)
        type(expr_t), allocatable :: s(:)
        integer :: n, k

        n = max(size(x), size(y))
        allocate (s(n))
        do k = 1, n
            if (k <= size(x) .and. k <= size(y)) then
                s(k) = x(k) + y(k)
            else if (k <= size(x)) then
                s(k) = x(k)
            else
                s(k) = y(k)
            end if
        end do
    end function poly_add

    function poly_mul(x, y) result(s)
        type(expr_t), intent(in)  :: x(:), y(:)
        type(expr_t), allocatable :: s(:)
        type(arena_t), pointer :: a
        integer :: n, i, j

        a => x(1)%a
        n = size(x) + size(y) - 1
        allocate (s(n))
        do i = 1, n
            s(i) = num(a, 0)
        end do
        do i = 1, size(x)
            do j = 1, size(y)
                s(i + j - 1) = s(i + j - 1) + x(i)*y(j)
            end do
        end do
    end function poly_mul

    !> Degree of a coefficient list after discarding leading coefficients that
    !> the engine proves zero. A coefficient it cannot decide stops the search:
    !> assuming it nonzero would overstate the degree, assuming it zero would
    !> understate it, and either way the limit could come out wrong. A zero
    !> polynomial reports degree -1.
    subroutine trim_leading(eng, coeffs, degree, ok)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: coeffs(:)
        integer,                   intent(out)   :: degree
        logical,                   intent(out)   :: ok
        integer :: k, verdict

        ok = .false.
        degree = -1

        do k = size(coeffs), 1, -1
            verdict = zero_verdict(eng, coeffs(k))
            if (verdict == VERDICT_FALSE) then
                degree = k - 1
                ok = .true.
                return
            end if
            if (verdict /= VERDICT_TRUE) return
        end do

        degree = -1
        ok = .true.
    end subroutine trim_leading

    ! ------------------------------------------------------------ helpers --

    recursive function depends_on(e, var) result(yes)
        type(expr_t), intent(in) :: e, var
        logical                  :: yes
        integer :: k

        yes = .true.
        if (e == var) return
        do k = 1, e%nargs()
            if (depends_on(e%arg(k), var)) return
        end do
        yes = .false.
    end function depends_on

    function num_like(e, value) result(r)
        type(expr_t), intent(in) :: e
        integer,      intent(in) :: value
        type(expr_t)             :: r
        type(arena_t), pointer :: a

        a => e%a
        r = num(a, value)
    end function num_like

    function simplified(eng, e, ok) result(r)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t),          intent(in)    :: e
        logical,               intent(out)   :: ok
        type(expr_t)                         :: r
        type(engine_result_t) :: res

        res = eng%simplify(e)
        ok = res%ok
        if (ok) then
            r = res%value
        else
            r = e
        end if
    end function simplified

    function eng_diff(eng, e, var, ok) result(r)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t),          intent(in)    :: e, var
        logical,               intent(out)   :: ok
        type(expr_t)                         :: r

        r = diff(e, var)
        r = simplified(eng, r, ok)
    end function eng_diff

    !> Three-valued: TRUE, FALSE, or UNKNOWN. UNKNOWN is never rounded to
    !> either of the other two anywhere in this module.
    function zero_verdict(eng, e) result(verdict)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t),          intent(in)    :: e
        integer                              :: verdict
        type(engine_result_t) :: res

        res = eng%zero_test(e)
        verdict = res%verdict
    end function zero_verdict

end module fortsym_limits
