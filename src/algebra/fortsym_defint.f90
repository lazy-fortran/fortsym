module fortsym_defint
    ! Definite integration by the fundamental theorem, with continuity proved
    ! before the theorem is applied.
    !
    ! The fundamental theorem says int_a^b f = F(b) - F(a) when f is continuous
    ! on [a, b] and F' = f there. Drop the continuity hypothesis and the formula
    ! still produces a number, which is the whole danger: for f = 1/x**2 on
    ! [-1, 1] it produces -2, a negative value for a strictly positive
    ! integrand, and nothing in the arithmetic complains. A definite integrator
    ! that evaluates an antiderivative at the endpoints without checking the
    ! path between them is therefore not an approximation of the right answer,
    ! it is a generator of confident wrong ones.
    !
    ! So this module spends nearly all of its code on the hypothesis rather
    ! than on the formula:
    !
    !   * the integrand must be closed in the integration variable. A free
    !     parameter makes continuity undecidable -- 1/(x - c) is continuous on
    !     [0, 1] for some c and not for others -- so it is refused rather than
    !     assumed benign.
    !   * every subexpression that can leave the reals or blow up is checked on
    !     the closed interval: denominators must stay away from zero, log
    !     arguments must stay positive, sqrt arguments nonnegative, asin and
    !     acos arguments inside [-1, 1], tan arguments clear of the odd
    !     multiples of pi/2.
    !   * those checks reduce to bounding a subexpression on [xa, xb], which is
    !     done the only way that is a proof rather than a sample: the extreme
    !     values of a differentiable g on a closed interval occur at the
    !     endpoints or at a zero of g'. When g' is a polynomial whose complete
    !     root list `solve_polynomial` verifies, that candidate set is
    !     complete, and min and max over it are the true min and max. When g'
    !     is not such a polynomial the bound is refused. Sampling is never used
    !     to conclude that a function has no zero, because it cannot.
    !   * the antiderivative gets the same continuity treatment as the
    !     integrand. F' = f is a formal identity that survives a jump in F, so
    !     a continuous integrand alone does not license the subtraction.
    !
    ! Everything else refuses: infinite endpoints (that is a limit, not an
    ! evaluation), symbolic endpoints, and any head with no continuity rule.
    ! The refusals are wide. They are also the reason the answers can be
    ! trusted.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, num, is_valid, operator(-)
    use fortsym_subs, only: subs
    use fortsym_diff, only: diff
    use fortsym_eval, only: binding_t, eval_expr, free_symbols_of
    use fortsym_integrate, only: integrate
    use fortsym_polysolve, only: solve_polynomial
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    private

    public :: definite_integral

    integer, parameter :: dp = real64

    real(dp), parameter :: PI_VALUE = &
        3.141592653589793238462643383279502884_dp

    ! Relative width of the band in which a sign test declines to decide. A
    ! bound that lands inside it is neither accepted as safely away from zero
    ! nor reported as a proven crossing: the integral is refused, because a
    ! floating-point extremum that close to zero does not distinguish a
    ! denominator that touches zero from one that misses it by rounding.
    real(dp), parameter :: SIGN_BAND = 1.0e-9_dp

    ! An exponent within this of an integer is treated as that integer. The
    ! exponents that reach here come from exact rationals evaluated to double,
    ! so the gap between a true integer and a near miss is many orders larger.
    real(dp), parameter :: INT_TOL = 1.0e-12_dp

contains

    !> int_lo^hi e d(var), or a refusal naming what stopped it.
    !>
    !> `lo` and `hi` are expressions and must be closed and finite; the result
    !> keeps their exact form, so int_0^Pi Sin[x] comes back as 2 rather than
    !> as a double.
    subroutine definite_integral(a, e, var, lo, hi, f, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e, var, lo, hi
        type(expr_t),              intent(out)   :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(native_engine_t)     :: eng
        type(engine_result_t)     :: res
        type(expr_t)              :: antideriv, upper, lower, whole
        type(str_t),  allocatable :: names(:)
        character(:), allocatable :: reason
        real(dp) :: lo_value, hi_value, left, right
        integer  :: k
        logical  :: lo_numeric, hi_numeric, parametric, formal_interval
        logical  :: endpoint_ok

        ok = .false.
        why = ""
        f = num(a, 0)

        if (.not. is_valid(e) .or. .not. is_valid(var)) then
            why = "integrand or variable is not a valid expression"
            return
        end if
        if (.not. is_valid(lo) .or. .not. is_valid(hi)) then
            why = "an integration limit is not a valid expression"
            return
        end if
        if (var%kind() /= NK_SYM) then
            why = "the integration variable must be a symbol"
            return
        end if
        if (.not. associated(e%a, a) .or. .not. associated(var%a, a)) then
            why = "integrand and variable must live in the given arena"
            return
        end if
        if (.not. associated(lo%a, a) .or. .not. associated(hi%a, a)) then
            why = "the integration limits must live in the given arena"
            return
        end if

        ! Numeric limits support the interval continuity proof below. A finite
        ! symbolic limit is also safe when the integrand is entire in the
        ! integration variable: the result is then a formal endpoint
        ! substitution, not a claim about a hidden pole. Infinite and opaque
        ! limits remain refused.
        call number_of(lo, lo_value, lo_numeric)
        if (.not. lo_numeric) then
            call finite_symbolic_endpoint(lo, endpoint_ok)
            if (.not. endpoint_ok) then
                why = "the lower limit is not a finite number: an infinite "// &
                    "or opaque endpoint is a limit process, not an evaluation"
                return
            end if
        end if
        call number_of(hi, hi_value, hi_numeric)
        if (.not. hi_numeric) then
            call finite_symbolic_endpoint(hi, endpoint_ok)
            if (.not. endpoint_ok) then
                why = "the upper limit is not a finite number: an infinite "// &
                    "or opaque endpoint is a limit process, not an evaluation"
                return
            end if
        end if

        formal_interval = .not. lo_numeric .or. .not. hi_numeric
        if (.not. formal_interval) then
            left = min(lo_value, hi_value)
            right = max(lo_value, hi_value)
        end if

        ! A parameter in the integrand normally decides continuity and is not
        ! known here -- 1/(x - c) is continuous on [0, 1] for some c and not
        ! for others -- so it is refused. The exception is an integrand that
        ! is entire in the variable, built only from sums, products, whole
        ! powers and functions with no singularity anywhere on the real line:
        ! that one is continuous for *every* value of the parameter, so there
        ! is nothing left for the unknown to decide.
        parametric = .false.
        names = free_symbols_of(e)
        do k = 1, size(names)
            if (chars(names(k)) /= chars(var%name())) then
                if (.not. entire_in(e, var)) then
                    why = "the integrand contains the free parameter "// &
                          chars(names(k))//" outside an entire expression: "// &
                          "continuity on the interval cannot be decided"
                    return
                end if
                parametric = .true.
                exit
            end if
        end do

        eng = make_native_engine(a)

        if (formal_interval) then
            if (.not. entire_in(e, var)) then
                why = "a symbolic endpoint needs an integrand that is entire "// &
                    "in the integration variable"
                return
            end if
        else if (.not. parametric) then
            call check_continuous(a, e, var, left, right, ok, reason)
            if (.not. ok) then
                why = "the integrand is not provably continuous on the "// &
                      "interval: "//reason
                return
            end if
        end if

        antideriv = integrate(a, e, var, ok, reason)
        if (.not. ok) then
            why = reason
            return
        end if

        ! F' = f holds formally on both sides of a jump in F, so a continuous
        ! integrand does not by itself license F(b) - F(a). The antiderivative
        ! is put through the same interval test.
        if (formal_interval .or. parametric) then
            if (.not. entire_in(antideriv, var)) then
                ok = .false.
                why = "the antiderivative is not entire in the variable, so "// &
                    "the formal endpoint substitution cannot rule out a "// &
                    "jump inside the interval"
                return
            end if
        else
            call check_continuous(a, antideriv, var, left, right, ok, reason)
            if (.not. ok) then
                why = "the antiderivative is not provably continuous on the "// &
                      "interval: "//reason
                return
            end if
        end if

        upper = subs(antideriv, var, hi)
        lower = subs(antideriv, var, lo)
        whole = upper - lower

        res = eng%simplify(whole)
        if (res%ok) whole = res%value

        f = whole
        ok = .true.
    end subroutine definite_integral

    !> A conservative syntax check for a finite formal endpoint. Unknown
    !> functions and directed infinities are not accepted, but arithmetic in
    !> symbols such as h*(1-r/R) is a legitimate parameterised endpoint for an
    !> entire integrand.
    recursive subroutine finite_symbolic_endpoint(e, ok)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: ok
        integer :: k
        character(:), allocatable :: name

        ok = .false.
        if (.not. is_valid(e)) return

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL, NK_SYM)
            ok = .true.
        case (NK_CONST)
            name = chars(e%name())
            if (name == "Infinity") return
            if (name == "ComplexInfinity") return
            if (name == "Indeterminate") return
            if (name == "DirectedInfinity") return
            ok = .true.
        case (NK_ADD, NK_MUL, NK_POW)
            ok = .true.
            do k = 1, e%nargs()
                call finite_symbolic_endpoint(e%arg(k), ok)
                if (.not. ok) return
            end do
        end select
    end subroutine finite_symbolic_endpoint

    !> Is `e` finite and continuous in `v` at every real point, whatever its
    !> other symbols stand for?
    !>
    !> This is what lets an integrand with a parameter through at all. It is
    !> a syntactic sufficient condition, not a characterisation: it admits
    !> sums, products, whole powers and the functions with no real
    !> singularity, and it admits any subexpression free of `v` regardless of
    !> shape, because such a subexpression is a constant of the integration
    !> and cannot introduce a discontinuity in `v`. Everything with a domain
    !> restriction -- a reciprocal, a log, a sqrt, a tan -- is excluded, since
    !> where those fail depends on the very parameter that is unknown.
    recursive function entire_in(e, v) result(yes)
        type(expr_t), intent(in) :: e, v
        logical                  :: yes
        real(dp) :: expo_value
        integer  :: k
        logical  :: numeric

        yes = .false.

        if (.not. depends_on(e, v)) then
            yes = .true.
            return
        end if

        select case (e%kind())

        case (NK_SYM)
            yes = .true.

        case (NK_ADD, NK_MUL)
            do k = 1, e%nargs()
                if (.not. entire_in(e%arg(k), v)) return
            end do
            yes = .true.

        case (NK_POW)
            if (depends_on(e%arg(2), v)) return
            call number_of(e%arg(2), expo_value, numeric)
            if (.not. numeric) return
            if (.not. is_integer_value(expo_value)) return
            if (expo_value < 0.0_dp) return
            yes = entire_in(e%arg(1), v)

        case (NK_FUNC)
            if (e%nargs() /= 1) return
            select case (chars(e%name()))
            case ("sin", "cos", "exp", "sinh", "cosh", "tanh", "atan", &
                  "asinh", "erf", "erfc", "abs")
                yes = entire_in(e%arg(1), v)
            end select

        end select
    end function entire_in

    ! ------------------------------------------------------- continuity --

    !> Is `e` provably continuous and real-valued at every point of [xa, xb]?
    recursive subroutine check_continuous(a, e, v, xa, xb, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e, v
        real(dp),                  intent(in)    :: xa, xb
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: base, expo, arg
        real(dp) :: value, expo_value, lowest, highest
        character(:), allocatable :: reason, name
        integer :: k
        logical :: numeric

        ok = .false.
        why = ""

        ! A subexpression free of the variable is a constant on the interval.
        ! It still has to be a finite real one: log(-1) is a constant too.
        if (.not. depends_on(e, v)) then
            call number_of(e, value, numeric)
            if (.not. numeric) then
                why = "a constant subexpression has no finite real value"
                return
            end if
            ok = .true.
            return
        end if

        select case (e%kind())

        case (NK_SYM)
            ! The integration variable itself.
            ok = .true.

        case (NK_ADD, NK_MUL)
            do k = 1, e%nargs()
                call check_continuous(a, e%arg(k), v, xa, xb, ok, reason)
                if (.not. ok) then
                    why = reason
                    return
                end if
            end do
            ok = .true.

        case (NK_POW)
            base = e%arg(1)
            expo = e%arg(2)
            if (depends_on(expo, v)) then
                why = "an exponent depends on the integration variable"
                return
            end if
            call number_of(expo, expo_value, numeric)
            if (.not. numeric) then
                why = "an exponent has no numeric value"
                return
            end if
            call check_continuous(a, base, v, xa, xb, ok, reason)
            if (.not. ok) then
                why = reason
                return
            end if
            if (is_integer_value(expo_value)) then
                if (expo_value >= 0.0_dp) then
                    ok = .true.
                    return
                end if
                ! A negative integer power is a denominator: it must not
                ! vanish anywhere on the closed interval.
                call require_nonzero(a, base, v, xa, xb, ok, reason)
                if (.not. ok) why = "a denominator "//reason
                return
            end if
            ! A fractional power needs a nonnegative base to stay real, and a
            ! strictly positive one when it is also a denominator.
            if (expo_value > 0.0_dp) then
                call require_sign(a, base, v, xa, xb, .false., ok, reason)
                if (.not. ok) then
                    why = "the base of a fractional power "//reason
                end if
            else
                call require_sign(a, base, v, xa, xb, .true., ok, reason)
                if (.not. ok) then
                    why = "the base of a negative fractional power "//reason
                end if
            end if

        case (NK_FUNC)
            name = chars(e%name())
            if (e%nargs() /= 1) then
                why = "no continuity rule for "//name//" of "// &
                      itoa(e%nargs())//" arguments"
                return
            end if
            arg = e%arg(1)
            call check_continuous(a, arg, v, xa, xb, ok, reason)
            if (.not. ok) then
                why = reason
                return
            end if
            ! The argument being continuous makes `ok` true; the head still
            ! has to earn it, so the verdict is withdrawn before the table
            ! runs and only a matching case restores it.
            ok = .false.
            select case (name)
            case ("sin", "cos", "exp", "sinh", "cosh", "tanh", "atan", &
                  "asinh", "erf", "erfc", "abs")
                ok = .true.
            case ("log")
                call require_sign(a, arg, v, xa, xb, .true., ok, reason)
                if (.not. ok) why = "the argument of Log "//reason
            case ("sqrt")
                call require_sign(a, arg, v, xa, xb, .false., ok, reason)
                if (.not. ok) why = "the argument of Sqrt "//reason
            case ("asin", "acos")
                call bound_on(a, arg, v, xa, xb, lowest, highest, ok, reason)
                if (.not. ok) then
                    why = "the argument of "//name//" "//reason
                    return
                end if
                if (lowest < -1.0_dp .or. highest > 1.0_dp) then
                    ok = .false.
                    why = "the argument of "//name//" leaves [-1, 1] on "// &
                          "the interval"
                    return
                end if
                ok = .true.
            case ("atanh")
                call bound_on(a, arg, v, xa, xb, lowest, highest, ok, reason)
                if (.not. ok) then
                    why = "the argument of ArcTanh "//reason
                    return
                end if
                if (lowest <= -1.0_dp .or. highest >= 1.0_dp) then
                    ok = .false.
                    why = "the argument of ArcTanh reaches +-1 on the interval"
                    return
                end if
                ok = .true.
            case ("acosh")
                call bound_on(a, arg, v, xa, xb, lowest, highest, ok, reason)
                if (.not. ok) then
                    why = "the argument of ArcCosh "//reason
                    return
                end if
                if (lowest < 1.0_dp) then
                    ok = .false.
                    why = "the argument of ArcCosh drops below 1 on the "// &
                          "interval"
                    return
                end if
                ok = .true.
            case ("tan")
                call bound_on(a, arg, v, xa, xb, lowest, highest, ok, reason)
                if (.not. ok) then
                    why = "the argument of Tan "//reason
                    return
                end if
                if (crosses_half_pi(lowest, highest)) then
                    ok = .false.
                    why = "the argument of Tan reaches an odd multiple of "// &
                          "pi/2 on the interval, where Tan has a pole"
                    return
                end if
                ok = .true.
            case default
                why = "no continuity rule for "//name
            end select

        case default
            why = "no continuity rule for this expression shape"

        end select
    end subroutine check_continuous

    !> `g` must stay away from zero on [xa, xb], of either sign.
    recursive subroutine require_nonzero(a, g, v, xa, xb, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: g, v
        real(dp),                  intent(in)    :: xa, xb
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        real(dp) :: lowest, highest, band
        character(:), allocatable :: reason

        call bound_on(a, g, v, xa, xb, lowest, highest, ok, reason)
        if (.not. ok) then
            why = reason
            return
        end if
        band = SIGN_BAND*max(1.0_dp, max(abs(lowest), abs(highest)))
        if (lowest > band .or. highest < -band) then
            ok = .true.
            return
        end if
        ok = .false.
        why = "reaches zero on the interval, so the integrand has a pole "// &
              "there and the integral is improper"
    end subroutine require_nonzero

    !> `g` must be nonnegative on [xa, xb], or strictly positive when `strict`.
    recursive subroutine require_sign(a, g, v, xa, xb, strict, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: g, v
        real(dp),                  intent(in)    :: xa, xb
        logical,                   intent(in)    :: strict
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        real(dp) :: lowest, highest, band
        character(:), allocatable :: reason

        call bound_on(a, g, v, xa, xb, lowest, highest, ok, reason)
        if (.not. ok) then
            why = reason
            return
        end if
        band = SIGN_BAND*max(1.0_dp, max(abs(lowest), abs(highest)))
        if (strict) then
            if (lowest > band) then
                ok = .true.
                return
            end if
            ok = .false.
            why = "is not provably positive on the interval (its minimum "// &
                  "there is "//real_text(lowest)//")"
            return
        end if
        if (lowest >= -band) then
            ok = .true.
            return
        end if
        ok = .false.
        why = "goes negative on the interval (its minimum there is "// &
              real_text(lowest)//"), so the expression leaves the reals"
    end subroutine require_sign

    !> The exact range of `g` over [xa, xb], or a refusal.
    !>
    !> This is the one place where a proof is required rather than a sample.
    !> A differentiable g attains its extrema on a closed interval at an
    !> endpoint or at a zero of g', so evaluating g at the endpoints and at
    !> every zero of g' in range gives the true min and max -- provided the
    !> zero list is complete. `solve_polynomial` returns a verified list whose
    !> length equals the degree, which is exactly that completeness guarantee,
    !> and refuses when it cannot deliver it. Nothing else is accepted: a grid
    !> of samples can miss a narrow spike entirely and would turn "I did not
    !> see a pole" into "there is no pole".
    recursive subroutine bound_on(a, g, v, xa, xb, lowest, highest, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: g, v
        real(dp),                  intent(in)    :: xa, xb
        real(dp),                  intent(out)   :: lowest, highest
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(native_engine_t)     :: eng
        type(engine_result_t)     :: res
        type(expr_t)              :: derivative
        type(expr_t), allocatable :: roots(:)
        character(:), allocatable :: reason
        real(dp) :: point, value
        integer  :: k
        logical  :: fine

        lowest = 0.0_dp
        highest = 0.0_dp
        ok = .false.
        why = ""

        if (.not. depends_on(g, v)) then
            call number_of(g, value, fine)
            if (.not. fine) then
                why = "has no finite real value"
                return
            end if
            lowest = value
            highest = value
            ok = .true.
            return
        end if

        ! The extremum argument needs g differentiable on the whole interval,
        ! which is exactly what continuity of the subexpressions plus a
        ! polynomial derivative gives. Continuity is checked first so that a g
        ! with its own pole cannot be "bounded" by sampling around it.
        call check_continuous(a, g, v, xa, xb, ok, reason)
        if (.not. ok) then
            why = "is itself not continuous on the interval: "//reason
            return
        end if

        eng = make_native_engine(a)
        derivative = diff(g, v)
        res = eng%simplify(derivative)
        if (res%ok) derivative = res%value

        call solve_polynomial(a, derivative, v, roots, ok, reason)
        if (.not. ok) then
            ! No complete critical-point list, so the exact route is out.
            ! Interval arithmetic still gives a sound enclosure: every rule in
            ! it over-approximates, so the interval it returns contains the
            ! true range. It is weaker -- it forgets that the two x in x - x
            ! are the same x -- which costs proofs, never correctness, and a
            ! lost proof here is a refusal rather than a wrong integral.
            call interval_bound(g, v, xa, xb, lowest, highest, ok, reason)
            if (.not. ok) then
                why = "cannot be bounded on the interval: "//reason
            end if
            return
        end if

        call sample_at(g, v, xa, lowest, ok)
        if (.not. ok) then
            why = "has no real value at the lower endpoint"
            return
        end if
        highest = lowest
        call sample_at(g, v, xb, value, ok)
        if (.not. ok) then
            why = "has no real value at the upper endpoint"
            return
        end if
        lowest = min(lowest, value)
        highest = max(highest, value)

        do k = 1, size(roots)
            call real_point(roots(k), point, fine, ok)
            if (.not. ok) then
                why = "has a critical point whose realness cannot be decided"
                return
            end if
            if (.not. fine) cycle       ! a complex zero of g' is not a point
            if (point < xa .or. point > xb) cycle
            call sample_at(g, v, point, value, ok)
            if (.not. ok) then
                why = "has no real value at one of its critical points"
                return
            end if
            lowest = min(lowest, value)
            highest = max(highest, value)
        end do

        ok = .true.
    end subroutine bound_on

    !> A sound enclosure of the range of `g` over [xa, xb], by interval
    !> arithmetic on the expression tree.
    !>
    !> Every rule below returns an interval that *contains* the true range, so
    !> a returned lower bound above zero is a proof that g is positive there.
    !> The converse does not hold: the enclosure can be far too wide, and then
    !> the caller refuses. That asymmetry is the point -- over-approximation
    !> can only cost a refusal, while an under-approximation would license a
    !> wrong integral.
    recursive subroutine interval_bound(g, v, xa, xb, lowest, highest, ok, why)
        type(expr_t),              intent(in)  :: g, v
        real(dp),                  intent(in)  :: xa, xb
        real(dp),                  intent(out) :: lowest, highest
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        real(dp) :: bl, bh, cl, ch, q, corners(4)
        character(:), allocatable :: reason, name
        integer :: k
        logical :: fine

        lowest = 0.0_dp
        highest = 0.0_dp
        ok = .false.
        why = ""

        if (.not. depends_on(g, v)) then
            call number_of(g, bl, fine)
            if (.not. fine) then
                why = "a constant subexpression has no finite real value"
                return
            end if
            lowest = bl
            highest = bl
            ok = .true.
            return
        end if

        select case (g%kind())

        case (NK_SYM)
            lowest = xa
            highest = xb
            ok = .true.

        case (NK_ADD)
            lowest = 0.0_dp
            highest = 0.0_dp
            do k = 1, g%nargs()
                call interval_bound(g%arg(k), v, xa, xb, bl, bh, ok, reason)
                if (.not. ok) then
                    why = reason
                    return
                end if
                lowest = lowest + bl
                highest = highest + bh
            end do
            ok = .true.

        case (NK_MUL)
            lowest = 1.0_dp
            highest = 1.0_dp
            do k = 1, g%nargs()
                call interval_bound(g%arg(k), v, xa, xb, bl, bh, ok, reason)
                if (.not. ok) then
                    why = reason
                    return
                end if
                corners = [lowest*bl, lowest*bh, highest*bl, highest*bh]
                lowest = minval(corners)
                highest = maxval(corners)
            end do
            ok = .true.

        case (NK_POW)
            if (depends_on(g%arg(2), v)) then
                why = "an exponent depends on the integration variable"
                return
            end if
            call number_of(g%arg(2), q, fine)
            if (.not. fine) then
                why = "an exponent has no numeric value"
                return
            end if
            call interval_bound(g%arg(1), v, xa, xb, bl, bh, ok, reason)
            if (.not. ok) then
                why = reason
                return
            end if
            call power_interval(bl, bh, q, lowest, highest, ok, reason)
            if (.not. ok) why = reason

        case (NK_FUNC)
            if (g%nargs() /= 1) then
                why = "no interval rule for a function of "// &
                      itoa(g%nargs())//" arguments"
                return
            end if
            name = chars(g%name())
            ! sin and cos are bounded whatever their argument does, which is
            ! what lets a denominator such as 2 + Sin[f(x)] be settled without
            ! knowing anything about f.
            if (name == "sin" .or. name == "cos") then
                lowest = -1.0_dp
                highest = 1.0_dp
                ok = .true.
                return
            end if
            call interval_bound(g%arg(1), v, xa, xb, bl, bh, ok, reason)
            if (.not. ok) then
                why = reason
                return
            end if
            ok = .true.
            select case (name)
            case ("exp")
                lowest = exp(bl)
                highest = exp(bh)
            case ("sinh")
                lowest = sinh(bl)
                highest = sinh(bh)
            case ("tanh")
                lowest = tanh(bl)
                highest = tanh(bh)
            case ("atan")
                lowest = atan(bl)
                highest = atan(bh)
            case ("asinh")
                lowest = log(bl + sqrt(bl*bl + 1.0_dp))
                highest = log(bh + sqrt(bh*bh + 1.0_dp))
            case ("erf")
                lowest = erf(bl)
                highest = erf(bh)
            case ("erfc")
                lowest = erfc(bh)
                highest = erfc(bl)
            case ("log")
                if (bl <= 0.0_dp) then
                    ok = .false.
                    why = "a Log argument is not provably positive"
                    return
                end if
                lowest = log(bl)
                highest = log(bh)
            case ("sqrt")
                if (bl < 0.0_dp) then
                    ok = .false.
                    why = "a Sqrt argument is not provably nonnegative"
                    return
                end if
                lowest = sqrt(bl)
                highest = sqrt(bh)
            case ("cosh")
                cl = cosh(bl)
                ch = cosh(bh)
                highest = max(cl, ch)
                if (bl >= 0.0_dp) then
                    lowest = cl
                else if (bh <= 0.0_dp) then
                    lowest = ch
                else
                    lowest = 1.0_dp     ! the interval straddles the minimum
                end if
            case ("abs")
                highest = max(abs(bl), abs(bh))
                if (bl >= 0.0_dp) then
                    lowest = bl
                else if (bh <= 0.0_dp) then
                    lowest = -bh
                else
                    lowest = 0.0_dp
                end if
            case default
                ok = .false.
                why = "no interval rule for "//name
            end select

        case default
            why = "no interval rule for this expression shape"

        end select
    end subroutine interval_bound

    !> The range of u**q for u in [bl, bh].
    subroutine power_interval(bl, bh, q, lowest, highest, ok, why)
        real(dp),                  intent(in)  :: bl, bh, q
        real(dp),                  intent(out) :: lowest, highest
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        real(dp) :: el, eh
        integer  :: n

        lowest = 0.0_dp
        highest = 0.0_dp
        ok = .false.
        why = ""

        ! Away from u = 0 the map u -> u**q has derivative q*u**(q-1), which
        ! never vanishes, so it is monotone and the endpoints bracket it.
        if (bl > 0.0_dp .or. bh < 0.0_dp) then
            if (bl < 0.0_dp .and. .not. is_integer_value(q)) then
                why = "a fractional power of a negative base is not real"
                return
            end if
            el = signed_power(bl, q)
            eh = signed_power(bh, q)
            lowest = min(el, eh)
            highest = max(el, eh)
            ok = .true.
            return
        end if

        ! The interval contains zero, so a negative exponent is a pole.
        if (.not. is_integer_value(q)) then
            why = "a fractional power whose base is not provably nonnegative"
            return
        end if
        n = nint(q)
        if (n < 0) then
            why = "a negative power whose base is not provably away from zero"
            return
        end if
        el = signed_power(bl, q)
        eh = signed_power(bh, q)
        if (mod(n, 2) == 0) then
            lowest = 0.0_dp             ! an even power bottoms out at u = 0
            highest = max(el, eh)
        else
            lowest = min(el, eh)
            highest = max(el, eh)
        end if
        ok = .true.
    end subroutine power_interval

    function signed_power(u, q) result(w)
        real(dp), intent(in) :: u, q
        real(dp)             :: w

        if (is_integer_value(q)) then
            w = u**nint(q)
            return
        end if
        w = u**q
    end function signed_power

    ! ------------------------------------------------------------ helpers --

    !> Value of `g` with `v` bound to `t`.
    subroutine sample_at(g, v, t, value, ok)
        type(expr_t), intent(in)  :: g, v
        real(dp),     intent(in)  :: t
        real(dp),     intent(out) :: value
        logical,      intent(out) :: ok
        type(binding_t) :: b

        allocate (b%names(1))
        allocate (b%values(1))
        b%names(1) = v%name()
        b%values(1) = t
        b%n = 1

        value = eval_expr(g, b, ok)
        if (.not. ok) then
            value = 0.0_dp
            return
        end if
        if (value /= value) then
            ok = .false.
            value = 0.0_dp
            return
        end if
        if (abs(value) > huge(1.0_dp)) then
            ok = .false.
            value = 0.0_dp
        end if
    end subroutine sample_at

    !> Decide whether an exact root is real, and give its value when it is.
    !>
    !> `real_root` false means the root is complex and irrelevant to an
    !> interval of the real line. `ok` false means neither could be decided,
    !> which the caller must treat as a refusal rather than as absence.
    subroutine real_point(root, point, real_root, ok)
        type(expr_t), intent(in)  :: root
        real(dp),     intent(out) :: point
        logical,      intent(out) :: real_root
        logical,      intent(out) :: ok

        point = 0.0_dp
        real_root = .false.
        ok = .true.

        if (uses_imaginary_unit(root)) return

        call number_of(root, point, real_root)
        if (.not. real_root) then
            ok = .false.
            point = 0.0_dp
        end if
    end subroutine real_point

    !> Does the expression mention the imaginary unit anywhere?
    function uses_imaginary_unit(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        integer :: id

        yes = .false.
        do id = 1, e%a%size()
            if (e%a%kind_of(id) /= NK_CONST) cycle
            if (chars(e%a%name_of(id)) /= "i") cycle
            if (mentions(e%a, e%id, id)) then
                yes = .true.
                return
            end if
        end do
    end function uses_imaginary_unit

    recursive function mentions(a, root, target) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: root, target
        logical                   :: yes
        integer :: k

        yes = root == target
        if (yes) return
        do k = 1, a%nargs_of(root)
            yes = mentions(a, a%arg_of(root, k), target)
            if (yes) return
        end do
    end function mentions

    !> Does [lowest, highest] contain an odd multiple of pi/2?
    function crosses_half_pi(lowest, highest) result(yes)
        real(dp), intent(in) :: lowest, highest
        logical              :: yes
        real(dp) :: first, pole
        integer  :: k

        ! Poles sit at (k + 1/2)*pi. The smallest index that can land at or
        ! above `lowest` is found directly rather than by scanning from zero,
        ! so an interval far from the origin costs the same as one near it.
        first = floor(lowest/PI_VALUE - 0.5_dp)
        yes = .false.
        do k = 0, 2
            pole = (first + real(k, dp) + 0.5_dp)*PI_VALUE
            if (pole >= lowest .and. pole <= highest) then
                yes = .true.
                return
            end if
        end do
    end function crosses_half_pi

    function depends_on(e, v) result(yes)
        type(expr_t), intent(in) :: e, v
        logical                  :: yes
        type(str_t), allocatable :: names(:)
        integer :: k

        yes = .false.
        names = free_symbols_of(e)
        do k = 1, size(names)
            if (chars(names(k)) == chars(v%name())) then
                yes = .true.
                return
            end if
        end do
    end function depends_on

    !> The finite real value of a closed expression, or a refusal.
    subroutine number_of(e, value, ok)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(out) :: value
        logical,      intent(out) :: ok
        type(binding_t) :: empty
        type(str_t), allocatable :: names(:)

        value = 0.0_dp
        ok = .false.

        names = free_symbols_of(e)
        if (size(names) > 0) return

        allocate (empty%names(0))
        allocate (empty%values(0))
        empty%n = 0

        value = eval_expr(e, empty, ok)
        if (.not. ok) return
        if (value /= value) then
            ok = .false.
            value = 0.0_dp
            return
        end if
        if (abs(value) > huge(1.0_dp)) then
            ok = .false.
            value = 0.0_dp
        end if
    end subroutine number_of

    function is_integer_value(x) result(yes)
        real(dp), intent(in) :: x
        logical              :: yes

        yes = .false.
        if (abs(x) > 1.0e15_dp) return
        yes = abs(x - anint(x)) < INT_TOL
    end function is_integer_value

    function itoa(n) result(text)
        integer, intent(in) :: n
        character(:), allocatable :: text
        character(len=16) :: buf

        write (buf, '(i0)') n
        text = trim(buf)
    end function itoa

    function real_text(x) result(text)
        real(dp), intent(in) :: x
        character(:), allocatable :: text
        character(len=32) :: buf

        write (buf, '(f0.7)') x
        text = trim(adjustl(buf))
    end function real_text

end module fortsym_defint
