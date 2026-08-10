module fortsym_integrate
    ! Symbolic indefinite integration, with every answer checked by
    ! differentiating it back.
    !
    ! Integration is the operation where a computer algebra system is most
    ! tempting to fake. Differentiation is mechanical, so a wrong derivative is
    ! a bug; integration is a search, so a wrong antiderivative looks exactly
    ! like a right one -- it is an expression of the correct shape, with the
    ! correct symbols in it, and only a check can tell the two apart. This
    ! module therefore has two halves that do not trust each other:
    !
    !   * a rule set that proposes a candidate antiderivative, and
    !   * a verifier that differentiates the candidate and demands that the
    !     result equal the integrand.
    !
    ! A candidate that fails the verifier is discarded and the input refused.
    ! Nothing is ever returned with a caveat, because a caveat on an integral
    ! is read as "probably right", which is the failure this module exists to
    ! avoid. The consequence is that the rule set may be sloppy in the safe
    ! direction: it can guess a form (see the quadratic denominators) and let
    ! the verifier throw the guess away.
    !
    ! What is implemented:
    !
    !   * constants in the variable, and the variable itself;
    !   * linearity: sums termwise, and constant factors pulled out;
    !   * the power rule over a linear base, including the exponent -1 case
    !     that gives a logarithm;
    !   * elementary antiderivatives with a *linear* argument f(s*v + b) --
    !     sin, cos, tan, exp, log, sqrt, sinh, cosh -- divided by the slope;
    !   * squares of sin and cos with a linear argument, via the double-angle
    !     identities;
    !   * an exponential with a constant base and a linear exponent;
    !   * 1/q and 1/sqrt(q) for a quadratic q with numeric coefficients, in the
    !     arctangent and arcsine cases respectively.
    !
    ! What is refused, and why:
    !
    !   * general rational functions. Hermite reduction (Bronstein, Symbolic
    !     Integration I, 2005, ch. 2) needs polynomial gcd, squarefree
    !     decomposition and a resultant-based logarithmic part; none of that
    !     exists here, and half of Hermite is worse than none of it, so a
    !     denominator that is not linear or a recognised quadratic is refused
    !     rather than approached with partial fractions that would silently
    !     drop the non-elementary part.
    !   * products of two non-exponential factors that both depend on the
    !     variable. There is no integration by parts and no substitution beyond
    !     the linear one.
    !   * a slope that is not provably nonzero. Dividing by a symbolic s is the
    !     one unsoundness the verifier cannot catch: F = f(s*v)/s differentiates
    !     back to the integrand *formally* even when s is zero, so the symbolic
    !     check would pass on an expression that is meaningless. Hence the slope
    !     must be a number the zero test can refute, not merely an unknown.
    !   * a symbolic exponent, since v**n has an antiderivative of two different
    !     shapes depending on whether n is -1, and there is no way to pick one
    !     without knowing.
    !   * definite integration. Not in scope: it needs limits, orientation and
    !     a pole analysis on the path, none of which this module has.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, num, rat, is_valid, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), operator(/=), &
        sin, cos, tan, asin, atan, sinh, cosh, exp, log, sqrt, abs
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_eval, only: binding_t, eval_expr, collect_free_symbols
    implicit none
    private

    public :: integrate

    integer, parameter :: dp = real64

    ! Sample points for the numeric fallback. They sit inside (0,1) so that the
    ! arcsine and square-root integrands are real there, and none of them is a
    ! special value of any elementary function -- an identity that happens to
    ! hold at 0, 1/2 or 1 would otherwise be mistaken for one that holds
    ! everywhere.
    integer,  parameter :: N_SAMPLES = 6
    real(dp), parameter :: SAMPLES(N_SAMPLES) = [ &
        0.1234567_dp, 0.2718281_dp, 0.4142135_dp, &
        0.5671432_dp, 0.7320508_dp, 0.8660254_dp]

    ! Samples where both sides are undefined carry no information; at least
    ! this many must survive before a numeric agreement counts as evidence.
    integer, parameter :: MIN_USABLE_SAMPLES = 4

    ! The derivative and the integrand are built by different code paths, so
    ! they are compared at working precision rather than bit for bit. The
    ! tolerance is relative to the local size of the integrand.
    real(dp), parameter :: SAMPLE_TOLERANCE = 1.0e-8_dp

contains

    !> The indefinite integral of `e` with respect to the symbol `var`, or a
    !> refusal naming what stopped it. The constant of integration is omitted,
    !> as usual; the caller adds it if the context needs one.
    function integrate(a, e, var, ok, why) result(f)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e, var
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f
        type(native_engine_t) :: eng
        type(expr_t) :: integrand, candidate
        type(engine_result_t) :: expanded
        character(:), allocatable :: reason
        logical :: found, good

        ok = .false.
        why = ""
        f = num(a, 0)

        if (.not. is_valid(e)) then
            why = "integrand is not a valid expression"
            return
        end if
        if (.not. is_valid(var)) then
            why = "integration variable is not a valid expression"
            return
        end if
        if (var%kind() /= NK_SYM) then
            why = "integration variable must be a symbol"
            return
        end if
        if (.not. associated(e%a, a) .or. .not. associated(var%a, a)) then
            why = "integrand and variable must live in the given arena"
            return
        end if

        eng = make_native_engine(a)

        ! Simplifying first is not cosmetic: the rules below dispatch on node
        ! kind, and an unsimplified 2*(3*x**1) is a shape none of them match
        ! even though 6*x is.
        integrand = simplified(eng, e)
        ! Distribute bounded polynomial products before dispatching rules. The
        ! verifier still checks the final candidate, while the expansion
        ! exposes terms such as r*(r**2 + c) to the existing power rules.
        expanded = eng%expand(integrand)
        if (expanded%ok) integrand = expanded%value
        integrand = combine_exponential_product(eng, integrand, var)

        candidate = antiderivative(eng, integrand, var, found, reason)
        if (.not. found) then
            why = reason
            return
        end if

        candidate = simplified(eng, candidate)
        call verify(eng, candidate, integrand, var, good, reason)
        if (.not. good) then
            ! The rules proposed something; it did not survive. Report the
            ! refusal, not the candidate.
            why = "no verified antiderivative: "//reason
            return
        end if

        f = candidate
        ok = .true.
    end function integrate

    ! ------------------------------------------------------------- rules --

    !> Propose an antiderivative. Nothing here is trusted; `verify` decides.
    recursive function antiderivative(eng, e, v, found, why) result(f)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, v
        logical,                   intent(out)   :: found
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f

        found = .false.
        why = ""
        f = num(e%a, 0)

        ! A factor free of the variable integrates to itself times the
        ! variable. This is also the base case that ends the product rule.
        if (.not. has_var(e, v)) then
            f = e*v
            found = .true.
            return
        end if

        if (e == v) then
            f = v*v*rat(e%a, 1_int64, 2_int64)
            found = .true.
            return
        end if

        select case (e%kind())
        case (NK_ADD)
            f = integrate_sum(eng, e, v, found, why)
        case (NK_MUL)
            f = integrate_product(eng, e, v, found, why)
        case (NK_POW)
            f = integrate_power(eng, e, v, found, why)
        case (NK_FUNC)
            f = integrate_function(eng, e, v, found, why)
        case default
            why = "no rule for this expression shape"
        end select
    end function antiderivative

    !> Linearity over a sum: every term must integrate, or the sum does not.
    !> Returning the terms that worked and dropping the rest would produce an
    !> expression that is wrong by exactly the terms that are missing.
    recursive function integrate_sum(eng, e, v, found, why) result(f)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, v
        logical,                   intent(out)   :: found
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f
        type(expr_t) :: piece
        character(:), allocatable :: reason
        logical :: term_ok
        integer :: k

        found = .false.
        why = ""
        f = num(e%a, 0)

        do k = 1, e%nargs()
            piece = antiderivative(eng, e%arg(k), v, term_ok, reason)
            if (.not. term_ok) then
                why = "term "//itoa(k)//" of the sum: "//reason
                return
            end if
            if (k == 1) then
                f = piece
            else
                f = f + piece
            end if
        end do

        found = .true.
    end function integrate_sum

    !> Constant multiples come out of the integral. Products of exponentials
    !> are combined into one exponential before applying the ordinary rule;
    !> other products of variable-dependent factors still need parts or a
    !> substitution and are refused rather than approximated.
    recursive function integrate_product(eng, e, v, found, why) result(f)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, v
        logical,                   intent(out)   :: found
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f
        type(expr_t) :: coefficient, active, inner, exponent
        character(:), allocatable :: reason
        logical :: all_exponential, inner_ok
        integer :: k, n_active

        found = .false.
        why = ""
        f = num(e%a, 0)

        coefficient = num(e%a, 1)
        active = num(e%a, 1)
        exponent = num(e%a, 0)
        all_exponential = .true.
        n_active = 0

        do k = 1, e%nargs()
            call collect_product_factor(e%arg(k), v, coefficient, exponent, &
                active, n_active, all_exponential)
        end do

        if (n_active > 1) then
            if (.not. all_exponential) then
                why = "product of "//itoa(n_active)// &
                    " factors that all depend on the variable: "// &
                    "no rule for parts or substitution"
                return
            end if
            active = exp(simplified(eng, exponent))
        end if

        inner = antiderivative(eng, active, v, inner_ok, reason)
        if (.not. inner_ok) then
            why = reason
            return
        end if

        f = coefficient*inner
        found = .true.
    end function integrate_product

    recursive subroutine collect_product_factor( &
            factor, v, coefficient, exponent, active, n_active, all_exponential)
        type(expr_t), intent(in) :: factor, v
        type(expr_t), intent(inout) :: coefficient, exponent, active
        integer, intent(inout) :: n_active
        logical, intent(inout) :: all_exponential
        integer :: k

        if (factor%kind() == NK_MUL) then
            do k = 1, factor%nargs()
                call collect_product_factor( &
                    factor%arg(k), v, coefficient, exponent, active, &
                    n_active, all_exponential)
            end do
            return
        end if

        if (.not. has_var(factor, v)) then
            coefficient = coefficient*factor
            return
        end if

        n_active = n_active + 1
        if (factor%kind() == NK_FUNC) then
            if (chars(factor%name()) == "exp" .or. &
                chars(factor%name()) == "Exp") then
                if (factor%nargs() == 1) then
                    if (n_active == 1) then
                        exponent = factor%arg(1)
                        active = factor
                    else
                        if (factor%arg(1) == -exponent) then
                            exponent = num(factor%a, 0)
                        else
                            exponent = exponent + factor%arg(1)
                        end if
                    end if
                    return
                end if
            end if
        end if
        all_exponential = .false.
        active = factor
    end subroutine collect_product_factor

    function combine_exponential_product(eng, e, v) result(r)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t), intent(in) :: e, v
        type(expr_t) :: r
        type(expr_t) :: coefficient, active, exponent
        integer :: k, n_active
        logical :: all_exponential

        r = e
        if (e%kind() /= NK_MUL) return
        coefficient = num(e%a, 1)
        active = num(e%a, 1)
        exponent = num(e%a, 0)
        n_active = 0
        all_exponential = .true.
        do k = 1, e%nargs()
            call collect_product_factor(e%arg(k), v, coefficient, exponent, &
                active, n_active, all_exponential)
        end do
        if (n_active <= 1 .or. .not. all_exponential) return
        r = coefficient*exp(simplified(eng, exponent))
    end function combine_exponential_product

    !> Powers. Four separate rules meet here, and the exponent decides which.
    recursive function integrate_power(eng, e, v, found, why) result(f)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, v
        logical,                   intent(out)   :: found
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f
        type(expr_t) :: base, expo, slope, offset, angle
        real(dp) :: expo_value, base_value
        logical  :: linear, numeric_expo, numeric_base
        logical  :: minus_one, minus_half, quadratic_base

        found = .false.
        why = ""
        f = num(e%a, 0)

        base = e%arg(1)
        expo = e%arg(2)

        ! sqrt(u)**k is u**(k/2). Rewriting it is what lets 1/sqrt(1 - v**2)
        ! reach the quadratic rule: written with a division it arrives here as
        ! a sqrt raised to -1, whose base is a function application and matches
        ! nothing. The rewrite is an identity on the domain where sqrt(u) is
        ! real, which is the only domain the integral has anyway.
        if (is_numeric_power_of_sqrt(base, expo)) then
            f = antiderivative(eng, simplified(eng, &
                base%arg(1)**(expo*rat(e%a, 1_int64, 2_int64))), v, found, why)
            return
        end if

        ! sin(u)**2 and cos(u)**2 are elementary after the double-angle
        ! identity. Keep the same linear-argument and nonzero-slope proof as
        ! the ordinary function rules; otherwise a formal division by a zero
        ! slope would look verified while describing a different function.
        if (exponent_equals(expo, 2_int64, 1_int64)) then
            if (base%kind() == NK_FUNC) then
                if (base%nargs() == 1) then
                    if (chars(base%name()) == "sin" .or. &
                        chars(base%name()) == "cos") then
                        angle = base%arg(1)
                        call linear_in(eng, angle, v, slope, offset, linear)
                        if (linear) then
                            if (chars(base%name()) == "sin") then
                                f = v*rat(e%a, 1_int64, 2_int64) - &
                                    sin(num(e%a, 2)*angle)/ &
                                    (num(e%a, 4)*slope)
                            else
                                f = v*rat(e%a, 1_int64, 2_int64) + &
                                    sin(num(e%a, 2)*angle)/ &
                                    (num(e%a, 4)*slope)
                            end if
                            found = .true.
                            return
                        end if
                    end if
                end if
            end if
        end if

        ! b**(s*v + c): an exponential in disguise. Requires a numeric base
        ! that is positive and not one, so that log(base) is real and nonzero.
        if (.not. has_var(base, v)) then
            call linear_in(eng, expo, v, slope, offset, linear)
            if (.not. linear) then
                why = "constant base with a non-linear exponent"
                return
            end if
            call number_of(base, base_value, numeric_base)
            if (.not. numeric_base) then
                why = "exponential with a symbolic base: log(base) may be "// &
                    "zero or undefined"
                return
            end if
            if (base_value <= 0.0_dp .or. base_value == 1.0_dp) then
                why = "exponential base is not positive and different from one"
                return
            end if
            f = e/(slope*log(base))
            found = .true.
            return
        end if

        call number_of(expo, expo_value, numeric_expo)

        ! The branch between the logarithm and the power rule is decided on the
        ! exact exponent node, never on its double approximation: an exponent
        ! such as -100000000000000001/100000000000000000 rounds to -1.0 in
        ! real64 while its antiderivative is a power, not a logarithm.
        minus_one = exponent_equals(expo, -1_int64, 1_int64)
        minus_half = exponent_equals(expo, -1_int64, 2_int64)

        ! 1/q and 1/sqrt(q) for a quadratic q. Tried before the linear-base
        ! rule fails, because a quadratic base is not linear.
        quadratic_base = .false.
        if (minus_one .or. minus_half) then
            f = integrate_quadratic(eng, base, minus_one, v, found, &
                quadratic_base, why)
            if (found) return
        end if

        call linear_in(eng, base, v, slope, offset, linear)
        if (.not. linear) then
            ! Keep the quadratic rule's reason when it applies: it names the
            ! branch that is missing, which the generic message does not.
            if (.not. quadratic_base) then
                why = "power of a base that is neither linear nor a "// &
                    "quadratic this module recognises"
            end if
            return
        end if

        if (.not. numeric_expo) then
            why = "symbolic exponent: the antiderivative of u**n has one "// &
                "shape for n = -1 and another otherwise"
            return
        end if

        if (minus_one) then
            ! log|base| is numerically stable across the whole dynamic range:
            ! squaring first underflows for tiny bases and overflows for large
            ! ones even though log|base| itself is finite. The differentiator
            ! carries d abs(u) = u/abs(u) * du, so the verifier still checks the
            ! candidate on either side of the zero of a real linear base.
            f = log(abs(base))/slope
        else
            f = base**(expo + 1)/(slope*(expo + 1))
        end if
        found = .true.
    end function integrate_power

    !> q**(-1) and q**(-1/2) for a quadratic q = c2*v**2 + c1*v + c0 with
    !> numeric coefficients.
    !>
    !> Both formulas are written out with a symbolic square root of the
    !> discriminant, so no root has to come out rational. The sign conditions
    !> select the branch that is elementary in the reals: an arctangent needs a
    !> denominator with no real root, an arcsine needs the radicand to be a
    !> downward parabola with two real roots. Everything else -- a positive
    !> discriminant under 1/q, which is a partial-fraction logarithm, and the
    !> degenerate double root -- is refused, because doing the logarithmic case
    !> properly is the rational-function algorithm this module does not have.
    function integrate_quadratic(eng, q, reciprocal, v, found, recognised, &
            why) result(f)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: q, v
        logical,                   intent(in)    :: reciprocal
        logical,                   intent(out)   :: found
        logical,                   intent(out)   :: recognised
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f
        type(expr_t) :: c2, c1, c0, root, u
        real(dp) :: v2, v1, v0, disc
        logical  :: quadratic, n2, n1, n0

        found = .false.
        recognised = .false.
        why = ""
        f = num(q%a, 0)

        call quadratic_in(eng, q, v, c2, c1, c0, quadratic)
        if (.not. quadratic) then
            why = "base is not a quadratic in the variable"
            return
        end if

        call number_of(c2, v2, n2)
        call number_of(c1, v1, n1)
        call number_of(c0, v0, n0)
        if (.not. (n2 .and. n1)) then
            why = "quadratic with symbolic coefficients: the sign of the "// &
                "discriminant decides the branch and is unknown"
            return
        end if
        if (.not. n0) then
            why = "quadratic with symbolic coefficients: the sign of the "// &
                "discriminant decides the branch and is unknown"
            return
        end if
        if (v2 == 0.0_dp) then
            why = "base is linear, not quadratic"
            return
        end if

        recognised = .true.
        disc = v1*v1 - 4.0_dp*v2*v0

        if (reciprocal) then
            ! Integral dv/q = 2/sqrt(-D) * atan((2*c2*v + c1)/sqrt(-D)),
            ! valid when D < 0, where the denominator has no real zero.
            if (disc >= 0.0_dp) then
                why = "1/quadratic with a non-negative discriminant is a "// &
                    "partial-fraction logarithm, which needs the rational "// &
                    "algorithm this module does not implement"
                return
            end if
            root = sqrt(-(c1*c1 - 4*c2*c0))
            u = (2*c2*v + c1)/root
            f = 2*atan(u)/root
            found = .true.
            return
        end if

        ! Integral dv/sqrt(q) with c2 < 0 and D > 0:
        !   -asin((2*c2*v + c1)/sqrt(D))/sqrt(-c2).
        if (v2 >= 0.0_dp .or. disc <= 0.0_dp) then
            why = "1/sqrt(quadratic) outside the arcsine case (needs a "// &
                "negative leading coefficient and a positive discriminant)"
            return
        end if
        root = sqrt(c1*c1 - 4*c2*c0)
        u = (2*c2*v + c1)/root
        f = -asin(u)/sqrt(-c2)
        found = .true.
    end function integrate_quadratic

    !> Standard antiderivatives, with the argument allowed to be linear in the
    !> variable. The linear substitution u = s*v + b contributes the 1/s; any
    !> other argument is refused, since a general substitution would need to
    !> find the inner function's derivative among the other factors, and there
    !> are no other factors by the time this is reached.
    function integrate_function(eng, e, v, found, why) result(f)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: e, v
        logical,                   intent(out)   :: found
        character(:), allocatable, intent(out)   :: why
        type(expr_t)                             :: f
        type(expr_t) :: u, slope, offset, primitive
        character(:), allocatable :: name
        logical :: linear

        found = .false.
        why = ""
        f = num(e%a, 0)

        name = chars(e%name())

        if (e%nargs() /= 1) then
            why = "no rule for "//name//" with "//itoa(e%nargs())//" arguments"
            return
        end if

        u = e%arg(1)
        call linear_in(eng, u, v, slope, offset, linear)
        if (.not. linear) then
            why = name//" of an argument that is not linear in the "// &
                "variable, with a provably nonzero slope"
            return
        end if

        select case (name)
        case ("sin");  primitive = -cos(u)
        case ("cos");  primitive = sin(u)
            ! -log|cos u|, written with the square so that it stays real where
            ! cos u < 0 and tan u is perfectly regular.
        case ("tan");  primitive = -log(cos(u)*cos(u))*rat(e%a, 1_int64, 2_int64)
        case ("exp");  primitive = exp(u)
        case ("sinh"); primitive = cosh(u)
        case ("cosh"); primitive = sinh(u)
        case ("log");  primitive = u*log(u) - u
        case ("sqrt"); primitive = 2*u*sqrt(u)*rat(e%a, 1_int64, 3_int64)
        case default
            why = "no antiderivative rule for "//name
            return
        end select

        f = primitive/slope
        found = .true.
    end function integrate_function

    ! ---------------------------------------------------------- verifier --

    !> Differentiate the candidate and demand the integrand back.
    !>
    !> The symbolic test comes first because it is a proof: if the difference
    !> simplifies to zero, the identity holds everywhere the expressions are
    !> defined. The native simplifier is not complete, though, so an unknown
    !> verdict is not evidence of a wrong answer -- it is no evidence at all,
    !> and the numeric sampling below is what turns it into evidence. Sampling
    !> at several unrelated points cannot prove an identity, but a wrong
    !> antiderivative disagrees at almost every point, so surviving six of them
    !> is a strong check and, crucially, an independent one: the sampler
    !> evaluates numbers and knows nothing about the rules that built the
    !> candidate.
    subroutine verify(eng, candidate, integrand, v, good, why)
        type(native_engine_t),     intent(inout) :: eng
        type(expr_t),              intent(in)    :: candidate, integrand, v
        logical,                   intent(out)   :: good
        character(:), allocatable, intent(out)   :: why
        type(engine_result_t) :: verdict
        type(expr_t) :: derivative, residual

        good = .false.
        why = ""

        derivative = simplified(eng, diff(candidate, v))
        residual = simplified(eng, derivative - integrand)

        verdict = eng%zero_test(residual)
        if (verdict%ok) then
            if (verdict%verdict == VERDICT_TRUE) then
                good = .true.
                return
            end if
            ! A VERDICT_FALSE is a proof that the residual is a nonzero
            ! number, so the candidate is wrong. Sampling cannot overrule a
            ! proof, and a residual smaller than the sample tolerance would
            ! otherwise let it.
            if (verdict%verdict == VERDICT_FALSE) then
                why = "the derivative of the candidate does not reproduce "// &
                    "the integrand (the symbolic zero test proved the "// &
                    "residual is nonzero)"
                return
            end if
        end if

        call sample_check(derivative, integrand, v, good, why)
        if (.not. good) then
            why = "the derivative of the candidate does not reproduce the "// &
                "integrand ("//why//")"
        end if
    end subroutine verify

    !> Compare the two expressions numerically at several points. Any symbol
    !> other than the integration variable is bound to a fixed unremarkable
    !> value, so the comparison is of two functions of one variable.
    subroutine sample_check(derivative, integrand, v, good, why)
        type(expr_t),              intent(in)  :: derivative, integrand, v
        logical,                   intent(out) :: good
        character(:), allocatable, intent(out) :: why
        type(binding_t) :: b
        type(str_t), allocatable :: names(:)
        type(expr_t) :: combined
        real(dp) :: left, right, scale
        integer  :: k, slot, usable
        logical  :: left_ok, right_ok

        good = .false.
        why = ""

        ! Take the free symbols of both sides at once, so a symbol that occurs
        ! only in the candidate still gets a value.
        combined = derivative + integrand
        call collect_free_symbols(combined, names)

        allocate (b%names(size(names)))
        allocate (b%values(size(names)))
        b%n = size(names)
        slot = 0
        do k = 1, size(names)
            b%names(k) = names(k)
            if (chars(names(k)) == chars(v%name())) then
                b%values(k) = 0.0_dp
            else
                slot = slot + 1
                b%values(k) = spectator_value(slot)
            end if
        end do

        usable = 0
        do k = 1, N_SAMPLES
            call set_variable(b, chars(v%name()), SAMPLES(k))
            left = eval_expr(derivative, b, left_ok)
            right = eval_expr(integrand, b, right_ok)
            if (.not. left_ok .or. .not. right_ok) cycle
            if (left /= left .or. right /= right) cycle
            usable = usable + 1
            scale = max(1.0_dp, abs(right))
            if (abs(left - right) > SAMPLE_TOLERANCE*scale) then
                why = "they differ at v = "//real_text(SAMPLES(k))
                return
            end if
        end do

        if (usable < MIN_USABLE_SAMPLES) then
            why = "the symbolic zero test was inconclusive and too few "// &
                "sample points are defined to decide numerically"
            return
        end if

        good = .true.
    end subroutine sample_check

    !> Values for symbols other than the integration variable. They are away
    !> from 0 and 1 and from each other, so a candidate that is wrong by a
    !> factor of such a symbol shows up rather than cancelling.
    pure function spectator_value(slot) result(x)
        integer, intent(in) :: slot
        real(dp)            :: x
        x = 1.3_dp + 0.37_dp*real(modulo(slot, 7), dp)
    end function spectator_value

    subroutine set_variable(b, name, value)
        type(binding_t), intent(inout) :: b
        character(*),    intent(in)    :: name
        real(dp),        intent(in)    :: value
        integer :: k

        do k = 1, b%n
            if (chars(b%names(k)) == name) b%values(k) = value
        end do
    end subroutine set_variable

    ! ----------------------------------------------------------- helpers --

    function simplified(eng, e) result(s)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t),          intent(in)    :: e
        type(expr_t)                         :: s
        type(engine_result_t) :: r

        s = e
        r = eng%simplify(e)
        if (r%ok) s = r%value
    end function simplified

    !> Does the variable occur anywhere in the expression? Structural, not
    !> semantic: an occurrence that cancels still counts, which only ever makes
    !> a rule decline to fire.
    recursive function has_var(e, v) result(yes)
        type(expr_t), intent(in) :: e, v
        logical                  :: yes
        integer :: k

        yes = .false.
        if (.not. is_valid(e)) return
        if (e == v) then
            yes = .true.
            return
        end if
        do k = 1, e%nargs()
            if (has_var(e%arg(k), v)) then
                yes = .true.
                return
            end if
        end do
    end function has_var

    !> Write `e` as slope*v + offset, with both parts free of v and the slope
    !> provably nonzero.
    !>
    !> The decomposition is read off the derivative rather than pattern
    !> matched, so it sees through whatever shape the simplifier chose. The
    !> offset check is what makes this a proof of linearity: if e were
    !> quadratic, e - e'*v would still contain v and the test fails.
    !>
    !> The slope must be refuted by the zero test, not merely unknown to it.
    !> An unknown slope is the one case where the symbolic verification is
    !> worthless -- f(s*v)/s differentiates back to f(s*v) whatever s is,
    !> including zero, where the original integral is a constant instead.
    subroutine linear_in(eng, e, v, slope, offset, linear)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t),          intent(in)    :: e, v
        type(expr_t),          intent(out)   :: slope, offset
        logical,               intent(out)   :: linear
        type(engine_result_t) :: verdict

        linear = .false.
        slope = num(e%a, 0)
        offset = num(e%a, 0)

        slope = simplified(eng, diff(e, v))
        if (has_var(slope, v)) return

        offset = simplified(eng, e - slope*v)
        if (has_var(offset, v)) return

        verdict = eng%zero_test(slope)
        if (.not. verdict%ok) return
        if (verdict%verdict /= VERDICT_FALSE) return

        linear = .true.
    end subroutine linear_in

    !> Write `e` as c2*v**2 + c1*v + c0 with all coefficients free of v.
    subroutine quadratic_in(eng, e, v, c2, c1, c0, quadratic)
        type(native_engine_t), intent(inout) :: eng
        type(expr_t),          intent(in)    :: e, v
        type(expr_t),          intent(out)   :: c2, c1, c0
        logical,               intent(out)   :: quadratic
        type(expr_t) :: first

        quadratic = .false.
        c2 = num(e%a, 0)
        c1 = num(e%a, 0)
        c0 = num(e%a, 0)

        first = simplified(eng, diff(e, v))
        c2 = simplified(eng, diff(first, v)*rat(e%a, 1_int64, 2_int64))
        if (has_var(c2, v)) return

        c1 = simplified(eng, first - 2*c2*v)
        if (has_var(c1, v)) return

        c0 = simplified(eng, e - c2*v*v - c1*v)
        if (has_var(c0, v)) return

        quadratic = .true.
    end subroutine quadratic_in

    !> The numeric value of a closed expression. An expression with a free
    !> symbol has no value, and reporting one -- by treating the symbol as zero
    !> -- would let a sign test below run on a number nobody supplied.
    subroutine number_of(e, value, ok)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(out) :: value
        logical,      intent(out) :: ok
        type(binding_t) :: empty
        type(str_t), allocatable :: names(:)

        value = 0.0_dp
        ok = .false.

        call collect_free_symbols(e, names)
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

    !> Is this a sqrt application raised to a numeric power? Written as nested
    !> tests rather than a chain of .and., because neither operator short
    !> circuits and name() is only meaningful once the kind is known.
    function is_numeric_power_of_sqrt(base, expo) result(yes)
        type(expr_t), intent(in) :: base, expo
        logical                  :: yes
        real(dp) :: value
        logical  :: numeric

        yes = .false.
        if (base%kind() /= NK_FUNC) return
        if (base%nargs() /= 1) return
        if (chars(base%name()) /= "sqrt") return
        call number_of(expo, value, numeric)
        yes = numeric
    end function is_numeric_power_of_sqrt

    !> Is `e` exactly the rational p/q? Decided on the exact node, never on a
    !> double: an exponent one part in 10**17 away from -1 rounds to -1.0 in
    !> real64 and would otherwise be integrated as a logarithm.
    function exponent_equals(e, p, q) result(yes)
        type(expr_t),   intent(in) :: e
        integer(int64), intent(in) :: p, q
        logical                    :: yes
        integer(int64) :: n, d

        yes = .false.
        select case (e%kind())
        case (NK_INT)
            n = e%int_value()
            d = 1_int64
        case (NK_RAT)
            n = e%int_value()
            d = e%den_value()
        case (NK_REAL)
            ! -1 and -1/2 are exact in binary, so an exact comparison is the
            ! right test here and no rounding slack is granted.
            yes = e%real_value() == real(p, dp)/real(q, dp)
            return
        case default
            return
        end select
        if (d == 0_int64) return
        ! Keep the cross multiplication inside int64. A magnitude this large
        ! cannot be the normalised form of -1 or -1/2 anyway, so refusing to
        ! decide it is the same answer as deciding it.
        if (abs(n) > huge(1_int64)/4_int64) return
        if (abs(d) > huge(1_int64)/4_int64) return
        yes = n*q == p*d
    end function exponent_equals

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

end module fortsym_integrate
