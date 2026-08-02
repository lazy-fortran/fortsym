program test_fortsym_integrate
    ! Integration checked against quadrature, not against itself.
    !
    ! The oracle is the fundamental theorem of calculus evaluated numerically:
    ! for an antiderivative F of f, F(b) - F(a) must equal the area under f on
    ! [a,b]. The area is computed here by composite Simpson quadrature over the
    ! integrand alone, using only the expression evaluator -- it never looks at
    ! F, at fortsym_diff, or at any part of the integrator. An antiderivative
    ! that is wrong by anything other than an additive constant fails it, and
    ! an additive constant is exactly what an indefinite integral is allowed to
    ! be wrong by.
    !
    ! Simpson on a smooth integrand converges like h**4, so with 400 panels on
    ! an interval of width one the quadrature error is far below the tolerance
    ! used here; the tolerance is therefore testing the answer, not the rule.
    !
    ! The refusal tests assert the other half of the contract: the cases with
    ! no implemented rule must come back as refusals with a reason, never as a
    ! plausible expression.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_FUNC, NK_RAT
    use fortsym_expr, only: expr_t, sym, num, rat, func, &
        operator(+), operator(-), operator(*), &
        operator(/), operator(**), &
        sin, cos, exp, log, sqrt, sinh, cosh, tan
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_integrate, only: integrate
    implicit none

    integer, parameter :: dp = real64

    ! Panels for the quadrature oracle. Even, as Simpson requires.
    integer, parameter :: PANELS = 400

    type(arena_t), target :: arena
    type(expr_t) :: x, y
    integer :: nfail = 0

    call arena%init()
    x = sym(arena, "x")
    y = sym(arena, "y")

    call test_power_rule()
    call test_reciprocal_is_log()
    call test_linearity()
    call test_elementary_functions()
    call test_linear_substitution()
    call test_atan_and_asin()
    call test_symbolic_constant_factor()
    call test_negative_log_arguments()
    call test_exponent_near_minus_one()
    call test_refusals()

    if (nfail /= 0) then
        print *, "test_fortsym_integrate: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_integrate: all checks passed"

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (cond) then
            print *, "  ok   ", label
        else
            print *, "  FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine ok

    !> Integrate, then confront the answer with quadrature on [lo,hi].
    subroutine check_against_quadrature(label, e, lo, hi)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e
        real(dp),     intent(in) :: lo, hi
        type(expr_t) :: f
        character(:), allocatable :: why
        real(dp) :: jump, area, fa, fb
        logical  :: good, ea, eb, area_ok

        f = integrate(arena, e, x, good, why)
        if (.not. good) then
            call ok(label//" (integrates)", .false.)
            print *, "       refused: ", why
            return
        end if

        fa = value_at(f, lo, ea)
        fb = value_at(f, hi, eb)
        if (.not. ea .or. .not. eb) then
            call ok(label//" (endpoints defined)", .false.)
            return
        end if
        jump = fb - fa

        area = simpson(e, lo, hi, area_ok)
        if (.not. area_ok) then
            call ok(label//" (quadrature defined)", .false.)
            return
        end if

        call ok(label, abs(jump - area) <= 1.0e-7_dp*max(1.0_dp, abs(area)))
        if (abs(jump - area) > 1.0e-7_dp*max(1.0_dp, abs(area))) then
            print *, "       F(b)-F(a) =", jump, "  quadrature =", area
        end if
    end subroutine check_against_quadrature

    !> Composite Simpson over the integrand. Independent of everything the
    !> integrator does; it only evaluates numbers.
    function simpson(e, lo, hi, good) result(area)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(in)  :: lo, hi
        logical,      intent(out) :: good
        real(dp) :: area
        real(dp) :: h, xk, fk, total
        integer  :: k
        logical  :: defined

        area = 0.0_dp
        good = .false.
        h = (hi - lo)/real(PANELS, dp)
        total = 0.0_dp

        do k = 0, PANELS
            xk = lo + real(k, dp)*h
            fk = value_at(e, xk, defined)
            if (.not. defined) return
            if (k == 0 .or. k == PANELS) then
                total = total + fk
            else if (modulo(k, 2) == 1) then
                total = total + 4.0_dp*fk
            else
                total = total + 2.0_dp*fk
            end if
        end do

        area = total*h/3.0_dp
        good = .true.
    end function simpson

    !> Evaluate an expression at x = point, with the spectator symbol y bound
    !> to a fixed value so that expressions carrying a symbolic constant are
    !> comparable on both sides.
    function value_at(e, point, defined) result(v)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(in)  :: point
        logical,      intent(out) :: defined
        real(dp) :: v
        type(binding_t) :: b

        allocate (b%names(2))
        allocate (b%values(2))
        b%n = 2
        b%names(1) = str("x")
        b%values(1) = point
        b%names(2) = str("y")
        b%values(2) = 2.5_dp

        v = eval_expr(e, b, defined)
        if (defined) then
            if (v /= v) defined = .false.
        end if
    end function value_at

    subroutine test_power_rule()
        call check_against_quadrature("int x dx", x, 0.3_dp, 1.3_dp)
        call check_against_quadrature("int x**2 dx", x**2, 0.3_dp, 1.3_dp)
        call check_against_quadrature("int x**5 dx", x**5, 0.2_dp, 1.1_dp)
        call check_against_quadrature("int x**(-3) dx", x**(-3), 0.5_dp, 1.5_dp)
        call check_against_quadrature("int sqrt(x) dx", sqrt(x), 0.2_dp, 1.4_dp)
        call check_against_quadrature("int x**(1/2) dx", &
            x**rat(arena, 1_int64, 2_int64), &
            0.2_dp, 1.4_dp)
    end subroutine test_power_rule

    !> The one exponent the power rule cannot take: n = -1 must produce a
    !> logarithm, and quadrature of 1/x is what says so.
    subroutine test_reciprocal_is_log()
        call check_against_quadrature("int 1/x dx", 1/x, 0.4_dp, 1.6_dp)
        call check_against_quadrature("int 1/(2*x+3) dx", 1/(2*x + 3), &
            0.1_dp, 1.1_dp)
    end subroutine test_reciprocal_is_log

    subroutine test_linearity()
        call check_against_quadrature("int (3*x**2 - 5*x + 7) dx", &
            3*x**2 - 5*x + 7, 0.2_dp, 1.2_dp)
        call check_against_quadrature("int (sin(x) + 1/x) dx", &
            sin(x) + 1/x, 0.4_dp, 1.4_dp)
        call check_against_quadrature("int 4*exp(x) dx", 4*exp(x), &
            0.0_dp, 1.0_dp)
        call check_against_quadrature("int exp(x)*exp(-x) dx", &
            exp(x)*exp(-x), 0.0_dp, 1.0_dp)
    end subroutine test_linearity

    subroutine test_elementary_functions()
        call check_against_quadrature("int sin(x) dx", sin(x), 0.2_dp, 1.2_dp)
        call check_against_quadrature("int cos(x) dx", cos(x), 0.2_dp, 1.2_dp)
        call check_against_quadrature("int tan(x) dx", tan(x), 0.1_dp, 1.0_dp)
        call check_against_quadrature("int exp(x) dx", exp(x), -0.5_dp, 0.9_dp)
        call check_against_quadrature("int sinh(x) dx", sinh(x), 0.1_dp, 1.1_dp)
        call check_against_quadrature("int cosh(x) dx", cosh(x), 0.1_dp, 1.1_dp)
        call check_against_quadrature("int log(x) dx", log(x), 0.3_dp, 1.9_dp)
    end subroutine test_elementary_functions

    !> The chain factor 1/s is easy to drop or invert, and quadrature notices
    !> either way.
    subroutine test_linear_substitution()
        call check_against_quadrature("int sin(3*x+1) dx", sin(3*x + 1), &
            0.1_dp, 1.1_dp)
        call check_against_quadrature("int exp(-2*x) dx", exp(-2*x), &
            0.0_dp, 1.0_dp)
        call check_against_quadrature("int cos(x/2) dx", &
            cos(x*rat(arena, 1_int64, 2_int64)), &
            0.2_dp, 1.4_dp)
        call check_against_quadrature("int (2*x+1)**4 dx", (2*x + 1)**4, &
            0.0_dp, 1.0_dp)
        call check_against_quadrature("int sqrt(4*x+1) dx", sqrt(4*x + 1), &
            0.0_dp, 1.0_dp)
        call check_against_quadrature("int 2**x dx", num(arena, 2)**x, &
            0.0_dp, 1.0_dp)
    end subroutine test_linear_substitution

    subroutine test_atan_and_asin()
        call check_against_quadrature("int 1/(1+x**2) dx", 1/(1 + x**2), &
            0.0_dp, 1.3_dp)
        call check_against_quadrature("int 1/(x**2+2*x+5) dx", &
            1/(x**2 + 2*x + 5), 0.0_dp, 1.5_dp)
        call check_against_quadrature("int 1/sqrt(1-x**2) dx", &
            1/sqrt(1 - x**2), 0.0_dp, 0.8_dp)
        call check_against_quadrature("int 1/sqrt(4-x**2) dx", &
            1/sqrt(4 - x**2), 0.0_dp, 1.5_dp)
    end subroutine test_atan_and_asin

    !> A symbol other than the integration variable is a constant. y is bound
    !> to 2.5 on both sides of the comparison, so a factor of y that went
    !> missing shows up as a factor-of-2.5 discrepancy.
    subroutine test_symbolic_constant_factor()
        call check_against_quadrature("int y*x**2 dx", y*x**2, 0.2_dp, 1.2_dp)
        call check_against_quadrature("int y*sin(x) dx", y*sin(x), &
            0.2_dp, 1.2_dp)
    end subroutine test_symbolic_constant_factor

    !> Regression: an antiderivative containing a logarithm must be real on
    !> the whole interval where the integrand is regular, so these intervals
    !> deliberately sit where the log argument is negative. Quadrature of the
    !> integrand is the oracle; it never looks at the returned expression.
    subroutine test_negative_log_arguments()
        call check_against_quadrature("int 1/(x-2) dx on [0,1]", 1/(x - 2), &
            0.0_dp, 1.0_dp)
        call check_against_quadrature("int 1/x dx on [-2,-1]", 1/x, &
            -2.0_dp, -1.0_dp)
        call check_against_quadrature("int tan(x) dx on [2,3]", tan(x), &
            2.0_dp, 3.0_dp)
    end subroutine test_negative_log_arguments

    !> An exponent one part in 10**17 away from -1 rounds to -1.0 in real64,
    !> but its antiderivative is a power, not a logarithm. The oracle here is
    !> the calculus fact that only the exponent exactly -1 gives a logarithm.
    subroutine test_exponent_near_minus_one()
        type(expr_t) :: e, f
        character(:), allocatable :: why
        logical :: good

        e = x**rat(arena, -100000000000000001_int64, 100000000000000000_int64)
        f = integrate(arena, e, x, good, why)
        call ok("int x**(-1-1e-17) dx integrates", good)
        if (.not. good) then
            print *, "       refused: ", why
            return
        end if
        ! Quadrature cannot referee this one: x**(-1e-17) evaluates to 1.0 in
        ! real64, so the true antiderivative -1e17*x**(-1e-17) cancels to zero
        ! numerically. The oracle is the power rule itself: for n /= -1 the
        ! antiderivative is a power with exponent n+1 = -1/10**17, and only
        ! n = -1 exactly gives a logarithm.
        call ok("int x**(-1-1e-17) dx is not a logarithm", &
            .not. has_head(f, "log"))
        call ok("int x**(-1-1e-17) dx carries exponent -1/10**17", &
            has_exact_rat(f, -1_int64, 100000000000000000_int64))
    end subroutine test_exponent_near_minus_one

    !> Does any node of `e` apply the named function?
    recursive function has_head(e, name) result(yes)
        type(expr_t), intent(in) :: e
        character(*), intent(in) :: name
        logical                  :: yes
        integer :: k

        yes = .false.
        if (e%kind() == NK_FUNC) then
            if (chars(e%name()) == name) then
                yes = .true.
                return
            end if
        end if
        do k = 1, e%nargs()
            if (has_head(e%arg(k), name)) then
                yes = .true.
                return
            end if
        end do
    end function has_head

    !> A refusal only counts if it names the actual obstruction: `needle` must
    !> occur in the reason, so a module that refused everything with a generic
    !> message would fail these.
    !> Does any node of `e` hold the exact rational p/q?
    recursive function has_exact_rat(e, p, q) result(yes)
        type(expr_t),   intent(in) :: e
        integer(int64), intent(in) :: p, q
        logical                    :: yes
        integer :: k

        yes = .false.
        if (e%kind() == NK_RAT) then
            if (e%int_value() == p) then
                if (e%den_value() == q) then
                    yes = .true.
                    return
                end if
            end if
        end if
        do k = 1, e%nargs()
            if (has_exact_rat(e%arg(k), p, q)) then
                yes = .true.
                return
            end if
        end do
    end function has_exact_rat

    subroutine refuses(label, e, needle)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e
        character(*), intent(in) :: needle
        type(expr_t) :: f
        character(:), allocatable :: why
        logical :: good

        f = integrate(arena, e, x, good, why)
        call ok(label//" is refused", .not. good)
        if (.not. good) then
            call ok(label//" says why ("//needle//")", index(why, needle) > 0)
            if (index(why, needle) == 0) print *, "       reason: ", why
        else
            print *, "       returned an answer instead of refusing"
        end if
    end subroutine refuses

    !> Everything with no implemented rule. These are not failures of the
    !> module; returning something for any of them would be.
    subroutine test_refusals()
        type(expr_t) :: n

        n = sym(arena, "n")

        ! Non-elementary: the error function is not in the rule set, and there
        ! is no substitution that reaches it from here.
        call refuses("int exp(x**2) dx", exp(x**2), "not linear")
        ! Needs integration by parts.
        call refuses("int x*sin(x) dx", x*sin(x), "parts")
        ! Needs a general substitution.
        call refuses("int sin(x**2) dx", sin(x**2), "not linear")
        ! Rational function with a real pole: partial fractions, not
        ! implemented, so refused rather than half-done.
        call refuses("int 1/(x**2-1) dx", 1/(x**2 - 1), "discriminant")
        ! Symbolic exponent: -1 and everything else have different shapes.
        call refuses("int x**n dx", x**n, "symbolic exponent")
        ! Symbolic slope: y could be zero, and the symbolic check could not
        ! tell.
        call refuses("int sin(y*x) dx", sin(y*x), "nonzero slope")
        ! No rule at all for this head.
        call refuses("int gamma(x) dx", func("gamma", [x]), "gamma")
    end subroutine test_refusals

end program test_fortsym_integrate
