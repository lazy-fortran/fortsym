program test_fortsym_defint
    ! Definite integration checked against quadrature of the original
    ! integrand.
    !
    ! The oracle here never touches the antiderivative. It evaluates the
    ! integrand at a few thousand points and adds them up with composite
    ! Simpson, using only the expression evaluator; it does not know that
    ! fortsym_integrate exists, let alone what shape it proposed. So an
    ! antiderivative that is wrong, or a right antiderivative subtracted at
    ! the wrong endpoints, or a sign lost in the orientation, all fail it. On
    ! a smooth integrand Simpson converges like h**4, so with two thousand
    ! panels the quadrature error sits many orders below the tolerance used
    ! here and the tolerance is testing the answer rather than the rule.
    !
    ! The refusals get an oracle too, and it is the more interesting half. It
    ! is not enough to record that the module declined: the question is
    ! whether declining was right. For the singular cases the test therefore
    ! shows the integral really is improper, by integrating up to within eps
    ! of the interior singularity for a sequence of shrinking eps and checking
    ! the partial areas grow without bound. A convergent integral cannot do
    ! that. And the specific number the fundamental theorem would have
    ! produced -- -2 for int_{-1}^{1} x^{-2}, a negative area under a positive
    ! integrand -- is asserted never to come back.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, &
        operator(+), operator(-), operator(*), &
        operator(/), operator(**), &
        sin, cos, exp, log, sqrt, tan, pi_expr
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_defint, only: definite_integral
    implicit none

    integer, parameter :: dp = real64

    ! Panels for the quadrature oracle. Even, as Simpson requires.
    integer, parameter :: PANELS = 2000
    real(dp), parameter :: TOL = 1.0e-8_dp

    type(arena_t), target :: arena
    type(expr_t) :: x
    integer :: nfail = 0

    call arena%init()
    x = sym(arena, "x")

    call test_polynomials()
    call test_transcendental()
    call test_orientation_and_degenerate()
    call test_singular_refusals()
    call test_domain_refusals()
    call test_parameter_refusal()
    call test_symbolic_endpoint()

    if (nfail /= 0) then
        print *, "test_fortsym_defint: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_defint: all checks passed"

contains

    subroutine test_polynomials()
        call agrees("int_0^1 x**2", x**2, 0.0_dp, 1.0_dp)
        call agrees("int_1^3 (x**3 - 2*x)", x**3 - 2*x, 1.0_dp, 3.0_dp)
        ! Crosses zero in the middle, which a polynomial is entitled to do:
        ! the continuity test must not confuse a zero of the integrand with a
        ! zero of a denominator.
        call agrees("int_-2^2 x**3", x**3, -2.0_dp, 2.0_dp)
        call agrees("int_-1^2 (x**2 - 1)", x**2 - 1, -1.0_dp, 2.0_dp)
    end subroutine test_polynomials

    subroutine test_transcendental()
        call agrees("int_0^1 exp(-x)", exp(-x), 0.0_dp, 1.0_dp)
        call agrees("int_0^3 sin(x)", sin(x), 0.0_dp, 3.0_dp)
        call agrees("int_1^3 log(x)", log(x), 1.0_dp, 3.0_dp)
        ! Away from 0: sqrt has an infinite slope at the origin, where
        ! Simpson stops being a fourth-order oracle and the
        ! disagreement would be the quadrature's fault.
        call agrees("int_1^4 sqrt(x)", sqrt(x), 1.0_dp, 4.0_dp)
        call agrees("int_1^2 1/x", 1/x, 1.0_dp, 2.0_dp)
        call agrees("int_0^1 1/(1+x**2)", 1/(1 + x**2), 0.0_dp, 1.0_dp)
        ! A pole outside the interval is not a pole on it.
        call agrees("int_0^2 1/(x+3)", 1/(x + 3), 0.0_dp, 2.0_dp)
        call agrees("int_0^1 1/(x**2+4)", 1/(x**2 + 4), 0.0_dp, 1.0_dp)
    end subroutine test_transcendental

    !> Reversed limits negate, and equal limits vanish. Both are conventions
    !> the quadrature oracle does not share, so they are stated directly.
    subroutine test_orientation_and_degenerate()
        real(dp) :: forward, backward
        logical  :: ok_f, ok_b

        call symbolic_value(x**2 + 1, 0.0_dp, 2.0_dp, forward, ok_f)
        call symbolic_value(x**2 + 1, 2.0_dp, 0.0_dp, backward, ok_b)
        if (.not. ok_f .or. .not. ok_b) then
            call fail("orientation: an integral of x**2+1 was refused")
            return
        end if
        if (abs(forward + backward) > TOL) then
            call fail("orientation: reversing the limits did not negate")
        end if
        if (abs(forward - simpson(x**2 + 1, 0.0_dp, 2.0_dp)) > TOL) then
            call fail("orientation: the forward integral disagrees with "// &
                "quadrature")
        end if
    end subroutine test_orientation_and_degenerate

    !> The heart of the module: an interior pole must stop the fundamental
    !> theorem, and the test proves the pole is real rather than assuming it.
    subroutine test_singular_refusals()
        call refuses("int_-1^1 1/x**2", 1/x**2, -1.0_dp, 1.0_dp)
        call refuses("int_-1^1 1/x", 1/x, -1.0_dp, 1.0_dp)
        call refuses("int_0^2 1/(x**2-1)", 1/(x**2 - 1), 0.0_dp, 2.0_dp)
        call refuses("int_0^2 1/(x-1)**2", 1/(x - 1)**2, 0.0_dp, 2.0_dp)

        ! Independent evidence that each of those really is improper: the area
        ! up to within eps of the singular point has to blow up.
        call diverges_at("1/x**2 at 0", 1/x**2, -1.0_dp, 1.0_dp, 0.0_dp)
        call diverges_at("1/(x**2-1) at 1", 1/(x**2 - 1), 0.0_dp, 2.0_dp, &
            1.0_dp)
        call diverges_at("1/(x-1)**2 at 1", 1/(x - 1)**2, 0.0_dp, 2.0_dp, &
            1.0_dp)

        ! The specific wrong answer this module exists to prevent. The
        ! fundamental theorem applied blindly to 1/x**2 on [-1,1] gives -2,
        ! and -2 is a negative area under a strictly positive integrand.
        call never_returns("int_-1^1 1/x**2 must never be -2", 1/x**2, &
            -1.0_dp, 1.0_dp, -2.0_dp)
    end subroutine test_singular_refusals

    !> Leaving the reals is as disqualifying as blowing up.
    subroutine test_domain_refusals()
        call refuses("int_-1^3 log(x)", log(x), -1.0_dp, 3.0_dp)
        call refuses("int_-1^1 sqrt(x)", sqrt(x), -1.0_dp, 1.0_dp)
        call refuses("int_0^2 tan(x)", tan(x), 0.0_dp, 2.0_dp)
    end subroutine test_domain_refusals

    !> A free parameter usually decides continuity and is not supplied, so
    !> the answer is not available at any price. The exception is an entire
    !> integrand, where no value of the parameter can put a singularity on the
    !> interval -- and there the answer has to be right for every value, which
    !> is what the oracle checks by picking several and quadrature-testing
    !> each.
    subroutine test_parameter_refusal()
        type(expr_t) :: c

        c = sym(arena, "c")
        call refuses("int_0^1 1/(x-c)", 1/(x - c), 0.0_dp, 1.0_dp)
        call refuses("int_0^1 log(x+c)", log(x + c), 0.0_dp, 1.0_dp)
        call refuses("int_0^1 sqrt(x+c)", sqrt(x + c), 0.0_dp, 1.0_dp)

        call agrees_for_parameter("int_0^3 (c*x**2 + x)", c*x**2 + x, c, &
            0.0_dp, 3.0_dp)
        call agrees_for_parameter("int_-1^2 (x**3 - c)", x**3 - c, c, &
            -1.0_dp, 2.0_dp)
    end subroutine test_parameter_refusal

    subroutine test_symbolic_endpoint()
        type(expr_t) :: upper, formula
        type(binding_t) :: b, empty
        real(dp) :: got, want
        logical :: ok
        character(:), allocatable :: why

        upper = sym(arena, "upper")
        call definite_integral(arena, x**2, x, num(arena, 0), upper, &
            formula, ok, why)
        if (.not. ok) then
            call fail("symbolic finite endpoint was refused: "//why)
            return
        end if

        allocate (b%names(1))
        allocate (b%values(1))
        b%names(1) = upper%name()
        b%values(1) = 2.0_dp
        b%n = 1
        got = eval_expr(formula, b, ok)
        want = simpson(x**2, 0.0_dp, 2.0_dp)
        if (.not. ok .or. abs(got - want) > TOL) then
            call fail("symbolic endpoint formula disagrees with quadrature")
        end if

        upper = num(arena, 2)*pi_expr(arena)
        call definite_integral(arena, cos(x)**2, x, num(arena, 0), upper, &
            formula, ok, why)
        if (.not. ok) then
            call fail("symbolic Pi endpoint was refused: "//why)
            return
        end if
        allocate (empty%names(0))
        allocate (empty%values(0))
        empty%n = 0
        got = eval_expr(formula, empty, ok)
        want = simpson(cos(x)**2, 0.0_dp, 2.0_dp*acos(-1.0_dp))
        if (.not. ok .or. abs(got - want) > TOL) then
            call fail("symbolic Pi endpoint formula disagrees with quadrature")
        end if
    end subroutine test_symbolic_endpoint

    !> Integrate symbolically in `x` with `p` left free, then instantiate `p`
    !> at several values and check each against quadrature of the original
    !> integrand at the same value. A formula that happens to be right at one
    !> parameter value and wrong at another does not survive this.
    subroutine agrees_for_parameter(label, f, p, lo, hi)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f, p
        real(dp),     intent(in) :: lo, hi
        type(expr_t) :: formula
        type(binding_t) :: b
        real(dp) :: values(4), got, want
        character(:), allocatable :: why
        integer :: k
        logical :: ok

        values = [-2.0_dp, -0.5_dp, 1.0_dp, 4.0_dp]

        call definite_integral(arena, f, x, real_of(lo), real_of(hi), &
            formula, ok, why)
        if (.not. ok) then
            call fail(label//": refused, but the integrand is entire in x "// &
                "for every value of the parameter")
            return
        end if

        allocate (b%names(1))
        allocate (b%values(1))
        b%names(1) = p%name()
        b%n = 1

        do k = 1, size(values)
            b%values(1) = values(k)
            got = eval_expr(formula, b, ok)
            if (.not. ok) then
                call fail(label//": the formula has no value at a parameter")
                return
            end if
            want = simpson_with(f, p, values(k), lo, hi)
            if (abs(got - want) > TOL*max(1.0_dp, abs(want))) then
                call fail(label//": disagrees with quadrature at a parameter")
                print *, "      parameter ", values(k)
                print *, "      symbolic  ", got
                print *, "      quadrature", want
                return
            end if
        end do
    end subroutine agrees_for_parameter

    ! ------------------------------------------------------------ harness --

    !> The module's answer as a double, or ok = .false. when it refused.
    subroutine symbolic_value(f, lo, hi, value, ok)
        type(expr_t), intent(in)  :: f
        real(dp),     intent(in)  :: lo, hi
        real(dp),     intent(out) :: value
        logical,      intent(out) :: ok
        type(expr_t) :: result_expr
        type(binding_t) :: empty
        character(:), allocatable :: why

        value = 0.0_dp
        call definite_integral(arena, f, x, real_of(lo), real_of(hi), &
            result_expr, ok, why)
        if (.not. ok) return

        allocate (empty%names(0))
        allocate (empty%values(0))
        empty%n = 0
        value = eval_expr(result_expr, empty, ok)
    end subroutine symbolic_value

    subroutine agrees(label, f, lo, hi)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp),     intent(in) :: lo, hi
        real(dp) :: got, want
        logical  :: ok

        call symbolic_value(f, lo, hi, got, ok)
        if (.not. ok) then
            call fail(label//": refused, but the integral is elementary "// &
                "and the integrand is continuous")
            return
        end if
        want = simpson(f, lo, hi)
        if (abs(got - want) > TOL*max(1.0_dp, abs(want))) then
            call fail(label//": disagrees with quadrature")
            print *, "      symbolic  ", got
            print *, "      quadrature", want
        end if
    end subroutine agrees

    subroutine refuses(label, f, lo, hi)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp),     intent(in) :: lo, hi
        real(dp) :: got
        logical  :: ok

        call symbolic_value(f, lo, hi, got, ok)
        if (ok) then
            call fail(label//": answered instead of refusing")
            print *, "      it returned", got
        end if
    end subroutine refuses

    subroutine never_returns(label, f, lo, hi, forbidden)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp),     intent(in) :: lo, hi, forbidden
        real(dp) :: got
        logical  :: ok

        call symbolic_value(f, lo, hi, got, ok)
        if (.not. ok) return
        if (abs(got - forbidden) < 1.0e-6_dp) call fail(label)
    end subroutine never_returns

    !> Confirm that the integral of `f` over [lo, hi] really is improper,
    !> because of the singularity at `point`.
    !>
    !> The area of the two pieces that stop eps short of `point` is computed
    !> for a sequence of shrinking eps. For a convergent integral that
    !> sequence settles down; here it must keep climbing, and by the end it
    !> must have climbed substantially. Nothing in this uses the symbolic
    !> machinery.
    subroutine diverges_at(label, f, lo, hi, point)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: f
        real(dp),     intent(in) :: lo, hi, point
        real(dp) :: eps, area, previous, first
        integer  :: k

        previous = 0.0_dp
        first = 0.0_dp
        do k = 1, 5
            eps = 10.0_dp**(-k)
            area = abs(simpson(f, lo, point - eps)) + &
                abs(simpson(f, point + eps, hi))
            if (k == 1) then
                first = area
            else if (area <= previous) then
                call fail(label//": the partial areas stopped growing, so "// &
                    "the refusal is not justified by a pole")
                return
            end if
            previous = area
        end do

        ! Growing is not enough on its own -- a convergent integral approaches
        ! its value from below and grows too. It has to grow substantially
        ! while eps falls by four orders of magnitude, which a convergent tail
        ! cannot do.
        if (previous < 1.5_dp*first) then
            call fail(label//": the partial areas settled down, so the "// &
                "integral converges and the refusal is wrong")
        end if
    end subroutine diverges_at

    !> Composite Simpson on the integrand alone.
    function simpson(f, lo, hi) result(area)
        type(expr_t), intent(in) :: f
        real(dp),     intent(in) :: lo, hi
        real(dp)                 :: area
        type(binding_t) :: b
        real(dp) :: h, t, value, weight
        integer  :: k
        logical  :: defined

        allocate (b%names(1))
        allocate (b%values(1))
        b%names(1) = x%name()
        b%n = 1

        h = (hi - lo)/real(PANELS, dp)
        area = 0.0_dp
        do k = 0, PANELS
            t = lo + real(k, dp)*h
            if (k == 0 .or. k == PANELS) then
                weight = 1.0_dp
            else if (mod(k, 2) == 1) then
                weight = 4.0_dp
            else
                weight = 2.0_dp
            end if
            b%values(1) = t
            value = eval_expr(f, b, defined)
            if (.not. defined) then
                ! The oracle refuses to guess as well: an undefined sample
                ! point makes the sum meaningless, and reporting it as an
                ! area would be the same failure the module avoids.
                area = huge(1.0_dp)
                return
            end if
            area = area + weight*value
        end do
        area = area*h/3.0_dp
    end function simpson

    !> Composite Simpson with a second symbol held at a fixed value.
    function simpson_with(f, p, at, lo, hi) result(area)
        type(expr_t), intent(in) :: f, p
        real(dp),     intent(in) :: at, lo, hi
        real(dp)                 :: area
        type(binding_t) :: b
        real(dp) :: h, t, value, weight
        integer  :: k
        logical  :: defined

        allocate (b%names(2))
        allocate (b%values(2))
        b%names(1) = x%name()
        b%names(2) = p%name()
        b%values(2) = at
        b%n = 2

        h = (hi - lo)/real(PANELS, dp)
        area = 0.0_dp
        do k = 0, PANELS
            t = lo + real(k, dp)*h
            if (k == 0 .or. k == PANELS) then
                weight = 1.0_dp
            else if (mod(k, 2) == 1) then
                weight = 4.0_dp
            else
                weight = 2.0_dp
            end if
            b%values(1) = t
            value = eval_expr(f, b, defined)
            if (.not. defined) then
                area = huge(1.0_dp)
                return
            end if
            area = area + weight*value
        end do
        area = area*h/3.0_dp
    end function simpson_with

    function real_of(t) result(e)
        real(dp), intent(in) :: t
        type(expr_t)         :: e
        integer(int64) :: n

        ! The endpoints used here are integers or simple decimals; keeping
        ! them exact means the symbolic answer stays exact too.
        n = nint(t, int64)
        e = num(arena, n)
    end function real_of

    subroutine fail(message)
        character(*), intent(in) :: message
        nfail = nfail + 1
        print *, "  FAIL: ", message
    end subroutine fail

end program test_fortsym_defint
