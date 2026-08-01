program test_fortsym_limits
    ! Limits checked by walking towards the point, not by reading the answer.
    !
    ! The oracle is numeric and independent of everything the module does. For a
    ! finite limit L at a point p, the expression is evaluated at p +/- h for a
    ! geometric sequence of h and the error |f - L| must both end up small and
    ! shrink along the sequence; a two-sided claim is checked from both sides,
    ! so an implementation that computed one side and reported it as two-sided
    ! would fail here. For an infinite limit the same sequence must show the
    ! magnitude growing past a threshold with a constant sign, which is what
    ! distinguishes +infinity from -infinity and from an oscillation.
    !
    ! Nothing below compares against a string or an expression produced by the
    ! module: the symbolic answer is converted to a double and confronted with
    ! samples of the original expression.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: str_t, str
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, rat, func, &
                            sin, cos, exp, log, sqrt, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**)
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_numeric, only: numeric_value
    use fortsym_limits, only: limit_of, limit_value_t, limit_point_t, &
                              finite_point, plus_infinity, minus_infinity, &
                              LIMIT_FINITE, LIMIT_PLUS_INF, LIMIT_MINUS_INF, &
                              FROM_BELOW, TWO_SIDED, FROM_ABOVE
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target :: arena
    type(expr_t) :: x
    integer :: nfail = 0

    call arena%init()
    x = sym(arena, "x")

    call test_direct_substitution()
    call test_removable_singularity()
    call test_lhopital_once()
    call test_lhopital_twice()
    call test_rational_at_infinity()
    call test_divergence_at_infinity()
    call test_growth_ordering()
    call test_constant_expression()
    call test_pole_refused()
    call test_oscillation_refused()
    call test_lhopital_cap_refused()
    call test_boundary_refused()
    call test_bad_input_refused()

    if (nfail /= 0) then
        print *, "test_fortsym_limits: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_limits: all checks passed"

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

    ! ------------------------------------------------------------ oracles --

    !> Value of `e` at x = xv, or not defined.
    function sample(e, xv, good) result(v)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(in)  :: xv
        logical,      intent(out) :: good
        real(dp)                  :: v
        type(binding_t) :: b

        allocate (b%names(1))
        allocate (b%values(1))
        b%names(1) = str("x")
        b%values(1) = xv
        b%n = 1
        v = eval_expr(e, b, good)
    end function sample

    !> The claimed finite limit L at p is confirmed when, along h halving from
    !> a quarter, the error at p + side*h shrinks by at least a quarter every
    !> step and ends up small. Steady geometric decay is the part that matters:
    !> if L were off by some delta the error would stop falling and settle at
    !> delta, so a wrong constant cannot pass however loose the final bound is.
    !> The sequence stops well short of the point where cancellation noise in
    !> the sampled expression would swamp the true error.
    function converges(e, p, side, L) result(good)
        type(expr_t), intent(in) :: e
        real(dp),     intent(in) :: p, side, L
        logical                  :: good
        real(dp) :: h, v, err, prev, first_err
        logical  :: defined
        integer  :: k

        good = .false.
        h = 0.25_dp
        prev = -1.0_dp
        first_err = -1.0_dp
        do k = 1, 8
            v = sample(e, p + side*h, defined)
            if (.not. defined) return
            err = abs(v - L)
            if (prev >= 0.0_dp) then
                if (err > 0.75_dp*prev + 1.0e-14_dp) return
            else
                first_err = err
            end if
            prev = err
            h = h*0.5_dp
        end do
        good = prev < 2.0e-3_dp .and. prev < 0.05_dp*first_err
    end function converges

    !> An infinite claim: magnitude must climb past a large threshold with one
    !> constant sign along the approach. Oscillation or a finite plateau fails.
    function diverges(e, points, want_positive) result(good)
        type(expr_t), intent(in) :: e
        real(dp),     intent(in) :: points(:)
        logical,      intent(in) :: want_positive
        logical                  :: good
        real(dp) :: v, prev
        logical  :: defined
        integer  :: k

        good = .false.
        prev = -1.0_dp
        do k = 1, size(points)
            v = sample(e, points(k), defined)
            if (.not. defined) return
            if (want_positive .and. v <= 0.0_dp) return
            if (.not. want_positive .and. v >= 0.0_dp) return
            if (abs(v) < prev) return
            prev = abs(v)
        end do
        good = prev > 1.0e5_dp
    end function diverges

    !> Confirm a finite two-sided limit against the oracle from both sides.
    subroutine expect_finite(label, e, p, r, good, why)
        character(*),              intent(in) :: label
        type(expr_t),              intent(in) :: e
        real(dp),                  intent(in) :: p
        type(limit_value_t),       intent(in) :: r
        logical,                   intent(in) :: good
        character(:), allocatable, intent(in) :: why
        real(dp) :: L
        logical  :: vgood
        character(:), allocatable :: inner

        call ok(label//": answered", good)
        if (.not. good) then
            print *, "       refusal: ", why
            return
        end if
        call ok(label//": finite kind", r%kind == LIMIT_FINITE)
        call numeric_value(r%value, L, vgood, inner)
        call ok(label//": answer is a number", vgood)
        if (.not. vgood) return
        call ok(label//": approach from below", converges(e, p, -1.0_dp, L))
        call ok(label//": approach from above", converges(e, p, 1.0_dp, L))
    end subroutine expect_finite

    ! ------------------------------------------------------------- cases --

    !> (x^2 + 3)/(x + 1) at 2 is continuous there; the oracle pins the value.
    subroutine test_direct_substitution()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        e = (x**num(arena, 2) + num(arena, 3))/(x + num(arena, 1))
        r = limit_of(arena, e, x, finite_point(num(arena, 2)), TWO_SIDED, &
                     good, why)
        call expect_finite("(x^2+3)/(x+1) at 2", e, 2.0_dp, r, good, why)
    end subroutine test_direct_substitution

    !> (x^2 - 1)/(x - 1) at 1: the hole is removable and the value is 2, which
    !> the oracle confirms without the module naming it.
    subroutine test_removable_singularity()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        e = (x**num(arena, 2) - num(arena, 1))/(x - num(arena, 1))
        r = limit_of(arena, e, x, finite_point(num(arena, 1)), TWO_SIDED, &
                     good, why)
        call expect_finite("(x^2-1)/(x-1) at 1", e, 1.0_dp, r, good, why)
    end subroutine test_removable_singularity

    subroutine test_lhopital_once()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        e = sin(x)/x
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call expect_finite("sin(x)/x at 0", e, 0.0_dp, r, good, why)

        e = (exp(x) - num(arena, 1))/x
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call expect_finite("(exp(x)-1)/x at 0", e, 0.0_dp, r, good, why)
    end subroutine test_lhopital_once

    !> (1 - cos x)/x^2 needs the rule twice. The oracle knows nothing of that.
    subroutine test_lhopital_twice()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        e = (num(arena, 1) - cos(x))/x**num(arena, 2)
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call expect_finite("(1-cos x)/x^2 at 0", e, 0.0_dp, r, good, why)
    end subroutine test_lhopital_twice

    !> Equal degrees give the coefficient ratio; a smaller numerator degree
    !> gives zero. Both are checked by sampling far out, which is where the
    !> claim actually lives.
    subroutine test_rational_at_infinity()
        type(expr_t) :: e
        type(limit_value_t) :: r
        real(dp) :: L, v
        logical :: good, vgood, defined
        character(:), allocatable :: why, inner

        e = (num(arena, 3)*x**num(arena, 2) + num(arena, 2))/ &
            (x**num(arena, 2) - num(arena, 5))
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("(3x^2+2)/(x^2-5) answered", good)
        if (good) then
            call numeric_value(r%value, L, vgood, inner)
            call ok("ratio is finite", r%kind == LIMIT_FINITE .and. vgood)
            v = sample(e, 1.0e6_dp, defined)
            call ok("ratio matches a far sample", &
                    defined .and. abs(v - L) < 1.0e-6_dp)
            v = sample(e, 1.0e8_dp, defined)
            call ok("ratio matches a farther sample", &
                    defined .and. abs(v - L) < 1.0e-9_dp)
        else
            print *, "       refusal: ", why
        end if

        e = (x + num(arena, 1))/(x**num(arena, 2) + num(arena, 1))
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("(x+1)/(x^2+1) answered", good)
        if (good) then
            call numeric_value(r%value, L, vgood, inner)
            call ok("decaying ratio is zero", vgood .and. L == 0.0_dp)
            v = sample(e, 1.0e7_dp, defined)
            call ok("decaying ratio sampled near zero", &
                    defined .and. abs(v) < 1.0e-6_dp)
        end if
    end subroutine test_rational_at_infinity

    !> (x^3+1)/(x^2+1) diverges: to +infinity one way and -infinity the other,
    !> and the sign is what the parity rule has to get right.
    subroutine test_divergence_at_infinity()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why
        real(dp) :: up(7), down(7)
        integer :: k

        do k = 1, 7
            up(k) = 10.0_dp**k
            down(k) = -10.0_dp**k
        end do

        e = (x**num(arena, 3) + num(arena, 1))/(x**num(arena, 2) + num(arena, 1))
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("cubic over quadratic at +inf answered", good)
        if (good) then
            call ok("claim is +infinity", r%kind == LIMIT_PLUS_INF)
            call ok("samples diverge upwards", diverges(e, up, .true.))
        else
            print *, "       refusal: ", why
        end if

        r = limit_of(arena, e, x, minus_infinity(), TWO_SIDED, good, why)
        call ok("cubic over quadratic at -inf answered", good)
        if (good) then
            call ok("claim is -infinity", r%kind == LIMIT_MINUS_INF)
            call ok("samples diverge downwards", diverges(e, down, .false.))
        else
            print *, "       refusal: ", why
        end if
    end subroutine test_divergence_at_infinity

    !> The growth ordering: the exponential beats the power both ways, and the
    !> power beats the logarithm. Sampling at 10^k is the check.
    subroutine test_growth_ordering()
        type(expr_t) :: e
        type(limit_value_t) :: r
        real(dp) :: v, up(7)
        logical :: good, defined
        character(:), allocatable :: why
        integer :: k

        do k = 1, 7
            up(k) = 10.0_dp**k
        end do

        ! x^2 exp(-x) -> 0
        e = x**num(arena, 2)*exp(-x)
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("x^2 exp(-x) answered", good)
        if (good) then
            call ok("x^2 exp(-x) is zero", r%kind == LIMIT_FINITE)
            v = sample(e, 100.0_dp, defined)
            call ok("x^2 exp(-x) sampled near zero", &
                    defined .and. abs(v) < 1.0e-30_dp)
        else
            print *, "       refusal: ", why
        end if

        ! exp(x)/x^3 -> +infinity
        e = exp(x)/x**num(arena, 3)
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("exp(x)/x^3 answered", good)
        if (good) then
            call ok("exp(x)/x^3 is +infinity", r%kind == LIMIT_PLUS_INF)
            call ok("exp(x)/x^3 samples diverge", &
                    diverges(e, [10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp], .true.))
        else
            print *, "       refusal: ", why
        end if

        ! log(x)/x -> 0
        e = log(x)/x
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("log(x)/x answered", good)
        if (good) then
            call ok("log(x)/x is zero", r%kind == LIMIT_FINITE)
            v = sample(e, 1.0e8_dp, defined)
            call ok("log(x)/x sampled near zero", &
                    defined .and. abs(v) < 1.0e-6_dp)
        else
            print *, "       refusal: ", why
        end if

        ! x/log(x) -> +infinity
        e = x/log(x)
        r = limit_of(arena, e, x, plus_infinity(), TWO_SIDED, good, why)
        call ok("x/log(x) answered", good)
        if (good) then
            call ok("x/log(x) is +infinity", r%kind == LIMIT_PLUS_INF)
            call ok("x/log(x) samples diverge", diverges(e, up, .true.))
        else
            print *, "       refusal: ", why
        end if
    end subroutine test_growth_ordering

    !> An expression free of the variable is its own limit; sampling anywhere
    !> gives the same number.
    subroutine test_constant_expression()
        type(expr_t) :: e
        type(limit_value_t) :: r
        real(dp) :: L, v
        logical :: good, vgood, defined
        character(:), allocatable :: why, inner

        e = sin(num(arena, 2)) + num(arena, 1)
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call ok("constant expression answered", good)
        if (.not. good) return
        call numeric_value(r%value, L, vgood, inner)
        v = sample(e, 3.0_dp, defined)
        call ok("constant limit equals the constant", &
                vgood .and. defined .and. abs(v - L) < 1.0e-14_dp)
    end subroutine test_constant_expression

    !> 1/x at 0 has one-sided limits of opposite sign. The only acceptable
    !> outcome is a refusal; an infinity here would be a two-sided claim that
    !> the samples below contradict.
    subroutine test_pole_refused()
        type(expr_t) :: e
        type(limit_value_t) :: r
        real(dp) :: below, above
        logical :: good, d1, d2
        character(:), allocatable :: why

        e = num(arena, 1)/x
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call ok("1/x at 0 is refused", .not. good)
        call ok("1/x refusal explains itself", len(why) > 0)

        ! The oracle for the refusal: the two sides really do disagree.
        below = sample(e, -1.0e-6_dp, d1)
        above = sample(e, 1.0e-6_dp, d2)
        call ok("1/x sides have opposite signs", &
                d1 .and. d2 .and. below < 0.0_dp .and. above > 0.0_dp)

        r = limit_of(arena, e, x, finite_point(num(arena, 0)), FROM_ABOVE, &
                     good, why)
        call ok("1/x from above is refused too", .not. good)
    end subroutine test_pole_refused

    !> sin(1/x) at 0 has no limit at all. Refusal is the only right answer, and
    !> the samples show why: the value keeps swinging across the whole range.
    subroutine test_oscillation_refused()
        type(expr_t) :: e
        type(limit_value_t) :: r
        real(dp) :: v, lo, hi
        logical :: good, defined
        character(:), allocatable :: why
        integer :: k

        e = sin(num(arena, 1)/x)
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call ok("sin(1/x) at 0 is refused", .not. good)
        call ok("oscillation refusal explains itself", len(why) > 0)

        lo = 1.0_dp
        hi = -1.0_dp
        do k = 1, 400
            v = sample(e, 1.0e-3_dp/real(k, dp), defined)
            if (.not. defined) cycle
            lo = min(lo, v)
            hi = max(hi, v)
        end do
        call ok("sin(1/x) really oscillates near 0", hi - lo > 1.5_dp)
    end subroutine test_oscillation_refused

    !> x^20/x^20 is 0/0 whose derivatives stay 0/0 for twenty rounds, well past
    !> the cap. The cap must produce a refusal, not the last quotient tried.
    subroutine test_lhopital_cap_refused()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        e = x**num(arena, 20)/x**num(arena, 20)
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call ok("L'Hopital cap yields a refusal", .not. good)
        call ok("cap refusal explains itself", len(why) > 0)
    end subroutine test_lhopital_cap_refused

    !> sqrt(x) at 0 has a one-sided limit only, and the module proves two-sided
    !> statements, so it refuses. The point of the check is that it does not
    !> quietly report the from-above value as the two-sided limit.
    subroutine test_boundary_refused()
        type(expr_t) :: e
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        e = sqrt(x)
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), TWO_SIDED, &
                     good, why)
        call ok("sqrt(x) at 0 is refused", .not. good)

        e = log(x)
        r = limit_of(arena, e, x, finite_point(num(arena, 0)), FROM_ABOVE, &
                     good, why)
        call ok("log(x) at 0 is refused", .not. good)
    end subroutine test_boundary_refused

    subroutine test_bad_input_refused()
        type(expr_t) :: e, y
        type(limit_value_t) :: r
        logical :: good
        character(:), allocatable :: why

        y = sym(arena, "y")
        e = x*y
        r = limit_of(arena, e, x, finite_point(num(arena, 1)), TWO_SIDED, &
                     good, why)
        call ok("second free symbol is refused", .not. good)
        call ok("refusal names y", index(why, "y") > 0)

        e = x + num(arena, 1)
        r = limit_of(arena, e, x, finite_point(y), TWO_SIDED, good, why)
        call ok("symbolic point is refused", .not. good)

        r = limit_of(arena, e, x, plus_infinity(), FROM_ABOVE, good, why)
        call ok("approaching +infinity from above is refused", .not. good)

        r = limit_of(arena, e, x, finite_point(num(arena, 1)), 7, good, why)
        call ok("unknown direction is refused", .not. good)
    end subroutine test_bad_input_refused

end program test_fortsym_limits
