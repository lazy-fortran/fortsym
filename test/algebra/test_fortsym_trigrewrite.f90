program test_fortsym_trigrewrite
    ! The rewrites checked against what they claim to be: identities.
    !
    ! Every accepted rewrite is compared to its input by numeric sampling at a
    ! fixed deterministic set of points. The evaluator used for that lives in
    ! this file and is written directly against the arena node kinds -- it
    ! shares no code with the module under test, so agreement at two dozen
    ! points is evidence about the mathematics rather than about the
    ! implementation agreeing with itself. Nothing here compares a printed
    ! form against a string, because the shape of an expanded expression is a
    ! choice while its value is not.
    !
    ! The evaluator is complex rather than real for one reason: Euler's
    ! formulae introduce the imaginary unit, and a real probe would have to
    ! refuse exactly the expressions those rewrites produce.
    !
    ! The sample points are a golden-ratio sequence, not a random one. They
    ! must be the same on every machine and in every run, or a failure cannot
    ! be reproduced.
    !
    ! For power_expand the interesting evidence is negative: at x = y = -1 the
    ! unsound rewrite of sqrt(x*y) has the wrong sign, and the test builds
    ! that wrong expression itself, confirms it really disagrees, and then
    ! confirms the module refused to produce it.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, sym, num, rat, func, i_expr, &
                            sin, cos, tan, exp, sqrt, sinh, cosh, tanh, &
                            besselj, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**), operator(==), &
                            operator(/=)
    use fortsym_assume, only: assumption_context_t, assume, positive
    use fortsym_trigrewrite, only: trig_expand, trig_reduce, trig_to_exp, &
                                   exp_to_trig, power_expand
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: NPOINT = 24
    real(dp), parameter :: TOL = 1.0e-9_dp

    type(arena_t), target :: arena
    type(expr_t) :: x, y, z
    integer :: nfail = 0

    call arena%init()
    x = sym(arena, "x")
    y = sym(arena, "y")
    z = sym(arena, "z")

    call test_evaluator_self_check()
    call test_trig_expand_sums()
    call test_trig_expand_multiples()
    call test_trig_expand_refuses_tan()
    call test_trig_expand_leaves_unknown_heads()
    call test_trig_reduce_products()
    call test_trig_reduce_powers()
    call test_euler_forward()
    call test_euler_backward()
    call test_euler_round_trip()
    call test_power_expand_integer()
    call test_power_expand_refuses_sqrt_product()
    call test_power_expand_refuses_nested()
    call test_power_expand_with_positivity()

    if (nfail /= 0) then
        print *, "test_fortsym_trigrewrite: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_trigrewrite: all checks passed"

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

    ! ------------------------------------------------- the sampling oracle --

    !> The k-th sample point. Three irrational multipliers give three
    !> uncorrelated coordinates and the same numbers on every run.
    subroutine sample_point(k, complex_probe, vx, vy, vz)
        integer, intent(in)  :: k
        logical, intent(in)  :: complex_probe
        complex(dp), intent(out) :: vx, vy, vz
        real(dp) :: fx, fy, fz, ax, ay, az

        fx = frac(real(k, dp)*0.6180339887498949_dp)
        fy = frac(real(k, dp)*0.4142135623730951_dp)
        fz = frac(real(k, dp)*0.3027756377319946_dp)

        ax = 3.0_dp*(2.0_dp*fx - 1.0_dp)
        ay = 2.5_dp*(2.0_dp*fy - 1.0_dp)
        az = 2.0_dp*(2.0_dp*fz - 1.0_dp)

        if (complex_probe) then
            vx = cmplx(ax, 0.4_dp*ay, dp)
            vy = cmplx(ay, 0.3_dp*az, dp)
            vz = cmplx(az, 0.2_dp*ax, dp)
        else
            vx = cmplx(ax, 0.0_dp, dp)
            vy = cmplx(ay, 0.0_dp, dp)
            vz = cmplx(az, 0.0_dp, dp)
        end if
    end subroutine sample_point

    pure function frac(v) result(f)
        real(dp), intent(in) :: v
        real(dp) :: f
        f = v - real(floor(v), dp)
    end function frac

    !> Two expressions agree at every sample point where both are defined.
    !> Points where either side is undefined -- a pole of tan, a logarithm of
    !> zero -- are skipped, and the count of usable points is reported so that
    !> "they agreed everywhere" cannot quietly mean "nowhere was usable".
    subroutine agrees(label, e1, e2, complex_probe, shift)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e1, e2
        logical,      intent(in) :: complex_probe
        real(dp),     intent(in) :: shift
        complex(dp) :: vx, vy, vz, a, b
        logical :: da, db
        integer :: k, used, bad
        real(dp) :: worst, err

        used = 0
        bad = 0
        worst = 0.0_dp
        do k = 1, NPOINT
            call sample_point(k, complex_probe, vx, vy, vz)
            vx = vx + shift
            vy = vy + shift
            vz = vz + shift
            a = ceval(e1, vx, vy, vz, da)
            b = ceval(e2, vx, vy, vz, db)
            if (.not. da) cycle
            if (.not. db) cycle
            used = used + 1
            err = abs(a - b)/(1.0_dp + abs(a) + abs(b))
            if (err > worst) worst = err
            if (err > TOL) bad = bad + 1
        end do

        call ok(label//" (enough usable points)", used >= 10)
        call ok(label, bad == 0 .and. used >= 10)
        if (bad /= 0) print *, "      worst relative error:", worst
    end subroutine agrees

    !> A complex evaluator over the arena, independent of the module under
    !> test. Undefined results are reported rather than signalled, so a
    !> sample point that lands on a pole costs a point and nothing else.
    recursive function ceval(e, vx, vy, vz, defined) result(v)
        type(expr_t), intent(in)  :: e
        complex(dp),  intent(in)  :: vx, vy, vz
        logical,      intent(out) :: defined
        complex(dp) :: v
        complex(dp) :: acc, b, p, arg
        character(:), allocatable :: nm
        logical :: sub
        integer :: k, n

        defined = .true.
        v = (0.0_dp, 0.0_dp)

        select case (e%kind())

        case (NK_INT)
            v = cmplx(real(e%int_value(), dp), 0.0_dp, dp)

        case (NK_RAT)
            v = cmplx(real(e%int_value(), dp)/real(e%den_value(), dp), &
                      0.0_dp, dp)

        case (NK_REAL)
            v = cmplx(e%real_value(), 0.0_dp, dp)

        case (NK_SYM)
            nm = chars(e%name())
            select case (nm)
            case ("x"); v = vx
            case ("y"); v = vy
            case ("z"); v = vz
            case default; defined = .false.
            end select

        case (NK_CONST)
            nm = chars(e%name())
            select case (nm)
            case ("pi")
                v = cmplx(3.141592653589793238462643_dp, 0.0_dp, dp)
            case ("e")
                v = cmplx(2.718281828459045235360287_dp, 0.0_dp, dp)
            case ("i")
                v = (0.0_dp, 1.0_dp)
            case default; defined = .false.
            end select

        case (NK_ADD)
            acc = (0.0_dp, 0.0_dp)
            do k = 1, e%nargs()
                acc = acc + ceval(e%arg(k), vx, vy, vz, sub)
                if (.not. sub) then
                    defined = .false.
                    return
                end if
            end do
            v = acc

        case (NK_MUL)
            acc = (1.0_dp, 0.0_dp)
            do k = 1, e%nargs()
                acc = acc*ceval(e%arg(k), vx, vy, vz, sub)
                if (.not. sub) then
                    defined = .false.
                    return
                end if
            end do
            v = acc

        case (NK_POW)
            b = ceval(e%arg(1), vx, vy, vz, sub)
            if (.not. sub) then
                defined = .false.
                return
            end if
            p = ceval(e%arg(2), vx, vy, vz, sub)
            if (.not. sub) then
                defined = .false.
                return
            end if
            v = cpower(b, p, defined)

        case (NK_FUNC)
            nm = chars(e%name())
            if (e%nargs() /= 1) then
                defined = .false.
                return
            end if
            arg = ceval(e%arg(1), vx, vy, vz, sub)
            if (.not. sub) then
                defined = .false.
                return
            end if
            select case (nm)
            case ("sin");  v = sin(arg)
            case ("cos");  v = cos(arg)
            case ("tan")
                if (abs(cos(arg)) < 1.0e-6_dp) then
                    defined = .false.
                else
                    v = sin(arg)/cos(arg)
                end if
            case ("sinh"); v = sinh(arg)
            case ("cosh"); v = cosh(arg)
            case ("tanh")
                if (abs(cosh(arg)) < 1.0e-6_dp) then
                    defined = .false.
                else
                    v = sinh(arg)/cosh(arg)
                end if
            case ("exp");  v = exp(arg)
            case ("log")
                if (abs(arg) < 1.0e-12_dp) then
                    defined = .false.
                else
                    v = log(arg)
                end if
            case ("sqrt"); v = sqrt(arg)
            case default;  defined = .false.
            end select

        case default
            defined = .false.
        end select

        if (defined) then
            if (v /= v) defined = .false.
            if (abs(v) > 1.0e150_dp) defined = .false.
        end if
    end function ceval

    !> Complex power. An integer exponent is done by repeated multiplication
    !> so that a negative base carries no branch cut; everything else uses the
    !> principal logarithm, which is the same convention the rewrites assume.
    function cpower(b, p, defined) result(v)
        complex(dp), intent(in)    :: b, p
        logical,     intent(inout) :: defined
        complex(dp) :: v
        integer :: n, k

        v = (0.0_dp, 0.0_dp)
        if (aimag(p) == 0.0_dp .and. abs(real(p, dp)) <= 64.0_dp) then
            if (real(p, dp) == real(nint(real(p, dp)), dp)) then
                n = nint(real(p, dp))
                if (abs(b) < 1.0e-14_dp .and. n <= 0) then
                    defined = .false.
                    return
                end if
                v = (1.0_dp, 0.0_dp)
                do k = 1, abs(n)
                    v = v*b
                end do
                if (n < 0) v = (1.0_dp, 0.0_dp)/v
                return
            end if
        end if

        if (abs(b) < 1.0e-14_dp) then
            defined = .false.
            return
        end if
        v = exp(p*log(b))
    end function cpower

    ! ------------------------------------------------ the evaluator itself --

    !> Before the evaluator is trusted as an oracle it is checked against
    !> values known without it: Euler's identity, a Pythagorean value, and an
    !> exact rational.
    subroutine test_evaluator_self_check()
        complex(dp) :: v
        logical :: good
        type(expr_t) :: e

        e = exp(i_expr(arena)*num(arena, 0))
        v = ceval(e, (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), &
                  good)
        call ok("oracle: exp(0) is 1", good .and. abs(v - 1.0_dp) < 1.0e-14_dp)

        e = sin(x)**num(arena, 2) + cos(x)**num(arena, 2)
        v = ceval(e, (1.3_dp, 0.0_dp), (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), &
                  good)
        call ok("oracle: sin^2+cos^2 is 1", &
                good .and. abs(v - 1.0_dp) < 1.0e-14_dp)

        e = rat(arena, 1_int64, 4_int64)
        v = ceval(e, (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), &
                  good)
        call ok("oracle: 1/4 is exact", good .and. v == (0.25_dp, 0.0_dp))

        e = exp(i_expr(arena))
        v = ceval(e, (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), &
                  good)
        call ok("oracle: exp(i) is cos1 + i sin1", good .and. &
                abs(v - cmplx(cos(1.0_dp), sin(1.0_dp), dp)) < 1.0e-14_dp)
    end subroutine test_evaluator_self_check

    ! ------------------------------------------------------- trig_expand --

    subroutine test_trig_expand_sums()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = sin(x + y)
        call trig_expand(e, r, good, why)
        call ok("sin(x+y) expands", good)
        call ok("sin(x+y) actually changed", r /= e)
        call agrees("sin(x+y) expansion is exact", e, r, .false., 0.0_dp)

        e = cos(x + y)
        call trig_expand(e, r, good, why)
        call ok("cos(x+y) expands", good)
        call ok("cos(x+y) actually changed", r /= e)
        call agrees("cos(x+y) expansion is exact", e, r, .false., 0.0_dp)

        e = sin(x - y)
        call trig_expand(e, r, good, why)
        call ok("sin(x-y) expands", good)
        call agrees("sin(x-y) expansion is exact", e, r, .false., 0.0_dp)

        e = cos(x + y + z)
        call trig_expand(e, r, good, why)
        call ok("cos(x+y+z) expands", good)
        call agrees("cos(x+y+z) expansion is exact", e, r, .false., 0.0_dp)

        ! The identity is complex, so it must hold off the real axis too.
        e = sin(x + y)
        call trig_expand(e, r, good, why)
        call agrees("sin(x+y) holds off the real axis", e, r, .true., 0.0_dp)
    end subroutine test_trig_expand_sums

    subroutine test_trig_expand_multiples()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why
        integer :: n

        do n = 2, 6
            e = sin(num(arena, n)*x)
            call trig_expand(e, r, good, why)
            call ok("sin(n*x) expands", good)
            call ok("sin(n*x) actually changed", r /= e)
            call agrees("sin(n*x) Chebyshev expansion is exact", e, r, &
                        .false., 0.0_dp)

            e = cos(num(arena, n)*x)
            call trig_expand(e, r, good, why)
            call ok("cos(n*x) expands", good)
            call agrees("cos(n*x) Chebyshev expansion is exact", e, r, &
                        .false., 0.0_dp)
        end do

        e = cos(num(arena, 3)*x + num(arena, 2)*y)
        call trig_expand(e, r, good, why)
        call ok("cos(3x+2y) expands", good)
        call agrees("cos(3x+2y) expansion is exact", e, r, .false., 0.0_dp)

        e = sin(num(arena, -2)*x)
        call trig_expand(e, r, good, why)
        call ok("sin(-2x) expands", good)
        call agrees("sin(-2x) expansion is exact", e, r, .false., 0.0_dp)

        ! Past the bound the answer would be enormous, so it is refused
        ! rather than attempted.
        e = cos(num(arena, 40)*x)
        call trig_expand(e, r, good, why)
        call ok("cos(40x) is refused as too large", .not. good)
        call ok("size refusal explains itself", len(why) > 0)
        call ok("refused expansion returns the input", r == e)
    end subroutine test_trig_expand_multiples

    subroutine test_trig_expand_refuses_tan()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = tan(x + y)
        call trig_expand(e, r, good, why)
        call ok("tan(x+y) is refused", .not. good)
        call ok("tan refusal names tan", index(why, "tan") > 0)

        e = tan(num(arena, 3)*x)
        call trig_expand(e, r, good, why)
        call ok("tan(3x) is refused", .not. good)

        ! A tangent of a bare symbol has nothing to expand, so there is no
        ! gap to report and the expression passes through.
        e = tan(x) + sin(x + y)
        call trig_expand(e, r, good, why)
        call ok("tan(x) alone is not refused", good)
        call agrees("tan(x)+sin(x+y) rewrite is exact", e, r, .false., 0.0_dp)
    end subroutine test_trig_expand_refuses_tan

    !> A head with no rule is copied, not refused, when copying it is correct.
    subroutine test_trig_expand_leaves_unknown_heads()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = besselj(0, x) + sin(x + y)
        call trig_expand(e, r, good, why)
        call ok("unknown head is traversed, not refused", good)
        call ok("the sine beside it was expanded", r /= e)
    end subroutine test_trig_expand_leaves_unknown_heads

    ! ------------------------------------------------------- trig_reduce --

    subroutine test_trig_reduce_products()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = sin(x)*sin(y)
        call trig_reduce(e, r, good, why)
        call ok("sin(x)sin(y) reduces", good)
        call ok("sin(x)sin(y) actually changed", r /= e)
        call agrees("sin(x)sin(y) linearisation is exact", e, r, .false., &
                    0.0_dp)

        e = cos(x)*cos(y)
        call trig_reduce(e, r, good, why)
        call agrees("cos(x)cos(y) linearisation is exact", e, r, .false., &
                    0.0_dp)

        e = sin(x)*cos(y)
        call trig_reduce(e, r, good, why)
        call agrees("sin(x)cos(y) linearisation is exact", e, r, .false., &
                    0.0_dp)

        e = sin(x)*cos(y)*cos(z)
        call trig_reduce(e, r, good, why)
        call ok("three-factor product reduces", good)
        call agrees("sin(x)cos(y)cos(z) linearisation is exact", e, r, &
                    .false., 0.0_dp)

        ! Non-trig factors ride along untouched.
        e = z*sin(x)*sin(y)
        call trig_reduce(e, r, good, why)
        call agrees("z sin(x) sin(y) linearisation is exact", e, r, .false., &
                    0.0_dp)

        e = sin(x)*sin(y)
        call trig_reduce(e, r, good, why)
        call agrees("linearisation holds off the real axis", e, r, .true., &
                    0.0_dp)
    end subroutine test_trig_reduce_products

    subroutine test_trig_reduce_powers()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = cos(x)**num(arena, 2)
        call trig_reduce(e, r, good, why)
        call ok("cos(x)**2 reduces", good)
        call ok("cos(x)**2 actually changed", r /= e)
        call agrees("cos(x)**2 linearisation is exact", e, r, .false., 0.0_dp)

        e = sin(x)**num(arena, 3)
        call trig_reduce(e, r, good, why)
        call ok("sin(x)**3 reduces", good)
        call agrees("sin(x)**3 linearisation is exact", e, r, .false., 0.0_dp)

        e = sin(x)**num(arena, 2)*cos(y)**num(arena, 2)
        call trig_reduce(e, r, good, why)
        call agrees("sin^2 cos^2 linearisation is exact", e, r, .false., &
                    0.0_dp)

        ! Too many factors to linearise within the bound.
        e = sin(x)**num(arena, 12)
        call trig_reduce(e, r, good, why)
        call ok("sin(x)**12 is refused as too large", .not. good)
        call ok("refused reduction returns the input", r == e)

        ! A negative power is not a repeated factor and is left alone.
        e = sin(x)**num(arena, -1)
        call trig_reduce(e, r, good, why)
        call ok("1/sin(x) is not refused", good)
        call agrees("1/sin(x) is unchanged in value", e, r, .false., 0.0_dp)
    end subroutine test_trig_reduce_powers

    ! -------------------------------------------------------- Euler pair --

    subroutine test_euler_forward()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = sin(x)
        call trig_to_exp(e, r, good, why)
        call ok("sin -> exp", good)
        call ok("sin -> exp actually changed", r /= e)
        call agrees("sin -> exp is exact", e, r, .true., 0.0_dp)

        e = cos(x)
        call trig_to_exp(e, r, good, why)
        call agrees("cos -> exp is exact", e, r, .true., 0.0_dp)

        e = tan(x)
        call trig_to_exp(e, r, good, why)
        call ok("tan -> exp", good)
        call agrees("tan -> exp is exact", e, r, .true., 0.0_dp)

        e = sinh(x) + cosh(y)*tanh(z)
        call trig_to_exp(e, r, good, why)
        call ok("hyperbolics -> exp", good)
        call agrees("hyperbolics -> exp is exact", e, r, .true., 0.0_dp)

        e = sin(x + y)*cos(num(arena, 2)*z)
        call trig_to_exp(e, r, good, why)
        call agrees("nested trig -> exp is exact", e, r, .true., 0.0_dp)
    end subroutine test_euler_forward

    subroutine test_euler_backward()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = exp(i_expr(arena)*x)
        call exp_to_trig(e, r, good, why)
        call ok("exp(i x) -> trig", good)
        call ok("exp(i x) actually changed", r /= e)
        call agrees("exp(i x) -> cos + i sin is exact", e, r, .true., 0.0_dp)

        e = exp(x)
        call exp_to_trig(e, r, good, why)
        call ok("exp(x) -> cosh + sinh", good)
        call agrees("exp(x) -> cosh + sinh is exact", e, r, .true., 0.0_dp)

        e = exp(i_expr(arena)*x + y)
        call exp_to_trig(e, r, good, why)
        call ok("exp(i x + y) -> trig", good)
        call agrees("mixed exponent rewrite is exact", e, r, .true., 0.0_dp)

        e = exp(i_expr(arena)*i_expr(arena)*x)
        call exp_to_trig(e, r, good, why)
        call ok("exp(i*i*x) is handled", good)
        call agrees("exp(i*i*x) rewrite is exact", e, r, .true., 0.0_dp)
    end subroutine test_euler_backward

    !> The two directions must compose to the identity in value. Not in
    !> shape -- nothing here collects terms -- which is exactly why the check
    !> is numeric.
    subroutine test_euler_round_trip()
        type(expr_t) :: e, mid, back
        logical :: good
        character(:), allocatable :: why

        e = sin(x)*cos(y)
        call trig_to_exp(e, mid, good, why)
        call ok("round trip: forward leg", good)
        call exp_to_trig(mid, back, good, why)
        call ok("round trip: backward leg", good)
        call agrees("trig -> exp -> trig preserves value", e, back, .true., &
                    0.0_dp)

        e = cos(x + y)
        call trig_to_exp(e, mid, good, why)
        call exp_to_trig(mid, back, good, why)
        call agrees("cos(x+y) survives the round trip", e, back, .true., &
                    0.0_dp)

        e = exp(i_expr(arena)*x)
        call exp_to_trig(e, mid, good, why)
        call ok("reverse round trip: first leg", good)
        call trig_to_exp(mid, back, good, why)
        call ok("reverse round trip: second leg", good)
        call agrees("exp -> trig -> exp preserves value", e, back, .true., &
                    0.0_dp)
    end subroutine test_euler_round_trip

    ! ------------------------------------------------------ power_expand --

    subroutine test_power_expand_integer()
        type(expr_t) :: e, r
        logical :: good
        character(:), allocatable :: why

        e = (x*y)**num(arena, 3)
        call power_expand(e, r, good, why)
        call ok("(x*y)**3 expands", good)
        call ok("(x*y)**3 actually changed", r /= e)
        ! The sample points straddle zero, so a sign error would show here.
        call agrees("(x*y)**3 split is exact", e, r, .false., 0.0_dp)

        e = (x*y)**num(arena, -2)
        call power_expand(e, r, good, why)
        call ok("(x*y)**(-2) expands", good)
        call agrees("(x*y)**(-2) split is exact", e, r, .false., 0.0_dp)

        e = (x**num(arena, 2))**num(arena, 3)
        call power_expand(e, r, good, why)
        call ok("(x**2)**3 collapses", good)
        call agrees("(x**2)**3 collapse is exact", e, r, .false., 0.0_dp)

        e = (x**y)**num(arena, 2)
        call power_expand(e, r, good, why)
        call ok("(x**y)**2 collapses", good)
        call agrees("(x**y)**2 collapse is exact", e, r, .true., 0.0_dp)
    end subroutine test_power_expand_integer

    !> The headline refusal. The test first builds the unsound rewrite by
    !> hand and confirms it really is wrong at x = y = -1, so that the refusal
    !> below is known to be protecting something rather than being cautious
    !> about nothing.
    subroutine test_power_expand_refuses_sqrt_product()
        type(expr_t) :: e, r, unsound
        complex(dp) :: a, b
        logical :: good, da, db
        character(:), allocatable :: why

        e = sqrt(x*y)
        unsound = sqrt(x)*sqrt(y)
        a = ceval(e, (-1.0_dp, 0.0_dp), (-1.0_dp, 0.0_dp), &
                  (0.0_dp, 0.0_dp), da)
        b = ceval(unsound, (-1.0_dp, 0.0_dp), (-1.0_dp, 0.0_dp), &
                  (0.0_dp, 0.0_dp), db)
        call ok("both forms evaluate at x = y = -1", da .and. db)
        call ok("sqrt(x y) is 1 at x = y = -1", abs(a - 1.0_dp) < 1.0e-12_dp)
        call ok("sqrt(x)sqrt(y) is -1 there", abs(b + 1.0_dp) < 1.0e-12_dp)
        call ok("the split is genuinely false", abs(a - b) > 1.0_dp)

        call power_expand(e, r, good, why)
        call ok("sqrt(x*y) is refused without assumptions", .not. good)
        call ok("sqrt refusal explains itself", index(why, "sqrt") > 0)
        call ok("refused power_expand returns the input", r == e)

        e = (x*y)**rat(arena, 1_int64, 2_int64)
        call power_expand(e, r, good, why)
        call ok("(x*y)**(1/2) is refused too", .not. good)

        e = (x*y)**z
        call power_expand(e, r, good, why)
        call ok("(x*y)**z with a symbolic exponent is refused", .not. good)
    end subroutine test_power_expand_refuses_sqrt_product

    subroutine test_power_expand_refuses_nested()
        type(expr_t) :: e, r, unsound
        complex(dp) :: a, b
        logical :: good, da, db
        character(:), allocatable :: why

        e = (x**num(arena, 2))**rat(arena, 1_int64, 2_int64)
        unsound = x**(num(arena, 2)*rat(arena, 1_int64, 2_int64))
        a = ceval(e, (-2.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), &
                  (0.0_dp, 0.0_dp), da)
        b = ceval(unsound, (-2.0_dp, 0.0_dp), (0.0_dp, 0.0_dp), &
                  (0.0_dp, 0.0_dp), db)
        call ok("both nested forms evaluate at x = -2", da .and. db)
        call ok("((-2)**2)**(1/2) is 2", abs(a - 2.0_dp) < 1.0e-12_dp)
        call ok("(-2)**(2*(1/2)) is -2", abs(b + 2.0_dp) < 1.0e-12_dp)

        call power_expand(e, r, good, why)
        call ok("(x**2)**(1/2) is refused", .not. good)
        call ok("nested refusal returns the input", r == e)

        e = sqrt(x**num(arena, 2))
        call power_expand(e, r, good, why)
        call ok("sqrt(x**2) is refused", .not. good)
        call ok("sqrt(x**2) refusal mentions abs", index(why, "abs") > 0)
    end subroutine test_power_expand_refuses_nested

    !> With positivity assumed the same rewrites are sound, and the sampling
    !> is moved to positive coordinates where the assumption holds.
    subroutine test_power_expand_with_positivity()
        type(expr_t) :: e, r
        type(assumption_context_t) :: ctx
        logical :: good
        character(:), allocatable :: why

        call ctx%init(arena)
        call assume(ctx, positive(x))
        call assume(ctx, positive(y))

        e = sqrt(x*y)
        call power_expand(e, r, good, why, ctx)
        call ok("sqrt(x*y) splits once x, y are positive", good)
        call ok("sqrt(x*y) actually changed", r /= e)
        ! Shifted to keep every coordinate positive, which is where the
        ! assumption -- and therefore the rewrite -- applies.
        call agrees("sqrt(x*y) split is exact for positive factors", e, r, &
                    .false., 4.0_dp)

        e = (x*y)**rat(arena, 1_int64, 3_int64)
        call power_expand(e, r, good, why, ctx)
        call ok("(x*y)**(1/3) splits once x, y are positive", good)
        call agrees("(x*y)**(1/3) split is exact for positive factors", e, &
                    r, .false., 4.0_dp)

        e = sqrt(x**num(arena, 2))
        call power_expand(e, r, good, why, ctx)
        call ok("sqrt(x**2) collapses once x is positive", good)
        call agrees("sqrt(x**2) collapse is exact for positive x", e, r, &
                    .false., 4.0_dp)

        ! z was never assumed positive, so the same rewrite is still refused.
        e = sqrt(x*z)
        call power_expand(e, r, good, why, ctx)
        call ok("an unassumed factor still refuses", .not. good)
    end subroutine test_power_expand_with_positivity

end program test_fortsym_trigrewrite
