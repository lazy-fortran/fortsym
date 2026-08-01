program test_fortsym_polysolve
    ! Roots checked by properties the solver cannot satisfy by accident.
    !
    ! Nothing here compares against a string or a tree the solver produced. The
    ! test carries its own complex evaluator for the expression DAG -- a hundred
    ! lines that know nothing about polynomials -- and uses it three ways:
    !
    !   * substitution. Each returned root expression is evaluated to a complex
    !     number, that number is put back into the original equation, and the
    !     result must be zero to within a relative tolerance. A wrong root fails
    !     this however plausible it looks.
    !   * Vieta. The sum and product of the returned roots must match
    !     -c(n-1)/c(n) and (-1)**n c(0)/c(n), computed from the coefficient list
    !     the test itself wrote down. Substitution alone cannot catch a missing
    !     or duplicated root; Vieta can, because it is a statement about the
    !     whole multiset.
    !   * count. On success the number of roots must equal the degree.
    !
    ! Exactness is checked separately, structurally: a root that ought to be
    ! rational must come back as an integer or rational node, never as a float.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, func, sin, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**)
    use fortsym_polysolve, only: solve_polynomial
    implicit none

    integer, parameter :: dp = real64
    real(dp), parameter :: TOL = 1.0e-9_dp

    type(arena_t), target :: arena
    integer :: nfail = 0

    call arena%init()

    call test_linear()
    call test_rational_quadratic()
    call test_irrational_quadratic()
    call test_complex_quadratic()
    call test_double_root()
    call test_zero_factoring()
    call test_rational_peeling()
    call test_mixed_cubic()
    call test_quartic_all_rational()
    call test_rational_coefficients()
    call test_constant_has_no_roots()
    call test_exact_forms()
    call test_refusals()
    call test_untested_refusals()

    if (nfail /= 0) then
        print *, "test_fortsym_polysolve: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_polysolve: all checks passed"

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

    ! ------------------------------------------------------------- oracles --

    !> Build sum c(k)*x**(k-1) from integer coefficients, solve it, and put the
    !> result through substitution, Vieta and the count check.
    subroutine check_poly(label, c)
        character(*),   intent(in) :: label
        integer(int64), intent(in) :: c(:)
        type(expr_t) :: x, e
        type(expr_t), allocatable :: roots(:)
        logical :: good, fine
        character(:), allocatable :: why
        complex(dp) :: value, total, product, z
        real(dp) :: scale
        integer :: k, n

        x = sym(arena, "x")
        e = num(arena, 0)
        do k = 1, size(c)
            e = e + num(arena, c(k))*x**(k - 1)
        end do

        call solve_polynomial(arena, e, x, roots, good, why)
        call ok(label//": solved", good)
        if (.not. good) then
            print *, "       refused: ", why
            return
        end if

        n = size(c) - 1
        call ok(label//": one root per degree", size(roots) == n)
        if (size(roots) /= n) return

        total = (0.0_dp, 0.0_dp)
        product = (1.0_dp, 0.0_dp)
        scale = 0.0_dp
        do k = 1, size(c)
            scale = max(scale, abs(real(c(k), dp)))
        end do

        do k = 1, n
            z = cev(roots(k), "x", (0.0_dp, 0.0_dp), fine)
            call ok(label//": root evaluates", fine)
            if (.not. fine) return
            value = cev(e, "x", z, fine)
            call ok(label//": root satisfies the equation", &
                    fine .and. abs(value) <= TOL*scale*max(1.0_dp, abs(z)**n))
            total = total + z
            product = product*z
        end do

        ! Vieta, against coefficients this test wrote down itself.
        call ok(label//": sum matches Vieta", &
                abs(total + cmplx(c(n), 0.0_dp, dp)/cmplx(c(n + 1), 0.0_dp, dp)) &
                <= TOL*max(1.0_dp, abs(total)))
        call ok(label//": product matches Vieta", &
                abs(product - real((-1)**n, dp)*cmplx(c(1), 0.0_dp, dp) &
                    /cmplx(c(n + 1), 0.0_dp, dp)) &
                <= TOL*max(1.0_dp, abs(product)))
    end subroutine check_poly

    !> A small complex evaluator over the expression DAG, written here so the
    !> oracle does not borrow any of the solver's arithmetic. Unknown nodes set
    !> fine to false rather than contributing a made-up value.
    recursive function cev(e, vname, z, fine) result(v)
        type(expr_t), intent(in)  :: e
        character(*), intent(in)  :: vname
        complex(dp),  intent(in)  :: z
        logical,      intent(out) :: fine
        type(expr_t) :: expo
        complex(dp) :: v, acc, base
        integer     :: k, n
        logical     :: sub_fine

        v = (0.0_dp, 0.0_dp)
        fine = .true.

        select case (e%kind())
        case (NK_INT)
            v = cmplx(real(e%int_value(), dp), 0.0_dp, dp)
        case (NK_RAT)
            v = cmplx(real(e%int_value(), dp)/real(e%den_value(), dp), &
                      0.0_dp, dp)
        case (NK_SYM)
            if (chars(e%name()) == vname) then
                v = z
            else
                fine = .false.
            end if
        case (NK_CONST)
            if (chars(e%name()) == "i") then
                v = (0.0_dp, 1.0_dp)
            else
                fine = .false.
            end if
        case (NK_ADD)
            acc = (0.0_dp, 0.0_dp)
            do k = 1, e%nargs()
                acc = acc + cev(e%arg(k), vname, z, sub_fine)
                if (.not. sub_fine) fine = .false.
            end do
            v = acc
        case (NK_MUL)
            acc = (1.0_dp, 0.0_dp)
            do k = 1, e%nargs()
                acc = acc*cev(e%arg(k), vname, z, sub_fine)
                if (.not. sub_fine) fine = .false.
            end do
            v = acc
        case (NK_POW)
            base = cev(e%arg(1), vname, z, sub_fine)
            if (.not. sub_fine) fine = .false.
            expo = e%arg(2)
            if (expo%kind() /= NK_INT) then
                fine = .false.
                return
            end if
            n = int(expo%int_value())
            if (n < 0) then
                if (abs(base) == 0.0_dp) then
                    fine = .false.
                    return
                end if
                v = (1.0_dp, 0.0_dp)/(base**(-n))
            else
                v = base**n
            end if
        case (NK_FUNC)
            if (chars(e%name()) == "sqrt" .and. e%nargs() == 1) then
                v = sqrt(cev(e%arg(1), vname, z, sub_fine))
                if (.not. sub_fine) fine = .false.
            else
                fine = .false.
            end if
        case default
            fine = .false.
        end select
    end function cev

    ! --------------------------------------------------------- solved cases --

    !> 3x + 6 = 0. Root -2 by hand.
    subroutine test_linear()
        call check_poly("3x+6", [6_int64, 3_int64])
    end subroutine test_linear

    !> x**2 - 5x + 6 and 2x**2 - 3x + 1, both with rational roots.
    subroutine test_rational_quadratic()
        call check_poly("x^2-5x+6", [6_int64, -5_int64, 1_int64])
        call check_poly("2x^2-3x+1", [1_int64, -3_int64, 2_int64])
    end subroutine test_rational_quadratic

    !> x**2 - 2: the roots are irrational, so the answer must be a surd rather
    !> than a refusal or a decimal.
    subroutine test_irrational_quadratic()
        call check_poly("x^2-2", [-2_int64, 0_int64, 1_int64])
        call check_poly("2x^2-3x-4", [-4_int64, -3_int64, 2_int64])
    end subroutine test_irrational_quadratic

    !> x**2 + 1 has no real root; the pair must still come back exactly.
    subroutine test_complex_quadratic()
        call check_poly("x^2+1", [1_int64, 0_int64, 1_int64])
        call check_poly("x^2+x+1", [1_int64, 1_int64, 1_int64])
    end subroutine test_complex_quadratic

    !> A double root must appear twice, or the product side of Vieta fails.
    subroutine test_double_root()
        call check_poly("x^2-4x+4", [4_int64, -4_int64, 1_int64])
    end subroutine test_double_root

    !> x**4 + x**2 = x**2 (x**2 + 1): two roots at zero and a complex pair.
    subroutine test_zero_factoring()
        call check_poly("x^4+x^2", [0_int64, 0_int64, 1_int64, 0_int64, 1_int64])
        call check_poly("x^3-x", [0_int64, -1_int64, 0_int64, 1_int64])
    end subroutine test_zero_factoring

    !> Degree 3 solved entirely by peeling rational roots: (x-1)(x-2)(x+3).
    subroutine test_rational_peeling()
        call check_poly("(x-1)(x-2)(x+3)", &
                        [6_int64, -7_int64, 0_int64, 1_int64])
    end subroutine test_rational_peeling

    !> (x-1)(x**2-2): one rational root peeled, an irrational pair left. This is
    !> the case where degree 3 is answerable without any cubic formula.
    subroutine test_mixed_cubic()
        call check_poly("(x-1)(x^2-2)", &
                        [2_int64, -2_int64, -1_int64, 1_int64])
    end subroutine test_mixed_cubic

    !> x**4 - 1: two rational roots and then the complex pair.
    subroutine test_quartic_all_rational()
        call check_poly("x^4-1", &
                        [-1_int64, 0_int64, 0_int64, 0_int64, 1_int64])
    end subroutine test_quartic_all_rational

    !> Rational coefficients, and a discriminant that is a rational rather than
    !> an integer: x**2 - (3/2)x + 1/2 has roots 1 and 1/2, and (1/3)x**2 - 2/3
    !> has roots +-sqrt(2). Checked by substitution through the test's own
    !> evaluator, not by looking at the returned form.
    subroutine test_rational_coefficients()
        type(expr_t) :: x, e
        type(expr_t), allocatable :: roots(:)
        logical :: good, fine, fine_value
        character(:), allocatable :: why
        complex(dp) :: z, value
        integer :: k
        logical :: all_zero

        x = sym(arena, "x")
        e = x**2 - rat(arena, 3_int64, 2_int64)*x + rat(arena, 1_int64, 2_int64)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("x^2-(3/2)x+1/2 solved with two roots", &
                good .and. size(roots) == 2)
        all_zero = good
        do k = 1, size(roots)
            z = cev(roots(k), "x", (0.0_dp, 0.0_dp), fine)
            if (.not. fine) all_zero = .false.
            value = cev(e, "x", z, fine_value)
            if (.not. fine_value) all_zero = .false.
            if (abs(value) > TOL) all_zero = .false.
        end do
        call ok("rational-coefficient roots satisfy the equation", all_zero)

        e = rat(arena, 1_int64, 3_int64)*x**2 - rat(arena, 2_int64, 3_int64)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("(1/3)x^2-2/3 solved with two roots", &
                good .and. size(roots) == 2)
        all_zero = good
        do k = 1, size(roots)
            z = cev(roots(k), "x", (0.0_dp, 0.0_dp), fine)
            if (.not. fine) all_zero = .false.
            value = cev(e, "x", z, fine_value)
            if (.not. fine_value) all_zero = .false.
            if (abs(value) > TOL) all_zero = .false.
        end do
        call ok("rational-discriminant surd roots verify", all_zero)
    end subroutine test_rational_coefficients

    !> A nonzero constant equation has no solutions, which is an answer, not a
    !> refusal.
    subroutine test_constant_has_no_roots()
        type(expr_t) :: x, e
        type(expr_t), allocatable :: roots(:)
        logical :: good
        character(:), allocatable :: why

        x = sym(arena, "x")
        e = num(arena, 5) + num(arena, 0)*x
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("5 == 0 answers with no roots", good .and. size(roots) == 0)
    end subroutine test_constant_has_no_roots

    !> Exactness is structural: rational roots must be exact nodes, and the
    !> irrational ones must be built from sqrt of an integer, never a float.
    subroutine test_exact_forms()
        type(expr_t) :: x, e
        type(expr_t), allocatable :: roots(:)
        logical :: good
        character(:), allocatable :: why
        integer :: k
        logical :: all_exact

        x = sym(arena, "x")
        e = num(arena, 2)*x**2 - num(arena, 3)*x + num(arena, 1)
        call solve_polynomial(arena, e, x, roots, good, why)
        all_exact = good
        do k = 1, size(roots)
            if (roots(k)%kind() /= NK_INT .and. roots(k)%kind() /= NK_RAT) then
                all_exact = .false.
            end if
        end do
        call ok("2x^2-3x+1 roots are exact numbers", all_exact)

        e = x**2 - num(arena, 2)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("x^2-2 has two roots", good .and. size(roots) == 2)
        call ok("x^2-2 roots contain no float", good .and. no_reals(roots))
    end subroutine test_exact_forms

    recursive function no_reals(list) result(clean)
        type(expr_t), intent(in) :: list(:)
        logical :: clean
        integer :: k

        clean = .true.
        do k = 1, size(list)
            if (list(k)%kind() == NK_REAL) then
                clean = .false.
            else if (list(k)%nargs() > 0) then
                if (.not. no_reals(children(list(k)))) clean = .false.
            end if
        end do
    end function no_reals

    function children(e) result(kids)
        type(expr_t), intent(in) :: e
        type(expr_t), allocatable :: kids(:)
        integer :: k

        allocate (kids(e%nargs()))
        do k = 1, e%nargs()
            kids(k) = e%arg(k)
        end do
    end function children

    ! ------------------------------------------------------------ refusals --

    subroutine test_refusals()
        type(expr_t) :: x, y, e
        type(expr_t), allocatable :: roots(:)
        logical :: good
        character(:), allocatable :: why

        x = sym(arena, "x")
        y = sym(arena, "y")

        ! Irreducible cubic: no rational root exists, and the cubic formula is
        ! deliberately absent.
        e = x**3 - num(arena, 2)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("x^3-2 is refused", .not. good)
        call ok("cubic refusal names the degree", index(why, "degree 3") > 0)
        call ok("refused solve returns no roots", size(roots) == 0)

        ! Quartic with no rational root either.
        e = x**4 + x + num(arena, 1)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("x^4+x+1 is refused", .not. good)

        e = sin(x) + x
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("sin(x)+x is refused", .not. good)
        call ok("refusal names the function", index(why, "sin") > 0)

        e = x*y - num(arena, 1)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("x*y-1 is refused", .not. good)
        call ok("refusal names the second symbol", &
                index(why, "second free symbol y") > 0)

        e = real_expr(arena, 0.5_dp)*x + num(arena, 1)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("floating coefficient is refused", .not. good)
        call ok("float refusal explains itself", index(why, "floating") > 0)

        e = num(arena, 1)/x + num(arena, 1)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("1/x + 1 is refused as non-polynomial", .not. good)

        e = x - x
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("0 == 0 is refused rather than answered", .not. good)
        call ok("identity refusal explains itself", len(why) > 0)

        e = x**2 + num(arena, 1)
        call solve_polynomial(arena, e, num(arena, 3), roots, good, why)
        call ok("a non-symbol solve variable is refused", .not. good)

        e = x**2 - func("sqrt", [num(arena, 2)])
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("an irrational coefficient is refused", .not. good)
    end subroutine test_refusals

    !> Refusal paths the module documents but that nothing exercised: deep
    !> nesting, the degree cap, a non-integer exponent, arbitrary-precision
    !> coefficients, int64 overflow, and an arena mismatch. Each one must come
    !> back as a refusal with a reason naming the actual obstruction.
    subroutine test_untested_refusals()
        type(arena_t), target :: other
        type(expr_t) :: x, e, big, elsewhere
        type(expr_t), allocatable :: roots(:)
        logical :: good, made
        character(:), allocatable :: why
        integer :: k
        integer(int64), parameter :: HALF_ROOT = 3037000500_int64

        x = sym(arena, "x")

        ! Deeply nested but perfectly legal linear polynomial. Depth 4000 used
        ! to be a SIGSEGV inside the structural walk.
        e = x
        do k = 1, 12000
            e = (e + num(arena, 1))*(x**0)
        end do
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("a 12000-level nested equation is refused, not a crash", &
                .not. good)
        call ok("nesting refusal names the depth limit", &
                index(why, "nests deeper") > 0)
        call ok("nesting refusal returns no roots", size(roots) == 0)

        ! Degree cap.
        e = x**17 + num(arena, 1)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("degree 17 is refused", .not. good)
        call ok("degree refusal names the maximum", index(why, "degree") > 0)

        ! Non-integer exponent: x**(1/2) as a power node with a rational
        ! exponent, which is a different code path from sqrt(x).
        e = x**rat(arena, 1_int64, 2_int64) - num(arena, 2)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("x**(1/2) is refused", .not. good)
        call ok("non-integer exponent refusal explains itself", &
                index(why, "exponent") > 0 .or. index(why, "function") > 0)

        ! Arbitrary-precision coefficient.
        big = exact(arena, "9223372036854775808", made)
        call ok("big-integer literal built", made)
        if (made) then
            e = x - big
            call solve_polynomial(arena, e, x, roots, good, why)
            call ok("an arbitrary-precision coefficient is refused", .not. good)
            call ok("big-integer refusal names the precision limit", &
                    index(why, "arbitrary-precision") > 0)
        end if

        ! int64 overflow while forming the discriminant.
        e = num(arena, HALF_ROOT)*x**2 + num(arena, HALF_ROOT)*x &
            - num(arena, HALF_ROOT)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("a coefficient set that overflows int64 is refused", .not. good)
        call ok("overflow refusal names the arithmetic", &
                index(why, "overflow") > 0)

        ! The one integer whose magnitude int64 cannot represent.
        e = x - num(arena, -huge(0_int64) - 1_int64)
        call solve_polynomial(arena, e, x, roots, good, why)
        call ok("a coefficient of -2**63 is refused", .not. good)
        call ok("-2**63 refusal names the arithmetic limit", &
                index(why, "64-bit") > 0)

        ! Arena mismatch.
        call other%init()
        elsewhere = sym(other, "x")
        e = x**2 - num(arena, 1)
        call solve_polynomial(arena, e, elsewhere, roots, good, why)
        call ok("a variable from another arena is refused", .not. good)
        call ok("arena refusal names the arenas", index(why, "arena") > 0)
    end subroutine test_untested_refusals

end program test_fortsym_polysolve
