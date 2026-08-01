program test_fortsym_sums
    ! Closed forms checked against the sums they claim to equal.
    !
    ! The oracle is never the module. Every expected value is produced by a
    ! plain Fortran loop that adds or multiplies the terms one at a time, which
    ! is the definition of the sum and shares no code with the closed form. The
    ! closed form is then evaluated numerically at the same bound and the two
    ! are required to agree.
    !
    ! Each family is checked at several upper bounds, including the empty range
    ! and the first non-empty one, because an off-by-one in the bounds is the
    ! failure this subject produces most reliably and a single sample point
    ! hides it.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, rat, func, &
                            operator(+), operator(-), operator(*), &
                            operator(/), operator(**)
    use fortsym_subs, only: subs
    use fortsym_numeric, only: numeric_value
    use fortsym_sums, only: sum_closed_form, product_closed_form
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target :: arena
    type(expr_t) :: k, n
    integer :: nfail = 0

    call arena%init()
    k = sym(arena, "k")
    n = sym(arena, "n")

    call test_faulhaber_against_loops()
    call test_shifted_lower_bound()
    call test_linearity()
    call test_geometric_concrete_ratio()
    call test_unit_ratio_branch()
    call test_symbolic_ratio()
    call test_telescoping()
    call test_expansion_path()
    call test_empty_ranges()
    call test_refusals()
    call test_products()
    call test_product_refusals()
    call test_huge_concrete_span()
    call test_non_integer_bounds()
    call test_wrapped_infinities()
    call test_telescoping_poles()
    call test_zero_ratio_product()

    if (nfail /= 0) then
        print *, "test_fortsym_sums: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_sums: all checks passed"

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

    !> Numeric value of an expression once the symbol `s` is set to `v`.
    !> Reports failure through `good` rather than inventing a number.
    function at(e, s, v, good) result(value)
        type(expr_t), intent(in)  :: e, s
        integer,      intent(in)  :: v
        logical,      intent(out) :: good
        real(dp) :: value
        character(:), allocatable :: why

        call numeric_value(subs(e, s, num(arena, v)), value, good, why)
    end function at

    !> Value with two symbols bound, for the symbolic-ratio case.
    function at2(e, s1, v1, s2, v2, good) result(value)
        type(expr_t), intent(in)  :: e, s1, s2
        integer,      intent(in)  :: v1, v2
        logical,      intent(out) :: good
        real(dp) :: value
        type(expr_t) :: bound
        character(:), allocatable :: why

        bound = subs(subs(e, s1, num(arena, v1)), s2, num(arena, v2))
        call numeric_value(bound, value, good, why)
    end function at2

    !> Agreement to within rounding. The closed forms carry exact rationals
    !> whose double evaluation rounds -- k**2/3 at k = 3 is not 3 exactly --
    !> so equality is asked for up to a relative tolerance rather than bit for
    !> bit. Exact zeros are still compared exactly, where they are meant.
    function near(v, expected) result(yes)
        real(dp), intent(in) :: v, expected
        logical :: yes
        yes = abs(v - expected) <= 1.0e-11_dp*max(1.0_dp, abs(expected))
    end function near

    !> The oracle for a power sum: add the terms up, one at a time.
    function loop_power_sum(lo, up, p) result(total)
        integer, intent(in) :: lo, up, p
        integer(int64) :: total, term, q
        integer :: i

        total = 0_int64
        do i = lo, up
            term = 1_int64
            do q = 1, int(p, int64)
                term = term*int(i, int64)
            end do
            total = total + term
        end do
    end function loop_power_sum

    !> sum k^p for p = 1, 2, 3, 5 with a symbolic upper bound, checked at
    !> several bounds against the loop. n = 0 is the empty range: the closed
    !> form has to produce zero there without being told about it.
    subroutine test_faulhaber_against_loops()
        type(expr_t) :: s
        real(dp) :: v
        logical :: good, eok
        character(:), allocatable :: why
        integer :: p, m
        integer, parameter :: powers(4) = [1, 2, 3, 5]
        character(len=1) :: tag

        do p = 1, 4
            s = sum_closed_form(arena, k**powers(p), k, num(arena, 1), n, &
                                eok, why)
            write (tag, '(i1)') powers(p)
            call ok("sum k**"//tag//" has a closed form", eok)
            if (.not. eok) cycle
            do m = 0, 6
                v = at(s, n, m, good)
                call ok("sum k**"//tag//" matches the loop", &
                        good .and. near(v, real(loop_power_sum(1, m, powers(p)), dp)))
            end do
        end do
    end subroutine test_faulhaber_against_loops

    !> A lower bound other than 1, where an antidifference evaluated at the
    !> wrong endpoint would show up immediately.
    subroutine test_shifted_lower_bound()
        type(expr_t) :: s
        real(dp) :: v
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m

        s = sum_closed_form(arena, k**2, k, num(arena, 3), n, eok, why)
        call ok("sum_{k=3..n} k**2 has a closed form", eok)
        if (.not. eok) return
        do m = 2, 8
            v = at(s, n, m, good)
            call ok("sum_{k=3..n} k**2 matches the loop", &
                    good .and. near(v, real(loop_power_sum(3, m, 2), dp)))
        end do
    end subroutine test_shifted_lower_bound

    !> Linearity: a three-term polynomial body against the same loop.
    subroutine test_linearity()
        type(expr_t) :: s, body
        real(dp) :: v
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m, i
        real(dp) :: expected

        body = num(arena, 3)*k**2 - num(arena, 2)*k + num(arena, 5)
        s = sum_closed_form(arena, body, k, num(arena, 1), n, eok, why)
        call ok("polynomial body has a closed form", eok)
        if (.not. eok) return

        do m = 0, 7
            expected = 0.0_dp
            do i = 1, m
                expected = expected + 3.0_dp*real(i, dp)**2 &
                           - 2.0_dp*real(i, dp) + 5.0_dp
            end do
            v = at(s, n, m, good)
            call ok("3k^2-2k+5 matches the loop", good .and. near(v, expected))
        end do
    end subroutine test_linearity

    !> Geometric with a literal ratio, starting at k = 0 so that the k = 0 term
    !> is included -- the place where a closed form written for k = 1 goes
    !> wrong by exactly one term.
    subroutine test_geometric_concrete_ratio()
        type(expr_t) :: s
        real(dp) :: v, expected
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m, i

        s = sum_closed_form(arena, num(arena, 2)**k, k, num(arena, 0), n, &
                            eok, why)
        call ok("sum 2**k has a closed form", eok)
        if (.not. eok) return
        do m = -1, 8
            expected = 0.0_dp
            do i = 0, m
                expected = expected + 2.0_dp**i
            end do
            v = at(s, n, m, good)
            call ok("sum_{k=0..n} 2**k matches the loop", &
                    good .and. v == expected)
        end do

        ! A rational ratio, where the sum is not an integer.
        s = sum_closed_form(arena, rat(arena, 1_int64, 2_int64)**k, k, &
                            num(arena, 0), n, eok, why)
        call ok("sum (1/2)**k has a closed form", eok)
        if (.not. eok) return
        do m = 0, 8
            expected = 0.0_dp
            do i = 0, m
                expected = expected + 0.5_dp**i
            end do
            v = at(s, n, m, good)
            call ok("sum_{k=0..n} (1/2)**k matches the loop", &
                    good .and. abs(v - expected) < 1.0e-13_dp)
        end do
    end subroutine test_geometric_concrete_ratio

    !> r = 1 is the removable singularity of the geometric closed form. With a
    !> literal 1 the branch is decidable, and the answer must be the number of
    !> terms rather than 0/0 or a wrong shift.
    subroutine test_unit_ratio_branch()
        type(expr_t) :: s
        real(dp) :: v
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m

        s = sum_closed_form(arena, num(arena, 1)**k, k, num(arena, 1), n, &
                            eok, why)
        call ok("sum 1**k has a closed form", eok)
        if (.not. eok) return
        do m = 0, 5
            v = at(s, n, m, good)
            call ok("sum_{k=1..n} 1**k is n", good .and. v == real(m, dp))
        end do

        s = sum_closed_form(arena, num(arena, 7)*num(arena, 1)**k, k, &
                            num(arena, 2), n, eok, why)
        call ok("sum 7*1**k has a closed form", eok)
        if (.not. eok) return
        do m = 1, 5
            v = at(s, n, m, good)
            call ok("sum_{k=2..n} 7 is 7*(n-1)", &
                    good .and. v == 7.0_dp*real(m - 1, dp))
        end do
    end subroutine test_unit_ratio_branch

    !> A symbolic ratio is refused until the caller asserts r /= 1, and the
    !> asserted form is then checked at a concrete ratio against the loop.
    subroutine test_symbolic_ratio()
        type(expr_t) :: s, r
        real(dp) :: v, expected
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m, i

        r = sym(arena, "r")
        s = sum_closed_form(arena, r**k, k, num(arena, 0), n, eok, why)
        call ok("symbolic ratio is refused by default", .not. eok)
        call ok("refusal mentions the r = 1 branch", index(why, "r = 1") > 0)

        s = sum_closed_form(arena, r**k, k, num(arena, 0), n, eok, why, &
                            assume_ratio_ne_one=.true.)
        call ok("asserted r /= 1 gives a closed form", eok)
        if (.not. eok) return
        do m = 0, 6
            expected = 0.0_dp
            do i = 0, m
                expected = expected + 3.0_dp**i
            end do
            v = at2(s, r, 3, n, m, good)
            call ok("symbolic geometric at r=3 matches the loop", &
                    good .and. abs(v - expected) < 1.0e-9_dp)
        end do
    end subroutine test_symbolic_ratio

    !> A body that is literally f(k+1) - f(k). Checked both on a polynomial
    !> difference, where the answer is also reachable by other means, and on
    !> 1/(k+1) - 1/k, where it is not.
    subroutine test_telescoping()
        type(expr_t) :: s, body
        real(dp) :: v, expected
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m, i

        body = (k + 1)**2 - k**2
        s = sum_closed_form(arena, body, k, num(arena, 1), n, eok, why)
        call ok("(k+1)^2 - k^2 has a closed form", eok)
        if (.not. eok) return
        do m = 0, 6
            expected = 0.0_dp
            do i = 1, m
                expected = expected + real(i + 1, dp)**2 - real(i, dp)**2
            end do
            v = at(s, n, m, good)
            call ok("telescoping polynomial matches the loop", &
                    good .and. v == expected)
        end do

        body = num(arena, 1)/(k + 1) - num(arena, 1)/k
        s = sum_closed_form(arena, body, k, num(arena, 1), n, eok, why)
        call ok("1/(k+1) - 1/k has a closed form", eok)
        if (.not. eok) return
        do m = 0, 6
            expected = 0.0_dp
            do i = 1, m
                expected = expected + 1.0_dp/real(i + 1, dp) - 1.0_dp/real(i, dp)
            end do
            v = at(s, n, m, good)
            call ok("telescoping reciprocal matches the loop", &
                    good .and. abs(v - expected) < 1.0e-13_dp)
        end do
    end subroutine test_telescoping

    !> Concrete bounds are expanded term by term. The result is compared with
    !> the loop, and with the closed form obtained symbolically, so the two
    !> paths are pinned to each other as well as to the definition.
    subroutine test_expansion_path()
        type(expr_t) :: s, closed
        real(dp) :: v, w
        logical :: good, eok
        character(:), allocatable :: why

        s = sum_closed_form(arena, k**3, k, num(arena, 1), num(arena, 10), &
                            eok, why)
        call ok("concrete bounds expand", eok)
        if (.not. eok) return
        call numeric_value(s, v, good, why)
        call ok("expanded sum k**3, 1..10 matches the loop", &
                good .and. v == real(loop_power_sum(1, 10, 3), dp))

        closed = sum_closed_form(arena, k**3, k, num(arena, 1), n, eok, why)
        if (.not. eok) return
        w = at(closed, n, 10, good)
        call ok("expansion and closed form agree at n = 10", &
                good .and. w == v)

        ! A body with no closed form is still summed exactly when the range is
        ! small enough to write out.
        s = sum_closed_form(arena, func("sin", [k]), k, num(arena, 1), &
                            num(arena, 3), eok, why)
        call ok("sin(k) over a small range expands", eok)
        if (eok) then
            call numeric_value(s, v, good, why)
            call ok("expanded sin sum matches the loop", &
                    good .and. abs(v - (sin(1.0_dp) + sin(2.0_dp) &
                                        + sin(3.0_dp))) < 1.0e-14_dp)
        end if
    end subroutine test_expansion_path

    !> Empty ranges: zero for a sum, one for a product, both from concrete
    !> bounds and from a closed form evaluated at upper = lower - 1.
    subroutine test_empty_ranges()
        type(expr_t) :: s
        real(dp) :: v
        logical :: good, eok
        character(:), allocatable :: why

        s = sum_closed_form(arena, k**2, k, num(arena, 5), num(arena, 3), &
                            eok, why)
        call ok("empty concrete sum is answered", eok)
        call numeric_value(s, v, good, why)
        call ok("empty concrete sum is 0", good .and. v == 0.0_dp)

        s = product_closed_form(arena, k**2, k, num(arena, 5), &
                                num(arena, 3), eok, why)
        call ok("empty concrete product is answered", eok)
        call numeric_value(s, v, good, why)
        call ok("empty concrete product is 1", good .and. v == 1.0_dp)

        s = sum_closed_form(arena, k**3, k, num(arena, 4), n, eok, why)
        if (eok) then
            v = at(s, n, 3, good)
            call ok("closed form is 0 at upper = lower - 1", &
                    good .and. v == 0.0_dp)
            v = at(s, n, 4, good)
            call ok("closed form is the single term at upper = lower", &
                    good .and. v == 64.0_dp)
        else
            call ok("sum_{k=4..n} k**3 has a closed form", .false.)
        end if

        s = product_closed_form(arena, num(arena, 3), k, num(arena, 4), n, &
                                eok, why)
        if (eok) then
            v = at(s, n, 3, good)
            call ok("empty symbolic product is 1", good .and. v == 1.0_dp)
        else
            call ok("constant product has a closed form", .false.)
        end if
    end subroutine test_empty_ranges

    subroutine test_refusals()
        type(expr_t) :: s
        logical :: eok
        character(:), allocatable :: why

        s = sum_closed_form(arena, func("sin", [k]), k, num(arena, 1), n, &
                            eok, why)
        call ok("sin(k) with a symbolic bound is refused", .not. eok)
        call ok("refusal explains itself", len(why) > 0)

        s = sum_closed_form(arena, num(arena, 1)/k, k, num(arena, 1), n, &
                            eok, why)
        call ok("harmonic sum is refused", .not. eok)
        call ok("harmonic refusal says why", index(why, "harmonic") > 0)

        s = sum_closed_form(arena, k**2, k, num(arena, 1), &
                            sym(arena, "inf"), eok, why)
        call ok("infinite upper bound is refused", .not. eok)
        call ok("infinite refusal mentions convergence", &
                index(why, "convergence") > 0)

        ! Too many terms to expand, and no closed form for the body: the answer
        ! is a refusal naming the cap, not a hang.
        s = sum_closed_form(arena, func("sin", [k]), k, num(arena, 1), &
                            num(arena, 5000), eok, why)
        call ok("oversized expansion is refused", .not. eok)
        call ok("cap refusal names the cap", index(why, "cap") > 0)

        ! k**k depends on the variable in both base and exponent.
        s = sum_closed_form(arena, k**k, k, num(arena, 1), n, eok, why)
        call ok("k**k is refused", .not. eok)

        s = sum_closed_form(arena, func("sin", [k])*k, k, num(arena, 1), n, &
                            eok, why)
        call ok("k*sin(k) is refused", .not. eok)
    end subroutine test_refusals

    !> Products, against a loop that multiplies the factors one at a time.
    subroutine test_products()
        type(expr_t) :: p
        real(dp) :: v, expected
        logical :: good, eok
        character(:), allocatable :: why
        integer :: m, i

        p = product_closed_form(arena, num(arena, 3), k, num(arena, 1), n, &
                                eok, why)
        call ok("constant product has a closed form", eok)
        if (eok) then
            do m = 0, 6
                expected = 1.0_dp
                do i = 1, m
                    expected = expected*3.0_dp
                end do
                v = at(p, n, m, good)
                call ok("product of 3 matches the loop", &
                        good .and. v == expected)
            end do
        end if

        p = product_closed_form(arena, num(arena, 2)**k, k, num(arena, 1), &
                                n, eok, why)
        call ok("product 2**k has a closed form", eok)
        if (eok) then
            do m = 0, 6
                expected = 1.0_dp
                do i = 1, m
                    expected = expected*2.0_dp**i
                end do
                v = at(p, n, m, good)
                call ok("product 2**k matches the loop", &
                        good .and. v == expected)
            end do
        end if

        ! A ratio body f(k+1)/f(k) telescopes to f(n+1)/f(lower).
        p = product_closed_form(arena, (k + 1)/k, k, num(arena, 1), n, &
                                eok, why)
        call ok("product (k+1)/k has a closed form", eok)
        if (eok) then
            do m = 0, 6
                expected = 1.0_dp
                do i = 1, m
                    expected = expected*real(i + 1, dp)/real(i, dp)
                end do
                v = at(p, n, m, good)
                call ok("telescoping product matches the loop", &
                        good .and. abs(v - expected) < 1.0e-12_dp)
            end do
        end if

        ! Concrete bounds are expanded, and must agree with the loop.
        p = product_closed_form(arena, k, k, num(arena, 1), num(arena, 6), &
                                eok, why)
        call ok("concrete product expands", eok)
        if (eok) then
            call numeric_value(p, v, good, why)
            call ok("6! is 720", good .and. v == 720.0_dp)
        end if
    end subroutine test_products

    subroutine test_product_refusals()
        type(expr_t) :: p
        logical :: eok
        character(:), allocatable :: why

        p = product_closed_form(arena, k**2, k, num(arena, 1), n, eok, why)
        call ok("product k**2 with symbolic bound is refused", .not. eok)
        call ok("product refusal mentions gamma", index(why, "gamma") > 0)

        p = product_closed_form(arena, k + 1, k, num(arena, 1), n, eok, why)
        call ok("product of a sum body is refused", .not. eok)

        p = product_closed_form(arena, func("sin", [k]), k, num(arena, 1), &
                                n, eok, why)
        call ok("product of sin(k) is refused", .not. eok)
    end subroutine test_product_refusals

    !> A concrete range whose term count overflows int64. `up - lo + 1` wraps
    !> negative for a span of 2**63 and slips under the expansion cap, so the
    !> module used to enter a 9.2e18-iteration loop instead of answering. The
    !> body has a closed form, so the right answer is the closed form; the
    !> oracle is the textbook L(L+1)(2L+1)/3 for sum_{k=-L}^{L} k**2, which
    !> shares no code with Faulhaber's Bernoulli recurrence.
    subroutine test_huge_concrete_span()
        type(expr_t) :: s
        real(dp) :: v, expected, big
        logical :: good, eok
        character(:), allocatable :: why
        integer(int64), parameter :: half = 4611686018427387904_int64

        s = sum_closed_form(arena, k**2, k, num(arena, -half), &
                            num(arena, half), eok, why)
        call ok("sum k**2 over a 2**63-wide range is answered, not looped", eok)
        if (eok) then
            call numeric_value(s, v, good, why)
            big = real(half, dp)
            expected = big*(big + 1.0_dp)*(2.0_dp*big + 1.0_dp)/3.0_dp
            call ok("2**63-wide power sum matches L(L+1)(2L+1)/3", good)
            if (good) then
                call ok("2**63-wide power sum value is right", &
                        abs(v - expected) <= 1.0e-9_dp*abs(expected))
            end if
        end if

        ! Same range, a body with no closed form: a refusal naming the cap,
        ! still not a loop.
        s = sum_closed_form(arena, func("sin", [k]), k, num(arena, -half), &
                            num(arena, half), eok, why)
        call ok("2**63-wide sin sum is refused", .not. eok)
        call ok("overflowing span refusal names the cap", index(why, "cap") > 0)
    end subroutine test_huge_concrete_span

    !> Non-integer bounds were only caught as bare literals, so any bound built
    !> from operators produced a confident non-integer answer.
    subroutine test_non_integer_bounds()
        type(expr_t) :: s, root2, halfn
        logical :: eok
        character(:), allocatable :: why

        root2 = num(arena, 2)**rat(arena, 1_int64, 2_int64)
        s = sum_closed_form(arena, k, k, num(arena, 1), root2, eok, why)
        call ok("sqrt(2) as an upper bound is refused", .not. eok)
        call ok("sqrt(2) refusal says the bound is not an integer index", &
                index(why, "integer index") > 0)

        halfn = n/num(arena, 2)
        s = sum_closed_form(arena, k, k, num(arena, 1), halfn, eok, why)
        call ok("n/2 as an upper bound is refused", .not. eok)

        s = product_closed_form(arena, num(arena, 2)**k, k, num(arena, 1), &
                                halfn, eok, why)
        call ok("n/2 as a product upper bound is refused", .not. eok)
    end subroutine test_non_integer_bounds

    !> An infinite bound wrapped in anything at all used to pass as a finite
    !> unknown, and a divergent sum was answered.
    subroutine test_wrapped_infinities()
        type(expr_t) :: s, minf, twice_oo
        logical :: eok
        character(:), allocatable :: why

        minf = num(arena, -1)*sym(arena, "inf")
        s = sum_closed_form(arena, num(arena, 2)**k, k, minf, n, eok, why)
        call ok("-1*inf as a lower bound is refused", .not. eok)
        call ok("-inf refusal mentions convergence", &
                index(why, "convergence") > 0)

        twice_oo = num(arena, 2)*sym(arena, "oo")
        s = sum_closed_form(arena, k, k, num(arena, 1), twice_oo, eok, why)
        call ok("2*oo as an upper bound is refused", .not. eok)

        s = product_closed_form(arena, num(arena, 2)**k, k, num(arena, 1), &
                                twice_oo, eok, why)
        call ok("2*oo as a product upper bound is refused", .not. eok)
    end subroutine test_wrapped_infinities

    !> Telescoping collapses to f(upper+1) - f(lower), which stays finite even
    !> when a term in between divides by zero. With both bounds symbolic there
    !> is no way to rule the pole out, so the answer must be a refusal; with a
    !> concrete lower bound above the pole the closed form is issued and must
    !> still match a loop.
    subroutine test_telescoping_poles()
        type(expr_t) :: s, body, m
        real(dp) :: v, expected
        logical :: good, eok
        character(:), allocatable :: why
        integer :: i

        m = sym(arena, "m")
        body = num(arena, 1)/(k + 1) - num(arena, 1)/k
        s = sum_closed_form(arena, body, k, m, n, eok, why)
        call ok("telescoping sum with symbolic bounds is refused", .not. eok)
        call ok("pole refusal names the denominator", &
                index(why, "denominator") > 0)

        body = (k + 1)/k
        s = product_closed_form(arena, body, k, m, n, eok, why)
        call ok("telescoping product with symbolic bounds is refused", &
                .not. eok)
        call ok("product pole refusal names the denominator", &
                index(why, "denominator") > 0)

        ! A concrete range that is too wide to expand, with the pole at k = 0
        ! safely below the lower bound: still answered, and still right.
        body = num(arena, 1)/(k + 1) - num(arena, 1)/k
        s = sum_closed_form(arena, body, k, num(arena, 1), &
                            num(arena, 2000), eok, why)
        call ok("telescoping above the pole is still answered", eok)
        if (eok) then
            call numeric_value(s, v, good, why)
            expected = 0.0_dp
            do i = 1, 2000
                expected = expected + 1.0_dp/real(i + 1, dp) - 1.0_dp/real(i, dp)
            end do
            call ok("telescoping over 1..2000 matches the loop", good)
            if (good) then
                call ok("telescoped value over 1..2000 is right", &
                        abs(v - expected) < 1.0e-12_dp)
            end if
        end if
    end subroutine test_telescoping_poles

    !> The sum refuses a zero geometric ratio because 0**0 leaves the k = 0
    !> term ambiguous. The product took the same body and answered.
    subroutine test_zero_ratio_product()
        type(expr_t) :: p
        logical :: eok
        character(:), allocatable :: why

        p = product_closed_form(arena, num(arena, 0)**k, k, num(arena, 0), n, &
                                eok, why)
        call ok("product of 0**k is refused", .not. eok)
    end subroutine test_zero_ratio_product

end program test_fortsym_sums
