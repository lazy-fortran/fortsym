program test_fortsym_complexdom
    ! The complex domain, checked against double-precision complex arithmetic.
    !
    ! The oracle is the small evaluator at the bottom of this file. It walks the
    ! expression DAG in complex(dp) and knows nothing about fortsym_complexdom:
    ! it does not split anything, it just multiplies and adds. So the identities
    ! below are properties the module cannot satisfy by accident.
    !
    !   * Re[e] + i Im[e] evaluates to the same complex number as e.
    !   * Re[e] and Im[e] evaluate with an imaginary part of exactly zero. This
    !     is the check that catches the failure this module is built around --
    !     a "real part" that still secretly contains i.
    !   * conj(e) evaluates to the conjugate of e, and e*conj(e) = |e|^2.
    !   * |e| exp(i Arg[e]) = e, which pins both the modulus and the branch.
    !
    ! Sample points are chosen with both coordinates nonzero and of mixed sign,
    ! because a symbol that happens to be sampled on the real axis would let a
    ! wrong split pass.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_ALGEBRAIC
    use fortsym_algebraic, only: algebraic_i, algebraic_from_re_im, &
        algebraic_sqrt, algebraic_mul, algebraic_conjugate, algebraic_re, &
        algebraic_im
    use fortsym_expr, only: expr_t, sym, num, rat, algebraic_expr, i_expr, pi_expr, func, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==)
    use fortsym_assume, only: assumption_context_t, assume, real_valued
    use fortsym_complexdom, only: re_part, im_part, conjugate, arg_of, &
        abs_of, complex_expand, complex_split
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: NPOINT = 4
    real(dp), parameter :: TOL = 1.0e-9_dp

    ! Genuinely complex sample points: no coordinate is zero, and the signs
    ! differ, so nothing here is accidentally real.
    real(dp), parameter :: XS(NPOINT) = [0.3_dp, -1.2_dp, 2.0_dp, 0.75_dp]
    real(dp), parameter :: YS(NPOINT) = [0.7_dp, 0.5_dp, -1.3_dp, -0.4_dp]

    type(arena_t), target :: arena
    type(assumption_context_t) :: facts
    type(expr_t) :: x, y, z
    integer :: nfail = 0

    call arena%init()
    call facts%init(arena)
    x = sym(arena, "x")
    y = sym(arena, "y")
    call assume(facts, real_valued(x))
    call assume(facts, real_valued(y))
    z = x + i_expr(arena)*y

    call test_split_reconstructs()
    call test_parts_are_real()
    call test_conjugate_matches()
    call test_modulus_and_argument()
    call test_expand_matches()
    call test_literals_and_unit()
    call test_supported_function_splits()
    call test_hyperbolic_pole_refused()
    call test_tangent_pole_refused()
    call test_log_zero_refused()
    call test_unknown_reality_refused()
    call test_branch_cases_refused()
    call test_unknown_heads_refused()
    call test_identically_zero_refused()
    call test_expansion_is_bounded()
    call test_conjugate_domain_is_wider()
    call test_algebraic_atoms()
    call test_cache_is_context_local()

    if (nfail /= 0) then
        print *, "test_fortsym_complexdom: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_complexdom: all checks passed"

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

    function func_one(name, argument) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: argument
        type(expr_t) :: e, arguments(1)

        arguments(1) = argument
        e = func(name, arguments)
    end function func_one

    function func_two(name, first, second) result(e)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: first, second
        type(expr_t) :: e, arguments(2)

        arguments(1) = first
        arguments(2) = second
        e = func(name, arguments)
    end function func_two

    !> The expressions every property is checked on. Each mixes a construction
    !> the module handles with one it has to recurse through.
    function probe(k) result(e)
        integer, intent(in) :: k
        type(expr_t)        :: e

        select case (k)
        case (1); e = z**num(arena, 3)
        case (2); e = (num(arena, 2) + i_expr(arena))*z
        case (3); e = func_one("exp", z)
        case (4); e = func_one("sin", z)
        case (5); e = func_one("cos", z)
        case (6); e = z**num(arena, -2)
        case (7); e = func_one("exp", z)*func_one("sin", z) + z**num(arena, 4)
        case (8)
            e = z**num(arena, 2) - num(arena, 3)*z &
                + rat(arena, 1_int64, 3_int64)
        case (9); e = func_one("cos", z**num(arena, 2))*z
        case (10); e = func_one("sinh", z)
        case (11); e = func_one("cosh", z)
        case (12); e = func_one("tanh", z)
        case (13); e = func_one("tan", z)
        case default
            ! nprobe() and this list are separate constants; an index outside
            ! the list must stop the run rather than hand back an expr_t that
            ! was never assigned.
            error stop "probe: index outside the probe list"
        end select
    end function probe

    integer function nprobe()
        nprobe = 13
    end function nprobe

    !> Re[e] + i Im[e] must be e, numerically, at every sample point.
    subroutine test_split_reconstructs()
        type(expr_t) :: e, re, im
        complex(dp) :: ze, zre, zim
        logical :: good, allgood
        character(:), allocatable :: why
        integer :: k, p

        do k = 1, nprobe()
            e = probe(k)
            call re_part(e, facts, re, good, why)
            call ok("re_part accepts probe", good)
            if (.not. good) cycle
            call im_part(e, facts, im, good, why)
            if (.not. good) then
                call ok("im_part accepts probe", .false.)
                cycle
            end if
            allgood = .true.
            do p = 1, NPOINT
                ze = at(e, p)
                zre = at(re, p)
                zim = at(im, p)
                if (abs(ze - (zre + cmplx(0.0_dp, 1.0_dp, dp)*zim)) &
                    > TOL*(1.0_dp + abs(ze))) allgood = .false.
            end do
            call ok("re + i*im reproduces the probe", allgood)
        end do
    end subroutine test_split_reconstructs

    !> The parts must be free of i. Evaluated at real symbol values their
    !> imaginary component is exactly zero if and only if no i survived.
    subroutine test_parts_are_real()
        type(expr_t) :: e, re, im
        logical :: good, allgood
        character(:), allocatable :: why
        integer :: k, p

        do k = 1, nprobe()
            e = probe(k)
            call re_part(e, facts, re, good, why)
            if (.not. good) cycle
            call im_part(e, facts, im, good, why)
            if (.not. good) cycle
            allgood = .true.
            do p = 1, NPOINT
                if (aimag(at(re, p)) /= 0.0_dp) allgood = .false.
                if (aimag(at(im, p)) /= 0.0_dp) allgood = .false.
            end do
            call ok("both parts are exactly real", allgood)
        end do
    end subroutine test_parts_are_real

    !> conj(e) = conjg(e) numerically, and e*conj(e) = |e|^2.
    subroutine test_conjugate_matches()
        type(expr_t) :: e, c, m
        complex(dp) :: ze, zc, zm
        logical :: good, allgood, modgood
        character(:), allocatable :: why
        integer :: k, p

        do k = 1, nprobe()
            e = probe(k)
            call conjugate(e, facts, c, good, why)
            call ok("conjugate accepts probe", good)
            if (.not. good) cycle
            call abs_of(e, facts, m, good, why)
            if (.not. good) cycle
            allgood = .true.
            modgood = .true.
            do p = 1, NPOINT
                ze = at(e, p)
                zc = at(c, p)
                zm = at(m, p)
                if (abs(zc - conjg(ze)) > TOL*(1.0_dp + abs(ze))) &
                    allgood = .false.
                if (abs(ze*zc - zm*zm) > TOL*(1.0_dp + abs(ze*ze))) &
                    modgood = .false.
            end do
            call ok("conjugate agrees with conjg", allgood)
            call ok("z*conj(z) equals abs(z)**2", modgood)
        end do
    end subroutine test_conjugate_matches

    !> |e| exp(i Arg[e]) = e. Catches a wrong modulus and a wrong branch in one
    !> identity, which atan2 gets right and a naive atan(im/re) does not.
    subroutine test_modulus_and_argument()
        type(expr_t) :: e, m, g
        complex(dp) :: ze, rebuilt, zm, zg
        real(dp) :: modulus, angle
        logical :: good, allgood, realgood
        character(:), allocatable :: why
        integer :: k, p

        do k = 1, nprobe()
            e = probe(k)
            call abs_of(e, facts, m, good, why)
            if (.not. good) cycle
            call arg_of(e, facts, g, good, why)
            call ok("arg_of accepts probe", good)
            if (.not. good) cycle
            allgood = .true.
            realgood = .true.
            do p = 1, NPOINT
                ze = at(e, p)
                zm = at(m, p)
                zg = at(g, p)
                ! Taking real() below would silently drop a surviving i, so
                ! the reality of both is checked before it is discarded. The
                ! atan2 walker in the oracle refuses a complex argument, but
                ! nothing guards the abs path.
                if (aimag(zm) /= 0.0_dp) realgood = .false.
                if (aimag(zg) /= 0.0_dp) realgood = .false.
                modulus = real(zm, dp)
                angle = real(zg, dp)
                if (abs(modulus - abs(ze)) > TOL*(1.0_dp + abs(ze))) &
                    allgood = .false.
                rebuilt = modulus*exp(cmplx(0.0_dp, angle, dp))
                if (abs(rebuilt - ze) > TOL*(1.0_dp + abs(ze))) &
                    allgood = .false.
            end do
            call ok("abs and arg rebuild the value", allgood)
            call ok("abs and arg are exactly real", realgood)
        end do
    end subroutine test_modulus_and_argument

    subroutine test_expand_matches()
        type(expr_t) :: e, w
        logical :: good, allgood
        character(:), allocatable :: why
        integer :: k, p

        do k = 1, nprobe()
            e = probe(k)
            call complex_expand(e, facts, w, good, why)
            if (.not. good) cycle
            allgood = .true.
            do p = 1, NPOINT
                if (abs(at(w, p) - at(e, p)) &
                    > TOL*(1.0_dp + abs(at(e, p)))) allgood = .false.
            end do
            call ok("complex_expand preserves the value", allgood)
        end do
    end subroutine test_expand_matches

    !> The base cases, checked against arithmetic done by hand: 7/2 is real, i
    !> splits as (0,1), and conj(i) is -i.
    subroutine test_literals_and_unit()
        type(expr_t) :: e, re, im, c
        logical :: good
        character(:), allocatable :: why

        e = rat(arena, 7_int64, 2_int64)
        call complex_split(e, facts, re, im, good, why)
        call ok("7/2 splits", good)
        call ok("7/2 has real part 7/2", at(re, 1) == cmplx(3.5_dp, 0.0_dp, dp))
        call ok("7/2 has zero imaginary part", at(im, 1) == (0.0_dp, 0.0_dp))

        e = i_expr(arena)
        call complex_split(e, facts, re, im, good, why)
        call ok("i splits", good)
        call ok("i has real part 0", at(re, 1) == (0.0_dp, 0.0_dp))
        call ok("i has imaginary part 1", at(im, 1) == (1.0_dp, 0.0_dp))

        ! Two separate checks rather than one conjunction: `.and.` does not
        ! short-circuit in Fortran, so `at(c, 1)` in the same expression would
        ! evaluate an unassigned `c` on the day `conjugate` starts refusing.
        call conjugate(e, facts, c, good, why)
        call ok("conj(i) is accepted", good)
        if (good) then
            call ok("conj(i) is -i", at(c, 1) == cmplx(0.0_dp, -1.0_dp, dp))
        end if

        call arg_of(num(arena, 0), facts, e, good, why)
        call ok("Arg at zero is refused", .not. good)
    end subroutine test_literals_and_unit

    subroutine test_supported_function_splits()
        call check_supported_function("sinh")
        call check_supported_function("cosh")
        call check_supported_function("tan")
        call check_supported_function("tanh")
        call check_supported_function("log")
    end subroutine test_supported_function_splits

    subroutine check_supported_function(name)
        character(*), intent(in) :: name
        type(expr_t) :: e, re, im
        complex(dp) :: original, reconstructed
        logical :: good, allgood
        character(:), allocatable :: why
        integer :: p

        e = func_one(name, z)
        call complex_split(e, facts, re, im, good, why)
        call ok(trim(name)//" complex split succeeds", good)
        if (.not. good) return

        allgood = .true.
        do p = 1, NPOINT
            original = at(e, p)
            reconstructed = at(re, p) + &
                cmplx(0.0_dp, 1.0_dp, dp)*at(im, p)
            if (abs(reconstructed - original) > TOL*(1.0_dp + abs(original))) &
                allgood = .false.
            if (aimag(at(re, p)) /= 0.0_dp) allgood = .false.
            if (aimag(at(im, p)) /= 0.0_dp) allgood = .false.
        end do
        call ok(trim(name)//" matches the independent complex oracle", allgood)
    end subroutine check_supported_function

    subroutine test_hyperbolic_pole_refused()
        type(expr_t) :: e, re, im
        logical :: good
        character(:), allocatable :: why

        e = func_one("tanh", i_expr(arena)*pi_expr(arena)/num(arena, 2))
        call complex_split(e, facts, re, im, good, why)
        call ok("tanh at an exact pole is refused", .not. good)
        if (.not. good) then
            call ok("tanh pole refusal names the denominator", &
                index(why, "denominator") > 0)
        end if
    end subroutine test_hyperbolic_pole_refused

    subroutine test_tangent_pole_refused()
        type(expr_t) :: e, re, im
        logical :: good
        character(:), allocatable :: why

        e = func_one("tan", pi_expr(arena)/num(arena, 2))
        call complex_split(e, facts, re, im, good, why)
        call ok("tan at an exact pole is refused", .not. good)
        if (.not. good) then
            call ok("tan pole refusal names the denominator", &
                index(why, "denominator") > 0)
        end if
    end subroutine test_tangent_pole_refused

    !> The point of the module: a symbol of unknown reality is refused, not
    !> assumed real.
    subroutine test_unknown_reality_refused()
        type(expr_t) :: u, e, out
        logical :: good
        character(:), allocatable :: why

        u = sym(arena, "u")

        call re_part(u, facts, out, good, why)
        call ok("Re of an unassumed symbol is refused", .not. good)
        call ok("refusal names the symbol", index(why, "symbol u") > 0)

        call im_part(u, facts, out, good, why)
        call ok("Im of an unassumed symbol is refused", .not. good)

        call conjugate(u, facts, out, good, why)
        call ok("conj of an unassumed symbol is refused", .not. good)

        e = u + i_expr(arena)*y
        call re_part(e, facts, out, good, why)
        call ok("a sum containing it is refused too", .not. good)

        e = func_one("exp", u)
        call abs_of(e, facts, out, good, why)
        call ok("Abs of exp(u) is refused", .not. good)

        e = func_one("zeta", num(arena, 2))
        call re_part(e, facts, out, good, why)
        call ok("Re of an unknown constant head is refused", .not. good)
    end subroutine test_unknown_reality_refused

    subroutine test_branch_cases_refused()
        type(expr_t) :: e, out
        logical :: good
        character(:), allocatable :: why

        e = z**rat(arena, 1_int64, 2_int64)
        call re_part(e, facts, out, good, why)
        call ok("fractional power is refused", .not. good)

        e = z**x
        call re_part(e, facts, out, good, why)
        call ok("symbolic exponent is refused", .not. good)

        e = func_one("log", z)
        call conjugate(e, facts, out, good, why)
        call ok("conj of log is refused across its branch cut", .not. good)

        e = func_one("sqrt", z)
        call conjugate(e, facts, out, good, why)
        call ok("conj of sqrt is refused", .not. good)

        e = z**num(arena, 0)
        call re_part(e, facts, out, good, why)
        call ok("zeroth power is refused", .not. good)

        e = z**num(arena, 100)
        call re_part(e, facts, out, good, why)
        call ok("an oversized power is refused", .not. good)
    end subroutine test_branch_cases_refused

    subroutine test_log_zero_refused()
        type(expr_t) :: e, re, im
        logical :: good
        character(:), allocatable :: why

        e = func_one("log", num(arena, 0))
        call complex_split(e, facts, re, im, good, why)
        call ok("log at zero is refused", .not. good)
        if (.not. good) then
            call ok("log zero refusal names the singularity", &
                index(why, "identically zero") > 0)
        end if

        e = func_one("log", num(arena, -1))
        call complex_split(e, facts, re, im, good, why)
        call ok("log on the negative real axis is accepted", good)
        if (good) then
            call ok("log negative real branch has argument pi", &
                abs(at(im, 1) - cmplx(acos(-1.0_dp), 0.0_dp, dp)) < TOL)
        end if
    end subroutine test_log_zero_refused

    subroutine test_unknown_heads_refused()
        type(expr_t) :: e, out
        logical :: good
        character(:), allocatable :: why

        e = func_two("besselj", num(arena, 0), z)
        call conjugate(e, facts, out, good, why)
        call ok("besselj has no conjugation rule", .not. good)
    end subroutine test_unknown_heads_refused

    !> An expression that is identically zero has no reciprocal and no
    !> argument, whatever it is spelled like.
    !>
    !> The oracle here is arithmetic, not the module: 1/0 is not a number and
    !> Arg(0) is not an angle, so the only acceptable answer is a refusal. The
    !> spellings are the ones that do not look like `0`: the arena does not
    !> fold, so `i*i + 1`, `z - z` and `0*x` all arrive intact.
    subroutine test_identically_zero_refused()
        type(expr_t) :: e, re, im, out
        logical :: good
        character(:), allocatable :: why

        e = num(arena, 0)**num(arena, -1)
        call complex_split(e, facts, re, im, good, why)
        call ok("0**(-1) is refused, not split into NaNs", .not. good)
        if (.not. good) then
            call ok("the refusal names the zero base", &
                index(why, "identically zero") > 0)
        end if

        e = (i_expr(arena)*i_expr(arena) + num(arena, 1))**num(arena, -1)
        call complex_split(e, facts, re, im, good, why)
        call ok("(i*i + 1)**(-1) is refused too", .not. good)

        call arg_of(z - z, facts, out, good, why)
        call ok("Arg(z - z) is refused", .not. good)
        call arg_of(num(arena, 0)*x, facts, out, good, why)
        call ok("Arg(0*x) is refused", .not. good)
        call arg_of(num(arena, 0) + num(arena, 0), facts, out, good, why)
        call ok("Arg(0 + 0) is refused", .not. good)
        call arg_of(rat(arena, 0_int64, 5_int64), facts, out, good, why)
        call ok("Arg(0/5) is refused", .not. good)

        e = (x + y) - (x + y)
        call arg_of(e, facts, out, good, why)
        call ok("Arg of an undecided-looking cancellation is refused", .not. good)
        e = e**num(arena, -1)
        call complex_split(e, facts, re, im, good, why)
        call ok("negative power of a hidden zero is refused", .not. good)

        ! The other direction: a base that is not zero must still go through,
        ! or the guard has been widened into a refusal of everything.
        call arg_of(z, facts, out, good, why)
        call ok("Arg(z) is still accepted", good)
        call complex_split(z**num(arena, -1), facts, re, im, good, why)
        call ok("z**(-1) is still accepted", good)
    end subroutine test_identically_zero_refused

    !> The cap has to bound the size of the result, not the size of one
    !> exponent, and nesting must not get round it.
    !>
    !> Re[z**n] has 2**n terms, so an exponent cap alone lets through a result
    !> with more terms than any consumer can walk. The independent measure used
    !> here is the term count the mathematics forces: 2**12 is accepted and
    !> checked numerically, 2**13 is refused, and the two ways of reaching a
    !> larger exponent without writing one -- nesting and multiplying -- are
    !> refused as well.
    subroutine test_expansion_is_bounded()
        type(expr_t) :: e, out
        logical :: good, allgood
        character(:), allocatable :: why
        integer :: p

        e = z**num(arena, 12)
        call re_part(e, facts, out, good, why)
        call ok("z**12 is inside the expansion bound", good)
        if (good) then
            allgood = .true.
            do p = 1, NPOINT
                if (aimag(at(out, p)) /= 0.0_dp) allgood = .false.
                if (abs(real(at(out, p), dp) - real(at(e, p), dp)) &
                    > TOL*(1.0_dp + abs(at(e, p)))) allgood = .false.
            end do
            call ok("Re[z**12] is the real part of z**12", allgood)
        end if

        e = z**num(arena, 13)
        call re_part(e, facts, out, good, why)
        call ok("z**13 exceeds the term bound", .not. good)
        if (.not. good) then
            call ok("the refusal names the term count", &
                index(why, "terms") > 0)
        end if

        e = (z**num(arena, 24))**num(arena, 24)
        call re_part(e, facts, out, good, why)
        call ok("nesting does not get round the bound", .not. good)

        e = z**num(arena, 12)*z**num(arena, 12)
        call re_part(e, facts, out, good, why)
        call ok("multiplying does not get round it either", .not. good)
    end subroutine test_expansion_is_bounded

    !> conjugate handles expressions complex_split refuses, on purpose.
    !>
    !> conj(w**n) = conj(w)**n needs no expansion, so the exponent bounds that
    !> stop the splitter do not apply. The check is that the wider answers are
    !> right -- against conjg in the oracle -- and that the splitter's refusal
    !> on the same input is still a refusal.
    subroutine test_conjugate_domain_is_wider()
        type(expr_t) :: e, c, out
        logical :: good, allgood
        character(:), allocatable :: why
        integer :: p

        e = z**num(arena, 0)
        call conjugate(e, facts, c, good, why)
        call ok("conj(z**0) is accepted", good)
        if (good) then
            allgood = .true.
            do p = 1, NPOINT
                if (abs(at(c, p) - conjg(at(e, p))) > TOL) allgood = .false.
            end do
            call ok("conj(z**0) agrees with conjg", allgood)
        end if
        call abs_of(e, facts, out, good, why)
        call ok("Abs(z**0) is still refused by the splitter", .not. good)

        e = z**num(arena, 30)
        call conjugate(e, facts, c, good, why)
        call ok("conj(z**30) is accepted", good)
        if (good) then
            allgood = .true.
            do p = 1, NPOINT
                if (abs(at(c, p) - conjg(at(e, p))) &
                    > TOL*(1.0_dp + abs(at(e, p)))) allgood = .false.
            end do
            call ok("conj(z**30) agrees with conjg", allgood)
        end if
        call abs_of(e, facts, out, good, why)
        call ok("Abs(z**30) is still refused by the splitter", .not. good)
    end subroutine test_conjugate_domain_is_wider

    !> Exact algebraic atoms participate in the rectangular boundary through
    !> FLINT's exact qqbar real and imaginary projections.
    subroutine test_algebraic_atoms()
        type(expr_t) :: real_root, imaginary_root, mixed_root
        type(expr_t) :: re, im, conjugated, expected
        type(str_t) :: base_text, real_text, imaginary_text, mixed_text
        type(str_t) :: imaginary_unit, minus_imaginary, expected_text
        logical :: good
        character(:), allocatable :: why

        base_text = algebraic_from_re_im("2", "0", good)
        real_text = algebraic_sqrt(chars(base_text), good)
        real_root = algebraic_expr(arena, chars(real_text), good)
        call ok("complex-domain algebraic atom has its node kind", &
            real_root%kind() == NK_ALGEBRAIC)
        call re_part(real_root, facts, re, good, why)
        call ok("real algebraic Re succeeds", good)
        if (good) call ok("real algebraic Re retains the atom", re == real_root)
        call im_part(real_root, facts, im, good, why)
        call ok("real algebraic Im succeeds", good)
        if (good) then
            expected_text = algebraic_im(chars(real_text), good)
            expected = algebraic_expr(arena, chars(expected_text), good)
            call ok("real algebraic Im matches bridge", im == expected)
        end if
        call conjugate(real_root, facts, conjugated, good, why)
        call ok("real algebraic conjugation succeeds", good)
        if (good) call ok("real algebraic conjugation is unchanged", &
            conjugated == real_root)

        base_text = algebraic_from_re_im("-2", "0", good)
        imaginary_text = algebraic_sqrt(chars(base_text), good)
        imaginary_root = algebraic_expr(arena, chars(imaginary_text), good)
        call im_part(imaginary_root, facts, im, good, why)
        call ok("pure-imaginary algebraic Im succeeds", good)
        if (good) then
            imaginary_unit = algebraic_i(good)
            minus_imaginary = algebraic_conjugate(chars(imaginary_unit), good)
            expected_text = algebraic_mul(chars(imaginary_text), &
                chars(minus_imaginary), good)
            expected = algebraic_expr(arena, chars(expected_text), good)
            call ok("pure-imaginary algebraic Im matches bridge", im == expected)
        end if
        call re_part(imaginary_root, facts, re, good, why)
        call ok("pure-imaginary algebraic Re succeeds", good)
        if (good) then
            expected_text = algebraic_re(chars(imaginary_text), good)
            expected = algebraic_expr(arena, chars(expected_text), good)
            call ok("pure-imaginary algebraic Re matches bridge", re == expected)
        end if

        base_text = algebraic_from_re_im("1", "1", good)
        mixed_text = base_text
        mixed_root = algebraic_expr(arena, chars(mixed_text), good)
        call re_part(mixed_root, facts, re, good, why)
        call ok("mixed algebraic real projection succeeds", good)
        if (good) then
            expected_text = algebraic_re(chars(mixed_text), good)
            expected = algebraic_expr(arena, chars(expected_text), good)
            call ok("mixed algebraic Re matches bridge", re == expected)
        end if
        call im_part(mixed_root, facts, im, good, why)
        call ok("mixed algebraic imaginary projection succeeds", good)
        if (good) then
            expected_text = algebraic_im(chars(mixed_text), good)
            expected = algebraic_expr(arena, chars(expected_text), good)
            call ok("mixed algebraic Im matches bridge", im == expected)
        end if
        call conjugate(mixed_root, facts, conjugated, good, why)
        call ok("mixed algebraic conjugation succeeds", good)
        if (good) then
            expected_text = algebraic_conjugate(chars(mixed_text), good)
            expected = algebraic_expr(arena, chars(expected_text), good)
            call ok("mixed algebraic conjugation matches bridge", &
                conjugated == expected)
        end if
    end subroutine test_algebraic_atoms

    subroutine test_cache_is_context_local()
        type(assumption_context_t) :: empty_facts
        type(expr_t) :: first_re, first_im, second_re, second_im
        type(expr_t) :: tangent, first_conjugate, second_conjugate
        logical :: good
        character(:), allocatable :: why

        call complex_split(z, facts, first_re, first_im, good, why)
        call ok("complex split cache source succeeds", good)
        if (good) then
            call complex_split(z, facts, second_re, second_im, good, why)
            call ok("cached complex split succeeds", good)
            if (good) call ok("cached pair retains both node ids", &
                first_re == second_re .and. first_im == second_im)
        end if

        call empty_facts%init(arena)
        call complex_split(z, empty_facts, second_re, second_im, good, why)
        call ok("complex split cache does not cross contexts", .not. good)

        tangent = func_one("tan", z)
        call conjugate(tangent, facts, first_conjugate, good, why)
        call ok("conjugate cache source succeeds", good)
        if (good) then
            call conjugate(tangent, facts, second_conjugate, good, why)
            call ok("cached conjugate succeeds", good)
            if (good) call ok("cached conjugate preserves the value", &
                abs(at(second_conjugate, 1) - conjg(at(tangent, 1))) < TOL)
        end if
        call conjugate(tangent, empty_facts, second_conjugate, good, why)
        call ok("conjugate cache does not cross contexts", .not. good)
    end subroutine test_cache_is_context_local

    ! ----------------------------------------------------- the oracle --

    !> Evaluate at sample point p, with x -> XS(p) and y -> YS(p).
    function at(e, p) result(v)
        type(expr_t), intent(in) :: e
        integer,      intent(in) :: p
        complex(dp)              :: v
        logical :: good

        v = ceval(e, XS(p), YS(p), good)
        if (.not. good) then
            print *, "  oracle could not evaluate a sample point"
            nfail = nfail + 1
            v = (0.0_dp, 0.0_dp)
        end if
    end function at

    !> A plain complex walker over the DAG. Deliberately independent of the
    !> module under test: it never splits anything.
    recursive function ceval(e, xv, yv, good) result(v)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(in)  :: xv, yv
        logical,      intent(out) :: good
        complex(dp)               :: v
        complex(dp) :: a, b
        character(:), allocatable :: name
        integer :: k
        integer(int64) :: n

        v = (0.0_dp, 0.0_dp)
        good = .true.

        select case (e%kind())
        case (NK_INT)
            v = cmplx(real(e%int_value(), dp), 0.0_dp, dp)
        case (NK_RAT)
            v = cmplx(real(e%int_value(), dp)/real(e%den_value(), dp), &
                0.0_dp, dp)
        case (NK_REAL)
            v = cmplx(e%real_value(), 0.0_dp, dp)
        case (NK_SYM)
            name = chars(e%name())
            if (name == "x") then
                v = cmplx(xv, 0.0_dp, dp)
            else if (name == "y") then
                v = cmplx(yv, 0.0_dp, dp)
            else
                good = .false.
            end if
        case (NK_CONST)
            name = chars(e%name())
            select case (name)
            case ("i"); v = (0.0_dp, 1.0_dp)
            case ("pi"); v = cmplx(acos(-1.0_dp), 0.0_dp, dp)
            case ("e"); v = cmplx(exp(1.0_dp), 0.0_dp, dp)
            case default; good = .false.
            end select
        case (NK_ADD)
            v = (0.0_dp, 0.0_dp)
            do k = 1, e%nargs()
                a = ceval(e%arg(k), xv, yv, good)
                if (.not. good) return
                v = v + a
            end do
        case (NK_MUL)
            v = (1.0_dp, 0.0_dp)
            do k = 1, e%nargs()
                a = ceval(e%arg(k), xv, yv, good)
                if (.not. good) return
                v = v*a
            end do
        case (NK_POW)
            a = ceval(e%arg(1), xv, yv, good)
            if (.not. good) return
            b = ceval(e%arg(2), xv, yv, good)
            if (.not. good) return
            if (aimag(b) == 0.0_dp .and. real(b, dp) == anint(real(b, dp))) then
                n = int(anint(real(b, dp)), int64)
                if (a == (0.0_dp, 0.0_dp)) then
                    good = n > 0_int64
                    if (.not. good) return
                end if
                v = a**int(n)
            else
                if (a == (0.0_dp, 0.0_dp)) then
                    good = .false.
                    return
                end if
                v = exp(b*log(a))
            end if
        case (NK_FUNC)
            v = ceval_func(e, xv, yv, good)
        case default
            good = .false.
        end select
    end function ceval

    recursive function ceval_func(e, xv, yv, good) result(v)
        type(expr_t), intent(in)  :: e
        real(dp),     intent(in)  :: xv, yv
        logical,      intent(out) :: good
        complex(dp)               :: v
        complex(dp) :: a, b
        character(:), allocatable :: name

        v = (0.0_dp, 0.0_dp)
        good = .true.
        name = chars(e%name())

        if (name == "atan2") then
            if (e%nargs() /= 2) then
                good = .false.
                return
            end if
            a = ceval(e%arg(1), xv, yv, good)
            if (.not. good) return
            b = ceval(e%arg(2), xv, yv, good)
            if (.not. good) return
            ! atan2 is real-only, so a complex argument here would mean the
            ! module produced an argument it had no right to.
            if (aimag(a) /= 0.0_dp .or. aimag(b) /= 0.0_dp) then
                good = .false.
                return
            end if
            if (real(a, dp) == 0.0_dp .and. real(b, dp) == 0.0_dp) then
                good = .false.
                return
            end if
            v = cmplx(atan2(real(a, dp), real(b, dp)), 0.0_dp, dp)
            return
        end if

        if (e%nargs() /= 1) then
            good = .false.
            return
        end if
        a = ceval(e%arg(1), xv, yv, good)
        if (.not. good) return

        select case (name)
        case ("exp"); v = exp(a)
        case ("sin"); v = sin(a)
        case ("cos"); v = cos(a)
        case ("tan"); v = sin(a)/cos(a)
        case ("sinh"); v = sinh(a)
        case ("cosh"); v = cosh(a)
        case ("tanh"); v = sinh(a)/cosh(a)
        case ("sqrt"); v = sqrt(a)
        case ("log")
            if (a == (0.0_dp, 0.0_dp)) then
                good = .false.
            else
                v = log(a)
            end if
        case default
            good = .false.
        end select
    end function ceval_func

end program test_fortsym_complexdom
