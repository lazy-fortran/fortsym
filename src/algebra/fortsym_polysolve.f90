module fortsym_polysolve
    ! Exact roots of a univariate polynomial equation `e == 0`.
    !
    ! The operation is deliberately narrow, because a root list is the kind of
    ! output nobody re-checks. Three separate stages each refuse rather than
    ! guess:
    !
    !   * extraction. The expression is walked structurally and turned into a
    !     coefficient vector of exact rationals. Anything that is not a sum,
    !     product or non-negative integer power of the variable and exact
    !     rational numbers is refused by name -- a floating-point literal, a
    !     second free symbol, sin(x), x**(1/2), 1/x. There is no attempt to
    !     "recognise" a polynomial hiding inside a function call.
    !   * solving. Powers of the variable are factored out, the Rational Root
    !     Theorem peels off exact rational roots one at a time (so multiplicity
    !     survives), and what is left is finished only if it has degree 0, 1 or
    !     2. Degree 3 and 4 by radicals are NOT implemented: doing them right
    !     needs the depressed substitution, the resolvent, and the casus
    !     irreducibilis branch where three real roots are only reachable through
    !     complex cube roots. Half of that is worse than none, so a remaining
    !     degree above 2 is an explicit refusal naming the degree.
    !   * verification. Every root that is about to be returned is substituted
    !     back into the *full* original coefficient vector and evaluated exactly
    !     in Q(sqrt(D)), the field the root actually lives in. It must come out
    !     identically zero in both components. One root that fails to verify
    !     fails the whole call; a partially verified list is never returned.
    !     This also audits the synthetic-division chain, since the check uses
    !     the untouched original coefficients rather than the reduced ones.
    !
    ! Roots are returned with multiplicity and over the complex numbers, so on
    ! success their count always equals the degree; that identity is asserted
    ! before returning. A negative discriminant produces `p + q*i*sqrt(m)`,
    ! which is an exact root, not an approximation.
    !
    ! All exact arithmetic is checked int64. An overflow is a refusal, never a
    ! wrapped value: a silently wrapped coefficient would produce roots of a
    ! different polynomial than the one the caller asked about.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        node_kind_name
    use fortsym_expr, only: expr_t, num, rat, i_expr, sqrt, is_valid, &
        same_arena, operator(+), operator(*)
    implicit none
    private

    public :: solve_polynomial

    integer(int64), parameter :: MAX_I64 = huge(0_int64)
    integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64

    ! Degree bound. Extraction multiplies polynomials, so an unbounded degree is
    ! an unbounded amount of work driven by user input.
    integer, parameter :: MAX_DEGREE = 16

    ! Above this the divisor enumeration behind the Rational Root Theorem costs
    ! more than it is worth. Skipping it loses completeness, never correctness:
    ! an unfound rational root simply leaves a higher remaining degree, which is
    ! then refused.
    integer(int64), parameter :: MAX_FACTORABLE = 1000000000000_int64
    integer, parameter :: MAX_DIVISORS = 1024
    integer, parameter :: MAX_CANDIDATES = 200000

    ! Trial-division bound for pulling square factors out of a discriminant.
    integer(int64), parameter :: SQUARE_FACTOR_LIMIT = 1000000_int64

    ! Structural recursion bound. `extract` walks the expression DAG with one
    ! stack frame per level, and each frame carries several allocatable arrays,
    ! so a machine-generated expression nested thousands of levels deep would
    ! overflow the process stack. Every other limit in this file is a refusal;
    ! so is this one. The previous 512-level cap rejected valid machine-built
    ! equations after roughly 256 nesting iterations; 4096 accepts the tested
    ! 1500-iteration envelope while refusing the old crash range near 12000
    ! iterations (which reaches about 24000 expression levels).
    integer, parameter :: MAX_EXPR_DEPTH = 4096

    !> An exact rational in lowest terms with a positive denominator.
    type :: rat_t
        integer(int64) :: n = 0_int64
        integer(int64) :: d = 1_int64
    end type rat_t

    !> An element p + q*s of Q(s) where s*s = disc. disc = 0 with q = 0 is an
    !> ordinary rational, which is how one verifier covers both root shapes.
    type :: surd_t
        type(rat_t)    :: p
        type(rat_t)    :: q
        integer(int64) :: disc = 0_int64
    end type surd_t

contains

    !> Exact roots of `e == 0` in `var`, with multiplicity, over the complex
    !> numbers. On refusal `roots` is allocated empty and `why` says what
    !> stopped the solve.
    subroutine solve_polynomial(a, e, var, roots, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        type(expr_t), allocatable, intent(out)   :: roots(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t), allocatable  :: c(:)
        type(surd_t), allocatable :: found(:)
        integer :: k

        allocate (roots(0))
        ok = .false.
        why = ""

        if (.not. is_valid(e)) then
            why = "the equation is not a valid expression"
            return
        end if
        if (.not. is_valid(var)) then
            why = "the solve variable is not a valid expression"
            return
        end if
        if (.not. same_arena(e, var)) then
            why = "equation and variable live in different arenas"
            return
        end if
        if (var%kind() /= NK_SYM) then
            why = "the solve variable must be a symbol, not "// &
                  chars(node_kind_name(var%kind()))
            return
        end if
        if (.not. associated(e%a, a)) then
            why = "the equation does not belong to the arena passed in"
            return
        end if

        call extract(e%a, e%id, chars(var%name()), c, ok, why)
        if (.not. ok) return
        call poly_trim(c)

        if (size(c) == 0) then
            ok = .false.
            why = "the equation reduces to 0 == 0: every value of "// &
                  chars(var%name())//" is a root, so there is no root list"
            return
        end if

        call solve_coefficients(c, found, ok, why)
        if (.not. ok) return

        ! Independent of how each root was produced, every one of them must
        ! annihilate the original coefficient vector exactly.
        call verify_all(c, found, ok, why)
        if (.not. ok) return

        if (size(found) /= size(c) - 1) then
            ok = .false.
            why = "internal check failed: root count does not match the degree"
            return
        end if

        deallocate (roots)
        allocate (roots(size(found)))
        do k = 1, size(found)
            roots(k) = root_expr(a, found(k))
            if (.not. is_valid(roots(k))) then
                deallocate (roots)
                allocate (roots(0))
                ok = .false.
                why = "a verified root could not be built as an expression"
                return
            end if
        end do
        ok = .true.
    end subroutine solve_polynomial

    ! ----------------------------------------------------------- extraction --

    !> Structural walk turning a node into ascending coefficients: c(k) is the
    !> coefficient of var**(k-1). Every node kind that is not provably a
    !> rational polynomial in var is refused with the reason.
    subroutine extract(a, id, vname, c, ok, why)
        type(arena_t),             intent(in)  :: a
        integer,                   intent(in)  :: id
        character(*),              intent(in)  :: vname
        type(rat_t), allocatable,  intent(out) :: c(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        call extract_at(a, id, vname, 1, c, ok, why)
    end subroutine extract

    !> The recursive body of `extract`. `depth` counts nesting levels and is
    !> refused past MAX_EXPR_DEPTH, so no legal input can exhaust the stack.
    recursive subroutine extract_at(a, id, vname, depth, c, ok, why)
        type(arena_t),             intent(in)  :: a
        integer,                   intent(in)  :: id
        character(*),              intent(in)  :: vname
        integer,                   intent(in)  :: depth
        type(rat_t), allocatable,  intent(out) :: c(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(rat_t), allocatable :: part(:), acc(:)
        type(rat_t)    :: r
        integer        :: k, kind
        integer(int64) :: expo
        character(len=16) :: text

        ok = .false.
        why = ""
        allocate (c(0))
        if (depth > MAX_EXPR_DEPTH) then
            write (text, '(i0)') MAX_EXPR_DEPTH
            why = "the expression nests deeper than the supported maximum "// &
                  "of "//trim(text)//" levels, which the structural walk "// &
                  "cannot descend without exhausting the stack"
            return
        end if
        kind = a%kind_of(id)

        select case (kind)

        case (NK_INT)
            if (a%num_of(id) == MIN_I64) then
                why = "integer coefficient is too large for checked "// &
                      "64-bit exact arithmetic"
                return
            end if
            r%n = a%num_of(id)
            r%d = 1_int64
            c = [r]
            ok = .true.

        case (NK_RAT)
            call rat_make(a%num_of(id), a%den_of(id), r, ok)
            if (.not. ok) then
                why = "rational coefficient could not be normalised in "// &
                      "checked 64-bit arithmetic"
                return
            end if
            c = [r]

        case (NK_SYM)
            if (chars(a%name_of(id)) == vname) then
                deallocate (c)
                allocate (c(2))
                c(1) = rat_of(0_int64)
                c(2) = rat_of(1_int64)
                ok = .true.
            else
                why = "expression contains the second free symbol "// &
                      chars(a%name_of(id))// &
                      ": solve_polynomial handles one variable with "// &
                      "rational coefficients only"
            end if

        case (NK_ADD)
            allocate (acc(1))
            acc(1) = rat_of(0_int64)
            do k = 1, a%nargs_of(id)
                call extract_at(a, a%arg_of(id, k), vname, depth + 1, part, &
                                ok, why)
                if (.not. ok) return
                call poly_add(acc, part, ok, why)
                if (.not. ok) return
            end do
            call move_alloc(acc, c)
            ok = .true.

        case (NK_MUL)
            allocate (acc(1))
            acc(1) = rat_of(1_int64)
            do k = 1, a%nargs_of(id)
                call extract_at(a, a%arg_of(id, k), vname, depth + 1, part, &
                                ok, why)
                if (.not. ok) return
                call poly_mul(acc, part, ok, why)
                if (.not. ok) return
            end do
            call move_alloc(acc, c)
            ok = .true.

        case (NK_POW)
            if (a%kind_of(a%arg_of(id, 2)) /= NK_INT) then
                why = "exponent is not a plain integer, so the expression "// &
                      "is not a polynomial in "//vname
                return
            end if
            expo = a%num_of(a%arg_of(id, 2))
            call extract_at(a, a%arg_of(id, 1), vname, depth + 1, part, ok, why)
            if (.not. ok) return
            call poly_pow(part, expo, c, ok, why)

        case (NK_REAL)
            why = "expression contains a floating-point coefficient: an "// &
                  "exact solver will not treat a rounded decimal as a rational"

        case (NK_CONST)
            why = "expression contains the constant "//chars(a%name_of(id))// &
                  ", which is not a rational coefficient"

        case (NK_FUNC)
            why = "expression contains the function "// &
                  chars(a%name_of(id))//", so it is not a polynomial in "//vname

        case (NK_BIG_INT, NK_BIG_RAT)
            why = "expression contains an arbitrary-precision coefficient, "// &
                  "beyond the checked 64-bit exact arithmetic used here"

        case default
            why = "unsupported node kind "//chars(node_kind_name(kind))// &
                  " in the equation"

        end select
    end subroutine extract_at

    ! ------------------------------------------------- polynomial machinery --

    !> Drop leading zero coefficients. An all-zero polynomial becomes size 0,
    !> which the caller reports as the identically-true equation.
    subroutine poly_trim(c)
        type(rat_t), allocatable, intent(inout) :: c(:)
        type(rat_t), allocatable :: shorter(:)
        integer :: top

        top = size(c)
        do while (top >= 1)
            if (c(top)%n /= 0_int64) exit
            top = top - 1
        end do
        allocate (shorter(top))
        if (top > 0) shorter(1:top) = c(1:top)
        call move_alloc(shorter, c)
    end subroutine poly_trim

    subroutine poly_add(acc, other, ok, why)
        type(rat_t), allocatable,  intent(inout) :: acc(:)
        type(rat_t),               intent(in)    :: other(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t), allocatable :: sum(:)
        type(rat_t) :: bumped
        integer :: k, n

        ok = .false.
        why = ""
        n = max(size(acc), size(other))
        allocate (sum(n))
        do k = 1, n
            sum(k) = rat_of(0_int64)
        end do
        do k = 1, size(acc)
            sum(k) = acc(k)
        end do
        do k = 1, size(other)
            call rat_add(sum(k), other(k), bumped, ok)
            if (ok) sum(k) = bumped
            if (.not. ok) then
                why = "coefficient addition overflowed checked 64-bit "// &
                      "exact arithmetic"
                return
            end if
        end do
        call move_alloc(sum, acc)
        ok = .true.
    end subroutine poly_add

    subroutine poly_mul(acc, other, ok, why)
        type(rat_t), allocatable,  intent(inout) :: acc(:)
        type(rat_t),               intent(in)    :: other(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t), allocatable :: prod(:)
        type(rat_t) :: term, bumped
        integer :: i, j, n
        character(len=16) :: text

        ok = .false.
        why = ""
        if (size(acc) == 0 .or. size(other) == 0) then
            allocate (prod(0))
            call move_alloc(prod, acc)
            ok = .true.
            return
        end if
        n = size(acc) + size(other) - 1
        if (n - 1 > MAX_DEGREE) then
            write (text, '(i0)') MAX_DEGREE
            why = "polynomial degree exceeds the supported maximum of "// &
                  trim(text)
            return
        end if
        allocate (prod(n))
        do i = 1, n
            prod(i) = rat_of(0_int64)
        end do
        do i = 1, size(acc)
            do j = 1, size(other)
                call rat_mul(acc(i), other(j), term, ok)
                if (.not. ok) then
                    why = "coefficient multiplication overflowed checked "// &
                          "64-bit exact arithmetic"
                    return
                end if
                call rat_add(prod(i + j - 1), term, bumped, ok)
                if (ok) prod(i + j - 1) = bumped
                if (.not. ok) then
                    why = "coefficient addition overflowed checked 64-bit "// &
                          "exact arithmetic"
                    return
                end if
            end do
        end do
        call move_alloc(prod, acc)
        ok = .true.
    end subroutine poly_mul

    !> base**expo. A negative exponent is only a polynomial when the base is a
    !> nonzero constant; anything else is a rational function and is refused.
    subroutine poly_pow(base, expo, c, ok, why)
        type(rat_t),               intent(in)  :: base(:)
        integer(int64),            intent(in)  :: expo
        type(rat_t), allocatable,  intent(out) :: c(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(rat_t), allocatable :: acc(:), factor(:)
        type(rat_t) :: inv
        integer(int64) :: k, times

        ok = .false.
        why = ""
        allocate (c(0))

        if (expo < 0_int64) then
            if (size(base) /= 1) then
                why = "a negative power of a non-constant base makes the "// &
                      "expression a rational function, not a polynomial"
                return
            end if
            if (base(1)%n == 0_int64) then
                why = "the expression divides by zero"
                return
            end if
            call rat_make(base(1)%d, base(1)%n, inv, ok)
            if (.not. ok) then
                why = "reciprocal coefficient overflowed checked 64-bit "// &
                      "exact arithmetic"
                return
            end if
            allocate (factor(1))
            factor(1) = inv
            times = -expo
        else
            allocate (factor(size(base)))
            if (size(base) > 0) factor = base
            times = expo
        end if

        if (times > int(MAX_DEGREE, int64) .and. size(factor) > 1) then
            why = "polynomial degree exceeds the supported maximum"
            return
        end if
        if (times > 64_int64) then
            why = "exponent is too large to expand exactly"
            return
        end if

        allocate (acc(1))
        acc(1) = rat_of(1_int64)
        do k = 1_int64, times
            call poly_mul(acc, factor, ok, why)
            if (.not. ok) return
        end do
        call move_alloc(acc, c)
        ok = .true.
    end subroutine poly_pow

    !> Horner evaluation of ascending coefficients at an element of Q(sqrt(disc)).
    subroutine poly_eval_surd(c, x, value, ok)
        type(rat_t),  intent(in)  :: c(:)
        type(surd_t), intent(in)  :: x
        type(surd_t), intent(out) :: value
        logical,      intent(out) :: ok
        type(surd_t) :: shifted
        type(rat_t)  :: bumped
        integer :: k

        value%p = rat_of(0_int64)
        value%q = rat_of(0_int64)
        value%disc = x%disc
        ok = .true.
        do k = size(c), 1, -1
            call surd_mul(value, x, shifted, ok)
            if (.not. ok) return
            call rat_add(shifted%p, c(k), bumped, ok)
            if (.not. ok) return
            value = shifted
            value%p = bumped
        end do
    end subroutine poly_eval_surd

    !> Synthetic division of c by (x - r). The remainder is returned so the
    !> caller can insist it is zero rather than assume it.
    subroutine poly_deflate(c, r, quot, remainder, ok)
        type(rat_t),              intent(in)  :: c(:)
        type(rat_t),              intent(in)  :: r
        type(rat_t), allocatable, intent(out) :: quot(:)
        type(rat_t),              intent(out) :: remainder
        logical,                  intent(out) :: ok
        type(rat_t) :: term
        integer     :: n, k

        ok = .false.
        remainder = rat_of(0_int64)
        n = size(c) - 1
        allocate (quot(max(n, 0)))
        if (n < 1) return

        quot(n) = c(n + 1)
        do k = n - 1, 1, -1
            call rat_mul(r, quot(k + 1), term, ok)
            if (.not. ok) return
            call rat_add(c(k + 1), term, quot(k), ok)
            if (.not. ok) return
        end do
        call rat_mul(r, quot(1), term, ok)
        if (.not. ok) return
        call rat_add(c(1), term, remainder, ok)
    end subroutine poly_deflate

    ! -------------------------------------------------------------- solving --

    !> Zeros first, then rational roots peeled one at a time, then whatever
    !> linear or quadratic factor is left.
    subroutine solve_coefficients(c, found, ok, why)
        type(rat_t),               intent(in)  :: c(:)
        type(surd_t), allocatable, intent(out) :: found(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(rat_t), allocatable  :: work(:), quot(:)
        type(surd_t), allocatable :: pair(:)
        type(rat_t)               :: r, remainder
        integer :: k, zeros, degree
        logical :: peeled
        character(len=16) :: text

        ok = .false.
        why = ""
        allocate (found(0))

        ! Factor out var**zeros. Each such factor contributes one root at 0.
        zeros = 0
        do k = 1, size(c)
            if (c(k)%n /= 0_int64) exit
            zeros = zeros + 1
        end do
        allocate (work(size(c) - zeros))
        work = c(zeros + 1:size(c))
        do k = 1, zeros
            call append_root(found, surd_rational(rat_of(0_int64)))
        end do

        ! Rational Root Theorem, repeatedly, so a repeated root is peeled as
        ! many times as it divides.
        do
            if (size(work) - 1 < 1) exit
            call find_rational_root(work, r, peeled)
            if (.not. peeled) exit
            call poly_deflate(work, r, quot, remainder, ok)
            if (.not. ok) then
                why = "deflation by an exact rational root overflowed "// &
                      "checked 64-bit exact arithmetic"
                return
            end if
            if (remainder%n /= 0_int64) then
                ok = .false.
                why = "internal check failed: deflation left a nonzero "// &
                      "remainder at a root"
                return
            end if
            call append_root(found, surd_rational(r))
            call move_alloc(quot, work)
        end do

        degree = size(work) - 1
        select case (degree)
        case (0)
            ok = .true.
        case (1)
            call solve_linear(work, r, ok, why)
            if (.not. ok) return
            call append_root(found, surd_rational(r))
        case (2)
            call solve_quadratic(work, pair, ok, why)
            if (.not. ok) return
            do k = 1, size(pair)
                call append_root(found, pair(k))
            end do
        case default
            ok = .false.
            write (text, '(i0)') degree
            why = "degree "//trim(text)//" remains after factoring out "// &
                  "powers of the variable and peeling exact rational roots: "// &
                  "solution by radicals above degree 2 is not implemented, "// &
                  "and a partial cubic or quartic would be worse than none"
        end select
    end subroutine solve_coefficients

    subroutine solve_linear(c, r, ok, why)
        type(rat_t),               intent(in)  :: c(:)
        type(rat_t),               intent(out) :: r
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(rat_t) :: quotient

        ok = .false.
        why = ""
        r = rat_of(0_int64)
        call rat_div(c(1), c(2), quotient, ok)
        if (.not. ok) then
            why = "linear root overflowed checked 64-bit exact arithmetic"
            return
        end if
        call rat_neg(quotient, r, ok)
        if (.not. ok) why = "linear root overflowed checked 64-bit arithmetic"
    end subroutine solve_linear

    !> a*x**2 + b*x + c with exact rational a, b, c. The discriminant is formed
    !> exactly; when it is a perfect square both roots are rational, otherwise
    !> they are exact elements of Q(sqrt(D)) with D squarefree as far as trial
    !> division reaches. A negative discriminant gives the complex pair, which
    !> is exact too -- there is no rounding anywhere on this path.
    subroutine solve_quadratic(c, pair, ok, why)
        type(rat_t),               intent(in)  :: c(:)
        type(surd_t), allocatable, intent(out) :: pair(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(rat_t) :: bb, ac4, disc, twoa, base, half, root, tmp
        integer(int64) :: dnum, squarefree, factored
        logical :: exact_square

        ok = .false.
        why = ""
        allocate (pair(0))

        call rat_mul(c(2), c(2), bb, ok)
        if (ok) call rat_mul(c(3), c(1), ac4, ok)
        if (ok) call rat_scale(ac4, -4_int64, tmp, ok)
        if (ok) call rat_add(bb, tmp, disc, ok)
        if (ok) call rat_scale(c(3), 2_int64, twoa, ok)
        if (.not. ok) then
            why = "discriminant overflowed checked 64-bit exact arithmetic"
            return
        end if

        ! -b/(2a) is the common part of both roots.
        call rat_div(c(2), twoa, tmp, ok)
        if (ok) call rat_neg(tmp, base, ok)
        if (.not. ok) then
            why = "quadratic root overflowed checked 64-bit exact arithmetic"
            return
        end if

        ! With D = n/m in lowest terms, sqrt(D) = sqrt(n*m)/m. That keeps the
        ! radicand an integer without ever leaving exact arithmetic.
        call i_mul(disc%n, disc%d, dnum, ok)
        if (.not. ok) then
            why = "discriminant overflowed checked 64-bit exact arithmetic"
            return
        end if
        call rat_make(1_int64, disc%d, tmp, ok)
        if (ok) call rat_div(tmp, twoa, half, ok)
        if (.not. ok) then
            why = "quadratic root overflowed checked 64-bit exact arithmetic"
            return
        end if

        if (dnum == 0_int64) then
            ! Double root. Returned twice, so the count still equals the degree.
            deallocate (pair)
            allocate (pair(2))
            pair(1) = surd_rational(base)
            pair(2) = surd_rational(base)
            ok = .true.
            return
        end if

        call square_split(dnum, factored, squarefree, exact_square, ok)
        if (.not. ok) then
            why = "the discriminant has no representable magnitude in "// &
                  "checked 64-bit exact arithmetic"
            return
        end if
        call rat_scale(half, factored, tmp, ok)
        if (.not. ok) then
            why = "quadratic root overflowed checked 64-bit exact arithmetic"
            return
        end if
        half = tmp

        if (exact_square) then
            call rat_add(base, half, root, ok)
            if (.not. ok) then
                why = "quadratic root overflowed checked 64-bit arithmetic"
                return
            end if
            deallocate (pair)
            allocate (pair(2))
            pair(1) = surd_rational(root)
            call rat_neg(half, tmp, ok)
            if (.not. ok) then
                why = "quadratic root overflowed checked 64-bit arithmetic"
                return
            end if
            call rat_add(base, tmp, root, ok)
            if (.not. ok) then
                why = "quadratic root overflowed checked 64-bit arithmetic"
                return
            end if
            pair(2) = surd_rational(root)
            ok = .true.
            return
        end if

        deallocate (pair)
        allocate (pair(2))
        pair(1)%p = base
        pair(1)%q = half
        pair(1)%disc = squarefree
        pair(2)%p = base
        call rat_neg(half, pair(2)%q, ok)
        if (.not. ok) then
            why = "quadratic root overflowed checked 64-bit exact arithmetic"
            return
        end if
        pair(2)%disc = squarefree
        ok = .true.
    end subroutine solve_quadratic

    !> Rational Root Theorem: p/q with p dividing the constant term and q the
    !> leading one, tested exactly. Returns the first root found, or reports
    !> that none was found -- which is a statement about this search, not a
    !> proof that none exists, and is why nothing downstream assumes otherwise.
    subroutine find_rational_root(c, r, found)
        type(rat_t), intent(in)  :: c(:)
        type(rat_t), intent(out) :: r
        logical,     intent(out) :: found
        integer(int64), allocatable :: pdiv(:), qdiv(:)
        type(surd_t)   :: x, value
        type(rat_t)    :: candidate
        integer(int64) :: sgn, magnitude
        integer        :: i, j, tried
        logical        :: good

        found = .false.
        r = rat_of(0_int64)
        if (size(c) < 2) return
        if (c(1)%n == 0_int64) return

        call abs_i64(c(1)%n, magnitude, good)
        if (.not. good) return
        call divisors(magnitude, pdiv, good)
        if (.not. good) return
        call abs_i64(c(size(c))%n, magnitude, good)
        if (.not. good) return
        call divisors(magnitude, qdiv, good)
        if (.not. good) return
        if (size(pdiv)*size(qdiv)*2 > MAX_CANDIDATES) return

        tried = 0
        do i = 1, size(pdiv)
            do j = 1, size(qdiv)
                do sgn = 1_int64, -1_int64, -2_int64
                    tried = tried + 1
                    if (tried > MAX_CANDIDATES) return
                    call rat_make(sgn*pdiv(i), qdiv(j), candidate, good)
                    if (.not. good) cycle
                    x = surd_rational(candidate)
                    call poly_eval_surd(c, x, value, good)
                    if (.not. good) cycle
                    if (value%p%n == 0_int64) then
                        r = candidate
                        found = .true.
                        return
                    end if
                end do
            end do
        end do
    end subroutine find_rational_root

    ! --------------------------------------------------------- verification --

    !> Substitute every candidate back into the untouched original coefficients
    !> and require an exact zero in both components of Q(sqrt(D)).
    subroutine verify_all(c, found, ok, why)
        type(rat_t),               intent(in)  :: c(:)
        type(surd_t),              intent(in)  :: found(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(surd_t) :: value
        integer :: k
        character(len=16) :: text

        why = ""
        do k = 1, size(found)
            call poly_eval_surd(c, found(k), value, ok)
            if (.not. ok) then
                why = "verifying a candidate root overflowed checked "// &
                      "64-bit exact arithmetic, so it cannot be returned"
                return
            end if
            if (value%p%n /= 0_int64 .or. value%q%n /= 0_int64) then
                ok = .false.
                write (text, '(i0)') k
                why = "candidate root "//trim(text)//" does not satisfy the "// &
                      "equation exactly; the whole solve is refused"
                return
            end if
        end do
        ok = .true.
    end subroutine verify_all

    ! ------------------------------------------------- expression rendering --

    !> p + q*sqrt(disc), with the negative-discriminant case rendered through
    !> the imaginary unit rather than a square root of a negative number.
    function root_expr(a, root) result(e)
        type(arena_t), target, intent(inout) :: a
        type(surd_t),          intent(in)    :: root
        type(expr_t) :: e, radical

        e = rat_expr(a, root%p)
        if (root%q%n == 0_int64) return

        if (root%disc > 0_int64) then
            radical = sqrt(num(a, root%disc))
        else if (root%disc == -1_int64) then
            radical = i_expr(a)
        else
            radical = i_expr(a)*sqrt(num(a, -root%disc))
        end if
        e = e + rat_expr(a, root%q)*radical
    end function root_expr

    function rat_expr(a, r) result(e)
        type(arena_t), target, intent(inout) :: a
        type(rat_t),           intent(in)    :: r
        type(expr_t) :: e

        if (r%d == 1_int64) then
            e = num(a, r%n)
        else
            e = rat(a, r%n, r%d)
        end if
    end function rat_expr

    ! ------------------------------------------------------ exact arithmetic --

    pure function rat_of(v) result(r)
        integer(int64), intent(in) :: v
        type(rat_t) :: r
        r%n = v
        r%d = 1_int64
    end function rat_of

    pure function surd_rational(r) result(s)
        type(rat_t), intent(in) :: r
        type(surd_t) :: s
        s%p = r
        s%q = rat_of(0_int64)
        s%disc = 0_int64
    end function surd_rational

    subroutine append_root(list, item)
        type(surd_t), allocatable, intent(inout) :: list(:)
        type(surd_t),              intent(in)    :: item
        type(surd_t), allocatable :: bigger(:)

        allocate (bigger(size(list) + 1))
        if (size(list) > 0) bigger(1:size(list)) = list
        bigger(size(list) + 1) = item
        call move_alloc(bigger, list)
    end subroutine append_root

    !> (p1 + q1 s)(p2 + q2 s) with s*s = disc. The two factors must agree on
    !> disc, or the product is meaningless; mismatched discriminants report a
    !> failure rather than silently picking one.
    subroutine surd_mul(x, y, z, ok)
        type(surd_t), intent(in)  :: x, y
        type(surd_t), intent(out) :: z
        logical,      intent(out) :: ok
        type(rat_t) :: t1, t2, t3

        ok = .false.
        z%p = rat_of(0_int64)
        z%q = rat_of(0_int64)
        z%disc = x%disc
        if (x%disc /= y%disc) then
            if (x%q%n /= 0_int64 .and. y%q%n /= 0_int64) return
            if (x%q%n == 0_int64) z%disc = y%disc
        end if

        call rat_mul(x%p, y%p, t1, ok)
        if (.not. ok) return
        call rat_mul(x%q, y%q, t2, ok)
        if (.not. ok) return
        call rat_scale(t2, z%disc, t3, ok)
        if (.not. ok) return
        call rat_add(t1, t3, z%p, ok)
        if (.not. ok) return

        call rat_mul(x%p, y%q, t1, ok)
        if (.not. ok) return
        call rat_mul(x%q, y%p, t2, ok)
        if (.not. ok) return
        call rat_add(t1, t2, z%q, ok)
    end subroutine surd_mul

    subroutine rat_make(n, d, r, ok)
        integer(int64), intent(in)  :: n, d
        type(rat_t),    intent(out) :: r
        logical,        intent(out) :: ok
        integer(int64) :: nn, dd, g

        r = rat_of(0_int64)
        ok = .false.
        if (d == 0_int64) return
        if (n == MIN_I64 .or. d == MIN_I64) return

        nn = n
        dd = d
        if (dd < 0_int64) then
            nn = -nn
            dd = -dd
        end if
        if (nn == 0_int64) then
            r = rat_of(0_int64)
            ok = .true.
            return
        end if
        g = gcd_i64(abs(nn), dd)
        r%n = nn/g
        r%d = dd/g
        ok = .true.
    end subroutine rat_make

    subroutine rat_add(x, y, z, ok)
        type(rat_t), intent(in)  :: x, y
        type(rat_t), intent(out) :: z
        logical,     intent(out) :: ok
        integer(int64) :: g, xs, ys, t1, t2, n, d

        z = rat_of(0_int64)
        g = gcd_i64(x%d, y%d)
        xs = x%d/g
        ys = y%d/g
        call i_mul(x%n, ys, t1, ok)
        if (.not. ok) return
        call i_mul(y%n, xs, t2, ok)
        if (.not. ok) return
        call i_add(t1, t2, n, ok)
        if (.not. ok) return
        call i_mul(x%d, ys, d, ok)
        if (.not. ok) return
        call rat_make(n, d, z, ok)
    end subroutine rat_add

    subroutine rat_mul(x, y, z, ok)
        type(rat_t), intent(in)  :: x, y
        type(rat_t), intent(out) :: z
        logical,     intent(out) :: ok
        integer(int64) :: g1, g2, n, d

        z = rat_of(0_int64)
        ok = .false.
        if (x%n == 0_int64 .or. y%n == 0_int64) then
            ok = .true.
            return
        end if
        if (x%n == MIN_I64 .or. y%n == MIN_I64) return
        g1 = gcd_i64(abs(x%n), y%d)
        g2 = gcd_i64(abs(y%n), x%d)
        call i_mul(x%n/g1, y%n/g2, n, ok)
        if (.not. ok) return
        call i_mul(x%d/g2, y%d/g1, d, ok)
        if (.not. ok) return
        call rat_make(n, d, z, ok)
    end subroutine rat_mul

    subroutine rat_div(x, y, z, ok)
        type(rat_t), intent(in)  :: x, y
        type(rat_t), intent(out) :: z
        logical,     intent(out) :: ok
        type(rat_t) :: inv

        z = rat_of(0_int64)
        ok = .false.
        if (y%n == 0_int64) return
        call rat_make(y%d, y%n, inv, ok)
        if (.not. ok) return
        call rat_mul(x, inv, z, ok)
    end subroutine rat_div

    subroutine rat_scale(x, k, z, ok)
        type(rat_t),    intent(in)  :: x
        integer(int64), intent(in)  :: k
        type(rat_t),    intent(out) :: z
        logical,        intent(out) :: ok

        call rat_mul(x, rat_of(k), z, ok)
    end subroutine rat_scale

    subroutine rat_neg(x, z, ok)
        type(rat_t), intent(in)  :: x
        type(rat_t), intent(out) :: z
        logical,     intent(out) :: ok

        z = rat_of(0_int64)
        ok = .false.
        if (x%n == MIN_I64) return
        z%n = -x%n
        z%d = x%d
        ok = .true.
    end subroutine rat_neg

    subroutine i_add(x, y, z, ok)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: z
        logical,        intent(out) :: ok

        z = 0_int64
        ok = .false.
        if (y > 0_int64) then
            if (x > MAX_I64 - y) return
        end if
        if (y < 0_int64) then
            if (x < MIN_I64 - y) return
        end if
        z = x + y
        ok = .true.
    end subroutine i_add

    subroutine i_mul(x, y, z, ok)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: z
        logical,        intent(out) :: ok

        z = 0_int64
        ok = .false.
        if (x == MIN_I64 .or. y == MIN_I64) return
        if (x == 0_int64 .or. y == 0_int64) then
            ok = .true.
            return
        end if
        if (abs(x) > MAX_I64/abs(y)) return
        z = x*y
        ok = .true.
    end subroutine i_mul

    pure function gcd_i64(x, y) result(g)
        integer(int64), intent(in) :: x, y
        integer(int64) :: g, a1, b1, t

        a1 = abs(x)
        b1 = abs(y)
        do while (b1 /= 0_int64)
            t = mod(a1, b1)
            a1 = b1
            b1 = t
        end do
        g = a1
        if (g == 0_int64) g = 1_int64
    end function gcd_i64

    !> |x| as a checked operation. |MIN_I64| is not representable, so it is a
    !> reported failure rather than a fabricated magnitude: a caller that got a
    !> substitute value back would go on to reason about the wrong integer.
    pure subroutine abs_i64(x, v, ok)
        integer(int64), intent(in)  :: x
        integer(int64), intent(out) :: v
        logical,        intent(out) :: ok

        v = 0_int64
        ok = .false.
        if (x == MIN_I64) return
        v = abs(x)
        ok = .true.
    end subroutine abs_i64

    !> Split |v| into factor**2 * squarefree, keeping the sign on squarefree.
    !> Trial division stops at a bound, so for a very large v the remaining part
    !> may still contain a square; that only leaves sqrt() less reduced, it
    !> never changes the value, and the verifier uses whatever comes out here.
    subroutine square_split(v, factor, squarefree, exact_square, ok)
        integer(int64), intent(in)  :: v
        integer(int64), intent(out) :: factor, squarefree
        logical,        intent(out) :: exact_square
        logical,        intent(out) :: ok
        integer(int64) :: m, p, sgn

        factor = 1_int64
        squarefree = 0_int64
        exact_square = .false.
        sgn = 1_int64
        if (v < 0_int64) sgn = -1_int64
        call abs_i64(v, m, ok)
        if (.not. ok) return

        p = 2_int64
        do while (p <= SQUARE_FACTOR_LIMIT)
            if (p*p > m) exit
            do while (mod(m, p*p) == 0_int64)
                m = m/(p*p)
                factor = factor*p
            end do
            p = p + 1_int64
        end do

        squarefree = sgn*m
        ok = .true.
        if (sgn > 0_int64) then
            if (m == 1_int64) exact_square = .true.
        end if
    end subroutine square_split

    !> All positive divisors of v, or a refusal when v is too large to factor
    !> cheaply or has too many divisors to enumerate.
    subroutine divisors(v, list, ok)
        integer(int64),              intent(in)  :: v
        integer(int64), allocatable, intent(out) :: list(:)
        logical,                     intent(out) :: ok
        integer(int64) :: buffer(MAX_DIVISORS)
        integer(int64) :: d
        integer :: n

        ok = .false.
        allocate (list(0))
        if (v <= 0_int64) return
        if (v > MAX_FACTORABLE) return

        n = 0
        d = 1_int64
        do while (d*d <= v)
            if (mod(v, d) == 0_int64) then
                if (n + 2 > MAX_DIVISORS) return
                n = n + 1
                buffer(n) = d
                if (d /= v/d) then
                    n = n + 1
                    buffer(n) = v/d
                end if
            end if
            d = d + 1_int64
        end do

        deallocate (list)
        allocate (list(n))
        if (n > 0) list = buffer(1:n)
        ok = .true.
    end subroutine divisors

end module fortsym_polysolve
