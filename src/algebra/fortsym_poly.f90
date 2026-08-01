module fortsym_poly
    ! Polynomial and rational-function manipulation over the exact rationals.
    !
    ! An expression is read into a *rational function* n/d, where n and d are
    ! sparse multivariate polynomials with checked int64 rational coefficients.
    ! The indeterminates are not only symbols: any subexpression the reader
    ! cannot decompose -- sin(x), pi, x**(1/2), an opaque head -- becomes a
    ! generator of the polynomial ring. That is sound because every identity
    ! proved in the generator ring maps forward under the ring homomorphism
    ! that sends each generator to the value it stands for. It is also what
    ! Wolfram does: Coefficient[Sin[x]*y, y] is Sin[x], not a refusal.
    !
    ! What is deliberately NOT done:
    !
    !   * no floating-point coefficient is ever admitted. A rounded decimal is
    !     not a rational, and pretending otherwise makes an exact answer that
    !     is exactly wrong.
    !   * no coefficient arithmetic wraps. Every add/multiply is checked and an
    !     overflow is a refusal naming itself, never a silent wrap.
    !   * factorisation is complete or refused. A partly factored polynomial
    !     presented as "the factorisation" is a wrong answer, so Factor only
    !     answers when every irreducible factor is *proved* irreducible.
    !
    ! Methods, with sources:
    !
    !   * multivariate GCD: primitive polynomial remainder sequence. The
    !     content is removed with respect to the main variable, the PRS is run
    !     with pseudo-division, and each remainder is made primitive again to
    !     stop coefficient growth. This is the "primitive PRS" of Collins,
    !     Geddes/Czapor/Labahn "Algorithms for Computer Algebra" (1992)
    !     Algorithm 7.2, the simple correct route noted alongside Brown's
    !     dense modular algorithm (Brown, JACM 18(4), 1971). Brown's and
    !     Zippel's (Zippel, EUROSAM 1979) modular algorithms are faster and are
    !     not implemented; the PRS answers the same question.
    !   * squarefree decomposition: Yun's algorithm (Yun, SYMSAC 1976), which
    !     gets the full a_1 a_2^2 a_3^3 ... splitting from gcd(p, p') and two
    !     divisions per step.
    !   * factorisation over Q: content, then Yun, then the Rational Root
    !     Theorem on each squarefree part. What survives is irreducible only
    !     when its degree is at most 3, because a polynomial of degree 2 or 3
    !     with no rational root has no factorisation over Q at all, while a
    !     quartic can split into two irreducible quadratics with no root in
    !     sight. Zassenhaus/Hensel lifting and van Hoeij's lattice
    !     recombination (van Hoeij, J. Number Theory 95, 2002) are NOT
    !     implemented, so a surviving factor of degree 4 or more is refused by
    !     name rather than reported as irreducible.
    !   * partial fractions: complete factorisation of the denominator, then
    !     undetermined coefficients solved by exact Gaussian elimination, then
    !     the result is recombined and cross-multiplied against the input --
    !     an answer that fails that identity is discarded.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        node_kind_name
    use fortsym_expr, only: expr_t, num, rat, is_valid, same_arena, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    implicit none
    private

    public :: poly_together, poly_cancel, poly_apart, poly_factor
    public :: poly_coefficient, poly_coefficient_list, poly_collect
    public :: poly_exponent, poly_gcd_expr, poly_divide
    public :: poly_numerator, poly_denominator

    integer(int64), parameter :: MAX_I64 = huge(0_int64)
    integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64

    ! Work bounds. Each is a refusal when exceeded, never a truncation: a
    ! silently dropped term is a different polynomial.
    integer, parameter :: MAX_TERMS = 2000
    integer, parameter :: MAX_VARS = 12
    integer, parameter :: MAX_DEGREE = 64

    ! Above this the divisor enumeration behind the Rational Root Theorem is
    ! not worth running; Factor refuses instead of missing a root and then
    ! calling the leftover irreducible.
    integer(int64), parameter :: MAX_FACTORABLE = 100000000_int64

    character(*), parameter :: OVERFLOW_WHY = &
        "exact coefficient arithmetic overflowed checked 64-bit integers"

    !> An exact rational in lowest terms with a positive denominator.
    type :: rat_t
        integer(int64) :: n = 0_int64
        integer(int64) :: d = 1_int64
    end type rat_t

    !> A sparse multivariate polynomial: term k has coefficient c(k) and
    !> exponent vector ex(:, k) over the generators of the enclosing context.
    !> Terms are kept sorted strictly descending in lex order, so the
    !> representation is canonical and term 1 is the leading term.
    type :: poly_t
        integer                  :: nv = 0
        integer, allocatable     :: ex(:, :)
        type(rat_t), allocatable :: c(:)
    end type poly_t

    !> A rational function. `d` is never the zero polynomial.
    type :: ratf_t
        type(poly_t) :: n
        type(poly_t) :: d
    end type ratf_t

    !> A factor with its multiplicity, held as a univariate coefficient vector.
    type :: ufactor_t
        type(rat_t), allocatable :: c(:)
        integer                  :: mult = 1
    end type ufactor_t

    !> The generators, as arena node ids. Interning makes equal expressions
    !> share an id, so identity of generators is exact rather than textual.
    type :: ctx_t
        integer, allocatable :: atom(:)
    end type ctx_t

contains

    ! =====================================================================
    ! Public entry points
    ! =====================================================================

    !> Together[e]: one fraction, with the common factor removed.
    subroutine poly_together(a, e, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f

        out = e
        call read_ratf(a, e, ctx, f, ok, why)
        if (.not. ok) return
        call ratf_reduce(f, ok, why)
        if (.not. ok) return
        call ratf_expr(a, ctx, f, out, ok, why)
    end subroutine poly_together

    !> Cancel[e]: common factors removed, summand by summand.
    !>
    !> Threading over a top-level sum keeps Cancel from silently turning a sum
    !> of fractions into a single one, which is Together's job.
    subroutine poly_cancel(a, e, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: part, acc
        integer :: k

        out = e
        if (e%kind() /= NK_ADD) then
            call cancel_one(a, e, out, ok, why)
            return
        end if
        do k = 1, e%nargs()
            call cancel_one(a, e%arg(k), part, ok, why)
            if (.not. ok) return
            if (k == 1) then
                acc = part
            else
                acc = acc + part
            end if
        end do
        out = acc
        ok = .true.
    end subroutine poly_cancel

    subroutine cancel_one(a, e, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f

        out = e
        call read_ratf(a, e, ctx, f, ok, why)
        if (.not. ok) return
        call ratf_reduce(f, ok, why)
        if (.not. ok) return
        call ratf_expr(a, ctx, f, out, ok, why)
    end subroutine cancel_one

    !> Coefficient[e, var, n].
    subroutine poly_coefficient(a, e, var, n, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        integer,                   intent(in)    :: n
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f
        type(poly_t) :: coeff
        integer :: iv

        out = e
        if (n < 0) then
            ok = .false.
            why = "a negative power is not a polynomial coefficient"
            return
        end if
        call read_in_var(a, e, var, ctx, f, iv, ok, why)
        if (.not. ok) return
        call p_coeff_v(f%n, iv, n, coeff)
        call ratf_expr(a, ctx, ratf_t(coeff, f%d), out, ok, why)
    end subroutine poly_coefficient

    !> CoefficientList[e, var]: coefficients of var**0 .. var**deg.
    subroutine poly_coefficient_list(a, e, var, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        type(expr_t), allocatable, intent(out)   :: out(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f
        type(poly_t) :: coeff
        integer :: iv, deg, k

        allocate (out(0))
        call read_in_var(a, e, var, ctx, f, iv, ok, why)
        if (.not. ok) return
        deg = p_deg_v(f%n, iv)
        if (deg < 0) deg = 0
        deallocate (out)
        allocate (out(deg + 1))
        do k = 0, deg
            call p_coeff_v(f%n, iv, k, coeff)
            call ratf_expr(a, ctx, ratf_t(coeff, f%d), out(k + 1), ok, why)
            if (.not. ok) return
        end do
        ok = .true.
    end subroutine poly_coefficient_list

    !> Collect[e, var]: the polynomial written by powers of var.
    subroutine poly_collect(a, e, var, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f
        type(poly_t) :: coeff
        type(expr_t) :: term, acc, vexpr
        integer :: iv, deg, k
        logical :: started

        out = e
        call read_in_var(a, e, var, ctx, f, iv, ok, why)
        if (.not. ok) return
        deg = p_deg_v(f%n, iv)
        vexpr = id_expr(a, ctx%atom(iv))
        started = .false.
        do k = 0, max(deg, 0)
            call p_coeff_v(f%n, iv, k, coeff)
            if (p_is_zero(coeff)) cycle
            call ratf_expr(a, ctx, ratf_t(coeff, f%d), term, ok, why)
            if (.not. ok) return
            if (k == 1) then
                term = term*vexpr
            else if (k > 1) then
                term = term*(vexpr**k)
            end if
            if (started) then
                acc = acc + term
            else
                acc = term
                started = .true.
            end if
        end do
        if (.not. started) acc = num(a, 0)
        out = acc
        ok = .true.
    end subroutine poly_collect

    !> Exponent[e, var]: the highest power of var that occurs.
    subroutine poly_exponent(a, e, var, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f
        integer :: iv, deg

        out = e
        call read_in_var(a, e, var, ctx, f, iv, ok, why)
        if (.not. ok) return
        if (p_is_zero(f%n)) then
            ok = .false.
            why = "Exponent of the zero polynomial is -Infinity, which "// &
                  "fortsym has no exact value for"
            return
        end if
        deg = p_deg_v(f%n, iv)
        out = num(a, deg)
        ok = .true.
    end subroutine poly_exponent

    !> PolynomialGCD[p, q], monic in lexicographic order.
    subroutine poly_gcd_expr(a, p, q, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: p, q
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: fp, fq
        type(poly_t) :: g
        type(expr_t) :: sum2

        out = p
        ! Reading both through one context puts them over the same generators.
        sum2 = p + q
        call read_ratf(a, sum2, ctx, fp, ok, why)
        if (.not. ok) return
        call read_in_ctx(a, p, ctx, fp, ok, why)
        if (.not. ok) return
        call read_in_ctx(a, q, ctx, fq, ok, why)
        if (.not. ok) return
        if (.not. p_is_const(fp%d)) then
            ok = .false.
            why = "PolynomialGCD needs polynomials, but the first argument "// &
                  "has a non-constant denominator"
            return
        end if
        if (.not. p_is_const(fq%d)) then
            ok = .false.
            why = "PolynomialGCD needs polynomials, but the second "// &
                  "argument has a non-constant denominator"
            return
        end if
        call p_gcd(fp%n, fq%n, g, ok, why)
        if (.not. ok) return
        call poly_to_expr(a, ctx, g, out, ok, why)
    end subroutine poly_gcd_expr

    !> PolynomialQuotient / PolynomialRemainder of p by q in var.
    subroutine poly_divide(a, p, q, var, want_quotient, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: p, q, var
        logical,                   intent(in)    :: want_quotient
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: fp, fq
        type(poly_t) :: quo, rem
        type(expr_t) :: seed
        integer :: iv

        out = p
        seed = p + q + var
        call read_ratf(a, seed, ctx, fp, ok, why)
        if (.not. ok) return
        call read_in_ctx(a, p, ctx, fp, ok, why)
        if (.not. ok) return
        call read_in_ctx(a, q, ctx, fq, ok, why)
        if (.not. ok) return
        call var_index(a, ctx, var, iv, ok, why)
        if (.not. ok) return
        if (.not. p_is_const(fp%d)) then
            ok = .false.
            why = "polynomial division needs a polynomial dividend"
            return
        end if
        if (.not. p_is_const(fq%d)) then
            ok = .false.
            why = "polynomial division needs a polynomial divisor"
            return
        end if
        call p_divmod_v(fp%n, fq%n, iv, quo, rem, ok, why)
        if (.not. ok) return
        if (want_quotient) then
            call ratf_expr(a, ctx, ratf_t(quo, fp%d), out, ok, why)
        else
            call ratf_expr(a, ctx, ratf_t(rem, fp%d), out, ok, why)
        end if
    end subroutine poly_divide

    !> Factor[e], complete or refused.
    subroutine poly_factor(a, e, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f
        type(expr_t) :: nfac, dfac

        out = e
        call read_ratf(a, e, ctx, f, ok, why)
        if (.not. ok) return
        call ratf_reduce(f, ok, why)
        if (.not. ok) return

        call factor_poly_expr(a, ctx, f%n, nfac, ok, why)
        if (.not. ok) return
        if (p_is_const(f%d)) then
            call const_divide_expr(a, nfac, f%d, out, ok, why)
            return
        end if
        call factor_poly_expr(a, ctx, f%d, dfac, ok, why)
        if (.not. ok) return
        out = nfac/dfac
        ok = .true.
    end subroutine poly_factor

    !> Apart[e, var]: partial fractions in var.
    subroutine poly_apart(a, e, var, has_var, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        logical,                   intent(in)    :: has_var
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ctx_t)  :: ctx
        type(ratf_t) :: f
        integer :: iv

        out = e
        call read_ratf(a, e, ctx, f, ok, why)
        if (.not. ok) return
        call ratf_reduce(f, ok, why)
        if (.not. ok) return
        if (has_var) then
            call var_index(a, ctx, var, iv, ok, why)
            if (.not. ok) return
        else
            if (size(ctx%atom) /= 1) then
                ok = .false.
                why = "Apart needs the variable to expand in, because the "// &
                      "expression has more than one indeterminate"
                return
            end if
            iv = 1
        end if
        call apart_in(a, ctx, f, iv, out, ok, why)
    end subroutine poly_apart

    !> Numerator[e] / Denominator[e], structurally as Wolfram defines them:
    !> the denominator is the product of the factors carrying a negative
    !> power. A sum is left alone, because Denominator does not call Together.
    function poly_numerator(a, e) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        type(expr_t)                         :: r
        type(expr_t) :: den

        call split_fraction(a, e, r, den)
    end function poly_numerator

    function poly_denominator(a, e) result(r)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        type(expr_t)                         :: r
        type(expr_t) :: nume

        call split_fraction(a, e, nume, r)
    end function poly_denominator

    recursive subroutine split_fraction(a, e, nume, den)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: e
        type(expr_t),          intent(out)   :: nume, den
        type(expr_t) :: fnum, fden, part
        integer :: k

        nume = e
        den = num(a, 1)
        select case (e%kind())
        case (NK_MUL)
            nume = num(a, 1)
            do k = 1, e%nargs()
                call split_fraction(a, e%arg(k), fnum, fden)
                nume = nume*fnum
                den = den*fden
            end do
        case (NK_POW)
            part = e%arg(2)
            if (part%kind() /= NK_INT) return
            if (part%int_value() >= 0_int64) return
            nume = num(a, 1)
            den = e%arg(1)**int(-part%int_value())
        case (NK_RAT)
            nume = num(a, e%int_value())
            den = num(a, e%den_value())
        end select
    end subroutine split_fraction

    ! =====================================================================
    ! Reading expressions into rational functions
    ! =====================================================================

    subroutine read_ratf(a, e, ctx, f, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(ctx_t),               intent(out)   :: ctx
        type(ratf_t),              intent(out)   :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why

        allocate (ctx%atom(0))
        call read_in_ctx(a, e, ctx, f, ok, why)
    end subroutine read_ratf

    !> Read `e` over an existing generator set, extending it as needed. Every
    !> polynomial handed back is padded to the final generator count.
    subroutine read_in_ctx(a, e, ctx, f, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(ctx_t),               intent(inout) :: ctx
        type(ratf_t),              intent(out)   :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why

        if (.not. is_valid(e)) then
            ok = .false.
            why = "the expression is not valid"
            return
        end if
        if (.not. associated(e%a, a)) then
            ok = .false.
            why = "the expression does not belong to the arena passed in"
            return
        end if
        call read_node(e%a, e%id, ctx, f, ok, why)
        if (.not. ok) return
        call p_pad(f%n, size(ctx%atom))
        call p_pad(f%d, size(ctx%atom))
    end subroutine read_in_ctx

    !> Read, then insist the denominator is free of the chosen variable, so
    !> that coefficients with respect to it are well defined.
    subroutine read_in_var(a, e, var, ctx, f, iv, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(expr_t),              intent(in)    :: var
        type(ctx_t),               intent(out)   :: ctx
        type(ratf_t),              intent(out)   :: f
        integer,                   intent(out)   :: iv
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: seed

        iv = 0
        seed = e + var
        call read_ratf(a, seed, ctx, f, ok, why)
        if (.not. ok) return
        call read_in_ctx(a, e, ctx, f, ok, why)
        if (.not. ok) return
        call var_index(a, ctx, var, iv, ok, why)
        if (.not. ok) return
        call ratf_reduce(f, ok, why)
        if (.not. ok) return
        if (p_deg_v(f%d, iv) > 0) then
            ok = .false.
            why = "the expression is not a polynomial in the given "// &
                  "variable: it survives only as a denominator"
            return
        end if
    end subroutine read_in_var

    subroutine var_index(a, ctx, var, iv, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(ctx_t),               intent(inout) :: ctx
        type(expr_t),              intent(in)    :: var
        integer,                   intent(out)   :: iv
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        integer :: k

        iv = 0
        ok = .false.
        why = ""
        if (.not. is_valid(var)) then
            why = "the variable is not a valid expression"
            return
        end if
        if (.not. associated(var%a, a)) then
            why = "the variable does not belong to the arena passed in"
            return
        end if
        do k = 1, size(ctx%atom)
            if (ctx%atom(k) == var%id) then
                iv = k
                ok = .true.
                return
            end if
        end do
        why = "the expression does not contain the given variable as an "// &
              "indeterminate"
    end subroutine var_index

    recursive subroutine read_node(a, id, ctx, f, ok, why)
        type(arena_t),             intent(in)    :: a
        integer,                   intent(in)    :: id
        type(ctx_t),               intent(inout) :: ctx
        type(ratf_t),              intent(out)   :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ratf_t) :: acc, part, tmp
        type(rat_t)  :: r
        integer      :: k, kind
        integer(int64) :: expo

        ok = .false.
        why = ""
        kind = a%kind_of(id)

        select case (kind)

        case (NK_INT)
            if (a%num_of(id) == MIN_I64) then
                why = OVERFLOW_WHY
                return
            end if
            r%n = a%num_of(id)
            r%d = 1_int64
            call ratf_const(r, size(ctx%atom), f)
            ok = .true.

        case (NK_RAT)
            call rat_make(a%num_of(id), a%den_of(id), r, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            call ratf_const(r, size(ctx%atom), f)

        case (NK_REAL)
            why = "the expression contains the floating-point literal it "// &
                  "would have to treat as exact; polynomial algebra here is "// &
                  "over the rationals only"

        case (NK_BIG_INT, NK_BIG_RAT)
            why = "the expression contains an arbitrary-precision "// &
                  "coefficient, beyond the checked 64-bit exact arithmetic "// &
                  "used here"

        case (NK_ADD)
            call ratf_const(rat_of(0_int64), size(ctx%atom), acc)
            do k = 1, a%nargs_of(id)
                call read_node(a, a%arg_of(id, k), ctx, part, ok, why)
                if (.not. ok) return
                call ratf_add(acc, part, tmp, ok, why)
                if (.not. ok) return
                acc = tmp
            end do
            f = acc
            ok = .true.

        case (NK_MUL)
            call ratf_const(rat_of(1_int64), size(ctx%atom), acc)
            do k = 1, a%nargs_of(id)
                call read_node(a, a%arg_of(id, k), ctx, part, ok, why)
                if (.not. ok) return
                call ratf_mul(acc, part, tmp, ok, why)
                if (.not. ok) return
                acc = tmp
            end do
            f = acc
            ok = .true.

        case (NK_POW)
            if (a%kind_of(a%arg_of(id, 2)) /= NK_INT) then
                ! A symbolic or fractional power is not a rational function of
                ! its base, so the whole power becomes one generator.
                call atom_ratf(ctx, id, f, ok, why)
                return
            end if
            expo = a%num_of(a%arg_of(id, 2))
            if (abs(expo) > int(MAX_DEGREE, int64)) then
                why = "an exponent beyond the supported degree bound"
                return
            end if
            call read_node(a, a%arg_of(id, 1), ctx, part, ok, why)
            if (.not. ok) return
            call ratf_pow(part, int(expo), f, ok, why)

        case (NK_SYM, NK_CONST, NK_FUNC)
            call atom_ratf(ctx, id, f, ok, why)

        case default
            why = "unsupported node kind "//chars(node_kind_name(kind))// &
                  " in the expression"
        end select
    end subroutine read_node

    subroutine atom_ratf(ctx, id, f, ok, why)
        type(ctx_t),               intent(inout) :: ctx
        integer,                   intent(in)    :: id
        type(ratf_t),              intent(out)   :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        integer, allocatable :: bigger(:)
        integer :: k, iv

        ok = .false.
        why = ""
        iv = 0
        do k = 1, size(ctx%atom)
            if (ctx%atom(k) == id) iv = k
        end do
        if (iv == 0) then
            if (size(ctx%atom) >= MAX_VARS) then
                why = "the expression has more indeterminates than the "// &
                      "polynomial engine handles"
                return
            end if
            allocate (bigger(size(ctx%atom) + 1))
            if (size(ctx%atom) > 0) bigger(1:size(ctx%atom)) = ctx%atom
            bigger(size(ctx%atom) + 1) = id
            call move_alloc(bigger, ctx%atom)
            iv = size(ctx%atom)
        end if
        call p_var(iv, size(ctx%atom), f%n)
        call p_const(rat_of(1_int64), size(ctx%atom), f%d)
        ok = .true.
    end subroutine atom_ratf

    ! =====================================================================
    ! Rational function arithmetic
    ! =====================================================================

    subroutine ratf_const(r, nv, f)
        type(rat_t), intent(in)  :: r
        integer,     intent(in)  :: nv
        type(ratf_t), intent(out) :: f

        call p_const(r, nv, f%n)
        call p_const(rat_of(1_int64), nv, f%d)
    end subroutine ratf_const

    subroutine ratf_add(x, y, z, ok, why)
        type(ratf_t),              intent(in)  :: x, y
        type(ratf_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: t1, t2, n, d

        call p_mul(x%n, y%d, t1, ok, why)
        if (.not. ok) return
        call p_mul(y%n, x%d, t2, ok, why)
        if (.not. ok) return
        call p_add(t1, t2, n, ok, why)
        if (.not. ok) return
        call p_mul(x%d, y%d, d, ok, why)
        if (.not. ok) return
        z%n = n
        z%d = d
    end subroutine ratf_add

    subroutine ratf_mul(x, y, z, ok, why)
        type(ratf_t),              intent(in)  :: x, y
        type(ratf_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: n, d

        call p_mul(x%n, y%n, n, ok, why)
        if (.not. ok) return
        call p_mul(x%d, y%d, d, ok, why)
        if (.not. ok) return
        z%n = n
        z%d = d
    end subroutine ratf_mul

    subroutine ratf_pow(x, k, z, ok, why)
        type(ratf_t),              intent(in)  :: x
        integer,                   intent(in)  :: k
        type(ratf_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(ratf_t) :: base, tmp
        integer :: j

        base = x
        if (k < 0) then
            if (p_is_zero(x%n)) then
                ok = .false.
                why = "the expression divides by zero"
                return
            end if
            base%n = x%d
            base%d = x%n
        end if
        call ratf_const(rat_of(1_int64), x%n%nv, z)
        do j = 1, abs(k)
            call ratf_mul(z, base, tmp, ok, why)
            if (.not. ok) return
            z = tmp
        end do
        ok = .true.
        why = ""
    end subroutine ratf_pow

    !> Divide out the greatest common divisor and normalise the sign, so the
    !> printed fraction is in lowest terms.
    subroutine ratf_reduce(f, ok, why)
        type(ratf_t),              intent(inout) :: f
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(poly_t) :: g, qn, qd, sn, sd
        type(rat_t)  :: lead, inv
        logical      :: exact

        ok = .false.
        why = ""
        if (p_is_zero(f%d)) then
            why = "the expression divides by zero"
            return
        end if
        if (p_is_zero(f%n)) then
            call p_const(rat_of(1_int64), f%d%nv, f%d)
            ok = .true.
            return
        end if
        call p_gcd(f%n, f%d, g, ok, why)
        if (.not. ok) return
        if (.not. p_is_const(g)) then
            call p_divide(f%n, g, qn, exact, ok, why)
            if (.not. ok) return
            if (.not. exact) then
                ok = .false.
                why = "internal check failed: the common divisor does not "// &
                      "divide the numerator exactly"
                return
            end if
            call p_divide(f%d, g, qd, exact, ok, why)
            if (.not. ok) return
            if (.not. exact) then
                ok = .false.
                why = "internal check failed: the common divisor does not "// &
                      "divide the denominator exactly"
                return
            end if
            f%n = qn
            f%d = qd
        end if
        ! Make the denominator's leading coefficient 1.
        lead = f%d%c(1)
        call rat_make(lead%d, lead%n, inv, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call p_scale(f%n, inv, sn, ok, why)
        if (.not. ok) return
        call p_scale(f%d, inv, sd, ok, why)
        if (.not. ok) return
        f%n = sn
        f%d = sd
    end subroutine ratf_reduce

    ! =====================================================================
    ! Polynomial arithmetic
    ! =====================================================================

    pure function rat_of(v) result(r)
        integer(int64), intent(in) :: v
        type(rat_t) :: r
        r%n = v
        r%d = 1_int64
    end function rat_of

    subroutine p_zero(nv, p)
        integer,      intent(in)  :: nv
        type(poly_t), intent(out) :: p
        p%nv = nv
        allocate (p%ex(nv, 0))
        allocate (p%c(0))
    end subroutine p_zero

    subroutine p_const(r, nv, p)
        type(rat_t),  intent(in)  :: r
        integer,      intent(in)  :: nv
        type(poly_t), intent(out) :: p

        if (r%n == 0_int64) then
            call p_zero(nv, p)
            return
        end if
        p%nv = nv
        allocate (p%ex(nv, 1))
        allocate (p%c(1))
        if (nv > 0) p%ex(:, 1) = 0
        p%c(1) = r
    end subroutine p_const

    subroutine p_var(iv, nv, p)
        integer,      intent(in)  :: iv, nv
        type(poly_t), intent(out) :: p

        p%nv = nv
        allocate (p%ex(nv, 1))
        allocate (p%c(1))
        p%ex(:, 1) = 0
        p%ex(iv, 1) = 1
        p%c(1) = rat_of(1_int64)
    end subroutine p_var

    !> Bring two polynomials over the same generator count. Generators are
    !> discovered as an expression is read, so a subexpression met early can
    !> carry fewer of them than one met later; combining those without padding
    !> would add exponent vectors of different length and silently build a
    !> different polynomial.
    subroutine align2(x, y)
        type(poly_t), intent(inout) :: x, y
        integer :: nv

        nv = max(x%nv, y%nv)
        call p_pad(x, nv)
        call p_pad(y, nv)
    end subroutine align2

    subroutine p_pad(p, nv)
        type(poly_t), intent(inout) :: p
        integer,      intent(in)    :: nv
        integer, allocatable :: ex(:, :)
        integer :: nt

        if (p%nv == nv) return
        nt = size(p%c)
        allocate (ex(nv, nt))
        ex = 0
        if (p%nv > 0) ex(1:p%nv, :) = p%ex(1:p%nv, :)
        call move_alloc(ex, p%ex)
        p%nv = nv
    end subroutine p_pad

    pure function p_is_zero(p) result(yes)
        type(poly_t), intent(in) :: p
        logical                  :: yes
        yes = size(p%c) == 0
    end function p_is_zero

    pure function p_is_const(p) result(yes)
        type(poly_t), intent(in) :: p
        logical                  :: yes

        yes = .true.
        if (size(p%c) == 0) return
        if (size(p%c) > 1) then
            yes = .false.
            return
        end if
        if (p%nv > 0) yes = all(p%ex(:, 1) == 0)
    end function p_is_const

    !> Lexicographic comparison of exponent vectors: 1 if x is the larger.
    pure function cmp_ex(x, y) result(c)
        integer, intent(in) :: x(:), y(:)
        integer             :: c
        integer :: k

        c = 0
        do k = 1, size(x)
            if (x(k) > y(k)) then
                c = 1
                return
            end if
            if (x(k) < y(k)) then
                c = -1
                return
            end if
        end do
    end function cmp_ex

    !> Sort descending, merge equal monomials, drop zero coefficients.
    subroutine p_norm(p, ok, why)
        type(poly_t),              intent(inout) :: p
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        integer, allocatable     :: key(:), ex(:, :)
        type(rat_t), allocatable :: c(:)
        type(rat_t) :: kc, sum, acc
        integer :: i, j, nt, out

        ok = .true.
        why = ""
        nt = size(p%c)
        allocate (key(max(p%nv, 1)))
        do i = 2, nt
            key(1:p%nv) = p%ex(:, i)
            kc = p%c(i)
            j = i - 1
            do while (j >= 1)
                if (cmp_ex(p%ex(:, j), key(1:p%nv)) >= 0) exit
                p%ex(:, j + 1) = p%ex(:, j)
                p%c(j + 1) = p%c(j)
                j = j - 1
            end do
            p%ex(:, j + 1) = key(1:p%nv)
            p%c(j + 1) = kc
        end do

        allocate (ex(p%nv, nt))
        allocate (c(nt))
        out = 0
        i = 1
        do while (i <= nt)
            sum = p%c(i)
            j = i + 1
            do while (j <= nt)
                if (cmp_ex(p%ex(:, j), p%ex(:, i)) /= 0) exit
                call rat_add(sum, p%c(j), acc, ok)
                sum = acc
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                j = j + 1
            end do
            if (sum%n /= 0_int64) then
                out = out + 1
                ex(:, out) = p%ex(:, i)
                c(out) = sum
            end if
            i = j
        end do

        deallocate (p%ex, p%c)
        allocate (p%ex(p%nv, out))
        allocate (p%c(out))
        if (out > 0) then
            p%ex = ex(:, 1:out)
            p%c = c(1:out)
        end if
        ok = .true.
    end subroutine p_norm

    subroutine p_add(x, y, z, ok, why)
        type(poly_t),              intent(in)  :: x, y
        type(poly_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: xa, ya
        integer :: nx, ny

        xa = x
        ya = y
        call align2(xa, ya)
        nx = size(xa%c)
        ny = size(ya%c)
        if (nx + ny > MAX_TERMS) then
            ok = .false.
            why = "the polynomial has more terms than the engine handles"
            return
        end if
        z%nv = xa%nv
        allocate (z%ex(z%nv, nx + ny))
        allocate (z%c(nx + ny))
        if (nx > 0) then
            z%ex(:, 1:nx) = xa%ex
            z%c(1:nx) = xa%c
        end if
        if (ny > 0) then
            z%ex(:, nx + 1:nx + ny) = ya%ex
            z%c(nx + 1:nx + ny) = ya%c
        end if
        call p_norm(z, ok, why)
    end subroutine p_add

    subroutine p_neg(x, z, ok, why)
        type(poly_t),              intent(in)  :: x
        type(poly_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: k

        z = x
        ok = .true.
        why = ""
        do k = 1, size(z%c)
            if (z%c(k)%n == MIN_I64) then
                ok = .false.
                why = OVERFLOW_WHY
                return
            end if
            z%c(k)%n = -z%c(k)%n
        end do
    end subroutine p_neg

    subroutine p_sub(x, y, z, ok, why)
        type(poly_t),              intent(in)  :: x, y
        type(poly_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: ny

        call p_neg(y, ny, ok, why)
        if (.not. ok) return
        call p_add(x, ny, z, ok, why)
    end subroutine p_sub

    subroutine p_mul(x, y, z, ok, why)
        type(poly_t),              intent(in)  :: x, y
        type(poly_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: xa, ya
        integer :: i, j, n, at

        xa = x
        ya = y
        call align2(xa, ya)
        n = size(xa%c)*size(ya%c)
        if (n > MAX_TERMS) then
            ok = .false.
            why = "the polynomial product has more terms than the engine "// &
                  "handles"
            return
        end if
        z%nv = xa%nv
        allocate (z%ex(z%nv, n))
        allocate (z%c(n))
        at = 0
        do i = 1, size(xa%c)
            do j = 1, size(ya%c)
                at = at + 1
                if (z%nv > 0) z%ex(:, at) = xa%ex(:, i) + ya%ex(:, j)
                call rat_mul(xa%c(i), ya%c(j), z%c(at), ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
            end do
        end do
        if (z%nv > 0) then
            if (n > 0) then
                if (maxval(z%ex) > MAX_DEGREE) then
                    ok = .false.
                    why = "the polynomial degree exceeds the supported bound"
                    return
                end if
            end if
        end if
        call p_norm(z, ok, why)
    end subroutine p_mul

    subroutine p_scale(x, r, z, ok, why)
        type(poly_t),              intent(in)  :: x
        type(rat_t),               intent(in)  :: r
        type(poly_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: k

        z = x
        ok = .true.
        why = ""
        if (r%n == 0_int64) then
            call p_zero(x%nv, z)
            return
        end if
        do k = 1, size(z%c)
            call rat_mul(x%c(k), r, z%c(k), ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
        end do
    end subroutine p_scale

    pure function p_deg_v(p, iv) result(d)
        type(poly_t), intent(in) :: p
        integer,      intent(in) :: iv
        integer                  :: d

        d = -1
        if (size(p%c) == 0) return
        d = maxval(p%ex(iv, :))
    end function p_deg_v

    !> The coefficient of var(iv)**k, as a polynomial in the other variables.
    subroutine p_coeff_v(p, iv, k, c)
        type(poly_t), intent(in)  :: p
        integer,      intent(in)  :: iv, k
        type(poly_t), intent(out) :: c
        integer :: j, at, n

        n = count(p%ex(iv, :) == k)
        c%nv = p%nv
        allocate (c%ex(c%nv, n))
        allocate (c%c(n))
        at = 0
        do j = 1, size(p%c)
            if (p%ex(iv, j) /= k) cycle
            at = at + 1
            c%ex(:, at) = p%ex(:, j)
            c%ex(iv, at) = 0
            c%c(at) = p%c(j)
        end do
    end subroutine p_coeff_v

    !> Multiply by var(iv)**k.
    subroutine p_shift_v(p, iv, k, z, ok, why)
        type(poly_t),              intent(in)  :: p
        integer,                   intent(in)  :: iv, k
        type(poly_t),              intent(out) :: z
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        z = p
        ok = .true.
        why = ""
        if (size(z%c) == 0) return
        z%ex(iv, :) = z%ex(iv, :) + k
        if (maxval(z%ex(iv, :)) > MAX_DEGREE) then
            ok = .false.
            why = "the polynomial degree exceeds the supported bound"
        end if
    end subroutine p_shift_v

    !> Exact division. `exact` is false when the remainder is nonzero; that is
    !> a fact about the inputs, not an error, so ok stays true.
    subroutine p_divide(x, y, q, exact, ok, why)
        type(poly_t),              intent(in)  :: x, y
        type(poly_t),              intent(out) :: q
        logical,                   intent(out) :: exact
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: r, t, prod, acc, ya
        type(rat_t)  :: factor
        integer :: k, guard

        exact = .false.
        ok = .false.
        why = ""
        if (p_is_zero(y)) then
            why = "polynomial division by the zero polynomial"
            return
        end if
        ya = y
        r = x
        call align2(r, ya)
        call p_zero(r%nv, q)
        guard = 0
        do while (.not. p_is_zero(r))
            guard = guard + 1
            if (guard > 100000) then
                why = "polynomial division did not terminate within the "// &
                      "step budget"
                return
            end if
            if (ya%nv > 0) then
                if (any(r%ex(:, 1) < ya%ex(:, 1))) then
                    ok = .true.
                    return
                end if
            end if
            call rat_div(r%c(1), ya%c(1), factor, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            t%nv = r%nv
            if (allocated(t%ex)) deallocate (t%ex)
            if (allocated(t%c)) deallocate (t%c)
            allocate (t%ex(t%nv, 1))
            allocate (t%c(1))
            do k = 1, t%nv
                t%ex(k, 1) = r%ex(k, 1) - ya%ex(k, 1)
            end do
            t%c(1) = factor
            call p_add(q, t, acc, ok, why)
            if (.not. ok) return
            q = acc
            call p_mul(t, ya, prod, ok, why)
            if (.not. ok) return
            call p_sub(r, prod, acc, ok, why)
            if (.not. ok) return
            r = acc
        end do
        exact = .true.
        ok = .true.
    end subroutine p_divide

    !> Division in one variable. The divisor's leading coefficient in that
    !> variable must be a constant; otherwise the quotient lives in a rational
    !> function field and calling the result a polynomial quotient would be a
    !> lie.
    subroutine p_divmod_v(x, y, iv, q, r, ok, why)
        type(poly_t),              intent(in)  :: x, y
        integer,                   intent(in)  :: iv
        type(poly_t),              intent(out) :: q, r
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: lc, t, prod, rc, sc, acc
        type(rat_t)  :: inv
        integer :: dy, dr, guard

        ok = .false.
        why = ""
        call p_zero(x%nv, q)
        r = x
        dy = p_deg_v(y, iv)
        if (dy < 0) then
            why = "polynomial division by zero"
            return
        end if
        call p_coeff_v(y, iv, dy, lc)
        if (.not. p_is_const(lc)) then
            why = "the divisor's leading coefficient in that variable is "// &
                  "not a constant, so the quotient is not a polynomial"
            return
        end if
        call rat_make(lc%c(1)%d, lc%c(1)%n, inv, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        guard = 0
        do
            dr = p_deg_v(r, iv)
            if (dr < dy) exit
            guard = guard + 1
            if (guard > 10000) then
                why = "polynomial division did not terminate within the "// &
                      "step budget"
                ok = .false.
                return
            end if
            call p_coeff_v(r, iv, dr, rc)
            call p_scale(rc, inv, sc, ok, why)
            if (.not. ok) return
            call p_shift_v(sc, iv, dr - dy, t, ok, why)
            if (.not. ok) return
            call p_add(q, t, acc, ok, why)
            if (.not. ok) return
            q = acc
            call p_mul(t, y, prod, ok, why)
            if (.not. ok) return
            call p_sub(r, prod, acc, ok, why)
            if (.not. ok) return
            r = acc
        end do
        ok = .true.
    end subroutine p_divmod_v

    !> Pseudo-remainder: lc_v(y)**(deg_v(x)-deg_v(y)+1) * x  mod y, in var iv.
    subroutine p_prem(x, y, iv, r, ok, why)
        type(poly_t),              intent(in)  :: x, y
        integer,                   intent(in)  :: iv
        type(poly_t),              intent(out) :: r
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: lc, rc, t, prod, acc
        integer :: dx, dy, dr, k, steps

        ok = .false.
        why = ""
        r = x
        dx = p_deg_v(x, iv)
        dy = p_deg_v(y, iv)
        if (dy < 0) then
            why = "pseudo-division by zero"
            return
        end if
        if (dx < dy) then
            ok = .true.
            return
        end if
        call p_coeff_v(y, iv, dy, lc)
        steps = dx - dy + 1
        do k = 1, steps
            dr = p_deg_v(r, iv)
            if (dr < dy) exit
            ! The leading coefficient has to be taken BEFORE r is scaled by
            ! lc: scaling first and then reading it makes the subtracted term
            ! carry lc twice, the leading terms fail to cancel, and the
            ! sequence neither terminates nor stays small.
            call p_coeff_v(r, iv, dr, rc)
            call p_mul(r, lc, acc, ok, why)
            if (.not. ok) return
            r = acc
            call p_shift_v(rc, iv, dr - dy, t, ok, why)
            if (.not. ok) return
            call p_mul(t, y, prod, ok, why)
            if (.not. ok) return
            call p_sub(r, prod, acc, ok, why)
            if (.not. ok) return
            r = acc
        end do
        ok = .true.
    end subroutine p_prem

    !> Divide by the leading coefficient in lex order, so gcd results are
    !> canonical up to nothing at all.
    subroutine p_monic(p, ok, why)
        type(poly_t),              intent(inout) :: p
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t)  :: inv
        type(poly_t) :: scaled

        ok = .true.
        why = ""
        if (p_is_zero(p)) return
        call rat_make(p%c(1)%d, p%c(1)%n, inv, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call p_scale(p, inv, scaled, ok, why)
        if (.not. ok) return
        p = scaled
    end subroutine p_monic

    !> The content of p with respect to iv: the gcd of its coefficients.
    recursive subroutine p_content_v(p, iv, c, ok, why)
        type(poly_t),              intent(in)  :: p
        integer,                   intent(in)  :: iv
        type(poly_t),              intent(out) :: c
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: coeff, g
        integer :: k, d

        call p_zero(p%nv, c)
        ok = .true.
        why = ""
        d = p_deg_v(p, iv)
        do k = 0, d
            call p_coeff_v(p, iv, k, coeff)
            if (p_is_zero(coeff)) cycle
            call p_gcd(c, coeff, g, ok, why)
            if (.not. ok) return
            c = g
        end do
    end subroutine p_content_v

    !> The primitive part of p with respect to iv.
    recursive subroutine p_prim_v(p, iv, pp, ok, why)
        type(poly_t),              intent(in)  :: p
        integer,                   intent(in)  :: iv
        type(poly_t),              intent(out) :: pp
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: c
        logical :: exact

        pp = p
        if (p_is_zero(p)) then
            ok = .true.
            why = ""
            return
        end if
        call p_content_v(p, iv, c, ok, why)
        if (.not. ok) return
        call p_divide(p, c, pp, exact, ok, why)
        if (.not. ok) return
        if (.not. exact) then
            ok = .false.
            why = "internal check failed: the content does not divide the "// &
                  "polynomial exactly"
        end if
    end subroutine p_prim_v

    !> Monic gcd, by the primitive polynomial remainder sequence.
    !>
    !> Geddes/Czapor/Labahn, "Algorithms for Computer Algebra" (1992), Alg 7.2.
    !> Removing the content before the PRS and re-primitivising every
    !> pseudo-remainder is what keeps the coefficients from exploding; the
    !> modular alternatives (Brown 1971, Zippel 1979) are faster but this one
    !> answers the same question with far less machinery to get wrong.
    recursive subroutine p_gcd(x, y, g, ok, why)
        type(poly_t),              intent(in)  :: x, y
        type(poly_t),              intent(out) :: g
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(poly_t) :: cx, cy, px, py, cg, r, aa, bb, prod
        integer :: iv, k, guard

        ok = .true.
        why = ""
        if (p_is_zero(x)) then
            g = y
            call p_monic(g, ok, why)
            return
        end if
        if (p_is_zero(y)) then
            g = x
            call p_monic(g, ok, why)
            return
        end if

        iv = 0
        do k = 1, x%nv
            if (p_deg_v(x, k) > 0) then
                iv = k
                exit
            end if
            if (p_deg_v(y, k) > 0) then
                iv = k
                exit
            end if
        end do
        if (iv == 0) then
            ! Two nonzero constants: over a field their gcd is a unit.
            call p_const(rat_of(1_int64), x%nv, g)
            return
        end if

        call p_content_v(x, iv, cx, ok, why)
        if (.not. ok) return
        call p_content_v(y, iv, cy, ok, why)
        if (.not. ok) return
        call p_prim_v(x, iv, px, ok, why)
        if (.not. ok) return
        call p_prim_v(y, iv, py, ok, why)
        if (.not. ok) return
        call p_gcd(cx, cy, cg, ok, why)
        if (.not. ok) return

        aa = px
        bb = py
        if (p_deg_v(aa, iv) < p_deg_v(bb, iv)) then
            aa = py
            bb = px
        end if
        guard = 0
        do
            if (p_is_zero(bb)) exit
            if (p_deg_v(bb, iv) == 0) then
                ! A primitive polynomial of degree zero in iv is a unit, so
                ! the two primitive parts share nothing but the content.
                call p_const(rat_of(1_int64), x%nv, aa)
                exit
            end if
            guard = guard + 1
            if (guard > 200) then
                ok = .false.
                why = "the gcd remainder sequence did not terminate within "// &
                      "the step budget"
                return
            end if
            call p_prem(aa, bb, iv, r, ok, why)
            if (.not. ok) return
            aa = bb
            if (p_is_zero(r)) then
                call p_zero(x%nv, bb)
            else
                call p_prim_v(r, iv, bb, ok, why)
                if (.not. ok) return
            end if
        end do
        if (p_deg_v(aa, iv) == 0) call p_const(rat_of(1_int64), x%nv, aa)
        call p_mul(cg, aa, prod, ok, why)
        if (.not. ok) return
        g = prod
        call p_monic(g, ok, why)
    end subroutine p_gcd

    ! =====================================================================
    ! Univariate work over Q: coefficient vectors, ascending
    ! =====================================================================

    !> Extract a univariate coefficient vector, or refuse. c(k) belongs to
    !> var(iv)**(k-1).
    subroutine u_from_poly(p, iv, c, ok, why)
        type(poly_t),              intent(in)  :: p
        integer,                   intent(in)  :: iv
        type(rat_t), allocatable,  intent(out) :: c(:)
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: k, j, d

        ok = .false.
        why = ""
        allocate (c(0))
        do j = 1, size(p%c)
            do k = 1, p%nv
                if (k == iv) cycle
                if (p%ex(k, j) /= 0) then
                    why = "the polynomial is not univariate in that variable"
                    return
                end if
            end do
        end do
        d = p_deg_v(p, iv)
        if (d < 0) d = 0
        deallocate (c)
        allocate (c(d + 1))
        do k = 1, d + 1
            c(k) = rat_of(0_int64)
        end do
        do j = 1, size(p%c)
            c(p%ex(iv, j) + 1) = p%c(j)
        end do
        ok = .true.
    end subroutine u_from_poly

    pure function u_deg(c) result(d)
        type(rat_t), intent(in) :: c(:)
        integer                 :: d

        d = size(c) - 1
        do while (d >= 0)
            if (c(d + 1)%n /= 0_int64) exit
            d = d - 1
        end do
    end function u_deg

    subroutine u_trim(c)
        type(rat_t), allocatable, intent(inout) :: c(:)
        type(rat_t), allocatable :: s(:)
        integer :: d

        d = u_deg(c)
        allocate (s(max(d + 1, 0)))
        if (d >= 0) s = c(1:d + 1)
        call move_alloc(s, c)
    end subroutine u_trim

    subroutine u_add(x, y, z, ok)
        type(rat_t),              intent(in)  :: x(:), y(:)
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        type(rat_t) :: t
        integer :: k, n

        n = max(size(x), size(y))
        allocate (z(n))
        do k = 1, n
            z(k) = rat_of(0_int64)
        end do
        ok = .true.
        do k = 1, size(x)
            z(k) = x(k)
        end do
        do k = 1, size(y)
            call rat_add(z(k), y(k), t, ok)
            if (.not. ok) return
            z(k) = t
        end do
        call u_trim(z)
    end subroutine u_add

    subroutine u_sub(x, y, z, ok)
        type(rat_t),              intent(in)  :: x(:), y(:)
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        type(rat_t), allocatable :: ny(:)
        integer :: k

        allocate (ny(size(y)))
        ok = .true.
        do k = 1, size(y)
            if (y(k)%n == MIN_I64) then
                ok = .false.
                return
            end if
            ny(k)%n = -y(k)%n
            ny(k)%d = y(k)%d
        end do
        call u_add(x, ny, z, ok)
    end subroutine u_sub

    subroutine u_mul(x, y, z, ok)
        type(rat_t),              intent(in)  :: x(:), y(:)
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        type(rat_t) :: t, u
        integer :: i, j, n

        ok = .true.
        if (size(x) == 0 .or. size(y) == 0) then
            allocate (z(0))
            return
        end if
        n = size(x) + size(y) - 1
        allocate (z(n))
        do i = 1, n
            z(i) = rat_of(0_int64)
        end do
        do i = 1, size(x)
            do j = 1, size(y)
                call rat_mul(x(i), y(j), t, ok)
                if (.not. ok) return
                call rat_add(z(i + j - 1), t, u, ok)
                if (.not. ok) return
                z(i + j - 1) = u
            end do
        end do
        call u_trim(z)
    end subroutine u_mul

    subroutine u_scale(x, r, z, ok)
        type(rat_t),              intent(in)  :: x(:)
        type(rat_t),              intent(in)  :: r
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        integer :: k

        allocate (z(size(x)))
        ok = .true.
        do k = 1, size(x)
            call rat_mul(x(k), r, z(k), ok)
            if (.not. ok) return
        end do
        call u_trim(z)
    end subroutine u_scale

    subroutine u_divmod(x, y, q, r, ok)
        type(rat_t),              intent(in)  :: x(:), y(:)
        type(rat_t), allocatable, intent(out) :: q(:), r(:)
        logical,                  intent(out) :: ok
        type(rat_t), allocatable :: t(:), prod(:), diff(:)
        type(rat_t) :: f
        integer :: dx, dy, k

        ok = .false.
        dy = u_deg(y)
        allocate (q(0))
        allocate (r(size(x)))
        if (size(x) > 0) r = x
        call u_trim(r)
        if (dy < 0) return
        dx = u_deg(r)
        deallocate (q)
        allocate (q(max(dx - dy + 1, 0)))
        do k = 1, size(q)
            q(k) = rat_of(0_int64)
        end do
        do
            dx = u_deg(r)
            if (dx < dy) exit
            call rat_div(r(dx + 1), y(dy + 1), f, ok)
            if (.not. ok) return
            q(dx - dy + 1) = f
            if (allocated(t)) deallocate (t)
            allocate (t(dx - dy + 1))
            do k = 1, dx - dy
                t(k) = rat_of(0_int64)
            end do
            t(dx - dy + 1) = f
            call u_mul(t, y, prod, ok)
            if (.not. ok) return
            call u_sub(r, prod, diff, ok)
            if (.not. ok) return
            call move_alloc(diff, r)
        end do
        call u_trim(q)
        ok = .true.
    end subroutine u_divmod

    subroutine u_monic(x, z, ok)
        type(rat_t),              intent(in)  :: x(:)
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        type(rat_t) :: inv
        integer :: d

        d = u_deg(x)
        if (d < 0) then
            allocate (z(0))
            ok = .true.
            return
        end if
        call rat_make(x(d + 1)%d, x(d + 1)%n, inv, ok)
        if (.not. ok) return
        call u_scale(x(1:d + 1), inv, z, ok)
    end subroutine u_monic

    subroutine u_gcd(x, y, g, ok)
        type(rat_t),              intent(in)  :: x(:), y(:)
        type(rat_t), allocatable, intent(out) :: g(:)
        logical,                  intent(out) :: ok
        type(rat_t), allocatable :: aa(:), bb(:), q(:), r(:)
        integer :: guard

        allocate (aa(size(x)))
        if (size(x) > 0) aa = x
        allocate (bb(size(y)))
        if (size(y) > 0) bb = y
        call u_trim(aa)
        call u_trim(bb)
        guard = 0
        do
            if (u_deg(bb) < 0) exit
            guard = guard + 1
            if (guard > 1000) then
                ok = .false.
                allocate (g(0))
                return
            end if
            call u_divmod(aa, bb, q, r, ok)
            if (.not. ok) then
                allocate (g(0))
                return
            end if
            call move_alloc(bb, aa)
            call move_alloc(r, bb)
        end do
        call u_monic(aa, g, ok)
    end subroutine u_gcd

    subroutine u_deriv(x, z, ok)
        type(rat_t),              intent(in)  :: x(:)
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        integer :: k, d

        ok = .true.
        d = u_deg(x)
        allocate (z(max(d, 0)))
        do k = 1, d
            call rat_mul(x(k + 1), rat_of(int(k, int64)), z(k), ok)
            if (.not. ok) return
        end do
        call u_trim(z)
    end subroutine u_deriv

    !> Evaluate at a rational point, exactly.
    subroutine u_eval(x, at, value, ok)
        type(rat_t), intent(in)  :: x(:)
        type(rat_t), intent(in)  :: at
        type(rat_t), intent(out) :: value
        logical,     intent(out) :: ok
        type(rat_t) :: t
        integer :: k

        value = rat_of(0_int64)
        ok = .true.
        do k = size(x), 1, -1
            call rat_mul(value, at, t, ok)
            if (.not. ok) return
            call rat_add(t, x(k), value, ok)
            if (.not. ok) return
        end do
    end subroutine u_eval

    ! =====================================================================
    ! Squarefree decomposition and factorisation over Q
    ! =====================================================================

    !> Yun's squarefree decomposition (Yun, SYMSAC 1976): p = prod a_i**i with
    !> the a_i pairwise coprime and squarefree.
    subroutine u_squarefree(p, parts, ok, why)
        type(rat_t),                  intent(in)  :: p(:)
        type(ufactor_t), allocatable, intent(out) :: parts(:)
        logical,                      intent(out) :: ok
        character(:), allocatable,    intent(out) :: why
        type(rat_t), allocatable :: a(:), b(:), c(:), d(:), g(:), q(:), r(:)
        type(rat_t), allocatable :: t(:)
        integer :: i

        ok = .false.
        why = ""
        allocate (parts(0))
        call u_deriv(p, d, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_gcd(p, d, g, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_divmod(p, g, b, r, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_divmod(d, g, c, r, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_deriv(b, t, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_sub(c, t, d, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if

        i = 1
        do while (u_deg(b) > 0)
            call u_gcd(b, d, a, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            if (u_deg(a) > 0) call append_factor(parts, a, i)
            call u_divmod(b, a, q, r, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            call u_divmod(d, a, c, r, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            call move_alloc(q, b)
            call u_deriv(b, t, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            call u_sub(c, t, d, ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            i = i + 1
            if (i > MAX_DEGREE) then
                why = "squarefree decomposition exceeded the degree bound"
                return
            end if
        end do
        ok = .true.
    end subroutine u_squarefree

    subroutine append_factor(list, c, mult)
        type(ufactor_t), allocatable, intent(inout) :: list(:)
        type(rat_t),                  intent(in)    :: c(:)
        integer,                      intent(in)    :: mult
        type(ufactor_t), allocatable :: bigger(:)
        integer :: n

        n = size(list)
        allocate (bigger(n + 1))
        if (n > 0) bigger(1:n) = list
        allocate (bigger(n + 1)%c(size(c)))
        bigger(n + 1)%c = c
        bigger(n + 1)%mult = mult
        call move_alloc(bigger, list)
    end subroutine append_factor

    !> Complete factorisation over Q of a univariate polynomial, or a refusal.
    !>
    !> Only irreducibility that is *proved* is reported. After the rational
    !> roots of a squarefree part are peeled off, what remains is irreducible
    !> when its degree is 2 or 3 (a factorisation would have produced a linear
    !> factor, hence a rational root) and unknown from degree 4 upwards, where
    !> a product of two irreducible quadratics leaves no root behind. Degree 4
    !> and above is therefore refused rather than reported as irreducible.
    subroutine u_factor(p, lead, factors, ok, why)
        type(rat_t),                  intent(in)  :: p(:)
        type(rat_t),                  intent(out) :: lead
        type(ufactor_t), allocatable, intent(out) :: factors(:)
        logical,                      intent(out) :: ok
        character(:), allocatable,    intent(out) :: why
        type(ufactor_t), allocatable :: parts(:), roots(:)
        type(rat_t), allocatable :: monic(:), rest(:), check(:), prod(:)
        integer :: i, j, k, d
        character(len=16) :: text

        ok = .false.
        why = ""
        lead = rat_of(1_int64)
        allocate (factors(0))
        d = u_deg(p)
        if (d < 0) then
            why = "the zero polynomial has no factorisation"
            return
        end if
        lead = p(d + 1)
        call u_monic(p, monic, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        if (d == 0) then
            ok = .true.
            return
        end if

        call u_squarefree(monic, parts, ok, why)
        if (.not. ok) return

        do i = 1, size(parts)
            call u_rational_factors(parts(i)%c, roots, rest, ok, why)
            if (.not. ok) return
            do j = 1, size(roots)
                call append_factor(factors, roots(j)%c, parts(i)%mult)
            end do
            if (u_deg(rest) >= 4) then
                write (text, '(i0)') u_deg(rest)
                why = "a factor of degree "//trim(text)//" is left over "// &
                      "after the rational roots, and fortsym has no "// &
                      "Zassenhaus/Hensel factorisation to decide it: "// &
                      "refusing rather than reporting a partial factorisation"
                ok = .false.
                return
            end if
            if (u_deg(rest) >= 1) call append_factor(factors, rest, parts(i)%mult)
        end do

        ! Independent check: the factors must multiply back to the input.
        allocate (check(1))
        check(1) = lead
        do i = 1, size(factors)
            do k = 1, factors(i)%mult
                call u_mul(check, factors(i)%c, prod, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call move_alloc(prod, check)
            end do
        end do
        do i = 1, size(factors)
            end do
        if (.not. u_equal(check, p)) then
            ok = .false.
            why = "internal check failed: the factors do not multiply back "// &
                  "to the input polynomial"
            return
        end if
        ok = .true.
    end subroutine u_factor

    pure function u_equal(x, y) result(same)
        type(rat_t), intent(in) :: x(:), y(:)
        logical                 :: same
        integer :: k, d

        same = .false.
        d = u_deg(x)
        if (d /= u_deg(y)) return
        do k = 1, d + 1
            if (x(k)%n /= y(k)%n) return
            if (x(k)%d /= y(k)%d) return
        end do
        same = .true.
    end function u_equal

    !> Peel off every rational root of a monic squarefree polynomial, by the
    !> Rational Root Theorem applied to its integer-primitive form.
    subroutine u_rational_factors(p, roots, rest, ok, why)
        type(rat_t),                  intent(in)  :: p(:)
        type(ufactor_t), allocatable, intent(out) :: roots(:)
        type(rat_t), allocatable,     intent(out) :: rest(:)
        logical,                      intent(out) :: ok
        character(:), allocatable,    intent(out) :: why
        type(rat_t), allocatable :: work(:), q(:), r(:), lin(:)
        integer(int64), allocatable :: ip(:), dn(:), dd(:)
        type(rat_t) :: root, value
        integer(int64) :: a0, an
        integer :: i, j, sgn, d
        logical :: found

        ok = .false.
        why = ""
        allocate (roots(0))
        allocate (work(size(p)))
        work = p
        call u_trim(work)
        allocate (rest(size(work)))
        rest = work

        do
            d = u_deg(rest)
            if (d < 1) exit
            call u_integer_primitive(rest, ip, ok, why)
            if (.not. ok) return
            a0 = ip(1)
            an = ip(d + 1)
            if (a0 == 0_int64) then
                ! x is a factor.
                allocate (lin(2))
                lin(1) = rat_of(0_int64)
                lin(2) = rat_of(1_int64)
                call append_factor(roots, lin, 1)
                deallocate (lin)
                call u_divmod(rest, [rat_of(0_int64), rat_of(1_int64)], q, r, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call move_alloc(q, rest)
                cycle
            end if
            call divisors_of(abs(a0), dn, ok, why)
            if (.not. ok) return
            call divisors_of(abs(an), dd, ok, why)
            if (.not. ok) return
            found = .false.
            outer: do i = 1, size(dn)
                do j = 1, size(dd)
                    do sgn = 1, -1, -2
                        call rat_make(int(sgn, int64)*dn(i), dd(j), root, ok)
                        if (.not. ok) then
                            why = OVERFLOW_WHY
                            return
                        end if
                        call u_eval(rest, root, value, ok)
                        if (.not. ok) then
                            why = OVERFLOW_WHY
                            return
                        end if
                        if (value%n /= 0_int64) cycle
                        found = .true.
                        exit outer
                    end do
                end do
            end do outer
            if (.not. found) exit
            ! x - root
            allocate (lin(2))
            lin(1) = root
            if (lin(1)%n == MIN_I64) then
                ok = .false.
                why = OVERFLOW_WHY
                return
            end if
            lin(1)%n = -lin(1)%n
            lin(2) = rat_of(1_int64)
            call append_factor(roots, lin, 1)
            call u_divmod(rest, lin, q, r, ok)
            deallocate (lin)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
            if (u_deg(r) >= 0) then
                ok = .false.
                why = "internal check failed: a verified root did not "// &
                      "divide the polynomial exactly"
                return
            end if
            call move_alloc(q, rest)
        end do
        ok = .true.
    end subroutine u_rational_factors

    !> Scale to integer coefficients with content 1.
    subroutine u_integer_primitive(p, ip, ok, why)
        type(rat_t),                 intent(in)  :: p(:)
        integer(int64), allocatable, intent(out) :: ip(:)
        logical,                     intent(out) :: ok
        character(:), allocatable,   intent(out) :: why
        integer(int64) :: l, g, m
        integer :: k, d

        ok = .false.
        why = ""
        d = u_deg(p)
        allocate (ip(max(d + 1, 0)))
        if (d < 0) then
            ok = .true.
            return
        end if
        l = 1_int64
        do k = 1, d + 1
            call lcm_i64(l, p(k)%d, m, ok)
            l = m
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
        end do
        do k = 1, d + 1
            call i_mul(p(k)%n, l/p(k)%d, ip(k), ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
        end do
        g = 0_int64
        do k = 1, d + 1
            g = gcd_i64(g, abs(ip(k)))
        end do
        if (g > 1_int64) ip = ip/g
        ok = .true.
    end subroutine u_integer_primitive

    subroutine divisors_of(n, list, ok, why)
        integer(int64),              intent(in)  :: n
        integer(int64), allocatable, intent(out) :: list(:)
        logical,                     intent(out) :: ok
        character(:), allocatable,   intent(out) :: why
        integer(int64) :: k
        integer(int64), allocatable :: buf(:)
        integer :: at

        ok = .false.
        why = ""
        allocate (list(0))
        if (n <= 0_int64) then
            why = "internal check failed: divisor enumeration of a "// &
                  "non-positive number"
            return
        end if
        if (n > MAX_FACTORABLE) then
            why = "a coefficient is too large for the rational-root search, "// &
                  "so a missing root could be mistaken for irreducibility"
            return
        end if
        allocate (buf(256))
        at = 0
        k = 1_int64
        do while (k*k <= n)
            if (mod(n, k) == 0_int64) then
                if (at + 2 > size(buf)) then
                    why = "too many divisors for the rational-root search"
                    return
                end if
                at = at + 1
                buf(at) = k
                if (n/k /= k) then
                    at = at + 1
                    buf(at) = n/k
                end if
            end if
            k = k + 1_int64
        end do
        deallocate (list)
        allocate (list(at))
        list = buf(1:at)
        ok = .true.
    end subroutine divisors_of

    ! =====================================================================
    ! Factor and Apart, at the expression level
    ! =====================================================================

    !> Factor one polynomial into an expression product.
    subroutine factor_poly_expr(a, ctx, p, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(ctx_t),               intent(in)    :: ctx
        type(poly_t),              intent(in)    :: p
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t), allocatable     :: c(:)
        type(ufactor_t), allocatable :: factors(:)
        type(rat_t)  :: lead
        type(expr_t) :: piece, acc
        type(poly_t) :: fp
        integer :: iv, k, nvars

        ok = .false.
        why = ""
        out = num(a, 0)
        if (p_is_zero(p)) then
            ok = .true.
            return
        end if

        nvars = 0
        iv = 0
        do k = 1, p%nv
            if (p_deg_v(p, k) > 0) then
                nvars = nvars + 1
                iv = k
            end if
        end do
        if (nvars == 0) then
            call rat_to_expr(a, p%c(1), out)
            ok = .true.
            return
        end if
        if (nvars > 1) then
            why = "Factor here handles one variable at a time, and this "// &
                  "polynomial has several: multivariate factorisation is "// &
                  "not implemented"
            return
        end if

        call u_from_poly(p, iv, c, ok, why)
        if (.not. ok) return
        call u_factor(c, lead, factors, ok, why)
        if (.not. ok) return

        call rat_to_expr(a, lead, acc)
        do k = 1, size(factors)
            call u_to_poly(factors(k)%c, iv, p%nv, fp)
            call poly_to_expr(a, ctx, fp, piece, ok, why)
            if (.not. ok) return
            if (factors(k)%mult == 1) then
                acc = acc*piece
            else
                acc = acc*(piece**factors(k)%mult)
            end if
        end do
        out = acc
        ok = .true.
    end subroutine factor_poly_expr

    subroutine const_divide_expr(a, e, d, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(expr_t),              intent(in)    :: e
        type(poly_t),              intent(in)    :: d
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t)  :: inv
        type(expr_t) :: scale

        out = e
        why = ""
        if (p_is_zero(d)) then
            ok = .false.
            why = "the expression divides by zero"
            return
        end if
        call rat_make(d%c(1)%d, d%c(1)%n, inv, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        if (inv%n == 1_int64) then
            if (inv%d == 1_int64) then
                ok = .true.
                return
            end if
        end if
        call rat_to_expr(a, inv, scale)
        out = scale*e
        ok = .true.
    end subroutine const_divide_expr

    subroutine u_to_poly(c, iv, nv, p)
        type(rat_t),  intent(in)  :: c(:)
        integer,      intent(in)  :: iv, nv
        type(poly_t), intent(out) :: p
        integer :: k, n, at

        n = count(c%n /= 0_int64)
        p%nv = nv
        allocate (p%ex(nv, n))
        allocate (p%c(n))
        at = 0
        do k = size(c), 1, -1
            if (c(k)%n == 0_int64) cycle
            at = at + 1
            p%ex(:, at) = 0
            p%ex(iv, at) = k - 1
            p%c(at) = c(k)
        end do
    end subroutine u_to_poly

    !> Partial fractions in one variable, by undetermined coefficients.
    !>
    !> num/den = quotient + rem/(lead*monic) with monic = prod q_i**m_i fully
    !> factored. The unknowns are the coefficients of a numerator of degree
    !> below deg(q_i) over each q_i**j, and the linear system says that the
    !> numerators, each multiplied by monic/q_i**j, add up to rem. The solved
    !> system is then re-multiplied and compared with rem exactly, so an
    !> expansion that does not reproduce the input is discarded rather than
    !> returned.
    subroutine apart_in(a, ctx, f, iv, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(ctx_t),               intent(in)    :: ctx
        type(ratf_t),              intent(in)    :: f
        integer,                   intent(in)    :: iv
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t), allocatable     :: cn(:), cd(:), quo(:), rem(:), scaled(:)
        type(rat_t), allocatable     :: cofac(:), col(:), tcol(:), power(:)
        type(rat_t), allocatable     :: monic(:), acc(:), prod(:), term(:)
        type(rat_t), allocatable     :: mat(:, :), sol(:), sum(:)
        type(ufactor_t), allocatable :: factors(:)
        type(rat_t)  :: lead, inv
        type(poly_t) :: pnum, pden
        type(expr_t) :: acc_e, piece, denom_e
        integer :: n, i, j, t, col_at, row, deg

        ok = .false.
        why = ""
        out = num(a, 0)

        call u_from_poly(f%n, iv, cn, ok, why)
        if (.not. ok) return
        call u_from_poly(f%d, iv, cd, ok, why)
        if (.not. ok) return
        if (u_deg(cd) < 1) then
            call ratf_expr(a, ctx, f, out, ok, why)
            return
        end if

        call u_divmod(cn, cd, quo, rem, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_factor(cd, lead, factors, ok, why)
        if (.not. ok) return
        call rat_make(lead%d, lead%n, inv, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call u_scale(rem, inv, scaled, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if
        call move_alloc(scaled, rem)
        call u_monic(cd, monic, ok)
        if (.not. ok) then
            why = OVERFLOW_WHY
            return
        end if

        n = u_deg(monic)
        allocate (mat(n, n + 1))
        do i = 1, n
            do j = 1, n + 1
                mat(i, j) = rat_of(0_int64)
            end do
        end do
        do i = 1, min(n, u_deg(rem) + 1)
            mat(i, n + 1) = rem(i)
        end do

        col_at = 0
        do i = 1, size(factors)
            do j = 1, factors(i)%mult
                call u_power(factors(i)%c, j, power, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call u_divmod(monic, power, cofac, col, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                if (u_deg(col) >= 0) then
                    ok = .false.
                    why = "internal check failed: a denominator factor "// &
                          "power does not divide the denominator"
                    return
                end if
                deg = u_deg(factors(i)%c)
                do t = 0, deg - 1
                    col_at = col_at + 1
                    if (col_at > n) then
                        ok = .false.
                        why = "internal check failed: too many partial "// &
                              "fraction unknowns"
                        return
                    end if
                    call u_shift(cofac, t, tcol)
                    do row = 1, min(size(tcol), n)
                        mat(row, col_at) = tcol(row)
                    end do
                end do
            end do
        end do
        if (col_at /= n) then
            ok = .false.
            why = "internal check failed: the partial fraction system is "// &
                  "not square"
            return
        end if

        call gauss_solve(mat, sol, ok, why)
        if (.not. ok) return

        ! Build the answer and, in the same pass, the exact recombination.
        allocate (acc(0))
        acc_e = num(a, 0)
        if (u_deg(quo) >= 0) then
            call u_to_poly(quo, iv, f%n%nv, pnum)
            call poly_to_expr(a, ctx, pnum, acc_e, ok, why)
            if (.not. ok) return
        end if
        col_at = 0
        do i = 1, size(factors)
            do j = 1, factors(i)%mult
                deg = u_deg(factors(i)%c)
                if (allocated(term)) deallocate (term)
                allocate (term(deg))
                do t = 1, deg
                    term(t) = sol(col_at + t)
                end do
                col_at = col_at + deg
                call u_trim(term)
                if (u_deg(term) < 0) cycle

                call u_power(factors(i)%c, j, power, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call u_divmod(monic, power, cofac, col, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call u_mul(term, cofac, prod, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call u_add(acc, prod, sum, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call move_alloc(sum, acc)

                call u_to_poly(term, iv, f%n%nv, pnum)
                call poly_to_expr(a, ctx, pnum, piece, ok, why)
                if (.not. ok) return
                call u_to_poly(factors(i)%c, iv, f%n%nv, pden)
                call poly_to_expr(a, ctx, pden, denom_e, ok, why)
                if (.not. ok) return
                if (j > 1) denom_e = denom_e**j
                acc_e = acc_e + piece/denom_e
            end do
        end do

        if (.not. u_equal(acc, rem)) then
            ok = .false.
            why = "internal check failed: the partial fraction expansion "// &
                  "does not reproduce the input"
            return
        end if
        out = acc_e
        ok = .true.
    end subroutine apart_in

    subroutine u_power(x, k, z, ok)
        type(rat_t),              intent(in)  :: x(:)
        integer,                  intent(in)  :: k
        type(rat_t), allocatable, intent(out) :: z(:)
        logical,                  intent(out) :: ok
        type(rat_t), allocatable :: t(:)
        integer :: j

        allocate (z(1))
        z(1) = rat_of(1_int64)
        ok = .true.
        do j = 1, k
            call u_mul(z, x, t, ok)
            if (.not. ok) return
            call move_alloc(t, z)
        end do
    end subroutine u_power

    !> Multiply by x**k.
    subroutine u_shift(p, k, z)
        type(rat_t),              intent(in)  :: p(:)
        integer,                  intent(in)  :: k
        type(rat_t), allocatable, intent(out) :: z(:)
        integer :: j

        allocate (z(size(p) + k))
        do j = 1, k
            z(j) = rat_of(0_int64)
        end do
        do j = 1, size(p)
            z(k + j) = p(j)
        end do
    end subroutine u_shift

    !> Exact Gaussian elimination with partial pivoting on a nonzero entry.
    subroutine gauss_solve(m, sol, ok, why)
        type(rat_t),               intent(inout) :: m(:, :)
        type(rat_t), allocatable,  intent(out)   :: sol(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(rat_t) :: factor, t, acc, u
        type(rat_t), allocatable :: row(:)
        integer :: n, i, j, k, p

        n = size(m, 1)
        allocate (sol(n))
        ok = .false.
        why = ""
        allocate (row(n + 1))
        do i = 1, n
            p = 0
            do k = i, n
                if (m(k, i)%n /= 0_int64) then
                    p = k
                    exit
                end if
            end do
            if (p == 0) then
                why = "the partial fraction system is singular, so the "// &
                      "expansion is not determined"
                return
            end if
            if (p /= i) then
                row = m(i, :)
                m(i, :) = m(p, :)
                m(p, :) = row
            end if
            do k = i + 1, n
                if (m(k, i)%n == 0_int64) cycle
                call rat_div(m(k, i), m(i, i), factor, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                do j = i, n + 1
                    call rat_mul(factor, m(i, j), t, ok)
                    if (.not. ok) then
                        why = OVERFLOW_WHY
                        return
                    end if
                    call rat_sub(m(k, j), t, u, ok)
                    m(k, j) = u
                    if (.not. ok) then
                        why = OVERFLOW_WHY
                        return
                    end if
                end do
            end do
        end do
        do i = n, 1, -1
            acc = m(i, n + 1)
            do j = i + 1, n
                call rat_mul(m(i, j), sol(j), t, ok)
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
                call rat_sub(acc, t, u, ok)
                acc = u
                if (.not. ok) then
                    why = OVERFLOW_WHY
                    return
                end if
            end do
            call rat_div(acc, m(i, i), sol(i), ok)
            if (.not. ok) then
                why = OVERFLOW_WHY
                return
            end if
        end do
        ok = .true.
    end subroutine gauss_solve

    ! =====================================================================
    ! Building expressions back
    ! =====================================================================

    function id_expr(a, id) result(e)
        type(arena_t), target, intent(inout) :: a
        integer,               intent(in)    :: id
        type(expr_t)                         :: e

        e%a => a
        e%id = id
    end function id_expr

    subroutine rat_to_expr(a, r, e)
        type(arena_t), target, intent(inout) :: a
        type(rat_t),           intent(in)    :: r
        type(expr_t),          intent(out)   :: e

        if (r%d == 1_int64) then
            e = num(a, r%n)
        else
            e = rat(a, r%n, r%d)
        end if
    end subroutine rat_to_expr

    subroutine poly_to_expr(a, ctx, p, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(ctx_t),               intent(in)    :: ctx
        type(poly_t),              intent(in)    :: p
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(expr_t) :: term, acc, base
        integer :: k, j

        ok = .true.
        why = ""
        if (p_is_zero(p)) then
            out = num(a, 0)
            return
        end if
        do k = 1, size(p%c)
            call rat_to_expr(a, p%c(k), term)
            do j = 1, p%nv
                if (p%ex(j, k) == 0) cycle
                base = id_expr(a, ctx%atom(j))
                if (p%ex(j, k) == 1) then
                    term = term*base
                else
                    term = term*(base**p%ex(j, k))
                end if
            end do
            if (k == 1) then
                acc = term
            else
                acc = acc + term
            end if
        end do
        out = acc
    end subroutine poly_to_expr

    subroutine ratf_expr(a, ctx, f, out, ok, why)
        type(arena_t), target,     intent(inout) :: a
        type(ctx_t),               intent(in)    :: ctx
        type(ratf_t),              intent(in)    :: f
        type(expr_t),              intent(out)   :: out
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        type(ratf_t) :: g
        type(expr_t) :: ne, de

        g = f
        call ratf_reduce(g, ok, why)
        if (.not. ok) return
        call poly_to_expr(a, ctx, g%n, ne, ok, why)
        if (.not. ok) return
        if (p_is_const(g%d)) then
            call const_divide_expr(a, ne, g%d, out, ok, why)
            return
        end if
        call poly_to_expr(a, ctx, g%d, de, ok, why)
        if (.not. ok) return
        out = ne/de
        ok = .true.
    end subroutine ratf_expr

    ! =====================================================================
    ! Checked exact rational arithmetic
    ! =====================================================================

    subroutine rat_make(n, d, r, ok)
        integer(int64), intent(in)  :: n, d
        type(rat_t),    intent(out) :: r
        logical,        intent(out) :: ok
        integer(int64) :: nn, dd, g

        r = rat_of(0_int64)
        ok = .false.
        if (d == 0_int64) return
        if (n == MIN_I64) return
        if (d == MIN_I64) return
        nn = n
        dd = d
        if (dd < 0_int64) then
            nn = -nn
            dd = -dd
        end if
        if (nn == 0_int64) then
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

    subroutine rat_sub(x, y, z, ok)
        type(rat_t), intent(in)  :: x, y
        type(rat_t), intent(out) :: z
        logical,     intent(out) :: ok
        type(rat_t) :: ny

        z = rat_of(0_int64)
        ok = .false.
        if (y%n == MIN_I64) return
        ny%n = -y%n
        ny%d = y%d
        call rat_add(x, ny, z, ok)
    end subroutine rat_sub

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
        if (x%n == MIN_I64) return
        if (y%n == MIN_I64) return
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

    subroutine lcm_i64(x, y, z, ok)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: z
        logical,        intent(out) :: ok
        integer(int64) :: g

        z = 0_int64
        ok = .false.
        if (x == 0_int64 .or. y == 0_int64) return
        g = gcd_i64(abs(x), abs(y))
        call i_mul(abs(x)/g, abs(y), z, ok)
    end subroutine lcm_i64

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
        if (x == MIN_I64) return
        if (y == MIN_I64) return
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
        integer(int64) :: g, a, b, t

        a = abs(x)
        b = abs(y)
        do while (b /= 0_int64)
            t = mod(a, b)
            a = b
            b = t
        end do
        g = a
        if (g == 0_int64) g = 1_int64
    end function gcd_i64

end module fortsym_poly
