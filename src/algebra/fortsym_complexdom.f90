module fortsym_complexdom
    ! The complex domain: Re, Im, conjugate, Arg, Abs, and the rectangular
    ! expansion built from them.
    !
    ! Everything here rests on one splitting routine. Given an expression it
    ! either produces a pair of expressions (a, b) with the property that the
    ! input equals a + i*b *and both a and b are provably real*, or it refuses.
    ! Every public operation is a thin reading of that pair, so there is exactly
    ! one place where reality can be got wrong.
    !
    ! The trap this module exists to avoid: a bare symbol has unknown reality.
    ! Re[x] is x only when x is known real, and a CAS that quietly assumes it is
    ! will hand back a confidently wrong answer for every complex-valued
    ! variable it is ever given. So a symbol is split only when the assumption
    ! context carries FACT_REAL for it, and is refused otherwise. The refusal is
    ! the feature; there is no fallback.
    !
    ! What is covered, and why the list stops where it does:
    !
    !   * exact and inexact real literals, pi and e -- real, imaginary part zero.
    !   * i -- the one node whose split is (0, 1).
    !   * sums and products -- the split is a homomorphism for both, so folding
    !     the children is exact rather than a heuristic.
    !   * integer powers -- repeated multiplication, and for a negative exponent
    !     one reciprocal, (p - i q)/(p^2 + q^2). A *non*-integer power is a
    !     branch-cut question this module cannot answer and refuses.
    !   * Exp, Sin, Cos of an already-split argument, via the addition formulas.
    !     These three are entire, so no branch enters. Log, Sqrt, Arg-like heads
    !     and everything else are refused rather than given a principal-branch
    !     answer the caller did not ask for.
    !
    ! Nothing here simplifies. `re_part` of a real symbol comes back as an
    ! expression that is equal to it rather than identical to it; deciding the
    ! two are the same is the engines' job, and a splitting routine that tried
    ! to do both would be harder to trust at exactly the point where trust
    ! matters.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_cache, only: expr_pair_cache_t
    use fortsym_expr, only: expr_t, num, algebraic_expr, i_expr, is_valid, &
        sin, cos, sinh, cosh, exp, sqrt, atan2, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_algebraic, only: algebraic_re, algebraic_im, algebraic_conjugate
    use fortsym_assume, only: assumption_context_t, FACT_REAL
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    private

    public :: complex_split, re_part, im_part, conjugate
    public :: arg_of, abs_of, complex_expand

    ! A power is expanded by repeated multiplication, so the *number of
    ! multiplications* grows with the exponent. This bounds that loop.
    integer(int64), parameter :: MAX_POWER = 24_int64

    ! What the loop count does not bound is the size of the result. Splitting
    ! z = x + i*y and raising it to the n-th power produces 2**n terms, so a
    ! cap on the exponent alone permits results nobody can read, evaluate or
    ! compile, and a cap applied per node is bypassed outright by nesting:
    ! (z**24)**24 has an effective exponent of 576. So the real bound is on the
    ! expanded term count, computed over the whole sub-expression before any
    ! work is done. 4096 = 2**12 admits z**12 and refuses z**13.
    integer(int64), parameter :: MAX_TERMS = 4096_int64

contains

    !> Split `e` into real and imaginary parts, both real expressions.
    !>
    !> On success `e` equals `re + i*im` exactly, and neither part contains the
    !> imaginary unit. On failure `re` and `im` are left invalid and `why` names
    !> the sub-expression that stopped the split.
    recursive subroutine complex_split(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_pair_cache_t), pointer :: cache => null()
        integer :: cached_re, cached_im

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "invalid expression has no real and imaginary parts"
            return
        end if
        if (allocated(facts%complex_cache)) then
            cache => facts%complex_cache
            if (cache%lookup(e%id, e%a%size(), e%a%generation_value(), &
                cached_re, cached_im)) then
                re = e
                im = e
                re%id = cached_re
                im%id = cached_im
                ok = .true.
                return
            end if
        end if
        if (expansion_terms(e) > MAX_TERMS) then
            why = "the rectangular expansion of this expression would have "// &
                "more than 4096 terms, which is past what this module "// &
                "will build"
            return
        end if

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL)
            re = e
            im = zero(e)
            ok = .true.
        case (NK_ALGEBRAIC)
            call split_algebraic(e, re, im, ok, why)
        case (NK_CONST)
            call split_const(e, re, im, ok, why)
        case (NK_SYM)
            call split_symbol(e, facts, re, im, ok, why)
        case (NK_ADD)
            call split_sum(e, facts, re, im, ok, why)
        case (NK_MUL)
            call split_product(e, facts, re, im, ok, why)
        case (NK_POW)
            call split_power(e, facts, re, im, ok, why)
        case (NK_FUNC)
            call split_function(e, facts, re, im, ok, why)
        case default
            why = "no complex rule for this node kind"
        end select
        if (ok .and. associated(cache)) then
            call cache%store(e%id, e%a%size(), e%a%generation_value(), &
                re%id, im%id)
        end if
    end subroutine complex_split

    !> pi and e are real, i is the imaginary unit. Any other named constant is
    !> refused: a constant fortsym does not know the value of is not known to be
    !> real either, and guessing is the failure this module is built to avoid.
    subroutine split_const(e, re, im, ok, why)
        type(expr_t),              intent(in)  :: e
        type(expr_t),              intent(out) :: re, im
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        character(:), allocatable :: name

        ok = .false.
        why = ""
        name = chars(e%name())
        select case (name)
        case ("i")
            re = zero(e)
            im = one(e)
            ok = .true.
        case ("pi", "e")
            re = e
            im = zero(e)
            ok = .true.
        case default
            why = "constant "//name//" is not known to be real"
        end select
    end subroutine split_const

    !> Split an exact algebraic atom through FLINT's exact qqbar projections.
    subroutine split_algebraic(e, re, im, ok, why)
        type(expr_t),              intent(in)  :: e
        type(expr_t),              intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t) :: real_text, imaginary_text

        ok = .false.
        why = ""
        real_text = algebraic_re(chars(e%algebraic_text()), ok)
        if (.not. ok) then
            why = "FLINT could not extract the algebraic real part"
            return
        end if
        imaginary_text = algebraic_im(chars(e%algebraic_text()), ok)
        if (.not. ok) then
            why = "FLINT could not extract the algebraic imaginary part"
            return
        end if
        re = algebraic_expr(e%a, chars(real_text), ok)
        if (.not. ok) then
            why = "FLINT could not retain the algebraic real part"
            return
        end if

        im = algebraic_expr(e%a, chars(imaginary_text), ok)
        if (.not. ok) why = "FLINT could not retain the algebraic imaginary part"
    end subroutine split_algebraic

    !> The single most dangerous case in the module, and therefore the shortest.
    subroutine split_symbol(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why

        ok = .false.
        why = ""
        if (.not. facts%has(e, FACT_REAL)) then
            why = "symbol "//chars(e%name())//" has unknown reality: "// &
                "Re and Im depend on it, so assume real_valued("// &
                chars(e%name())//") or expect a refusal"
            return
        end if
        re = e
        im = zero(e)
        ok = .true.
    end subroutine split_symbol

    !> Splitting is additive, so the parts of a sum are the sums of the parts.
    recursive subroutine split_sum(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: tre, tim
        integer :: k

        re = zero(e)
        im = zero(e)
        ok = .false.
        why = ""
        do k = 1, e%nargs()
            call complex_split(e%arg(k), facts, tre, tim, ok, why)
            if (.not. ok) return
            re = re + tre
            im = im + tim
        end do
        ok = .true.
    end subroutine split_sum

    !> Folded pairwise with the ordinary product rule. Written out rather than
    !> expanded symbolically so the identity being used stays visible.
    recursive subroutine split_product(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: tre, tim, nre, nim
        integer :: k

        re = one(e)
        im = zero(e)
        ok = .false.
        why = ""
        do k = 1, e%nargs()
            call complex_split(e%arg(k), facts, tre, tim, ok, why)
            if (.not. ok) return
            nre = re*tre - im*tim
            nim = re*tim + im*tre
            re = nre
            im = nim
        end do
        ok = .true.
    end subroutine split_product

    !> Only integer exponents. z**(1/2), z**x and z**2.5 all need a branch of
    !> the logarithm chosen, and choosing one silently is how a CAS ends up
    !> reporting the wrong sheet.
    recursive subroutine split_power(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t)   :: base, base_re, base_im, nre, nim, den, expo
        integer(int64) :: n, k

        ok = .false.
        why = ""
        expo = e%arg(2)
        if (expo%kind() /= NK_INT) then
            why = "non-integer exponent: the value depends on a branch of "// &
                "the logarithm that this module does not choose"
            return
        end if

        n = expo%int_value()
        if (n == 0_int64) then
            why = "exponent 0: z**0 is 1 only away from z = 0, which is "// &
                "not decided here"
            return
        end if
        if (abs(n) > MAX_POWER) then
            why = "exponent magnitude exceeds the expansion limit"
            return
        end if

        base = e%arg(1)
        if (provably_zero(base)) then
            why = "the base of this negative power is identically "// &
                "zero, so the expression has no value anywhere"
            return
        end if
        call complex_split(base, facts, base_re, base_im, ok, why)
        if (.not. ok) return

        re = one(e)
        im = zero(e)
        do k = 1_int64, abs(n)
            nre = re*base_re - im*base_im
            nim = re*base_im + im*base_re
            re = nre
            im = nim
        end do

        if (n < 0_int64) then
            ! 1/(p + i q) = (p - i q)/(p^2 + q^2). For a base that is not
            ! identically zero the quotient inherits the pole of the original
            ! expression at p = q = 0 and adds none, which is the general case.
            ! A base that is identically zero is the degenerate one: there the
            ! formula divides by an identically zero denominator and would hand
            ! back a pair of NaNs while claiming success, so it is refused. The
            ! test is a decision, not a syntax match: 0**(-1) and
            ! (i*i + 1)**(-1) are the same expression to it.
            if (provably_zero(base_re)) then
                if (provably_zero(base_im)) then
                    ok = .false.
                    why = "the base of this negative power is identically "// &
                        "zero, so the expression has no value anywhere"
                    return
                end if
            end if
            den = re*re + im*im
            nre = re/den
            nim = -(im/den)
            re = nre
            im = nim
        end if
        ok = .true.
    end subroutine split_power

    !> Exp, Sin, Cos, Sinh, and Cosh only. All five are entire, so the addition
    !> formulas hold everywhere and no branch is involved. Every other head,
    !> including Log and Sqrt, is refused.
    recursive subroutine split_function(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: a, b
        character(:), allocatable :: name

        ok = .false.
        why = ""
        name = chars(e%name())
        if (e%nargs() /= 1) then
            why = "no complex rule for "//name//" with this arity"
            return
        end if
        select case (name)
        case ("exp", "sin", "cos", "sinh", "cosh")
        case default
            why = "no complex rule for head "//name// &
                " (only exp, sin, cos, sinh, and cosh are split)"
            return
        end select

        call complex_split(e%arg(1), facts, a, b, ok, why)
        if (.not. ok) return

        select case (name)
        case ("exp")
            re = exp(a)*cos(b)
            im = exp(a)*sin(b)
        case ("sin")
            re = sin(a)*cosh(b)
            im = cos(a)*sinh(b)
        case ("cos")
            re = cos(a)*cosh(b)
            im = -(sin(a)*sinh(b))
        case ("sinh")
            re = sinh(a)*cos(b)
            im = cosh(a)*sin(b)
        case ("cosh")
            re = cosh(a)*cos(b)
            im = sinh(a)*sin(b)
        end select
        ok = .true.
    end subroutine split_function

    !> Re[e] as a real expression, or a refusal.
    subroutine re_part(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        out = re
    end subroutine re_part

    !> Im[e] as a real expression, or a refusal.
    subroutine im_part(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        out = im
    end subroutine im_part

    !> The complex conjugate, computed structurally rather than as re - i*im.
    !>
    !> Two routes to the same quantity is deliberate: conjugation distributes
    !> over sums, products and integer powers and commutes with the entire
    !> functions, so the recursion below never needs the parts. That makes it an
    !> independent check on the splitter rather than a restatement of it, and it
    !> keeps the result small -- conj(z**5) stays a fifth power.
    !>
    !> Its domain is deliberately wider than `complex_split`'s, and the two do
    !> not line up: `conj(z**0)` and `conj(z**1000)` succeed where the splitter
    !> refuses. That is not an oversight. The splitter refuses those because it
    !> would have to *build* the expansion, and the size of that expansion (or,
    !> at exponent 0, the value at the removable point) is the obstruction.
    !> Conjugation builds nothing: `conj(w**n) = conj(w)**n` holds wherever
    !> `w**n` is defined, whatever n is, so there is nothing here to refuse. The
    !> consequence a caller must expect is that `abs_of(e)` can refuse an `e`
    !> that `conjugate(e)` handles.
    recursive subroutine conjugate(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: part, expo
        type(str_t) :: conjugated_text
        character(:), allocatable :: name
        integer :: k

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "invalid expression has no conjugate"
            return
        end if

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL)
            out = e
            ok = .true.
        case (NK_ALGEBRAIC)
            conjugated_text = algebraic_conjugate(chars(e%algebraic_text()), ok)
            if (.not. ok) then
                why = "FLINT could not conjugate the algebraic atom"
                return
            end if
            out = algebraic_expr(e%a, chars(conjugated_text), ok)
            if (.not. ok) why = "FLINT could not retain the conjugated algebraic atom"
        case (NK_CONST)
            name = chars(e%name())
            select case (name)
            case ("i")
                out = -i_expr(e%a)
                ok = .true.
            case ("pi", "e")
                out = e
                ok = .true.
            case default
                why = "constant "//name//" is not known to be real, so its "// &
                    "conjugate is not itself"
            end select
        case (NK_SYM)
            if (.not. facts%has(e, FACT_REAL)) then
                why = "symbol "//chars(e%name())//" has unknown reality: "// &
                    "its conjugate is not itself"
                return
            end if
            out = e
            ok = .true.
        case (NK_ADD, NK_MUL)
            call conjugate(e%arg(1), facts, out, ok, why)
            if (.not. ok) return
            do k = 2, e%nargs()
                call conjugate(e%arg(k), facts, part, ok, why)
                if (.not. ok) return
                if (e%kind() == NK_ADD) then
                    out = out + part
                else
                    out = out*part
                end if
            end do
            ok = .true.
        case (NK_POW)
            expo = e%arg(2)
            if (expo%kind() /= NK_INT) then
                why = "non-integer exponent: conjugation does not commute "// &
                    "with a power whose branch is unresolved"
                return
            end if
            call conjugate(e%arg(1), facts, part, ok, why)
            if (.not. ok) return
            out = part**expo
            ok = .true.
        case (NK_FUNC)
            name = chars(e%name())
            if (e%nargs() /= 1) then
                why = "no conjugation rule for "//name//" with this arity"
                return
            end if
            select case (name)
            case ("exp", "sin", "cos")
            case default
                why = "no conjugation rule for head "//name// &
                    " (only exp, sin and cos commute here)"
                return
            end select
            call conjugate(e%arg(1), facts, part, ok, why)
            if (.not. ok) return
            select case (name)
            case ("exp")
                out = exp(part)
            case ("sin")
                out = sin(part)
            case ("cos")
                out = cos(part)
            end select
            ok = .true.
        case default
            why = "no conjugation rule for this node kind"
        end select
    end subroutine conjugate

    !> |e| as sqrt(Re^2 + Im^2). Nonnegative and real by construction, and
    !> free of any branch question because the square root is of a nonnegative
    !> real.
    subroutine abs_of(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        out = sqrt(re*re + im*im)
    end subroutine abs_of

    !> The principal argument, as atan2(Im, Re).
    !>
    !> This is the one branch choice the module does make, and it makes it
    !> explicitly: atan2 pins the result to (-pi, pi] with the same convention
    !> Fortran and C use, so a generated kernel and the symbolic form agree.
    !> The origin, where the argument is undefined rather than merely
    !> multivalued, is refused whenever the parts can be *decided* to be zero
    !> rather than merely spelled `0`: `Arg[z - z]` and `Arg[0*x]` are refused
    !> for the same reason `Arg[0]` is. Zero-testing is not decidable in
    !> general, so an expression that is identically zero and that the zero
    !> test cannot see through still gets an atan2 back; the refusal covers
    !> what can be decided, not everything that is true.
    subroutine arg_of(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        if (provably_zero(e)) then
            ok = .false.
            why = "Arg is undefined at zero"
            return
        end if
        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        if (provably_zero(re)) then
            if (provably_zero(im)) then
                ok = .false.
                why = "Arg is undefined at zero"
                return
            end if
        end if
        out = atan2(im, re)
    end subroutine arg_of

    !> The rectangular form Re[e] + i*Im[e]: the same value as `e`, written so
    !> that the real and imaginary parts are separated.
    subroutine complex_expand(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), target, intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        out = re + i_expr(e%a)*im
    end subroutine complex_expand

    !> Decides "is this expression identically zero?", one-sided.
    !>
    !> `.true.` means zero was *proved*; `.false.` means "not zero, or not
    !> decided", and every caller treats it as "carry on". Matching the literal
    !> node `0` would be no test at all -- the arena does not fold, so `x - x`,
    !> `0*x` and `i*i + 1` all reach here unfolded -- so the work is handed to
    !> the native engine's zero test, which normalises first and answers three
    !> valued. Only VERDICT_TRUE counts.
    function provably_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(native_engine_t) :: eng
        type(engine_result_t) :: r

        yes = .false.
        if (.not. is_valid(e)) return
        if (structurally_zero(e)) then
            yes = .true.
            return
        end if
        if (e%kind() == NK_INT) then
            yes = e%int_value() == 0_int64
            return
        end if
        eng = make_native_engine(e%a)
        r = eng%zero_test(e)
        if (.not. r%ok) return
        yes = r%verdict == VERDICT_TRUE
    end function provably_zero

    !> Small, engine-independent zero proofs used at domain boundaries.
    !>
    !> The native simplifier is deliberately conservative and can return
    !> UNKNOWN for a cancellation hidden inside an applied expression. Arg and
    !> negative powers need a stronger local guarantee: a proved zero must be
    !> rejected, while UNKNOWN must never be turned into a zero claim.
    recursive function structurally_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        integer :: k, j, m
        type(expr_t) :: exponent

        yes = .false.
        select case (e%kind())
        case (NK_INT, NK_RAT)
            yes = e%int_value() == 0_int64
        case (NK_REAL)
            yes = e%real_value() == 0.0
        case (NK_MUL)
            do k = 1, e%nargs()
                if (structurally_zero(e%arg(k))) then
                    yes = .true.
                    return
                end if
            end do
        case (NK_ADD)
            if (e%nargs() == 0) then
                yes = .true.
                return
            end if
            yes = .true.
            do k = 1, e%nargs()
                if (.not. structurally_zero(e%arg(k))) then
                    yes = .false.
                    exit
                end if
            end do
            if (yes) return
            if (negative_sum_cancels(e)) then
                yes = .true.
                return
            end if
            do k = 1, e%nargs()
                do j = k + 1, e%nargs()
                    if (.not. opposite_terms(e%arg(k), e%arg(j))) cycle
                    yes = .true.
                    do m = 1, e%nargs()
                        if (m == k .or. m == j) cycle
                        if (.not. structurally_zero(e%arg(m))) yes = .false.
                    end do
                    if (yes) return
                end do
            end do
        case (NK_POW)
            exponent = e%arg(2)
            if (exponent%kind() == NK_INT) then
                if (exponent%int_value() > 0_int64) then
                    yes = structurally_zero(e%arg(1))
                end if
            end if
        end select
    end function structurally_zero

    function negative_sum_cancels(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        logical, allocatable     :: used(:)
        logical                  :: found
        type(expr_t)             :: term, payload, inner, candidate
        integer                  :: k, j, m

        yes = .false.
        if (e%kind() /= NK_ADD) return
        allocate (used(e%nargs()))
        do k = 1, e%nargs()
            term = e%arg(k)
            call negated_sum(term, payload, found)
            if (.not. found) cycle
            used = .false.
            used(k) = .true.
            yes = .true.
            do j = 1, payload%nargs()
                inner = payload%arg(j)
                found = .false.
                do m = 1, e%nargs()
                    if (used(m)) cycle
                    candidate = e%arg(m)
                    if (candidate%id == inner%id) then
                        used(m) = .true.
                        found = .true.
                        exit
                    end if
                end do
                if (.not. found) then
                    yes = .false.
                    exit
                end if
            end do
            if (.not. yes) cycle
            do m = 1, e%nargs()
                if (used(m)) cycle
                if (.not. structurally_zero(e%arg(m))) then
                    yes = .false.
                    exit
                end if
            end do
            if (yes) return
        end do
    end function negative_sum_cancels

    subroutine negated_sum(e, payload, yes)
        type(expr_t), intent(in)  :: e
        type(expr_t), intent(out) :: payload
        logical, intent(out)      :: yes
        type(expr_t) :: factor, other
        integer :: k

        yes = .false.
        if (e%kind() /= NK_MUL .or. e%nargs() /= 2) return
        do k = 1, 2
            factor = e%arg(k)
            other = e%arg(3 - k)
            if (is_minus_one(factor)) then
                if (other%kind() == NK_ADD) then
                    payload = other
                    yes = .true.
                    return
                end if
            end if
        end do
    end subroutine negated_sum

    function opposite_terms(left, right) result(yes)
        type(expr_t), intent(in) :: left, right
        logical                  :: yes
        type(expr_t)             :: left_factor, left_term
        type(expr_t)             :: right_factor, right_term

        yes = .false.
        if (left%id == right%id) return
        if (left%kind() == NK_MUL) then
            if (left%nargs() == 2) then
                left_factor = left%arg(1)
                left_term = left%arg(2)
                if (is_minus_one(left_factor)) then
                    if (left_term%id == right%id) yes = .true.
                end if
                left_factor = left%arg(2)
                left_term = left%arg(1)
                if (is_minus_one(left_factor)) then
                    if (left_term%id == right%id) yes = .true.
                end if
            end if
        end if
        if (right%kind() == NK_MUL) then
            if (right%nargs() == 2) then
                right_factor = right%arg(1)
                right_term = right%arg(2)
                if (is_minus_one(right_factor)) then
                    if (right_term%id == left%id) yes = .true.
                end if
                right_factor = right%arg(2)
                right_term = right%arg(1)
                if (is_minus_one(right_factor)) then
                    if (right_term%id == left%id) yes = .true.
                end if
            end if
        end if
    end function opposite_terms

    function is_minus_one(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes

        yes = .false.
        select case (e%kind())
        case (NK_INT)
            yes = e%int_value() == -1_int64
        case (NK_RAT)
            yes = e%int_value() == -1_int64 .and. e%den_value() == 1_int64
        end select
    end function is_minus_one

    !> An upper bound on the number of terms the rectangular expansion of `e`
    !> would have, saturating at MAX_TERMS + 1 so a nested power cannot
    !> overflow the count on its way to being refused.
    !>
    !> The recursion is memoised on arena node ids. Without that a shared DAG
    !> (`u = t*t`, `v = u*u`, ...) would be walked as the tree it unfolds to,
    !> which is exactly the blow-up this function exists to prevent.
    function expansion_terms(e) result(n)
        type(expr_t), intent(in) :: e
        integer(int64)           :: n
        integer(int64), allocatable :: memo(:)

        allocate (memo(e%a%size()))
        memo = -1_int64
        n = terms_of(e, memo)
    end function expansion_terms

    recursive function terms_of(e, memo) result(n)
        type(expr_t),   intent(in)    :: e
        integer(int64), intent(inout) :: memo(:)
        integer(int64)                :: n
        type(expr_t)   :: expo
        integer(int64) :: w, p
        integer        :: k

        n = 1_int64
        if (e%id < 1 .or. e%id > size(memo)) return
        if (memo(e%id) >= 0_int64) then
            n = memo(e%id)
            return
        end if

        select case (e%kind())
        case (NK_ADD)
            n = 0_int64
            do k = 1, e%nargs()
                n = n + terms_of(e%arg(k), memo)
                if (n > MAX_TERMS) exit
            end do
        case (NK_MUL)
            n = 1_int64
            do k = 1, e%nargs()
                n = n*terms_of(e%arg(k), memo)
                if (n > MAX_TERMS) exit
            end do
        case (NK_POW)
            expo = e%arg(2)
            if (expo%kind() /= NK_INT) then
                ! Refused elsewhere for a better reason than its size.
                n = 1_int64
            else
                w = terms_of(e%arg(1), memo)
                if (w <= 1_int64) then
                    ! A base that stays one term stays one term at any
                    ! exponent, and the loop below must not be run 10**18
                    ! times to discover that.
                    n = w
                else
                    n = 1_int64
                    do p = 1_int64, abs(expo%int_value())
                        n = n*w
                        if (n > MAX_TERMS) exit
                    end do
                end if
            end if
        case (NK_FUNC)
            ! exp, sin and cos of a split argument each cost two products of
            ! the parts; other heads are refused before size matters.
            n = 1_int64
            do k = 1, e%nargs()
                n = n*2_int64*terms_of(e%arg(k), memo)
                if (n > MAX_TERMS) exit
            end do
        case default
            n = 1_int64
        end select

        if (n > MAX_TERMS) n = MAX_TERMS + 1_int64
        memo(e%id) = n
    end function terms_of

    function zero(e) result(z)
        type(expr_t), intent(in) :: e
        type(expr_t)             :: z
        z = num(e%a, 0)
    end function zero

    function one(e) result(z)
        type(expr_t), intent(in) :: e
        type(expr_t)             :: z
        z = num(e%a, 1)
    end function one

end module fortsym_complexdom
