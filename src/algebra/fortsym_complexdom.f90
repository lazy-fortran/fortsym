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
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr, only: expr_t, num, i_expr, is_valid, &
        sin, cos, sinh, cosh, exp, sqrt, atan2, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_assume, only: assumption_context_t, FACT_REAL
    implicit none
    private

    public :: complex_split, re_part, im_part, conjugate
    public :: arg_of, abs_of, complex_expand

    ! A power is expanded by repeated multiplication, so the work grows with the
    ! exponent. Past this the honest answer is a refusal rather than an
    ! expression nobody can read or evaluate.
    integer(int64), parameter :: MAX_POWER = 24_int64

contains

    !> Split `e` into real and imaginary parts, both real expressions.
    !>
    !> On success `e` equals `re + i*im` exactly, and neither part contains the
    !> imaginary unit. On failure `re` and `im` are left invalid and `why` names
    !> the sub-expression that stopped the split.
    recursive subroutine complex_split(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "invalid expression has no real and imaginary parts"
            return
        end if

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT)
            re = e
            im = zero(e)
            ok = .true.
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

    !> The single most dangerous case in the module, and therefore the shortest.
    subroutine split_symbol(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), intent(in)  :: facts
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
        type(assumption_context_t), intent(in)  :: facts
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
        type(assumption_context_t), intent(in)  :: facts
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
        type(assumption_context_t), intent(in)  :: facts
        type(expr_t),               intent(out) :: re, im
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t)   :: base_re, base_im, nre, nim, den, expo
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

        call complex_split(e%arg(1), facts, base_re, base_im, ok, why)
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
            ! 1/(p + i q) = (p - i q)/(p^2 + q^2). The quotient inherits the
            ! pole of the original expression at p = q = 0 and adds none.
            den = re*re + im*im
            nre = re/den
            nim = -(im/den)
            re = nre
            im = nim
        end if
        ok = .true.
    end subroutine split_power

    !> Exp, Sin and Cos only. All three are entire, so the addition formulas
    !> hold everywhere and no branch is involved. Every other head, including
    !> Log and Sqrt, is refused.
    recursive subroutine split_function(e, facts, re, im, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), intent(in)  :: facts
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
        case ("exp", "sin", "cos")
        case default
            why = "no complex rule for head "//name// &
                  " (only exp, sin and cos are split)"
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
        end select
        ok = .true.
    end subroutine split_function

    !> Re[e] as a real expression, or a refusal.
    subroutine re_part(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), intent(in)  :: facts
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
        type(assumption_context_t), intent(in)  :: facts
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
    recursive subroutine conjugate(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: part, expo
        character(:), allocatable :: name
        integer :: k

        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "invalid expression has no conjugate"
            return
        end if

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL, NK_BIG_INT, NK_BIG_RAT)
            out = e
            ok = .true.
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
        type(assumption_context_t), intent(in)  :: facts
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
    !> multivalued, is refused when it can be recognised outright.
    subroutine arg_of(e, facts, out, ok, why)
        type(expr_t),               intent(in)  :: e
        type(assumption_context_t), intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        if (is_literal_zero(re)) then
            if (is_literal_zero(im)) then
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
        type(assumption_context_t), intent(in)  :: facts
        type(expr_t),               intent(out) :: out
        logical,                    intent(out) :: ok
        character(:), allocatable,  intent(out) :: why
        type(expr_t) :: re, im

        call complex_split(e, facts, re, im, ok, why)
        if (.not. ok) return
        out = re + i_expr(e%a)*im
    end subroutine complex_expand

    !> Recognises the interned integer zero only. A structurally different but
    !> mathematically zero expression is not claimed to be zero here, which is
    !> the conservative direction: it costs an unnecessary Arg, never a wrong
    !> one.
    function is_literal_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes

        yes = .false.
        if (e%kind() /= NK_INT) return
        yes = e%int_value() == 0_int64
    end function is_literal_zero

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
