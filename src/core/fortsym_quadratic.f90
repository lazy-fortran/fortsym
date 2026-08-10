module fortsym_quadratic
    !> Exact arithmetic in the real quadratic field Q(sqrt(d)).
    !>
    !> Some published Runge-Kutta methods are exact but not rational. DOP853's
    !> nodes are built from sqrt(6): c4 = (6-sqrt(6))/30 exactly, and every
    !> coefficient derived from it lives in Q(sqrt(6)). Rounding those to
    !> decimals and working from the decimals is how a tableau stops being
    !> checkable, so the derivation needs the field itself.
    !>
    !> An element is a + b*sqrt(d) with a, b exact rationals over FLINT and d a
    !> squarefree integer. Division is by the conjugate:
    !>
    !>     (a + b*sqrt d) / (c + e*sqrt d)
    !>         = (a + b*sqrt d)(c - e*sqrt d) / (c^2 - d*e^2)
    !>
    !> The norm c^2 - d*e^2 vanishes only when c = e = 0, because sqrt(d) is
    !> irrational for squarefree d > 1. So division is exact and total away from
    !> zero: there is no cancellation to guard against and no conditioning to
    !> reason about, which is the whole reason for carrying the field rather
    !> than a float.
    use fortsym_string, only: str_t, str, chars
    use fortsym_exact, only: exact_add, exact_sub, exact_mul, exact_div, &
        exact_normalize, exact_to_real
    implicit none
    private

    public :: quadratic_t, quad, quad_rational, quad_root
    public :: quad_add, quad_sub, quad_mul, quad_div, quad_pow, quad_neg
    public :: quad_is_zero, quad_equal, quad_conjugate, quad_norm
    public :: quad_to_real, quad_text

    !> a + b*sqrt(radicand), with a and b exact rationals as canonical text.
    type :: quadratic_t
        type(str_t) :: a
        type(str_t) :: b
        !> Squarefree d > 1. Elements of different radicands never mix.
        integer :: radicand = 0
    end type quadratic_t

contains

    !> a + b*sqrt(d) from exact rational text.
    function quad(a_text, b_text, radicand, ok) result(value)
        character(*), intent(in) :: a_text, b_text
        integer, intent(in) :: radicand
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        logical :: a_ok, b_ok

        value%radicand = radicand
        value%a = exact_normalize(a_text, a_ok)
        value%b = exact_normalize(b_text, b_ok)
        ok = a_ok .and. b_ok .and. radicand > 1
    end function quad

    !> A rational, carried in the same field so it can meet other elements.
    function quad_rational(a_text, radicand, ok) result(value)
        character(*), intent(in) :: a_text
        integer, intent(in) :: radicand
        logical, intent(out) :: ok
        type(quadratic_t) :: value

        value = quad(a_text, "0", radicand, ok)
    end function quad_rational

    !> sqrt(d) itself.
    function quad_root(radicand, ok) result(value)
        integer, intent(in) :: radicand
        logical, intent(out) :: ok
        type(quadratic_t) :: value

        value = quad("0", "1", radicand, ok)
    end function quad_root

    function quad_add(x, y, ok) result(value)
        type(quadratic_t), intent(in) :: x, y
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        logical :: a_ok, b_ok

        value%radicand = x%radicand
        ok = x%radicand == y%radicand
        if (.not. ok) return
        value%a = exact_add(chars(x%a), chars(y%a), a_ok)
        value%b = exact_add(chars(x%b), chars(y%b), b_ok)
        ok = a_ok .and. b_ok
    end function quad_add

    function quad_sub(x, y, ok) result(value)
        type(quadratic_t), intent(in) :: x, y
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        logical :: a_ok, b_ok

        value%radicand = x%radicand
        ok = x%radicand == y%radicand
        if (.not. ok) return
        value%a = exact_sub(chars(x%a), chars(y%a), a_ok)
        value%b = exact_sub(chars(x%b), chars(y%b), b_ok)
        ok = a_ok .and. b_ok
    end function quad_sub

    function quad_neg(x, ok) result(value)
        type(quadratic_t), intent(in) :: x
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        type(quadratic_t) :: zero

        zero = quad_rational("0", x%radicand, ok)
        if (.not. ok) return
        value = quad_sub(zero, x, ok)
    end function quad_neg

    !> (a + b r)(c + e r) = (ac + d be) + (ae + bc) r.
    function quad_mul(x, y, ok) result(value)
        type(quadratic_t), intent(in) :: x, y
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        type(str_t) :: ac, be, ae, bc, d_be
        character(len=32) :: d_text
        logical :: s(5)

        value%radicand = x%radicand
        ok = x%radicand == y%radicand
        if (.not. ok) return
        write (d_text, "(i0)") x%radicand
        ac = exact_mul(chars(x%a), chars(y%a), s(1))
        be = exact_mul(chars(x%b), chars(y%b), s(2))
        d_be = exact_mul(trim(d_text), chars(be), s(3))
        value%a = exact_add(chars(ac), chars(d_be), s(4))
        ae = exact_mul(chars(x%a), chars(y%b), s(5))
        bc = exact_mul(chars(x%b), chars(y%a), ok)
        if (.not. (all(s) .and. ok)) then
            ok = .false.
            return
        end if
        value%b = exact_add(chars(ae), chars(bc), ok)
    end function quad_mul

    !> Conjugate a - b*sqrt(d), the other root of the same minimal polynomial.
    function quad_conjugate(x, ok) result(value)
        type(quadratic_t), intent(in) :: x
        logical, intent(out) :: ok
        type(quadratic_t) :: value

        value%radicand = x%radicand
        value%a = x%a
        value%b = exact_sub("0", chars(x%b), ok)
    end function quad_conjugate

    !> Field norm a^2 - d b^2, a rational. Zero only for the zero element.
    function quad_norm(x, ok) result(value)
        type(quadratic_t), intent(in) :: x
        logical, intent(out) :: ok
        type(str_t) :: value
        type(str_t) :: aa, bb, d_bb
        character(len=32) :: d_text
        logical :: s(3)

        write (d_text, "(i0)") x%radicand
        aa = exact_mul(chars(x%a), chars(x%a), s(1))
        bb = exact_mul(chars(x%b), chars(x%b), s(2))
        d_bb = exact_mul(trim(d_text), chars(bb), s(3))
        value = exact_sub(chars(aa), chars(d_bb), ok)
        ok = ok .and. all(s)
    end function quad_norm

    !> x / y, by the conjugate. ok is false when y is zero.
    function quad_div(x, y, ok) result(value)
        type(quadratic_t), intent(in) :: x, y
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        type(quadratic_t) :: numerator
        type(str_t) :: n
        logical :: s(3)

        value%radicand = x%radicand
        ok = x%radicand == y%radicand
        if (.not. ok) return
        if (quad_is_zero(y)) then
            ok = .false.
            return
        end if
        numerator = quad_mul(x, quad_conjugate(y, s(1)), s(2))
        n = quad_norm(y, s(3))
        if (.not. all(s)) then
            ok = .false.
            return
        end if
        value%a = exact_div(chars(numerator%a), chars(n), s(1))
        value%b = exact_div(chars(numerator%b), chars(n), ok)
        ok = ok .and. s(1)
    end function quad_div

    function quad_pow(x, exponent, ok) result(value)
        type(quadratic_t), intent(in) :: x
        integer, intent(in) :: exponent
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        integer :: k

        value = quad_rational("1", x%radicand, ok)
        if (.not. ok) return
        do k = 1, exponent
            value = quad_mul(value, x, ok)
            if (.not. ok) return
        end do
    end function quad_pow

    !> True when both components vanish.
    !>
    !> Both sides go through the same renderer, so this does not depend on the
    !> spelling FLINT happens to choose for a canonical zero.
    function quad_is_zero(x) result(is_zero)
        type(quadratic_t), intent(in) :: x
        logical :: is_zero
        type(str_t) :: zero, ca, cb
        logical :: s(3)

        zero = exact_normalize("0", s(1))
        ca = exact_normalize(chars(x%a), s(2))
        cb = exact_normalize(chars(x%b), s(3))
        is_zero = all(s) .and. chars(ca) == chars(zero) .and. &
                  chars(cb) == chars(zero)
    end function quad_is_zero

    function quad_equal(x, y) result(same)
        type(quadratic_t), intent(in) :: x, y
        logical :: same
        logical :: ok

        same = x%radicand == y%radicand
        if (.not. same) return
        same = quad_is_zero(quad_sub(x, y, ok)) .and. ok
    end function quad_equal

    !> Nearest double. For comparison against published decimals only; nothing
    !> in the derivation goes through this.
    function quad_to_real(x, ok) result(value)
        type(quadratic_t), intent(in) :: x
        logical, intent(out) :: ok
        double precision :: value
        double precision :: a, b
        logical :: a_ok, b_ok

        a = exact_to_real(chars(x%a), a_ok)
        b = exact_to_real(chars(x%b), b_ok)
        ok = a_ok .and. b_ok
        value = a + b*sqrt(dble(x%radicand))
    end function quad_to_real

    function quad_text(x) result(text)
        type(quadratic_t), intent(in) :: x
        character(:), allocatable :: text
        character(len=32) :: d_text

        write (d_text, "(i0)") x%radicand
        text = chars(x%a)//" + "//chars(x%b)//"*sqrt("//trim(d_text)//")"
    end function quad_text

end module fortsym_quadratic
