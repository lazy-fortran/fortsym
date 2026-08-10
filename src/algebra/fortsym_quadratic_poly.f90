module fortsym_quadratic_poly
    !> Univariate polynomials over Q(sqrt(d)), and the gcd that closes a
    !> reduced Runge-Kutta system.
    !>
    !> The last step of Dormand and Prince's construction leaves two conditions
    !> that are quadratic in one remaining parameter. Solving either alone would
    !> need a square root, which need not exist in the field. But the two share
    !> the sought parameter as a common root, so their gcd is the factor that
    !> both agree on. When that gcd is linear the parameter follows by one
    !> division and the whole tableau stays inside Q(sqrt(d)) -- no radical
    !> beyond the one the field already carries, and no numerical root finding.
    !>
    !> Coefficients are stored lowest degree first. The zero polynomial has no
    !> terms, and `poly_degree` reports -1 for it.
    use fortsym_quadratic, only: quadratic_t, quad_rational, quad_add, &
        quad_sub, quad_mul, quad_div, quad_is_zero, quad_neg
    implicit none
    private

    public :: qpoly_t, qpoly_from, qpoly_degree, qpoly_add, qpoly_sub
    public :: qpoly_mul, qpoly_scale, qpoly_is_zero, qpoly_eval
    public :: qpoly_remainder, qpoly_gcd, qpoly_linear_root

    type :: qpoly_t
        !> coefficient(k) multiplies x**(k-1).
        type(quadratic_t), allocatable :: coefficient(:)
    end type qpoly_t

contains

    function qpoly_from(coefficients) result(p)
        type(quadratic_t), intent(in) :: coefficients(:)
        type(qpoly_t) :: p

        p%coefficient = coefficients
    end function qpoly_from

    !> Highest index with a non-zero coefficient, or -1 for the zero
    !> polynomial. Trailing zeros never change the degree.
    function qpoly_degree(p) result(d)
        type(qpoly_t), intent(in) :: p
        integer :: d
        integer :: k

        d = -1
        if (.not. allocated(p%coefficient)) return
        do k = size(p%coefficient), 1, -1
            if (.not. quad_is_zero(p%coefficient(k))) then
                d = k - 1
                return
            end if
        end do
    end function qpoly_degree

    function qpoly_is_zero(p) result(is_zero)
        type(qpoly_t), intent(in) :: p
        logical :: is_zero

        is_zero = qpoly_degree(p) < 0
    end function qpoly_is_zero

    function qpoly_add(p, q, ok) result(r)
        type(qpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qpoly_t) :: r
        integer :: n, k
        type(quadratic_t) :: zero

        n = max(size(p%coefficient), size(q%coefficient))
        zero = quad_rational("0", p%coefficient(1)%radicand, ok)
        if (.not. ok) return
        allocate (r%coefficient(n))
        r%coefficient = zero
        do k = 1, n
            r%coefficient(k) = zero
            if (k <= size(p%coefficient)) then
                r%coefficient(k) = quad_add(r%coefficient(k), &
                                            p%coefficient(k), ok)
                if (.not. ok) return
            end if
            if (k <= size(q%coefficient)) then
                r%coefficient(k) = quad_add(r%coefficient(k), &
                                            q%coefficient(k), ok)
                if (.not. ok) return
            end if
        end do
    end function qpoly_add

    function qpoly_sub(p, q, ok) result(r)
        type(qpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qpoly_t) :: r
        type(qpoly_t) :: negated
        integer :: k

        allocate (negated%coefficient(size(q%coefficient)))
        do k = 1, size(q%coefficient)
            negated%coefficient(k) = quad_neg(q%coefficient(k), ok)
            if (.not. ok) return
        end do
        r = qpoly_add(p, negated, ok)
    end function qpoly_sub

    function qpoly_mul(p, q, ok) result(r)
        type(qpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qpoly_t) :: r
        integer :: i, j
        type(quadratic_t) :: zero, term

        zero = quad_rational("0", p%coefficient(1)%radicand, ok)
        if (.not. ok) return
        allocate (r%coefficient(size(p%coefficient) + size(q%coefficient) - 1))
        r%coefficient = zero
        do i = 1, size(p%coefficient)
            do j = 1, size(q%coefficient)
                term = quad_mul(p%coefficient(i), q%coefficient(j), ok)
                if (.not. ok) return
                r%coefficient(i + j - 1) = quad_add(r%coefficient(i + j - 1), &
                                                    term, ok)
                if (.not. ok) return
            end do
        end do
    end function qpoly_mul

    function qpoly_scale(p, factor, ok) result(r)
        type(qpoly_t), intent(in) :: p
        type(quadratic_t), intent(in) :: factor
        logical, intent(out) :: ok
        type(qpoly_t) :: r
        integer :: k

        allocate (r%coefficient(size(p%coefficient)))
        do k = 1, size(p%coefficient)
            r%coefficient(k) = quad_mul(p%coefficient(k), factor, ok)
            if (.not. ok) return
        end do
    end function qpoly_scale

    function qpoly_eval(p, x, ok) result(value)
        type(qpoly_t), intent(in) :: p
        type(quadratic_t), intent(in) :: x
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        integer :: k

        value = quad_rational("0", x%radicand, ok)
        if (.not. ok) return
        do k = size(p%coefficient), 1, -1
            value = quad_mul(value, x, ok)
            if (.not. ok) return
            value = quad_add(value, p%coefficient(k), ok)
            if (.not. ok) return
        end do
    end function qpoly_eval

    !> Remainder of p divided by q. Exact: the field has inverses, so the
    !> leading coefficient always divides.
    function qpoly_remainder(p, q, ok) result(r)
        type(qpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qpoly_t) :: r
        integer :: dp_, dq, shift, k
        type(quadratic_t) :: factor, term

        r = p
        ok = .true.
        dq = qpoly_degree(q)
        if (dq < 0) then
            ok = .false.
            return
        end if
        do
            dp_ = qpoly_degree(r)
            if (dp_ < dq) return
            factor = quad_div(r%coefficient(dp_ + 1), q%coefficient(dq + 1), ok)
            if (.not. ok) return
            shift = dp_ - dq
            do k = 0, dq
                term = quad_mul(factor, q%coefficient(k + 1), ok)
                if (.not. ok) return
                r%coefficient(shift + k + 1) = &
                    quad_sub(r%coefficient(shift + k + 1), term, ok)
                if (.not. ok) return
            end do
        end do
    end function qpoly_remainder

    !> Euclidean gcd, monic-free. Two conditions that must both hold on the
    !> same parameter share it as a root, so the gcd carries it.
    function qpoly_gcd(p, q, ok) result(g)
        type(qpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qpoly_t) :: g
        type(qpoly_t) :: a, b, r
        integer :: guard

        a = p
        b = q
        ok = .true.
        guard = 0
        do
            if (qpoly_is_zero(b)) exit
            r = qpoly_remainder(a, b, ok)
            if (.not. ok) return
            a = b
            b = r
            guard = guard + 1
            if (guard > 64) then
                ok = .false.
                return
            end if
        end do
        g = a
    end function qpoly_gcd

    !> The root of a degree-one polynomial, -c0/c1.
    !>
    !> ok is false unless the polynomial really is linear, so a caller cannot
    !> mistake a quadratic gcd -- two shared roots, an ambiguous construction --
    !> for a determined one.
    function qpoly_linear_root(p, ok) result(root)
        type(qpoly_t), intent(in) :: p
        logical, intent(out) :: ok
        type(quadratic_t) :: root
        type(quadratic_t) :: negated

        ok = qpoly_degree(p) == 1
        if (.not. ok) then
            root = p%coefficient(1)
            return
        end if
        negated = quad_neg(p%coefficient(1), ok)
        if (.not. ok) return
        root = quad_div(negated, p%coefficient(2), ok)
    end function qpoly_linear_root

end module fortsym_quadratic_poly
