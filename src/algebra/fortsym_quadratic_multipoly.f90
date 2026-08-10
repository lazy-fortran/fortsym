module fortsym_quadratic_multipoly
    !> Sparse multivariate polynomials over Q(sqrt(d)), with elimination.
    !>
    !> A term is an exponent vector and a coefficient in the field. Terms are
    !> kept sorted by exponent and merged on construction, so a polynomial has
    !> one canonical representation and equality is structural: two expressions
    !> that are mathematically equal compare equal without a normalisation pass.
    !> A coefficient that cancels to zero is dropped rather than stored, which
    !> is what makes `qmpoly_is_zero` exact.
    !>
    !> Elimination is by the Sylvester resultant. Given two polynomials sharing
    !> a root in one variable, the resultant is the polynomial in the remaining
    !> variables that vanishes exactly when such a shared root exists, so it
    !> removes a variable without approximating anything. That is what closes a
    !> reduced Runge-Kutta system: the closing conditions are polynomial in the
    !> free parameters, and eliminating one leaves a univariate polynomial whose
    !> roots are the admissible methods.
    !>
    !> The Sylvester determinant is taken by cofactor expansion over polynomial
    !> entries. Exact, and the matrices that arise here are small.
    use fortsym_quadratic, only: quadratic_t, quad_rational, quad_add, &
        quad_sub, quad_mul, quad_div, quad_neg, quad_is_zero, quad_equal
    use fortsym_quadratic_poly, only: qpoly_t, qpoly_from
    implicit none
    private

    public :: qmpoly_t, qmterm_t
    public :: qmpoly_zero, qmpoly_const, qmpoly_var, qmpoly_from_terms
    public :: qmpoly_add, qmpoly_sub, qmpoly_mul, qmpoly_neg, qmpoly_scale
    public :: qmpoly_pow, qmpoly_is_zero, qmpoly_equal
    public :: qmpoly_degree, qmpoly_coefficient_of, qmpoly_total_degree
    public :: qmpoly_eval_var, qmpoly_substitute, qmpoly_eval_all
    public :: qmpoly_resultant, qmpoly_to_univariate, qmpoly_nvars

    type :: qmterm_t
        integer, allocatable :: exponent(:)
        type(quadratic_t) :: coefficient
    end type qmterm_t

    type :: qmpoly_t
        integer :: nvars = 0
        integer :: radicand = 0
        !> Sorted by exponent vector, no zero coefficients, no duplicates.
        type(qmterm_t), allocatable :: term(:)
    end type qmpoly_t

contains

    pure function qmpoly_nvars(p) result(n)
        type(qmpoly_t), intent(in) :: p
        integer :: n
        n = p%nvars
    end function qmpoly_nvars

    function qmpoly_zero(nvars, radicand) result(p)
        integer, intent(in) :: nvars, radicand
        type(qmpoly_t) :: p

        p%nvars = nvars
        p%radicand = radicand
        allocate (p%term(0))
    end function qmpoly_zero

    function qmpoly_const(value, nvars) result(p)
        type(quadratic_t), intent(in) :: value
        integer, intent(in) :: nvars
        type(qmpoly_t) :: p

        p = qmpoly_zero(nvars, value%radicand)
        if (quad_is_zero(value)) return
        deallocate (p%term)
        allocate (p%term(1))
        allocate (p%term(1)%exponent(nvars))
        p%term(1)%exponent = 0
        p%term(1)%coefficient = value
    end function qmpoly_const

    function qmpoly_var(index, nvars, radicand, ok) result(p)
        integer, intent(in) :: index, nvars, radicand
        logical, intent(out) :: ok
        type(qmpoly_t) :: p

        p = qmpoly_zero(nvars, radicand)
        ok = index >= 1 .and. index <= nvars
        if (.not. ok) return
        deallocate (p%term)
        allocate (p%term(1))
        allocate (p%term(1)%exponent(nvars))
        p%term(1)%exponent = 0
        p%term(1)%exponent(index) = 1
        p%term(1)%coefficient = quad_rational("1", radicand, ok)
    end function qmpoly_var

    !> Canonicalise an arbitrary term list: sort, merge equal exponents, drop
    !> zero coefficients.
    function qmpoly_from_terms(terms, nvars, radicand, ok) result(p)
        type(qmterm_t), intent(in) :: terms(:)
        integer, intent(in) :: nvars, radicand
        logical, intent(out) :: ok
        type(qmpoly_t) :: p

        type(qmterm_t), allocatable :: work(:), kept_terms(:)
        integer :: n, i, j, kept
        type(quadratic_t) :: total

        ok = .true.
        n = size(terms)
        p = qmpoly_zero(nvars, radicand)
        if (n == 0) return
        allocate (work(n))
        work = terms
        call sort_terms(work)

        if (allocated(p%term)) deallocate (p%term)
        allocate (kept_terms(n))
        kept = 0
        i = 1
        do while (i <= n)
            total = work(i)%coefficient
            j = i + 1
            do while (j <= n)
                if (.not. same_exponent(work(j)%exponent, work(i)%exponent)) exit
                total = quad_add(total, work(j)%coefficient, ok)
                if (.not. ok) return
                j = j + 1
            end do
            if (.not. quad_is_zero(total)) then
                kept = kept + 1
                kept_terms(kept)%exponent = work(i)%exponent
                kept_terms(kept)%coefficient = total
            end if
            i = j
        end do
        allocate (p%term(kept))
        do i = 1, kept
            p%term(i) = kept_terms(i)
        end do
    end function qmpoly_from_terms

    pure function same_exponent(x, y) result(same)
        integer, intent(in) :: x(:), y(:)
        logical :: same
        same = size(x) == size(y)
        if (same) same = all(x == y)
    end function same_exponent

    !> Lexicographic order on exponent vectors, so the canonical form is unique.
    pure function exponent_less(x, y) result(less)
        integer, intent(in) :: x(:), y(:)
        logical :: less
        integer :: k

        less = .false.
        do k = 1, size(x)
            if (x(k) < y(k)) then
                less = .true.
                return
            else if (x(k) > y(k)) then
                return
            end if
        end do
    end function exponent_less

    subroutine sort_terms(terms)
        type(qmterm_t), intent(inout) :: terms(:)
        integer :: i, j
        type(qmterm_t) :: pivot

        do i = 2, size(terms)
            pivot = terms(i)
            j = i - 1
            do while (j >= 1)
                if (.not. exponent_less(pivot%exponent, terms(j)%exponent)) exit
                terms(j + 1) = terms(j)
                j = j - 1
            end do
            terms(j + 1) = pivot
        end do
    end subroutine sort_terms

    function qmpoly_add(p, q, ok) result(r)
        type(qmpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        type(qmterm_t), allocatable :: merged(:)

        ok = p%nvars == q%nvars .and. p%radicand == q%radicand
        if (.not. ok) then
            r = qmpoly_zero(p%nvars, p%radicand)
            return
        end if
        allocate (merged(size(p%term) + size(q%term)))
        if (size(p%term) > 0) merged(1:size(p%term)) = p%term
        if (size(q%term) > 0) merged(size(p%term) + 1:) = q%term
        r = qmpoly_from_terms(merged, p%nvars, p%radicand, ok)
    end function qmpoly_add

    function qmpoly_neg(p, ok) result(r)
        type(qmpoly_t), intent(in) :: p
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        integer :: k

        r = p
        ok = .true.
        do k = 1, size(r%term)
            r%term(k)%coefficient = quad_neg(r%term(k)%coefficient, ok)
            if (.not. ok) return
        end do
    end function qmpoly_neg

    function qmpoly_sub(p, q, ok) result(r)
        type(qmpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qmpoly_t) :: r

        r = qmpoly_add(p, qmpoly_neg(q, ok), ok)
    end function qmpoly_sub

    function qmpoly_mul(p, q, ok) result(r)
        type(qmpoly_t), intent(in) :: p, q
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        type(qmterm_t), allocatable :: products(:)
        integer :: i, j, n

        ok = p%nvars == q%nvars .and. p%radicand == q%radicand
        if (.not. ok) then
            r = qmpoly_zero(p%nvars, p%radicand)
            return
        end if
        n = 0
        allocate (products(max(1, size(p%term)*size(q%term))))
        do i = 1, size(p%term)
            do j = 1, size(q%term)
                n = n + 1
                products(n)%exponent = p%term(i)%exponent + q%term(j)%exponent
                products(n)%coefficient = quad_mul(p%term(i)%coefficient, &
                                                   q%term(j)%coefficient, ok)
                if (.not. ok) return
            end do
        end do
        r = qmpoly_from_terms(products(1:n), p%nvars, p%radicand, ok)
    end function qmpoly_mul

    function qmpoly_scale(p, factor, ok) result(r)
        type(qmpoly_t), intent(in) :: p
        type(quadratic_t), intent(in) :: factor
        logical, intent(out) :: ok
        type(qmpoly_t) :: r

        r = qmpoly_mul(p, qmpoly_const(factor, p%nvars), ok)
    end function qmpoly_scale

    function qmpoly_pow(p, exponent, ok) result(r)
        type(qmpoly_t), intent(in) :: p
        integer, intent(in) :: exponent
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        integer :: k
        type(quadratic_t) :: one

        one = quad_rational("1", p%radicand, ok)
        r = qmpoly_const(one, p%nvars)
        if (.not. ok) return
        do k = 1, exponent
            r = qmpoly_mul(r, p, ok)
            if (.not. ok) return
        end do
    end function qmpoly_pow

    pure function qmpoly_is_zero(p) result(is_zero)
        type(qmpoly_t), intent(in) :: p
        logical :: is_zero
        is_zero = size(p%term) == 0
    end function qmpoly_is_zero

    function qmpoly_equal(p, q) result(same)
        type(qmpoly_t), intent(in) :: p, q
        logical :: same
        logical :: ok

        same = qmpoly_is_zero(qmpoly_sub(p, q, ok)) .and. ok
    end function qmpoly_equal

    pure function qmpoly_degree(p, var) result(d)
        type(qmpoly_t), intent(in) :: p
        integer, intent(in) :: var
        integer :: d
        integer :: k

        d = -1
        do k = 1, size(p%term)
            d = max(d, p%term(k)%exponent(var))
        end do
    end function qmpoly_degree

    pure function qmpoly_total_degree(p) result(d)
        type(qmpoly_t), intent(in) :: p
        integer :: d
        integer :: k

        d = -1
        do k = 1, size(p%term)
            d = max(d, sum(p%term(k)%exponent))
        end do
    end function qmpoly_total_degree

    !> The coefficient of var**power: a polynomial in the same variables with
    !> that variable's exponent set to zero.
    function qmpoly_coefficient_of(p, var, power, ok) result(r)
        type(qmpoly_t), intent(in) :: p
        integer, intent(in) :: var, power
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        type(qmterm_t), allocatable :: picked(:)
        integer :: k, n

        allocate (picked(size(p%term)))
        n = 0
        do k = 1, size(p%term)
            if (p%term(k)%exponent(var) /= power) cycle
            n = n + 1
            picked(n)%exponent = p%term(k)%exponent
            picked(n)%exponent(var) = 0
            picked(n)%coefficient = p%term(k)%coefficient
        end do
        r = qmpoly_from_terms(picked(1:n), p%nvars, p%radicand, ok)
    end function qmpoly_coefficient_of

    !> Replace one variable by a field element.
    function qmpoly_eval_var(p, var, value, ok) result(r)
        type(qmpoly_t), intent(in) :: p
        integer, intent(in) :: var
        type(quadratic_t), intent(in) :: value
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        type(qmterm_t), allocatable :: mapped(:)
        integer :: k, e
        type(quadratic_t) :: factor

        allocate (mapped(size(p%term)))
        ok = .true.
        do k = 1, size(p%term)
            factor = quad_rational("1", p%radicand, ok)
            if (.not. ok) return
            do e = 1, p%term(k)%exponent(var)
                factor = quad_mul(factor, value, ok)
                if (.not. ok) return
            end do
            mapped(k)%exponent = p%term(k)%exponent
            mapped(k)%exponent(var) = 0
            mapped(k)%coefficient = quad_mul(p%term(k)%coefficient, factor, ok)
            if (.not. ok) return
        end do
        r = qmpoly_from_terms(mapped, p%nvars, p%radicand, ok)
    end function qmpoly_eval_var

    !> Replace one variable by a polynomial.
    function qmpoly_substitute(p, var, replacement, ok) result(r)
        type(qmpoly_t), intent(in) :: p, replacement
        integer, intent(in) :: var
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        type(qmpoly_t) :: piece, powered, stripped
        type(qmterm_t) :: one_term(1)
        integer :: k

        r = qmpoly_zero(p%nvars, p%radicand)
        ok = .true.
        do k = 1, size(p%term)
            allocate (one_term(1)%exponent(p%nvars))
            one_term(1)%exponent = p%term(k)%exponent
            one_term(1)%exponent(var) = 0
            one_term(1)%coefficient = p%term(k)%coefficient
            stripped = qmpoly_from_terms(one_term, p%nvars, p%radicand, ok)
            deallocate (one_term(1)%exponent)
            if (.not. ok) return
            powered = qmpoly_pow(replacement, p%term(k)%exponent(var), ok)
            if (.not. ok) return
            piece = qmpoly_mul(stripped, powered, ok)
            if (.not. ok) return
            r = qmpoly_add(r, piece, ok)
            if (.not. ok) return
        end do
    end function qmpoly_substitute

    function qmpoly_eval_all(p, values, ok) result(value)
        type(qmpoly_t), intent(in) :: p
        type(quadratic_t), intent(in) :: values(:)
        logical, intent(out) :: ok
        type(quadratic_t) :: value
        type(qmpoly_t) :: work
        integer :: k

        work = p
        ok = .true.
        do k = 1, p%nvars
            work = qmpoly_eval_var(work, k, values(k), ok)
            if (.not. ok) return
        end do
        value = quad_rational("0", p%radicand, ok)
        if (.not. ok) return
        if (size(work%term) == 1) value = work%term(1)%coefficient
    end function qmpoly_eval_all

    !> A polynomial in one remaining variable, as a dense univariate.
    function qmpoly_to_univariate(p, var, ok) result(u)
        type(qmpoly_t), intent(in) :: p
        integer, intent(in) :: var
        logical, intent(out) :: ok
        type(qpoly_t) :: u
        type(quadratic_t), allocatable :: coefficients(:)
        type(qmpoly_t) :: piece
        integer :: d, k

        d = max(qmpoly_degree(p, var), 0)
        allocate (coefficients(d + 1))
        ok = .true.
        do k = 0, d
            piece = qmpoly_coefficient_of(p, var, k, ok)
            if (.not. ok) return
            coefficients(k + 1) = quad_rational("0", p%radicand, ok)
            if (.not. ok) return
            if (size(piece%term) == 1) then
                ! Must be a bare constant, or the caller left a variable in.
                if (any(piece%term(1)%exponent /= 0)) then
                    ok = .false.
                    return
                end if
                coefficients(k + 1) = piece%term(1)%coefficient
            else if (size(piece%term) > 1) then
                ok = .false.
                return
            end if
        end do
        u = qpoly_from(coefficients)
    end function qmpoly_to_univariate

    !> Sylvester resultant of p and q with respect to `var`.
    !>
    !> Zero exactly when p and q share a root in that variable (or both lose
    !> degree), so it is the elimination step: the result is a polynomial in
    !> the other variables carrying every consequence of the pair.
    function qmpoly_resultant(p, q, var, ok) result(r)
        type(qmpoly_t), intent(in) :: p, q
        integer, intent(in) :: var
        logical, intent(out) :: ok
        type(qmpoly_t) :: r
        type(qmpoly_t), allocatable :: sylvester(:, :)
        integer :: m, n, size_, i, j

        m = qmpoly_degree(p, var)
        n = qmpoly_degree(q, var)
        ok = m >= 0 .and. n >= 0
        r = qmpoly_zero(p%nvars, p%radicand)
        if (.not. ok) return
        if (m == 0 .and. n == 0) return
        size_ = m + n
        allocate (sylvester(size_, size_))
        do i = 1, size_
            do j = 1, size_
                sylvester(i, j) = qmpoly_zero(p%nvars, p%radicand)
            end do
        end do
        do i = 1, n
            do j = 0, m
                sylvester(i, i + j) = qmpoly_coefficient_of(p, var, m - j, ok)
                if (.not. ok) return
            end do
        end do
        do i = 1, m
            do j = 0, n
                sylvester(n + i, i + j) = qmpoly_coefficient_of(q, var, n - j, ok)
                if (.not. ok) return
            end do
        end do
        r = determinant(sylvester, ok)
    end function qmpoly_resultant

    !> Cofactor expansion. Exact over polynomial entries, and the Sylvester
    !> matrices that arise from a reduced Runge-Kutta system are small.
    recursive function determinant(m, ok) result(value)
        type(qmpoly_t), intent(in) :: m(:, :)
        logical, intent(out) :: ok
        type(qmpoly_t) :: value
        type(qmpoly_t), allocatable :: minor(:, :)
        type(qmpoly_t) :: term
        integer :: n, j, r, cc, rr, ccc
        logical :: negate

        n = size(m, 1)
        ok = .true.
        if (n == 1) then
            value = m(1, 1)
            return
        end if
        value = qmpoly_zero(m(1, 1)%nvars, m(1, 1)%radicand)
        allocate (minor(n - 1, n - 1))
        do j = 1, n
            if (qmpoly_is_zero(m(1, j))) cycle
            rr = 0
            do r = 2, n
                rr = rr + 1
                ccc = 0
                do cc = 1, n
                    if (cc == j) cycle
                    ccc = ccc + 1
                    minor(rr, ccc) = m(r, cc)
                end do
            end do
            term = qmpoly_mul(m(1, j), determinant(minor, ok), ok)
            if (.not. ok) return
            negate = mod(j, 2) == 0
            if (negate) then
                term = qmpoly_neg(term, ok)
                if (.not. ok) return
            end if
            value = qmpoly_add(value, term, ok)
            if (.not. ok) return
        end do
    end function determinant

end module fortsym_quadratic_multipoly
