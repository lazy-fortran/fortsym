module fortsym_chart
    ! Differential geometry on a user-supplied coordinate chart.
    !
    ! A chart is a map from coordinates to Cartesian position. Everything else
    ! -- basis vectors, metric, Jacobian, Christoffel symbols, and the vector
    ! calculus operators -- follows from it mechanically, which is exactly the
    ! derivation currently done by hand in notebook scripts across these repos.
    !
    ! Deliberately generic. The chart comes from the caller, so no physics, no
    ! particular geometry and no convention is baked in here; a torus, a
    ! spherical chart and a flux-coordinate map are all just three expressions.
    !
    ! Index convention: subscript for covariant, superscript for contravariant.
    ! g_ij is the covariant metric, g^ij its inverse, and the Jacobian is
    ! sqrt(det g) up to sign. Getting one of these backwards is the classic
    ! error in this area, so the tests assert the identities that catch it:
    ! g^ik g_kj = delta, det g = J**2, curl grad = 0, div curl = 0.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, &
        operator(+), operator(-), operator(*), operator(/), operator(**), sqrt
    use fortsym_diff, only: diff
    implicit none
    private

    public :: chart_t, chart_create
    public :: covariant_basis, reciprocal_basis
    public :: metric_covariant, metric_contravariant, sqrtg
    public :: jacobian, christoffel
    public :: grad, divergence, div_density, field_line_derivative, curl, &
        curl_density, laplacian
    public :: mms_source

    integer, parameter :: dp = real64

    !> The dimension everything here works in. Three is what the physical charts
    !> in these repos need, and fixing it keeps the index bookkeeping readable.
    integer, parameter, public :: DIM = 3

    type :: chart_t
        type(arena_t), pointer :: a => null()
        !> The coordinate symbols, u(1..3).
        type(expr_t) :: u(DIM)
        !> Cartesian position as a function of the coordinates, x(1..3).
        type(expr_t) :: x(DIM)
    end type chart_t

contains

    !> Build a chart from coordinate symbols and the position map.
    function chart_create(a, u, x) result(c)
        type(arena_t), target, intent(inout) :: a
        type(expr_t),          intent(in)    :: u(DIM), x(DIM)
        type(chart_t)                        :: c
        c%a => a
        c%u = u
        c%x = x
    end function chart_create

    !> Covariant basis vectors e_i = dx/du^i, as e(component, index).
    function covariant_basis(c) result(e)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: e(DIM, DIM)
        integer :: i, k
        do i = 1, DIM
            do k = 1, DIM
                e(k, i) = diff(c%x(k), c%u(i))
            end do
        end do
    end function covariant_basis

    !> Reciprocal basis e^i, defined by e^i . e_j = delta^i_j.
    !>
    !> The cross-product form is the three-dimensional inverse of the basis
    !> matrix. It keeps the orientation explicit and avoids a symbolic matrix
    !> inversion or a temporary rank-two linear-system object.
    function reciprocal_basis(c) result(e)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: e(DIM, DIM)
        type(expr_t) :: covariant(DIM, DIM), j

        covariant = covariant_basis(c)
        j = det3(covariant)

        e(1, 1) = (covariant(2, 2)*covariant(3, 3) - &
            covariant(3, 2)*covariant(2, 3))/j
        e(2, 1) = (covariant(3, 2)*covariant(1, 3) - &
            covariant(1, 2)*covariant(3, 3))/j
        e(3, 1) = (covariant(1, 2)*covariant(2, 3) - &
            covariant(2, 2)*covariant(1, 3))/j

        e(1, 2) = (covariant(2, 3)*covariant(3, 1) - &
            covariant(3, 3)*covariant(2, 1))/j
        e(2, 2) = (covariant(3, 3)*covariant(1, 1) - &
            covariant(1, 3)*covariant(3, 1))/j
        e(3, 2) = (covariant(1, 3)*covariant(2, 1) - &
            covariant(2, 3)*covariant(1, 1))/j

        e(1, 3) = (covariant(2, 1)*covariant(3, 2) - &
            covariant(3, 1)*covariant(2, 2))/j
        e(2, 3) = (covariant(3, 1)*covariant(1, 2) - &
            covariant(1, 1)*covariant(3, 2))/j
        e(3, 3) = (covariant(1, 1)*covariant(2, 2) - &
            covariant(2, 1)*covariant(1, 2))/j
    end function reciprocal_basis

    !> Covariant metric g_ij = e_i . e_j.
    function metric_covariant(c) result(g)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: g(DIM, DIM)
        type(expr_t) :: e(DIM, DIM)
        integer :: i, j, k

        e = covariant_basis(c)
        do i = 1, DIM
            do j = 1, DIM
                g(i, j) = e(1, i)*e(1, j)
                do k = 2, DIM
                    g(i, j) = g(i, j) + e(k, i)*e(k, j)
                end do
            end do
        end do
    end function metric_covariant

    !> Determinant of a 3x3 symbolic matrix, by cofactor expansion. Three
    !> dimensions is small enough that the explicit form beats any elimination
    !> scheme and, unlike elimination, never divides -- so it cannot introduce a
    !> spurious pole where a pivot happens to vanish.
    function det3(m) result(d)
        type(expr_t), intent(in) :: m(DIM, DIM)
        type(expr_t)             :: d
        d = m(1, 1)*(m(2, 2)*m(3, 3) - m(2, 3)*m(3, 2)) &
            - m(1, 2)*(m(2, 1)*m(3, 3) - m(2, 3)*m(3, 1)) &
            + m(1, 3)*(m(2, 1)*m(3, 2) - m(2, 2)*m(3, 1))
    end function det3

    !> Contravariant metric g^ij, the inverse of g_ij, via the adjugate.
    !>
    !> Adjugate over determinant rather than elimination, for the same reason as
    !> det3: no pivoting means no division by an expression that might vanish
    !> symbolically, and the result is one clean quotient per entry.
    function metric_contravariant(c) result(ginv)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: ginv(DIM, DIM)
        type(expr_t) :: g(DIM, DIM), d
        integer :: i, j

        g = metric_covariant(c)
        d = det3(g)

        ! Adjugate is the transpose of the cofactor matrix; the metric is
        ! symmetric, so the transpose costs nothing but is kept explicit.
        do i = 1, DIM
            do j = 1, DIM
                ginv(i, j) = cofactor(g, j, i)/d
            end do
        end do
    end function metric_contravariant

    !> Positive metric volume factor sqrt(det(g_ij)).
    !>
    !> The signed chart Jacobian remains available through jacobian(c). For an
    !> orientation-preserving Euclidean chart the two agree. Keeping both
    !> operations separate prevents orientation from disappearing accidentally
    !> in magnetic-density and differential-form calculations.
    function sqrtg(c) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: value
        type(expr_t)              :: g(DIM, DIM)

        g = metric_covariant(c)
        value = sqrt(det3(g))
    end function sqrtg

    !> Signed cofactor C_ij of a 3x3 matrix.
    function cofactor(m, i, j) result(cf)
        type(expr_t), intent(in) :: m(DIM, DIM)
        integer,      intent(in) :: i, j
        type(expr_t)             :: cf
        integer :: r1, r2, c1, c2

        call others(i, r1, r2)
        call others(j, c1, c2)

        cf = m(r1, c1)*m(r2, c2) - m(r1, c2)*m(r2, c1)
        if (mod(i + j, 2) == 1) cf = -cf
    end function cofactor

    pure subroutine others(i, a, b)
        integer, intent(in)  :: i
        integer, intent(out) :: a, b
        select case (i)
        case (1); a = 2; b = 3
        case (2); a = 1; b = 3
        case default; a = 1; b = 2
        end select
    end subroutine others

    !> Jacobian determinant of the chart: det of the basis matrix e(k,i).
    !>
    !> Taken from the basis rather than as sqrt(det g), because the basis form
    !> keeps the sign. det g is the square, so its square root discards the
    !> orientation -- and orientation is precisely what the sign-convention
    !> derivations in these repos are about.
    function jacobian(c) result(j)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: j
        type(expr_t) :: e(DIM, DIM)
        e = covariant_basis(c)
        j = det3(e)
    end function jacobian

    !> Christoffel symbols of the second kind, Gamma^k_ij, as gamma(k,i,j).
    !>
    !>   Gamma^k_ij = 1/2 g^kl ( d_i g_lj + d_j g_li - d_l g_ij )
    function christoffel(c) result(gamma)
        type(chart_t), intent(in) :: c
        type(expr_t)              :: gamma(DIM, DIM, DIM)
        type(expr_t) :: g(DIM, DIM), ginv(DIM, DIM), term
        integer :: i, j, k, l

        g = metric_covariant(c)
        ginv = metric_contravariant(c)

        do k = 1, DIM
            do i = 1, DIM
                do j = 1, DIM
                    gamma(k, i, j) = num(c%a, 0)
                    do l = 1, DIM
                        term = diff(g(l, j), c%u(i)) &
                            + diff(g(l, i), c%u(j)) &
                            - diff(g(i, j), c%u(l))
                        gamma(k, i, j) = gamma(k, i, j) + ginv(k, l)*term
                    end do
                    gamma(k, i, j) = gamma(k, i, j)/2
                end do
            end do
        end do
    end function christoffel

    ! ------------------------------------------------- vector calculus --

    !> Gradient of a scalar, contravariant components: (grad f)^i = g^ij d_j f.
    function grad(c, f) result(v)
        type(chart_t), intent(in) :: c
        type(expr_t),  intent(in) :: f
        type(expr_t)              :: v(DIM)
        type(expr_t) :: ginv(DIM, DIM)
        integer :: i, j

        ginv = metric_contravariant(c)
        do i = 1, DIM
            v(i) = ginv(i, 1)*diff(f, c%u(1))
            do j = 2, DIM
                v(i) = v(i) + ginv(i, j)*diff(f, c%u(j))
            end do
        end do
    end function grad

    !> Directional derivative along a contravariant coordinate vector.
    !>
    !> For a magnetic field this is the field-line derivative
    !> B^i partial_i f. It is a chart operation rather than a plasma-specific
    !> one, so the same owner serves characteristics, flow lines, and magnetic
    !> surfaces without introducing a second vector-calculus implementation.
    function field_line_derivative(c, vector, f) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: vector(DIM), f
        type(expr_t) :: value

        value = vector(1)*diff(f, c%u(1)) + vector(2)*diff(f, c%u(2)) + &
            vector(3)*diff(f, c%u(3))
    end function field_line_derivative

    !> Divergence of a contravariant vector field:
    !>
    !>   div v = (1/J) d_i ( J v^i )
    !>
    !> The Jacobian weight is what makes this a divergence rather than a plain
    !> sum of derivatives, and omitting it is the standard way curvilinear
    !> vector calculus goes wrong.
    function divergence(c, v) result(d)
        type(chart_t), intent(in) :: c
        type(expr_t),  intent(in) :: v(DIM)
        type(expr_t)              :: d
        type(expr_t) :: j
        integer :: i

        j = jacobian(c)
        d = diff(j*v(1), c%u(1))
        do i = 2, DIM
            d = d + diff(j*v(i), c%u(i))
        end do
        d = d/j
    end function divergence

    !> Divergence of a contravariant vector density of weight +1:
    !>
    !>   div_density D = d_i D^i
    !>
    !> This is the density-valued operator used by the differential-form and
    !> finite-element formulations. It deliberately has no metric or Jacobian
    !> factor; callers that hold an ordinary vector use divergence instead.
    function div_density(c, v) result(d)
        type(chart_t), intent(in) :: c
        type(expr_t),  intent(in) :: v(DIM)
        type(expr_t)              :: d

        d = diff(v(1), c%u(1))
        d = d + diff(v(2), c%u(2))
        d = d + diff(v(3), c%u(3))
    end function div_density

    !> Curl of a covariant vector field, returning contravariant components:
    !>
    !>   (curl w)^i = (1/J) eps^ijk d_j w_k
    !>
    !> The input is covariant on purpose: the curl of a gradient vanishing is an
    !> identity about covariant components, and taking contravariant input here
    !> would silently require a metric raise that the caller did not ask for.
    function curl(c, w) result(r)
        type(chart_t), intent(in) :: c
        type(expr_t),  intent(in) :: w(DIM)
        type(expr_t)              :: r(DIM)
        type(expr_t) :: j

        j = jacobian(c)
        r(1) = (diff(w(3), c%u(2)) - diff(w(2), c%u(3)))/j
        r(2) = (diff(w(1), c%u(3)) - diff(w(3), c%u(1)))/j
        r(3) = (diff(w(2), c%u(1)) - diff(w(1), c%u(2)))/j
    end function curl

    !> Curl of a covariant vector field as a contravariant vector density:
    !>
    !>   (curl_density w)^1 = d_2 w_3 - d_3 w_2
    !>   (curl_density w)^2 = d_3 w_1 - d_1 w_3
    !>   (curl_density w)^3 = d_1 w_2 - d_2 w_1
    !>
    !> This is the metric-free density form of curl. For a regular chart it is
    !> exactly jacobian(c)*curl(c,w), with the chart orientation retained.
    function curl_density(c, w) result(r)
        type(chart_t), intent(in) :: c
        type(expr_t),  intent(in) :: w(DIM)
        type(expr_t)              :: r(DIM)

        r(1) = diff(w(3), c%u(2)) - diff(w(2), c%u(3))
        r(2) = diff(w(1), c%u(3)) - diff(w(3), c%u(1))
        r(3) = diff(w(2), c%u(1)) - diff(w(1), c%u(2))
    end function curl_density

    !> Laplace-Beltrami operator: div grad.
    function laplacian(c, f) result(l)
        type(chart_t), intent(in) :: c
        type(expr_t),  intent(in) :: f
        type(expr_t)              :: l
        type(expr_t) :: gradient_value(DIM)
        gradient_value = grad(c, f)
        l = divergence(c, gradient_value)
    end function laplacian

    !> Method of manufactured solutions: the source term that makes a chosen
    !> function an exact solution of a chosen operator.
    !>
    !> Given a residual operator already applied to the manufactured solution,
    !> the source is simply that residual -- subtracting it from the equation
    !> makes the manufactured function satisfy it exactly. Trivial arithmetic,
    !> but naming it puts the genex workflow in the library rather than leaving
    !> each caller to rediscover the sign.
    function mms_source(applied_operator) result(s)
        type(expr_t), intent(in) :: applied_operator
        type(expr_t)             :: s
        s = applied_operator
    end function mms_source

end module fortsym_chart
