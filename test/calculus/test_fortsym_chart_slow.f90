program test_fortsym_chart_slow
    ! Differential geometry, checked against the identities that hold for any
    ! chart.
    !
    ! The oracle is mathematics, not a stored answer: g^ik g_kj must be the
    ! identity, det g must equal J squared, curl grad must vanish and div curl
    ! must vanish. These are exactly the properties that catch a raised index
    ! left lowered, a missing Jacobian weight, or a transposed inverse -- the
    ! standard errors in curvilinear vector calculus, and the ones the
    ! sign-convention derivations in these repos exist to pin down.
    !
    ! Checks run on a torus chart rather than something trivial, because a
    ! diagonal metric would let a transposed inverse pass unnoticed.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**), sin, cos
    use fortsym_diff, only: diff
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_zero
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_chart
    implicit none

    integer, parameter :: dp = real64

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    type(suite_t)            :: s
    type(chart_t)            :: torus, cartesian

    call arena%init()
    eng = make_symengine_engine(arena)

    torus = make_torus()
    cartesian = make_cartesian()

    call suite_begin(s, "chart identities")

    call test_differentiation()
    call test_metric_inverse()
    call test_determinant_relation()
    call test_christoffel_symmetry()
    call test_curl_of_grad_vanishes()
    call test_div_of_curl_vanishes()
    call test_laplacian_on_cartesian()

    ! suite_end reports and then stops on failure, but the failing exit lives
    ! inside it where a reader -- and fo lint -- cannot see it. Stating it here
    ! as well makes this program's own failure path explicit.
    if (s%failed /= 0) then
        print *, "test_fortsym_chart_slow: ", s%failed, " check(s) FAILED"
        error stop 1
    end if

    call suite_end(s, "/tmp/fortsym_chart.json")
    print *, "test_fortsym_chart_slow: all checks passed"

contains

    !> A torus chart: R = R0 + r cos(th), and the position in Cartesian space.
    !> Off-diagonal metric terms are what make this a real test.
    function make_torus() result(c)
        type(chart_t) :: c
        type(expr_t)  :: u(DIM), x(DIM), rr, big_r, r0

        u(1) = sym(arena, "r")
        u(2) = sym(arena, "th")
        u(3) = sym(arena, "ph")
        r0 = sym(arena, "R0")

        big_r = r0 + u(1)*cos(u(2))
        x(1) = big_r*cos(u(3))
        x(2) = big_r*sin(u(3))
        x(3) = u(1)*sin(u(2))

        c = chart_create(arena, u, x)
    end function make_torus

    !> The identity chart. Anything that fails here is broken outright.
    function make_cartesian() result(c)
        type(chart_t) :: c
        type(expr_t)  :: u(DIM), x(DIM)

        u(1) = sym(arena, "cx")
        u(2) = sym(arena, "cy")
        u(3) = sym(arena, "cz")
        x = u

        c = chart_create(arena, u, x)
    end function make_cartesian

    subroutine test_differentiation()
        type(expr_t) :: x, y

        x = sym(arena, "dx")
        y = sym(arena, "dy")

        call check_zero(s, eng, "d/dx of x is 1", diff(x, x) - 1)
        call check_zero(s, eng, "d/dx of y is 0", diff(y, x))
        call check_zero(s, eng, "product rule", diff(x*y, x) - y)
        call check_zero(s, eng, "power rule", diff(x**3, x) - 3*x**2)
        call check_zero(s, eng, "chain rule", &
            diff(sin(x*y), x) - y*cos(x*y))
        call check_zero(s, eng, "quotient", &
            diff(x/y, x) - 1/y)
        ! A second derivative must compose correctly.
        call check_zero(s, eng, "second derivative of sin", &
            diff(diff(sin(x), x), x) + sin(x))
    end subroutine test_differentiation

    !> g^ik g_kj = delta^i_j. This is what catches a transposed or mis-scaled
    !> inverse, and it needs an off-diagonal metric to be meaningful.
    subroutine test_metric_inverse()
        type(expr_t) :: g(DIM, DIM), ginv(DIM, DIM), prod
        integer :: i, j, k
        character(len=48) :: label

        g = metric_covariant(torus)
        ginv = metric_contravariant(torus)

        do i = 1, DIM
            do j = 1, DIM
                prod = ginv(i, 1)*g(1, j)
                do k = 2, DIM
                    prod = prod + ginv(i, k)*g(k, j)
                end do
                if (i == j) prod = prod - 1

                write (label, '(a,i0,i0)') "metric inverse: g^ik g_kj at ", i, j
                call check_zero(s, eng, trim_label(label), prod)
            end do
        end do
    end subroutine test_metric_inverse

    !> det g = J**2. Relates the metric to the basis determinant, so a chart
    !> whose Jacobian was computed inconsistently shows up here.
    subroutine test_determinant_relation()
        type(expr_t) :: g(DIM, DIM), j, detg
        type(expr_t) :: e(DIM, DIM)
        integer :: i, k

        g = metric_covariant(torus)
        j = jacobian(torus)

        detg = g(1, 1)*(g(2, 2)*g(3, 3) - g(2, 3)*g(3, 2)) &
            - g(1, 2)*(g(2, 1)*g(3, 3) - g(2, 3)*g(3, 1)) &
            + g(1, 3)*(g(2, 1)*g(3, 2) - g(2, 2)*g(3, 1))

        call check_zero(s, eng, "det g = J**2", detg - j**2)
    end subroutine test_determinant_relation

    !> Christoffel symbols of the second kind are symmetric in their lower
    !> indices for any torsion-free connection.
    subroutine test_christoffel_symmetry()
        type(expr_t) :: gamma(DIM, DIM, DIM)
        integer :: k, i, j
        character(len=48) :: label

        gamma = christoffel(torus)

        do k = 1, DIM
            do i = 1, DIM
                do j = i + 1, DIM
                    write (label, '(a,i0,i0,i0)') "christoffel symmetry ", k, i, j
                    call check_zero(s, eng, trim_label(label), &
                        gamma(k, i, j) - gamma(k, j, i))
                end do
            end do
        end do
    end subroutine test_christoffel_symmetry

    !> curl grad f = 0, for any f and any chart. Catches a missing Jacobian
    !> weight or a swapped index in curl.
    subroutine test_curl_of_grad_vanishes()
        type(expr_t) :: f, gradf(DIM), c(DIM), lowered(DIM)
        type(expr_t) :: g(DIM, DIM)
        integer :: i, j
        character(len=48) :: label

        f = sym(arena, "r")**2*cos(sym(arena, "th")) + sym(arena, "ph")

        ! curl takes covariant components, and grad returns contravariant ones,
        ! so the index has to be lowered with the metric first. Skipping that is
        ! the mistake this test exists to catch.
        gradf = grad(torus, f)
        g = metric_covariant(torus)
        do i = 1, DIM
            lowered(i) = g(i, 1)*gradf(1)
            do j = 2, DIM
                lowered(i) = lowered(i) + g(i, j)*gradf(j)
            end do
        end do

        c = curl(torus, lowered)
        do i = 1, DIM
            write (label, '(a,i0)') "curl grad vanishes, component ", i
            call check_zero(s, eng, trim_label(label), c(i))
        end do
    end subroutine test_curl_of_grad_vanishes

    !> div curl w = 0, for any w and any chart.
    subroutine test_div_of_curl_vanishes()
        type(expr_t) :: w(DIM), c(DIM), d

        w(1) = sym(arena, "r")*cos(sym(arena, "th"))
        w(2) = sym(arena, "r")**2*sym(arena, "ph")
        w(3) = sin(sym(arena, "th"))

        c = curl(torus, w)
        d = divergence(torus, c)

        call check_zero(s, eng, "div curl vanishes", d)
    end subroutine test_div_of_curl_vanishes

    !> On the identity chart the Laplace-Beltrami operator must reduce to the
    !> ordinary sum of second derivatives. If the Jacobian weighting or the
    !> metric raise were wrong, this is where it shows plainly.
    subroutine test_laplacian_on_cartesian()
        type(expr_t) :: f, l, plain
        type(expr_t) :: cx, cy, cz

        cx = sym(arena, "cx")
        cy = sym(arena, "cy")
        cz = sym(arena, "cz")

        f = cx**3 + cx*cy**2 + cos(cz)

        l = laplacian(cartesian, f)
        plain = diff(diff(f, cx), cx) + diff(diff(f, cy), cy) &
            + diff(diff(f, cz), cz)

        call check_zero(s, eng, "laplacian reduces on the identity chart", &
            l - plain)
    end subroutine test_laplacian_on_cartesian

    function trim_label(label) result(t)
        character(*), intent(in)  :: label
        character(:), allocatable :: t
        integer :: n
        n = len(label)
        do while (n > 0)
            if (label(n:n) /= " ") exit
            n = n - 1
        end do
        t = label(1:n)
    end function trim_label

end program test_fortsym_chart_slow
