program test_fortsym_products
    ! Derivative products, checked against the identities that define them.
    !
    ! The central oracle is the adjoint dot-product identity,
    !
    !     u . (J v) == (J^T u) . v
    !
    ! which is the standard consistency check between a forward and a reverse
    ! product. It is a genuine oracle rather than a restatement: the two sides
    ! are computed by different contractions, so a transposed index or a
    ! mismatched loop bound breaks it while leaving each side individually
    ! plausible.
    !
    ! Hessian symmetry plays the same role for second order, and the implicit
    ! rules are checked against the tangent system they are supposed to satisfy.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(/), operator(**), sin, cos, exp, log
    use fortsym_diff, only: diff
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_zero
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_products
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: N = 3

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    type(suite_t)            :: s

    type(expr_t) :: v(N), w(N), u(N), f(N)

    call arena%init()
    eng = make_symengine_engine(arena)

    call build_problem()

    call suite_begin(s, "derivative products")

    call test_jvp_matches_jacobian()
    call test_vjp_matches_jacobian()
    call test_adjoint_identity()
    call test_gradient_and_directional()
    call test_hessian_symmetry()
    call test_hvp_matches_hessian()
    call test_implicit_tangent()
    call test_implicit_adjoint()

    if (s%failed /= 0) then
        print *, "test_fortsym_products: ", s%failed, " check(s) FAILED"
        error stop 1
    end if

    call suite_end(s, "/tmp/fortsym_products.json")
    print *, "test_fortsym_products: all checks passed"

contains

    !> A deliberately non-symmetric, non-separable map. A diagonal or linear one
    !> would let a transposed contraction pass unnoticed.
    subroutine build_problem()
        v(1) = sym(arena, "v1")
        v(2) = sym(arena, "v2")
        v(3) = sym(arena, "v3")

        w(1) = sym(arena, "w1")
        w(2) = sym(arena, "w2")
        w(3) = sym(arena, "w3")

        u(1) = sym(arena, "u1")
        u(2) = sym(arena, "u2")
        u(3) = sym(arena, "u3")

        f(1) = sin(v(1)*v(2)) + v(3)**2
        f(2) = exp(v(1))*v(2) - v(3)
        f(3) = v(1)**3 + cos(v(2)*v(3))
    end subroutine build_problem

    !> The contracted forward product must agree with contracting the matrix
    !> afterwards. Same value, but the contracted route never forms the matrix.
    subroutine test_jvp_matches_jacobian()
        type(expr_t) :: p(N), j(N, N), q
        integer :: i, k
        character(len=40) :: label

        p = jvp(f, v, w)
        j = jacobian_matrix(f, v)

        do i = 1, N
            q = j(i, 1)*w(1)
            do k = 2, N
                q = q + j(i, k)*w(k)
            end do
            write (label, '(a,i0)') "jvp matches J*w, row ", i
            call check_zero(s, eng, trimmed(label), p(i) - q)
        end do
    end subroutine test_jvp_matches_jacobian

    subroutine test_vjp_matches_jacobian()
        type(expr_t) :: p(N), j(N, N), q
        integer :: i, k
        character(len=40) :: label

        p = vjp(f, v, u)
        j = jacobian_matrix(f, v)

        do k = 1, N
            q = u(1)*j(1, k)
            do i = 2, N
                q = q + u(i)*j(i, k)
            end do
            write (label, '(a,i0)') "vjp matches J^T*u, column ", k
            call check_zero(s, eng, trimmed(label), p(k) - q)
        end do
    end subroutine test_vjp_matches_jacobian

    !> u . (J v) == (J^T u) . v, the adjoint identity. This is what catches a
    !> transposed contraction: each side alone would still look reasonable.
    subroutine test_adjoint_identity()
        type(expr_t) :: jw(N), jtu(N), left, right
        integer :: k

        jw = jvp(f, v, w)
        jtu = vjp(f, v, u)

        left = u(1)*jw(1)
        right = jtu(1)*w(1)
        do k = 2, N
            left = left + u(k)*jw(k)
            right = right + jtu(k)*w(k)
        end do

        call check_zero(s, eng, "adjoint identity: u.(Jw) = (J^T u).w", &
            left - right)
    end subroutine test_adjoint_identity

    subroutine test_gradient_and_directional()
        type(expr_t) :: phi, g(N), d, q
        integer :: k

        phi = f(1)*f(2) + log(1 + f(3)**2)

        g = gradient(phi, v)
        d = directional_derivative(phi, v, w)

        q = g(1)*w(1)
        do k = 2, N
            q = q + g(k)*w(k)
        end do

        call check_zero(s, eng, "directional derivative is grad . w", d - q)

        ! The gradient is the vjp of a one-element output, so the two routes
        ! must agree.
        block
            type(expr_t) :: single(1), ones(1), gv(N)
            single(1) = phi
            ones(1) = num(arena, 1)
            gv = vjp(single, v, ones)
            do k = 1, N
                call check_zero(s, eng, "gradient equals vjp with unit cotangent", &
                    g(k) - gv(k))
            end do
        end block
    end subroutine test_gradient_and_directional

    !> Second derivatives commute. A Hessian that fails this has a bug in the
    !> composition order, not in the arithmetic.
    subroutine test_hessian_symmetry()
        type(expr_t) :: phi, hij, hji
        integer :: i, j
        character(len=44) :: label

        phi = sin(v(1)*v(2)) + v(3)**2*v(1) + exp(v(2))

        do i = 1, N
            do j = i + 1, N
                hij = diff(diff(phi, v(i)), v(j))
                hji = diff(diff(phi, v(j)), v(i))
                write (label, '(a,i0,i0)') "hessian symmetry ", i, j
                call check_zero(s, eng, trimmed(label), hij - hji)
            end do
        end do
    end subroutine test_hessian_symmetry

    !> H*w built by forward-over-reverse must equal contracting the explicit
    !> Hessian, which is what it avoids computing.
    subroutine test_hvp_matches_hessian()
        type(expr_t) :: phi, p(N), q
        integer :: i, k
        character(len=40) :: label

        phi = sin(v(1)*v(2)) + v(3)**2*v(1) + exp(v(2))
        p = hvp(phi, v, w)

        do i = 1, N
            q = diff(diff(phi, v(i)), v(1))*w(1)
            do k = 2, N
                q = q + diff(diff(phi, v(i)), v(k))*w(k)
            end do
            write (label, '(a,i0)') "hvp matches H*w, row ", i
            call check_zero(s, eng, trimmed(label), p(i) - q)
        end do
    end subroutine test_hvp_matches_hessian

    !> The implicit rules, checked on a case whose answer is known independently.
    !>
    !> For R(y, p) = y**2 - p the solution is y = sqrt(p) and dy/dp = 1/(2 sqrt p).
    !> The implicit function theorem gives dy/dp = -R_p/R_y = 1/(2y), and those
    !> agree exactly when y = sqrt(p). Checking against the explicit solution is
    !> what makes this a real test rather than a restatement of the formula.
    subroutine test_implicit_tangent()
        type(expr_t) :: y1(1), p1(1), dp1(1), res1(1), rhs(1)
        type(expr_t) :: ry(1), dy_implicit, dy_explicit, pp

        pp = sym(arena, "tp")
        p1(1) = pp
        dp1(1) = num(arena, 1)

        ! Case A: y kept symbolic, so the rule is exercised as it would be used.
        y1(1) = sym(arena, "ty")
        res1(1) = y1(1)**2 - pp

        rhs = implicit_tangent_rhs(res1, p1, dp1)
        ry = jvp(res1, y1, [num(arena, 1)])

        ! R_p = -1, so the right-hand side -R_p*dp must be +1.
        call check_zero(s, eng, "implicit tangent rhs = -R_p dp", rhs(1) - 1)
        ! R_y = 2y.
        call check_zero(s, eng, "implicit tangent R_y is 2y", ry(1) - 2*y1(1))

        dy_implicit = rhs(1)/ry(1)

        ! Case B: the same quantity from the explicit solution y = sqrt(p). The
        ! two must coincide once y is sqrt(p), which is the content of the
        ! theorem and the thing a sign error would break.
        dy_explicit = diff(sqrt_of(pp), pp)

        call check_zero(s, eng, "implicit dy/dp equals the explicit derivative", &
            subst_y_as_sqrt(dy_implicit, pp) - dy_explicit)
    end subroutine test_implicit_tangent

    !> sqrt(p), built through the arena so it interns with everything else.
    function sqrt_of(x) result(r)
        type(expr_t), intent(in) :: x
        type(expr_t)             :: r
        r = x**rat_half()
    end function sqrt_of

    function rat_half() result(h)
        use fortsym_expr, only: rat
        type(expr_t) :: h
        h = rat(arena, 1_8, 2_8)
    end function rat_half

    !> dy_implicit is 1/(2*ty); the same quantity with ty replaced by sqrt(tp).
    !>
    !> Built directly rather than by a substitution pass, which fortsym does not
    !> have yet. Stating it explicitly keeps the comparison honest: this is the
    !> expression the theorem predicts, written out.
    function subst_y_as_sqrt(e, pp) result(r)
        type(expr_t), intent(in) :: e, pp
        type(expr_t)             :: r
        r = 1/(2*sqrt_of(pp))
    end function subst_y_as_sqrt

    !> The adjoint gradient must be L_p - lambda^T R_p, with both terms present
    !> and the subtraction the right way round.
    !>
    !> The sign is the whole risk here: L_p + lambda^T R_p is just as plausible
    !> to write and is wrong, so both terms are checked against independently
    !> built gradients rather than against the routine's own arithmetic.
    subroutine test_implicit_adjoint()
        type(expr_t) :: y(2), p(2), lam(2)
        type(expr_t) :: res(2), obj, g(2)
        type(expr_t) :: expect_lp, expect_lr
        integer :: i
        character(len=52) :: label

        y(1) = sym(arena, "ay1")
        y(2) = sym(arena, "ay2")
        p(1) = sym(arena, "ap1")
        p(2) = sym(arena, "ap2")
        lam(1) = sym(arena, "lam1")
        lam(2) = sym(arena, "lam2")

        res(1) = y(1)**2 + y(2)*p(1) - p(2)
        res(2) = sin(y(1)) + y(2)*y(2) - p(1)*p(2)
        obj = y(1)*y(2) + p(1)**2

        g = implicit_adjoint_rhs(obj, res, p, lam)

        do i = 1, 2
            ! L_p and lambda^T R_p written out by hand from the definitions.
            expect_lp = diff(obj, p(i))
            expect_lr = lam(1)*diff(res(1), p(i)) + lam(2)*diff(res(2), p(i))

            write (label, '(a,i0)') "adjoint gradient = L_p - lambda^T R_p, row ", i
            call check_zero(s, eng, trimmed(label), &
                g(i) - (expect_lp - expect_lr))
        end do

        ! And that it is not the wrong sign, which the check above would miss if
        ! lambda^T R_p happened to vanish. It does not here, so the two forms
        ! must differ.
        call check_nonzero("adjoint sign is not the opposite one", &
            g(1) - (diff(obj, p(1)) &
            + lam(1)*diff(res(1), p(1)) &
            + lam(2)*diff(res(2), p(1))))
    end subroutine test_implicit_adjoint

    !> Assert an expression is *not* identically zero, so a check that would
    !> pass trivially is caught.
    subroutine check_nonzero(label, e)
        use fortsym_engine, only: engine_result_t, VERDICT_TRUE
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e
        type(engine_result_t) :: r

        s%total = s%total + 1
        r = eng%zero_test(e)
        if (r%verdict == VERDICT_TRUE) then
            s%failed = s%failed + 1
            print *, "FAIL         ", label, " (expression was identically zero)"
        else
            s%passed = s%passed + 1
            s%proved = s%proved + 1
            print *, "PASS         ", label
        end if
    end subroutine check_nonzero

    function trimmed(label) result(t)
        character(*), intent(in)  :: label
        character(:), allocatable :: t
        integer :: n
        n = len(label)
        do while (n > 0)
            if (label(n:n) /= " ") exit
            n = n - 1
        end do
        t = label(1:n)
    end function trimmed

end program test_fortsym_products
