module fortsym_products
    ! Derivative products, built contracted.
    !
    ! A caller almost never wants a Jacobian. It wants J*v, or J^T*u, or a
    ! gradient, or H*v -- and forming the matrix first costs O(n*m) storage and
    ! O(n*m) expressions to reach an answer that is O(n) or O(m) long. So each
    ! product here is built by contracting as it differentiates, and the full
    ! matrix is available separately for the cases that genuinely need it.
    !
    ! The same reasoning applies to the implicit-function products. Given a
    ! residual R(y, p) = 0, the sensitivity dy/dp = -R_y^-1 R_p is defined by a
    ! linear solve, and what a solver needs is the action of R_y and R_p on a
    ! vector, not their entries. Those actions are generated directly.
    !
    ! Nothing here decides which derivative method wins, or benchmarks anything.
    ! fortsym generates candidates; selecting among them belongs to the caller.
    use fortsym_expr, only: expr_t, &
        operator(+), operator(-), operator(*), operator(/)
    use fortsym_diff, only: diff
    implicit none
    private

    public :: jacobian_matrix, jvp, vjp, gradient, hvp, directional_derivative
    public :: implicit_tangent_rhs, implicit_adjoint_rhs

contains

    !> Full Jacobian J(i,j) = d f_i / d v_j.
    !>
    !> Provided for the cases that really consume a matrix -- assembling a
    !> sparse operator, or reusing a factorization across many right-hand sides.
    !> For a single product, the contracted routines below are cheaper in both
    !> expression size and generated code.
    function jacobian_matrix(f, v) result(j)
        type(expr_t), intent(in) :: f(:), v(:)
        type(expr_t)             :: j(size(f), size(v))
        integer :: r, c

        do r = 1, size(f)
            do c = 1, size(v)
                j(r, c) = diff(f(r), v(c))
            end do
        end do
    end function jacobian_matrix

    !> Jacobian-vector product, (J*w)_i = sum_j (d f_i / d v_j) w_j.
    !>
    !> The sum is accumulated as the derivatives are taken, so the Jacobian is
    !> never materialised. This is the forward-mode product: cheap when there
    !> are few directions and many outputs.
    function jvp(f, v, w) result(r)
        type(expr_t), intent(in) :: f(:), v(:), w(:)
        type(expr_t)             :: r(size(f))
        integer :: i, k

        do i = 1, size(f)
            r(i) = diff(f(i), v(1))*w(1)
            do k = 2, size(v)
                r(i) = r(i) + diff(f(i), v(k))*w(k)
            end do
        end do
    end function jvp

    !> Vector-Jacobian product, (J^T*u)_j = sum_i u_i (d f_i / d v_j).
    !>
    !> The reverse-mode product: cheap when there are many inputs and few
    !> outputs, which is the shape of every gradient of a scalar objective.
    !> Again the matrix is never formed.
    function vjp(f, v, u) result(r)
        type(expr_t), intent(in) :: f(:), v(:), u(:)
        type(expr_t)             :: r(size(v))
        integer :: i, j

        do j = 1, size(v)
            r(j) = u(1)*diff(f(1), v(j))
            do i = 2, size(f)
                r(j) = r(j) + u(i)*diff(f(i), v(j))
            end do
        end do
    end function vjp

    !> Gradient of a scalar.
    function gradient(f, v) result(g)
        type(expr_t), intent(in) :: f, v(:)
        type(expr_t)             :: g(size(v))
        integer :: j
        do j = 1, size(v)
            g(j) = diff(f, v(j))
        end do
    end function gradient

    !> Directional derivative of a scalar: grad(f) . w.
    function directional_derivative(f, v, w) result(d)
        type(expr_t), intent(in) :: f, v(:), w(:)
        type(expr_t)             :: d
        integer :: k

        d = diff(f, v(1))*w(1)
        do k = 2, size(v)
            d = d + diff(f, v(k))*w(k)
        end do
    end function directional_derivative

    !> Hessian-vector product for a scalar f, (H*w)_j = sum_k (d2 f / dv_j dv_k) w_k.
    !>
    !> Computed as d/dv_j of the directional derivative, which is the
    !> forward-over-reverse composition: one contraction, then one
    !> differentiation. Building the Hessian and multiplying would cost n^2
    !> second derivatives to produce an answer of length n.
    function hvp(f, v, w) result(r)
        type(expr_t), intent(in) :: f, v(:), w(:)
        type(expr_t)             :: r(size(v))
        type(expr_t) :: d
        integer :: j

        d = directional_derivative(f, v, w)
        do j = 1, size(v)
            r(j) = diff(d, v(j))
        end do
    end function hvp

    !> Right-hand side of the implicit tangent system.
    !>
    !> For a residual R(y, p) = 0, differentiating the defining equation gives
    !>
    !>     R_y dy = -R_p dp
    !>
    !> so a solver needs -R_p*dp, and separately the action of R_y. This returns
    !> the right-hand side, contracted against the parameter direction and
    !> already negated.
    !>
    !> The point of differentiating the equation rather than the solver is that
    !> the iteration that found y does not appear at all: no unrolled Newton
    !> steps, no differentiated convergence test, no dependence on how many
    !> iterations it happened to take.
    function implicit_tangent_rhs(residual, p, dp) result(rhs)
        type(expr_t), intent(in) :: residual(:), p(:), dp(:)
        type(expr_t)             :: rhs(size(residual))
        type(expr_t) :: rp(size(residual))
        integer :: i

        rp = jvp(residual, p, dp)
        do i = 1, size(residual)
            rhs(i) = -rp(i)
        end do
    end function implicit_tangent_rhs

    !> Parameter-space gradient contribution of the implicit adjoint method.
    !>
    !> For a scalar objective L(y, p) subject to R(y, p) = 0, with lambda
    !> solving R_y^T lambda = L_y^T,
    !>
    !>     dL/dp = L_p - lambda^T R_p
    !>
    !> This returns that expression given lambda. One adjoint solve replaces one
    !> tangent solve per parameter, which is the whole reason to prefer the
    !> adjoint form when there are many parameters and one objective.
    function implicit_adjoint_rhs(objective, residual, p, lambda) result(g)
        type(expr_t), intent(in) :: objective
        type(expr_t), intent(in) :: residual(:), p(:), lambda(:)
        type(expr_t)             :: g(size(p))
        type(expr_t) :: lp(size(p)), lr(size(p))
        integer :: j

        lp = gradient(objective, p)
        lr = vjp(residual, p, lambda)

        do j = 1, size(p)
            g(j) = lp(j) - lr(j)
        end do
    end function implicit_adjoint_rhs

end module fortsym_products
