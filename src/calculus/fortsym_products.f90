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
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr, only: expr_t, num, func, is_valid, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), sin, cos, tan, exp, log, sqrt, sinh, cosh, tanh
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
        integer :: i

        do i = 1, size(f)
            r(i) = forward_tangent(f(i), v, w)
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
        type(expr_t), allocatable :: bars(:)
        logical, allocatable :: reachable(:)
        type(arena_t), pointer :: a
        integer :: i, j, node_count

        a => f(1)%a
        node_count = a%size()
        allocate (bars(node_count), reachable(node_count))
        reachable = .false.
        do i = 1, node_count
            bars(i) = num(a, 0)
        end do
        do i = 1, size(f)
            call mark_reachable(a, f(i)%id, reachable)
            call add_bar(bars(f(i)%id), u(i))
        end do
        do i = node_count, 1, -1
            if (.not. reachable(i)) cycle
            if (is_zero_expr(bars(i))) cycle
            call propagate_bar(a, i, bars(i), bars)
        end do
        do j = 1, size(v)
            r(j) = bars(v(j)%id)
        end do
    end function vjp

    !> Forward-mode propagation over the primal DAG. Unlike a sum of separately
    !> expanded partial derivatives, this visits every primal node once and
    !> preserves shared intermediates for code-generation CSE.
    function forward_tangent(root, variables, directions) result(tangent)
        type(expr_t), intent(in) :: root, variables(:), directions(:)
        type(expr_t) :: tangent
        type(expr_t), allocatable :: cache(:)
        logical, allocatable :: known(:)
        integer :: node_count

        node_count = root%a%size()
        allocate (cache(node_count), known(node_count))
        known = .false.
        tangent = tangent_node(root, variables, directions, cache, known)
    end function forward_tangent

    recursive function tangent_node(e, variables, directions, cache, known) &
            result(tangent)
        type(expr_t), intent(in) :: e, variables(:), directions(:)
        type(expr_t), intent(inout) :: cache(:)
        logical, intent(inout) :: known(:)
        type(expr_t) :: tangent
        type(expr_t) :: child, base, expo, dbase, dexpo, term
        integer :: i, j
        character(:), allocatable :: name

        if (known(e%id)) then
            tangent = cache(e%id)
            return
        end if

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT, NK_REAL, NK_CONST)
            tangent = num(e%a, 0)
        case (NK_SYM)
            tangent = num(e%a, 0)
            do i = 1, size(variables)
                if (e == variables(i)) then
                    tangent = directions(i)
                    exit
                end if
            end do
        case (NK_ADD)
            tangent = tangent_node( &
                e%arg(1), variables, directions, cache, known)
            do i = 2, e%nargs()
                tangent = tangent + tangent_node( &
                    e%arg(i), variables, directions, cache, known)
            end do
        case (NK_MUL)
            tangent = num(e%a, 0)
            do i = 1, e%nargs()
                term = tangent_node( &
                    e%arg(i), variables, directions, cache, known)
                do j = 1, e%nargs()
                    if (j /= i) term = term*e%arg(j)
                end do
                tangent = tangent + term
            end do
        case (NK_POW)
            base = e%arg(1)
            expo = e%arg(2)
            dbase = tangent_node( &
                base, variables, directions, cache, known)
            dexpo = tangent_node( &
                expo, variables, directions, cache, known)
            if (is_zero_expr(dexpo)) then
                tangent = expo*base**(expo - 1)*dbase
            else if (is_zero_expr(dbase)) then
                tangent = base**expo*log(base)*dexpo
            else
                tangent = base**expo*( &
                    dexpo*log(base) + expo*dbase/base)
            end if
        case (NK_FUNC)
            name = chars(e%name())
            child = e%arg(1)
            dbase = tangent_node( &
                child, variables, directions, cache, known)
            if (name == "atan2") then
                expo = e%arg(2)
                dexpo = tangent_node( &
                    expo, variables, directions, cache, known)
                tangent = (dbase*expo - child*dexpo)/(child**2 + expo**2)
            else
                tangent = function_tangent(name, child, dbase)
            end if
        case default
            tangent = num(e%a, 0)
        end select

        known(e%id) = .true.
        cache(e%id) = tangent
    end function tangent_node

    function function_tangent(name, x, dx) result(tangent)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: x, dx
        type(expr_t) :: tangent

        select case (name)
        case ("sin");   tangent = cos(x)*dx
        case ("cos");   tangent = -sin(x)*dx
        case ("tan");   tangent = dx/cos(x)**2
        case ("asin");  tangent = dx/sqrt(1 - x**2)
        case ("acos");  tangent = -dx/sqrt(1 - x**2)
        case ("atan");  tangent = dx/(1 + x**2)
        case ("sinh");  tangent = cosh(x)*dx
        case ("cosh");  tangent = sinh(x)*dx
        case ("tanh");  tangent = dx/cosh(x)**2
        case ("asinh"); tangent = dx/sqrt(x**2 + 1)
        case ("acosh"); tangent = dx/sqrt(x**2 - 1)
        case ("atanh"); tangent = dx/(1 - x**2)
        case ("exp");   tangent = exp(x)*dx
        case ("log");   tangent = dx/x
        case ("sqrt");  tangent = dx/(2*sqrt(x))
        case ("erf")
            tangent = 2*exp(-x**2)*dx/sqrt(pi_of(x%a))
        case ("erfc")
            tangent = -2*exp(-x**2)*dx/sqrt(pi_of(x%a))
        case ("gamma")
            tangent = func("gamma", [x])* &
                func("polygamma", [zero_of(x%a), x])*dx
        case ("loggamma")
            tangent = func("polygamma", [zero_of(x%a), x])*dx
        case default
            tangent = func("Derivative_"//name, [x])*dx
        end select
    end function function_tangent

    recursive subroutine mark_reachable(a, id, reachable)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(inout) :: reachable(:)
        integer :: i

        if (reachable(id)) return
        reachable(id) = .true.
        do i = 1, a%nargs_of(id)
            call mark_reachable(a, a%arg_of(id, i), reachable)
        end do
    end subroutine mark_reachable

    subroutine propagate_bar(a, id, bar, bars)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(expr_t), intent(in) :: bar
        type(expr_t), intent(inout) :: bars(:)
        type(expr_t) :: e, child, other, contribution
        integer :: i, j
        character(:), allocatable :: name

        e%a => bar%a
        e%id = id
        select case (a%kind_of(id))
        case (NK_ADD)
            do i = 1, e%nargs()
                child = e%arg(i)
                call add_bar(bars(child%id), bar)
            end do
        case (NK_MUL)
            do i = 1, e%nargs()
                contribution = bar
                do j = 1, e%nargs()
                    if (j /= i) contribution = contribution*e%arg(j)
                end do
                child = e%arg(i)
                call add_bar(bars(child%id), contribution)
            end do
        case (NK_POW)
            child = e%arg(1)
            other = e%arg(2)
            contribution = bar*other*child**(other - 1)
            call add_bar(bars(child%id), contribution)
            if (.not. is_constant_kind(other%kind())) then
                contribution = bar*child**other*log(child)
                call add_bar(bars(other%id), contribution)
            end if
        case (NK_FUNC)
            name = chars(e%name())
            child = e%arg(1)
            if (name == "atan2") then
                other = e%arg(2)
                contribution = bar*other/(child**2 + other**2)
                call add_bar(bars(child%id), contribution)
                contribution = -bar*child/(child**2 + other**2)
                call add_bar(bars(other%id), contribution)
            else
                contribution = bar*function_tangent( &
                    name, child, num(bar%a, 1))
                call add_bar(bars(child%id), contribution)
            end if
        end select
    end subroutine propagate_bar

    subroutine add_bar(target, contribution)
        type(expr_t), intent(inout) :: target
        type(expr_t), intent(in) :: contribution

        if (is_zero_expr(contribution)) return
        if (is_zero_expr(target)) then
            target = contribution
        else
            target = target + contribution
        end if
    end subroutine add_bar

    function is_zero_expr(e) result(zero)
        type(expr_t), intent(in) :: e
        logical :: zero

        zero = .false.
        if (.not. is_valid(e)) return
        if (e%kind() == NK_INT) zero = e%a%num_of(e%id) == 0_int64
    end function is_zero_expr

    pure function is_constant_kind(kind) result(constant)
        integer, intent(in) :: kind
        logical :: constant

        constant = kind == NK_INT .or. kind == NK_RAT .or. &
            kind == NK_BIG_INT .or. kind == NK_BIG_RAT .or. &
            kind == NK_REAL .or. kind == NK_CONST
    end function is_constant_kind

    function pi_of(a) result(p)
        type(arena_t), target, intent(inout) :: a
        type(expr_t) :: p

        p = num(a, 0)
        p%id = a%const("pi")
    end function pi_of

    function zero_of(a) result(zero)
        type(arena_t), target, intent(inout) :: a
        type(expr_t) :: zero

        zero = num(a, 0)
    end function zero_of

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
