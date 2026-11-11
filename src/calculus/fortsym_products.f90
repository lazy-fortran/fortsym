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
    use fortsym_expr, only: expr_t, sym, num, func, is_valid, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), sin, cos, tan, exp, log, sqrt, abs, sinh, cosh, tanh
    use fortsym_diff, only: diff
    use fortsym_subs, only: subs_many
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
        integer :: i, node_count
        type(expr_t), allocatable :: cache(:)
        logical, allocatable :: known(:)
        type(arena_t), pointer :: a

        a => null()
        do i = 1, size(f)
            ! Outputs in one arena share a primal-DAG traversal. Preserve
            ! the previous behavior for outputs from distinct arenas.
            if (allocated(cache)) then
                if (.not. associated(a, f(i)%a)) then
                    deallocate(cache, known)
                end if
            end if
            if (.not. allocated(cache)) then
                a => f(i)%a
                node_count = a%size()
                allocate(cache(node_count), known(node_count))
                known = .false.
            end if
            r(i) = tangent_node(f(i), v, w, cache, known)
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
            call propagate_bar(a, i, bars(i), bars, v)
        end do
        do j = 1, size(v)
            r(j) = bars(v(j)%id)
        end do
    end function vjp

    !> Forward-mode propagation over the primal DAG. Exact inactive
    !> tangents remain zero instead of acquiring products with zero factors.
    recursive function tangent_node(e, variables, directions, cache, known) &
            result(tangent)
        type(expr_t), intent(in) :: e, variables(:), directions(:)
        type(expr_t), intent(inout) :: cache(:)
        logical, intent(inout) :: known(:)
        type(expr_t) :: tangent
        type(expr_t) :: child, base, expo, dbase, dexpo, term
        type(expr_t), allocatable :: slot_tangents(:), slot_partials(:)
        integer :: i, j, count
        integer, allocatable :: terms(:)
        character(:), allocatable :: name
        logical :: active

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
            allocate(terms(e%nargs()))
            count = 0
            do i = 1, e%nargs()
                term = tangent_node(e%arg(i), variables, directions, cache, known)
                if (is_zero_expr(term)) cycle
                count = count + 1
                terms(count) = term%id
            end do
            tangent = sum_tangents(e, terms(:count))
        case (NK_MUL)
            allocate(terms(e%nargs()))
            count = 0
            do i = 1, e%nargs()
                term = tangent_node(e%arg(i), variables, directions, cache, known)
                if (is_zero_expr(term)) cycle
                do j = 1, e%nargs()
                    if (j == i) cycle
                    child = e%arg(j)
                    if (is_zero_expr(child)) then
                        term = num(e%a, 0)
                        exit
                    end if
                    term = term*child
                end do
                if (is_zero_expr(term)) cycle
                count = count + 1
                terms(count) = term%id
            end do
            tangent = sum_tangents(e, terms(:count))
        case (NK_POW)
            base = e%arg(1)
            expo = e%arg(2)
            dbase = tangent_node( &
                base, variables, directions, cache, known)
            dexpo = tangent_node( &
                expo, variables, directions, cache, known)
            if (is_zero_expr(dexpo)) then
                tangent = num(e%a, 0)
                if (.not. is_zero_expr(dbase)) then
                    tangent = expo*base**(expo - 1)*dbase
                end if
            else if (is_zero_expr(dbase)) then
                tangent = base**expo*log(base)*dexpo
            else
                tangent = base**expo*( &
                    dexpo*log(base) + expo*dbase/base)
            end if
        case (NK_FUNC)
            if (e%nargs() == 0) then
                tangent = num(e%a, 0)
                cache(e%id) = tangent
                known(e%id) = .true.
                return
            end if
            name = chars(e%name())
            if (.not. scalar_slots(name)) then
                tangent = num(e%a, 0)
                do i = 1, size(variables)
                    term = diff(e, variables(i))
                    if (is_zero_expr(term)) cycle
                    call add_bar(tangent, term*directions(i))
                end do
                cache(e%id) = tangent
                known(e%id) = .true.
                return
            end if
            child = e%arg(1)
            dbase = tangent_node( &
                child, variables, directions, cache, known)
            if (name == "atan2") then
                expo = e%arg(2)
                dexpo = tangent_node( &
                    expo, variables, directions, cache, known)
                tangent = num(e%a, 0)
                if (.not. is_zero_expr(dbase) .or. .not. is_zero_expr(dexpo)) then
                    tangent = (dbase*expo - child*dexpo)/(child**2 + expo**2)
                end if
            else if (tabulated_unary(name) .and. e%nargs() == 1) then
                tangent = num(e%a, 0)
                if (.not. is_zero_expr(dbase)) then
                    tangent = function_tangent(name, child, dbase)
                end if
            else
                allocate(slot_tangents(e%nargs()))
                slot_tangents(1) = dbase
                active = .not. is_zero_expr(dbase)
                do i = 2, e%nargs()
                    slot_tangents(i) = tangent_node( &
                        e%arg(i), variables, directions, cache, known)
                    if (.not. is_zero_expr(slot_tangents(i))) active = .true.
                end do
                tangent = num(e%a, 0)
                if (active) then
                    slot_partials = argument_partials(e)
                    do i = 1, e%nargs()
                        if (is_zero_expr(slot_tangents(i))) cycle
                        if (is_zero_expr(slot_partials(i))) cycle
                        call add_bar(tangent, slot_partials(i)*slot_tangents(i))
                    end do
                end if
            end if
        case default
            tangent = num(e%a, 0)
        end select

        known(e%id) = .true.
        cache(e%id) = tangent
    end function tangent_node

    function sum_tangents(e, terms) result(value)
        type(expr_t), intent(in) :: e
        integer, intent(in) :: terms(:)
        type(expr_t) :: value
        value = num(e%a, 0)
        if (size(terms) == 0) return
        value = e
        ! Flatten and sort one n-ary sum, rather than every growing prefix.
        value%id = e%a%add(terms)
    end function sum_tangents

    function function_tangent(name, x, dx) result(tangent)
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: x, dx
        type(expr_t) :: tangent
        type(expr_t) :: one_arg(1), two_args(2)

        tangent = num(x%a, 0)
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
        case ("csc");   tangent = -cos(x)/sin(x)**2*dx
        case ("sec");   tangent = sin(x)/cos(x)**2*dx
        case ("cot");   tangent = -dx/sin(x)**2
        case ("csch");  tangent = -cosh(x)/sinh(x)**2*dx
        case ("sech");  tangent = -sinh(x)/cosh(x)**2*dx
        case ("coth");  tangent = -dx/sinh(x)**2
        case ("asinh"); tangent = dx/sqrt(x**2 + 1)
        case ("acosh"); tangent = dx/sqrt(x**2 - 1)
        case ("atanh"); tangent = dx/(1 - x**2)
        case ("exp");   tangent = exp(x)*dx
        case ("log");   tangent = dx/x
        case ("sqrt");  tangent = dx/(2*sqrt(x))
        ! Real branch only, x /= 0; the cusp has no derivative.
        case ("abs");   tangent = x/abs(x)*dx
        case ("erf")
            tangent = 2*exp(-x**2)*dx/sqrt(pi_of(x%a))
        case ("erfc")
            tangent = -2*exp(-x**2)*dx/sqrt(pi_of(x%a))
        case ("gamma")
            one_arg(1) = x
            two_args(1) = zero_of(x%a)
            two_args(2) = x
            tangent = func("gamma", one_arg)* &
                func("polygamma", two_args)*dx
        case ("loggamma")
            two_args(1) = zero_of(x%a)
            two_args(2) = x
            tangent = func("polygamma", two_args)*dx
        end select
    end function function_tangent

    !> Unary heads whose derivative `function_tangent` writes directly.
    pure logical function tabulated_unary(name) result(yes)
        character(*), intent(in) :: name

        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", &
                "tanh", "csc", "sec", "cot", "csch", "sech", "coth", "asinh", &
                "acosh", "atanh", "exp", "log", "sqrt", "abs", "erf", "erfc", &
                "gamma", "loggamma")
            yes = .true.
        case default
            yes = .false.
        end select
    end function tabulated_unary

    !> False for heads whose arguments are containers, conditions or
    !> operator operands instead of scalar slots of a chain rule. Their
    !> products are contracted from `diff` itself.
    pure logical function scalar_slots(name) result(yes)
        character(*), intent(in) :: name

        yes = name /= "List" .and. name /= "Piecewise" .and. name /= "If" &
            .and. name /= "partial"
    end function scalar_slots

    !> Partial derivatives of the application `e` with respect to each of its
    !> arguments. They come from `diff` applied to the head on placeholder
    !> arguments, so every head follows the rule `diff` uses for it: special
    !> functions, and opaque heads as `DerivativeN(head, indices, arguments)`.
    function argument_partials(e) result(partials)
        type(expr_t), intent(in) :: e
        type(expr_t), allocatable :: partials(:)
        type(expr_t), allocatable :: slots(:), arguments(:), placed(:)
        type(expr_t) :: applied
        character(:), allocatable :: name
        character(16) :: label
        integer :: k, n, first, order, ios

        n = e%nargs()
        name = chars(e%name())
        ! DerivativeN(head, i1, ..., iN, arguments) differentiates only its
        ! trailing arguments.
        first = 1
        if (len(name) > 10) then
            if (name(1:10) == "Derivative") then
                read (name(11:), *, iostat=ios) order
                if (ios == 0 .and. order >= 1) first = min(order + 2, n + 1)
            end if
        end if
        allocate (partials(n), arguments(n), placed(n))
        allocate (slots(n - first + 1))
        do k = 1, n
            arguments(k) = e%arg(k)
            placed(k) = arguments(k)
            partials(k) = num(e%a, 0)
            if (k < first) cycle
            write (label, '(i0)') k
            slots(k - first + 1) = sym(e%a, "fortsym_argument_slot_"//trim(label))
            placed(k) = slots(k - first + 1)
        end do
        applied = func(name, placed)
        do k = first, n
            ! Replacements are not revisited, so an argument that is itself
            ! named like a placeholder stays intact.
            partials(k) = subs_many(diff(applied, slots(k - first + 1)), &
                slots, arguments(first:))
        end do
    end function argument_partials

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

    subroutine propagate_bar(a, id, bar, bars, variables)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(expr_t), intent(in) :: bar
        type(expr_t), intent(inout) :: bars(:)
        type(expr_t), intent(in) :: variables(:)
        type(expr_t) :: e, child, other, contribution
        type(expr_t), allocatable :: slot_partials(:)
        integer :: i, j
        character(:), allocatable :: name

        e%a => bar%a
        e%id = id
        e%generation = bar%generation
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
            if (e%nargs() == 0) return
            name = chars(e%name())
            if (.not. scalar_slots(name)) then
                ! The whole application is contracted here; its arguments
                ! receive no cotangent from it.
                do i = 1, size(variables)
                    contribution = diff(e, variables(i))
                    if (is_zero_expr(contribution)) cycle
                    call add_bar(bars(variables(i)%id), bar*contribution)
                end do
                return
            end if
            child = e%arg(1)
            if (name == "atan2") then
                other = e%arg(2)
                contribution = bar*other/(child**2 + other**2)
                call add_bar(bars(child%id), contribution)
                contribution = -bar*child/(child**2 + other**2)
                call add_bar(bars(other%id), contribution)
            else if (tabulated_unary(name) .and. e%nargs() == 1) then
                contribution = bar*function_tangent( &
                    name, child, num(bar%a, 1))
                call add_bar(bars(child%id), contribution)
            else
                slot_partials = argument_partials(e)
                do i = 1, e%nargs()
                    if (is_zero_expr(slot_partials(i))) cycle
                    child = e%arg(i)
                    call add_bar(bars(child%id), bar*slot_partials(i))
                end do
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
