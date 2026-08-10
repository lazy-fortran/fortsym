module fortsym_diff
    ! Symbolic differentiation, natively.
    !
    ! Differentiation is mechanical -- a fixed rule per node kind -- so it needs
    ! no computer algebra system. Doing it here rather than through an engine
    ! matters at scale: a three-dimensional chart produces nine metric
    ! components and twenty-seven of their derivatives, and paying a conversion
    ! (or worse, a subprocess) for each would dominate everything else.
    !
    ! Results are not simplified. d(x*y)/dx comes back as 1*y + x*0 rather than
    ! y, because simplification belongs to the engines and the council, and
    ! hash-consing means the redundant pieces cost one node each rather than a
    ! subtree. Callers that want a tidy result ask an engine for one; callers
    ! generating a kernel let CSE and the tournament handle it.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr, only: expr_t, sym, num, func, is_valid, besselj, &
        legendrep, legendreq, &
                            operator(+), operator(-), operator(*), operator(/), operator(**), &
            operator(==), sin, cos, tan, exp, log, sqrt, abs, sinh, cosh, tanh
    implicit none
    private

    public :: diff, diff_n, partial_derivative

contains

    !> Derivative of `e` with respect to the symbol `v`.
    recursive function diff(e, v) result(d)
        type(expr_t), intent(in) :: e, v
        type(expr_t)             :: d
        type(arena_t), pointer :: a
        integer :: k, n

        a => e%a

        select case (e%kind())

        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT, NK_REAL, NK_CONST)
            d = num(a, 0)

        case (NK_SYM)
            ! Every other symbol is treated as independent of v. Chain rules
            ! between symbols belong to the caller, which knows the dependency
            ! structure; guessing one here would be wrong more often than right.
            if (e == v) then
                d = num(a, 1)
            else
                d = num(a, 0)
            end if

        case (NK_ADD)
            d = diff(e%arg(1), v)
            do k = 2, e%nargs()
                d = d + diff(e%arg(k), v)
            end do

        case (NK_MUL)
            d = diff_product(e, v)

        case (NK_POW)
            d = diff_power(e, v)

        case (NK_FUNC)
            d = diff_function(e, v)

        case default
            d = num(a, 0)
        end select
    end function diff

    !> Product rule over an n-ary product: sum over each factor of its
    !> derivative times the others.
    recursive function diff_product(e, v) result(d)
        type(expr_t), intent(in) :: e, v
        type(expr_t)             :: d
        type(expr_t) :: term
        integer :: i, j, n

        n = e%nargs()
        d = num(e%a, 0)

        do i = 1, n
            term = diff(e%arg(i), v)
            do j = 1, n
                if (j /= i) term = term*e%arg(j)
            end do
            d = d + term
        end do
    end function diff_product

    !> Power rule.
    !>
    !> The general form is a**b * (b' * log(a) + b * a'/a), which is correct but
    !> introduces a logarithm. When the exponent does not depend on v -- by far
    !> the common case, and the only one that appears in a metric -- the
    !> elementary form b * a**(b-1) * a' avoids that, and avoids a log(a) that
    !> would be undefined for negative a even where the derivative is fine.
    recursive function diff_power(e, v) result(d)
        type(expr_t), intent(in) :: e, v
        type(expr_t)             :: d
        type(expr_t) :: base, expo, dbase, dexpo

        base = e%arg(1)
        expo = e%arg(2)
        dbase = diff(base, v)
        dexpo = diff(expo, v)

        if (is_zero(dexpo)) then
            d = expo*base**(expo - 1)*dbase
            return
        end if

        if (is_zero(dbase)) then
            d = base**expo*log(base)*dexpo
            return
        end if

        d = base**expo*(dexpo*log(base) + expo*dbase/base)
    end function diff_power

    !> Chain rule for a named function.
    recursive function diff_function(e, v) result(d)
        type(expr_t), intent(in) :: e, v
        type(expr_t)             :: d
        type(expr_t) :: x, dx, y, dy, denom, term
        type(expr_t) :: one_arg(1), two_args(2)
        type(expr_t), allocatable :: list_args(:)
        type(arena_t), pointer :: a
        character(:), allocatable :: name
        integer :: k, order, first_arg

        a => e%a
        name = chars(e%name())

        ! List is a container, not a mathematical function of its elements.
        ! Differentiating it componentwise preserves the Wolfram rule
        ! D[{f, g}, x] == {D[f, x], D[g, x]}; treating List as an opaque
        ! applied function instead manufactures Derivative[List, ...] nodes
        ! and prevents later Dot/Cross/Transpose evaluation.
        if (name == "List") then
            allocate (list_args(e%nargs()))
            do k = 1, e%nargs()
                list_args(k) = diff(e%arg(k), v)
            end do
            d = func("List", list_args)
            return
        end if

        if (derivative_order(name, order)) then
            first_arg = order + 2
            d = num(a, 0)
            do k = first_arg, e%nargs()
                dx = diff(e%arg(k), v)
                if (is_zero(dx)) cycle
                term = partial_derivative(e, k - first_arg + 1)
                if (.not. is_one(dx)) term = term*dx
                if (is_zero(d)) then
                    d = term
                else
                    d = d + term
                end if
            end do
            return
        end if

        ! DLMF 10.6.1 gives
        !   d J_n(x)/dx = (J_(n-1)(x) - J_(n+1)(x))/2.
        ! Keeping the order symbolic makes the rule valid beyond integer n and
        ! directly yields J_0' = -J_1 after the native simplifier is applied.
        ! https://dlmf.nist.gov/10.6.E1
        if (name == "besselj") then
            x = e%arg(2)
            dx = diff(x, v)
            d = (besselj(e%arg(1) - 1, x) - besselj(e%arg(1) + 1, x))*dx/2
            return
        end if

        ! DLMF 14.10.5, continued from the Ferrers interval to x > 1:
        ! (x^2-1) dY_nu^mu/dx =
        !     nu*x*Y_nu^mu - (nu+mu)*Y_(nu-1)^mu.
        ! Both Hobson P and Q satisfy the same relation in its regular range.
        if (name == "legendrep" .or. name == "legendreq") then
            x = e%arg(3)
            dx = diff(x, v)
            if (name == "legendrep") then
                term = legendrep(e%arg(1) - 1, e%arg(2), x)
            else
                term = legendreq(e%arg(1) - 1, e%arg(2), x)
            end if
            d = (e%arg(1)*x*e - (e%arg(1) + e%arg(2))*term)*dx/ &
                (x*x - 1)
            return
        end if

        x = e%arg(1)
        dx = diff(x, v)

        ! Two-argument atan2 first: it has two derivatives to combine, and its
        ! argument order is (y, x).
        if (name == "atan2") then
            y = e%arg(2)
            dy = diff(y, v)
            denom = x*x + y*y
            d = (dx*y - x*dy)/denom
            return
        end if

        select case (name)
        case ("sin");   d = cos(x)*dx
        case ("cos");   d = -sin(x)*dx
        case ("tan");   d = dx/cos(x)**2
        case ("asin");  d = dx/sqrt(1 - x**2)
        case ("acos");  d = -dx/sqrt(1 - x**2)
        case ("atan");  d = dx/(1 + x**2)
        case ("sinh");  d = cosh(x)*dx
        case ("cosh");  d = sinh(x)*dx
        case ("tanh");  d = dx/cosh(x)**2
        case ("asinh"); d = dx/sqrt(x**2 + 1)
        case ("acosh"); d = dx/sqrt(x**2 - 1)
        case ("atanh"); d = dx/(1 - x**2)
        case ("exp");   d = exp(x)*dx
        case ("log");   d = dx/x
        case ("abs");   d = x/abs(x)*dx
        case ("sqrt");  d = dx/(2*sqrt(x))
        case ("erf");   d = 2*exp(-x**2)*dx/sqrt(pi_of(a))
        case ("erfc");  d = -2*exp(-x**2)*dx/sqrt(pi_of(a))
        case ("gamma")
            one_arg(1) = x
            two_args(1) = zero_of(a)
            two_args(2) = x
            d = func("gamma", one_arg)*func("polygamma", two_args)*dx
        case ("loggamma")
            two_args(1) = zero_of(a)
            two_args(2) = x
            d = func("polygamma", two_args)*dx
        case default
            ! The chain rule must visit every argument. Treating only arg(1)
            ! silently made d psi(r,u)/du zero. Partial derivatives are kept as
            ! opaque applications with a canonical, sorted multi-index.
            d = num(a, 0)
            do k = 1, e%nargs()
                dx = diff(e%arg(k), v)
                if (is_zero(dx)) cycle
                term = partial_derivative(e, k)
                if (.not. is_one(dx)) term = term*dx
                if (is_zero(d)) then
                    d = term
                else
                    d = d + term
                end if
            end do
        end select
    end function diff_function

    !> Symbolic partial derivative of an applied function.
    !>
    !> The internal application is
    !>   DerivativeN(head, i1, ..., iN, arg1, ..., argM)
    !> with sorted argument indices. Sorting makes mixed partials canonical,
    !> under the standard smooth-function convention used by symbolic CASes.
    function partial_derivative(e, arg_index) result(d)
        type(expr_t), intent(in) :: e
        integer,      intent(in) :: arg_index
        type(expr_t)             :: d
        type(expr_t), allocatable :: args(:)
        type(expr_t) :: index_expr
        integer, allocatable :: indices(:)
        integer :: old_order, new_order, n_original, k, first_arg
        character(:), allocatable :: name

        name = chars(e%name())
        if (derivative_order(name, old_order)) then
            first_arg = old_order + 2
            n_original = e%nargs() - old_order - 1
            new_order = old_order + 1
            allocate (indices(new_order))
            do k = 1, old_order
                index_expr = e%arg(k + 1)
                indices(k) = int(index_expr%int_value())
            end do
            indices(new_order) = arg_index
            call sort_integers(indices)

            allocate (args(1 + new_order + n_original))
            args(1) = e%arg(1)
            do k = 1, new_order
                args(k + 1) = num(e%a, indices(k))
            end do
            do k = 1, n_original
                args(1 + new_order + k) = e%arg(first_arg + k - 1)
            end do
        else
            old_order = 0
            new_order = 1
            n_original = e%nargs()
            allocate (args(2 + n_original))
            args(1) = sym(e%a, name)
            args(2) = num(e%a, arg_index)
            do k = 1, n_original
                args(k + 2) = e%arg(k)
            end do
        end if

        d = func("Derivative"//chars(str(new_order)), args)
    end function partial_derivative

    function derivative_order(name, order) result(yes)
        character(*), intent(in)  :: name
        integer,      intent(out) :: order
        logical                   :: yes
        integer :: ios

        yes = .false.
        order = 0
        if (len(name) <= 10) return
        if (name(1:10) /= "Derivative") return
        read (name(11:), *, iostat=ios) order
        if (ios /= 0) return
        if (order < 1) return
        yes = .true.
    end function derivative_order

    pure subroutine sort_integers(values)
        integer, intent(inout) :: values(:)
        integer :: i, j, key

        do i = 2, size(values)
            key = values(i)
            j = i - 1
            do while (j >= 1)
                if (values(j) <= key) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = key
        end do
    end subroutine sort_integers

    function pi_of(a) result(p)
        type(arena_t), target, intent(inout) :: a
        type(expr_t)                         :: p
        p = sym(a, "pi")
        p%id = a%const("pi")
    end function pi_of

    function zero_of(a) result(z)
        type(arena_t), target, intent(inout) :: a
        type(expr_t)                         :: z
        z = num(a, 0)
    end function zero_of

    pure function is_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        yes = .false.
        if (.not. is_valid(e)) return
        if (e%a%kind_of(e%id) == NK_INT) yes = e%a%num_of(e%id) == 0_int64
    end function is_zero

    pure function is_one(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        yes = .false.
        if (.not. is_valid(e)) return
        if (e%a%kind_of(e%id) == NK_INT) yes = e%a%num_of(e%id) == 1_int64
    end function is_one

    !> Repeated differentiation with respect to the same symbol.
    function diff_n(e, v, order) result(d)
        type(expr_t), intent(in) :: e, v
        integer,      intent(in) :: order
        type(expr_t)             :: d
        integer :: k

        d = e
        do k = 1, order
            d = diff(d, v)
        end do
    end function diff_n

end module fortsym_diff
