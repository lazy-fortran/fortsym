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
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t, sym, num, func, is_valid, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), sin, cos, tan, exp, log, sqrt, sinh, cosh, tanh
    implicit none
    private

    public :: diff, diff_n

contains

    !> Derivative of `e` with respect to the symbol `v`.
    recursive function diff(e, v) result(d)
        type(expr_t), intent(in) :: e, v
        type(expr_t)             :: d
        type(arena_t), pointer :: a
        integer :: k, n

        a => e%a

        select case (e%kind())

        case (NK_INT, NK_RAT, NK_REAL, NK_CONST)
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
        type(expr_t) :: x, dx, y, dy, denom
        type(arena_t), pointer :: a
        character(:), allocatable :: name

        a => e%a
        name = chars(e%name())
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
        case ("sqrt");  d = dx/(2*sqrt(x))
        case ("erf");   d = 2*exp(-x**2)*dx/sqrt(pi_of(a))
        case ("erfc");  d = -2*exp(-x**2)*dx/sqrt(pi_of(a))
        case ("gamma"); d = func("gamma", [x])*func("polygamma", [zero_of(a), x])*dx
        case ("loggamma"); d = func("polygamma", [zero_of(a), x])*dx
        case default
            ! An unknown head keeps its derivative symbolic rather than being
            ! guessed at. Emitting zero would be a silent, wrong answer; a
            ! named derivative is honest and an engine may still resolve it.
            d = func("Derivative_"//name, [x])*dx
        end select
    end function diff_function

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
