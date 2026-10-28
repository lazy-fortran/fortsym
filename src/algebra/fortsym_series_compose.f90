module fortsym_series_compose
    ! Taylor series by composition of truncated power series.
    !
    ! The derivative route (k-th derivative at the point, divided by k!) fails
    ! whenever the expression is written with a removable singularity, such as
    ! (u - asinh(u))/u**3 at u = 0: every derivative is a 0/0 form at the
    ! point. Here each node is expanded in t = v - point from its children
    ! instead. Sums and products are exact; reciprocals, non-integer powers
    ! and function applications are expanded around a leading term that the
    ! engine has proved nonzero. Negative powers of t that cancel are then
    ! decided by the engine's zero test, never assumed.
    !
    ! Every series carries an absolute precision: all exponents below it are
    ! exact. A result is returned only when that precision covers the
    ! requested order and no negative power survives. Otherwise the caller
    ! receives a refusal, with the precision raised a bounded number of
    ! times first to absorb cancellation.
    use fortsym_string, only: chars
    use fortsym_arena, only: NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, &
        NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, &
        NK_ALGEBRAIC
    use fortsym_expr, only: expr_t, sym, num, func, operator(+), &
        operator(-), operator(*), operator(/), operator(**)
    use fortsym_subs, only: subs
    use fortsym_engine, only: engine_t, engine_result_t, VERDICT_TRUE, &
        VERDICT_FALSE
    use fortsym_series_guard, only: contains_nonfinite, contains_singular_power, &
        contains_opaque_derivative
    implicit none
    private

    public :: compose_series

    !> Precision of an exact (finite) expansion.
    integer, parameter :: EXACT_AP = 1000000
    integer, parameter :: MAX_EXTRA = 64

    !> sum_k c(k) t**(val + k - 1); exponents below ap are exact.
    type :: tps_t
        integer :: val = EXACT_AP
        integer :: ap = EXACT_AP
        type(expr_t), allocatable :: c(:)
    end type tps_t

    !> Shared state of one expansion.
    type :: ctx_t
        class(engine_t), pointer :: engine => null()
        type(expr_t) :: v, point, x
        integer :: rel = 0
    end type ctx_t

contains

    !> Taylor polynomial of e about v = point through (v - point)**order.
    !> The optional `coefficients(k+1)` holds the coefficient of
    !> (v - point)**k for k = 0..order.
    subroutine compose_series(engine, e, v, point, order, value, ok, why, &
            coefficients)
        class(engine_t), target, intent(inout) :: engine
        type(expr_t), intent(in) :: e, v, point
        integer, intent(in) :: order
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), allocatable, intent(out), optional :: coefficients(:)
        type(ctx_t) :: ctx
        type(tps_t) :: s
        type(engine_result_t) :: res
        integer :: extra, k, exponent

        value = e
        ok = .false.
        why = ""
        if (v%kind() /= NK_SYM) then
            why = "native: series variable must be a symbol"
            return
        end if
        ctx%engine => engine
        ctx%v = v
        ctx%point = point
        ctx%x = sym(e%a, "fortsym_series_arg")
        extra = 4
        do
            ctx%rel = order + extra
            call expand_node(ctx, e, s, ok, why)
            if (.not. ok) return
            if (s%ap > order .and. (size(s%c) == 0 .or. s%val >= 0)) exit
            if (size(s%c) > 0 .and. s%val < 0 .and. s%ap > s%val) then
                ok = .false.
                why = "native: series has a pole at the expansion point, "// &
                    "or a cancellation the zero test cannot decide"
                return
            end if
            if (extra >= MAX_EXTRA) then
                ok = .false.
                why = "native: series cancellation exceeds the bounded "// &
                    "working precision"
                return
            end if
            extra = 2*extra
        end do

        if (present(coefficients)) then
            allocate (coefficients(order + 1))
            do k = 0, order
                coefficients(k + 1) = coefficient(ctx, s, k)
            end do
        end if
        value = point - point
        do k = 1, size(s%c)
            exponent = s%val + k - 1
            if (exponent > order) exit
            value = value + s%c(k)*(v - point)**exponent
        end do
        res = engine%simplify(value)
        if (.not. res%ok) then
            ok = .false.
            why = chars(res%message)
            return
        end if
        value = res%value
        ok = .true.
    end subroutine compose_series

    recursive subroutine expand_node(ctx, e, s, ok, why)
        type(ctx_t), intent(inout) :: ctx
        type(expr_t), intent(in) :: e
        type(tps_t), intent(out) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(tps_t) :: child, acc
        integer :: k

        ok = .true.
        why = ""
        if (.not. depends_on(e, ctx%v)) then
            if (e%kind() == NK_CONST .or. contains_nonfinite(e%a, e%id)) then
                ok = .false.
                why = "native: series of a non-finite constant"
                return
            end if
            call constant(ctx, e, s)
            return
        end if

        select case (e%kind())
        case (NK_SYM)
            ! The variable itself: point + t.
            call constant(ctx, ctx%point, s)
            s = add(ctx, s, monomial(ctx, 1), ok, why)
        case (NK_ADD)
            call constant(ctx, num(e%a, 0), acc)
            do k = 1, e%nargs()
                call expand_node(ctx, e%arg(k), child, ok, why)
                if (.not. ok) return
                acc = add(ctx, acc, child, ok, why)
                if (.not. ok) return
            end do
            s = acc
        case (NK_MUL)
            call expand_node(ctx, e%arg(1), acc, ok, why)
            if (.not. ok) return
            do k = 2, e%nargs()
                call expand_node(ctx, e%arg(k), child, ok, why)
                if (.not. ok) return
                acc = mul(ctx, acc, child, ok, why)
                if (.not. ok) return
            end do
            s = acc
        case (NK_POW)
            call expand_power(ctx, e, s, ok, why)
        case (NK_FUNC)
            call expand_function(ctx, e, s, ok, why)
        case default
            ok = .false.
            why = "native: series of an unsupported node"
        end select
    end subroutine expand_node

    recursive subroutine expand_power(ctx, e, s, ok, why)
        type(ctx_t), intent(inout) :: ctx
        type(expr_t), intent(in) :: e
        type(tps_t), intent(out) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: base, exponent
        type(tps_t) :: b

        base = e%arg(1)
        exponent = e%arg(2)
        if (depends_on(exponent, ctx%v)) then
            ! b**p = exp(p*log(b)) when the exponent varies.
            call expand_node(ctx, func("exp", [exponent*func("log", [base])]), &
                s, ok, why)
            return
        end if
        call expand_node(ctx, base, b, ok, why)
        if (.not. ok) return
        call power(ctx, b, exponent, s, ok, why)
    end subroutine expand_power

    !> f(..., s(t), ...) = sum_k f^(k)(s0)/k! (s - s0)**k for one varying
    !> argument. Derivatives come from the engine, so any differentiable
    !> head is covered; a non-finite derivative at s0 refuses.
    recursive subroutine expand_function(ctx, e, s, ok, why)
        type(ctx_t), intent(inout) :: ctx
        type(expr_t), intent(in) :: e
        type(tps_t), intent(out) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), allocatable :: fargs(:)
        type(expr_t) :: f, derivative, s0, value
        type(tps_t) :: inner, w, wk, term
        type(engine_result_t) :: res
        integer :: k, slot, nterms

        slot = 0
        allocate (fargs(e%nargs()))
        do k = 1, e%nargs()
            fargs(k) = e%arg(k)
            if (depends_on(fargs(k), ctx%v)) then
                if (slot /= 0) then
                    ok = .false.
                    why = "native: series of a function with several "// &
                        "varying arguments"
                    return
                end if
                slot = k
            end if
        end do
        call expand_node(ctx, fargs(slot), inner, ok, why)
        if (.not. ok) return
        if (size(inner%c) > 0 .and. inner%val < 0) then
            ok = .false.
            why = "native: series of a function at an argument pole"
            return
        end if
        s0 = coefficient(ctx, inner, 0)
        call constant(ctx, s0, term)
        w = sub(ctx, inner, term, ok, why)
        if (.not. ok) return

        fargs(slot) = ctx%x
        f = func(chars(e%name()), fargs)
        derivative = f
        nterms = min(ctx%rel, w%ap - 1)
        call constant(ctx, num(e%a, 0), s)
        call constant(ctx, num(e%a, 1), wk)
        do k = 0, nterms
            if (k > 0) then
                res = ctx%engine%diff(derivative, ctx%x)
                if (.not. res%ok) then
                    ok = .false.
                    why = chars(res%message)
                    return
                end if
                derivative = res%value/num(e%a, k)
                wk = mul(ctx, wk, w, ok, why)
                if (.not. ok) return
            end if
            if (size(wk%c) == 0 .and. wk%ap >= EXACT_AP) exit
            res = ctx%engine%simplify(subs(derivative, ctx%x, s0))
            if (.not. res%ok) then
                ok = .false.
                why = chars(res%message)
                return
            end if
            value = res%value
            if (contains_nonfinite(e%a, value%id) .or. &
                contains_singular_power(e%a, value%id) .or. &
                contains_opaque_derivative(e%a, value%id)) then
                ok = .false.
                why = "native: series of a function at a singular argument"
                return
            end if
            call scale(wk, value, term)
            s = add(ctx, s, term, ok, why)
            if (.not. ok) return
        end do
        ! Terms beyond nterms are unknown; w has valuation >= 1.
        s%ap = min(s%ap, nterms + 1, w%ap)
        call trim(ctx, s, ok, why)
    end subroutine expand_function

    !> b**p for an expansion b and a constant exponent p.
    recursive subroutine power(ctx, b, p, s, ok, why)
        type(ctx_t), intent(inout) :: ctx
        type(tps_t), intent(in) :: b
        type(expr_t), intent(in) :: p
        type(tps_t), intent(out) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(tps_t) :: base, w, wk, term, lead
        type(expr_t) :: c0, binom, lead_power
        type(engine_result_t) :: res
        integer :: n, k, nterms, shift
        logical :: integral

        ok = .true.
        why = ""
        call integer_value(p, n, integral)
        if (integral .and. n >= 0) then
            call constant(ctx, num(p%a, 1), s)
            base = b
            do while (n > 0)
                if (mod(n, 2) == 1) then
                    s = mul(ctx, s, base, ok, why)
                    if (.not. ok) return
                end if
                n = n/2
                if (n > 0) then
                    base = mul(ctx, base, base, ok, why)
                    if (.not. ok) return
                end if
            end do
            return
        end if

        if (size(b%c) == 0) then
            ok = .false.
            why = "native: series of a negative or fractional power of a "// &
                "vanishing expansion"
            return
        end if
        c0 = b%c(1)
        if (.not. nonzero(ctx, c0)) then
            ok = .false.
            why = "native: series leading coefficient is not provably nonzero"
            return
        end if
        ! b = c0 t**val (1 + w), with w of valuation >= 1.
        call constant(ctx, num(p%a, 1)/c0, lead)
        lead%val = -b%val
        lead%ap = EXACT_AP
        w = mul(ctx, b, lead, ok, why)
        if (.not. ok) return
        call constant(ctx, num(p%a, -1), term)
        w = add(ctx, w, term, ok, why)
        if (.not. ok) return

        res = ctx%engine%simplify(p*num(p%a, b%val))
        if (.not. res%ok) then
            ok = .false.
            why = chars(res%message)
            return
        end if
        call integer_value(res%value, shift, integral)
        if (.not. integral) then
            ok = .false.
            why = "native: series needs fractional powers of the variable"
            return
        end if

        nterms = min(ctx%rel, w%ap - 1)
        call constant(ctx, num(p%a, 0), s)
        call constant(ctx, num(p%a, 1), wk)
        binom = num(p%a, 1)
        do k = 0, nterms
            if (k > 0) then
                binom = binom*(p - num(p%a, k - 1))/num(p%a, k)
                wk = mul(ctx, wk, w, ok, why)
                if (.not. ok) return
            end if
            if (size(wk%c) == 0 .and. wk%ap >= EXACT_AP) exit
            call scale(wk, binom, term)
            s = add(ctx, s, term, ok, why)
            if (.not. ok) return
        end do
        s%ap = min(s%ap, nterms + 1, w%ap)
        lead_power = c0**p
        call constant(ctx, lead_power, lead)
        lead%val = shift
        s = mul(ctx, s, lead, ok, why)
    end subroutine power

    ! ------------------------------------------------------ arithmetic --

    function add(ctx, x, y, ok, why) result(s)
        type(ctx_t), intent(inout) :: ctx
        type(tps_t), intent(in) :: x, y
        type(tps_t) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: lo, hi, k, ap

        ap = min(x%ap, y%ap)
        if (size(x%c) == 0 .and. size(y%c) == 0) then
            s%val = ap
            s%ap = ap
            allocate (s%c(0))
            ok = .true.
            why = ""
            return
        end if
        lo = huge(1)
        hi = -huge(1)
        if (size(x%c) > 0) then
            lo = min(lo, x%val)
            hi = max(hi, x%val + size(x%c) - 1)
        end if
        if (size(y%c) > 0) then
            lo = min(lo, y%val)
            hi = max(hi, y%val + size(y%c) - 1)
        end if
        hi = min(hi, ap - 1)
        s%val = lo
        s%ap = ap
        allocate (s%c(max(0, hi - lo + 1)))
        do k = 1, size(s%c)
            s%c(k) = coefficient(ctx, x, lo + k - 1) + &
                coefficient(ctx, y, lo + k - 1)
        end do
        call trim(ctx, s, ok, why)
    end function add

    function sub(ctx, x, y, ok, why) result(s)
        type(ctx_t), intent(inout) :: ctx
        type(tps_t), intent(in) :: x, y
        type(tps_t) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(tps_t) :: negated

        call scale(y, num(ctx%v%a, -1), negated)
        s = add(ctx, x, negated, ok, why)
    end function sub

    function mul(ctx, x, y, ok, why) result(s)
        type(ctx_t), intent(inout) :: ctx
        type(tps_t), intent(in) :: x, y
        type(tps_t) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: ap, i, j, hi, n

        ap = min(capped(x%val + y%ap), capped(y%val + x%ap))
        if (size(x%c) == 0 .or. size(y%c) == 0) then
            s%val = ap
            s%ap = ap
            allocate (s%c(0))
            ok = .true.
            why = ""
            return
        end if
        s%val = x%val + y%val
        s%ap = ap
        hi = min(x%val + size(x%c) - 1 + y%val + size(y%c) - 1, ap - 1)
        n = max(0, hi - s%val + 1)
        allocate (s%c(n))
        do i = 1, n
            s%c(i) = num(ctx%v%a, 0)
        end do
        do i = 1, size(x%c)
            do j = 1, size(y%c)
                if (i + j - 1 > n) exit
                s%c(i + j - 1) = s%c(i + j - 1) + x%c(i)*y%c(j)
            end do
        end do
        call trim(ctx, s, ok, why)
    end function mul

    subroutine scale(x, factor, s)
        type(tps_t), intent(in) :: x
        type(expr_t), intent(in) :: factor
        type(tps_t), intent(out) :: s
        integer :: k

        s%val = x%val
        s%ap = x%ap
        allocate (s%c(size(x%c)))
        do k = 1, size(x%c)
            s%c(k) = x%c(k)*factor
        end do
    end subroutine scale

    !> Simplify coefficients, drop proved leading/trailing zeros and terms at
    !> or beyond the precision.
    subroutine trim(ctx, s, ok, why)
        type(ctx_t), intent(inout) :: ctx
        type(tps_t), intent(inout) :: s
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(engine_result_t) :: res
        integer :: first, last, k
        type(expr_t), allocatable :: kept(:)

        ok = .true.
        why = ""
        last = min(size(s%c), s%ap - s%val)
        do k = 1, last
            res = ctx%engine%simplify(s%c(k))
            if (.not. res%ok) then
                ok = .false.
                why = chars(res%message)
                return
            end if
            s%c(k) = res%value
        end do
        first = 1
        do while (first <= last)
            if (.not. is_zero(ctx, s%c(first))) exit
            first = first + 1
        end do
        do while (last >= first)
            if (.not. is_zero(ctx, s%c(last))) exit
            last = last - 1
        end do
        if (last < first) then
            s%val = s%ap
            deallocate (s%c)
            allocate (s%c(0))
            return
        end if
        kept = s%c(first:last)
        s%val = s%val + first - 1
        call move_alloc(kept, s%c)
    end subroutine trim

    ! --------------------------------------------------------- helpers --

    subroutine constant(ctx, value, s)
        type(ctx_t), intent(inout) :: ctx
        type(expr_t), intent(in) :: value
        type(tps_t), intent(out) :: s

        s%ap = EXACT_AP
        if (is_zero(ctx, value)) then
            s%val = EXACT_AP
            allocate (s%c(0))
        else
            s%val = 0
            s%c = [value]
        end if
    end subroutine constant

    function monomial(ctx, exponent) result(s)
        type(ctx_t), intent(in) :: ctx
        integer, intent(in) :: exponent
        type(tps_t) :: s

        s%val = exponent
        s%ap = EXACT_AP
        s%c = [num(ctx%v%a, 1)]
    end function monomial

    function coefficient(ctx, s, exponent) result(c)
        type(ctx_t), intent(in) :: ctx
        type(tps_t), intent(in) :: s
        integer, intent(in) :: exponent
        type(expr_t) :: c

        if (size(s%c) > 0 .and. exponent >= s%val .and. &
            exponent < s%val + size(s%c)) then
            c = s%c(exponent - s%val + 1)
        else
            c = num(ctx%v%a, 0)
        end if
    end function coefficient

    logical function is_zero(ctx, value) result(zero)
        type(ctx_t), intent(inout) :: ctx
        type(expr_t), intent(in) :: value
        type(engine_result_t) :: res

        if (value%kind() == NK_INT) then
            zero = value%int_value() == 0
            return
        end if
        res = ctx%engine%zero_test(value)
        zero = res%verdict == VERDICT_TRUE
    end function is_zero

    logical function nonzero(ctx, value) result(yes)
        type(ctx_t), intent(inout) :: ctx
        type(expr_t), intent(in) :: value
        type(engine_result_t) :: res

        select case (value%kind())
        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT)
            yes = .not. (value%kind() == NK_INT .and. value%int_value() == 0)
        case default
            res = ctx%engine%zero_test(value)
            yes = res%verdict == VERDICT_FALSE
        end select
    end function nonzero

    subroutine integer_value(e, n, integral)
        type(expr_t), intent(in) :: e
        integer, intent(out) :: n
        logical, intent(out) :: integral

        n = 0
        integral = e%kind() == NK_INT
        if (integral) then
            integral = abs(e%int_value()) < 100000
            if (integral) n = int(e%int_value())
        end if
    end subroutine integer_value

    pure integer function capped(n) result(m)
        integer, intent(in) :: n
        m = min(n, EXACT_AP)
    end function capped

    recursive logical function depends_on(e, v) result(yes)
        type(expr_t), intent(in) :: e, v
        integer :: k

        yes = .false.
        select case (e%kind())
        case (NK_SYM)
            yes = e%id == v%id
        case (NK_ADD, NK_MUL, NK_POW, NK_FUNC)
            do k = 1, e%nargs()
                if (depends_on(e%arg(k), v)) then
                    yes = .true.
                    return
                end if
            end do
        case (NK_INT, NK_RAT, NK_REAL, NK_CONST, NK_BIG_INT, NK_BIG_RAT, &
              NK_BIG_REAL, NK_ALGEBRAIC)
            yes = .false.
        case default
            do k = 1, e%nargs()
                if (depends_on(e%arg(k), v)) then
                    yes = .true.
                    return
                end if
            end do
        end select
    end function depends_on

end module fortsym_series_compose
