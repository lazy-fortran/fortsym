module fortsym_engine_native
    ! Native Fortran algebra over fortsym's hash-consed expression DAG.
    !
    ! This first engine fragment covers checked int64 rational arithmetic,
    ! collection of like terms and integer powers, bounded polynomial
    ! expansion, mechanical differentiation, and conservative zero decisions.
    ! Unsupported forms remain expressions and zero_test returns UNKNOWN.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_expr, only: expr_t
    use fortsym_diff, only: diff_expr => diff
    use fortsym_engine, only: engine_t, engine_result_t, wall_seconds, &
        VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, CAP_ZERO_TEST, &
        CAP_SIMPLIFY, CAP_DIFF, CAP_EXPAND
    implicit none
    private

    public :: native_engine_t, make_native_engine

    integer, parameter :: dp = real64
    integer(int64), parameter :: MAX_I64 = huge(0_int64)
    integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64
    integer(int64), parameter :: MAX_EXPAND_POWER = 32_int64

    type, extends(engine_t) :: native_engine_t
        type(arena_t), pointer :: home => null()
    contains
        procedure :: zero_test => native_zero_test
        procedure :: simplify => native_simplify
        procedure :: diff => native_diff
        procedure :: expand => native_expand
    end type native_engine_t

contains

    function make_native_engine(home) result(eng)
        type(arena_t), target, intent(inout) :: home
        type(native_engine_t)                :: eng

        eng%name = str("native")
        eng%available = .true.
        eng%in_process = .true.
        eng%caps = CAP_ZERO_TEST + CAP_SIMPLIFY + CAP_DIFF + CAP_EXPAND
        eng%home => home
    end function make_native_engine

    function native_simplify(self, e) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(engine_result_t)                 :: r
        integer, allocatable :: memo(:)
        logical, allocatable :: done(:)
        real(dp) :: started

        started = wall_seconds()
        r%value = e
        if (.not. associated(e%a, self%home)) then
            r%message = str("native: expression belongs to a different arena")
            r%seconds = wall_seconds() - started
            return
        end if

        allocate (memo(e%a%size()), source=0)
        allocate (done(e%a%size()), source=.false.)
        r%value%id = simplify_id(e%a, e%id, memo, done)
        r%ok = .true.
        r%seconds = wall_seconds() - started
    end function native_simplify

    function native_zero_test(self, e) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(engine_result_t)                 :: r
        type(engine_result_t) :: simplified

        simplified = self%simplify(e)
        r = simplified
        r%verdict = VERDICT_UNKNOWN
        if (.not. simplified%ok) return

        select case (simplified%value%kind())
        case (NK_INT, NK_RAT)
            if (simplified%value%int_value() == 0_int64) then
                r%verdict = VERDICT_TRUE
            else
                r%verdict = VERDICT_FALSE
            end if
        case (NK_REAL)
            if (simplified%value%real_value() == 0.0_dp) then
                r%verdict = VERDICT_TRUE
            else
                r%verdict = VERDICT_FALSE
            end if
        end select
    end function native_zero_test

    function native_diff(self, e, v) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e, v
        type(engine_result_t)                 :: r
        type(expr_t) :: raw
        real(dp) :: started

        started = wall_seconds()
        raw = diff_expr(e, v)
        r = self%simplify(raw)
        r%seconds = wall_seconds() - started
    end function native_diff

    function native_expand(self, e) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(engine_result_t)                 :: r
        integer, allocatable :: memo(:)
        logical, allocatable :: done(:)
        type(expr_t) :: expanded
        real(dp) :: started

        started = wall_seconds()
        r%value = e
        if (.not. associated(e%a, self%home)) then
            r%message = str("native: expression belongs to a different arena")
            r%seconds = wall_seconds() - started
            return
        end if

        allocate (memo(e%a%size()), source=0)
        allocate (done(e%a%size()), source=.false.)
        expanded = e
        expanded%id = expand_id(e%a, e%id, memo, done)
        r = self%simplify(expanded)
        r%seconds = wall_seconds() - started
    end function native_expand

    recursive function simplify_id(a, id, memo, done) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: id
        integer,       intent(inout) :: memo(:)
        logical,       intent(inout) :: done(:)
        integer                      :: out
        integer, allocatable :: children(:)
        integer :: k

        if (done(id)) then
            out = memo(id)
            return
        end if

        select case (a%kind_of(id))
        case (NK_ADD, NK_MUL, NK_FUNC)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = simplify_id(a, a%arg_of(id, k), memo, done)
            end do
            select case (a%kind_of(id))
            case (NK_ADD)
                out = simplify_add(a, children)
            case (NK_MUL)
                out = simplify_mul(a, children)
            case default
                out = simplify_function(a, chars(a%name_of(id)), children)
            end select
        case (NK_POW)
            allocate (children(2))
            children(1) = simplify_id(a, a%arg_of(id, 1), memo, done)
            children(2) = simplify_id(a, a%arg_of(id, 2), memo, done)
            out = simplify_power(a, children(1), children(2))
        case default
            out = id
        end select

        done(id) = .true.
        memo(id) = out
    end function simplify_id

    function simplify_add(a, operands) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: operands(:)
        integer                      :: out
        integer, allocatable :: terms(:), bases(:), result(:)
        logical, allocatable :: live(:)
        integer(int64), allocatable :: cnum(:), cden(:)
        integer(int64) :: n, d, sn, sd
        integer :: flat, i, j, count, base, term
        logical :: exact, combined

        flat = a%add(operands)
        if (a%kind_of(flat) /= NK_ADD) then
            out = flat
            return
        end if

        allocate (terms(a%nargs_of(flat)))
        do i = 1, size(terms)
            terms(i) = a%arg_of(flat, i)
        end do
        allocate (bases(size(terms)), cnum(size(terms)), cden(size(terms)))
        allocate (live(size(terms)), source=.false.)
        count = 0

        do i = 1, size(terms)
            call split_coefficient(a, terms(i), base, n, d, exact)
            if (.not. exact) then
                base = terms(i)
                n = 1_int64
                d = 1_int64
            end if

            combined = .false.
            do j = 1, count
                if (bases(j) /= base) cycle
                call fraction_add(cnum(j), cden(j), n, d, sn, sd, exact)
                if (.not. exact) cycle
                cnum(j) = sn
                cden(j) = sd
                combined = .true.
                exit
            end do
            if (combined) cycle

            count = count + 1
            bases(count) = base
            cnum(count) = n
            cden(count) = d
            live(count) = .true.
        end do

        allocate (result(count))
        j = 0
        do i = 1, count
            if (.not. live(i)) cycle
            if (cnum(i) == 0_int64) cycle
            term = a%rat(cnum(i), cden(i))
            if (bases(i) /= 0) then
                if (cnum(i) == cden(i)) then
                    term = bases(i)
                else
                    term = a%mul([term, bases(i)])
                end if
            end if
            j = j + 1
            result(j) = term
        end do

        if (j == 0) then
            out = a%int(0_int64)
        else
            out = a%add(result(1:j))
        end if
    end function simplify_add

    subroutine split_coefficient(a, id, base, n, d, exact)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: id
        integer,       intent(out)   :: base
        integer(int64), intent(out)  :: n, d
        logical,       intent(out)   :: exact
        integer, allocatable :: factors(:)
        integer(int64) :: fn, fd, pn, pd
        integer :: i, nf
        logical :: factor_exact, product_ok

        call exact_value(a, id, n, d, exact)
        if (exact) then
            base = 0
            return
        end if

        if (a%kind_of(id) /= NK_MUL) then
            base = id
            n = 1_int64
            d = 1_int64
            exact = .true.
            return
        end if

        allocate (factors(a%nargs_of(id)))
        nf = 0
        n = 1_int64
        d = 1_int64
        do i = 1, a%nargs_of(id)
            call exact_value(a, a%arg_of(id, i), fn, fd, factor_exact)
            if (factor_exact) then
                call fraction_mul(n, d, fn, fd, pn, pd, product_ok)
                if (.not. product_ok) then
                    exact = .false.
                    base = id
                    return
                end if
                n = pn
                d = pd
            else
                nf = nf + 1
                factors(nf) = a%arg_of(id, i)
            end if
        end do

        if (nf == 0) then
            base = 0
        else
            base = a%mul(factors(1:nf))
        end if
        exact = .true.
    end subroutine split_coefficient

    function simplify_mul(a, operands) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: operands(:)
        integer                      :: out
        integer, allocatable :: factors(:), bases(:), result(:)
        integer(int64), allocatable :: exponents(:)
        integer(int64) :: n, d, fn, fd, pn, pd, exponent, sum_exp
        integer :: flat, i, j, count, base
        logical :: exact, product_ok, combined, power_factor

        flat = a%mul(operands)
        if (a%kind_of(flat) /= NK_MUL) then
            out = flat
            return
        end if

        allocate (factors(a%nargs_of(flat)))
        do i = 1, size(factors)
            factors(i) = a%arg_of(flat, i)
        end do
        allocate (bases(size(factors)), exponents(size(factors)))

        n = 1_int64
        d = 1_int64
        count = 0
        do i = 1, size(factors)
            call exact_value(a, factors(i), fn, fd, exact)
            if (exact) then
                call fraction_mul(n, d, fn, fd, pn, pd, product_ok)
                if (.not. product_ok) then
                    count = count + 1
                    bases(count) = factors(i)
                    exponents(count) = 1_int64
                else
                    n = pn
                    d = pd
                end if
                cycle
            end if

            call integer_power_factor(a, factors(i), base, exponent, power_factor)
            if (.not. power_factor) then
                base = factors(i)
                exponent = 1_int64
            end if

            combined = .false.
            do j = 1, count
                if (bases(j) /= base) cycle
                call checked_add(exponents(j), exponent, sum_exp, exact)
                if (.not. exact) cycle
                exponents(j) = sum_exp
                combined = .true.
                exit
            end do
            if (combined) cycle

            count = count + 1
            bases(count) = base
            exponents(count) = exponent
        end do

        if (n == 0_int64) then
            out = a%int(0_int64)
            return
        end if

        allocate (result(count + 1))
        j = 0
        if (n /= d) then
            j = j + 1
            result(j) = a%rat(n, d)
        end if
        do i = 1, count
            if (exponents(i) == 0_int64) cycle
            j = j + 1
            if (exponents(i) == 1_int64) then
                result(j) = bases(i)
            else
                result(j) = a%pow(bases(i), a%int(exponents(i)))
            end if
        end do

        if (j == 0) then
            out = a%int(1_int64)
        else
            out = a%mul(result(1:j))
        end if
    end function simplify_mul

    subroutine integer_power_factor(a, id, base, exponent, yes)
        type(arena_t), intent(in)  :: a
        integer,       intent(in)  :: id
        integer,       intent(out) :: base
        integer(int64), intent(out) :: exponent
        logical,       intent(out) :: yes
        integer(int64) :: den
        logical :: exact

        yes = .false.
        base = id
        exponent = 1_int64
        if (a%kind_of(id) /= NK_POW) return
        call exact_value(a, a%arg_of(id, 2), exponent, den, exact)
        if (.not. exact) return
        if (den /= 1_int64) return
        base = a%arg_of(id, 1)
        yes = .true.
    end subroutine integer_power_factor

    function simplify_power(a, base, exponent_id) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base, exponent_id
        integer                      :: out
        integer(int64) :: exponent, den, bn, bd, rn, rd, nested, combined
        logical :: exact, base_exact, power_ok, nested_ok

        call exact_value(a, exponent_id, exponent, den, exact)
        if (.not. exact) then
            out = a%pow(base, exponent_id)
            return
        end if
        if (den /= 1_int64) then
            out = a%pow(base, exponent_id)
            return
        end if

        if (exponent == 0_int64) then
            if (is_zero_id(a, base)) then
                out = a%pow(base, exponent_id)
            else
                out = a%int(1_int64)
            end if
            return
        end if
        if (exponent == 1_int64) then
            out = base
            return
        end if
        if (is_one_id(a, base)) then
            out = a%int(1_int64)
            return
        end if
        if (is_zero_id(a, base)) then
            if (exponent > 0_int64) then
                out = a%int(0_int64)
            else
                out = a%pow(base, exponent_id)
            end if
            return
        end if

        call exact_value(a, base, bn, bd, base_exact)
        if (base_exact) then
            call fraction_power(bn, bd, exponent, rn, rd, power_ok)
            if (power_ok) then
                out = a%rat(rn, rd)
                return
            end if
        end if

        call integer_power_factor(a, base, out, nested, nested_ok)
        if (nested_ok) then
            call checked_mul(nested, exponent, combined, exact)
            if (exact) then
                out = a%pow(out, a%int(combined))
                return
            end if
        end if
        out = a%pow(base, exponent_id)
    end function simplify_power

    function simplify_function(a, name, args) result(out)
        type(arena_t), intent(inout) :: a
        character(*),  intent(in)    :: name
        integer,       intent(in)    :: args(:)
        integer                      :: out
        integer(int64) :: order, den
        logical :: exact

        out = a%func(name, args)
        if (size(args) == 0) return

        select case (name)
        case ("sin", "tan", "sinh", "tanh", "asin", "atan", "asinh")
            if (is_zero_id(a, args(1))) out = a%int(0_int64)
        case ("cos", "cosh", "exp")
            if (is_zero_id(a, args(1))) out = a%int(1_int64)
        case ("log")
            if (is_one_id(a, args(1))) out = a%int(0_int64)
        case ("sqrt", "abs")
            if (is_zero_id(a, args(1))) out = a%int(0_int64)
            if (is_one_id(a, args(1))) out = a%int(1_int64)
        case ("besselj")
            if (size(args) < 2) return
            call exact_value(a, args(1), order, den, exact)
            if (.not. exact) return
            if (den /= 1_int64) return
            if (order >= 0_int64) return
            if (order == MIN_I64) return
            out = a%func("besselj", [a%int(-order), args(2)])
            if (mod(-order, 2_int64) == 1_int64) then
                out = a%mul([a%int(-1_int64), out])
            end if
        end select
    end function simplify_function

    recursive function expand_id(a, id, memo, done) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: id
        integer,       intent(inout) :: memo(:)
        logical,       intent(inout) :: done(:)
        integer                      :: out
        integer, allocatable :: children(:)
        integer(int64) :: exponent, den
        integer :: k, base, acc
        logical :: exact

        if (done(id)) then
            out = memo(id)
            return
        end if

        select case (a%kind_of(id))
        case (NK_ADD)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = expand_id(a, a%arg_of(id, k), memo, done)
            end do
            out = a%add(children)
        case (NK_MUL)
            acc = a%int(1_int64)
            do k = 1, a%nargs_of(id)
                base = expand_id(a, a%arg_of(id, k), memo, done)
                acc = distribute(a, acc, base)
            end do
            out = acc
        case (NK_POW)
            base = expand_id(a, a%arg_of(id, 1), memo, done)
            out = expand_id(a, a%arg_of(id, 2), memo, done)
            call exact_value(a, out, exponent, den, exact)
            if (exact) then
                if (den /= 1_int64) exact = .false.
            end if
            if (exact) then
                if (exponent < 0_int64) exact = .false.
                if (exponent > MAX_EXPAND_POWER) exact = .false.
            end if
            if (exact) then
                acc = a%int(1_int64)
                do k = 1, int(exponent)
                    acc = distribute(a, acc, base)
                end do
                out = acc
            else
                out = a%pow(base, out)
            end if
        case (NK_FUNC)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = expand_id(a, a%arg_of(id, k), memo, done)
            end do
            out = a%func(chars(a%name_of(id)), children)
        case default
            out = id
        end select

        done(id) = .true.
        memo(id) = out
    end function expand_id

    function distribute(a, left, right) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: left, right
        integer                      :: out
        integer, allocatable :: terms(:)
        integer :: i, j, nleft, nright, k, lid, rid

        nleft = 1
        if (a%kind_of(left) == NK_ADD) nleft = a%nargs_of(left)
        nright = 1
        if (a%kind_of(right) == NK_ADD) nright = a%nargs_of(right)

        if (nleft == 1 .and. nright == 1) then
            out = a%mul([left, right])
            return
        end if

        allocate (terms(nleft*nright))
        k = 0
        do i = 1, nleft
            lid = left
            if (a%kind_of(left) == NK_ADD) lid = a%arg_of(left, i)
            do j = 1, nright
                rid = right
                if (a%kind_of(right) == NK_ADD) rid = a%arg_of(right, j)
                k = k + 1
                terms(k) = a%mul([lid, rid])
            end do
        end do
        out = a%add(terms)
    end function distribute

    subroutine exact_value(a, id, n, d, exact)
        type(arena_t), intent(in)  :: a
        integer,       intent(in)  :: id
        integer(int64), intent(out) :: n, d
        logical,       intent(out) :: exact

        exact = .true.
        select case (a%kind_of(id))
        case (NK_INT)
            n = a%num_of(id)
            d = 1_int64
        case (NK_RAT)
            n = a%num_of(id)
            d = a%den_of(id)
        case default
            n = 0_int64
            d = 1_int64
            exact = .false.
        end select
    end subroutine exact_value

    function is_zero_id(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            yes = a%num_of(id) == 0_int64
        case (NK_REAL)
            yes = a%real_of(id) == 0.0_dp
        end select
    end function is_zero_id

    function is_one_id(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            yes = a%num_of(id) == a%den_of(id)
        case (NK_REAL)
            yes = a%real_of(id) == 1.0_dp
        end select
    end function is_one_id

    subroutine fraction_add(n1, d1, n2, d2, n, d, ok)
        integer(int64), intent(in)  :: n1, d1, n2, d2
        integer(int64), intent(out) :: n, d
        logical,        intent(out) :: ok
        integer(int64) :: g, m1, m2, a1, a2, divisor

        g = gcd_positive(d1, d2)
        m1 = d2/g
        m2 = d1/g
        call checked_mul(n1, m1, a1, ok)
        if (.not. ok) return
        call checked_mul(n2, m2, a2, ok)
        if (.not. ok) return
        call checked_add(a1, a2, n, ok)
        if (.not. ok) return
        call checked_mul(d1, m1, d, ok)
        if (.not. ok) return
        if (n == MIN_I64) then
            ok = .false.
            return
        end if
        divisor = gcd_positive(abs(n), d)
        n = n/divisor
        d = d/divisor
    end subroutine fraction_add

    subroutine fraction_mul(n1, d1, n2, d2, n, d, ok)
        integer(int64), intent(in)  :: n1, d1, n2, d2
        integer(int64), intent(out) :: n, d
        logical,        intent(out) :: ok
        integer(int64) :: g1, g2, an1, an2, ad1, ad2

        ok = .false.
        if (n1 == MIN_I64) return
        if (n2 == MIN_I64) return
        g1 = gcd_positive(abs(n1), d2)
        g2 = gcd_positive(abs(n2), d1)
        an1 = n1/g1
        ad2 = d2/g1
        an2 = n2/g2
        ad1 = d1/g2
        call checked_mul(an1, an2, n, ok)
        if (.not. ok) return
        call checked_mul(ad1, ad2, d, ok)
    end subroutine fraction_mul

    subroutine fraction_power(bn, bd, exponent, n, d, ok)
        integer(int64), intent(in)  :: bn, bd, exponent
        integer(int64), intent(out) :: n, d
        logical,        intent(out) :: ok
        integer(int64) :: k, count, pn, pd, next_n, next_d

        ok = .false.
        n = 1_int64
        d = 1_int64
        if (bn == MIN_I64) return
        if (exponent == MIN_I64) return
        count = abs(exponent)
        if (count > 64_int64) return
        if (exponent < 0_int64) then
            if (bn == 0_int64) return
            pn = bd
            pd = bn
            if (pd < 0_int64) then
                pn = -pn
                pd = -pd
            end if
        else
            pn = bn
            pd = bd
        end if

        ok = .true.
        do k = 1_int64, count
            call fraction_mul(n, d, pn, pd, next_n, next_d, ok)
            if (.not. ok) return
            n = next_n
            d = next_d
        end do
    end subroutine fraction_power

    subroutine checked_add(x, y, result, ok)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: result
        logical,        intent(out) :: ok

        ok = .false.
        result = 0_int64
        if (y > 0_int64) then
            if (x > MAX_I64 - y) return
        else if (y < 0_int64) then
            if (x < MIN_I64 - y) return
        end if
        result = x + y
        ok = .true.
    end subroutine checked_add

    subroutine checked_mul(x, y, result, ok)
        integer(int64), intent(in)  :: x, y
        integer(int64), intent(out) :: result
        logical,        intent(out) :: ok

        ok = .false.
        result = 0_int64
        if (x == 0_int64 .or. y == 0_int64) then
            ok = .true.
            return
        end if

        if (x > 0_int64) then
            if (y > 0_int64) then
                if (x > MAX_I64/y) return
            else
                if (y < MIN_I64/x) return
            end if
        else
            if (y > 0_int64) then
                if (x < MIN_I64/y) return
            else
                if (y < MAX_I64/x) return
            end if
        end if

        result = x*y
        ok = .true.
    end subroutine checked_mul

    function gcd_positive(a, b) result(g)
        integer(int64), intent(in) :: a, b
        integer(int64)             :: g
        integer(int64) :: x, y, t

        x = a
        y = b
        do while (y /= 0_int64)
            t = mod(x, y)
            x = y
            y = t
        end do
        g = x
        if (g == 0_int64) g = 1_int64
    end function gcd_positive

end module fortsym_engine_native
