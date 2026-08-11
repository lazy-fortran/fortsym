module fortsym_engine_native
    ! Native Fortran algebra over fortsym's hash-consed expression DAG.
    !
    ! Exact scalar operations use checked int64 arithmetic when possible and
    ! promote to the bounded FLINT bridge on overflow.  The engine also covers
    ! collection of like terms and integer powers, bounded polynomial expansion,
    ! mechanical differentiation, and conservative zero decisions. Unsupported
    ! forms remain expressions and zero_test returns UNKNOWN.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr, only: expr_t, num, operator(+), operator(-), operator(*), &
        operator(/), operator(**)
    use fortsym_exact, only: exact_add, exact_mul, exact_pow
    use fortsym_assume, only: assumption_context_t, FACT_REAL, &
        FACT_POSITIVE, FACT_NONNEGATIVE
    use fortsym_diff, only: diff_expr => diff
    use fortsym_subs, only: subs
    use fortsym_engine, only: engine_t, engine_result_t, resource_limit_t, &
        resource_exceeded, resource_visit, resource_failure, wall_seconds, &
        VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, CAP_ZERO_TEST, &
        CAP_SIMPLIFY, CAP_DIFF, CAP_EXPAND, CAP_SOLVE, CAP_SERIES
    implicit none
    private

    public :: native_engine_t, make_native_engine

    integer, parameter :: dp = real64
    integer(int64), parameter :: MAX_I64 = huge(0_int64)
    integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64
    integer(int64), parameter :: MAX_EXPAND_POWER = 32_int64
    integer(int64), parameter :: MAX_EXPAND_TERMS = 100000_int64

    type :: exact_coefficient_t
        logical :: compact = .true.
        integer(int64) :: numerator = 0_int64
        integer(int64) :: denominator = 1_int64
        integer :: id = 0
    end type exact_coefficient_t

    type, extends(engine_t) :: native_engine_t
        type(arena_t), pointer :: home => null()
        type(assumption_context_t), pointer :: assumptions => null()
        integer, allocatable :: simplify_cache(:)
        integer, allocatable :: expand_cache(:)
    contains
        procedure :: zero_test => native_zero_test
        procedure :: simplify => native_simplify
        procedure :: diff => native_diff
        procedure :: expand => native_expand
        procedure :: series => native_series
        procedure :: series_coeff => native_series_coeff
        procedure :: solve => native_solve
    end type native_engine_t

contains

    function make_native_engine(home, assumptions) result(eng)
        type(arena_t), target, intent(inout) :: home
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(native_engine_t)                :: eng

        eng%name = str("native")
        eng%available = .true.
        eng%in_process = .true.
        eng%caps = CAP_ZERO_TEST + CAP_SIMPLIFY + CAP_DIFF + CAP_EXPAND + &
            CAP_SERIES + CAP_SOLVE
        eng%home => home
        if (present(assumptions)) eng%assumptions => assumptions
    end function make_native_engine

    function native_simplify(self, e, limit) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        integer, allocatable :: memo(:), contextual_memo(:)
        logical, allocatable :: done(:), contextual_done(:)
        integer :: cached_id, simplified_id
        real(dp) :: started
        logical :: refused
        character(:), allocatable :: reason
        type(resource_limit_t) :: active_limit

        started = wall_seconds()
        r%value = e
        call resource_exceeded(e, limit, "simplify", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (present(limit)) active_limit = limit
        active_limit%visited_nodes = 0_int64
        active_limit%exceeded_kind = 0
        if (.not. associated(e%a, self%home)) then
            r%message = str("native: expression belongs to a different arena")
            r%seconds = wall_seconds() - started
            return
        end if
        if (.not. associated(self%assumptions)) then
            call ensure_cache(self%simplify_cache, e%a%size())
            if (self%simplify_cache(e%id) /= 0) then
                cached_id = self%simplify_cache(e%id)
                r%value%id = abs(cached_id)
                r%ok = .true.
                if (cached_id < 0) then
                    r%conditional = .true.
                    r%condition = str( &
                        "cancelled denominator bases must be nonzero")
                end if
                r%seconds = wall_seconds() - started
                return
            end if
        end if

        allocate (memo(e%a%size()), source=0)
        allocate (done(e%a%size()), source=.false.)
        simplified_id = simplify_id(e%a, e%id, memo, done, active_limit)
        if (active_limit%exceeded_kind /= 0) then
            r%message = str(resource_failure("simplify", active_limit))
            r%seconds = wall_seconds() - started
            return
        end if
        if (associated(self%assumptions)) then
            allocate (contextual_memo(e%a%size()), source=0)
            allocate (contextual_done(e%a%size()), source=.false.)
            simplified_id = contextual_id(e%a, simplified_id, self%assumptions, &
                contextual_memo, contextual_done, active_limit)
            if (active_limit%exceeded_kind /= 0) then
                r%message = str(resource_failure("simplify", active_limit))
                r%seconds = wall_seconds() - started
                return
            end if
            deallocate (memo, done)
            allocate (memo(e%a%size()), source=0)
            allocate (done(e%a%size()), source=.false.)
            simplified_id = simplify_id(e%a, simplified_id, memo, done, active_limit)
            if (active_limit%exceeded_kind /= 0) then
                r%message = str(resource_failure("simplify", active_limit))
                r%seconds = wall_seconds() - started
                return
            end if
        end if
        r%value%id = simplified_id
        r%ok = .true.
        call set_simplify_condition(e, simplified_id, r)
        if (.not. associated(self%assumptions)) then
            if (r%conditional) then
                self%simplify_cache(e%id) = -simplified_id
            else
                self%simplify_cache(e%id) = simplified_id
            end if
        end if
        r%seconds = wall_seconds() - started
    end function native_simplify

    subroutine set_simplify_condition(original, simplified_id, result)
        type(expr_t),        intent(in)    :: original
        integer,             intent(in)    :: simplified_id
        type(engine_result_t), intent(inout) :: result

        if (simplified_id == original%id) return
        if (.not. has_symbolic_denominator(original%a, original%id)) return
        result%conditional = .true.
        result%condition = str("cancelled denominator bases must be nonzero")
    end subroutine set_simplify_condition

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
        case (NK_BIG_INT, NK_BIG_RAT)
            ! Canonical zero is always downcast to NK_INT.
            r%verdict = VERDICT_FALSE
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

    function native_expand(self, e, limit) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        integer, allocatable :: memo(:)
        logical, allocatable :: done(:)
        type(expr_t) :: expanded
        real(dp) :: started
        logical :: refused
        character(:), allocatable :: reason
        type(resource_limit_t) :: active_limit

        started = wall_seconds()
        r%value = e
        call resource_exceeded(e, limit, "expand", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (present(limit)) active_limit = limit
        active_limit%visited_nodes = 0_int64
        active_limit%exceeded_kind = 0
        if (.not. associated(e%a, self%home)) then
            r%message = str("native: expression belongs to a different arena")
            r%seconds = wall_seconds() - started
            return
        end if
        if (.not. associated(self%assumptions)) then
            call ensure_cache(self%expand_cache, e%a%size())
            if (self%expand_cache(e%id) /= 0) then
                r%value%id = self%expand_cache(e%id)
                r%ok = .true.
                r%seconds = wall_seconds() - started
                return
            end if
        end if

        allocate (memo(e%a%size()), source=0)
        allocate (done(e%a%size()), source=.false.)
        expanded = e
        expanded%id = expand_id(e%a, e%id, memo, done, active_limit)
        if (active_limit%exceeded_kind /= 0) then
            r%message = str(resource_failure("expand", active_limit))
            r%seconds = wall_seconds() - started
            return
        end if
        r = self%simplify(expanded, limit)
        if (r%ok) then
            if (.not. associated(self%assumptions)) then
                self%expand_cache(e%id) = r%value%id
            end if
        end if
        r%seconds = wall_seconds() - started
    end function native_expand

    subroutine ensure_cache(cache, needed)
        integer, allocatable, intent(inout) :: cache(:)
        integer,              intent(in)    :: needed
        integer, allocatable :: larger(:)
        integer :: capacity

        if (.not. allocated(cache)) then
            allocate (cache(max(256, needed)), source=0)
            return
        end if
        if (size(cache) >= needed) return

        capacity = size(cache)
        do while (capacity < needed)
            capacity = 2*capacity
        end do
        allocate (larger(capacity), source=0)
        larger(1:size(cache)) = cache
        call move_alloc(larger, cache)
    end subroutine ensure_cache

    function native_series_coeff(self, e, v, point, order, limit) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e, v, point
        integer,                intent(in)    :: order
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        type(engine_result_t) :: prepared, differentiated
        type(expr_t) :: derivative, at_point
        integer(int64) :: factorial
        real(dp) :: started
        logical :: factorial_ok
        integer :: k, normalized_base
        logical :: refused
        character(:), allocatable :: reason

        started = wall_seconds()
        r%value = e
        call resource_exceeded(e, limit, "series", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (order < 0) then
            r%message = str("native: series order must be nonnegative")
            r%seconds = wall_seconds() - started
            return
        end if

        call factorial_i64(order, factorial, factorial_ok)
        if (.not. factorial_ok) then
            r%message = str("native: series factorial exceeds int64")
            r%seconds = wall_seconds() - started
            return
        end if

        ! Expand and cancel polynomial powers before substituting the center.
        ! This turns removable forms such as (2*a*r + 4*b*r**3)/r into a
        ! regular polynomial before r=0 is applied.
        prepared = self%expand(e, limit)
        if (.not. prepared%ok) then
            r = prepared
            r%seconds = wall_seconds() - started
            return
        end if
        derivative = prepared%value
        do k = 1, order
            differentiated = self%diff(derivative, v)
            if (.not. differentiated%ok) then
                r = differentiated
                r%seconds = wall_seconds() - started
                return
            end if
            derivative = differentiated%value
        end do
        at_point = subs(derivative, v, point)
        r = self%simplify(at_point/num(e%a, factorial), limit)
        r%seconds = wall_seconds() - started
    end function native_series_coeff

    function native_series(self, e, v, point, order, limit) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e, v, point
        integer,                intent(in)    :: order
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        type(engine_result_t) :: coefficient, simplified
        type(expr_t) :: polynomial, term
        integer :: k
        real(dp) :: started
        logical :: refused
        character(:), allocatable :: reason

        started = wall_seconds()
        r%value = e
        call resource_exceeded(e, limit, "series", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (order < 0) then
            r%message = str("native: series order must be nonnegative")
            r%seconds = wall_seconds() - started
            return
        end if

        polynomial = point - point
        do k = 0, order
            coefficient = self%series_coeff(e, v, point, k, limit)
            if (.not. coefficient%ok) then
                r = coefficient
                r%seconds = wall_seconds() - started
                return
            end if
            term = coefficient%value*(v - point)**k
            polynomial = polynomial + term
        end do
        simplified = self%simplify(polynomial, limit)
        r = simplified
        r%seconds = wall_seconds() - started
    end function native_series

    function native_solve(self, e, v, limit) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e, v
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        type(engine_result_t) :: coefficient, constant, linearity, candidate
        type(engine_result_t) :: verified
        type(expr_t) :: derivative, residual
        real(dp) :: started
        logical :: conditional
        logical :: refused
        character(:), allocatable :: reason

        started = wall_seconds()
        r%value = e
        conditional = .false.
        call resource_exceeded(e, limit, "solve", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if

        derivative = diff_expr(e, v)
        coefficient = self%simplify(derivative, limit)
        if (.not. coefficient%ok) then
            r = coefficient
            r%seconds = wall_seconds() - started
            return
        end if

        linearity = self%zero_test(diff_expr(coefficient%value, v))
        if (linearity%verdict /= VERDICT_TRUE) then
            r%message = str("native: equation is not linear in the variable")
            r%seconds = wall_seconds() - started
            return
        end if

        verified = self%zero_test(coefficient%value)
        if (verified%verdict == VERDICT_TRUE) then
            r%message = str("native: linear coefficient is zero")
            r%seconds = wall_seconds() - started
            return
        end if
        if (verified%verdict == VERDICT_UNKNOWN) then
            conditional = .true.
        end if

        constant = self%simplify(subs(e, v, v - v), limit)
        if (.not. constant%ok) then
            r = constant
            r%seconds = wall_seconds() - started
            return
        end if
        candidate = self%simplify(-constant%value/coefficient%value, limit)
        if (.not. candidate%ok) then
            r = candidate
            r%seconds = wall_seconds() - started
            return
        end if

        residual = subs(e, v, candidate%value)
        verified = self%zero_test(residual)
        if (verified%verdict /= VERDICT_TRUE) then
            r%message = str("native: candidate could not be verified")
            r%seconds = wall_seconds() - started
            return
        end if

        r = candidate
        r%ok = .true.
        if (conditional) then
            r%conditional = .true.
            r%condition = str("linear coefficient must be nonzero")
        end if
        r%seconds = wall_seconds() - started
    end function native_solve

    subroutine factorial_i64(order, value, ok)
        integer,        intent(in)  :: order
        integer(int64), intent(out) :: value
        logical,        intent(out) :: ok
        integer(int64) :: next
        integer :: k

        value = 1_int64
        ok = .true.
        do k = 2, order
            call checked_mul(value, int(k, int64), next, ok)
            if (.not. ok) return
            value = next
        end do
    end subroutine factorial_i64

    recursive function simplify_id(a, id, memo, done, limit) result(out)
        type(arena_t), target, intent(inout) :: a
        integer,       intent(in)    :: id
        integer,       intent(inout) :: memo(:)
        logical,       intent(inout) :: done(:)
        type(resource_limit_t), intent(inout), optional :: limit
        integer                      :: out
        integer, allocatable :: children(:)
        integer :: k
        logical :: refused
        character(:), allocatable :: reason

        if (present(limit)) then
            call resource_visit(limit, "simplify", refused, reason)
            if (refused) then
                out = id
                return
            end if
        end if

        if (done(id)) then
            out = memo(id)
            return
        end if

        select case (a%kind_of(id))
        case (NK_ADD, NK_MUL, NK_FUNC)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = simplify_id(a, a%arg_of(id, k), memo, done, limit)
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
            children(1) = simplify_id(a, a%arg_of(id, 1), memo, done, limit)
            children(2) = simplify_id(a, a%arg_of(id, 2), memo, done, limit)
            out = simplify_power(a, children(1), children(2))
        case default
            out = id
        end select

        done(id) = .true.
        memo(id) = out
    end function simplify_id

    recursive function contextual_id(a, id, context, memo, done, limit) result(out)
        type(arena_t), target, intent(inout) :: a
        integer,       intent(in)    :: id
        type(assumption_context_t), intent(in) :: context
        integer,       intent(inout) :: memo(:)
        logical,       intent(inout) :: done(:)
        type(resource_limit_t), intent(inout), optional :: limit
        integer                      :: out
        type(expr_t) :: base_expression
        integer, allocatable :: children(:)
        integer :: k, exponent_id, base_id
        logical :: square, definitely_positive, definitely_nonnegative, real_valued
        integer :: abs_argument(1)
        logical :: refused
        character(:), allocatable :: reason

        if (present(limit)) then
            call resource_visit(limit, "simplify", refused, reason)
            if (refused) then
                out = id
                return
            end if
        end if

        if (done(id)) then
            out = memo(id)
            return
        end if

        select case (a%kind_of(id))
        case (NK_ADD)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = contextual_id(a, a%arg_of(id, k), context, &
                    memo, done, limit)
            end do
            out = simplify_add(a, children)
        case (NK_MUL)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = contextual_id(a, a%arg_of(id, k), context, &
                    memo, done, limit)
            end do
            out = a%mul(children)
        case (NK_POW)
            base_id = contextual_id(a, a%arg_of(id, 1), context, memo, done, limit)
            exponent_id = contextual_id(a, a%arg_of(id, 2), context, memo, done, limit)
            out = a%pow(base_id, exponent_id)
        case (NK_FUNC)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = contextual_id(a, a%arg_of(id, k), context, &
                    memo, done, limit)
            end do
            out = a%func(chars(a%name_of(id)), children)

            if (size(children) > 0) then
                base_expression%a => a
                base_expression%id = children(1)
                base_expression%generation = a%generation_value()
                if (chars(a%name_of(id)) == "abs") then
                    if (context%has(base_expression, FACT_POSITIVE)) out = children(1)
                end if
                if (chars(a%name_of(id)) == "sqrt") then
                    if (a%kind_of(children(1)) == NK_POW) then
                        call is_square_power(a, children(1), base_id, square)
                        if (square) then
                            base_expression%id = base_id
                            definitely_positive = context%has(base_expression, &
                                FACT_POSITIVE)
                            definitely_nonnegative = context%has(base_expression, &
                                FACT_NONNEGATIVE)
                            real_valued = context%has(base_expression, FACT_REAL)
                            if (definitely_positive .or. definitely_nonnegative) &
                                out = base_id
                            if (.not. (definitely_positive .or. &
                                definitely_nonnegative) .and. real_valued) then
                                abs_argument(1) = base_id
                                out = a%func("abs", abs_argument)
                            end if
                        end if
                    end if
                end if
            end if
        case default
            out = id
        end select

        done(id) = .true.
        memo(id) = out
    end function contextual_id

    subroutine is_square_power(a, id, base, square)
        type(arena_t), intent(in)  :: a
        integer,       intent(in)  :: id
        integer,       intent(out) :: base
        logical,       intent(out) :: square
        integer(int64) :: exponent, den
        logical :: exact

        base = id
        square = .false.
        if (a%kind_of(id) /= NK_POW) return
        call exact_value(a, a%arg_of(id, 2), exponent, den, exact)
        if (.not. exact) return
        if (den /= 1_int64) return
        if (exponent /= 2_int64) return
        base = a%arg_of(id, 1)
        square = .true.
    end subroutine is_square_power

    function simplify_add(a, operands) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: operands(:)
        integer                      :: out
        integer, allocatable :: terms(:), bases(:), result(:), filtered(:)
        type(exact_coefficient_t), allocatable :: coefficients(:)
        type(exact_coefficient_t) :: coefficient, coefficient2, sum_coefficient
        logical, allocatable :: top_live(:)
        integer :: flat, i, j, count, base, base2, term, remaining
        integer :: pair(2)
        logical :: exact, exact2, combined

        ! Preserve a composite term long enough to cancel its explicit
        ! negative. Flattening u + (-u) first would splice u's children into
        ! the outer sum and hide that the two operands are opposites.
        allocate (top_live(size(operands)), source=.true.)
        do i = 1, size(operands)
            if (.not. top_live(i)) cycle
            call split_coefficient(a, operands(i), base, coefficient, exact)
            if (.not. exact) cycle
            do j = i + 1, size(operands)
                if (.not. top_live(j)) cycle
                call split_coefficient(a, operands(j), base2, coefficient2, &
                    exact2)
                if (.not. exact2) cycle
                if (base2 /= base) cycle
                call coefficient_add(a, coefficient, coefficient2, &
                    sum_coefficient, exact2)
                if (.not. exact2) cycle
                if (.not. coefficient_is_zero(sum_coefficient)) cycle
                top_live(i) = .false.
                top_live(j) = .false.
                exit
            end do
        end do

        remaining = 0
        do i = 1, size(top_live)
            if (top_live(i)) remaining = remaining + 1
        end do
        if (remaining == 0) then
            out = a%int(0_int64)
            return
        end if
        allocate (filtered(remaining))
        j = 0
        do i = 1, size(operands)
            if (.not. top_live(i)) cycle
            j = j + 1
            filtered(j) = operands(i)
        end do

        flat = a%add(filtered)
        if (a%kind_of(flat) /= NK_ADD) then
            out = flat
            return
        end if

        allocate (terms(a%nargs_of(flat)))
        do i = 1, size(terms)
            terms(i) = a%arg_of(flat, i)
        end do
        allocate (bases(size(terms)), coefficients(size(terms)))
        count = 0

        do i = 1, size(terms)
            call split_coefficient(a, terms(i), base, coefficient, exact)
            if (.not. exact) then
                base = terms(i)
                coefficient = coefficient_one()
            end if

            combined = .false.
            do j = 1, count
                if (bases(j) /= base) cycle
                call coefficient_add(a, coefficients(j), coefficient, &
                    sum_coefficient, exact)
                if (.not. exact) then
                    ! Do not expose earlier coefficient rewrites when the
                    ! resource-bounded exact result cannot be interned.
                    out = flat
                    return
                end if
                coefficients(j) = sum_coefficient
                combined = .true.
                exit
            end do
            if (combined) cycle

            count = count + 1
            bases(count) = base
            coefficients(count) = coefficient
        end do

        allocate (result(count))
        j = 0
        do i = 1, count
            if (coefficient_is_zero(coefficients(i))) cycle
            term = coefficient_node(a, coefficients(i))
            if (bases(i) /= 0) then
                if (coefficient_is_one(coefficients(i))) then
                    term = bases(i)
                else
                    pair(1) = term
                    pair(2) = bases(i)
                    term = a%mul(pair)
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

    function factor_common_add_node(a, id) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: id
        integer                      :: out, i
        integer, allocatable         :: terms(:)

        if (a%kind_of(id) /= NK_ADD) then
            out = id
            return
        end if
        allocate (terms(a%nargs_of(id)))
        do i = 1, size(terms)
            terms(i) = a%arg_of(id, i)
        end do
        out = factor_common_add(a, terms)
    end function factor_common_add_node

    function factor_common_add(a, terms) result(out)
        ! Pull a common positive rational factor from an exact sum.  This is
        ! a canonicalization, not a heuristic simplification: every term is
        ! multiplied by the reciprocal factor before the factored product is
        ! built.  It makes exact polynomial forms independent of whether the
        ! parser attached decimal-looking rational constants to each term or
        ! to the whole denominator.
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: terms(:)
        integer                      :: out, i, base, term, inner
        integer(int64)               :: common_n, common_d, n, d, gcd_n, gcd_d
        integer(int64)               :: lcm_d
        integer, allocatable         :: normalized(:)
        integer                      :: pair(2)
        type(exact_coefficient_t)    :: coefficient, inverse, scaled
        logical                      :: exact, ok, changed

        if (size(terms) < 2) then
            out = a%add(terms)
            return
        end if

        common_n = 0_int64
        common_d = 1_int64
        do i = 1, size(terms)
            call split_coefficient(a, terms(i), base, coefficient, exact)
            if (.not. exact .or. .not. coefficient%compact) then
                out = a%add(terms)
                return
            end if
            n = coefficient%numerator
            d = coefficient%denominator
            if (n == MIN_I64) then
                out = a%add(terms)
                return
            end if
            if (n == 0_int64) cycle
            if (common_n == 0_int64) then
                common_n = abs(n)
            else
                common_n = gcd_positive(common_n, abs(n))
            end if
            gcd_d = gcd_positive(common_d, d)
            call checked_mul(common_d/gcd_d, d, lcm_d, ok)
            if (.not. ok) then
                out = a%add(terms)
                return
            end if
            common_d = lcm_d
        end do

        if (common_n == 0_int64) then
            out = a%add(terms)
            return
        end if
        changed = common_n /= common_d
        if (.not. changed) then
            out = a%add(terms)
            return
        end if

        inverse = coefficient_one()
        inverse%numerator = common_d
        inverse%denominator = common_n
        allocate (normalized(size(terms)))
        do i = 1, size(terms)
            call split_coefficient(a, terms(i), base, coefficient, exact)
            if (.not. exact) then
                out = a%add(terms)
                return
            end if
            call coefficient_mul(a, coefficient, inverse, scaled, ok)
            if (.not. ok) then
                out = a%add(terms)
                return
            end if
            term = coefficient_node(a, scaled)
            if (base /= 0) then
                if (coefficient_is_one(scaled)) then
                    term = base
                else
                    pair(1) = term
                    pair(2) = base
                    term = a%mul(pair)
                end if
            end if
            normalized(i) = term
        end do

        ! Run the ordinary like-term cancellation on the normalized inner
        ! sum.  Factoring before that pass would hide x + 1 - x inside a
        ! product and can make downstream coefficient extraction refuse a
        ! polynomial that was previously recognized.
        inner = simplify_add(a, normalized)
        pair(1) = a%rat(common_n, common_d)
        pair(2) = inner
        out = a%mul(pair)
    end function factor_common_add

    subroutine split_coefficient(a, id, base, coefficient, exact)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: id
        integer,       intent(out)   :: base
        type(exact_coefficient_t), intent(out) :: coefficient
        logical,       intent(out)   :: exact
        integer, allocatable :: factors(:)
        type(exact_coefficient_t) :: factor_coefficient, product
        integer :: i, nf, factor
        logical :: product_ok

        if (is_exact_scalar_kind(a%kind_of(id))) then
            base = 0
            call coefficient_from_node(a, id, coefficient, exact)
            return
        end if

        if (a%kind_of(id) /= NK_MUL) then
            base = id
            coefficient = coefficient_one()
            exact = .true.
            return
        end if

        allocate (factors(a%nargs_of(id)))
        nf = 0
        coefficient = coefficient_one()
        do i = 1, a%nargs_of(id)
            factor = a%arg_of(id, i)
            if (is_exact_scalar_kind(a%kind_of(factor))) then
                call coefficient_from_node(a, factor, factor_coefficient, &
                    product_ok)
                if (product_ok) then
                    call coefficient_mul(a, coefficient, factor_coefficient, &
                        product, product_ok)
                end if
                if (.not. product_ok) then
                    exact = .false.
                    base = id
                    return
                end if
                coefficient = product
            else
                nf = nf + 1
                factors(nf) = factor
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
        type(exact_coefficient_t) :: numeric_product, factor_coefficient, product
        integer(int64) :: exponent, sum_exp
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

        numeric_product = coefficient_one()
        count = 0
        do i = 1, size(factors)
            if (is_exact_scalar_kind(a%kind_of(factors(i)))) then
                call coefficient_from_node(a, factors(i), factor_coefficient, &
                    product_ok)
                if (product_ok) then
                    call coefficient_mul(a, numeric_product, factor_coefficient, &
                        product, product_ok)
                end if
                if (.not. product_ok) then
                    ! Resource refusal is not an algebraic result: retain the
                    ! original multiplication without partial rewriting.
                    out = flat
                    return
                end if
                numeric_product = product
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

        if (coefficient_is_zero(numeric_product)) then
            out = a%int(0_int64)
            return
        end if

        allocate (result(count + 1))
        j = 0
        if (.not. coefficient_is_one(numeric_product)) then
            j = j + 1
            result(j) = coefficient_node(a, numeric_product)
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

    recursive function simplify_power(a, base, exponent_id) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base, exponent_id
        integer                      :: out
        integer, allocatable :: factors(:)
        integer(int64) :: exponent, den, nested, combined
        integer :: k, normalized_base
        logical :: exact, power_ok, nested_ok

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
        if (exponent == 2_int64 .and. a%kind_of(base) == NK_FUNC) then
            if (chars(a%name_of(base)) == "sqrt") then
                if (a%nargs_of(base) == 1) then
                    call exact_value(a, a%arg_of(base, 1), nested, den, exact)
                    if (exact .and. nested >= 0_int64) then
                        out = a%arg_of(base, 1)
                        return
                    end if
                end if
            end if
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

        if (is_exact_scalar_kind(a%kind_of(base))) then
            out = exact_pow_node(a, base, exponent, power_ok)
            if (power_ok) then
                return
            end if
        end if

        ! A common scalar in a denominator is the one place where Wolfram's
        ! FullSimplify consistently exposes a useful canonical form. Keep
        ! this local to negative powers: factoring every sum would alter the
        ! structural spelling of ordinary polynomial results and can hide
        ! like-term cancellation from downstream coefficient extraction.
        if (exponent < 0_int64 .and. a%kind_of(base) == NK_ADD) then
            normalized_base = factor_common_add_node(a, base)
            if (normalized_base /= base) then
                out = simplify_power(a, normalized_base, exponent_id)
                return
            end if
        end if

        if (a%kind_of(base) == NK_MUL) then
            allocate (factors(a%nargs_of(base)))
            do k = 1, size(factors)
                factors(k) = simplify_power(a, a%arg_of(base, k), exponent_id)
            end do
            out = simplify_mul(a, factors)
            return
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
        integer(int64) :: pi_multiple
        logical :: exact
        logical :: pi_multiple_ok
        integer :: bessel_args(2), pair(2)

        out = a%func(name, args)
        if (size(args) == 0) return

        select case (name)
        case ("sin", "tan", "sinh", "tanh", "asin", "atan", "asinh")
            if (is_zero_id(a, args(1))) out = a%int(0_int64)
            if (name == "sin") then
                call integer_pi_multiple(a, args(1), pi_multiple, &
                    pi_multiple_ok)
                if (pi_multiple_ok) out = a%int(0_int64)
            end if
        case ("cos", "cosh", "exp")
            if (is_zero_id(a, args(1))) out = a%int(1_int64)
            if (name == "cos") then
                call integer_pi_multiple(a, args(1), pi_multiple, &
                    pi_multiple_ok)
                if (pi_multiple_ok) then
                    if (modulo(pi_multiple, 2_int64) == 0_int64) then
                        out = a%int(1_int64)
                    else
                        out = a%int(-1_int64)
                    end if
                end if
            end if
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
            bessel_args(1) = a%int(-order)
            bessel_args(2) = args(2)
            out = a%func("besselj", bessel_args)
            if (mod(-order, 2_int64) == 1_int64) then
                pair(1) = a%int(-1_int64)
                pair(2) = out
                out = a%mul(pair)
            end if
        end select
    end function simplify_function

    !> Recognise n*pi without approximating pi. Only an integer coefficient is
    !> accepted, so a symbolic or fractional multiple cannot be simplified by
    !> this identity.
    subroutine integer_pi_multiple(a, id, multiple, ok)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer(int64), intent(out) :: multiple
        logical,        intent(out) :: ok
        integer :: k, factor
        integer(int64) :: numerator, denominator, product
        logical :: exact, saw_pi, product_ok

        multiple = 0_int64
        ok = .false.
        saw_pi = .false.

        if (a%kind_of(id) == NK_CONST) then
            if (chars(a%name_of(id)) == "pi") then
                multiple = 1_int64
                ok = .true.
            end if
            return
        end if
        if (a%kind_of(id) /= NK_MUL) return

        multiple = 1_int64
        do k = 1, a%nargs_of(id)
            factor = a%arg_of(id, k)
            if (a%kind_of(factor) == NK_CONST) then
                if (chars(a%name_of(factor)) == "pi") then
                    if (saw_pi) then
                        ok = .false.
                        return
                    end if
                    saw_pi = .true.
                    cycle
                end if
            end if
            call exact_value(a, factor, numerator, denominator, exact)
            if (.not. exact) return
            if (denominator /= 1_int64) return
            call checked_mul(multiple, numerator, product, product_ok)
            if (.not. product_ok) return
            multiple = product
        end do
        ok = saw_pi
    end subroutine integer_pi_multiple

    recursive function expand_id(a, id, memo, done, limit) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: id
        integer,       intent(inout) :: memo(:)
        logical,       intent(inout) :: done(:)
        type(resource_limit_t), intent(inout), optional :: limit
        integer                      :: out
        integer, allocatable :: children(:)
        integer(int64) :: exponent, den, remaining
        integer :: k, base, acc, factor
        logical :: exact, multinomial_ok
        logical :: refused
        character(:), allocatable :: reason

        if (present(limit)) then
            call resource_visit(limit, "expand", refused, reason)
            if (refused) then
                out = id
                return
            end if
        end if

        if (done(id)) then
            out = memo(id)
            return
        end if

        select case (a%kind_of(id))
        case (NK_ADD)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = expand_id(a, a%arg_of(id, k), memo, done, limit)
            end do
            out = a%add(children)
        case (NK_MUL)
            acc = a%int(1_int64)
            do k = 1, a%nargs_of(id)
                base = expand_id(a, a%arg_of(id, k), memo, done, limit)
                acc = distribute(a, acc, base, limit)
            end do
            out = acc
        case (NK_POW)
            base = expand_id(a, a%arg_of(id, 1), memo, done, limit)
            out = expand_id(a, a%arg_of(id, 2), memo, done, limit)
            call exact_value(a, out, exponent, den, exact)
            if (exact) then
                if (den /= 1_int64) exact = .false.
            end if
            if (exact) then
                if (exponent < 0_int64) exact = .false.
                if (exponent > MAX_EXPAND_POWER) exact = .false.
            end if
            if (exact) then
                multinomial_ok = .false.
                if (exponent == 0_int64) then
                    out = a%int(1_int64)
                    multinomial_ok = .true.
                else if (exponent == 1_int64) then
                    out = base
                    multinomial_ok = .true.
                else if (a%kind_of(base) == NK_ADD) then
                    out = expand_sum_power(a, base, exponent, multinomial_ok, limit)
                    if (.not. multinomial_ok) then
                        out = a%pow(base, a%int(exponent))
                        multinomial_ok = .true.
                    end if
                end if
                if (.not. multinomial_ok) then
                    acc = a%int(1_int64)
                    factor = base
                    remaining = exponent
                    do while (remaining > 0_int64)
                        if (mod(remaining, 2_int64) == 1_int64) then
                            acc = distribute(a, acc, factor, limit)
                            acc = simplify_root_id(a, acc, limit)
                        end if
                        remaining = remaining/2_int64
                        if (remaining > 0_int64) then
                            factor = distribute(a, factor, factor, limit)
                            factor = simplify_root_id(a, factor, limit)
                        end if
                    end do
                    out = acc
                end if
            else
                out = a%pow(base, out)
            end if
        case (NK_FUNC)
            allocate (children(a%nargs_of(id)))
            do k = 1, size(children)
                children(k) = expand_id(a, a%arg_of(id, k), memo, done, limit)
            end do
            out = a%func(chars(a%name_of(id)), children)
        case default
            out = id
        end select

        done(id) = .true.
        memo(id) = out
    end function expand_id

    !> Expand a nonnegative integer power of a sum directly by the multinomial
    !> theorem. This constructs each final monomial once instead of generating
    !> and recollecting the same monomial at every repeated distribution step.
    !> Coefficient or output-size overflow declines the fast path without
    !> changing the expression.
    function expand_sum_power(a, base, exponent, success, limit) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base
        integer(int64), intent(in)   :: exponent
        logical,       intent(out)   :: success
        type(resource_limit_t), intent(inout), optional :: limit
        integer                      :: out
        integer(int64), allocatable :: composition(:)
        integer, allocatable :: factors(:), terms(:)
        integer(int64) :: term_count
        integer :: index, nterms
        logical :: count_ok

        out = base
        success = .false.
        call binomial_nonnegative(exponent + int(a%nargs_of(base) - 1, int64), &
            int(a%nargs_of(base) - 1, int64), term_count, count_ok)
        if (.not. count_ok) return
        if (term_count > MAX_EXPAND_TERMS) return

        nterms = int(term_count)
        allocate (composition(a%nargs_of(base)), source=0_int64)
        success = .true.
        call validate_multinomial(exponent, 1, composition, success, limit)
        if (.not. success) return

        allocate (factors(a%nargs_of(base) + 1))
        allocate (terms(nterms))
        index = 0
        success = .true.
        call enumerate_multinomial(a, base, exponent, 1, composition, terms, &
            factors, index, success, limit)
        if (.not. success) return
        if (index /= nterms) then
            success = .false.
            return
        end if
        out = simplify_add(a, terms)
    end function expand_sum_power

    recursive subroutine validate_multinomial(remaining, position, &
            composition, success, limit)
        integer(int64), intent(in)    :: remaining
        integer,        intent(in)    :: position
        integer(int64), intent(inout) :: composition(:)
        logical,        intent(inout) :: success
        type(resource_limit_t), intent(inout), optional :: limit
        integer(int64) :: coefficient, power
        logical :: refused
        character(:), allocatable :: reason

        if (present(limit)) then
            call resource_visit(limit, "expand", refused, reason)
            if (refused) then
                success = .false.
                return
            end if
        end if

        if (.not. success) return
        if (position == size(composition)) then
            composition(position) = remaining
            call multinomial_coefficient(composition, coefficient, success)
            return
        end if

        do power = 0_int64, remaining
            composition(position) = power
            call validate_multinomial(remaining - power, position + 1, &
                composition, success, limit)
            if (.not. success) return
        end do
    end subroutine validate_multinomial

    recursive subroutine enumerate_multinomial(a, base, remaining, position, &
            composition, terms, factors, index, success, limit)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base, position
        integer(int64), intent(in)   :: remaining
        integer(int64), intent(inout) :: composition(:)
        integer,       intent(inout) :: terms(:), factors(:), index
        logical,       intent(inout) :: success
        type(resource_limit_t), intent(inout), optional :: limit
        integer(int64) :: power
        logical :: refused
        character(:), allocatable :: reason

        if (present(limit)) then
            call resource_visit(limit, "expand", refused, reason)
            if (refused) then
                success = .false.
                return
            end if
        end if

        if (.not. success) return
        if (position == size(composition)) then
            composition(position) = remaining
            index = index + 1
            if (index > size(terms)) then
                success = .false.
                return
            end if
            call build_multinomial_term(a, base, composition, factors, &
                terms(index), success)
            return
        end if

        do power = 0_int64, remaining
            composition(position) = power
            call enumerate_multinomial(a, base, remaining - power, &
                position + 1, composition, terms, factors, index, success, limit)
            if (.not. success) return
        end do
    end subroutine enumerate_multinomial

    subroutine build_multinomial_term(a, base, composition, factors, term, &
            success)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base
        integer(int64), intent(in)   :: composition(:)
        integer,       intent(inout) :: factors(:)
        integer,       intent(out)   :: term
        logical,       intent(out)   :: success
        integer(int64) :: coefficient
        integer :: i, nf, operand

        call multinomial_coefficient(composition, coefficient, success)
        if (.not. success) then
            term = base
            return
        end if

        nf = 0
        if (coefficient /= 1_int64) then
            nf = nf + 1
            factors(nf) = a%int(coefficient)
        end if
        do i = 1, size(composition)
            if (composition(i) == 0_int64) cycle
            operand = a%arg_of(base, i)
            nf = nf + 1
            if (composition(i) == 1_int64) then
                factors(nf) = operand
            else
                factors(nf) = simplify_power(a, operand, &
                    a%int(composition(i)))
            end if
        end do

        if (nf == 0) then
            term = a%int(1_int64)
        else
            term = simplify_mul(a, factors(1:nf))
        end if
    end subroutine build_multinomial_term

    subroutine multinomial_coefficient(composition, coefficient, success)
        integer(int64), intent(in)  :: composition(:)
        integer(int64), intent(out) :: coefficient
        logical,        intent(out) :: success
        integer(int64) :: remaining, choice, next
        integer :: i
        logical :: choose_ok, product_ok

        remaining = sum(composition)
        coefficient = 1_int64
        success = .true.
        do i = 1, size(composition) - 1
            call binomial_nonnegative(remaining, composition(i), choice, &
                choose_ok)
            if (.not. choose_ok) then
                success = .false.
                return
            end if
            call checked_mul(coefficient, choice, next, product_ok)
            if (.not. product_ok) then
                success = .false.
                return
            end if
            coefficient = next
            remaining = remaining - composition(i)
        end do
    end subroutine multinomial_coefficient

    subroutine binomial_nonnegative(n, k, value, success)
        integer(int64), intent(in)  :: n, k
        integer(int64), intent(out) :: value
        logical,        intent(out) :: success
        integer(int64) :: i, kk, numerator, denominator, divisor, product
        logical :: product_ok

        value = 0_int64
        success = .false.
        if (n < 0_int64) return
        if (k < 0_int64) return
        if (k > n) return

        kk = min(k, n - k)
        value = 1_int64
        do i = 1_int64, kk
            numerator = n - kk + i
            denominator = i
            divisor = gcd_positive(numerator, denominator)
            numerator = numerator/divisor
            denominator = denominator/divisor
            divisor = gcd_positive(value, denominator)
            value = value/divisor
            denominator = denominator/divisor
            call checked_mul(value, numerator, product, product_ok)
            if (.not. product_ok) return
            if (denominator /= 1_int64) then
                if (mod(product, denominator) /= 0_int64) return
                product = product/denominator
            end if
            value = product
        end do
        success = .true.
    end subroutine binomial_nonnegative

    function simplify_root_id(a, id, limit) result(out)
        type(arena_t), target, intent(inout) :: a
        integer,       intent(in)            :: id
        type(resource_limit_t), intent(inout), optional :: limit
        integer                              :: out
        integer, allocatable :: memo(:)
        logical, allocatable :: done(:)

        allocate (memo(a%size()), source=0)
        allocate (done(a%size()), source=.false.)
        out = simplify_id(a, id, memo, done, limit)
    end function simplify_root_id

    function distribute(a, left, right, limit) result(out)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: left, right
        type(resource_limit_t), intent(inout), optional :: limit
        integer                      :: out
        integer, allocatable :: terms(:)
        integer :: i, j, nleft, nright, k, lid, rid
        integer :: pair(2)
        logical :: refused
        character(:), allocatable :: reason

        nleft = 1
        if (a%kind_of(left) == NK_ADD) nleft = a%nargs_of(left)
        nright = 1
        if (a%kind_of(right) == NK_ADD) nright = a%nargs_of(right)

        if (nleft == 1 .and. nright == 1) then
            pair(1) = left
            pair(2) = right
            out = a%mul(pair)
            return
        end if

        allocate (terms(nleft*nright))
        k = 0
        do i = 1, nleft
            lid = left
            if (a%kind_of(left) == NK_ADD) lid = a%arg_of(left, i)
            do j = 1, nright
                if (present(limit)) then
                    call resource_visit(limit, "expand", refused, reason)
                    if (refused) then
                        out = left
                        return
                    end if
                end if
                rid = right
                if (a%kind_of(right) == NK_ADD) rid = a%arg_of(right, j)
                k = k + 1
                pair(1) = lid
                pair(2) = rid
                terms(k) = a%mul(pair)
            end do
        end do
        out = a%add(terms)
    end function distribute

    pure function is_exact_scalar_kind(kind) result(exact)
        integer, intent(in) :: kind
        logical             :: exact
        exact = kind == NK_INT .or. kind == NK_RAT .or. &
            kind == NK_BIG_INT .or. kind == NK_BIG_RAT
    end function is_exact_scalar_kind

    pure function coefficient_one() result(coefficient)
        type(exact_coefficient_t) :: coefficient
        coefficient%numerator = 1_int64
    end function coefficient_one

    pure function coefficient_is_zero(coefficient) result(zero)
        type(exact_coefficient_t), intent(in) :: coefficient
        logical                              :: zero
        ! Canonical arbitrary-precision zero is always a compact node.
        zero = coefficient%compact .and. &
            coefficient%numerator == 0_int64
    end function coefficient_is_zero

    pure function coefficient_is_one(coefficient) result(one)
        type(exact_coefficient_t), intent(in) :: coefficient
        logical                              :: one
        ! Canonical arbitrary-precision one is always a compact node.
        one = coefficient%compact .and. &
            coefficient%numerator == coefficient%denominator
    end function coefficient_is_one

    subroutine coefficient_from_node(a, id, coefficient, ok)
        type(arena_t), intent(in)             :: a
        integer, intent(in)                   :: id
        type(exact_coefficient_t), intent(out) :: coefficient
        logical, intent(out)                  :: ok

        call exact_value(a, id, coefficient%numerator, &
            coefficient%denominator, coefficient%compact)
        ok = coefficient%compact
        if (ok) return
        ok = is_exact_scalar_kind(a%kind_of(id))
        if (ok) coefficient%id = id
    end subroutine coefficient_from_node

    function coefficient_node(a, coefficient) result(id)
        type(arena_t), intent(inout)          :: a
        type(exact_coefficient_t), intent(in) :: coefficient
        integer                               :: id

        if (coefficient%compact) then
            id = a%rat(coefficient%numerator, coefficient%denominator)
        else
            id = coefficient%id
        end if
    end function coefficient_node

    subroutine coefficient_add(a, left, right, result, ok)
        type(arena_t), intent(inout)          :: a
        type(exact_coefficient_t), intent(in) :: left, right
        type(exact_coefficient_t), intent(out) :: result
        logical, intent(out)                  :: ok
        integer :: id

        if (coefficient_is_zero(left)) then
            result = right
            ok = .true.
            return
        end if
        if (coefficient_is_zero(right)) then
            result = left
            ok = .true.
            return
        end if
        if (left%compact .and. right%compact) then
            call fraction_add(left%numerator, left%denominator, &
                right%numerator, right%denominator, result%numerator, &
                result%denominator, ok)
            if (ok) return
        end if

        id = exact_add_node(a, coefficient_node(a, left), &
            coefficient_node(a, right), ok)
        if (.not. ok) return
        call coefficient_from_node(a, id, result, ok)
    end subroutine coefficient_add

    subroutine coefficient_mul(a, left, right, result, ok)
        type(arena_t), intent(inout)          :: a
        type(exact_coefficient_t), intent(in) :: left, right
        type(exact_coefficient_t), intent(out) :: result
        logical, intent(out)                  :: ok
        integer :: id

        if (coefficient_is_zero(left) .or. coefficient_is_one(left)) then
            if (coefficient_is_zero(left)) then
                result = left
            else
                result = right
            end if
            ok = .true.
            return
        end if
        if (coefficient_is_zero(right) .or. coefficient_is_one(right)) then
            if (coefficient_is_zero(right)) then
                result = right
            else
                result = left
            end if
            ok = .true.
            return
        end if
        if (left%compact .and. right%compact) then
            call fraction_mul(left%numerator, left%denominator, &
                right%numerator, right%denominator, result%numerator, &
                result%denominator, ok)
            if (ok) return
        end if

        id = exact_mul_node(a, coefficient_node(a, left), &
            coefficient_node(a, right), ok)
        if (.not. ok) return
        call coefficient_from_node(a, id, result, ok)
    end subroutine coefficient_mul

    function exact_add_node(a, left, right, ok) result(id)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: left, right
        logical,       intent(out)   :: ok
        integer                      :: id
        type(str_t) :: value
        logical :: inserted
        integer(int64) :: n1, d1, n2, d2, n, d

        if (a%kind_of(left) == NK_INT .or. a%kind_of(left) == NK_RAT) then
            if (a%kind_of(right) == NK_INT .or. a%kind_of(right) == NK_RAT) then
                call exact_value(a, left, n1, d1, ok)
                call exact_value(a, right, n2, d2, ok)
                call fraction_add(n1, d1, n2, d2, n, d, ok)
                if (ok) then
                    id = a%rat(n, d)
                    return
                end if
            end if
        end if

        id = 0
        value = exact_add(chars(a%exact_text_of(left)), &
            chars(a%exact_text_of(right)), ok)
        if (.not. ok) return
        id = a%exact(chars(value), inserted)
        ok = inserted
        if (.not. ok) id = 0
    end function exact_add_node

    function exact_mul_node(a, left, right, ok) result(id)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: left, right
        logical,       intent(out)   :: ok
        integer                      :: id
        type(str_t) :: value
        logical :: inserted
        integer(int64) :: n1, d1, n2, d2, n, d

        if (a%kind_of(left) == NK_INT .or. a%kind_of(left) == NK_RAT) then
            if (a%kind_of(right) == NK_INT .or. a%kind_of(right) == NK_RAT) then
                call exact_value(a, left, n1, d1, ok)
                call exact_value(a, right, n2, d2, ok)
                call fraction_mul(n1, d1, n2, d2, n, d, ok)
                if (ok) then
                    id = a%rat(n, d)
                    return
                end if
            end if
        end if

        id = 0
        value = exact_mul(chars(a%exact_text_of(left)), &
            chars(a%exact_text_of(right)), ok)
        if (.not. ok) return
        id = a%exact(chars(value), inserted)
        ok = inserted
        if (.not. ok) id = 0
    end function exact_mul_node

    function exact_pow_node(a, base, exponent, ok) result(id)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base
        integer(int64), intent(in)   :: exponent
        logical,       intent(out)   :: ok
        integer                      :: id
        type(str_t) :: value
        logical :: inserted
        integer(int64) :: bn, bd, n, d

        if (a%kind_of(base) == NK_INT .or. a%kind_of(base) == NK_RAT) then
            call exact_value(a, base, bn, bd, ok)
            call fraction_power(bn, bd, exponent, n, d, ok)
            if (ok) then
                id = a%rat(n, d)
                return
            end if
        end if

        id = 0
        value = exact_pow(chars(a%exact_text_of(base)), exponent, ok)
        if (.not. ok) return
        id = a%exact(chars(value), inserted)
        ok = inserted
        if (.not. ok) id = 0
    end function exact_pow_node

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

    function has_symbolic_denominator(a, id) result(found)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: found
        logical, allocatable :: seen(:)

        allocate (seen(a%size()), source=.false.)
        found = .false.
        call find_symbolic_denominator(a, id, seen, found)
    end function has_symbolic_denominator

    recursive subroutine find_symbolic_denominator(a, id, seen, found)
        type(arena_t), intent(in)    :: a
        integer,       intent(in)    :: id
        logical,       intent(inout) :: seen(:), found
        integer(int64) :: exponent, denominator
        integer :: base, k
        logical :: exact

        if (found) return
        if (seen(id)) return
        seen(id) = .true.

        if (a%kind_of(id) == NK_POW) then
            call exact_value(a, a%arg_of(id, 2), exponent, denominator, exact)
            if (exact) then
                if (denominator == 1_int64 .and. exponent < 0_int64) then
                    base = a%arg_of(id, 1)
                    if (.not. definitely_nonzero(a, base)) then
                        found = .true.
                        return
                    end if
                end if
            end if
        end if

        do k = 1, a%nargs_of(id)
            call find_symbolic_denominator(a, a%arg_of(id, k), seen, found)
            if (found) return
        end do
    end subroutine find_symbolic_denominator

    function definitely_nonzero(a, id) result(nonzero)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: nonzero

        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            nonzero = a%num_of(id) /= 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            ! Canonical zero always downcasts to NK_INT.
            nonzero = .true.
        case (NK_REAL)
            nonzero = a%real_of(id) /= 0.0_dp
        case default
            nonzero = .false.
        end select
    end function definitely_nonzero

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
