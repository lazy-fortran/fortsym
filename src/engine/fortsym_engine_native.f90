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
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_SYM, NK_BIG_INT, NK_BIG_RAT, &
        NK_ALGEBRAIC
    use fortsym_expr, only: expr_t, num, algebraic_expr, operator(+), operator(-), &
        operator(*), &
        operator(/), operator(**), operator(==)
    use fortsym_exact, only: exact_add, exact_mul, exact_pow
    use fortsym_algebraic, only: algebraic_i, algebraic_add, algebraic_sub, &
        algebraic_from_re_im, algebraic_mul, algebraic_pow, algebraic_signs
    use fortsym_assume, only: assumption_context_t, FACT_REAL, FACT_ZERO, &
        FACT_NEGATIVE, FACT_NONPOSITIVE, FACT_POSITIVE, FACT_NONNEGATIVE, &
        FACT_NONZERO
    use fortsym_diff, only: diff_expr => diff
    use fortsym_subs, only: subs
    use fortsym_poly, only: poly_together, poly_cancel, poly_factor, &
        poly_numerator, poly_denominator
    use fortsym_trigrewrite, only: trig_to_exp
    use fortsym_engine, only: engine_t, engine_result_t, resource_limit_t, &
        resource_exceeded, resource_visit, resource_failure, wall_seconds, &
        VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE, CAP_ZERO_TEST, &
        CAP_SIMPLIFY, CAP_FACTOR, CAP_DIFF, CAP_EXPAND, CAP_SOLVE, CAP_SERIES
    implicit none
    private

    public :: native_engine_t, make_native_engine

    integer, parameter :: dp = real64
    integer(int64), parameter :: MAX_I64 = huge(0_int64)
    integer(int64), parameter :: MIN_I64 = -huge(0_int64) - 1_int64
    integer(int64), parameter :: MAX_EXPAND_POWER = 32_int64
    integer(int64), parameter :: MAX_EXPAND_TERMS = 100000_int64
    integer(int64), parameter :: MAX_NATIVE_LEGENDRE_DEGREE = 16_int64
    integer, parameter :: DOMAIN_NONE = 0
    integer, parameter :: DOMAIN_OO = 1
    integer, parameter :: DOMAIN_ZOO = 2

    type :: exact_coefficient_t
        logical :: compact = .true.
        logical :: algebraic = .false.
        logical :: zero = .false.
        logical :: one = .false.
        integer(int64) :: numerator = 0_int64
        integer(int64) :: denominator = 1_int64
        integer :: id = 0
        type(str_t) :: algebraic_text
    end type exact_coefficient_t

    type, extends(engine_t) :: native_engine_t
        type(arena_t), pointer :: home => null()
        type(assumption_context_t), pointer :: assumptions => null()
        integer, allocatable :: simplify_cache(:)
        integer, allocatable :: expand_cache(:)
    contains
        procedure :: zero_test => native_zero_test
        procedure :: simplify => native_simplify
        procedure :: factor => native_factor
        procedure :: diff => native_diff
        procedure :: expand => native_expand
        procedure :: series => native_series
        procedure :: series_coeff => native_series_coeff
        procedure :: laurent_series => native_laurent_series
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
        eng%caps = CAP_ZERO_TEST + CAP_SIMPLIFY + CAP_FACTOR + CAP_DIFF + &
            CAP_EXPAND + CAP_SERIES + CAP_SOLVE
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
        logical :: applied
        logical :: cancel_ok
        logical :: algebraic_ok
        character(:), allocatable :: reason
        character(:), allocatable :: cancel_reason
        type(expr_t) :: simplified_expr, cancelled
        type(resource_limit_t) :: active_limit

        started = wall_seconds()
        r%value = e
        if (.not. present(limit)) then
            if (associated(self%assumptions)) then
                if (associated(e%a, self%home)) then
                    call try_contextual_composition(e%a, e%id, self%assumptions, &
                        simplified_id, applied)
                    if (applied) then
                        r%value%id = simplified_id
                        r%ok = .true.
                        r%seconds = wall_seconds() - started
                        return
                    end if
                end if
            end if
        end if
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
        call try_algebraic_value(e%a, simplified_id, algebraic_ok)
        if (algebraic_ok) then
            r%value%id = simplified_id
            r%ok = .true.
            if (.not. associated(self%assumptions)) then
                self%simplify_cache(e%id) = simplified_id
            end if
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
        ! Polynomial cancellation is a native candidate, not a replacement
        ! for the bounded recursive simplifier.  Keep it out of limited calls
        ! until it accepts the caller's resource budget, and only retain a
        ! strictly smaller result.
        if (.not. present(limit)) then
            if (e%kind() == NK_ADD .or. e%kind() == NK_MUL .or. &
                e%kind() == NK_POW) then
                simplified_expr = e
                simplified_expr%id = simplified_id
                call poly_cancel(e%a, simplified_expr, cancelled, cancel_ok, &
                    cancel_reason)
                if (cancel_ok) then
                    deallocate (memo, done)
                    allocate (memo(e%a%size()), source=0)
                    allocate (done(e%a%size()), source=.false.)
                    cancelled%id = simplify_id(e%a, cancelled%id, memo, done, &
                        active_limit)
                    if (cancelled%node_count() < &
                        simplified_expr%node_count()) then
                        simplified_id = cancelled%id
                    end if
                end if
                ! Factoring is currently a polynomial candidate only.  Do not
                ! fold rational functions back into products: callers such as
                ! integration deliberately expose those products to Apart.
                if (.not. has_symbolic_denominator(e%a, simplified_id)) then
                    simplified_expr = e
                    simplified_expr%id = simplified_id
                    call poly_factor(e%a, simplified_expr, cancelled, &
                        cancel_ok, cancel_reason)
                    if (cancel_ok) then
                        deallocate (memo, done)
                        allocate (memo(e%a%size()), source=0)
                        allocate (done(e%a%size()), source=.false.)
                        cancelled%id = simplify_id(e%a, cancelled%id, memo, &
                            done, active_limit)
                        if (cancelled%node_count() < &
                            simplified_expr%node_count()) then
                            simplified_id = cancelled%id
                        end if
                    end if
                end if
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

    function native_factor(self, e, limit) result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        type(expr_t)                          :: factored
        real(dp)                              :: started
        logical                               :: refused, factor_ok
        character(:), allocatable             :: reason, factor_reason
        type(resource_limit_t)                :: active_limit

        started = wall_seconds()
        r%value = e
        call resource_exceeded(e, limit, "factor", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (.not. associated(e%a, self%home)) then
            r%message = str("native: expression belongs to a different arena")
            r%seconds = wall_seconds() - started
            return
        end if

        call poly_factor(e%a, e, factored, factor_ok, factor_reason)
        if (.not. factor_ok) then
            r%message = str(factor_reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (present(limit)) active_limit = limit
        active_limit%visited_nodes = 0_int64
        active_limit%exceeded_kind = 0
        factored%id = simplify_root_id(e%a, factored%id, active_limit)
        if (active_limit%exceeded_kind /= 0) then
            r%message = str(resource_failure("factor", active_limit))
            r%seconds = wall_seconds() - started
            return
        end if
        r%value = factored
        r%ok = .true.
        call set_simplify_condition(e, factored%id, r)
        r%seconds = wall_seconds() - started
    end function native_factor

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
        type(engine_result_t) :: expanded
        type(expr_t) :: normalised, exp_input, trig_normalised, together, cancelled
        type(expr_t) :: renormalised
        type(expr_t) :: numerator, denominator
        logical :: saw_exponential, decidable, formal_exponential
        logical :: branch_sensitive
        logical :: trig_ok, together_ok, cancel_ok
        logical :: saw_exponential_again, decidable_again, formal_exponential_again
        character(:), allocatable :: trig_reason, cancel_reason

        simplified = self%simplify(e)
        r = simplified
        r%verdict = VERDICT_UNKNOWN
        if (.not. simplified%ok) return

        ! The general simplifier deliberately does not rewrite exponentials:
        ! that would make ordinary pretty-printing choose a complex
        ! exponential spelling.  Zero testing has a different contract, so
        ! use a conservative formal exponential fragment here and then run
        ! the ordinary simplifier over the resulting product.
        exp_input = simplified%value
        call trig_to_exp(exp_input, trig_normalised, trig_ok, trig_reason)
        if (trig_ok) then
            if (trig_normalised%id /= exp_input%id) then
                exp_input = trig_normalised
            end if
        end if
        normalised = native_exp_normal_form(exp_input, &
            saw_exponential, decidable, formal_exponential)
        call poly_together(normalised%a, normalised, together, together_ok, &
            cancel_reason)
        if (together_ok) normalised = together
        numerator = poly_numerator(normalised%a, normalised)
        denominator = poly_denominator(normalised%a, normalised)
        expanded = self%expand(numerator)
        if (expanded%ok) then
            normalised = expanded%value / denominator
            renormalised = native_exp_normal_form(normalised, &
                saw_exponential_again, decidable_again, &
                formal_exponential_again)
            normalised = renormalised
            saw_exponential = saw_exponential .or. saw_exponential_again
            decidable = decidable .and. decidable_again
            formal_exponential = formal_exponential .and. &
                formal_exponential_again
            call poly_together(normalised%a, normalised, together, together_ok, &
                cancel_reason)
            if (together_ok) normalised = together
        end if
        call poly_cancel(normalised%a, normalised, cancelled, cancel_ok, &
            cancel_reason)
        if (cancel_ok) normalised = cancelled
        renormalised = native_exp_normal_form(normalised, &
            saw_exponential_again, decidable_again, formal_exponential_again)
        normalised = renormalised
        saw_exponential = saw_exponential .or. saw_exponential_again
        decidable = decidable .and. decidable_again
        formal_exponential = formal_exponential .and. formal_exponential_again
        ! Polynomial folding can expose an ordinary exact zero even when the
        ! input contained no exponential. Re-run the native simplifier for
        ! every normalized result before selecting the verdict.
        r = self%simplify(normalised)
        if (.not. r%ok) return
        branch_sensitive = has_branch_sensitive_power(r%value%a, r%value%id)

        select case (r%value%kind())
        case (NK_INT, NK_RAT)
            if (r%value%int_value() == 0_int64) then
                r%verdict = VERDICT_TRUE
            else
                r%verdict = VERDICT_FALSE
            end if
        case (NK_BIG_INT, NK_BIG_RAT)
            ! Canonical zero is always downcast to NK_INT.
            r%verdict = VERDICT_FALSE
        case (NK_REAL)
            if (r%value%real_value() == 0.0_dp) then
                r%verdict = VERDICT_TRUE
            else
                r%verdict = VERDICT_FALSE
            end if
        case (NK_ALGEBRAIC)
            call algebraic_zero_status(r%value, r%verdict)
        end select
        if (decidable .and. formal_exponential .and. .not. branch_sensitive .and. &
            r%verdict == VERDICT_UNKNOWN) then
            ! Distinct canonical formal exponentials are distinct functions.
            ! The same fragment also includes exact rational expressions; an
            ! unsupported head must remain UNKNOWN.
            r%verdict = VERDICT_FALSE
        end if
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
        type(expr_t) :: expanded, reexpanded
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
            ! Simplification may select a compact factored candidate.  Expand
            ! that result once more so this operation keeps its public
            ! expanded-form contract.
            deallocate (memo, done)
            allocate (memo(e%a%size()), source=0)
            allocate (done(e%a%size()), source=.false.)
            reexpanded = r%value
            reexpanded%id = expand_id(e%a, r%value%id, memo, done, &
                active_limit)
            if (active_limit%exceeded_kind /= 0) then
                r%ok = .false.
                r%message = str(resource_failure("expand", active_limit))
            else
                deallocate (memo, done)
                allocate (memo(e%a%size()), source=0)
                allocate (done(e%a%size()), source=.false.)
                reexpanded%id = simplify_id(e%a, reexpanded%id, memo, done, &
                    active_limit)
                if (active_limit%exceeded_kind /= 0) then
                    r%ok = .false.
                    r%message = str(resource_failure("expand", active_limit))
                    r%value = e
                else
                    r%value = reexpanded
                end if
            end if
        end if
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
        integer :: k
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

    !> Laurent series for an expression with a structurally recognised integer
    !> pole at `point`. The pole is removed first, then the ordinary Taylor
    !> coefficient path is used on the regularised expression. This keeps the
    !> two series implementations on one coefficient oracle and refuses
    !> singular structures whose order cannot be established exactly.
    function native_laurent_series(self, e, v, point, lowest, highest, limit) &
            result(r)
        class(native_engine_t), intent(inout) :: self
        type(expr_t),           intent(in)    :: e, v, point
        integer,                intent(in)    :: lowest, highest
        type(resource_limit_t), intent(in), optional :: limit
        type(engine_result_t)                 :: r
        type(engine_result_t) :: shifted_result, regular_result, coefficient
        type(expr_t) :: shifted, regular, series, term
        integer :: pole, first, last, k
        logical :: found, refused
        real(dp) :: started
        character(:), allocatable :: reason

        started = wall_seconds()
        r%value = e
        call resource_exceeded(e, limit, "laurent series", refused, reason)
        if (refused) then
            r%message = str(reason)
            r%seconds = wall_seconds() - started
            return
        end if
        if (lowest > highest) then
            r%message = str("native: Laurent series bounds are reversed")
            r%seconds = wall_seconds() - started
            return
        end if
        if (lowest >= 0) then
            r = self%series(e, v, point, highest, limit)
            r%seconds = wall_seconds() - started
            return
        end if

        shifted_result = self%simplify(v - point, limit)
        if (shifted_result%ok) then
            shifted = shifted_result%value
        else
            shifted = v - point
        end if
        pole = 0
        found = .false.
        call pole_order(e, shifted, pole, found)
        if (.not. found .or. pole <= 0) then
            r%message = str("native: Laurent series needs a recognised "// &
                "integer power of the shifted variable")
            r%seconds = wall_seconds() - started
            return
        end if

        first = max(0, lowest + pole)
        last = highest + pole
        if (last < first .or. last > 64) then
            r%message = str("native: Laurent series order exceeds the "// &
                "bounded regular-series limit")
            r%seconds = wall_seconds() - started
            return
        end if

        regular_result = self%simplify(e*shifted**num(e%a, pole), limit)
        if (.not. regular_result%ok) then
            r = regular_result
            r%seconds = wall_seconds() - started
            return
        end if
        regular = regular_result%value
        series = num(e%a, 0)
        do k = first, last
            coefficient = self%series_coeff(regular, v, point, k, limit)
            if (.not. coefficient%ok) then
                r = coefficient
                r%seconds = wall_seconds() - started
                return
            end if
            term = coefficient%value*shifted**num(e%a, k - pole)
            series = series + term
        end do
        r = self%simplify(series, limit)
        r%seconds = wall_seconds() - started
    end function native_laurent_series

    recursive subroutine pole_order(e, shifted, order, found)
        type(expr_t), intent(in)    :: e, shifted
        integer,      intent(inout) :: order
        logical,      intent(inout) :: found
        type(expr_t) :: exponent_node, base
        integer(int64) :: exponent, denominator
        logical :: exact
        integer :: k, child_order
        logical :: child_found

        select case (e%kind())
        case (NK_ADD)
            do k = 1, e%nargs()
                child_order = 0
                child_found = .false.
                call pole_order(e%arg(k), shifted, child_order, child_found)
                if (child_found) then
                    order = max(order, child_order)
                    found = .true.
                end if
            end do
        case (NK_MUL)
            do k = 1, e%nargs()
                child_order = 0
                child_found = .false.
                call pole_order(e%arg(k), shifted, child_order, child_found)
                if (child_found) then
                    order = order + child_order
                    found = .true.
                end if
            end do
        case (NK_POW)
            base = e%arg(1)
            exponent_node = e%arg(2)
            if (.not. (base == shifted)) return
            call exact_value(e%a, exponent_node%id, exponent, denominator, exact)
            if (.not. exact .or. denominator /= 1_int64) return
            if (exponent >= 0_int64 .or. exponent == MIN_I64) return
            if (exponent < -64_int64) return
            order = order + int(-exponent)
            found = .true.
        end select
    end subroutine pole_order

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
        if (verified%verdict == VERDICT_UNKNOWN .or. &
            (verified%verdict == VERDICT_FALSE .and. &
            .not. definitely_nonzero(e%a, coefficient%value%id))) then
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

    subroutine try_contextual_composition(a, id, context, out, applied)
        type(arena_t), target, intent(inout) :: a
        integer, intent(in) :: id
        type(assumption_context_t), intent(in) :: context
        integer, intent(out) :: out
        logical, intent(out) :: applied
        integer :: child_id
        character(:), allocatable :: name

        out = id
        applied = .false.
        if (a%kind_of(id) /= NK_FUNC) return
        if (a%nargs_of(id) /= 1) return
        child_id = a%arg_of(id, 1)
        name = chars(a%name_of(id))
        call try_log_exp_composition(a, name, child_id, context, out, applied)
    end subroutine try_contextual_composition

    subroutine try_log_exp_composition(a, name, child_id, context, out, applied)
        type(arena_t), target, intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: child_id
        type(assumption_context_t), intent(in) :: context
        integer, intent(inout) :: out
        logical, intent(out) :: applied
        type(expr_t) :: base_expression
        integer :: composition_base
        integer :: required_fact
        character(:), allocatable :: expected_inner
        character(:), allocatable :: inner_name

        applied = .false.
        select case (name)
        case ("log")
            expected_inner = "exp"
            required_fact = FACT_REAL
        case ("exp")
            expected_inner = "log"
            required_fact = FACT_NONZERO
        case default
            return
        end select
        if (a%kind_of(child_id) /= NK_FUNC) return
        if (a%nargs_of(child_id) /= 1) return
        inner_name = chars(a%name_of(child_id))
        if (inner_name /= expected_inner) return
        composition_base = a%arg_of(child_id, 1)
        base_expression%a => a
        base_expression%id = composition_base
        base_expression%generation = a%generation_value()
        if (.not. context%has(base_expression, required_fact)) return
        out = composition_base
        applied = .true.
    end subroutine try_log_exp_composition

    recursive subroutine try_algebraic_value(a, out, ok)
        type(arena_t), target, intent(inout) :: a
        integer, intent(inout) :: out
        logical, intent(out) :: ok
        type(str_t) :: value
        type(expr_t) :: expression

        ok = .false.
        if (a%kind_of(out) /= NK_ADD .and. a%kind_of(out) /= NK_MUL .and. &
            a%kind_of(out) /= NK_POW) return
        if (.not. contains_algebraic_atom(a, out)) return
        value = algebraic_value(a, out, ok)
        if (.not. ok) return
        expression = algebraic_expr(a, chars(value), ok)
        if (ok) out = expression%id
    end subroutine try_algebraic_value

    recursive function contains_algebraic_atom(a, id) result(found)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical :: found
        integer :: k

        found = .false.
        select case (a%kind_of(id))
        case (NK_ALGEBRAIC)
            found = .true.
        case (NK_CONST)
            found = chars(a%name_of(id)) == "i"
        case (NK_ADD, NK_MUL)
            do k = 1, a%nargs_of(id)
                if (contains_algebraic_atom(a, a%arg_of(id, k))) then
                    found = .true.
                    return
                end if
            end do
        case (NK_POW)
            found = contains_algebraic_atom(a, a%arg_of(id, 1))
        end select
    end function contains_algebraic_atom

    recursive function algebraic_value(a, id, ok) result(value)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(out) :: ok
        type(str_t) :: value
        type(str_t) :: left, right
        integer(int64) :: exponent
        integer :: k

        ok = .true.
        select case (a%kind_of(id))
        case (NK_ALGEBRAIC)
            value = a%algebraic_text_of(id)
        case (NK_CONST)
            if (chars(a%name_of(id)) == "i") then
                value = algebraic_i(ok)
            else
                ok = .false.
            end if
        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT)
            value = algebraic_from_real_text(chars(a%exact_text_of(id)), ok)
        case (NK_ADD, NK_MUL)
            value = str("")
            do k = 1, a%nargs_of(id)
                right = algebraic_value(a, a%arg_of(id, k), ok)
                if (.not. ok) return
                if (k == 1) then
                    value = right
                else
                    left = value
                    if (a%kind_of(id) == NK_ADD) then
                        value = algebraic_add(chars(left), chars(right), ok)
                    else
                        value = algebraic_mul(chars(left), chars(right), ok)
                    end if
                    if (.not. ok) return
                end if
            end do
        case (NK_POW)
            if (a%kind_of(a%arg_of(id, 2)) /= NK_INT) then
                ok = .false.
                return
            end if
            exponent = a%num_of(a%arg_of(id, 2))
            left = algebraic_value(a, a%arg_of(id, 1), ok)
            if (.not. ok) return
            value = algebraic_pow(chars(left), exponent, ok)
        case default
            ok = .false.
        end select
    end function algebraic_value

    function algebraic_from_real_text(text, ok) result(value)
        character(*), intent(in) :: text
        logical, intent(out) :: ok
        type(str_t) :: value

        value = algebraic_from_re_im(text, "0", ok)
    end function algebraic_from_real_text

    subroutine algebraic_zero_status(e, verdict)
        type(expr_t), intent(in) :: e
        integer, intent(out) :: verdict
        integer :: real_sign, imag_sign
        logical :: ok

        call algebraic_signs(chars(e%algebraic_text()), real_sign, imag_sign, ok)
        if (.not. ok) then
            verdict = VERDICT_UNKNOWN
        else if (real_sign == 0 .and. imag_sign == 0) then
            verdict = VERDICT_TRUE
        else
            verdict = VERDICT_FALSE
        end if
    end subroutine algebraic_zero_status

    function native_exp_normal_form(e, saw_exponential, decidable, &
            formal_exponential) result(out)
        type(expr_t), intent(in) :: e
        logical, intent(out)      :: saw_exponential, decidable
        logical, intent(out)      :: formal_exponential
        type(expr_t)              :: out
        integer, allocatable      :: memo(:)
        logical, allocatable      :: done(:)

        saw_exponential = .false.
        decidable = .true.
        formal_exponential = .true.
        allocate (memo(e%a%size()), source=0)
        allocate (done(e%a%size()), source=.false.)
        out = e
        out%id = normalise_exp_id(e%a, e%id, memo, done, &
            saw_exponential, decidable, formal_exponential)
    end function native_exp_normal_form

    recursive function normalise_exp_id(a, id, memo, done, saw_exponential, &
            decidable, formal_exponential) result(out)
        type(arena_t), target, intent(inout) :: a
        integer, intent(in)                  :: id
        integer, intent(inout)                :: memo(:)
        logical, intent(inout)                :: done(:)
        logical, intent(inout)                :: saw_exponential, decidable
        logical, intent(inout)                :: formal_exponential
        integer                                :: out, k, child, base, exponent
        integer(int64)                         :: power, denominator
        logical                                :: exact
        character(:), allocatable              :: name
        integer :: one(1)
        integer(int64) :: pi_multiple, pi_denominator
        integer :: periodic_value
        logical :: pi_multiple_ok, periodic_ok

        if (done(id)) then
            out = memo(id)
            return
        end if

        select case (a%kind_of(id))
        case (NK_ADD)
            out = a%int(0_int64)
            do k = 1, a%nargs_of(id)
                child = normalise_exp_id(a, a%arg_of(id, k), memo, done, &
                    saw_exponential, decidable, formal_exponential)
                out = add_pair(a, out, child)
            end do
        case (NK_MUL)
            out = normalise_exp_product(a, id, memo, done, saw_exponential, &
                decidable, formal_exponential)
        case (NK_POW)
            base = normalise_exp_id(a, a%arg_of(id, 1), memo, done, &
                saw_exponential, decidable, formal_exponential)
            exponent = normalise_exp_id(a, a%arg_of(id, 2), memo, done, &
                saw_exponential, decidable, formal_exponential)
            if (is_exp_node(a, base)) then
                call exact_value(a, exponent, power, denominator, exact)
                if (exact .and. denominator == 1_int64) then
                    saw_exponential = .true.
                    if (power == 0_int64) then
                        out = a%int(1_int64)
                    else
                        child = mul_pair(a, a%arg_of(base, 1), &
                            a%int(power))
                        out = normalise_exp_argument(a, child)
                    end if
                else
                    out = simplify_power(a, base, exponent)
                end if
            else
                out = simplify_power(a, base, exponent)
            end if
        case (NK_FUNC)
            name = chars(a%name_of(id))
            if (name == "exp") then
                saw_exponential = .true.
                if (a%nargs_of(id) /= 1) then
                    decidable = .false.
                    out = id
                else
                    child = normalise_exp_id(a, a%arg_of(id, 1), memo, done, &
                        saw_exponential, decidable, formal_exponential)
                    if (.not. formal_exp_argument(a, child)) &
                        formal_exponential = .false.
                    call rational_i_pi_multiple(a, child, pi_multiple, &
                        pi_denominator, pi_multiple_ok)
                    periodic_ok = .false.
                    if (pi_multiple_ok) then
                        periodic_value = exact_periodic_constant(a, pi_multiple, &
                            pi_denominator, periodic_ok)
                    end if
                    if (periodic_ok) then
                        out = periodic_value
                    else
                        out = normalise_exp_argument(a, child)
                    end if
                end if
            else if (name == "log") then
                if (a%nargs_of(id) /= 1) then
                    decidable = .false.
                    out = id
                else
                    child = normalise_exp_id(a, a%arg_of(id, 1), memo, done, &
                        saw_exponential, decidable, formal_exponential)
                    one(1) = child
                    out = a%func(name, one)
                end if
            else if (name == "sqrt") then
                if (a%nargs_of(id) /= 1) then
                    decidable = .false.
                    out = id
                else
                    child = normalise_exp_id(a, a%arg_of(id, 1), memo, done, &
                        saw_exponential, decidable, formal_exponential)
                    out = simplify_power(a, child, &
                        a%rat(1_int64, 2_int64))
                end if
            else
                ! This normal form is deliberately narrower than the general
                ! native simplifier. Returning UNKNOWN for an unrecognised
                ! function is safer than treating it as an algebraic atom and
                ! claiming that its residual is nonzero.
                decidable = .false.
                out = id
            end if
        case default
            out = id
        end select

        done(id) = .true.
        memo(id) = out
    end function normalise_exp_id

    function normalise_exp_product(a, id, memo, done, saw_exponential, &
            decidable, formal_exponential) result(out)
        type(arena_t), target, intent(inout) :: a
        integer, intent(in)                  :: id
        integer, intent(inout)                :: memo(:)
        logical, intent(inout)                :: done(:)
        logical, intent(inout)                :: saw_exponential, decidable
        logical, intent(inout)                :: formal_exponential
        integer                                :: out, k, child, exponent
        integer                                :: other, exp_sum, exp_factor
        logical                                :: have_exponential

        other = a%int(1_int64)
        have_exponential = .false.
        do k = 1, a%nargs_of(id)
            child = normalise_exp_id(a, a%arg_of(id, k), memo, done, &
                saw_exponential, decidable, formal_exponential)
            if (is_exp_node(a, child)) then
                exponent = a%arg_of(child, 1)
                if (have_exponential) then
                    exp_sum = add_pair(a, exp_sum, exponent)
                else
                    exp_sum = exponent
                    have_exponential = .true.
                end if
            else
                other = mul_pair(a, other, child)
            end if
        end do

        if (have_exponential) then
            exp_factor = normalise_exp_argument(a, exp_sum)
            out = mul_pair(a, other, exp_factor)
        else
            out = other
        end if
    end function normalise_exp_product

    function normalise_exp_argument(a, argument) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in)           :: argument
        integer                       :: out, k, factor

        out = a%int(1_int64)
        if (a%kind_of(argument) == NK_ADD) then
            do k = 1, a%nargs_of(argument)
                factor = make_exp(a, a%arg_of(argument, k))
                out = mul_pair(a, out, factor)
            end do
        else
            out = make_exp(a, argument)
        end if
    end function normalise_exp_argument

    function make_exp(a, argument) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in)           :: argument
        integer                       :: out
        integer                       :: one(1)
        logical :: log_power_ok

        call exp_log_power(a, argument, out, log_power_ok)
        if (log_power_ok) return
        one(1) = argument
        out = simplify_function(a, "exp", one)
    end function make_exp

    !> Rewrite exp(c*log(x)) to x**c for an exact rational c. The operation is
    !> used only by the exponential zero-test normal form; symbolic cofactors
    !> stay untouched and therefore remain UNKNOWN there.
    subroutine exp_log_power(a, argument, out, ok)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)     :: argument
        integer,       intent(out)   :: out
        logical,       intent(out)   :: ok
        integer :: k, factor, log_id
        integer(int64) :: coefficient_numerator, coefficient_denominator
        integer(int64) :: numerator, denominator
        integer(int64) :: product_numerator, product_denominator
        logical :: exact, product_ok

        out = argument
        ok = .false.
        log_id = 0
        coefficient_numerator = 1_int64
        coefficient_denominator = 1_int64

        if (a%kind_of(argument) == NK_FUNC) then
            if (a%nargs_of(argument) == 1) then
                if (chars(a%name_of(argument)) == "log") then
                    out = a%arg_of(argument, 1)
                    ok = .true.
                end if
            end if
            return
        end if
        if (a%kind_of(argument) /= NK_MUL) return

        do k = 1, a%nargs_of(argument)
            factor = a%arg_of(argument, k)
            if (a%kind_of(factor) == NK_FUNC) then
                if (a%nargs_of(factor) == 1) then
                    if (chars(a%name_of(factor)) == "log") then
                        if (log_id /= 0) return
                        log_id = factor
                        cycle
                    end if
                end if
            end if
            call exact_value(a, factor, numerator, denominator, exact)
            if (.not. exact) return
            call fraction_mul(coefficient_numerator, coefficient_denominator, &
                numerator, denominator, product_numerator, product_denominator, &
                product_ok)
            if (.not. product_ok) return
            coefficient_numerator = product_numerator
            coefficient_denominator = product_denominator
        end do
        if (log_id == 0) return
        if (coefficient_denominator == 1_int64) then
            out = simplify_power(a, a%arg_of(log_id, 1), &
                a%int(coefficient_numerator))
        else
            out = simplify_power(a, a%arg_of(log_id, 1), &
                a%rat(coefficient_numerator, coefficient_denominator))
        end if
        ok = .true.
    end subroutine exp_log_power

    function add_pair(a, left, right) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in)           :: left, right
        integer                       :: out
        integer                       :: pair(2)

        pair(1) = left
        pair(2) = right
        out = simplify_add(a, pair)
    end function add_pair

    function mul_pair(a, left, right) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in)           :: left, right
        integer                       :: out
        integer                       :: pair(2)

        pair(1) = left
        pair(2) = right
        out = simplify_mul(a, pair)
    end function mul_pair

    function is_exp_node(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in)       :: id
        logical                   :: yes

        yes = .false.
        if (a%kind_of(id) /= NK_FUNC) return
        if (a%nargs_of(id) /= 1) return
        yes = chars(a%name_of(id)) == "exp"
    end function is_exp_node

    recursive function formal_exp_argument(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in)       :: id
        logical                   :: yes
        integer                   :: k

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT, NK_SYM)
            yes = .true.
        case (NK_ADD, NK_MUL)
            yes = .true.
            do k = 1, a%nargs_of(id)
                if (.not. formal_exp_argument(a, a%arg_of(id, k))) then
                    yes = .false.
                    return
                end if
            end do
        end select
    end function formal_exp_argument

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
        logical :: square, definitely_zero, definitely_negative
        logical :: definitely_nonpositive, definitely_positive
        logical :: definitely_nonnegative, real_valued
        integer :: abs_argument(1)
        integer :: negative_pair(2)
        logical :: refused
        logical :: applied
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
                    definitely_zero = context%has(base_expression, FACT_ZERO)
                    definitely_negative = context%has(base_expression, FACT_NEGATIVE)
                    definitely_nonpositive = context%has(base_expression, &
                        FACT_NONPOSITIVE)
                    definitely_positive = context%has(base_expression, FACT_POSITIVE)
                    definitely_nonnegative = context%has(base_expression, &
                        FACT_NONNEGATIVE)
                    if (definitely_zero) then
                        out = a%int(0_int64)
                    else if (definitely_negative .or. definitely_nonpositive) then
                        negative_pair(1) = a%int(-1_int64)
                        negative_pair(2) = children(1)
                        out = simplify_mul(a, negative_pair)
                    else if (definitely_positive .or. definitely_nonnegative) then
                        out = children(1)
                    end if
                end if
                call try_log_exp_composition(a, chars(a%name_of(id)), children(1), &
                    context, out, applied)
                if (chars(a%name_of(id)) == "sqrt") then
                    if (a%kind_of(children(1)) == NK_POW) then
                        call is_square_power(a, children(1), base_id, square)
                        if (square) then
                            base_expression%id = base_id
                            definitely_zero = context%has(base_expression, FACT_ZERO)
                            definitely_negative = context%has(base_expression, &
                                FACT_NEGATIVE)
                            definitely_nonpositive = context%has(base_expression, &
                                FACT_NONPOSITIVE)
                            definitely_positive = context%has(base_expression, &
                                FACT_POSITIVE)
                            definitely_nonnegative = context%has(base_expression, &
                                FACT_NONNEGATIVE)
                            real_valued = context%has(base_expression, FACT_REAL)
                            if (definitely_zero) then
                                out = a%int(0_int64)
                            else if (definitely_positive .or. definitely_nonnegative) then
                                out = base_id
                            else if (definitely_negative .or. definitely_nonpositive) then
                                negative_pair(1) = a%int(-1_int64)
                                negative_pair(2) = base_id
                                out = simplify_mul(a, negative_pair)
                            else if (real_valued) then
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

        if (has_nan_operand(a, operands)) then
            out = nan_node(a)
            return
        end if
        call simplify_domain_add(a, operands, out, exact)
        if (exact) return

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

        if (has_nan_operand(a, operands)) then
            out = nan_node(a)
            return
        end if
        call simplify_domain_mul(a, operands, out, product_ok)
        if (product_ok) return

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
        integer :: k, normalized_base, pair(2)
        logical :: exact, power_ok, nested_ok

        if (is_nan_id(a, exponent_id)) then
            out = nan_node(a)
            return
        end if

        call exact_value(a, exponent_id, exponent, den, exact)
        if (.not. exact) then
            if (is_nan_id(a, base)) then
                out = nan_node(a)
            else
                out = a%pow(base, exponent_id)
            end if
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
        if (is_nan_id(a, base)) then
            out = nan_node(a)
            return
        end if
        call simplify_domain_power(a, base, exponent, out, power_ok)
        if (power_ok) return
        if (exponent == 1_int64) then
            out = base
            return
        end if
        if (exponent == 2_int64 .and. a%kind_of(base) == NK_FUNC) then
            if (chars(a%name_of(base)) == "sqrt") then
                if (a%nargs_of(base) == 1) then
                    out = a%arg_of(base, 1)
                    return
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

        if (a%kind_of(base) == NK_CONST) then
            if (chars(a%name_of(base)) == "i") then
                select case (modulo(exponent, 4_int64))
                case (0_int64)
                    out = a%int(1_int64)
                case (1_int64)
                    out = base
                case (2_int64)
                    out = a%int(-1_int64)
                case default
                    pair(1) = a%int(-1_int64)
                    pair(2) = base
                    out = simplify_mul(a, pair)
                end select
                return
            end if
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
        integer(int64) :: periodic_numerator, periodic_denominator
        integer(int64) :: factorial
        logical :: exact
        logical :: factorial_ok
        logical :: trig_constant_ok
        logical :: periodic_ok
        logical :: negated_argument, odd_head, even_head
        integer :: trig_constant
        integer :: periodic_constant
        integer :: positive_argument
        integer :: bessel_args(2), pair(2), one_arg(1)
        logical :: domain_applied

        if (nan_propagates_function(name, a, args)) then
            out = nan_node(a)
            return
        end if
        call simplify_domain_function(a, name, args, out, domain_applied)
        if (domain_applied) return

        out = a%func(name, args)
        if (size(args) == 0) return

        call exact_inverse_value(a, name, args(1), trig_constant, &
            trig_constant_ok)
        if (trig_constant_ok) then
            out = trig_constant
            return
        end if

        call split_negated_argument(a, args(1), positive_argument, &
            negated_argument)
        odd_head = .false.
        even_head = .false.
        select case (name)
        case ("sin", "tan", "sinh", "tanh", "csc", "cot", "csch", &
                "coth", "asin", "atan", "asinh", "atanh")
            odd_head = .true.
        case ("cos", "cosh", "sec", "sech", "abs")
            even_head = .true.
        end select
        if (negated_argument .and. (odd_head .or. even_head)) then
            one_arg(1) = positive_argument
            if (odd_head) then
                pair(1) = a%int(-1_int64)
                pair(2) = a%func(name, one_arg)
                out = simplify_mul(a, pair)
            else
                out = a%func(name, one_arg)
            end if
            return
        end if

        select case (name)
        case ("Piecewise")
            out = simplify_piecewise(a, args)
        case ("If")
            call simplify_if(a, args, out)
        case ("Boole")
            call simplify_boole(a, args, out)
        case ("Less", "LessEqual", "Greater", "GreaterEqual", "Equal", &
                "Unequal")
            call simplify_condition(a, name, args, out)
        case ("sin", "tan", "sinh", "tanh", "asin", "atan", "asinh")
            if (is_zero_id(a, args(1))) out = a%int(0_int64)
            if (name == "sin" .or. name == "tan") then
                call exact_trig_value(a, name, args(1), trig_constant, &
                    trig_constant_ok)
                if (trig_constant_ok) out = trig_constant
            end if
        case ("cos", "cosh", "exp")
            if (is_zero_id(a, args(1))) out = a%int(1_int64)
            if (name == "cos") then
                call exact_trig_value(a, name, args(1), trig_constant, &
                    trig_constant_ok)
                if (trig_constant_ok) out = trig_constant
            else if (name == "exp") then
                call exact_value(a, args(1), order, den, exact)
                if (exact) then
                    out = simplify_power(a, a%const("e"), args(1))
                else
                    call rational_i_pi_multiple(a, args(1), &
                        periodic_numerator, periodic_denominator, periodic_ok)
                    if (periodic_ok) then
                        periodic_constant = exact_periodic_constant(a, &
                            periodic_numerator, periodic_denominator, &
                            periodic_ok)
                        if (periodic_ok) out = periodic_constant
                    end if
                end if
            end if
        case ("log")
            call exact_log_value(a, args(1), trig_constant, trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("log10")
            call exact_log10_value(a, args(1), trig_constant, trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("min", "max", "Min", "Max")
            call exact_extremum_value(a, name, args, trig_constant, &
                trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("atan2")
            call exact_atan2_value(a, args, trig_constant, trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("sign")
            call exact_sign_value(a, args(1), trig_constant, trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("floor", "ceiling")
            call exact_rounding_value(a, name, args(1), trig_constant, &
                trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("csc", "sec", "cot")
            call exact_reciprocal_trig_value(a, name, args(1), trig_constant, &
                trig_constant_ok)
            if (trig_constant_ok) out = trig_constant
        case ("sech")
            if (is_zero_id(a, args(1))) out = a%int(1_int64)
        case ("sqrt", "abs")
            if (is_zero_id(a, args(1))) out = a%int(0_int64)
            if (is_one_id(a, args(1))) out = a%int(1_int64)
            if (name == "sqrt") then
                call exact_square_root(a, args(1), trig_constant, &
                    trig_constant_ok)
                if (trig_constant_ok) out = trig_constant
            else
                call exact_absolute_value(a, args(1), trig_constant, &
                    trig_constant_ok)
                if (trig_constant_ok) out = trig_constant
                if (a%kind_of(args(1)) == NK_FUNC) then
                    if (a%nargs_of(args(1)) == 1) then
                        if (chars(a%name_of(args(1))) == "abs") then
                            out = args(1)
                        end if
                    end if
                end if
            end if
        case ("erf", "erfc")
            if (is_zero_id(a, args(1))) then
                if (name == "erf") then
                    out = a%int(0_int64)
                else
                    out = a%int(1_int64)
                end if
            end if
        case ("gamma")
            call exact_value(a, args(1), order, den, exact)
            if (exact) then
                if (den == 1_int64 .and. order >= 1_int64 .and. &
                    order <= 21_int64) then
                    call factorial_i64(int(order - 1_int64), factorial, &
                        factorial_ok)
                    if (factorial_ok) out = a%int(factorial)
                else if (order == 1_int64 .and. den == 2_int64) then
                    one_arg(1) = a%const("pi")
                    out = a%func("sqrt", one_arg)
                end if
            end if
        case ("loggamma")
            call exact_loggamma_value(a, args(1), out, exact)
        case ("legendrep")
            call exact_legendre_value(a, args, out, exact)
        case ("besselj")
            if (size(args) < 2) return
            call exact_value(a, args(1), order, den, exact)
            if (.not. exact) return
            if (den /= 1_int64) return
            if (order >= 0_int64 .and. is_zero_id(a, args(2))) then
                if (order == 0_int64) then
                    out = a%int(1_int64)
                else
                    out = a%int(0_int64)
                end if
                return
            end if
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
        case ("besseli")
            if (size(args) < 2) return
            call exact_value(a, args(1), order, den, exact)
            if (.not. exact .or. den /= 1_int64 .or. order < 0_int64) return
            if (is_zero_id(a, args(2))) then
                if (order == 0_int64) then
                    out = a%int(1_int64)
                else
                    out = a%int(0_int64)
                end if
            end if
        end select
    end function simplify_function

    function simplify_piecewise(a, args) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: args(:)
        integer :: out
        integer, allocatable :: kept(:), output(:)
        integer :: branches_id, pair_id, k, nkept, default_id
        logical :: decided, truth, have_unknown

        out = a%func("Piecewise", args)
        if (size(args) < 1) return
        branches_id = args(1)
        if (a%kind_of(branches_id) /= NK_FUNC) return
        if (chars(a%name_of(branches_id)) /= "List") return

        allocate (kept(a%nargs_of(branches_id)))
        nkept = 0
        have_unknown = .false.
        do k = 1, a%nargs_of(branches_id)
            pair_id = a%arg_of(branches_id, k)
            if (a%kind_of(pair_id) /= NK_FUNC) return
            if (chars(a%name_of(pair_id)) /= "List") return
            if (a%nargs_of(pair_id) /= 2) return
            call simplify_condition_value(a, a%arg_of(pair_id, 2), decided, truth)
            if (decided) then
                if (truth) then
                    if (.not. have_unknown) then
                        out = a%arg_of(pair_id, 1)
                        return
                    end if
                    nkept = nkept + 1
                    kept(nkept) = pair_id
                    exit
                end if
                cycle
            end if
            have_unknown = .true.
            nkept = nkept + 1
            kept(nkept) = pair_id
        end do

        if (.not. have_unknown) then
            if (size(args) >= 2) then
                out = args(2)
            else
                out = a%int(0_int64)
            end if
            return
        end if

        if (size(args) >= 2) then
            default_id = args(2)
        else
            default_id = a%int(0_int64)
        end if
        allocate (output(nkept + 1))
        if (nkept > 0) output(1:nkept) = kept(1:nkept)
        output(nkept + 1) = default_id
        out = a%func("Piecewise", output)
    end function simplify_piecewise

    subroutine simplify_if(a, args, out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: args(:)
        integer, intent(out) :: out
        logical :: decided, truth

        out = a%func("If", args)
        if (size(args) /= 3) return
        call simplify_condition_value(a, args(1), decided, truth)
        if (.not. decided) return
        if (truth) then
            out = args(2)
        else
            out = args(3)
        end if
    end subroutine simplify_if

    subroutine simplify_boole(a, args, out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: args(:)
        integer, intent(out) :: out
        logical :: decided, truth

        out = a%func("Boole", args)
        if (size(args) /= 1) return
        call simplify_condition_value(a, args(1), decided, truth)
        if (.not. decided) return
        if (truth) then
            out = a%int(1_int64)
        else
            out = a%int(0_int64)
        end if
    end subroutine simplify_boole

    subroutine simplify_condition(a, name, args, out)
        type(arena_t), intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: args(:)
        integer, intent(out) :: out
        logical :: decided, truth

        out = a%func(name, args)
        if (size(args) /= 2) return
        call simplify_condition_value(a, a%func(name, args), decided, truth)
        if (.not. decided) return
        if (truth) then
            out = a%sym("True")
        else
            out = a%sym("False")
        end if
    end subroutine simplify_condition

    subroutine simplify_condition_value(a, id, decided, truth)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(out) :: decided, truth
        character(:), allocatable :: name
        integer(int64) :: n1, d1, n2, d2, left, right
        logical :: exact1, exact2, product_ok

        decided = .false.
        truth = .false.
        if (a%kind_of(id) == NK_SYM) then
            name = chars(a%name_of(id))
            if (name == "True") then
                decided = .true.
                truth = .true.
            else if (name == "False") then
                decided = .true.
            end if
            return
        end if
        if (a%kind_of(id) /= NK_FUNC) return
        name = chars(a%name_of(id))
        if (a%nargs_of(id) /= 2) return
        call exact_value(a, a%arg_of(id, 1), n1, d1, exact1)
        if (.not. exact1) return
        call exact_value(a, a%arg_of(id, 2), n2, d2, exact2)
        if (.not. exact2) return
        call checked_mul(n1, d2, left, product_ok)
        if (.not. product_ok) return
        call checked_mul(n2, d1, right, product_ok)
        if (.not. product_ok) return
        select case (name)
        case ("Less")
            truth = left < right
        case ("LessEqual")
            truth = left <= right
        case ("Greater")
            truth = left > right
        case ("GreaterEqual")
            truth = left >= right
        case ("Equal")
            truth = left == right
        case ("Unequal")
            truth = left /= right
        case default
            return
        end select
        decided = .true.
    end subroutine simplify_condition_value

    subroutine exact_trig_value(a, name, id, out, ok)
        type(arena_t), intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: numerator, denominator
        integer :: sine, cosine
        logical :: parts_ok

        out = id
        ok = .false.
        if (name /= "sin" .and. name /= "cos" .and. name /= "tan") return
        call rational_pi_multiple(a, id, numerator, denominator, parts_ok)
        if (.not. parts_ok) return
        call exact_sine_cosine(a, numerator, denominator, sine, cosine, &
            parts_ok)
        if (.not. parts_ok) return

        select case (name)
        case ("sin")
            out = sine
        case ("cos")
            out = cosine
        case ("tan")
            if (is_zero_id(a, cosine)) return
            out = mul_pair(a, sine, simplify_power(a, cosine, a%int(-1_int64)))
        end select
        ok = .true.
    end subroutine exact_trig_value

    subroutine exact_reciprocal_trig_value(a, name, id, out, ok)
        type(arena_t), intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer :: sine, cosine
        logical :: sine_ok, cosine_ok

        out = id
        ok = .false.
        call exact_trig_value(a, "sin", id, sine, sine_ok)
        call exact_trig_value(a, "cos", id, cosine, cosine_ok)
        if (.not. sine_ok .or. .not. cosine_ok) return

        select case (name)
        case ("csc")
            if (is_zero_id(a, sine)) return
            out = simplify_power(a, sine, a%int(-1_int64))
        case ("sec")
            if (is_zero_id(a, cosine)) return
            out = simplify_power(a, cosine, a%int(-1_int64))
        case ("cot")
            if (is_zero_id(a, sine)) return
            if (sine == cosine) then
                out = a%int(1_int64)
            else
                out = mul_pair(a, cosine, &
                    simplify_power(a, sine, a%int(-1_int64)))
            end if
        case default
            return
        end select
        ok = .true.
    end subroutine exact_reciprocal_trig_value

    subroutine exact_inverse_value(a, name, id, out, ok)
        type(arena_t), intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer :: half_pi, quarter_pi

        out = id
        ok = .false.
        half_pi = mul_pair(a, a%rat(1_int64, 2_int64), a%const("pi"))
        quarter_pi = mul_pair(a, a%rat(1_int64, 4_int64), a%const("pi"))

        select case (name)
        case ("asin")
            if (is_zero_id(a, id)) then
                out = a%int(0_int64)
            else if (is_one_id(a, id)) then
                out = half_pi
            else if (is_minus_one_id(a, id)) then
                out = mul_pair(a, a%int(-1_int64), half_pi)
            else
                return
            end if
        case ("acos")
            if (is_one_id(a, id)) then
                out = a%int(0_int64)
            else if (is_zero_id(a, id)) then
                out = half_pi
            else if (is_minus_one_id(a, id)) then
                out = a%const("pi")
            else
                return
            end if
        case ("atan")
            if (is_zero_id(a, id)) then
                out = a%int(0_int64)
            else if (is_one_id(a, id)) then
                out = quarter_pi
            else if (is_minus_one_id(a, id)) then
                out = mul_pair(a, a%int(-1_int64), quarter_pi)
            else
                return
            end if
        case ("asinh")
            if (.not. is_zero_id(a, id)) return
            out = a%int(0_int64)
        case ("acosh")
            if (.not. is_one_id(a, id)) return
            out = a%int(0_int64)
        case ("atanh")
            if (.not. is_zero_id(a, id)) return
            out = a%int(0_int64)
        case default
            return
        end select
        ok = .true.
    end subroutine exact_inverse_value

    subroutine exact_log_value(a, id, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer :: half_pi, positive
        logical :: negated
        integer(int64) :: numerator, denominator
        logical :: exact

        out = id
        ok = .false.
        if (is_one_id(a, id)) then
            out = a%int(0_int64)
            ok = .true.
            return
        end if
        if (a%kind_of(id) == NK_CONST) then
            if (chars(a%name_of(id)) == "e") then
                out = a%int(1_int64)
                ok = .true.
                return
            else if (chars(a%name_of(id)) == "i") then
                half_pi = mul_pair(a, a%rat(1_int64, 2_int64), &
                    a%const("pi"))
                out = mul_pair(a, a%const("i"), half_pi)
                ok = .true.
                return
            end if
        end if
        if (a%kind_of(id) == NK_POW) then
            if (a%nargs_of(id) == 2) then
                if (a%kind_of(a%arg_of(id, 1)) == NK_CONST) then
                    if (chars(a%name_of(a%arg_of(id, 1))) == "e") then
                        call exact_value(a, a%arg_of(id, 2), numerator, &
                            denominator, exact)
                        if (exact) then
                            out = a%arg_of(id, 2)
                            ok = .true.
                            return
                        end if
                    end if
                end if
            end if
        end if
        call exact_value(a, id, numerator, denominator, exact)
        if (exact .and. numerator == -1_int64 .and. denominator == 1_int64) then
            out = mul_pair(a, a%const("i"), a%const("pi"))
            ok = .true.
            return
        end if
        call split_negated_argument(a, id, positive, negated)
        if (negated) then
            if (a%kind_of(positive) == NK_CONST) then
                if (chars(a%name_of(positive)) == "i") then
                    half_pi = mul_pair(a, a%rat(1_int64, 2_int64), &
                        a%const("pi"))
                    out = mul_pair(a, a%int(-1_int64), &
                        mul_pair(a, a%const("i"), half_pi))
                    ok = .true.
                end if
            end if
        end if
    end subroutine exact_log_value

    subroutine exact_log10_value(a, id, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: numerator, denominator
        integer(int64) :: numerator_power, denominator_power
        logical :: exact, numerator_ok, denominator_ok
        integer :: one_arg(1)

        one_arg(1) = id
        out = a%func("log10", one_arg)
        ok = .false.
        call exact_value(a, id, numerator, denominator, exact)
        if (.not. exact) return
        call power_of_ten_exponent(numerator, numerator_power, numerator_ok)
        if (.not. numerator_ok) return
        call power_of_ten_exponent(denominator, denominator_power, &
            denominator_ok)
        if (.not. denominator_ok) return
        out = a%int(numerator_power - denominator_power)
        ok = .true.
    end subroutine exact_log10_value

    subroutine power_of_ten_exponent(value, exponent, ok)
        integer(int64), intent(in) :: value
        integer(int64), intent(out) :: exponent
        logical, intent(out) :: ok
        integer(int64) :: remaining

        remaining = value
        exponent = 0_int64
        ok = .false.
        if (remaining < 1_int64) return
        do while (remaining > 1_int64)
            if (modulo(remaining, 10_int64) /= 0_int64) return
            remaining = remaining/10_int64
            exponent = exponent + 1_int64
        end do
        ok = .true.
    end subroutine power_of_ten_exponent

    subroutine exact_extremum_value(a, name, args, out, ok)
        type(arena_t), intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: args(:)
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer :: best, k
        integer(int64) :: best_numerator, best_denominator
        integer(int64) :: numerator, denominator, left, right
        logical :: best_exact, exact, compare_ok, is_minimum

        out = a%func(name, args)
        ok = .false.
        if (size(args) < 2) return
        call exact_value(a, args(1), best_numerator, best_denominator, &
            best_exact)
        if (.not. best_exact) return
        best = args(1)
        is_minimum = name == "min" .or. name == "Min"
        do k = 2, size(args)
            call exact_value(a, args(k), numerator, denominator, exact)
            if (.not. exact) return
            call checked_mul(best_numerator, denominator, left, compare_ok)
            if (.not. compare_ok) return
            call checked_mul(numerator, best_denominator, right, compare_ok)
            if (.not. compare_ok) return
            if (is_minimum) then
                if (right < left) then
                    best = args(k)
                    best_numerator = numerator
                    best_denominator = denominator
                end if
            else
                if (right > left) then
                    best = args(k)
                    best_numerator = numerator
                    best_denominator = denominator
                end if
            end if
        end do
        out = best
        ok = .true.
    end subroutine exact_extremum_value

    subroutine exact_legendre_value(a, args, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in)           :: args(:)
        integer, intent(out)          :: out
        logical, intent(out)          :: ok
        integer(int64) :: degree, order, den
        integer(int64) :: denominator, next_denominator
        integer(int64) :: left_choice, right_choice, numerator
        integer(int64) :: product
        integer :: m, power
        integer :: coefficient, power_id, term, result
        logical :: exact, coefficient_ok

        out = a%func("legendrep", args)
        ok = .false.
        if (size(args) /= 3) return

        call exact_value(a, args(1), degree, den, exact)
        if (.not. exact .or. den /= 1_int64) return
        if (degree < 0_int64 .or. degree > MAX_NATIVE_LEGENDRE_DEGREE) return
        call exact_value(a, args(2), order, den, exact)
        if (.not. exact .or. den /= 1_int64 .or. order /= 0_int64) return

        if (degree == 0_int64) then
            out = a%int(1_int64)
            ok = .true.
            return
        end if

        denominator = 1_int64
        do m = 1, int(degree)
            call checked_mul(denominator, 2_int64, next_denominator, &
                coefficient_ok)
            if (.not. coefficient_ok) return
            denominator = next_denominator
        end do

        result = a%int(0_int64)
        do m = 0, int(degree/2_int64)
            call binomial_nonnegative(degree, int(m, int64), left_choice, &
                coefficient_ok)
            if (.not. coefficient_ok) return
            call binomial_nonnegative(2_int64*degree - 2_int64*int(m, int64), &
                degree, right_choice, coefficient_ok)
            if (.not. coefficient_ok) return
            call checked_mul(left_choice, right_choice, product, coefficient_ok)
            if (.not. coefficient_ok) return
            if (mod(m, 2) == 1) product = -product
            numerator = product
            coefficient = a%rat(numerator, denominator)
            power = int(degree - 2_int64*int(m, int64))
            if (power == 0) then
                power_id = a%int(1_int64)
            else
                power_id = simplify_power(a, args(3), a%int(int(power, int64)))
            end if
            term = mul_pair(a, coefficient, power_id)
            if (m == 0) then
                result = term
            else
                result = add_pair(a, result, term)
            end if
        end do
        out = result
        ok = .true.
    end subroutine exact_legendre_value

    subroutine exact_loggamma_value(a, id, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: order, denominator
        integer(int64) :: factorial_value, factorial_n
        integer(int64) :: four_power, next_value
        integer(int64) :: coefficient_denominator
        logical :: exact, factorial_ok, product_ok
        integer :: n, k, one_arg(1)
        integer :: coefficient, square_root_pi, gamma_value

        one_arg(1) = id
        out = a%func("loggamma", one_arg)
        ok = .false.
        call exact_value(a, id, order, denominator, exact)
        if (.not. exact) return

        if (denominator == 1_int64) then
            if (order < 1_int64 .or. order > 21_int64) return
            call factorial_i64(int(order - 1_int64), factorial_value, &
                factorial_ok)
            if (.not. factorial_ok) return
            one_arg(1) = a%int(factorial_value)
            call exact_log_value(a, one_arg(1), out, ok)
            if (.not. ok) out = a%func("log", one_arg)
            ok = .true.
            return
        end if

        if (denominator /= 2_int64) return
        if (order < 1_int64 .or. order > 21_int64) return
        if (modulo(order, 2_int64) /= 1_int64) return
        n = int((order - 1_int64)/2_int64)
        call factorial_i64(2*n, factorial_value, factorial_ok)
        if (.not. factorial_ok) return
        call factorial_i64(n, factorial_n, factorial_ok)
        if (.not. factorial_ok) return
        four_power = 1_int64
        do k = 1, n
            call checked_mul(four_power, 4_int64, next_value, product_ok)
            if (.not. product_ok) return
            four_power = next_value
        end do
        call checked_mul(four_power, factorial_n, coefficient_denominator, &
            product_ok)
        if (.not. product_ok) return
        coefficient = a%rat(factorial_value, coefficient_denominator)
        one_arg(1) = a%const("pi")
        square_root_pi = a%func("sqrt", one_arg)
        gamma_value = mul_pair(a, coefficient, square_root_pi)
        one_arg(1) = gamma_value
        out = a%func("log", one_arg)
        ok = .true.
    end subroutine exact_loggamma_value

    subroutine exact_atan2_value(a, args, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: args(:)
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: y_numerator, y_denominator
        integer(int64) :: x_numerator, x_denominator
        logical :: y_exact, x_exact
        integer :: half_pi, quarter_pi, three_quarter_pi

        out = a%func("atan2", args)
        ok = .false.
        if (size(args) /= 2) return
        call exact_value(a, args(1), y_numerator, y_denominator, y_exact)
        if (.not. y_exact) return
        call exact_value(a, args(2), x_numerator, x_denominator, x_exact)
        if (.not. x_exact) return

        half_pi = mul_pair(a, a%rat(1_int64, 2_int64), a%const("pi"))
        quarter_pi = mul_pair(a, a%rat(1_int64, 4_int64), a%const("pi"))
        three_quarter_pi = mul_pair(a, a%rat(3_int64, 4_int64), &
            a%const("pi"))

        if (y_numerator == 0_int64) then
            if (x_numerator > 0_int64) then
                out = a%int(0_int64)
                ok = .true.
            else if (x_numerator < 0_int64) then
                out = a%const("pi")
                ok = .true.
            end if
            return
        end if
        if (x_numerator == 0_int64) then
            if (y_numerator > 0_int64) then
                out = half_pi
                ok = .true.
            else if (y_numerator < 0_int64) then
                out = mul_pair(a, a%rat(-1_int64, 2_int64), &
                    a%const("pi"))
                ok = .true.
            end if
            return
        end if

        if (y_denominator /= x_denominator) return
        if (y_numerator == x_numerator) then
            if (x_numerator > 0_int64) then
                out = quarter_pi
            else
                out = mul_pair(a, a%rat(-3_int64, 4_int64), &
                    a%const("pi"))
            end if
            ok = .true.
            return
        end if
        if (x_numerator == MIN_I64) return
        if (y_numerator == -x_numerator) then
            if (x_numerator > 0_int64) then
                out = mul_pair(a, a%rat(-1_int64, 4_int64), &
                    a%const("pi"))
            else
                out = three_quarter_pi
            end if
            ok = .true.
        end if
    end subroutine exact_atan2_value

    subroutine exact_square_root(a, id, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: numerator, denominator, root_numerator, root_denominator
        logical :: exact, numerator_ok, denominator_ok

        out = id
        ok = .false.
        call exact_value(a, id, numerator, denominator, exact)
        if (.not. exact .or. numerator < 0_int64) return
        call integer_square_root(numerator, root_numerator, numerator_ok)
        call integer_square_root(denominator, root_denominator, denominator_ok)
        if (.not. numerator_ok .or. .not. denominator_ok) return
        if (root_denominator == 1_int64) then
            out = a%int(root_numerator)
        else
            out = a%rat(root_numerator, root_denominator)
        end if
        ok = .true.
    end subroutine exact_square_root

    subroutine exact_absolute_value(a, id, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: numerator, denominator
        logical :: exact

        out = id
        ok = .false.
        call exact_value(a, id, numerator, denominator, exact)
        if (.not. exact) return
        if (numerator < 0_int64) then
            if (numerator == MIN_I64) return
            numerator = -numerator
        end if
        if (denominator == 1_int64) then
            out = a%int(numerator)
        else
            out = a%rat(numerator, denominator)
        end if
        ok = .true.
    end subroutine exact_absolute_value

    subroutine split_negated_argument(a, id, positive, ok)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer, intent(out) :: positive
        logical, intent(out) :: ok
        integer :: first, second

        positive = id
        ok = .false.
        if (a%kind_of(id) /= NK_MUL) return
        if (a%nargs_of(id) /= 2) return
        first = a%arg_of(id, 1)
        second = a%arg_of(id, 2)
        if (is_minus_one_id(a, first)) then
            positive = second
        else if (is_minus_one_id(a, second)) then
            positive = first
        else
            return
        end if
        ok = .true.
    end subroutine split_negated_argument

    function is_minus_one_id(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical :: yes
        integer(int64) :: numerator, denominator
        logical :: exact

        yes = .false.
        call exact_value(a, id, numerator, denominator, exact)
        if (exact) yes = numerator == -1_int64 .and. denominator == 1_int64
    end function is_minus_one_id

    subroutine exact_sign_value(a, id, out, ok)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: numerator, denominator
        logical :: exact

        out = id
        ok = .false.
        call exact_value(a, id, numerator, denominator, exact)
        if (.not. exact) return
        if (numerator < 0_int64) then
            out = a%int(-1_int64)
        else if (numerator > 0_int64) then
            out = a%int(1_int64)
        else
            out = a%int(0_int64)
        end if
        ok = .true.
    end subroutine exact_sign_value

    subroutine exact_rounding_value(a, name, id, out, ok)
        type(arena_t), intent(inout) :: a
        character(*), intent(in) :: name
        integer, intent(in) :: id
        integer, intent(out) :: out
        logical, intent(out) :: ok
        integer(int64) :: numerator, denominator, rounded
        logical :: exact

        out = id
        ok = .false.
        call exact_value(a, id, numerator, denominator, exact)
        if (.not. exact) return
        rounded = numerator/denominator
        if (name == "floor") then
            if (numerator < 0_int64 .and. &
                modulo(numerator, denominator) /= 0_int64) then
                rounded = rounded - 1_int64
            end if
        else
            if (numerator > 0_int64 .and. &
                modulo(numerator, denominator) /= 0_int64) then
                rounded = rounded + 1_int64
            end if
        end if
        out = a%int(rounded)
        ok = .true.
    end subroutine exact_rounding_value

    subroutine integer_square_root(value, root, ok)
        integer(int64), intent(in) :: value
        integer(int64), intent(out) :: root
        logical, intent(out) :: ok
        real(dp) :: approximate

        root = 0_int64
        ok = .false.
        if (value < 0_int64) return
        if (value == 0_int64) then
            ok = .true.
            return
        end if
        approximate = sqrt(real(value, dp))
        root = int(approximate, int64)
        if (root < 1_int64) root = 1_int64
        do while (root > value/root)
            root = root - 1_int64
        end do
        do while (root < value/root)
            root = root + 1_int64
        end do
        ok = root*root == value
    end subroutine integer_square_root

    subroutine exact_sine_cosine(a, numerator, denominator, sine, cosine, ok)
        type(arena_t), intent(inout) :: a
        integer(int64), intent(in) :: numerator, denominator
        integer, intent(out) :: sine, cosine
        logical, intent(out) :: ok
        integer :: one(1), half, root2, root2_inverse, root3, half_root3
        integer(int64) :: residue

        sine = a%int(0_int64)
        cosine = a%int(0_int64)
        ok = .false.
        half = a%rat(1_int64, 2_int64)
        one(1) = a%int(2_int64)
        root2 = a%func("sqrt", one)
        root2_inverse = simplify_power(a, root2, a%int(-1_int64))
        one(1) = a%int(3_int64)
        root3 = a%func("sqrt", one)
        half_root3 = mul_pair(a, root3, half)

        select case (denominator)
        case (1_int64)
            residue = modulo(numerator, 2_int64)
            if (residue == 0_int64) then
                cosine = a%int(1_int64)
            else
                cosine = a%int(-1_int64)
            end if
        case (2_int64)
            residue = modulo(numerator, 4_int64)
            select case (residue)
            case (0_int64)
                cosine = a%int(1_int64)
            case (1_int64)
                sine = a%int(1_int64)
            case (2_int64)
                cosine = a%int(-1_int64)
            case (3_int64)
                sine = a%int(-1_int64)
            end select
        case (3_int64)
            residue = modulo(numerator, 6_int64)
            select case (residue)
            case (0_int64)
                cosine = a%int(1_int64)
            case (1_int64)
                sine = half_root3
                cosine = half
            case (2_int64)
                sine = half_root3
                cosine = a%rat(-1_int64, 2_int64)
            case (3_int64)
                cosine = a%int(-1_int64)
            case (4_int64)
                sine = mul_pair(a, a%int(-1_int64), half_root3)
                cosine = a%rat(-1_int64, 2_int64)
            case (5_int64)
                sine = mul_pair(a, a%int(-1_int64), half_root3)
                cosine = half
            end select
        case (4_int64)
            residue = modulo(numerator, 8_int64)
            select case (residue)
            case (0_int64)
                cosine = a%int(1_int64)
            case (1_int64)
                sine = root2_inverse
                cosine = root2_inverse
            case (2_int64)
                sine = a%int(1_int64)
            case (3_int64)
                sine = root2_inverse
                cosine = mul_pair(a, a%int(-1_int64), root2_inverse)
            case (4_int64)
                cosine = a%int(-1_int64)
            case (5_int64)
                sine = mul_pair(a, a%int(-1_int64), root2_inverse)
                cosine = mul_pair(a, a%int(-1_int64), root2_inverse)
            case (6_int64)
                sine = a%int(-1_int64)
            case (7_int64)
                sine = mul_pair(a, a%int(-1_int64), root2_inverse)
                cosine = root2_inverse
            end select
        case (6_int64)
            residue = modulo(numerator, 12_int64)
            select case (residue)
            case (0_int64)
                cosine = a%int(1_int64)
            case (1_int64)
                sine = half
                cosine = half_root3
            case (2_int64)
                sine = half_root3
                cosine = half
            case (3_int64)
                sine = a%int(1_int64)
            case (4_int64)
                sine = half_root3
                cosine = a%rat(-1_int64, 2_int64)
            case (5_int64)
                sine = half
                cosine = mul_pair(a, a%int(-1_int64), half_root3)
            case (6_int64)
                cosine = a%int(-1_int64)
            case (7_int64)
                sine = mul_pair(a, a%int(-1_int64), half)
                cosine = mul_pair(a, a%int(-1_int64), half_root3)
            case (8_int64)
                sine = mul_pair(a, a%int(-1_int64), half_root3)
                cosine = a%rat(-1_int64, 2_int64)
            case (9_int64)
                sine = a%int(-1_int64)
            case (10_int64)
                sine = mul_pair(a, a%int(-1_int64), half_root3)
                cosine = half
            case (11_int64)
                sine = mul_pair(a, a%int(-1_int64), half)
                cosine = half_root3
            end select
        case default
            return
        end select
        ok = .true.
    end subroutine exact_sine_cosine

    !> Recognise q*pi without approximating pi. A symbolic coefficient or a
    !> second transcendental factor is refused, so the exact root fragment
    !> never guesses at an angle.
    subroutine rational_pi_multiple(a, id, numerator, denominator, ok)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer(int64), intent(out) :: numerator, denominator
        logical, intent(out) :: ok
        integer :: k, factor
        integer(int64) :: factor_numerator, factor_denominator
        integer(int64) :: product_numerator, product_denominator
        logical :: exact, saw_pi, product_ok

        numerator = 1_int64
        denominator = 1_int64
        ok = .false.
        saw_pi = .false.

        if (a%kind_of(id) == NK_CONST) then
            if (chars(a%name_of(id)) == "pi") then
                ok = .true.
            end if
            return
        end if
        if (a%kind_of(id) /= NK_MUL) return

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
            call exact_value(a, factor, factor_numerator, factor_denominator, &
                exact)
            if (.not. exact) return
            call fraction_mul(numerator, denominator, factor_numerator, &
                factor_denominator, product_numerator, product_denominator, &
                product_ok)
            if (.not. product_ok) return
            numerator = product_numerator
            denominator = product_denominator
        end do
        ok = saw_pi
    end subroutine rational_pi_multiple

    !> Recognise i*q*pi without approximating pi. Integer and half-integer
    !> multiples are reduced exactly; other fractional multiples remain
    !> exponential nodes for the conservative zero-test fragment.
    subroutine rational_i_pi_multiple(a, id, numerator, denominator, ok)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer(int64), intent(out) :: numerator, denominator
        logical,        intent(out) :: ok
        integer :: k, factor
        integer(int64) :: factor_numerator, factor_denominator
        integer(int64) :: product_numerator, product_denominator
        logical :: exact, saw_i, saw_pi, product_ok

        numerator = 1_int64
        denominator = 1_int64
        ok = .false.
        saw_i = .false.
        saw_pi = .false.

        if (a%kind_of(id) /= NK_MUL) return
        do k = 1, a%nargs_of(id)
            factor = a%arg_of(id, k)
            if (a%kind_of(factor) == NK_CONST) then
                if (chars(a%name_of(factor)) == "i") then
                    if (saw_i) return
                    saw_i = .true.
                    cycle
                else if (chars(a%name_of(factor)) == "pi") then
                    if (saw_pi) return
                    saw_pi = .true.
                    cycle
                end if
            end if
            call exact_value(a, factor, factor_numerator, factor_denominator, &
                exact)
            if (.not. exact) return
            call fraction_mul(numerator, denominator, factor_numerator, &
                factor_denominator, product_numerator, product_denominator, &
                product_ok)
            if (.not. product_ok) return
            numerator = product_numerator
            denominator = product_denominator
        end do
        ok = saw_i .and. saw_pi
    end subroutine rational_i_pi_multiple

    !> Construct the small exact root-of-unity fragment represented by the
    !> native scalar vocabulary. The denominator is the reduced multiple of pi.
    function exact_periodic_constant(a, numerator, denominator, ok) result(out)
        type(arena_t), target, intent(inout) :: a
        integer(int64), intent(in)    :: numerator, denominator
        integer                       :: out
        logical, intent(out)          :: ok
        integer :: one(1), root, half, half_root, imaginary
        integer :: root_inverse, real_part, imaginary_part

        out = a%int(0_int64)
        ok = .false.
        one(1) = a%int(2_int64)
        half = a%rat(1_int64, 2_int64)

        select case (denominator)
        case (1_int64)
            if (modulo(numerator, 2_int64) == 0_int64) then
                out = a%int(1_int64)
            else
                out = a%int(-1_int64)
            end if
            ok = .true.
        case (2_int64)
            select case (modulo(numerator, 4_int64))
            case (0)
                out = a%int(1_int64)
            case (1)
                out = a%const("i")
            case (2)
                out = a%int(-1_int64)
            case default
                out = mul_pair(a, a%int(-1_int64), a%const("i"))
            end select
            ok = .true.
        case (4_int64)
            root = a%func("sqrt", one)
            root_inverse = simplify_power(a, root, a%int(-1_int64))
            select case (modulo(numerator, 8_int64))
            case (1)
                real_part = a%int(1_int64)
                imaginary_part = a%const("i")
            case (3)
                real_part = a%int(-1_int64)
                imaginary_part = a%const("i")
            case (5)
                real_part = a%int(-1_int64)
                imaginary_part = mul_pair(a, a%int(-1_int64), a%const("i"))
            case (7)
                real_part = a%int(1_int64)
                imaginary_part = mul_pair(a, a%int(-1_int64), a%const("i"))
            case default
                return
            end select
            out = mul_pair(a, add_pair(a, real_part, imaginary_part), &
                root_inverse)
            ok = .true.
        case (3_int64)
            one(1) = a%int(3_int64)
            root = a%func("sqrt", one)
            half_root = mul_pair(a, root, half)
            imaginary = mul_pair(a, a%const("i"), half_root)
            select case (modulo(numerator, 6_int64))
            case (1)
                out = add_pair(a, half, imaginary)
            case (2)
                out = add_pair(a, a%rat(-1_int64, 2_int64), imaginary)
            case (4)
                out = add_pair(a, a%rat(-1_int64, 2_int64), &
                    mul_pair(a, a%int(-1_int64), imaginary))
            case (5)
                out = add_pair(a, half, mul_pair(a, a%int(-1_int64), imaginary))
            case default
                return
            end select
            ok = .true.
        case (6_int64)
            one(1) = a%int(3_int64)
            root = a%func("sqrt", one)
            half_root = mul_pair(a, root, half)
            imaginary = mul_pair(a, a%const("i"), half)
            select case (modulo(numerator, 12_int64))
            case (1)
                out = add_pair(a, half_root, imaginary)
            case (5)
                out = add_pair(a, mul_pair(a, a%int(-1_int64), half_root), &
                    imaginary)
            case (7)
                out = add_pair(a, mul_pair(a, a%int(-1_int64), half_root), &
                    mul_pair(a, a%int(-1_int64), imaginary))
            case (11)
                out = add_pair(a, half_root, &
                    mul_pair(a, a%int(-1_int64), imaginary))
            case default
                return
            end select
            ok = .true.
        end select
    end function exact_periodic_constant

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
            kind == NK_BIG_INT .or. kind == NK_BIG_RAT .or. &
            kind == NK_ALGEBRAIC
    end function is_exact_scalar_kind

    pure function coefficient_one() result(coefficient)
        type(exact_coefficient_t) :: coefficient
        coefficient%numerator = 1_int64
    end function coefficient_one

    pure function coefficient_is_zero(coefficient) result(zero)
        type(exact_coefficient_t), intent(in) :: coefficient
        logical                              :: zero
        if (coefficient%algebraic) then
            zero = coefficient%zero
        else
            ! Canonical arbitrary-precision zero is always a compact node.
            zero = coefficient%compact .and. &
                coefficient%numerator == 0_int64
        end if
    end function coefficient_is_zero

    pure function coefficient_is_one(coefficient) result(one)
        type(exact_coefficient_t), intent(in) :: coefficient
        logical                              :: one
        if (coefficient%algebraic) then
            one = coefficient%one
        else
            ! Canonical arbitrary-precision one is always a compact node.
            one = coefficient%compact .and. &
                coefficient%numerator == coefficient%denominator
        end if
    end function coefficient_is_one

    subroutine coefficient_from_node(a, id, coefficient, ok)
        type(arena_t), intent(in)             :: a
        integer, intent(in)                   :: id
        type(exact_coefficient_t), intent(out) :: coefficient
        logical, intent(out)                  :: ok
        type(str_t) :: difference
        type(str_t) :: one_text
        integer :: real_sign, imaginary_sign
        logical :: signs_ok

        coefficient = coefficient_one()
        if (a%kind_of(id) == NK_ALGEBRAIC) then
            coefficient%compact = .false.
            coefficient%algebraic = .true.
            coefficient%id = id
            coefficient%algebraic_text = a%algebraic_text_of(id)
            call algebraic_signs(chars(coefficient%algebraic_text), &
                real_sign, imaginary_sign, signs_ok)
            if (.not. signs_ok) then
                ok = .false.
                return
            end if
            coefficient%zero = real_sign == 0 .and. imaginary_sign == 0
            one_text = algebraic_from_real_text("1", signs_ok)
            if (signs_ok) difference = algebraic_sub( &
                chars(coefficient%algebraic_text), chars(one_text), signs_ok)
            if (signs_ok) then
                call algebraic_signs(chars(difference), real_sign, &
                    imaginary_sign, signs_ok)
                coefficient%one = signs_ok .and. real_sign == 0 .and. &
                    imaginary_sign == 0
            end if
            ok = signs_ok
            return
        end if

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

        if (coefficient%algebraic) then
            id = coefficient%id
        else if (coefficient%compact) then
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
        type(str_t) :: value, other
        logical :: inserted
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
        if (left%algebraic .or. right%algebraic) then
            call algebraic_coefficient_text(a, left, value, ok)
            if (.not. ok) return
            call algebraic_coefficient_text(a, right, other, ok)
            if (.not. ok) return
            value = algebraic_add(chars(value), chars(other), ok)
            if (.not. ok) return
            id = a%algebraic(chars(value), inserted)
            ok = inserted
            if (.not. ok) return
            call coefficient_from_node(a, id, result, ok)
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
        type(str_t) :: value, other
        logical :: inserted
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
        if (left%algebraic .or. right%algebraic) then
            call algebraic_coefficient_text(a, left, value, ok)
            if (.not. ok) return
            call algebraic_coefficient_text(a, right, other, ok)
            if (.not. ok) return
            value = algebraic_mul(chars(value), chars(other), ok)
            if (.not. ok) return
            id = a%algebraic(chars(value), inserted)
            ok = inserted
            if (.not. ok) return
            call coefficient_from_node(a, id, result, ok)
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

    subroutine algebraic_coefficient_text(a, coefficient, value, ok)
        type(arena_t), intent(inout) :: a
        type(exact_coefficient_t), intent(in) :: coefficient
        type(str_t), intent(out) :: value
        logical, intent(out) :: ok
        integer :: id

        if (coefficient%algebraic) then
            value = coefficient%algebraic_text
            ok = .true.
            return
        end if
        id = coefficient_node(a, coefficient)
        value = algebraic_from_real_text(chars(a%exact_text_of(id)), ok)
    end subroutine algebraic_coefficient_text

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

        if (a%kind_of(base) == NK_ALGEBRAIC) then
            value = algebraic_pow(chars(a%algebraic_text_of(base)), exponent, ok)
            if (.not. ok) return
            id = a%algebraic(chars(value), inserted)
            ok = inserted
            return
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
        integer :: real_sign, imaginary_sign
        logical :: signs_ok

        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            nonzero = a%num_of(id) /= 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            ! Canonical zero always downcasts to NK_INT.
            nonzero = .true.
        case (NK_REAL)
            nonzero = a%real_of(id) /= 0.0_dp
        case (NK_ALGEBRAIC)
            call algebraic_signs(chars(a%algebraic_text_of(id)), &
                real_sign, imaginary_sign, signs_ok)
            if (signs_ok) then
                nonzero = real_sign /= 0 .or. imaginary_sign /= 0
            else
                nonzero = .false.
            end if
        case default
            nonzero = .false.
        end select
    end function definitely_nonzero

    recursive function has_branch_sensitive_power(a, id) result(found)
        type(arena_t), intent(in) :: a
        integer, intent(in)       :: id
        logical                   :: found
        integer                   :: k
        integer(int64)            :: numerator, denominator
        logical                   :: exact

        found = .false.
        select case (a%kind_of(id))
        case (NK_FUNC)
            if (chars(a%name_of(id)) == "sqrt") then
                found = .true.
                return
            end if
        case (NK_POW)
            call exact_value(a, a%arg_of(id, 2), numerator, denominator, exact)
            if (exact) then
                if (denominator /= 1_int64) then
                    found = .true.
                    return
                end if
            end if
        end select

        do k = 1, a%nargs_of(id)
            if (has_branch_sensitive_power(a, a%arg_of(id, k))) then
                found = .true.
                return
            end if
        end do
    end function has_branch_sensitive_power

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
        case (NK_ALGEBRAIC)
            yes = is_algebraic_zero_id(a, id)
        end select
    end function is_zero_id

    !> A structural NaN/undefined sentinel is absorbing for the supported
    !> arithmetic heads. Keep this test in the native simplifier's one domain
    !> helper rather than teaching every coefficient and function rule about
    !> one special constant. Unknown applied heads remain opaque: their domain
    !> semantics are not ours to guess.
    function has_nan_operand(a, ids) result(found)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: ids(:)
        logical                   :: found
        integer                   :: k

        found = .false.
        do k = 1, size(ids)
            if (is_nan_id(a, ids(k))) then
                found = .true.
                return
            end if
        end do
    end function has_nan_operand

    function is_nan_id(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes

        yes = a%kind_of(id) == NK_CONST .and. &
            chars(a%name_of(id)) == "nan"
    end function is_nan_id

    function nan_node(a) result(id)
        type(arena_t), intent(inout) :: a
        integer                     :: id

        id = a%const("nan")
    end function nan_node

    function nan_propagates_function(name, a, args) result(yes)
        character(*),  intent(in) :: name
        type(arena_t), intent(in)  :: a
        integer,       intent(in)  :: args(:)
        logical                    :: yes

        yes = .false.
        if (.not. has_nan_operand(a, args)) return
        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", &
                "exp", "log", "sqrt", "abs", "erf", "erfc", "gamma", &
                "loggamma", "log10", "floor", "ceiling", "sign", &
                "csc", "sec", "cot", "csch", "sech", "coth", &
                "besselj", "besseli", "legendrep", "legendreq")
            yes = .true.
        end select
    end function nan_propagates_function

    subroutine simplify_domain_add(a, operands, out, applied)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: operands(:)
        integer,       intent(out)   :: out
        logical,       intent(out)   :: applied
        integer :: i, domain, direction
        integer :: positive_oo, negative_oo, zoo_count
        logical :: known, contains_domain, has_zero

        applied = .false.
        positive_oo = 0
        negative_oo = 0
        zoo_count = 0
        do i = 1, size(operands)
            call classify_domain_id(a, operands(i), domain, direction, known, &
                contains_domain, has_zero)
            if (.not. known) return
            if (.not. contains_domain) cycle
            if (domain == DOMAIN_ZOO) then
                zoo_count = zoo_count + 1
            else if (domain == DOMAIN_OO) then
                if (direction < 0) then
                    negative_oo = negative_oo + 1
                else
                    positive_oo = positive_oo + 1
                end if
            else
                return
            end if
        end do

        if (positive_oo + negative_oo + zoo_count == 0) return
        applied = .true.
        if (zoo_count > 0) then
            if (zoo_count > 1 .or. positive_oo + negative_oo > 0) then
                out = nan_node(a)
            else
                out = a%const("zoo")
            end if
        else if (positive_oo > 0 .and. negative_oo > 0) then
            out = nan_node(a)
        else if (positive_oo > 0) then
            out = a%const("oo")
        else
            out = signed_oo_node(a, -1)
        end if
    end subroutine simplify_domain_add

    subroutine simplify_domain_mul(a, operands, out, applied)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: operands(:)
        integer,       intent(out)   :: out
        logical,       intent(out)   :: applied
        integer :: i, domain, direction, infinity_direction
        logical :: known, contains_domain, has_zero
        logical :: have_domain, have_oo, have_zoo, all_known, any_zero

        applied = .false.
        have_domain = .false.
        have_oo = .false.
        have_zoo = .false.
        all_known = .true.
        any_zero = .false.
        infinity_direction = 1
        do i = 1, size(operands)
            call classify_domain_id(a, operands(i), domain, direction, known, &
                contains_domain, has_zero)
            all_known = all_known .and. known
            any_zero = any_zero .or. has_zero
            if (.not. contains_domain) then
                if (known .and. .not. has_zero) then
                    infinity_direction = infinity_direction * direction
                end if
                cycle
            end if
            have_domain = .true.
            if (domain == DOMAIN_ZOO) then
                have_zoo = .true.
            else if (domain == DOMAIN_OO) then
                have_oo = .true.
                infinity_direction = infinity_direction * direction
            end if
        end do

        if (.not. have_domain) return
        if (any_zero) then
            applied = .true.
            out = nan_node(a)
            return
        end if
        if (.not. all_known) return

        applied = .true.
        if (have_zoo) then
            out = a%const("zoo")
        else if (have_oo) then
            out = signed_oo_node(a, infinity_direction)
        else
            applied = .false.
        end if
    end subroutine simplify_domain_mul

    subroutine simplify_domain_power(a, base, exponent, out, applied)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: base
        integer(int64), intent(in)   :: exponent
        integer,       intent(out)   :: out
        logical,       intent(out)   :: applied
        integer :: domain, direction
        logical :: known, contains_domain, has_zero

        applied = .false.
        call classify_domain_id(a, base, domain, direction, known, &
            contains_domain, has_zero)
        if (.not. known .or. .not. contains_domain .or. has_zero) return
        if (domain /= DOMAIN_OO .and. domain /= DOMAIN_ZOO) return

        applied = .true.
        if (exponent < 0_int64) then
            out = a%int(0_int64)
        else if (domain == DOMAIN_ZOO) then
            out = a%const("zoo")
        else if (direction < 0 .and. modulo(exponent, 2_int64) == 1_int64) then
            out = signed_oo_node(a, -1)
        else
            out = a%const("oo")
        end if
    end subroutine simplify_domain_power

    recursive subroutine classify_domain_id(a, id, domain, direction, known, &
            contains_domain, has_zero)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer,       intent(out) :: domain, direction
        logical,       intent(out) :: known, contains_domain, has_zero
        integer :: factor_domain, factor_direction, k
        logical :: factor_known, factor_contains, factor_zero
        integer :: scalar_sign
        logical :: scalar_known

        domain = DOMAIN_NONE
        direction = 1
        known = .true.
        contains_domain = .false.
        has_zero = .false.
        select case (a%kind_of(id))
        case (NK_CONST)
            select case (chars(a%name_of(id)))
            case ("oo")
                domain = DOMAIN_OO
                contains_domain = .true.
            case ("zoo")
                domain = DOMAIN_ZOO
                contains_domain = .true.
                direction = 0
            case default
                known = .false.
            end select
        case (NK_MUL)
            do k = 1, a%nargs_of(id)
                call classify_domain_id(a, a%arg_of(id, k), factor_domain, &
                    factor_direction, factor_known, factor_contains, factor_zero)
                known = known .and. factor_known
                has_zero = has_zero .or. factor_zero
                if (.not. factor_contains) then
                    if (factor_known .and. .not. factor_zero) then
                        direction = direction * factor_direction
                    end if
                    cycle
                end if
                contains_domain = .true.
                if (factor_domain == DOMAIN_ZOO) then
                    domain = DOMAIN_ZOO
                    direction = 0
                else if (factor_domain == DOMAIN_OO .and. domain /= DOMAIN_ZOO) then
                    domain = DOMAIN_OO
                    direction = direction * factor_direction
                end if
            end do
            if (.not. contains_domain) then
                known = .false.
                return
            end if
        case default
            call finite_scalar_sign(a, id, scalar_sign, scalar_known)
            if (scalar_known) then
                direction = scalar_sign
                has_zero = scalar_sign == 0
            else
                known = .false.
            end if
        end select
    end subroutine classify_domain_id

    subroutine finite_scalar_sign(a, id, sign, known)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer,       intent(out) :: sign
        logical,       intent(out) :: known
        integer :: real_sign, imaginary_sign
        logical :: signs_ok
        integer(int64) :: numerator
        character(:), allocatable :: text

        sign = 0
        known = .true.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            numerator = a%num_of(id)
            if (numerator > 0_int64) then
                sign = 1
            else if (numerator < 0_int64) then
                sign = -1
            end if
        case (NK_BIG_INT, NK_BIG_RAT)
            text = chars(a%exact_text_of(id))
            if (text(1:1) == "-") sign = -1
            if (text(1:1) /= "-") sign = 1
        case (NK_REAL)
            if (a%real_of(id) > 0.0_dp) then
                sign = 1
            else if (a%real_of(id) < 0.0_dp) then
                sign = -1
            end if
        case (NK_ALGEBRAIC)
            call algebraic_signs(chars(a%algebraic_text_of(id)), real_sign, &
                imaginary_sign, signs_ok)
            if (signs_ok .and. imaginary_sign == 0) then
                sign = real_sign
            else
                known = .false.
            end if
        case default
            known = .false.
        end select
    end subroutine finite_scalar_sign

    function signed_oo_node(a, direction) result(id)
        type(arena_t), intent(inout) :: a
        integer,       intent(in)    :: direction
        integer :: id
        integer :: pair(2)

        if (direction >= 0) then
            id = a%const("oo")
        else
            pair(1) = a%int(-1_int64)
            pair(2) = a%const("oo")
            id = a%mul(pair)
        end if
    end function signed_oo_node

    subroutine simplify_domain_function(a, name, args, out, applied)
        type(arena_t), intent(inout) :: a
        character(*),  intent(in)    :: name
        integer,       intent(in)    :: args(:)
        integer,       intent(out)   :: out
        logical,       intent(out)   :: applied
        integer :: domain, direction
        logical :: known, contains_domain, has_zero
        integer :: pair(2)

        applied = .false.
        if (size(args) /= 1) return
        call classify_domain_id(a, args(1), domain, direction, known, &
            contains_domain, has_zero)
        if (.not. known .or. .not. contains_domain .or. has_zero) return

        select case (name)
        case ("sqrt")
            applied = .true.
            if (domain == DOMAIN_ZOO) then
                out = a%const("zoo")
            else if (direction < 0) then
                pair(1) = a%const("i")
                pair(2) = a%const("oo")
                out = a%mul(pair)
            else
                out = a%const("oo")
            end if
        case ("abs")
            applied = .true.
            out = a%const("oo")
        case ("exp")
            applied = .true.
            if (domain == DOMAIN_ZOO) then
                out = nan_node(a)
            else if (direction < 0) then
                out = a%int(0_int64)
            else
                out = a%const("oo")
            end if
        case ("log")
            applied = .true.
            if (domain == DOMAIN_ZOO) then
                out = a%const("zoo")
            else
                out = a%const("oo")
            end if
        end select
    end subroutine simplify_domain_function

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
        case (NK_ALGEBRAIC)
            yes = is_algebraic_one_id(a, id)
        end select
    end function is_one_id

    function is_algebraic_zero_id(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: real_sign, imaginary_sign
        logical :: ok

        yes = .false.
        call algebraic_signs(chars(a%algebraic_text_of(id)), real_sign, &
            imaginary_sign, ok)
        if (ok) yes = real_sign == 0 .and. imaginary_sign == 0
    end function is_algebraic_zero_id

    function is_algebraic_one_id(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        type(str_t) :: one_text, difference
        integer :: real_sign, imaginary_sign
        logical :: ok

        yes = .false.
        one_text = algebraic_from_re_im("1", "0", ok)
        if (.not. ok) return
        difference = algebraic_sub(chars(a%algebraic_text_of(id)), &
            chars(one_text), ok)
        if (.not. ok) return
        call algebraic_signs(chars(difference), real_sign, imaginary_sign, ok)
        if (ok) yes = real_sign == 0 .and. imaginary_sign == 0
    end function is_algebraic_one_id

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
