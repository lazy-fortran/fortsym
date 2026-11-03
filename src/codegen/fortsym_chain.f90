module fortsym_chain
    ! Forward-mode derivative propagation through named intermediates.
    !
    ! A kernel often computes outputs from a few intermediates, such as
    ! v = (p - A(x))/h(x), and needs their derivatives. Expanding every
    ! intermediate and differentiating the result repeats the work of the
    ! intermediates in every derivative and invites a simplifier to expand
    ! differences that cancel. A chain instead keeps each intermediate as an
    ! opaque symbol and applies the chain rule at the code level:
    !
    !     D_i e = de/dx_i + sum_k (de/dv_k) D_i v_k,
    !
    ! where D_i v_k is itself a named intermediate defined by the same rule
    ! applied to the definition of v_k. Higher derivatives recurse, and mixed
    ! partials share one name by their multi-index. The result is exact: every
    ! intermediate is a symbolic expression in the inputs, recoverable through
    ! chain_expand for proofs, and nothing is approximated.
    !
    ! A quotient intermediate v = n/b differentiates as (D n - v D b)/b, which
    ! reuses v instead of recomputing n/b**2, the form hand-written kernels
    ! use for implicit differentiation of b v = n.
    !
    ! A derivative whose expanded form the native simplifier reduces to exact
    ! zero is replaced by zero. This is the only rewrite applied to the
    ! expanded form; all emitted text keeps the intermediates' own definitions,
    ! so the simplifier never expands a difference that cancels.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_MUL, &
        NK_POW, NK_CONST, NK_FUNC, NK_ADD
    use fortsym_expr, only: expr_t, sym, num, is_valid, operator(+), &
        operator(-), operator(*), operator(**)
    use fortsym_diff, only: diff
    use fortsym_subs, only: subs
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_string, only: str_t, str, chars
    implicit none
    private

    public :: chain_t, fold_numbers

    type :: chain_t
        type(arena_t), pointer :: a => null()
        !> Independent variables of the total derivatives, in index order.
        type(expr_t), allocatable :: vars(:)
        !> Intermediates in creation order. A definition reads only inputs,
        !> opaque functions and earlier intermediates, so this order is a
        !> valid evaluation order.
        type(expr_t), allocatable :: names(:)
        type(expr_t), allocatable :: defs(:)
        integer :: n = 0
        !> Declared intermediate whose derivative an entry is (itself for a
        !> declared one), and its derivative multi-index as counts per variable.
        integer, allocatable :: base(:)
        integer, allocatable :: orders(:, :)
        !> Known derivatives keyed by (declared intermediate, multi-index):
        !> a named intermediate, an atom, or exact zero.
        integer :: n_known = 0
        integer, allocatable :: known_base(:)
        integer, allocatable :: known_order(:, :)
        type(expr_t), allocatable :: known_value(:)
        !> Differentiate a quotient intermediate as (D n - v D b)/b.
        logical :: quotient_reuse = .true.
        !> Replace a derivative whose expanded form simplifies to exact zero.
        logical :: prove_zeros = .true.
    contains
        procedure :: init => chain_init
        procedure :: let => chain_let
        procedure :: d => chain_d
        procedure :: expand => chain_expand
        procedure :: needed => chain_needed
        procedure :: index_of => chain_index_of
    end type chain_t

contains

    !> Start a chain over the independent variables `vars`, which must be
    !> symbols of one arena.
    subroutine chain_init(self, vars)
        class(chain_t), intent(inout) :: self
        type(expr_t), intent(in) :: vars(:)
        integer :: k

        if (size(vars) == 0) error stop "chain: no independent variables"
        do k = 1, size(vars)
            if (.not. is_valid(vars(k))) error stop "chain: invalid variable"
            if (vars(k)%kind() /= NK_SYM) error stop "chain: variables must be symbols"
        end do
        self%a => vars(1)%a
        self%vars = vars
        self%n = 0
        self%n_known = 0
        allocate (self%names(16), self%defs(16), self%base(16))
        allocate (self%orders(size(vars), 16))
        allocate (self%known_base(16), self%known_order(size(vars), 16), &
            self%known_value(16))
    end subroutine chain_init

    !> Declare an intermediate `name` = `definition` and return its symbol.
    !> The definition may read earlier intermediates.
    function chain_let(self, name, definition) result(v)
        class(chain_t), intent(inout) :: self
        character(*), intent(in) :: name
        type(expr_t), intent(in) :: definition
        type(expr_t) :: v
        integer :: order(size(self%vars)), k

        if (.not. associated(self%a)) error stop "chain: init was not called"
        v = sym(self%a, name)
        if (self%index_of(v) > 0) error stop "chain: intermediate declared twice"
        do k = 1, size(self%vars)
            if (v%id == self%vars(k)%id) error stop "chain: name of a variable"
        end do
        order = 0
        call append_entry(self, v, definition, 0, order)
        self%base(self%n) = self%n
        call remember(self, self%n, order, v)
    end function chain_let

    !> Total derivative of `e` with respect to variable `i`. Intermediates and
    !> their derivatives stay symbols; new derivative intermediates are added
    !> on demand.
    recursive function chain_d(self, e, i) result(d)
        class(chain_t), intent(inout) :: self
        type(expr_t), intent(in) :: e
        integer, intent(in) :: i
        type(expr_t) :: d, p, dk
        logical, allocatable :: occurs(:)
        integer :: k

        if (i < 1 .or. i > size(self%vars)) error stop "chain: variable index"
        d = diff(e, self%vars(i))
        allocate (occurs(self%n), source=.false.)
        call mark_occurrences(self, e, occurs)
        do k = 1, size(occurs)
            if (.not. occurs(k)) cycle
            dk = derivative_of(self, k, i)
            if (is_zero(dk)) cycle
            p = diff(e, self%names(k))
            if (is_zero(p)) cycle
            if (is_zero(d)) then
                d = p*dk
            else
                d = d + p*dk
            end if
        end do
        d = fold_numbers(d)
    end function chain_d

    !> Substitute every intermediate by its definition, for proofs and
    !> comparisons. Never emit this form: it is what the chain avoids.
    function chain_expand(self, e) result(r)
        class(chain_t), intent(in) :: self
        type(expr_t), intent(in) :: e
        type(expr_t) :: r
        integer :: k

        r = e
        ! A definition reads only earlier intermediates, so substituting the
        ! latest first leaves none behind.
        do k = self%n, 1, -1
            r = subs(r, self%names(k), self%defs(k))
        end do
    end function chain_expand

    !> Intermediates that `roots` read directly or through other
    !> intermediates, in evaluation order: assign names(k) = defs(k) for each
    !> returned k before evaluating the roots.
    function chain_needed(self, roots) result(indices)
        class(chain_t), intent(in) :: self
        type(expr_t), intent(in) :: roots(:)
        integer, allocatable :: indices(:)
        logical, allocatable :: needed(:)
        integer :: k

        allocate (needed(self%n), source=.false.)
        do k = 1, size(roots)
            call mark_occurrences(self, roots(k), needed)
        end do
        do k = self%n, 1, -1
            if (needed(k)) call mark_occurrences(self, self%defs(k), needed)
        end do
        indices = pack([(k, k=1, self%n)], needed)
    end function chain_needed

    !> Position of the intermediate whose symbol is `v`, or zero.
    function chain_index_of(self, v) result(index)
        class(chain_t), intent(in) :: self
        type(expr_t), intent(in) :: v
        integer :: index, k

        index = 0
        if (v%kind() /= NK_SYM) return
        do k = 1, self%n
            if (self%names(k)%id == v%id) then
                index = k
                return
            end if
        end do
    end function chain_index_of

    !> D_i of intermediate k: a known value, or a new named intermediate.
    recursive function derivative_of(self, k, i) result(value)
        type(chain_t), intent(inout) :: self
        integer, intent(in) :: k, i
        type(expr_t) :: value, definition, numerator, denominator, dden
        integer :: order(size(self%vars)), b, slot
        logical :: quotient

        b = self%base(k)
        order = self%orders(:, k)
        order(i) = order(i) + 1
        slot = find_known(self, b, order)
        if (slot > 0) then
            value = self%known_value(slot)
            return
        end if

        quotient = .false.
        if (self%quotient_reuse) &
            call split_quotient(self%defs(k), numerator, denominator, quotient)
        if (quotient) then
            dden = chain_d(self, denominator, i)
            if (is_zero(dden)) then
                definition = chain_d(self, self%defs(k), i)
            else
                definition = (chain_d(self, numerator, i) - self%names(k)*dden)* &
                    denominator**(-1)
            end if
        else
            definition = chain_d(self, self%defs(k), i)
        end if

        definition = fold_numbers(definition)
        if (.not. is_zero(definition) .and. self%prove_zeros) then
            if (proved_zero(self, definition)) definition = num(self%a, 0)
        end if
        if (is_atom(definition) .or. is_monomial(definition)) then
            ! Keeping a product of atoms inline lets the sums that read it
            ! share and factor its factors; CSE still names it once.
            value = definition
        else
            value = sym(self%a, derivative_name(self, b, order))
            call append_entry(self, value, definition, b, order)
        end if
        call remember(self, b, order, value)
    end function derivative_of

    !> Split v = n * b**(-1) at its first reciprocal factor.
    subroutine split_quotient(e, numerator, denominator, found)
        type(expr_t), intent(in) :: e
        type(expr_t), intent(out) :: numerator, denominator
        logical, intent(out) :: found
        type(expr_t) :: factor, exponent
        integer :: k, j

        found = .false.
        if (e%kind() /= NK_MUL) return
        do k = 1, e%nargs()
            factor = e%arg(k)
            if (factor%kind() /= NK_POW) cycle
            exponent = factor%arg(2)
            if (exponent%kind() /= NK_INT) cycle
            if (exponent%int_value() /= -1_int64) cycle
            denominator = factor%arg(1)
            numerator = num(e%a, 1)
            do j = 1, e%nargs()
                if (j /= k) numerator = numerator*e%arg(j)
            end do
            found = .true.
            return
        end do
    end subroutine split_quotient

    function proved_zero(self, e) result(zero)
        type(chain_t), intent(in) :: self
        type(expr_t), intent(in) :: e
        logical :: zero
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified

        engine = make_native_engine(self%a)
        simplified = engine%simplify(chain_expand(self, e))
        zero = .false.
        if (simplified%ok) zero = is_zero(simplified%value)
    end function proved_zero

    recursive subroutine mark_occurrences(self, e, occurs)
        type(chain_t), intent(in) :: self
        type(expr_t), intent(in) :: e
        logical, intent(inout) :: occurs(:)
        logical, allocatable :: visited(:)

        allocate (visited(e%a%size()), source=.false.)
        call walk(e%id)
    contains
        recursive subroutine walk(id)
            integer, intent(in) :: id
            integer :: k

            if (visited(id)) return
            visited(id) = .true.
            if (e%a%kind_of(id) == NK_SYM) then
                do k = 1, min(self%n, size(occurs))
                    if (self%names(k)%id == id) occurs(k) = .true.
                end do
                return
            end if
            do k = 1, e%a%nargs_of(id)
                call walk(e%a%arg_of(id, k))
            end do
        end subroutine walk
    end subroutine mark_occurrences

    subroutine append_entry(self, name, definition, base, order)
        type(chain_t), intent(inout) :: self
        type(expr_t), intent(in) :: name, definition
        integer, intent(in) :: base, order(:)
        type(expr_t), allocatable :: names(:), defs(:)
        integer, allocatable :: bases(:), orders(:, :)
        integer :: capacity

        if (self%n == size(self%names)) then
            capacity = 2*size(self%names)
            allocate (names(capacity), defs(capacity), bases(capacity), &
                orders(size(self%vars), capacity))
            names(:self%n) = self%names(:self%n)
            defs(:self%n) = self%defs(:self%n)
            bases(:self%n) = self%base(:self%n)
            orders(:, :self%n) = self%orders(:, :self%n)
            call move_alloc(names, self%names)
            call move_alloc(defs, self%defs)
            call move_alloc(bases, self%base)
            call move_alloc(orders, self%orders)
        end if
        self%n = self%n + 1
        self%names(self%n) = name
        self%defs(self%n) = definition
        self%base(self%n) = base
        self%orders(:, self%n) = order
    end subroutine append_entry

    subroutine remember(self, base, order, value)
        type(chain_t), intent(inout) :: self
        integer, intent(in) :: base, order(:)
        type(expr_t), intent(in) :: value
        integer, allocatable :: bases(:), orders(:, :)
        type(expr_t), allocatable :: values(:)
        integer :: capacity

        if (self%n_known == size(self%known_base)) then
            capacity = 2*size(self%known_base)
            allocate (bases(capacity), orders(size(self%vars), capacity), &
                values(capacity))
            bases(:self%n_known) = self%known_base(:self%n_known)
            orders(:, :self%n_known) = self%known_order(:, :self%n_known)
            values(:self%n_known) = self%known_value(:self%n_known)
            call move_alloc(bases, self%known_base)
            call move_alloc(orders, self%known_order)
            call move_alloc(values, self%known_value)
        end if
        self%n_known = self%n_known + 1
        self%known_base(self%n_known) = base
        self%known_order(:, self%n_known) = order
        self%known_value(self%n_known) = value
    end subroutine remember

    function find_known(self, base, order) result(slot)
        type(chain_t), intent(in) :: self
        integer, intent(in) :: base, order(:)
        integer :: slot, k

        slot = 0
        do k = 1, self%n_known
            if (self%known_base(k) /= base) cycle
            if (any(self%known_order(:, k) /= order)) cycle
            slot = k
            return
        end do
    end function find_known

    !> name_d<i>[_<j>...], one variable index per derivative order.
    function derivative_name(self, base, order) result(name)
        type(chain_t), intent(in) :: self
        integer, intent(in) :: base, order(:)
        character(:), allocatable :: name
        character(16) :: buffer
        integer :: i, m
        logical :: first

        name = chars(self%names(base)%name())//"_d"
        first = .true.
        do i = 1, size(order)
            do m = 1, order(i)
                write (buffer, '(i0)') i
                if (.not. first) name = name//"_"
                name = name//trim(buffer)
                first = .false.
            end do
        end do
    end function derivative_name

    !> Exact rewrites that remove work without expanding a sum: numbers are
    !> folded, integer powers of one base merged (x/x cancels), like terms
    !> merged, common factors taken out of a sum, a factor distributed into a
    !> sum when that removes a division, and 1/(b/c) written as c/b. Products
    !> have no cancellation and a power of a sum is never expanded, so no
    !> rewrite can introduce a difference that cancels.
    function fold_numbers(e) result(r)
        type(expr_t), intent(in) :: e
        type(expr_t) :: r
        integer, allocatable :: memo(:)

        r = e
        if (.not. is_valid(e)) return
        allocate (memo(e%a%size()), source=0)
        r%id = fold_node(e%a, e%id, memo)
    end function fold_numbers

    recursive function fold_node(a, id, memo) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: id
        integer, intent(inout) :: memo(:)
        integer :: out, k, j, kind, child, base, exponent
        integer(int64) :: cn, cd, n, d
        integer, allocatable :: kept(:)
        logical :: exact

        if (id <= size(memo)) then
            if (memo(id) > 0) then
                out = memo(id)
                return
            end if
        end if
        kind = a%kind_of(id)
        select case (kind)
        case (NK_MUL, NK_ADD)
            cn = 0_int64
            if (kind == NK_MUL) cn = 1_int64
            cd = 1_int64
            allocate (kept(0))
            do k = 1, a%nargs_of(id)
                child = fold_node(a, a%arg_of(id, k), memo)
                call small_number(a, child, n, d, exact)
                if (exact) then
                    if (kind == NK_MUL) then
                        call rational_mul(cn, cd, n, d, exact)
                    else
                        call rational_add(cn, cd, n, d, exact)
                    end if
                end if
                if (.not. exact) then
                    if (kind == NK_MUL .and. a%kind_of(child) == NK_MUL) then
                        kept = [kept, (a%arg_of(child, j), j=1, a%nargs_of(child))]
                    else
                        kept = [kept, child]
                    end if
                end if
            end do
            if (kind == NK_MUL) then
                ! A spliced product may carry its own number factor.
                do j = 1, size(kept)
                    call small_number(a, kept(j), n, d, exact)
                    if (exact) call rational_mul(cn, cd, n, d, exact)
                    if (exact) kept(j) = -1
                end do
                kept = pack(kept, kept > 0)
            end if
            if (kind == NK_ADD) call merge_like_terms(a, kept)
            if (kind == NK_MUL) then
                call merge_powers(a, kept)
                call cancel_into_sums(a, kept, memo)
            end if
            if (kind == NK_MUL .and. cn == 0_int64) then
                out = a%int(0_int64)
            else
                if (kind == NK_MUL .and. .not. (cn == 1_int64 .and. cd == 1_int64)) &
                    kept = [a%rat(cn, cd), kept]
                if (kind == NK_ADD .and. cn /= 0_int64) kept = [kept, a%rat(cn, cd)]
                select case (size(kept))
                case (0)
                    out = a%rat(cn, cd)
                case (1)
                    out = kept(1)
                case default
                    if (kind == NK_MUL) then
                        out = a%mul(kept)
                    else
                        out = factored_sum(a, kept)
                    end if
                end select
            end if
        case (NK_POW)
            base = fold_node(a, a%arg_of(id, 1), memo)
            exponent = fold_node(a, a%arg_of(id, 2), memo)
            call small_number(a, exponent, n, d, exact)
            if (exact .and. d == 1_int64 .and. a%kind_of(base) == NK_POW) then
                ! (b**m)**n -> b**(m*n) for integers m and n.
                call base_power(a, base, child, k)
                if (child /= base) then
                    n = n*k
                    base = child
                    exponent = a%int(n)
                end if
            end if
            if (exact .and. n == 1_int64 .and. d == 1_int64) then
                out = base
            else if (exact .and. n == 0_int64) then
                out = a%int(1_int64)
            else if (exact .and. n == -1_int64 .and. d == 1_int64 .and. &
                    has_reciprocal_factor(a, base)) then
                ! 1/(b/c) -> c/b: one division instead of two.
                allocate (kept(a%nargs_of(base)))
                do k = 1, size(kept)
                    kept(k) = fold_node(a, a%pow(a%arg_of(base, k), a%int(-1_int64)), memo)
                end do
                call merge_powers(a, kept)
                out = a%mul(kept)
            else if (exact .and. d == 1_int64 .and. abs(n) <= 4_int64) then
                out = numeric_power(a, base, int(n))
            else
                out = a%pow(base, exponent)
            end if
        case (NK_FUNC)
            allocate (kept(a%nargs_of(id)))
            do k = 1, size(kept)
                kept(k) = fold_node(a, a%arg_of(id, k), memo)
            end do
            out = id
            if (any(kept /= [(a%arg_of(id, k), k=1, size(kept))])) &
                out = a%func(chars(a%name_of(id)), kept)
        case default
            out = id
        end select
        if (id <= size(memo)) memo(id) = out
    end function fold_node

    !> a*x + b*x -> x*(a + b) for the factors x common to every term, with
    !> integer powers of one sign. This saves a product per term and adds no
    !> cancellation: the sum cancels exactly when the unfactored one does.
    function factored_sum(a, terms) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: terms(:)
        integer :: out
        integer, allocatable :: bases(:), powers(:), tb(:), tp(:), inner(:), common(:)
        integer :: i, j, k, slot, b, p

        ! Candidate common factors from the first term.
        call term_factors(a, terms(1), bases, powers)
        do i = 2, size(terms)
            call term_factors(a, terms(i), tb, tp)
            do k = 1, size(bases)
                if (powers(k) == 0) cycle
                slot = findloc(tb, bases(k), 1)
                if (slot == 0) then
                    powers(k) = 0
                else if (sign(1, tp(slot)) /= sign(1, powers(k))) then
                    powers(k) = 0
                else
                    powers(k) = sign(min(abs(powers(k)), abs(tp(slot))), powers(k))
                end if
            end do
        end do
        if (all(powers == 0)) then
            out = partially_factored_sum(a, terms)
            return
        end if

        allocate (inner(size(terms)))
        do i = 1, size(terms)
            call term_factors(a, terms(i), tb, tp)
            do k = 1, size(bases)
                if (powers(k) == 0) cycle
                slot = findloc(tb, bases(k), 1)
                tp(slot) = tp(slot) - powers(k)
            end do
            allocate (common(0))
            do j = 1, size(tb)
                if (tp(j) == 0) cycle
                if (tp(j) == 1) then
                    common = [common, tb(j)]
                else
                    common = [common, a%pow(tb(j), a%int(int(tp(j), int64)))]
                end if
            end do
            select case (size(common))
            case (0)
                inner(i) = a%int(1_int64)
            case (1)
                inner(i) = common(1)
            case default
                inner(i) = a%mul(common)
            end select
            deallocate (common)
        end do
        allocate (common(0))
        do k = 1, size(bases)
            b = bases(k)
            p = powers(k)
            if (p == 0) cycle
            if (p == 1) then
                common = [common, b]
            else
                common = [common, a%pow(b, a%int(int(p, int64)))]
            end if
        end do
        out = a%mul([common, a%add(inner)])
    end function factored_sum

    !> a*x + b*x + c -> x*(a + b) + c when the sum a + b is already a node:
    !> the product saved is joined by the sum shared with other expressions.
    !> Without such a node the choice of x is ambiguous, so the sum is kept.
    function partially_factored_sum(a, terms) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: terms(:)
        integer :: out
        integer, allocatable :: bases(:), powers(:), tb(:), tp(:), inner(:), rest(:)
        integer :: i, k, slot, existing, candidate, factor, n_before

        out = 0
        if (size(terms) >= 3) then
            do i = 1, size(terms)
                call term_factors(a, terms(i), bases, powers)
                do k = 1, size(bases)
                    if (powers(k) /= 1) cycle
                    n_before = a%size()
                    allocate (inner(0), rest(0))
                    do slot = 1, size(terms)
                        call term_factors(a, terms(slot), tb, tp)
                        existing = findloc(tb, bases(k), 1)
                        if (existing > 0) then
                            if (tp(existing) /= 1) existing = 0
                        end if
                        if (existing > 0) then
                            tp(existing) = 0
                            inner = [inner, rebuild(tb, tp)]
                        else
                            rest = [rest, terms(slot)]
                        end if
                    end do
                    if (size(inner) >= 2 .and. size(rest) >= 1) then
                        candidate = a%add(inner)
                        if (candidate <= n_before) then
                            factor = a%mul([bases(k), candidate])
                            out = a%add([rest, factor])
                            return
                        end if
                    end if
                    deallocate (inner, rest)
                end do
            end do
        end if
        out = a%add(terms)
    contains
        function rebuild(tb, tp) result(id)
            integer, intent(in) :: tb(:), tp(:)
            integer :: id, j
            integer, allocatable :: f(:)

            allocate (f(0))
            do j = 1, size(tb)
                if (tp(j) == 0) cycle
                if (tp(j) == 1) then
                    f = [f, tb(j)]
                else
                    f = [f, a%pow(tb(j), a%int(int(tp(j), int64)))]
                end if
            end do
            select case (size(f))
            case (0)
                id = a%int(1_int64)
            case (1)
                id = f(1)
            case default
                id = a%mul(f)
            end select
        end function rebuild
    end function partially_factored_sum

    !> Factors of a term as distinct bases with integer powers; a number
    !> factor is listed as an ordinary base with power one.
    subroutine term_factors(a, term, bases, powers)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: term
        integer, allocatable, intent(out) :: bases(:), powers(:)
        integer :: k, b, p, slot

        allocate (bases(0), powers(0))
        if (a%kind_of(term) /= NK_MUL) then
            call base_power(a, term, b, p)
            bases = [b]
            powers = [p]
            return
        end if
        do k = 1, a%nargs_of(term)
            call base_power(a, a%arg_of(term, k), b, p)
            slot = findloc(bases, b, 1)
            if (slot > 0) then
                powers(slot) = powers(slot) + p
            else
                bases = [bases, b]
                powers = [powers, p]
            end if
        end do
    end subroutine term_factors

    !> x**p * (... + y*x**(-q) + ...) -> (... + y*x**(p-q) + ...) when a term
    !> of the sum divides by x: the division disappears. Distributing a
    !> product over a sum adds no cancellation.
    recursive subroutine cancel_into_sums(a, factors, memo)
        type(arena_t), intent(inout) :: a
        integer, allocatable, intent(inout) :: factors(:)
        integer, intent(inout) :: memo(:)
        integer :: i, j, k, b, p, tb, tp, sum_id
        integer, allocatable :: terms(:), parts(:)
        logical :: divides

        do i = 1, size(factors)
            if (a%kind_of(factors(i)) /= NK_ADD) cycle
            sum_id = factors(i)
            do j = 1, size(factors)
                if (j == i .or. factors(j) < 0) cycle
                call base_power(a, factors(j), b, p)
                if (p <= 0 .or. a%kind_of(b) == NK_ADD) cycle
                divides = .false.
                do k = 1, a%nargs_of(sum_id)
                    call term_power(a, a%arg_of(sum_id, k), b, tp)
                    if (tp < 0) divides = .true.
                end do
                if (.not. divides) cycle
                allocate (terms(a%nargs_of(sum_id)))
                do k = 1, size(terms)
                    parts = [a%arg_of(sum_id, k), factors(j)]
                    if (a%kind_of(parts(1)) == NK_MUL) &
                        parts = [(a%arg_of(parts(1), tb), tb=1, a%nargs_of(parts(1))), factors(j)]
                    terms(k) = fold_node(a, a%mul(parts), memo)
                end do
                sum_id = fold_node(a, a%add(terms), memo)
                deallocate (terms)
                factors(j) = -1
            end do
            factors(i) = sum_id
        end do
        factors = pack(factors, factors > 0)
    end subroutine cancel_into_sums

    !> Integer power of base b in a term, zero if absent.
    subroutine term_power(a, term, b, power)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: term, b
        integer, intent(out) :: power
        integer, allocatable :: bases(:), powers(:)
        integer :: slot

        call term_factors(a, term, bases, powers)
        power = 0
        slot = findloc(bases, b, 1)
        if (slot > 0) power = powers(slot)
    end subroutine term_power

    !> A product with a factor b**p, p a negative integer.
    function has_reciprocal_factor(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical :: yes
        integer :: k, b, p

        yes = .false.
        if (a%kind_of(id) /= NK_MUL) return
        do k = 1, a%nargs_of(id)
            call base_power(a, a%arg_of(id, k), b, p)
            if (p < 0) yes = .true.
        end do
    end function has_reciprocal_factor

    !> b**p of a small rational b, or the unevaluated power.
    function numeric_power(a, base, p) result(out)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: base, p
        integer :: out, k
        integer(int64) :: n, d, pn, pd
        logical :: exact

        out = a%pow(base, a%int(int(p, int64)))
        call small_number(a, base, n, d, exact)
        if (.not. exact .or. n == 0_int64) return
        pn = 1_int64
        pd = 1_int64
        do k = 1, abs(p)
            call rational_mul(pn, pd, n, d, exact)
            if (.not. exact) return
        end do
        if (p < 0) then
            if (pn < 0_int64) then
                out = a%rat(-pd, -pn)
            else
                out = a%rat(pd, pn)
            end if
        else
            out = a%rat(pn, pd)
        end if
    end function numeric_power

    !> x**a * x**b -> x**(a + b) for integer exponents, including the factors
    !> of a power of a product when one of its bases recurs. A product has no
    !> cancellation, and a factor that cancels exactly is no longer rounded.
    subroutine merge_powers(a, factors)
        type(arena_t), intent(inout) :: a
        integer, allocatable, intent(inout) :: factors(:)
        integer, allocatable :: bases(:), powers(:), all_bases(:)
        integer :: i, j, k, b, p, m
        logical :: shared

        ! Every base that occurs at the top level of the product.
        allocate (all_bases(0))
        do i = 1, size(factors)
            call base_power(a, factors(i), b, p)
            all_bases = [all_bases, b]
        end do
        allocate (bases(0), powers(0))
        do i = 1, size(factors)
            call base_power(a, factors(i), b, p)
            if (a%kind_of(b) == NK_MUL .and. p /= 1) then
                shared = .false.
                do k = 1, a%nargs_of(b)
                    call base_power(a, a%arg_of(b, k), j, m)
                    if (any(all_bases == j)) shared = .true.
                end do
                if (shared) then
                    do k = 1, a%nargs_of(b)
                        call base_power(a, a%arg_of(b, k), j, m)
                        call add_power(bases, powers, j, m*p)
                    end do
                    cycle
                end if
            end if
            call add_power(bases, powers, b, p)
        end do
        if (size(bases) == size(factors) .and. all(powers /= 0)) then
            if (all(powers == [(power_of(a, factors(i)), i=1, size(factors))])) return
        end if
        deallocate (factors)
        allocate (factors(0))
        do i = 1, size(bases)
            if (powers(i) == 0) cycle
            if (powers(i) == 1) then
                factors = [factors, bases(i)]
            else
                factors = [factors, a%pow(bases(i), a%int(int(powers(i), int64)))]
            end if
        end do
    end subroutine merge_powers

    subroutine add_power(bases, powers, base, power)
        integer, allocatable, intent(inout) :: bases(:), powers(:)
        integer, intent(in) :: base, power
        integer :: slot

        slot = findloc(bases, base, 1)
        if (slot > 0) then
            powers(slot) = powers(slot) + power
        else
            bases = [bases, base]
            powers = [powers, power]
        end if
    end subroutine add_power

    integer function power_of(a, factor)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: factor
        integer :: base

        call base_power(a, factor, base, power_of)
    end function power_of

    !> factor = base**power with a small integer power (one if none).
    subroutine base_power(a, factor, base, power)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: factor
        integer, intent(out) :: base, power
        integer(int64) :: n, d
        logical :: exact

        base = factor
        power = 1
        if (a%kind_of(factor) /= NK_POW) return
        call small_number(a, a%arg_of(factor, 2), n, d, exact)
        if (.not. exact .or. d /= 1_int64 .or. abs(n) > 64_int64) return
        base = a%arg_of(factor, 1)
        power = int(n)
    end subroutine base_power

    !> c1*x + c2*x -> (c1 + c2)*x for exact small coefficients. Integer
    !> multiples of a term are exact, so this only replaces repeated
    !> additions of one term.
    subroutine merge_like_terms(a, terms)
        type(arena_t), intent(inout) :: a
        integer, allocatable, intent(inout) :: terms(:)
        integer, allocatable :: rest(:)
        integer(int64), allocatable :: cn(:), cd(:)
        logical, allocatable :: keep(:), merged(:)
        integer :: i, j
        logical :: exact

        allocate (rest(size(terms)), cn(size(terms)), cd(size(terms)))
        allocate (keep(size(terms)), merged(size(terms)), source=.true.)
        merged = .false.
        do i = 1, size(terms)
            call split_coefficient(a, terms(i), cn(i), cd(i), rest(i))
        end do
        do i = 1, size(terms)
            if (.not. keep(i)) cycle
            do j = i + 1, size(terms)
                if (.not. keep(j) .or. rest(j) /= rest(i)) cycle
                call rational_add(cn(i), cd(i), cn(j), cd(j), exact)
                if (.not. exact) cycle
                keep(j) = .false.
                merged(i) = .true.
            end do
        end do
        do i = 1, size(terms)
            if (.not. keep(i) .or. .not. merged(i)) cycle
            if (cn(i) == 0_int64) then
                keep(i) = .false.
            else if (cn(i) == 1_int64 .and. cd(i) == 1_int64) then
                terms(i) = rest(i)
            else
                terms(i) = a%mul([a%rat(cn(i), cd(i)), rest(i)])
            end if
        end do
        terms = pack(terms, keep)
    end subroutine merge_like_terms

    !> term = (cn/cd)*rest with rest free of a number factor.
    subroutine split_coefficient(a, term, cn, cd, rest)
        type(arena_t), intent(inout) :: a
        integer, intent(in) :: term
        integer(int64), intent(out) :: cn, cd
        integer, intent(out) :: rest
        integer, allocatable :: factors(:)
        integer :: k
        integer(int64) :: n, d
        logical :: number, found

        cn = 1_int64
        cd = 1_int64
        rest = term
        if (a%kind_of(term) /= NK_MUL) return
        found = .false.
        allocate (factors(0))
        do k = 1, a%nargs_of(term)
            call small_number(a, a%arg_of(term, k), n, d, number)
            if (number .and. .not. found) then
                cn = n
                cd = d
                found = .true.
            else
                factors = [factors, a%arg_of(term, k)]
            end if
        end do
        if (.not. found) return
        if (size(factors) == 1) then
            rest = factors(1)
        else
            rest = a%mul(factors)
        end if
    end subroutine split_coefficient

    !> Integer or rational node with numerator and denominator below 2**31,
    !> so that one product or sum cannot overflow.
    subroutine small_number(a, id, n, d, yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer(int64), intent(out) :: n, d
        logical, intent(out) :: yes
        integer(int64), parameter :: LIMIT = 2_int64**31

        n = 0_int64
        d = 1_int64
        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT)
            n = a%num_of(id)
        case (NK_RAT)
            n = a%num_of(id)
            d = a%den_of(id)
        case default
            return
        end select
        yes = abs(n) < LIMIT .and. d > 0_int64 .and. d < LIMIT
    end subroutine small_number

    subroutine rational_mul(cn, cd, n, d, ok)
        integer(int64), intent(inout) :: cn, cd
        integer(int64), intent(in) :: n, d
        logical, intent(out) :: ok
        integer(int64) :: pn, pd

        ok = .false.
        if (abs(cn) >= 2_int64**31 .or. cd >= 2_int64**31) return
        pn = cn*n
        pd = cd*d
        call reduce(pn, pd)
        cn = pn
        cd = pd
        ok = .true.
    end subroutine rational_mul

    subroutine rational_add(cn, cd, n, d, ok)
        integer(int64), intent(inout) :: cn, cd
        integer(int64), intent(in) :: n, d
        logical, intent(out) :: ok
        integer(int64) :: pn, pd

        ok = .false.
        if (abs(cn) >= 2_int64**31 .or. cd >= 2_int64**31) return
        pn = cn*d + n*cd
        pd = cd*d
        call reduce(pn, pd)
        cn = pn
        cd = pd
        ok = .true.
    end subroutine rational_add

    pure subroutine reduce(n, d)
        integer(int64), intent(inout) :: n, d
        integer(int64) :: x, y, t

        x = abs(n)
        y = d
        do while (y /= 0_int64)
            t = mod(x, y)
            x = y
            y = t
        end do
        if (x > 1_int64) then
            n = n/x
            d = d/x
        end if
    end subroutine reduce

    pure function is_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical :: yes

        yes = .false.
        if (.not. is_valid(e)) return
        if (e%a%kind_of(e%id) == NK_INT) yes = e%a%num_of(e%id) == 0_int64
    end function is_zero

    !> A product of atoms and integer powers of atoms.
    function is_monomial(e) result(yes)
        type(expr_t), intent(in) :: e
        logical :: yes
        type(expr_t) :: factor, base, exponent
        integer :: k

        yes = .false.
        if (e%kind() /= NK_MUL .and. e%kind() /= NK_POW) return
        do k = 1, merge(e%nargs(), 1, e%kind() == NK_MUL)
            factor = e
            if (e%kind() == NK_MUL) factor = e%arg(k)
            if (factor%kind() == NK_POW) then
                base = factor%arg(1)
                exponent = factor%arg(2)
                if (exponent%kind() /= NK_INT) return
                if (.not. is_atom(base)) return
            else if (.not. is_atom(factor)) then
                return
            end if
        end do
        yes = .true.
    end function is_monomial

    !> Values a name would not save anything over: numbers, symbols and
    !> partial derivatives of opaque functions, which are kernel inputs.
    function is_atom(e) result(yes)
        type(expr_t), intent(in) :: e
        logical :: yes
        character(:), allocatable :: name

        select case (e%kind())
        case (NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST)
            yes = .true.
        case (NK_FUNC)
            name = chars(e%name())
            yes = index(name, "Derivative") == 1
        case default
            yes = .false.
        end select
    end function is_atom

end module fortsym_chain
