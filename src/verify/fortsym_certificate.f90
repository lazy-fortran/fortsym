module fortsym_certificate
    ! Complementary variational bounds for K f = s with K = S + A, S symmetric
    ! and positive semidefinite, A skew, derived symbolically with build-time
    ! proof obligations (doc/certificate.md).
    !
    ! The space splits by parity; S preserves parity, A flips it. For an even
    ! source s the output q = <s, K^{-1} s> equals <s, Q^{-1} s> with the
    ! Schur complement Q = S_e + A^* S_o^{-1} A, and
    !
    !   L(u) = 2<s,u> - <u,S_e u> - <A u, S_o^{-1} A u>   <=  q   (any even u)
    !   U(w) = <r, S_e^+ r> + <w, S_o w>                   >=  q   (odd w, P_0 r = 0)
    !
    ! with r = s + A w and P_0 the projection onto the null space of S_e. An
    ! odd source enters through t = A^* S_o^{-1} s_o. Mixed coefficients follow
    ! by polarisation. The module works on two representations:
    !
    ! * a finite split with diagonal S in an orthonormal basis
    !   (`operator_split_t`): fortsym derives L, U, the constraint rows, and,
    !   for small systems, the exact q and the optimal trials, and checks that
    !   the derived functionals are tight at the optimum;
    ! * a ladder (`ladder obligations`): levels l = 0, 1, 2, ... with parity
    !   (-1)**l, S = sigma(l) on level l, and A coupling l to l+1 through
    !   first-order operators a D + b in a derivation D with constant
    !   coefficients on a torus. fortsym derives the weighted adjoint by
    !   integration by parts and checks skewness symbolically in l, the null
    !   level, and the l = 0 constraint (integrating factor and zero mean).
    !
    ! Every derived formula comes with obligations recorded in an
    ! `obligation_ledger_t`; a consumer refuses to emit kernels unless the
    ! ledger holds.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_string, only: str_t, str, chars
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_subs, only: subs_many
    use fortsym_numeric, only: numeric_precision_text
    use fortsym_obligation, only: obligation_ledger_t, obligation_t, prove_zero, &
        record_obligation, opaque_leaves, OBLIGATION_PROVED, OBLIGATION_PROBED, &
        OBLIGATION_FAILED
    implicit none
    private

    public :: operator_split_t, split_bounds_t, derivation_t, first_order_t
    public :: split_obligations, split_bounds, split_exact, odd_source_transfer
    public :: polarisation, apply_derivation, derivation_symbol
    public :: first_order_adjoint, ladder_obligations, constraint_obligations
    public :: prove_positive, split_indicators, weighted_adjoint
    public :: dwr_t, split_dwr, dwr_exact, dwr_gradient

    !> Finite split: S = diag(sigma) in an orthonormal basis, A a matrix,
    !> parity(i) = 0 (even) or 1 (odd).
    type :: operator_split_t
        type(expr_t), allocatable :: sigma(:)
        type(expr_t), allocatable :: a(:, :)
        integer, allocatable :: parity(:)
    end type operator_split_t

    type :: split_bounds_t
        type(expr_t) :: lower
        type(expr_t) :: upper
        type(expr_t), allocatable :: constraints(:)
        !> Trial symbols: u for the even components, w for the odd ones.
        type(expr_t), allocatable :: u(:), w(:)
        integer, allocatable :: even(:), odd(:), null(:)
    end type split_bounds_t

    !> D = sum_k c_k d/dx_k with constant c_k. On the torus a Fourier mode
    !> exp(i kappa . x) is an eigenfunction with eigenvalue i sum_k c_k kappa_k.
    type :: derivation_t
        type(expr_t), allocatable :: coords(:)
        type(expr_t), allocatable :: coeffs(:)
        type(expr_t), allocatable :: wavenumbers(:)
    end type derivation_t

    !> Dual-weighted-residual data for J = <a, K^{-1} b> with K = S + A
    !> (residual pairing, doc/certificate.md): for every trial f with
    !> P_0 (b - K f) = 0 and g with P_0 (a - K^* g) = 0,
    !>   |J - estimate| <= sqrt(primal_norm2 * dual_norm2).
    type :: dwr_t
        type(expr_t) :: estimate
        type(expr_t) :: primal_norm2
        type(expr_t) :: dual_norm2
        type(expr_t), allocatable :: primal_constraints(:), dual_constraints(:)
        !> Per-level contributions r_i**2/sigma_i of the two squared norms.
        type(expr_t), allocatable :: primal_indicators(:), dual_indicators(:)
        type(expr_t), allocatable :: f(:), g(:)
    end type dwr_t

    !> The first-order operator a D + b.
    type :: first_order_t
        type(expr_t) :: a
        type(expr_t) :: b
    end type first_order_t

contains

    function simp(e) result(r)
        type(expr_t), intent(in) :: e
        type(expr_t) :: r
        type(native_engine_t) :: eng
        type(engine_result_t) :: res

        eng = make_native_engine(e%a)
        res = eng%simplify(e)
        if (res%ok) then
            r = res%value
        else
            r = e
        end if
    end function simp

    function dif(e, x) result(r)
        type(expr_t), intent(in) :: e, x
        type(expr_t) :: r
        type(native_engine_t) :: eng
        type(engine_result_t) :: res

        eng = make_native_engine(e%a)
        res = eng%diff(e, x)
        if (.not. res%ok) error stop "fortsym_certificate: differentiation failed"
        r = res%value
    end function dif

    logical function is_zero(e)
        type(expr_t), intent(in) :: e
        type(native_engine_t) :: eng
        type(engine_result_t) :: res

        eng = make_native_engine(e%a)
        res = eng%zero_test(e)
        is_zero = res%ok .and. res%verdict == VERDICT_TRUE
    end function is_zero

    function itoa(n) result(text)
        integer, intent(in) :: n
        character(:), allocatable :: text
        character(len=16) :: buf

        write (buf, '(i0)') n
        text = trim(buf)
    end function itoa

    !> Record `e > 0`: exact for numbers, otherwise a probe at random
    !> positive rational values of every free symbol (labelled PROBED).
    subroutine prove_positive(ledger, label, e)
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e
        type(obligation_t) :: item
        type(expr_t), allocatable :: olds(:), news(:)
        type(expr_t) :: v
        character(:), allocatable :: text, why
        integer :: k, j, n
        integer(int64) :: seed
        real(kind(1.0d0)) :: value
        logical :: ok

        call leaf_exprs(e, olds)
        n = size(olds)
        allocate (news(n))
        seed = 7654321_int64 + int(ledger%n, int64)
        item%label = str(label)
        item%status = OBLIGATION_PROBED
        item%evidence = str("positive at 8 random positive rational points")
        if (n == 0) item%status = OBLIGATION_PROVED
        if (n == 0) item%evidence = str("exact number")
        do k = 1, merge(1, 8, n == 0)
            do j = 1, n
                seed = mod(48271_int64*seed, 2147483647_int64)
                news(j) = rat_of(e, 1_int64 + mod(seed, 4096_int64), 1024_int64)
            end do
            v = e
            if (n > 0) v = subs_many(e, olds, news)
            call numeric_precision_text(v, 30, text, ok, why)
            if (ok) read (text, *) value
            if (.not. ok) value = -1.0d0
            if (.not. (value > 0.0d0)) then
                item%status = OBLIGATION_FAILED
                item%evidence = str("not positive at a probe point")
                exit
            end if
        end do
        call record_obligation(ledger, item)
    end subroutine prove_positive

    subroutine leaf_exprs(e, leaves)
        type(expr_t), intent(in) :: e
        type(expr_t), allocatable, intent(out) :: leaves(:)
        integer, allocatable :: ids(:)
        integer :: k

        call opaque_leaves(e, ids)
        allocate (leaves(size(ids)))
        do k = 1, size(ids)
            leaves(k)%a => e%a
            leaves(k)%id = ids(k)
            leaves(k)%generation = e%generation
        end do
    end subroutine leaf_exprs

    function rat_of(e, p, q) result(r)
        type(expr_t), intent(in) :: e
        integer(int64), intent(in) :: p, q
        type(expr_t) :: r

        r = num(e%a, p)/num(e%a, q)
    end function rat_of


    !> Obligations of a finite split: skewness of A, parity pattern, sigma >= 0
    !> with the even null set proven zero and the odd sigma positive.
    subroutine split_obligations(p, ledger, label)
        type(operator_split_t), intent(in) :: p
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        integer :: i, j, n

        n = size(p%sigma)
        do i = 1, n
            do j = i, n
                call prove_zero(ledger, label//": A skew ("//itoa(i)//","//itoa(j)//")", &
                    p%a(i, j) + p%a(j, i))
                if (p%parity(i) == p%parity(j) .and. i /= j) &
                    call prove_zero(ledger, label//": A flips parity ("//itoa(i)// &
                    ","//itoa(j)//")", p%a(i, j))
            end do
        end do
        do i = 1, n
            if (p%parity(i) == 0 .and. is_zero(p%sigma(i))) then
                call prove_zero(ledger, label//": null level "//itoa(i), p%sigma(i))
            else
                call prove_positive(ledger, label//": sigma("//itoa(i)//") > 0", &
                    p%sigma(i))
            end if
        end do
    end subroutine split_obligations

    !> L(u), U(w) and the constraint rows P_0 (s + A w) for an even source.
    function split_bounds(p, source, prefix) result(b)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: source(:)
        character(*), intent(in) :: prefix
        type(split_bounds_t) :: b
        type(expr_t) :: zero, au, r
        integer :: i, j, n, k

        n = size(p%sigma)
        zero = num(p%sigma(1)%a, 0_int64)
        b%even = pack([(i, i=1, n)], p%parity == 0)
        b%odd = pack([(i, i=1, n)], p%parity == 1)
        allocate (b%null(0))
        do k = 1, size(b%even)
            if (is_zero(p%sigma(b%even(k)))) b%null = [b%null, b%even(k)]
        end do
        allocate (b%u(size(b%even)), b%w(size(b%odd)))
        do k = 1, size(b%even)
            b%u(k) = sym(p%sigma(1)%a, prefix//"u"//itoa(b%even(k)))
        end do
        do k = 1, size(b%odd)
            b%w(k) = sym(p%sigma(1)%a, prefix//"w"//itoa(b%odd(k)))
        end do
        b%lower = zero
        do k = 1, size(b%even)
            i = b%even(k)
            b%lower = b%lower + 2*source(i)*b%u(k) - p%sigma(i)*b%u(k)**2
        end do
        do k = 1, size(b%odd)
            i = b%odd(k)
            au = zero
            do j = 1, size(b%even)
                au = au + p%a(i, b%even(j))*b%u(j)
            end do
            b%lower = b%lower - au**2/p%sigma(i)
        end do
        b%upper = zero
        allocate (b%constraints(0))
        do k = 1, size(b%even)
            i = b%even(k)
            r = source(i)
            do j = 1, size(b%odd)
                r = r + p%a(i, b%odd(j))*b%w(j)
            end do
            if (any(b%null == i)) then
                b%constraints = [b%constraints, r]
            else
                b%upper = b%upper + r**2/p%sigma(i)
            end if
        end do
        do k = 1, size(b%odd)
            i = b%odd(k)
            b%upper = b%upper + p%sigma(i)*b%w(k)**2
        end do
    end function split_bounds

    !> Even transfer of an odd source: t = A^* S_o^{-1} s_o (zero on odd rows).
    function odd_source_transfer(p, source) result(t)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: source(:)
        type(expr_t), allocatable :: t(:)
        integer :: i, j, n

        n = size(p%sigma)
        allocate (t(n))
        do i = 1, n
            t(i) = num(p%sigma(1)%a, 0_int64)
            if (p%parity(i) /= 0) cycle
            do j = 1, n
                if (p%parity(j) /= 1) cycle
                ! (A^*)_{ij} = A_{ji} in an orthonormal real basis.
                t(i) = t(i) + p%a(j, i)*source(j)/p%sigma(j)
            end do
            t(i) = simp(t(i))
        end do
    end function odd_source_transfer

    !> <x, Q^{-1} y> = (q(alpha x + y/alpha) - q(alpha x - y/alpha))/4 for any
    !> alpha > 0; bracket endpoints combine as [(lo+ - hi-)/4, (hi+ - lo-)/4].
    function polarisation(q_plus, q_minus) result(q)
        type(expr_t), intent(in) :: q_plus, q_minus
        type(expr_t) :: q

        q = (q_plus - q_minus)/4
    end function polarisation

    !> Exact q, optimal trials, and the obligations that the derived L and U
    !> are tight at the optimum. Symbolic Gauss-Jordan elimination on the
    !> Schur complement; the elimination is not trusted, its solution is
    !> checked by substitution.
    subroutine split_exact(p, source, bounds, ledger, label, q, u_opt, w_opt)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: source(:)
        type(split_bounds_t), intent(in) :: bounds
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(expr_t), intent(out) :: q
        type(expr_t), allocatable, intent(out) :: u_opt(:), w_opt(:)
        type(expr_t), allocatable :: qm(:, :), aug(:, :), olds(:), news(:)
        type(expr_t) :: zero, res
        integer :: ne, no, i, j, k

        ne = size(bounds%even)
        no = size(bounds%odd)
        zero = num(p%sigma(1)%a, 0_int64)
        allocate (qm(ne, ne), aug(ne, ne + 1))
        do i = 1, ne
            do j = 1, ne
                qm(i, j) = zero
                if (i == j) qm(i, j) = p%sigma(bounds%even(i))
                do k = 1, no
                    qm(i, j) = qm(i, j) + p%a(bounds%odd(k), bounds%even(i))* &
                        p%a(bounds%odd(k), bounds%even(j))/p%sigma(bounds%odd(k))
                end do
                qm(i, j) = simp(qm(i, j))
                aug(i, j) = qm(i, j)
            end do
            aug(i, ne + 1) = source(bounds%even(i))
        end do
        call solve_symbolic(aug, ne)
        allocate (u_opt(ne), w_opt(no))
        q = zero
        do i = 1, ne
            u_opt(i) = aug(i, ne + 1)
            q = q + source(bounds%even(i))*u_opt(i)
        end do
        q = simp(q)
        do k = 1, no
            w_opt(k) = zero
            do j = 1, ne
                w_opt(k) = w_opt(k) + p%a(bounds%odd(k), bounds%even(j))*u_opt(j)
            end do
            w_opt(k) = simp(w_opt(k)/p%sigma(bounds%odd(k)))
        end do
        do i = 1, ne
            res = -source(bounds%even(i))
            do j = 1, ne
                res = res + qm(i, j)*u_opt(j)
            end do
            call prove_zero(ledger, label//": Schur solve row "//itoa(i), res)
        end do
        olds = [bounds%u, bounds%w]
        news = [u_opt, w_opt]
        call prove_zero(ledger, label//": Ritz bound tight at the optimum", &
            subs_many(bounds%lower, olds, news) - q)
        call prove_zero(ledger, label//": complementary bound tight at the optimum", &
            subs_many(bounds%upper, olds, news) - q)
        do k = 1, size(bounds%constraints)
            call prove_zero(ledger, label//": optimum satisfies constraint "//itoa(k), &
                subs_many(bounds%constraints(k), olds, news))
        end do
    end subroutine split_exact

    !> Gauss-Jordan on an n x (n+1) augmented symbolic matrix; the last
    !> column returns the solution. Pivots are the first entries not proven
    !> zero. The result is untrusted: callers check it by substitution.
    subroutine solve_symbolic(aug, ne)
        type(expr_t), intent(inout) :: aug(:, :)
        integer, intent(in) :: ne
        type(expr_t), allocatable :: row(:)
        type(expr_t) :: f
        integer :: i, j, k, piv

        do k = 1, ne
            piv = 0
            do i = k, ne
                if (.not. is_zero(aug(i, k))) then
                    piv = i
                    exit
                end if
            end do
            if (piv == 0) error stop "fortsym_certificate: singular symbolic system"
            if (piv /= k) then
                row = aug(k, :)
                aug(k, :) = aug(piv, :)
                aug(piv, :) = row
            end if
            do j = k + 1, ne + 1
                aug(k, j) = simp(aug(k, j)/aug(k, k))
            end do
            aug(k, k) = num(aug(1, 1)%a, 1_int64)
            do i = 1, ne
                if (i == k) cycle
                f = aug(i, k)
                do j = k, ne + 1
                    aug(i, j) = simp(aug(i, j) - f*aug(k, j))
                end do
            end do
        end do
    end subroutine solve_symbolic

    !> Nonnegative per-level contributions to the certificate gap:
    !>   U(w) - L(u) = sum_i eta_i  on admissible w,
    !> eta_i = (r_i - sigma_i u_i)**2/sigma_i on even non-null levels and
    !> (sigma_i w_i - (A u)_i)**2/sigma_i on odd levels, zero on null levels.
    !> The identity is recorded as an obligation.
    function split_indicators(p, source, b, ledger, label) result(eta)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: source(:)
        type(split_bounds_t), intent(in) :: b
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(expr_t), allocatable :: eta(:)
        type(expr_t) :: zero, r, au, total, nullpart
        integer :: i, j, k

        zero = num(p%sigma(1)%a, 0_int64)
        allocate (eta(size(p%sigma)))
        eta = zero
        total = zero
        nullpart = zero
        do k = 1, size(b%even)
            i = b%even(k)
            r = source(i)
            do j = 1, size(b%odd)
                r = r + p%a(i, b%odd(j))*b%w(j)
            end do
            if (any(b%null == i)) then
                nullpart = nullpart - 2*r*b%u(k)
            else
                eta(i) = (r - p%sigma(i)*b%u(k))**2/p%sigma(i)
            end if
        end do
        do k = 1, size(b%odd)
            i = b%odd(k)
            au = zero
            do j = 1, size(b%even)
                au = au + p%a(i, b%even(j))*b%u(j)
            end do
            eta(i) = (p%sigma(i)*b%w(k) - au)**2/p%sigma(i)
        end do
        do i = 1, size(eta)
            total = total + eta(i)
        end do
        call prove_zero(ledger, label//": gap is the sum of level indicators", &
            b%upper - b%lower - total - nullpart)
    end function split_indicators

    !> Adjoint of a matrix operator in the inner product <x, y> = sum_i
    !> weight_i x_i y_i: (K^*)_{ij} = K_{ji} weight_j / weight_i.
    function weighted_adjoint(k, weight) result(ks)
        type(expr_t), intent(in) :: k(:, :), weight(:)
        type(expr_t), allocatable :: ks(:, :)
        integer :: i, j

        allocate (ks(size(k, 2), size(k, 1)))
        do i = 1, size(k, 2)
            do j = 1, size(k, 1)
                ks(i, j) = simp(k(j, i)*weight(j)/weight(i))
            end do
        end do
    end function weighted_adjoint

    function split_operator(p) result(k)
        type(operator_split_t), intent(in) :: p
        type(expr_t), allocatable :: k(:, :)
        integer :: i

        k = p%a
        do i = 1, size(p%sigma)
            k(i, i) = k(i, i) + p%sigma(i)
        end do
    end function split_operator

    !> Residual-pairing (dual-weighted-residual) functionals for
    !> J = <a, K^{-1} b>, K = S + A, in terms of trial symbols f and g.
    function split_dwr(p, a_src, b_src, prefix) result(d)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: a_src(:), b_src(:)
        character(*), intent(in) :: prefix
        type(dwr_t) :: d
        type(expr_t), allocatable :: k(:, :)
        type(expr_t) :: zero, rb, ra
        integer :: i, j, n

        n = size(p%sigma)
        k = split_operator(p)
        zero = num(p%sigma(1)%a, 0_int64)
        allocate (d%f(n), d%g(n), d%primal_indicators(n), d%dual_indicators(n))
        allocate (d%primal_constraints(0), d%dual_constraints(0))
        do i = 1, n
            d%f(i) = sym(zero%a, prefix//"f"//itoa(i))
            d%g(i) = sym(zero%a, prefix//"g"//itoa(i))
        end do
        d%estimate = zero
        d%primal_norm2 = zero
        d%dual_norm2 = zero
        do i = 1, n
            rb = b_src(i)
            ra = a_src(i)
            do j = 1, n
                rb = rb - k(i, j)*d%f(j)
                ra = ra - k(j, i)*d%g(j)
            end do
            d%estimate = d%estimate + a_src(i)*d%f(i) + d%g(i)*rb
            if (is_zero(p%sigma(i))) then
                d%primal_constraints = [d%primal_constraints, rb]
                d%dual_constraints = [d%dual_constraints, ra]
                d%primal_indicators(i) = zero
                d%dual_indicators(i) = zero
            else
                d%primal_indicators(i) = rb**2/p%sigma(i)
                d%dual_indicators(i) = ra**2/p%sigma(i)
                d%primal_norm2 = d%primal_norm2 + d%primal_indicators(i)
                d%dual_norm2 = d%dual_norm2 + d%dual_indicators(i)
            end if
        end do
    end function split_dwr

    !> Exact J, primal and dual solutions (small systems), and the
    !> obligations that the estimate is exact and both residual norms and
    !> constraints vanish there.
    subroutine dwr_exact(p, a_src, b_src, d, ledger, label, j_exact, f_opt, g_opt)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: a_src(:), b_src(:)
        type(dwr_t), intent(in) :: d
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(expr_t), intent(out) :: j_exact
        type(expr_t), allocatable, intent(out) :: f_opt(:), g_opt(:)
        type(expr_t), allocatable :: k(:, :), aug(:, :), olds(:), news(:)
        integer :: i, n

        n = size(p%sigma)
        k = split_operator(p)
        allocate (aug(n, n + 1))
        aug(:, 1:n) = k
        aug(:, n + 1) = b_src
        call solve_symbolic(aug, n)
        f_opt = aug(:, n + 1)
        aug(:, 1:n) = transpose(k)
        aug(:, n + 1) = a_src
        call solve_symbolic(aug, n)
        g_opt = aug(:, n + 1)
        j_exact = num(k(1, 1)%a, 0_int64)
        do i = 1, n
            j_exact = j_exact + a_src(i)*f_opt(i)
        end do
        j_exact = simp(j_exact)
        olds = [d%f, d%g]
        news = [f_opt, g_opt]
        call prove_zero(ledger, label//": DWR estimate exact at the solutions", &
            subs_many(d%estimate, olds, news) - j_exact)
        call prove_zero(ledger, label//": primal residual vanishes", &
            subs_many(d%primal_norm2, olds, news))
        call prove_zero(ledger, label//": dual residual vanishes", &
            subs_many(d%dual_norm2, olds, news))
        do i = 1, size(d%primal_constraints)
            call prove_zero(ledger, label//": primal constraint at the solution", &
                subs_many(d%primal_constraints(i), olds, news))
            call prove_zero(ledger, label//": dual constraint at the solution", &
                subs_many(d%dual_constraints(i), olds, news))
        end do
    end subroutine dwr_exact

    !> dJ/dtheta = <a_theta, f> + <g, b_theta - K_theta f> (exact at f =
    !> K^{-1} b, g = K^{-*} a; the same dual information as the DWR estimate).
    function dwr_gradient(p, a_src, b_src, d, theta) result(dj)
        type(operator_split_t), intent(in) :: p
        type(expr_t), intent(in) :: a_src(:), b_src(:), theta
        type(dwr_t), intent(in) :: d
        type(expr_t) :: dj
        type(expr_t), allocatable :: k(:, :)
        type(expr_t) :: rb
        integer :: i, j, n

        n = size(p%sigma)
        k = split_operator(p)
        dj = num(theta%a, 0_int64)
        do i = 1, n
            rb = dif(b_src(i), theta)
            do j = 1, n
                rb = rb - dif(k(i, j), theta)*d%f(j)
            end do
            dj = dj + dif(a_src(i), theta)*d%f(i) + d%g(i)*rb
        end do
    end function dwr_gradient

    !> D f = sum_k c_k df/dx_k.
    function apply_derivation(d, f) result(r)
        type(derivation_t), intent(in) :: d
        type(expr_t), intent(in) :: f
        type(expr_t) :: r
        integer :: k

        r = num(f%a, 0_int64)
        do k = 1, size(d%coords)
            r = r + d%coeffs(k)*dif(f, d%coords(k))
        end do
    end function apply_derivation

    !> The Fourier symbol: D exp(i kappa . x) = i derivation_symbol exp(i kappa . x).
    function derivation_symbol(d) result(r)
        type(derivation_t), intent(in) :: d
        type(expr_t) :: r
        integer :: k

        r = num(d%coeffs(1)%a, 0_int64)
        do k = 1, size(d%coeffs)
            r = r + d%coeffs(k)*d%wavenumbers(k)
        end do
    end function derivation_symbol

    !> Adjoint of a D + b in L^2(weight dx) on the torus, by integration by
    !> parts with constant-coefficient D: (a D + b)^* = -a D + b - D(a w)/w.
    function first_order_adjoint(op, d, weight) result(adj)
        type(first_order_t), intent(in) :: op
        type(derivation_t), intent(in) :: d
        type(expr_t), intent(in) :: weight
        type(first_order_t) :: adj

        adj%a = -op%a
        adj%b = simp(op%b - apply_derivation(d, op%a*weight)/weight)
    end function first_order_adjoint

    !> Ladder obligations, symbolic in the level: constant coefficients of D,
    !> sigma(0) = 0 (the null level), sigma > 0 above it, and skewness
    !> A_{l+1,l} = -(A_{l,l+1})^* in the weighted inner product.
    subroutine ladder_obligations(ledger, label, d, weight, sigma, up, dn, level)
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(derivation_t), intent(in) :: d
        type(expr_t), intent(in) :: weight, sigma, level
        type(first_order_t), intent(in) :: up, dn
        type(first_order_t) :: adj
        integer :: k, j

        do k = 1, size(d%coeffs)
            do j = 1, size(d%coords)
                call prove_zero(ledger, label//": derivation coefficient "//itoa(k)// &
                    " constant in coordinate "//itoa(j), dif(d%coeffs(k), d%coords(j)))
            end do
        end do
        call prove_zero(ledger, label//": sigma vanishes on level 0", &
            subs_many(sigma, [level], [num(level%a, 0_int64)]))
        call prove_positive(ledger, label//": sigma > 0 above level 0", sigma)
        adj = first_order_adjoint(up, d, weight)
        call prove_zero(ledger, label//": skew, derivative part", dn%a + adj%a)
        call prove_zero(ledger, label//": skew, multiplier part", dn%b + adj%b)
    end subroutine ladder_obligations

    !> The level-0 constraint s_0 + A_{0,1} w_1 = 0 with w_1 = mu F becomes
    !> D F = g. Obligations: mu is an integrating factor (a D mu + b mu = 0)
    !> and g is a divergence (sum_k d potentials(k)/dx_k), so its torus mean
    !> vanishes and every nonresonant Fourier mode is solvable. Returns g.
    function constraint_obligations(ledger, label, d, up0, s0, mu, potentials) &
            result(g)
        type(obligation_ledger_t), intent(inout) :: ledger
        character(*), intent(in) :: label
        type(derivation_t), intent(in) :: d
        type(first_order_t), intent(in) :: up0
        type(expr_t), intent(in) :: s0, mu
        type(expr_t), intent(in) :: potentials(:)
        type(expr_t) :: g, div
        integer :: k

        call prove_zero(ledger, label//": integrating factor", &
            up0%a*apply_derivation(d, mu) + up0%b*mu)
        g = simp(-s0/(up0%a*mu))
        div = num(g%a, 0_int64)
        do k = 1, size(potentials)
            div = div + dif(potentials(k), d%coords(k))
        end do
        call prove_zero(ledger, label//": right-hand side is a divergence", g - div)
    end function constraint_obligations

end module fortsym_certificate
