!> Complementary bounds for a small linear system K f = s with K = S + A,
!> S diagonal and positive semidefinite, A skew and parity-flipping.
!>
!> fortsym derives the Ritz functional L(u), the complementary functional
!> U(w) with its constraint, and the exact output q = <s, K^{-1} s>; it proves
!> the structural obligations (skewness, parity, null level, positivity) and
!> that both functionals are tight at the optimum. The example then checks q
!> against an independent dense Gaussian elimination in real64, checks the
!> bracket L(u) <= q <= U(w) at perturbed trial vectors, and emits the float
!> and rigorous kernels of L and U.
program example_certificate_matrix
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym, only: arena_t, expr_t, sym, num, rat, operator(+), operator(-), &
        operator(*), operator(/), operator(**), numeric_value, str, chars
    use fortsym_subs, only: subs_many
    use fortsym_obligation, only: obligation_ledger_t, ledger_holds, ledger_report
    use fortsym_certificate, only: operator_split_t, split_bounds_t, &
        split_obligations, split_bounds, split_exact
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, ball_runtime, &
        emit_rigorous_kernel, emit_float_kernel
    implicit none

    integer, parameter :: dp = real64, n = 4
    type(arena_t), target :: arena
    type(operator_split_t) :: split
    type(split_bounds_t) :: b
    type(obligation_ledger_t) :: ledger
    type(expr_t) :: p, zero, s(n), q, pval
    type(expr_t), allocatable :: u_opt(:), w_opt(:)
    real(dp) :: kmat(n, n), f(n), sv(n), qd, qs, lo, hi
    integer :: i, j, failures

    failures = 0
    p = sym(arena, "p")
    zero = num(arena, 0_int64)
    ! Levels 1 and 3 are even, 2 and 4 odd; level 1 is the null level of S.
    split%parity = [0, 1, 0, 1]
    split%sigma = [zero, num(arena, 2_int64), num(arena, 3_int64), r(5, 2)]
    allocate (split%a(n, n))
    split%a = zero
    call couple(1, 2, num(arena, 1_int64))
    call couple(3, 2, p)
    call couple(3, 4, num(arena, 2_int64))
    call couple(1, 4, r(1, 2))
    s = [num(arena, 1_int64), zero, num(arena, 2_int64), zero]

    call split_obligations(split, ledger, "matrix split")
    b = split_bounds(split, s, "t")
    call split_exact(split, s, b, ledger, "matrix split", q, u_opt, w_opt)
    call ledger_report(ledger)
    if (.not. ledger_holds(ledger)) failures = failures + 1

    ! Independent oracle: dense real64 elimination of K f = s at p = 3/2.
    pval = r(3, 2)
    kmat = 0.0_dp
    do i = 1, n
        do j = 1, n
            kmat(i, j) = value(subs_many(split%a(i, j), [p], [pval]))
        end do
        kmat(i, i) = kmat(i, i) + value(split%sigma(i))
        sv(i) = value(s(i))
    end do
    call gauss(kmat, sv, f)
    qd = dot_product(sv, f)
    qs = value(subs_many(q, [p], [pval]))
    print '(a,es23.15,a,es23.15)', "q symbolic ", qs, "   q dense ", qd
    if (abs(qs - qd) > 1.0e-13_dp*abs(qd)) failures = failures + 1

    call bracket_at_perturbed_trials(lo, hi)
    print '(a,es23.15,a,es23.15)', "L(u) ", lo, "   U(w) ", hi
    if (.not. (lo <= qs .and. qs <= hi)) failures = failures + 1

    call emit_kernels()
    if (failures > 0) error stop 1
    print '(a)', "example_certificate_matrix: all checks passed"

contains

    function r(a, c) result(e)
        integer, intent(in) :: a, c
        type(expr_t) :: e

        e = rat(arena, int(a, int64), int(c, int64))
    end function r

    subroutine couple(i, j, v)
        integer, intent(in) :: i, j
        type(expr_t), intent(in) :: v

        split%a(i, j) = v
        split%a(j, i) = -v
    end subroutine couple

    real(dp) function value(e)
        type(expr_t), intent(in) :: e
        logical :: ok
        character(:), allocatable :: why

        call numeric_value(e, value, ok, why)
        if (.not. ok) error stop "example_certificate_matrix: no numeric value"
    end function value

    subroutine gauss(a, rhs, x)
        real(dp), intent(in) :: a(:, :), rhs(:)
        real(dp), intent(out) :: x(:)
        real(dp) :: m(size(rhs), size(rhs) + 1), row(size(rhs) + 1)
        integer :: k, i, piv, nn

        nn = size(rhs)
        m(:, 1:nn) = a
        m(:, nn + 1) = rhs
        do k = 1, nn
            piv = k - 1 + maxloc(abs(m(k:nn, k)), 1)
            row = m(k, :)
            m(k, :) = m(piv, :)
            m(piv, :) = row
            do i = k + 1, nn
                m(i, :) = m(i, :) - m(i, k)/m(k, k)*m(k, :)
            end do
        end do
        do k = nn, 1, -1
            x(k) = (m(k, nn + 1) - dot_product(m(k, k + 1:nn), x(k + 1:nn)))/m(k, k)
        end do
    end subroutine gauss

    !> Perturb the optimal trials; keep w admissible by solving the single
    !> constraint row for the w attached to level 2.
    subroutine bracket_at_perturbed_trials(lo, hi)
        real(dp), intent(out) :: lo, hi
        type(expr_t) :: u(2), w(2), c
        integer :: k

        do k = 1, 2
            u(k) = subs_many(u_opt(k), [p], [pval]) + r(k, 16)
        end do
        w(2) = subs_many(w_opt(2), [p], [pval]) - r(1, 8)
        ! constraint: s_1 + A(1,2) w_2 + A(1,4) w_4 = 0
        c = subs_many(b%constraints(1), [b%w(2), p], [w(2), pval])
        w(1) = -subs_many(c, [b%w(1)], [zero])/ &
            subs_many(split%a(1, 2), [p], [pval])
        lo = value(subs_many(b%lower, [b%u, p], [u, pval]))
        hi = value(subs_many(b%upper, [b%w, p], [w, pval]))
        if (abs(value(subs_many(b%constraints(1), [b%w, p], [w, pval]))) > 0.0_dp) &
            error stop "perturbed w is not admissible"
    end subroutine bracket_at_perturbed_trials

    subroutine emit_kernels()
        type(rigorous_kernel_spec_t) :: spec
        logical :: ok1, ok2
        character(:), allocatable :: why

        spec%name = str("ritz_lower")
        spec%args = [b%u(1)%a%name_of(b%u(1)%id), b%u(2)%a%name_of(b%u(2)%id), str("p")]
        spec%outputs = [str("lower")]
        spec%runtime = ball_runtime()
        print '(a)', chars(emit_rigorous_kernel([b%lower], spec, ok1, why))
        spec%name = str("complementary_upper_f")
        spec%args = [b%w(1)%a%name_of(b%w(1)%id), b%w(2)%a%name_of(b%w(2)%id), str("p")]
        spec%outputs = [str("upper")]
        print '(a)', chars(emit_float_kernel([b%upper], spec, ok2, why))
        if (.not. (ok1 .and. ok2)) failures = failures + 1
    end subroutine emit_kernels

end program example_certificate_matrix
