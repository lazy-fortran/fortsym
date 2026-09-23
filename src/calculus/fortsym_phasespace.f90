module fortsym_phasespace
    ! First-order (phase-space) Lagrangian mechanics, natively.
    !
    ! A phase-space Lagrangian is a one-form
    !     L = sum_i a_i(q) qdot_i - h(q)
    ! over coordinates q_1..q_n, where a_i and h are arbitrary symbolic
    ! functions of q (and, through the caller's own expression, of t). This is
    ! the "first-order" or "symplectic potential" form used whenever the
    ! equations of motion are already first order in time and no separate
    ! momentum variable is wanted: the a_i already play that role.
    !
    ! The Lagrange (symplectic) two-form
    !     omega_ij = d_i a_j - d_j a_i
    ! is antisymmetric by construction. Its Pfaffian Pf(omega) satisfies
    ! det(omega) = Pf(omega)**2 and is the Liouville / phase-space density:
    ! the equations of motion omega qdot = grad h preserve the volume form
    ! Pf(omega) dq_1 ^ ... ^ dq_n.
    !
    ! Nothing here names a physical system. A caller assembling a magnetic
    ! field, a spin system, or any other first-order mechanics supplies its
    ! own a_i and h; this module only knows the general two-form algebra.
    use fortsym_expr, only: expr_t, num, is_valid, same_arena, &
        operator(+), operator(-), operator(*), operator(/)
    use fortsym_diff, only: diff
    use fortsym_matrix, only: from_matrix, to_matrix, matrix_inverse
    use fortsym_string, only: str_t, chars
    implicit none
    private

    public :: phase_lagrangian_t, phase_lagrangian_create
    public :: symplectic_form, pfaffian, phase_space_rates, noether_rate
    public :: reparametrize_rates, el_residual

    !> A first-order (phase-space) Lagrangian L = sum_i a_i(q) qdot_i - h(q).
    type :: phase_lagrangian_t
        integer :: n = 0
        type(expr_t), allocatable :: q(:)
        type(expr_t), allocatable :: a(:)
        type(expr_t) :: h
    end type phase_lagrangian_t

contains

    !> Build a phase-space Lagrangian from coordinates q, one-form
    !> coefficients a (one per coordinate), and h. All expressions must share
    !> one arena.
    function phase_lagrangian_create(q, a, h, ok, message) result(pl)
        type(expr_t), intent(in) :: q(:), a(:), h
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(phase_lagrangian_t) :: pl
        integer :: k

        ok = .false.
        message = ""
        if (size(q) == 0) then
            message = "phase_lagrangian_create: no coordinates supplied"
            return
        end if
        if (size(a) /= size(q)) then
            message = "phase_lagrangian_create: a(:) must have one entry per coordinate"
            return
        end if
        if (.not. is_valid(h)) then
            message = "phase_lagrangian_create: h is not a valid expression"
            return
        end if
        do k = 1, size(q)
            if (.not. is_valid(q(k)) .or. .not. is_valid(a(k))) then
                message = "phase_lagrangian_create: invalid coordinate or a-component"
                return
            end if
            if (.not. same_arena(q(k), h) .or. .not. same_arena(a(k), h)) then
                message = "phase_lagrangian_create: expressions belong to "// &
                    "different arenas"
                return
            end if
        end do
        pl%n = size(q)
        pl%q = q
        pl%a = a
        pl%h = h
        ok = .true.
    end function phase_lagrangian_create

    !> The Lagrange (symplectic) two-form omega_ij = d_i a_j - d_j a_i.
    !> Antisymmetric by construction: the diagonal is exact zero and
    !> omega(j,i) is the negation of omega(i,j) as an expression, not merely
    !> as a value.
    function symplectic_form(pl) result(omega)
        type(phase_lagrangian_t), intent(in) :: pl
        type(expr_t), allocatable :: omega(:, :)
        integer :: i, j

        allocate (omega(pl%n, pl%n))
        do j = 1, pl%n
            omega(j, j) = num(pl%q(1)%a, 0)
            do i = 1, j - 1
                omega(i, j) = diff(pl%a(j), pl%q(i)) - diff(pl%a(i), pl%q(j))
            end do
        end do
        do j = 1, pl%n
            do i = j + 1, pl%n
                omega(i, j) = -omega(j, i)
            end do
        end do
    end function symplectic_form

    !> Pfaffian of an antisymmetric matrix, by the standard recursive
    !> definition
    !>     Pf(A) = sum_{j=2}^{n} (-1)^j A(1,j) Pf(A with rows/cols 1,j removed)
    !> with Pf of the empty (0x0) matrix taken as 1. This module exposes it
    !> for even dimension 2, 4, or 6, which covers every case this API is
    !> meant to serve; the recursion itself is correct for any even n; larger
    !> sizes are refused so that untested combinatorial blow-up is never
    !> silently accepted.
    recursive function pfaffian(omega, ok, message) result(pf)
        type(expr_t), intent(in) :: omega(:, :)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(expr_t) :: pf
        integer :: n, j
        type(expr_t), allocatable :: minor(:, :)
        type(expr_t) :: term, subpf
        logical :: first, sub_ok

        n = size(omega, 1)
        ok = .false.
        message = ""
        if (size(omega, 2) /= n) then
            message = "pfaffian: matrix is not square"
            return
        end if
        if (n < 2 .or. mod(n, 2) /= 0) then
            message = "pfaffian: defined only for even dimension >= 2"
            return
        end if
        if (n > 6) then
            message = "pfaffian: supported only up to dimension 6"
            return
        end if

        if (n == 2) then
            pf = omega(1, 2)
            ok = .true.
            return
        end if

        first = .true.
        do j = 2, n
            call remove_row_col(omega, 1, j, minor)
            subpf = pfaffian(minor, sub_ok, message)
            if (.not. sub_ok) then
                ok = .false.
                return
            end if
            term = omega(1, j)*subpf
            if (mod(j, 2) == 1) term = -term
            if (first) then
                pf = term
                first = .false.
            else
                pf = pf + term
            end if
        end do
        ok = .true.
    end function pfaffian

    !> Remove row/column i1 and i2 (i1 < i2) from a square expression matrix.
    subroutine remove_row_col(m, i1, i2, sub)
        type(expr_t), intent(in) :: m(:, :)
        integer, intent(in) :: i1, i2
        type(expr_t), allocatable, intent(out) :: sub(:, :)
        integer, allocatable :: keep(:)
        integer :: n, i, k

        n = size(m, 1)
        allocate (keep(n - 2))
        k = 0
        do i = 1, n
            if (i == i1 .or. i == i2) cycle
            k = k + 1
            keep(k) = i
        end do
        allocate (sub(n - 2, n - 2))
        sub = m(keep, keep)
    end subroutine remove_row_col

    !> Solve omega qdot = grad h for qdot by symbolic matrix inversion.
    !> Works for any n whose symplectic form is invertible; the Pfaffian
    !> (n = 2, 4, 6 only) is reported alongside as the Liouville density when
    !> available, with pf_ok = .false. otherwise.
    subroutine phase_space_rates(pl, qdot, ok, message, pf, pf_ok)
        type(phase_lagrangian_t), intent(in) :: pl
        type(expr_t), allocatable, intent(out) :: qdot(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(expr_t), intent(out), optional :: pf
        logical, intent(out), optional :: pf_ok

        type(expr_t), allocatable :: omega(:, :), inv_m(:, :)
        type(expr_t), allocatable :: grad_h(:)
        type(expr_t) :: omega_list, inv_list
        type(str_t) :: why
        logical :: mok
        character(:), allocatable :: pf_message
        integer :: i, j

        ok = .false.
        message = ""
        allocate (grad_h(pl%n))
        do i = 1, pl%n
            grad_h(i) = diff(pl%h, pl%q(i))
        end do

        omega = symplectic_form(pl)

        if (present(pf)) then
            if (present(pf_ok)) then
                pf = pfaffian(omega, pf_ok, pf_message)
            else
                pf = pfaffian(omega, mok, pf_message)
            end if
        end if

        omega_list = from_matrix(pl%q(1)%a, omega)
        inv_list = matrix_inverse(pl%q(1)%a, omega_list, mok, why)
        if (.not. mok) then
            message = "phase_space_rates: symplectic form is not invertible: "// &
                chars(why)
            return
        end if
        call to_matrix(inv_list, inv_m, mok)
        if (.not. mok) then
            message = "phase_space_rates: could not recover the inverse matrix"
            return
        end if

        allocate (qdot(pl%n))
        do i = 1, pl%n
            qdot(i) = inv_m(i, 1)*grad_h(1)
            do j = 2, pl%n
                qdot(i) = qdot(i) + inv_m(i, j)*grad_h(j)
            end do
        end do
        ok = .true.
    end subroutine phase_space_rates

    !> Rate of change of the k-th one-form coefficient along the flow,
    !> d/dt(a_k) = sum_j (d_j a_k) qdot_j. This is the general chain-rule
    !> expression; the caller substitutes the solved qdot and simplifies to
    !> read off a conserved quantity (a_k is conserved exactly when this
    !> vanishes identically, e.g. because q_k does not appear in a or h).
    function noether_rate(pl, k, qdot) result(rate)
        type(phase_lagrangian_t), intent(in) :: pl
        integer, intent(in) :: k
        type(expr_t), intent(in) :: qdot(:)
        type(expr_t) :: rate
        integer :: j

        rate = diff(pl%a(k), pl%q(1))*qdot(1)
        do j = 2, pl%n
            rate = rate + diff(pl%a(k), pl%q(j))*qdot(j)
        end do
    end function noether_rate

    !> Reparametrisation helper: given a rates vector (e.g. the solved qdot)
    !> and a chosen index k, return d q_i/d q_k = rates(i)/rates(k), suitable
    !> for using q_k as the new independent variable. The caller is
    !> responsible for rates(k) being nonzero on the domain of interest.
    function reparametrize_rates(rates, k) result(dq_dqk)
        type(expr_t), intent(in) :: rates(:)
        integer, intent(in) :: k
        type(expr_t), allocatable :: dq_dqk(:)
        integer :: i

        allocate (dq_dqk(size(rates)))
        do i = 1, size(rates)
            dq_dqk(i) = rates(i)/rates(k)
        end do
    end function reparametrize_rates

    !> Classical second-order Euler-Lagrange residual for L(q, qdot), with q
    !> and qdot treated as independent symbols (no q-qdot dependency is
    !> assumed or inferred). The total time derivative of the conjugate
    !> momentum p_i = dL/dqdot_i is expanded purely formally by the chain
    !> rule
    !>     d/dt p_i = sum_j (d p_i/dq_j) qdot_j + sum_j (d p_i/dqdot_j) qddot_j
    !> where qddot_j is a caller-supplied independent symbol standing for
    !> d(qdot_j)/dt. No assumption about how qdot relates to q is used beyond
    !> this substitution, matching the classical Lagrangian's own convention:
    !> the returned residual is
    !>     residual_i = d/dt(dL/dqdot_i) - dL/dq_i.
    !> This module does not support Lagrangians with explicit t dependence
    !> here; a caller needing that adds dp_i/dt itself since t is otherwise
    !> just another symbol.
    function el_residual(lagr, q, qdot, qddot) result(residual)
        type(expr_t), intent(in) :: lagr
        type(expr_t), intent(in) :: q(:), qdot(:), qddot(:)
        type(expr_t), allocatable :: residual(:)
        type(expr_t) :: p_i, total_ddt
        integer :: i, j

        allocate (residual(size(q)))
        do i = 1, size(q)
            p_i = diff(lagr, qdot(i))
            total_ddt = diff(p_i, q(1))*qdot(1) + diff(p_i, qdot(1))*qddot(1)
            do j = 2, size(q)
                total_ddt = total_ddt + diff(p_i, q(j))*qdot(j) + &
                    diff(p_i, qdot(j))*qddot(j)
            end do
            residual(i) = total_ddt - diff(lagr, q(i))
        end do
    end function el_residual

end module fortsym_phasespace
