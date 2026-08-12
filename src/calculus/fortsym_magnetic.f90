module fortsym_magnetic
    ! Magnetic-coordinate views built from the generic chart and metric owners.
    !
    ! The module deliberately stores no second magnetic-field representation.
    ! A covariant vector potential is converted by fortsym_chart%curl to
    ! contravariant magnetic components. Lowering and density weighting then
    ! use the chart's metric and positive volume factor. This keeps B^i, B_i,
    ! and sqrt(g) B^i distinct without coupling the generic expression arena to
    ! a plasma equilibrium package.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_chart, only: chart_t, DIM, curl, metric_covariant, sqrtg
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, i_expr, num, operator(+), operator(-), &
        operator(*), operator(/)
    implicit none
    private

    public :: b_con, b_cov, b_density, b_fourier, b_fourier_density

contains

    !> Contravariant magnetic components from a covariant vector potential.
    !>
    !> The result is the coordinate curl. For a potential one-form A_i, its
    !> components are B^i = (1/sqrt(g)) eps^(ijk) partial_j A_k for an
    !> orientation-preserving chart. The generic chart owner retains the signed
    !> Jacobian in curl, so orientation-sensitive callers can distinguish the
    !> two conventions explicitly.
    function b_con(c, potential) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)    :: potential(DIM)
        type(expr_t)                :: value(DIM)

        value = curl(c, potential)
    end function b_con

    !> Covariant magnetic components B_i = g_ij B^j.
    function b_cov(c, contravariant) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)    :: contravariant(DIM)
        type(expr_t)                :: value(DIM)
        type(expr_t) :: g(DIM, DIM)

        g = metric_covariant(c)
        value(1) = g(1, 1)*contravariant(1) + &
            g(1, 2)*contravariant(2) + g(1, 3)*contravariant(3)
        value(2) = g(2, 1)*contravariant(1) + &
            g(2, 2)*contravariant(2) + g(2, 3)*contravariant(3)
        value(3) = g(3, 1)*contravariant(1) + &
            g(3, 2)*contravariant(2) + g(3, 3)*contravariant(3)
    end function b_cov

    !> Contravariant vector density sqrt(g) B^i.
    function b_density(c, contravariant) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)    :: contravariant(DIM)
        type(expr_t)                :: value(DIM)
        type(expr_t) :: volume

        volume = sqrtg(c)
        value(1) = volume*contravariant(1)
        value(2) = volume*contravariant(2)
        value(3) = volume*contravariant(3)
    end function b_density

    !> Contravariant Fourier-mode curl for a symmetry coordinate u^3.
    !>
    !> The input contains mode amplitudes, so the derivative in the symmetry
    !> direction is replaced by i*n. This is the native form of the
    !> paper_magnetic reduction and is deliberately separate from `curl`,
    !> whose input is a full coordinate-dependent field. The chart must have
    !> an orientation-preserving, block metric for the usual plasma convention
    !> in which the positive factor sqrt(g) is the signed Jacobian.
    function b_fourier(c, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM)
        integer, intent(in)        :: mode
        type(expr_t)               :: value(DIM)
        type(expr_t) :: volume, mode_derivative

        volume = sqrtg(c)
        mode_derivative = i_expr(c%a)*num(c%a, int(mode, int64))
        value(1) = (diff(potential(3), c%u(2)) - &
            mode_derivative*potential(2))/volume
        value(2) = (mode_derivative*potential(1) - &
            diff(potential(3), c%u(1)))/volume
        value(3) = (diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2)))/volume
    end function b_fourier

    !> Fourier-mode magnetic density sqrt(g) B^i for symmetry coordinate u^3.
    !>
    !> Returning the density directly avoids introducing a division followed
    !> by a multiplication in generated kernels and preserves the paper's
    !> natural finite-element unknowns.
    function b_fourier_density(c, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM)
        integer, intent(in)        :: mode
        type(expr_t)               :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*num(c%a, int(mode, int64))
        value(1) = diff(potential(3), c%u(2)) - &
            mode_derivative*potential(2)
        value(2) = mode_derivative*potential(1) - &
            diff(potential(3), c%u(1))
        value(3) = diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2))
    end function b_fourier_density

end module fortsym_magnetic
