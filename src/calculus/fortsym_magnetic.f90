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
    use fortsym_expr, only: expr_t, i_expr, num, is_valid, operator(+), &
        operator(-), operator(*), operator(/)
    use fortsym_tensor, only: tensor_t, tensor_vector, tensor_covector, density
    implicit none
    private

    public :: magnetic_field_t, magnetic_field, magnetic_upper, magnetic_lower
    public :: magnetic_density
    public :: b_con, b_cov, b_density, b_fourier, b_fourier_density, j_fourier

    type :: magnetic_field_t
        type(tensor_t) :: upper
        type(tensor_t) :: lower
        type(tensor_t) :: density
    end type magnetic_field_t

    interface b_fourier
        module procedure b_fourier_integer, b_fourier_expression
    end interface b_fourier

    interface b_fourier_density
        module procedure b_fourier_density_integer, b_fourier_density_expression
    end interface b_fourier_density

    interface j_fourier
        module procedure j_fourier_integer, j_fourier_expression
    end interface j_fourier

contains

    !> Assemble the three typed views of one magnetic field.
    !>
    !> The component arrays remain owned by the established magnetic
    !> operators; this wrapper adds variance and density metadata without
    !> creating a second symbolic field representation.
    function magnetic_field(c, potential) result(field)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: potential(DIM)
        type(magnetic_field_t) :: field
        type(expr_t) :: upper(DIM), lower_value(DIM), density_value(DIM)

        upper = b_con(c, potential)
        lower_value = b_cov(c, upper)
        density_value = b_density(c, upper)
        field%upper = tensor_vector(c, upper)
        field%lower = tensor_covector(c, lower_value)
        field%density = density(tensor_vector(c, density_value), 1)
    end function magnetic_field

    !> Return the contravariant B^i view.
    function magnetic_upper(field) result(value)
        type(magnetic_field_t), intent(in) :: field
        type(tensor_t) :: value

        value = field%upper
    end function magnetic_upper

    !> Return the covariant B_i view.
    function magnetic_lower(field) result(value)
        type(magnetic_field_t), intent(in) :: field
        type(tensor_t) :: value

        value = field%lower
    end function magnetic_lower

    !> Return the contravariant weight-one density sqrtg B^i.
    function magnetic_density(field) result(value)
        type(magnetic_field_t), intent(in) :: field
        type(tensor_t) :: value

        value = field%density
    end function magnetic_density

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
    function b_fourier_integer(c, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM)
        integer, intent(in)        :: mode
        type(expr_t)               :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*num(c%a, int(mode, int64))
        value = b_fourier_from_factor(c, potential, mode_derivative)
    end function b_fourier_integer

    !> Symbolic Fourier-mode curl, useful when the mode number is itself part
    !> of a derivation rather than a compile-time integer.
    function b_fourier_expression(c, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM), mode
        type(expr_t)               :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*mode
        value = b_fourier_from_factor(c, potential, mode_derivative)
    end function b_fourier_expression

    function b_fourier_from_factor(c, potential, mode_derivative) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM), mode_derivative
        type(expr_t)               :: value(DIM)
        type(expr_t) :: volume

        volume = sqrtg(c)
        value(1) = (diff(potential(3), c%u(2)) - &
            mode_derivative*potential(2))/volume
        value(2) = (mode_derivative*potential(1) - &
            diff(potential(3), c%u(1)))/volume
        value(3) = (diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2)))/volume
    end function b_fourier_from_factor

    !> Fourier-mode magnetic density sqrt(g) B^i for symmetry coordinate u^3.
    !>
    !> Returning the density directly avoids introducing a division followed
    !> by a multiplication in generated kernels and preserves the paper's
    !> natural finite-element unknowns.
    function b_fourier_density_integer(c, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM)
        integer, intent(in)        :: mode
        type(expr_t)               :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*num(c%a, int(mode, int64))
        value = b_fourier_density_from_factor(c, potential, mode_derivative)
    end function b_fourier_density_integer

    function b_fourier_density_expression(c, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM), mode
        type(expr_t)               :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*mode
        value = b_fourier_density_from_factor(c, potential, mode_derivative)
    end function b_fourier_density_expression

    function b_fourier_density_from_factor(c, potential, mode_derivative) &
            result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in)   :: potential(DIM), mode_derivative
        type(expr_t)               :: value(DIM)

        value(1) = diff(potential(3), c%u(2)) - &
            mode_derivative*potential(2)
        value(2) = mode_derivative*potential(1) - &
            diff(potential(3), c%u(1))
        value(3) = diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2))
    end function b_fourier_density_from_factor

    !> Fourier-mode current from a covariant potential and reluctivity.
    !>
    !> This is the paper's curl-curl owner. The potential and reluctivity are
    !> mode amplitudes in (u1,u2); d/du3 is replaced by i*n. The operator is
    !> deliberately written as two curls around the supplied 3x3 reluctivity,
    !> so the block-diagonal Eq. (40--42) reduction is a specialization rather
    !> than a second implementation.
    function j_fourier_integer(c, reluctivity, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: reluctivity(DIM, DIM), potential(DIM)
        integer, intent(in) :: mode
        type(expr_t) :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*num(c%a, int(mode, int64))
        value = j_fourier_from_factor(c, reluctivity, potential, mode_derivative)
    end function j_fourier_integer

    function j_fourier_expression(c, reluctivity, potential, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: reluctivity(DIM, DIM), potential(DIM), mode
        type(expr_t) :: value(DIM)
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*mode
        value = j_fourier_from_factor(c, reluctivity, potential, mode_derivative)
    end function j_fourier_expression

    function j_fourier_from_factor(c, reluctivity, potential, mode_derivative) &
            result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: reluctivity(DIM, DIM), potential(DIM)
        type(expr_t), intent(in) :: mode_derivative
        type(expr_t) :: value(DIM)
        type(expr_t) :: magnetic(DIM), field(DIM)
        integer :: i, j

        do i = 1, DIM
            value(i) = expr_t()
        end do
        if (.not. associated(c%a)) return
        do i = 1, DIM
            if (.not. is_valid(potential(i))) return
            if (.not. associated(potential(i)%a, c%a)) return
            do j = 1, DIM
                if (.not. is_valid(reluctivity(i, j))) return
                if (.not. associated(reluctivity(i, j)%a, c%a)) return
            end do
        end do
        if (.not. is_valid(mode_derivative)) return
        if (.not. associated(mode_derivative%a, c%a)) return

        magnetic(1) = diff(potential(3), c%u(2)) - &
            mode_derivative*potential(2)
        magnetic(2) = mode_derivative*potential(1) - &
            diff(potential(3), c%u(1))
        magnetic(3) = diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2))

        do i = 1, DIM
            field(i) = reluctivity(i, 1)*magnetic(1)
            do j = 2, DIM
                field(i) = field(i) + reluctivity(i, j)*magnetic(j)
            end do
        end do

        value(1) = diff(field(3), c%u(2)) - mode_derivative*field(2)
        value(2) = mode_derivative*field(1) - diff(field(3), c%u(1))
        value(3) = diff(field(2), c%u(1)) - diff(field(1), c%u(2))
    end function j_fourier_from_factor

end module fortsym_magnetic
