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
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: chart_t, DIM, curl, metric_covariant, &
        metric_contravariant, sqrtg, chart_surface_measure => surface_measure
    use fortsym_defint, only: definite_integral
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, i_expr, num, pi_expr, is_valid, &
        operator(+), operator(-), operator(*), operator(/)
    use fortsym_tensor, only: tensor_t, tensor_vector, tensor_covector, density, &
        tensor_valid
    implicit none
    private

    public :: magnetic_field_t, magnetic_field, magnetic_upper, magnetic_lower
    public :: magnetic_density
    public :: magnetic_chart_t, magnetic_chart, magnetic_chart_valid, &
        magnetic_chart_surface, magnetic_chart_field, magnetic_chart_upper, &
        magnetic_chart_lower, magnetic_chart_density, magnetic_chart_average
    public :: flux_surface_t, flux_surface, flux_surface_valid, &
        flux_surface_label, flux_surface_measure, flux_surface_average
    public :: b_con, b_cov, b_density, h_cov, h_con, b_fourier, &
        b_fourier_density, j_fourier

    type :: magnetic_field_t
        type(tensor_t) :: upper
        type(tensor_t) :: lower
        type(tensor_t) :: density
    end type magnetic_field_t

    !> A coordinate flux surface u(label_index)=constant.
    !>
    !> The two remaining coordinates are ordered as the integration angles.
    !> This is metadata only: it does not claim that an arbitrary chart is a
    !> magnetic equilibrium. A caller supplies a field and proves tangency
    !> through its label component.
    type :: flux_surface_t
        type(chart_t) :: chart
        integer :: label_index = 0
        integer :: angle_one = 0
        integer :: angle_two = 0
        logical :: valid = .false.
    end type flux_surface_t

    !> A physicist-facing magnetic coordinate toolkit owner.
    !>
    !> The chart and flux-surface metadata remain separate owners; the field
    !> views are the existing magnetic_field_t representations. This wrapper
    !> only packages them so a caller cannot accidentally lose the surface
    !> label while moving between B^i, B_i, and sqrt(g) B^i.
    type :: magnetic_chart_t
        type(chart_t) :: chart
        type(flux_surface_t) :: surface
        type(magnetic_field_t) :: field
        logical :: valid = .false.
    end type magnetic_chart_t

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

    !> Build one magnetic-coordinate owner from a covariant potential.
    function magnetic_chart(c, potential, label_index) result(owner)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: potential(DIM)
        integer, optional, intent(in) :: label_index
        type(magnetic_chart_t) :: owner
        integer :: selected_label

        selected_label = 1
        if (present(label_index)) selected_label = label_index
        owner%chart = c
        owner%surface = flux_surface(c, selected_label)
        owner%field = magnetic_field(c, potential)
        owner%valid = flux_surface_valid(owner%surface)
        if (.not. owner%valid) return
        if (.not. tensor_valid(owner%field%upper)) owner%valid = .false.
        if (.not. owner%valid) return
        if (.not. tensor_valid(owner%field%lower)) owner%valid = .false.
        if (.not. owner%valid) return
        if (.not. tensor_valid(owner%field%density)) owner%valid = .false.
    end function magnetic_chart

    !> Check the chart, surface metadata, and all three typed field views.
    function magnetic_chart_valid(owner) result(valid)
        type(magnetic_chart_t), intent(in) :: owner
        logical :: valid

        valid = owner%valid
        if (.not. valid) return
        valid = flux_surface_valid(owner%surface)
        if (.not. valid) return
        valid = tensor_valid(owner%field%upper)
        if (.not. valid) return
        valid = tensor_valid(owner%field%lower)
        if (.not. valid) return
        valid = tensor_valid(owner%field%density)
    end function magnetic_chart_valid

    !> Return the coordinate flux surface owned by the toolkit.
    function magnetic_chart_surface(owner) result(surface)
        type(magnetic_chart_t), intent(in) :: owner
        type(flux_surface_t) :: surface

        if (.not. magnetic_chart_valid(owner)) return
        surface = owner%surface
    end function magnetic_chart_surface

    !> Return all typed magnetic views without copying their components.
    function magnetic_chart_field(owner) result(field)
        type(magnetic_chart_t), intent(in) :: owner
        type(magnetic_field_t) :: field

        if (.not. magnetic_chart_valid(owner)) return
        field = owner%field
    end function magnetic_chart_field

    !> Return B^i and retain its upper-index tensor metadata.
    function magnetic_chart_upper(owner) result(value)
        type(magnetic_chart_t), intent(in) :: owner
        type(tensor_t) :: value

        if (.not. magnetic_chart_valid(owner)) return
        value = magnetic_upper(owner%field)
    end function magnetic_chart_upper

    !> Return B_i and retain its lower-index tensor metadata.
    function magnetic_chart_lower(owner) result(value)
        type(magnetic_chart_t), intent(in) :: owner
        type(tensor_t) :: value

        if (.not. magnetic_chart_valid(owner)) return
        value = magnetic_lower(owner%field)
    end function magnetic_chart_lower

    !> Return sqrt(g) B^i and retain its weight-one density metadata.
    function magnetic_chart_density(owner) result(value)
        type(magnetic_chart_t), intent(in) :: owner
        type(tensor_t) :: value

        if (.not. magnetic_chart_valid(owner)) return
        value = magnetic_density(owner%field)
    end function magnetic_chart_density

    !> Average a scalar on the owner's coordinate flux surface.
    subroutine magnetic_chart_average(owner, scalar, value, ok, why, period_one, &
            period_two)
        type(magnetic_chart_t), intent(in) :: owner
        type(expr_t), intent(in) :: scalar
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), optional, intent(in) :: period_one, period_two

        ok = .false.
        why = ""
        value = expr_t()
        if (.not. magnetic_chart_valid(owner)) then
            why = "magnetic chart metadata is invalid"
            return
        end if
        call flux_surface_average(owner%surface, scalar, value, ok, why, &
            period_one, period_two)
    end subroutine magnetic_chart_average

    !> Describe the coordinate surface u(label_index)=constant.
    function flux_surface(c, label_index) result(surface)
        type(chart_t), intent(in) :: c
        integer, intent(in) :: label_index
        type(flux_surface_t) :: surface

        if (.not. associated(c%a)) return
        if (label_index < 1 .or. label_index > DIM) return
        surface%chart = c
        surface%label_index = label_index
        select case (label_index)
        case (1)
            surface%angle_one = 2
            surface%angle_two = 3
        case (2)
            surface%angle_one = 1
            surface%angle_two = 3
        case (3)
            surface%angle_one = 1
            surface%angle_two = 2
        end select
        surface%valid = .true.
    end function flux_surface

    !> Validate the chart and its label/angle metadata.
    function flux_surface_valid(surface) result(valid)
        type(flux_surface_t), intent(in) :: surface
        logical :: valid

        valid = .false.
        if (.not. surface%valid) return
        if (.not. associated(surface%chart%a)) return
        if (surface%label_index < 1 .or. surface%label_index > DIM) return
        if (surface%angle_one < 1 .or. surface%angle_one > DIM) return
        if (surface%angle_two < 1 .or. surface%angle_two > DIM) return
        if (surface%angle_one == surface%label_index) return
        if (surface%angle_two == surface%label_index) return
        if (surface%angle_one == surface%angle_two) return
        if (.not. is_valid(surface%chart%u(surface%label_index))) return
        valid = .true.
    end function flux_surface_valid

    !> Return the coordinate expression labelling the surface.
    function flux_surface_label(surface) result(label)
        type(flux_surface_t), intent(in) :: surface
        type(expr_t) :: label

        if (.not. flux_surface_valid(surface)) return
        label = surface%chart%u(surface%label_index)
    end function flux_surface_label

    !> Return the positive induced measure on the coordinate surface.
    function flux_surface_measure(surface) result(value)
        type(flux_surface_t), intent(in) :: surface
        type(expr_t) :: value

        if (.not. flux_surface_valid(surface)) return
        value = chart_surface_measure(surface%chart, surface%label_index)
    end function flux_surface_measure

    !> Average a scalar over the two angular coordinates.
    !>
    !> The default integration domain is [0,2*pi] in each angle. Optional
    !> upper bounds make the operation usable for non-2*pi periodic charts;
    !> the lower bounds are zero. The result is returned only when both the
    !> numerator and normalization integrals are verified by definite_integral.
    subroutine flux_surface_average(surface, scalar, value, ok, why, period_one, &
            period_two)
        type(flux_surface_t), intent(in) :: surface
        type(expr_t), intent(in) :: scalar
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), optional, intent(in) :: period_one, period_two
        type(arena_t), pointer :: a
        type(expr_t) :: lower, upper_one, upper_two, weight
        type(expr_t) :: numerator, denominator, first_numerator
        type(expr_t) :: first_denominator, integrated_numerator
        type(expr_t) :: integrated_denominator
        logical :: numerator_ok, denominator_ok
        character(:), allocatable :: reason

        ok = .false.
        why = ""
        value = expr_t()
        if (.not. flux_surface_valid(surface)) then
            why = "flux surface metadata is invalid"
            return
        end if
        a => surface%chart%a
        if (.not. is_valid(scalar)) then
            why = "surface-average scalar is invalid"
            return
        end if
        if (.not. associated(scalar%a, a)) then
            why = "surface-average scalar belongs to another arena"
            return
        end if

        lower = num(a, 0)
        upper_one = 2*pi_expr(a)
        upper_two = upper_one
        if (present(period_one)) then
            if (.not. is_valid(period_one)) then
                why = "first angular upper bound is invalid"
                return
            end if
            if (.not. associated(period_one%a, a)) then
                why = "first angular upper bound belongs to another arena"
                return
            end if
            upper_one = period_one
        end if
        if (present(period_two)) then
            if (.not. is_valid(period_two)) then
                why = "second angular upper bound is invalid"
                return
            end if
            if (.not. associated(period_two%a, a)) then
                why = "second angular upper bound belongs to another arena"
                return
            end if
            upper_two = period_two
        end if

        weight = flux_surface_measure(surface)
        numerator = scalar*weight
        denominator = weight
        call definite_integral(a, numerator, surface%chart%u(surface%angle_one), &
            lower, upper_one, first_numerator, numerator_ok, reason)
        if (.not. numerator_ok) then
            why = "surface numerator: "//reason
            return
        end if
        call definite_integral(a, first_numerator, &
            surface%chart%u(surface%angle_two), lower, upper_two, &
            integrated_numerator, numerator_ok, reason)
        if (.not. numerator_ok) then
            why = "surface numerator: "//reason
            return
        end if
        call definite_integral(a, denominator, surface%chart%u(surface%angle_one), &
            lower, upper_one, first_denominator, denominator_ok, reason)
        if (.not. denominator_ok) then
            why = "surface normalization: "//reason
            return
        end if
        call definite_integral(a, first_denominator, &
            surface%chart%u(surface%angle_two), lower, upper_two, &
            integrated_denominator, denominator_ok, reason)
        if (.not. denominator_ok) then
            why = "surface normalization: "//reason
            return
        end if
        value = integrated_numerator/integrated_denominator
        ok = .true.
    end subroutine flux_surface_average

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

    !> Covariant magnetic field intensity from a reluctivity map.
    !>
    !> The constitutive convention is explicit and coordinate based:
    !> H_i = nu_ij B^j.  The supplied reluctivity therefore maps a
    !> contravariant magnetic vector to a covector; index raising is kept in
    !> the separate h_con owner below.  This is also the convention used by
    !> the Fourier constitutive reduction.
    function h_cov(c, reluctivity, contravariant) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: reluctivity(DIM, DIM)
        type(expr_t), intent(in) :: contravariant(DIM)
        type(expr_t) :: value(DIM)
        integer :: i, j

        if (.not. associated(c%a)) return
        do i = 1, DIM
            if (.not. is_valid(contravariant(i))) return
            if (.not. associated(contravariant(i)%a, c%a)) return
            do j = 1, DIM
                if (.not. is_valid(reluctivity(i, j))) return
                if (.not. associated(reluctivity(i, j)%a, c%a)) return
            end do
        end do

        value = num(c%a, 0)
        do i = 1, DIM
            value(i) = reluctivity(i, 1)*contravariant(1)
            do j = 2, DIM
                value(i) = value(i) + reluctivity(i, j)*contravariant(j)
            end do
        end do
    end function h_cov

    !> Contravariant magnetic field intensity H^i from covariant H_i.
    !>
    !> This is the metric raising operation H^i = g^ij H_j.  It deliberately
    !> accepts H_i rather than reluctivity and B again, so constitutive
    !> evaluation and index conversion remain composable single-responsibility
    !> operations.
    function h_con(c, covariant) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: covariant(DIM)
        type(expr_t) :: value(DIM)
        type(expr_t) :: inverse(DIM, DIM)
        integer :: i, j

        if (.not. associated(c%a)) return
        do i = 1, DIM
            if (.not. is_valid(covariant(i))) return
            if (.not. associated(covariant(i)%a, c%a)) return
        end do

        inverse = metric_contravariant(c)
        value = num(c%a, 0)
        do i = 1, DIM
            value(i) = inverse(i, 1)*covariant(1)
            do j = 2, DIM
                value(i) = value(i) + inverse(i, j)*covariant(j)
            end do
        end do
    end function h_con

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
