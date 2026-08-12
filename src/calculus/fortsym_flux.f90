module fortsym_flux
    ! Reusable metadata and residuals for coordinate flux descriptions.
    !
    ! This owner deliberately stops at the coordinate identities. Constructing
    ! an equilibrium, solving for a rotational transform, or proving that a
    ! supplied residual vanishes belongs to a caller or a higher-level
    ! physics package. Keeping those concerns separate makes the identities
    ! cheap to reuse from both Fortran and the Python facade.
    use fortsym_chart, only: chart_t, DIM, chart_valid
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, is_valid, operator(+), operator(-), &
        operator(*)
    implicit none
    private

    integer, parameter, public :: FLUX_GENERIC = 0
    integer, parameter, public :: FLUX_CLEBSCH = 1
    integer, parameter, public :: FLUX_STRAIGHT_FIELD_LINE = 2
    integer, parameter, public :: FLUX_BOOZER = 3
    integer, parameter, public :: FLUX_HAMADA = 4
    integer, parameter, public :: BOOZER_RESIDUAL_COUNT = 5

    !> Coordinate metadata for a flux label and its two ordered angles.
    !>
    !> The label and angles are retained as indices into the chart rather than
    !> copied expression arrays. This keeps one source of truth for the chart
    !> and makes residual evaluation allocation-free at the interface.
    type, public :: flux_coordinate_t
        type(chart_t) :: chart
        integer :: label_index = 0
        integer :: angle_one = 0
        integer :: angle_two = 0
        integer :: kind = FLUX_GENERIC
        logical :: valid = .false.
    end type flux_coordinate_t

    public :: flux_coordinates, flux_coordinate_valid
    public :: flux_coordinate_label, flux_coordinate_kind
    public :: flux_coordinate_angles
    public :: flux_normal_residual, straight_field_line_residual
    public :: boozer_residuals

contains

    !> Describe u(label_index)=constant and retain an optional representation.
    function flux_coordinates(c, label_index, coordinate_kind) result(owner)
        type(chart_t), intent(in) :: c
        integer, intent(in) :: label_index
        integer, optional, intent(in) :: coordinate_kind
        type(flux_coordinate_t) :: owner
        integer :: selected_kind

        selected_kind = FLUX_GENERIC
        if (present(coordinate_kind)) selected_kind = coordinate_kind
        if (.not. chart_valid(c)) return
        if (label_index < 1 .or. label_index > DIM) return
        if (selected_kind < FLUX_GENERIC .or. selected_kind > FLUX_HAMADA) return

        owner%chart = c
        owner%label_index = label_index
        owner%kind = selected_kind
        select case (label_index)
        case (1)
            owner%angle_one = 2
            owner%angle_two = 3
        case (2)
            owner%angle_one = 1
            owner%angle_two = 3
        case (3)
            owner%angle_one = 1
            owner%angle_two = 2
        end select
        owner%valid = .true.
    end function flux_coordinates

    !> Validate all descriptor metadata and the chart it refers to.
    function flux_coordinate_valid(owner) result(valid)
        type(flux_coordinate_t), intent(in) :: owner
        logical :: valid

        valid = .false.
        if (.not. owner%valid) return
        if (.not. chart_valid(owner%chart)) return
        if (owner%label_index < 1 .or. owner%label_index > DIM) return
        if (owner%angle_one < 1 .or. owner%angle_one > DIM) return
        if (owner%angle_two < 1 .or. owner%angle_two > DIM) return
        if (owner%angle_one == owner%label_index) return
        if (owner%angle_two == owner%label_index) return
        if (owner%angle_one == owner%angle_two) return
        if (owner%kind < FLUX_GENERIC .or. owner%kind > FLUX_HAMADA) return
        valid = .true.
    end function flux_coordinate_valid

    !> Return the coordinate expression used as the flux label.
    function flux_coordinate_label(owner) result(label)
        type(flux_coordinate_t), intent(in) :: owner
        type(expr_t) :: label

        if (.not. flux_coordinate_valid(owner)) return
        label = owner%chart%u(owner%label_index)
    end function flux_coordinate_label

    !> Return the representation kind, or FLUX_GENERIC for invalid metadata.
    function flux_coordinate_kind(owner) result(coordinate_kind)
        type(flux_coordinate_t), intent(in) :: owner
        integer :: coordinate_kind

        coordinate_kind = FLUX_GENERIC
        if (.not. flux_coordinate_valid(owner)) return
        coordinate_kind = owner%kind
    end function flux_coordinate_kind

    !> Return the ordered angular coordinate indices.
    function flux_coordinate_angles(owner) result(angles)
        type(flux_coordinate_t), intent(in) :: owner
        integer :: angles(2)

        angles = 0
        if (.not. flux_coordinate_valid(owner)) return
        angles(1) = owner%angle_one
        angles(2) = owner%angle_two
    end function flux_coordinate_angles

    !> Residual of B^label=0, the tangency condition for a flux surface.
    function flux_normal_residual(owner, vector) result(residual)
        type(flux_coordinate_t), intent(in) :: owner
        type(expr_t), intent(in) :: vector(DIM)
        type(expr_t) :: residual
        integer :: i

        if (.not. flux_coordinate_valid(owner)) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, owner%chart%a)) return
        end do
        residual = vector(owner%label_index)
    end function flux_normal_residual

    !> Residual of B^angle_one - iota B^angle_two = 0.
    function straight_field_line_residual(owner, vector, rotational_transform) &
            result(residual)
        type(flux_coordinate_t), intent(in) :: owner
        type(expr_t), intent(in) :: vector(DIM)
        type(expr_t), intent(in) :: rotational_transform
        type(expr_t) :: residual
        integer :: i

        if (.not. flux_coordinate_valid(owner)) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, owner%chart%a)) return
        end do
        if (.not. is_valid(rotational_transform)) return
        if (.not. associated(rotational_transform%a, owner%chart%a)) return
        residual = vector(owner%angle_one) - rotational_transform* &
            vector(owner%angle_two)
    end function straight_field_line_residual

    !> Boozer residuals for (B_psi, d_theta B_theta, d_phi B_theta,
    !> d_theta B_phi, d_phi B_phi) in the descriptor's ordered angles.
    !>
    !> Vanishing of the last four entries is the local statement that the
    !> covariant angular components are flux functions. Their dependence on
    !> the label is intentionally left to the caller's zero/identity oracle.
    function boozer_residuals(owner, covariant) result(residual)
        type(flux_coordinate_t), intent(in) :: owner
        type(expr_t), intent(in) :: covariant(DIM)
        type(expr_t) :: residual(BOOZER_RESIDUAL_COUNT)
        integer :: i

        if (.not. flux_coordinate_valid(owner)) return
        if (owner%kind /= FLUX_BOOZER) return
        do i = 1, DIM
            if (.not. is_valid(covariant(i))) return
            if (.not. associated(covariant(i)%a, owner%chart%a)) return
        end do

        residual(1) = covariant(owner%label_index)
        residual(2) = diff(covariant(owner%angle_one), &
            owner%chart%u(owner%angle_one))
        residual(3) = diff(covariant(owner%angle_one), &
            owner%chart%u(owner%angle_two))
        residual(4) = diff(covariant(owner%angle_two), &
            owner%chart%u(owner%angle_one))
        residual(5) = diff(covariant(owner%angle_two), &
            owner%chart%u(owner%angle_two))
    end function boozer_residuals

end module fortsym_flux
