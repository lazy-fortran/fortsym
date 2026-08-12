module fortsym_magnetic_weak
    ! Fourier finite-element contracts for the block constitutive reduction.
    !
    ! This module owns the paper-specific weak-form metadata.  The generic
    ! chart, tensor, and magnetic modules continue to own coordinates and
    ! component kernels; this layer only turns a block reluctivity and a
    ! Fourier mode into the two variational branches used by the paper.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_chart, only: chart_t, DIM
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, i_expr, num, is_valid, same_arena, &
        operator(+), operator(-), operator(*), operator(==)
    implicit none
    private

    integer, parameter, public :: FOURIER_INVALID = 0
    integer, parameter, public :: FOURIER_LONGITUDINAL = 1
    integer, parameter, public :: FOURIER_TRANSVERSE = 2

    integer, parameter, public :: SPACE_NONE = 0
    integer, parameter, public :: SPACE_NODAL = 1
    integer, parameter, public :: SPACE_EDGE = 2

    integer, parameter, public :: TRACE_NONE = 0
    integer, parameter, public :: TRACE_NORMAL = 1
    integer, parameter, public :: TRACE_TANGENTIAL = 2

    type, public :: fourier_constitutive_t
        ! The 2D transverse block and the longitudinal nu_33 coefficient.
        type(expr_t) :: nu_t(2, 2)
        type(expr_t) :: nu33
        ! nubar_t = -E_t nu_t E_t, with E_t(1,2)=1 and E_t(2,1)=-1.
        type(expr_t) :: nubar_t(2, 2)
        logical :: valid = .false.
        logical :: block_diagonal = .false.
    end type fourier_constitutive_t

    type, public :: fourier_weak_form_t
        integer :: mode = 0
        integer :: branch = FOURIER_INVALID
        integer :: trial_space = SPACE_NONE
        integer :: test_space = SPACE_NONE
        integer :: trial_components = 0
        integer :: test_components = 0
        integer :: boundary_trace = TRACE_NONE
        logical :: valid = .false.
        logical :: has_gradient_term = .false.
        logical :: has_curl_term = .false.
        logical :: has_mass_term = .false.
        logical :: requires_current_compatibility = .false.
        type(expr_t) :: scalar_coefficient
        type(expr_t) :: transverse_curl_coefficient
        type(expr_t) :: transverse_mass(2, 2)
    end type fourier_weak_form_t

    interface fourier_constitutive
        module procedure fourier_constitutive_matrix
        module procedure fourier_constitutive_blocks
    end interface fourier_constitutive

    interface fourier_weak_form
        module procedure fourier_weak_form_integer
    end interface fourier_weak_form

    interface current_compatibility
        module procedure current_compatibility_integer
        module procedure current_compatibility_expression
    end interface current_compatibility

    public :: fourier_constitutive
    public :: fourier_weak_form
    public :: current_compatibility
    public :: nubar, fourier_constitutive_valid, fourier_weak_form_valid

contains

    !> Build the paper's constitutive blocks from a full 3x3 reluctivity.
    !>
    !> The Fourier reduction is valid only for a block-diagonal constitutive
    !> tensor.  Cross-block entries are therefore retained as an explicit
    !> validity condition instead of being silently discarded.
    function fourier_constitutive_matrix(c, reluctivity) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: reluctivity(DIM, DIM)
        type(fourier_constitutive_t) :: value
        type(expr_t) :: zero
        integer :: i, j

        value%valid = .false.
        value%block_diagonal = .false.
        if (.not. associated(c%a)) return
        zero = num(c%a, 0_int64)
        do i = 1, DIM
            do j = 1, DIM
                if (.not. is_valid(reluctivity(i, j))) return
                if (.not. same_arena(reluctivity(i, j), zero)) return
            end do
        end do

        value%nu_t(1, 1) = reluctivity(1, 1)
        value%nu_t(1, 2) = reluctivity(1, 2)
        value%nu_t(2, 1) = reluctivity(2, 1)
        value%nu_t(2, 2) = reluctivity(2, 2)
        value%nu33 = reluctivity(3, 3)
        call set_nubar(value%nubar_t, value%nu_t)

        value%block_diagonal = reluctivity(1, 3) == zero .and. &
            reluctivity(2, 3) == zero .and. reluctivity(3, 1) == zero .and. &
            reluctivity(3, 2) == zero
        value%valid = value%block_diagonal
    end function fourier_constitutive_matrix

    !> Build the same owner from explicit 2D and longitudinal blocks.
    !>
    !> This overload is useful when a finite-element package already stores
    !> the paper's blocks and should not manufacture a discarded 3x3 matrix.
    function fourier_constitutive_blocks(c, nu_t, nu33) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: nu_t(2, 2), nu33
        type(fourier_constitutive_t) :: value
        integer :: i, j

        value%valid = .false.
        value%block_diagonal = .true.
        if (.not. associated(c%a)) return
        if (.not. is_valid(nu33)) return
        if (.not. same_arena(nu33, c%u(1))) return
        do i = 1, 2
            do j = 1, 2
                if (.not. is_valid(nu_t(i, j))) return
                if (.not. same_arena(nu_t(i, j), c%u(1))) return
                value%nu_t(i, j) = nu_t(i, j)
            end do
        end do
        value%nu33 = nu33
        call set_nubar(value%nubar_t, value%nu_t)
        value%valid = .true.
    end function fourier_constitutive_blocks

    !> Return nubar_t = -E_t nu_t E_t.
    function nubar(nu_t) result(value)
        type(expr_t), intent(in) :: nu_t(2, 2)
        type(expr_t) :: value(2, 2)

        call set_nubar(value, nu_t)
    end function nubar

    subroutine set_nubar(value, nu_t)
        type(expr_t), intent(out) :: value(2, 2)
        type(expr_t), intent(in) :: nu_t(2, 2)

        value(1, 1) = nu_t(2, 2)
        value(1, 2) = -nu_t(2, 1)
        value(2, 1) = -nu_t(1, 2)
        value(2, 2) = nu_t(1, 1)
    end subroutine set_nubar

    !> Describe the n=0 scalar or n/=0 transverse variational branch.
    function fourier_weak_form_integer(c, material, mode) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        integer, intent(in) :: mode
        type(fourier_weak_form_t) :: value
        type(expr_t) :: mode_square
        type(expr_t) :: zero
        integer :: i, j

        value%mode = mode
        value%requires_current_compatibility = .true.
        if (.not. material%valid) return
        if (.not. same_arena(material%nu33, c%u(1))) return
        zero = num(c%a, 0_int64)
        value%scalar_coefficient = zero
        value%transverse_curl_coefficient = zero
        do i = 1, 2
            do j = 1, 2
                value%transverse_mass(i, j) = zero
            end do
        end do
        mode_square = num(c%a, int(mode, int64)*int(mode, int64))
        if (mode == 0) then
            value%branch = FOURIER_LONGITUDINAL
            value%trial_space = SPACE_NODAL
            value%test_space = SPACE_NODAL
            value%trial_components = 1
            value%test_components = 1
            value%boundary_trace = TRACE_NORMAL
            value%has_gradient_term = .true.
            value%scalar_coefficient = material%nu33
        else
            value%branch = FOURIER_TRANSVERSE
            value%trial_space = SPACE_EDGE
            value%test_space = SPACE_EDGE
            value%trial_components = 2
            value%test_components = 2
            value%boundary_trace = TRACE_TANGENTIAL
            value%has_curl_term = .true.
            value%has_mass_term = .true.
            value%transverse_curl_coefficient = material%nu33
            do i = 1, 2
                do j = 1, 2
                    value%transverse_mass(i, j) = mode_square * &
                        material%nubar_t(i, j)
                end do
            end do
        end if
        value%valid = .true.
    end function fourier_weak_form_integer

    !> Metric-free compatibility residual for a Fourier current density.
    !>
    !> The input is the contravariant weight-one density, so no metric or
    !> volume factor is introduced here: div(J) = d_1 J^1 + d_2 J^2 + i n J^3.
    function current_compatibility_integer(c, current, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: current(DIM)
        integer, intent(in) :: mode
        type(expr_t) :: value
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*num(c%a, int(mode, int64))
        value = current_compatibility_from_factor(c, current, mode_derivative)
    end function current_compatibility_integer

    function current_compatibility_expression(c, current, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: current(DIM), mode
        type(expr_t) :: value
        type(expr_t) :: mode_derivative

        mode_derivative = i_expr(c%a)*mode
        value = current_compatibility_from_factor(c, current, mode_derivative)
    end function current_compatibility_expression

    function current_compatibility_from_factor(c, current, mode_derivative) &
            result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: current(DIM), mode_derivative
        type(expr_t) :: value

        value = diff(current(1), c%u(1)) + diff(current(2), c%u(2)) + &
            mode_derivative*current(3)
    end function current_compatibility_from_factor

    pure function fourier_constitutive_valid(material) result(value)
        type(fourier_constitutive_t), intent(in) :: material
        logical :: value

        value = material%valid
    end function fourier_constitutive_valid

    pure function fourier_weak_form_valid(form) result(value)
        type(fourier_weak_form_t), intent(in) :: form
        logical :: value

        value = form%valid
    end function fourier_weak_form_valid

end module fortsym_magnetic_weak
