module fortsym_magnetic_weak
    ! Fourier symbolic weak-form contracts for the block constitutive reduction.
    !
    ! This module owns the paper-specific weak-form metadata.  The generic
    ! chart, tensor, and magnetic modules continue to own coordinates and
    ! component kernels; this layer only turns a block reluctivity and a
    ! Fourier mode into the two variational branches used by the paper.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_chart, only: chart_t, DIM, sqrtg
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_expr, only: expr_t, i_expr, num, is_valid, same_arena, &
        operator(+), operator(-), operator(*), operator(/), operator(==)
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
        ! The n=0 scalar problem uses -div_t(nubar_t grad_t A_3).
        type(expr_t) :: longitudinal_diffusion(2, 2)
        type(expr_t) :: transverse_curl_coefficient
        type(expr_t) :: transverse_mass(2, 2)
    end type fourier_weak_form_t

    ! A mode-specific volumetric source.  The n=0 longitudinal branch owns
    ! one J^3 component; every nonzero transverse branch owns (J^1,J^2).
    type, public :: fourier_source_t
        integer :: mode = 0
        integer :: branch = FOURIER_INVALID
        integer :: components = 0
        type(expr_t) :: value(2)
        logical :: valid = .false.
    end type fourier_source_t

    ! A mode-specific boundary load.  Normal traces are scalar nodal loads;
    ! tangential traces are two-component edge loads.  The load owner keeps
    ! the weak-form sign and surface measure with the caller.
    type, public :: fourier_load_t
        integer :: mode = 0
        integer :: branch = FOURIER_INVALID
        integer :: trace = TRACE_NONE
        integer :: components = 0
        type(expr_t) :: value(2)
        logical :: valid = .false.
    end type fourier_load_t

    interface fourier_constitutive
        module procedure fourier_constitutive_matrix
        module procedure fourier_constitutive_blocks
    end interface fourier_constitutive

    interface reluctivity_density
        module procedure reluctivity_density_scalar
        module procedure reluctivity_density_matrix
    end interface reluctivity_density

    interface fourier_weak_form
        module procedure fourier_weak_form_integer
    end interface fourier_weak_form

    interface fourier_source
        module procedure fourier_source_scalar
        module procedure fourier_source_pair
    end interface fourier_source

    interface fourier_load
        module procedure fourier_load_scalar
        module procedure fourier_load_pair
    end interface fourier_load

    interface current_compatibility
        module procedure current_compatibility_integer
        module procedure current_compatibility_expression
    end interface current_compatibility

    interface fourier_longitudinal_residual
        module procedure fourier_longitudinal_residual_scalar
    end interface fourier_longitudinal_residual

    interface fourier_transverse_residual
        module procedure fourier_transverse_residual_integer
        module procedure fourier_transverse_residual_expression
    end interface fourier_transverse_residual

    interface fourier_longitudinal_flux
        module procedure fourier_longitudinal_flux_component
    end interface fourier_longitudinal_flux

    interface fourier_transverse_flux
        module procedure fourier_transverse_flux_scalar
    end interface fourier_transverse_flux

    public :: fourier_constitutive
    public :: reluctivity_density
    public :: fourier_weak_form
    public :: current_compatibility
    public :: fourier_longitudinal_residual, fourier_transverse_residual
    public :: fourier_longitudinal_flux, fourier_transverse_flux, &
        fourier_longitudinal_boundary_flux, fourier_transverse_boundary_flux, &
        fourier_transverse_boundary_contraction
    public :: fourier_source, fourier_load, fourier_source_valid, &
        fourier_load_valid, fourier_source_matches, fourier_load_matches
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
        type(native_engine_t) :: engine
        type(engine_result_t) :: checked

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

        engine = make_native_engine(c%a)
        value%block_diagonal = reluctivity(1, 3) == zero
        if (.not. value%block_diagonal) then
            checked = engine%zero_test(reluctivity(1, 3))
            value%block_diagonal = checked%verdict == VERDICT_TRUE
        end if
        if (value%block_diagonal) then
            value%block_diagonal = reluctivity(2, 3) == zero
            if (.not. value%block_diagonal) then
                checked = engine%zero_test(reluctivity(2, 3))
                value%block_diagonal = checked%verdict == VERDICT_TRUE
            end if
        end if
        if (value%block_diagonal) then
            value%block_diagonal = reluctivity(3, 1) == zero
            if (.not. value%block_diagonal) then
                checked = engine%zero_test(reluctivity(3, 1))
                value%block_diagonal = checked%verdict == VERDICT_TRUE
            end if
        end if
        if (value%block_diagonal) then
            value%block_diagonal = reluctivity(3, 2) == zero
            if (.not. value%block_diagonal) then
                checked = engine%zero_test(reluctivity(3, 2))
                value%block_diagonal = checked%verdict == VERDICT_TRUE
            end if
        end if
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

    !> Convert an isotropic Cartesian reluctivity into the paper's covariant
    !> weight-minus-one density components.
    !>
    !> The physical scalar multiplies the Euclidean Cartesian identity, so
    !> nu_ij = physical * (e_i dot e_j) / sqrtg.  The output is a component
    !> matrix for fourier_constitutive, whose caller records the -1 weight.
    subroutine reluctivity_density_scalar(c, physical, value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: physical
        type(expr_t), intent(out) :: value(DIM, DIM)
        type(expr_t) :: volume, term, left, right, zero
        integer :: i, j, k

        do i = 1, DIM
            do j = 1, DIM
                value(i, j) = expr_t()
            end do
        end do
        if (.not. associated(c%a)) return
        if (.not. is_valid(physical)) return
        if (.not. same_arena(physical, c%u(1))) return
        volume = sqrtg(c)
        if (.not. is_valid(volume)) return
        zero = num(c%a, 0_int64)
        do i = 1, DIM
            do j = 1, DIM
                value(i, j) = zero
                do k = 1, DIM
                    left = diff(c%x(k), c%u(i))
                    right = diff(c%x(k), c%u(j))
                    if (left == zero) cycle
                    if (right == zero) cycle
                    term = left*right
                    value(i, j) = value(i, j) + physical*term
                end do
                if (value(i, j) == zero) cycle
                value(i, j) = value(i, j)/volume
            end do
        end do
    end subroutine reluctivity_density_scalar

    !> Convert a Cartesian reluctivity matrix into covariant weight-minus-one
    !> density components, nu_ij = e_i^a physical_ab e_j^b / sqrtg.
    !>
    !> The input is expressed in the orthonormal embedding basis owned by the
    !> chart position map.  No metric is inferred from the matrix, and no
    !> density factor is hidden in the returned component values.
    subroutine reluctivity_density_matrix(c, physical, value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: physical(DIM, DIM)
        type(expr_t), intent(out) :: value(DIM, DIM)
        type(expr_t) :: volume, term, left, right, zero
        integer :: i, j, aindex, bindex

        do i = 1, DIM
            do j = 1, DIM
                value(i, j) = expr_t()
            end do
        end do
        if (.not. associated(c%a)) return
        do aindex = 1, DIM
            do bindex = 1, DIM
                if (.not. is_valid(physical(aindex, bindex))) return
                if (.not. same_arena(physical(aindex, bindex), c%u(1))) return
            end do
        end do
        volume = sqrtg(c)
        if (.not. is_valid(volume)) return
        zero = num(c%a, 0_int64)
        do i = 1, DIM
            do j = 1, DIM
                value(i, j) = zero
                do aindex = 1, DIM
                    do bindex = 1, DIM
                        if (physical(aindex, bindex) == zero) cycle
                        left = diff(c%x(aindex), c%u(i))
                        right = diff(c%x(bindex), c%u(j))
                        if (left == zero) cycle
                        if (right == zero) cycle
                        term = left*physical(aindex, bindex)*right
                        value(i, j) = value(i, j) + term
                    end do
                end do
                if (value(i, j) == zero) cycle
                value(i, j) = value(i, j)/volume
            end do
        end do
    end subroutine reluctivity_density_matrix

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
        value%transverse_curl_coefficient = zero
        do i = 1, 2
            do j = 1, 2
                value%longitudinal_diffusion(i, j) = zero
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
            do i = 1, 2
                do j = 1, 2
                    value%longitudinal_diffusion(i, j) = &
                        material%nubar_t(i, j)
                end do
            end do
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

    !> Create the n=0 scalar current-density source J^3.
    function fourier_source_scalar(c, current, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: current
        integer, intent(in) :: mode
        type(fourier_source_t) :: value

        if (.not. associated(c%a)) return
        if (mode /= 0) return
        if (.not. is_valid(current)) return
        if (.not. same_arena(current, c%u(1))) return
        value%mode = mode
        value%branch = FOURIER_LONGITUDINAL
        value%components = 1
        value%value(1) = current
        value%value(2) = num(c%a, 0_int64)
        value%valid = .true.
    end function fourier_source_scalar

    !> Create an n/=0 transverse current-density source (J^1,J^2).
    function fourier_source_pair(c, current, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: current(2)
        integer, intent(in) :: mode
        type(fourier_source_t) :: value
        integer :: i

        if (.not. associated(c%a)) return
        if (mode == 0) return
        do i = 1, 2
            if (.not. is_valid(current(i))) return
            if (.not. same_arena(current(i), c%u(1))) return
            value%value(i) = current(i)
        end do
        value%mode = mode
        value%branch = FOURIER_TRANSVERSE
        value%components = 2
        value%valid = .true.
    end function fourier_source_pair

    !> Create a scalar normal-trace load for the n=0 branch.
    function fourier_load_scalar(c, load_value, trace, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: load_value
        integer, intent(in) :: trace, mode
        type(fourier_load_t) :: value

        if (.not. associated(c%a)) return
        if (trace /= TRACE_NORMAL .or. mode /= 0) return
        if (.not. is_valid(load_value)) return
        if (.not. same_arena(load_value, c%u(1))) return
        value%mode = mode
        value%branch = FOURIER_LONGITUDINAL
        value%trace = trace
        value%components = 1
        value%value(1) = load_value
        value%value(2) = num(c%a, 0_int64)
        value%valid = .true.
    end function fourier_load_scalar

    !> Create a two-component tangential edge load for the n/=0 branch.
    function fourier_load_pair(c, load_value, trace, mode) result(value)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: load_value(2)
        integer, intent(in) :: trace, mode
        type(fourier_load_t) :: value
        integer :: i

        if (.not. associated(c%a)) return
        if (trace /= TRACE_TANGENTIAL .or. mode == 0) return
        do i = 1, 2
            if (.not. is_valid(load_value(i))) return
            if (.not. same_arena(load_value(i), c%u(1))) return
            value%value(i) = load_value(i)
        end do
        value%mode = mode
        value%branch = FOURIER_TRANSVERSE
        value%trace = trace
        value%components = 2
        value%valid = .true.
    end function fourier_load_pair

    function fourier_source_valid(source) result(value)
        type(fourier_source_t), intent(in) :: source
        logical :: value
        integer :: i

        value = source%valid .and. source%components >= 1 .and. &
            source%components <= 2
        if (.not. value) return
        if (source%branch == FOURIER_LONGITUDINAL) then
            value = source%mode == 0 .and. source%components == 1
        else if (source%branch == FOURIER_TRANSVERSE) then
            value = source%mode /= 0 .and. source%components == 2
        else
            value = .false.
        end if
        if (.not. value) return
        do i = 1, source%components
            if (.not. is_valid(source%value(i))) then
                value = .false.
                return
            end if
        end do
    end function fourier_source_valid

    function fourier_load_valid(load) result(value)
        type(fourier_load_t), intent(in) :: load
        logical :: value
        integer :: i

        value = load%valid .and. load%components >= 1 .and. &
            load%components <= 2
        if (.not. value) return
        if (load%branch == FOURIER_LONGITUDINAL) then
            value = load%mode == 0 .and. load%trace == TRACE_NORMAL .and. &
                load%components == 1
        else if (load%branch == FOURIER_TRANSVERSE) then
            value = load%mode /= 0 .and. load%trace == TRACE_TANGENTIAL .and. &
                load%components == 2
        else
            value = .false.
        end if
        if (.not. value) return
        do i = 1, load%components
            if (.not. is_valid(load%value(i))) then
                value = .false.
                return
            end if
        end do
    end function fourier_load_valid

    function fourier_source_matches(form, source) result(value)
        type(fourier_weak_form_t), intent(in) :: form
        type(fourier_source_t), intent(in) :: source
        logical :: value

        value = fourier_weak_form_valid(form) .and. fourier_source_valid(source)
        if (.not. value) return
        value = form%mode == source%mode .and. form%branch == source%branch .and. &
            form%test_components == source%components
    end function fourier_source_matches

    function fourier_load_matches(form, load) result(value)
        type(fourier_weak_form_t), intent(in) :: form
        type(fourier_load_t), intent(in) :: load
        logical :: value

        value = fourier_weak_form_valid(form) .and. fourier_load_valid(load)
        if (.not. value) return
        value = form%mode == load%mode .and. form%branch == load%branch .and. &
            form%boundary_trace == load%trace .and. &
            form%test_components == load%components
    end function fourier_load_matches

    !> Return one component of the scalar branch's constitutive flux.
    !>
    !> The variational boundary term is -w n_i q_i with
    !> q_i = nubar_t(i,j) partial_j A_3.  The caller supplies the outward
    !> normal and performs the boundary contraction, so this owner does not
    !> introduce a normal orientation or a surface measure.
    function fourier_longitudinal_flux_component(c, material, potential, &
            component) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential
        integer, intent(in) :: component
        type(expr_t) :: value
        type(expr_t) :: gradient_one, gradient_two

        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        if (.not. is_valid(potential)) return
        if (.not. same_arena(potential, c%u(1))) return
        if (component < 1 .or. component > 2) return

        gradient_one = diff(potential, c%u(1))
        gradient_two = diff(potential, c%u(2))
        select case (component)
        case (1)
            value = material%nubar_t(1, 1)*gradient_one + &
                material%nubar_t(1, 2)*gradient_two
        case (2)
            value = material%nubar_t(2, 1)*gradient_one + &
                material%nubar_t(2, 2)*gradient_two
        end select
    end function fourier_longitudinal_flux_component

    !> Return the scalar branch of the transverse edge boundary flux.
    !>
    !> The variational boundary term is -w_k s_k q with
    !> q = nu33 curl_t(a), where s_k = -E_t(k,j) n_j.  The caller supplies
    !> the boundary tangent/normal convention and surface measure.
    function fourier_transverse_flux_scalar(c, material, potential) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential(2)
        type(expr_t) :: value
        type(expr_t) :: curl_scalar
        integer :: i

        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        do i = 1, 2
            if (.not. is_valid(potential(i))) return
            if (.not. same_arena(potential(i), c%u(1))) return
        end do

        curl_scalar = diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2))
        value = material%nu33*curl_scalar
    end function fourier_transverse_flux_scalar

    !> Contract the scalar-branch flux q_i with an outward covariant normal.
    !>
    !> `normal` contains the two coordinate components n_i.  No normalization,
    !> surface measure, or minus sign from the weak boundary term is introduced
    !> here; the returned value is n_i q_i.
    function fourier_longitudinal_boundary_flux(c, material, potential, &
            normal) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential, normal(2)
        type(expr_t) :: value
        type(expr_t) :: gradient_one, gradient_two
        integer :: i

        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        if (.not. is_valid(potential)) return
        if (.not. same_arena(potential, c%u(1))) return
        do i = 1, 2
            if (.not. is_valid(normal(i))) return
            if (.not. same_arena(normal(i), c%u(1))) return
        end do
        gradient_one = diff(potential, c%u(1))
        gradient_two = diff(potential, c%u(2))
        value = normal(1)*(material%nubar_t(1, 1)*gradient_one + &
            material%nubar_t(1, 2)*gradient_two) + normal(2)*( &
            material%nubar_t(2, 1)*gradient_one + &
            material%nubar_t(2, 2)*gradient_two)
    end function fourier_longitudinal_boundary_flux

    !> Return the two edge-branch boundary coefficients s_k q.
    !>
    !> For the two-dimensional antisymmetric density convention,
    !> `s_1 = -n_2`, `s_2 = n_1`, and q is the transverse scalar flux.  The
    !> caller supplies the edge test components and any surface measure.
    subroutine fourier_transverse_boundary_flux(c, material, potential, &
            normal, value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential(2), normal(2)
        type(expr_t), intent(out) :: value(2)
        type(expr_t) :: flux
        integer :: i

        do i = 1, 2
            value(i) = expr_t()
        end do
        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        do i = 1, 2
            if (.not. is_valid(potential(i))) return
            if (.not. same_arena(potential(i), c%u(1))) return
            if (.not. is_valid(normal(i))) return
            if (.not. same_arena(normal(i), c%u(1))) return
        end do
        flux = fourier_transverse_flux(c, material, potential)
        if (.not. is_valid(flux)) return
        value(1) = -normal(2)*flux
        value(2) = normal(1)*flux
    end subroutine fourier_transverse_boundary_flux

    !> Contract the edge-branch boundary coefficients with an edge test.
    !>
    !> The returned value is w_k s_k q.  The weak-form sign remains with the
    !> caller, which may choose the convention `-w_k s_k q`.
    function fourier_transverse_boundary_contraction(c, material, potential, &
            normal, test) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential(2), normal(2), test(2)
        type(expr_t) :: value, flux
        integer :: i

        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        do i = 1, 2
            if (.not. is_valid(potential(i))) return
            if (.not. same_arena(potential(i), c%u(1))) return
            if (.not. is_valid(normal(i))) return
            if (.not. same_arena(normal(i), c%u(1))) return
            if (.not. is_valid(test(i))) return
            if (.not. same_arena(test(i), c%u(1))) return
        end do
        flux = fourier_transverse_flux(c, material, potential)
        if (.not. is_valid(flux)) return
        value = test(1)*(-normal(2)*flux) + test(2)*normal(1)*flux
    end function fourier_transverse_boundary_contraction

    !> Residual of the n=0 longitudinal scalar equation.
    !>
    !> The scalar unknown is A_3 and the supplied source is the longitudinal
    !> contravariant current density J^3.  The returned expression is
    !> -div_t(nubar_t grad_t A_3) - J^3, so a solution is represented by zero.
    function fourier_longitudinal_residual_scalar(c, material, potential, &
            current) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential, current
        type(expr_t) :: value
        type(expr_t) :: gradient_one, gradient_two
        type(expr_t) :: flux_one, flux_two

        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        if (.not. is_valid(potential)) return
        if (.not. is_valid(current)) return
        if (.not. same_arena(potential, c%u(1))) return
        if (.not. same_arena(current, c%u(1))) return

        gradient_one = diff(potential, c%u(1))
        gradient_two = diff(potential, c%u(2))
        flux_one = material%nubar_t(1, 1)*gradient_one + &
            material%nubar_t(1, 2)*gradient_two
        flux_two = material%nubar_t(2, 1)*gradient_one + &
            material%nubar_t(2, 2)*gradient_two
        value = -(diff(flux_one, c%u(1)) + diff(flux_two, c%u(2))) - current
    end function fourier_longitudinal_residual_scalar

    !> Residual of the n/=0 transverse two-component equation.
    !>
    !> The supplied potential is the covariant transverse pair (A_1,A_2)
    !> and the source is the contravariant density pair (J^1,J^2).  The
    !> result is curl_t(nu33 curl_t(a)) + n^2 nubar_t a - j.
    function fourier_transverse_residual_integer(c, material, potential, &
            current, mode) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential(2), current(2)
        integer, intent(in) :: mode
        type(expr_t) :: value(2)
        type(expr_t) :: mode_square

        mode_square = num(c%a, int(mode, int64)*int(mode, int64))
        value = fourier_transverse_residual_from_square(c, material, potential, &
            current, mode_square)
    end function fourier_transverse_residual_integer

    function fourier_transverse_residual_expression(c, material, potential, &
            current, mode) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential(2), current(2), mode
        type(expr_t) :: value(2)
        type(expr_t) :: mode_square

        mode_square = mode*mode
        value = fourier_transverse_residual_from_square(c, material, potential, &
            current, mode_square)
    end function fourier_transverse_residual_expression

    function fourier_transverse_residual_from_square(c, material, potential, &
            current, mode_square) result(value)
        type(chart_t), intent(in) :: c
        type(fourier_constitutive_t), intent(in) :: material
        type(expr_t), intent(in) :: potential(2), current(2), mode_square
        type(expr_t) :: value(2)
        type(expr_t) :: curl_scalar, weighted_curl
        type(expr_t) :: mass_one, mass_two
        integer :: i

        do i = 1, 2
            value(i) = expr_t()
        end do
        if (.not. associated(c%a)) return
        if (.not. material%valid) return
        if (.not. is_valid(mode_square)) return
        if (.not. same_arena(mode_square, c%u(1))) return
        do i = 1, 2
            if (.not. is_valid(potential(i))) return
            if (.not. is_valid(current(i))) return
            if (.not. same_arena(potential(i), c%u(1))) return
            if (.not. same_arena(current(i), c%u(1))) return
        end do

        curl_scalar = diff(potential(2), c%u(1)) - &
            diff(potential(1), c%u(2))
        weighted_curl = material%nu33*curl_scalar
        mass_one = material%nubar_t(1, 1)*potential(1) + &
            material%nubar_t(1, 2)*potential(2)
        mass_two = material%nubar_t(2, 1)*potential(1) + &
            material%nubar_t(2, 2)*potential(2)
        value(1) = diff(weighted_curl, c%u(2)) + mode_square*mass_one - &
            current(1)
        value(2) = -diff(weighted_curl, c%u(1)) + mode_square*mass_two - &
            current(2)
    end function fourier_transverse_residual_from_square

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
