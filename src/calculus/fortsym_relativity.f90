module fortsym_relativity
    ! Dimension-aware pseudo-Riemannian metric and curvature owner.
    !
    ! The first supported dimension is four, but the owner stores the runtime
    ! dimension explicitly and all contractions use it. This is the bridge
    ! from the fixed-3D chart toolkit to relativity without smuggling a fourth
    ! coordinate into a 3D type.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, is_valid, same_arena, &
        operator(+), operator(-), operator(*), operator(/), operator(==), &
        expr_abs => abs, sqrt
    use fortsym_diff, only: diff
    use fortsym_subs, only: subs
    implicit none
    private

    integer, parameter, public :: SPACETIME_DIM = 4

    type, public :: spacetime_metric_t
        private
        type(arena_t), pointer :: a => null()
        type(expr_t) :: component(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: coordinate(SPACETIME_DIM)
        integer :: dimension = 0
        integer :: signature(SPACETIME_DIM) = 0
        integer :: orientation = 0
        logical :: valid = .false.
        logical :: has_coordinates = .false.
    end type spacetime_metric_t

    public :: spacetime_metric_create, spacetime_metric_covariant
    public :: spacetime_metric_contravariant, spacetime_metric_det
    public :: spacetime_metric_sqrtg, spacetime_metric_signature
    public :: spacetime_metric_orientation, spacetime_metric_dimension
    public :: spacetime_metric_valid
    public :: spacetime_metric_arena, spacetime_metric_coordinates
    public :: spacetime_metric_has_coordinates
    public :: spacetime_christoffel, spacetime_riemann, spacetime_ricci
    public :: spacetime_scalar_curvature, spacetime_einstein
    public :: spacetime_geodesic_residual

contains

    !> Construct a nondegenerate-metadata metric in dimensions 1..4.
    function spacetime_metric_create(components, dimension, coordinates, &
            signature, orientation) result(result)
        type(expr_t), intent(in) :: components(SPACETIME_DIM, SPACETIME_DIM)
        integer, intent(in) :: dimension
        type(expr_t), optional, intent(in) :: coordinates(SPACETIME_DIM)
        integer, optional, intent(in) :: signature(SPACETIME_DIM)
        integer, optional, intent(in) :: orientation
        type(spacetime_metric_t) :: result
        integer :: i, j

        if (dimension < 1 .or. dimension > SPACETIME_DIM) return
        if (.not. is_valid(components(1, 1))) return
        result%a => components(1, 1)%a
        do i = 1, dimension
            do j = 1, dimension
                if (.not. is_valid(components(i, j))) return
                if (.not. associated(components(i, j)%a, result%a)) return
            end do
        end do
        result%dimension = dimension
        result%signature = 1
        if (present(signature)) result%signature = signature
        result%orientation = 1
        if (present(orientation)) result%orientation = orientation
        if (any(abs(result%signature(1:dimension)) /= 1)) return
        if (abs(result%orientation) /= 1) return
        result%component = components
        if (present(coordinates)) then
            do i = 1, dimension
                if (.not. is_valid(coordinates(i))) return
                if (.not. associated(coordinates(i)%a, result%a)) return
                result%coordinate(i) = coordinates(i)
            end do
            result%has_coordinates = .true.
        end if
        result%valid = .true.
        if (all_components_zero(result)) result%valid = .false.
    end function spacetime_metric_create

    function spacetime_metric_covariant(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM, SPACETIME_DIM)

        if (.not. spacetime_metric_valid(g)) return
        value = g%component
    end function spacetime_metric_covariant

    function spacetime_metric_contravariant(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: determinant
        type(expr_t) :: one
        integer :: i, j

        if (.not. spacetime_metric_valid(g)) return
        value = num(g%a, 0)
        if (is_diagonal_metric(g)) then
            one = num(g%a, 1)
            do i = 1, g%dimension
                value(i, i) = one/g%component(i, i)
            end do
            return
        end if
        determinant = spacetime_metric_det(g)
        do i = 1, g%dimension
            do j = 1, g%dimension
                value(i, j) = cofactor(g, j, i)/determinant
            end do
        end do
    end function spacetime_metric_contravariant

    function spacetime_metric_det(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value
        integer :: i

        if (.not. spacetime_metric_valid(g)) return
        if (is_diagonal_metric(g)) then
            value = num(g%a, 1)
            do i = 1, g%dimension
                value = value*g%component(i, i)
            end do
        else
            select case (g%dimension)
            case (1)
                value = g%component(1, 1)
            case (2)
                value = g%component(1, 1)*g%component(2, 2) - &
                    g%component(1, 2)*g%component(2, 1)
            case (3)
                value = determinant3(g%component)
            case (4)
                value = g%component(1, 1)*cofactor(g, 1, 1) + &
                    g%component(1, 2)*cofactor(g, 1, 2) + &
                    g%component(1, 3)*cofactor(g, 1, 3) + &
                    g%component(1, 4)*cofactor(g, 1, 4)
            end select
        end if
    end function spacetime_metric_det

    function spacetime_metric_sqrtg(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value

        if (.not. spacetime_metric_valid(g)) return
        value = sqrt(expr_abs(spacetime_metric_det(g)))
    end function spacetime_metric_sqrtg

    function spacetime_metric_signature(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        integer :: value(SPACETIME_DIM)

        value = 0
        if (spacetime_metric_valid(g)) value = g%signature
    end function spacetime_metric_signature

    function spacetime_metric_dimension(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        integer :: value

        value = 0
        if (spacetime_metric_valid(g)) value = g%dimension
    end function spacetime_metric_dimension

    function spacetime_metric_orientation(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        integer :: value

        value = 0
        if (spacetime_metric_valid(g)) value = g%orientation
    end function spacetime_metric_orientation

    function spacetime_metric_valid(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        logical :: value
        integer :: i, j

        value = g%valid .and. associated(g%a)
        if (.not. value) return
        if (g%dimension < 1 .or. g%dimension > SPACETIME_DIM) then
            value = .false.
            return
        end if
        if (any(abs(g%signature(1:g%dimension)) /= 1)) then
            value = .false.
            return
        end if
        if (abs(g%orientation) /= 1) then
            value = .false.
            return
        end if
        do i = 1, g%dimension
            do j = 1, g%dimension
                if (.not. is_valid(g%component(i, j))) value = .false.
                if (.not. associated(g%component(i, j)%a, g%a)) value = .false.
            end do
        end do
    end function spacetime_metric_valid

    function spacetime_metric_arena(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(arena_t), pointer :: value

        value => g%a
    end function spacetime_metric_arena

    function spacetime_metric_coordinates(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM)
        integer :: i

        if (.not. spacetime_metric_has_coordinates(g)) return
        do i = 1, g%dimension
            value(i) = g%coordinate(i)
        end do
    end function spacetime_metric_coordinates

    function spacetime_metric_has_coordinates(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        logical :: value

        value = spacetime_metric_valid(g) .and. g%has_coordinates
    end function spacetime_metric_has_coordinates

    !> Levi-Civita connection Gamma^a_bc for the supplied coordinates.
    function spacetime_christoffel(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: inverse(SPACETIME_DIM, SPACETIME_DIM), term
        integer :: a, b, c, l

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        inverse = spacetime_metric_contravariant(g)
        value = num(g%a, 0)
        do a = 1, g%dimension
            do b = 1, g%dimension
                do c = 1, g%dimension
                    value(a, b, c) = num(g%a, 0)
                    do l = 1, g%dimension
                        term = diff(g%component(l, c), g%coordinate(b)) + &
                            diff(g%component(l, b), g%coordinate(c)) - &
                            diff(g%component(b, c), g%coordinate(l))
                        value(a, b, c) = value(a, b, c) + &
                            inverse(a, l)*term/2
                    end do
                end do
            end do
        end do
    end function spacetime_christoffel

    !> R^a_bcd = partial_c Gamma^a_db - partial_d Gamma^a_cb + products.
    function spacetime_riemann(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM, SPACETIME_DIM, &
            SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: gamma(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: term
        integer :: a, b, c, d, m

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        gamma = spacetime_christoffel(g)
        value = num(g%a, 0)
        do a = 1, g%dimension
            do b = 1, g%dimension
                do c = 1, g%dimension
                    do d = 1, g%dimension
                        term = diff(gamma(a, d, b), g%coordinate(c)) - &
                            diff(gamma(a, c, b), g%coordinate(d))
                        do m = 1, g%dimension
                            term = term + gamma(a, c, m)*gamma(m, d, b) - &
                                gamma(a, d, m)*gamma(m, c, b)
                        end do
                        value(a, b, c, d) = term
                    end do
                end do
            end do
        end do
    end function spacetime_riemann

    function spacetime_ricci(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: riemann(SPACETIME_DIM, SPACETIME_DIM, &
            SPACETIME_DIM, SPACETIME_DIM)
        integer :: a, b, d

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        riemann = spacetime_riemann(g)
        value = num(g%a, 0)
        do b = 1, g%dimension
            do d = 1, g%dimension
                value(b, d) = num(g%a, 0)
                do a = 1, g%dimension
                    value(b, d) = value(b, d) + riemann(a, b, a, d)
                end do
            end do
        end do
    end function spacetime_ricci

    function spacetime_scalar_curvature(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value
        type(expr_t) :: inverse(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: ricci(SPACETIME_DIM, SPACETIME_DIM)
        integer :: i, j

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        inverse = spacetime_metric_contravariant(g)
        ricci = spacetime_ricci(g)
        value = num(g%a, 0)
        do i = 1, g%dimension
            do j = 1, g%dimension
                value = value + inverse(i, j)*ricci(i, j)
            end do
        end do
    end function spacetime_scalar_curvature

    function spacetime_einstein(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t) :: value(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: ricci(SPACETIME_DIM, SPACETIME_DIM), scalar, half
        integer :: i, j

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        ricci = spacetime_ricci(g)
        scalar = spacetime_scalar_curvature(g)
        half = num(g%a, 1)/num(g%a, 2)
        value = num(g%a, 0)
        do i = 1, g%dimension
            do j = 1, g%dimension
                value(i, j) = ricci(i, j) - half*scalar*g%component(i, j)
            end do
        end do
    end function spacetime_einstein

    function spacetime_geodesic_residual(g, curve, parameter) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: curve(SPACETIME_DIM), parameter
        type(expr_t) :: value(SPACETIME_DIM)
        type(expr_t) :: gamma(SPACETIME_DIM, SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: velocity(SPACETIME_DIM), acceleration, term
        integer :: a, b, c

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        if (.not. is_valid(parameter)) return
        if (.not. same_arena(parameter, g%component(1, 1))) return
        do a = 1, SPACETIME_DIM
            if (.not. is_valid(curve(a))) return
            if (.not. same_arena(curve(a), g%component(1, 1))) return
        end do
        gamma = spacetime_christoffel(g)
        value = num(g%a, 0)
        do b = 1, g%dimension
            velocity(b) = diff(curve(b), parameter)
        end do
        do a = 1, g%dimension
            acceleration = diff(diff(curve(a), parameter), parameter)
            value(a) = acceleration
            do b = 1, g%dimension
                do c = 1, g%dimension
                    term = substitute_curve(gamma(a, b, c), g, curve)
                    value(a) = value(a) + term*velocity(b)*velocity(c)
                end do
            end do
        end do
    end function spacetime_geodesic_residual

    function substitute_curve(expression, g, curve) result(value)
        type(expr_t), intent(in) :: expression
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: curve(SPACETIME_DIM)
        type(expr_t) :: value
        integer :: i

        value = expression
        do i = 1, g%dimension
            value = subs(value, g%coordinate(i), curve(i))
        end do
    end function substitute_curve

    function cofactor(g, row, column) result(value)
        type(spacetime_metric_t), intent(in) :: g
        integer, intent(in) :: row, column
        type(expr_t) :: value
        integer :: rows(3), columns(3)

        call remaining_indices(row, rows)
        call remaining_indices(column, columns)
        value = minor3(g%component, rows, columns)
        if (mod(row + column, 2) == 1) value = -value
    end function cofactor

    function minor3(matrix, rows, columns) result(value)
        type(expr_t), intent(in) :: matrix(SPACETIME_DIM, SPACETIME_DIM)
        integer, intent(in) :: rows(3), columns(3)
        type(expr_t) :: value

        value = matrix(rows(1), columns(1)) * ( &
            matrix(rows(2), columns(2))*matrix(rows(3), columns(3)) - &
            matrix(rows(2), columns(3))*matrix(rows(3), columns(2))) - &
            matrix(rows(1), columns(2)) * ( &
            matrix(rows(2), columns(1))*matrix(rows(3), columns(3)) - &
            matrix(rows(2), columns(3))*matrix(rows(3), columns(1))) + &
            matrix(rows(1), columns(3)) * ( &
            matrix(rows(2), columns(1))*matrix(rows(3), columns(2)) - &
            matrix(rows(2), columns(2))*matrix(rows(3), columns(1)))
    end function minor3

    function determinant3(matrix) result(value)
        type(expr_t), intent(in) :: matrix(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: value

        value = matrix(1, 1)*(matrix(2, 2)*matrix(3, 3) - &
            matrix(2, 3)*matrix(3, 2)) - matrix(1, 2)*( &
            matrix(2, 1)*matrix(3, 3) - matrix(2, 3)*matrix(3, 1)) + &
            matrix(1, 3)*(matrix(2, 1)*matrix(3, 2) - &
            matrix(2, 2)*matrix(3, 1))
    end function determinant3

    subroutine remaining_indices(excluded, values)
        integer, intent(in) :: excluded
        integer, intent(out) :: values(3)
        integer :: i, n

        n = 0
        do i = 1, SPACETIME_DIM
            if (i == excluded) cycle
            n = n + 1
            values(n) = i
        end do
    end subroutine remaining_indices

    function all_components_zero(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        logical :: value
        type(expr_t) :: zero
        integer :: i, j

        value = .true.
        zero = num(g%a, 0)
        do i = 1, g%dimension
            do j = 1, g%dimension
                if (.not. (g%component(i, j) == zero)) value = .false.
            end do
        end do
    end function all_components_zero

    function is_diagonal_metric(g) result(value)
        type(spacetime_metric_t), intent(in) :: g
        logical :: value
        type(expr_t) :: zero
        integer :: i, j

        value = .false.
        if (.not. spacetime_metric_valid(g)) return
        zero = num(g%a, 0)
        value = .true.
        do i = 1, g%dimension
            do j = 1, g%dimension
                if (i == j) cycle
                if (.not. (g%component(i, j) == zero)) value = .false.
            end do
        end do
    end function is_diagonal_metric

end module fortsym_relativity
