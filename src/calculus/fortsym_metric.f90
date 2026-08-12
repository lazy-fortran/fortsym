module fortsym_metric
    ! Explicit metric metadata and metric-owned component operations.
    !
    ! A chart supplies a convenient Euclidean metric, but a metric is a
    ! separate owner: its signature and orientation are data, not properties
    ! inferred from a variable name or hidden in sqrtg. This fixed-3D owner is
    ! the first step toward the general manifold/patch contract in ROADMAP.md.
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: chart_t, DIM, &
        chart_metric_covariant => metric_covariant
    use fortsym_expr, only: expr_t, num, is_valid, &
        operator(+), operator(-), operator(*), operator(/), &
        operator(==), expr_abs => abs, sqrt
    implicit none
    private

    public :: metric_t, metric_create, metric_from_chart
    public :: metric_covariant, metric_contravariant, metric_det, metric_sqrtg
    public :: metric_signature, metric_orientation, metric_valid
    public :: metric_arena, metric_same_arena
    public :: metric_coordinates, metric_has_coordinates

    type :: metric_t
        private
        type(arena_t), pointer :: a => null()
        type(expr_t) :: component(DIM, DIM)
        type(expr_t) :: coordinate(DIM)
        integer :: signature(DIM) = 0
        integer :: orientation = 0
        logical :: valid = .false.
        logical :: has_coordinates = .false.
    end type metric_t

contains

    !> Construct a metric from coordinate components and explicit metadata.
    !>
    !> Signature entries are +1 or -1 and orientation is +1 or -1. The
    !> constructor checks arena ownership and metadata, but cannot prove that
    !> a symbolic determinant is nonzero; such a proof remains an engine-level
    !> obligation at the operation that needs the inverse.
    function metric_create(components, signature, orientation, coordinates) &
            result(result)
        type(expr_t), intent(in) :: components(DIM, DIM)
        integer, optional, intent(in) :: signature(DIM)
        integer, optional, intent(in) :: orientation
        type(expr_t), optional, intent(in) :: coordinates(DIM)
        type(metric_t) :: result
        integer :: i, j

        if (.not. is_valid(components(1, 1))) return
        result%a => components(1, 1)%a
        do i = 1, DIM
            do j = 1, DIM
                if (.not. is_valid(components(i, j))) return
                if (.not. associated(components(i, j)%a, result%a)) return
            end do
        end do

        result%signature = 1
        if (present(signature)) result%signature = signature
        result%orientation = 1
        if (present(orientation)) result%orientation = orientation
        if (any(abs(result%signature) /= 1)) return
        if (abs(result%orientation) /= 1) return

        result%component = components
        if (present(coordinates)) then
            do i = 1, DIM
                if (.not. is_valid(coordinates(i))) return
                if (.not. associated(coordinates(i)%a, result%a)) return
            end do
            result%coordinate = coordinates
            result%has_coordinates = .true.
        end if
        result%valid = .true.
        if (.not. metric_valid(result)) result%valid = .false.
    end function metric_create

    !> Materialize the metric induced by a chart with explicit metadata.
    function metric_from_chart(c, signature, orientation) result(result)
        type(chart_t), intent(in) :: c
        integer, optional, intent(in) :: signature(DIM)
        integer, optional, intent(in) :: orientation
        type(metric_t) :: result
        type(expr_t) :: components(DIM, DIM)

        components = chart_metric_covariant(c)
        result = metric_create(components, signature, orientation, c%u)
    end function metric_from_chart

    !> Covariant metric components g_ij.
    function metric_covariant(g) result(components)
        type(metric_t), intent(in) :: g
        type(expr_t) :: components(DIM, DIM)

        if (.not. g%valid) return
        components = g%component
    end function metric_covariant

    !> Contravariant metric components g^ij, the inverse metric.
    function metric_contravariant(g) result(inverse)
        type(metric_t), intent(in) :: g
        type(expr_t) :: inverse(DIM, DIM)
        type(expr_t) :: determinant

        if (.not. g%valid) return
        determinant = metric_det(g)
        inverse(1, 1) = cofactor(g, 1, 1)/determinant
        inverse(1, 2) = cofactor(g, 2, 1)/determinant
        inverse(1, 3) = cofactor(g, 3, 1)/determinant
        inverse(2, 1) = cofactor(g, 1, 2)/determinant
        inverse(2, 2) = cofactor(g, 2, 2)/determinant
        inverse(2, 3) = cofactor(g, 3, 2)/determinant
        inverse(3, 1) = cofactor(g, 1, 3)/determinant
        inverse(3, 2) = cofactor(g, 2, 3)/determinant
        inverse(3, 3) = cofactor(g, 3, 3)/determinant
    end function metric_contravariant

    !> Determinant of the covariant metric.
    function metric_det(g) result(determinant)
        type(metric_t), intent(in) :: g
        type(expr_t) :: determinant

        if (.not. g%valid) return
        determinant = g%component(1, 1)*(g%component(2, 2)*g%component(3, 3) - &
            g%component(2, 3)*g%component(3, 2)) - &
            g%component(1, 2)*(g%component(2, 1)*g%component(3, 3) - &
            g%component(2, 3)*g%component(3, 1)) + &
            g%component(1, 3)*(g%component(2, 1)*g%component(3, 2) - &
            g%component(2, 2)*g%component(3, 1))
    end function metric_det

    !> Positive metric volume density sqrt(abs(det(g))).
    !>
    !> Orientation is intentionally absent. An oriented volume form is a
    !> separate owner operation and may multiply this value by -1.
    function metric_sqrtg(g) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t) :: value

        if (.not. g%valid) return
        value = sqrt(expr_abs(metric_det(g)))
    end function metric_sqrtg

    !> Return the explicit metric signature, with zeroes for an invalid metric.
    function metric_signature(g) result(signature)
        type(metric_t), intent(in) :: g
        integer :: signature(DIM)

        signature = 0
        if (g%valid) signature = g%signature
    end function metric_signature

    !> Return the explicit orientation, or zero for an invalid metric.
    function metric_orientation(g) result(orientation)
        type(metric_t), intent(in) :: g
        integer :: orientation

        orientation = 0
        if (g%valid) orientation = g%orientation
    end function metric_orientation

    !> Check metadata, arena ownership, and component validity.
    function metric_valid(g) result(valid)
        type(metric_t), intent(in) :: g
        logical :: valid
        integer :: i, j

        valid = g%valid .and. associated(g%a)
        if (.not. valid) return
        if (any(abs(g%signature) /= 1)) then
            valid = .false.
            return
        end if
        if (abs(g%orientation) /= 1) then
            valid = .false.
            return
        end if
        do i = 1, DIM
            do j = 1, DIM
                if (.not. is_valid(g%component(i, j))) valid = .false.
                if (.not. associated(g%component(i, j)%a, g%a)) valid = .false.
            end do
        end do
        if (valid) then
            if (all_components_zero(g)) valid = .false.
        end if
    end function metric_valid

    !> Return the owning arena without exposing metric component storage.
    function metric_arena(g) result(a)
        type(metric_t), intent(in) :: g
        type(arena_t), pointer :: a

        a => g%a
    end function metric_arena

    !> Check that a tensor component arena is the metric's arena.
    function metric_same_arena(g, a) result(same)
        type(metric_t), intent(in) :: g
        type(arena_t), pointer, intent(in) :: a
        logical :: same

        same = metric_valid(g) .and. associated(g%a, a)
    end function metric_same_arena

    !> Return the coordinates with respect to which metric derivatives are read.
    function metric_coordinates(g) result(coordinates)
        type(metric_t), intent(in) :: g
        type(expr_t) :: coordinates(DIM)

        if (.not. metric_has_coordinates(g)) return
        coordinates = g%coordinate
    end function metric_coordinates

    !> Whether the metric carries an explicit coordinate tuple.
    function metric_has_coordinates(g) result(has_coordinates)
        type(metric_t), intent(in) :: g
        logical :: has_coordinates

        has_coordinates = metric_valid(g) .and. g%has_coordinates
    end function metric_has_coordinates

    function cofactor(g, i, j) result(value)
        type(metric_t), intent(in) :: g
        integer, intent(in) :: i, j
        type(expr_t) :: value
        integer :: r1, r2, c1, c2

        call other_indices(i, r1, r2)
        call other_indices(j, c1, c2)
        value = g%component(r1, c1)*g%component(r2, c2) - &
            g%component(r1, c2)*g%component(r2, c1)
        if (mod(i + j, 2) == 1) value = -value
    end function cofactor

    pure subroutine other_indices(index, first, second)
        integer, intent(in) :: index
        integer, intent(out) :: first, second

        select case (index)
        case (1); first = 2; second = 3
        case (2); first = 1; second = 3
        case default; first = 1; second = 2
        end select
    end subroutine other_indices

    function all_components_zero(g) result(zero)
        type(metric_t), intent(in) :: g
        logical :: zero
        type(expr_t) :: value
        integer :: i, j

        zero = .true.
        value = num(g%a, 0)
        do i = 1, DIM
            do j = 1, DIM
                if (.not. (g%component(i, j) == value)) zero = .false.
            end do
        end do
    end function all_components_zero

end module fortsym_metric
