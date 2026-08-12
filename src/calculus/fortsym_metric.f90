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
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, num, is_valid, &
        operator(+), operator(-), operator(*), operator(/), &
        operator(==), expr_abs => abs, sqrt
    use fortsym_geometry_metadata, only: orientation_t, signature_t, &
        orientation_create, orientation_valid, orientation_value, &
        signature_create, signature_valid, signature_dimension, signature_component
    implicit none
    private

    public :: metric_t, metric_create, metric_from_chart
    public :: metric_covariant, metric_contravariant, metric_det, metric_sqrtg
    public :: metric_surface_measure
    public :: metric_inner
    public :: metric_grad, metric_divergence, metric_laplacian
    public :: metric_signature, metric_orientation, metric_valid
    public :: metric_signature_type, metric_orientation_type
    public :: metric_create_metadata
    public :: metric_arena, metric_same_arena
    public :: metric_coordinates, metric_has_coordinates

    type :: metric_t
        private
        type(arena_t), pointer :: a => null()
        type(expr_t) :: component(DIM, DIM)
        type(expr_t) :: coordinate(DIM)
        type(signature_t) :: signature_metadata
        type(orientation_t) :: orientation_metadata
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
        integer :: signature_values(DIM), orientation_sign
        type(signature_t) :: signature_metadata
        type(orientation_t) :: orientation_metadata

        signature_values = 1
        if (present(signature)) signature_values = signature
        orientation_sign = 1
        if (present(orientation)) orientation_sign = orientation
        signature_metadata = signature_create(signature_values)
        orientation_metadata = orientation_create(orientation_sign)
        if (present(coordinates)) then
            result = metric_create_with_metadata(components, signature_metadata, &
                orientation_metadata, coordinates)
        else
            result = metric_create_with_metadata(components, signature_metadata, &
                orientation_metadata)
        end if
    end function metric_create

    !> Construct a metric from typed metadata owners.
    function metric_create_metadata(components, signature, orientation, coordinates) &
            result(result)
        type(expr_t), intent(in) :: components(DIM, DIM)
        type(signature_t), intent(in) :: signature
        type(orientation_t), intent(in) :: orientation
        type(expr_t), optional, intent(in) :: coordinates(DIM)
        type(metric_t) :: result

        if (present(coordinates)) then
            result = metric_create_with_metadata(components, signature, &
                orientation, coordinates)
        else
            result = metric_create_with_metadata(components, signature, orientation)
        end if
    end function metric_create_metadata

    function metric_create_with_metadata(components, signature_metadata, &
            orientation_metadata, coordinates) result(result)
        type(expr_t), intent(in) :: components(DIM, DIM)
        type(signature_t), intent(in) :: signature_metadata
        type(orientation_t), intent(in) :: orientation_metadata
        type(expr_t), optional, intent(in) :: coordinates(DIM)
        type(metric_t) :: result
        integer :: i, j

        if (.not. signature_valid(signature_metadata)) return
        if (signature_dimension(signature_metadata) /= DIM) return
        if (.not. orientation_valid(orientation_metadata)) return
        if (.not. is_valid(components(1, 1))) return
        result%a => components(1, 1)%a
        do i = 1, DIM
            do j = 1, DIM
                if (.not. is_valid(components(i, j))) return
                if (.not. associated(components(i, j)%a, result%a)) return
            end do
        end do

        result%signature_metadata = signature_metadata
        result%orientation_metadata = orientation_metadata
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
    end function metric_create_with_metadata

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
        type(expr_t) :: one
        integer :: i, j

        if (.not. g%valid) return
        if (is_diagonal_metric(g)) then
            one = num(g%a, 1)
            inverse = num(g%a, 0)
            do i = 1, DIM
                inverse(i, i) = one/g%component(i, i)
            end do
            return
        end if
        determinant = metric_det(g)
        do i = 1, DIM
            do j = 1, DIM
                inverse(i, j) = cofactor(g, j, i)/determinant
            end do
        end do
    end function metric_contravariant

    !> Determinant of the covariant metric.
    function metric_det(g) result(determinant)
        type(metric_t), intent(in) :: g
        type(expr_t) :: determinant

        if (.not. g%valid) return
        if (is_diagonal_metric(g)) then
            determinant = g%component(1, 1)*g%component(2, 2)*g%component(3, 3)
            return
        end if
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

    !> Positive induced measure on u(normal_index)=const.
    !>
    !> The absolute value is required because an explicit metric may be
    !> pseudo-Riemannian. Orientation remains separate and is not folded into
    !> this positive surface density.
    function metric_surface_measure(g, normal_index) result(value)
        type(metric_t), intent(in) :: g
        integer, intent(in) :: normal_index
        type(expr_t) :: value
        type(expr_t) :: minor
        integer :: first, second

        if (.not. metric_valid(g)) return
        if (normal_index < 1 .or. normal_index > DIM) return
        call other_indices(normal_index, first, second)
        minor = g%component(first, first)*g%component(second, second) - &
            g%component(first, second)*g%component(second, first)
        value = sqrt(expr_abs(minor))
    end function metric_surface_measure

    !> Metric inner product of two contravariant component vectors.
    !>
    !> The contraction is kept here, rather than reimplemented by magnetic or
    !> tensor callers, so nonorthogonal metrics have one checked and optimized
    !> path: g_ij a^i b^j.  The fixed dimension keeps the small accumulation
    !> cheaper than materializing a lowered vector or a metric component array
    !> for each call.
    function metric_inner(g, left, right) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t), intent(in) :: left(DIM), right(DIM)
        type(expr_t) :: value
        type(expr_t) :: term
        integer :: i, j

        if (.not. metric_valid(g)) return
        do i = 1, DIM
            if (.not. is_valid(left(i))) return
            if (.not. associated(left(i)%a, g%a)) return
            if (.not. is_valid(right(i))) return
            if (.not. associated(right(i)%a, g%a)) return
        end do

        value = num(g%a, 0)
        do i = 1, DIM
            if (is_zero_expr(left(i))) cycle
            do j = 1, DIM
                if (is_zero_expr(g%component(i, j))) cycle
                if (is_zero_expr(right(j))) cycle
                term = g%component(i, j)*left(i)*right(j)
                if (.not. is_zero_expr(term)) value = value + term
            end do
        end do
    end function metric_inner

    !> Contravariant metric gradient (grad f)^i = g^ij partial_j f.
    !>
    !> This is the metric-owner counterpart of chart `grad`. It uses the
    !> supplied coordinate tuple rather than a chart position map, so a
    !> pseudo-Riemannian metric can be used without inventing an embedding.
    function metric_grad(g, f) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t), intent(in) :: f
        type(expr_t) :: value(DIM)
        type(expr_t) :: inverse(DIM, DIM), coordinates(DIM)
        type(expr_t) :: derivative, term
        integer :: i, j

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. is_valid(f)) return
        if (.not. associated(f%a, g%a)) return
        inverse = metric_contravariant(g)
        coordinates = metric_coordinates(g)
        value = num(g%a, 0)
        do j = 1, DIM
            derivative = diff(f, coordinates(j))
            if (is_zero_expr(derivative)) cycle
            do i = 1, DIM
                if (is_zero_expr(inverse(i, j))) cycle
                term = inverse(i, j)*derivative
                if (is_zero_expr(value(i))) then
                    value(i) = term
                else
                    value(i) = value(i) + term
                end if
            end do
        end do
    end function metric_grad

    !> Divergence of a contravariant vector using sqrt(abs(det(g))).
    !>
    !> The positive metric volume density is used for both Riemannian and
    !> pseudo-Riemannian metrics; orientation belongs to the separate volume
    !> form owner and must not leak into this scalar divergence.
    function metric_divergence(g, vector) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t), intent(in) :: vector(DIM)
        type(expr_t) :: value
        type(expr_t) :: coordinates(DIM), inverse(DIM, DIM)
        type(expr_t) :: derivative, term, half
        integer :: i, j, k

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, g%a)) return
        end do
        coordinates = metric_coordinates(g)
        inverse = metric_contravariant(g)
        half = num(g%a, 1)/num(g%a, 2)
        value = num(g%a, 0)
        do i = 1, DIM
            if (is_zero_expr(vector(i))) cycle
            term = diff(vector(i), coordinates(i))
            do j = 1, DIM
                do k = 1, DIM
                    derivative = diff(g%component(j, k), coordinates(i))
                    if (is_zero_expr(derivative)) cycle
                    if (is_zero_expr(inverse(k, j))) cycle
                    term = term + half*vector(i)*inverse(k, j)*derivative
                end do
            end do
            if (.not. is_zero_expr(term)) value = value + term
        end do
    end function metric_divergence

    !> Laplace--Beltrami operator, including its pseudo-Riemannian wave form.
    function metric_laplacian(g, f) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t), intent(in) :: f
        type(expr_t) :: value
        type(expr_t) :: gradient(DIM)

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        gradient = metric_grad(g, f)
        value = metric_divergence(g, gradient)
    end function metric_laplacian

    !> Return the explicit metric signature, with zeroes for an invalid metric.
    function metric_signature(g) result(signature)
        type(metric_t), intent(in) :: g
        integer :: signature(DIM)
        integer :: i

        signature = 0
        if (.not. metric_valid(g)) return
        do i = 1, DIM
            signature(i) = signature_component(g%signature_metadata, i)
        end do
    end function metric_signature

    function metric_signature_type(g) result(signature)
        type(metric_t), intent(in) :: g
        type(signature_t) :: signature

        if (metric_valid(g)) signature = g%signature_metadata
    end function metric_signature_type

    !> Return the explicit orientation, or zero for an invalid metric.
    function metric_orientation(g) result(orientation)
        type(metric_t), intent(in) :: g
        integer :: orientation

        orientation = 0
        if (metric_valid(g)) orientation = orientation_value(g%orientation_metadata)
    end function metric_orientation

    function metric_orientation_type(g) result(orientation)
        type(metric_t), intent(in) :: g
        type(orientation_t) :: orientation

        if (metric_valid(g)) orientation = g%orientation_metadata
    end function metric_orientation_type

    !> Check metadata, arena ownership, and component validity.
    function metric_valid(g) result(valid)
        type(metric_t), intent(in) :: g
        logical :: valid
        integer :: i, j

        valid = g%valid .and. associated(g%a)
        if (.not. valid) return
        if (.not. signature_valid(g%signature_metadata)) then
            valid = .false.
            return
        end if
        if (.not. orientation_valid(g%orientation_metadata)) then
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

    function is_diagonal_metric(g) result(diagonal)
        type(metric_t), intent(in) :: g
        logical :: diagonal
        type(expr_t) :: zero
        integer :: i, j

        diagonal = .false.
        if (.not. associated(g%a)) return
        zero = num(g%a, 0)
        diagonal = .true.
        do i = 1, DIM
            do j = 1, DIM
                if (i /= j) then
                    if (.not. (g%component(i, j) == zero)) diagonal = .false.
                end if
            end do
        end do
    end function is_diagonal_metric

    function is_zero_expr(value) result(zero)
        type(expr_t), intent(in) :: value
        logical :: zero

        zero = .false.
        if (.not. is_valid(value)) return
        zero = value == num(value%a, 0)
    end function is_zero_expr

end module fortsym_metric
