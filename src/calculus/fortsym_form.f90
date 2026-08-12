module fortsym_form
    ! Coordinate differential forms for the native geometry toolkit.
    !
    ! A form stores coefficients in the ordered coordinate coframe
    ! du^1,du^2,du^3. The bit mask of a component names its ordered basis
    ! wedge: 1, 2, 4 are du^1, du^2, du^3; 3, 5, 6 are du^1^du^2,
    ! du^1^du^3, du^2^du^3; and 7 is the volume basis. This representation is
    ! small, explicit, and keeps antisymmetry in the owner rather than in a
    ! caller convention.
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: chart_t, DIM, chart_valid, metric_covariant, &
        metric_contravariant, sqrtg
    use fortsym_metric, only: metric_t, metric_valid, metric_arena, &
        metric_orientation, metric_sqrtg, metric_signature, &
        metric_coordinate, metric_coordinates, metric_has_coordinates, &
        metric_contravariant_owner => metric_contravariant
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, num, is_valid, operator(+), operator(-), &
        operator(*), operator(/), operator(==)
    implicit none
    private

    public :: form_t, form, form_scalar, form_one, form_two, form_three
    public :: form_zero
    public :: volume_form, volume
    public :: form_component, form_degree, form_valid
    public :: form_chart_bound, form_same_chart, form_chart_compatible, &
        form_metric_compatible, form_bind_chart, form_bind_metric, &
        form_copy_owner, form_merge_owner
    public :: add_forms, subtract_forms, negate_form
    public :: wedge, d, exterior_diff, star, hodge_star
    public :: codifferential, codiff, laplace_de_rham
    public :: interior, interior_product, lie, lie_derivative
    public :: flat, sharp, scale_form

    type :: form_t
        type(arena_t), pointer :: a => null()
        integer :: degree = -1
        logical :: zero_extension = .false.
        logical :: chart_bound = .false.
        logical :: chart_has_position = .false.
        type(expr_t) :: chart_coordinates(DIM)
        type(expr_t) :: chart_position(DIM)
        type(expr_t) :: component(0:2**DIM - 1)
    end type form_t

    interface form
        module procedure form_scalar
    end interface form

    interface d
        module procedure exterior_diff_chart
        module procedure exterior_diff_metric
    end interface d

    interface exterior_diff
        module procedure exterior_diff_chart
        module procedure exterior_diff_metric
    end interface exterior_diff

    interface star
        module procedure hodge_star_chart
        module procedure hodge_star_metric
    end interface star

    interface hodge_star
        module procedure hodge_star_chart
        module procedure hodge_star_metric
    end interface hodge_star

    interface codifferential
        module procedure codifferential_chart
        module procedure codifferential_metric
    end interface codifferential

    interface codiff
        module procedure codifferential_chart
        module procedure codifferential_metric
    end interface codiff

    interface laplace_de_rham
        module procedure laplace_de_rham_chart
        module procedure laplace_de_rham_metric
    end interface laplace_de_rham

    interface volume
        module procedure volume_form_chart
        module procedure volume_form_metric
    end interface volume

    interface volume_form
        module procedure volume_form_chart
        module procedure volume_form_metric
    end interface volume_form

    interface interior
        module procedure interior_product
    end interface interior

    interface lie
        module procedure lie_derivative
    end interface lie

contains

    !> Construct an explicit zero k-form, including the degree DIM+1 zero
    !> extension returned by d of a top-degree form.
    function form_zero(c, degree) result(alpha)
        type(chart_t), intent(in) :: c
        integer, intent(in) :: degree
        type(form_t) :: alpha

        if (.not. associated(c%a)) return
        if (degree < 0 .or. degree > DIM + 1) return
        alpha = zero_form(c%a, degree)
        call form_bind_chart(alpha, c)
    end function form_zero

    !> Construct a zero k-form in an existing expression arena.
    function zero_form(a, degree) result(alpha)
        type(arena_t), pointer, intent(in) :: a
        integer, intent(in) :: degree
        type(form_t) :: alpha
        integer :: mask

        if (.not. associated(a)) return
        alpha%a => a
        alpha%degree = degree
        alpha%zero_extension = degree > DIM
        do mask = 0, 2**DIM - 1
            alpha%component(mask) = num(a, 0)
        end do
    end function zero_form

    !> A scalar is a degree-zero form.
    function form_scalar(value) result(alpha)
        type(expr_t), intent(in) :: value
        type(form_t) :: alpha

        if (.not. is_valid(value)) return
        alpha = zero_form(value%a, 0)
        alpha%component(0) = value
    end function form_scalar

    !> A one-form with coefficients (alpha_1, alpha_2, alpha_3).
    function form_one(c, values) result(alpha)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: values(DIM)
        type(form_t) :: alpha
        integer :: i

        alpha = zero_form(c%a, 1)
        if (.not. associated(c%a)) return
        call form_bind_chart(alpha, c)
        do i = 1, DIM
            if (.not. is_valid(values(i))) then
                alpha = form_t()
                return
            end if
            if (.not. associated(values(i)%a, c%a)) then
                alpha = form_t()
                return
            end if
            alpha%component(2**(i - 1)) = values(i)
        end do
    end function form_one

    !> A two-form with ordered coefficients (alpha_12, alpha_13, alpha_23).
    function form_two(c, values) result(alpha)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: values(3)
        type(form_t) :: alpha
        integer :: i

        alpha = zero_form(c%a, 2)
        if (.not. associated(c%a)) return
        call form_bind_chart(alpha, c)
        do i = 1, 3
            if (.not. is_valid(values(i))) then
                alpha = form_t()
                return
            end if
            if (.not. associated(values(i)%a, c%a)) then
                alpha = form_t()
                return
            end if
        end do
        alpha%component(3) = values(1)
        alpha%component(5) = values(2)
        alpha%component(6) = values(3)
    end function form_two

    !> A three-form with coefficient alpha_123.
    function form_three(c, value) result(alpha)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: value
        type(form_t) :: alpha

        alpha = zero_form(c%a, 3)
        if (.not. associated(c%a)) return
        call form_bind_chart(alpha, c)
        if (.not. is_valid(value)) then
            alpha = form_t()
            return
        end if
        if (.not. associated(value%a, c%a)) then
            alpha = form_t()
            return
        end if
        alpha%component(7) = value
    end function form_three

    !> Oriented metric volume form sigma*sqrt(g) du^1^du^2^du^3.
    function volume_form_chart(c, orientation) result(alpha)
        type(chart_t), intent(in) :: c
        integer, optional, intent(in) :: orientation
        type(form_t) :: alpha
        type(expr_t) :: coefficient
        integer :: sign

        sign = 1
        if (present(orientation)) sign = orientation
        if (sign /= 1 .and. sign /= -1) return
        coefficient = sqrtg(c)
        if (sign < 0) coefficient = -coefficient
        alpha = form_three(c, coefficient)
    end function volume_form_chart

    !> Oriented metric volume form from an explicit metric owner.
    !>
    !> The stored metric orientation is used by default; an optional sign is
    !> an explicit view change. The positive metric volume is never modified.
    function volume_form_metric(g, orientation) result(alpha)
        type(metric_t), intent(in) :: g
        integer, optional, intent(in) :: orientation
        type(form_t) :: alpha
        type(expr_t) :: coefficient
        integer :: sign

        if (.not. metric_valid(g)) return
        sign = metric_orientation(g)
        if (present(orientation)) sign = orientation
        if (sign /= 1 .and. sign /= -1) return
        coefficient = metric_sqrtg(g)
        if (sign < 0) coefficient = -coefficient
        alpha = zero_form(metric_arena(g), 3)
        call form_bind_metric(alpha, g)
        alpha%component(7) = coefficient
    end function volume_form_metric

    !> Return a coefficient by its ordered basis mask.
    function form_component(alpha, mask) result(value)
        type(form_t), intent(in) :: alpha
        integer, intent(in) :: mask
        type(expr_t) :: value

        if (.not. form_valid(alpha)) return
        if (mask < 0 .or. mask >= 2**DIM) return
        value = alpha%component(mask)
    end function form_component

    function form_degree(alpha) result(degree)
        type(form_t), intent(in) :: alpha
        integer :: degree

        degree = alpha%degree
    end function form_degree

    !> A valid form has one coefficient for every mask. Degree DIM+1 is the
    !> explicit zero extension used for d of a top-degree form.
    function form_valid(alpha) result(valid)
        type(form_t), intent(in) :: alpha
        logical :: valid
        integer :: mask

        valid = .false.
        if (.not. associated(alpha%a)) return
        if (alpha%degree < 0 .or. alpha%degree > DIM + 1) return
        if (alpha%degree > DIM .and. .not. alpha%zero_extension) return
        if (alpha%chart_bound) then
            do mask = 1, DIM
                if (.not. is_valid(alpha%chart_coordinates(mask))) return
                if (.not. associated(alpha%chart_coordinates(mask)%a, alpha%a)) return
                if (alpha%chart_has_position) then
                    if (.not. is_valid(alpha%chart_position(mask))) return
                    if (.not. associated(alpha%chart_position(mask)%a, alpha%a)) return
                end if
            end do
        end if
        do mask = 0, 2**DIM - 1
            if (.not. is_valid(alpha%component(mask))) return
        end do
        valid = .true.
    end function form_valid

    !> Whether a form carries an explicit chart/map owner.
    function form_chart_bound(alpha) result(bound)
        type(form_t), intent(in) :: alpha
        logical :: bound

        bound = form_valid(alpha) .and. alpha%chart_bound
    end function form_chart_bound

    !> Bind a form created from a chart to its value-semantic key.
    subroutine form_bind_chart(alpha, c)
        type(form_t), intent(inout) :: alpha
        type(chart_t), intent(in) :: c

        if (.not. chart_valid(c)) return
        if (.not. associated(alpha%a, c%a)) return
        alpha%chart_bound = .true.
        alpha%chart_has_position = .true.
        alpha%chart_coordinates = c%u
        alpha%chart_position = c%x
    end subroutine form_bind_chart

    !> Bind a form created from an explicit metric to its coordinates.
    subroutine form_bind_metric(alpha, g)
        type(form_t), intent(inout) :: alpha
        type(metric_t), intent(in) :: g
        integer :: i

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. associated(alpha%a, metric_arena(g))) return
        alpha%chart_bound = .true.
        alpha%chart_has_position = .false.
        do i = 1, DIM
            alpha%chart_coordinates(i) = metric_coordinate(g, i)
        end do
    end subroutine form_bind_metric

    !> Copy chart/map ownership through a form view.
    subroutine form_copy_owner(target, source)
        type(form_t), intent(inout) :: target
        type(form_t), intent(in) :: source

        target%chart_bound = source%chart_bound
        target%chart_has_position = source%chart_has_position
        target%chart_coordinates = source%chart_coordinates
        target%chart_position = source%chart_position
    end subroutine form_copy_owner

    !> Merge ownership for a wedge, sum, or transport involving two forms.
    subroutine form_merge_owner(target, left, right)
        type(form_t), intent(inout) :: target
        type(form_t), intent(in) :: left, right

        if (left%chart_bound) then
            call form_copy_owner(target, left)
        else if (right%chart_bound) then
            call form_copy_owner(target, right)
        end if
    end subroutine form_merge_owner

    !> Check whether a chart-bound form belongs to the supplied chart.
    function form_chart_compatible(alpha, c) result(same)
        type(form_t), intent(in) :: alpha
        type(chart_t), intent(in) :: c
        logical :: same
        integer :: i

        same = .false.
        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, c%a)) return
        if (.not. alpha%chart_bound) then
            same = .true.
            return
        end if
        do i = 1, DIM
            if (.not. (alpha%chart_coordinates(i) == c%u(i))) return
            if (alpha%chart_has_position) then
                if (.not. (alpha%chart_position(i) == c%x(i))) return
            end if
        end do
        same = .true.
    end function form_chart_compatible

    !> Check whether a chart-bound form belongs to an explicit metric.
    function form_metric_compatible(alpha, g) result(same)
        type(form_t), intent(in) :: alpha
        type(metric_t), intent(in) :: g
        logical :: same
        type(expr_t) :: coordinates(DIM)
        integer :: i

        same = .false.
        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, metric_arena(g))) return
        if (.not. alpha%chart_bound) then
            same = .true.
            return
        end if
        coordinates = metric_coordinates(g)
        do i = 1, DIM
            if (.not. (alpha%chart_coordinates(i) == coordinates(i))) return
        end do
        same = .true.
    end function form_metric_compatible

    !> Check whether two forms can be combined without changing coordinates.
    function form_same_chart(left, right) result(same)
        type(form_t), intent(in) :: left, right
        logical :: same
        integer :: i

        same = .false.
        if (.not. form_valid(left)) return
        if (.not. form_valid(right)) return
        if (.not. associated(left%a, right%a)) return
        if (.not. left%chart_bound .or. .not. right%chart_bound) then
            same = .true.
            return
        end if
        do i = 1, DIM
            if (.not. (left%chart_coordinates(i) == right%chart_coordinates(i))) return
            if (left%chart_has_position .and. right%chart_has_position) then
                if (.not. (left%chart_position(i) == right%chart_position(i))) return
            end if
        end do
        same = .true.
    end function form_same_chart

    !> Exterior product, with the sign fixed by the ordered coordinate coframe.
    function wedge(left, right) result(result)
        type(form_t), intent(in) :: left, right
        type(form_t) :: result
        integer :: left_mask, right_mask, output_mask, sign

        if (.not. form_valid(left)) return
        if (.not. form_valid(right)) return
        if (.not. form_same_chart(left, right)) return
        if (left%degree > DIM .or. right%degree > DIM) return

        result = zero_form(left%a, left%degree + right%degree)
        call form_merge_owner(result, left, right)
        if (left%degree + right%degree > DIM) return
        do left_mask = 0, 2**DIM - 1
            if (mask_degree(left_mask) /= left%degree) cycle
            do right_mask = 0, 2**DIM - 1
                if (mask_degree(right_mask) /= right%degree) cycle
                if (iand(left_mask, right_mask) /= 0) cycle
                output_mask = ior(left_mask, right_mask)
                sign = wedge_sign(left_mask, right_mask)
                result%component(output_mask) = result%component(output_mask) + &
                    sign*left%component(left_mask)*right%component(right_mask)
            end do
        end do
    end function wedge

    function add_forms(left, right) result(result)
        type(form_t), intent(in) :: left, right
        type(form_t) :: result
        integer :: mask

        if (.not. form_valid(left)) return
        if (.not. form_valid(right)) return
        if (left%degree /= right%degree) return
        if (.not. form_same_chart(left, right)) return
        result = zero_form(left%a, left%degree)
        call form_merge_owner(result, left, right)
        do mask = 0, 2**DIM - 1
            result%component(mask) = left%component(mask) + right%component(mask)
        end do
    end function add_forms

    function subtract_forms(left, right) result(result)
        type(form_t), intent(in) :: left, right
        type(form_t) :: result

        result = add_forms(left, negate_form(right))
    end function subtract_forms

    function negate_form(alpha) result(result)
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        integer :: mask

        if (.not. form_valid(alpha)) return
        result = zero_form(alpha%a, alpha%degree)
        call form_copy_owner(result, alpha)
        result%zero_extension = alpha%zero_extension
        do mask = 0, 2**DIM - 1
            result%component(mask) = -alpha%component(mask)
        end do
    end function negate_form

    !> Multiply a form by one scalar expression without changing its degree.
    function scale_form(alpha, factor) result(result)
        type(form_t), intent(in) :: alpha
        type(expr_t), intent(in) :: factor
        type(form_t) :: result
        integer :: mask

        if (.not. form_valid(alpha)) return
        if (.not. is_valid(factor)) return
        if (.not. associated(factor%a, alpha%a)) return
        result = zero_form(alpha%a, alpha%degree)
        call form_copy_owner(result, alpha)
        result%zero_extension = alpha%zero_extension
        do mask = 0, 2**DIM - 1
            result%component(mask) = factor*alpha%component(mask)
        end do
    end function scale_form

    !> Exterior derivative in the coordinate coframe.
    function exterior_diff_chart(c, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(form_t) :: result

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_chart_compatible(alpha, c)) return
        result = exterior_diff_components(c%a, c%u, alpha)
        call form_bind_chart(result, c)
    end function exterior_diff_chart

    function exterior_diff_metric(g, alpha) result(result)
        type(metric_t), intent(in) :: g
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        type(expr_t) :: coordinates(DIM)

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_metric_compatible(alpha, g)) return
        coordinates = metric_coordinates(g)
        result = exterior_diff_components(metric_arena(g), coordinates, alpha)
        call form_bind_metric(result, g)
    end function exterior_diff_metric

    function exterior_diff_components(a, coordinates, alpha) result(result)
        type(arena_t), pointer, intent(in) :: a
        type(expr_t), intent(in) :: coordinates(DIM)
        type(form_t), intent(in) :: alpha
        type(form_t) :: result

        if (alpha%degree > DIM) then
            result = zero_form(a, DIM + 1)
            return
        end if

        result = zero_form(a, alpha%degree + 1)
        select case (alpha%degree)
        case (0)
            result%component(1) = diff(alpha%component(0), coordinates(1))
            result%component(2) = diff(alpha%component(0), coordinates(2))
            result%component(4) = diff(alpha%component(0), coordinates(3))
        case (1)
            result%component(3) = diff(alpha%component(2), coordinates(1)) - &
                diff(alpha%component(1), coordinates(2))
            result%component(5) = diff(alpha%component(4), coordinates(1)) - &
                diff(alpha%component(1), coordinates(3))
            result%component(6) = diff(alpha%component(4), coordinates(2)) - &
                diff(alpha%component(2), coordinates(3))
        case (2)
            result%component(7) = diff(alpha%component(6), coordinates(1)) - &
                diff(alpha%component(5), coordinates(2)) + &
                diff(alpha%component(3), coordinates(3))
        case default
            ! d of a top-degree form is the zero extension in this dimension.
        end select
    end function exterior_diff_components

    !> Metric codifferential delta = (-1)^(n(k+1)+s+1) * d * on k-forms.
    !> The explicit +1 gives delta(f_i du^i) = -div(f^sharp) in Euclidean
    !> three-space, while s counts negative metric directions.
    function codifferential_chart(c, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(form_t) :: result, first, second

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_chart_compatible(alpha, c)) return
        if (alpha%degree < 1 .or. alpha%degree > DIM) return
        first = hodge_star_chart(c, alpha)
        second = exterior_diff_chart(c, first)
        result = hodge_star_chart(c, second)
        if (codifferential_sign(alpha%degree, 0) < 0) result = negate_form(result)
    end function codifferential_chart

    function codifferential_metric(g, alpha) result(result)
        type(metric_t), intent(in) :: g
        type(form_t), intent(in) :: alpha
        type(form_t) :: result, first, second
        integer :: signature(DIM), negatives

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_metric_compatible(alpha, g)) return
        if (alpha%degree < 1 .or. alpha%degree > DIM) return
        first = hodge_star_metric(g, alpha)
        second = exterior_diff_metric(g, first)
        result = hodge_star_metric(g, second)
        signature = metric_signature(g)
        negatives = count(signature == -1)
        if (codifferential_sign(alpha%degree, negatives) < 0) result = negate_form(result)
    end function codifferential_metric

    function laplace_de_rham_chart(c, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(form_t) :: result, first, second

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_chart_compatible(alpha, c)) return
        if (alpha%degree > DIM) return
        result = zero_form(c%a, alpha%degree)
        call form_bind_chart(result, c)
        if (alpha%degree > 0) then
            first = codifferential_chart(c, alpha)
            result = exterior_diff_chart(c, first)
        end if
        if (alpha%degree < DIM) then
            first = exterior_diff_chart(c, alpha)
            second = codifferential_chart(c, first)
            result = add_forms(result, second)
        end if
    end function laplace_de_rham_chart

    function laplace_de_rham_metric(g, alpha) result(result)
        type(metric_t), intent(in) :: g
        type(form_t), intent(in) :: alpha
        type(form_t) :: result, first, second

        if (.not. metric_valid(g)) return
        if (.not. metric_has_coordinates(g)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_metric_compatible(alpha, g)) return
        if (alpha%degree > DIM) return
        result = zero_form(metric_arena(g), alpha%degree)
        call form_bind_metric(result, g)
        if (alpha%degree > 0) then
            first = codifferential_metric(g, alpha)
            result = exterior_diff_metric(g, first)
        end if
        if (alpha%degree < DIM) then
            first = exterior_diff_metric(g, alpha)
            second = codifferential_metric(g, first)
            result = add_forms(result, second)
        end if
    end function laplace_de_rham_metric

    pure function codifferential_sign(degree, negatives) result(sign)
        integer, intent(in) :: degree, negatives
        integer :: sign, exponent

        exponent = DIM*(degree + 1) + negatives + 1
        sign = 1
        if (mod(exponent, 2) == 1) sign = -1
    end function codifferential_sign

    !> Metric Hodge star with the chart orientation and positive sqrt(g).
    function hodge_star_chart(c, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        type(expr_t) :: ginv(DIM, DIM), volume

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_chart_compatible(alpha, c)) return
        if (alpha%degree > DIM) return

        ginv = metric_contravariant(c)
        volume = sqrtg(c)
        result = hodge_star_components(c%a, alpha, ginv, volume, 1)
        call form_bind_chart(result, c)
    end function hodge_star_chart

    !> Hodge star from an explicit metric owner.
    !>
    !> The inverse metric, absolute volume density, signature, and orientation
    !> all come from the supplied owner. A Lorentzian metric therefore follows
    !> the same formula as a Euclidean metric, with the signature signs entering
    !> through the inverse metric rather than through an ad hoc branch.
    function hodge_star_metric(g, alpha) result(result)
        type(metric_t), intent(in) :: g
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        type(expr_t) :: ginv(DIM, DIM), volume

        if (.not. metric_valid(g)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_metric_compatible(alpha, g)) return
        if (alpha%degree > DIM) return

        ginv = metric_contravariant_owner(g)
        volume = metric_sqrtg(g)
        result = hodge_star_components(metric_arena(g), alpha, ginv, volume, &
            metric_orientation(g))
        call form_bind_metric(result, g)
    end function hodge_star_metric

    function hodge_star_components(a, alpha, ginv, volume, orientation) &
            result(result)
        type(arena_t), pointer, intent(in) :: a
        type(form_t), intent(in) :: alpha
        type(expr_t), intent(in) :: ginv(DIM, DIM), volume
        integer, intent(in) :: orientation
        type(form_t) :: result
        type(expr_t) :: term
        integer :: i, j, k, l, input_mask, output_mask

        result = zero_form(a, DIM - alpha%degree)
        select case (alpha%degree)
        case (0)
            result%component(7) = orientation*volume*alpha%component(0)
        case (1)
            do i = 1, DIM
                input_mask = 2**(i - 1)
                do k = 1, DIM
                    do l = k + 1, DIM
                        output_mask = 2**(k - 1) + 2**(l - 1)
                        do j = 1, DIM
                            term = epsilon3(j, k, l)*ginv(i, j)
                            result%component(output_mask) = &
                                result%component(output_mask) + orientation*volume* &
                                alpha%component(input_mask)*term
                        end do
                    end do
                end do
            end do
        case (2)
            do input_mask = 0, 2**DIM - 1
                if (mask_degree(input_mask) /= 2) cycle
                do k = 1, DIM
                    do i = 1, DIM
                        do j = 1, DIM
                            term = ginv(mask_first(input_mask), i)* &
                                ginv(mask_second(input_mask), j)*epsilon3(i, j, k)
                            result%component(2**(k - 1)) = &
                                result%component(2**(k - 1)) + orientation*volume* &
                                alpha%component(input_mask)*term
                        end do
                    end do
                end do
            end do
        case (3)
            result%component(0) = orientation*alpha%component(7)/volume
        end select
    end function hodge_star_components

    !> Interior product i_v alpha for a contravariant coordinate vector.
    function interior_product(c, vector, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: vector(DIM)
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        integer :: i, input_mask, output_mask, rank, mask
        integer :: sign

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_chart_compatible(alpha, c)) return
        if (alpha%degree < 1 .or. alpha%degree > DIM) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, c%a)) return
        end do

        result = zero_form(c%a, alpha%degree - 1)
        call form_bind_chart(result, c)
        do input_mask = 0, 2**DIM - 1
            if (mask_degree(input_mask) /= alpha%degree) cycle
            do i = 1, DIM
                if (.not. btest(input_mask, i - 1)) cycle
                output_mask = ibclr(input_mask, i - 1)
                rank = 1
                do mask = 1, i - 1
                    if (btest(input_mask, mask - 1)) rank = rank + 1
                end do
                sign = 1
                if (mod(rank, 2) == 0) sign = -1
                result%component(output_mask) = result%component(output_mask) + &
                    sign*vector(i)*alpha%component(input_mask)
            end do
        end do
    end function interior_product

    !> Cartan's formula L_v alpha = i_v(d alpha) + d(i_v alpha).
    function lie_derivative(c, vector, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: vector(DIM)
        type(form_t), intent(in) :: alpha
        type(form_t) :: result, first, second, contraction
        integer :: i

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (.not. form_chart_compatible(alpha, c)) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, c%a)) return
        end do
        if (alpha%degree == 0) then
            result = form_scalar(num(c%a, 0))
            call form_bind_chart(result, c)
            do i = 1, DIM
                result%component(0) = result%component(0) + vector(i)* &
                    diff(alpha%component(0), c%u(i))
            end do
            return
        end if
        contraction = interior_product(c, vector, alpha)
        second = exterior_diff(c, contraction)
        if (alpha%degree == DIM) then
            first = zero_form(c%a, alpha%degree)
            call form_bind_chart(first, c)
        else
            first = interior_product(c, vector, exterior_diff(c, alpha))
        end if
        result = add_forms(first, second)
    end function lie_derivative

    !> Metric lowering of a contravariant vector, returned as a one-form.
    function flat(c, vector) result(alpha)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: vector(DIM)
        type(form_t) :: alpha
        type(expr_t) :: g(DIM, DIM), values(DIM)
        integer :: i, j

        if (.not. chart_valid(c)) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, c%a)) return
        end do
        g = metric_covariant(c)
        do i = 1, DIM
            values(i) = g(i, 1)*vector(1)
            do j = 2, DIM
                values(i) = values(i) + g(i, j)*vector(j)
            end do
        end do
        alpha = form_one(c, values)
    end function flat

    !> Metric raising of a one-form, returned as contravariant components.
    function sharp(c, alpha) result(vector)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(expr_t) :: vector(DIM)
        type(expr_t) :: ginv(DIM, DIM)
        integer :: i, j

        if (.not. chart_valid(c)) return
        if (.not. form_valid(alpha)) return
        if (alpha%degree /= 1) return
        if (.not. form_chart_compatible(alpha, c)) return
        ginv = metric_contravariant(c)
        do i = 1, DIM
            vector(i) = ginv(i, 1)*alpha%component(1)
            do j = 2, DIM
                vector(i) = vector(i) + ginv(i, j)* &
                    alpha%component(2**(j - 1))
            end do
        end do
    end function sharp

    pure function mask_degree(mask) result(degree)
        integer, intent(in) :: mask
        integer :: degree

        degree = popcnt(mask)
    end function mask_degree

    pure function wedge_sign(left_mask, right_mask) result(sign)
        integer, intent(in) :: left_mask, right_mask
        integer :: sign, i, j, inversions

        inversions = 0
        do i = 1, DIM
            if (.not. btest(left_mask, i - 1)) cycle
            do j = 1, i - 1
                if (btest(right_mask, j - 1)) inversions = inversions + 1
            end do
        end do
        sign = 1
        if (mod(inversions, 2) == 1) sign = -1
    end function wedge_sign

    pure function mask_first(mask) result(index)
        integer, intent(in) :: mask
        integer :: index, i

        index = 0
        do i = 1, DIM
            if (btest(mask, i - 1)) then
                index = i
                return
            end if
        end do
    end function mask_first

    pure function mask_second(mask) result(index)
        integer, intent(in) :: mask
        integer :: index, i, found

        index = 0
        found = 0
        do i = 1, DIM
            if (.not. btest(mask, i - 1)) cycle
            found = found + 1
            if (found == 2) then
                index = i
                return
            end if
        end do
    end function mask_second

    pure function epsilon3(i, j, k) result(value)
        integer, intent(in) :: i, j, k
        integer :: value

        value = 0
        if (i == j .or. i == k .or. j == k) return
        if ((i == 1 .and. j == 2 .and. k == 3) .or. &
            (i == 2 .and. j == 3 .and. k == 1) .or. &
            (i == 3 .and. j == 1 .and. k == 2)) then
            value = 1
        else
            value = -1
        end if
    end function epsilon3

end module fortsym_form
