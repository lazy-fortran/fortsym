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
    use fortsym_chart, only: chart_t, DIM, metric_covariant, &
        metric_contravariant, sqrtg
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, num, is_valid, operator(+), operator(-), &
        operator(*), operator(/), operator(==)
    implicit none
    private

    public :: form_t, form, form_scalar, form_one, form_two, form_three
    public :: form_zero
    public :: volume_form, volume
    public :: form_component, form_degree, form_valid
    public :: add_forms, subtract_forms, negate_form
    public :: wedge, d, exterior_diff, star, hodge_star
    public :: interior, interior_product, lie, lie_derivative
    public :: flat, sharp, scale_form

    type :: form_t
        type(arena_t), pointer :: a => null()
        integer :: degree = -1
        logical :: zero_extension = .false.
        type(expr_t) :: component(0:2**DIM - 1)
    end type form_t

    interface form
        module procedure form_scalar
    end interface form

    interface d
        module procedure exterior_diff
    end interface d

    interface star
        module procedure hodge_star
    end interface star

    interface volume
        module procedure volume_form
    end interface volume

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
    function volume_form(c, orientation) result(alpha)
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
    end function volume_form

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
        do mask = 0, 2**DIM - 1
            if (.not. is_valid(alpha%component(mask))) return
        end do
        valid = .true.
    end function form_valid

    !> Exterior product, with the sign fixed by the ordered coordinate coframe.
    function wedge(left, right) result(result)
        type(form_t), intent(in) :: left, right
        type(form_t) :: result
        integer :: left_mask, right_mask, output_mask, sign

        if (.not. form_valid(left)) return
        if (.not. form_valid(right)) return
        if (.not. associated(left%a, right%a)) return
        if (left%degree > DIM .or. right%degree > DIM) return

        result = zero_form(left%a, left%degree + right%degree)
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
        if (.not. associated(left%a, right%a)) return
        result = zero_form(left%a, left%degree)
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
        result%zero_extension = alpha%zero_extension
        do mask = 0, 2**DIM - 1
            result%component(mask) = factor*alpha%component(mask)
        end do
    end function scale_form

    !> Exterior derivative in the coordinate coframe.
    function exterior_diff(c, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(form_t) :: result

        if (.not. associated(c%a)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, c%a)) return
        if (alpha%degree > DIM) then
            result = form_zero(c, DIM + 1)
            return
        end if

        result = zero_form(c%a, alpha%degree + 1)
        select case (alpha%degree)
        case (0)
            result%component(1) = diff(alpha%component(0), c%u(1))
            result%component(2) = diff(alpha%component(0), c%u(2))
            result%component(4) = diff(alpha%component(0), c%u(3))
        case (1)
            result%component(3) = diff(alpha%component(2), c%u(1)) - &
                diff(alpha%component(1), c%u(2))
            result%component(5) = diff(alpha%component(4), c%u(1)) - &
                diff(alpha%component(1), c%u(3))
            result%component(6) = diff(alpha%component(4), c%u(2)) - &
                diff(alpha%component(2), c%u(3))
        case (2)
            result%component(7) = diff(alpha%component(6), c%u(1)) - &
                diff(alpha%component(5), c%u(2)) + &
                diff(alpha%component(3), c%u(3))
        case default
            ! d of a top-degree form is the zero extension in this dimension.
        end select
    end function exterior_diff

    !> Metric Hodge star with the chart orientation and positive sqrt(g).
    function hodge_star(c, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        type(expr_t) :: ginv(DIM, DIM), volume, term
        integer :: i, j, k, l, input_mask, output_mask

        if (.not. associated(c%a)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, c%a)) return
        if (alpha%degree > DIM) return

        result = zero_form(c%a, DIM - alpha%degree)
        volume = sqrtg(c)
        select case (alpha%degree)
        case (0)
            result%component(7) = volume*alpha%component(0)
        case (1)
            ginv = metric_contravariant(c)
            do i = 1, DIM
                input_mask = 2**(i - 1)
                do k = 1, DIM
                    do l = k + 1, DIM
                        output_mask = 2**(k - 1) + 2**(l - 1)
                        do j = 1, DIM
                            term = epsilon3(j, k, l)*ginv(i, j)
                            result%component(output_mask) = &
                                result%component(output_mask) + volume* &
                                alpha%component(input_mask)*term
                        end do
                    end do
                end do
            end do
        case (2)
            ginv = metric_contravariant(c)
            do input_mask = 0, 2**DIM - 1
                if (mask_degree(input_mask) /= 2) cycle
                do k = 1, DIM
                    do i = 1, DIM
                        do j = 1, DIM
                            term = ginv(mask_first(input_mask), i)* &
                                ginv(mask_second(input_mask), j)*epsilon3(i, j, k)
                            result%component(2**(k - 1)) = &
                                result%component(2**(k - 1)) + volume* &
                                alpha%component(input_mask)*term
                        end do
                    end do
                end do
            end do
        case (3)
            result%component(0) = alpha%component(7)/volume
        end select
    end function hodge_star

    !> Interior product i_v alpha for a contravariant coordinate vector.
    function interior_product(c, vector, alpha) result(result)
        type(chart_t), intent(in) :: c
        type(expr_t), intent(in) :: vector(DIM)
        type(form_t), intent(in) :: alpha
        type(form_t) :: result
        integer :: i, input_mask, output_mask, rank, mask
        integer :: sign

        if (.not. associated(c%a)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, c%a)) return
        if (alpha%degree < 1 .or. alpha%degree > DIM) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, c%a)) return
        end do

        result = zero_form(c%a, alpha%degree - 1)
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

        if (.not. associated(c%a)) return
        if (.not. form_valid(alpha)) return
        if (.not. associated(alpha%a, c%a)) return
        do i = 1, DIM
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, c%a)) return
        end do
        if (alpha%degree == 0) then
            result = form_scalar(num(c%a, 0))
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

        if (.not. associated(c%a)) return
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

        if (.not. associated(c%a)) return
        if (.not. form_valid(alpha)) return
        if (alpha%degree /= 1) return
        if (.not. associated(alpha%a, c%a)) return
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
