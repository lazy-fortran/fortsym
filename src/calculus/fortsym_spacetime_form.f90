module fortsym_spacetime_form
    ! Degree-k differential forms for the dimension-aware spacetime owner.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, is_valid, operator(+), operator(-), &
        operator(*), operator(/)
    use fortsym_diff, only: diff
    use fortsym_relativity, only: SPACETIME_DIM, &
        spacetime_metric_t, spacetime_metric_valid, spacetime_metric_arena, &
        spacetime_metric_has_coordinates, spacetime_metric_coordinates, &
        spacetime_metric_contravariant, spacetime_metric_sqrtg, &
        spacetime_metric_signature, spacetime_metric_orientation, &
        spacetime_metric_dimension
    implicit none
    private

    type, public :: spacetime_form_t
        private
        type(arena_t), pointer :: a => null()
        integer :: degree = -1
        integer :: dimension = 0
        type(expr_t) :: component(0:2**SPACETIME_DIM - 1)
        logical :: zero_extension = .false.
        logical :: valid = .false.
    end type spacetime_form_t

    public :: spacetime_form_zero, spacetime_form_scalar, spacetime_form_one
    public :: spacetime_form_two, spacetime_form_three, spacetime_form_four
    public :: spacetime_volume_form
    public :: spacetime_form_component, spacetime_form_degree, &
        spacetime_form_dimension
    public :: spacetime_form_valid, spacetime_form_same_arena, spacetime_wedge, spacetime_d
    public :: spacetime_add, spacetime_subtract, spacetime_negate
    public :: spacetime_exterior_diff, spacetime_hodge, spacetime_star
    public :: spacetime_codifferential, spacetime_interior
    public :: spacetime_interior_product, spacetime_lie
    public :: spacetime_lie_derivative, spacetime_laplace_de_rham

    interface spacetime_form_scalar
        module procedure spacetime_form_scalar_value
        module procedure spacetime_form_scalar_metric
    end interface spacetime_form_scalar

contains

    !> Oriented metric volume form sigma*sqrt(abs(det(g))) du^1^...^du^n.
    function spacetime_volume_form(g, orientation) result(value)
        type(spacetime_metric_t), intent(in) :: g
        integer, optional, intent(in) :: orientation
        type(spacetime_form_t) :: value
        type(expr_t) :: coefficient
        integer :: sign, dimension

        if (.not. spacetime_metric_valid(g)) return
        sign = spacetime_metric_orientation(g)
        if (present(orientation)) sign = orientation
        if (sign /= 1 .and. sign /= -1) return
        dimension = spacetime_metric_dimension(g)
        value = spacetime_form_zero(g, dimension)
        coefficient = spacetime_metric_sqrtg(g)
        if (sign < 0) coefficient = -coefficient
        value%component(2**dimension - 1) = coefficient
    end function spacetime_volume_form

    function spacetime_form_zero(g, degree) result(value)
        type(spacetime_metric_t), intent(in) :: g
        integer, intent(in) :: degree
        type(spacetime_form_t) :: value
        integer :: mask

        if (.not. spacetime_metric_valid(g)) return
        if (degree < 0 .or. degree > spacetime_metric_dimension(g) + 1) return
        value%a => spacetime_metric_arena(g)
        value%degree = degree
        value%dimension = spacetime_metric_dimension(g)
        value%zero_extension = degree > value%dimension
        do mask = 0, 2**SPACETIME_DIM - 1
            value%component(mask) = num(value%a, 0)
        end do
        value%valid = .true.
    end function spacetime_form_zero

    function spacetime_form_scalar_value(value) result(form)
        type(expr_t), intent(in) :: value
        type(spacetime_form_t) :: form
        integer :: mask

        if (.not. is_valid(value)) return
        form%a => value%a
        form%degree = 0
        form%dimension = SPACETIME_DIM
        do mask = 0, 2**SPACETIME_DIM - 1
            form%component(mask) = num(form%a, 0)
        end do
        form%component(0) = value
        form%valid = .true.
    end function spacetime_form_scalar_value

    function spacetime_form_scalar_metric(g, value) result(form)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: value
        type(spacetime_form_t) :: form

        if (.not. spacetime_metric_valid(g)) return
        if (.not. is_valid(value)) return
        if (.not. associated(value%a, spacetime_metric_arena(g))) return
        form = spacetime_form_zero(g, 0)
        form%component(0) = value
    end function spacetime_form_scalar_metric

    function spacetime_form_one(g, values) result(form)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: values(SPACETIME_DIM)
        type(spacetime_form_t) :: form
        integer :: i

        form = spacetime_form_zero(g, 1)
        if (.not. form%valid) return
        do i = 1, form%dimension
            if (.not. is_valid(values(i))) then
                form = spacetime_form_t()
                return
            end if
            if (.not. associated(values(i)%a, form%a)) then
                form = spacetime_form_t()
                return
            end if
            form%component(2**(i - 1)) = values(i)
        end do
    end function spacetime_form_one

    function spacetime_form_two(g, values) result(form)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: values(6)
        type(spacetime_form_t) :: form
        integer :: mask, index

        form = spacetime_form_zero(g, 2)
        if (.not. form%valid) return
        index = 0
        do mask = 0, 2**form%dimension - 1
            if (mask_degree(mask) /= 2) cycle
            index = index + 1
            if (.not. is_valid(values(index))) then
                form = spacetime_form_t()
                return
            end if
            if (.not. associated(values(index)%a, form%a)) then
                form = spacetime_form_t()
                return
            end if
            form%component(mask) = values(index)
        end do
    end function spacetime_form_two

    function spacetime_form_three(g, values) result(form)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: values(4)
        type(spacetime_form_t) :: form
        integer :: mask, index

        form = spacetime_form_zero(g, 3)
        if (.not. form%valid) return
        index = 0
        do mask = 0, 2**form%dimension - 1
            if (mask_degree(mask) /= 3) cycle
            index = index + 1
            if (.not. is_valid(values(index))) then
                form = spacetime_form_t()
                return
            end if
            if (.not. associated(values(index)%a, form%a)) then
                form = spacetime_form_t()
                return
            end if
            form%component(mask) = values(index)
        end do
    end function spacetime_form_three

    function spacetime_form_four(g, value) result(form)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: value
        type(spacetime_form_t) :: form

        form = spacetime_form_zero(g, 4)
        if (.not. form%valid) return
        if (.not. is_valid(value)) then
            form = spacetime_form_t()
            return
        end if
        if (.not. associated(value%a, form%a)) then
            form = spacetime_form_t()
            return
        end if
        form%component(15) = value
    end function spacetime_form_four

    function spacetime_form_component(form, mask) result(value)
        type(spacetime_form_t), intent(in) :: form
        integer, intent(in) :: mask
        type(expr_t) :: value

        if (.not. spacetime_form_valid(form)) return
        if (mask < 0 .or. mask >= 2**SPACETIME_DIM) return
        value = form%component(mask)
    end function spacetime_form_component

    function spacetime_form_degree(form) result(value)
        type(spacetime_form_t), intent(in) :: form
        integer :: value

        value = form%degree
    end function spacetime_form_degree

    function spacetime_form_dimension(form) result(value)
        type(spacetime_form_t), intent(in) :: form
        integer :: value

        value = form%dimension
    end function spacetime_form_dimension

    function spacetime_form_valid(form) result(value)
        type(spacetime_form_t), intent(in) :: form
        logical :: value
        integer :: mask

        value = form%valid .and. associated(form%a)
        if (.not. value) return
        if (form%dimension < 1 .or. form%dimension > SPACETIME_DIM) then
            value = .false.
            return
        end if
        if (form%degree < 0 .or. form%degree > form%dimension + 1) then
            value = .false.
            return
        end if
        if (form%degree > form%dimension .and. .not. form%zero_extension) then
            value = .false.
            return
        end if
        do mask = 0, 2**form%dimension - 1
            if (.not. is_valid(form%component(mask))) value = .false.
            if (.not. associated(form%component(mask)%a, form%a)) value = .false.
        end do
    end function spacetime_form_valid

    function spacetime_form_same_arena(form, a) result(value)
        type(spacetime_form_t), intent(in) :: form
        type(arena_t), pointer, intent(in) :: a
        logical :: value

        value = spacetime_form_valid(form) .and. associated(form%a, a)
    end function spacetime_form_same_arena

    function spacetime_wedge(left, right) result(value)
        type(spacetime_form_t), intent(in) :: left, right
        type(spacetime_form_t) :: value
        integer :: left_mask, right_mask, output_mask, sign

        if (.not. spacetime_form_valid(left)) return
        if (.not. spacetime_form_valid(right)) return
        if (.not. associated(left%a, right%a)) return
        if (left%dimension /= right%dimension) return
        if (left%degree > left%dimension .or. right%degree > right%dimension) return
        if (left%degree + right%degree > left%dimension) return
        value = zero_form_arena(left%a, left%degree + right%degree, left%dimension)
        do left_mask = 0, 2**left%dimension - 1
            if (mask_degree(left_mask) /= left%degree) cycle
            do right_mask = 0, 2**right%dimension - 1
                if (mask_degree(right_mask) /= right%degree) cycle
                if (iand(left_mask, right_mask) /= 0) cycle
                output_mask = ior(left_mask, right_mask)
                sign = wedge_sign(left_mask, right_mask)
                value%component(output_mask) = value%component(output_mask) + &
                    sign*left%component(left_mask)*right%component(right_mask)
            end do
        end do
    end function spacetime_wedge

    function spacetime_add(left, right) result(value)
        type(spacetime_form_t), intent(in) :: left, right
        type(spacetime_form_t) :: value
        integer :: mask

        if (.not. same_form_shape(left, right)) return
        value = zero_form_arena(left%a, left%degree, left%dimension)
        do mask = 0, 2**left%dimension - 1
            value%component(mask) = left%component(mask) + right%component(mask)
        end do
    end function spacetime_add

    function spacetime_subtract(left, right) result(value)
        type(spacetime_form_t), intent(in) :: left, right
        type(spacetime_form_t) :: value
        integer :: mask

        if (.not. same_form_shape(left, right)) return
        value = zero_form_arena(left%a, left%degree, left%dimension)
        do mask = 0, 2**left%dimension - 1
            value%component(mask) = left%component(mask) - right%component(mask)
        end do
    end function spacetime_subtract

    function spacetime_negate(form) result(value)
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value
        integer :: mask

        if (.not. spacetime_form_valid(form)) return
        value = zero_form_arena(form%a, form%degree, form%dimension)
        do mask = 0, 2**form%dimension - 1
            value%component(mask) = -form%component(mask)
        end do
    end function spacetime_negate

    function spacetime_d(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value

        value = spacetime_exterior_diff(g, form)
    end function spacetime_d

    function spacetime_exterior_diff(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value
        type(expr_t) :: coordinates(SPACETIME_DIM)
        integer :: output_mask, coordinate, position, input_mask, sign

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        if (.not. spacetime_form_valid(form)) return
        if (.not. associated(form%a, spacetime_metric_arena(g))) return
        if (form%dimension /= spacetime_metric_dimension(g)) return
        if (form%degree > form%dimension) return
        coordinates = spacetime_metric_coordinates(g)
        value = zero_form_arena(form%a, form%degree + 1, form%dimension)
        do output_mask = 0, 2**form%dimension - 1
            if (mask_degree(output_mask) /= form%degree + 1) cycle
            do coordinate = 1, form%dimension
                if (.not. btest(output_mask, coordinate - 1)) cycle
                input_mask = ibclr(output_mask, coordinate - 1)
                position = 1
                do sign = 1, coordinate - 1
                    if (btest(output_mask, sign - 1)) position = position + 1
                end do
                if (mod(position, 2) == 0) then
                    value%component(output_mask) = value%component(output_mask) - &
                        diff(form%component(input_mask), coordinates(coordinate))
                else
                    value%component(output_mask) = value%component(output_mask) + &
                        diff(form%component(input_mask), coordinates(coordinate))
                end if
            end do
        end do
    end function spacetime_exterior_diff

    function spacetime_hodge(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value
        type(expr_t) :: inverse(SPACETIME_DIM, SPACETIME_DIM), volume
        integer :: orientation, top_sign, negatives, i
        integer :: signature(SPACETIME_DIM)

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_form_valid(form)) return
        if (.not. associated(form%a, spacetime_metric_arena(g))) return
        if (form%dimension /= spacetime_metric_dimension(g)) return
        if (form%degree > form%dimension) return
        inverse = spacetime_metric_contravariant(g)
        volume = spacetime_metric_sqrtg(g)
        orientation = spacetime_metric_orientation(g)
        signature = spacetime_metric_signature(g)
        negatives = 0
        do i = 1, spacetime_metric_dimension(g)
            if (signature(i) == -1) negatives = negatives + 1
        end do
        top_sign = 1
        if (mod(negatives, 2) == 1) top_sign = -1
        value = hodge_components(form, inverse, volume, orientation, top_sign, &
            spacetime_metric_dimension(g))
    end function spacetime_hodge

    function spacetime_star(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value

        value = spacetime_hodge(g, form)
    end function spacetime_star

    function spacetime_codifferential(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value, first, second
        integer :: negatives, exponent, sign, i, signature(SPACETIME_DIM)

        if (.not. spacetime_form_valid(form)) return
        if (form%degree == 0) return
        if (form%degree == 1) then
            value = codifferential_one_form(g, form)
            return
        end if
        first = spacetime_hodge(g, form)
        second = spacetime_exterior_diff(g, first)
        value = spacetime_hodge(g, second)
        negatives = 0
        signature = spacetime_metric_signature(g)
        do i = 1, spacetime_metric_dimension(g)
            if (signature(i) == -1) negatives = negatives + 1
        end do
        exponent = form%dimension*(form%degree + 1) + negatives
        sign = 1
        if (mod(exponent, 2) == 1) sign = -1
        if (sign < 0) value = spacetime_negate(value)
    end function spacetime_codifferential

    function spacetime_interior(vector, form) result(value)
        type(expr_t), intent(in) :: vector(SPACETIME_DIM)
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value
        integer :: input_mask, output_mask, coordinate, position, sign, i

        if (.not. spacetime_form_valid(form)) return
        if (form%degree == 0) return
        do i = 1, form%dimension
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, form%a)) return
        end do
        value = zero_form_arena(form%a, form%degree - 1, form%dimension)
        do input_mask = 0, 2**form%dimension - 1
            if (mask_degree(input_mask) /= form%degree) cycle
            do coordinate = 1, form%dimension
                if (.not. btest(input_mask, coordinate - 1)) cycle
                output_mask = ibclr(input_mask, coordinate - 1)
                position = 1
                do sign = 1, coordinate - 1
                    if (btest(input_mask, sign - 1)) position = position + 1
                end do
                if (mod(position, 2) == 0) then
                    value%component(output_mask) = value%component(output_mask) - &
                        vector(coordinate)*form%component(input_mask)
                else
                    value%component(output_mask) = value%component(output_mask) + &
                        vector(coordinate)*form%component(input_mask)
                end if
            end do
        end do
    end function spacetime_interior

    function spacetime_interior_product(vector, form) result(value)
        type(expr_t), intent(in) :: vector(SPACETIME_DIM)
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value

        value = spacetime_interior(vector, form)
    end function spacetime_interior_product

    function spacetime_lie(g, vector, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: vector(SPACETIME_DIM)
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value, first, second, derivative, contraction
        integer :: i

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_form_valid(form)) return
        if (.not. associated(form%a, spacetime_metric_arena(g))) return
        do i = 1, form%dimension
            if (.not. is_valid(vector(i))) return
            if (.not. associated(vector(i)%a, form%a)) return
        end do
        first = spacetime_form_t()
        if (form%degree < form%dimension) then
            derivative = spacetime_exterior_diff(g, form)
            first = spacetime_interior(vector, derivative)
        else
            first = zero_form_arena(form%a, form%degree, form%dimension)
        end if
        second = zero_form_arena(form%a, form%degree, form%dimension)
        if (form%degree > 0) then
            contraction = spacetime_interior(vector, form)
            second = spacetime_exterior_diff(g, contraction)
        end if
        if (.not. spacetime_form_valid(first)) return
        if (.not. spacetime_form_valid(second)) return
        value = zero_form_arena(form%a, form%degree, form%dimension)
        do i = 0, 2**form%dimension - 1
            value%component(i) = first%component(i) + second%component(i)
        end do
    end function spacetime_lie

    function spacetime_lie_derivative(g, vector, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(expr_t), intent(in) :: vector(SPACETIME_DIM)
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value

        value = spacetime_lie(g, vector, form)
    end function spacetime_lie_derivative

    function spacetime_laplace_de_rham(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value, first, second, derivative, contraction
        integer :: i

        if (.not. spacetime_metric_valid(g)) return
        if (.not. spacetime_form_valid(form)) return
        if (.not. associated(form%a, spacetime_metric_arena(g))) return
        first = zero_form_arena(form%a, form%degree, form%dimension)
        if (form%degree > 0) then
            contraction = spacetime_codifferential(g, form)
            first = spacetime_exterior_diff(g, contraction)
        end if
        second = zero_form_arena(form%a, form%degree, form%dimension)
        if (form%degree < form%dimension) then
            derivative = spacetime_exterior_diff(g, form)
            second = spacetime_codifferential(g, derivative)
        end if
        if (.not. spacetime_form_valid(first)) return
        if (.not. spacetime_form_valid(second)) return
        value = zero_form_arena(form%a, form%degree, form%dimension)
        do i = 0, 2**form%dimension - 1
            value%component(i) = first%component(i) + second%component(i)
        end do
    end function spacetime_laplace_de_rham

    function codifferential_one_form(g, form) result(value)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: form
        type(spacetime_form_t) :: value
        type(expr_t) :: inverse(SPACETIME_DIM, SPACETIME_DIM), volume
        type(expr_t) :: coordinates(SPACETIME_DIM), term, flux
        integer :: i, j, dimension

        if (.not. spacetime_metric_valid(g)) return
        if (form%dimension /= spacetime_metric_dimension(g)) return
        if (.not. spacetime_metric_has_coordinates(g)) return
        if (.not. associated(form%a, spacetime_metric_arena(g))) return
        inverse = spacetime_metric_contravariant(g)
        volume = spacetime_metric_sqrtg(g)
        coordinates = spacetime_metric_coordinates(g)
        dimension = spacetime_metric_dimension(g)
        value = zero_form_arena(form%a, 0, dimension)
        term = num(form%a, 0)
        do i = 1, dimension
            do j = 1, dimension
                flux = volume*inverse(i, j)*form%component(2**(j - 1))
                term = term + diff(flux, coordinates(i))/volume
            end do
        end do
        value%component(0) = -term
    end function codifferential_one_form

    function zero_form_arena(a, degree, dimension) result(value)
        type(arena_t), pointer, intent(in) :: a
        integer, intent(in) :: degree
        integer, optional, intent(in) :: dimension
        type(spacetime_form_t) :: value
        integer :: mask, selected_dimension

        if (.not. associated(a)) return
        selected_dimension = SPACETIME_DIM
        if (present(dimension)) selected_dimension = dimension
        if (selected_dimension < 1 .or. selected_dimension > SPACETIME_DIM) return
        if (degree < 0 .or. degree > selected_dimension + 1) return
        value%a => a
        value%degree = degree
        value%dimension = selected_dimension
        value%zero_extension = degree > selected_dimension
        do mask = 0, 2**SPACETIME_DIM - 1
            value%component(mask) = num(a, 0)
        end do
        value%valid = .true.
    end function zero_form_arena

    function hodge_components(form, inverse, volume, orientation, top_sign, &
            dimension) &
            result(value)
        type(spacetime_form_t), intent(in) :: form
        type(expr_t), intent(in) :: inverse(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t), intent(in) :: volume
        integer, intent(in) :: orientation, top_sign
        integer, intent(in) :: dimension
        type(spacetime_form_t) :: value
        integer :: input_mask, output_mask, output_indices(4)
        integer :: input_indices(4), input_count, output_count
        type(expr_t) :: term

        value = zero_form_arena(form%a, dimension - form%degree, dimension)
        if (form%degree == dimension) then
            value%component(0) = orientation*top_sign* &
                form%component(2**dimension - 1)/volume
            return
        end if
        do input_mask = 0, 2**dimension - 1
            if (mask_degree(input_mask) /= form%degree) cycle
            call mask_indices(input_mask, input_indices, input_count, dimension)
            output_mask = ieor(2**dimension - 1, input_mask)
            call mask_indices(output_mask, output_indices, output_count, dimension)
            term = hodge_term(form%degree, input_indices, output_indices, inverse, &
                dimension)
            value%component(output_mask) = value%component(output_mask) + &
                orientation*volume*form%component(input_mask)*term
        end do
    end function hodge_components

    function hodge_term(degree, input_indices, output_indices, inverse, dimension) &
            result(value)
        integer, intent(in) :: degree, input_indices(4), output_indices(4)
        integer, intent(in) :: dimension
        type(expr_t), intent(in) :: inverse(SPACETIME_DIM, SPACETIME_DIM)
        type(expr_t) :: value
        integer :: a1, a2, a3, a4, indices(4)

        value = num(inverse(1, 1)%a, 0)
        indices = 0
        select case (degree)
        case (0)
            indices = output_indices
            value = num(inverse(1, 1)%a, epsilon_dimension(indices, dimension))
        case (1)
            value = num(inverse(1, 1)%a, 0)
            do a1 = 1, dimension
                indices(1) = a1
                indices(2) = output_indices(1)
                indices(3) = output_indices(2)
                indices(4) = output_indices(3)
                value = value + inverse(input_indices(1), a1)* &
                    epsilon_dimension(indices, dimension)
            end do
        case (2)
            value = num(inverse(1, 1)%a, 0)
            do a1 = 1, dimension
                do a2 = 1, dimension
                    indices(1) = a1
                    indices(2) = a2
                    indices(3) = output_indices(1)
                    indices(4) = output_indices(2)
                    value = value + inverse(input_indices(1), a1)* &
                        inverse(input_indices(2), a2)* &
                        epsilon_dimension(indices, dimension)
                end do
            end do
        case (3)
            value = num(inverse(1, 1)%a, 0)
            do a1 = 1, dimension
                do a2 = 1, dimension
                    do a3 = 1, dimension
                        indices(1) = a1
                        indices(2) = a2
                        indices(3) = a3
                        indices(4) = output_indices(1)
                        value = value + inverse(input_indices(1), a1)* &
                            inverse(input_indices(2), a2)* &
                            inverse(input_indices(3), a3)* &
                            epsilon_dimension(indices, dimension)
                    end do
                end do
            end do
        case (4)
            value = num(inverse(1, 1)%a, 0)
            do a1 = 1, dimension
                do a2 = 1, dimension
                    do a3 = 1, dimension
                        do a4 = 1, dimension
                            indices(1) = a1
                            indices(2) = a2
                            indices(3) = a3
                            indices(4) = a4
                            value = value + inverse(input_indices(1), a1)* &
                                inverse(input_indices(2), a2)* &
                                inverse(input_indices(3), a3)* &
                                inverse(input_indices(4), a4)* &
                                epsilon_dimension(indices, dimension)
                        end do
                    end do
                end do
            end do
        end select
    end function hodge_term

    function same_form_shape(left, right) result(valid)
        type(spacetime_form_t), intent(in) :: left, right
        logical :: valid

        valid = spacetime_form_valid(left) .and. spacetime_form_valid(right)
        if (.not. valid) return
        valid = left%degree == right%degree .and. &
            left%dimension == right%dimension .and. associated(left%a, right%a)
    end function same_form_shape

    pure integer function mask_degree(mask)
        integer, intent(in) :: mask

        mask_degree = popcnt(mask)
    end function mask_degree

    subroutine mask_indices(mask, values, count, dimension)
        integer, intent(in) :: mask
        integer, intent(out) :: values(4), count
        integer, intent(in) :: dimension
        integer :: i

        values = 0
        count = 0
        do i = 1, dimension
            if (.not. btest(mask, i - 1)) cycle
            count = count + 1
            values(count) = i
        end do
    end subroutine mask_indices

    pure integer function wedge_sign(left_mask, right_mask)
        integer, intent(in) :: left_mask, right_mask
        integer :: i, j, inversions

        inversions = 0
        do i = 1, SPACETIME_DIM
            if (.not. btest(left_mask, i - 1)) cycle
            do j = 1, i - 1
                if (btest(right_mask, j - 1)) inversions = inversions + 1
            end do
        end do
        wedge_sign = 1
        if (mod(inversions, 2) == 1) wedge_sign = -1
    end function wedge_sign

    pure integer function epsilon_dimension(indices, dimension)
        integer, intent(in) :: indices(4)
        integer, intent(in) :: dimension
        integer :: i, j, inversions

        inversions = 0
        do i = 1, dimension
            do j = i + 1, dimension
                if (indices(i) > indices(j)) inversions = inversions + 1
            end do
        end do
        epsilon_dimension = 0
        do i = 1, dimension
            if (indices(i) < 1 .or. indices(i) > dimension) return
        end do
        if (has_duplicate_dimension(indices, dimension)) return
        if (mod(inversions, 2) == 0) then
            epsilon_dimension = 1
        else
            epsilon_dimension = -1
        end if
    end function epsilon_dimension

    pure logical function has_duplicate_dimension(values, dimension)
        integer, intent(in) :: values(4)
        integer, intent(in) :: dimension
        integer :: i, j

        has_duplicate_dimension = .false.
        do i = 1, dimension
            do j = i + 1, dimension
                if (values(i) == values(j)) then
                    has_duplicate_dimension = .true.
                    return
                end if
            end do
        end do
    end function has_duplicate_dimension

end module fortsym_spacetime_form
