module fortsym_lie
    ! Finite scalar pullbacks exp(t L_g) f in an arbitrary coordinate dimension.
    ! Mathematical definition: L_g f = sum_i g_i partial_i f.
    ! No application-specific symplectic form or homological equation is assumed.
    use fortsym_arena, only: NK_SYM
    use fortsym_expr, only: expr_t, num, is_valid, operator(+), operator(*), &
                            operator(/), operator(==)
    use fortsym_diff, only: diff
    use fortsym_products, only: jvp
    implicit none
    private
    public :: lie_series
contains
    subroutine lie_series(f, coordinates, vector, parameter, order, value, ok, why)
        type(expr_t), intent(in) :: f, coordinates(:), vector(:), parameter
        integer, intent(in) :: order
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: term, derivative, roots(1), tangent(1)
        integer :: i, j, k

        ok = .false.
        why = 'Lie series requires a valid scalar and parameter'
        if (.not. is_valid(f)) return
        if (.not. is_valid(parameter)) return
        why = 'Lie series requires a common arena'
        if (.not. associated(f%a, parameter%a)) return
        why = 'Lie series order must be between 0 and 12'
        if (order < 0 .or. order > 12) return
        why = 'Lie series requires equally sized nonempty coordinates and vector'
        if (size(coordinates) == 0) return
        if (size(coordinates) /= size(vector)) return
        do i = 1, size(coordinates)
            why = 'Lie series has an invalid coordinate or vector component'
            if (.not. is_valid(coordinates(i))) return
            if (.not. is_valid(vector(i))) return
            why = 'Lie series coordinates must be symbols'
            if (coordinates(i)%kind() /= NK_SYM) return
            why = 'Lie series requires a common arena'
            if (.not. associated(f%a, coordinates(i)%a)) return
            if (.not. associated(f%a, vector(i)%a)) return
            why = 'Lie series parameter must be independent of coordinates'
            derivative = diff(parameter, coordinates(i))
            if (.not. (derivative == num(f%a, 0))) return
            do j = 1, i - 1
                why = 'Lie series coordinates must be distinct'
                if (coordinates(i) == coordinates(j)) return
            end do
        end do
        value = f
        term = f
        do k = 1, order
            roots(1) = term
            tangent = jvp(roots, coordinates, vector)
            term = tangent(1)/k
            value = value + parameter_power(parameter, k)*term
        end do
        ok = .true.
        why = ''
    end subroutine lie_series

    function parameter_power(parameter, k) result(value)
        type(expr_t), intent(in) :: parameter
        integer, intent(in) :: k
        type(expr_t) :: value
        integer :: i
        value = num(parameter%a, 1)
        do i = 1, k
            value = value*parameter
        end do
    end function parameter_power
end module fortsym_lie
