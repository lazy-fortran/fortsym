module fortsym_volume
    ! Metric volume and Levi-Civita objects for the fixed three-dimensional
    ! geometry owner.  The positive volume density, orientation, and index
    ! variance are kept separate so callers cannot accidentally use a signed
    ! Jacobian where a density is required.
    use fortsym_arena, only: arena_t
    use fortsym_chart, only: DIM
    use fortsym_metric, only: metric_t, metric_valid, metric_arena, &
        metric_orientation, metric_signature, metric_sqrtg
    use fortsym_expr, only: expr_t, num, operator(*), operator(/)
    implicit none
    private

    public :: metric_volume_density, levi_civita_symbol, metric_levi_civita

contains

    !> Positive scalar density sqrt(abs(det(g))).
    function metric_volume_density(g) result(value)
        type(metric_t), intent(in) :: g
        type(expr_t) :: value

        value = metric_sqrtg(g)
    end function metric_volume_density

    !> Three-dimensional Levi-Civita symbol [ijk] in the native orientation.
    pure function levi_civita_symbol(i, j, k) result(value)
        integer, intent(in) :: i, j, k
        integer :: value

        value = 0
        if (i == j .or. i == k .or. j == k) return
        if ((i == 1 .and. j == 2 .and. k == 3) .or. &
            (i == 2 .and. j == 3 .and. k == 1) .or. &
            (i == 3 .and. j == 1 .and. k == 2)) then
            value = 1
        else if ((i == 1 .and. j == 3 .and. k == 2) .or. &
            (i == 3 .and. j == 2 .and. k == 1) .or. &
            (i == 2 .and. j == 1 .and. k == 3)) then
            value = -1
        end if
    end function levi_civita_symbol

    !> Metric Levi-Civita tensor with all indices covariant (-1) or
    !> contravariant (+1).  The stored metric orientation is included in the
    !> result; the raw symbol is available separately above.
    function metric_levi_civita(g, variance) result(value)
        type(metric_t), intent(in) :: g
        integer, intent(in) :: variance
        type(expr_t) :: value(DIM, DIM, DIM)
        type(arena_t), pointer :: a
        type(expr_t) :: factor
        integer :: signature(DIM)
        integer :: i, j, k, symbol, signature_product

        if (.not. metric_valid(g)) return
        if (variance /= -1 .and. variance /= 1) return
        a => metric_arena(g)
        do k = 1, DIM
            do j = 1, DIM
                do i = 1, DIM
                    value(i, j, k) = num(a, 0)
                end do
            end do
        end do

        factor = num(a, metric_orientation(g))
        if (variance == -1) then
            factor = factor*metric_sqrtg(g)
        else
            signature = metric_signature(g)
            signature_product = signature(1)*signature(2)*signature(3)
            factor = factor*num(a, signature_product)/metric_sqrtg(g)
        end if

        do k = 1, DIM
            do j = 1, DIM
                do i = 1, DIM
                    symbol = levi_civita_symbol(i, j, k)
                    if (symbol /= 0) value(i, j, k) = factor*num(a, symbol)
                end do
            end do
        end do
    end function metric_levi_civita

end module fortsym_volume
