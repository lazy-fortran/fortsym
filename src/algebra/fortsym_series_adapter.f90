module fortsym_series_adapter
    ! Binding-safe boundary for the native Taylor engine.  The engine returns
    ! the normal polynomial (without SymPy's O-term); the compatibility layer
    ! maps its term-count convention to the native highest-degree convention.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t
    use fortsym_series_guard, only: contains_nonfinite, contains_singular_power, &
        contains_opaque_derivative, series_refusal
    use fortsym_expr, only: expr_t
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    implicit none
    private

    public :: calculate_series
    public :: calculate_series_coeff

    integer, parameter :: MAX_ORDER = 64

contains

    subroutine calculate_series(a, e, var, point, order, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: e, var, point
        integer, intent(in) :: order
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        if (contains_nonfinite(a, point%id)) then
            value = e
            ok = .false.
            why = "native: Taylor expansion point must be finite"
            return
        end if
        if (order > MAX_ORDER) then
            value = e
            ok = .false.
            why = "native: Taylor order exceeds the bounded maximum of 64"
            return
        end if
        engine = make_native_engine(a)
        result = engine%series(e, var, point, order)
        value = result%value
        ok = result%ok
        if (.not. ok) then
            why = chars(result%message)
            return
        end if
        if (contains_nonfinite(a, value%id) .or. &
            contains_singular_power(a, value%id) .or. &
            contains_opaque_derivative(a, value%id)) then
            ok = .false.
            why = series_refusal(a, value%id)
        end if
    end subroutine calculate_series

    subroutine calculate_series_coeff(a, e, var, point, order, value, ok, why)
        type(arena_t), target, intent(inout) :: a
        type(expr_t), intent(in) :: e, var, point
        integer, intent(in) :: order
        type(expr_t), intent(out) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(native_engine_t) :: engine
        type(engine_result_t) :: result

        if (contains_nonfinite(a, point%id)) then
            value = e
            ok = .false.
            why = "native: Taylor expansion point must be finite"
            return
        end if
        if (order > MAX_ORDER) then
            value = e
            ok = .false.
            why = "native: Taylor order exceeds the bounded maximum of 64"
            return
        end if
        engine = make_native_engine(a)
        result = engine%series_coeff(e, var, point, order)
        value = result%value
        ok = result%ok
        if (.not. ok) then
            why = chars(result%message)
            return
        end if
        if (contains_nonfinite(a, value%id) .or. &
            contains_singular_power(a, value%id) .or. &
            contains_opaque_derivative(a, value%id)) then
            ok = .false.
            why = series_refusal(a, value%id)
        end if
    end subroutine calculate_series_coeff

end module fortsym_series_adapter
