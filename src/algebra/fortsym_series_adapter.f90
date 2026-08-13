module fortsym_series_adapter
    ! Binding-safe boundary for the native Taylor engine.  The engine returns
    ! the normal polynomial (without SymPy's O-term); the compatibility layer
    ! maps its term-count convention to the native highest-degree convention.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_CONST, NK_POW, NK_FUNC
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

    recursive logical function contains_nonfinite(a, id) result(found)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer :: k
        character(:), allocatable :: name

        found = .false.
        if (a%kind_of(id) == NK_CONST) then
            name = chars(a%name_of(id))
            select case (name)
            case ("oo", "zoo", "nan")
                found = .true.
                return
            end select
        end if
        do k = 1, a%nargs_of(id)
            if (contains_nonfinite(a, a%arg_of(id, k))) then
                found = .true.
                return
            end if
        end do
    end function contains_nonfinite

    recursive logical function contains_singular_power(a, id) result(found)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer :: base, exponent, k
        logical :: negative_exponent

        found = .false.
        if (a%kind_of(id) == NK_POW) then
            base = a%arg_of(id, 1)
            exponent = a%arg_of(id, 2)
            negative_exponent = .false.
            select case (a%kind_of(exponent))
            case (NK_INT, NK_RAT)
                negative_exponent = a%num_of(exponent) < 0
            end select
            if (negative_exponent .and. exact_zero(a, base)) then
                found = .true.
                return
            end if
        end if
        do k = 1, a%nargs_of(id)
            if (contains_singular_power(a, a%arg_of(id, k))) then
                found = .true.
                return
            end if
        end do
    end function contains_singular_power

    recursive logical function contains_opaque_derivative(a, id) result(found)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer :: k
        character(:), allocatable :: name

        found = .false.
        if (a%kind_of(id) == NK_FUNC) then
            name = chars(a%name_of(id))
            if (index(name, "Derivative") == 1) then
                found = .true.
                return
            end if
        end if
        do k = 1, a%nargs_of(id)
            if (contains_opaque_derivative(a, a%arg_of(id, k))) then
                found = .true.
                return
            end if
        end do
    end function contains_opaque_derivative

    function series_refusal(a, id) result(why)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        character(:), allocatable :: why

        if (contains_opaque_derivative(a, id)) then
            why = "native: Taylor expansion requires an unsupported symbolic "// &
                "derivative"
        else
            why = "native: Taylor series reaches a non-finite coefficient "// &
                "at the expansion point"
        end if
    end function series_refusal

    logical function exact_zero(a, id) result(zero)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id

        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            zero = a%num_of(id) == 0
        case default
            zero = .false.
        end select
    end function exact_zero

end module fortsym_series_adapter
