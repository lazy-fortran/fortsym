module fortsym_series_guard
    ! Shared validity boundary for native Taylor coefficients and bindings.
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_CONST, NK_POW, NK_FUNC
    implicit none
    private
    public :: contains_nonfinite, contains_singular_power
    public :: contains_opaque_derivative, series_refusal
contains
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

end module fortsym_series_guard
