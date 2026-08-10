module fortsym_precision
    ! Precision choices for generated scalar kernels.
    implicit none
    private

    public :: PRECISION_REAL64, PRECISION_REAL32, PRECISION_MIXED
    public :: precision_is_valid, precision_name

    integer, parameter :: PRECISION_REAL64 = 1
    integer, parameter :: PRECISION_REAL32 = 2
    integer, parameter :: PRECISION_MIXED = 3

contains

    pure function precision_is_valid(precision) result(valid)
        integer, intent(in) :: precision
        logical :: valid

        valid = precision == PRECISION_REAL64 .or. &
            precision == PRECISION_REAL32 .or. precision == PRECISION_MIXED
    end function precision_is_valid

    pure function precision_name(precision) result(name)
        integer, intent(in) :: precision
        character(:), allocatable :: name

        select case (precision)
        case (PRECISION_REAL64)
            name = "real64"
        case (PRECISION_REAL32)
            name = "real32"
        case (PRECISION_MIXED)
            name = "mixed"
        case default
            name = "invalid"
        end select
    end function precision_name

end module fortsym_precision
