module fortsym_algebraic
    ! Bounded exact real and complex algebraic arithmetic over FLINT qqbar.
    !
    ! The public value is a lossless qqbar1 string containing the primitive
    ! minimal polynomial and the root's index in FLINT's canonical conjugate
    ! order. No qqbar object or allocation crosses the C boundary.
    use, intrinsic :: iso_c_binding, only: c_char, c_double, c_int, c_int64_t, &
        c_null_char, c_size_t
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, str
    use fortsym_capi, only: FSYM_ALGEBRAIC_ADD, FSYM_ALGEBRAIC_SUB, &
        FSYM_ALGEBRAIC_MUL, FSYM_ALGEBRAIC_DIV, FSYM_ALGEBRAIC_CONJ, &
        FSYM_ALGEBRAIC_SQRT, fsym_algebraic_normalize, fsym_algebraic_get_d, &
        fsym_algebraic_i, &
        fsym_algebraic_from_re_im, fsym_algebraic_binary, &
        fsym_algebraic_re, fsym_algebraic_im, fsym_algebraic_unary, &
        fsym_algebraic_pow_si, fsym_algebraic_signs, fsym_algebraic_fetch
    implicit none
    private

    public :: algebraic_normalize, algebraic_i, algebraic_from_re_im
    public :: algebraic_to_real
    public :: algebraic_re, algebraic_im
    public :: algebraic_add, algebraic_sub, algebraic_mul, algebraic_div
    public :: algebraic_conjugate, algebraic_sqrt, algebraic_pow
    public :: algebraic_signs

    integer, parameter :: MAX_ALGEBRAIC_BYTES = 64*1024

contains

    function algebraic_normalize(input, ok) result(value)
        character(*), intent(in) :: input
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t) :: n

        ok = valid_input(input)
        if (.not. ok) then
            value = str("")
            return
        end if
        n = fsym_algebraic_normalize(c_string(input))
        value = fetch_algebraic(n, ok)
    end function algebraic_normalize

    function algebraic_i(ok) result(value)
        logical, intent(out) :: ok
        type(str_t)          :: value
        value = fetch_algebraic(fsym_algebraic_i(), ok)
    end function algebraic_i

    function algebraic_to_real(input, ok) result(value)
        character(*), intent(in) :: input
        logical, intent(out) :: ok
        real(c_double) :: value

        value = 0.0_c_double
        ok = valid_input(input)
        if (.not. ok) return
        ok = fsym_algebraic_get_d(c_string(input), value) /= 0_c_int
    end function algebraic_to_real

    function algebraic_from_re_im(real_part, imag_part, ok) result(value)
        character(*), intent(in) :: real_part, imag_part
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t) :: n

        ok = valid_input(real_part) .and. valid_input(imag_part)
        if (.not. ok) then
            value = str("")
            return
        end if
        n = fsym_algebraic_from_re_im(c_string(real_part), c_string(imag_part))
        value = fetch_algebraic(n, ok)
    end function algebraic_from_re_im

    function algebraic_re(input, ok) result(value)
        character(*), intent(in) :: input
        logical,      intent(out) :: ok
        type(str_t)               :: value

        value = algebraic_component(input, .true., ok)
    end function algebraic_re

    function algebraic_im(input, ok) result(value)
        character(*), intent(in) :: input
        logical,      intent(out) :: ok
        type(str_t)               :: value

        value = algebraic_component(input, .false., ok)
    end function algebraic_im

    function algebraic_add(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = algebraic_binary(left, right, FSYM_ALGEBRAIC_ADD, ok)
    end function algebraic_add

    function algebraic_sub(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = algebraic_binary(left, right, FSYM_ALGEBRAIC_SUB, ok)
    end function algebraic_sub

    function algebraic_mul(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = algebraic_binary(left, right, FSYM_ALGEBRAIC_MUL, ok)
    end function algebraic_mul

    function algebraic_div(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = algebraic_binary(left, right, FSYM_ALGEBRAIC_DIV, ok)
    end function algebraic_div

    function algebraic_conjugate(input, ok) result(value)
        character(*), intent(in) :: input
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = algebraic_unary(input, FSYM_ALGEBRAIC_CONJ, ok)
    end function algebraic_conjugate

    function algebraic_sqrt(input, ok) result(value)
        character(*), intent(in) :: input
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = algebraic_unary(input, FSYM_ALGEBRAIC_SQRT, ok)
    end function algebraic_sqrt

    function algebraic_pow(base, exponent, ok) result(value)
        character(*), intent(in) :: base
        integer(int64), intent(in) :: exponent
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t) :: n

        ok = valid_input(base)
        if (.not. ok) then
            value = str("")
            return
        end if
        n = fsym_algebraic_pow_si(c_string(base), &
            int(exponent, c_int64_t))
        value = fetch_algebraic(n, ok)
    end function algebraic_pow

    subroutine algebraic_signs(input, real_sign, imag_sign, ok)
        character(*), intent(in) :: input
        integer,      intent(out) :: real_sign, imag_sign
        logical,      intent(out) :: ok
        integer(c_int) :: c_real, c_imag

        real_sign = 0
        imag_sign = 0
        ok = valid_input(input)
        if (.not. ok) return
        ok = fsym_algebraic_signs(c_string(input), c_real, c_imag) /= 0_c_int
        if (.not. ok) return
        real_sign = int(c_real)
        imag_sign = int(c_imag)
    end subroutine algebraic_signs

    function algebraic_binary(left, right, operation, ok) result(value)
        character(*), intent(in) :: left, right
        integer(c_int), intent(in) :: operation
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t) :: n

        ok = valid_input(left) .and. valid_input(right)
        if (.not. ok) then
            value = str("")
            return
        end if
        n = fsym_algebraic_binary(c_string(left), c_string(right), operation)
        value = fetch_algebraic(n, ok)
    end function algebraic_binary

    function algebraic_unary(input, operation, ok) result(value)
        character(*), intent(in) :: input
        integer(c_int), intent(in) :: operation
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t) :: n

        ok = valid_input(input)
        if (.not. ok) then
            value = str("")
            return
        end if
        n = fsym_algebraic_unary(c_string(input), operation)
        value = fetch_algebraic(n, ok)
    end function algebraic_unary

    function algebraic_component(input, real_part, ok) result(value)
        character(*), intent(in) :: input
        logical,      intent(in) :: real_part
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t)         :: n

        ok = valid_input(input)
        if (.not. ok) then
            value = str("")
            return
        end if
        if (real_part) then
            n = fsym_algebraic_re(c_string(input))
        else
            n = fsym_algebraic_im(c_string(input))
        end if
        value = fetch_algebraic(n, ok)
    end function algebraic_component

    pure function valid_input(value) result(valid)
        character(*), intent(in) :: value
        logical                  :: valid
        valid = len(value) > 0 .and. len(value) <= MAX_ALGEBRAIC_BYTES
        if (valid) valid = index(value, achar(0)) == 0
    end function valid_input

    pure function c_string(value) result(c)
        character(*), intent(in) :: value
        character(len=len(value) + 1, kind=c_char) :: c
        c = value//c_null_char
    end function c_string

    function fetch_algebraic(n, ok) result(value)
        integer(c_size_t), intent(in) :: n
        logical,           intent(out) :: ok
        type(str_t)                    :: value
        character(kind=c_char), allocatable :: buffer(:)
        character(:), allocatable :: text
        integer(c_size_t) :: copied
        integer :: i, count

        ok = n > 0_c_size_t .and. n <= int(huge(0), c_size_t)
        if (.not. ok) then
            value = str("")
            return
        end if
        count = int(n)
        allocate (buffer(count))
        copied = fsym_algebraic_fetch(buffer, n)
        ok = copied == n .and. copied <= int(huge(0), c_size_t)
        if (.not. ok) then
            value = str("")
            return
        end if
        count = int(copied)
        allocate (character(len=count) :: text)
        do i = 1, count
            text(i:i) = buffer(i)
        end do
        value = str(text)
    end function fetch_algebraic

end module fortsym_algebraic
