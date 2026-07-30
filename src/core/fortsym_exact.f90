module fortsym_exact
    ! Exact scalar arithmetic over FLINT's canonical fmpq domain.
    !
    ! Values cross the C boundary as base-ten strings. No FLINT object or
    ! allocation escapes the shim: each operation renders into a thread-local
    ! buffer, and this module copies it into fortsym's value string immediately.
    ! There is one pending result per thread: same-thread re-entry before fetch
    ! overwrites it, so the render/fetch pair remains private to these wrappers.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, &
        c_null_char, c_size_t
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_string, only: str_t, str
    use fortsym_capi, only: FSYM_EXACT_ADD, FSYM_EXACT_SUB, FSYM_EXACT_MUL, &
        FSYM_EXACT_DIV, fsym_exact_normalize, fsym_exact_binary, &
        fsym_exact_pow_si, fsym_exact_fetch, fsym_flint_is_shared
    implicit none
    private

    public :: exact_normalize
    public :: exact_add, exact_sub, exact_mul, exact_div, exact_pow
    public :: exact_uses_shared_flint

    integer, parameter :: MAX_EXACT_INPUT_BYTES = 1024*1024

contains

    function exact_uses_shared_flint() result(shared)
        logical :: shared
        shared = fsym_flint_is_shared() /= 0_c_int
    end function exact_uses_shared_flint

    function exact_normalize(value, ok) result(canonical)
        character(*), intent(in) :: value
        logical,      intent(out) :: ok
        type(str_t)               :: canonical
        integer(c_size_t) :: n

        ok = valid_input(value)
        if (.not. ok) then
            canonical = str("")
            return
        end if
        n = fsym_exact_normalize(c_string(value))
        canonical = fetch_exact(n, ok)
    end function exact_normalize

    function exact_add(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = exact_binary(left, right, FSYM_EXACT_ADD, ok)
    end function exact_add

    function exact_sub(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = exact_binary(left, right, FSYM_EXACT_SUB, ok)
    end function exact_sub

    function exact_mul(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = exact_binary(left, right, FSYM_EXACT_MUL, ok)
    end function exact_mul

    function exact_div(left, right, ok) result(value)
        character(*), intent(in) :: left, right
        logical,      intent(out) :: ok
        type(str_t)               :: value
        value = exact_binary(left, right, FSYM_EXACT_DIV, ok)
    end function exact_div

    function exact_pow(base, exponent, ok) result(value)
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
        n = fsym_exact_pow_si(c_string(base), int(exponent, c_int64_t))
        value = fetch_exact(n, ok)
    end function exact_pow

    function exact_binary(left, right, operation, ok) result(value)
        character(*), intent(in) :: left, right
        integer(c_int), intent(in) :: operation
        logical,      intent(out) :: ok
        type(str_t)               :: value
        integer(c_size_t) :: n

        ok = valid_input(left)
        if (ok) ok = valid_input(right)
        if (.not. ok) then
            value = str("")
            return
        end if
        n = fsym_exact_binary(c_string(left), c_string(right), operation)
        value = fetch_exact(n, ok)
    end function exact_binary

    pure function valid_input(value) result(valid)
        character(*), intent(in) :: value
        logical                  :: valid

        valid = .false.
        if (len(value) > MAX_EXACT_INPUT_BYTES) return
        if (index(value, achar(0)) /= 0) return
        valid = len(value) > 0
    end function valid_input

    pure function c_string(value) result(c)
        character(*), intent(in) :: value
        character(len=len(value) + 1, kind=c_char) :: c
        c = value//c_null_char
    end function c_string

    function fetch_exact(n, ok) result(value)
        integer(c_size_t), intent(in) :: n
        logical,           intent(out) :: ok
        type(str_t)                    :: value
        character(kind=c_char), allocatable :: buffer(:)
        character(:), allocatable :: text
        integer(c_size_t) :: copied
        integer :: i, count

        ok = n > 0_c_size_t
        if (ok) ok = n <= int(huge(0), c_size_t)
        if (.not. ok) then
            value = str("")
            return
        end if

        count = int(n)
        allocate (buffer(count))
        copied = fsym_exact_fetch(buffer, n)
        ok = copied == n
        if (ok) ok = copied <= int(huge(0), c_size_t)
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
    end function fetch_exact

end module fortsym_exact
