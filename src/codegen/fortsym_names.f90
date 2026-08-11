module fortsym_names
    ! Shared spelling rules for source emitters that target Fortran.
    use fortsym_string, only: str_t, str, chars
    implicit none
    private

    public :: valid_fortran_name, valid_fortran_symbol, same_fortran_name, &
        lower_fortran_name
    public :: map_fortran_names

contains

    pure function valid_fortran_name(name) result(ok)
        character(*), intent(in) :: name
        logical :: ok
        integer :: k
        character :: c

        ok = len(name) > 0 .and. len(name) <= 63
        if (.not. ok) return
        c = name(1:1)
        if (.not. is_letter(c)) then
            ok = .false.
            return
        end if
        do k = 2, len(name)
            c = name(k:k)
            if (.not. (is_letter(c) .or. is_digit(c) .or. c == "_")) then
                ok = .false.
                return
            end if
        end do
    end function valid_fortran_name

    pure function valid_fortran_symbol(name) result(ok)
        character(*), intent(in) :: name
        logical :: ok
        integer :: open, close, k, code
        character(:), allocatable :: base

        ok = valid_fortran_name(name)
        if (ok) return
        open = index(name, "(")
        close = len_trim(name)
        if (open <= 1 .or. close <= open + 1) return
        if (name(close:close) /= ")") return
        base = trim(name(:open - 1))
        if (.not. valid_fortran_name(base)) return
        do k = open + 1, close - 1
            code = iachar(name(k:k))
            if (.not. ((code >= iachar("A") .and. code <= iachar("Z")) .or. &
                (code >= iachar("a") .and. code <= iachar("z")) .or. &
                (code >= iachar("0") .and. code <= iachar("9")) .or. &
                index("_ ,:+-*/", name(k:k)) > 0)) return
        end do
        ok = .true.
    end function valid_fortran_symbol

    pure function same_fortran_name(left, right) result(matches)
        character(*), intent(in) :: left, right
        logical :: matches
        integer :: k

        matches = len(left) == len(right)
        if (.not. matches) return
        do k = 1, len(left)
            if (lower_ascii(left(k:k)) /= lower_ascii(right(k:k))) then
                matches = .false.
                return
            end if
        end do
    end function same_fortran_name

    pure function lower_fortran_name(name) result(lower)
        character(*), intent(in) :: name
        character(len(name)) :: lower
        integer :: k

        do k = 1, len(name)
            lower(k:k) = lower_ascii(name(k:k))
        end do
    end function lower_fortran_name

    !> Make a Fortran interface unique under its case-insensitive identifier
    !> rules. Names that do not collide remain byte-for-byte unchanged. In a
    !> collision group the lower-case spelling is retained when present and
    !> the remaining names receive deterministic suffixes.
    subroutine map_fortran_names(names, mapped, changed, ok, message)
        type(str_t), intent(in) :: names(:)
        type(str_t), allocatable, intent(out) :: mapped(:)
        logical, allocatable, intent(out) :: changed(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message

        integer :: i, j, k, suffix, canonical
        logical :: collision, used
        character(:), allocatable :: base, candidate

        allocate (mapped(size(names)))
        allocate (changed(size(names)), source=.false.)
        ok = .false.
        message = ""
        do i = 1, size(names)
            if (.not. valid_fortran_name(chars(names(i)))) then
                message = "kernel emitter: name is not a valid Fortran identifier: "// &
                    chars(names(i))
                return
            end if
            do j = 1, i - 1
                if (chars(names(j)) == chars(names(i))) then
                    message = "kernel emitter: exact Fortran name is duplicated: "// &
                        chars(names(i))
                    return
                end if
            end do
        end do

        do i = 1, size(names)
            collision = .false.
            do j = 1, size(names)
                if (j /= i .and. same_fortran_name(chars(names(i)), &
                    chars(names(j)))) then
                    collision = .true.
                    exit
                end if
            end do
            if (.not. collision) then
                mapped(i) = names(i)
                cycle
            end if

            canonical = 0
            do j = 1, size(names)
                if (.not. same_fortran_name(chars(names(i)), chars(names(j)))) cycle
                if (chars(names(j)) == lower_fortran_name(chars(names(j)))) then
                    canonical = j
                    exit
                end if
            end do
            if (canonical == 0) then
                do j = 1, size(names)
                    if (same_fortran_name(chars(names(i)), chars(names(j)))) then
                        canonical = j
                        exit
                    end if
                end do
            end if

            base = lower_fortran_name(chars(names(canonical)))
            if (i == canonical) then
                candidate = base
            else
                suffix = 1
                do
                    candidate = base//"__"//suffix_text(suffix)
                    used = .not. valid_fortran_name(candidate)
                    do k = 1, size(names)
                        if (same_fortran_name(candidate, chars(names(k)))) used = .true.
                    end do
                    do k = 1, i - 1
                        if (same_fortran_name(candidate, chars(mapped(k)))) used = .true.
                    end do
                    if (.not. used) exit
                    suffix = suffix + 1
                end do
            end if
            if (.not. valid_fortran_name(candidate)) then
                message = "kernel emitter: cannot create a valid Fortran name from: "// &
                    chars(names(i))
                return
            end if
            mapped(i) = str(candidate)
            changed(i) = chars(mapped(i)) /= chars(names(i))
        end do
        ok = .true.
    end subroutine map_fortran_names

    pure function is_letter(c) result(ok)
        character, intent(in) :: c
        logical :: ok
        ok = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z")
    end function is_letter

    pure function is_digit(c) result(ok)
        character, intent(in) :: c
        logical :: ok
        ok = c >= "0" .and. c <= "9"
    end function is_digit

    pure function lower_ascii(c) result(lower)
        character, intent(in) :: c
        character :: lower
        if (c >= "A" .and. c <= "Z") then
            lower = achar(iachar(c) + iachar("a") - iachar("A"))
        else
            lower = c
        end if
    end function lower_ascii

    pure function suffix_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(16) :: buffer
        write (buffer, "(i0)") value
        text = trim(buffer)
    end function suffix_text

end module fortsym_names
