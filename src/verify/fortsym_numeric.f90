module fortsym_numeric
    ! N[expr]: the numeric value of a closed expression.
    !
    ! This is the operation with the most room to be confidently wrong, so it is
    ! deliberately narrow. Three things are refused rather than answered:
    !
    !   * an expression with a free symbol. There is no defensible value for
    !     `x + 1`, and the tempting default -- treat the unknown as zero -- turns
    !     a missing binding into a number that looks like an answer.
    !   * a result that is NaN or infinite. `1/0` is a pole; reporting it as a
    !     finite double, or as the largest one, is the worst failure available
    !     here because the caller cannot tell it apart from a real value.
    !   * anything the evaluator itself declines (unknown function head, a real
    !     probe meeting the imaginary unit, a branch cut).
    !
    ! numeric_text goes from short to long rather than long to short. Rounding a
    ! 17-digit string down to fewer digits can change the value it reads back as;
    ! growing the digit count and stopping at the first string that reads back
    ! bit-identical cannot, because the accepted string is verified by reading it.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_expr, only: expr_t
    use fortsym_eval, only: binding_t, eval_expr, free_symbols_of
    implicit none
    private

    public :: numeric_value, numeric_text

    integer, parameter :: dp = real64

    ! 17 significant digits recover every real64 exactly; no string needs more,
    ! so a search that reaches this without a round trip indicates a broken
    ! formatter rather than a hard number.
    integer, parameter :: MAX_SIGNIFICANT = 17

contains

    !> The real value of a closed expression, or a refusal naming what stopped
    !> the evaluation.
    subroutine numeric_value(e, value, ok, why)
        type(expr_t),              intent(in)  :: e
        real(dp),                  intent(out) :: value
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t), allocatable :: names(:)
        logical :: defined

        value = 0.0_dp
        ok = .false.
        why = ""

        names = free_symbols_of(e)
        if (size(names) > 0) then
            why = "free symbol "//chars(names(1))// &
                  ": N needs a closed expression"
            return
        end if

        value = eval_expr(e, empty_binding(), defined)
        if (.not. defined) then
            value = 0.0_dp
            why = "expression has no real numeric value at this point "// &
                  "(pole, branch cut, or unsupported head)"
            return
        end if

        ! eval_expr already reports these, but N is the operation whose output
        ! gets printed and acted on, so the guard is repeated where it matters.
        if (value /= value) then
            value = 0.0_dp
            why = "result is not a number"
            return
        end if
        if (value > huge(1.0_dp) .or. value < -huge(1.0_dp)) then
            value = 0.0_dp
            why = "result is infinite"
            return
        end if

        ok = .true.
    end subroutine numeric_value

    !> The shortest decimal string that reads back as exactly the same real64.
    function numeric_text(e, ok, why) result(text)
        type(expr_t),              intent(in)  :: e
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        character(:), allocatable :: text
        real(dp) :: value

        text = ""
        call numeric_value(e, value, ok, why)
        if (.not. ok) return

        text = shortest_round_trip(value, ok)
        if (.not. ok) then
            text = ""
            why = "no decimal form of the result reads back exactly"
        end if
    end function numeric_text

    !> Try one significant digit, then two, and so on, accepting the first form
    !> that reads back bit-identical. Every candidate is verified by reading it,
    !> so a shorter string is never accepted on the assumption that it is safe.
    function shortest_round_trip(value, ok) result(text)
        real(dp), intent(in)  :: value
        logical,  intent(out) :: ok
        character(:), allocatable :: text
        real(dp) :: back
        integer  :: d, ios

        text = ""
        ok = .false.

        do d = 1, MAX_SIGNIFICANT
            text = formatted(value, d)
            if (len(text) == 0) cycle
            back = 0.0_dp
            read (text, *, iostat=ios) back
            if (ios /= 0) cycle
            if (back == value) then
                ok = .true.
                return
            end if
        end do

        text = ""
    end function shortest_round_trip

    !> Scientific form with `digits` significant digits. The fixed local buffer
    !> is the one the standard forces on an internal write; the result is sliced
    !> to its written length so no padding escapes. A field too narrow fills with
    !> asterisks rather than truncating, hence the width check.
    function formatted(value, digits) result(text)
        real(dp), intent(in) :: value
        integer,  intent(in) :: digits
        character(:), allocatable :: text
        character(len=64) :: buf
        character(len=32) :: fmt
        integer :: ios

        text = ""
        write (fmt, '("(es40.",i0,"e3)")') digits - 1
        write (buf, fmt, iostat=ios) value
        if (ios /= 0) return
        buf = adjustl(buf)
        if (index(buf, "*") > 0) return
        text = buf(1:len_trim(buf))
    end function formatted

    !> A binding with no names: every symbol lookup fails, which is what makes
    !> an accidentally open expression a refusal rather than a zero.
    function empty_binding() result(b)
        type(binding_t) :: b
        allocate (b%names(0))
        allocate (b%values(0))
        b%n = 0
    end function empty_binding

end module fortsym_numeric
