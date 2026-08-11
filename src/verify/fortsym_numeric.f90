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
    use fortsym_arena, only: NK_SYM
    use fortsym_expr, only: expr_t, real_expr, is_valid, same_arena, &
        operator(==)
    use fortsym_eval, only: binding_t, eval_expr, collect_free_symbols
    use fortsym_assume, only: assumption_context_t
    use fortsym_complexdom, only: complex_split
    use fortsym_engine_symengine, only: symengine_evalf_text
    use fortsym_subs, only: subs_many
    implicit none
    private

    public :: numeric_value, numeric_text, numeric_precision_text
    public :: numeric_real_text_t, numeric_complex_text, numeric_complex_text_t
    public :: numeric_callable_t

    integer, parameter :: dp = real64

    ! 17 significant digits recover every real64 exactly; no string needs more,
    ! so a search that reaches this without a round trip indicates a broken
    ! formatter rather than a hard number.
    integer, parameter :: MAX_SIGNIFICANT = 17

    !> A requested-precision real result. The text remains decimal so the
    !> caller cannot mistake it for a real64 projection. `digits` records the
    !> requested decimal precision used by the evaluator.
    type :: numeric_real_text_t
        character(:), allocatable :: text
        integer :: digits = 0
    end type numeric_real_text_t

    !> Precision-bearing rectangular result. The components are decimal text
    !> rather than real64 so a caller cannot mistake a rounded projection for
    !> the requested precision.
    type :: numeric_complex_text_t
        character(:), allocatable :: real
        character(:), allocatable :: imag
        integer :: digits = 0
    end type numeric_complex_text_t

    !> A closed expression plus its real64 argument order. This is the small
    !> adapter boundary used by numerical libraries: they own the quadrature,
    !> root-finding, or interpolation algorithm and call evaluate, while
    !> fortsym owns substitution and domain refusal.
    type :: numeric_callable_t
        private
        type(expr_t) :: expression
        type(expr_t), allocatable :: variables(:)
        logical :: ready = .false.
    contains
        procedure :: initialise => numeric_callable_initialise
        procedure :: evaluate => numeric_callable_evaluate
        procedure :: evaluate_text => numeric_callable_evaluate_text
    end type numeric_callable_t

    interface numeric_precision_text
        module procedure numeric_precision_text_chars
        module procedure numeric_precision_text_value
    end interface numeric_precision_text

contains

    !> The real value of a closed expression, or a refusal naming what stopped
    !> the evaluation.
    subroutine numeric_value(e, value, ok, why)
        type(expr_t),              intent(in)  :: e
        real(dp),                  intent(out) :: value
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t), allocatable :: names(:)
        type(binding_t) :: empty
        logical :: defined

        value = 0.0_dp
        ok = .false.
        why = ""

        call collect_free_symbols(e, names)
        if (size(names) > 0) then
            why = "free symbol "//chars(names(1))// &
                ": N needs a closed expression"
            return
        end if

        empty = empty_binding()
        value = eval_expr(e, empty, defined)
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

    !> Evaluate a closed real expression to a decimal at requested precision.
    !> The MPFR-backed SymEngine path is kept separate from numeric_value so a
    !> high-precision result is never rounded into real64 on the way out.
    subroutine numeric_precision_text_chars(e, digits, text, ok, why)
        type(expr_t),              intent(in)  :: e
        integer,                   intent(in)  :: digits
        character(:), allocatable, intent(out) :: text
        logical,                   intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(str_t), allocatable :: names(:)

        text = ""
        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "invalid expression has no numeric value"
            return
        end if
        if (digits < 1 .or. digits > 512) then
            why = "requested precision must be between 1 and 512 digits"
            return
        end if

        call collect_free_symbols(e, names)
        if (size(names) > 0) then
            why = "free symbol "//chars(names(1))// &
                ": requested-precision evaluation needs a closed expression"
            return
        end if
        text = symengine_evalf_text(e, digits, ok, why)
    end subroutine numeric_precision_text_chars

    subroutine numeric_precision_text_value(e, digits, result, ok, why)
        type(expr_t),                intent(in)  :: e
        integer,                     intent(in)  :: digits
        type(numeric_real_text_t),   intent(out) :: result
        logical,                     intent(out) :: ok
        character(:), allocatable,   intent(out) :: why
        character(:), allocatable :: text

        result%text = ""
        result%digits = 0
        call numeric_precision_text_chars(e, digits, text, ok, why)
        if (.not. ok) return
        result%text = text
        result%digits = digits
    end subroutine numeric_precision_text_value

    !> Evaluate a supported closed complex expression as independent
    !> high-precision real and imaginary decimal components. The rectangular
    !> splitter supplies the domain boundary; branch-sensitive heads and
    !> unknown functions are refused rather than assigned a principal branch.
    subroutine numeric_complex_text(e, digits, result, ok, why)
        type(expr_t),                  intent(in)  :: e
        integer,                       intent(in)  :: digits
        type(numeric_complex_text_t),  intent(out) :: result
        logical,                       intent(out) :: ok
        character(:), allocatable,     intent(out) :: why
        type(assumption_context_t) :: facts
        type(expr_t) :: re, im
        logical :: split_ok

        result%real = ""
        result%imag = ""
        result%digits = 0
        ok = .false.
        why = ""
        if (.not. is_valid(e)) then
            why = "invalid expression has no complex numeric value"
            return
        end if

        call facts%init(e%a)
        call complex_split(e, facts, re, im, split_ok, why)
        if (.not. split_ok) return
        call numeric_precision_text(re, digits, result%real, ok, why)
        if (.not. ok) return
        call numeric_precision_text(im, digits, result%imag, ok, why)
        if (.not. ok) then
            result%real = ""
            return
        end if
        result%digits = digits
    end subroutine numeric_complex_text

    subroutine numeric_callable_initialise(self, expression, variables, ok, why)
        class(numeric_callable_t), intent(inout) :: self
        type(expr_t),              intent(in)    :: expression
        type(expr_t),              intent(in)    :: variables(:)
        logical,                   intent(out)   :: ok
        character(:), allocatable, intent(out)   :: why
        integer :: i, j

        self%ready = .false.
        if (allocated(self%variables)) deallocate(self%variables)
        ok = .false.
        why = ""
        if (.not. is_valid(expression)) then
            why = "numeric callable expression is invalid"
            return
        end if
        do i = 1, size(variables)
            if (.not. is_valid(variables(i))) then
                why = "numeric callable variable is invalid"
                return
            end if
            if (variables(i)%kind() /= NK_SYM) then
                why = "numeric callable variables must be symbols"
                return
            end if
            if (.not. same_arena(expression, variables(i))) then
                why = "numeric callable expression and variables use different arenas"
                return
            end if
            do j = 1, i - 1
                if (variables(i) == variables(j)) then
                    why = "numeric callable variable list contains a duplicate"
                    return
                end if
            end do
        end do

        self%expression = expression
        allocate (self%variables(size(variables)))
        self%variables = variables
        self%ready = .true.
        ok = .true.
    end subroutine numeric_callable_initialise

    subroutine numeric_callable_evaluate(self, point, value, ok, why)
        class(numeric_callable_t), intent(in) :: self
        real(dp),                 intent(in) :: point(:)
        real(dp),                 intent(out) :: value
        logical,                  intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: point_expression

        value = 0.0_dp
        call callable_substitute(self, point, point_expression, ok, why)
        if (.not. ok) return
        call numeric_value(point_expression, value, ok, why)
    end subroutine numeric_callable_evaluate

    subroutine numeric_callable_evaluate_text(self, point, digits, text, ok, why)
        class(numeric_callable_t), intent(in) :: self
        real(dp),                 intent(in) :: point(:)
        integer,                  intent(in) :: digits
        character(:), allocatable, intent(out) :: text
        logical,                  intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: point_expression

        text = ""
        call callable_substitute(self, point, point_expression, ok, why)
        if (.not. ok) return
        call numeric_precision_text(point_expression, digits, text, ok, why)
    end subroutine numeric_callable_evaluate_text

    subroutine callable_substitute(self, point, expression, ok, why)
        class(numeric_callable_t), intent(in) :: self
        real(dp),                 intent(in) :: point(:)
        type(expr_t),             intent(out) :: expression
        logical,                  intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), allocatable :: replacements(:)
        integer :: i

        expression = expr_t()
        ok = .false.
        why = ""
        if (.not. self%ready) then
            why = "numeric callable is not initialised"
            return
        end if
        if (size(point) /= size(self%variables)) then
            why = "numeric callable point has the wrong dimension"
            return
        end if
        do i = 1, size(point)
            if (point(i) /= point(i) .or. &
                abs(point(i)) > huge(1.0_dp)/2.0_dp) then
                why = "numeric callable point contains a non-finite value"
                return
            end if
        end do

        allocate (replacements(size(point)))
        do i = 1, size(point)
            replacements(i) = real_expr(self%expression%a, point(i))
        end do
        expression = subs_many(self%expression, self%variables, replacements)
        if (.not. is_valid(expression)) then
            why = "numeric callable substitution failed"
            return
        end if
        ok = .true.
    end subroutine callable_substitute

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
