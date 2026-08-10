module fortsym_accuracy
    ! Accuracy measurements for compiled real64 kernels.
    !
    ! The caller owns the executable kernel and supplies its result through a
    ! procedure callback.  FortSym independently substitutes the declared
    ! binary64 sample point into the symbolic expression, evaluates that
    ! expression through SymEngine's MPFR path, and compares the callback's
    ! result to the retained decimal reference.  A sample matrix and its
    ! domain/sequence labels are therefore part of the report rather than
    ! hidden state or an implicit random stream.
    use, intrinsic :: iso_c_binding, only: c_char, c_double, c_int, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortsym_arena, only: NK_SYM
    use fortsym_capi, only: fsym_mpfr_ulp_error
    use fortsym_diff, only: diff
    use fortsym_engine_symengine, only: symengine_evalf_text
    use fortsym_eval, only: collect_free_symbols
    use fortsym_expr, only: expr_t, is_valid, real_expr
    use fortsym_numeric, only: numeric_value
    use fortsym_string, only: str_t, chars
    use fortsym_subs, only: subs_many
    implicit none
    private

    public :: accuracy_spec_t, accuracy_report_t, kernel_evaluator
    public :: measure_accuracy

    integer, parameter :: dp = real64
    integer, parameter :: MAX_REFERENCE_DIGITS = 512

    !> A deterministic accuracy experiment. `samples` passed to
    !> measure_accuracy is ordered by the caller; `domain` and `sequence`
    !> record what that order means to a consumer.
    type :: accuracy_spec_t
        integer :: reference_digits = 80
        character(:), allocatable :: domain
        character(:), allocatable :: sequence
    end type accuracy_spec_t

    !> Summary of one deterministic experiment. The input stored in
    !> max_input is the column of the declared sample matrix at which max_ulp
    !> was observed.
    type :: accuracy_report_t
        integer :: reference_digits = 0
        integer :: sample_count = 0
        integer :: valid_count = 0
        integer :: skipped_count = 0
        real(dp) :: max_ulp = 0.0_dp
        real(dp) :: rms_ulp = 0.0_dp
        real(dp) :: condition_number = 0.0_dp
        real(dp) :: max_reference = 0.0_dp
        real(dp) :: max_observed = 0.0_dp
        logical :: condition_available = .false.
        real(dp), allocatable :: max_input(:)
        character(:), allocatable :: domain
        character(:), allocatable :: sequence
    end type accuracy_report_t

    !> Signature of a compiled kernel adapter. `input` is one column of the
    !> declared sample matrix. A false `ok` refuses the point without treating
    !> a kernel domain failure as a numerical result.
    abstract interface
        subroutine kernel_evaluator(input, output, ok, why)
            import :: dp
            real(dp), intent(in) :: input(:)
            real(dp), intent(out) :: output
            logical, intent(out) :: ok
            character(:), allocatable, intent(out) :: why
        end subroutine kernel_evaluator
    end interface

contains

    !> Measure a compiled kernel against an MPFR reference at every declared
    !> point. `samples` has shape (size(variables), number_of_points). The
    !> relative condition number, when available, is
    !>   sum_i |x_i * df/dx_i| / |f|.
    subroutine measure_accuracy(expression, variables, samples, kernel, spec, &
            report, ok, why)
        type(expr_t), intent(in) :: expression
        type(expr_t), intent(in) :: variables(:)
        real(dp), intent(in) :: samples(:,:)
        procedure(kernel_evaluator) :: kernel
        type(accuracy_spec_t), intent(in) :: spec
        type(accuracy_report_t), intent(out) :: report
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        type(expr_t), allocatable :: derivatives(:), replacements(:)
        type(expr_t) :: point_expression, point_derivative
        type(str_t), allocatable :: names(:)
        character(:), allocatable :: reference_text, local_why, last_why
        real(dp) :: observed, reference_dp, ulp_error, sum_squares
        real(dp) :: condition
        integer :: i, j, ios, nvariables, npoints
        logical :: reference_ok, kernel_ok, condition_ok, read_ok

        call initialise_report(report, spec, size(variables), size(samples, 2))
        ok = .false.
        why = ""

        if (.not. is_valid(expression)) then
            why = "accuracy expression is invalid"
            return
        end if
        nvariables = size(variables)
        npoints = size(samples, 2)
        if (size(samples, 1) /= nvariables) then
            why = "sample rows must match the number of variables"
            return
        end if
        if (npoints == 0) then
            why = "accuracy experiment has no declared sample points"
            return
        end if
        if (spec%reference_digits < 32 .or. &
                spec%reference_digits > MAX_REFERENCE_DIGITS) then
            why = "reference_digits must be between 32 and 512 decimal digits"
            return
        end if

        do i = 1, nvariables
            if (.not. is_valid(variables(i))) then
                why = "accuracy variable is invalid"
                return
            end if
            if (variables(i)%kind() /= NK_SYM) then
                why = "accuracy variables must be symbols"
                return
            end if
            if (.not. associated(variables(i)%a, expression%a)) then
                why = "accuracy variables and expression use different arenas"
                return
            end if
            do j = 1, i - 1
                if (variables(i)%id == variables(j)%id) then
                    why = "accuracy variable list contains a duplicate symbol"
                    return
                end if
            end do
        end do

        allocate (replacements(nvariables))
        allocate (derivatives(nvariables))
        do i = 1, nvariables
            derivatives(i) = diff(expression, variables(i))
        end do

        sum_squares = 0.0_dp
        last_why = "all declared samples were refused"
        do i = 1, npoints
            do j = 1, nvariables
                replacements(j) = real_expr(expression%a, samples(j, i))
            end do
            point_expression = subs_many(expression, variables, replacements)
            call collect_free_symbols(point_expression, names)
            if (size(names) /= 0) then
                last_why = "sample leaves free symbol "//chars(names(1))
                report%skipped_count = report%skipped_count + 1
                cycle
            end if

            reference_text = symengine_evalf_text(point_expression, &
                spec%reference_digits, reference_ok, local_why)
            if (.not. reference_ok) then
                last_why = "high-precision reference refused: "//local_why
                report%skipped_count = report%skipped_count + 1
                cycle
            end if

            call kernel(samples(:, i), observed, kernel_ok, local_why)
            if (.not. kernel_ok) then
                last_why = "kernel refused sample: "//local_why
                report%skipped_count = report%skipped_count + 1
                cycle
            end if
            if (.not. ieee_is_finite(observed)) then
                last_why = "kernel returned a non-finite value"
                report%skipped_count = report%skipped_count + 1
                cycle
            end if

            ulp_error = 0.0_dp
            if (fsym_mpfr_ulp_error(c_string(reference_text), observed, &
                    ulp_error) == 0_c_int) then
                last_why = "MPFR could not form a finite ulp error"
                report%skipped_count = report%skipped_count + 1
                cycle
            end if

            report%valid_count = report%valid_count + 1
            sum_squares = sum_squares + ulp_error*ulp_error
            read_ok = .false.
            reference_dp = 0.0_dp
            read (reference_text, *, iostat=ios) reference_dp
            if (ios == 0) read_ok = ieee_is_finite(reference_dp)

            condition = 0.0_dp
            condition_ok = .false.
            if (read_ok) then
                call condition_at(derivatives, variables, replacements, &
                    reference_dp, condition, condition_ok)
            end if

            if (report%valid_count == 1 .or. ulp_error > report%max_ulp) then
                report%max_ulp = ulp_error
                report%max_input = samples(:, i)
                report%max_reference = reference_dp
                report%max_observed = observed
                report%condition_available = condition_ok
                if (condition_ok) report%condition_number = condition
            end if
        end do

        if (report%valid_count > 0) then
            report%rms_ulp = sqrt(sum_squares/real(report%valid_count, dp))
            ok = .true.
            why = ""
        else
            why = last_why
        end if
    end subroutine measure_accuracy

    subroutine initialise_report(report, spec, nvariables, npoints)
        type(accuracy_report_t), intent(out) :: report
        type(accuracy_spec_t), intent(in) :: spec
        integer, intent(in) :: nvariables, npoints

        report%reference_digits = spec%reference_digits
        report%sample_count = npoints
        report%valid_count = 0
        report%skipped_count = 0
        report%max_ulp = 0.0_dp
        report%rms_ulp = 0.0_dp
        report%condition_number = 0.0_dp
        report%max_reference = 0.0_dp
        report%max_observed = 0.0_dp
        report%condition_available = .false.
        allocate (report%max_input(nvariables), source=0.0_dp)
        if (allocated(spec%domain)) then
            report%domain = spec%domain
        else
            report%domain = ""
        end if
        if (allocated(spec%sequence)) then
            report%sequence = spec%sequence
        else
            report%sequence = ""
        end if
    end subroutine initialise_report

    subroutine condition_at(derivatives, variables, replacements, reference, &
            condition, ok)
        type(expr_t), intent(in) :: derivatives(:), variables(:), replacements(:)
        real(dp), intent(in) :: reference
        real(dp), intent(out) :: condition
        logical, intent(out) :: ok
        type(expr_t) :: point_derivative
        real(dp) :: derivative_value, numerator
        character(:), allocatable :: local_why
        integer :: i
        logical :: derivative_ok

        condition = 0.0_dp
        ok = .false.
        if (.not. ieee_is_finite(reference)) return
        if (reference == 0.0_dp) return

        do i = 1, size(variables)
            point_derivative = subs_many(derivatives(i), variables, replacements)
            call numeric_value(point_derivative, derivative_value, derivative_ok, &
                local_why)
            if (.not. derivative_ok) return
            numerator = abs(replacements(i)%real_value()*derivative_value)
            condition = condition + numerator
        end do
        condition = condition/abs(reference)
        ok = ieee_is_finite(condition)
    end subroutine condition_at

    pure function c_string(text) result(c)
        character(*), intent(in) :: text
        character(len=len_trim(text) + 1, kind=c_char) :: c

        c = trim(text)//c_null_char
    end function c_string

end module fortsym_accuracy
