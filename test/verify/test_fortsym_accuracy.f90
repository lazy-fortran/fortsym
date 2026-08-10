module test_fortsym_accuracy_support
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    private

    public :: one_ulp_kernel

contains

    subroutine one_ulp_kernel(input, output, kernel_ok, kernel_why)
        integer, parameter :: dp = real64
        real(dp), intent(in) :: input(:)
        real(dp), intent(out) :: output
        logical, intent(out) :: kernel_ok
        character(:), allocatable, intent(out) :: kernel_why

        output = input(1) + spacing(input(1))
        kernel_ok = .true.
        kernel_why = ""
    end subroutine one_ulp_kernel

end module test_fortsym_accuracy_support

program test_fortsym_accuracy
    ! The expected result here is independent of fortsym: the test kernel adds
    ! exactly one binary64 spacing to x, so x and the one-ulp successor are
    ! known without consulting the instrument's implementation.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_accuracy, only: accuracy_spec_t, accuracy_report_t, &
        measure_accuracy
    use test_fortsym_accuracy_support, only: one_ulp_kernel
    use fortsym_expr, only: expr_t, sym
    implicit none

    integer, parameter :: dp = real64
    type(arena_t), target :: arena
    type(expr_t) :: expression, variables(1)
    type(accuracy_spec_t) :: spec
    type(accuracy_report_t) :: report
    real(dp) :: samples(1, 2)
    character(:), allocatable :: why
    logical :: ok
    integer :: nfail

    nfail = 0
    call arena%init()
    expression = sym(arena, "x")
    variables(1) = expression
    samples = reshape([1.0_dp, 2.0_dp], [1, 2])
    spec%reference_digits = 80
    spec%domain = "x in {1, 2}"
    spec%sequence = "ascending declaration order"

    call measure_accuracy(expression, variables, samples, one_ulp_kernel, &
        spec, report, ok, why)
    call check("experiment has a usable reference", ok)
    call check("both declared points were measured", &
        report%sample_count == 2 .and. report%valid_count == 2 .and. &
        report%skipped_count == 0)
    call check("maximum error is one ulp", &
        abs(report%max_ulp - 1.0_dp) < 1.0e-12_dp)
    call check("RMS error is one ulp", &
        abs(report%rms_ulp - 1.0_dp) < 1.0e-12_dp)
    call check("maximum input is retained", report%max_input(1) == 1.0_dp)
    call check("relative condition number is retained", &
        report%condition_available .and. &
        abs(report%condition_number - 1.0_dp) < 1.0e-12_dp)
    call check("domain metadata is retained", report%domain == "x in {1, 2}")
    call check("sequence metadata is retained", &
        report%sequence == "ascending declaration order")

    if (nfail /= 0) then
        print *, "test_fortsym_accuracy: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_accuracy: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (condition) then
            print *, "  ok   ", label
        else
            print *, "  FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine check

end program test_fortsym_accuracy
