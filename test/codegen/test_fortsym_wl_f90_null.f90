program test_fortsym_wl_f90_null
    ! Behavioral test for standalone Wolfram Null statements.
    ! The expected values are computed independently in the generated caller;
    ! this test does not compare generated source text with a golden file.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_wl_f90, only: translate_wl_assignments
    implicit none

    integer :: nfail

    nfail = 0
    call test_null_separators_compile_and_agree(nfail)
    call test_nonstandalone_null_is_refused(nfail)

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_f90_null"
    else
        print *, "FAIL test_fortsym_wl_f90_null:", nfail
        error stop 1
    end if

contains

    subroutine test_null_separators_compile_and_agree(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok
        integer :: unit, ios, status
        character(*), parameter :: generated = &
            "/tmp/fortsym_wl_f90_null_generated.f90"
        character(*), parameter :: driver = &
            "/tmp/fortsym_wl_f90_null_driver.f90"
        character(*), parameter :: executable = &
            "/tmp/fortsym_wl_f90_null_driver"

        code = translate_wl_assignments( &
            "Null; result = x + 1; Null; squared = result^2; Null", ok, message)
        call check("standalone Null separators accepted", ok, nfail)
        if (.not. ok) then
            print *, "translation message:", message
            return
        end if

        open (newunit=unit, file=generated, status="replace", action="write", &
            iostat=ios)
        call check("Null generated source opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") chars(code)
        close (unit)

        open (newunit=unit, file=driver, status="replace", action="write", &
            iostat=ios)
        call check("Null oracle driver opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") &
            "program independent_null_oracle"//new_line("a")// &
            "  use, intrinsic :: iso_fortran_env, only: real64"//new_line("a")// &
            "  real(real64) :: x, result, squared, expected_result"// &
            new_line("a")// &
            "  x = 2.25_real64"//new_line("a")// &
            "  expected_result = x + 1.0_real64"//new_line("a")// &
            "  call fortsym_generated_assignment(x, result, squared)"// &
            new_line("a")// &
            "  if (abs(result - expected_result) > 1.0e-14_real64) "// &
            "error stop 1"//new_line("a")// &
            "  if (abs(squared - expected_result**2) > 1.0e-14_real64) "// &
            "error stop 2"//new_line("a")// &
            "  print *, 'PASS independent Null oracle'"//new_line("a")// &
            "end program independent_null_oracle"
        close (unit)

        call execute_command_line("gfortran -std=f2018 -Wall -Werror -o "// &
            executable//" "//generated//" "//driver, exitstat=status)
        call check("Null generated source compiles", status == 0, nfail)
        if (status /= 0) return
        call execute_command_line(executable, exitstat=status)
        call check("Null source agrees with independent oracle", &
            status == 0, nfail)
    end subroutine test_null_separators_compile_and_agree

    subroutine test_nonstandalone_null_is_refused(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok

        code = translate_wl_assignments("result = Null", ok, message)
        call check("Null used as a value remains refused", .not. ok, nfail)
        call check("Null value refusal has a diagnostic", len(message) > 0, nfail)

        code = translate_wl_assignments("Null", ok, message)
        call check("a stream containing only Null remains refused", .not. ok, nfail)
        call check("empty-after-Null refusal has a diagnostic", len(message) > 0, nfail)
    end subroutine test_nonstandalone_null_is_refused

    subroutine check(label, condition, nfail)
        character(*), intent(in)    :: label
        logical,      intent(in)    :: condition
        integer,      intent(inout) :: nfail
        if (.not. condition) then
            print *, "FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine check

end program test_fortsym_wl_f90_null
