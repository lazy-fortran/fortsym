program test_fortsym_wl_f90
    ! Behavioral test for the bounded standalone Wolfram assignment path.
    ! The expected value is derived independently in the generated caller;
    ! this test does not compare the generated source with a golden string.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_wl_f90, only: translate_wl_assignment
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail

    nfail = 0
    call test_compiles_and_agrees(nfail)
    call test_bounded_refusals(nfail)

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_f90"
    else
        print *, "FAIL test_fortsym_wl_f90:", nfail
        error stop 1
    end if

contains

    subroutine test_compiles_and_agrees(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok
        integer :: unit, ios, status
        character(*), parameter :: generated = "/tmp/fortsym_wl_f90_generated.f90"
        character(*), parameter :: driver = "/tmp/fortsym_wl_f90_driver.f90"
        character(*), parameter :: executable = "/tmp/fortsym_wl_f90_driver"

        code = translate_wl_assignment("r = 2*x + Sin[x]", ok, message)
        call check("simple assignment accepted", ok, nfail)
        if (.not. ok) then
            print *, "translation message:", message
            return
        end if

        open (newunit=unit, file=generated, status="replace", action="write", &
            iostat=ios)
        call check("generated source opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") chars(code)
        close (unit)

        open (newunit=unit, file=driver, status="replace", action="write", &
            iostat=ios)
        call check("oracle driver opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") &
            "program independent_oracle"//new_line("a")// &
            "  use, intrinsic :: iso_fortran_env, only: real64"//new_line("a")// &
            "  real(real64) :: x, r"//new_line("a")// &
            "  x = 0.5_real64"//new_line("a")// &
            "  call fortsym_generated_assignment(x, r)"//new_line("a")// &
            "  if (abs(r - (2.0_real64*0.5_real64 + "// &
            "sin(0.5_real64))) > 1.0e-14_real64) error stop 1"// &
            new_line("a")// &
            "  print *, 'PASS independent Fortran oracle'"//new_line("a")// &
            "end program independent_oracle"
        close (unit)

        call execute_command_line("gfortran -std=f2018 -Wall -Werror -o "// &
            executable//" "//generated//" "//driver, exitstat=status)
        call check("generated source compiles", status == 0, nfail)
        if (status /= 0) return
        call execute_command_line(executable, exitstat=status)
        call check("generated source agrees with independent oracle", &
            status == 0, nfail)
    end subroutine test_compiles_and_agrees

    subroutine test_bounded_refusals(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok

        code = translate_wl_assignment("r = x"//char(10)//"q = 1", ok, message)
        call check("multiple assignments refused", .not. ok, nfail)
        call check("multiple assignment explains refusal", &
            index(message, "multiple statements") > 0, nfail)

        code = translate_wl_assignment("r = $x + 1", ok, message)
        call check("non-Fortran symbol refused", .not. ok, nfail)
        call check("non-Fortran symbol explains refusal", &
            index(message, "non-Fortran symbol") > 0, nfail)
    end subroutine test_bounded_refusals

    subroutine check(label, condition, nfail)
        character(*), intent(in)    :: label
        logical,      intent(in)    :: condition
        integer,      intent(inout) :: nfail
        if (.not. condition) then
            print *, "FAIL ", label
            nfail = nfail + 1
        end if
    end subroutine check

end program test_fortsym_wl_f90
