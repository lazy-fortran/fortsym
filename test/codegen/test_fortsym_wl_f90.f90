program test_fortsym_wl_f90
    ! Behavioral test for the bounded standalone Wolfram assignment stream.
    ! The expected value is derived independently in the generated caller;
    ! this test does not compare the generated source with a golden string.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_wl_f90, only: translate_wl_assignments
    implicit none

    integer :: nfail

    nfail = 0
    call test_stream_compiles_and_agrees(nfail)
    call test_comment_preamble_compiles_and_agrees(nfail)
    call test_safe_setup_compiles_and_agrees(nfail)
    call test_scalar_minmax_compiles_and_agrees(nfail)
    call test_bounded_refusals(nfail)

    if (nfail == 0) then
        print *, "PASS test_fortsym_wl_f90"
    else
        print *, "FAIL test_fortsym_wl_f90:", nfail
        error stop 1
    end if

contains

    subroutine test_stream_compiles_and_agrees(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok
        integer :: unit, ios, status
        character(*), parameter :: generated = "/tmp/fortsym_wl_f90_generated.f90"
        character(*), parameter :: driver = "/tmp/fortsym_wl_f90_driver.f90"
        character(*), parameter :: executable = "/tmp/fortsym_wl_f90_driver"

        code = translate_wl_assignments( &
            "a = 2*x + Sin[x]"//new_line("a")// &
            "b = a^2 - 3; c := b + a/2", ok, message)
        call check("sequential assignment stream accepted", ok, nfail)
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
            "  real(real64) :: x, a, b, c, expected_a, expected_b, expected_c"// &
            new_line("a")// &
            "  x = 0.5_real64"//new_line("a")// &
            "  expected_a = 2.0_real64*x + sin(x)"//new_line("a")// &
            "  expected_b = expected_a**2 - 3.0_real64"//new_line("a")// &
            "  expected_c = expected_b + expected_a/2.0_real64"// &
            new_line("a")// &
            "  call fortsym_generated_assignment(x, a, b, c)"//new_line("a")// &
            "  if (abs(a - expected_a) > 1.0e-14_real64) error stop 1"// &
            new_line("a")// &
            "  if (abs(b - expected_b) > 1.0e-14_real64) error stop 2"// &
            new_line("a")// &
            "  if (abs(c - expected_c) > 1.0e-14_real64) error stop 3"// &
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
    end subroutine test_stream_compiles_and_agrees

    subroutine test_comment_preamble_compiles_and_agrees(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok
        integer :: unit, ios, status
        character(*), parameter :: generated = &
            "/tmp/fortsym_wl_f90_comment_preamble.f90"
        character(*), parameter :: driver = &
            "/tmp/fortsym_wl_f90_comment_preamble_driver.f90"
        character(*), parameter :: executable = &
            "/tmp/fortsym_wl_f90_comment_preamble_driver"

        code = translate_wl_assignments( &
            "(* generated preamble: no executable semantics *)"//new_line("a")// &
            "result = x + 1", ok, message)
        call check("leading comment preamble accepted", ok, nfail)
        if (.not. ok) then
            print *, "translation message:", message
            return
        end if

        open (newunit=unit, file=generated, status="replace", action="write", &
            iostat=ios)
        call check("comment-preamble source opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") chars(code)
        close (unit)

        open (newunit=unit, file=driver, status="replace", action="write", &
            iostat=ios)
        call check("comment-preamble oracle driver opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") &
            "program independent_comment_preamble_oracle"//new_line("a")// &
            "  use, intrinsic :: iso_fortran_env, only: real64"//new_line("a")// &
            "  real(real64) :: x, result"//new_line("a")// &
            "  x = 2.25_real64"//new_line("a")// &
            "  call fortsym_generated_assignment(x, result)"//new_line("a")// &
            "  if (abs(result - (x + 1.0_real64)) > 1.0e-14_real64) "// &
            "error stop 1"//new_line("a")// &
            "  print *, 'PASS independent comment-preamble oracle'"// &
            new_line("a")// &
            "end program independent_comment_preamble_oracle"
        close (unit)

        call execute_command_line("gfortran -std=f2018 -Wall -Werror -o "// &
            executable//" "//generated//" "//driver, exitstat=status)
        call check("comment-preamble source compiles", status == 0, nfail)
        if (status /= 0) return
        call execute_command_line(executable, exitstat=status)
        call check("comment-preamble source agrees with independent oracle", &
            status == 0, nfail)
    end subroutine test_comment_preamble_compiles_and_agrees

    subroutine test_safe_setup_compiles_and_agrees(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok
        integer :: unit, ios, status
        character(*), parameter :: generated = &
            "/tmp/fortsym_wl_f90_safe_setup.f90"
        character(*), parameter :: driver = &
            "/tmp/fortsym_wl_f90_safe_setup_driver.f90"
        character(*), parameter :: executable = &
            "/tmp/fortsym_wl_f90_safe_setup_driver"

        code = translate_wl_assignments( &
            "(* notebook setup *)"//new_line("a")// &
            "ClearAll[""Global`*""]; Clear[x, y]; ClearAll[result];"// &
            new_line("a")// &
            "result = x*y + 1", ok, message)
        call check("safe leading setup accepted", ok, nfail)
        if (.not. ok) then
            print *, "translation message:", message
            return
        end if

        open (newunit=unit, file=generated, status="replace", action="write", &
            iostat=ios)
        call check("safe setup source opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") chars(code)
        close (unit)

        open (newunit=unit, file=driver, status="replace", action="write", &
            iostat=ios)
        call check("safe setup oracle driver opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") &
            "program independent_safe_setup_oracle"//new_line("a")// &
            "  use, intrinsic :: iso_fortran_env, only: real64"//new_line("a")// &
            "  real(real64) :: x, y, result, expected"//new_line("a")// &
            "  x = 2.5_real64"//new_line("a")// &
            "  y = -4.0_real64"//new_line("a")// &
            "  expected = x*y + 1.0_real64"//new_line("a")// &
            "  call fortsym_generated_assignment(x, y, result)"//new_line("a")// &
            "  if (abs(result - expected) > 1.0e-14_real64) error stop 1"// &
            new_line("a")// &
            "  print *, 'PASS independent safe-setup oracle'"//new_line("a")// &
            "end program independent_safe_setup_oracle"
        close (unit)

        call execute_command_line("gfortran -std=f2018 -Wall -Werror -o "// &
            executable//" "//generated//" "//driver, exitstat=status)
        call check("safe setup source compiles", status == 0, nfail)
        if (status /= 0) return
        call execute_command_line(executable, exitstat=status)
        call check("safe setup source agrees with independent oracle", &
            status == 0, nfail)
    end subroutine test_safe_setup_compiles_and_agrees

    subroutine test_scalar_minmax_compiles_and_agrees(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok
        integer :: unit, ios, status
        character(*), parameter :: generated = "/tmp/fortsym_wl_f90_minmax.f90"
        character(*), parameter :: driver = "/tmp/fortsym_wl_f90_minmax_driver.f90"
        character(*), parameter :: executable = "/tmp/fortsym_wl_f90_minmax_driver"

        code = translate_wl_assignments( &
            "lo = Min[x, y, z]"//new_line("a")// &
            "hi = Max[x, y, z]", ok, message)
        call check("scalar Min/Max stream accepted", ok, nfail)
        if (.not. ok) then
            print *, "translation message:", message
            return
        end if

        open (newunit=unit, file=generated, status="replace", action="write", &
            iostat=ios)
        call check("Min/Max generated source opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") chars(code)
        close (unit)

        open (newunit=unit, file=driver, status="replace", action="write", &
            iostat=ios)
        call check("Min/Max oracle driver opens", ios == 0, nfail)
        if (ios /= 0) return
        write (unit, "(a)") &
            "program independent_minmax_oracle"//new_line("a")// &
            "  use, intrinsic :: iso_fortran_env, only: real64"//new_line("a")// &
            "  real(real64) :: x, y, z, lo, hi"//new_line("a")// &
            "  x = 3.5_real64"//new_line("a")// &
            "  y = -2.0_real64"//new_line("a")// &
            "  z = 1.25_real64"//new_line("a")// &
            "  call fortsym_generated_assignment(x, y, z, lo, hi)"//new_line("a")// &
            "  if (abs(lo - (-2.0_real64)) > 1.0e-14_real64) error stop 1"// &
            new_line("a")// &
            "  if (abs(hi - 3.5_real64) > 1.0e-14_real64) error stop 2"// &
            new_line("a")// &
            "  print *, 'PASS independent Min/Max oracle'"//new_line("a")// &
            "end program independent_minmax_oracle"
        close (unit)

        call execute_command_line("gfortran -std=f2018 -Wall -Werror -o "// &
            executable//" "//generated//" "//driver, exitstat=status)
        call check("Min/Max generated source compiles", status == 0, nfail)
        if (status /= 0) return
        call execute_command_line(executable, exitstat=status)
        call check("Min/Max generated source agrees with independent oracle", &
            status == 0, nfail)
    end subroutine test_scalar_minmax_compiles_and_agrees

    subroutine test_bounded_refusals(nfail)
        integer, intent(inout) :: nfail
        type(str_t) :: code
        character(:), allocatable :: message
        logical :: ok

        code = translate_wl_assignments( &
            "If[x > 0, a = x, a = -x]", ok, message)
        call check("control flow statement refused", .not. ok, nfail)

        code = translate_wl_assignments("r = $x + 1", ok, message)
        call check("non-Fortran symbol refused", .not. ok, nfail)
        call check("non-Fortran symbol explains refusal", &
            index(message, "non-Fortran symbol") > 0, nfail)

        code = translate_wl_assignments("r = If[x > 0, x, -x]", ok, message)
        call check("unsupported expression refused", .not. ok, nfail)

        code = translate_wl_assignments("r = {x, 1}", ok, message)
        call check("list-valued expression refused", .not. ok, nfail)

        code = translate_wl_assignments("r = I", ok, message)
        call check("complex constant refused by real emitter", .not. ok, nfail)

        code = translate_wl_assignments("r = Min[x]", ok, message)
        call check("unary Min refused", .not. ok, nfail)

        code = translate_wl_assignments("r = Max[x]", ok, message)
        call check("unary Max refused", .not. ok, nfail)

        code = translate_wl_assignments( &
            "Set[x, 1]; result = x + 1", ok, message)
        call check("value-setting setup refused", .not. ok, nfail)

        code = translate_wl_assignments( &
            "Needs[""Some`Package`""]; result = x + 1", ok, message)
        call check("import setup refused", .not. ok, nfail)

        code = translate_wl_assignments( &
            "Clear[Sin]; result = x + 1", ok, message)
        call check("builtin-clearing setup refused", .not. ok, nfail)

        code = translate_wl_assignments( &
            "clear[x]; result = x + 1", ok, message)
        call check("case-mismatched setup refused", .not. ok, nfail)

        code = translate_wl_assignments("(* unclosed preamble", ok, message)
        call check("unclosed comment preamble refused", .not. ok, nfail)
        call check("unclosed comment explains refusal", &
            index(message, "unclosed Wolfram comment") > 0, nfail)

        code = translate_wl_assignments("r[1] = x", ok, message)
        call check("non-scalar target refused", .not. ok, nfail)

        code = translate_wl_assignments( &
            "r = x"//new_line("a")//"r = r + 1", ok, message)
        call check("reassignment refused", .not. ok, nfail)
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
