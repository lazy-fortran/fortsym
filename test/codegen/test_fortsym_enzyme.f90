program test_fortsym_enzyme
    use fortsym_string, only: str, chars
    use fortsym_enzyme, only: enzyme_scalar_wrapper_spec_t, &
        emit_enzyme_scalar_wrapper
    implicit none

    type(enzyme_scalar_wrapper_spec_t) :: spec
    character(:), allocatable :: source
    integer :: failures

    failures = 0
    spec%module_name = str("generated_scalar_one")
    spec%wrapper_prefix = str("scalar_one")
    spec%primal_symbol = str("test_primal_one")
    spec%generator = str("test_fortsym_enzyme")
    spec%generator_revision = str("test-revision")
    spec%regenerate_command = str("fo test test_fortsym_enzyme")
    source = chars(emit_enzyme_scalar_wrapper(spec))
    call check(index(source, "function scalar_one_jvp(x1, tangent1)") > 0, &
        "one-input JVP")
    call check(index(source, &
        "subroutine scalar_one_vjp(x1, cotangent, cotangent1)") > 0, &
        "one-input VJP")
    call check(index(source, "type, bind(c) :: enzyme_gradient_t") == 0, &
        "one-input scalar reverse result")
    call check(index(source, "enzyme_pair_t") == 0, &
        "custom rule omitted by default")
    call compile_source(source, "one")

    spec%module_name = str("generated_scalar_four")
    spec%wrapper_prefix = str("scalar_four")
    spec%primal_symbol = str("test_primal_four")
    spec%active_inputs = 4
    spec%analytical_jvp_symbol = str("test_analytical_four_jvp")
    spec%custom_forward_symbol = str("test_custom_four_forward")
    source = chars(emit_enzyme_scalar_wrapper(spec))
    call check(index(source, "real(c_double) :: values(4)") > 0, &
        "four-input reverse result")
    call check(index(source, &
        "function test_custom_four_forward(x1, tangent1, x2, tangent2, " // &
        "x3, tangent3, x4, tangent4)") > 0, "custom forward rule")
    call check(index(source, &
        "pair%tangent = analytical_jvp(x1, tangent1, x2, tangent2, " // &
        "x3, tangent3, x4, tangent4)") > 0, "analytical JVP hook")
    call compile_source(source, "four")

    if (failures > 0) error stop "fortsym Enzyme wrapper tests failed"

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        if (.not. condition) then
            failures = failures + 1
            print *, "FAIL ", label
        end if
    end subroutine check

    subroutine compile_source(code, label)
        character(*), intent(in) :: code, label
        character(:), allocatable :: command, path
        integer :: unit, ios, status

        path = "/tmp/fortsym_enzyme_"//label//".f90"
        open (newunit=unit, file=path, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            call check(.false., label//" source opens")
            return
        end if
        write (unit, "(a)") code
        close (unit)
        command = "gfortran -c -J /tmp -o /tmp/fortsym_enzyme_"//label// &
            ".o "//path//" > /tmp/fortsym_enzyme_"//label//".log 2>&1"
        call execute_command_line(command, wait=.true., exitstat=status)
        call check(status == 0, label//" generated wrapper compiles")
    end subroutine compile_source

end program test_fortsym_enzyme
