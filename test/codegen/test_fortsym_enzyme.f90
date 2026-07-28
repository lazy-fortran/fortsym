program test_fortsym_enzyme
    use fortsym_string, only: str, chars
    use fortsym_enzyme, only: enzyme_fixed_array_map_wrapper_spec_t, &
        enzyme_fixed_array_wrapper_spec_t, &
        enzyme_scalar_vector_wrapper_spec_t, enzyme_scalar_wrapper_spec_t, &
        emit_enzyme_fixed_array_map_wrapper, emit_enzyme_fixed_array_wrapper, &
        emit_enzyme_scalar_vector_wrapper, &
        emit_enzyme_scalar_wrapper
    implicit none

    type(enzyme_scalar_wrapper_spec_t) :: spec
    type(enzyme_scalar_vector_wrapper_spec_t) :: vector_spec
    type(enzyme_fixed_array_wrapper_spec_t) :: array_spec
    type(enzyme_fixed_array_map_wrapper_spec_t) :: map_spec
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
    call check(index(source, &
        "function scalar_one_vjp_scalar(x1, cotangent) result(cotangent1)") &
        > 0, "one-input scalar-return VJP")
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
    spec%custom_forward_counter_symbol = str("test_rule_counter_record")
    source = chars(emit_enzyme_scalar_wrapper(spec))
    call check(index(source, "real(c_double) :: values(4)") > 0, &
        "four-input reverse result")
    call check(index(source, &
        "function test_custom_four_forward(x1, tangent1, x2, tangent2, " // &
        "x3, tangent3, x4, tangent4)") > 0, "custom forward rule")
    call check(index(source, &
        "pair%tangent = analytical_jvp(x1, tangent1, x2, tangent2, " // &
        "x3, tangent3, x4, tangent4)") > 0, "analytical JVP hook")
    call check(index(source, "call record_custom_rule()") > 0, &
        "shared custom-rule counter hook")
    call compile_source(source, "four")

    spec%module_name = str("generated_scalar_five")
    spec%wrapper_prefix = str("scalar_five")
    spec%primal_symbol = str("test_primal_five")
    spec%active_inputs = 5
    spec%analytical_jvp_symbol = str("")
    spec%custom_forward_symbol = str("")
    spec%custom_forward_counter_symbol = str("")
    source = chars(emit_enzyme_scalar_wrapper(spec))
    call check(index(source, "real(c_double) :: values(5)") > 0, &
        "five-input reverse result")
    call check(index(source, &
        "function scalar_five_jvp(x1, tangent1, x2, tangent2, " // &
        "x3, tangent3, x4, tangent4, x5, tangent5)") > 0, &
        "five-input JVP")
    call check(index(source, &
        "subroutine scalar_five_vjp(x1, x2, x3, x4, x5, cotangent, " // &
        "cotangent1, cotangent2, cotangent3, cotangent4, cotangent5)") > 0, &
        "five-input VJP")
    call compile_source(source, "five")

    vector_spec%module_name = str("generated_scalar_vector")
    vector_spec%wrapper_prefix = str("scalar_vector")
    vector_spec%primal_symbol = str("test_scalar_vector_primal")
    vector_spec%vector_size = 4
    vector_spec%generator = str("test_fortsym_enzyme")
    vector_spec%generator_revision = str("test-revision")
    vector_spec%regenerate_command = str("fo test test_fortsym_enzyme")
    source = chars(emit_enzyme_scalar_vector_wrapper(vector_spec))
    call check(index(source, &
        "function scalar_vector_jvp(x, values, tangent, dvalues)") > 0, &
        "scalar-vector JVP")
    call check(index(source, &
        "subroutine scalar_vector_vjp(x, values, cotangent, xbar, values_bar)") &
        > 0, "scalar-vector VJP")
    call check(index(source, &
        "real(c_double), intent(in) :: values(4)") > 0, &
        "scalar-vector fixed extent")
    call compile_source(source, "scalar_vector")

    array_spec%module_name = str("generated_fixed_arrays")
    array_spec%wrapper_prefix = str("fixed_arrays")
    array_spec%primal_symbol = str("test_fixed_array_primal")
    array_spec%array_sizes = [16, 4]
    array_spec%inactive_integer_count = 2
    array_spec%generator = str("test_fortsym_enzyme")
    array_spec%generator_revision = str("test-revision")
    array_spec%regenerate_command = str("fo test test_fortsym_enzyme")
    source = chars(emit_enzyme_fixed_array_wrapper(array_spec))
    call check(index(source, &
        "function fixed_arrays_jvp(x1, tangent1, x2, tangent2, selector1, selector2)") > 0, &
        "fixed-array JVP")
    call check(index(source, &
        "function fixed_arrays_vjp(x1, x2, cotangent, bar1, bar2, selector1, selector2) result(value)") &
        > 0, "fixed-array VJP")
    call check(index(source, &
        "real(c_double), intent(in) :: x1(16)") > 0, &
        "fixed-array first extent")
    call check(index(source, "integer(c_int), value :: selector2") > 0, &
        "fixed-array inactive selectors")
    call compile_source(source, "fixed_arrays")

    map_spec%module_name = str("generated_fixed_array_map")
    map_spec%wrapper_prefix = str("fixed_array_map")
    map_spec%primal_symbol = str("test_fixed_array_map")
    map_spec%input_size = 16
    map_spec%output_size = 10
    map_spec%generator = str("test_fortsym_enzyme")
    map_spec%generator_revision = str("test-revision")
    map_spec%regenerate_command = str("fo test test_fortsym_enzyme")
    source = chars(emit_enzyme_fixed_array_map_wrapper(map_spec))
    call check(index(source, &
        "subroutine fixed_array_map_jvp(x, tangent, y, product)") > 0, &
        "fixed-array map JVP")
    call check(index(source, &
        "real(c_double), intent(in) :: x(16), tangent(16)") > 0, &
        "fixed-array map input extent")
    call check(index(source, &
        "real(c_double), intent(out) :: y(10), product(10)") > 0, &
        "fixed-array map output extent")
    call compile_source(source, "fixed_array_map")

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
