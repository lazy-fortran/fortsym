program test_fortsym_kernel
    ! Code generation, checked on what the output must *mean* rather than on
    ! what it looks like.
    !
    ! The strongest check here compiles the generated kernel and compares it
    ! numerically against the same expression evaluated another way. A
    ! golden-string test would pin the current formatting and catch nothing
    ! about correctness; this catches a lost sign, a wrong precedence or a
    ! mis-ordered CSE temporary. Golden strings are used only where formatting
    ! itself is the requirement.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_parse, only: parse_expr
    use fortsym_products, only: jvp, vjp
    use fortsym_kernel
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_cse_finds_sharing()
    call test_cse_skips_atoms()
    call test_operation_count_uses_shared_dag()
    call test_projected_exact_operation_count()
    call test_unrepresentable_exact_is_refused()
    call test_temporaries_are_declared()
    call test_default_temporary_prefix()
    call test_explicit_regeneration_command()
    call test_generator_revision()
    call test_module_wrapper()
    call test_intrinsic_integer_argument_is_real()
    call test_complex_kernel()
    call test_array_shaped_arguments()
    call test_rank_two_array_arguments()
    call test_multi_element_array_output()
    call test_pure_elemental_procedure()
    call test_device_leaf_annotations()
    call test_product_codegen_scales_linearly()
    call test_simplified_negative_product_emission()
    call test_undeclared_symbol_is_refused()
    call test_fortran_symbol_case_is_ignored()
    call test_pure_procedure()
    call test_ordering_is_topological()
    call test_codegen_is_construction_history_independent()
    call test_line_wrapping()
    call test_long_interface_wrapping()
    call test_snippet_mode()
    call test_generated_kernel_compiles_and_agrees()

    if (nfail == 0) then
        print *, "test_fortsym_kernel: all checks passed"
    else
        print *, "test_fortsym_kernel: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    function parsed(a, text) result(e)
        type(arena_t), target, intent(inout) :: a
        character(*),          intent(in)    :: text
        type(expr_t)                         :: e
        character(:), allocatable :: message
        logical :: good
        e = parse_expr(a, text, good, message)
        if (.not. good) then
            nfail = nfail + 1
            print *, "PARSE-FAIL ", text, " : ", message
        end if
    end function parsed

    function spec_for(name, args, outs) result(sp)
        character(*), intent(in) :: name, args(:), outs(:)
        type(kernel_spec_t)      :: sp
        integer :: k

        sp%name = str(name)
        sp%mode = KERNEL_SUBROUTINE
        sp%temp_prefix = str("t")
        sp%generator = str("gen_test")
        allocate (sp%args(size(args)), sp%outputs(size(outs)))
        do k = 1, size(args)
            sp%args(k) = str(args(k))
        end do
        do k = 1, size(outs)
            sp%outputs(k) = str(outs(k))
        end do
    end function spec_for

    subroutine test_cse_finds_sharing()
        type(arena_t), target :: a
        type(expr_t) :: shared, roots(1)
        type(cse_result_t) :: res

        call a%init()

        ! sin(x*y) appears three times. Hash-consing already stores it once, so
        ! CSE has only to notice the reference count.
        shared = parsed(a, "sin(x*y)")
        roots(1) = parsed(a, "sin(x*y) + sin(x*y)*sin(x*y)")

        res = cse_analyse(roots, "t")
        call ok("shared subexpression named", res%n >= 1)
        call ok("the shared node is the repeated one", &
            any(res%ids(1:res%n) == shared%id))
    end subroutine test_cse_finds_sharing

    subroutine test_cse_skips_atoms()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(cse_result_t) :: res
        integer :: k
        logical :: named_an_atom

        call a%init()
        ! x appears many times but naming it would cost a line and save nothing.
        roots(1) = parsed(a, "x + x*x + x*x*x")

        res = cse_analyse(roots, "t")

        named_an_atom = .false.
        do k = 1, res%n
            if (a%nargs_of(res%ids(k)) == 0) named_an_atom = .true.
        end do
        call ok("atoms are not given temporaries", .not. named_an_atom)
    end subroutine test_cse_skips_atoms

    subroutine test_codegen_is_construction_history_independent()
        type(arena_t), target :: a, b
        type(expr_t) :: ax, ay, bx, by, shared_a, shared_b
        type(expr_t) :: roots_a(1), roots_b(1)
        character(:), allocatable :: code_a, code_b

        call a%init()
        call b%init()
        ax = sym(a, "x")
        ay = sym(a, "y")
        by = sym(b, "y")
        bx = sym(b, "x")
        shared_a = sin(ax*ay)
        shared_b = sin(by*bx)
        roots_a(1) = shared_a**2 + exp(shared_a) + ax*ay + 1
        roots_b(1) = 1 + by*bx + exp(shared_b) + shared_b**2

        code_a = chars(emit_kernel(roots_a, &
            spec_for("stable_k", ["x", "y"], ["r"])))
        code_b = chars(emit_kernel(roots_b, &
            spec_for("stable_k", ["x", "y"], ["r"])))
        call ok("codegen is byte-identical across construction histories", &
            code_a == code_b)
    end subroutine test_codegen_is_construction_history_independent

    subroutine test_operation_count_uses_shared_dag()
        type(arena_t), target :: a
        type(expr_t) :: roots(2)
        type(operation_count_t) :: counts

        call a%init()
        ! x*y is shared by both roots and must be charged once. The independent
        ! expected work is one multiplication, one addition, and two function
        ! calls: sin(x*y) + exp(x*y).
        roots(1) = parsed(a, "sin(x*y)")
        roots(2) = parsed(a, "sin(x*y) + exp(x*y)")

        counts = count_operations(roots)
        call ok("operation count additions", counts%additions == 1)
        call ok("operation count multiplications", counts%multiplications == 1)
        call ok("operation count functions", counts%functions == 2)
        call ok("operation count total", counts%total == 4)
    end subroutine test_operation_count_uses_shared_dag

    !> A large rational is projected once to a real64 literal, so the generated
    !> arithmetic and its independently counted work both contain no division.
    subroutine test_projected_exact_operation_count()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(operation_count_t) :: counts
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, &
            "18446744073709551617/18446744073709551616")
        counts = count_operations(roots)
        code = chars(emit_kernel(roots, &
            spec_for("exact_k", ["x"], ["r"])))
        call ok("projected exact count has no division", &
            counts%divisions == 0 .and. counts%total == 0)
        call ok("projected exact code is one literal", &
            index(code, "1.0000000000000000E+000_dp") > 0)
    end subroutine test_projected_exact_operation_count

    subroutine test_unrepresentable_exact_is_refused()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code
        logical :: good

        call a%init()
        roots(1) = exact(a, "1"//repeat("0", 400))
        code = chars(emit_kernel(roots, &
            spec_for("too_large_k", ["x"], ["r"]), good))
        call ok("non-finite exact kernel reports refusal", .not. good)
        call ok("non-finite exact kernel emits no partial source", &
            len(code) == 0)
    end subroutine test_unrepresentable_exact_is_refused

    subroutine test_temporaries_are_declared()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "sin(x*y) + sin(x*y)*sin(x*y)")
        code = chars(emit_kernel(roots, spec_for("k", ["x", "y"], ["r"])))

        ! Every temporary must be declared. KiLCA's generated kernels used
        ! implicit real(dp) (s-t) instead, which types anything beginning with s
        ! or t and turns a misspelling into a silent zero.
        call ok("declares temporaries", index(code, "real(dp) :: t1") > 0)
        call ok("no implicit typing", index(code, "implicit real") == 0)
        call ok("has implicit none", index(code, "implicit none") > 0)
        call ok("declares inputs", index(code, "intent(in) :: x, y") > 0)
        call ok("declares output", index(code, "intent(out) :: r") > 0)
        call ok("names its generator", index(code, "gen_test") > 0)
    end subroutine test_temporaries_are_declared

    subroutine test_default_temporary_prefix()
        type(arena_t), target :: a
        type(expr_t) :: x, y, shared, roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")
        shared = exp(x*y)
        roots(1) = shared + shared*shared
        spec = spec_for("k", ["x", "y"], ["r"])
        spec%temp_prefix = str("")
        code = chars(emit_kernel(roots, spec))

        call ok("empty temporary prefix defaults to a Fortran identifier", &
            index(code, "real(dp) :: t1") > 0)
    end subroutine test_default_temporary_prefix

    subroutine test_simplified_negative_product_emission()
        type(arena_t), target :: a
        type(symengine_engine_t) :: engine
        type(engine_result_t) :: simplified
        type(expr_t) :: x, values(1), variables(1), cotangents(1), roots(1)
        character(:), allocatable :: code

        call a%init()
        engine = make_symengine_engine(a)
        x = sym(a, "x")
        values(1) = erfc(x)
        variables(1) = x
        cotangents(1) = sym(a, "u")
        roots = vjp(values, variables, cotangents)
        simplified = engine%simplify(roots(1))
        call ok("erfc VJP simplification succeeds", simplified%ok)
        if (.not. simplified%ok) return
        roots(1) = simplified%value
        code = chars(emit_kernel(roots, spec_for("k", ["x", "u"], ["vjp"])))
        call ok("negative product has no doubled unary sign", &
            index(code, "--") == 0)
        call ok("negative product has no embedded negative factor", &
            index(code, "*-") == 0)
    end subroutine test_simplified_negative_product_emission

    subroutine test_explicit_regeneration_command()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "x*x")
        spec = spec_for("k", ["x"], ["r"])
        spec%regenerate_command = str("fpm run --example gen_k")
        code = chars(emit_kernel(roots, spec))
        call ok("explicit regeneration command", &
            index(code, "Regenerate with: fpm run --example gen_k") > 0)
    end subroutine test_explicit_regeneration_command

    subroutine test_generator_revision()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "x*x")
        spec = spec_for("k", ["x"], ["r"])
        spec%generator_revision = str("fortsym@0123456789abcdef")
        code = chars(emit_kernel(roots, spec))
        call ok("generator revision", &
            index(code, "Generator revision: fortsym@0123456789abcdef") > 0)
    end subroutine test_generator_revision

    subroutine test_module_wrapper()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = parsed(a, "x*x")
        spec = spec_for("k", ["x"], ["r"])
        spec%module_name = str("generated_k")
        code = chars(emit_kernel(roots, spec))
        call ok("module wrapper declaration", &
            index(code, "module generated_k") > 0)
        call ok("module wrapper exports kernel", &
            index(code, "public :: k") > 0)
        call ok("module wrapper contains procedures", &
            index(code, "contains") > 0)
        call ok("module wrapper closes", &
            index(code, "end module generated_k") > 0)

        open (newunit=unit, file="/tmp/fortsym_gen_module.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("module wrapper fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_module"
        write (unit, "(a)") "  use generated_k, only: k"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: r"
        write (unit, "(a)") "  call k(3.0_dp, r)"
        write (unit, "(a)") "  if (abs(r - 9.0_dp) > 1.0e-14_dp) error stop 1"
        write (unit, "(a)") "end program drive_module"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_module "// &
            "/tmp/fortsym_gen_module.f90 > /tmp/fortsym_gen_module.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("module-wrapped kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_gen_module", wait=.true., &
                exitstat=stat)
            call ok("module-wrapped kernel runs", stat == 0)
        end if
    end subroutine test_module_wrapper

    subroutine test_intrinsic_integer_argument_is_real()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = func("max", [num(a, 0), sym(a, "x")])
        spec = spec_for("clamp_zero", ["x"], ["r"])
        spec%module_name = str("generated_clamp_zero")
        code = chars(emit_kernel(roots, spec))

        open (newunit=unit, file="/tmp/fortsym_gen_intrinsic.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("intrinsic fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_intrinsic"
        write (unit, "(a)") "  use generated_clamp_zero, only: clamp_zero"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: r"
        write (unit, "(a)") "  call clamp_zero(-2.0_dp, r)"
        write (unit, "(a)") "  if (r /= 0.0_dp) error stop 1"
        write (unit, "(a)") "  call clamp_zero(3.0_dp, r)"
        write (unit, "(a)") "  if (r /= 3.0_dp) error stop 2"
        write (unit, "(a)") "end program drive_intrinsic"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_intrinsic "// &
            "/tmp/fortsym_gen_intrinsic.f90 "// &
            "> /tmp/fortsym_gen_intrinsic.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("real intrinsic literal kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_gen_intrinsic", &
                wait=.true., exitstat=stat)
            call ok("real intrinsic literal kernel runs", stat == 0)
        end if
    end subroutine test_intrinsic_integer_argument_is_real

    subroutine test_complex_kernel()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = parsed(a, "y/x")
        spec = spec_for("complex_quotient", ["x", "y"], ["value"])
        spec%module_name = str("generated_complex_quotient")
        spec%scalar_type = str("complex(dp)")
        code = chars(emit_kernel(roots, spec))
        call ok("complex inputs are declared", &
            index(code, "complex(dp), intent(in) :: x, y") > 0)
        call ok("complex outputs are declared", &
            index(code, "complex(dp), intent(out) :: value") > 0)

        open (newunit=unit, file="/tmp/fortsym_gen_complex.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("complex kernel fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_complex"
        write (unit, "(a)") "  use generated_complex_quotient"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  complex(dp) :: value"
        write (unit, "(a)") "  call complex_quotient((1.0_dp, 2.0_dp), "// &
            "(3.0_dp, -1.0_dp), value)"
        write (unit, "(a)") "  if (abs(value - (0.2_dp, -1.4_dp)) > "// &
            "1.0e-14_dp) error stop 1"
        write (unit, "(a)") "end program drive_complex"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_complex "// &
            "/tmp/fortsym_gen_complex.f90 > /tmp/fortsym_gen_complex.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("complex generated kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_gen_complex", wait=.true., &
                exitstat=stat)
            call ok("complex generated kernel runs", stat == 0)
        end if
    end subroutine test_complex_kernel

    subroutine test_array_shaped_arguments()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = sym(a, "x(1)")*sym(a, "v(1)")
        spec = spec_for("array_product", ["x", "v"], ["jvp"])
        spec%module_name = str("generated_array_product")
        allocate (spec%arg_shapes(2), spec%output_shapes(1), &
            spec%output_references(1))
        spec%arg_shapes = [str("(1)"), str("(1)")]
        spec%output_shapes = [str("(1)")]
        spec%output_references = [str("jvp(1)")]
        code = chars(emit_kernel(roots, spec))
        call ok("array input declarations", &
            index(code, "intent(in) :: x(1), v(1)") > 0)
        call ok("array output declaration", &
            index(code, "intent(out) :: jvp(1)") > 0)
        call ok("array output reference", &
            index(code, "jvp(1) = v(1)*x(1)") > 0)

        open (newunit=unit, file="/tmp/fortsym_gen_array.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("array kernel fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_array"
        write (unit, "(a)") "  use generated_array_product, only: array_product"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: x(1), v(1), jvp(1)"
        write (unit, "(a)") "  x = 3.0_dp; v = 2.0_dp"
        write (unit, "(a)") "  call array_product(x, v, jvp)"
        write (unit, "(a)") "  if (abs(jvp(1) - 6.0_dp) > 1.0e-14_dp) error stop 1"
        write (unit, "(a)") "end program drive_array"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_array "// &
            "/tmp/fortsym_gen_array.f90 > /tmp/fortsym_gen_array.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("array-shaped generated kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_gen_array", wait=.true., &
                exitstat=stat)
            call ok("array-shaped generated kernel runs", stat == 0)
        end if
    end subroutine test_array_shaped_arguments

    subroutine test_rank_two_array_arguments()
        type(arena_t), target :: a
        type(expr_t) :: roots(4)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots = [sym(a, "x(1,1)")*sym(a, "v(1,1)"), &
            sym(a, "x(2,1)")*sym(a, "v(2,1)"), &
            sym(a, "x(1,2)")*sym(a, "v(1,2)"), &
            sym(a, "x(2,2)")*sym(a, "v(2,2)")]
        spec = spec_for("matrix_product", ["x", "v"], ["jvp"])
        spec%module_name = str("generated_matrix_product")
        allocate (spec%arg_shapes(2), spec%output_shapes(1), &
            spec%output_references(4))
        spec%arg_shapes = [str("(2,2)"), str("(2,2)")]
        spec%output_shapes = [str("(2,2)")]
        spec%output_references = [str("jvp(1,1)"), str("jvp(2,1)"), &
            str("jvp(1,2)"), str("jvp(2,2)")]
        code = chars(emit_kernel(roots, spec))
        call ok("rank-two input declarations", &
            index(code, "intent(in) :: x(2,2), v(2,2)") > 0)
        call ok("rank-two output declaration", &
            index(code, "intent(out) :: jvp(2,2)") > 0)

        open (newunit=unit, file="/tmp/fortsym_gen_matrix.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("rank-two kernel fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_matrix"
        write (unit, "(a)") "  use generated_matrix_product"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: x(2,2), v(2,2), jvp(2,2)"
        write (unit, "(a)") "  x = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2,2])"
        write (unit, "(a)") "  v = reshape([5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp], [2,2])"
        write (unit, "(a)") "  call matrix_product(x, v, jvp)"
        write (unit, "(a)") "  if (any(abs(jvp - x*v) > 1.0e-14_dp)) error stop 1"
        write (unit, "(a)") "end program drive_matrix"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_matrix "// &
            "/tmp/fortsym_gen_matrix.f90 > /tmp/fortsym_gen_matrix.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("rank-two generated kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_gen_matrix", wait=.true., &
                exitstat=stat)
            call ok("rank-two generated kernel runs", stat == 0)
        end if
    end subroutine test_rank_two_array_arguments

    subroutine test_multi_element_array_output()
        type(arena_t), target :: a
        type(expr_t) :: roots(2)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = sym(a, "x") + 1
        roots(2) = sym(a, "x")*2
        spec = spec_for("array_fill", ["x"], ["y"])
        spec%module_name = str("generated_array_fill")
        allocate (spec%output_shapes(1), spec%output_references(2))
        spec%output_shapes(1) = str("(2)")
        spec%output_references = [str("y(1)"), str("y(2)")]
        code = chars(emit_kernel(roots, spec))
        call ok("multi-element array declared once", &
            index(code, "intent(out) :: y(2)") > 0)
        call ok("first array element assigned", &
            index(code, "y(1) = x + 1") > 0)
        call ok("second array element assigned", &
            index(code, "y(2) = x*2") > 0)

        open (newunit=unit, file="/tmp/fortsym_gen_array_fill.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("multi-element array fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_array_fill"
        write (unit, "(a)") "  use generated_array_fill, only: array_fill"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: y(2)"
        write (unit, "(a)") "  call array_fill(3.0_dp, y)"
        write (unit, "(a)") "  if (maxval(abs(y - [4.0_dp, 6.0_dp])) > 1.0e-14_dp) error stop 1"
        write (unit, "(a)") "end program drive_array_fill"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_array_fill "// &
            "/tmp/fortsym_gen_array_fill.f90 "// &
            "> /tmp/fortsym_gen_array_fill.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("multi-element array kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line( &
                "/tmp/fortsym_gen_array_fill", wait=.true., exitstat=stat)
            call ok("multi-element array kernel runs", stat == 0)
        end if
    end subroutine test_multi_element_array_output

    subroutine test_pure_elemental_procedure()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = sym(a, "x")*sym(a, "v")
        spec = spec_for("elemental_product", ["x", "v"], ["jvp"])
        spec%module_name = str("generated_elemental_product")
        spec%pure_procedure = .true.
        spec%elemental_procedure = .true.
        code = chars(emit_kernel(roots, spec))
        call ok("pure elemental declaration", &
            index(code, "pure elemental subroutine elemental_product") > 0)

        open (newunit=unit, file="/tmp/fortsym_gen_elemental.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("elemental kernel fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive_elemental"
        write (unit, "(a)") "  use generated_elemental_product"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: x(2), v(2), jvp(2)"
        write (unit, "(a)") "  x = [3.0_dp, 4.0_dp]; v = [2.0_dp, -1.0_dp]"
        write (unit, "(a)") "  call elemental_product(x, v, jvp)"
        write (unit, "(a)") "  if (any(abs(jvp - [6.0_dp, -4.0_dp]) > 1.0e-14_dp)) error stop 1"
        write (unit, "(a)") "end program drive_elemental"
        close (unit)
        call execute_command_line( &
            "gfortran -J /tmp -o /tmp/fortsym_gen_elemental "// &
            "/tmp/fortsym_gen_elemental.f90 "// &
            "> /tmp/fortsym_gen_elemental.log 2>&1", wait=.true., &
            exitstat=stat)
        call ok("pure elemental generated kernel compiles", stat == 0)
        if (stat == 0) then
            call execute_command_line("/tmp/fortsym_gen_elemental", &
                wait=.true., exitstat=stat)
            call ok("pure elemental generated kernel maps arrays", stat == 0)
        end if
    end subroutine test_pure_elemental_procedure

    subroutine test_device_leaf_annotations()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        integer :: unit, ios, stat

        call a%init()
        roots(1) = parsed(a, "x*x")
        spec = spec_for("k", ["x"], ["r"])
        spec%module_name = str("generated_device_leaf")
        spec%pure_procedure = .true.
        spec%openmp_declare_target = .true.
        spec%openacc_routine_seq = .true.
        spec%nvfortran_inline = .true.
        code = chars(emit_kernel(roots, spec))
        call ok("OpenACC routine directive", &
            index(code, "!$acc routine seq") > 0)
        call ok("NVFORTRAN inline pragma immediately precedes procedure", &
            index(code, "!NVF$ INLINE"//new_line("a")// &
            "    pure subroutine k(x, r)") > 0)
        call ok("OpenMP declare-target directive annotates the leaf", &
            index(code, "subroutine k(x, r)"//new_line("a")// &
            "        !$omp declare target") > 0)
        call ok("device emission adds no parallel schedule", &
            index(code, "!$omp target") == 0 .and. &
            index(code, "!$acc parallel") == 0 .and. &
            index(code, "!$acc kernels") == 0)

        open (newunit=unit, file="/tmp/fortsym_gen_device_leaf.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            call ok("device leaf fixture opens", .false.)
            return
        end if
        write (unit, "(a)") code
        close (unit)
        call execute_command_line( &
            "gfortran -fopenmp -c -J /tmp -o /tmp/fortsym_gen_device_leaf.o "// &
            "/tmp/fortsym_gen_device_leaf.f90 "// &
            "> /tmp/fortsym_gen_device_leaf.log 2>&1", &
            wait=.true., exitstat=stat)
        call ok("OpenMP-annotated generated leaf compiles", stat == 0)
    end subroutine test_device_leaf_annotations

    subroutine test_product_codegen_scales_linearly()
        integer :: operations8, operations16

        operations8 = product_operations(8, "a")
        operations16 = product_operations(16, "b")
        call ok("contracted product code grows near-linearly", &
            2*operations16 <= 5*operations8)
    end subroutine test_product_codegen_scales_linearly

    function product_operations(n, prefix) result(total)
        integer, intent(in) :: n
        character(*), intent(in) :: prefix
        integer :: total
        type(arena_t), target :: a
        type(expr_t), allocatable :: x(:), direction(:), variables(:)
        type(expr_t), allocatable :: tangents(:), cotangent(:), products(:)
        type(expr_t) :: forward(1)
        type(expr_t), allocatable :: roots(:)
        type(expr_t) :: value, sum_x
        type(operation_count_t) :: operations
        character(3) :: index_text
        integer :: i

        call a%init()
        allocate (x(n), direction(n), variables(n), tangents(n))
        do i = 1, n
            write (index_text, "(i0)") i
            x(i) = sym(a, prefix//"x"//trim(index_text))
            direction(i) = sym(a, prefix//"v"//trim(index_text))
            variables(i) = x(i)
            tangents(i) = direction(i)
            if (i == 1) then
                sum_x = x(i)
                value = sin(x(i))
            else
                sum_x = sum_x + x(i)
                value = value + sin(x(i))
            end if
        end do
        value = value + sum_x**2/2
        cotangent = [sym(a, prefix//"u")]
        products = vjp([value], variables, cotangent)
        forward = jvp([value], variables, tangents)
        allocate (roots(n + 2))
        roots(1) = value
        roots(2) = forward(1)
        roots(3:) = products
        operations = count_operations(roots)
        total = operations%total
    end function product_operations

    subroutine test_pure_procedure()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "x*x")
        spec = spec_for("k", ["x"], ["r"])
        spec%pure_procedure = .true.
        code = chars(emit_kernel(roots, spec))
        call ok("pure procedure prefix", &
            index(code, "pure subroutine k") > 0)
    end subroutine test_pure_procedure

    subroutine test_undeclared_symbol_is_refused()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        logical :: accepted

        call a%init()
        roots(1) = parsed(a, "x + undeclared_parameter")
        spec = spec_for("k", ["x"], ["r"])
        code = chars(emit_kernel(roots, spec, accepted))
        call ok("undeclared kernel symbol is refused", .not. accepted)
        call ok("refused kernel source is empty", len(code) == 0)
    end subroutine test_undeclared_symbol_is_refused

    subroutine test_fortran_symbol_case_is_ignored()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: spec
        character(:), allocatable :: code
        logical :: accepted

        call a%init()
        roots(1) = parsed(a, "Phi_eff + x")
        spec = spec_for("k", [character(len=7) :: "phi_eff", "x"], ["r"])
        code = chars(emit_kernel(roots, spec, accepted))
        call ok("Fortran kernel symbols match inputs case-insensitively", &
            accepted .and. len(code) > 0)
    end subroutine test_fortran_symbol_case_is_ignored

    !> A temporary must be assigned before anything uses it.
    subroutine test_ordering_is_topological()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code
        integer :: def1, def2, use_in_2

        call a%init()
        ! exp(x+y) is shared, and sin of it is shared too, so one temporary must
        ! depend on the other.
        roots(1) = parsed(a, &
            "sin(exp(x + y))*sin(exp(x + y)) + exp(x + y) + sin(exp(x + y))")
        code = chars(emit_kernel(roots, spec_for("k", ["x", "y"], ["r"])))

        def1 = index(code, "    t1 = ")
        def2 = index(code, "    t2 = ")
        call ok("two temporaries generated", def1 > 0 .and. def2 > 0)

        if (def1 > 0 .and. def2 > 0) then
            ! t2's definition must come after t1's, and if t2 mentions t1 that
            ! reference must be to an already-assigned value.
            call ok("temporaries defined in order", def1 < def2)
            use_in_2 = index(code(def2:), "t1")
            if (use_in_2 > 0) then
                call ok("dependency defined before use", def1 < def2)
            end if
        end if
    end subroutine test_ordering_is_topological

    !> Long statements must be continued, and never broken inside a literal.
    subroutine test_line_wrapping()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(expr_t) :: variables(5), values(4), cotangents(4), products(5)
        type(native_engine_t) :: engine
        type(engine_result_t) :: simplified
        character(:), allocatable :: code
        integer :: longest, k

        call a%init()
        roots(1) = parsed(a, &
            "aaaa*bbbb + cccc*dddd + eeee*ffff + gggg*hhhh + iiii*jjjj + "// &
            "kkkk*llll + mmmm*nnnn + oooo*pppp + qqqq*rrrr + ssss*tttt")
        code = chars(emit_kernel(roots, spec_for("k", &
            ["aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "gggg", &
            "hhhh", "iiii", "jjjj", "kkkk", "llll", "mmmm", "nnnn", &
            "oooo", "pppp", "qqqq", "rrrr", "ssss", "tttt"], ["r"])))

        call ok("long statement is continued", index(code, "&") > 0)

        longest = longest_line(code)
        call ok("lines stay within the Fortran limit", longest <= 132)

        ! A break inside an exponent would change the constant's value.
        call a%clear()
        call a%init()
        roots(1) = parsed(a, &
            "1.0e-8*aaaa + 1.0e-8*bbbb + 1.0e-8*cccc + 1.0e-8*dddd + "// &
            "1.0e-8*eeee + 1.0e-8*ffff + 1.0e-8*gggg + 1.0e-8*hhhh")
        code = chars(emit_kernel(roots, spec_for("k", &
            ["aaaa", "bbbb", "cccc", "dddd", "eeee", "ffff", "gggg", &
            "hhhh"], ["r"])))
        call ok("never breaks inside an exponent", index(code, "e- &") == 0)
        call ok("never breaks after an exponent sign", index(code, "e-&") == 0)

        ! The power operator is a two-character token.  Splitting it would
        ! turn valid `x**2` into the invalid continuation `x* &` / `*2`.
        call a%clear()
        call a%init()
        variables = [sym(a, "target"), sym(a, "node_1"), sym(a, "node_2"), &
            sym(a, "node_3"), sym(a, "node_4")]
        values = [ &
            (variables(1) - variables(3))*(variables(1) - variables(4))* &
            (variables(1) - variables(5))/ &
            ((variables(2) - variables(3))*(variables(2) - variables(4))* &
            (variables(2) - variables(5))), &
            (variables(1) - variables(2))*(variables(1) - variables(4))* &
            (variables(1) - variables(5))/ &
            ((variables(3) - variables(2))*(variables(3) - variables(4))* &
            (variables(3) - variables(5))), &
            (variables(1) - variables(2))*(variables(1) - variables(3))* &
            (variables(1) - variables(5))/ &
            ((variables(4) - variables(2))*(variables(4) - variables(3))* &
            (variables(4) - variables(5))), &
            (variables(1) - variables(2))*(variables(1) - variables(3))* &
            (variables(1) - variables(4))/ &
            ((variables(5) - variables(2))*(variables(5) - variables(3))* &
            (variables(5) - variables(4)))]
        cotangents = [sym(a, "weight_1_bar"), sym(a, "weight_2_bar"), &
            sym(a, "weight_3_bar"), sym(a, "weight_4_bar")]
        products = vjp(values, variables, cotangents)
        engine = make_native_engine(a)
        do k = 1, size(products)
            simplified = engine%simplify(products(k))
            call ok("cubic VJP simplification succeeds", simplified%ok)
            if (simplified%ok) products(k) = simplified%value
        end do
        code = chars(emit_kernel(products, spec_for("k", &
            [character(len=12) :: "target", "node_1", "node_2", "node_3", "node_4", &
            "weight_1_bar", "weight_2_bar", "weight_3_bar", "weight_4_bar"], &
            ["target_bar", "node_1_bar", "node_2_bar", "node_3_bar", &
            "node_4_bar"])))
        call ok("power operator is exercised", index(code, "**") > 0)
        call ok("never splits the power operator", .not. contains_split_power(code))
    end subroutine test_line_wrapping

    subroutine test_long_interface_wrapping()
        integer, parameter :: emitter_line_limit = 100
        type(arena_t), target :: a
        type(expr_t) :: roots(9)
        type(kernel_spec_t) :: spec
        character(3) :: args(18), outputs(9)
        character(:), allocatable :: code
        integer :: k

        call a%init()
        do k = 1, size(args)
            write (args(k), "(a,i0)") "a", k
        end do
        do k = 1, size(outputs)
            write (outputs(k), "(a,i0)") "r", k
            roots(k) = parsed(a, trim(args(k)))
        end do
        spec = spec_for("long_generated_kernel", args, outputs)
        code = chars(emit_kernel(roots, spec))
        call ok("long interface stays within emitter line limit", &
            longest_line(code) <= emitter_line_limit)
        call ok("continuation indentation has no carried source blank", &
            index(code, new_line("a")//"         a") == 0)
    end subroutine test_long_interface_wrapping

    function longest_line(text) result(n)
        character(*), intent(in) :: text
        integer                  :: n
        integer :: start, k
        n = 0
        start = 1
        do k = 1, len(text)
            if (text(k:k) == new_line("a")) then
                n = max(n, k - start)
                start = k + 1
            end if
        end do
        n = max(n, len(text) - start + 1)
    end function longest_line

    !> Detect a continuation placed between the two characters of `**`, without
    !> depending on the emitter's current continuation indentation.
    logical function contains_split_power(text) result(found)
        character(*), intent(in) :: text
        integer :: start, mark, k

        found = .false.
        start = 1
        do
            mark = index(text(start:), "* &")
            if (mark == 0) return
            k = start + mark + 2
            do while (k <= len(text) .and. text(k:k) == new_line("a"))
                k = k + 1
            end do
            do while (k <= len(text) .and. text(k:k) == " ")
                k = k + 1
            end do
            if (k <= len(text) .and. text(k:k) == "*") then
                found = .true.
                return
            end if
            start = start + mark + 2
        end do
    end function contains_split_power

    subroutine test_snippet_mode()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: sp
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "x*y + 1")
        sp = spec_for("k", ["x", "y"], ["r"])
        sp%mode = KERNEL_SNIPPET
        code = chars(emit_kernel(roots, sp))

        ! A snippet is spliced into an existing scope, so it must contribute
        ! statements and nothing else.
        call ok("snippet has no subroutine header", &
            index(code, "subroutine") == 0)
        call ok("snippet has no declarations", index(code, "real(dp)") == 0)
        call ok("snippet assigns the output", index(code, "r = ") > 0)
        call ok("snippet still records its generator", &
            index(code, "gen_test") > 0)
    end subroutine test_snippet_mode

    !> The real check: compile the generated kernel and compare it numerically
    !> against a hand-written evaluation of the same formula.
    subroutine test_generated_kernel_compiles_and_agrees()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code
        integer :: unit, ios, stat
        real(dp) :: got, want, xv, yv
        integer :: k
        logical :: agreed
        character(:), allocatable :: huge_numerator, huge_denominator

        call a%init()
        roots(1) = parsed(a, &
            "sin(x*y)**2 + cos(x*y) + exp(x)/(1 + y**2) - 3*x/(y + 1)")
        ! Independent exact projections: (-2^64)*2^-64 = -1, while
        ! -10^400/(10^400+1) rounds nearest-even to -1. Their product, scaled
        ! by 2^-64, is +1, independently checking even sign parity. This
        ! exercises negative arbitrary integers and rationals through emitted,
        ! compiled Fortran rather than comparing source text.
        huge_numerator = "1"//repeat("0", 400)
        huge_denominator = "1"//repeat("0", 399)//"1"
        roots(1) = roots(1) + &
            exact(a, "-18446744073709551616")*&
            real_expr(a, 2.0_dp**(-64)) + &
            exact(a, "-"//huge_numerator//"/"//huge_denominator) + &
            exact(a, "-18446744073709551616")*&
            exact(a, "-"//huge_numerator//"/"//huge_denominator)*&
            real_expr(a, 2.0_dp**(-64))
        code = chars(emit_kernel(roots, spec_for("k", ["x", "y"], ["r"])))

        open (newunit=unit, file="/tmp/fortsym_gen_kernel.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            nfail = nfail + 1
            print *, "FAIL could not write generated kernel"
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: x, y, r"
        write (unit, "(a)") "  integer :: i, j"
        write (unit, "(a)") "  do i = 1, 5"
        write (unit, "(a)") "  do j = 1, 5"
        write (unit, "(a)") "    x = 0.1_dp*i"
        write (unit, "(a)") "    y = 0.1_dp*j"
        write (unit, "(a)") "    call k(x, y, r)"
        write (unit, "(a)") "    write(*,'(es24.16)') r"
        write (unit, "(a)") "  end do"
        write (unit, "(a)") "  end do"
        write (unit, "(a)") "end program drive"
        close (unit)

        call execute_command_line( &
            "gfortran -o /tmp/fortsym_gen_kernel /tmp/fortsym_gen_kernel.f90"// &
            " > /tmp/fortsym_gen_kernel.log 2>&1", wait=.true., exitstat=stat)
        call ok("generated kernel compiles", stat == 0)
        if (stat /= 0) then
            print *, "   see /tmp/fortsym_gen_kernel.log"
            return
        end if

        call execute_command_line( &
            "/tmp/fortsym_gen_kernel > /tmp/fortsym_gen_kernel.out 2>&1", &
            wait=.true., exitstat=stat)
        call ok("generated kernel runs", stat == 0)
        if (stat /= 0) return

        open (newunit=unit, file="/tmp/fortsym_gen_kernel.out", &
            status="old", action="read", iostat=ios)
        if (ios /= 0) then
            nfail = nfail + 1
            return
        end if

        agreed = .true.
        do k = 1, 25
            read (unit, *, iostat=ios) got
            if (ios /= 0) exit
            xv = 0.1_dp*(1 + (k - 1)/5)
            yv = 0.1_dp*(1 + mod(k - 1, 5))
            want = sin(xv*yv)**2 + cos(xv*yv) + exp(xv)/(1 + yv**2) &
                - 3*xv/(yv + 1) - 1.0_dp
            if (abs(got - want) > 1.0e-13_dp*max(1.0_dp, abs(want))) then
                agreed = .false.
                print *, "   mismatch at x=", xv, " y=", yv
                print *, "   got ", got, " want ", want
            end if
        end do
        close (unit)

        call ok("generated kernel agrees numerically", agreed)
    end subroutine test_generated_kernel_compiles_and_agrees

end program test_fortsym_kernel
