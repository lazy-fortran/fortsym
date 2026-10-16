program test_fortsym_ordered_outputs
    ! Ordered roots are a consumer-owned straight-line schedule. Compile the
    ! emitted procedures and check their values against independent formulas.
    use fortsym_string, only: str, str_t, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_products, only: jvp, vjp
    use fortsym_kernel, only: kernel_spec_t, cse_result_t, emit_kernel, &
        emit_statements, CSE_NONE, CSE_THRESHOLDED, CSE_FULL
    implicit none

    integer :: nfail = 0, unit, stat
    character(4096) :: compiler

    call test_refusals()
    call test_statement_refusals()
    open (newunit=unit, file="ordered_fixture.f90", status="replace")
    call write_staged_fixture(unit)
    call write_scalar_fixture(unit)
    call write_renamed_fixture(unit)
    call write_rank_two_fixture(unit)
    call write_lower_bound_fixture(unit)
    call write_statements_fixture(unit)
    call write_driver(unit)
    close (unit)
    call fixture_compiler(compiler)
    if (nfail == 0) then
        call execute_command_line(shell_quote(trim(compiler))// &
            " -o ordered_fixture ordered_fixture.f90 > ordered_fixture.log 2>&1", &
            wait=.true., exitstat=stat)
        call check("ordered fixtures compile", stat == 0)
        if (stat == 0) then
            call execute_command_line("./ordered_fixture >> ordered_fixture.log 2>&1", &
                wait=.true., exitstat=stat)
            call check("compiled primal, JVP, VJP and range oracles", stat == 0)
        end if
    end if
    if (nfail /= 0) then
        print *, "test_fortsym_ordered_outputs:", nfail, "checks FAILED"
        error stop 1
    end if
    print *, "test_fortsym_ordered_outputs: all checks passed"

contains

    subroutine fixture_compiler(compiler)
        character(*), intent(out) :: compiler
        integer :: stat

        ! CTest supplies its configured compiler; fpm callers retain their
        ! compiler environment, with the repository's GNU default last.
        call get_environment_variable("FORTSYM_TEST_COMPILER", compiler, status=stat)
        if (stat == 0) then
            if (len_trim(compiler) > 0) return
        end if
        call get_environment_variable("FPM_FC", compiler, status=stat)
        if (stat == 0) then
            if (len_trim(compiler) > 0) return
        end if
        call get_environment_variable("FC", compiler, status=stat)
        if (stat == 0) then
            if (len_trim(compiler) > 0) return
        end if
        compiler = "gfortran"
    end subroutine fixture_compiler

    function make_spec(name, arguments, outputs) result(spec)
        character(*), intent(in) :: name, arguments, outputs
        type(kernel_spec_t) :: spec

        spec%name = str(name)
        spec%module_name = str(name//"_module")
        spec%cse_level = CSE_NONE
        spec%ordered_output_dependencies = .true.
        call set_names(spec%args, arguments, ",")
        call set_names(spec%outputs, outputs, ",")
    end function make_spec

    subroutine set_names(names, text, separator)
        type(str_t), allocatable, intent(out) :: names(:)
        character(*), intent(in) :: text
        character, intent(in) :: separator
        integer :: i, count, first, entry

        count = 1
        do i = 1, len_trim(text)
            if (text(i:i) == separator) count = count + 1
        end do
        allocate (names(count))
        first = 1
        entry = 1
        do i = 1, len_trim(text)
            if (text(i:i) /= separator) cycle
            names(entry) = str(trim(adjustl(text(first:i - 1))))
            first = i + 1
            entry = entry + 1
        end do
        names(entry) = str(trim(adjustl(text(first:))))
    end subroutine set_names

    subroutine set_shape(spec, shape)
        type(kernel_spec_t), intent(inout) :: spec
        character(*), intent(in) :: shape

        if (allocated(spec%output_shapes)) deallocate (spec%output_shapes)
        allocate (spec%output_shapes(1))
        spec%output_shapes(1) = str(shape)
    end subroutine set_shape

    subroutine write_staged_fixture(unit)
        integer, intent(in) :: unit
        type(arena_t), target :: arena
        type(expr_t) :: x, y, dx, dy, weight, q, t, dq, dt, bt, bq
        type(expr_t) :: roots(10), local(1), reverse(2)
        type(expr_t) :: variables(2), directions(2), variable(1), direction(1), seed(1)
        type(kernel_spec_t) :: spec
        character(16) :: reference
        integer :: i

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        dx = sym(arena, "dx")
        dy = sym(arena, "dy")
        weight = sym(arena, "weight")
        q = sym(arena, "STATE ( 1 )")
        t = sym(arena, "state(2)")
        dq = sym(arena, "state(4)")
        dt = sym(arena, "state(5)")
        bt = sym(arena, "state(7)")
        bq = sym(arena, "state(8)")
        roots(1) = x/y
        roots(2) = q**2
        roots(3) = t/(num(arena, 1) + t)
        variables(1) = x
        variables(2) = y
        directions(1) = dx
        directions(2) = dy
        local = jvp(roots(1:1), variables, directions)
        roots(4) = local(1)
        variable(1) = q
        direction(1) = dq
        local = jvp(roots(2:2), variable, direction)
        roots(5) = local(1)
        variable(1) = t
        direction(1) = dt
        local = jvp(roots(3:3), variable, direction)
        roots(6) = local(1)
        seed(1) = weight
        local = vjp(roots(3:3), variable, seed)
        roots(7) = local(1)
        variable(1) = q
        seed(1) = bt
        local = vjp(roots(2:2), variable, seed)
        roots(8) = local(1)
        seed(1) = bq
        reverse = vjp(roots(1:1), variables, seed)
        roots(9:10) = reverse
        spec = make_spec("ordered_products", "x,y,dx,dy,weight", "state")
        call set_shape(spec, "(10)")
        allocate (spec%output_references(10))
        do i = 1, 10
            write (reference, '(a,i0,a)') "state(", i, ")"
            spec%output_references(i) = str(trim(reference))
        end do
        call emit_fixture(unit, roots, spec)

        ! A separate primal avoids unrelated derivative intermediate ranges
        ! in the extreme-scale check. Expanding x**2/(y**2+x**2) overflows.
        spec = make_spec("ordered_primal", "x,y", "state")
        call set_shape(spec, "(3)")
        call set_names(spec%output_references, "state(1);state(2);state(3)", ";")
        call emit_fixture(unit, roots(:3), spec)
    end subroutine write_staged_fixture

    subroutine write_scalar_fixture(unit)
        integer, intent(in) :: unit
        type(arena_t), target :: arena
        type(expr_t) :: roots(3)
        type(kernel_spec_t) :: spec

        call arena%init()
        roots(1) = sym(arena, "x") + num(arena, 1)
        roots(2) = sym(arena, "FIRST")**2
        roots(3) = sym(arena, "second") - sym(arena, "First")
        spec = make_spec("ordered_scalars", "x", "first,second,value")
        call emit_fixture(unit, roots, spec)
    end subroutine write_scalar_fixture

    subroutine write_renamed_fixture(unit)
        integer, intent(in) :: unit
        type(arena_t), target :: arena
        type(expr_t) :: roots(3)
        type(kernel_spec_t) :: spec

        call arena%init()
        roots(1) = sym(arena, "x") + num(arena, 1)
        roots(2) = sym(arena, "Q")**2
        roots(3) = sym(arena, "Q") + sym(arena, "q")
        spec = make_spec("ordered_renamed", "x", "Q,q,value")
        call emit_fixture(unit, roots, spec)
    end subroutine write_renamed_fixture

    subroutine write_rank_two_fixture(unit)
        integer, intent(in) :: unit
        type(arena_t), target :: arena
        type(expr_t) :: roots(4)
        type(kernel_spec_t) :: spec

        call arena%init()
        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "GRID (1, 1)") + num(arena, 1)
        roots(3) = num(arena, 2)*sym(arena, "grid(2,1)")
        roots(4) = sym(arena, "grid(1,2)") + sym(arena, "grid(1,1)")
        spec = make_spec("ordered_rank_two", "x", "grid")
        call set_shape(spec, "(2,2)")
        call set_names(spec%output_references, &
            "grid(1,1);grid(2,1);grid(1,2);grid(2,2)", ";")
        call emit_fixture(unit, roots, spec)
    end subroutine write_rank_two_fixture

    subroutine write_lower_bound_fixture(unit)
        integer, intent(in) :: unit
        type(arena_t), target :: arena
        type(expr_t) :: roots(2)
        type(kernel_spec_t) :: spec

        call arena%init()
        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "range(3)")**2
        spec = make_spec("ordered_lower_bound", "x", "range")
        call set_shape(spec, "(3:5)")
        call set_names(spec%output_references, "range(3);range(5)", ";")
        call emit_fixture(unit, roots, spec)
    end subroutine write_lower_bound_fixture

    subroutine write_statements_fixture(unit)
        integer, intent(in) :: unit
        type(arena_t), target :: arena
        type(expr_t) :: roots(2)
        type(kernel_spec_t) :: spec
        type(cse_result_t) :: res
        type(str_t) :: source
        logical :: good

        call arena%init()
        roots(1) = sym(arena, "x") + num(arena, 1)
        roots(2) = sym(arena, "STATE ( 1 )")**2
        spec = make_spec("ordered_statements", "x", "state")
        call set_shape(spec, "(2)")
        call set_names(spec%output_references, "state(1);state(2)", ";")
        allocate (res%ids(0), res%names(0))
        source = emit_statements(roots, spec, res, good)
        call check("direct statements accept prior array element", good)
        if (.not. good) return
        write (unit, '(a)') &
            "module ordered_statements_module", &
            "use, intrinsic :: iso_fortran_env, only: dp => real64", &
            "implicit none", &
            "private", &
            "public :: ordered_statements", &
            "contains", &
            "subroutine ordered_statements(x,state)", &
            "real(dp), intent(in) :: x", &
            "real(dp), intent(out) :: state(2)", &
            chars(source), &
            "end subroutine ordered_statements", &
            "end module ordered_statements_module"
    end subroutine write_statements_fixture

    subroutine emit_fixture(unit, roots, spec)
        integer, intent(in) :: unit
        type(expr_t), intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(str_t) :: source
        character(:), allocatable :: message
        logical :: good

        source = emit_kernel(roots, spec, good, message=message)
        call check("emit "//chars(spec%name), good)
        if (.not. good) print *, message
        if (good) write (unit, '(a)') chars(source)
    end subroutine emit_fixture

    subroutine write_driver(unit)
        integer, intent(in) :: unit

        write (unit, '(a)') &
            "program oracle", &
            "use, intrinsic :: iso_fortran_env, only: dp => real64", &
            "use, intrinsic :: ieee_arithmetic, only: ieee_is_finite", &
            "use ordered_products_module", &
            "use ordered_primal_module", &
            "use ordered_scalars_module", &
            "use ordered_renamed_module", &
            "use ordered_rank_two_module", &
            "use ordered_lower_bound_module", &
            "use ordered_statements_module", &
            "implicit none", &
            "real(dp) :: x,y,dx,dy,w,r(10),p(3),a,b,c,g(2,2),f,gx,gy,fd,h", &
            "real(dp) :: range(3:5), direct(2)", &
            "integer :: i", &
            "do i=1,13", &
            "x=-2.0_dp+0.375_dp*real(i,dp)", &
            "y=0.75_dp+0.125_dp*real(i,dp)", &
            "dx=0.3_dp-0.07_dp*real(i,dp)", &
            "dy=-0.2_dp+0.03_dp*real(i,dp)", &
            "w=-0.8_dp+0.1_dp*real(i,dp)", &
            "f=x*x/(x*x+y*y)", &
            "gx=2.0_dp*x*y*y/(x*x+y*y)**2", &
            "gy=-2.0_dp*y*x*x/(x*x+y*y)**2", &
            "call ordered_products(x,y,dx,dy,w,r)", &
            "if (.not. all(ieee_is_finite(r))) error stop 10", &
            "if (abs(r(3)-f)>2.e-13_dp) error stop 11", &
            "if (abs(r(6)-gx*dx-gy*dy)>2.e-13_dp) error stop 12", &
            "if (abs(r(9)-w*gx)>2.e-13_dp) error stop 13", &
            "if (abs(r(10)-w*gy)>2.e-13_dp) error stop 14", &
            "if (abs(w*r(6)-r(9)*dx-r(10)*dy)>2.e-13_dp) error stop 15", &
            "h=1.e-5_dp", &
            "call ordered_primal(x+h*dx,y+h*dy,p)", &
            "fd=p(3)", &
            "call ordered_primal(x-h*dx,y-h*dy,p)", &
            "fd=(fd-p(3))/(2.0_dp*h)", &
            "if (abs(fd-r(6))>2.e-10_dp) error stop 16", &
            "end do", &
            "x=1.e200_dp", &
            "call ordered_primal(x,x,p)", &
            "if (.not. all(ieee_is_finite(p))) error stop 20", &
            "if (abs(p(3)-0.5_dp)>2.e-15_dp) error stop 21", &
            "if (ieee_is_finite(x*x)) error stop 22", &
            "call ordered_scalars(2.0_dp,a,b,c)", &
            "if (maxval(abs([a,b,c]-[3.0_dp,9.0_dp,6.0_dp]))>1.e-13_dp) &", &
            "error stop 30", &
            "call ordered_renamed(2.0_dp,a,b,c)", &
            "if (maxval(abs([a,b,c]-[3.0_dp,9.0_dp,12.0_dp]))>1.e-13_dp) &", &
            "error stop 31", &
            "call ordered_rank_two(2.0_dp,g)", &
            "if (maxval(abs(reshape(g,[4])-[2.0_dp,3.0_dp,6.0_dp,8.0_dp])) &", &
            ">1.e-13_dp) error stop 32", &
            "call ordered_lower_bound(2.0_dp,range)", &
            "if (range(3)/=2.0_dp .or. range(5)/=4.0_dp) error stop 33", &
            "call ordered_statements(2.0_dp,direct)", &
            "if (maxval(abs(direct-[3.0_dp,9.0_dp]))>1.e-13_dp) error stop 34", &
            "end program oracle"
    end subroutine write_driver

    subroutine test_refusals()
        type(arena_t), target :: arena
        type(expr_t) :: roots(2)
        type(kernel_spec_t) :: spec, default_spec
        character(24), parameter :: invalid_reads(*) = [character(24) :: &
            "state(1)", "state(2)", "state(3)", "state", "state(i)", &
            "state(1+1)", "state(0)", "state(-1)", "state(1:2)", &
            "state(1))", "other(1)", "state(1,1)"]
        character(24), parameter :: invalid_targets(*) = [character(24) :: &
            "other(1)", "state(i)", "state(0)", "state(-1)", &
            "state(1+1)", "state(1:2)", "state(1))", "state", &
            "state(1,1)", "state(3)"]
        character(5), parameter :: boolean_names(4) = &
            [character(5) :: "True", "False", "TRUE", "false"]
        character(5), parameter :: boolean_atoms(4) = &
            [character(5) :: "True", "False", "True", "False"]
        integer :: i

        call arena%init()
        spec = make_spec("refused", "x", "state")
        call set_shape(spec, "(2)")
        call set_names(spec%output_references, "state(1);state(2)", ";")
        do i = 1, size(invalid_reads)
            roots(1) = sym(arena, trim(invalid_reads(i))) + num(arena, 1)
            roots(2) = sym(arena, "x")
            call expect_refused("read "//trim(invalid_reads(i)), roots, spec)
        end do
        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "state(3)")
        call expect_refused("unassigned later element", roots, spec)
        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "state(1)")
        spec%ordered_output_dependencies = default_spec%ordered_output_dependencies
        call expect_refused("output reads remain disabled by default", roots, spec)
        spec%ordered_output_dependencies = .true.
        spec%cse_level = CSE_FULL
        call expect_refused("full CSE with ordered dependencies", roots, spec)
        spec%cse_level = CSE_THRESHOLDED
        call expect_refused("thresholded CSE with ordered dependencies", roots, spec)
        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "x") + num(arena, 1)
        call expect_refused("thresholded CSE without dependent roots", roots, spec)
        spec%cse_level = CSE_FULL
        call expect_refused("full CSE without dependent roots", roots, spec)
        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "x") + num(arena, 1)
        spec%cse_level = CSE_NONE
        do i = 1, size(invalid_targets)
            spec%output_references(2) = str(trim(invalid_targets(i)))
            call expect_refused("target "//trim(invalid_targets(i)), roots, spec)
        end do
        spec%output_references(2) = str("STATE ( 1 )")
        call expect_refused("duplicate assignment target", roots, spec)
        spec%output_references(2) = str("state(01)")
        call expect_refused("duplicate with leading-zero index", roots, spec)

        spec = make_spec("refused_rank_two", "x", "grid")
        call set_shape(spec, "(2,2)")
        call set_names(spec%output_references, "grid(1,1);grid(3,1)", ";")
        call expect_refused("rank-two upper first bound", roots, spec)
        spec%output_references(2) = str("grid(1,3)")
        call expect_refused("rank-two upper second bound", roots, spec)

        spec = make_spec("refused_lower_bound", "x", "range")
        call set_shape(spec, "(3:5)")
        call set_names(spec%output_references, "range(3);range(2)", ";")
        call expect_refused("explicit lower bound", roots, spec)
        spec%output_references(2) = str("range(6)")
        call expect_refused("explicit upper bound", roots, spec)

        ! An exact output spelling must not fall through to a similarly
        ! cased input when the requested output is still unassigned.
        spec = make_spec("refused_case_forward", "q", "value,Q")
        roots(1) = sym(arena, "Q") + num(arena, 1)
        roots(2) = sym(arena, "q")
        call expect_refused("exact case-collision forward output", roots, spec)
        spec%outputs(1) = str("Q")
        spec%outputs(2) = str("value")
        call expect_refused("exact case-collision self output", roots, spec)

        ! Boolean atoms have dedicated symbolic meaning and cannot serve as
        ! ordered numeric output names, including case-insensitive aliases.
        do i = 1, size(boolean_names)
            spec = make_spec("refused_boolean_output", "x", &
                "value,"//trim(boolean_names(i)))
            roots(1) = sym(arena, trim(boolean_atoms(i)))
            roots(2) = sym(arena, "x")
            call expect_refused("Boolean forward "//trim(boolean_names(i)), roots, spec)
            spec%outputs(1) = str(trim(boolean_names(i)))
            spec%outputs(2) = str("value")
            call expect_refused("Boolean self "//trim(boolean_names(i)), roots, spec)
        end do
    end subroutine test_refusals

    subroutine test_statement_refusals()
        type(arena_t), target :: arena
        type(expr_t) :: roots(2)
        type(kernel_spec_t) :: spec
        type(cse_result_t) :: res
        type(str_t) :: source
        logical :: good

        call arena%init()
        allocate (res%ids(0), res%names(0))
        spec = make_spec("refused_statements", "x", "first,second")
        roots(1) = sym(arena, "second") + num(arena, 1)
        roots(2) = sym(arena, "x")
        source = emit_statements(roots, spec, res, good)
        call check("direct scalar forward statements refused", .not. good)
        call check("direct forward source empty", len(chars(source)) == 0)
        roots(1) = sym(arena, "first") + num(arena, 1)
        source = emit_statements(roots, spec, res, good)
        call check("direct scalar self statements refused", .not. good)
        call check("direct self source empty", len(chars(source)) == 0)

        roots(1) = sym(arena, "x")
        roots(2) = sym(arena, "x") + num(arena, 1)
        source = emit_statements(roots, spec, res, good, prechecked=.false.)
        call check("direct explicit false precheck refused", .not. good)
        call check("direct false-precheck source empty", len(chars(source)) == 0)
        spec%cse_level = CSE_FULL
        source = emit_statements(roots, spec, res, good)
        call check("direct full CSE statements refused", .not. good)
        spec%cse_level = CSE_THRESHOLDED
        source = emit_statements(roots, spec, res, good)
        call check("direct thresholded CSE statements refused", .not. good)
        spec%cse_level = CSE_NONE
        deallocate (res%ids, res%names)
        allocate (res%ids(1), res%names(1))
        res%n = 1
        res%ids(1) = roots(2)%id
        res%names(1) = str("t1")
        source = emit_statements(roots, spec, res, good)
        call check("direct nonempty CSE result refused", .not. good)
    end subroutine test_statement_refusals

    subroutine expect_refused(label, roots, spec)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: roots(:)
        type(kernel_spec_t), intent(in) :: spec
        type(str_t) :: source
        character(:), allocatable :: message
        logical :: good

        source = emit_kernel(roots, spec, good, message=message)
        call check("refused "//label, .not. good)
        call check("empty refused source: "//label, len(chars(source)) == 0)
        call check("diagnostic for "//label, len_trim(message) > 0)
    end subroutine expect_refused

    subroutine check(label, good)
        character(*), intent(in) :: label
        logical, intent(in) :: good

        if (good) return
        nfail = nfail + 1
        print *, "FAIL: ", label
    end subroutine check

    function shell_quote(value) result(quoted)
        character(*), intent(in) :: value
        character(:), allocatable :: quoted
        integer :: i

        quoted = "'"
        do i = 1, len(value)
            if (value(i:i) == "'") then
                quoted = quoted//"'"//achar(34)//"'"//achar(34)//"'"
            else
                quoted = quoted//value(i:i)
            end if
        end do
        quoted = quoted//"'"
    end function shell_quote
end program test_fortsym_ordered_outputs
