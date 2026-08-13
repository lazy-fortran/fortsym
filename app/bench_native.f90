program bench_native
    ! Warm, batched end-to-end comparison of the native and SymEngine backends.
    !
    ! Output is CSV. Each operation is validated before its timing row is
    ! emitted. These rows include backend conversion and result construction,
    ! so they are not direct library-kernel timings.
    use, intrinsic :: iso_fortran_env, only: int64, real64, compiler_version
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_string, only: str, chars
    use fortsym_engine, only: engine_t, engine_result_t, wall_seconds
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: WARMUPS = 5
    integer, parameter :: REPETITIONS = 31
    integer, parameter :: BATCH = 50
    integer, parameter :: COLD_WARMUPS = 2
    integer, parameter :: COLD_REPETITIONS = 11
    integer, parameter :: COLD_BATCH = 10
    integer, parameter :: OP_SIMPLIFY = 1
    integer, parameter :: OP_DIFF = 2
    integer, parameter :: OP_EXPAND = 3
    integer, parameter :: OP_FLAT_SIMPLIFY = 4

    write (*, '(a)') "schema,compiler,workload,engine,scope,warmups,"// &
        "repetitions,batch,median_seconds,min_seconds,correct"
    call run_native_suite()
    call run_symengine_suite()

contains

    subroutine run_native_suite()
        type(arena_t), target :: a
        type(native_engine_t) :: eng

        call a%init()
        eng = make_native_engine(a)
        call run_suite(eng, "native", a)
    end subroutine run_native_suite

    subroutine run_symengine_suite()
        type(arena_t), target :: a
        type(symengine_engine_t) :: eng

        call a%init()
        eng = make_symengine_engine(a)
        call run_suite(eng, "symengine", a)
    end subroutine run_symengine_suite

    subroutine run_suite(eng, engine_name, a)
        class(engine_t), intent(inout) :: eng
        character(*),    intent(in)    :: engine_name
        type(arena_t), target, intent(inout) :: a
        type(expr_t) :: x, y, simplify_input, diff_input, expand_input

        x = sym(a, "x")
        y = sym(a, "y")
        simplify_input = x + x + 2*x - 4*x + &
            rat(a, 1_int64, 2_int64) + rat(a, 1_int64, 3_int64) - &
            rat(a, 5_int64, 6_int64)
        diff_input = (x + 1)**8
        expand_input = (x + y + 1)**7

        call benchmark(eng, engine_name, OP_SIMPLIFY, "simplify_collect", &
            simplify_input, x, a)
        call benchmark(eng, engine_name, OP_DIFF, "differentiate_power", &
            diff_input, x, a)
        call benchmark(eng, engine_name, OP_EXPAND, "expand_power", &
            expand_input, x, a)
        call benchmark_cold(eng, engine_name, OP_SIMPLIFY, "simplify_collect", &
            x, y, a)
        call benchmark_cold(eng, engine_name, OP_FLAT_SIMPLIFY, &
            "simplify_flat_like_terms", x, y, a)
        call benchmark_cold(eng, engine_name, OP_DIFF, "differentiate_power", &
            x, y, a)
        call benchmark_cold(eng, engine_name, OP_EXPAND, "expand_power", &
            x, y, a)
    end subroutine run_suite

    subroutine benchmark(eng, engine_name, operation, workload, input, &
            variable, a)
        class(engine_t), intent(inout) :: eng
        character(*),    intent(in)    :: engine_name, workload
        integer,         intent(in)    :: operation
        type(expr_t),    intent(in)    :: input, variable
        type(arena_t),   intent(inout) :: a
        real(dp) :: samples(REPETITIONS), started
        type(engine_result_t) :: result
        integer :: repetition, k
        logical :: correct

        do repetition = 1, WARMUPS
            do k = 1, BATCH
                result = perform(eng, operation, input, variable)
            end do
        end do

        do repetition = 1, REPETITIONS
            started = wall_seconds()
            do k = 1, BATCH
                result = perform(eng, operation, input, variable)
            end do
            samples(repetition) = (wall_seconds() - started)/real(BATCH, dp)
        end do

        correct = validate(operation, result, input, variable, a)
        call emit_row(workload, engine_name, "warm_same_expression", samples, &
            correct, WARMUPS, REPETITIONS, BATCH)
    end subroutine benchmark

    subroutine benchmark_cold(eng, engine_name, operation, workload, x, y, a)
        class(engine_t), intent(inout) :: eng
        character(*),    intent(in)    :: engine_name, workload
        integer,         intent(in)    :: operation
        type(expr_t),    intent(in)    :: x, y
        type(arena_t),   intent(inout) :: a
        type(expr_t), allocatable :: inputs(:)
        real(dp) :: samples(COLD_REPETITIONS), started
        type(engine_result_t) :: result
        integer :: repetition, k, offset, total
        logical :: correct

        total = (COLD_WARMUPS + COLD_REPETITIONS)*COLD_BATCH
        allocate (inputs(total))
        do k = 1, total
            inputs(k) = distinct_input(operation, k, x, y, a)
        end do

        do repetition = 1, COLD_WARMUPS
            offset = (repetition - 1)*COLD_BATCH
            do k = 1, COLD_BATCH
                result = perform(eng, operation, inputs(offset + k), x)
            end do
        end do

        do repetition = 1, COLD_REPETITIONS
            offset = (COLD_WARMUPS + repetition - 1)*COLD_BATCH
            started = wall_seconds()
            do k = 1, COLD_BATCH
                result = perform(eng, operation, inputs(offset + k), x)
            end do
            samples(repetition) = (wall_seconds() - started)/real(COLD_BATCH, dp)
        end do

        correct = validate(operation, result, inputs(total), x, a)
        call emit_row(workload, engine_name, "cold_distinct_expression", &
            samples, correct, COLD_WARMUPS, COLD_REPETITIONS, COLD_BATCH)
    end subroutine benchmark_cold

    function distinct_input(operation, index, x, y, a) result(e)
        integer, intent(in) :: operation, index
        type(expr_t), intent(in) :: x, y
        type(arena_t), intent(inout) :: a
        type(expr_t)        :: e
        type(expr_t) :: marker
        real(dp) :: shift
        integer :: i

        select case (operation)
        case (OP_SIMPLIFY)
            marker = sym(a, "cold_"//chars(str(index)))
            e = x + x + 2*x - 4*x + marker - marker
        case (OP_FLAT_SIMPLIFY)
            marker = sym(a, "flat_"//chars(str(index)))
            e = marker
            do i = 2, 64
                e = e + marker
            end do
        case (OP_DIFF)
            shift = real(index, dp)*1.0e-6_dp
            e = (x + shift)**8
        case (OP_EXPAND)
            shift = real(index, dp)*1.0e-6_dp
            e = (x + y + shift)**7
        end select
    end function distinct_input

    function perform(eng, operation, input, variable) result(r)
        class(engine_t), intent(inout) :: eng
        integer,         intent(in)    :: operation
        type(expr_t),    intent(in)    :: input, variable
        type(engine_result_t)          :: r

        select case (operation)
        case (OP_SIMPLIFY, OP_FLAT_SIMPLIFY)
            r = eng%simplify(input)
        case (OP_DIFF)
            r = eng%diff(input, variable)
        case (OP_EXPAND)
            r = eng%expand(input)
        end select
    end function perform

    function validate(operation, result, input, variable, a) result(correct)
        integer,               intent(in) :: operation
        type(engine_result_t), intent(in) :: result
        type(expr_t),          intent(in) :: input, variable
        type(arena_t),         intent(inout) :: a
        logical                           :: correct

        correct = .false.
        if (.not. result%ok) return

        select case (operation)
        case (OP_SIMPLIFY)
            correct = result%value == num(a, 0)
        case (OP_FLAT_SIMPLIFY)
            correct = result%value == num(a, 64)*input%arg(1)
        case (OP_DIFF)
            correct = derivative_agrees(result%value, input, 0.375_dp, -0.5_dp)
        case (OP_EXPAND)
            correct = values_agree(result%value, input, 0.375_dp, -0.5_dp)
        end select
    end function validate

    function values_agree(left, right, xv, yv) result(agree)
        type(expr_t), intent(in) :: left, right
        real(dp),     intent(in) :: xv, yv
        logical                  :: agree
        type(binding_t) :: bindings
        real(dp) :: lv, rv, scale
        logical :: left_ok, right_ok

        allocate (bindings%names(2), bindings%values(2))
        bindings%names(1) = str("x")
        bindings%names(2) = str("y")
        bindings%values(1) = xv
        bindings%values(2) = yv
        bindings%n = 2
        lv = eval_expr(left, bindings, left_ok)
        rv = eval_expr(right, bindings, right_ok)
        agree = .false.
        if (.not. left_ok) return
        if (.not. right_ok) return
        scale = max(1.0_dp, abs(lv), abs(rv))
        agree = abs(lv - rv) <= 1.0e-12_dp*scale
    end function values_agree

    function derivative_agrees(derivative, input, xv, yv) result(agree)
        type(expr_t), intent(in) :: derivative, input
        real(dp),     intent(in) :: xv, yv
        logical                  :: agree
        type(binding_t) :: bindings
        real(dp) :: symbolic, upper, lower, finite_difference, step, scale
        logical :: defined

        step = 1.0e-5_dp
        allocate (bindings%names(2), bindings%values(2))
        bindings%names(1) = str("x")
        bindings%names(2) = str("y")
        bindings%values(1) = xv
        bindings%values(2) = yv
        bindings%n = 2
        symbolic = eval_expr(derivative, bindings, defined)
        agree = .false.
        if (.not. defined) return
        bindings%values(1) = xv + step
        upper = eval_expr(input, bindings, defined)
        if (.not. defined) return
        bindings%values(1) = xv - step
        lower = eval_expr(input, bindings, defined)
        if (.not. defined) return
        finite_difference = (upper - lower)/(2*step)
        scale = max(1.0_dp, abs(symbolic), abs(finite_difference))
        agree = abs(symbolic - finite_difference) <= 1.0e-7_dp*scale
    end function derivative_agrees

    subroutine emit_row(workload, engine_name, scope, samples, correct, &
            warmups, repetitions, batch)
        character(*), intent(in) :: workload, engine_name, scope
        real(dp),     intent(in) :: samples(:)
        logical,      intent(in) :: correct
        integer,      intent(in) :: warmups, repetitions, batch
        character(5) :: correct_text

        if (correct) then
            correct_text = "true "
        else
            correct_text = "false"
        end if
        write (*, '(a,",",a,",",a,",",a,",",a,",",i0,",",i0,",",i0,'// &
            '",",es16.8,",",es16.8,",",a)') &
            "1", trim(compiler_version()), workload, engine_name, scope, &
            warmups, repetitions, batch, median(samples), minval(samples), &
            trim(correct_text)
    end subroutine emit_row

    function median(values) result(value)
        real(dp), intent(in) :: values(:)
        real(dp)             :: value
        real(dp), allocatable :: sorted(:)
        real(dp) :: key
        integer :: i, j

        sorted = values
        do i = 2, size(sorted)
            key = sorted(i)
            j = i - 1
            do while (j >= 1)
                if (sorted(j) <= key) exit
                sorted(j + 1) = sorted(j)
                j = j - 1
            end do
            sorted(j + 1) = key
        end do
        value = sorted((size(sorted) + 1)/2)
    end function median

end program bench_native
