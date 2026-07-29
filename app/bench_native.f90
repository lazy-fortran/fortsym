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
    use fortsym_string, only: str
    use fortsym_engine, only: engine_t, engine_result_t, wall_seconds
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: WARMUPS = 5
    integer, parameter :: REPETITIONS = 31
    integer, parameter :: BATCH = 50
    integer, parameter :: OP_SIMPLIFY = 1
    integer, parameter :: OP_DIFF = 2
    integer, parameter :: OP_EXPAND = 3

    type(arena_t), target :: arena
    type(native_engine_t) :: native
    type(symengine_engine_t) :: symengine
    type(expr_t) :: x, y, simplify_input, diff_input, expand_input

    call arena%init()
    native = make_native_engine(arena)
    symengine = make_symengine_engine(arena)
    x = sym(arena, "x")
    y = sym(arena, "y")

    simplify_input = x + x + 2*x - 4*x + &
        rat(arena, 1_int64, 2_int64) + &
        rat(arena, 1_int64, 3_int64) - &
        rat(arena, 5_int64, 6_int64)
    diff_input = (x + 1)**8
    expand_input = (x + y + 1)**7

    write (*, '(a)') "schema,compiler,workload,engine,scope,warmups,"// &
        "repetitions,batch,median_seconds,min_seconds,correct"
    call benchmark(native, "native", OP_SIMPLIFY, "simplify_collect", &
        simplify_input, x)
    call benchmark(symengine, "symengine", OP_SIMPLIFY, "simplify_collect", &
        simplify_input, x)
    call benchmark(native, "native", OP_DIFF, "differentiate_power", &
        diff_input, x)
    call benchmark(symengine, "symengine", OP_DIFF, "differentiate_power", &
        diff_input, x)
    call benchmark(native, "native", OP_EXPAND, "expand_power", &
        expand_input, x)
    call benchmark(symengine, "symengine", OP_EXPAND, "expand_power", &
        expand_input, x)

contains

    subroutine benchmark(eng, engine_name, operation, workload, input, variable)
        class(engine_t), intent(inout) :: eng
        character(*),    intent(in)    :: engine_name, workload
        integer,         intent(in)    :: operation
        type(expr_t),    intent(in)    :: input, variable
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

        correct = validate(operation, result, input)
        call emit_row(workload, engine_name, samples, correct)
    end subroutine benchmark

    function perform(eng, operation, input, variable) result(r)
        class(engine_t), intent(inout) :: eng
        integer,         intent(in)    :: operation
        type(expr_t),    intent(in)    :: input, variable
        type(engine_result_t)          :: r

        select case (operation)
        case (OP_SIMPLIFY)
            r = eng%simplify(input)
        case (OP_DIFF)
            r = eng%diff(input, variable)
        case (OP_EXPAND)
            r = eng%expand(input)
        end select
    end function perform

    function validate(operation, result, input) result(correct)
        integer,               intent(in) :: operation
        type(engine_result_t), intent(in) :: result
        type(expr_t),          intent(in) :: input
        logical                           :: correct
        type(expr_t) :: expected

        correct = .false.
        if (.not. result%ok) return

        select case (operation)
        case (OP_SIMPLIFY)
            correct = result%value == num(arena, 0)
        case (OP_DIFF)
            expected = 8*(x + 1)**7
            correct = values_agree(result%value, expected, 0.375_dp, -0.5_dp)
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

        bindings%names = [str("x"), str("y")]
        bindings%values = [xv, yv]
        bindings%n = 2
        lv = eval_expr(left, bindings, left_ok)
        rv = eval_expr(right, bindings, right_ok)
        agree = .false.
        if (.not. left_ok) return
        if (.not. right_ok) return
        scale = max(1.0_dp, abs(lv), abs(rv))
        agree = abs(lv - rv) <= 1.0e-12_dp*scale
    end function values_agree

    subroutine emit_row(workload, engine_name, samples, correct)
        character(*), intent(in) :: workload, engine_name
        real(dp),     intent(in) :: samples(:)
        logical,      intent(in) :: correct
        character(5) :: correct_text

        if (correct) then
            correct_text = "true "
        else
            correct_text = "false"
        end if
        write (*, '(a,",",a,",",a,",",a,",",a,",",i0,",",i0,",",i0,'// &
            '",",es16.8,",",es16.8,",",a)') &
            "1", trim(compiler_version()), workload, engine_name, "end_to_end", &
            WARMUPS, REPETITIONS, BATCH, median(samples), minval(samples), &
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
