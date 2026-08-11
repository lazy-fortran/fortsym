program bench_complexdom
    ! Reproducible cold and warm timing for complex-domain operations. Cold
    ! split rows clear the context cache before every call; warm split rows
    ! reuse the same prebuilt expression and cached pair of node ids.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_assume, only: assumption_context_t, assume, real_valued
    use fortsym_complexdom, only: complex_split, conjugate
    use fortsym_engine, only: wall_seconds
    use fortsym_expr, only: expr_t, sym, i_expr, sinh, cosh, tan, tanh, operator(+), &
        operator(*)
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: ITERATIONS = 10000

    write (*, '(a)') "schema,backend,scope,workload,iterations,seconds,correct"
    call benchmark_scope("cold", "sinh_cosh_split")
    call benchmark_scope("warm", "sinh_cosh_split")
    call benchmark_scope("cold", "tan_split")
    call benchmark_scope("warm", "tan_split")
    call benchmark_scope("cold", "tanh_split")
    call benchmark_scope("warm", "tanh_split")
    call benchmark_scope("cold", "conjugate_tan")
    call benchmark_scope("warm", "conjugate_tan")
    call benchmark_scope("cold", "conjugate_tanh")
    call benchmark_scope("warm", "conjugate_tanh")

contains

    subroutine benchmark_scope(scope, workload)
        character(*), intent(in) :: scope
        character(*), intent(in) :: workload
        type(arena_t), target :: arena
        type(assumption_context_t) :: facts
        type(expr_t) :: x, y, z, sinh_input, cosh_input, tan_input, tanh_input, &
            re, im
        real(dp) :: started, elapsed
        logical :: ok, correct
        character(:), allocatable :: why
        integer :: i

        call arena%init()
        call facts%init(arena)
        x = sym(arena, "x")
        y = sym(arena, "y")
        call assume(facts, real_valued(x))
        call assume(facts, real_valued(y))
        z = x + i_expr(arena)*y
        sinh_input = sinh(z)
        cosh_input = cosh(z)
        tan_input = tan(z)
        tanh_input = tanh(z)

        if (scope == "warm") then
            call run_workload(workload, 0, facts, sinh_input, cosh_input, &
                tan_input, tanh_input, re, im, ok, why)
            correct = ok
            if (workload == "sinh_cosh_split") then
                call run_workload(workload, 1, facts, sinh_input, cosh_input, &
                    tan_input, tanh_input, re, im, ok, why)
                correct = correct .and. ok
            end if
        else
            correct = .true.
        end if

        started = wall_seconds()
        do i = 1, ITERATIONS
            if (scope == "cold") then
                call facts%complex_cache%clear()
                call facts%conjugate_cache%clear()
            end if
            call run_workload(workload, i, facts, sinh_input, cosh_input, &
                tan_input, tanh_input, re, im, ok, why)
            correct = correct .and. ok
        end do
        elapsed = wall_seconds() - started
        write (*, '(a,",",a,",",a,",",a,",",i0,",",f12.6,",",l1)') &
            "complexdom_v1", "native", trim(scope), trim(workload), &
            ITERATIONS, elapsed, correct
    end subroutine benchmark_scope

    subroutine run_workload(workload, iteration, facts, sinh_input, cosh_input, &
            tan_input, tanh_input, first, second, ok, why)
        character(*), intent(in) :: workload
        integer,      intent(in) :: iteration
        type(assumption_context_t), target, intent(in) :: facts
        type(expr_t), intent(in) :: sinh_input, cosh_input, tan_input, tanh_input
        type(expr_t), intent(out) :: first, second
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        select case (workload)
        case ("sinh_cosh_split")
            if (mod(iteration, 2) == 0) then
                call complex_split(sinh_input, facts, first, second, ok, why)
            else
                call complex_split(cosh_input, facts, first, second, ok, why)
            end if
        case ("tanh_split")
            call complex_split(tanh_input, facts, first, second, ok, why)
        case ("tan_split")
            call complex_split(tan_input, facts, first, second, ok, why)
        case ("conjugate_tan")
            call conjugate(tan_input, facts, first, ok, why)
            second = first
        case ("conjugate_tanh")
            call conjugate(tanh_input, facts, first, ok, why)
            second = first
        case default
            ok = .false.
            why = "unknown benchmark workload "//workload
        end select
    end subroutine run_workload

end program bench_complexdom
