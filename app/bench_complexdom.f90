program bench_complexdom
    ! Reproducible cold and warm timing for the assumption-context complex
    ! split cache. Cold rows clear the context cache before every call; warm
    ! rows reuse the same prebuilt expression and cached pair of node ids.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_arena, only: arena_t
    use fortsym_assume, only: assumption_context_t, assume, real_valued
    use fortsym_complexdom, only: complex_split
    use fortsym_engine, only: wall_seconds
    use fortsym_expr, only: expr_t, sym, i_expr, sinh, cosh, operator(+), &
        operator(*)
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: ITERATIONS = 10000

    write (*, '(a)') "schema,backend,scope,workload,iterations,seconds,correct"
    call benchmark_scope("cold")
    call benchmark_scope("warm")

contains

    subroutine benchmark_scope(scope)
        character(*), intent(in) :: scope
        type(arena_t), target :: arena
        type(assumption_context_t) :: facts
        type(expr_t) :: x, y, z, sinh_input, cosh_input, re, im
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

        if (scope == "warm") then
            call complex_split(sinh_input, facts, re, im, ok, why)
            correct = ok
            call complex_split(cosh_input, facts, re, im, ok, why)
            correct = correct .and. ok
        else
            correct = .true.
        end if

        started = wall_seconds()
        do i = 1, ITERATIONS
            if (scope == "cold") call facts%complex_cache%clear()
            if (mod(i, 2) == 0) then
                call complex_split(sinh_input, facts, re, im, ok, why)
            else
                call complex_split(cosh_input, facts, re, im, ok, why)
            end if
            correct = correct .and. ok
        end do
        elapsed = wall_seconds() - started
        write (*, '(a,",",a,",",a,",",a,",",i0,",",f12.6,",",l1)') &
            "complexdom_v1", "native", trim(scope), "sinh_cosh_split", &
            ITERATIONS, elapsed, correct
    end subroutine benchmark_scope

end program bench_complexdom
