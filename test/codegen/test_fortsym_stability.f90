program test_fortsym_stability
    use, intrinsic :: iso_fortran_env, only: real64, real128
    use fortsym_string, only: str
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sqrt, operator(+), operator(-)
    use fortsym_stability, only: rationalize_sqrt_difference
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_eval, only: binding_t, eval_expr
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: qp = real128
    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(expr_t) :: a, b, raw, stable
    type(engine_result_t) :: result
    type(binding_t) :: bindings
    real(dp) :: av, bv, raw_value, stable_value
    real(qp) :: reference
    logical :: defined
    integer :: nfail

    nfail = 0
    call arena%init()
    engine = make_symengine_engine(arena)
    a = sym(arena, "a")
    b = sym(arena, "b")
    raw = sqrt(a) - sqrt(b)
    stable = rationalize_sqrt_difference(a, b)

    result = engine%zero_test(raw - stable)
    call check("symbolic equivalence", &
        result%ok .and. result%verdict == VERDICT_TRUE)

    av = nearest(1.0_dp, 1.0_dp)
    bv = 1.0_dp
    bindings%n = 2
    allocate (bindings%names(2), bindings%values(2))
    bindings%names(1) = str("a")
    bindings%names(2) = str("b")
    bindings%values = [av, bv]
    raw_value = eval_expr(raw, bindings, defined)
    call check("raw boundary value defined", defined)
    stable_value = eval_expr(stable, bindings, defined)
    call check("stable boundary value defined", defined)
    reference = sqrt(real(av, qp)) - sqrt(real(bv, qp))
    call check("rationalization reduces cancellation error", &
        abs(real(stable_value, qp) - reference) < &
        abs(real(raw_value, qp) - reference))
    call check("stable boundary retains a nonzero increment", &
        stable_value > 0.0_dp .and. raw_value == 0.0_dp)

    if (nfail /= 0) error stop 1
    print *, "test_fortsym_stability: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check

end program test_fortsym_stability
