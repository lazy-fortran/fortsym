program test_external_assumptions
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sqrt, abs, operator(**), &
        operator(-), operator(+)
    use fortsym_assume, only: assumption_context_t, assume, positive, &
        nonnegative, nonzero, real_valued
    use fortsym_engine_ext, only: make_sympy_engine, sympy_engine_t
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_UNKNOWN
    use fortsym_string, only: chars
    implicit none
    type(arena_t), target :: arena, other
    type(assumption_context_t), target :: facts, unsupported, foreign
    type(sympy_engine_t) :: plain, constrained
    type(expr_t) :: x, y, e
    type(engine_result_t) :: result
    logical :: ok
    call arena%init()
    call other%init()
    call facts%init(arena)
    call unsupported%init(arena)
    call foreign%init(other)
    x = sym(arena, 'x')
    y = sym(arena, 'y')
    call assume(facts, positive(x), ok)
    if (.not. ok) error stop 'positive fact'
    call assume(facts, nonnegative(y), ok)
    if (.not. ok) error stop 'nonnegative fact'
    call assume(facts, nonzero(y), ok)
    if (.not. ok) error stop 'nonzero fact'
    call assume(facts, real_valued(y), ok)
    if (.not. ok) error stop 'real fact'
    call assume(unsupported, positive(x+1), ok)
    if (.not. ok) error stop 'composite fact setup'
    constrained = make_sympy_engine(arena, unsupported)
    result = constrained%simplify(x)
    if (result%ok) error stop 'composite assumption silently dropped'
    if (index(chars(result%message),'assumption context') == 0) &
        error stop 'missing refusal diagnostic'
    constrained = make_sympy_engine(arena, foreign)
    result = constrained%simplify(x)
    if (result%ok) error stop 'foreign assumption context accepted'
    plain = make_sympy_engine(arena)
    if (.not. plain%available) stop 77
    constrained = make_sympy_engine(arena, facts)
    ! At x=-1 both unconstrained residuals equal 2, so they cannot be identities.
    e = sqrt(x**2)-x
    result = plain%zero_test(e)
    if (result%verdict /= VERDICT_UNKNOWN) error stop 'unconstrained sqrt'
    result = constrained%zero_test(e)
    if (result%verdict /= VERDICT_TRUE) error stop 'positive sqrt'
    if (.not. result%conditional) error stop 'result conditions omitted'
    if (index(chars(result%condition),'positive=True')==0) error stop 'condition facts lost'
    e = abs(x)-x
    result = plain%zero_test(e)
    if (result%verdict /= VERDICT_UNKNOWN) error stop 'unconstrained abs'
    result = constrained%zero_test(e)
    if (result%verdict /= VERDICT_TRUE) error stop 'positive abs'
    e = sqrt(y**2)-y
    result = constrained%zero_test(e)
    if (result%verdict /= VERDICT_TRUE) error stop 'nonnegative real sqrt'
    print *, 'external assumptions and negative domain witnesses PASS'
end program
