program test_fortsym_lie
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(-), &
                            operator(*), operator(/), operator(**)
    use fortsym_lie, only: lie_series
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    implicit none
    type(arena_t), target :: arena, other
    type(expr_t) :: x, y, t, got, invalid, vars1(1), vec1(1), vars2(2), vec2(2)
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    logical :: ok
    character(:), allocatable :: why
    call arena%init()
    call other%init()
    x = sym(arena, 'x')
    y = sym(arena, 'y')
    t = sym(arena, 't')
    vars1(1) = x
    vec1(1) = num(arena, 1)
    vars2(1) = x
    vars2(2) = y
    vec2(1) = -y
    vec2(2) = x
    engine = make_symengine_engine(arena)
    call suite_begin(suite, 'scalar Lie pullback')
    ! Exact translation is an independent finite-flow oracle.
    call lie_series(x**3, vars1, vec1, t, 3, got, ok, why)
    if (.not. ok) error stop 'translation refused'
    call check_identity(suite, engine, 'translation', got - (x + t)**3)
    ! Rotation flow: x(t)=x cos(t)-y sin(t), through cubic order.
    call lie_series(x, vars2, vec2, t, 3, got, ok, why)
    if (.not. ok) error stop 'rotation refused'
    call check_identity(suite, engine, 'rotation Taylor polynomial', &
                        got - (x - t*y - t**2*x/2 + t**3*y/6))
    call lie_series(x*x + y*y, vars2, vec2, t, 5, got, ok, why)
    if (.not. ok) error stop 'invariant refused'
    call check_identity(suite, engine, 'rotation invariant', got - x*x - y*y)
    vec1(1) = x
    call lie_series(x, vars1, vec1, t, 0, got, ok, why)
    if (.not. ok) error stop 'order zero refused'
    call check_identity(suite, engine, 'order zero', got - x)
    call lie_series(x, vars1, vec1, t, -1, got, ok, why)
    if (ok) error stop 'negative order accepted'
    call lie_series(x, vars1, vec1, t, 13, got, ok, why)
    if (ok) error stop 'unbounded order accepted'
    vars2(2) = x
    call lie_series(x, vars2, vec2, t, 2, got, ok, why)
    if (ok) error stop 'duplicate coordinates accepted'
    call lie_series(x, vars1, vec1, x, 2, got, ok, why)
    if (ok) error stop 'coordinate-dependent parameter accepted'
    vec1(1) = sym(other, 'y')
    call lie_series(x, vars1, vec1, t, 2, got, ok, why)
    if (ok) error stop 'foreign arena accepted'
    vec1(1) = x
    call lie_series(invalid, vars1, vec1, t, 2, got, ok, why)
    if (ok) error stop 'invalid scalar accepted'
    call suite_end(suite)
end program test_fortsym_lie
