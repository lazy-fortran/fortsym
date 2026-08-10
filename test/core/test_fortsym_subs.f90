program test_fortsym_subs
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, func, operator(+), operator(*), &
        operator(**), operator(==)
    use fortsym_subs, only: subs, subs_many
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: x, y, f, got, expected
    type(expr_t) :: old(2), new(2), function_arguments(1)
    integer :: passed, failed

    passed = 0
    failed = 0
    call arena%init()
    x = sym(arena, "x")
    y = sym(arena, "y")

    f = (x + y)*(x + y)
    got = subs(f, x + y, x)
    expected = x*x
    call check(got == expected, "arbitrary shared subexpression")

    old(1) = x
    old(2) = y
    new(1) = y
    new(2) = x
    got = subs_many(x + 2*y, old, new)
    expected = y + 2*x
    call check(got == expected, "simultaneous substitution does not cascade")

    function_arguments(1) = x
    f = func("Derivative_Bt", function_arguments)
    got = subs(f, f, y)
    expected = y
    call check(got == expected, "named derivative replacement")

    print '(a,i0,a,i0)', "subs: ", passed, " passed, ", failed, " failed"
    if (failed > 0) stop 1

contains

    subroutine check(condition, label)
        logical, intent(in) :: condition
        character(*), intent(in) :: label

        if (condition) then
            passed = passed + 1
        else
            failed = failed + 1
            print '(a,a)', "FAIL: ", label
        end if
    end subroutine check

end program test_fortsym_subs
