program test_fortsym_algebraic_expr
    ! The bridge supplies the independent algebraic oracle. The native engine
    ! must preserve its canonical qqbar1 value while combining arena nodes.
    use fortsym_algebraic, only: algebraic_i, algebraic_conjugate, &
        algebraic_add, algebraic_sqrt, algebraic_mul
    use fortsym_arena, only: arena_t, NK_ALGEBRAIC
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, algebraic_expr, operator(+), operator(*), &
        operator(==), is_valid, sym, num
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars, str, str_t
    implicit none

    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(symengine_engine_t) :: symengine
    type(expr_t) :: minus_i, i_atom, root, expected, x, derivative
    type(engine_result_t) :: result, zero_result
    type(str_t) :: i_text, minus_i_text, expected_text, root_text
    type(str_t) :: minus_two_text
    logical :: good
    integer :: nfail

    nfail = 0
    call arena%init()
    engine = make_native_engine(arena)
    symengine = make_symengine_engine(arena)
    x = sym(arena, "x")

    i_text = algebraic_i(good)
    call check("algebraic i oracle is available", good)
    i_atom = algebraic_expr(arena, chars(i_text), good)
    call check("algebraic atom is accepted", good .and. is_valid(i_atom))
    call check("algebraic atom has its own node kind", &
        i_atom%kind() == NK_ALGEBRAIC)
    call check("algebraic atom retains canonical text", &
        chars(i_atom%algebraic_text()) == chars(i_text))

    minus_i_text = algebraic_conjugate(chars(i_text), good)
    minus_i = algebraic_expr(arena, chars(minus_i_text), good)
    root = i_atom + minus_i
    result = engine%simplify(root)
    expected_text = algebraic_add(chars(i_text), chars(minus_i_text), good)
    expected = algebraic_expr(arena, chars(expected_text), good)
    call check("native algebraic addition succeeds", result%ok)
    call check("native algebraic addition matches bridge", &
        result%ok .and. result%value == expected)
    zero_result = engine%zero_test(root)
    call check("native algebraic zero test is proved", &
        zero_result%verdict == VERDICT_TRUE)

    minus_two_text = str("qqbar1:0:2,1")
    root_text = algebraic_sqrt(chars(minus_two_text), good)
    ! The independent bridge result is used directly for the next operation.
    root = algebraic_expr(arena, chars(root_text), good)
    result = engine%simplify(root*root)
    expected_text = algebraic_mul(chars(root_text), chars(root_text), good)
    expected = algebraic_expr(arena, chars(expected_text), good)
    call check("native algebraic multiplication succeeds", result%ok)
    call check("native algebraic multiplication matches bridge", &
        result%ok .and. result%value == expected)
    call check("native algebraic atom prints canonically", &
        chars(print_expr(root)) == chars(root_text))
    derivative = diff(root, x)
    call check("algebraic atoms differentiate as constants", &
        derivative == num(arena, 0))

    zero_result = symengine%zero_test(i_atom)
    call check("symengine refuses algebraic zero testing", &
        .not. zero_result%ok)
    result = symengine%simplify(i_atom)
    call check("symengine refuses algebraic simplification", .not. result%ok)

    if (nfail /= 0) then
        print *, "test_fortsym_algebraic_expr: ", nfail, " check(s) FAILED"
        error stop 1
    end if
    print *, "test_fortsym_algebraic_expr: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check

end program test_fortsym_algebraic_expr
