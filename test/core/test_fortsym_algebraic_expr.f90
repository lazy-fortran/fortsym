program test_fortsym_algebraic_expr
    ! The bridge supplies the independent algebraic oracle. The native engine
    ! must preserve exact meaning while combining algebraic coefficients in the
    ! arena.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_algebraic, only: algebraic_i, algebraic_from_re_im, &
        algebraic_conjugate, algebraic_add, algebraic_sqrt, algebraic_mul, &
        algebraic_signs, algebraic_pow
    use fortsym_arena, only: arena_t, NK_ALGEBRAIC
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_FALSE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine_symengine, only: symengine_engine_t, &
        make_symengine_engine
    use fortsym_diff, only: diff
    use fortsym_expr, only: expr_t, algebraic_expr, operator(+), operator(-), &
        operator(*), operator(**), operator(==), is_valid, sym, num, rat, i_expr
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars, str, str_t
    implicit none

    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(symengine_engine_t) :: symengine
    type(expr_t) :: minus_i, i_atom, root, expected, gaussian
    type(expr_t) :: zero_atom, one_atom
    type(expr_t) :: gaussian_expected, x, derivative
    type(engine_result_t) :: result, zero_result
    type(str_t) :: i_text, minus_i_text, expected_text, root_text
    type(str_t) :: gaussian_text
    type(str_t) :: minus_two_text, two_text, sqrt_two_text
    type(str_t) :: two_sqrt_two_text, half_text, mixed_text
    type(str_t) :: zero_text, one_text
    logical :: good
    integer :: real_sign, imag_sign
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
    call algebraic_signs(chars(expected_text), real_sign, imag_sign, good)
    call check("native algebraic addition succeeds", result%ok)
    call check("native algebraic addition canonicalizes to zero", &
        result%ok .and. result%value == num(arena, 0_int64))
    call check("bridge confirms algebraic addition is zero", &
        good .and. real_sign == 0 .and. imag_sign == 0)
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

    zero_text = algebraic_from_re_im("0", "0", good)
    zero_atom = algebraic_expr(arena, chars(zero_text), good)
    one_text = algebraic_from_re_im("1", "0", good)
    one_atom = algebraic_expr(arena, chars(one_text), good)
    result = engine%simplify(zero_atom**num(arena, 2_int64))
    call check("algebraic zero is recognised by power simplification", &
        result%ok .and. result%value == num(arena, 0_int64))
    result = engine%simplify(one_atom**num(arena, 7_int64))
    call check("algebraic one is recognised by power simplification", &
        result%ok .and. result%value == num(arena, 1_int64))

    two_text = algebraic_from_re_im("2", "0", good)
    sqrt_two_text = algebraic_sqrt(chars(two_text), good)
    root = algebraic_expr(arena, chars(sqrt_two_text), good)
    result = engine%simplify(root*x + root*x)
    two_sqrt_two_text = algebraic_mul(chars(two_text), chars(sqrt_two_text), &
        good)
    expected = algebraic_expr(arena, chars(two_sqrt_two_text), good)*x
    call check("native collects repeated algebraic coefficients", &
        result%ok .and. result%value == expected)

    half_text = algebraic_from_re_im("1/2", "0", good)
    result = engine%simplify(root*x + rat(arena, 1_int64, 2_int64)*x)
    mixed_text = algebraic_add(chars(sqrt_two_text), chars(half_text), good)
    expected = algebraic_expr(arena, chars(mixed_text), good)*x
    call check("native combines algebraic and rational coefficients", &
        result%ok .and. result%value == expected)

    result = engine%simplify(root*root*x)
    expected = algebraic_expr(arena, chars(two_text), good)*x
    call check("native multiplies algebraic coefficients", &
        result%ok .and. result%value == expected)

    result = engine%simplify((root**num(arena, 2_int64))*x)
    expected = algebraic_expr(arena, chars(two_text), good)*x
    call check("native powers algebraic coefficients", &
        result%ok .and. result%value == expected)

    result = engine%solve(root*x - num(arena, 1_int64), x)
    expected_text = algebraic_pow(chars(sqrt_two_text), -1_int64, good)
    expected = algebraic_expr(arena, chars(expected_text), good)
    call check("native solves with an algebraic nonzero coefficient", &
        result%ok .and. result%value == expected)
    call check("algebraic coefficient solve needs no nonzero condition", &
        result%ok .and. .not. result%conditional)

    gaussian_text = algebraic_from_re_im("1/2", "3/4", good)
    call check("Gaussian-rational algebraic oracle is available", good)
    gaussian = algebraic_expr(arena, chars(gaussian_text), good)
    call check("Gaussian-rational algebraic atom is accepted", &
        good .and. is_valid(gaussian))
    gaussian_expected = rat(arena, 1_int64, 2_int64) + &
        rat(arena, 3_int64, 4_int64)*i_expr(arena)
    result = symengine%simplify(gaussian)
    call check("SymEngine simplifies Gaussian-rational algebraic atoms", &
        result%ok .and. result%value == gaussian_expected)
    zero_result = symengine%zero_test(gaussian)
    call check("SymEngine proves Gaussian-rational algebraic nonzero", &
        zero_result%ok .and. zero_result%verdict == VERDICT_FALSE)
    result = symengine%diff(gaussian, x)
    call check("SymEngine differentiates Gaussian-rational constants", &
        result%ok .and. result%value == num(arena, 0))

    zero_result = symengine%zero_test(root)
    call check("SymEngine refuses irrational algebraic zero testing", &
        .not. zero_result%ok)
    result = symengine%simplify(root)
    call check("SymEngine refuses irrational algebraic simplification", &
        .not. result%ok)

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
