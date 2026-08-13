program test_fortsym_convenience
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: str_t, chars
    implicit none

    type(arena_t), target :: explicit_arena, concurrent_arena
    type(arena_t), pointer :: default_storage
    type(assumption_context_t) :: base_context, positive_context, nonnegative_context
    type(assumption_context_t) :: integer_context
    type(assumption_context_t) :: rational_context
    type(assumption_context_t) :: relation_context
    type(assumption_context_t) :: foreign_context
    type(expr_t) :: x, mu, sigma, best, xi, literal
    type(expr_t) :: bulk_mu, bulk_sigma, bulk_best, bulk_xi
    type(expr_t) :: explicit_mu, explicit_sigma, explicit_best, explicit_xi
    type(expr_t) :: concurrent_mu
    type(expr_t) :: explicit_expression, default_expression, mixed
    type(expr_t) :: sine, substituted, derivative, simplified, expanded, factored
    type(expr_t) :: series_value, series_expected, series_coefficient
    type(expr_t) :: huge_integer, exact_fraction, relation
    type(expr_t) :: substitution_old(2), substitution_new(2)
    type(expr_t) :: free_expression
    type(expr_t), allocatable :: roots(:)
    type(expr_t) :: linear_matrix(2, 2), linear_right_hand_side(2)
    type(expr_t), allocatable :: linear_values(:)
    type(expr_t) :: determinant_row_one(2), determinant_row_two(2)
    type(expr_t) :: determinant_rows(2), determinant_matrix
    type(expr_t) :: inverse_row, inverse_entry
    type(str_t), allocatable :: free_names(:)
    type(expr_t) :: stale
    type(engine_result_t) :: result, zero_result
    logical :: good, exact_good, context_ok
    character(:), allocatable :: why
    integer :: failures, root_count

    failures = 0
    call explicit_arena%init()
    call concurrent_arena%init()
    explicit_mu = sym(explicit_arena, "mu")
    explicit_sigma = sym(explicit_arena, "sigma")
    explicit_best = sym(explicit_arena, "best")
    explicit_xi = sym(explicit_arena, "xi")
    explicit_expression = (explicit_best - explicit_xi - explicit_mu)/explicit_sigma
    concurrent_mu = sym(concurrent_arena, "mu")
    call check("explicit arenas remain independent", &
        .not. same_arena(explicit_mu, concurrent_mu), failures)

    call reset()
    x = "x"
    mu = "mu"
    sigma = "sigma"
    best = "best"
    xi = "xi"
    default_expression = (best - xi - mu)/sigma

    call check("character assignment creates a symbol", &
        x%kind() == NK_SYM .and. chars(print_expr(x)) == "x", failures)
    literal = "x+y"
    call check("character assignment does not parse", &
        literal%kind() == NK_SYM, failures)
    call check("explicit and default routes have the same structure", &
        chars(print_expr(explicit_expression)) == &
        chars(print_expr(default_expression)), failures)

    call symbols("mu sigma best xi", bulk_mu, bulk_sigma, bulk_best, bulk_xi, &
        ok=good)
    call check("bulk symbol helper accepts four names", good, failures)
    call check("bulk helper preserves names", &
        chars(print_expr(bulk_best)) == "best", failures)

    default_storage => default_arena()
    call check("default and explicit constructors share the default arena", &
        same_arena(mu, sym(default_storage, "mu")), failures)
    huge_integer = exact(default_storage, "9223372036854775808", exact_good)
    call check("facade accepts arbitrary-precision integers", &
        exact_good .and. chars(huge_integer%exact_text()) == &
        "9223372036854775808", failures)
    exact_fraction = exact(default_storage, "6/-8", exact_good)
    call check("facade canonicalizes arbitrary-precision rationals", &
        exact_good .and. exact_fraction == rat(default_storage, -3_int64, 4_int64), &
        failures)
    mixed = mu + sigma
    call check("default expression participates in ordinary operators", &
        is_valid(mixed), failures)
    call check("facade exposes SymPy-style operation counting", &
        operation_count((mu + 1)**2) == 2, failures)
    free_expression = sin(mu) + sigma + pi_expr(default_storage)
    call free_symbols(free_expression, free_names)
    call check("facade exposes distinct free symbols", &
        size(free_names) == 2 .and. &
        contains_name(free_names, "mu") .and. &
        contains_name(free_names, "sigma"), failures)
    sine = sin(mu)
    call check("facade uses the intrinsic spelling for expression functions", &
        chars(print_expr(sine)) == "sin(mu)", failures)
    result = subs(mu + sigma, mu, sigma)
    substituted = result%value
    call check("facade exposes structural substitution", &
        result%ok .and. substituted == sigma + sigma, failures)
    substitution_old(1) = mu
    substitution_old(2) = sigma
    substitution_new(1) = sigma
    substitution_new(2) = mu
    result = subs_many(mu + 2*sigma, substitution_old, substitution_new)
    call check("facade exposes simultaneous substitution", &
        result%ok .and. result%value == sigma + 2*mu, failures)
    result = diff(mu*mu, mu)
    derivative = result%value
    call check("facade exposes evaluated differentiation", &
        result%ok .and. derivative == 2*mu, failures)
    result = simplify(mu + 0)
    simplified = result%value
    call check("facade exposes native simplification", &
        result%ok .and. simplified == mu, failures)
    result = expand((mu + 1)*(mu + 2))
    expanded = result%value
    call check("facade exposes native expansion", &
        result%ok .and. expanded == mu**2 + 3*mu + 2, failures)
    result = factor(mu**2 + 2*mu + 1)
    factored = result%value
    call check("facade exposes native factorisation", &
        result%ok .and. factored == (mu + 1)**2, failures)
    call solve((mu - 1)**2, mu, roots, root_count, good, why)
    call check("facade exposes distinct verified roots", &
        good .and. root_count == 1 .and. roots(1) == num(default_storage, 1_int64), &
        failures)
    linear_matrix(1, 1) = num(default_storage, 1_int64)
    linear_matrix(1, 2) = num(default_storage, 2_int64)
    linear_matrix(2, 1) = num(default_storage, 3_int64)
    linear_matrix(2, 2) = num(default_storage, 4_int64)
    linear_right_hand_side(1) = num(default_storage, 5_int64)
    linear_right_hand_side(2) = num(default_storage, 6_int64)
    call linsolve(linear_matrix, linear_right_hand_side, linear_values, good, why)
    call check("facade exposes verified exact linsolve", &
        good .and. size(linear_values) == 2 .and. &
        linear_values(1) == num(default_storage, -4_int64) .and. &
        linear_values(2) == rat(default_storage, 9_int64, 2_int64), failures)
    determinant_row_one(1) = num(default_storage, 1_int64)
    determinant_row_one(2) = num(default_storage, 2_int64)
    determinant_row_two(1) = num(default_storage, 3_int64)
    determinant_row_two(2) = num(default_storage, 4_int64)
    determinant_rows(1) = func("List", determinant_row_one)
    determinant_rows(2) = func("List", determinant_row_two)
    determinant_matrix = func("List", determinant_rows)
    result = det(determinant_matrix)
    call check("facade exposes exact matrix determinant", &
        result%ok .and. result%value == num(default_storage, -2_int64), failures)
    result = rank(determinant_matrix)
    call check("facade exposes exact matrix rank", &
        result%ok .and. result%value == num(default_storage, 2_int64), failures)
    result = inv(determinant_matrix)
    inverse_row = result%value%arg(1)
    inverse_entry = inverse_row%arg(1)
    call check("facade exposes exact matrix inverse", &
        result%ok .and. inverse_entry == num(default_storage, -2_int64), failures)
    result = series(exp(mu), mu, num(default_storage, 0_int64), 3)
    series_value = result%value
    series_expected = 1 + mu + mu**2/2 + mu**3/6
    zero_result = zero_test(series_value - series_expected)
    call check("facade exposes bounded Taylor series", &
        result%ok .and. zero_result%ok .and. &
        zero_result%verdict == VERDICT_TRUE, failures)
    result = series_coeff((mu + 2)**4, mu, num(default_storage, 0_int64), 2)
    series_coefficient = result%value
    call check("facade exposes exact Taylor coefficients", &
        result%ok .and. series_coefficient == num(default_storage, 24_int64), &
        failures)
    result = series(1/mu, mu, num(default_storage, 0_int64), 2)
    call check("facade refuses singular Taylor coefficients", .not. result%ok, failures)
    result = re_part(i_expr(default_storage))
    call check("facade exposes the real-part operation", &
        result%ok .and. result%value == num(default_storage, 0_int64), failures)
    result = im_part(i_expr(default_storage))
    call check("facade exposes the imaginary-part operation", &
        result%ok .and. result%value == num(default_storage, 1_int64), failures)
    result = conjugate(i_expr(default_storage))
    zero_result = zero_test(result%value + i_expr(default_storage))
    call check("facade conjugation has the independent inverse identity", &
        result%ok .and. zero_result%ok .and. &
        zero_result%verdict == VERDICT_TRUE, &
        failures)
    result = arg_of(num(default_storage, -1_int64))
    call check("facade exposes the principal argument", &
        result%ok .and. result%value == pi_expr(default_storage), failures)
    result = abs_of(i_expr(default_storage))
    call check("facade exposes the complex modulus", &
        result%ok .and. result%value == num(default_storage, 1_int64), failures)
    result = abs_of(num(default_storage, 2_int64) + i_expr(default_storage))
    call check("complex modulus keeps exact rectangular values", &
        result%ok .and. result%value == sqrt(num(default_storage, 5_int64)), failures)
    result = abs_of(mu)
    call check("native complex modulus refuses unknown reality", .not. result%ok, failures)
    result = complex_expand(i_expr(default_storage))
    call check("facade exposes rectangular complex expansion", &
        result%ok .and. result%value == i_expr(default_storage), failures)
    result = complex_expand(exp(i_expr(default_storage)))
    call check("complex expansion preserves the principal exponential form", &
        result%ok .and. result%value == cos(num(default_storage, 1_int64)) + &
        i_expr(default_storage)*sin(num(default_storage, 1_int64)), failures)
    result = complex_expand(oo_expr(default_storage))
    call check("complex expansion preserves positive infinity", &
        result%ok .and. result%value == oo_expr(default_storage), failures)
    result = complex_expand(zoo_expr(default_storage))
    call check("complex expansion maps complex infinity to undefined", &
        result%ok .and. result%value == nan_expr(default_storage), failures)
    result = complex_expand(nan_expr(default_storage))
    call check("complex expansion preserves undefined", &
        result%ok .and. result%value == nan_expr(default_storage), failures)
    result = complex_expand(mu)
    call check("native complex expansion refuses unknown reality", .not. result%ok, failures)
    result = re_part(mu)
    call check("facade refuses unknown complex reality", .not. result%ok, failures)
    result = subs(explicit_mu + explicit_sigma, explicit_mu, explicit_sigma)
    substituted = result%value
    call check("explicit arena uses facade substitution", &
        result%ok .and. substituted == explicit_sigma + explicit_sigma, failures)
    result = diff(concurrent_mu*concurrent_mu, concurrent_mu)
    call check("independent arenas can be interleaved", &
        result%ok .and. result%value == 2*concurrent_mu, failures)
    result = diff(explicit_mu*explicit_mu, explicit_mu)
    derivative = result%value
    call check("explicit arena uses facade differentiation", &
        result%ok .and. derivative == 2*explicit_mu, failures)
    result = simplify(explicit_mu + 0)
    simplified = result%value
    call check("explicit arena uses facade simplification", &
        result%ok .and. simplified == explicit_mu, failures)
    result = expand((explicit_mu + 1)*(explicit_mu + 2))
    expanded = result%value
    call check("explicit arena uses facade expansion", &
        result%ok .and. expanded == explicit_mu**2 + 3*explicit_mu + 2, failures)
    result = factor(explicit_mu**2 + 2*explicit_mu + 1)
    factored = result%value
    call check("explicit arena uses facade factorisation", &
        result%ok .and. factored == (explicit_mu + 1)**2, failures)

    base_context = make_assumption_context(explicit_arena)
    positive_context = with_assumption(base_context, positive(explicit_mu), context_ok)
    call check("value-style context accepts same-arena fact", context_ok, failures)
    result = simplify(sqrt(explicit_mu**2), assumptions=base_context)
    call check("base context stays assumption-free", &
        result%ok .and. result%value == sqrt(explicit_mu**2), failures)
    result = simplify(sqrt(explicit_mu**2), assumptions=positive_context)
    call check("derived positive context is isolated and effective", &
        result%ok .and. result%value == explicit_mu, failures)
    result = refine(sqrt(explicit_mu**2), positive_context)
    call check("facade refine uses an explicit context", &
        result%ok .and. result%value == explicit_mu, failures)
    relation = greater(explicit_mu, num(explicit_arena, 1_int64))
    relation_context = with_assumption(base_context, relation, context_ok)
    call check("value-style context accepts a bounded lower relation", &
        context_ok, failures)
    result = refine(sqrt(explicit_mu**2), relation_context)
    call check("bounded lower relation refines through the facade", &
        result%ok .and. result%value == explicit_mu, failures)
    relation = greater(explicit_mu, num(explicit_arena, -1_int64))
    relation_context = with_assumption(base_context, relation, context_ok)
    call check("unsupported lower relation is refused", .not. context_ok, failures)
    relation = greater(concurrent_mu, num(concurrent_arena, 0_int64))
    relation_context = with_assumption(base_context, relation, context_ok)
    call check("foreign relation is refused", .not. context_ok, failures)
    nonnegative_context = with_assumption(base_context, &
        nonnegative(explicit_sigma), context_ok)
    call check("second derived context accepts same-arena fact", context_ok, failures)
    result = simplify(sqrt(explicit_sigma**2), assumptions=nonnegative_context)
    call check("derived nonnegative context is independent", &
        result%ok .and. result%value == explicit_sigma, failures)
    integer_context = with_assumption(base_context, &
        integer_valued(explicit_mu), context_ok)
    call check("value-style context accepts integer fact", context_ok, failures)
    integer_context = with_assumption(integer_context, &
        positive_integer(explicit_sigma), context_ok)
    call check("value-style context accepts positive integer shorthand", &
        context_ok, failures)
    rational_context = with_assumption(base_context, &
        rational_valued(explicit_mu), context_ok)
    call check("value-style context accepts rational fact", context_ok, failures)
    result = re_part(explicit_mu, assumptions=positive_context)
    call check("real-part facade accepts explicit assumptions", &
        result%ok .and. result%value == explicit_mu, failures)
    result = im_part(explicit_mu, assumptions=positive_context)
    call check("imaginary-part facade uses explicit reality", &
        result%ok .and. result%value == num(explicit_arena, 0_int64), failures)
    result = conjugate(explicit_mu, assumptions=positive_context)
    call check("conjugation facade uses explicit reality", &
        result%ok .and. result%value == explicit_mu, failures)
    result = zero_test(sqrt(explicit_mu**2) - explicit_mu, &
        assumptions=positive_context)
    call check("zero query accepts an explicit context", &
        result%ok .and. result%verdict == VERDICT_TRUE, failures)
    result = diff(explicit_mu**2, explicit_mu, assumptions=positive_context)
    call check("differentiation accepts an explicit context", &
        result%ok .and. result%value == 2*explicit_mu, failures)
    foreign_context = make_assumption_context(concurrent_arena)
    result = simplify(sqrt(explicit_mu**2), assumptions=foreign_context)
    call check("foreign explicit context is refused", &
        .not. result%ok .and. chars(result%message) == &
        "simplify: assumptions belong to a different arena", failures)
    result = zero_test(mu - mu)
    call check("facade zero query proves an identity", &
        result%ok .and. result%verdict == VERDICT_TRUE, failures)
    result = zero_test(num(default_storage, 7))
    call check("facade zero query proves a nonzero literal", &
        result%ok .and. result%verdict == VERDICT_FALSE, failures)
    result = zero_test(sin(mu))
    call check("facade zero query preserves unknown", &
        result%ok .and. result%verdict == VERDICT_UNKNOWN, failures)
    result = subs(mu, mu, explicit_mu)
    call check("facade reports cross-arena substitution refusal", &
        .not. result%ok .and. .not. is_valid(result%value) .and. &
        chars(result%message) == "subs: expressions belong to different arenas", &
        failures)

    stale = mu
    call reset()
    call reset()
    call check("reset invalidates old handles", &
        .not. is_valid(mixed) .and. .not. is_valid(stale), failures)
    mu = "mu"
    call check("the default arena can be reused after reset", &
        is_valid(mu) .and. .not. is_valid(stale), failures)
    call check("the default arena pointer remains usable after reset", &
        associated(default_storage) .and. &
        same_arena(mu, sym(default_storage, "mu")), failures)

    if (failures /= 0) error stop failures
    write (*, '(a)') "PASS fortsym convenience API"

contains

    subroutine check(label, condition, failures)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        integer, intent(inout) :: failures
        if (condition) then
            write (*, '(a)') "PASS "//label
        else
            write (*, '(a)') "FAIL "//label
            failures = failures + 1
        end if
    end subroutine check

    logical function contains_name(names, wanted)
        type(str_t), intent(in) :: names(:)
        character(*), intent(in) :: wanted
        integer :: k
        contains_name = .false.
        do k = 1, size(names)
            if (chars(names(k)) == wanted) then
                contains_name = .true.
                return
            end if
        end do
    end function contains_name

end program test_fortsym_convenience
