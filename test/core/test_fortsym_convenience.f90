program test_fortsym_convenience
    use fortsym
    use fortsym_print, only: print_expr
    use fortsym_string, only: chars
    implicit none

    type(arena_t), target :: explicit_arena
    type(arena_t), pointer :: default_storage
    type(expr_t) :: x, mu, sigma, best, xi, literal
    type(expr_t) :: bulk_mu, bulk_sigma, bulk_best, bulk_xi
    type(expr_t) :: explicit_mu, explicit_sigma, explicit_best, explicit_xi
    type(expr_t) :: explicit_expression, default_expression, mixed
    type(expr_t) :: sine, substituted, derivative, simplified, expanded, factored
    type(expr_t) :: failed
    character(:), allocatable :: why
    logical :: good
    integer :: failures

    failures = 0
    call explicit_arena%init()
    explicit_mu = sym(explicit_arena, "mu")
    explicit_sigma = sym(explicit_arena, "sigma")
    explicit_best = sym(explicit_arena, "best")
    explicit_xi = sym(explicit_arena, "xi")
    explicit_expression = (explicit_best - explicit_xi - explicit_mu)/explicit_sigma

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
    mixed = mu + sigma
    call check("default expression participates in ordinary operators", &
        is_valid(mixed), failures)
    sine = sin(mu)
    call check("facade uses the intrinsic spelling for expression functions", &
        chars(print_expr(sine)) == "sin(mu)", failures)
    substituted = subs(mu + sigma, mu, sigma, good, why)
    call check("facade exposes structural substitution", &
        good .and. substituted == sigma + sigma, failures)
    derivative = diff(mu*mu, mu, good, why)
    call check("facade exposes evaluated differentiation", &
        good .and. derivative == 2*mu, failures)
    simplified = simplify(mu + 0, good, why)
    call check("facade exposes native simplification", &
        good .and. simplified == mu, failures)
    expanded = expand((mu + 1)*(mu + 2), good, why)
    call check("facade exposes native expansion", &
        good .and. expanded == mu**2 + 3*mu + 2, failures)
    factored = factor(mu**2 + 2*mu + 1, good, why)
    call check("facade exposes native factorisation", &
        good .and. factored == (mu + 1)**2, failures)
    failed = subs(mu, mu, explicit_mu, good, why)
    call check("facade reports cross-arena substitution refusal", &
        .not. good .and. .not. is_valid(failed), failures)

    call reset()
    call check("reset invalidates old handles", .not. is_valid(mixed), failures)
    mu = "mu"
    call check("the default arena can be reused after reset", is_valid(mu), failures)

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

end program test_fortsym_convenience
