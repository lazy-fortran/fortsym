module fortsym_stability
    ! Explicit, domain-aware rewrites for numerically sensitive expressions.
    !
    ! These transformations are opt-in. Algebraic equivalence alone does not
    ! prove that a rewrite is preferable over every floating-point domain, so
    ! callers select them only after defining the numerical contract and
    ! measuring the complete workload.
    use fortsym_expr, only: expr_t, sqrt, operator(+), operator(-), operator(/)
    implicit none
    private

    public :: rationalize_sqrt_difference

contains

    !> Replace sqrt(a)-sqrt(b) by (a-b)/(sqrt(a)+sqrt(b)).
    !>
    ! The forms are equivalent where both square roots exist and their sum is
    ! nonzero. The rationalized form avoids subtracting nearby square roots,
    ! but remains an explicit candidate rather than an automatic global rule.
    function rationalize_sqrt_difference(a, b) result(stable)
        type(expr_t), intent(in) :: a, b
        type(expr_t) :: stable

        stable = (a - b)/(sqrt(a) + sqrt(b))
    end function rationalize_sqrt_difference

end module fortsym_stability
