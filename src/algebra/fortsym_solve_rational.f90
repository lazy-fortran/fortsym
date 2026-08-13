module fortsym_solve_rational
    ! Shared bounded rational-function solve path.
    !
    ! Together converts a rational residual to one exact numerator and
    ! denominator. Roots are obtained only from the existing polynomial
    ! solver, then substituted into the untouched residual. A numerator root
    ! at a denominator pole is discarded; an undecidable residual is refused
    ! rather than returned with an unstated condition.
    use fortsym_arena, only: arena_t
    use fortsym_engine, only: engine_t, engine_result_t, VERDICT_TRUE, &
        VERDICT_UNKNOWN
    use fortsym_expr, only: expr_t
    use fortsym_poly, only: poly_numerator, poly_together
    use fortsym_polysolve, only: solve_polynomial
    use fortsym_subs, only: subs
    implicit none
    private

    public :: solve_rational_polynomial

contains

    subroutine solve_rational_polynomial( &
            a, engine, residual, variable, roots, root_count, ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: residual, variable
        type(expr_t), allocatable, intent(out) :: roots(:)
        integer, intent(out) :: root_count
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: together, numerator, probe
        type(engine_result_t) :: verdict
        character(:), allocatable :: rational_why
        integer :: candidate_count, k, j
        logical :: together_ok, polynomial_ok, duplicate

        allocate (roots(0))
        root_count = 0
        ok = .false.
        why = ""

        call poly_together(a, residual, together, together_ok, rational_why)
        if (.not. together_ok) then
            why = "rational normalization: "//rational_why
            return
        end if
        numerator = poly_numerator(a, together)
        call solve_polynomial(a, numerator, variable, roots, polynomial_ok, &
            rational_why)
        if (.not. polynomial_ok) then
            why = "rational numerator: "//rational_why
            return
        end if

        candidate_count = size(roots)
        do k = 1, candidate_count
            probe = subs(residual, variable, roots(k))
            verdict = engine%zero_test(probe)
            if (verdict%verdict == VERDICT_UNKNOWN) then
                ok = .false.
                root_count = 0
                why = "rational candidate could not be verified"
                deallocate (roots)
                allocate (roots(0))
                return
            end if
            if (verdict%verdict /= VERDICT_TRUE) cycle
            duplicate = .false.
            do j = 1, root_count
                if (roots(j)%id == roots(k)%id) then
                    duplicate = .true.
                    exit
                end if
            end do
            if (duplicate) cycle
            root_count = root_count + 1
            roots(root_count) = roots(k)
        end do

        ok = .true.
        why = ""
    end subroutine solve_rational_polynomial

end module fortsym_solve_rational
