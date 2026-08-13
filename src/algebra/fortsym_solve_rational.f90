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
    use fortsym_diff, only: diff
    use fortsym_poly, only: poly_denominator, poly_numerator, poly_together
    use fortsym_polysolve, only: solve_polynomial
    use fortsym_subs, only: subs
    use fortsym_string, only: chars
    implicit none
    private

    public :: solve_rational_polynomial

contains

    subroutine solve_rational_polynomial( &
            a, engine, residual, variable, roots, root_count, ok, why, &
            excluded_roots, excluded_count)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: residual, variable
        type(expr_t), allocatable, intent(out) :: roots(:)
        integer, intent(out) :: root_count
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t), allocatable, intent(out), optional :: excluded_roots(:)
        integer, intent(out), optional :: excluded_count
        type(expr_t) :: together, numerator, denominator, probe, derivative
        type(engine_result_t) :: verdict
        type(engine_result_t) :: denominator_solution
        character(:), allocatable :: rational_why
        integer :: candidate_count, k, j
        logical :: together_ok, polynomial_ok, denominator_ok, duplicate
        logical :: need_exclusions

        allocate (roots(0))
        root_count = 0
        ok = .false.
        why = ""
        need_exclusions = present(excluded_roots) .and. present(excluded_count)
        if (present(excluded_roots)) then
            allocate (excluded_roots(0))
        end if
        if (present(excluded_count)) then
            excluded_count = 0
        end if

        call poly_together(a, residual, together, together_ok, rational_why)
        if (.not. together_ok) then
            why = "rational normalization: "//rational_why
            return
        end if
        numerator = poly_numerator(a, together)
        if (need_exclusions) then
            denominator = poly_denominator(a, together)
            derivative = diff(denominator, variable)
            verdict = engine%zero_test(derivative)
            if (verdict%verdict /= VERDICT_TRUE) then
                call solve_polynomial(a, denominator, variable, &
                    excluded_roots, denominator_ok, rational_why)
                if (.not. denominator_ok) then
                    denominator_solution = engine%solve(denominator, variable)
                    if (.not. denominator_solution%ok) then
                        why = "rational denominator: "//rational_why//"; "// &
                            "scalar solver: "//chars(denominator_solution%message)
                        deallocate (excluded_roots)
                        allocate (excluded_roots(0))
                        return
                    end if
                    deallocate (excluded_roots)
                    allocate (excluded_roots(1))
                    excluded_roots(1) = denominator_solution%value
                    excluded_count = 1
                else
                    call compact_distinct(excluded_roots, excluded_count)
                end if
            end if
        end if
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
            if (verdict%verdict == VERDICT_UNKNOWN .and. .not. &
                need_exclusions) then
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

    subroutine compact_distinct(values, value_count)
        type(expr_t), allocatable, intent(inout) :: values(:)
        integer, intent(out) :: value_count
        integer :: k, j
        logical :: duplicate

        value_count = 0
        do k = 1, size(values)
            duplicate = .false.
            do j = 1, value_count
                if (values(j)%id == values(k)%id) then
                    duplicate = .true.
                    exit
                end if
            end do
            if (duplicate) cycle
            value_count = value_count + 1
            values(value_count) = values(k)
        end do
    end subroutine compact_distinct

end module fortsym_solve_rational
