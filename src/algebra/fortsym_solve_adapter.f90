module fortsym_solve_adapter
    ! Binding-safe public adapter for the verified one-equation solver.
    !
    ! The polynomial solver preserves multiplicity internally because that is
    ! useful to algebraic callers. SymPy's solve() returns distinct roots, so
    ! this compatibility boundary compacts distinct handles into the prefix
    ! reported by root_count after full polynomial verification succeeds.
    use fortsym_arena, only: arena_t, NK_FUNC, NK_SYM
    use fortsym_engine, only: engine_t, engine_result_t
    use fortsym_expr, only: expr_t, operator(-)
    use fortsym_polysolve, only: solve_polynomial
    use fortsym_solve_rational, only: solve_rational_polynomial
    use fortsym_string, only: chars
    implicit none
    private

    public :: calculate_solve

contains

    subroutine calculate_solve(a, engine, equation, variable, roots, root_count, &
            ok, why)
        type(arena_t), target, intent(inout) :: a
        class(engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: equation, variable
        type(expr_t), allocatable, intent(out) :: roots(:)
        integer, intent(out) :: root_count
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: residual
        type(engine_result_t) :: scalar
        character(:), allocatable :: polynomial_why
        character(:), allocatable :: rational_why
        logical :: good
        integer :: k, count

        allocate (roots(0))
        root_count = 0
        ok = .false.
        why = ""

        if (.not. same_arena_for_solve(equation, variable, a)) then
            why = "equation, variable, and arena must agree"
            return
        end if
        if (variable%kind() /= NK_SYM) then
            why = "solve variable must be a symbol"
            return
        end if
        call equation_residual(equation, residual, good, why)
        if (.not. good) return

        call solve_polynomial(a, residual, variable, roots, ok, &
            polynomial_why)
        if (ok) then
            ! solve_polynomial verifies every entry against the untouched
            ! original coefficient vector. Only the output convention differs
            ! here: SymPy's solve() suppresses repeated roots. Keep the
            ! solver-owned allocation and report the compact prefix instead of
            ! allocating a second array.
            count = 0
            do k = 1, size(roots)
                if (.not. contains_root(roots, count, roots(k))) then
                    count = count + 1
                    roots(count) = roots(k)
                end if
            end do
            root_count = count
            ok = .true.
            why = ""
            return
        end if

        call solve_rational_polynomial( &
            a, engine, residual, variable, roots, root_count, ok, rational_why)
        if (ok) return

        ! Polynomial extraction deliberately rejects symbolic coefficients and
        ! non-polynomial forms. The native scalar engine still covers the
        ! verified linear case, including expressions such as x + a.
        scalar = engine%solve(residual, variable)
        if (.not. scalar%ok) then
            why = polynomial_why//"; rational solver: "//rational_why// &
                "; scalar solver: "//chars(scalar%message)
            return
        end if
        deallocate (roots)
        allocate (roots(1))
        roots(1) = scalar%value
        root_count = 1
        ok = .true.
        why = ""
    end subroutine calculate_solve

    subroutine equation_residual(equation, residual, ok, why)
        type(expr_t), intent(in) :: equation
        type(expr_t), intent(out) :: residual
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why

        residual = equation
        ok = .true.
        why = ""
        if (equation%kind() /= NK_FUNC) return
        if (chars(equation%name()) /= "Equal") return
        if (equation%nargs() /= 2) then
            ok = .false.
            why = "solve requires an equality with two sides"
            return
        end if
        residual = equation%arg(1) - equation%arg(2)
    end subroutine equation_residual

    logical function same_arena_for_solve(equation, variable, a) result(same)
        type(expr_t), intent(in) :: equation, variable
        type(arena_t), target, intent(in) :: a

        same = associated(equation%a, a) .and. associated(variable%a, a)
    end function same_arena_for_solve

    logical function contains_root(values, count, candidate) result(found)
        type(expr_t), intent(in) :: values(:), candidate
        integer, intent(in) :: count
        integer :: k

        found = .false.
        do k = 1, count
            if (values(k)%id == candidate%id) then
                found = .true.
                return
            end if
        end do
    end function contains_root

end module fortsym_solve_adapter
