module fortsym_predicates
    ! Structural predicates owned separately from expression construction.
    !
    ! `is_number` follows SymPy's expression-level meaning: a compound is
    ! numeric when its children are numeric, while relation nodes are Boolean.
    use fortsym_string, only: chars
    use fortsym_arena, only: NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, &
        NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, &
        NK_BIG_RAT, NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_expr, only: expr_t, is_valid
    use fortsym_assume, only: assumption_context_t, assumption_has, &
        FACT_ALGEBRAIC
    use fortsym_engine, only: VERDICT_UNKNOWN, VERDICT_TRUE, VERDICT_FALSE
    implicit none
    private

    public :: is_number, is_algebraic

contains

    recursive function is_number(expression) result(number)
        type(expr_t), intent(in) :: expression
        logical                  :: number
        integer                  :: kind, index
        character(:), allocatable :: head

        number = .false.
        if (.not. is_valid(expression)) return
        kind = expression%kind()
        select case (kind)
        case (NK_INT, NK_RAT, NK_REAL, NK_CONST, NK_BIG_INT, NK_BIG_RAT, &
                NK_BIG_REAL, NK_ALGEBRAIC)
            number = .true.
        case (NK_SYM)
            number = .false.
        case (NK_ADD, NK_MUL, NK_POW, NK_FUNC)
            if (kind == NK_FUNC) then
                head = chars(expression%name())
                if (head == "Equal" .or. head == "Unequal" .or. &
                    head == "Less" .or. head == "LessEqual" .or. &
                    head == "Greater" .or. head == "GreaterEqual") then
                    return
                end if
            end if
            number = .true.
            do index = 1, expression%nargs()
                if (.not. is_number(expression%arg(index))) then
                    number = .false.
                    return
                end if
            end do
        case default
            number = .false.
        end select
    end function is_number

    recursive function is_algebraic(expression, assumptions) result(verdict)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), intent(in), optional :: assumptions
        integer                  :: verdict
        integer                  :: kind
        logical                  :: known
        character(:), allocatable :: head

        verdict = VERDICT_UNKNOWN
        if (.not. is_valid(expression)) return
        if (present(assumptions)) then
            call assumption_has(assumptions, expression, FACT_ALGEBRAIC, known)
            if (known) then
                verdict = VERDICT_TRUE
                return
            end if
        end if

        kind = expression%kind()
        select case (kind)
        case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT, NK_ALGEBRAIC)
            verdict = VERDICT_TRUE
        case (NK_REAL, NK_BIG_REAL)
            verdict = VERDICT_UNKNOWN
        case (NK_CONST)
            head = chars(expression%name())
            select case (head)
            case ("i")
                verdict = VERDICT_TRUE
            case ("pi", "e", "oo", "zoo")
                verdict = VERDICT_FALSE
            case default
                verdict = VERDICT_UNKNOWN
            end select
        case (NK_SYM)
            verdict = VERDICT_UNKNOWN
        case (NK_ADD, NK_MUL)
            verdict = compound_status(expression, assumptions)
        case (NK_POW)
            verdict = power_status(expression, assumptions)
        case (NK_FUNC)
            head = chars(expression%name())
            select case (head)
            case ("sqrt", "abs", "Abs", "re", "im", "conjugate")
                verdict = compound_status(expression, assumptions)
            case ("sin", "cos", "tan", "asin", "acos", "atan", &
                    "atan2", "sinh", "cosh", "tanh", "asinh", "acosh", &
                    "atanh", "exp", "log", "erf", "erfc", "gamma", &
                    "loggamma", "factorial", "besselj", "besseli", &
                    "legendrep", "legendreq")
                verdict = compound_status(expression, assumptions)
                if (verdict == VERDICT_TRUE) verdict = VERDICT_FALSE
            case default
                verdict = VERDICT_UNKNOWN
            end select
        case default
            verdict = VERDICT_UNKNOWN
        end select
    contains
        function child_status(expression, index, assumptions) result(child)
            type(expr_t), intent(in) :: expression
            integer, intent(in) :: index
            type(assumption_context_t), intent(in), optional :: assumptions
            integer :: child

            if (present(assumptions)) then
                child = is_algebraic(expression%arg(index), assumptions)
            else
                child = is_algebraic(expression%arg(index))
            end if
        end function child_status

        function compound_status(expression, assumptions) result(status)
            type(expr_t), intent(in) :: expression
            type(assumption_context_t), intent(in), optional :: assumptions
            integer :: status, child, index
            logical :: unknown

            status = VERDICT_TRUE
            unknown = .false.
            do index = 1, expression%nargs()
                child = child_status(expression, index, assumptions)
                if (child == VERDICT_FALSE) then
                    status = VERDICT_FALSE
                    return
                end if
                if (child == VERDICT_UNKNOWN) unknown = .true.
            end do
            if (unknown) status = VERDICT_UNKNOWN
        end function compound_status

        function power_status(expression, assumptions) result(status)
            type(expr_t), intent(in) :: expression
            type(assumption_context_t), intent(in), optional :: assumptions
            integer :: status, base_status

            if (expression%nargs() /= 2) then
                status = VERDICT_UNKNOWN
                return
            end if
            base_status = child_status(expression, 1, assumptions)
            if (.not. exact_rational(expression%arg(2))) then
                status = VERDICT_UNKNOWN
            else
                status = base_status
            end if
        end function power_status

        logical function exact_rational(value)
            type(expr_t), intent(in) :: value
            select case (value%kind())
            case (NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT)
                exact_rational = .true.
            case default
                exact_rational = .false.
            end select
        end function exact_rational
    end function is_algebraic

end module fortsym_predicates
