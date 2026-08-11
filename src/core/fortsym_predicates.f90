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
    implicit none
    private

    public :: is_number

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

end module fortsym_predicates
