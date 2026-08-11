module fortsym
    ! The short public Fortran surface. Explicit arenas and the lower-level
    ! modules remain available for callers that need independent state.
    use fortsym_arena, only: arena_t, node_kind_name, &
        NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, NK_POW, &
        NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, &
        real_text_expr, const, func, func_in, partial, pi_expr, e_expr, &
        i_expr, &
        is_valid, same_arena, operator(+), operator(-), operator(*), &
        operator(/), operator(**), operator(==), operator(/=)
    use fortsym_expr, only: sin_expr => sin, cos_expr => cos, tan_expr => tan, &
        asin_expr => asin, acos_expr => acos, atan_expr => atan, &
        atan2_expr => atan2, sinh_expr => sinh, cosh_expr => cosh, &
        tanh_expr => tanh, asinh_expr => asinh, acosh_expr => acosh, &
        atanh_expr => atanh, exp_expr => exp, log_expr => log, &
        sqrt_expr => sqrt, abs_expr => abs, erf_expr => erf, &
        erfc_expr => erfc, gamma_expr => gamma, besselj_expr => besselj, &
        legendrep_expr => legendrep, legendreq_expr => legendreq
    implicit none
    private

    public :: arena_t, node_kind_name
    public :: NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, &
        NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL
    public :: expr_t, sym, num, rat, exact, real_expr, real_text_expr, const, &
        func, func_in, partial, pi_expr, e_expr, i_expr, is_valid, same_arena
    public :: operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), operator(/=)
    public :: sin_expr, cos_expr, tan_expr, asin_expr, acos_expr, atan_expr, &
        atan2_expr, sinh_expr, cosh_expr, tanh_expr, asinh_expr, acosh_expr, &
        atanh_expr, exp_expr, log_expr, sqrt_expr, abs_expr, erf_expr, &
        erfc_expr, gamma_expr, besselj_expr, legendrep_expr, legendreq_expr
    public :: fortsym_default_arena, fortsym_reset, symbols
    public :: assignment(=)

    type(arena_t), target, save :: default_store
    logical, save :: default_ready = .false.

    interface assignment(=)
        module procedure assign_character
    end interface assignment(=)

contains

    !> Return the process-local arena used by character assignment and symbols.
    !> It is single-threaded state. Callers that need concurrency should create
    !> an arena_t and use the explicit constructors from the same module.
    function fortsym_default_arena() result(a)
        type(arena_t), pointer :: a
        call ensure_default()
        a => default_store
    end function fortsym_default_arena

    !> Clear the convenience arena. Handles made before this call become stale,
    !> including handles whose node index is reused after the next construction.
    subroutine fortsym_reset()
        call default_store%clear()
        default_ready = .false.
    end subroutine fortsym_reset

    !> Assigning text creates one symbol in the default arena. Text is never
    !> parsed as an expression.
    subroutine assign_character(lhs, rhs)
        type(expr_t), intent(out) :: lhs
        character(*), intent(in)   :: rhs
        call ensure_default()
        lhs = sym(default_store, trim(rhs))
    end subroutine assign_character

    !> Read up to eight whitespace- or comma-separated symbol names into scalar
    !> outputs. A missing output or an extra name sets ok=.false.; every output
    !> remains a valid symbol only when a corresponding name was present.
    subroutine symbols(names, first, second, third, fourth, fifth, sixth, &
            seventh, eighth, ok)
        character(*), intent(in) :: names
        type(expr_t), intent(out) :: first
        type(expr_t), intent(out), optional :: second, third, fourth, fifth
        type(expr_t), intent(out), optional :: sixth, seventh, eighth
        logical, intent(out), optional :: ok
        integer :: position
        logical :: good

        first = expr_t()
        if (present(second)) second = expr_t()
        if (present(third)) third = expr_t()
        if (present(fourth)) fourth = expr_t()
        if (present(fifth)) fifth = expr_t()
        if (present(sixth)) sixth = expr_t()
        if (present(seventh)) seventh = expr_t()
        if (present(eighth)) eighth = expr_t()

        call ensure_default()
        position = 1
        good = .true.
        call read_symbol(names, position, first, good)
        if (present(second)) call read_symbol(names, position, second, good)
        if (present(third)) call read_symbol(names, position, third, good)
        if (present(fourth)) call read_symbol(names, position, fourth, good)
        if (present(fifth)) call read_symbol(names, position, fifth, good)
        if (present(sixth)) call read_symbol(names, position, sixth, good)
        if (present(seventh)) call read_symbol(names, position, seventh, good)
        if (present(eighth)) call read_symbol(names, position, eighth, good)
        call skip_separators(names, position)
        if (position <= len_trim(names)) good = .false.
        if (present(ok)) ok = good
    end subroutine symbols

    subroutine ensure_default()
        if (default_ready) return
        call default_store%init()
        default_ready = .true.
    end subroutine ensure_default

    subroutine read_symbol(names, position, target, good)
        character(*), intent(in) :: names
        integer, intent(inout) :: position
        type(expr_t), intent(inout) :: target
        logical, intent(inout) :: good
        integer :: start, finish

        call skip_separators(names, position)
        if (position > len_trim(names)) then
            good = .false.
            return
        end if
        start = position
        do while (position <= len_trim(names))
            if (is_separator(names(position:position))) exit
            position = position + 1
        end do
        finish = position - 1
        target = sym(default_store, names(start:finish))
    end subroutine read_symbol

    subroutine skip_separators(names, position)
        character(*), intent(in) :: names
        integer, intent(inout) :: position
        do while (position <= len_trim(names))
            if (.not. is_separator(names(position:position))) return
            position = position + 1
        end do
    end subroutine skip_separators

    pure logical function is_separator(character)
        character, intent(in) :: character
        is_separator = character == ' ' .or. character == ',' .or. &
            iachar(character) == 9
    end function is_separator

end module fortsym
