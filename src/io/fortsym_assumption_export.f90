module fortsym_assumption_export
    ! Shared spelling for explicitly supported, positive assumption facts.
    use fortsym_assume, only: assumption_context_t
    use fortsym_arena, only: arena_t, NK_SYM
    implicit none
    private
    public :: sympy_assumption_keywords, exportable_assumptions
contains
    function sympy_assumption_keywords(facts) result(text)
        integer, intent(in) :: facts
        character(:), allocatable :: text
        character(16), parameter :: names(11) = [character(16) :: &
            'real', 'positive', 'nonnegative', 'nonzero', 'integer', &
            'positive', 'zero', 'negative', 'nonpositive', 'rational', 'algebraic']
        integer :: k
        text = ''
        do k = 1, 11
            if (btest(facts, k-1)) then
                ! Positive-integer has its own native bit; avoid duplicate kwargs.
                if (k == 6) then
                    if (.not. btest(facts, 1)) text = text//', positive=True'
                    if (.not. btest(facts, 4)) text = text//', integer=True'
                else
                    text = text//', '//trim(names(k))//'=True'
                end if
            end if
        end do
    end function

    logical function exportable_assumptions(context, home) result(ok)
        type(assumption_context_t), intent(in) :: context
        type(arena_t), target, intent(in) :: home
        integer :: k
        ok = .false.
        if (.not. associated(context%home, home)) return
        do k = 1, context%n
            if (context%ids(k) < 1 .or. context%ids(k) > home%size()) return
            if (home%kind_of(context%ids(k)) /= NK_SYM) return
            if (context%facts(k) < 0 .or. context%facts(k) > 2047) return
        end do
        ok = .true.
    end function
end module
