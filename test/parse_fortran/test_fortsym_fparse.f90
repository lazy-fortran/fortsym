program test_fortsym_fparse
    ! Reading expressions out of real Fortran source.
    !
    ! The centrepiece reproduces the libneo workflow that motivated this: a
    ! hand-written kernel computes derivatives of a basis function, and the
    ! check confirms each one against the symbolic derivative of the definition
    ! -- by reading the actual file, not a transcription of it.
    !
    ! A deliberately wrong derivative is included in the fixture. If the check
    ! did not catch that, it would be testing nothing.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(-), &
        operator(*), operator(/), operator(**), log
    use fortsym_diff, only: diff
    use fortsym_parse, only: parse_expr
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_fparse
    implicit none

    integer, parameter :: dp = real64
    character(*), parameter :: FIXTURE = "/tmp/fortsym_fixture_kernel.f90"

    type(arena_t), target    :: arena
    type(symengine_engine_t) :: eng
    integer :: nfail = 0

    call arena%init()
    eng = make_symengine_engine(arena)

    call write_fixture()

    call test_logical_lines()
    call test_finds_assignments()
    call test_rejects_non_assignments()
    call test_case_insensitive()
    call test_continuations()
    call test_verifies_a_real_kernel()
    call test_catches_a_wrong_derivative()

    if (nfail == 0) then
        print *, "test_fortsym_fparse: all checks passed"
    else
        print *, "test_fortsym_fparse: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    !> A kernel in the shape of the ones this is meant to check: Cerfon-Freidberg
    !> style basis functions and their derivatives, with continuations, comments,
    !> mixed case, and one derivative that is wrong on purpose.
    subroutine write_fixture()
        integer :: unit, ios
        open (newunit=unit, file=FIXTURE, status="replace", action="write", &
            iostat=ios)
        if (ios /= 0) then
            nfail = nfail + 1
            return
        end if
        write (unit, "(a)") "subroutine basis(x, y, psi3, dpsi3_dx, dpsi5_dxx, bad)"
        write (unit, "(a)") "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "    real(dp), intent(in) :: x, y"
        write (unit, "(a)") "    real(dp), intent(out) :: psi3, dpsi3_dx"
        write (unit, "(a)") "    real(dp), intent(out) :: dpsi5_dxx, bad"
        write (unit, "(a)") "    real(dp) :: logx"
        write (unit, "(a)") ""
        write (unit, "(a)") "    logx = log(x)   ! shared subexpression"
        write (unit, "(a)") ""
        write (unit, "(a)") "    ! psi_3 = y**2 - x**2 log(x)"
        write (unit, "(a)") "    psi3 = y**2 - x**2*logx"
        write (unit, "(a)") "    dpsi3_dx = -2.0_dp*x*logx - x"
        write (unit, "(a)") ""
        write (unit, "(a)") "    ! second derivative, split across continuations"
        write (unit, "(a)") "    dpsi5_dxx = -18.0_dp*y**2 + 36.0_dp*x**2*logx &"
        write (unit, "(a)") "                + 21.0_dp*x**2"
        write (unit, "(a)") ""
        write (unit, "(a)") "    ! deliberately wrong: a sign error in the second term"
        write (unit, "(a)") "    BAD = -2.0_dp*x*logx + x"
        write (unit, "(a)") "end subroutine basis"
        close (unit)
    end subroutine write_fixture

    subroutine test_logical_lines()
        type(str_t), allocatable :: lines(:)
        character(:), allocatable :: message
        integer :: n, k
        logical :: good, found_joined

        call logical_lines(FIXTURE, lines, n, good, message)
        call ok("fixture reads", good .and. n > 0)

        ! The continuation must have been joined into one logical line, or the
        ! assignment split across it would be invisible.
        found_joined = .false.
        do k = 1, n
            if (index(chars(lines(k)), "36.0_dp") > 0 .and. &
                index(chars(lines(k)), "21.0_dp") > 0) found_joined = .true.
        end do
        call ok("continuation lines are joined", found_joined)

        ! Comments must be gone, including the trailing one after a statement.
        do k = 1, n
            call ok("comments removed", index(chars(lines(k)), "!") == 0)
        end do

        call logical_lines("/tmp/definitely_not_there.f90", lines, n, good, message)
        call ok("missing file reports rather than stops", .not. good)
    end subroutine test_logical_lines

    subroutine test_finds_assignments()
        type(expr_t) :: e
        character(:), allocatable :: message
        logical :: good

        e = parse_fortran_expr(arena, FIXTURE, "psi3", good, message)
        call ok("finds a simple assignment", good)

        e = parse_fortran_expr(arena, FIXTURE, "nosuchname", good, message)
        call ok("missing symbol reports", .not. good)
    end subroutine test_finds_assignments

    !> Declarations, comparisons and keyword arguments contain '=' but are not
    !> assignments. Treating one as an assignment would silently return the
    !> wrong expression.
    subroutine test_rejects_non_assignments()
        type(str_t), allocatable :: lines(:)
        character(:), allocatable :: rhs, message
        integer :: n
        logical :: good

        call logical_lines(FIXTURE, lines, n, good, message)

        ! 'intent(in) :: x, y' must not be read as an assignment to anything.
        call find_assignment(lines, n, "intent", rhs, good, message)
        call ok("declaration is not an assignment", .not. good)

        call find_assignment(lines, n, "real", rhs, good, message)
        call ok("type declaration is not an assignment", .not. good)
    end subroutine test_rejects_non_assignments

    !> Fortran is case-insensitive. The fixture spells one output BAD while the
    !> declaration spells it bad.
    subroutine test_case_insensitive()
        type(expr_t) :: e
        character(:), allocatable :: message
        logical :: good

        e = parse_fortran_expr(arena, FIXTURE, "bad", good, message)
        call ok("lowercase query finds uppercase assignment", good)

        e = parse_fortran_expr(arena, FIXTURE, "PSI3", good, message)
        call ok("uppercase query finds lowercase assignment", good)
    end subroutine test_case_insensitive

    subroutine test_continuations()
        type(expr_t) :: e
        character(:), allocatable :: message
        logical :: good

        e = parse_fortran_expr(arena, FIXTURE, "dpsi5_dxx", good, message)
        call ok("parses across a continuation", good)
        if (.not. good) print *, "   ", message
    end subroutine test_continuations

    !> The real thing: check a hand-written derivative against the symbolic
    !> derivative of the definition, reading both from source.
    !>
    !> This is what retires the transcription dict. The kernel writes logx for
    !> log(x), so the comparison substitutes it back -- a real check has to cope
    !> with the kernel's own temporaries.
    subroutine test_verifies_a_real_kernel()
        type(expr_t) :: got, x, y, psi3, want
        character(:), allocatable :: message
        logical :: good

        x = sym(arena, "x")
        y = sym(arena, "y")

        ! The definition, stated independently of the implementation.
        psi3 = y**2 - x**2*log(x)
        want = diff(psi3, x)

        got = parse_fortran_expr(arena, FIXTURE, "dpsi3_dx", good, message)
        call ok("reads dpsi3_dx", good)
        if (.not. good) then
            print *, "   ", message
            return
        end if

        ! The kernel names log(x) as logx, so substitute the definition of the
        ! temporary before comparing.
        got = substitute_logx(got, x)

        call ok("hand-written dpsi3_dx matches the symbolic derivative", &
            is_zero(got - want))
    end subroutine test_verifies_a_real_kernel

    !> The same check against the deliberately wrong entry. If this passed, the
    !> whole exercise would be worthless.
    subroutine test_catches_a_wrong_derivative()
        type(expr_t) :: got, x, y, psi3, want
        character(:), allocatable :: message
        logical :: good

        x = sym(arena, "x")
        y = sym(arena, "y")
        psi3 = y**2 - x**2*log(x)
        want = diff(psi3, x)

        got = parse_fortran_expr(arena, FIXTURE, "bad", good, message)
        call ok("reads the wrong entry", good)
        if (.not. good) return

        got = substitute_logx(got, x)

        call ok("a wrong derivative is caught", .not. is_zero(got - want))
    end subroutine test_catches_a_wrong_derivative

    !> Rebuild an expression with the symbol logx replaced by log(x).
    !>
    !> Done by reparsing rather than by a substitution pass, which fortsym does
    !> not have yet; the kernel's text is available, so this stays honest and
    !> small.
    function substitute_logx(e, x) result(r)
        type(expr_t), intent(in) :: e, x
        type(expr_t)             :: r
        character(:), allocatable :: text, replaced, message
        logical :: good
        integer :: pos

        text = printed(e)
        replaced = ""
        do
            pos = index(text, "logx")
            if (pos == 0) exit
            replaced = replaced//text(1:pos - 1)//"log(x)"
            text = text(pos + 4:)
        end do
        replaced = replaced//text

        r = parse_expr(arena, replaced, good, message)
        if (.not. good) then
            nfail = nfail + 1
            print *, "FAIL substitution reparse: ", message
            r = e
        end if
    end function substitute_logx

    function printed(e) result(t)
        use fortsym_print, only: print_expr
        type(expr_t), intent(in)  :: e
        character(:), allocatable :: t
        t = chars(print_expr(e))
    end function printed

    function is_zero(e) result(yes)
        type(expr_t), intent(in) :: e
        logical                  :: yes
        type(engine_result_t) :: r
        r = eng%zero_test(e)
        yes = r%verdict == VERDICT_TRUE
    end function is_zero

end program test_fortsym_fparse
