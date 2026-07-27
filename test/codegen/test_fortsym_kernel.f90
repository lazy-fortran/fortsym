program test_fortsym_kernel
    ! Code generation, checked on what the output must *mean* rather than on
    ! what it looks like.
    !
    ! The strongest check here compiles the generated kernel and compares it
    ! numerically against the same expression evaluated another way. A
    ! golden-string test would pin the current formatting and catch nothing
    ! about correctness; this catches a lost sign, a wrong precedence or a
    ! mis-ordered CSE temporary. Golden strings are used only where formatting
    ! itself is the requirement.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr
    use fortsym_parse, only: parse_expr
    use fortsym_kernel
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_cse_finds_sharing()
    call test_cse_skips_atoms()
    call test_temporaries_are_declared()
    call test_ordering_is_topological()
    call test_line_wrapping()
    call test_snippet_mode()
    call test_generated_kernel_compiles_and_agrees()

    if (nfail == 0) then
        print *, "test_fortsym_kernel: all checks passed"
    else
        print *, "test_fortsym_kernel: ", nfail, " check(s) FAILED"
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

    function parsed(a, text) result(e)
        type(arena_t), target, intent(inout) :: a
        character(*),          intent(in)    :: text
        type(expr_t)                         :: e
        character(:), allocatable :: message
        logical :: good
        e = parse_expr(a, text, good, message)
        if (.not. good) then
            nfail = nfail + 1
            print *, "PARSE-FAIL ", text, " : ", message
        end if
    end function parsed

    function spec_for(name, args, outs) result(sp)
        character(*), intent(in) :: name, args(:), outs(:)
        type(kernel_spec_t)      :: sp
        integer :: k

        sp%name = str(name)
        sp%mode = KERNEL_SUBROUTINE
        sp%temp_prefix = str("t")
        sp%generator = str("gen_test")
        allocate (sp%args(size(args)), sp%outputs(size(outs)))
        do k = 1, size(args)
            sp%args(k) = str(args(k))
        end do
        do k = 1, size(outs)
            sp%outputs(k) = str(outs(k))
        end do
    end function spec_for

    subroutine test_cse_finds_sharing()
        type(arena_t), target :: a
        type(expr_t) :: shared, roots(1)
        type(cse_result_t) :: res

        call a%init()

        ! sin(x*y) appears three times. Hash-consing already stores it once, so
        ! CSE has only to notice the reference count.
        shared = parsed(a, "sin(x*y)")
        roots(1) = parsed(a, "sin(x*y) + sin(x*y)*sin(x*y)")

        res = cse_analyse(roots, "t")
        call ok("shared subexpression named", res%n >= 1)
        call ok("the shared node is the repeated one", &
            any(res%ids(1:res%n) == shared%id))
    end subroutine test_cse_finds_sharing

    subroutine test_cse_skips_atoms()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(cse_result_t) :: res
        integer :: k
        logical :: named_an_atom

        call a%init()
        ! x appears many times but naming it would cost a line and save nothing.
        roots(1) = parsed(a, "x + x*x + x*x*x")

        res = cse_analyse(roots, "t")

        named_an_atom = .false.
        do k = 1, res%n
            if (a%nargs_of(res%ids(k)) == 0) named_an_atom = .true.
        end do
        call ok("atoms are not given temporaries", .not. named_an_atom)
    end subroutine test_cse_skips_atoms

    subroutine test_temporaries_are_declared()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "sin(x*y) + sin(x*y)*sin(x*y)")
        code = chars(emit_kernel(roots, spec_for("k", ["x", "y"], ["r"])))

        ! Every temporary must be declared. KiLCA's generated kernels used
        ! implicit real(dp) (s-t) instead, which types anything beginning with s
        ! or t and turns a misspelling into a silent zero.
        call ok("declares temporaries", index(code, "real(dp) :: t1") > 0)
        call ok("no implicit typing", index(code, "implicit real") == 0)
        call ok("has implicit none", index(code, "implicit none") > 0)
        call ok("declares inputs", index(code, "intent(in) :: x, y") > 0)
        call ok("declares output", index(code, "intent(out) :: r") > 0)
        call ok("names its generator", index(code, "gen_test") > 0)
    end subroutine test_temporaries_are_declared

    !> A temporary must be assigned before anything uses it.
    subroutine test_ordering_is_topological()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code
        integer :: def1, def2, use_in_2

        call a%init()
        ! exp(x+y) is shared, and sin of it is shared too, so one temporary must
        ! depend on the other.
        roots(1) = parsed(a, &
            "sin(exp(x + y))*sin(exp(x + y)) + exp(x + y) + sin(exp(x + y))")
        code = chars(emit_kernel(roots, spec_for("k", ["x", "y"], ["r"])))

        def1 = index(code, "    t1 = ")
        def2 = index(code, "    t2 = ")
        call ok("two temporaries generated", def1 > 0 .and. def2 > 0)

        if (def1 > 0 .and. def2 > 0) then
            ! t2's definition must come after t1's, and if t2 mentions t1 that
            ! reference must be to an already-assigned value.
            call ok("temporaries defined in order", def1 < def2)
            use_in_2 = index(code(def2:), "t1")
            if (use_in_2 > 0) then
                call ok("dependency defined before use", def1 < def2)
            end if
        end if
    end subroutine test_ordering_is_topological

    !> Long statements must be continued, and never broken inside a literal.
    subroutine test_line_wrapping()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code
        integer :: longest

        call a%init()
        roots(1) = parsed(a, &
            "aaaa*bbbb + cccc*dddd + eeee*ffff + gggg*hhhh + iiii*jjjj + "// &
            "kkkk*llll + mmmm*nnnn + oooo*pppp + qqqq*rrrr + ssss*tttt")
        code = chars(emit_kernel(roots, spec_for("k", ["aaaa"], ["r"])))

        call ok("long statement is continued", index(code, "&") > 0)

        longest = longest_line(code)
        call ok("lines stay within the Fortran limit", longest <= 132)

        ! A break inside an exponent would change the constant's value.
        call a%clear()
        call a%init()
        roots(1) = parsed(a, &
            "1.0e-8*aaaa + 1.0e-8*bbbb + 1.0e-8*cccc + 1.0e-8*dddd + "// &
            "1.0e-8*eeee + 1.0e-8*ffff + 1.0e-8*gggg + 1.0e-8*hhhh")
        code = chars(emit_kernel(roots, spec_for("k", ["aaaa"], ["r"])))
        call ok("never breaks inside an exponent", index(code, "e- &") == 0)
        call ok("never breaks after an exponent sign", index(code, "e-&") == 0)
    end subroutine test_line_wrapping

    function longest_line(text) result(n)
        character(*), intent(in) :: text
        integer                  :: n
        integer :: start, k
        n = 0
        start = 1
        do k = 1, len(text)
            if (text(k:k) == new_line("a")) then
                n = max(n, k - start)
                start = k + 1
            end if
        end do
        n = max(n, len(text) - start + 1)
    end function longest_line

    subroutine test_snippet_mode()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        type(kernel_spec_t) :: sp
        character(:), allocatable :: code

        call a%init()
        roots(1) = parsed(a, "x*y + 1")
        sp = spec_for("k", ["x", "y"], ["r"])
        sp%mode = KERNEL_SNIPPET
        code = chars(emit_kernel(roots, sp))

        ! A snippet is spliced into an existing scope, so it must contribute
        ! statements and nothing else.
        call ok("snippet has no subroutine header", &
            index(code, "subroutine") == 0)
        call ok("snippet has no declarations", index(code, "real(dp)") == 0)
        call ok("snippet assigns the output", index(code, "r = ") > 0)
        call ok("snippet still records its generator", &
            index(code, "gen_test") > 0)
    end subroutine test_snippet_mode

    !> The real check: compile the generated kernel and compare it numerically
    !> against a hand-written evaluation of the same formula.
    subroutine test_generated_kernel_compiles_and_agrees()
        type(arena_t), target :: a
        type(expr_t) :: roots(1)
        character(:), allocatable :: code
        integer :: unit, ios, stat
        real(dp) :: got, want, xv, yv
        integer :: k
        logical :: agreed

        call a%init()
        roots(1) = parsed(a, &
            "sin(x*y)**2 + cos(x*y) + exp(x)/(1 + y**2) - 3*x/(y + 1)")
        code = chars(emit_kernel(roots, spec_for("k", ["x", "y"], ["r"])))

        open (newunit=unit, file="/tmp/fortsym_gen_kernel.f90", &
            status="replace", action="write", iostat=ios)
        if (ios /= 0) then
            nfail = nfail + 1
            print *, "FAIL could not write generated kernel"
            return
        end if
        write (unit, "(a)") code
        write (unit, "(a)") "program drive"
        write (unit, "(a)") "  use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "  implicit none"
        write (unit, "(a)") "  real(dp) :: x, y, r"
        write (unit, "(a)") "  integer :: i, j"
        write (unit, "(a)") "  do i = 1, 5"
        write (unit, "(a)") "  do j = 1, 5"
        write (unit, "(a)") "    x = 0.1_dp*i"
        write (unit, "(a)") "    y = 0.1_dp*j"
        write (unit, "(a)") "    call k(x, y, r)"
        write (unit, "(a)") "    write(*,'(es24.16)') r"
        write (unit, "(a)") "  end do"
        write (unit, "(a)") "  end do"
        write (unit, "(a)") "end program drive"
        close (unit)

        call execute_command_line( &
            "gfortran -o /tmp/fortsym_gen_kernel /tmp/fortsym_gen_kernel.f90"// &
            " > /tmp/fortsym_gen_kernel.log 2>&1", wait=.true., exitstat=stat)
        call ok("generated kernel compiles", stat == 0)
        if (stat /= 0) then
            print *, "   see /tmp/fortsym_gen_kernel.log"
            return
        end if

        call execute_command_line( &
            "/tmp/fortsym_gen_kernel > /tmp/fortsym_gen_kernel.out 2>&1", &
            wait=.true., exitstat=stat)
        call ok("generated kernel runs", stat == 0)
        if (stat /= 0) return

        open (newunit=unit, file="/tmp/fortsym_gen_kernel.out", &
            status="old", action="read", iostat=ios)
        if (ios /= 0) then
            nfail = nfail + 1
            return
        end if

        agreed = .true.
        do k = 1, 25
            read (unit, *, iostat=ios) got
            if (ios /= 0) exit
            xv = 0.1_dp*(1 + (k - 1)/5)
            yv = 0.1_dp*(1 + mod(k - 1, 5))
            want = sin(xv*yv)**2 + cos(xv*yv) + exp(xv)/(1 + yv**2) &
                - 3*xv/(yv + 1)
            if (abs(got - want) > 1.0e-13_dp*max(1.0_dp, abs(want))) then
                agreed = .false.
                print *, "   mismatch at x=", xv, " y=", yv
                print *, "   got ", got, " want ", want
            end if
        end do
        close (unit)

        call ok("generated kernel agrees numerically", agreed)
    end subroutine test_generated_kernel_compiles_and_agrees

end program test_fortsym_kernel
