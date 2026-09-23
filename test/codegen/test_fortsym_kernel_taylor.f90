program test_fortsym_kernel_taylor
    ! Independent oracle: a hand-written truncated-power-series harness
    ! implements ts_mul, ts_div, ts_sincos, ts_exp by the textbook Taylor/AD
    ! recurrences (never touched by the emitter). The emitted order-by-order
    ! step for
    !     f1(x) = sin(x)
    !     f2(x) = x/(1 - x)
    !     f3(x) = exp(x)
    ! evaluated at the series x(t) = t (x_0=0, x_1=1, rest 0) is driven for
    ! k = 0..N by that harness, and the resulting coefficients are checked
    ! against two closed forms neither derived from the emitter or the
    ! harness: the n!-based Maclaurin coefficients of sin and exp, and the
    ! geometric series t/(1-t) = sum_n t^n.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sin, exp, num, &
        operator(+), operator(-), operator(*), operator(/)
    use fortsym_kernel_taylor, only: taylor_emit_spec_t, emit_taylor_step
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: N = 8
    integer :: nfail = 0

    call test_taylor_step_compiles_and_matches_oracle()

    if (nfail == 0) then
        print *, "test_fortsym_kernel_taylor: all checks passed"
    else
        print *, "test_fortsym_kernel_taylor: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine test_taylor_step_compiles_and_matches_oracle()
        type(arena_t), target :: arena
        type(expr_t) :: x, f1, f2, f3
        type(taylor_emit_spec_t) :: spec
        type(str_t) :: source
        type(str_t), allocatable :: params(:)
        logical :: good
        character(:), allocatable :: message, header
        integer :: unit, ios, stat, j, nl

        call arena%init()
        x = sym(arena, "x")
        f1 = sin(x)
        f2 = x/(num(arena, 1) - x)
        f3 = exp(x)

        spec%name = str("taylor_demo_step")
        allocate (spec%args(1), spec%outputs(3))
        spec%args(1) = str("x")
        spec%outputs(1) = str("f1")
        spec%outputs(2) = str("f2")
        spec%outputs(3) = str("f3")
        spec%mul_name = str("ts_mul")
        spec%div_name = str("ts_div")
        spec%sincos_name = str("ts_sincos")
        spec%exp_name = str("ts_exp")

        source = emit_taylor_step([f1, f2, f3], spec, good, message)
        call ok("taylor step emits", good)
        if (.not. good) then
            print *, "taylor step emit error: ", message
            return
        end if
        call ok("taylor step calls ts_mul or ts_div", &
            index(chars(source), "call ts_") > 0)
        call ok("taylor step calls ts_sincos", &
            index(chars(source), "call ts_sincos") > 0)
        call ok("taylor step calls ts_exp", &
            index(chars(source), "call ts_exp") > 0)

        nl = index(chars(source), achar(10))
        header = chars(source)
        header = header(1:nl - 1)
        call extract_params(header, params)
        params = params(2:size(params))

        open (newunit=unit, file="/tmp/fortsym_taylor_demo.f90", &
            status="replace", action="write", iostat=ios)
        call ok("taylor step fixture opens", ios == 0)
        if (ios /= 0) return
        call write_ts_module(unit)
        write (unit, "(a)") "module taylor_demo_kernel_mod"
        write (unit, "(a)") "    use ts_recurrence_mod"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "contains"
        write (unit, "(a)") chars(source)
        write (unit, "(a)") "end module taylor_demo_kernel_mod"
        write (unit, "(a)") "program drive_taylor_demo"
        write (unit, "(a)") &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "    use taylor_demo_kernel_mod, only: taylor_demo_step"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "    integer, parameter :: N = 8"
        do j = 1, size(params)
            write (unit, "(a)") "    real(dp) :: "//chars(params(j))//"(0:N)"
        end do
        write (unit, "(a)") "    integer :: k"
        do j = 1, size(params)
            write (unit, "(a)") "    "//chars(params(j))//" = 0.0_dp"
        end do
        write (unit, "(a)") "    x(0) = 0.0_dp"
        write (unit, "(a)") "    x(1) = 1.0_dp"
        write (unit, "(a)") "    do k = 0, N"
        write (unit, "(a)") "        call taylor_demo_step(k, "// &
            join_names(params)//")"
        write (unit, "(a)") "    end do"
        write (unit, "(a)") "    do k = 0, N"
        write (unit, "(a)") "        write (*, '(3(es24.16,1x))') f1(k), f2(k), f3(k)"
        write (unit, "(a)") "    end do"
        write (unit, "(a)") "end program drive_taylor_demo"
        close (unit)

        call execute_command_line( &
            "gfortran -o /tmp/fortsym_taylor_demo /tmp/fortsym_taylor_demo.f90 "// &
            "> /tmp/fortsym_taylor_demo.log 2>&1", wait=.true., exitstat=stat)
        call ok("taylor step fixture compiles", stat == 0)
        if (stat /= 0) return

        call execute_command_line( &
            "/tmp/fortsym_taylor_demo > /tmp/fortsym_taylor_demo.out", &
            wait=.true., exitstat=stat)
        call ok("taylor step fixture runs", stat == 0)
        if (stat /= 0) return

        call check_against_oracle()
    end subroutine test_taylor_step_compiles_and_matches_oracle

    !> Independent oracles: sin/exp Maclaurin coefficients from n!, and the
    !> geometric series for t/(1-t). None of these formulas appears in
    !> fortsym_kernel_taylor or in the hand-written ts_* harness.
    subroutine check_against_oracle()
        integer :: run_unit, ios, k
        real(dp) :: f1k, f2k, f3k, expected1, expected2, expected3, fact

        open (newunit=run_unit, file="/tmp/fortsym_taylor_demo.out", &
            status="old", action="read", iostat=ios)
        call ok("taylor step output opens", ios == 0)
        if (ios /= 0) return

        fact = 1.0_dp
        do k = 0, N
            read (run_unit, *) f1k, f2k, f3k
            if (k > 0) fact = fact*real(k, dp)

            if (mod(k, 2) == 0) then
                expected1 = 0.0_dp
            else
                expected1 = (-1.0_dp)**((k - 1)/2)/fact
            end if
            expected2 = merge(0.0_dp, 1.0_dp, k == 0)
            expected3 = 1.0_dp/fact

            call ok("sin series coefficient matches n!-based formula", &
                abs(f1k - expected1) < 1.0e-9_dp)
            call ok("geometric series coefficient matches closed form", &
                abs(f2k - expected2) < 1.0e-9_dp)
            call ok("exp series coefficient matches n!-based formula", &
                abs(f3k - expected3) < 1.0e-9_dp)
        end do
        close (run_unit)
    end subroutine check_against_oracle

    subroutine extract_params(header, names)
        character(*), intent(in) :: header
        type(str_t), allocatable, intent(out) :: names(:)
        integer :: lp, rp, p, start
        character(:), allocatable :: inner, tok

        lp = index(header, "(")
        rp = index(header, ")")
        inner = header(lp + 1:rp - 1)
        allocate (names(0))
        start = 1
        do
            p = index(inner(start:), ",")
            if (p == 0) then
                tok = trim(adjustl(inner(start:)))
                if (len(tok) > 0) names = [names, str(tok)]
                exit
            else
                tok = trim(adjustl(inner(start:start + p - 2)))
                if (len(tok) > 0) names = [names, str(tok)]
                start = start + p
            end if
        end do
    end subroutine extract_params

    function join_names(names) result(text)
        type(str_t), intent(in) :: names(:)
        character(:), allocatable :: text
        integer :: j

        text = ""
        do j = 1, size(names)
            if (j > 1) text = text//", "
            text = text//chars(names(j))
        end do
    end function join_names

    !> The oracle building blocks: textbook Taylor/AD recurrences, written
    !> once here and never touched by fortsym_kernel_taylor.
    subroutine write_ts_module(unit)
        integer, intent(in) :: unit
        write (unit, "(a)") "module ts_recurrence_mod"
        write (unit, "(a)") "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "contains"
        write (unit, "(a)") "    subroutine ts_mul(k, a, b, c)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        real(dp), intent(in) :: a(0:*), b(0:*)"
        write (unit, "(a)") "        real(dp), intent(inout) :: c(0:*)"
        write (unit, "(a)") "        integer :: j"
        write (unit, "(a)") "        c(k) = 0.0_dp"
        write (unit, "(a)") "        do j = 0, k"
        write (unit, "(a)") "            c(k) = c(k) + a(j)*b(k - j)"
        write (unit, "(a)") "        end do"
        write (unit, "(a)") "    end subroutine ts_mul"
        write (unit, "(a)") "    subroutine ts_div(k, a, b, c)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        real(dp), intent(in) :: a(0:*), b(0:*)"
        write (unit, "(a)") "        real(dp), intent(inout) :: c(0:*)"
        write (unit, "(a)") "        integer :: j"
        write (unit, "(a)") "        real(dp) :: s"
        write (unit, "(a)") "        s = a(k)"
        write (unit, "(a)") "        do j = 1, k"
        write (unit, "(a)") "            s = s - b(j)*c(k - j)"
        write (unit, "(a)") "        end do"
        write (unit, "(a)") "        c(k) = s/b(0)"
        write (unit, "(a)") "    end subroutine ts_div"
        write (unit, "(a)") "    subroutine ts_sincos(k, a, s, c)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        real(dp), intent(in) :: a(0:*)"
        write (unit, "(a)") "        real(dp), intent(inout) :: s(0:*), c(0:*)"
        write (unit, "(a)") "        integer :: j"
        write (unit, "(a)") "        real(dp) :: s1, s2"
        write (unit, "(a)") "        if (k == 0) then"
        write (unit, "(a)") "            s(0) = sin(a(0)); c(0) = cos(a(0))"
        write (unit, "(a)") "        else"
        write (unit, "(a)") "            s1 = 0.0_dp; s2 = 0.0_dp"
        write (unit, "(a)") "            do j = 1, k"
        write (unit, "(a)") "                s1 = s1 + real(j, dp)*a(j)*c(k - j)"
        write (unit, "(a)") "                s2 = s2 + real(j, dp)*a(j)*s(k - j)"
        write (unit, "(a)") "            end do"
        write (unit, "(a)") "            s(k) = s1/real(k, dp)"
        write (unit, "(a)") "            c(k) = -s2/real(k, dp)"
        write (unit, "(a)") "        end if"
        write (unit, "(a)") "    end subroutine ts_sincos"
        write (unit, "(a)") "    subroutine ts_exp(k, a, e)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        real(dp), intent(in) :: a(0:*)"
        write (unit, "(a)") "        real(dp), intent(inout) :: e(0:*)"
        write (unit, "(a)") "        integer :: j"
        write (unit, "(a)") "        real(dp) :: s"
        write (unit, "(a)") "        if (k == 0) then"
        write (unit, "(a)") "            e(0) = exp(a(0))"
        write (unit, "(a)") "        else"
        write (unit, "(a)") "            s = 0.0_dp"
        write (unit, "(a)") "            do j = 1, k"
        write (unit, "(a)") "                s = s + real(j, dp)*a(j)*e(k - j)"
        write (unit, "(a)") "            end do"
        write (unit, "(a)") "            e(k) = s/real(k, dp)"
        write (unit, "(a)") "        end if"
        write (unit, "(a)") "    end subroutine ts_exp"
        write (unit, "(a)") "end module ts_recurrence_mod"
    end subroutine write_ts_module

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

end program test_fortsym_kernel_taylor
