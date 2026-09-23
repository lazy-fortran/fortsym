program test_fortsym_kernel_taylor_typed
    ! Independent oracle for the generic element-type knob
    ! (taylor_emit_spec_t%type_name / %literal_constructor): the emitted
    ! Taylor step is compiled against an independent hand-written "dual"
    ! series type dual_t (v, d) -- forward-mode AD over a scalar parameter
    ! a, layered on top of the same textbook Taylor recurrences -- instead
    ! of real(dp) arrays. x(t) = a*t is seeded as a dual series, and
    !     f1(x) = (2+x)**(-2)     [negative integer power]
    !     f2(x) = (3+x)/(1+x)     [division]
    ! are driven for k = 0..N at a = 2.0. Both the VALUE and the
    ! DERIVATIVE-wrt-a component of every coefficient are checked against
    ! closed forms derived directly from the Maclaurin/binomial series of
    ! (2+a*t)**(-2) and (3+a*t)/(1+a*t), worked out by hand here and never
    ! touched by fortsym_kernel_taylor or by the dual_t harness.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_kernel_taylor, only: taylor_emit_spec_t, emit_taylor_step
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: N = 6
    real(dp), parameter :: a_param = 2.0_dp
    integer :: nfail = 0

    call test_taylor_step_dual_type()

    if (nfail == 0) then
        print *, "test_fortsym_kernel_taylor_typed: all checks passed"
    else
        print *, "test_fortsym_kernel_taylor_typed: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine test_taylor_step_dual_type()
        type(arena_t), target :: arena
        type(expr_t) :: x, f1, f2
        type(taylor_emit_spec_t) :: spec
        type(str_t) :: source
        type(str_t), allocatable :: params(:)
        logical :: good
        character(:), allocatable :: message, header
        integer :: unit, ios, stat, j, nl

        call arena%init()
        x = sym(arena, "x")
        f1 = (num(arena, 2) + x)**(-2)
        f2 = (num(arena, 3) + x)/(num(arena, 1) + x)

        spec%name = str("taylor_dual_step")
        allocate (spec%args(1), spec%outputs(2))
        spec%args(1) = str("x")
        spec%outputs(1) = str("f1")
        spec%outputs(2) = str("f2")
        spec%mul_name = str("ts_mul")
        spec%div_name = str("ts_div")
        spec%sincos_name = str("ts_sincos")
        spec%exp_name = str("ts_exp")
        spec%type_name = str("dual_t")
        spec%literal_constructor = str("dual_t")

        source = emit_taylor_step([f1, f2], spec, good, message)
        call ok("dual taylor step emits", good)
        if (.not. good) then
            print *, "dual taylor step emit error: ", message
            return
        end if
        call ok("dual taylor step calls ts_div (negative power/division)", &
            index(chars(source), "call ts_div") > 0)
        call ok("dual taylor step calls ts_mul", &
            index(chars(source), "call ts_mul") > 0)
        call ok("dual taylor step declares type(dual_t)", &
            index(chars(source), "type(dual_t)") > 0)
        call ok("dual taylor step never has unparenthesized **-", &
            index(chars(source), "**-") == 0)

        nl = index(chars(source), achar(10))
        header = chars(source)
        header = header(1:nl - 1)
        call extract_params(header, params)
        params = params(2:size(params))

        open (newunit=unit, file="/tmp/fortsym_taylor_dual.f90", &
            status="replace", action="write", iostat=ios)
        call ok("dual taylor step fixture opens", ios == 0)
        if (ios /= 0) return
        call write_dual_module(unit)
        write (unit, "(a)") "module taylor_dual_kernel_mod"
        write (unit, "(a)") "    use dual_recurrence_mod"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "contains"
        write (unit, "(a)") chars(source)
        write (unit, "(a)") "end module taylor_dual_kernel_mod"
        write (unit, "(a)") "program drive_taylor_dual"
        write (unit, "(a)") &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "    use dual_recurrence_mod, only: dual_t"
        write (unit, "(a)") "    use taylor_dual_kernel_mod, only: taylor_dual_step"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "    integer, parameter :: N = 6"
        do j = 1, size(params)
            write (unit, "(a)") "    type(dual_t) :: "//chars(params(j))//"(0:N)"
        end do
        write (unit, "(a)") "    integer :: k"
        write (unit, "(a)") "    x(0) = dual_t(0.0_dp, 0.0_dp)"
        write (unit, "(a)") "    x(1) = dual_t(2.0_dp, 1.0_dp)"
        write (unit, "(a)") "    do k = 0, N"
        write (unit, "(a)") "        call taylor_dual_step(k, "// &
            join_names(params)//")"
        write (unit, "(a)") "    end do"
        write (unit, "(a)") "    do k = 0, N"
        write (unit, "(a)") "        write (*, '(4(es24.16,1x))') "// &
            "f1(k)%v, f1(k)%d, f2(k)%v, f2(k)%d"
        write (unit, "(a)") "    end do"
        write (unit, "(a)") "end program drive_taylor_dual"
        close (unit)

        call execute_command_line( &
            "gfortran -o /tmp/fortsym_taylor_dual /tmp/fortsym_taylor_dual.f90 "// &
            "> /tmp/fortsym_taylor_dual.log 2>&1", wait=.true., exitstat=stat)
        call ok("dual taylor step fixture compiles", stat == 0)
        if (stat /= 0) return

        call execute_command_line( &
            "/tmp/fortsym_taylor_dual > /tmp/fortsym_taylor_dual.out", &
            wait=.true., exitstat=stat)
        call ok("dual taylor step fixture runs", stat == 0)
        if (stat /= 0) return

        call check_against_oracle()
    end subroutine test_taylor_step_dual_type

    !> Independent oracle, hand-derived from the binomial/geometric series
    !> of (2+a*t)**(-2) and (3+a*t)/(1+a*t); neither formula, nor its
    !> a-derivative, appears in fortsym_kernel_taylor or in dual_recurrence_mod.
    !>     f1(t) = (2+a t)**-2 = (1/4)(1+a t/2)**-2
    !>           = sum_k (-1)^k (k+1) a^k / 2^(k+2) * t^k
    !>     f2(t) = (3+a t)/(1+a t) = 1 + 2/(1+a t) = 1 + 2 sum_k (-a t)^k
    subroutine check_against_oracle()
        integer :: run_unit, ios, k
        real(dp) :: f1v, f1d, f2v, f2d
        real(dp) :: exp_f1v, exp_f1d, exp_f2v, exp_f2d
        real(dp) :: sign_k, powk, powkm1

        open (newunit=run_unit, file="/tmp/fortsym_taylor_dual.out", &
            status="old", action="read", iostat=ios)
        call ok("dual taylor step output opens", ios == 0)
        if (ios /= 0) return

        do k = 0, N
            read (run_unit, *) f1v, f1d, f2v, f2d

            sign_k = merge(1.0_dp, -1.0_dp, mod(k, 2) == 0)
            powk = a_param**k
            exp_f1v = sign_k*real(k + 1, dp)*powk/2.0_dp**(k + 2)
            if (k == 0) then
                exp_f1d = 0.0_dp
            else
                powkm1 = a_param**(k - 1)
                exp_f1d = sign_k*real(k + 1, dp)*real(k, dp)*powkm1/2.0_dp**(k + 2)
            end if

            if (k == 0) then
                exp_f2v = 3.0_dp
                exp_f2d = 0.0_dp
            else
                exp_f2v = 2.0_dp*sign_k*powk
                exp_f2d = 2.0_dp*sign_k*real(k, dp)*a_param**(k - 1)
            end if

            call ok("f1 value matches binomial-series closed form", &
                abs(f1v - exp_f1v) < 1.0e-9_dp)
            call ok("f1 d/da matches binomial-series closed form", &
                abs(f1d - exp_f1d) < 1.0e-9_dp)
            call ok("f2 value matches geometric-series closed form", &
                abs(f2v - exp_f2v) < 1.0e-9_dp)
            call ok("f2 d/da matches geometric-series closed form", &
                abs(f2d - exp_f2d) < 1.0e-9_dp)
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

    !> The oracle building blocks: a hand-written dual (v, d) forward-mode
    !> AD number type overloading +, -, *, /, and integer ** (needed by the
    !> emitted "**(-...)" reciprocal-power text), plus ts_mul/ts_div using
    !> the same textbook Taylor recurrences as the real(dp) harness, applied
    !> through dual_t arithmetic instead of bare real(dp) arithmetic. None of
    !> this is touched by fortsym_kernel_taylor.
    subroutine write_dual_module(unit)
        integer, intent(in) :: unit
        write (unit, "(a)") "module dual_recurrence_mod"
        write (unit, "(a)") "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "    type :: dual_t"
        write (unit, "(a)") "        real(dp) :: v = 0.0_dp"
        write (unit, "(a)") "        real(dp) :: d = 0.0_dp"
        write (unit, "(a)") "    end type dual_t"
        write (unit, "(a)") "    interface operator(+)"
        write (unit, "(a)") "        module procedure dual_add"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(-)"
        write (unit, "(a)") "        module procedure dual_sub"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(*)"
        write (unit, "(a)") "        module procedure dual_mul"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(/)"
        write (unit, "(a)") "        module procedure dual_div"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(**)"
        write (unit, "(a)") "        module procedure dual_pow_int"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "contains"
        write (unit, "(a)") "    function dual_add(a, b) result(c)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: c"
        write (unit, "(a)") "        c%v = a%v + b%v; c%d = a%d + b%d"
        write (unit, "(a)") "    end function dual_add"
        write (unit, "(a)") "    function dual_sub(a, b) result(c)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: c"
        write (unit, "(a)") "        c%v = a%v - b%v; c%d = a%d - b%d"
        write (unit, "(a)") "    end function dual_sub"
        write (unit, "(a)") "    function dual_mul(a, b) result(c)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: c"
        write (unit, "(a)") "        c%v = a%v*b%v"
        write (unit, "(a)") "        c%d = a%d*b%v + a%v*b%d"
        write (unit, "(a)") "    end function dual_mul"
        write (unit, "(a)") "    function dual_div(a, b) result(c)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: c"
        write (unit, "(a)") "        c%v = a%v/b%v"
        write (unit, "(a)") "        c%d = (a%d*b%v - a%v*b%d)/(b%v*b%v)"
        write (unit, "(a)") "    end function dual_div"
        write (unit, "(a)") "    function dual_pow_int(a, n) result(c)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        integer, intent(in) :: n"
        write (unit, "(a)") "        type(dual_t) :: c"
        write (unit, "(a)") "        integer :: i"
        write (unit, "(a)") "        c = dual_t(1.0_dp, 0.0_dp)"
        write (unit, "(a)") "        do i = 1, abs(n)"
        write (unit, "(a)") "            c = dual_mul(c, a)"
        write (unit, "(a)") "        end do"
        write (unit, "(a)") "        if (n < 0) c = dual_div(dual_t(1.0_dp, 0.0_dp), c)"
        write (unit, "(a)") "    end function dual_pow_int"
        write (unit, "(a)") "    subroutine ts_mul(k, a, b, c)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a(0:*), b(0:*)"
        write (unit, "(a)") "        type(dual_t), intent(inout) :: c(0:*)"
        write (unit, "(a)") "        integer :: j"
        write (unit, "(a)") "        c(k) = dual_t(0.0_dp, 0.0_dp)"
        write (unit, "(a)") "        do j = 0, k"
        write (unit, "(a)") "            c(k) = c(k) + a(j)*b(k - j)"
        write (unit, "(a)") "        end do"
        write (unit, "(a)") "    end subroutine ts_mul"
        write (unit, "(a)") "    subroutine ts_div(k, a, b, c)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a(0:*), b(0:*)"
        write (unit, "(a)") "        type(dual_t), intent(inout) :: c(0:*)"
        write (unit, "(a)") "        integer :: j"
        write (unit, "(a)") "        type(dual_t) :: s"
        write (unit, "(a)") "        s = a(k)"
        write (unit, "(a)") "        do j = 1, k"
        write (unit, "(a)") "            s = s - b(j)*c(k - j)"
        write (unit, "(a)") "        end do"
        write (unit, "(a)") "        c(k) = s/b(0)"
        write (unit, "(a)") "    end subroutine ts_div"
        write (unit, "(a)") "    subroutine ts_sincos(k, a, s, c)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a(0:*)"
        write (unit, "(a)") "        type(dual_t), intent(inout) :: s(0:*), c(0:*)"
        write (unit, "(a)") "        s(k) = a(k); c(k) = a(k)"
        write (unit, "(a)") "    end subroutine ts_sincos"
        write (unit, "(a)") "    subroutine ts_exp(k, a, e)"
        write (unit, "(a)") "        integer, intent(in) :: k"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a(0:*)"
        write (unit, "(a)") "        type(dual_t), intent(inout) :: e(0:*)"
        write (unit, "(a)") "        e(k) = a(k)"
        write (unit, "(a)") "    end subroutine ts_exp"
        write (unit, "(a)") "end module dual_recurrence_mod"
    end subroutine write_dual_module

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

end program test_fortsym_kernel_taylor_typed
