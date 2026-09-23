program test_fortsym_kernel_typed
    ! Independent oracle: a forward-mode dual-number type implements +, -,
    ! *, /, integer **, sin, cos, sqrt, exp by the standard AD chain rule
    ! (written once here, never touched by the emitter). The emitted
    ! operator-overloaded kernel for
    !     f(x, y) = sin(x*y)**2 + sqrt(x)/y + 0.5*x
    ! is compiled against that type and its value and both partial
    ! derivatives are checked against (a) a hand-differentiated closed form
    ! and (b) a central finite difference, neither derived from the emitter.
    use, intrinsic :: iso_fortran_env, only: real64, int64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sin, sqrt, &
        rat, operator(+), operator(*), operator(/), operator(**)
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_typed, only: typed_kernel_spec_t, emit_typed_kernel
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_dual_kernel_compiles_and_matches_oracle()

    if (nfail == 0) then
        print *, "test_fortsym_kernel_typed: all checks passed"
    else
        print *, "test_fortsym_kernel_typed: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine test_dual_kernel_compiles_and_matches_oracle()
        type(arena_t), target :: arena
        type(expr_t) :: x, y, f
        type(kernel_ir_t) :: ir
        type(typed_kernel_spec_t) :: spec
        type(str_t) :: source
        logical :: good
        character(:), allocatable :: message
        integer :: unit, ios, stat
        real(dp) :: xv, yv, h
        real(dp) :: f0, dfdx_ad, dfdy_ad, dfdx_hand, dfdy_hand
        real(dp) :: fplus, fminus, dfdx_fd, dfdy_fd

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        f = sin(x*y)**2 + sqrt(x)/y + rat(arena, 1_int64, 2_int64)*x

        call lower_kernel_ir([f], ir, good, message)
        call ok("typed kernel: IR lowers", good)
        if (.not. good) return

        spec%name = str("dual_demo_f")
        allocate (spec%args(2), spec%outputs(1))
        spec%args(1) = str("x")
        spec%args(2) = str("y")
        spec%outputs(1) = str("f")
        spec%type_name = str("dual_t")
        spec%literal_constructor = str("dual_t")

        source = emit_typed_kernel([f], spec, good, message)
        call ok("typed kernel emits", good)
        if (.not. good) then
            print *, "typed kernel emit error: ", message
            return
        end if
        call ok("typed kernel uses only dual_t temporaries", &
            index(chars(source), "real(") == 0)
        call ok("typed kernel calls sin by its canonical name", &
            index(chars(source), "sin(") > 0)
        call ok("typed kernel calls sqrt by its canonical name", &
            index(chars(source), "sqrt(") > 0)

        open (newunit=unit, file="/tmp/fortsym_dual_demo.f90", &
            status="replace", action="write", iostat=ios)
        call ok("typed kernel fixture opens", ios == 0)
        if (ios /= 0) return
        call write_dual_number_module(unit)
        write (unit, "(a)") "module dual_demo_kernel_mod"
        write (unit, "(a)") "    use dual_number_mod"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "contains"
        write (unit, "(a)") chars(source)
        write (unit, "(a)") "end module dual_demo_kernel_mod"
        write (unit, "(a)") "program drive_dual_demo"
        write (unit, "(a)") &
            "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, "(a)") "    use dual_number_mod, only: dual_t"
        write (unit, "(a)") "    use dual_demo_kernel_mod, only: dual_demo_f"
        write (unit, "(a)") "    implicit none"
        write (unit, "(a)") "    type(dual_t) :: xd, yd, fd"
        write (unit, "(a)") "    real(dp) :: xv, yv"
        write (unit, "(a)") "    xv = 0.6_dp"
        write (unit, "(a)") "    yv = 1.3_dp"
        write (unit, "(a)") "    xd = dual_t(xv, 1.0_dp)"
        write (unit, "(a)") "    yd = dual_t(yv, 0.0_dp)"
        write (unit, "(a)") "    call dual_demo_f(xd, yd, fd)"
        write (unit, "(a)") "    write (*, '(3(es24.16,1x))') fd%v, fd%d"
        write (unit, "(a)") "    xd = dual_t(xv, 0.0_dp)"
        write (unit, "(a)") "    yd = dual_t(yv, 1.0_dp)"
        write (unit, "(a)") "    call dual_demo_f(xd, yd, fd)"
        write (unit, "(a)") "    write (*, '(es24.16)') fd%d"
        write (unit, "(a)") "end program drive_dual_demo"
        close (unit)

        call execute_command_line( &
            "gfortran -o /tmp/fortsym_dual_demo /tmp/fortsym_dual_demo.f90 "// &
            "> /tmp/fortsym_dual_demo.log 2>&1", wait=.true., exitstat=stat)
        call ok("typed dual kernel compiles", stat == 0)
        if (stat /= 0) return

        block
            integer :: run_unit
            call execute_command_line( &
                "/tmp/fortsym_dual_demo > /tmp/fortsym_dual_demo.out", &
                wait=.true., exitstat=stat)
            call ok("typed dual kernel runs", stat == 0)
            if (stat /= 0) return
            open (newunit=run_unit, file="/tmp/fortsym_dual_demo.out", &
                status="old", action="read", iostat=ios)
            call ok("typed dual kernel output opens", ios == 0)
            if (ios /= 0) return
            read (run_unit, *) f0, dfdx_ad
            read (run_unit, *) dfdy_ad
            close (run_unit)
        end block

        xv = 0.6_dp
        yv = 1.3_dp

        ! Hand-differentiated closed form, independent of both the emitter
        ! and the dual-number AD chain rule.
        dfdx_hand = 2.0_dp*sin(xv*yv)*cos(xv*yv)*yv + &
            1.0_dp/(2.0_dp*sqrt(xv)*yv) + 0.5_dp
        dfdy_hand = 2.0_dp*sin(xv*yv)*cos(xv*yv)*xv - sqrt(xv)/yv**2

        call ok("dual kernel value matches closed form", &
            abs(f0 - (sin(xv*yv)**2 + sqrt(xv)/yv + 0.5_dp*xv)) < 1.0e-12_dp)
        call ok("dual kernel df/dx matches hand derivative", &
            abs(dfdx_ad - dfdx_hand) < 1.0e-10_dp)
        call ok("dual kernel df/dy matches hand derivative", &
            abs(dfdy_ad - dfdy_hand) < 1.0e-10_dp)

        ! Central finite difference, a second independent oracle.
        h = 1.0e-6_dp
        fplus = sin((xv + h)*yv)**2 + sqrt(xv + h)/yv + 0.5_dp*(xv + h)
        fminus = sin((xv - h)*yv)**2 + sqrt(xv - h)/yv + 0.5_dp*(xv - h)
        dfdx_fd = (fplus - fminus)/(2.0_dp*h)
        fplus = sin(xv*(yv + h))**2 + sqrt(xv)/(yv + h) + 0.5_dp*xv
        fminus = sin(xv*(yv - h))**2 + sqrt(xv)/(yv - h) + 0.5_dp*xv
        dfdy_fd = (fplus - fminus)/(2.0_dp*h)

        call ok("dual kernel df/dx matches finite difference", &
            abs(dfdx_ad - dfdx_fd) < 1.0e-6_dp)
        call ok("dual kernel df/dy matches finite difference", &
            abs(dfdy_ad - dfdy_fd) < 1.0e-6_dp)
    end subroutine test_dual_kernel_compiles_and_matches_oracle

    !> The oracle numeric type: forward-mode dual numbers, +, -, *, /,
    !> integer **, sin, cos, sqrt, exp by the standard AD chain rule. This
    !> is written once, independently of fortsym_kernel_typed, and never
    !> touched by the emitter under test.
    subroutine write_dual_number_module(unit)
        integer, intent(in) :: unit
        write (unit, "(a)") "module dual_number_mod"
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
        write (unit, "(a)") "        module procedure dual_sub, dual_neg"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(*)"
        write (unit, "(a)") "        module procedure dual_mul"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(/)"
        write (unit, "(a)") "        module procedure dual_div"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface operator(**)"
        write (unit, "(a)") "        module procedure dual_pow_i"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface sin"
        write (unit, "(a)") "        module procedure dual_sin"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface cos"
        write (unit, "(a)") "        module procedure dual_cos"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface sqrt"
        write (unit, "(a)") "        module procedure dual_sqrt"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "    interface exp"
        write (unit, "(a)") "        module procedure dual_exp"
        write (unit, "(a)") "    end interface"
        write (unit, "(a)") "contains"
        write (unit, "(a)") "    function dual_add(a, b) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = a%v + b%v; r%d = a%d + b%d"
        write (unit, "(a)") "    end function dual_add"
        write (unit, "(a)") "    function dual_sub(a, b) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = a%v - b%v; r%d = a%d - b%d"
        write (unit, "(a)") "    end function dual_sub"
        write (unit, "(a)") "    function dual_neg(a) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = -a%v; r%d = -a%d"
        write (unit, "(a)") "    end function dual_neg"
        write (unit, "(a)") "    function dual_mul(a, b) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = a%v*b%v; r%d = a%d*b%v + a%v*b%d"
        write (unit, "(a)") "    end function dual_mul"
        write (unit, "(a)") "    function dual_div(a, b) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a, b"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = a%v/b%v"
        write (unit, "(a)") "        r%d = (a%d*b%v - a%v*b%d)/(b%v*b%v)"
        write (unit, "(a)") "    end function dual_div"
        write (unit, "(a)") "    function dual_pow_i(a, n) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        integer, intent(in) :: n"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = a%v**n"
        write (unit, "(a)") "        r%d = real(n, dp)*a%v**(n - 1)*a%d"
        write (unit, "(a)") "    end function dual_pow_i"
        write (unit, "(a)") "    function dual_sin(a) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = sin(a%v); r%d = cos(a%v)*a%d"
        write (unit, "(a)") "    end function dual_sin"
        write (unit, "(a)") "    function dual_cos(a) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = cos(a%v); r%d = -sin(a%v)*a%d"
        write (unit, "(a)") "    end function dual_cos"
        write (unit, "(a)") "    function dual_sqrt(a) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = sqrt(a%v); r%d = a%d/(2.0_dp*sqrt(a%v))"
        write (unit, "(a)") "    end function dual_sqrt"
        write (unit, "(a)") "    function dual_exp(a) result(r)"
        write (unit, "(a)") "        type(dual_t), intent(in) :: a"
        write (unit, "(a)") "        type(dual_t) :: r"
        write (unit, "(a)") "        r%v = exp(a%v); r%d = exp(a%v)*a%d"
        write (unit, "(a)") "    end function dual_exp"
        write (unit, "(a)") "end module dual_number_mod"
    end subroutine write_dual_number_module

    subroutine ok(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

end program test_fortsym_kernel_typed
