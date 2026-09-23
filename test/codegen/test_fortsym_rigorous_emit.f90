!> Rigorous emission: the ball and interval leaves enclose high-precision
!> values of the expression, and the float leaf of the same DAG agrees with
!> them. The oracle is fortsym's requested-precision evaluation at exact
!> rational points (40 digits), which shares no code with the emitter or the
!> runtimes; the emitted Fortran is compiled and run by gfortran.
program test_fortsym_rigorous_emit
    use, intrinsic :: iso_fortran_env, only: real64, real128, int64
    use fortsym, only: arena_t, expr_t, sym, rat, real_expr, pi_expr, i_expr, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        sqrt, sin, numeric_precision_text, numeric_complex_text, &
        numeric_complex_text_t
    use fortsym_string, only: str_t, str, chars
    use fortsym_subs, only: subs_many
    use fortsym_rigorous_emit, only: rigorous_kernel_spec_t, ball_runtime, &
        interval_runtime, emit_rigorous_kernel, emit_float_kernel
    implicit none

    integer, parameter :: dp = real64, qp = real128
    integer, parameter :: npts = 24
    real(qp), parameter :: rel_hp = 1.0e-36_qp
    integer :: failures = 0
    type(arena_t), target :: arena
    type(expr_t) :: x, y, e(4)
    character(:), allocatable :: workdir, rtdir

    x = sym(arena, "x")
    y = sym(arena, "y")
    e(1) = (x**2 - 2*x + q(1, 3))/(y + 5) + sqrt(x + 2)*y**3 - pi_expr(arena)*x/7
    e(2) = x**5 - 3*x**4 + q(7, 2)*x**2 - x + q(1, 10)
    e(3) = x**q(-3, 2)*y + 1/(x*y) + (x - y)**2
    e(4) = i_expr(arena)*x*y - y**2/(1 + x**2)

    call test_refusals()
    call test_source_shape()
    call locate_dirs()
    call test_enclosure()

    if (failures > 0) then
        print '(a,i0,a)', "FAIL  ", failures, " rigorous emission checks"
        error stop 1
    end if
    print '(a)', "PASS  rigorous emission"

contains

    function q(p, d) result(r)
        integer, intent(in) :: p, d
        type(expr_t) :: r

        r = rat(arena, int(p, int64), int(d, int64))
    end function q

    subroutine check(label, cond)
        character(*), intent(in) :: label
        logical, intent(in) :: cond

        if (cond) then
            print '(a)', "PASS  "//label
        else
            print '(a)', "FAIL  "//label
            failures = failures + 1
        end if
    end subroutine check

    function spec_of(name, args, outs) result(s)
        character(*), intent(in) :: name
        character(*), intent(in) :: args(:), outs(:)
        type(rigorous_kernel_spec_t) :: s
        integer :: k

        s%name = str(name)
        allocate (s%args(size(args)), s%outputs(size(outs)))
        do k = 1, size(args)
            s%args(k) = str(trim(args(k)))
        end do
        do k = 1, size(outs)
            s%outputs(k) = str(trim(outs(k)))
        end do
        s%runtime = ball_runtime()
    end function spec_of

    subroutine test_refusals()
        type(rigorous_kernel_spec_t) :: s
        type(str_t) :: src
        logical :: ok
        character(:), allocatable :: why

        s = spec_of("k", ["x"], ["r"])
        src = emit_rigorous_kernel([real_expr(arena, 0.1_dp)*x], s, ok, why)
        call check("decimal literal refused", .not. ok .and. index(why, "decimal") > 0)
        src = emit_rigorous_kernel([sin(x)], s, ok, why)
        call check("function outside the interface refused", &
            .not. ok .and. index(why, "sin") > 0)
        src = emit_rigorous_kernel([x*y], s, ok, why)
        call check("free symbol refused", .not. ok .and. index(why, "y") > 0)
        s%runtime = interval_runtime()
        src = emit_rigorous_kernel([i_expr(arena)*x], s, ok, why)
        call check("imaginary unit refused by the interval runtime", .not. ok)
        s%runtime = ball_runtime()
        src = emit_float_kernel([i_expr(arena)*x], s, ok, why)
        call check("complex value to a real float output refused", .not. ok)
        src = emit_rigorous_kernel([x**q(1, 3)], s, ok, why)
        call check("cube root refused", .not. ok .and. index(why, "exponent") > 0)
    end subroutine test_refusals

    subroutine test_source_shape()
        type(rigorous_kernel_spec_t) :: s
        type(str_t) :: src, fsrc
        logical :: ok, fok
        character(:), allocatable :: why, t, ft

        s = spec_of("k2", ["x"], ["r"])
        src = emit_rigorous_kernel([e(2)], s, ok, why)
        fsrc = emit_float_kernel([e(2)], s, fok, why)
        t = chars(src)
        ft = chars(fsrc)
        if (.not. (ok .and. fok)) print '(a)', "      "//why
        call check("polynomial emits", ok .and. fok)
        call check("polynomial uses Horner, not powers", &
            index(t, "bpowi") == 0 .and. index(t, "bmul") > 0)
        call check("rational 7/2 is an exact scaling", index(t, "3.5") > 0)
        call check("non-dyadic 1/10 is a runtime division", index(t, "bdiv") > 0)
        call check("float leaf has the same operation count", &
            count_char(t, "=") == count_char(ft, "="))
        s = spec_of("k1", ["x", "y"], ["r"])
        src = emit_rigorous_kernel([e(1)], s, ok, why)
        t = chars(src)
        call check("pi is an enclosure", index(t, "benclose") > 0)
        call check("square root uses the runtime", index(t, "bsqrt") > 0)
        call check("rigorous leaf is pure", index(t, "pure subroutine k1") > 0)
    end subroutine test_source_shape

    integer function count_char(text, c) result(n)
        character(*), intent(in) :: text, c
        integer :: k

        n = 0
        do k = 1, len(text)
            if (text(k:k) == c) n = n + 1
        end do
    end function count_char

    subroutine locate_dirs()
        integer :: n, stat
        character(len=4096) :: buf
        logical :: there

        call get_environment_variable("FORTSYM_TEST_RUNTIME_DIR", buf, n, stat)
        if (stat == 0 .and. n > 0) then
            rtdir = trim(buf)
        else
            rtdir = "test/codegen/runtime"
        end if
        if (rtdir(1:1) /= "/") then
            call getcwd(buf, stat)
            rtdir = trim(buf)//"/"//rtdir
        end if
        inquire (file=rtdir//"/fortsym_ball_runtime.f90", exist=there)
        call check("reference runtimes found", there)
        workdir = "/tmp/fortsym_rigorous_emit"
        call execute_command_line("mkdir -p "//workdir, exitstat=stat)
    end subroutine locate_dirs

    subroutine write_text(path, text)
        character(*), intent(in) :: path, text
        integer :: u

        open (newunit=u, file=path, status="replace", action="write", access="stream", &
            form="unformatted")
        write (u) text
        close (u)
    end subroutine write_text

    function kernels_module() result(text)
        character(:), allocatable :: text
        type(rigorous_kernel_spec_t) :: s
        type(str_t) :: src
        logical :: ok
        character(:), allocatable :: why

        text = "module gen_kernels"//new_line("a")//"contains"//new_line("a")
        s = spec_of("kf", ["x", "y"], ["r1", "r2", "r3"])
        src = emit_float_kernel(e(1:3), s, ok, why)
        if (.not. ok) print '(a)', "      "//why
        call check("float leaf emits", ok)
        text = text//chars(src)
        s%name = str("kb3")
        src = emit_rigorous_kernel(e(1:3), s, ok, why)
        call check("ball leaf emits", ok)
        text = text//chars(src)
        s%name = str("ki")
        s%runtime = interval_runtime()
        src = emit_rigorous_kernel(e(1:3), s, ok, why)
        call check("interval leaf emits", ok)
        text = text//chars(src)
        s = spec_of("kc", ["x", "y"], ["r4"])
        s%complex_outputs = .true.
        src = emit_float_kernel(e(4:4), s, ok, why)
        call check("complex float leaf emits", ok)
        text = text//chars(src)
        s%name = str("kb4")
        s%elemental_procedure = .true.
        src = emit_rigorous_kernel(e(4:4), s, ok, why)
        call check("elemental complex ball leaf emits", ok)
        text = text//chars(src)//"end module gen_kernels"//new_line("a")
    end function kernels_module

    function driver() result(text)
        character(:), allocatable :: text
        character(len=*), parameter :: nl = new_line("a")

        text = "program drive"//nl// &
            "use, intrinsic :: iso_fortran_env, only: dp => real64"//nl// &
            "use fortsym_ball_runtime"//nl//"use fortsym_interval_runtime"//nl// &
            "use gen_kernels"//nl//"implicit none"//nl// &
            "integer :: n, k, kx, ky, kr, u, v, j"//nl// &
            "real(dp) :: xv, yv, rv, f(3)"//nl//"complex(dp) :: c4"//nl// &
            "type(ball_t) :: b(4)"//nl//"type(interval_t) :: t(3)"//nl// &
            "open(newunit=u, file='points.txt', action='read')"//nl// &
            "open(newunit=v, file='out.txt', action='write')"//nl// &
            "read(u,*) n"//nl//"do k = 1, n"//nl// &
            "read(u,*) kx, ky, kr"//nl// &
            "xv = kx/256.0_dp; yv = ky/256.0_dp; rv = kr/1048576.0_dp"//nl// &
            "call kf(xv, yv, f(1), f(2), f(3)); call kc(xv, yv, c4)"//nl// &
            "write(v,'(5es26.17e3)') f, real(c4), aimag(c4)"//nl// &
            "call kb3(bpoint(xv), bpoint(yv), b(1), b(2), b(3))"//nl// &
            "call kb4(bpoint(xv), bpoint(yv), b(4))"//nl// &
            "write(v,'(12es26.17e3)') (real(b(j)%c), aimag(b(j)%c), b(j)%r, j=1,4)"//nl// &
            "call ki(ipoint(xv), ipoint(yv), t(1), t(2), t(3))"//nl// &
            "write(v,'(6es26.17e3)') (t(j)%lo, t(j)%hi, j=1,3)"//nl// &
            "call kb3(benclose(xv, rv), benclose(yv, rv), b(1), b(2), b(3))"//nl// &
            "call kb4(benclose(xv, rv), benclose(yv, rv), b(4))"//nl// &
            "write(v,'(12es26.17e3)') (real(b(j)%c), aimag(b(j)%c), b(j)%r, j=1,4)"//nl// &
            "call ki(ienclose(xv, rv), ienclose(yv, rv), t(1), t(2), t(3))"//nl// &
            "write(v,'(6es26.17e3)') (t(j)%lo, t(j)%hi, j=1,3)"//nl// &
            "end do"//nl//"end program drive"//nl
    end function driver

    !> High-precision value of e(k) at x = xn/2**20, y = yn/2**20.
    subroutine hp_value(k, xn, yn, re, im, ok)
        integer, intent(in) :: k
        integer(int64), intent(in) :: xn, yn
        real(qp), intent(out) :: re, im
        logical, intent(out) :: ok
        type(expr_t) :: v
        character(:), allocatable :: text, why
        type(numeric_complex_text_t) :: c
        integer :: ios

        re = 0.0_qp
        im = 0.0_qp
        v = subs_many(e(k), [x, y], [rat(arena, xn, 1048576_int64), &
            rat(arena, yn, 1048576_int64)])
        if (k < 4) then
            call numeric_precision_text(v, 40, text, ok, why)
            if (.not. ok) return
            read (text, *, iostat=ios) re
        else
            call numeric_complex_text(v, 40, c, ok, why)
            if (.not. ok) return
            read (c%real, *, iostat=ios) re
            if (ios == 0) read (c%imag, *, iostat=ios) im
        end if
        ok = ios == 0
    end subroutine hp_value

    logical function in_ball(re, im, c_re, c_im, r)
        real(qp), intent(in) :: re, im
        real(dp), intent(in) :: c_re, c_im, r
        real(qp) :: d

        d = sqrt((re - real(c_re, qp))**2 + (im - real(c_im, qp))**2)
        in_ball = d + rel_hp*(abs(re) + abs(im)) + 1.0e-300_qp <= real(r, qp)
    end function in_ball

    logical function in_interval(v, lo, hi)
        real(qp), intent(in) :: v
        real(dp), intent(in) :: lo, hi
        real(qp) :: d

        d = rel_hp*abs(v) + 1.0e-300_qp
        in_interval = real(lo, qp) <= v - d .and. v + d <= real(hi, qp)
    end function in_interval

    subroutine test_enclosure()
        integer :: u, k, j, s, stat, bad_float, bad_ball, bad_int, bad_wide, bad_tight
        integer :: kx(npts), ky(npts), kr(npts)
        integer(int64) :: seed, xn, yn
        real(dp) :: f(5), b(12), t(6), bw(12), tw(6)
        real(qp) :: re, im, v(4), vi(4)
        logical :: ok, hp_ok
        integer, parameter :: sx(5) = [0, 1, 1, -1, -1], sy(5) = [0, 1, -1, 1, -1]
        character(len=1024) :: cmd

        seed = 20260923_int64
        seed = mod(seed, 2147483647_int64)
        do k = 1, npts
            kx(k) = 128 + int(mod(lcg(seed), 385_int64))
            ky(k) = 64 + int(mod(lcg(seed), 705_int64))
            kr(k) = 1 + int(mod(lcg(seed), 64_int64))
        end do
        kx(1) = 256
        ky(1) = 256
        call write_text(workdir//"/kernels.f90", kernels_module())
        call write_text(workdir//"/drive.f90", driver())
        open (newunit=u, file=workdir//"/points.txt", status="replace", action="write")
        write (u, *) npts
        do k = 1, npts
            write (u, *) kx(k), ky(k), kr(k)
        end do
        close (u)
        cmd = "cd "//workdir//" && gfortran -O1 -ffp-contract=off -o drive "// &
            rtdir//"/fortsym_ball_runtime.f90 "//rtdir//"/fortsym_interval_runtime.f90 "// &
            "kernels.f90 drive.f90 > build.log 2>&1 && ./drive"
        call execute_command_line(trim(cmd), exitstat=stat)
        call check("emitted kernels compile and run", stat == 0)
        if (stat /= 0) return

        bad_float = 0
        bad_ball = 0
        bad_int = 0
        bad_wide = 0
        bad_tight = 0
        hp_ok = .true.
        open (newunit=u, file=workdir//"/out.txt", action="read")
        do k = 1, npts
            read (u, *) f
            read (u, *) b
            read (u, *) t
            read (u, *) bw
            read (u, *) tw
            xn = int(kx(k), int64)*4096_int64
            yn = int(ky(k), int64)*4096_int64
            do j = 1, 4
                call hp_value(j, xn, yn, v(j), vi(j), ok)
                hp_ok = hp_ok .and. ok
            end do
            do j = 1, 3
                if (abs(real(f(j), qp) - v(j)) > 1.0e-12_qp*max(1.0_qp, abs(v(j)))) &
                    bad_float = bad_float + 1
                if (.not. in_ball(v(j), 0.0_qp, b(3*j - 2), b(3*j - 1), b(3*j))) &
                    bad_ball = bad_ball + 1
                if (.not. in_interval(v(j), t(2*j - 1), t(2*j))) bad_int = bad_int + 1
                if (b(3*j) > 1.0e-12_dp*max(1.0_dp, abs(b(3*j - 2))) .or. &
                    t(2*j) - t(2*j - 1) > 2.0e-12_dp*max(1.0_dp, abs(t(2*j)))) &
                    bad_tight = bad_tight + 1
            end do
            if (abs(real(f(4), qp) - v(4)) + abs(real(f(5), qp) - vi(4)) > &
                1.0e-12_qp*max(1.0_qp, abs(v(4)) + abs(vi(4)))) bad_float = bad_float + 1
            if (.not. in_ball(v(4), vi(4), b(10), b(11), b(12))) bad_ball = bad_ball + 1
            do s = 1, 5
                do j = 1, 4
                    call hp_value(j, xn + sx(s)*kr(k), yn + sy(s)*kr(k), re, im, ok)
                    hp_ok = hp_ok .and. ok
                    if (.not. in_ball(re, im, bw(3*j - 2), bw(3*j - 1), bw(3*j))) &
                        bad_wide = bad_wide + 1
                    if (j < 4) then
                        if (.not. in_interval(re, tw(2*j - 1), tw(2*j))) &
                            bad_wide = bad_wide + 1
                    end if
                end do
            end do
        end do
        close (u)
        call check("high-precision oracle evaluates every point", hp_ok)
        call check("float leaf agrees with the oracle to 1e-12", bad_float == 0)
        call check("ball leaf encloses the oracle at exact points", bad_ball == 0)
        call check("interval leaf encloses the oracle at exact points", bad_int == 0)
        call check("point enclosures are tight (radius <= 1e-12 relative)", &
            bad_tight == 0)
        call check("widened enclosures contain the oracle across the input box", &
            bad_wide == 0)
    end subroutine test_enclosure

    integer(int64) function lcg(seed)
        integer(int64), intent(inout) :: seed

        seed = mod(48271_int64*seed, 2147483647_int64)
        lcg = seed
    end function lcg

end program test_fortsym_rigorous_emit
