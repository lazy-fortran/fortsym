program test_fortsym_plotfamily
    ! The Wolfram plotting family.
    !
    ! Nothing here compares against a picture. The oracles are properties the
    ! sampler cannot satisfy by accident: every point it keeps must satisfy the
    ! equation that defines the curve, a pole must be missing rather than
    ! flattened to zero, a field that is undefined on part of its rectangle
    ! must refuse, and an overlay of two curves must not be byte-identical to
    ! the same overlay of one.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t
    use fortsym_parse, only: parse_expr_in
    use fortsym_dialect, only: dialect, DIA_WOLFRAM
    use fortsym_plot, only: plot_spec_t, curve_t, sample_curve, &
        sample_parametric_curve, render_surface, set_grid_samples
    use fortsym_wl, only: wl_session_t, wl_session_begin, wl_run_source, &
        wl_binding_count, wl_binding_at, wl_binding_t
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_pole_is_dropped_not_flattened()
    call test_parametric_points_lie_on_the_curve()
    call test_log_axis_drops_non_positive_samples()
    call test_partly_undefined_field_refuses()
    call test_show_keeps_every_curve()
    call test_graphics_primitives_refuse()
    call test_show_of_a_stranger_refuses()

    if (nfail == 0) then
        print *, "PASS test_fortsym_plotfamily"
    else
        print *, "FAIL test_fortsym_plotfamily:", nfail
        error stop 1
    end if

contains

    subroutine fail(what)
        character(*), intent(in) :: what
        print *, "FAIL ", what
        nfail = nfail + 1
    end subroutine fail

    !> Parse one Wolfram expression into `a`.
    function expr_of(a, text) result(e)
        type(arena_t), target, intent(inout) :: a
        character(*),          intent(in)    :: text
        type(expr_t)                         :: e
        logical :: ok
        character(:), allocatable :: why

        e = parse_expr_in(a, text, dialect(DIA_WOLFRAM), ok, why)
        if (.not. ok) call fail("parse: "//text)
    end function expr_of

    !> 1/x over a range straddling the pole. Every surviving point must satisfy
    !> x*y = 1, which a zero standing in for the pole cannot do, and some
    !> points must be missing.
    subroutine test_pole_is_dropped_not_flattened()
        type(arena_t), target :: a
        type(plot_spec_t) :: spec
        type(curve_t) :: c
        type(str_t) :: why
        integer :: k
        logical :: ok

        call a%init()
        spec%variable = str("x")
        spec%lower = -1.0_dp
        spec%upper = 1.0_dp
        spec%samples = 101
        ok = sample_curve(expr_of(a, "1/x"), spec, c, why)
        if (.not. ok) then
            call fail("1/x should still be plottable: "//chars(why))
            return
        end if
        if (size(c%x) >= spec%samples) call fail("the pole was not dropped")
        do k = 1, size(c%x)
            if (abs(c%x(k)*c%y(k) - 1.0_dp) > 1.0e-9_dp) then
                call fail("a kept point does not satisfy x*y = 1")
                return
            end if
        end do
    end subroutine test_pole_is_dropped_not_flattened

    !> {cos t, sin t} must land on the unit circle, which also fixes that the
    !> two components are not swapped or paired across different t.
    subroutine test_parametric_points_lie_on_the_curve()
        type(arena_t), target :: a
        type(plot_spec_t) :: spec
        type(curve_t) :: c
        type(str_t) :: why
        integer :: k
        logical :: ok

        call a%init()
        spec%variable = str("t")
        spec%lower = 0.0_dp
        spec%upper = 6.0_dp
        spec%samples = 64
        ok = sample_parametric_curve(expr_of(a, "Cos[t]"), &
                                     expr_of(a, "Sin[t]"), spec, c, why)
        if (.not. ok) then
            call fail("the unit circle should be plottable: "//chars(why))
            return
        end if
        if (size(c%x) /= spec%samples) call fail("a defined point was dropped")
        do k = 1, size(c%x)
            if (abs(c%x(k)**2 + c%y(k)**2 - 1.0_dp) > 1.0e-9_dp) then
                call fail("a parametric point is off the unit circle")
                return
            end if
        end do
    end subroutine test_parametric_points_lie_on_the_curve

    !> A logarithmic axis has no place for a non-positive value, so those
    !> samples go the way of an undefined one instead of being clamped.
    subroutine test_log_axis_drops_non_positive_samples()
        type(arena_t), target :: a
        type(plot_spec_t) :: spec
        type(curve_t) :: c
        type(str_t) :: why
        integer :: k
        logical :: ok

        call a%init()
        spec%variable = str("x")
        spec%lower = 0.0_dp
        spec%upper = 12.0_dp
        spec%samples = 200
        spec%positive_y = .true.
        ok = sample_curve(expr_of(a, "Sin[x]"), spec, c, why)
        if (.not. ok) then
            call fail("half of a sine is positive: "//chars(why))
            return
        end if
        if (size(c%y) >= spec%samples) call fail("negative samples survived")
        do k = 1, size(c%y)
            if (c%y(k) <= 0.0_dp) then
                call fail("a non-positive sample reached a log axis")
                return
            end if
        end do
    end subroutine test_log_axis_drops_non_positive_samples

    !> Sqrt[x] is undefined on half of this rectangle. A grid cannot drop a
    !> point, so the surface refuses rather than drawing invented values. The
    !> control on the same rectangle rules out a refusal that comes from the
    !> grid rather than from the function.
    subroutine test_partly_undefined_field_refuses()
        type(arena_t), target :: a
        type(plot_spec_t) :: sx, sy
        type(str_t) :: why
        integer :: unit
        logical :: ok, there

        call a%init()
        sx%variable = str("x")
        sx%lower = -1.0_dp
        sx%upper = 1.0_dp
        sy%variable = str("y")
        sy%lower = -1.0_dp
        sy%upper = 1.0_dp
        call set_grid_samples(sx, sy)
        ok = render_surface(expr_of(a, "Sqrt[x]"), sx, sy, &
                            "fortsym-test-should-not-exist.png", why)
        if (ok) call fail("a half-undefined field was drawn anyway")

        ok = render_surface(expr_of(a, "x*y"), sx, sy, &
                            "fortsym-test-control.png", why)
        if (.not. ok) then
            call fail("a defined field was refused: "//chars(why))
            return
        end if
        inquire (file="fortsym-test-control.png", exist=there)
        if (.not. there) then
            call fail("the control surface wrote no file")
            return
        end if
        open (newunit=unit, file="fortsym-test-control.png", status="old")
        close (unit, status="delete")
    end subroutine test_partly_undefined_field_refuses

    !> Show[p1, p2] must draw both curves. Two curves cannot produce the same
    !> bytes as one, so a Show that quietly returned its first argument, or
    !> drew only one of the two, fails here.
    subroutine test_show_keeps_every_curve()
        character(:), allocatable :: one, two
        integer :: n_one, n_two

        call run_and_size("s = Show[Plot[Sin[x], {x, 0, 6}]]", one, n_one)
        call run_and_size("s = Show[Plot[Sin[x], {x, 0, 6}], "// &
                          "Plot[Cos[x], {x, 0, 6}]]", two, n_two)
        if (n_one <= 0 .or. n_two <= 0) then
            call fail("Show wrote no file")
            return
        end if
        if (n_one == n_two) then
            call fail("Show drew the same picture for one and for two curves")
        end if
        if (len(one) == 0 .or. len(two) == 0) call fail("Show returned no file")
    end subroutine test_show_keeps_every_curve

    !> Run a one-line script and report the file the binding named. The file is
    !> deleted again: these tests must not leave pictures behind.
    subroutine run_and_size(source, file, bytes)
        character(*),              intent(in)  :: source
        character(:), allocatable, intent(out) :: file
        integer,                   intent(out) :: bytes
        type(arena_t), target :: a
        type(wl_session_t) :: s
        type(wl_binding_t) :: b
        integer :: k
        logical :: there

        file = ""
        bytes = 0
        call a%init()
        call wl_session_begin(s, a)
        call wl_run_source(s, source)
        do k = 1, wl_binding_count(s)
            b = wl_binding_at(s, k)
            if (chars(b%name) /= "s") cycle
            if (.not. b%ok) then
                call fail("Show refused: "//chars(b%message))
                return
            end if
            file = chars(b%value%name())
        end do
        if (len(file) == 0) return

        inquire (file=file, exist=there, size=bytes)
        if (.not. there) bytes = 0
        ! The pictures the script drew on the way, plus the overlay itself.
        do k = 1, s%plot_count
            call remove_plot(k)
        end do
    end subroutine run_and_size

    subroutine remove_plot(k)
        integer, intent(in) :: k
        character(32) :: tag
        character(:), allocatable :: name
        integer :: unit
        logical :: there

        write (tag, "(i0)") k
        name = "fortsym-plot-"//trim(tag)//".png"
        inquire (file=name, exist=there)
        if (.not. there) return
        open (newunit=unit, file=name, status="old")
        close (unit, status="delete")
    end subroutine remove_plot

    !> Graphics primitives are refused by name rather than half drawn.
    subroutine test_graphics_primitives_refuse()
        call expect_refusal("g = Graphics[{Line[{{0, 0}, {1, 1}}]}]", "g")
    end subroutine test_graphics_primitives_refuse

    !> Show over something that is not a plot of this script refuses instead of
    !> passing the stranger through as though it were a figure.
    subroutine test_show_of_a_stranger_refuses()
        call expect_refusal("g = Show[q, Plot[Sin[x], {x, 0, 6}]]", "g")
    end subroutine test_show_of_a_stranger_refuses

    subroutine expect_refusal(source, name)
        character(*), intent(in) :: source, name
        type(arena_t), target :: a
        type(wl_session_t) :: s
        type(wl_binding_t) :: b
        integer :: k
        logical :: seen

        call a%init()
        call wl_session_begin(s, a)
        call wl_run_source(s, source)
        seen = .false.
        do k = 1, wl_binding_count(s)
            b = wl_binding_at(s, k)
            if (chars(b%name) /= name) cycle
            seen = .true.
            if (b%ok) call fail("no refusal for: "//source)
        end do
        if (.not. seen) call fail("no binding at all for: "//source)
    end subroutine expect_refusal

end program test_fortsym_plotfamily
