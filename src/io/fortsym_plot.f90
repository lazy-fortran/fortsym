module fortsym_plot
    ! Render a symbolic expression by sampling it and handing arrays to
    ! fortplot.
    !
    ! fortsym implements no plotting. It samples, maps options, and calls
    ! fortplot; anything fortplot cannot do is fixed upstream in fortplot rather
    ! than worked around here. See issue #44.
    !
    ! Sampling is where a plot becomes a lie, so two rules apply. A point where
    ! the expression is undefined is dropped rather than replaced by zero,
    ! because a zero in a pole's place draws a line through infinity that looks
    ! like data. And a range yielding almost no defined points is refused rather
    ! than rendered, because a nearly empty axes reads as "the function is zero
    ! here" to anyone who did not write the script.
    !
    ! Curves carry scattered points, so dropping one is enough. A grid -- the
    ! contour, density, surface and streamline family -- cannot drop a point:
    ! fortplot's marching squares and mesh renderers need a full rectangular
    ! array, and NaN compares false against every level, which silently moves
    ! isolines rather than leaving a hole. So a grid with any undefined,
    ! infinite or NaN sample is refused outright, naming how many samples went
    ! bad. Masked grids are a fortplot feature that does not exist yet.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_expr, only: expr_t
    use fortsym_arena, only: NK_FUNC
    use fortsym_eval, only: binding_t, eval_expr
    ! At module level rather than inside render(): fpm's dependency scanner only
    ! reads top-level use statements, so a nested one builds nothing and then
    ! fails to find the module.
    use fortplot, only: figure, plot, xlabel, ylabel, savefig, contour_filled, &
        pcolormesh, add_surface, streamplot, quiver, set_xscale, set_yscale, &
        subplot
    implicit none
    private

    public :: plot_expression, plot_spec_t, read_plot_range, plot_constant
    public :: curve_t, figure_data_t, CURVE_LINE, CURVE_POINTS
    public :: sample_curve, sample_parametric_curve
    public :: render_figure, render_panels
    public :: render_surface, render_contour, render_density
    public :: render_stream, render_vector
    public :: set_grid_samples, set_arrow_samples

    integer, parameter :: dp = real64

    !> Enough samples that a smooth curve looks smooth at normal figure sizes,
    !> and few enough that a script plotting twenty curves stays interactive.
    integer, parameter :: DEFAULT_SAMPLES = 400

    !> Below this many surviving points there is no curve to draw. Joining a
    !> handful of survivors invents shape that is not in the function.
    integer, parameter :: MIN_POINTS = 4

    !> Grid resolution for the two-dimensional families. A field costs the
    !> square of this many evaluations, so it stays well below the curve count.
    integer, parameter :: DEFAULT_GRID = 60

    !> Arrows are read one by one, so a vector field is sampled coarsely.
    integer, parameter :: DEFAULT_ARROWS = 15

    integer, parameter :: CURVE_LINE = 1
    integer, parameter :: CURVE_POINTS = 2

    type :: plot_spec_t
        type(str_t) :: variable
        real(dp)    :: lower = 0.0_dp
        real(dp)    :: upper = 1.0_dp
        integer     :: samples = DEFAULT_SAMPLES
        type(str_t) :: file
        !> Log axes: a non-positive sample has no place on a logarithmic axis
        !> and is dropped like an undefined one.
        logical     :: positive_x = .false.
        logical     :: positive_y = .false.
    end type plot_spec_t

    !> One drawn curve, kept so that Show can redraw it beside another.
    type :: curve_t
        real(dp), allocatable :: x(:), y(:)
        integer               :: style = CURVE_LINE
    end type curve_t

    !> Everything needed to redraw a two-dimensional plot from scratch.
    type :: figure_data_t
        type(curve_t), allocatable :: curves(:)
        type(str_t)                :: xname, yname
        logical                    :: xlog = .false.
        logical                    :: ylog = .false.
    end type figure_data_t

contains

    !> Read a {x, a, b} plot range.
    function read_plot_range(spec, out) result(ok)
        type(expr_t),      intent(in)  :: spec
        type(plot_spec_t), intent(out) :: out
        logical                        :: ok
        type(expr_t) :: variable
        logical :: got

        ok = .false.
        if (spec%kind() /= NK_FUNC) return
        if (chars(spec%name()) /= "List") return
        if (spec%nargs() /= 3) return

        variable = spec%arg(1)
        out%variable = variable%name()
        if (len(chars(out%variable)) == 0) return

        out%lower = plot_constant(spec%arg(2), got)
        if (.not. got) return
        out%upper = plot_constant(spec%arg(3), got)
        if (.not. got) return
        ! An inverted or degenerate range is a mistake in the script, not a
        ! plot of nothing.
        if (.not. (out%upper > out%lower)) return

        out%samples = DEFAULT_SAMPLES
        ok = .true.
    end function read_plot_range

    !> Evaluate an expression that must not depend on any variable.
    function plot_constant(e, ok) result(v)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: ok
        real(dp)                  :: v
        type(binding_t) :: empty

        v = eval_expr(e, empty, ok)
    end function plot_constant

    !> Sample and render a single curve. False with a reason when it cannot.
    function plot_expression(e, spec, why) result(ok)
        type(expr_t),      intent(in)    :: e
        type(plot_spec_t), intent(inout) :: spec
        type(str_t),       intent(out)   :: why
        logical                          :: ok
        type(figure_data_t) :: fd
        type(curve_t) :: c

        ok = sample_curve(e, spec, c, why)
        if (.not. ok) return

        allocate (fd%curves(1))
        fd%curves(1) = c
        fd%xname = spec%variable
        fd%xlog = spec%positive_x
        fd%ylog = spec%positive_y
        call render_figure(fd, chars(spec%file))
    end function plot_expression

    !> Sample one expression over a range into a curve.
    function sample_curve(e, spec, c, why) result(ok)
        type(expr_t),      intent(in)  :: e
        type(plot_spec_t), intent(in)  :: spec
        type(curve_t),     intent(out) :: c
        type(str_t),       intent(out) :: why
        logical                        :: ok

        type(binding_t) :: point
        real(dp), allocatable :: xs(:), ys(:)
        real(dp) :: x, y, step
        logical :: defined
        integer :: k, kept

        why = str("")
        allocate (xs(spec%samples), ys(spec%samples))
        kept = 0
        step = (spec%upper - spec%lower)/real(spec%samples - 1, dp)

        allocate (point%names(1), point%values(1))
        point%names(1) = spec%variable
        point%n = 1

        do k = 1, spec%samples
            x = spec%lower + real(k - 1, dp)*step
            if (spec%positive_x) then
                if (.not. (x > 0.0_dp)) cycle
            end if
            point%values(1) = x
            y = eval_expr(e, point, defined)
            if (.not. defined) cycle
            if (.not. usable(y)) cycle
            if (spec%positive_y) then
                if (.not. (y > 0.0_dp)) cycle
            end if
            kept = kept + 1
            xs(kept) = x
            ys(kept) = y
        end do

        ok = enough(kept, why)
        if (.not. ok) return
        c%x = xs(1:kept)
        c%y = ys(1:kept)
        c%style = CURVE_LINE
    end function sample_curve

    !> Sample {fx(t), fy(t)} over a parameter range. A point survives only when
    !> both components do: half a point is not a point.
    function sample_parametric_curve(fx, fy, spec, c, why) result(ok)
        type(expr_t),      intent(in)  :: fx, fy
        type(plot_spec_t), intent(in)  :: spec
        type(curve_t),     intent(out) :: c
        type(str_t),       intent(out) :: why
        logical                        :: ok

        type(binding_t) :: point
        real(dp), allocatable :: xs(:), ys(:)
        real(dp) :: t, u, v, step
        logical :: du, dv
        integer :: k, kept

        why = str("")
        allocate (xs(spec%samples), ys(spec%samples))
        kept = 0
        step = (spec%upper - spec%lower)/real(spec%samples - 1, dp)

        allocate (point%names(1), point%values(1))
        point%names(1) = spec%variable
        point%n = 1

        do k = 1, spec%samples
            t = spec%lower + real(k - 1, dp)*step
            point%values(1) = t
            u = eval_expr(fx, point, du)
            v = eval_expr(fy, point, dv)
            if (.not. du) cycle
            if (.not. dv) cycle
            if (.not. usable(u)) cycle
            if (.not. usable(v)) cycle
            kept = kept + 1
            xs(kept) = u
            ys(kept) = v
        end do

        ok = enough(kept, why)
        if (.not. ok) return
        c%x = xs(1:kept)
        c%y = ys(1:kept)
        c%style = CURVE_LINE
    end function sample_parametric_curve

    !> A sample is usable when it is a finite real number.
    pure function usable(y) result(good)
        real(dp), intent(in) :: y
        logical              :: good
        good = .false.
        ! NaN and infinity go the same way as an undefined point: a pole drawn
        ! at a finite height is a line that looks like data.
        if (y /= y) return
        if (y > huge(y)) return
        if (y < -huge(y)) return
        good = .true.
    end function usable

    !> Decide whether enough points survived to draw anything.
    function enough(kept, why) result(ok)
        integer,     intent(in)  :: kept
        type(str_t), intent(out) :: why
        logical                  :: ok

        ok = kept >= MIN_POINTS
        if (ok) then
            why = str("")
        else if (kept == 0) then
            why = str("expression is undefined everywhere on the range")
        else
            why = str("expression is defined at too few points to plot")
        end if
    end function enough

    !> Draw a set of curves into one file.
    subroutine render_figure(fd, file)
        type(figure_data_t), intent(in) :: fd
        character(*),        intent(in) :: file

        call figure()
        call draw_curves(fd)
        if (len(chars(fd%xname)) > 0) call xlabel(chars(fd%xname))
        if (len(chars(fd%yname)) > 0) call ylabel(chars(fd%yname))
        call savefig(file)
    end subroutine render_figure

    !> Draw several plots as panels of one figure, row-major.
    subroutine render_panels(panels, rows, cols, file)
        type(figure_data_t), intent(in) :: panels(:)
        integer,             intent(in) :: rows, cols
        character(*),        intent(in) :: file
        integer :: k

        call figure()
        do k = 1, size(panels)
            call subplot(rows, cols, k)
            call draw_curves(panels(k))
            if (len(chars(panels(k)%xname)) > 0) then
                call xlabel(chars(panels(k)%xname))
            end if
        end do
        call savefig(file)
    end subroutine render_panels

    subroutine draw_curves(fd)
        type(figure_data_t), intent(in) :: fd
        integer :: k

        if (fd%xlog) call set_xscale("log")
        if (fd%ylog) call set_yscale("log")
        if (.not. allocated(fd%curves)) return
        do k = 1, size(fd%curves)
            if (fd%curves(k)%style == CURVE_POINTS) then
                ! Markers without a joining line: list data is a set of
                ! measurements, and joining them asserts values in between.
                call plot(fd%curves(k)%x, fd%curves(k)%y, linestyle="None", &
                          marker="o")
            else
                call plot(fd%curves(k)%x, fd%curves(k)%y)
            end if
        end do
    end subroutine draw_curves

    !> Sample f(x, y) on a full rectangular grid. z is indexed (y, x), which is
    !> fortplot's convention for contour, mesh and surface data.
    subroutine sample_grid(e, sx, sy, xs, ys, z, bad, total)
        type(expr_t),          intent(in)  :: e
        type(plot_spec_t),     intent(in)  :: sx, sy
        real(dp), allocatable, intent(out) :: xs(:), ys(:), z(:, :)
        integer,               intent(out) :: bad, total

        type(binding_t) :: point
        real(dp) :: v
        logical :: defined
        integer :: i, j, nx, ny

        nx = sx%samples
        ny = sy%samples
        allocate (xs(nx), ys(ny), z(ny, nx))
        bad = 0
        total = nx*ny

        do i = 1, nx
            xs(i) = sx%lower + (sx%upper - sx%lower)*real(i - 1, dp)/ &
                    real(nx - 1, dp)
        end do
        do j = 1, ny
            ys(j) = sy%lower + (sy%upper - sy%lower)*real(j - 1, dp)/ &
                    real(ny - 1, dp)
        end do

        allocate (point%names(2), point%values(2))
        point%names(1) = sx%variable
        point%names(2) = sy%variable
        point%n = 2

        do i = 1, nx
            do j = 1, ny
                point%values(1) = xs(i)
                point%values(2) = ys(j)
                v = eval_expr(e, point, defined)
                if (.not. defined) then
                    bad = bad + 1
                    v = 0.0_dp
                else if (.not. usable(v)) then
                    bad = bad + 1
                    v = 0.0_dp
                end if
                z(j, i) = v
            end do
        end do
    end subroutine sample_grid

    !> Report a grid that could not be filled. The zeros sample_grid left
    !> behind never reach fortplot: every caller refuses first.
    function grid_message(bad, total) result(why)
        integer, intent(in) :: bad, total
        type(str_t)         :: why
        character(32) :: a, b

        write (a, "(i0)") bad
        write (b, "(i0)") total
        why = str("expression is undefined or infinite at "//trim(a)//" of "// &
                  trim(b)//" grid points, and a grid cannot drop a point")
    end function grid_message

    !> Plot3D: a surface over a rectangle.
    function render_surface(e, sx, sy, file, why) result(ok)
        type(expr_t),      intent(in)  :: e
        type(plot_spec_t), intent(in)  :: sx, sy
        character(*),      intent(in)  :: file
        type(str_t),       intent(out) :: why
        logical                        :: ok
        real(dp), allocatable :: xs(:), ys(:), z(:, :)
        integer :: bad, total

        call sample_grid(e, sx, sy, xs, ys, z, bad, total)
        ok = bad == 0
        if (.not. ok) then
            why = grid_message(bad, total)
            return
        end if
        why = str("")
        call figure()
        call add_surface(xs, ys, z)
        call xlabel(chars(sx%variable))
        call ylabel(chars(sy%variable))
        call savefig(file)
    end function render_surface

    !> ContourPlot, drawn as shaded bands, which is also what Wolfram's
    !> ContourPlot shows by default.
    !>
    !> The banded renderer is used rather than fortplot's line contours because
    !> the two disagree about the index order of the field: the band tracer
    !> reads z(y, x) and the line tracer reads z(x, y), so one of them draws a
    !> transposed picture. The banded path was checked against a field that
    !> varies only in x and came out right; the line path came out rotated.
    !> That is a fortplot bug to fix upstream, not here.
    function render_contour(e, sx, sy, file, why) result(ok)
        type(expr_t),      intent(in)  :: e
        type(plot_spec_t), intent(in)  :: sx, sy
        character(*),      intent(in)  :: file
        type(str_t),       intent(out) :: why
        logical                        :: ok
        real(dp), allocatable :: xs(:), ys(:), z(:, :)
        integer :: bad, total

        call sample_grid(e, sx, sy, xs, ys, z, bad, total)
        ok = bad == 0
        if (.not. ok) then
            why = grid_message(bad, total)
            return
        end if
        why = str("")
        call figure()
        call contour_filled(xs, ys, z)
        call xlabel(chars(sx%variable))
        call ylabel(chars(sy%variable))
        call savefig(file)
    end function render_contour

    !> DensityPlot: the field itself as colour.
    function render_density(e, sx, sy, file, why) result(ok)
        type(expr_t),      intent(in)  :: e
        type(plot_spec_t), intent(in)  :: sx, sy
        character(*),      intent(in)  :: file
        type(str_t),       intent(out) :: why
        logical                        :: ok
        real(dp), allocatable :: xs(:), ys(:), z(:, :)
        integer :: bad, total

        call sample_grid(e, sx, sy, xs, ys, z, bad, total)
        ok = bad == 0
        if (.not. ok) then
            why = grid_message(bad, total)
            return
        end if
        why = str("")
        call figure()
        call pcolormesh(xs, ys, z, shading="nearest")
        call xlabel(chars(sx%variable))
        call ylabel(chars(sy%variable))
        call savefig(file)
    end function render_density

    !> Sample a vector field on a grid. u and v are indexed (x, y), which is
    !> fortplot's convention for streamplot components.
    subroutine sample_field(eu, ev, sx, sy, xs, ys, u, v, bad, total)
        type(expr_t),          intent(in)  :: eu, ev
        type(plot_spec_t),     intent(in)  :: sx, sy
        real(dp), allocatable, intent(out) :: xs(:), ys(:), u(:, :), v(:, :)
        integer,               intent(out) :: bad, total

        type(binding_t) :: point
        real(dp) :: a, b
        logical :: da, db
        integer :: i, j, nx, ny

        nx = sx%samples
        ny = sy%samples
        allocate (xs(nx), ys(ny), u(nx, ny), v(nx, ny))
        bad = 0
        total = nx*ny

        do i = 1, nx
            xs(i) = sx%lower + (sx%upper - sx%lower)*real(i - 1, dp)/ &
                    real(nx - 1, dp)
        end do
        do j = 1, ny
            ys(j) = sy%lower + (sy%upper - sy%lower)*real(j - 1, dp)/ &
                    real(ny - 1, dp)
        end do

        allocate (point%names(2), point%values(2))
        point%names(1) = sx%variable
        point%names(2) = sy%variable
        point%n = 2

        do j = 1, ny
            do i = 1, nx
                point%values(1) = xs(i)
                point%values(2) = ys(j)
                a = eval_expr(eu, point, da)
                b = eval_expr(ev, point, db)
                if (da) then
                    if (.not. usable(a)) da = .false.
                end if
                if (db) then
                    if (.not. usable(b)) db = .false.
                end if
                if (da .and. db) then
                    u(i, j) = a
                    v(i, j) = b
                else
                    bad = bad + 1
                    u(i, j) = 0.0_dp
                    v(i, j) = 0.0_dp
                end if
            end do
        end do
    end subroutine sample_field

    !> StreamPlot. Streamlines are integrated through the whole grid, so a
    !> single missing sample would bend every line that passes near it.
    function render_stream(eu, ev, sx, sy, file, why) result(ok)
        type(expr_t),      intent(in)  :: eu, ev
        type(plot_spec_t), intent(in)  :: sx, sy
        character(*),      intent(in)  :: file
        type(str_t),       intent(out) :: why
        logical                        :: ok
        real(dp), allocatable :: xs(:), ys(:), u(:, :), v(:, :)
        integer :: bad, total

        call sample_field(eu, ev, sx, sy, xs, ys, u, v, bad, total)
        ok = bad == 0
        if (.not. ok) then
            why = grid_message(bad, total)
            return
        end if
        why = str("")
        call figure()
        call streamplot(xs, ys, u, v)
        call xlabel(chars(sx%variable))
        call ylabel(chars(sy%variable))
        call savefig(file)
    end function render_stream

    !> VectorPlot. Arrows are independent, so an arrow whose components are
    !> undefined is dropped rather than drawn at zero length -- a zero-length
    !> arrow would read as a stagnation point.
    function render_vector(eu, ev, sx, sy, file, why) result(ok)
        type(expr_t),      intent(in)  :: eu, ev
        type(plot_spec_t), intent(in)  :: sx, sy
        character(*),      intent(in)  :: file
        type(str_t),       intent(out) :: why
        logical                        :: ok

        type(binding_t) :: point
        real(dp), allocatable :: px(:), py(:), pu(:), pv(:)
        real(dp) :: x, y, a, b
        logical :: da, db
        integer :: i, j, nx, ny, kept

        nx = sx%samples
        ny = sy%samples
        allocate (px(nx*ny), py(nx*ny), pu(nx*ny), pv(nx*ny))
        kept = 0

        allocate (point%names(2), point%values(2))
        point%names(1) = sx%variable
        point%names(2) = sy%variable
        point%n = 2

        do j = 1, ny
            do i = 1, nx
                x = sx%lower + (sx%upper - sx%lower)*real(i - 1, dp)/ &
                    real(nx - 1, dp)
                y = sy%lower + (sy%upper - sy%lower)*real(j - 1, dp)/ &
                    real(ny - 1, dp)
                point%values(1) = x
                point%values(2) = y
                a = eval_expr(eu, point, da)
                b = eval_expr(ev, point, db)
                if (.not. da) cycle
                if (.not. db) cycle
                if (.not. usable(a)) cycle
                if (.not. usable(b)) cycle
                kept = kept + 1
                px(kept) = x
                py(kept) = y
                pu(kept) = a
                pv(kept) = b
            end do
        end do

        ok = enough(kept, why)
        if (.not. ok) return
        call figure()
        call quiver(px(1:kept), py(1:kept), pu(1:kept), pv(1:kept), &
                    color="blue")
        call xlabel(chars(sx%variable))
        call ylabel(chars(sy%variable))
        call savefig(file)
    end function render_vector

    !> Default grid resolutions, so callers do not repeat the numbers.
    subroutine set_grid_samples(sx, sy)
        type(plot_spec_t), intent(inout) :: sx, sy
        sx%samples = DEFAULT_GRID
        sy%samples = DEFAULT_GRID
    end subroutine set_grid_samples

    subroutine set_arrow_samples(sx, sy)
        type(plot_spec_t), intent(inout) :: sx, sy
        sx%samples = DEFAULT_ARROWS
        sy%samples = DEFAULT_ARROWS
    end subroutine set_arrow_samples

end module fortsym_plot
