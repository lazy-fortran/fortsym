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
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, str, chars
    use fortsym_expr, only: expr_t
    use fortsym_arena, only: NK_FUNC
    use fortsym_eval, only: binding_t, eval_expr
    ! At module level rather than inside render(): fpm's dependency scanner only
    ! reads top-level use statements, so a nested one builds nothing and then
    ! fails to find the module.
    use fortplot, only: figure, plot, xlabel, savefig
    implicit none
    private

    public :: plot_expression, plot_spec_t, read_plot_range

    integer, parameter :: dp = real64

    !> Enough samples that a smooth curve looks smooth at normal figure sizes,
    !> and few enough that a script plotting twenty curves stays interactive.
    integer, parameter :: DEFAULT_SAMPLES = 400

    !> Below this many surviving points there is no curve to draw. Joining a
    !> handful of survivors invents shape that is not in the function.
    integer, parameter :: MIN_POINTS = 4

    type :: plot_spec_t
        type(str_t) :: variable
        real(dp)    :: lower = 0.0_dp
        real(dp)    :: upper = 1.0_dp
        integer     :: samples = DEFAULT_SAMPLES
        type(str_t) :: file
    end type plot_spec_t

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

        out%lower = constant_value(spec%arg(2), got)
        if (.not. got) return
        out%upper = constant_value(spec%arg(3), got)
        if (.not. got) return
        ! An inverted or degenerate range is a mistake in the script, not a
        ! plot of nothing.
        if (.not. (out%upper > out%lower)) return

        out%samples = DEFAULT_SAMPLES
        ok = .true.
    end function read_plot_range

    !> Evaluate an expression that must not depend on any variable.
    function constant_value(e, ok) result(v)
        type(expr_t), intent(in)  :: e
        logical,      intent(out) :: ok
        real(dp)                  :: v
        type(binding_t) :: empty

        v = eval_expr(e, empty, ok)
    end function constant_value

    !> Sample and render. False with a reason when it cannot.
    function plot_expression(e, spec, why) result(ok)
        type(expr_t),      intent(in)    :: e
        type(plot_spec_t), intent(inout) :: spec
        type(str_t),       intent(out)   :: why
        logical                          :: ok

        real(dp), allocatable :: xs(:), ys(:)
        integer :: kept

        why = str("")
        call sample(e, spec, xs, ys, kept)

        if (kept < MIN_POINTS) then
            ok = .false.
            if (kept == 0) then
                why = str("expression is undefined everywhere on the range")
            else
                why = str("expression is defined at too few points to plot")
            end if
            return
        end if

        call render(xs(1:kept), ys(1:kept), spec)
        ok = .true.
    end function plot_expression

    !> Evaluate over the range, keeping only defined, finite points.
    subroutine sample(e, spec, xs, ys, kept)
        type(expr_t),          intent(in)  :: e
        type(plot_spec_t),     intent(in)  :: spec
        real(dp), allocatable, intent(out) :: xs(:), ys(:)
        integer,               intent(out) :: kept

        type(binding_t) :: point
        real(dp) :: x, y, step
        logical :: defined
        integer :: k

        allocate (xs(spec%samples), ys(spec%samples))
        kept = 0
        step = (spec%upper - spec%lower)/real(spec%samples - 1, dp)

        allocate (point%names(1), point%values(1))
        point%names(1) = spec%variable
        point%n = 1

        do k = 1, spec%samples
            x = spec%lower + real(k - 1, dp)*step
            point%values(1) = x
            y = eval_expr(e, point, defined)
            if (.not. defined) cycle
            ! NaN and infinity go the same way as an undefined point: a pole
            ! drawn at a finite height is a line that looks like data.
            if (y /= y) cycle
            if (y > huge(y) .or. y < -huge(y)) cycle
            kept = kept + 1
            xs(kept) = x
            ys(kept) = y
        end do
    end subroutine sample

    subroutine render(xs, ys, spec)
        real(dp),          intent(in) :: xs(:), ys(:)
        type(plot_spec_t), intent(in) :: spec

        call figure()
        call plot(xs, ys)
        call xlabel(chars(spec%variable))
        call savefig(chars(spec%file))
    end subroutine render

end module fortsym_plot
