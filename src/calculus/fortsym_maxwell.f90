module fortsym_maxwell
    ! Maxwell-form conveniences layered over the native spacetime form owner.
    ! The module owns only the physical combinations; component algebra stays
    ! in fortsym_spacetime_form.
    use fortsym_expr, only: expr_t, same_arena
    use fortsym_relativity, only: spacetime_metric_t, &
        spacetime_metric_arena, spacetime_metric_coordinates
    use fortsym_spacetime_form, only: spacetime_form_t, spacetime_form_valid, &
        spacetime_form_same_arena, &
        spacetime_form_degree, spacetime_form_scalar, spacetime_d, &
        spacetime_star, spacetime_add, spacetime_subtract
    implicit none
    private

    public :: maxwell_field_strength, maxwell_gauge_transform
    public :: maxwell_residual

contains

    !> Electromagnetic field strength F=dA for a potential one-form A.
    function maxwell_field_strength(g, potential) result(field)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: potential
        type(spacetime_form_t) :: field

        if (.not. spacetime_form_valid(potential)) return
        if (spacetime_form_degree(potential) /= 1) return
        if (.not. spacetime_form_same_arena(potential, spacetime_metric_arena(g))) return
        field = spacetime_d(g, potential)
    end function maxwell_field_strength

    !> Gauge transformation A -> A + d(chi), with scalar chi in the metric arena.
    function maxwell_gauge_transform(g, potential, chi) result(transformed)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: potential
        type(expr_t), intent(in) :: chi
        type(spacetime_form_t) :: transformed, exact
        type(expr_t) :: coordinates(4)

        if (.not. spacetime_form_valid(potential)) return
        if (spacetime_form_degree(potential) /= 1) return
        if (.not. spacetime_form_same_arena(potential, spacetime_metric_arena(g))) return
        coordinates = spacetime_metric_coordinates(g)
        if (.not. same_arena(chi, coordinates(1))) return
        exact = spacetime_d(g, spacetime_form_scalar(chi))
        transformed = spacetime_add(potential, exact)
    end function maxwell_gauge_transform

    !> Maxwell source residual d(*F)-J for F=dA and a three-form current J.
    function maxwell_residual(g, potential, current) result(residual)
        type(spacetime_metric_t), intent(in) :: g
        type(spacetime_form_t), intent(in) :: potential, current
        type(spacetime_form_t) :: residual, field, dual, lhs

        if (.not. spacetime_form_valid(current)) return
        if (spacetime_form_degree(current) /= 3) return
        if (.not. spacetime_form_same_arena(current, spacetime_metric_arena(g))) return
        field = maxwell_field_strength(g, potential)
        if (.not. spacetime_form_valid(field)) return
        dual = spacetime_star(g, field)
        lhs = spacetime_d(g, dual)
        residual = spacetime_subtract(lhs, current)
    end function maxwell_residual

end module fortsym_maxwell
