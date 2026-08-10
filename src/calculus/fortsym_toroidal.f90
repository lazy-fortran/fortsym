module fortsym_toroidal
    ! Toroidal coordinates and scalar harmonics for Laplace problems.
    !
    ! Coordinates follow DLMF 14.19.1 with eta > 0. The separated real mode is
    ! sqrt(cosh(eta)-cos(theta)) R(eta) cos(n theta) cos(m phi), where
    ! R is P_(n-1/2)^m(cosh eta) or Q_(n-1/2)^m(cosh eta).
    use, intrinsic :: iso_fortran_env, only: int64
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, rat, sin, cos, sinh, cosh, sqrt, &
        legendrep, legendreq, operator(+), operator(-), operator(*), &
        operator(/)
    use fortsym_chart, only: chart_t, chart_create, DIM
    implicit none
    private

    public :: make_toroidal_chart
    public :: toroidal_scalar_ansatz
    public :: toroidal_harmonic_p, toroidal_harmonic_q

contains

    function make_toroidal_chart( &
            arena, eta, theta, phi, scale) result(chart)
        type(arena_t), target, intent(inout) :: arena
        type(expr_t), intent(in) :: eta, theta, phi, scale
        type(chart_t) :: chart
        type(expr_t) :: coordinates(DIM), position(DIM), denominator

        denominator = cosh(eta) - cos(theta)
        coordinates(1) = eta
        coordinates(2) = theta
        coordinates(3) = phi
        position(1) = scale*sinh(eta)*cos(phi)/denominator
        position(2) = scale*sinh(eta)*sin(phi)/denominator
        position(3) = scale*sin(theta)/denominator
        chart = chart_create(arena, coordinates, position)
    end function make_toroidal_chart

    function toroidal_scalar_ansatz( &
            radial, eta, theta, phi, degree_index, order) result(field)
        type(expr_t), intent(in) :: radial, eta, theta, phi
        type(expr_t), intent(in) :: degree_index, order
        type(expr_t) :: field

        field = sqrt(cosh(eta) - cos(theta))*radial* &
            cos(degree_index*theta)*cos(order*phi)
    end function toroidal_scalar_ansatz

    function toroidal_harmonic_p( &
            eta, theta, phi, degree_index, order) result(field)
        type(expr_t), intent(in) :: eta, theta, phi, degree_index, order
        type(expr_t) :: field
        type(expr_t) :: radial

        radial = legendrep( &
            degree_index - rat(eta%a, 1_int64, 2_int64), &
            order, cosh(eta))
        field = toroidal_scalar_ansatz( &
            radial, eta, theta, phi, degree_index, order)
    end function toroidal_harmonic_p

    function toroidal_harmonic_q( &
            eta, theta, phi, degree_index, order) result(field)
        type(expr_t), intent(in) :: eta, theta, phi, degree_index, order
        type(expr_t) :: field
        type(expr_t) :: radial

        radial = legendreq( &
            degree_index - rat(eta%a, 1_int64, 2_int64), &
            order, cosh(eta))
        field = toroidal_scalar_ansatz( &
            radial, eta, theta, phi, degree_index, order)
    end function toroidal_harmonic_q

end module fortsym_toroidal
