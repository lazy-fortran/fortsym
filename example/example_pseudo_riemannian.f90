program example_pseudo_riemannian
    ! A compact pseudo-Riemannian corpus: flat Cartesian Minkowski space and
    ! the two-dimensional expanding (de Sitter) patch.  The Riemann convention
    ! used by the native owner is
    !   R^a_bcd = d_c Gamma^a_db - d_d Gamma^a_cb
    !              + Gamma^a_ce Gamma^e_db - Gamma^a_de Gamma^e_cb.
    use fortsym
    implicit none

    type(arena_t), pointer :: arena
    type(spacetime_metric_t) :: flat_metric, curved_metric
    type(expr_t) :: coordinates(SPACETIME_DIM)
    type(expr_t) :: components(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: curvature(SPACETIME_DIM, SPACETIME_DIM, &
        SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: ricci(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: einstein(SPACETIME_DIM, SPACETIME_DIM)
    type(expr_t) :: curve(SPACETIME_DIM), residual(SPACETIME_DIM)
    type(expr_t) :: h, parameter, scalar, scale
    type(engine_result_t) :: checked
    integer :: signature(SPACETIME_DIM), i, j, k, ell

    call reset()
    arena => default_arena()
    coordinates(1) = "t"
    coordinates(2) = "x"
    coordinates(3) = "y"
    coordinates(4) = "z"
    signature = [-1, 1, 1, 1]

    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = num(arena, 1)
    components(3, 3) = num(arena, 1)
    components(4, 4) = num(arena, 1)
    flat_metric = spacetime_metric_create(components, SPACETIME_DIM, coordinates, &
        signature, 1)
    if (.not. spacetime_metric_valid(flat_metric)) error stop "invalid flat metric"
    curvature = spacetime_riemann(flat_metric)
    ricci = spacetime_ricci(flat_metric)
    einstein = spacetime_einstein(flat_metric)
    do i = 1, SPACETIME_DIM
        do j = 1, SPACETIME_DIM
            do k = 1, SPACETIME_DIM
                do ell = 1, SPACETIME_DIM
                    call assert_zero(curvature(i, j, k, ell), &
                        "flat Riemann tensor")
                end do
            end do
            call assert_zero(ricci(i, j), "flat Ricci tensor")
            call assert_zero(einstein(i, j), "flat Einstein tensor")
        end do
    end do

    parameter = "lambda"
    curve = num(arena, 0)
    curve(1) = parameter
    curve(2) = parameter
    residual = spacetime_geodesic_residual(flat_metric, curve, parameter)
    do i = 1, SPACETIME_DIM
        call assert_zero(residual(i), "flat Cartesian geodesic residual")
    end do

    h = "H"
    scale = exp(h*coordinates(1))
    components = num(arena, 0)
    components(1, 1) = num(arena, -1)
    components(2, 2) = scale**2
    curved_metric = spacetime_metric_create(components, 2, coordinates, signature, 1)
    if (.not. spacetime_metric_valid(curved_metric)) error stop "invalid curved metric"

    scalar = spacetime_scalar_curvature(curved_metric)
    call assert_zero(scalar - 2*h**2, "curved Lorentzian scalar curvature")
    ricci = spacetime_ricci(curved_metric)
    call assert_zero(ricci(1, 1) + h**2, "curved Lorentzian Ricci tt")
    call assert_zero(ricci(2, 2) - scale**2*h**2, &
        "curved Lorentzian Ricci xx")
    einstein = spacetime_einstein(curved_metric)
    do i = 1, 2
        do j = 1, 2
            call assert_zero(einstein(i, j), "two-dimensional Einstein tensor")
        end do
    end do
    curvature = spacetime_riemann(curved_metric)
    call assert_zero(curvature(1, 2, 1, 2) - scale**2*h**2, &
        "standard-sign curved Riemann component")

    curve = num(arena, 0)
    curve(1) = parameter
    residual = spacetime_geodesic_residual(curved_metric, curve, parameter)
    call assert_zero(residual(1), "curved comoving geodesic time residual")
    call assert_zero(residual(2), "curved comoving geodesic space residual")

    print '(a)', "pseudo-Riemannian relativity corpus"
    print '(a)', "  standard R^a_bcd sign convention checked"
    print '(a)', "  flat 4D Minkowski: Riemann = Ricci = Einstein = 0"
    print '(a)', "  curved 2D Lorentzian patch: R = 2 H^2"
    print '(a)', "  flat and comoving geodesic residuals vanish"

contains

    subroutine assert_zero(expression, label)
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: label

        checked = zero_test(expression)
        if (.not. checked%ok .or. checked%verdict /= VERDICT_TRUE) then
            write (*, '(a)') "FAILED: "//label
            error stop 1
        end if
    end subroutine assert_zero

end program example_pseudo_riemannian
