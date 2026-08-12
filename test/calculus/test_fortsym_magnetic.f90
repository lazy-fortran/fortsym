program test_fortsym_magnetic
    ! Native reciprocal-basis and magnetic-component identities.
    !
    ! The shear chart is deliberately nonorthogonal. A Cartesian or diagonal
    ! chart would allow a transposed basis or a missed metric raise to pass.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, i_expr, operator(+), &
        operator(-), operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create, covariant_basis, &
        reciprocal_basis, metric_covariant, jacobian, sqrtg
    use fortsym_magnetic, only: b_con, b_cov, b_density, b_fourier, &
        b_fourier_density, j_fourier, magnetic_field_t, magnetic_field, &
        magnetic_upper, magnetic_lower, magnetic_density
    use fortsym_tensor, only: tensor_t, tensor_component, tensor_variance, &
        tensor_density_weight, tensor_valid
    implicit none

    type(arena_t), target :: arena
    type(symengine_engine_t) :: engine
    type(suite_t) :: suite
    type(chart_t) :: shear
    type(expr_t) :: u(DIM), position(DIM), covariant(DIM, DIM)
    type(expr_t) :: reciprocal(DIM, DIM), metric(DIM, DIM)
    type(expr_t) :: potential(DIM), b_up(DIM), b_down(DIM), b_den(DIM)
    type(expr_t) :: fourier_potential(DIM), fourier_up(DIM), fourier_den(DIM)
    type(expr_t) :: fourier_integer(DIM), mode
    type(expr_t) :: reluctivity(DIM, DIM), current(DIM)
    type(expr_t) :: residual, det_metric, volume, signed_jacobian
    type(magnetic_field_t) :: typed_field
    type(tensor_t) :: typed_up, typed_down, typed_density
    integer :: tensor_index(1)
    type(engine_result_t) :: reduced
    integer :: i, j
    character(len=64) :: label

    call arena%init()
    engine = make_symengine_engine(arena)
    shear = make_shear_chart()
    call suite_begin(suite, "native magnetic geometry")

    covariant = covariant_basis(shear)
    reciprocal = reciprocal_basis(shear)
    do i = 1, DIM
        do j = 1, DIM
            residual = reciprocal(1, i)*covariant(1, j) + &
                reciprocal(2, i)*covariant(2, j) + &
                reciprocal(3, i)*covariant(3, j)
            if (i == j) residual = residual - 1
            write (label, '(a,i0,a,i0)') "reciprocal basis e^", i, &
                " dot e_", j
            call check_identity(suite, engine, trim(label), residual)
        end do
    end do

    metric = metric_covariant(shear)
    signed_jacobian = jacobian(shear)
    volume = sqrtg(shear)
    det_metric = metric(1, 1)*(metric(2, 2)*metric(3, 3) - &
        metric(2, 3)*metric(3, 2)) - metric(1, 2)*(metric(2, 1)*metric(3, 3) - &
        metric(2, 3)*metric(3, 1)) + metric(1, 3)*(metric(2, 1)*metric(3, 2) - &
        metric(2, 2)*metric(3, 1))
    call check_identity(suite, engine, "det metric equals signed Jacobian square", &
        det_metric - signed_jacobian**2)
    call check_identity(suite, engine, "sqrtg square equals metric determinant", &
        volume**2 - det_metric)
    call check_identity(suite, engine, "shear chart has unit sqrtg", volume - 1)

    potential(1) = u(2)*u(3)
    potential(2) = u(1)**2
    potential(3) = u(2) + u(3)**2
    b_up = b_con(shear, potential)
    b_down = b_cov(shear, b_up)
    b_den = b_density(shear, b_up)
    typed_field = magnetic_field(shear, potential)
    typed_up = magnetic_upper(typed_field)
    typed_down = magnetic_lower(typed_field)
    typed_density = magnetic_density(typed_field)

    if (.not. tensor_valid(typed_up)) error stop "typed B^i is invalid"
    if (.not. tensor_valid(typed_down)) error stop "typed B_i is invalid"
    if (.not. tensor_valid(typed_density)) error stop "typed B density is invalid"
    if (tensor_variance(typed_up, 1) /= 1) error stop "B^i variance failed"
    if (tensor_variance(typed_down, 1) /= -1) error stop "B_i variance failed"
    if (tensor_variance(typed_density, 1) /= 1) error stop "B density variance failed"
    if (tensor_density_weight(typed_up) /= 0) error stop "B^i weight failed"
    if (tensor_density_weight(typed_down) /= 0) error stop "B_i weight failed"
    if (tensor_density_weight(typed_density) /= 1) error stop "B density weight failed"
    tensor_index(1) = 1
    call check_identity(suite, engine, "typed B^1 component", &
        tensor_component(typed_up, tensor_index) - b_up(1))
    call check_identity(suite, engine, "typed B_1 component", &
        tensor_component(typed_down, tensor_index) - b_down(1))
    call check_identity(suite, engine, "typed B density component", &
        tensor_component(typed_density, tensor_index) - b_den(1))

    call check_identity(suite, engine, "B^1 from covariant potential", b_up(1) - 1)
    call check_identity(suite, engine, "B^2 from covariant potential", b_up(2) - u(2))
    call check_identity(suite, engine, "B^3 from covariant potential", &
        b_up(3) - (2*u(1) - u(3)))
    call check_identity(suite, engine, "metric lowering gives B_1", &
        b_down(1) - (1 + u(2)))
    call check_identity(suite, engine, "metric lowering gives B_2", &
        b_down(2) - (1 + 2*u(2)))
    call check_identity(suite, engine, "metric lowering gives B_3", &
        b_down(3) - (2*u(1) - u(3)))
    call check_identity(suite, engine, "unit sqrtg preserves B density", &
        b_den(1) - b_up(1))
    call check_identity(suite, engine, "unit sqrtg preserves all B densities", &
        b_den(2) - b_up(2) + b_den(3) - b_up(3))

    ! The paper_magnetic mode uses A_3 = 0 and replaces d/du3 by i*n.
    ! The nonorthogonal shear keeps this test sensitive to the chart path,
    ! while its unit volume factor makes the paper's density equations direct.
    fourier_potential(1) = u(1)*u(2)
    fourier_potential(2) = u(1)**2
    fourier_potential(3) = num(arena, 0)
    mode = sym(arena, "n")
    fourier_up = b_fourier(shear, fourier_potential, mode)
    fourier_den = b_fourier_density(shear, fourier_potential, mode)
    call check_identity(suite, engine, "Fourier B^1", &
        fourier_up(1) + i_expr(arena)*mode*u(1)**2)
    call check_identity(suite, engine, "Fourier B^2", &
        fourier_up(2) - i_expr(arena)*mode*u(1)*u(2))
    call check_identity(suite, engine, "Fourier B^3", fourier_up(3) - u(1))
    call check_identity(suite, engine, "Fourier density B^1", &
        fourier_den(1) + i_expr(arena)*mode*u(1)**2)
    call check_identity(suite, engine, "Fourier density B^2", &
        fourier_den(2) - i_expr(arena)*mode*u(1)*u(2))
    call check_identity(suite, engine, "Fourier density B^3", fourier_den(3) - u(1))
    fourier_integer = b_fourier(shear, fourier_potential, 2)
    call check_identity(suite, engine, "integer Fourier mode overload", &
        fourier_integer(1) + 2*i_expr(arena)*u(1)**2)

    ! Generic Fourier curl-curl: J = curl(nu curl(A)), with d/du3 = i*n.
    ! The off-diagonal transverse entries make the constitutive contraction
    ! observable rather than testing only the paper's diagonal special case.
    do i = 1, DIM
        do j = 1, DIM
            reluctivity(i, j) = num(arena, 0)
        end do
    end do
    reluctivity(1, 1) = num(arena, 2)
    reluctivity(1, 2) = num(arena, 3)
    reluctivity(2, 1) = num(arena, 5)
    reluctivity(2, 2) = num(arena, 7)
    reluctivity(3, 3) = num(arena, 11)
    current = j_fourier(shear, reluctivity, fourier_potential, mode)
    call check_identity(suite, engine, "Fourier current J^1", current(1) - &
        mode**2*(7*u(1)*u(2) - 5*u(1)**2))
    residual = current(2) - (mode**2*(-3*u(1)*u(2) + 2*u(1)**2) - 11)
    reduced = engine%simplify(residual)
    if (reduced%ok) then
        call check_identity(suite, engine, "Fourier current J^2", reduced%value)
    else
        call check_identity(suite, engine, "Fourier current J^2", residual)
    end if
    call check_identity(suite, engine, "Fourier current J^3", current(3) - &
        i_expr(arena)*mode*(-13*u(1) + 7*u(2)))

    if (suite%failed /= 0) then
        print *, "test_fortsym_magnetic: ", suite%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(suite, "/tmp/fortsym_magnetic.json")
    print *, "test_fortsym_magnetic: all checks passed"

contains

    function make_shear_chart() result(c)
        type(chart_t) :: c

        u(1) = sym(arena, "u1")
        u(2) = sym(arena, "u2")
        u(3) = sym(arena, "u3")
        position(1) = u(1) + u(2)
        position(2) = u(2)
        position(3) = u(3)
        c = chart_create(arena, u, position)
    end function make_shear_chart

end program test_fortsym_magnetic
