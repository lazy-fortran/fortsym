program test_fortsym_magnetic
    ! Native reciprocal-basis and magnetic-component identities.
    !
    ! The shear chart is deliberately nonorthogonal. A Cartesian or diagonal
    ! chart would allow a transposed basis or a missed metric raise to pass.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, i_expr, sin, cos, operator(+), &
        operator(-), operator(*), operator(**)
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_symengine, only: symengine_engine_t, make_symengine_engine
    use fortsym_chart, only: DIM, chart_t, chart_create, covariant_basis, &
        reciprocal_basis, metric_covariant, jacobian, sqrtg, &
        field_line_derivative
    use fortsym_magnetic, only: b_con, b_cov, b_density, h_cov, h_con, b_fourier, &
        b_fourier_density, j_fourier, magnetic_field_t, magnetic_field, &
        b_flux_form, b_con_form, b_density_form, &
        magnetic_upper, magnetic_lower, magnetic_density, flux_surface_t, &
        flux_surface, flux_surface_valid, flux_surface_label, &
        flux_surface_measure, flux_surface_average, magnetic_chart_t, &
        magnetic_chart, magnetic_chart_valid, magnetic_chart_surface, &
        magnetic_chart_field, magnetic_chart_upper, magnetic_chart_lower, &
        magnetic_chart_density, magnetic_chart_average, &
        magnetic_chart_potential_form, magnetic_chart_flux_form
    use fortsym_tensor, only: tensor_t, tensor_component, tensor_variance, &
        tensor_density_weight, tensor_valid
    use fortsym_form, only: form_t, form_component, form_degree, form_valid, &
        volume_form, interior, d
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
    type(expr_t) :: h_down(DIM), h_up(DIM)
    type(expr_t) :: form_up(DIM)
    type(expr_t) :: residual, det_metric, volume, signed_jacobian, average
    type(magnetic_field_t) :: typed_field
    type(flux_surface_t) :: flux_surface_owner
    type(magnetic_chart_t) :: magnetic_chart_owner
    type(flux_surface_t) :: magnetic_surface_owner
    type(magnetic_field_t) :: magnetic_field_owner
    type(tensor_t) :: magnetic_up_owner, magnetic_down_owner, magnetic_density_owner
    type(form_t) :: potential_form_owner, flux_form_owner, field_flux_form, flux_from_b, &
        closed_form
    type(form_t) :: volume_form_owner
    type(form_t) :: negative_volume, negative_flux_form
    type(tensor_t) :: typed_up, typed_down, typed_density
    type(tensor_t) :: form_density_owner
    type(tensor_t) :: negative_density_owner
    integer :: tensor_index(1)
    type(engine_result_t) :: reduced
    integer :: i, j
    character(len=64) :: label
    logical :: average_ok
    character(:), allocatable :: average_reason

    call arena%init()
    engine = make_symengine_engine(arena)
    shear = make_shear_chart()
    call suite_begin(suite, "native magnetic geometry")

    flux_surface_owner = flux_surface(shear, 1)
    if (.not. flux_surface_valid(flux_surface_owner)) then
        error stop "flux surface metadata invalid"
    end if
    call check_identity(suite, engine, "flux surface label", &
        flux_surface_label(flux_surface_owner) - u(1))
    call check_identity(suite, engine, "flux surface measure", &
        flux_surface_measure(flux_surface_owner)**2 - 2)
    call flux_surface_average(flux_surface_owner, &
        1 + sin(u(2)) + cos(u(3)), average, average_ok, average_reason)
    if (.not. average_ok) then
        write (*, '(a)') "FAIL flux surface average: "//average_reason
        error stop 1
    end if
    call check_identity(suite, engine, "periodic flux surface average", &
        average - 1)

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

    magnetic_chart_owner = magnetic_chart(shear, potential, 1)
    if (.not. magnetic_chart_valid(magnetic_chart_owner)) then
        error stop "magnetic chart owner is invalid"
    end if
    magnetic_surface_owner = magnetic_chart_surface(magnetic_chart_owner)
    magnetic_field_owner = magnetic_chart_field(magnetic_chart_owner)
    magnetic_up_owner = magnetic_chart_upper(magnetic_chart_owner)
    magnetic_down_owner = magnetic_chart_lower(magnetic_chart_owner)
    magnetic_density_owner = magnetic_chart_density(magnetic_chart_owner)
    if (.not. flux_surface_valid(magnetic_surface_owner)) then
        error stop "magnetic chart surface is invalid"
    end if
    if (.not. tensor_valid(magnetic_up_owner)) error stop "magnetic B^i view invalid"
    if (.not. tensor_valid(magnetic_down_owner)) error stop "magnetic B_i view invalid"
    if (.not. tensor_valid(magnetic_density_owner)) then
        error stop "magnetic density view invalid"
    end if
    if (.not. tensor_valid(magnetic_upper(magnetic_field_owner))) then
        error stop "magnetic field bundle invalid"
    end if
    call check_identity(suite, engine, "magnetic chart label", &
        flux_surface_label(magnetic_surface_owner) - u(1))
    call check_identity(suite, engine, "magnetic chart B^1 view", &
        tensor_component(magnetic_up_owner, tensor_index) - b_up(1))
    call magnetic_chart_average(magnetic_chart_owner, &
        1 + sin(u(2)) + cos(u(3)), average, average_ok, average_reason)
    if (.not. average_ok) then
        write (*, '(a)') "FAIL magnetic chart average: "//average_reason
        error stop 1
    end if
    call check_identity(suite, engine, "magnetic chart periodic average", &
        average - 1)
    potential_form_owner = magnetic_chart_potential_form(magnetic_chart_owner)
    flux_form_owner = magnetic_chart_flux_form(magnetic_chart_owner)
    volume_form_owner = volume_form(shear)
    field_flux_form = interior(shear, b_up, volume_form_owner)
    flux_from_b = b_flux_form(shear, b_up)
    closed_form = d(shear, flux_form_owner)
    if (.not. form_valid(potential_form_owner)) then
        error stop "magnetic potential form is invalid"
    end if
    if (.not. form_valid(flux_form_owner)) then
        error stop "magnetic flux form is invalid"
    end if
    if (form_degree(potential_form_owner) /= 1) then
        error stop "magnetic potential form degree failed"
    end if
    if (form_degree(flux_form_owner) /= 2) then
        error stop "magnetic flux form degree failed"
    end if
    call check_identity(suite, engine, "magnetic beta psi-theta", &
        form_component(flux_form_owner, 3) - form_component(field_flux_form, 3))
    call check_identity(suite, engine, "magnetic beta psi-phi", &
        form_component(flux_form_owner, 5) - form_component(field_flux_form, 5))
    call check_identity(suite, engine, "magnetic beta theta-phi", &
        form_component(flux_form_owner, 6) - form_component(field_flux_form, 6))
    call check_identity(suite, engine, "forward magnetic beta psi-theta", &
        form_component(flux_from_b, 3) - form_component(field_flux_form, 3))
    call check_identity(suite, engine, "forward magnetic beta psi-phi", &
        form_component(flux_from_b, 5) - form_component(field_flux_form, 5))
    call check_identity(suite, engine, "forward magnetic beta theta-phi", &
        form_component(flux_from_b, 6) - form_component(field_flux_form, 6))
    form_up = b_con_form(shear, flux_form_owner)
    form_density_owner = b_density_form(shear, flux_form_owner)
    if (.not. tensor_valid(form_density_owner)) then
        error stop "magnetic form density bridge is invalid"
    end if
    if (tensor_variance(form_density_owner, 1) /= 1) then
        error stop "magnetic form density variance failed"
    end if
    if (tensor_density_weight(form_density_owner) /= 1) then
        error stop "magnetic form density weight failed"
    end if
    call check_identity(suite, engine, "magnetic form B^1 bridge", &
        form_up(1) - b_up(1))
    call check_identity(suite, engine, "magnetic form density bridge", &
        tensor_component(form_density_owner, tensor_index) - b_den(1))
    ! Reversing both the volume used to contract B and the Hodge orientation
    ! must leave the recovered vector density unchanged.
    negative_volume = volume_form(shear, -1)
    negative_flux_form = interior(shear, b_up, negative_volume)
    negative_density_owner = b_density_form(shear, negative_flux_form, -1)
    if (.not. tensor_valid(negative_density_owner)) then
        error stop "oriented magnetic form bridge is invalid"
    end if
    call check_identity(suite, engine, "oriented magnetic density bridge", &
        tensor_component(negative_density_owner, tensor_index) - b_den(1))
    call check_identity(suite, engine, "magnetic beta is closed", &
        form_component(closed_form, 7))

    call check_identity(suite, engine, "B^1 from covariant potential", b_up(1) - 1)
    call check_identity(suite, engine, "B^2 from covariant potential", b_up(2) - u(2))
    call check_identity(suite, engine, "B^3 from covariant potential", &
        b_up(3) - (2*u(1) - u(3)))
    call check_identity(suite, engine, "field-line derivative of u1", &
        field_line_derivative(shear, b_up, u(1)) - 1)
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

    ! The constitutive owner uses H_i = nu_ij B^j, then raises with g^ij.
    reluctivity = num(arena, 0)
    reluctivity(1, 1) = num(arena, 2)
    reluctivity(1, 2) = num(arena, 1)
    reluctivity(2, 2) = num(arena, 3)
    reluctivity(3, 3) = num(arena, 4)
    h_down = h_cov(shear, reluctivity, b_up)
    h_up = h_con(shear, h_down)
    call check_identity(suite, engine, "H_1 constitutive map", &
        h_down(1) - (2 + u(2)))
    call check_identity(suite, engine, "H_2 constitutive map", &
        h_down(2) - 3*u(2))
    call check_identity(suite, engine, "H_3 constitutive map", &
        h_down(3) - 4*(2*u(1) - u(3)))
    call check_identity(suite, engine, "H^1 metric raise", &
        h_up(1) - (4 - u(2)))
    call check_identity(suite, engine, "H^2 metric raise", &
        h_up(2) - (-2 + 2*u(2)))
    call check_identity(suite, engine, "H^3 metric raise", &
        h_up(3) - (8*u(1) - 4*u(3)))

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
