program test_fortsym_phasespace
    ! Independent oracles for fortsym_phasespace:
    !
    ! - A charged particle in a uniform magnetic field (symmetric-gauge
    !   phase-space Lagrangian) must reduce to hand-derived circular motion
    !   at the textbook cyclotron frequency omega_c = qB/m.
    ! - A canonical pendulum must conserve its own Hamiltonian along the
    !   solved flow, and its small-angle limit must reduce to the textbook
    !   simple-harmonic-oscillator equation (period 2*pi*sqrt(I/(mgL)), a
    !   fact this test does not need to re-derive, only to reach).
    ! - A linear "drift" model (uniform field with a linear gradient plus a
    !   linear potential) has a closed-form rate vector obtainable by hand
    !   inverting a 2x2 antisymmetric matrix; the generated rates must match.
    ! - The Liouville / phase-space-volume identity div(Pf*qdot) = 0 is
    !   checked both symbolically and by numeric sampling.
    !
    ! Every comparison below is against a value derived by hand outside this
    ! module, not against whatever the module itself produces.
    use, intrinsic :: iso_fortran_env, only: dp => real64, int64
    use fortsym_string, only: str
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, rat, sin, cos, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_diff, only: diff
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym, only: simplify
    use fortsym_engine, only: engine_result_t
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_phasespace, only: phase_lagrangian_t, phase_lagrangian_create, &
        symplectic_form, pfaffian, phase_space_rates, noether_rate, &
        reparametrize_rates, el_residual
    implicit none

    type(arena_t), target :: arena
    type(native_engine_t) :: engine
    type(suite_t) :: suite
    type(binding_t) :: bindings
    logical :: defined
    integer :: nfail

    nfail = 0
    call arena%init()
    engine = make_native_engine(arena)
    call suite_begin(suite, "fortsym_phasespace")

    call test_magnetic_field()
    call test_pendulum()
    call test_linear_drift()
    call test_second_order_el()

    call suite_end(suite)
    if (nfail /= 0) error stop 1
    print *, "test_fortsym_phasespace: all checks passed"

contains

    !> Symmetric-gauge phase-space Lagrangian for a charged particle in a
    !> uniform magnetic field B along z:
    !>     L = (m*vx - qB/2*y) xdot + (m*vy + qB/2*x) ydot - (m/2)(vx**2+vy**2)
    !> Hand solution of omega qdot = grad h gives
    !>     xdot = vx,  ydot = vy,
    !>     vxdot = (qB/m) vy,  vydot = -(qB/m) vx,
    !> which is circular motion at the cyclotron frequency omega_c = qB/m,
    !> and Pf(omega) = -m**2.
    subroutine test_magnetic_field()
        type(expr_t) :: x, y, vx, vy, mass, qcharge, bfield
        type(expr_t) :: coord(4), aform(4), h
        type(phase_lagrangian_t) :: pl
        type(expr_t), allocatable :: omega(:, :), qdot(:)
        type(expr_t) :: pf, residual, liouville
        logical :: ok
        character(:), allocatable :: message
        character(6) :: names(7)
        real(dp) :: values(7)

        x = sym(arena, "x_mag")
        y = sym(arena, "y_mag")
        vx = sym(arena, "vx_mag")
        vy = sym(arena, "vy_mag")
        mass = sym(arena, "m_mag")
        qcharge = sym(arena, "q_mag")
        bfield = sym(arena, "b_mag")

        coord = [x, y, vx, vy]
        aform(1) = mass*vx - rat(arena, 1_int64, 2_int64)*qcharge*bfield*y
        aform(2) = mass*vy + rat(arena, 1_int64, 2_int64)*qcharge*bfield*x
        aform(3) = num(arena, 0)
        aform(4) = num(arena, 0)
        h = rat(arena, 1_int64, 2_int64)*mass*(vx**2 + vy**2)

        pl = phase_lagrangian_create(coord, aform, h, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL magnetic: build Lagrangian: ", message
            return
        end if

        call phase_space_rates(pl, qdot, ok, message, pf=pf)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL magnetic: solve rates: ", message
            return
        end if
        call simplify_all(qdot)
        block
            type(engine_result_t) :: pf_simplified
            pf_simplified = simplify(pf)
            pf = pf_simplified%value
        end block

        call check_identity(suite, engine, "magnetic: xdot = vx", qdot(1) - vx)
        call check_identity(suite, engine, "magnetic: ydot = vy", qdot(2) - vy)
        residual = mass*qdot(3) - qcharge*bfield*vy
        call check_identity(suite, engine, "magnetic: m*vxdot = qB*vy", residual)
        residual = mass*qdot(4) + qcharge*bfield*vx
        call check_identity(suite, engine, "magnetic: m*vydot = -qB*vx", residual)
        residual = pf + mass**2
        call check_identity(suite, engine, "magnetic: Pf(omega) = -m**2", residual)

        residual = noether_rate(pl, 1, qdot) - &
            rat(arena, 1_int64, 2_int64)*qcharge*bfield*vy
        call check_identity(suite, engine, "magnetic: d/dt(a_x) = qB/2 vy", &
            residual)

        ! Liouville / phase-space volume preservation: div(Pf*qdot) = 0.
        omega = symplectic_form(pl)
        liouville = diff(pf*qdot(1), x) + diff(pf*qdot(2), y) + &
            diff(pf*qdot(3), vx) + diff(pf*qdot(4), vy)
        call check_identity(suite, engine, "magnetic: div(Pf*qdot) = 0", liouville)

        names = ["x_mag ", "y_mag ", "vx_mag", "vy_mag", "m_mag ", "q_mag ", &
            "b_mag "]
        values = [0.3_dp, -0.7_dp, 1.1_dp, 0.4_dp, 2.0_dp, 1.5_dp, 0.6_dp]
        call check_numeric("magnetic: m*vxdot - qB*vy (numeric)", &
            mass*qdot(3) - qcharge*bfield*vy, names, values, 1.0e-10_dp)
        call check_numeric("magnetic: div(Pf*qdot) = 0 (numeric)", liouville, &
            names, values, 1.0e-10_dp)
        values = [1.9_dp, 0.2_dp, -0.5_dp, 0.9_dp, 1.3_dp, -0.8_dp, 1.7_dp]
        call check_numeric("magnetic: div(Pf*qdot) = 0 (numeric, point 2)", &
            liouville, names, values, 1.0e-10_dp)
    end subroutine test_magnetic_field

    !> Canonical pendulum: q = (theta, p), a = (p, 0),
    !>     h = p**2/(2*inertia) - m*g*length*cos(theta).
    !> Hand solution: thetadot = p/inertia, pdot = -m*g*length*sin(theta).
    !> Energy is exactly conserved along the flow: dh/dt = 0 by cancellation,
    !> with no trigonometric identity required. The small-angle Hamiltonian
    !> h_lin = p**2/(2*inertia) + (1/2)*m*g*length*theta**2 reduces the same
    !> construction to the textbook simple-harmonic-oscillator equation
    !>     thetaddot + (m*g*length/inertia)*theta = 0,
    !> whose period 2*pi*sqrt(inertia/(m*g*length)) is a standard fact this
    !> test does not re-derive, only reach.
    subroutine test_pendulum()
        type(expr_t) :: theta, p, mass, gval, length, inertia
        type(expr_t) :: coord(2), aform(2), h, h_lin
        type(phase_lagrangian_t) :: pl, pl_lin
        type(expr_t), allocatable :: qdot(:), qdot_lin(:)
        type(expr_t) :: dh_dt, thetaddot, residual
        logical :: ok
        character(:), allocatable :: message
        character(6) :: names(4)
        real(dp) :: values(4)

        theta = sym(arena, "theta")
        p = sym(arena, "p_pend")
        mass = sym(arena, "m_pend")
        gval = sym(arena, "g_pend")
        length = sym(arena, "l_pend")
        inertia = sym(arena, "i_pend")

        coord = [theta, p]
        aform(1) = p
        aform(2) = num(arena, 0)
        h = p**2/(2*inertia) - mass*gval*length*cos(theta)

        pl = phase_lagrangian_create(coord, aform, h, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL pendulum: build Lagrangian: ", message
            return
        end if
        call phase_space_rates(pl, qdot, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL pendulum: solve rates: ", message
            return
        end if
        call simplify_all(qdot)

        residual = qdot(1) - p/inertia
        call check_identity(suite, engine, "pendulum: thetadot = p/I", residual)
        residual = qdot(2) + mass*gval*length*sin(theta)
        call check_identity(suite, engine, "pendulum: pdot = -mgL sin(theta)", &
            residual)

        dh_dt = diff(h, theta)*qdot(1) + diff(h, p)*qdot(2)
        call check_identity(suite, engine, "pendulum: dh/dt = 0", dh_dt)

        names = ["theta ", "p_pend", "m_pend", "g_pend"]
        values = [0.8_dp, 1.2_dp, 1.5_dp, 9.8_dp]
        call check_numeric("pendulum: dh/dt = 0 (numeric)", dh_dt, &
            [names, "l_pend", "i_pend"], [values, 0.5_dp, 2.2_dp], 1.0e-10_dp)

        ! Small-angle limit: harmonic Hamiltonian, textbook SHM equation.
        h_lin = p**2/(2*inertia) + rat(arena, 1_int64, 2_int64)* &
            mass*gval*length*theta**2
        pl_lin = phase_lagrangian_create(coord, aform, h_lin, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL pendulum: build linearised Lagrangian: ", message
            return
        end if
        call phase_space_rates(pl_lin, qdot_lin, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL pendulum: solve linearised rates: ", message
            return
        end if
        call simplify_all(qdot_lin)
        ! thetaddot = d(thetadot)/dt along the flow, by the same chain rule
        ! noether_rate uses for a_k, applied here to the rate itself.
        thetaddot = diff(qdot_lin(1), theta)*qdot_lin(1) + &
            diff(qdot_lin(1), p)*qdot_lin(2)
        residual = thetaddot + (mass*gval*length/inertia)*theta
        call check_identity(suite, engine, &
            "pendulum: small-angle SHM thetaddot + omega0**2 theta = 0", residual)
    end subroutine test_pendulum

    !> Linear drift model: q = (x, y), a = (0, b0*x), h = qd*efield*y.
    !> omega_12 = b0 (constant), grad h = (0, qd*efield). Hand inversion of
    !> the constant antisymmetric 2x2 matrix [[0, b0], [-b0, 0]] gives
    !>     ydot = 0,  xdot = -qd*efield/b0,
    !> the constant "E cross B"-style drift velocity, reached here from a
    !> generic linear field and a generic linear potential with no reference
    !> to any specific physical system.
    subroutine test_linear_drift()
        type(expr_t) :: x, y, b0, efield, qd
        type(expr_t) :: coord(2), aform(2), h
        type(phase_lagrangian_t) :: pl
        type(expr_t), allocatable :: qdot(:)
        type(expr_t) :: residual
        logical :: ok
        character(:), allocatable :: message
        character(6) :: names(3)
        real(dp) :: values(3)

        x = sym(arena, "x_drft")
        y = sym(arena, "y_drft")
        b0 = sym(arena, "b0_drf")
        efield = sym(arena, "e0_drf")
        qd = sym(arena, "qd_drf")

        coord = [x, y]
        aform(1) = num(arena, 0)
        aform(2) = b0*x
        h = qd*efield*y

        pl = phase_lagrangian_create(coord, aform, h, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL drift: build Lagrangian: ", message
            return
        end if
        call phase_space_rates(pl, qdot, ok, message)
        if (.not. ok) then
            nfail = nfail + 1
            print *, "FAIL drift: solve rates: ", message
            return
        end if
        call simplify_all(qdot)

        call check_identity(suite, engine, "drift: ydot = 0", qdot(2))
        residual = b0*qdot(1) + qd*efield
        call check_identity(suite, engine, "drift: b0*xdot = -qd*efield", residual)

        names = ["x_drft", "y_drft", "b0_drf"]
        values = [0.4_dp, -1.1_dp, 2.3_dp]
        call check_numeric("drift: b0*xdot + qd*efield = 0 (numeric)", &
            residual, [names, "e0_drf", "qd_drf"], [values, 0.9_dp, -1.4_dp], &
            1.0e-10_dp)

        ! Reparametrisation: dy/dx along the flow when b0 == 0 is undefined
        ! (ydot == 0 identically), so exercise the helper on the nontrivial
        ! ratio dx/dy at points where ydot is not the pivot: here the pair
        ! (xdot, ydot) reparametrised by index 1 must return (1, ydot/xdot).
        block
            type(expr_t), allocatable :: dq_dx(:)
            dq_dx = reparametrize_rates(qdot, 1)
            call check_identity(suite, engine, "drift: dx/dx = 1", &
                dq_dx(1) - num(arena, 1))
            call check_identity(suite, engine, "drift: dy/dx = ydot/xdot", &
                dq_dx(2)*qdot(1) - qdot(2))
        end block
    end subroutine test_linear_drift

    !> Classical (second-order) Euler-Lagrange residual, checked against the
    !> textbook pendulum ODE derived independently by hand:
    !>     L(theta, thetadot) = (1/2)*inertia*thetadot**2 + m*g*length*cos(theta)
    !>     d/dt(dL/dthetadot) - dL/dtheta = inertia*thetaddot + m*g*length*sin(theta)
    subroutine test_second_order_el()
        type(expr_t) :: theta2, thetadot2, thetaddot2, mass2, gval2, length2
        type(expr_t) :: lagr, residual(1)
        real(dp) :: values(6)
        character(6) :: names(6)

        theta2 = sym(arena, "th2")
        thetadot2 = sym(arena, "thd2")
        thetaddot2 = sym(arena, "thdd2")
        mass2 = sym(arena, "m2")
        gval2 = sym(arena, "g2")
        length2 = sym(arena, "l2")

        lagr = rat(arena, 1_int64, 2_int64)*mass2*length2**2*thetadot2**2 + &
            mass2*gval2*length2*cos(theta2)

        residual = el_residual(lagr, [theta2], [thetadot2], [thetaddot2])
        call check_identity(suite, engine, &
            "el_residual: matches hand-derived pendulum ODE", &
            residual(1) - (mass2*length2**2*thetaddot2 + &
            mass2*gval2*length2*sin(theta2)))

        names = ["th2   ", "thd2  ", "thdd2 ", "m2    ", "g2    ", "l2    "]
        values = [0.6_dp, 1.1_dp, -0.3_dp, 1.4_dp, 9.8_dp, 0.7_dp]
        call check_numeric("el_residual: numeric agreement", residual(1) - &
            (mass2*length2**2*thetaddot2 + mass2*gval2*length2*sin(theta2)), &
            names, values, 1.0e-10_dp)
    end subroutine test_second_order_el

    !> matrix_inverse's fraction-free elimination can leave qdot components
    !> with removable spurious singularities in their unsimplified form (a
    !> 0/0 that cancels algebraically but trips a naive numeric evaluator).
    !> Simplifying once, right after solving, is the same "simplify before
    !> using" convention fortsym's own kernel generation follows.
    subroutine simplify_all(v)
        type(expr_t), intent(inout) :: v(:)
        type(engine_result_t) :: r
        integer :: k
        do k = 1, size(v)
            r = simplify(v(k))
            v(k) = r%value
        end do
    end subroutine simplify_all

    subroutine check_numeric(label, expression, names, values, tolerance)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: expression
        character(*), intent(in) :: names(:)
        real(dp), intent(in) :: values(:)
        real(dp), intent(in) :: tolerance
        real(dp) :: value
        integer :: k

        bindings%n = size(names)
        if (allocated(bindings%names)) deallocate (bindings%names)
        if (allocated(bindings%values)) deallocate (bindings%values)
        allocate (bindings%names(bindings%n), bindings%values(bindings%n))
        do k = 1, bindings%n
            bindings%names(k) = str(trim(names(k)))
            bindings%values(k) = values(k)
        end do

        value = eval_expr(expression, bindings, defined)
        if (.not. defined .or. abs(value) > tolerance) then
            nfail = nfail + 1
            print *, "FAIL ", label, " residual=", value, " defined=", defined
        end if
    end subroutine check_numeric

end program test_fortsym_phasespace
