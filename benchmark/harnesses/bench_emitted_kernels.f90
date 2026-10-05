program bench_emitted_kernels
    use, intrinsic :: iso_fortran_env, only: real64, real128, int64, &
        compiler_version, compiler_options
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortsym_interval_runtime, only: interval_t, ipoint, ienclose
    use emitted_audit_kernels, only: audit_primal, audit_jvp, audit_vjp, &
        audit_interval, audit_paired_float, source_revision
    implicit none

    integer, parameter :: dp = real64, qp = real128
    integer, parameter :: points = 257, batches = 200, repetitions = 11, warmups = 2
    real(dp) :: inputs(2, points), checksum, elapsed, expected_values(2, points, 3)
    real(qp) :: exact_values(2, points)
    real(dp) :: last_values(2, points)
    type(interval_t) :: last_intervals(2, points), boxes(2, points)
    character(64) :: argument
    integer :: k, stage, repetition, batch
    integer(int64) :: started, finished, rate
    logical :: validate_only

    call get_command_argument(1, argument)
    validate_only = trim(argument) == "--validate-only"
    if (len_trim(argument) > 0 .and. .not. validate_only) then
        error stop "usage: bench_emitted_kernels [--validate-only]"
    end if
    do k = 1, points
        inputs(1, k) = real(k - 129, dp)/128.0_dp
        inputs(2, k) = real(mod(73*k, 257) - 128, dp)/128.0_dp
        boxes(1, k) = ienclose(inputs(1, k), 1.0_dp/1024.0_dp)
        boxes(2, k) = ienclose(inputs(2, k), 1.0_dp/1024.0_dp)
    end do
    call validate()
    if (validate_only) then
        print '(a)', "PASS emitted primal/JVP/VJP and reference interval audit"
        stop
    end if
    print '(a)', '# source_revision='//source_revision
    print '(a)', '# compiler='//compiler_version()
    print '(a)', '# compiler_options='//compiler_options()
    print '(a)', '# domain=[-1,1]^2; direction=(3/8,-5/8); weights=(-1/2,3/4)'
    print '(a)', '# interval_runtime=reference_fixture; cell_radius=1/1024'
    write (*, '(a,i0)') '# persistent_harness_state_bytes=', &
        (storage_size(inputs)*size(inputs) + &
        storage_size(last_values)*size(last_values) + &
        storage_size(expected_values)*size(expected_values) + &
        storage_size(exact_values)*size(exact_values) + &
        storage_size(last_intervals)*size(last_intervals) + &
        storage_size(boxes)*size(boxes))/8
    print '(a)', '# validation=real128_primal,finite_difference,adjoint,point_and_cell'
    print '(a)', 'schema,stage,sample,warmups,points,batches,total_seconds,'// &
        'seconds_per_call,checksum'
    call system_clock(count_rate=rate)
    if (rate <= 0_int64) error stop "monotonic timer unavailable"
    do stage = 1, 6
        do repetition = 1, warmups
            do batch = 1, batches
                call evaluate_stage(stage)
            end do
        end do
        do repetition = 1, repetitions
            call system_clock(started)
            do batch = 1, batches
                call evaluate_stage(stage)
            end do
            call system_clock(finished)
            elapsed = real(finished - started, dp)/real(rate, dp)
            ! The exact replay is independently validated outside timing.
            call check_timed(stage)
            checksum = sum(last_values)
            if (stage == 4 .or. stage == 5) checksum = sum(last_intervals%lo) + &
                sum(last_intervals%hi)
            if (.not. ieee_is_finite(checksum)) error stop "invalid timed output"
            write (*, '(i0,a,a,4(a,i0),3(a,es24.16e3))') 1, ',', &
                stage_name(stage), ',', repetition, ',', warmups, ',', points, &
                ',', batches, ',', elapsed, ',', elapsed/real(points*batches, dp), &
                ',', checksum
        end do
    end do

contains

    function reference_value(x, y) result(value)
        real(qp), intent(in) :: x, y
        real(qp) :: value(2)

        ! Independent numerical oracle; no symbolic evaluation or generated leaf.
        value(1) = (x + y)*(x + y)/(3.0_qp + x*x)
        value(2) = (x - y)*(x - y)*(x - y)/5.0_qp + y*y
    end function reference_value

    subroutine validate()
        real(qp), parameter :: h = 1.0e-6_qp
        real(qp) :: x, y, jacobian(2, 2), shifted(2), exact(2), plus(2), minus(2)
        real(dp) :: value(2), tangent(2), adjoint(2), paired(2)
        type(interval_t) :: enclosed(2), cell(2)
        integer :: i, sx, sy, component

        do i = 1, points
            x = real(inputs(1, i), qp)
            y = real(inputs(2, i), qp)
            exact = reference_value(x, y)
            exact_values(:, i) = exact
            expected_values(:, i, 1) = real(exact, dp)
            plus = reference_value(x + h, y)
            minus = reference_value(x - h, y)
            do component = 1, 2
                jacobian(component, 1) = (plus(component) - &
                    minus(component))/(2.0_qp*h)
            end do
            plus = reference_value(x, y + h)
            minus = reference_value(x, y - h)
            do component = 1, 2
                jacobian(component, 2) = (plus(component) - &
                    minus(component))/(2.0_qp*h)
            end do
            expected_values(:, i, 2) = real( &
                0.375_qp*jacobian(:, 1) - 0.625_qp*jacobian(:, 2), dp)
            expected_values(:, i, 3) = real( &
                -0.5_qp*jacobian(1, :) + 0.75_qp*jacobian(2, :), dp)
            call audit_primal(inputs(1, i), inputs(2, i), value(1), value(2))
            call audit_paired_float(inputs(1, i), inputs(2, i), paired(1), paired(2))
            call check_vector(value, expected_values(:, i, 1), 2.0e-14_dp)
            call check_vector(paired, expected_values(:, i, 1), 2.0e-14_dp)
            call audit_jvp(inputs(1, i), inputs(2, i), 0.375_dp, -0.625_dp, &
                tangent(1), tangent(2))
            call audit_vjp(inputs(1, i), inputs(2, i), -0.5_dp, 0.75_dp, &
                adjoint(1), adjoint(2))
            call check_vector(tangent, expected_values(:, i, 2), 1.0e-8_dp)
            call check_vector(adjoint, expected_values(:, i, 3), 1.0e-8_dp)
            if (abs(-0.5_dp*tangent(1) + 0.75_dp*tangent(2) - &
                0.375_dp*adjoint(1) + 0.625_dp*adjoint(2)) > &
                2.0e-13_dp) error stop "adjoint identity failed"
            call audit_interval(ipoint(inputs(1, i)), ipoint(inputs(2, i)), &
                enclosed(1), enclosed(2))
            call check_enclosure(enclosed, exact)
            if (maxval(enclosed%hi - enclosed%lo) > &
                1.0e-11_dp) error stop "reference point enclosure too wide"
            call audit_interval(boxes(1, i), boxes(2, i), cell(1), cell(2))
            do sx = -1, 1
                do sy = -1, 1
                    shifted = reference_value(x + real(sx, qp)/1024.0_qp, &
                        y + real(sy, qp)/1024.0_qp)
                    call check_enclosure(cell, shifted)
                end do
            end do
        end do
        ! The checker must reject a corrupted result and a missed enclosure.
        value = expected_values(:, 1, 1)
        value(1) = value(1) + 1.0_dp
        if (vector_matches(value, expected_values(:, 1, 1), &
            2.0e-14_dp)) error stop "corruption oracle failed"
        enclosed(1) = ipoint(real(exact_values(1, 1), dp) + 1.0_dp)
        if (contains_value(enclosed(1), exact_values(1, 1))) &
            error stop "missed-enclosure oracle failed"
    end subroutine validate

    subroutine check_timed(selected)
        integer, intent(in) :: selected
        real(dp) :: tolerance
        integer :: i, oracle

        tolerance = 1.0e-8_dp
        if (selected == 1 .or. selected == 6) tolerance = 2.0e-14_dp
        oracle = selected
        if (selected == 6) oracle = 1
        do i = 1, points
            if (selected <= 3 .or. selected == 6) then
                call check_vector(last_values(:, i), &
                    expected_values(:, i, oracle), tolerance)
            else
                call check_enclosure(last_intervals(:, i), exact_values(:, i))
            end if
        end do
    end subroutine check_timed

    logical function vector_matches(value, expected, tolerance) result(matches)
        real(dp), intent(in) :: value(2), expected(2), tolerance

        matches = .false.
        if (.not. all(ieee_is_finite(value))) return
        matches = maxval(abs(value - expected)) <= &
            tolerance*max(1.0_dp, maxval(abs(expected)))
    end function vector_matches

    subroutine check_vector(value, expected, tolerance)
        real(dp), intent(in) :: value(2), expected(2), tolerance

        if (.not. vector_matches(value, expected, tolerance)) &
            error stop "independent value/derivative oracle failed"
    end subroutine check_vector

    logical function contains_value(box, value) result(contains)
        type(interval_t), intent(in) :: box
        real(qp), intent(in) :: value

        contains = .false.
        if (.not. ieee_is_finite(box%lo)) return
        if (.not. ieee_is_finite(box%hi)) return
        contains = real(box%lo, qp) <= value .and. value <= real(box%hi, qp)
    end function contains_value

    subroutine check_enclosure(box, value)
        type(interval_t), intent(in) :: box(2)
        real(qp), intent(in) :: value(2)
        integer :: i

        do i = 1, 2
            if (.not. contains_value(box(i), value(i))) &
                error stop "independent enclosure oracle failed"
        end do
    end subroutine check_enclosure

    subroutine evaluate_stage(selected)
        integer, intent(in) :: selected
        integer :: i

        do i = 1, points
            select case (selected)
            case (1)
                call audit_primal(inputs(1, i), inputs(2, i), &
                    last_values(1, i), last_values(2, i))
            case (2)
                call audit_jvp(inputs(1, i), inputs(2, i), 0.375_dp, -0.625_dp, &
                    last_values(1, i), last_values(2, i))
            case (3)
                call audit_vjp(inputs(1, i), inputs(2, i), -0.5_dp, 0.75_dp, &
                    last_values(1, i), last_values(2, i))
            case (4)
                call audit_interval(ipoint(inputs(1, i)), ipoint(inputs(2, i)), &
                    last_intervals(1, i), last_intervals(2, i))
            case (5)
                call audit_interval(boxes(1, i), boxes(2, i), &
                    last_intervals(1, i), last_intervals(2, i))
            case (6)
                call audit_paired_float(inputs(1, i), inputs(2, i), &
                    last_values(1, i), last_values(2, i))
            end select
        end do
    end subroutine evaluate_stage

    function stage_name(selected) result(name)
        integer, intent(in) :: selected
        character(:), allocatable :: name

        select case (selected)
        case (1)
            name = "primal"
        case (2)
            name = "jvp"
        case (3)
            name = "vjp"
        case (4)
            name = "reference_interval_point"
        case (5)
            name = "reference_interval_cell"
        case (6)
            name = "paired_float"
        end select
    end function stage_name
end program bench_emitted_kernels
