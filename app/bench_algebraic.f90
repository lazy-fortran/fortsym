program bench_algebraic
    ! End-to-end benchmark for the public bounded algebraic API. Every timed
    ! workload is checked against an independently derived exact result.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_algebraic, only: algebraic_normalize, &
        algebraic_from_re_im, algebraic_add, algebraic_mul, algebraic_div, &
        algebraic_conjugate, algebraic_sqrt, algebraic_pow, algebraic_signs
    use fortsym_engine, only: wall_seconds
    use fortsym_string, only: str_t, str, chars
    implicit none

    integer, parameter :: dp = real64
    integer, parameter :: WARMUPS = 3
    integer, parameter :: REPETITIONS = 21
    integer, parameter :: BATCH = 10
    integer, parameter :: COLD_REPETITIONS = 11
    integer, parameter :: NWORK = 11
    character(32), parameter :: WORKLOADS(NWORK) = [character(32) :: &
        "normalize_irreducible", "normalize_reducible", &
        "normalize_repeated", "gaussian_construct", "add_trace", &
        "multiply_norm", "divide_gaussian", "conjugate", "signed_power", &
        "principal_sqrt", "component_signs"]
    integer :: i

    write (*, '(a)') "schema,backend,scope,workload,warmups,repetitions,"// &
        "batch,median_seconds,p05_seconds,p95_seconds,min_seconds,"// &
        "max_seconds,correct"
    do i = 1, NWORK
        call benchmark_warm(trim(WORKLOADS(i)))
        call benchmark_cold(trim(WORKLOADS(i)))
    end do
    call benchmark_refusal()

contains

    subroutine benchmark_warm(workload)
        character(*), intent(in) :: workload
        real(dp) :: samples(REPETITIONS), started
        type(str_t) :: value
        logical :: ok, correct
        integer :: repetition, k

        do repetition = 1, WARMUPS
            do k = 1, BATCH
                call perform(workload, 1, value, ok)
            end do
        end do
        do repetition = 1, REPETITIONS
            started = wall_seconds()
            do k = 1, BATCH
                call perform(workload, 1, value, ok)
            end do
            samples(repetition) = (wall_seconds() - started)/real(BATCH, dp)
        end do
        correct = validate(workload, value, ok)
        call emit(workload, "warm_same_value", samples, WARMUPS, &
            REPETITIONS, BATCH, correct)
    end subroutine benchmark_warm

    subroutine benchmark_cold(workload)
        character(*), intent(in) :: workload
        real(dp) :: samples(COLD_REPETITIONS), started
        type(str_t) :: value
        logical :: ok, correct
        integer :: repetition

        correct = .true.
        do repetition = 1, COLD_REPETITIONS
            started = wall_seconds()
            call perform(workload, repetition + 1, value, ok)
            samples(repetition) = wall_seconds() - started
            correct = correct .and. validate(workload, value, ok)
        end do
        call emit(workload, "cold_distinct_encoding", samples, 0, &
            COLD_REPETITIONS, 1, correct)
    end subroutine benchmark_cold

    subroutine benchmark_refusal()
        real(dp) :: sample(1), started
        type(str_t) :: value
        logical :: ok
        character(:), allocatable :: oversized

        ! This remains below the 64 KiB serialization cap but is far above the
        ! 4096-bit algebraic-height cap. It measures the current post-operation
        ! refusal path requested by the independent review.
        oversized = "1"//repeat("0", 65500)
        started = wall_seconds()
        value = algebraic_from_re_im(oversized, "0", ok)
        sample(1) = wall_seconds() - started
        call emit("resource_refusal_height", "guard_single", sample, 0, 1, &
            1, .not. ok .and. len(chars(value)) == 0)
    end subroutine benchmark_refusal

    subroutine perform(workload, scale, value, ok)
        character(*), intent(in) :: workload
        integer,      intent(in) :: scale
        type(str_t),  intent(out) :: value
        logical,      intent(out) :: ok
        type(str_t) :: left
        integer :: real_sign, imag_sign

        select case (workload)
        case ("normalize_irreducible")
            value = algebraic_normalize(poly_sqrt_two(scale), ok)
        case ("normalize_reducible")
            value = algebraic_normalize(poly_reducible(scale), ok)
        case ("normalize_repeated")
            value = algebraic_normalize(poly_repeated(scale), ok)
        case ("gaussian_construct")
            value = algebraic_from_re_im(rational(scale, 2*scale), &
                rational(3*scale, 4*scale), ok)
        case ("add_trace")
            value = algebraic_add(poly_z(0, scale), poly_z(1, scale), ok)
        case ("multiply_norm")
            value = algebraic_mul(poly_z(0, scale), poly_z(1, scale), ok)
        case ("divide_gaussian")
            value = algebraic_div(poly_one_plus_i(0, scale), &
                poly_one_plus_i(1, scale), ok)
        case ("conjugate")
            value = algebraic_conjugate(poly_z(0, scale), ok)
        case ("signed_power")
            value = algebraic_pow(poly_one_plus_i(0, scale), 8_int64, ok)
        case ("principal_sqrt")
            value = algebraic_sqrt(poly_minus_two(scale), ok)
        case ("component_signs")
            left = algebraic_sqrt(poly_minus_two(scale), ok)
            if (.not. ok) then
                value = str("")
                return
            end if
            call algebraic_signs(chars(left), real_sign, imag_sign, ok)
            if (ok .and. real_sign == 0 .and. imag_sign == 1) then
                value = str("0,+1")
            else
                value = str("")
                ok = .false.
            end if
        end select
    end subroutine perform

    function validate(workload, value, ok) result(correct)
        character(*), intent(in) :: workload
        type(str_t),   intent(in) :: value
        logical,       intent(in) :: ok
        logical :: correct
        character(:), allocatable :: expected

        select case (workload)
        case ("normalize_irreducible", "normalize_reducible")
            expected = "qqbar1:0:-2,0,1"
        case ("normalize_repeated", "add_trace")
            expected = "qqbar1:0:-1,1"
        case ("gaussian_construct")
            expected = "qqbar1:0:13,-16,16"
        case ("multiply_norm")
            expected = "qqbar1:0:-13,16"
        case ("divide_gaussian")
            expected = "qqbar1:0:1,0,1"
        case ("conjugate")
            expected = "qqbar1:1:13,-16,16"
        case ("signed_power")
            expected = "qqbar1:0:-16,1"
        case ("principal_sqrt")
            expected = "qqbar1:0:2,0,1"
        case ("component_signs")
            expected = "0,+1"
        end select
        correct = ok .and. chars(value) == expected
    end function validate

    function poly_sqrt_two(scale) result(value)
        integer, intent(in) :: scale
        character(:), allocatable :: value
        value = "qqbar1:0:"//integer_text(-2*scale)//",0,"// &
            integer_text(scale)
    end function poly_sqrt_two

    function poly_minus_two(scale) result(value)
        integer, intent(in) :: scale
        character(:), allocatable :: value
        value = "qqbar1:0:"//integer_text(2*scale)//","//integer_text(scale)
    end function poly_minus_two

    function poly_reducible(scale) result(value)
        integer, intent(in) :: scale
        character(:), allocatable :: value
        value = "qqbar1:1:"//integer_text(6*scale)//","// &
            integer_text(-2*scale)//","//integer_text(-3*scale)//","// &
            integer_text(scale)
    end function poly_reducible

    function poly_repeated(scale) result(value)
        integer, intent(in) :: scale
        character(:), allocatable :: value
        value = "qqbar1:1:"//integer_text(scale)//","// &
            integer_text(-2*scale)//","//integer_text(scale)
    end function poly_repeated

    function poly_z(index, scale) result(value)
        integer, intent(in) :: index, scale
        character(:), allocatable :: value
        value = "qqbar1:"//integer_text(index)//":"// &
            integer_text(13*scale)//","//integer_text(-16*scale)//","// &
            integer_text(16*scale)
    end function poly_z

    function poly_one_plus_i(index, scale) result(value)
        integer, intent(in) :: index, scale
        character(:), allocatable :: value
        value = "qqbar1:"//integer_text(index)//":"// &
            integer_text(2*scale)//","//integer_text(-2*scale)//","// &
            integer_text(scale)
    end function poly_one_plus_i

    function rational(numerator, denominator) result(value)
        integer, intent(in) :: numerator, denominator
        character(:), allocatable :: value
        value = integer_text(numerator)//"/"//integer_text(denominator)
    end function rational

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(32) :: buffer
        write (buffer, '(i0)') value
        text = trim(buffer)
    end function integer_text

    subroutine emit(workload, scope, samples, warmups, repetitions, batch, &
            correct)
        character(*), intent(in) :: workload, scope
        real(dp),     intent(in) :: samples(:)
        integer,      intent(in) :: warmups, repetitions, batch
        logical,      intent(in) :: correct
        real(dp) :: ordered(size(samples))
        character(5) :: correct_text

        ordered = samples
        call sort(ordered)
        if (correct) then
            correct_text = "true "
        else
            correct_text = "false"
        end if
        write (*, '(a,",",a,",",a,",",a,3(",",i0),5(",",es16.8),",",a)') &
            "1", "fortsym_qqbar_bridge", scope, workload, warmups, &
            repetitions, batch, percentile(ordered, 0.50_dp), &
            percentile(ordered, 0.05_dp), percentile(ordered, 0.95_dp), &
            ordered(1), ordered(size(ordered)), trim(correct_text)
    end subroutine emit

    subroutine sort(values)
        real(dp), intent(inout) :: values(:)
        real(dp) :: key
        integer :: i, j

        do i = 2, size(values)
            key = values(i)
            j = i - 1
            do while (j >= 1)
                if (values(j) <= key) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = key
        end do
    end subroutine sort

    function percentile(ordered, fraction) result(value)
        real(dp), intent(in) :: ordered(:), fraction
        real(dp) :: value
        integer :: index
        index = 1 + int(fraction*real(size(ordered) - 1, dp))
        value = ordered(index)
    end function percentile

end program bench_algebraic
