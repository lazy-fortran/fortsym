program gen_preference_leaf
    ! Derive the pairwise-preference likelihood family for FortBO and emit it as
    ! a scalar Fortran leaf. As with the acquisition leaf, nothing is
    ! transcribed: the Thurstone-Mosteller (probit) preference model is stated
    ! once from its definition and every derivative comes from fortsym's
    ! differentiation rules.
    !
    ! FortBO minimizes, so `a` is preferred to `b` when its latent value is
    ! lower. Under independent Gaussian judgement noise of scale `s` on each
    ! latent value, the difference of the two noisy judgements is Gaussian with
    ! standard deviation sqrt(2) s, so with
    !
    !     z     = (f_b - f_a) / (sqrt(2) s)
    !     delta = z / sqrt(2) = (f_b - f_a) / (2 s)
    !     P     = Phi(z) = (1 + erf(delta)) / 2 = erfc(-delta) / 2
    !     L     = log(P)
    !
    ! P is the probability that `a` is judged better than `b`. The emitted leaf
    ! returns P and L together with their first derivatives with respect to both
    ! latent values and the noise scale, all from one expression graph, because
    ! preference-model fitting needs the score and the acquisition needs the
    ! probability and they must agree exactly.
    !
    ! `erfc` rather than `1 + erf` is used for the log branch: for a confidently
    ! ranked pair delta is large and negative, `1 + erf(delta)` cancels to zero
    ! in double precision, and the log of it is -infinity where the true value
    ! is merely large. The caller guards the far tail asymptotically.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fortsym_arena, only: arena_t
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_expr, only: expr_t, sym, num, erfc_expr => erfc, &
        log_expr => log, operator(+), operator(-), &
        operator(*), operator(/)
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_string, only: chars, str, str_t
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: f_a, f_b, scale
    type(expr_t) :: two, delta, probability, log_probability, root(8)
    type(native_engine_t) :: engine
    type(engine_result_t) :: simplified
    type(kernel_ir_t) :: ir
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(len=2048) :: output, revision
    character(:), allocatable :: message
    integer :: output_length, revision_length, status, unit, ios, k
    logical :: good

    call get_command_argument(1, output, length=output_length, status=status)
    call get_command_argument(2, revision, length=revision_length, status=status)
    if (output_length == 0 .or. revision_length == 0) then
        write (output_unit, '(a)') &
            'usage: gen_preference_leaf OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    f_a = sym(arena, 'f_a')
    f_b = sym(arena, 'f_b')
    scale = sym(arena, 'scale')

    two = num(arena, 2)
    ! The standard normal CDF is Phi(z) = erfc(-z / sqrt(2)) / 2, so the erfc
    ! argument absorbs a second factor of sqrt(2) and the divisor is 2 s, not
    ! sqrt(2) s. Writing it any other way misplaces the noise scale by sqrt(2).
    delta = (f_b - f_a)/(two*scale)
    probability = erfc_expr(0 - delta)/two
    log_probability = log_expr(erfc_expr(0 - delta)) - log_expr(two)

    root(1) = probability
    root(2) = diff(probability, f_a)
    root(3) = diff(probability, f_b)
    root(4) = diff(probability, scale)
    root(5) = log_probability
    root(6) = diff(log_probability, f_a)
    root(7) = diff(log_probability, f_b)
    root(8) = diff(log_probability, scale)

    ! Differentiation leaves zero and unit debris behind; simplifying each root
    ! before lowering is what turns the derivative graph into a kernel worth
    ! compiling.
    engine = make_native_engine(arena)
    do k = 1, size(root)
        simplified = engine%simplify(root(k))
        if (.not. simplified%ok) then
            write (output_unit, '(a)') 'gen_preference_leaf: simplification failed: '// &
                chars(simplified%message)
            error stop 1
        end if
        root(k) = simplified%value
    end do

    call lower_kernel_ir(root, ir, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_preference_leaf: '//message
        error stop 1
    end if

    spec%name = str('fortbo_generated_preference_leaf')
    spec%args = [str('f_a'), str('f_b'), str('scale')]
    spec%outputs = [str('probability'), str('probability_d_f_a'), &
        str('probability_d_f_b'), str('probability_d_scale'), &
        str('log_probability'), str('log_probability_d_f_a'), &
        str('log_probability_d_f_b'), str('log_probability_d_scale')]
    spec%temp_prefix = str('t')
    ! FortBO calls this leaf from `pure` scalar helpers.
    spec%pure_procedure = .true.
    spec%generator = str('gen_preference_leaf')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_preference_leaf OUTPUT_PATH FORTSYM_REVISION')
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_preference_leaf: '//message
        error stop 1
    end if

    open (newunit=unit, file=trim(output(:output_length)), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        write (output_unit, '(a)') 'gen_preference_leaf: cannot write output'
        error stop 1
    end if
    write (unit, '(a)') chars(source)
    close (unit)
    write (output_unit, '(a)') 'gen_preference_leaf: wrote '// &
        trim(output(:output_length))
end program gen_preference_leaf
