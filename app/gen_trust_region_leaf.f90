program gen_trust_region_leaf
    ! Derive the trust-region side-length rescaling for FortBO and emit it as a
    ! scalar Fortran leaf.
    !
    ! TuRBO shapes its trust region by the surrogate's ARD lengthscales, so that
    ! a dimension the model considers unimportant gets a wider side. The rule
    ! must not change the region's *volume* when the surrogate refits, or the
    ! success and failure counters would be measuring a moving target. That
    ! fixes the normalization to the geometric mean:
    !
    !     length_j = (ell_j / geometric_mean(ell)) * L
    !              = exp(log ell_j - mean_k log ell_k) * L
    !
    ! and the invariant follows because the deviations from a mean sum to zero:
    !
    !     product_j length_j = L^d.
    !
    ! Everything with mathematical content here is scalar in `j`, so fortsym
    ! derives it: the exponential relation and its derivatives with respect to
    ! the log lengthscale, the log mean, and the base length. What is left over
    ! is the mean itself, which is a reduction rather than a formula, and which
    ! FortBO performs in Fortran. That split is deliberate and recorded: fortsym
    ! still cannot express a reduction, so the summation stays outside it, but
    ! nothing that *is* an expression is transcribed by hand any more.
    !
    ! Logs throughout. The direct ratio of products underflows at a few hundred
    ! dimensions, which is exactly the regime TuRBO exists for.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fortsym_arena, only: arena_t
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_expr, only: expr_t, sym, exp_expr => exp, operator(-), operator(*)
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_string, only: chars, str, str_t
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: log_lengthscale, log_mean, base_length
    type(expr_t) :: side_length, root(4)
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
            'usage: gen_trust_region_leaf OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    log_lengthscale = sym(arena, 'log_lengthscale')
    log_mean = sym(arena, 'log_mean')
    base_length = sym(arena, 'base_length')

    side_length = exp_expr(log_lengthscale - log_mean)*base_length

    root(1) = side_length
    root(2) = diff(side_length, log_lengthscale)
    root(3) = diff(side_length, log_mean)
    root(4) = diff(side_length, base_length)

    engine = make_native_engine(arena)
    do k = 1, size(root)
        simplified = engine%simplify(root(k))
        if (.not. simplified%ok) then
            write (output_unit, '(a)') &
                'gen_trust_region_leaf: simplification failed: '// &
                chars(simplified%message)
            error stop 1
        end if
        root(k) = simplified%value
    end do

    call lower_kernel_ir(root, ir, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_trust_region_leaf: '//message
        error stop 1
    end if

    spec%name = str('fortbo_generated_trust_region_leaf')
    spec%args = [str('log_lengthscale'), str('log_mean'), str('base_length')]
    spec%outputs = [str('side_length'), str('side_d_log_lengthscale'), &
        str('side_d_log_mean'), str('side_d_base_length')]
    spec%temp_prefix = str('t')
    ! FortBO calls this from a `pure` elemental loop.
    spec%pure_procedure = .true.
    spec%generator = str('gen_trust_region_leaf')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_trust_region_leaf OUTPUT_PATH FORTSYM_REVISION')
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_trust_region_leaf: '//message
        error stop 1
    end if

    open (newunit=unit, file=trim(output(:output_length)), status='replace', &
          action='write', iostat=ios)
    if (ios /= 0) then
        write (output_unit, '(a)') 'gen_trust_region_leaf: cannot write output'
        error stop 1
    end if
    write (unit, '(a)') chars(source)
    close (unit)
    write (output_unit, '(a)') 'gen_trust_region_leaf: wrote '// &
        trim(output(:output_length))
end program gen_trust_region_leaf
