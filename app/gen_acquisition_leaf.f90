program gen_acquisition_leaf
    ! Derive the analytic acquisition family for FortBO and emit it as a scalar
    ! Fortran leaf. Nothing here is transcribed: expected improvement and
    ! probability of improvement are stated once from the definition of the
    ! Gaussian CDF and PDF, and every derivative is produced by fortsym's
    ! differentiation rules rather than by a human copying a textbook formula.
    !
    ! FortBO minimizes. With incumbent `best` and exploration offset `xi` the
    ! improvement threshold is tau = best - xi and z = (tau - mu) / sigma, so
    !
    !     Phi(z) = (1 + erf(z / sqrt(2))) / 2
    !     phi(z) = exp(-z^2 / 2) / sqrt(2 pi)
    !     EI     = (tau - mu) Phi(z) + sigma phi(z)
    !     PI     = Phi(z)
    !
    ! The emitted leaf returns EI, PI, and the four first-order products that a
    ! candidate optimizer needs, all from the same expression graph.
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fortsym_arena, only: arena_t
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_expr, only: expr_t, sym, num, pi_expr, exp_expr => exp, &
        sqrt_expr => sqrt, erf_expr => erf, operator(+), operator(-), &
        operator(*), operator(/), operator(**)
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_string, only: chars, str, str_t
    implicit none

    type(arena_t), target :: arena
    type(expr_t) :: mu, sigma, best, xi
    type(expr_t) :: tau, gap, z, two, normal_cdf, normal_pdf, ei, root(6)
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
            'usage: gen_acquisition_leaf OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    mu = sym(arena, 'mu')
    sigma = sym(arena, 'sigma')
    best = sym(arena, 'best')
    xi = sym(arena, 'xi')

    tau = best - xi
    gap = tau - mu
    z = gap/sigma
    two = num(arena, 2)
    normal_cdf = (1 + erf_expr(z/sqrt_expr(two)))/two
    normal_pdf = exp_expr(-(z**2)/two)/sqrt_expr(two*pi_expr(arena))
    ei = gap*normal_cdf + sigma*normal_pdf

    root(1) = ei
    root(2) = diff(ei, mu)
    root(3) = diff(ei, sigma)
    root(4) = normal_cdf
    root(5) = diff(normal_cdf, mu)
    root(6) = diff(normal_cdf, sigma)

    ! Differentiation leaves the usual zero and unit debris behind. Simplifying
    ! each root before lowering is what turns the derivative graph into a kernel
    ! worth compiling; without it the emitted leaf carries dozens of
    ! multiply-by-zero temporaries.
    engine = make_native_engine(arena)
    do k = 1, size(root)
        simplified = engine%simplify(root(k))
        if (.not. simplified%ok) then
            write (output_unit, '(a)') 'gen_acquisition_leaf: simplification failed: '// &
                chars(simplified%message)
            error stop 1
        end if
        root(k) = simplified%value
    end do

    call lower_kernel_ir(root, ir, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_acquisition_leaf: '//message
        error stop 1
    end if

    spec%name = str('fortbo_generated_acquisition_leaf')
    spec%args = [str('mu'), str('sigma'), str('best'), str('xi')]
    spec%outputs = [str('ei'), str('ei_d_mu'), str('ei_d_sigma'), &
        str('pi_value'), str('pi_d_mu'), str('pi_d_sigma')]
    spec%temp_prefix = str('t')
    spec%generator = str('gen_acquisition_leaf')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_acquisition_leaf OUTPUT_PATH FORTSYM_REVISION')
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_acquisition_leaf: '//message
        error stop 1
    end if

    open (newunit=unit, file=trim(output(:output_length)), status='replace', &
        action='write', iostat=ios)
    if (ios /= 0) then
        write (output_unit, '(a)') 'gen_acquisition_leaf: cannot write output'
        error stop 1
    end if
    write (unit, '(a)') chars(source)
    close (unit)
    write (output_unit, '(a)') 'gen_acquisition_leaf: wrote '// &
        trim(output(:output_length))
end program gen_acquisition_leaf
