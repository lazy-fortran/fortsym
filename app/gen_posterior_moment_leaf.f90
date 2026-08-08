program gen_posterior_moment_leaf
    ! Derive FortBO's posterior standard-deviation derivatives and emit them as
    ! a scalar Fortran leaf.
    !
    ! A GP posterior gradient looks like block-matrix work and is not. Writing
    ! the mean and variance at a query point,
    !
    !     m(x) = k(x)^T alpha,          alpha = K^{-1} y
    !     v(x) = k(x,x) - k(x)^T K^{-1} k(x)
    !
    ! the derivatives with respect to x are
    !
    !     dm/dx_i = (dk/dx_i)^T alpha
    !     dv/dx_i = dk(x,x)/dx_i - 2 (dk/dx_i)^T K^{-1} k(x)
    !
    ! and every matrix in those expressions -- alpha, K^{-1} k(x) -- is a
    ! *numeric* vector produced by a runtime solve against data. Nothing here
    ! requires a symbolic inverse, a symbolic determinant, or a symbolic
    ! eigenvalue. The symbolic content is exactly two things: the kernel's own
    ! input derivatives, which fortsym already emits per kernel, and the chain
    ! rule from variance to standard deviation, which is this leaf.
    !
    ! That chain rule is where FortBO's one numerical safeguard comes from:
    !
    !     s   = sqrt(v)
    !     s'  = v' / (2 sqrt(v))
    !     s'' = v'' / (2 sqrt(v)) - (v')^2 / (4 v^{3/2})
    !
    ! The second term carries `v^{-3/2}`, which is `s^{-3}`. That is not a
    ! stylistic observation -- it is why `FORTBO_SD_HESSIAN_FLOOR` exists and
    ! why it sits where it does: at a standard deviation of 1e-6 the term
    ! already reaches about 1e18, and a Newton step built on it means nothing.
    ! Deriving the expression rather than asserting it makes that bound
    ! something the generator produces rather than something a comment claims.
    !
    ! Everything with mathematical content here is scalar, so fortsym derives
    ! it today. The block-matrix milestone M9 is not on the path.
    use, intrinsic :: iso_fortran_env, only: output_unit, real64
    use fortsym_arena, only: arena_t
    use fortsym_diff, only: diff
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: make_native_engine, native_engine_t
    use fortsym_expr, only: expr_t, sym, exp_expr => exp, operator(-), operator(*), &
        operator(+), operator(**)
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_string, only: chars, str, str_t
    implicit none
    integer, parameter :: dp = real64

    type(arena_t), target :: arena
    type(expr_t) :: variance, variance_d1, variance_d2
    type(expr_t) :: sd, sd_d1, sd_d2, root(3)
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
            'usage: gen_posterior_moment_leaf OUTPUT_PATH FORTSYM_REVISION'
        error stop 2
    end if

    call arena%init()
    engine = make_native_engine(arena)
    variance = sym(arena, 'variance')
    variance_d1 = sym(arena, 'variance_d1')
    variance_d2 = sym(arena, 'variance_d2')

    ! sqrt(v) written as v^(1/2), so the engine differentiates a power rather
    ! than a named function and the emitted leaf stays free of a sqrt call in
    ! the derivative terms.
    sd = variance**0.5_dp
    sd_d1 = diff(sd, variance)*variance_d1
    ! The second derivative of a composition: the curvature of sqrt applied to
    ! the first derivative squared, plus sqrt' applied to the second. Written
    ! by differentiating the first derivative symbolically rather than by
    ! quoting the closed form, so the v^(-3/2) term is derived and not asserted.
    sd_d2 = diff(sd, variance)*variance_d2 &
        + diff(diff(sd, variance), variance)*variance_d1*variance_d1
    root = [sd, sd_d1, sd_d2]

    do k = 1, size(root)
        simplified = engine%simplify(root(k))
        if (.not. simplified%ok) then
            write (output_unit, '(a)') &
                'gen_posterior_moment_leaf: simplification failed: '// &
                chars(simplified%message)
            error stop 1
        end if
        root(k) = simplified%value
    end do

    call lower_kernel_ir(root, ir, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_posterior_moment_leaf: '//message
        error stop 1
    end if

    spec%name = str('fortbo_generated_posterior_moment_leaf')
    spec%args = [str('variance'), str('variance_d1'), str('variance_d2')]
    spec%outputs = [str('sd'), str('sd_d1'), str('sd_d2')]
    spec%temp_prefix = str('t')
    ! FortBO calls this from a `pure` elemental loop.
    spec%pure_procedure = .true.
    spec%generator = str('gen_posterior_moment_leaf')
    spec%generator_revision = str(trim(revision(:revision_length)))
    spec%regenerate_command = str( &
        'fo exec gen_posterior_moment_leaf OUTPUT_PATH FORTSYM_REVISION')
    source = emit_fortran_kernel_ir(ir, spec, good, message)
    if (.not. good) then
        write (output_unit, '(a)') 'gen_posterior_moment_leaf: '//message
        error stop 1
    end if

    open (newunit=unit, file=trim(output(:output_length)), status='replace', &
          action='write', iostat=ios)
    if (ios /= 0) then
        write (output_unit, '(a)') 'gen_posterior_moment_leaf: cannot write output'
        error stop 1
    end if
    write (unit, '(a)') chars(source)
    close (unit)
    write (output_unit, '(a)') 'gen_posterior_moment_leaf: wrote '// &
        trim(output(:output_length))
end program gen_posterior_moment_leaf
