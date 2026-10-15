program test_fortsym_cse_dag
    ! Full CSE must traverse the DAG, not its exponentially expanded tree.
    ! Compiler execution against a numerical recurrence is the value oracle.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sin, cos, &
                            operator(+), operator(/), operator(**)
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, TARGET_FORTRAN_CPU, &
                              PRECISION_REAL32
    use fortsym_string, only: str_t, str, chars
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine, only: engine_result_t
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: e, x, roots(1), wide(4400)
    type(kernel_spec_t) :: spec
    type(native_engine_t) :: engine
    type(engine_result_t) :: simplified
    integer :: i, unit, status
    call arena%init()
    x = sym(arena, 'x')
    e = x
    do i = 1, 45
        e = sin(e) + cos(e) + e**2/100 + e**3/1000
    end do
    engine = make_native_engine(arena)
    simplified = engine%simplify(e)
    if (.not. simplified%ok) error stop 'shared DAG simplification refused'
    roots(1) = simplified%value
    spec%name = str('deep_shared')
    spec%target = TARGET_FORTRAN_CPU
    allocate (spec%args(1), spec%outputs(1))
    spec%args(1) = str('x')
    spec%outputs(1) = str('y')
    open (newunit=unit, file='cse_dag_fixture.f90', status='replace')
    write (unit, '(a)') chars(emit_kernel(roots, spec))
    do i = 1, size(wide)
        e = sin(x + i)
        wide(i) = e**2 + cos(e)
    end do
    deallocate (spec%outputs)
    allocate (spec%outputs(1), spec%output_shapes(1), &
              spec%output_references(size(wide)))
    spec%name = str('wide_shared')
    spec%outputs(1) = str('y')
    spec%output_shapes(1) = str('(4400)')
    do i = 1, size(wide)
        spec%output_references(i) = str('y('//chars(str(i))//')')
    end do
    write (unit, '(a)') chars(emit_kernel(wide, spec))
    spec%name = str('wide_shared_array')
    spec%temp_prefix = str('dp')
    spec%array_temporaries = .true.
    write (unit, '(a)') chars(emit_kernel(wide, spec))
    block
        type(kernel_spec_t) :: collision
        type(expr_t) :: q, repeated, expression(1)
        type(str_t) :: source
        logical :: ok
        collision = spec
        collision%args(1) = str('dp')
        deallocate (collision%output_references)
        allocate (collision%output_references(1))
        collision%output_references(1) = str('y(1)')
        q = sym(arena, 'dp')
        repeated = sin(q)
        expression(1) = repeated**2 + cos(repeated)
        source = emit_kernel(expression, collision, ok=ok)
        if (ok) error stop 'temporary array collision was accepted'
        collision%temp_prefix = collision%name
        source = emit_kernel(expression, collision, ok=ok)
        if (ok) error stop 'temporary array procedure collision was accepted'
        collision%temp_prefix = str('real32')
        collision%precision = PRECISION_REAL32
        source = emit_kernel(expression, collision, ok=ok)
        if (ok) error stop 'temporary array kind collision was accepted'
    end block
    write (unit, '(a)') 'program check_deep_shared'
    write (unit, '(a)') 'use, intrinsic :: iso_fortran_env, only: dp=>real64'
    write (unit, '(a)') 'implicit none'
    write (unit, '(a)') 'real(dp) :: reference, result, values(4400), phase'
    write (unit, '(a)') 'integer :: i'
    write (unit, '(a)') 'reference=0.37_dp'
    write (unit, '(a)') 'do i=1,45'
    write (unit, '(a)') 'reference=sin(reference)+cos(reference) &'
    write (unit, '(a)') '    +reference**2/100+reference**3/1000'
    write (unit, '(a)') 'end do'
    write (unit, '(a)') 'call deep_shared(0.37_dp,result)'
    write (unit, '(a)') 'if(abs(result-reference)>2.0e-14_dp) error stop 1'
    write (unit, '(a)') 'call wide_shared(0.37_dp,values)'
    write (unit, '(a)') 'do i=1,4400'
    write (unit, '(a)') 'phase=sin(0.37_dp+i)'
    write (unit, '(a)') 'reference=phase**2+cos(phase)'
    write (unit, '(a)') 'if(abs(values(i)-reference)>2.0e-14_dp) error stop 2'
    write (unit, '(a)') 'end do'
    write (unit, '(a)') 'call wide_shared_array(0.37_dp,values)'
    write (unit, '(a)') 'do i=1,4400'
    write (unit, '(a)') 'phase=sin(0.37_dp+i)'
    write (unit, '(a)') 'reference=phase**2+cos(phase)'
    write (unit, '(a)') 'if(abs(values(i)-reference)>2.0e-14_dp) error stop 3'
    write (unit, '(a)') 'end do'
    write (unit, '(a)') 'end program check_deep_shared'
    close (unit)
    call execute_command_line('gfortran -std=f2018 -pedantic-errors '// &
                              '-o cse_dag_fixture cse_dag_fixture.f90', &
                              exitstat=status)
    if (status /= 0) error stop 'DAG fixture did not compile'
    call execute_command_line('./cse_dag_fixture', exitstat=status)
    if (status /= 0) error stop 'DAG fixture disagreed with recurrence'
end program test_fortsym_cse_dag
