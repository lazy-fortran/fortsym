program test_fortsym_cse_dag
    ! Full CSE must traverse the DAG, not its exponentially expanded tree.
    ! Compiler execution against a numerical recurrence is the value oracle.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, sin, cos, operator(+)
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, TARGET_FORTRAN_CPU
    use fortsym_string, only: str, chars
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: e, x, roots(1)
    type(kernel_spec_t) :: spec
    integer :: i, unit, status
    call arena%init()
    x = sym(arena, 'x')
    e = x
    do i = 1, 45
        e = sin(e)+cos(e)
    end do
    roots(1) = e
    spec%name = str('deep_shared')
    spec%target = TARGET_FORTRAN_CPU
    allocate(spec%args(1),spec%outputs(1))
    spec%args(1) = str('x')
    spec%outputs(1) = str('y')
    open(newunit=unit,file='cse_dag_fixture.f90',status='replace')
    write(unit,'(a)') chars(emit_kernel(roots,spec))
    write(unit,'(a)') 'program check_deep_shared'
    write(unit,'(a)') 'use, intrinsic :: iso_fortran_env, only: dp=>real64'
    write(unit,'(a)') 'implicit none'
    write(unit,'(a)') 'real(dp) :: reference, result'
    write(unit,'(a)') 'integer :: i'
    write(unit,'(a)') 'reference=0.37_dp'
    write(unit,'(a)') 'do i=1,45'
    write(unit,'(a)') 'reference=sin(reference)+cos(reference)'
    write(unit,'(a)') 'end do'
    write(unit,'(a)') 'call deep_shared(0.37_dp,result)'
    write(unit,'(a)') 'if(abs(result-reference)>2.0e-14_dp) error stop 1'
    write(unit,'(a)') 'end program check_deep_shared'
    close(unit)
    call execute_command_line('gfortran -o cse_dag_fixture cse_dag_fixture.f90', &
        exitstat=status)
    if (status /= 0) error stop 'DAG fixture did not compile'
    call execute_command_line('./cse_dag_fixture',exitstat=status)
    if (status /= 0) error stop 'DAG fixture disagreed with recurrence'
end program test_fortsym_cse_dag
