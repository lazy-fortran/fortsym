program generate_precision_family
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(*), operator(**)
    use fortsym_diff, only: diff
    use fortsym_string, only: str_t, str, chars
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_cpu_precision_kernels
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, f, roots(2)
    type(kernel_emit_spec_t) :: spec
    type(str_t) :: source
    character(:), allocatable :: message, text
    character(1024) :: path
    integer :: unit
    logical :: ok
    if (command_argument_count() == 0) stop
    call get_command_argument(1,path)
    call arena%init()
    x = sym(arena,'x')
    f = (x+1)**2
    roots = [f,diff(f,x)]
    spec%name = str('symbolic_polynomial')
    spec%args = [str('x')]
    spec%outputs = [str('y'),str('dy')]
    spec%generator = str('test/codegen/generate_precision_family.f90')
    source = emit_cpu_precision_kernels(roots,spec,ok,message)
    if (.not. ok) then
        print *, message
        error stop 1
    end if
    open(newunit=unit,file=trim(path),status='replace')
    text = chars(source)
    do while (len(text) > 0)
        if (text(len(text):) /= new_line('a')) exit
        text = text(:len(text)-1)
    end do
    write(unit,'(a)') text
    close(unit)
end program
