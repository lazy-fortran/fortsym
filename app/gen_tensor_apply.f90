program gen_tensor_apply
    !! Minimal consumer generator: a caller-supplied coefficient matrix acts
    !! on reference gradients. Geometry and quadrature weights are already in G.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(+), operator(*)
    use fortsym_kernel_emit, only: tensor_kernel_spec_t, kernel_emit_spec_t, &
        emit_tensor_fortran, emit_tensor_cuda, PRECISION_REAL32, PRECISION_REAL64
    use fortsym_string, only: str_t, str, chars
    implicit none
    type(arena_t), target :: arena
    type(expr_t), allocatable :: x(:), roots(:)
    type(kernel_emit_spec_t) :: leaf
    type(tensor_kernel_spec_t) :: tensor
    type(str_t) :: source
    character(2048) :: path, target, arg
    character(:), allocatable :: message
    integer :: d, p, extra, tables, bits, i, j, k, unit, stat
    logical :: ok

    call get_command_argument(1, target)
    call get_command_argument(2, path)
    call get_command_argument(3, arg)
    read (arg, *, iostat=stat) d
    if (stat /= 0) error stop 'expected dimension 2 or 3'
    call get_command_argument(4, arg)
    read (arg, *, iostat=stat) p
    if (stat /= 0) error stop 'expected degree 1..8'
    call get_command_argument(5, arg)
    read (arg, *, iostat=stat) extra
    if (stat /= 0) error stop 'expected quadrature increment 0 or 1'
    call get_command_argument(6, arg)
    read (arg, *, iostat=stat) tables
    if (stat /= 0) error stop 'expected cell tables 0 or 1'
    call get_command_argument(7, arg)
    read (arg, *, iostat=stat) bits
    if (stat /= 0) error stop 'expected precision 32 or 64'
    if (d < 2 .or. d > 3) error stop 'dimension outside 2..3'
    if (p < 1 .or. p > 8) error stop 'degree outside 1..8'
    if (extra < 0 .or. extra > 1) error stop 'quadrature increment outside 0..1'
    if (tables < 0 .or. tables > 1) error stop 'cell tables outside 0..1'
    if (bits /= 32 .and. bits /= 64) error stop 'precision must be 32 or 64'
    tensor%name = str('tensor_apply')
    tensor%dimension = d
    tensor%nloc = p + 1
    tensor%nq = p + 1 + extra
    tensor%ncoeff = d*d
    tensor%cell_tables = tables == 1
    tensor%precision = PRECISION_REAL64
    if (bits == 32) tensor%precision = PRECISION_REAL32
    allocate (x(d + 1 + d*d), roots(d + 1), leaf%args(d + 1 + d*d), leaf%outputs(d + 1))
    call arena%init()
    do i = 1, size(x)
        leaf%args(i) = str('x'//number(i))
        x(i) = sym(arena, chars(leaf%args(i)))
    end do
    roots(1) = num(arena, 0)
    do i = 1, d
        roots(i + 1) = num(arena, 0)
        do j = 1, d
            ! coef(q,(i-1)*d+j) stores G(i,j), including integration weights.
            k = d + 1 + (i - 1)*d + j
            roots(i + 1) = roots(i + 1) + x(k)*x(j + 1)
        end do
    end do
    do i = 1, d + 1
        leaf%outputs(i) = str('flux'//number(i))
    end do
    leaf%name = str('gradient_flux')
    if (trim(target) == 'fortran') then
        source = emit_tensor_fortran(roots, leaf, tensor, ok, message)
    else if (trim(target) == 'cuda') then
        source = emit_tensor_cuda(roots, leaf, tensor, ok, message)
    else
        error stop 'target must be fortran or cuda'
    end if
    if (.not. ok) then
        print *, message
        error stop 'tensor generation failed'
    end if
    open (newunit=unit, file=trim(path), status='replace', iostat=stat)
    if (stat /= 0) error stop 'cannot open generated source'
    write (unit, '(a)') chars(source)
    close (unit)
contains
    function number(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(32) :: tmp
        write (tmp, '(i0)') i
        s = trim(tmp)
    end function
end program gen_tensor_apply
