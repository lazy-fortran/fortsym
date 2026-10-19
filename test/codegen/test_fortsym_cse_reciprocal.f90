program test_fortsym_cse_reciprocal
    ! A denominator shared by several roots is named once. The printer emits a
    ! reciprocal power as a division by its base, and full CSE never names
    ! the reciprocal itself, so the base must be named when the reciprocal is
    ! shared; otherwise the denominator is recomputed at every use.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, operator(+), operator(*), operator(/)
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    use fortsym_string, only: str, chars
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: x, y, d, roots(2)
    type(kernel_spec_t) :: spec
    character(:), allocatable :: source

    call arena%init()
    x = sym(arena, 'x')
    y = sym(arena, 'y')
    d = x*y + x + y
    roots = [x/d, y/d]
    spec%name = str('shared_denominator')
    allocate (spec%args(2), spec%outputs(2))
    spec%args(1) = str('x')
    spec%args(2) = str('y')
    spec%outputs(1) = str('a')
    spec%outputs(2) = str('b')
    source = chars(emit_kernel(roots, spec))
    if (occurrences(source, "x*y") /= 1) then
        print '(a)', source
        error stop 'shared denominator was not named once'
    end if
    print '(a)', 'PASS: shared reciprocal denominator named once'

contains

    integer function occurrences(text, pattern)
        character(*), intent(in) :: text, pattern
        integer :: start, hit

        occurrences = 0
        start = 1
        do
            hit = index(text(start:), pattern)
            if (hit == 0) return
            occurrences = occurrences + 1
            start = start + hit + len(pattern) - 1
        end do
    end function occurrences

end program test_fortsym_cse_reciprocal
