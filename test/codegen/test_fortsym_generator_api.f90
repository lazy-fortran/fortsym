program test_fortsym_generator_api
    ! Scoped context, derivative sets, jet names, declarative bindings and
    ! multi-target emission, checked against hand-written expectations.
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, func, is_valid, same_arena, &
        operator(*), operator(/), operator(+), operator(**)
    use fortsym_context, only: context_t, current_arena
    use fortsym_string, only: str_t, str, chars, itoa, indexed, words
    use fortsym_chain, only: chain_t
    use fortsym_kernel, only: kernel_spec_t
    use fortsym_kernel_emit, only: kernel_emit_spec_t, TARGET_FORTRAN_CPU, &
        TARGET_CUDA
    use fortsym_jet, only: derivative_set_t, partials, radial_mixed, &
        multi_indices, jet_names_t, members, suffixed, listed, bind, jet
    use fortsym_emit, only: kernel_t, emit
    implicit none

    integer :: nfail = 0

    call test_context()
    call test_sets()
    call test_names()
    call test_fortran_jet_kernel()
    call test_targets()
    call test_strings()
    if (nfail > 0) then
        print '(i0,a)', nfail, ' generator API checks FAILED'
        error stop 1
    end if
    print '(a)', 'PASS generator API'

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition
        if (.not. condition) then
            print '(a)', 'FAIL '//label
            nfail = nfail + 1
        end if
    end subroutine check

    subroutine test_context()
        type(context_t) :: outer, inner
        type(arena_t), pointer :: base
        type(expr_t) :: x, y, z, stale

        base => current_arena()
        x = sym('x')
        call check('arena-free symbols use the base context', &
            associated(x%a, base))
        call outer%open()
        y = sym('x')
        call check('an open context has its own arena', .not. same_arena(x, y))
        call inner%open()
        stale = sym('t')
        call inner%close()
        call check('closing a context invalidates its handles', .not. is_valid(stale))
        z = sym('x')
        call check('closing returns to the enclosing context', same_arena(y, z))
        call check('interning is shared within a context', z%id == y%id)
        call outer%close()
        call check('the base context survives nested contexts', is_valid(x))
        x = func('mu')
        call check('func with no arguments uses the context', associated(x%a, base))
    end subroutine test_context

    subroutine test_sets()
        type(derivative_set_t) :: s

        s = partials(3, 2)
        call check('partials(3, 2) has ten members', s%count() == 10)
        call check('partials are in canonical order', &
            all(s%orders(:, 2) == [1, 0, 0]) .and. all(s%orders(:, 4) == [0, 0, 1]) &
            .and. all(s%orders(:, 6) == [1, 1, 0]) .and. all(s%orders(:, 10) == [0, 0, 2]))
        s = radial_mixed(3)
        call check('radial_mixed(3) is the libneo rmix table', s%count() == 7 .and. &
            all(reshape(s%orders, [21]) == [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, &
            2, 0, 0, 1, 1, 0, 1, 0, 1]))
        s = partials(4, 2)
        s = s%of_order(2)
        s = s%touching([1, 4])
        s = s%without([0, 0, 0, 2])
        call check('second partials touching r or p_phi', s%count() == 6)
        s = multi_indices(reshape([0, 1, 1, 0], [2, 2]))
        call check('explicit sets are canonicalised', &
            all(s%orders(:, 1) == [1, 0]) .and. s%valid())
        s = multi_indices(reshape([1, 0, 1, 0], [2, 2]))
        call check('duplicates are invalid', .not. s%valid())
    end subroutine test_sets

    subroutine test_names()
        type(jet_names_t) :: n
        integer :: packing(4, 4)

        n = members('f%B', 'f%dB', 'f%d2B')
        call check('member value', n%of([0, 0, 0]) == 'f%B')
        call check('member gradient', n%of([0, 1, 0]) == 'f%dB(2)')
        call check('member Hessian, upper triangle by rows', n%of([0, 1, 1]) == 'f%d2B(5)')
        call check('member third order is unnamed', n%of([3, 0, 0]) == '')
        packing = reshape([1, 2, 3, 7, 2, 4, 5, 8, 3, 5, 6, 9, 7, 8, 9, 10], [4, 4])
        n = members('f%H', 'f%dH', 'f%d2H', packing)
        call check('explicit packing', n%of([1, 0, 0, 1]) == 'f%d2H(7)')
        n = suffixed('r', ['r', 't', 'p'])
        call check('suffixed partial', n%of([1, 0, 1]) == 'r_rp')
        n = suffixed('p', ['i', 'j'], separator='')
        call check('suffixed without separator', n%of([1, 1]) == 'pij')
        n = listed([character(5) :: 'e1', 'de1', 'd2e1'])
        call check('listed names follow canonical order', &
            n%of([1]) == 'de1' .and. n%of([2]) == 'd2e1')
        call check('listed names end', n%of([3]) == '')
    end subroutine test_names

    !> v = p/h(r) with v named in a chain; the kernel assigns v, dv/dr and
    !> d2v/dr2 to members of s and reads h through its bound jet.
    subroutine test_fortran_jet_kernel()
        type(context_t) :: ctx
        type(chain_t) :: c
        type(kernel_t) :: k
        type(kernel_spec_t) :: spec
        type(expr_t) :: r, p, h, v
        character(:), allocatable :: source

        call ctx%open()
        r = sym('r')
        p = sym('p')
        h = func('h', [r])
        call bind(h, listed([character(5) :: 's%h', 's%dh', 's%d2h']))
        call c%init([r, p])
        v = c%let('v', p/h)
        call k%jet([v*v], [members('s%e', 's%de', 's%d2e')], partials(2, 2), chain=c)
        spec%name = str('energy_jet')
        spec%args = [str('p'), str('s')]
        spec%arg_types = [str('real(dp)'), str('type(state_t)')]
        spec%arg_intents = [str('in'), str('inout')]
        allocate (spec%outputs(0))
        source = chars(emit(k, spec, chain=c))
        call check('jet outputs assign the named members', &
            index(source, 's%e =') > 0 .and. index(source, 's%de(1) =') > 0 .and. &
            index(source, 's%d2e(3) =') > 0)
        call check('bound jet inputs are read by name', &
            index(source, 's%dh') > 0 .and. index(source, 's%d2h') > 0)
        call check('the chain intermediate is a local', index(source, 'v =') > 0)
        call ctx%close()
    end subroutine test_fortran_jet_kernel

    !> One definition for Fortran and CUDA; the bound spline value becomes
    !> the scalar argument b.
    subroutine test_targets()
        type(context_t) :: ctx
        type(kernel_t) :: k
        type(kernel_emit_spec_t) :: spec
        type(str_t), allocatable :: sources(:)
        type(expr_t) :: x, b

        call ctx%open()
        x = sym('x')
        b = func('B', [x])
        call bind(b, listed([character(2) :: 'b', 'db']))
        call k%assign('y', x*b**2 + num(3))
        call k%assign('dy', jet_first(x*b**2, x))
        spec%name = str('two_targets')
        spec%args = words('x b db')
        spec%outputs = words('y dy')
        sources = emit(k, spec, [TARGET_FORTRAN_CPU, TARGET_CUDA])
        call check('one source per target', size(sources) == 2)
        call check('Fortran target', index(chars(sources(1)), 'subroutine two_targets') > 0)
        call check('CUDA target', index(chars(sources(2)), '__device__') > 0)
        call check('bound functions are symbols on the IR path', &
            index(chars(sources(1)), 'B(') == 0 .and. index(chars(sources(1)), 'db') > 0)
        call ctx%close()
    end subroutine test_targets

    function jet_first(e, x) result(d)
        type(expr_t), intent(in) :: e, x
        type(expr_t) :: d
        type(expr_t), allocatable :: all(:)

        all = jet(e, partials(1, 1), [x])
        d = all(2)
    end function jet_first

    subroutine test_strings()
        type(str_t), allocatable :: w(:)
        type(expr_t) :: a12

        call check('itoa', itoa(-12) == '-12')
        call check('indexed', indexed('jac', 1, 2) == 'jac(1, 2)' .and. &
            indexed('x', 3) == 'x(3)')
        w = words('  r th  ph ')
        call check('words', size(w) == 3 .and. chars(w(3)) == 'ph')
        a12 = sym('a', 1, 2)
        call check('indexed symbols', chars(a12%name()) == 'a(1, 2)')
    end subroutine test_strings

end program test_fortsym_generator_api
