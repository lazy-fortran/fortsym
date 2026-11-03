program test_fortsym_chain
    ! Derivatives propagated through named intermediates must equal direct
    ! symbolic derivatives of the expanded expressions, keep their
    ! intermediates opaque, and compile into a kernel that updates a derived
    ! type in place.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str, str_t, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_POW, NK_ADD, NK_SYM
    use fortsym_expr, only: expr_t, sym, num, func, operator(+), operator(-), &
        operator(*), operator(/), operator(**), sin, cos
    use fortsym_diff, only: diff, partial_derivative
    use fortsym_eval, only: binding_t, eval_expr
    use fortsym_chain, only: chain_t, fold_numbers
    use fortsym_kernel, only: kernel_spec_t, kernel_binding_t, emit_kernel, &
        count_operations, operation_count_t
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_derivatives_match_direct()
    call test_quotient_reuses_its_value()
    call test_exact_zero_is_proved()
    call test_folding_keeps_differences()
    call test_chain_costs_less_than_expansion()
    call test_locals_and_inout_kernel()

    if (nfail /= 0) then
        print *, "test_fortsym_chain:", nfail, "checks FAILED"
        error stop 1
    end if
    print *, "test_fortsym_chain: all checks passed"

contains

    subroutine check(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        if (.not. condition) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine check

    !> v = (p - A)/h, H = v**2/2 + x*y*v, with concrete A and h. Every first
    !> and second chain derivative of H and v, expanded, must evaluate to the
    !> direct derivative of the expanded expression.
    subroutine test_derivatives_match_direct()
        type(arena_t), target :: arena
        type(chain_t) :: c
        type(expr_t) :: x, y, p, vars(3), a, h, v, hamiltonian, direct_v, direct_h
        type(binding_t) :: values
        real(dp) :: point(3, 3)
        integer :: i, j, k

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        p = sym(arena, "p")
        vars = [x, y, p]
        a = sin(x*y) + x**2
        h = 2 + cos(x) + y**2
        call c%init(vars)
        v = c%let("v", (p - a)/h)
        hamiltonian = v**2/2 + x*y*v
        direct_v = (p - a)/h
        direct_h = direct_v**2/2 + x*y*direct_v
        point = reshape([0.3_dp, -0.7_dp, 1.9_dp, 1.1_dp, 0.4_dp, -0.2_dp, &
            -0.6_dp, 2.3_dp, 0.05_dp], [3, 3])

        values%n = 3
        values%names = [str("x"), str("y"), str("p")]
        do k = 1, 3
            values%values = point(:, k)
            do i = 1, 3
                call compare(c, values, c%d(v, i), diff(direct_v, vars(i)), &
                    "first derivative of v")
                call compare(c, values, c%d(hamiltonian, i), diff(direct_h, vars(i)), &
                    "first derivative of H")
                do j = 1, 3
                    call compare(c, values, c%d(c%d(v, i), j), &
                        diff(diff(direct_v, vars(i)), vars(j)), "second derivative of v")
                    call compare(c, values, c%d(c%d(hamiltonian, i), j), &
                        diff(diff(direct_h, vars(i)), vars(j)), "second derivative of H")
                end do
            end do
        end do
    end subroutine test_derivatives_match_direct

    subroutine compare(c, values, chain_form, direct_form, label)
        type(chain_t), intent(in) :: c
        type(binding_t), intent(in) :: values
        type(expr_t), intent(in) :: chain_form, direct_form
        character(*), intent(in) :: label
        real(dp) :: got, want
        logical :: defined_got, defined_want

        got = eval_expr(c%expand(chain_form), values, defined_got)
        want = eval_expr(direct_form, values, defined_want)
        call check(label//" is defined", defined_got .and. defined_want)
        call check(label//" matches the direct derivative", &
            abs(got - want) <= 1.0e-12_dp*max(1.0_dp, abs(want)))
    end subroutine compare

    !> D v of v = n/b reads v itself, (D n - v D b)/b, and the second
    !> derivative reads the first one by name.
    subroutine test_quotient_reuses_its_value()
        type(arena_t), target :: arena
        type(chain_t) :: c
        type(expr_t) :: x, y, a, b, v, dv, d2v, other
        integer :: k

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        a = func("A", [x, y])
        b = func("B", [x, y])
        call c%init([x, y])
        v = c%let("v", (y - a)/b)
        dv = c%d(v, 1)
        d2v = c%d(dv, 2)
        call check("first derivative is a named intermediate", dv%kind() == NK_SYM)
        call check("second derivative is a named intermediate", d2v%kind() == NK_SYM)
        k = c%index_of(dv)
        call check("first derivative reads v", reads(c%defs(k), v))
        k = c%index_of(d2v)
        call check("mixed derivative reads the first derivative", reads(c%defs(k), dv))
        other = c%d(c%d(v, 2), 1)
        call check("mixed partials share one name", other%id == d2v%id)
        call check("the needed list is in evaluation order", &
            all(c%needed([d2v]) == [1, 2, 3, 4]))
    end subroutine test_quotient_reuses_its_value

    !> p = g*w with g = b(x)/m(x, y) and w = m(x, y)*q(x) depends on x only,
    !> although each factor depends on y; its y derivative must be exact zero.
    subroutine test_exact_zero_is_proved()
        type(arena_t), target :: arena
        type(chain_t) :: c, unproved
        type(expr_t) :: x, y, m, g, w, product, dy

        call arena%init()
        x = sym(arena, "x")
        y = sym(arena, "y")
        m = func("m", [x, y])
        call c%init([x, y])
        g = c%let("g", func("b", [x])/m)
        w = c%let("w", m*func("q", [x]))
        product = c%let("product", g*w)
        dy = c%d(product, 2)
        call check("cancelling derivative is exact zero", is_zero(dy))

        call unproved%init([x, y])
        unproved%prove_zeros = .false.
        g = unproved%let("g", func("b", [x])/m)
        w = unproved%let("w", m*func("q", [x]))
        product = unproved%let("product", g*w)
        dy = unproved%d(product, 2)
        call check("without proofs the derivative stays symbolic", .not. is_zero(dy))
    end subroutine test_exact_zero_is_proved

    !> Folding merges numbers and powers but never expands a power of a
    !> difference, which would cancel when p is close to a/r.
    subroutine test_folding_keeps_differences()
        type(arena_t), target :: arena
        type(expr_t) :: p, a, r, x, folded, base
        type(operation_count_t) :: counts

        call arena%init()
        p = sym(arena, "p")
        a = sym(arena, "a")
        r = sym(arena, "r")
        x = sym(arena, "x")
        folded = fold_numbers(2*((p - a/r)**2/2))
        call check("numbers fold to a unit factor", folded%kind() == NK_POW)
        base = folded%arg(1)
        call check("the difference is kept", base%kind() == NK_ADD)
        folded = fold_numbers(x*r*(1/x)*1)
        call check("x/x cancels exactly", folded%id == r%id)
        folded = fold_numbers(x*r + 2*x*r - 3*(x*r))
        call check("like terms cancel to zero", is_zero(folded))
        folded = fold_numbers(1/(x/r))
        counts = count_operations([folded])
        call check("reciprocal of a quotient is one quotient", counts%divisions == 1)
    end subroutine test_folding_keeps_differences

    !> The Hessian of H = v**2/2 + mu*B with v = (p - A)/h through the chain
    !> must cost fewer operations than direct differentiation of the expanded
    !> H, both counted after CSE.
    subroutine test_chain_costs_less_than_expansion()
        type(arena_t), target :: arena
        type(chain_t) :: c
        type(expr_t) :: r, t, p, vars(3), a, h, b, mu, v, hamiltonian, direct
        type(expr_t), allocatable :: roots(:), expanded(:)
        type(operation_count_t) :: chain_count, direct_count
        integer, allocatable :: needed(:)
        integer :: i, j

        call arena%init()
        r = sym(arena, "r")
        t = sym(arena, "t")
        p = sym(arena, "p")
        mu = sym(arena, "mu")
        vars = [r, t, p]
        a = func("A", [r, t])
        h = func("h", [r, t])
        b = func("B", [r, t])
        call c%init(vars)
        v = c%let("v", (p - a)/h)
        hamiltonian = v**2/2 + mu*b
        direct = ((p - a)/h)**2/2 + mu*b
        allocate (roots(0), expanded(0))
        do i = 1, 3
            do j = i, 3
                roots = [roots, c%d(c%d(hamiltonian, i), j)]
                expanded = [expanded, diff(diff(direct, vars(i)), vars(j))]
            end do
        end do
        needed = c%needed(roots)
        chain_count = count_operations([c%defs(needed), roots])
        direct_count = count_operations(expanded)
        call check("chain Hessian needs fewer FLOPs", chain_count%flops < direct_count%flops)
        call check("chain Hessian divides once", chain_count%divisions <= 1)
    end subroutine test_chain_costs_less_than_expansion

    !> A kernel with locals assigns the field members of an inout derived
    !> type. The compiled kernel is compared with closed-form values.
    subroutine test_locals_and_inout_kernel()
        type(arena_t), target :: arena
        type(chain_t) :: c
        type(expr_t) :: r, p, v, hamiltonian, roots(4)
        type(kernel_spec_t) :: spec
        type(kernel_binding_t) :: bindings(6)
        type(str_t) :: source, refused
        integer, allocatable :: needed(:)
        character(:), allocatable :: message
        character(4096) :: compiler
        logical :: good
        integer :: unit, stat, k

        call arena%init()
        r = sym(arena, "r")
        p = sym(arena, "p")
        call c%init([r, p])
        v = c%let("v", (p - func("A", [r]))/func("h", [r]))
        hamiltonian = v**2/2
        roots = [v, c%d(v, 1), c%d(hamiltonian, 1), c%d(c%d(hamiltonian, 1), 1)]
        bindings(1) = binding("A", [integer ::], "s%A")
        bindings(2) = binding("A", [1], "s%dA")
        bindings(3) = binding("A", [1, 1], "s%d2A")
        bindings(4) = binding("h", [integer ::], "s%h")
        bindings(5) = binding("h", [1], "s%dh")
        bindings(6) = binding("h", [1, 1], "s%d2h")
        needed = c%needed(roots)

        spec%name = str("field_update")
        spec%args = [str("p"), str("s")]
        spec%arg_types = [str("real(dp)"), str("type(field_t)")]
        spec%arg_intents = [str("in"), str("inout")]
        allocate (spec%outputs(0))
        spec%locals = [(c%names(needed(k))%name(), k=1, size(needed))]
        spec%output_references = [spec%locals, str("s%v"), str("s%dv"), &
            str("s%dE"), str("s%d2E")]
        spec%bindings = bindings
        spec%temp_prefix = str("t")
        source = emit_kernel([c%defs(needed), roots], spec, good, message=message)
        call check("kernel with locals emits", good)
        if (.not. good) print *, message
        call check("the derived type is updated in place", &
            index(chars(source), "type(field_t), intent(inout) :: s") > 0)

        ! A local read before the root that assigns it is refused.
        refused = emit_kernel([roots(2), c%defs(needed)], reorder(spec), good, &
            message=message)
        call check("a local read before assignment is refused", .not. good)

        open (newunit=unit, file="chain_fixture.f90", status="replace")
        write (unit, '(a)') "module chain_fixture"
        write (unit, '(a)') "    use, intrinsic :: iso_fortran_env, only: dp => real64"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "    type :: field_t"
        write (unit, '(a)') "        real(dp) :: A, dA, d2A, h, dh, d2h, v, dv, dE, d2E"
        write (unit, '(a)') "    end type field_t"
        write (unit, '(a)') "contains"
        write (unit, '(a)') chars(source)
        write (unit, '(a)') "end module chain_fixture"
        write (unit, '(a)') "program driver"
        write (unit, '(a)') "    use chain_fixture"
        write (unit, '(a)') "    implicit none"
        write (unit, '(a)') "    type(field_t) :: s"
        write (unit, '(a)') "    real(dp) :: p, v, dv, d2v, err"
        write (unit, '(a)') "    p = 0.7_dp"
        write (unit, '(a)') "    s%A = 0.3_dp; s%dA = -1.2_dp; s%d2A = 0.5_dp"
        write (unit, '(a)') "    s%h = 1.6_dp; s%dh = 0.4_dp; s%d2h = -0.9_dp"
        write (unit, '(a)') "    call field_update(p, s)"
        ! Implicit differentiation of h*v = p - A, written independently.
        write (unit, '(a)') "    v = (p - s%A)/s%h"
        write (unit, '(a)') "    dv = (-s%dA - v*s%dh)/s%h"
        write (unit, '(a)') "    d2v = (-s%d2A - 2*dv*s%dh - v*s%d2h)/s%h"
        write (unit, '(a)') "    err = abs(s%v - v) + abs(s%dv - dv) + abs(s%dE - v*dv) + &"
        write (unit, '(a)') "        abs(s%d2E - (dv*dv + v*d2v))"
        write (unit, '(a)') "    if (err > 1.0e-13_dp) error stop 1"
        write (unit, '(a)') "end program driver"
        close (unit)
        call fixture_compiler(compiler)
        call execute_command_line(trim(compiler)// &
            " -o chain_fixture chain_fixture.f90 > chain_fixture.log 2>&1", &
            wait=.true., exitstat=stat)
        call check("kernel with locals compiles", stat == 0)
        if (stat /= 0) return
        call execute_command_line("./chain_fixture >> chain_fixture.log 2>&1", &
            wait=.true., exitstat=stat)
        call check("compiled kernel matches implicit differentiation", stat == 0)
    end subroutine test_locals_and_inout_kernel

    function reorder(spec) result(swapped)
        type(kernel_spec_t), intent(in) :: spec
        type(kernel_spec_t) :: swapped

        swapped = spec
        swapped%output_references = [str("s%dv"), spec%locals]
    end function reorder

    function binding(head, indices, replacement) result(b)
        character(*), intent(in) :: head, replacement
        integer, intent(in) :: indices(:)
        type(kernel_binding_t) :: b

        b%head = str(head)
        if (size(indices) > 0) b%derivative_indices = indices
        b%replacement = str(replacement)
    end function binding

    recursive logical function reads(node, s) result(found)
        type(expr_t), intent(in) :: node, s
        integer :: k

        found = node%id == s%id
        do k = 1, node%nargs()
            if (found) return
            found = reads(node%arg(k), s)
        end do
    end function reads

    logical function is_zero(e)
        type(expr_t), intent(in) :: e

        is_zero = .false.
        if (e%kind() == NK_INT) is_zero = e%int_value() == 0
    end function is_zero

    subroutine fixture_compiler(compiler)
        character(*), intent(out) :: compiler
        integer :: stat

        call get_environment_variable("FORTSYM_TEST_COMPILER", compiler, status=stat)
        if (stat == 0) then
            if (len_trim(compiler) > 0) return
        end if
        call get_environment_variable("FC", compiler, status=stat)
        if (stat == 0) then
            if (len_trim(compiler) > 0) return
        end if
        compiler = "gfortran"
    end subroutine fixture_compiler

end program test_fortsym_chain
