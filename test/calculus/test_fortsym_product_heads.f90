program test_fortsym_product_heads
    ! Contracted products through abs and through heads with several
    ! arguments. Every check is a decided identity; numeric probe evidence is
    ! not accepted. The oracles are the chain rule written out by hand, the
    ! contraction of `diff`, and the forward/reverse duality
    !
    !     u . (J w) == (J^T u) . w
    use fortsym_string, only: chars
    use fortsym_arena, only: arena_t, NK_FUNC
    use fortsym_expr, only: expr_t, sym, num, func, partial, besselj, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), sin, cos, abs
    use fortsym_relation, only: less
    use fortsym_diff, only: diff
    use fortsym_assume, only: assumption_context_t, assume, positive, negative
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
    use fortsym_engine, only: engine_result_t
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_products, only: jvp, vjp, hvp, gradient
    implicit none

    type(arena_t), target :: arena
    type(assumption_context_t), target :: signs
    type(native_engine_t) :: eng, signed_eng
    type(suite_t) :: s
    type(expr_t) :: p, n, q, x, y, w(3), u(3)

    call arena%init()
    p = sym(arena, "p")
    n = sym(arena, "n")
    q = sym(arena, "q")
    x = sym(arena, "x")
    y = sym(arena, "y")
    w = [sym(arena, "w1"), sym(arena, "w2"), sym(arena, "w3")]
    u = [sym(arena, "u1"), sym(arena, "u2"), sym(arena, "u3")]
    call signs%init(arena)
    call assume(signs, positive(p))
    call assume(signs, negative(n))
    eng = make_native_engine(arena)
    signed_eng = make_native_engine(arena, signs)

    call suite_begin(s, "derivative products through abs and multivariate heads")
    call test_abs_signs()
    call test_abs_unknown_sign()
    call test_two_argument_head()
    call test_derivative_heads()
    call test_special_heads()
    call test_piecewise()
    call test_piecewise_second_order()
    call test_piecewise_simplified_shape()
    call test_container_heads()
    if (s%failed /= 0) then
        print *, "test_fortsym_product_heads: ", s%failed, " check(s) FAILED"
        error stop 1
    end if
    call suite_end(s, "/tmp/fortsym_product_heads.json")
    print *, "test_fortsym_product_heads: all checks passed"

contains

    !> With p > 0 and n < 0 the outputs are p*q**2 and -n*q + p, whose
    !> products are written out here by hand.
    subroutine test_abs_signs()
        type(expr_t) :: f(2), v(3), forward(2), reverse(3), second(3), phi

        v = [p, n, q]
        f(1) = abs(p)*q**2
        f(2) = abs(n)*q + abs(p)
        forward = jvp(f, v, w)
        call check_identity(s, signed_eng, "abs JVP, positive argument", &
            forward(1) - (q**2*w(1) + 2*p*q*w(3)))
        call check_identity(s, signed_eng, "abs JVP, negative argument", &
            forward(2) - (w(1) - q*w(2) - n*w(3)))
        reverse = vjp(f, v, u(:2))
        call check_identity(s, signed_eng, "abs VJP, positive argument", &
            reverse(1) - (q**2*u(1) + u(2)))
        call check_identity(s, signed_eng, "abs VJP, negative argument", &
            reverse(2) + q*u(2))
        call check_identity(s, signed_eng, "abs VJP, free argument", &
            reverse(3) - (2*p*q*u(1) - n*u(2)))
        call check_duality("abs duality under sign assumptions", signed_eng, &
            f, v, 2)
        phi = abs(p)*q**2 + abs(n)*p
        second = hvp(phi, v, w)
        call check_identity(s, signed_eng, "abs HVP, row p", &
            second(1) - (2*q*w(3) - w(2)))
        call check_identity(s, signed_eng, "abs HVP, row n", second(2) + w(1))
        call check_identity(s, signed_eng, "abs HVP, row q", &
            second(3) - (2*q*w(1) + 2*p*w(3)))
    end subroutine test_abs_signs

    !> No sign is known for x*y - 1: the products keep abs and must still
    !> equal the contraction of `diff` and satisfy the duality.
    subroutine test_abs_unknown_sign()
        type(expr_t) :: f(2), v(2)

        v = [x, y]
        f(1) = abs(x*y - 1)*x
        f(2) = sin(abs(x) + y**2)
        call check_against_diff("abs of unknown sign", eng, f, v, 2)
        call check_duality("abs duality without assumptions", eng, f, v, 2)
    end subroutine test_abs_unknown_sign

    subroutine test_two_argument_head()
        type(expr_t) :: a, b, g, g1, g2, f(2), v(2), forward(2), reverse(2)
        type(expr_t) :: one(1), single(1)

        v = [x, y]
        a = x*y
        b = x + y**2
        g = func("g", [a, b])
        g1 = func("Derivative1", [sym(arena, "g"), num(arena, 1), a, b])
        g2 = func("Derivative1", [sym(arena, "g"), num(arena, 2), a, b])
        f(1) = g*x
        f(2) = sin(g) + y
        forward = jvp(f, v, w(:2))
        ! Chain rule by hand: da = y w1 + x w2, db = w1 + 2 y w2.
        call check_identity(s, eng, "two-argument head JVP, product", &
            forward(1) - (g*w(1) + x*(g1*(y*w(1) + x*w(2)) + &
            g2*(w(1) + 2*y*w(2)))))
        call check_identity(s, eng, "two-argument head JVP, composition", &
            forward(2) - (cos(g)*(g1*(y*w(1) + x*w(2)) + &
            g2*(w(1) + 2*y*w(2))) + w(2)))
        reverse = vjp(f, v, u(:2))
        call check_identity(s, eng, "two-argument head VJP, x", &
            reverse(1) - (u(1)*(g + x*(g1*y + g2)) + u(2)*cos(g)*(g1*y + g2)))
        call check_identity(s, eng, "two-argument head VJP, y", &
            reverse(2) - (u(1)*x*(g1*x + 2*g2*y) + &
            u(2)*(cos(g)*(g1*x + 2*g2*y) + 1)))
        call check_against_diff("two-argument head", eng, f, v, 2)
        call check_duality("two-argument head duality", eng, f, v, 2)

        ! Only the second argument is active: its partial must not be lost.
        one(1) = num(arena, 1)
        single(1) = func("h", [x, y])
        forward(:1) = jvp(single, [y], one)
        call check_identity(s, eng, "second argument alone, JVP", forward(1) - &
            func("Derivative1", [sym(arena, "h"), num(arena, 2), x, y]))
        reverse(:1) = vjp(single, [y], one)
        call check_identity(s, eng, "second argument alone, VJP", reverse(1) - &
            func("Derivative1", [sym(arena, "h"), num(arena, 2), x, y]))
    end subroutine test_two_argument_head

    !> Second order goes through DerivativeN heads, whose leading arguments
    !> are the head and its indices and carry no tangent.
    subroutine test_derivative_heads()
        type(expr_t) :: phi, v(2), g(2), forward(2), second(2)

        v = [x, y]
        phi = func("g", [x*y, x + y**2])*y
        g = gradient(phi, v)
        forward = jvp(g, v, w(:2))
        second = hvp(phi, v, w(:2))
        call check_identity(s, eng, "JVP of the gradient is the HVP, x", &
            forward(1) - second(1))
        call check_identity(s, eng, "JVP of the gradient is the HVP, y", &
            forward(2) - second(2))
        call check_duality("derivative head duality", eng, g, v, 2)
    end subroutine test_derivative_heads

    !> The order of a Bessel function is its first argument. DLMF 10.6.1:
    !> J_1'(z) = (J_0(z) - J_2(z))/2.
    subroutine test_special_heads()
        type(expr_t) :: f(1), v(2), forward(1), reverse(2), z

        v = [x, y]
        z = x*y
        f(1) = besselj(1, z)
        forward = jvp(f, v, w(:2))
        call check_identity(s, eng, "Bessel JVP", forward(1) - &
            (besselj(0, z) - besselj(2, z))*(y*w(1) + x*w(2))/2)
        reverse = vjp(f, v, u(:1))
        call check_identity(s, eng, "Bessel VJP, x", reverse(1) - &
            u(1)*(besselj(0, z) - besselj(2, z))*y/2)
        call check_identity(s, eng, "Bessel VJP, y", reverse(2) - &
            u(1)*(besselj(0, z) - besselj(2, z))*x/2)
    end subroutine test_special_heads

    !> Branch values are differentiated, conditions are kept.
    subroutine test_piecewise()
        type(expr_t) :: f(2), v(2), branch, pieces

        v = [x, y]
        branch = func("List", [x**2*y, less(x, y)])
        pieces = func("List", [branch])
        f(1) = func("Piecewise", [pieces, x + y**3])*y
        f(2) = sin(f(1)) + x
        call check_against_diff("Piecewise", eng, f, v, 2)
        call check_duality("Piecewise duality", eng, f, v, 2)
    end subroutine test_piecewise

    !> Oracles written by hand, independent of `diff`: the derivative of a
    !> Piecewise must keep the {branch list, default} shape, so that a second
    !> derivative differentiates the branch values again.
    subroutine test_piecewise_second_order()
        type(expr_t) :: f(1), v(1), one(1), zero, first(1), second(1)
        type(expr_t) :: back(1), closed(1), open_condition

        v = [x]
        one = [num(arena, 1)]
        zero = num(arena, 0)
        open_condition = less(x, zero)
        f(1) = guarded(x**3, open_condition, x**2)

        call check_shape("first diff of a Piecewise", diff(f(1), x), &
            [3*x**2], [open_condition], 2*x)
        call check_shape("second diff of a Piecewise", &
            diff(diff(f(1), x), x), [6*x], [open_condition], 2*one(1))

        first = jvp(f, v, one)
        call check_shape("Piecewise JVP", first(1), [3*x**2], &
            [open_condition], 2*x)
        second = jvp(first, v, one)
        call check_shape("Piecewise JVP of JVP", second(1), [6*x], &
            [open_condition], 2*one(1))
        back = vjp(f, v, one)
        call check_shape("Piecewise VJP", back(1), [3*x**2], &
            [open_condition], 2*x)
        second = vjp(back, v, one)
        call check_shape("Piecewise VJP of VJP", second(1), [6*x], &
            [open_condition], 2*one(1))
        second = hvp(f(1), v, one)
        call check_shape("Piecewise HVP", second(1), [6*x], &
            [open_condition], 2*one(1))

        ! A condition between numbers is decided after differentiation, so
        ! the second derivative is the one of the selected branch.
        closed(1) = guarded(x**3, less(zero, one(1)), x**2)
        first = jvp(closed, v, one)
        second = jvp(first, v, one)
        call check_identity(s, eng, "true guard: second JVP is 6 x", &
            second(1) - 6*x)
        second = hvp(closed(1), v, one)
        call check_identity(s, eng, "true guard: HVP is 6 x", second(1) - 6*x)
        closed(1) = guarded(x**3, less(one(1), zero), x**2)
        first = jvp(closed, v, one)
        second = jvp(first, v, one)
        call check_identity(s, eng, "false guard: second JVP is 2", &
            second(1) - 2)
        back = vjp(closed, v, one)
        second = vjp(back, v, one)
        call check_identity(s, eng, "false guard: second VJP is 2", &
            second(1) - 2)
    end subroutine test_piecewise_second_order

    !> Simplification that keeps some branches must keep the shape too.
    subroutine test_piecewise_simplified_shape()
        type(expr_t) :: zero, one, open_condition, true_condition, pieces, f
        type(engine_result_t) :: kept

        zero = num(arena, 0)
        one = num(arena, 1)
        open_condition = less(x, zero)
        true_condition = less(zero, one)
        pieces = func("List", [func("List", [x**3, open_condition]), &
            func("List", [x**2, true_condition]), &
            func("List", [x, less(x, one)])])
        f = func("Piecewise", [pieces, num(arena, 7)])
        kept = eng%simplify(f)
        call check_true("partly decided Piecewise simplifies", kept%ok)
        ! The decided guard is rewritten to the truth symbol and ends the list.
        true_condition = sym(arena, "True")
        call check_shape("partly decided Piecewise", kept%value, &
            [x**3, x**2], [open_condition, true_condition], num(arena, 7))
        call check_shape("diff of a simplified Piecewise", &
            diff(kept%value, x), [3*x**2, 2*x], &
            [open_condition, true_condition], zero)
    end subroutine test_piecewise_simplified_shape

    !> The other heads whose products are contracted from `diff`.
    subroutine test_container_heads()
        type(expr_t) :: f(2), v(2), forward(2), reverse(2), zero, one, g

        v = [x, y]
        zero = num(arena, 0)
        one = num(arena, 1)
        f(1) = func("If", [less(x, y), x**2*y, x + y**3])*y
        f(2) = sin(f(1)) + x
        call check_against_diff("If", eng, f, v, 2)
        call check_duality("If duality", eng, f, v, 2)

        ! Decided guards select a branch, whose products are written by hand.
        f(1) = func("If", [less(zero, one), x**2*y, x + y**3])*y
        f(2) = func("If", [less(one, zero), x**2*y, x + y**3])*y
        forward = jvp(f, v, w(:2))
        call check_identity(s, eng, "If JVP, true guard", &
            forward(1) - (2*x*y**2*w(1) + 2*x**2*y*w(2)))
        call check_identity(s, eng, "If JVP, false guard", &
            forward(2) - (y*w(1) + (x + 4*y**3)*w(2)))
        reverse = vjp(f, v, u(:2))
        call check_identity(s, eng, "If VJP, x", &
            reverse(1) - (2*x*y**2*u(1) + y*u(2)))
        call check_identity(s, eng, "If VJP, y", &
            reverse(2) - (2*x**2*y*u(1) + (x + 4*y**3)*u(2)))

        g = func("g", [x*y, y])
        f(1) = partial(g, x)*y
        f(2) = sin(f(1)) + x
        call check_against_diff("partial", eng, f, v, 2)
        call check_duality("partial duality", eng, f, v, 2)

        ! A List output is differentiated element by element.
        f(1) = func("List", [x*y, sin(x)])
        f(2) = func("List", [g, x**2])
        call check_against_diff("List", eng, f, v, 2)
        forward = jvp(f, v, [one, zero])
        call check_list("List JVP along x", forward(1), [y, cos(x)])
        forward = jvp(f, v, [zero, one])
        call check_list("List JVP along y", forward(1), [x, zero])
    end subroutine test_container_heads

    function guarded(value, condition, default_value) result(e)
        type(expr_t), intent(in) :: value, condition, default_value
        type(expr_t) :: e

        e = func("Piecewise", [func("List", [func("List", &
            [value, condition])]), default_value])
    end function guarded

    subroutine check_true(label, condition)
        character(*), intent(in) :: label
        logical, intent(in) :: condition

        call check_identity(s, eng, label, num(arena, merge(0, 1, condition)))
    end subroutine check_true

    !> `e` simplifies to Piecewise(List(List(value, condition)...), default)
    !> with the given conditions and with values equal to the given ones.
    subroutine check_shape(label, e, values, conditions, default_value)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e, values(:), conditions(:), default_value
        type(engine_result_t) :: simplified
        type(expr_t) :: p, branches, pair
        logical :: shaped
        integer :: k

        simplified = eng%simplify(e)
        p = simplified%value
        shaped = simplified%ok .and. p%kind() == NK_FUNC
        if (shaped) shaped = chars(p%name()) == "Piecewise" .and. p%nargs() == 2
        if (shaped) then
            branches = p%arg(1)
            shaped = is_list(branches, size(values))
        end if
        do k = 1, size(values)
            if (.not. shaped) exit
            pair = branches%arg(k)
            shaped = is_list(pair, 2)
            if (shaped) shaped = pair%arg(2) == conditions(k)
        end do
        call check_true(label//": branch list and default", shaped)
        if (.not. shaped) return
        do k = 1, size(values)
            pair = branches%arg(k)
            call check_identity(s, eng, label//": branch value", &
                pair%arg(1) - values(k))
        end do
        call check_identity(s, eng, label//": default value", &
            p%arg(2) - default_value)
    end subroutine check_shape

    subroutine check_list(label, e, values)
        character(*), intent(in) :: label
        type(expr_t), intent(in) :: e, values(:)
        type(engine_result_t) :: simplified
        type(expr_t) :: list
        logical :: shaped
        integer :: k

        simplified = eng%simplify(e)
        list = simplified%value
        shaped = simplified%ok
        if (shaped) shaped = is_list(list, size(values))
        call check_true(label//": is a List", shaped)
        if (.not. shaped) return
        do k = 1, size(values)
            call check_identity(s, eng, label//": element", &
                list%arg(k) - values(k))
        end do
    end subroutine check_list

    logical function is_list(e, n) result(yes)
        type(expr_t), intent(in) :: e
        integer, intent(in) :: n

        yes = e%kind() == NK_FUNC
        if (yes) yes = chars(e%name()) == "List" .and. e%nargs() == n
    end function is_list

    subroutine check_against_diff(label, engine, f, v, m)
        character(*), intent(in) :: label
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: f(:), v(:)
        integer, intent(in) :: m
        type(expr_t) :: forward(size(f)), reverse(size(v)), contracted
        integer :: i, j

        forward = jvp(f, v, w(:size(v)))
        reverse = vjp(f, v, u(:m))
        do i = 1, size(f)
            contracted = num(arena, 0)
            do j = 1, size(v)
                contracted = contracted + diff(f(i), v(j))*w(j)
            end do
            call check_identity(s, engine, label//": JVP equals diff contraction", &
                forward(i) - contracted)
        end do
        do j = 1, size(v)
            contracted = num(arena, 0)
            do i = 1, size(f)
                contracted = contracted + u(i)*diff(f(i), v(j))
            end do
            call check_identity(s, engine, label//": VJP equals diff contraction", &
                reverse(j) - contracted)
        end do
    end subroutine check_against_diff

    subroutine check_duality(label, engine, f, v, m)
        character(*), intent(in) :: label
        type(native_engine_t), intent(inout) :: engine
        type(expr_t), intent(in) :: f(:), v(:)
        integer, intent(in) :: m
        type(expr_t) :: forward(size(f)), reverse(size(v)), left, right
        integer :: k

        forward = jvp(f, v, w(:size(v)))
        reverse = vjp(f, v, u(:m))
        left = num(arena, 0)
        right = num(arena, 0)
        do k = 1, size(f)
            left = left + u(k)*forward(k)
        end do
        do k = 1, size(v)
            right = right + reverse(k)*w(k)
        end do
        call check_identity(s, engine, label, left - right)
    end subroutine check_duality

end program test_fortsym_product_heads
