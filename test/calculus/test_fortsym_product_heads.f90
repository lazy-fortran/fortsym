program test_fortsym_product_heads
    ! Contracted products through abs and through heads with several
    ! arguments. Every check is a decided identity; numeric probe evidence is
    ! not accepted. The oracles are the chain rule written out by hand, the
    ! contraction of `diff`, and the forward/reverse duality
    !
    !     u . (J w) == (J^T u) . w
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, func, besselj, &
        operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), sin, cos, abs
    use fortsym_relation, only: less
    use fortsym_diff, only: diff
    use fortsym_assume, only: assumption_context_t, assume, positive, negative
    use fortsym_check, only: suite_t, suite_begin, suite_end, check_identity
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
