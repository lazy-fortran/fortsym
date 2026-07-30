program test_fortsym_arena
    ! Behavioural checks for the hash-consed arena and expr_t.
    !
    ! The oracle throughout is structural sharing: two expressions that denote
    ! the same tree must land on the same node index, and two that do not must
    ! not. Everything downstream -- equality, CSE, the node-count measure --
    ! rests on that, so it is what these tests pin.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_string, only: str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT
    use fortsym_expr
    implicit none

    integer, parameter :: dp = real64
    integer :: nfail = 0

    call test_atoms()
    call test_interning()
    call test_rational_normalisation()
    call test_arbitrary_precision_exact()
    call test_large_exact_pool()
    call test_commutative_canonicalisation()
    call test_large_semantic_sort()
    call test_associative_flattening()
    call test_noncommutative_order()
    call test_structural_sharing()
    call test_no_premature_simplification()
    call test_node_count()
    call test_growth()

    if (nfail == 0) then
        print *, "test_fortsym_arena: all checks passed"
    else
        print *, "test_fortsym_arena: ", nfail, " check(s) FAILED"
        error stop 1
    end if

contains

    subroutine ok(label, cond)
        character(*), intent(in) :: label
        logical,      intent(in) :: cond
        if (.not. cond) then
            nfail = nfail + 1
            print *, "FAIL ", label
        end if
    end subroutine ok

    subroutine ok_int(label, got, want)
        character(*),   intent(in) :: label
        integer(int64), intent(in) :: got, want
        if (got /= want) then
            nfail = nfail + 1
            print *, "FAIL ", label, " got", got, " want", want
        end if
    end subroutine ok_int

    subroutine test_atoms()
        type(arena_t), target :: a
        type(expr_t) :: x, n, r, q, c

        call a%init()
        x = sym(a, "x")
        n = num(a, 42)
        r = real_expr(a, 1.5_dp)
        q = rat(a, 1_int64, 3_int64)
        c = pi_expr(a)

        call ok("symbol kind", x%kind() == NK_SYM)
        call ok("symbol name", chars(x%name()) == "x")
        call ok("integer kind", n%kind() == NK_INT)
        call ok_int("integer value", n%int_value(), 42_int64)
        call ok("real kind", r%kind() == NK_REAL)
        call ok("real value", r%real_value() == 1.5_dp)
        call ok("rational kind", q%kind() == NK_RAT)
        call ok_int("rational numerator", q%int_value(), 1_int64)
        call ok_int("rational denominator", q%den_value(), 3_int64)
        call ok("constant kind", c%kind() == NK_CONST)
        call ok("constant name", chars(c%name()) == "pi")

        call ok("expression is valid", is_valid(x))
        block
            type(expr_t) :: fresh
            call ok("default expr is invalid", .not. is_valid(fresh))
        end block
    end subroutine test_atoms

    subroutine test_interning()
        type(arena_t), target :: a
        type(expr_t) :: x1, x2, y, n1, n2

        call a%init()

        ! The same symbol twice must be one node, not two.
        x1 = sym(a, "x")
        x2 = sym(a, "x")
        call ok("same symbol interns once", x1 == x2)
        call ok("arena holds one node", a%size() == 1)

        y = sym(a, "y")
        call ok("different symbols differ", x1 /= y)
        call ok("arena holds two nodes", a%size() == 2)

        n1 = num(a, 7)
        n2 = num(a, 7)
        call ok("same integer interns once", n1 == n2)

        ! An integer and a symbol named the same must never collide.
        call ok("integer differs from symbol", n1 /= x1)

        ! A real and an integer of equal value are different objects: one emits
        ! as 7 and the other as a floating literal.
        call ok("real 7 differs from integer 7", real_expr(a, 7.0_dp) /= n1)
    end subroutine test_interning

    subroutine test_rational_normalisation()
        type(arena_t), target :: a
        type(expr_t) :: half1, half2, half3, whole

        call a%init()

        ! Equal rationals in different spellings must intern to one node.
        half1 = rat(a, 1_int64, 2_int64)
        half2 = rat(a, 2_int64, 4_int64)
        half3 = rat(a, -1_int64, -2_int64)
        call ok("2/4 normalises to 1/2", half1 == half2)
        call ok("-1/-2 normalises to 1/2", half1 == half3)

        ! Sign belongs to the numerator, so 1/-2 and -1/2 are one node.
        call ok("sign moves to numerator", &
            rat(a, 1_int64, -2_int64) == rat(a, -1_int64, 2_int64))

        ! A rational that reduces to an integer becomes one.
        whole = rat(a, 6_int64, 3_int64)
        call ok("6/3 becomes integer", whole%kind() == NK_INT)
        call ok_int("6/3 equals 2", whole%int_value(), 2_int64)
        call ok("6/3 is the integer node", whole == num(a, 2))
    end subroutine test_rational_normalisation

    !> Golden values are derived by powers of two, independently of FLINT:
    !> 2^64 is 18446744073709551616 and doubling then dividing by two must
    !> reconstruct the same exact integer node.
    subroutine test_arbitrary_precision_exact()
        type(arena_t), target :: a, default_arena
        type(expr_t) :: power64, reconstructed, fraction, small, invalid
        logical :: good

        call a%init()
        power64 = exact(a, "18446744073709551616", good)
        call ok("2^64 exact input accepted", good)
        call ok("2^64 uses arbitrary integer node", &
            power64%kind() == NK_BIG_INT)
        call ok("2^64 canonical text", &
            chars(power64%exact_text()) == "18446744073709551616")

        reconstructed = exact(a, "36893488147419103232/2", good)
        call ok("double divided by two accepted", good)
        call ok("exact reconstruction interns one node", &
            reconstructed == power64)

        fraction = exact(a, &
            "18446744073709551617/18446744073709551616", good)
        call ok("large coprime rational accepted", good)
        call ok("large coprime rational kind", fraction%kind() == NK_BIG_RAT)
        call ok("large rational canonical text", &
            chars(fraction%exact_text()) == &
            "18446744073709551617/18446744073709551616")

        small = exact(a, "300000000000000000000/400000000000000000000", good)
        call ok("large spelling reduces to compact rational", good .and. &
            small == rat(a, 3_int64, 4_int64))

        invalid = exact(a, "1/0", good)
        call ok("zero denominator rejected", .not. good)
        call ok("rejected exact input is invalid", .not. is_valid(invalid))

        power64 = exact(default_arena, "18446744073709551616", good)
        call ok("exact input initializes a default arena", &
            good .and. is_valid(power64) .and. default_arena%size() == 1)
    end subroutine test_arbitrary_precision_exact

    !> Reinsert 2,049 distinct large values after crossing every text-table
    !> growth boundary. The independent oracle is stable hash-consing: reverse
    !> lookup must recover every original node without growing the arena.
    subroutine test_large_exact_pool()
        integer, parameter :: N = 2049
        type(arena_t), target :: a
        type(expr_t), allocatable :: values(:)
        type(expr_t) :: again
        character(:), allocatable :: text
        integer :: i, before
        logical :: good

        call a%init()
        allocate (values(N))
        do i = 1, N
            text = "10000000000000000000000000000000000000000000000000"//&
                chars(str(i))
            values(i) = exact(a, text, good)
            call ok("large exact pool insertion", good)
        end do
        before = a%size()
        do i = N, 1, -1
            text = "10000000000000000000000000000000000000000000000000"//&
                chars(str(i))
            again = exact(a, text, good)
            call ok("large exact pool reverse lookup", &
                good .and. again == values(i))
        end do
        call ok("large exact pool lookup does not grow arena", &
            a%size() == before)
    end subroutine test_large_exact_pool

    subroutine test_commutative_canonicalisation()
        type(arena_t), target :: a
        type(expr_t) :: x, y, z

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")
        z = sym(a, "z")

        ! Operand order must not produce distinct nodes for a commutative op.
        call ok("x+y equals y+x", (x + y) == (y + x))
        call ok("x*y equals y*x", (x*y) == (y*x))
        call ok("three-way sum order", (x + y + z) == (z + y + x))
        call ok("three-way product order", (x*y*z) == (z*y*x))
    end subroutine test_commutative_canonicalisation

    subroutine test_large_semantic_sort()
        integer, parameter :: N = 257
        type(arena_t), target :: a
        integer :: operands(N), sum_id, i
        character(16) :: name

        call a%init()
        do i = 1, N
            write (name, '("v",i3.3)') N - i + 1
            operands(i) = a%sym(trim(name))
        end do
        sum_id = a%add(operands)

        call ok("large semantic sort keeps every operand", &
            a%nargs_of(sum_id) == N)
        do i = 1, N
            write (name, '("v",i3.3)') i
            call ok("large semantic sort is lexical", &
                chars(a%name_of(a%arg_of(sum_id, i))) == trim(name))
        end do
    end subroutine test_large_semantic_sort

    subroutine test_associative_flattening()
        type(arena_t), target :: a
        type(expr_t) :: x, y, z, left, right

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")
        z = sym(a, "z")

        ! Bracketing must not survive: a sum is n-ary and flat.
        left = (x + y) + z
        right = x + (y + z)
        call ok("sum association is flattened", left == right)
        call ok("flattened sum is ternary", left%nargs() == 3)

        left = (x*y)*z
        right = x*(y*z)
        call ok("product association is flattened", left == right)
        call ok("flattened product is ternary", left%nargs() == 3)
    end subroutine test_associative_flattening

    subroutine test_noncommutative_order()
        type(arena_t), target :: a
        type(expr_t) :: x, y, p

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")

        ! Power and general functions must keep their argument order.
        call ok("x**y differs from y**x", (x**y) /= (y**x))
        call ok("atan2(x,y) differs from atan2(y,x)", &
            atan2(x, y) /= atan2(y, x))

        p = x**y
        call ok("pow is binary", p%nargs() == 2)
        call ok("pow base is x", p%arg(1) == x)
        call ok("pow exponent is y", p%arg(2) == y)
    end subroutine test_noncommutative_order

    subroutine test_structural_sharing()
        type(arena_t), target :: a
        type(expr_t) :: x, y, common, e1, e2
        integer :: before, after

        call a%init()
        x = sym(a, "x")
        y = sym(a, "y")

        common = sin(x*y)
        before = a%size()

        ! Rebuilding an identical subexpression must add no nodes at all. This
        ! is the property CSE later depends on.
        e1 = sin(x*y)
        after = a%size()
        call ok("rebuilt subexpression is shared", common == e1)
        call ok("rebuilding allocates nothing", after == before)

        ! Two expressions over the same subexpression share it.
        e1 = sin(x*y) + 1
        e2 = sin(x*y)*2
        call ok("shared subterm in first", e1%arg(1) == common .or. &
            e1%arg(2) == common)
        call ok("shared subterm in second", e2%arg(1) == common .or. &
            e2%arg(2) == common)
    end subroutine test_structural_sharing

    subroutine test_no_premature_simplification()
        type(arena_t), target :: a
        type(expr_t) :: x, zero_ish

        call a%init()
        x = sym(a, "x")

        ! The arena is a faithful structure, not an algebra. x - x is a sum, and
        ! deciding it is zero belongs to the engines. If this ever starts
        ! simplifying, the separation that keeps the frontend engine-agnostic
        ! has been broken.
        zero_ish = x - x
        call ok("x - x stays a sum", zero_ish%kind() == NK_ADD)
        call ok("x - x is not the integer zero", zero_ish /= num(a, 0))

        ! Likewise x + x is not folded into 2*x.
        call ok("x + x differs from 2*x", (x + x) /= (2*x))

        ! Subtraction is addition of a negation, so both spellings agree.
        call ok("a-b equals a+(-1)*b", (x - x) == (x + (-1)*x))
    end subroutine test_no_premature_simplification

    subroutine test_node_count()
        type(arena_t), target :: a
        type(expr_t) :: x, shared, e

        call a%init()
        x = sym(a, "x")

        ! x alone is one node.
        call ok("single symbol counts one", x%node_count() == 1)

        ! A shared subterm is counted once however often it appears, which is
        ! what makes node_count a measure of kernel work rather than of how the
        ! expression was written.
        shared = sin(x)
        e = shared + shared*shared
        call ok("shared subterm counted once", &
            e%node_count() < 2*shared%node_count() + 3)
    end subroutine test_node_count

    subroutine test_growth()
        type(arena_t), target :: a
        type(expr_t) :: acc, xi, composite, rebuilt
        character(len=16) :: nm
        integer :: i, before

        call a%init()

        ! Preserve a composite made before any growth. Reconstructing this
        ! exact shape after rehashing proves that old collision chains were
        ! rebuilt rather than merely that new post-growth nodes can intern.
        composite = (sym(a, "v1") + rat(a, 2_int64, 3_int64))*sym(a, "v2")

        ! Push well past the initial capacities so every growth path runs, and
        ! confirm the structure survives reallocation of nodes, args and names.
        acc = num(a, 0)
        do i = 1, 400
            write (nm, '(a,i0)') "v", i
            xi = sym(a, chars_of(nm, i))
            acc = acc + xi*i
        end do

        call ok("grown arena passes initial bucket count", a%size() > 1024)

        ! Interning still works after the tables moved.
        call ok("interning survives growth", sym(a, "v1") == sym(a, "v1"))
        call ok("distinct names survive growth", sym(a, "v1") /= sym(a, "v2"))

        before = a%size()
        rebuilt = sym(a, "v2")*(rat(a, 2_int64, 3_int64) + sym(a, "v1"))
        call ok("pre-growth composite survives bucket rehash", rebuilt == composite)
        call ok("pre-growth rebuild allocates nothing", a%size() == before)
    end subroutine test_growth

    !> Exact-length name for the growth loop, without relying on TRIM semantics.
    function chars_of(buf, i) result(s)
        character(*), intent(in)  :: buf
        integer,      intent(in)  :: i
        character(:), allocatable :: s
        integer :: n
        n = 1
        do while (10**n <= i)
            n = n + 1
        end do
        s = buf(1:1 + n)
    end function chars_of

end program test_fortsym_arena
