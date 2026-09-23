module fortsym_rigorous_emit
    ! One expression DAG, two scalar leaves: a floating-point kernel for trial
    ! computations and a rigorous kernel whose result encloses the exact value
    ! of the expression for every input in the argument enclosures.
    !
    ! Both leaves are rendered from the same lowered operation list, so they
    ! perform the same operations in the same order. The rigorous leaf calls a
    ! small runtime interface (doc/rigorous-runtime.md) instead of intrinsic
    ! arithmetic. The interface is pluggable: a runtime descriptor names the
    ! module, the enclosure type, and one procedure per operation, so the same
    ! lowering serves a complex ball runtime and a real interval runtime.
    !
    ! Exactness is part of the contract. Integer and rational literals stay
    ! exact until the runtime sees them: an integer up to 2**53 is an exact
    ! real64 point, a rational with a power-of-two denominator is an exact
    ! point, and any other rational becomes a runtime division of two exact
    ! points. Decimal floating literals are refused, because the value the
    ! author meant is unknown; so are functions the runtime interface does not
    ! cover. Refusal is a diagnostic, never a silently widened kernel.
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_SYM, NK_CONST, NK_ADD, &
        NK_MUL, NK_POW, NK_FUNC, node_kind_name
    use fortsym_expr, only: expr_t
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_names, only: valid_fortran_name
    implicit none
    private

    public :: rigorous_runtime_t, rigorous_kernel_spec_t
    public :: RUNTIME_BALL, RUNTIME_INTERVAL
    public :: ball_runtime, interval_runtime
    public :: emit_rigorous_kernel, emit_float_kernel

    integer, parameter :: dp = real64

    !> Complex midpoint-radius balls: complex arithmetic and the imaginary unit.
    integer, parameter :: RUNTIME_BALL = 1
    !> Real intervals [lo, hi]: real arithmetic only.
    integer, parameter :: RUNTIME_INTERVAL = 2

    !> Largest integer magnitude that is exact in real64.
    integer(int64), parameter :: EXACT_LIMIT = 9007199254740992_int64

    integer, parameter :: OP_ARG = 1, OP_POINT = 2, OP_CPOINT = 3, &
        OP_ENCLOSE = 4, OP_ADD = 5, OP_SUB = 6, OP_MUL = 7, OP_DIV = 8, &
        OP_NEG = 9, OP_INV = 10, OP_SQRT = 11, OP_POWI = 12, OP_SCALE = 13

    !> The runtime interface a rigorous leaf calls. Every procedure is pure
    !> and elemental in the reference runtimes; a consumer runtime must at
    !> least be pure when `pure_procedure` is requested. Semantics, for
    !> enclosures a, b of the exact values and exact real64 x:
    !>   add(a,b) sub(a,b) mul(a,b) div(a,b) neg(a) inv(a) sqrt(a)
    !>   powi(a,n)  a**n for integer n >= 2
    !>   scale(a,x) a*x
    !>   point(x)   the exact real x
    !>   cpoint(x,y) the exact complex x + i y (ball runtimes only)
    !>   enclose(m,r) the set |z - m| <= r
    !> Each result must enclose the exact result for all values in the operand
    !> enclosures. An operation outside its domain returns an enclosure that
    !> carries no information (for example an infinite radius).
    type :: rigorous_runtime_t
        integer :: kind = RUNTIME_BALL
        type(str_t) :: module_name
        type(str_t) :: type_name
        type(str_t) :: add, sub, mul, div, neg, inv, sqrt, powi, scale
        type(str_t) :: point, cpoint, enclose
    end type rigorous_runtime_t

    type :: rigorous_kernel_spec_t
        type(str_t) :: name
        type(str_t), allocatable :: args(:)
        type(str_t), allocatable :: outputs(:)
        !> Float leaf only: which arguments are complex(real64). Absent means
        !> all real. The rigorous leaf passes every argument as an enclosure.
        logical, allocatable :: complex_args(:)
        !> Float leaf only: declare the outputs complex(real64).
        logical :: complex_outputs = .false.
        type(rigorous_runtime_t) :: runtime
        !> Emit `pure subroutine`; requires pure runtime procedures.
        logical :: pure_procedure = .true.
        !> Emit `elemental subroutine`, so the leaf maps over arrays.
        logical :: elemental_procedure = .false.
        !> Evaluate univariate polynomial sums by Horner's rule.
        logical :: horner = .true.
        type(str_t) :: temp_prefix
        type(str_t) :: generator
    end type rigorous_kernel_spec_t

    type :: rop_t
        integer :: op = 0
        integer :: a = 0
        integer :: b = 0
        integer :: n = 0
        real(dp) :: x = 0.0_dp
        real(dp) :: y = 0.0_dp
    end type rop_t

    type :: rir_t
        type(rop_t), allocatable :: ops(:)
        integer :: n = 0
        integer, allocatable :: outputs(:)
    end type rir_t

    type :: lowering_t
        type(arena_t), pointer :: arena => null()
        integer, allocatable :: memo(:)
        type(str_t), allocatable :: args(:)
        integer :: runtime_kind = RUNTIME_BALL
        logical :: horner = .true.
        logical :: ok = .true.
        character(:), allocatable :: message
    end type lowering_t

contains

    !> Reference complex-ball runtime names (test/codegen/runtime).
    function ball_runtime(module_name, type_name) result(rt)
        character(*), intent(in), optional :: module_name, type_name
        type(rigorous_runtime_t) :: rt

        rt%kind = RUNTIME_BALL
        rt%module_name = str("fortsym_ball_runtime")
        rt%type_name = str("ball_t")
        if (present(module_name)) rt%module_name = str(module_name)
        if (present(type_name)) rt%type_name = str(type_name)
        rt%add = str("badd")
        rt%sub = str("bsub")
        rt%mul = str("bmul")
        rt%div = str("bdiv")
        rt%neg = str("bneg")
        rt%inv = str("binv")
        rt%sqrt = str("bsqrt")
        rt%powi = str("bpowi")
        rt%scale = str("bscale")
        rt%point = str("bpoint")
        rt%cpoint = str("bcpoint")
        rt%enclose = str("benclose")
    end function ball_runtime

    !> Reference real-interval runtime names (test/codegen/runtime).
    function interval_runtime(module_name, type_name) result(rt)
        character(*), intent(in), optional :: module_name, type_name
        type(rigorous_runtime_t) :: rt

        rt%kind = RUNTIME_INTERVAL
        rt%module_name = str("fortsym_interval_runtime")
        rt%type_name = str("interval_t")
        if (present(module_name)) rt%module_name = str(module_name)
        if (present(type_name)) rt%type_name = str(type_name)
        rt%add = str("iadd")
        rt%sub = str("isub")
        rt%mul = str("imul")
        rt%div = str("idiv")
        rt%neg = str("ineg")
        rt%inv = str("iinv")
        rt%sqrt = str("isqrt")
        rt%powi = str("ipowi")
        rt%scale = str("iscale")
        rt%point = str("ipoint")
        rt%cpoint = str("")
        rt%enclose = str("ienclose")
    end function interval_runtime

    !> Rigorous leaf: every argument and output is an enclosure of the
    !> runtime's type, and every operation is a runtime call.
    function emit_rigorous_kernel(roots, spec, ok, message) result(source)
        type(expr_t), intent(in) :: roots(:)
        type(rigorous_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(rir_t) :: ir

        source = str("")
        call validate_spec(roots, spec, .true., ok, message)
        if (.not. ok) return
        call lower(roots, spec, ir, ok, message)
        if (.not. ok) return
        call validate_runtime_use(ir, spec%runtime, ok, message)
        if (.not. ok) return
        source = render(ir, spec, .true.)
    end function emit_rigorous_kernel

    !> Floating-point leaf of the same operation list, for trial solvers.
    function emit_float_kernel(roots, spec, ok, message) result(source)
        type(expr_t), intent(in) :: roots(:)
        type(rigorous_kernel_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source
        type(rir_t) :: ir
        logical, allocatable :: cplx(:)
        integer :: k

        source = str("")
        call validate_spec(roots, spec, .false., ok, message)
        if (.not. ok) return
        call lower(roots, spec, ir, ok, message)
        if (.not. ok) return
        call complex_flags(ir, spec, cplx)
        if (.not. spec%complex_outputs) then
            do k = 1, size(ir%outputs)
                if (cplx(ir%outputs(k))) then
                    ok = .false.
                    message = "rigorous emitter: complex value for real output "// &
                        chars(spec%outputs(k))
                    return
                end if
            end do
        end if
        source = render(ir, spec, .false.)
    end function emit_float_kernel

    subroutine validate_spec(roots, spec, rigorous, ok, message)
        type(expr_t), intent(in) :: roots(:)
        type(rigorous_kernel_spec_t), intent(in) :: spec
        logical, intent(in) :: rigorous
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: k, j

        ok = .false.
        message = ""
        if (.not. valid_fortran_name(chars(spec%name))) then
            message = "rigorous emitter: invalid kernel name"
            return
        end if
        if (.not. allocated(spec%args) .or. .not. allocated(spec%outputs)) then
            message = "rigorous emitter: args and outputs must be allocated"
            return
        end if
        if (size(spec%outputs) /= size(roots) .or. size(roots) == 0) then
            message = "rigorous emitter: one output name per root is required"
            return
        end if
        do k = 1, size(spec%args)
            if (.not. valid_fortran_name(chars(spec%args(k)))) then
                message = "rigorous emitter: invalid argument name "// &
                    chars(spec%args(k))
                return
            end if
        end do
        do k = 1, size(spec%outputs)
            if (.not. valid_fortran_name(chars(spec%outputs(k)))) then
                message = "rigorous emitter: invalid output name "// &
                    chars(spec%outputs(k))
                return
            end if
            do j = 1, size(spec%args)
                if (lower_eq(chars(spec%args(j)), chars(spec%outputs(k)))) then
                    message = "rigorous emitter: output name repeats an argument"
                    return
                end if
            end do
        end do
        if (allocated(spec%complex_args)) then
            if (size(spec%complex_args) /= size(spec%args)) then
                message = "rigorous emitter: complex_args size differs from args"
                return
            end if
        end if
        if (rigorous) then
            if (.not. valid_fortran_name(chars(spec%runtime%module_name)) .or. &
                .not. valid_fortran_name(chars(spec%runtime%type_name))) then
                message = "rigorous emitter: runtime module or type name invalid"
                return
            end if
        end if
        do k = 1, size(roots)
            if (.not. associated(roots(k)%a)) then
                message = "rigorous emitter: invalid root"
                return
            end if
            if (.not. associated(roots(k)%a, roots(1)%a)) then
                message = "rigorous emitter: roots belong to different arenas"
                return
            end if
        end do
        ok = .true.
    end subroutine validate_spec

    pure logical function lower_eq(a, b)
        character(*), intent(in) :: a, b
        integer :: k, ca, cb

        lower_eq = .false.
        if (len(a) /= len(b)) return
        do k = 1, len(a)
            ca = iachar(a(k:k))
            cb = iachar(b(k:k))
            if (ca >= 65 .and. ca <= 90) ca = ca + 32
            if (cb >= 65 .and. cb <= 90) cb = cb + 32
            if (ca /= cb) return
        end do
        lower_eq = .true.
    end function lower_eq

    !> The runtime must name every procedure the lowered list calls.
    subroutine validate_runtime_use(ir, rt, ok, message)
        type(rir_t), intent(in) :: ir
        type(rigorous_runtime_t), intent(in) :: rt
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: k
        character(:), allocatable :: name

        ok = .true.
        message = ""
        do k = 1, ir%n
            if (ir%ops(k)%op == OP_ARG) cycle
            name = runtime_name(rt, ir%ops(k)%op)
            if (.not. valid_fortran_name(name)) then
                ok = .false.
                message = "rigorous emitter: runtime provides no procedure for "// &
                    op_label(ir%ops(k)%op)
                return
            end if
        end do
    end subroutine validate_runtime_use

    function runtime_name(rt, op) result(name)
        type(rigorous_runtime_t), intent(in) :: rt
        integer, intent(in) :: op
        character(:), allocatable :: name

        select case (op)
        case (OP_POINT)
            name = chars(rt%point)
        case (OP_CPOINT)
            name = chars(rt%cpoint)
        case (OP_ENCLOSE)
            name = chars(rt%enclose)
        case (OP_ADD)
            name = chars(rt%add)
        case (OP_SUB)
            name = chars(rt%sub)
        case (OP_MUL)
            name = chars(rt%mul)
        case (OP_DIV)
            name = chars(rt%div)
        case (OP_NEG)
            name = chars(rt%neg)
        case (OP_INV)
            name = chars(rt%inv)
        case (OP_SQRT)
            name = chars(rt%sqrt)
        case (OP_POWI)
            name = chars(rt%powi)
        case (OP_SCALE)
            name = chars(rt%scale)
        case default
            name = ""
        end select
    end function runtime_name

    function op_label(op) result(name)
        integer, intent(in) :: op
        character(:), allocatable :: name

        select case (op)
        case (OP_POINT)
            name = "point"
        case (OP_CPOINT)
            name = "cpoint"
        case (OP_ENCLOSE)
            name = "enclose"
        case (OP_ADD)
            name = "add"
        case (OP_SUB)
            name = "sub"
        case (OP_MUL)
            name = "mul"
        case (OP_DIV)
            name = "div"
        case (OP_NEG)
            name = "neg"
        case (OP_INV)
            name = "inv"
        case (OP_SQRT)
            name = "sqrt"
        case (OP_POWI)
            name = "powi"
        case (OP_SCALE)
            name = "scale"
        case default
            name = "argument"
        end select
    end function op_label

    subroutine lower(roots, spec, ir, ok, message)
        type(expr_t), intent(in) :: roots(:)
        type(rigorous_kernel_spec_t), intent(in) :: spec
        type(rir_t), intent(out) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(lowering_t) :: low
        integer :: k

        low%arena => roots(1)%a
        allocate (low%memo(low%arena%size()), source=0)
        low%args = spec%args
        low%runtime_kind = spec%runtime%kind
        low%horner = spec%horner
        low%ok = .true.
        low%message = ""
        allocate (ir%ops(64), ir%outputs(size(roots)))
        ir%n = 0
        do k = 1, size(roots)
            ir%outputs(k) = lower_node(low, ir, roots(k)%id)
            if (.not. low%ok) exit
        end do
        ok = low%ok
        message = low%message
    end subroutine lower

    subroutine fail(low, text)
        type(lowering_t), intent(inout) :: low
        character(*), intent(in) :: text

        if (low%ok) then
            low%ok = .false.
            low%message = "rigorous emitter: "//text
        end if
    end subroutine fail

    !> Append an operation, reusing an identical earlier one.
    function push(ir, op, a, b, n, x, y) result(idx)
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: op
        integer, intent(in), optional :: a, b, n
        real(dp), intent(in), optional :: x, y
        integer :: idx
        type(rop_t) :: r
        type(rop_t), allocatable :: grown(:)

        r%op = op
        if (present(a)) r%a = a
        if (present(b)) r%b = b
        if (present(n)) r%n = n
        if (present(x)) r%x = x
        if (present(y)) r%y = y
        do idx = 1, ir%n
            if (ir%ops(idx)%op == r%op .and. ir%ops(idx)%a == r%a .and. &
                ir%ops(idx)%b == r%b .and. ir%ops(idx)%n == r%n) then
                if (same_bits(ir%ops(idx)%x, r%x) .and. &
                    same_bits(ir%ops(idx)%y, r%y)) return
            end if
        end do
        if (ir%n == size(ir%ops)) then
            allocate (grown(2*size(ir%ops)))
            grown(1:ir%n) = ir%ops(1:ir%n)
            call move_alloc(grown, ir%ops)
        end if
        ir%n = ir%n + 1
        ir%ops(ir%n) = r
        idx = ir%n
    end function push

    pure logical function same_bits(a, b)
        real(dp), intent(in) :: a, b

        same_bits = transfer(a, 0_int64) == transfer(b, 0_int64)
    end function same_bits

    pure logical function power_of_two(q)
        integer(int64), intent(in) :: q

        power_of_two = q > 0_int64 .and. iand(q, q - 1_int64) == 0_int64
    end function power_of_two

    !> Exact real64 point of an integer, or a failure beyond 2**53.
    function int_point(low, ir, p) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer(int64), intent(in) :: p
        integer :: idx

        idx = 0
        if (abs(p) > EXACT_LIMIT) then
            call fail(low, "integer literal is not exact in real64")
            return
        end if
        idx = push(ir, OP_POINT, x=real(p, dp))
    end function int_point

    function rational_value(low, ir, p, q) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer(int64), intent(in) :: p, q
        integer :: idx

        idx = 0
        if (q == 1_int64) then
            idx = int_point(low, ir, p)
        else if (power_of_two(q) .and. abs(p) <= EXACT_LIMIT) then
            idx = push(ir, OP_POINT, x=real(p, dp)/real(q, dp))
        else
            idx = push(ir, OP_DIV, a=int_point(low, ir, p), b=int_point(low, ir, q))
        end if
    end function rational_value

    !> v * (p/q) with exact scalings where the rational allows it.
    function scaled(low, ir, v, p, q) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: v
        integer(int64), intent(in) :: p, q
        integer :: idx

        idx = v
        if (p == q) return
        if (p == -q) then
            idx = push(ir, OP_NEG, a=v)
        else if (abs(p) > EXACT_LIMIT .or. q > EXACT_LIMIT) then
            call fail(low, "rational coefficient is not exact in real64")
        else if (power_of_two(q)) then
            idx = push(ir, OP_SCALE, a=v, x=real(p, dp)/real(q, dp))
        else
            if (abs(p) /= 1_int64) idx = push(ir, OP_SCALE, a=v, x=real(p, dp))
            if (p == -1_int64) idx = push(ir, OP_NEG, a=v)
            idx = push(ir, OP_DIV, a=idx, b=int_point(low, ir, q))
        end if
    end function scaled

    !> Exact product of the numeric factors of a product node.
    logical function product_coefficient(low, id, p, q) result(ok)
        type(lowering_t), intent(in) :: low
        integer, intent(in) :: id
        integer(int64), intent(out) :: p, q
        integer(int64) :: ep, eq, g
        integer :: k

        ok = .true.
        p = 1_int64
        q = 1_int64
        do k = 1, low%arena%nargs_of(id)
            if (.not. numeric_node(low, low%arena%arg_of(id, k), ep, eq)) cycle
            if (ep == 0_int64) then
                p = 0_int64
                q = 1_int64
                return
            end if
            if (abs(p) > huge(p)/abs(ep) .or. q > huge(q)/eq) then
                ok = .false.
                return
            end if
            p = p*ep
            q = q*eq
            g = gcd64(abs(p), q)
            p = p/g
            q = q/g
        end do
    end function product_coefficient

    subroutine coefficient(low, id, p, q)
        type(lowering_t), intent(inout) :: low
        integer, intent(in) :: id
        integer(int64), intent(out) :: p, q

        if (.not. product_coefficient(low, id, p, q)) &
            call fail(low, "numeric coefficient overflows 64-bit integers")
    end subroutine coefficient

    pure integer(int64) function gcd64(a, b) result(g)
        integer(int64), intent(in) :: a, b
        integer(int64) :: x, y, t

        x = a
        y = b
        do while (y /= 0_int64)
            t = mod(x, y)
            x = y
            y = t
        end do
        g = max(x, 1_int64)
    end function gcd64

    logical function numeric_node(low, id, p, q)
        type(lowering_t), intent(in) :: low
        integer, intent(in) :: id
        integer(int64), intent(out) :: p, q

        numeric_node = .true.
        p = 0_int64
        q = 1_int64
        select case (low%arena%kind_of(id))
        case (NK_INT)
            p = low%arena%num_of(id)
        case (NK_RAT)
            p = low%arena%num_of(id)
            q = low%arena%den_of(id)
        case default
            numeric_node = .false.
        end select
    end function numeric_node

    recursive function lower_node(low, ir, id) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: id
        integer :: idx
        integer(int64) :: p, q, ep, eq
        integer :: k, base
        character(:), allocatable :: name

        idx = 0
        if (.not. low%ok) return
        if (id >= 1 .and. id <= size(low%memo)) then
            if (low%memo(id) > 0) then
                idx = low%memo(id)
                return
            end if
        end if
        select case (low%arena%kind_of(id))
        case (NK_SYM)
            name = chars(low%arena%name_of(id))
            do k = 1, size(low%args)
                if (chars(low%args(k)) == name) then
                    idx = push(ir, OP_ARG, n=k)
                    exit
                end if
            end do
            if (idx == 0) call fail(low, "free symbol "//name//" is not an argument")
        case (NK_INT, NK_RAT)
            if (numeric_node(low, id, p, q)) idx = rational_value(low, ir, p, q)
        case (NK_CONST)
            name = chars(low%arena%name_of(id))
            select case (name)
            case ("pi")
                ! |pi - 3.141592653589793| < 1.2247e-16
                idx = push(ir, OP_ENCLOSE, x=3.141592653589793_dp, y=1.3e-16_dp)
            case ("e")
                ! |e - 2.718281828459045| < 1.4457e-16
                idx = push(ir, OP_ENCLOSE, x=2.718281828459045_dp, y=1.5e-16_dp)
            case ("i")
                if (low%runtime_kind /= RUNTIME_BALL) then
                    call fail(low, "the imaginary unit needs a complex ball runtime")
                else
                    idx = push(ir, OP_CPOINT, x=0.0_dp, y=1.0_dp)
                end if
            case default
                call fail(low, "constant "//name//" has no enclosure")
            end select
        case (NK_ADD)
            idx = lower_sum(low, ir, id)
        case (NK_MUL)
            call coefficient(low, id, p, q)
            if (.not. low%ok) return
            idx = lower_product(low, ir, id, p, q)
        case (NK_POW)
            base = low%arena%arg_of(id, 1)
            if (.not. numeric_node(low, low%arena%arg_of(id, 2), ep, eq)) then
                call fail(low, "power with a non-numeric exponent")
                return
            end if
            idx = lower_power(low, ir, base, ep, eq)
        case (NK_FUNC)
            name = chars(low%arena%name_of(id))
            if (name == "sqrt" .and. low%arena%nargs_of(id) == 1) then
                k = lower_node(low, ir, low%arena%arg_of(id, 1))
                if (low%ok) idx = push(ir, OP_SQRT, a=k)
            else
                call fail(low, "function "//name//" is outside the runtime interface")
            end if
        case default
            call fail(low, "unsupported node kind "// &
                chars(node_kind_name(low%arena%kind_of(id)))// &
                " (decimal literals are refused; use exact rationals)")
        end select
        if (.not. low%ok) then
            idx = 0
            return
        end if
        if (id >= 1 .and. id <= size(low%memo)) low%memo(id) = idx
    end function lower_node

    !> base**(ep/eq) for eq = 1 (integer powers) or eq = 2 (square roots).
    recursive function lower_power(low, ir, base, ep, eq) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: base
        integer(int64), intent(in) :: ep, eq
        integer :: idx
        integer :: b
        integer(int64) :: m

        idx = 0
        if (eq /= 1_int64 .and. eq /= 2_int64) then
            call fail(low, "power with exponent outside integers and halves")
            return
        end if
        if (ep == 0_int64) then
            idx = push(ir, OP_POINT, x=1.0_dp)
            return
        end if
        if (abs(ep) > int(huge(1), int64)) then
            call fail(low, "exponent too large")
            return
        end if
        b = lower_node(low, ir, base)
        if (.not. low%ok) return
        if (eq == 2_int64) b = push(ir, OP_SQRT, a=b)
        m = abs(ep)
        idx = b
        if (m >= 2_int64) idx = push(ir, OP_POWI, a=b, n=int(m))
        if (ep < 0_int64) idx = push(ir, OP_INV, a=idx)
    end function lower_power

    !> Product of the non-numeric factors times p/q. Factors with negative
    !> exponents form one denominator, so a quotient costs one division.
    recursive function lower_product(low, ir, id, p, q) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: id
        integer(int64), intent(in) :: p, q
        integer :: idx
        integer :: k, a, t, num, den
        integer(int64) :: ep, eq, cp, cq

        idx = 0
        if (p == 0_int64) then
            idx = push(ir, OP_POINT, x=0.0_dp)
            return
        end if
        num = 0
        den = 0
        do k = 1, low%arena%nargs_of(id)
            a = low%arena%arg_of(id, k)
            if (numeric_node(low, a, ep, eq)) cycle
            t = 0
            if (low%arena%kind_of(a) == NK_POW) then
                if (numeric_node(low, low%arena%arg_of(a, 2), ep, eq)) then
                    if (ep < 0_int64) then
                        t = lower_power(low, ir, low%arena%arg_of(a, 1), -ep, eq)
                        if (.not. low%ok) return
                        if (den == 0) then
                            den = t
                        else
                            den = push(ir, OP_MUL, a=den, b=t)
                        end if
                        cycle
                    end if
                end if
            end if
            t = lower_node(low, ir, a)
            if (.not. low%ok) return
            if (num == 0) then
                num = t
            else
                num = push(ir, OP_MUL, a=num, b=t)
            end if
        end do
        cp = p
        cq = q
        if (num == 0 .and. den == 0) then
            idx = rational_value(low, ir, cp, cq)
            return
        end if
        if (den == 0) then
            idx = scaled(low, ir, num, cp, cq)
            return
        end if
        if (cq /= 1_int64 .and. .not. power_of_two(cq)) then
            if (cq > EXACT_LIMIT) then
                call fail(low, "rational coefficient is not exact in real64")
                return
            end if
            den = push(ir, OP_SCALE, a=den, x=real(cq, dp))
            cq = 1_int64
        end if
        if (num == 0) then
            num = rational_value(low, ir, cp, cq)
            idx = push(ir, OP_DIV, a=num, b=den)
        else
            idx = scaled(low, ir, push(ir, OP_DIV, a=num, b=den), cp, cq)
        end if
    end function lower_product

    !> A term's magnitude and sign, so sums use subtraction for negative terms.
    recursive subroutine signed_term(low, ir, t, v, negative)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: t
        integer, intent(out) :: v
        logical, intent(out) :: negative
        integer(int64) :: p, q, ep, eq
        integer :: k

        negative = .false.
        if (numeric_node(low, t, p, q)) then
            negative = p < 0_int64
            v = rational_value(low, ir, abs(p), q)
            return
        end if
        if (low%arena%kind_of(t) == NK_MUL) then
            call coefficient(low, t, p, q)
            if (.not. low%ok) return
            if (p < 0_int64) then
                negative = .true.
                v = lower_product(low, ir, t, -p, q)
                return
            end if
        end if
        v = lower_node(low, ir, t)
    end subroutine signed_term

    recursive function lower_sum(low, ir, id) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: id
        integer :: idx
        integer :: k, nt, start
        integer, allocatable :: v(:)
        logical, allocatable :: neg(:)

        idx = 0
        if (low%horner) then
            idx = try_horner(low, ir, id)
            if (idx > 0 .or. .not. low%ok) return
        end if
        nt = low%arena%nargs_of(id)
        allocate (v(nt), neg(nt))
        do k = 1, nt
            call signed_term(low, ir, low%arena%arg_of(id, k), v(k), neg(k))
            if (.not. low%ok) return
        end do
        start = 0
        do k = 1, nt
            if (.not. neg(k)) then
                start = k
                exit
            end if
        end do
        if (start == 0) then
            start = 1
            idx = push(ir, OP_NEG, a=v(1))
        else
            idx = v(start)
        end if
        do k = 1, nt
            if (k == start) cycle
            if (neg(k)) then
                idx = push(ir, OP_SUB, a=idx, b=v(k))
            else
                idx = push(ir, OP_ADD, a=idx, b=v(k))
            end if
        end do
    end function lower_sum

    !> Classify a sum term as c * base**d (d >= 0, exact rational c).
    logical function monomial(low, t, base, d, p, q)
        type(lowering_t), intent(in) :: low
        integer, intent(in) :: t
        integer, intent(out) :: base, d
        integer(int64), intent(out) :: p, q
        integer(int64) :: ep, eq
        integer :: k, x, nnum

        monomial = .false.
        base = 0
        d = 0
        p = 1_int64
        q = 1_int64
        if (numeric_node(low, t, p, q)) then
            monomial = .true.
            return
        end if
        p = 1_int64
        q = 1_int64
        x = t
        if (low%arena%kind_of(t) == NK_MUL) then
            x = 0
            nnum = 0
            do k = 1, low%arena%nargs_of(t)
                if (numeric_node(low, low%arena%arg_of(t, k), ep, eq)) cycle
                nnum = nnum + 1
                x = low%arena%arg_of(t, k)
            end do
            if (nnum /= 1) return
            if (.not. product_coefficient(low, t, p, q)) return
        end if
        select case (low%arena%kind_of(x))
        case (NK_MUL, NK_ADD)
            return
        case (NK_POW)
            if (numeric_node(low, low%arena%arg_of(x, 2), ep, eq)) then
                if (eq == 1_int64 .and. ep >= 1_int64 .and. ep <= 64_int64) then
                    base = low%arena%arg_of(x, 1)
                    d = int(ep)
                    monomial = .true.
                    return
                end if
            end if
        end select
        base = x
        d = 1
        monomial = .true.
    end function monomial

    !> Horner evaluation of a univariate polynomial sum in one base node.
    !> Returns 0 when the sum is not such a polynomial.
    recursive function try_horner(low, ir, id) result(idx)
        type(lowering_t), intent(inout) :: low
        type(rir_t), intent(inout) :: ir
        integer, intent(in) :: id
        integer :: idx
        integer :: nt, k, base, b, d, maxdeg, nonconst, x
        integer(int64) :: p, q
        integer(int64), allocatable :: cp(:), cq(:)
        logical, allocatable :: have(:)

        idx = 0
        nt = low%arena%nargs_of(id)
        base = 0
        maxdeg = 0
        nonconst = 0
        allocate (cp(0:64), cq(0:64), have(0:64))
        cp = 0_int64
        cq = 1_int64
        have = .false.
        do k = 1, nt
            if (.not. monomial(low, low%arena%arg_of(id, k), b, d, p, q)) return
            if (d > 0) then
                if (base == 0) then
                    base = b
                else if (base /= b) then
                    return
                end if
                nonconst = nonconst + 1
            end if
            if (have(d)) return
            have(d) = .true.
            cp(d) = p
            cq(d) = q
            maxdeg = max(maxdeg, d)
        end do
        if (nonconst < 2 .or. maxdeg < 2) return
        x = lower_node(low, ir, base)
        if (.not. low%ok) return
        idx = scaled(low, ir, x, cp(maxdeg), cq(maxdeg))
        do k = maxdeg - 1, 0, -1
            if (have(k)) then
                if (cp(k) < 0_int64) then
                    idx = push(ir, OP_SUB, a=idx, b=rational_value(low, ir, -cp(k), cq(k)))
                else
                    idx = push(ir, OP_ADD, a=idx, b=rational_value(low, ir, cp(k), cq(k)))
                end if
            end if
            if (k > 0) idx = push(ir, OP_MUL, a=idx, b=x)
        end do
        if (.not. low%ok) idx = 0
    end function try_horner

    subroutine complex_flags(ir, spec, cplx)
        type(rir_t), intent(in) :: ir
        type(rigorous_kernel_spec_t), intent(in) :: spec
        logical, allocatable, intent(out) :: cplx(:)
        integer :: k

        allocate (cplx(ir%n), source=.false.)
        do k = 1, ir%n
            select case (ir%ops(k)%op)
            case (OP_ARG)
                if (allocated(spec%complex_args)) cplx(k) = spec%complex_args(ir%ops(k)%n)
            case (OP_CPOINT)
                cplx(k) = .true.
            case (OP_POINT, OP_ENCLOSE)
                cplx(k) = .false.
            case default
                if (ir%ops(k)%a > 0) cplx(k) = cplx(ir%ops(k)%a)
                if (ir%ops(k)%b > 0) cplx(k) = cplx(k) .or. cplx(ir%ops(k)%b)
            end select
        end do
    end subroutine complex_flags

    function render(ir, spec, rigorous) result(source)
        type(rir_t), intent(in) :: ir
        type(rigorous_kernel_spec_t), intent(in) :: spec
        logical, intent(in) :: rigorous
        type(str_t) :: source
        type(strbuf_t) :: b
        character(:), allocatable :: prefix, kind_in, kind_out, kind_tmp
        type(str_t), allocatable :: names(:)
        logical, allocatable :: cplx(:), used(:)
        integer :: k, op

        prefix = chars(spec%temp_prefix)
        if (len(prefix) == 0) prefix = "t"
        call complex_flags(ir, spec, cplx)
        call b%append("! Generated by fortsym. Do not edit.")
        call b%newline()
        if (len(chars(spec%generator)) > 0) then
            call b%append("! Generator: "//chars(spec%generator))
            call b%newline()
        end if
        if (rigorous) then
            call b%append("! Rigorous leaf: results enclose the exact values.")
        else
            call b%append("! Floating-point leaf of the same operation list.")
        end if
        call b%newline()
        if (spec%elemental_procedure) then
            call b%append("elemental ")
        else if (spec%pure_procedure) then
            call b%append("pure ")
        end if
        call b%append("subroutine "//chars(spec%name)//"(")
        do k = 1, size(spec%args)
            call b%append(chars(spec%args(k))//", ")
        end do
        do k = 1, size(spec%outputs)
            call b%append(chars(spec%outputs(k)))
            if (k < size(spec%outputs)) call b%append(", ")
        end do
        call b%append(")")
        call b%newline()
        call b%append("    use, intrinsic :: iso_fortran_env, only: real64")
        call b%newline()
        if (rigorous) then
            allocate (used(13), source=.false.)
            do k = 1, ir%n
                if (ir%ops(k)%op /= OP_ARG) used(ir%ops(k)%op) = .true.
            end do
            allocate (names(0))
            names = [names, spec%runtime%type_name]
            do op = OP_POINT, OP_SCALE
                if (used(op)) names = [names, str(runtime_name(spec%runtime, op))]
            end do
            call append_list(b, "    use "//chars(spec%runtime%module_name)//", only: ", &
                names)
            deallocate (names)
        end if
        call b%append("    implicit none")
        call b%newline()
        if (rigorous) then
            kind_in = "type("//chars(spec%runtime%type_name)//")"
            kind_out = kind_in
        else
            kind_out = "real(real64)"
            if (spec%complex_outputs) kind_out = "complex(real64)"
        end if
        do k = 1, size(spec%args)
            if (.not. rigorous) then
                kind_in = "real(real64)"
                if (allocated(spec%complex_args)) then
                    if (spec%complex_args(k)) kind_in = "complex(real64)"
                end if
            end if
            call b%append("    "//kind_in//", intent(in) :: "//chars(spec%args(k)))
            call b%newline()
        end do
        call append_list(b, "    "//kind_out//", intent(out) :: ", spec%outputs)
        do op = 1, 2
            allocate (names(0))
            do k = 1, ir%n
                if (ir%ops(k)%op == OP_ARG) cycle
                if (rigorous .and. op == 2) cycle
                if (.not. rigorous .and. (cplx(k) .neqv. (op == 2))) cycle
                names = [names, str(prefix//itoa(k))]
            end do
            if (size(names) > 0) then
                if (rigorous) then
                    kind_tmp = "type("//chars(spec%runtime%type_name)//")"
                else if (op == 2) then
                    kind_tmp = "complex(real64)"
                else
                    kind_tmp = "real(real64)"
                end if
                call append_list(b, "    "//kind_tmp//" :: ", names)
            end if
            deallocate (names)
        end do
        call b%newline()
        do k = 1, ir%n
            if (ir%ops(k)%op == OP_ARG) cycle
            call b%append("    "//prefix//itoa(k)//" = "// &
                op_text(ir, k, spec, prefix, rigorous))
            call b%newline()
        end do
        do k = 1, size(spec%outputs)
            call b%append("    "//chars(spec%outputs(k))//" = "// &
                ref(ir, ir%outputs(k), spec, prefix))
            call b%newline()
        end do
        call b%append("end subroutine "//chars(spec%name))
        call b%newline()
        source = b%to_str()
    end function render

    !> "head a, b, c" with at most six names per line.
    subroutine append_list(b, head, names)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: head
        type(str_t), intent(in) :: names(:)
        integer :: k

        call b%append(head)
        do k = 1, size(names)
            call b%append(chars(names(k)))
            if (k < size(names)) then
                call b%append(", ")
                if (mod(k, 6) == 0) then
                    call b%append("&")
                    call b%newline()
                    call b%append("        ")
                end if
            end if
        end do
        call b%newline()
    end subroutine append_list

    function ref(ir, k, spec, prefix) result(text)
        type(rir_t), intent(in) :: ir
        integer, intent(in) :: k
        type(rigorous_kernel_spec_t), intent(in) :: spec
        character(*), intent(in) :: prefix
        character(:), allocatable :: text

        if (ir%ops(k)%op == OP_ARG) then
            text = chars(spec%args(ir%ops(k)%n))
        else
            text = prefix//itoa(k)
        end if
    end function ref

    function op_text(ir, k, spec, prefix, rigorous) result(text)
        type(rir_t), intent(in) :: ir
        integer, intent(in) :: k
        type(rigorous_kernel_spec_t), intent(in) :: spec
        character(*), intent(in) :: prefix
        logical, intent(in) :: rigorous
        character(:), allocatable :: text
        character(:), allocatable :: a, b, fn
        type(rop_t) :: r

        r = ir%ops(k)
        a = ""
        b = ""
        if (r%a > 0) a = ref(ir, r%a, spec, prefix)
        if (r%b > 0) b = ref(ir, r%b, spec, prefix)
        if (rigorous) then
            fn = runtime_name(spec%runtime, r%op)
            select case (r%op)
            case (OP_POINT)
                text = fn//"("//literal(r%x)//")"
            case (OP_CPOINT, OP_ENCLOSE)
                text = fn//"("//literal(r%x)//", "//literal(r%y)//")"
            case (OP_ADD, OP_SUB, OP_MUL, OP_DIV)
                text = fn//"("//a//", "//b//")"
            case (OP_NEG, OP_INV, OP_SQRT)
                text = fn//"("//a//")"
            case (OP_POWI)
                text = fn//"("//a//", "//itoa(r%n)//")"
            case (OP_SCALE)
                text = fn//"("//a//", "//literal(r%x)//")"
            case default
                text = a
            end select
            return
        end if
        select case (r%op)
        case (OP_POINT, OP_ENCLOSE)
            text = literal(r%x)
        case (OP_CPOINT)
            text = "cmplx("//literal(r%x)//", "//literal(r%y)//", real64)"
        case (OP_ADD)
            text = a//" + "//b
        case (OP_SUB)
            text = a//" - "//b
        case (OP_MUL)
            text = a//"*"//b
        case (OP_DIV)
            text = a//"/"//b
        case (OP_NEG)
            text = "-"//a
        case (OP_INV)
            text = "1.0_real64/"//a
        case (OP_SQRT)
            text = "sqrt("//a//")"
        case (OP_POWI)
            text = a//"**"//itoa(r%n)
        case (OP_SCALE)
            text = a//"*("//literal(r%x)//")"
        case default
            text = a
        end select
    end function op_text

    !> Exact real64 literal: integers in integer spelling, everything else with
    !> 17 significant digits, which read back to the same double.
    function literal(x) result(text)
        real(dp), intent(in) :: x
        character(:), allocatable :: text
        character(len=40) :: buf

        if (abs(x) < 9.0e15_dp .and. x == aint(x)) then
            write (buf, '(i0)') int(x, int64)
            text = trim(buf)//".0_real64"
        else
            write (buf, '(es25.17e3)') x
            text = trim(adjustl(buf))//"_real64"
        end if
    end function literal

    pure function itoa(n) result(text)
        integer, intent(in) :: n
        character(:), allocatable :: text
        character(len=16) :: buf

        write (buf, '(i0)') n
        text = trim(buf)
    end function itoa

end module fortsym_rigorous_emit
