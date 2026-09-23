module fortsym_kernel_taylor
    ! Order-by-order Taylor-series (SSA) emission.
    !
    ! A truncated power series y = sum_k y_k t^k is advanced one coefficient
    ! at a time by a small set of well-known recurrences (Moore 1966;
    ! standard in the automatic-differentiation literature):
    !     c = a*b:  c_k = sum_{j=0}^{k} a_j b_{k-j}
    !     c = a/b:  c_k = (a_k - sum_{j=1}^{k} b_j c_{k-j}) / b_0
    !     s = sin(a), c = cos(a):
    !         s_k = (1/k) sum_{j=1}^{k} j a_j c_{k-j}
    !         c_k = -(1/k) sum_{j=1}^{k} j a_j s_{k-j}      (k >= 1; s_0=sin(a_0),
    !                                                         c_0=cos(a_0))
    !     e = exp(a):
    !         e_k = (1/k) sum_{j=1}^{k} j a_j e_{k-j}        (k >= 1; e_0=exp(a_0))
    ! and coefficient k of every other elementary operation (+, -, scaling by
    ! a constant) is linear: (a+b)_k = a_k + b_k, (c*a)_k = c*a_k for a true
    ! constant c. This module emits a single Fortran subroutine that computes
    ! coefficient k of an expression DAG given coefficient k (and, through
    ! caller-owned series arrays, coefficients 0..k-1) of every input, by
    ! calling caller-named building-block subroutines for the nonlinear
    ! recurrences and inlining the linear ones directly. It does not emit the
    ! outer order loop or own the series storage: a downstream generator or
    ! hand-written driver calls the emitted subroutine for k = 0, 1, 2, ...,
    ! owns the series arrays (including this kernel's own temporaries, passed
    ! through so their history survives between calls), and supplies mul_name,
    ! div_name, sincos_name, and exp_name as real subroutines with the above
    ! recurrences -- fortsym_kernel_taylor never assumes a particular
    ! numeric representation for them.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortsym_string, only: str_t, strbuf_t, str, chars, operator(//)
    use fortsym_expr, only: expr_t
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir, &
        IR_LITERAL, IR_SYMBOL, IR_CONSTANT, IR_ADD, IR_MUL, IR_POW, IR_FUNCTION
    implicit none
    private

    public :: taylor_emit_spec_t, emit_taylor_step

    integer, parameter :: dp = real64

    !> Everything needed to render one Taylor-series order-k step subroutine.
    type :: taylor_emit_spec_t
        type(str_t) :: name
        !> Input series array names, in argument order. Each represents the
        !> full history 0..k of one symbol.
        type(str_t), allocatable :: args(:)
        !> Output series array names, one per root expression.
        type(str_t), allocatable :: outputs(:)
        !> Prefix for generated temporary series arrays. Defaults to "s".
        type(str_t) :: temp_prefix
        !> Caller-supplied elementary recurrence names. Defaults below.
        type(str_t) :: mul_name
        type(str_t) :: div_name
        type(str_t) :: sincos_name
        type(str_t) :: exp_name
        !> Empty (default): every series array is declared "real(dp),
        !> intent(inout) :: name(0:*)", exactly as before this field
        !> existed. Nonempty: the coefficient element type is instead
        !> "type(<type_name>)", e.g. a caller-supplied complex-dual or
        !> interval number type that overloads +, -, *, / and whichever
        !> elementary functions mul_name/div_name/sincos_name/exp_name and
        !> the emitted `**` need over that type.
        type(str_t) :: type_name
        !> Empty (default): real literals are emitted bare (e.g. "2.0_dp"),
        !> exactly as before this field existed. Nonempty: every literal
        !> (including the "0.0_dp"/"1.0_dp" identities the merge-seed and
        !> constant-series contributions need) is instead wrapped in a call
        !> to this constructor name, e.g. "dual_t(2.0_dp)".
        type(str_t) :: literal_constructor
    end type taylor_emit_spec_t

    !> How to refer to one IR node's coefficient k (scalar) and, once
    !> materialised, its backing series array (needed only when the node is
    !> passed whole to a mul/div/sincos/exp call).
    type :: noderef_t
        !> Coefficient-k text: a series index expression such as "x(k)" or
        !> "t7(k)", or, for a literal, "merge(c, 0, k == 0)" -- the correct
        !> contribution of a *constant series* to an additive sum at order k.
        type(str_t) :: scalar
        type(str_t) :: array
        logical :: has_array = .false.
        !> True for a literal/constant leaf. Its raw value (not the merge
        !> form above) is what a true scalar-times-series multiplication
        !> needs, since that scale factor multiplies every order, not only
        !> order 0.
        logical :: is_literal = .false.
        type(str_t) :: literal_raw
    end type noderef_t

contains

    !> Emit `subroutine <name>(k, args..., outputs..., temps...)` computing
    !> coefficient k of every root, where args/outputs/temps are all
    !> intent(inout) real(dp) series arrays owned by the caller (temps carry
    !> their history between successive calls at k = 0, 1, 2, ...). Supported
    !> operations: +, -, *, / (through a reciprocal), positive and negative
    !> integer powers, sin, cos (jointly), and exp. Anything else, including
    !> a non-integer power, is refused rather than silently approximated.
    function emit_taylor_step(roots, spec, ok, message) result(source)
        type(expr_t), intent(in) :: roots(:)
        type(taylor_emit_spec_t), intent(in) :: spec
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: source

        type(kernel_ir_t) :: ir
        type(noderef_t), allocatable :: ref(:)
        type(str_t), allocatable :: temp_names(:)
        integer :: n_temps, synth_id
        logical :: needs_one
        type(str_t) :: one_name
        type(strbuf_t) :: body
        integer :: k

        source = str("")
        ok = .false.
        message = ""
        if (.not. allocated(spec%args) .or. size(spec%args) == 0) then
            message = "emit_taylor_step: no argument names supplied"
            return
        end if
        if (.not. allocated(spec%outputs) .or. size(spec%outputs) /= size(roots)) then
            message = "emit_taylor_step: outputs must have one name per root"
            return
        end if

        call lower_kernel_ir(roots, ir, ok, message)
        if (.not. ok) return

        allocate (ref(ir%n_nodes))
        allocate (temp_names(0))
        n_temps = 0
        synth_id = 0
        needs_one = has_negative_power(ir)
        one_name = str("")
        if (needs_one) then
            one_name = temp_prefix_of(spec)//str("one")
            call body%append("    ")
            call body%append(one_name)
            call body%append("(k) = ")
            call body%append(merge_text(1.0_dp, spec))
            call body%newline()
        end if

        do k = 1, ir%n_nodes
            call render_taylor_node(ir, k, spec, ref, temp_names, n_temps, &
                synth_id, one_name, body, ok, message)
            if (.not. ok) return
        end do

        block
            integer :: j
            do j = 1, size(spec%outputs)
                call body%append("    ")
                call body%append(spec%outputs(j))
                call body%append("(k) = ")
                call body%append(ref(ir%outputs(j))%scalar)
                call body%newline()
            end do
        end block

        source = assemble_taylor(spec, temp_names, needs_one, one_name, body)
        ok = .true.
    end function emit_taylor_step

    subroutine render_taylor_node(ir, idx, spec, ref, temp_names, n_temps, &
            synth_id, one_name, body, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: idx
        type(taylor_emit_spec_t), intent(in) :: spec
        type(noderef_t), intent(inout) :: ref(:)
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps, synth_id
        type(str_t), intent(in) :: one_name
        type(strbuf_t), intent(inout) :: body
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        integer :: op, j, base_idx, exp_idx

        ok = .true.
        message = ""
        op = ir%nodes(idx)%operation

        select case (op)
        case (IR_SYMBOL)
            call find_arg_index(spec, chars(ir%nodes(idx)%name), j, ok)
            if (.not. ok) then
                message = "emit_taylor_step: symbol not in argument list: "// &
                    chars(ir%nodes(idx)%name)
                return
            end if
            ref(idx)%array = spec%args(j)
            ref(idx)%scalar = spec%args(j)//str("(k)")
            ref(idx)%has_array = .true.
            return
        case (IR_LITERAL, IR_CONSTANT)
            ref(idx)%scalar = merge_text(ir%nodes(idx)%value, spec)
            ref(idx)%has_array = .false.
            ref(idx)%is_literal = .true.
            ref(idx)%literal_raw = raw_text(ir%nodes(idx)%value, spec)
            return
        end select

        select case (op)
        case (IR_ADD)
            call render_add(ir, idx, spec, ref, temp_names, n_temps, body)
        case (IR_MUL)
            call render_mul(ir, idx, spec, ref, temp_names, n_temps, synth_id, &
                body)
        case (IR_POW)
            base_idx = ir%operands(ir%nodes(idx)%first_operand)
            exp_idx = ir%operands(ir%nodes(idx)%first_operand + 1)
            call render_pow(ir, idx, base_idx, exp_idx, spec, ref, temp_names, &
                n_temps, synth_id, one_name, body, ok, message)
        case (IR_FUNCTION)
            call render_function(ir, idx, spec, ref, temp_names, n_temps, &
                synth_id, body, ok, message)
        case default
            ok = .false.
            message = "emit_taylor_step: unsupported IR operation"
        end select
    end subroutine render_taylor_node

    subroutine render_add(ir, idx, spec, ref, temp_names, n_temps, body)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: idx
        type(taylor_emit_spec_t), intent(in) :: spec
        type(noderef_t), intent(inout) :: ref(:)
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps
        type(strbuf_t), intent(inout) :: body
        type(str_t) :: name, text
        integer :: j, child

        name = temp_prefix_of(spec)//str(idx)
        call append_temp_name(temp_names, n_temps, name)
        text = str("")
        do j = 1, ir%nodes(idx)%n_operands
            child = ir%operands(ir%nodes(idx)%first_operand + j - 1)
            if (j > 1) text = text//str(" + ")
            text = text//str("(")//ref(child)%scalar//str(")")
        end do
        call body%append("    ")
        call body%append(name)
        call body%append("(k) = ")
        call body%append(text)
        call body%newline()
        ref(idx)%array = name
        ref(idx)%scalar = name//str("(k)")
        ref(idx)%has_array = .true.
    end subroutine render_add

    subroutine render_mul(ir, idx, spec, ref, temp_names, n_temps, synth_id, &
            body)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: idx
        type(taylor_emit_spec_t), intent(in) :: spec
        type(noderef_t), intent(inout) :: ref(:)
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps, synth_id
        type(strbuf_t), intent(inout) :: body
        type(noderef_t) :: acc, result
        integer :: j, child

        child = ir%operands(ir%nodes(idx)%first_operand)
        acc = ref(child)
        do j = 2, ir%nodes(idx)%n_operands
            child = ir%operands(ir%nodes(idx)%first_operand + j - 1)
            call multiply_step(acc, ref(child), spec, temp_names, n_temps, &
                synth_id, body, result)
            acc = result
        end do
        ref(idx) = acc
    end subroutine render_mul

    !> One pairwise product. A pure-constant operand (no backing array) is
    !> handled by direct scalar scaling, which is exact and needs no call:
    !> (c*a)_k = c*a_k for a true constant c. Two series call mul_name.
    subroutine multiply_step(a, b, spec, temp_names, n_temps, synth_id, body, &
            result)
        type(noderef_t), intent(in) :: a, b
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps, synth_id
        type(strbuf_t), intent(inout) :: body
        type(noderef_t), intent(out) :: result
        type(str_t) :: name

        synth_id = synth_id + 1
        name = temp_prefix_of(spec)//str("x")//str(synth_id)
        call append_temp_name(temp_names, n_temps, name)
        if (.not. a%has_array .or. .not. b%has_array) then
            call body%append("    ")
            call body%append(name)
            call body%append("(k) = (")
            call body%append(scale_text(a))
            call body%append(") * (")
            call body%append(scale_text(b))
            call body%append(")")
        else
            call body%append("    call ")
            call body%append(spec%mul_name)
            call body%append("(k, ")
            call body%append(a%array)
            call body%append(", ")
            call body%append(b%array)
            call body%append(", ")
            call body%append(name)
            call body%append(")")
        end if
        call body%newline()
        result%array = name
        result%scalar = name//str("(k)")
        result%has_array = .true.
    end subroutine multiply_step

    subroutine render_pow(ir, idx, base_idx, exp_idx, spec, ref, temp_names, &
            n_temps, synth_id, one_name, body, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: idx, base_idx, exp_idx
        type(taylor_emit_spec_t), intent(in) :: spec
        type(noderef_t), intent(inout) :: ref(:)
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps, synth_id
        type(str_t), intent(in) :: one_name
        type(strbuf_t), intent(inout) :: body
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        real(dp) :: expval
        integer :: n, i
        type(noderef_t) :: acc, result
        type(str_t) :: name

        ok = .true.
        message = ""
        if (ir%nodes(exp_idx)%operation /= IR_LITERAL) then
            ok = .false.
            message = "emit_taylor_step: only a literal integer exponent is supported"
            return
        end if
        expval = ir%nodes(exp_idx)%value
        if (abs(expval - nint(expval)) > 1.0e-9_dp) then
            ok = .false.
            message = "emit_taylor_step: only integer powers are supported"
            return
        end if
        n = nint(expval)
        if (n == 0) then
            ref(idx)%scalar = merge_text(1.0_dp, spec)
            ref(idx)%has_array = .false.
            ref(idx)%is_literal = .true.
            ref(idx)%literal_raw = raw_text(1.0_dp, spec)
            return
        end if
        if (n == 1) then
            ref(idx) = ref(base_idx)
            return
        end if
        acc = ref(base_idx)
        do i = 2, abs(n)
            call multiply_step(acc, ref(base_idx), spec, temp_names, n_temps, &
                synth_id, body, result)
            acc = result
        end do
        if (n > 0) then
            ref(idx) = acc
            return
        end if
        synth_id = synth_id + 1
        name = temp_prefix_of(spec)//str("x")//str(synth_id)
        call append_temp_name(temp_names, n_temps, name)
        call body%append("    call ")
        call body%append(spec%div_name)
        call body%append("(k, ")
        call body%append(one_name)
        call body%append(", ")
        call body%append(acc%array)
        call body%append(", ")
        call body%append(name)
        call body%append(")")
        call body%newline()
        ref(idx)%array = name
        ref(idx)%scalar = name//str("(k)")
        ref(idx)%has_array = .true.
    end subroutine render_pow

    subroutine ensure_array(r, spec, temp_names, n_temps, synth_id, body)
        type(noderef_t), intent(inout) :: r
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps, synth_id
        type(strbuf_t), intent(inout) :: body
        type(str_t) :: name

        if (r%has_array) return
        synth_id = synth_id + 1
        name = temp_prefix_of(spec)//str("x")//str(synth_id)
        call append_temp_name(temp_names, n_temps, name)
        call body%append("    ")
        call body%append(name)
        call body%append("(k) = ")
        call body%append(r%scalar)
        call body%newline()
        r%array = name
        r%scalar = name//str("(k)")
        r%has_array = .true.
    end subroutine ensure_array

    subroutine render_function(ir, idx, spec, ref, temp_names, n_temps, &
            synth_id, body, ok, message)
        type(kernel_ir_t), intent(in) :: ir
        integer, intent(in) :: idx
        type(taylor_emit_spec_t), intent(in) :: spec
        type(noderef_t), intent(inout) :: ref(:)
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps, synth_id
        type(strbuf_t), intent(inout) :: body
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: message
        type(str_t) :: fname, name, sname, cname
        integer :: arg_idx
        type(noderef_t) :: argref

        ok = .true.
        message = ""
        fname = ir%nodes(idx)%name
        arg_idx = ir%operands(ir%nodes(idx)%first_operand)
        argref = ref(arg_idx)
        call ensure_array(argref, spec, temp_names, n_temps, synth_id, body)

        select case (chars(fname))
        case ("sin", "cos")
            synth_id = synth_id + 1
            sname = temp_prefix_of(spec)//str("x")//str(synth_id)//str("s")
            cname = temp_prefix_of(spec)//str("x")//str(synth_id)//str("c")
            call append_temp_name(temp_names, n_temps, sname)
            call append_temp_name(temp_names, n_temps, cname)
            call body%append("    call ")
            call body%append(spec%sincos_name)
            call body%append("(k, ")
            call body%append(argref%array)
            call body%append(", ")
            call body%append(sname)
            call body%append(", ")
            call body%append(cname)
            call body%append(")")
            call body%newline()
            if (chars(fname) == "sin") then
                ref(idx)%array = sname
                ref(idx)%scalar = sname//str("(k)")
            else
                ref(idx)%array = cname
                ref(idx)%scalar = cname//str("(k)")
            end if
            ref(idx)%has_array = .true.
        case ("exp")
            synth_id = synth_id + 1
            name = temp_prefix_of(spec)//str("x")//str(synth_id)
            call append_temp_name(temp_names, n_temps, name)
            call body%append("    call ")
            call body%append(spec%exp_name)
            call body%append("(k, ")
            call body%append(argref%array)
            call body%append(", ")
            call body%append(name)
            call body%append(")")
            call body%newline()
            ref(idx)%array = name
            ref(idx)%scalar = name//str("(k)")
            ref(idx)%has_array = .true.
        case default
            ok = .false.
            message = "emit_taylor_step: unsupported function for Taylor "// &
                "SSA emission: "//chars(fname)
        end select
    end subroutine render_function

    function has_negative_power(ir) result(yes)
        type(kernel_ir_t), intent(in) :: ir
        logical :: yes
        integer :: i, exp_idx

        yes = .false.
        do i = 1, ir%n_nodes
            if (ir%nodes(i)%operation /= IR_POW) cycle
            exp_idx = ir%operands(ir%nodes(i)%first_operand + 1)
            if (ir%nodes(exp_idx)%operation /= IR_LITERAL) cycle
            if (ir%nodes(exp_idx)%value < 0.0_dp) then
                yes = .true.
                return
            end if
        end do
    end function has_negative_power

    function temp_prefix_of(spec) result(prefix)
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t) :: prefix

        prefix = spec%temp_prefix
        if (prefix%is_empty()) prefix = str("s")
    end function temp_prefix_of

    !> The k==0-only contribution of a constant series to an additive sum,
    !> generalised to spec%type_name/literal_constructor: "merge(<value>,
    !> <zero>, k == 0)", with both the value and the zero built through
    !> typed_literal/zero_text so a non-default element type gets a real
    !> zero of its own type rather than a bare "0.0_dp" it may not accept.
    function merge_text(value, spec) result(text)
        real(dp), intent(in) :: value
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t) :: text

        text = str("merge(")//typed_literal(value, spec)//str(", ")// &
            zero_text(spec)//str(", k == 0)")
    end function merge_text

    !> "real(dp)" (default) or "type(<type_name>)": the element type every
    !> series array (args, outputs, temporaries, the constant-one series)
    !> is declared with.
    function elem_type_text(spec) result(text)
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t) :: text

        if (spec%type_name%is_empty()) then
            text = str("real(dp)")
        else
            text = str("type(")//spec%type_name//str(")")
        end if
    end function elem_type_text

    !> A literal value, bare (default) or wrapped in literal_constructor.
    !> Shared by merge_text (the nonzero side) and raw_text.
    function typed_literal(value, spec) result(text)
        real(dp), intent(in) :: value
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t) :: text
        character(32) :: buf

        write (buf, '(ES24.16E3)') value
        if (spec%literal_constructor%is_empty()) then
            text = str(trim(adjustl(buf))//"_dp")
        else
            text = spec%literal_constructor//str("(")// &
                str(trim(adjustl(buf))//"_dp")//str(")")
        end if
    end function typed_literal

    !> The additive/multiplicative-identity zero literal. Default emits the
    !> exact "0.0_dp" merge_text always used before this field existed
    !> (not typed_literal(0.0_dp, ...), which would reformat it through
    !> ES24.16E3 and change the default emission); a nonempty
    !> literal_constructor wraps it the same way as any other literal.
    function zero_text(spec) result(text)
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t) :: text

        if (spec%literal_constructor%is_empty()) then
            text = str("0.0_dp")
        else
            text = spec%literal_constructor//str("(0.0_dp)")
        end if
    end function zero_text

    !> The text to use when this node is one factor of a scalar-times-series
    !> multiplication: a true constant's raw value (multiplies every order),
    !> or an ordinary node's own order-k coefficient otherwise.
    function scale_text(r) result(text)
        type(noderef_t), intent(in) :: r
        type(str_t) :: text

        if (r%is_literal) then
            text = r%literal_raw
        else
            text = r%scalar
        end if
    end function scale_text

    !> The literal's own value, undecorated by order -- what a true
    !> scalar-times-series scaling needs, as opposed to merge_text's
    !> per-order constant-series contribution.
    function raw_text(value, spec) result(text)
        real(dp), intent(in) :: value
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t) :: text

        text = typed_literal(value, spec)
    end function raw_text

    subroutine find_arg_index(spec, name, j, ok)
        type(taylor_emit_spec_t), intent(in) :: spec
        character(*), intent(in) :: name
        integer, intent(out) :: j
        logical, intent(out) :: ok

        ok = .false.
        do j = 1, size(spec%args)
            if (chars(spec%args(j)) == name) then
                ok = .true.
                return
            end if
        end do
    end subroutine find_arg_index

    subroutine append_temp_name(temp_names, n_temps, name)
        type(str_t), allocatable, intent(inout) :: temp_names(:)
        integer, intent(inout) :: n_temps
        type(str_t), intent(in) :: name

        temp_names = [temp_names, name]
        n_temps = n_temps + 1
    end subroutine append_temp_name

    function assemble_taylor(spec, temp_names, needs_one, one_name, body) &
            result(source)
        type(taylor_emit_spec_t), intent(in) :: spec
        type(str_t), intent(in) :: temp_names(:)
        logical, intent(in) :: needs_one
        type(str_t), intent(in) :: one_name
        type(strbuf_t), intent(inout) :: body
        type(str_t) :: source
        type(strbuf_t) :: out
        integer :: k

        call out%append("subroutine ")
        call out%append(spec%name)
        call out%append("(k")
        do k = 1, size(spec%args)
            call out%append(", ")
            call out%append(spec%args(k))
        end do
        do k = 1, size(spec%outputs)
            call out%append(", ")
            call out%append(spec%outputs(k))
        end do
        if (needs_one) then
            call out%append(", ")
            call out%append(one_name)
        end if
        do k = 1, size(temp_names)
            call out%append(", ")
            call out%append(temp_names(k))
        end do
        call out%append(")")
        call out%newline()
        call out%append("    integer, intent(in) :: k")
        call out%newline()
        do k = 1, size(spec%args)
            call out%append("    ")
            call out%append(elem_type_text(spec))
            call out%append(", intent(inout) :: ")
            call out%append(spec%args(k))
            call out%append("(0:*)")
            call out%newline()
        end do
        do k = 1, size(spec%outputs)
            call out%append("    ")
            call out%append(elem_type_text(spec))
            call out%append(", intent(inout) :: ")
            call out%append(spec%outputs(k))
            call out%append("(0:*)")
            call out%newline()
        end do
        if (needs_one) then
            call out%append("    ")
            call out%append(elem_type_text(spec))
            call out%append(", intent(inout) :: ")
            call out%append(one_name)
            call out%append("(0:*)")
            call out%newline()
        end if
        do k = 1, size(temp_names)
            call out%append("    ")
            call out%append(elem_type_text(spec))
            call out%append(", intent(inout) :: ")
            call out%append(temp_names(k))
            call out%append("(0:*)")
            call out%newline()
        end do
        call out%append(body%to_str())
        call out%append("end subroutine ")
        call out%append(spec%name)
        call out%newline()
        source = out%to_str()
    end function assemble_taylor

end module fortsym_kernel_taylor
