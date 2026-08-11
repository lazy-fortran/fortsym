module fortsym_print
    ! Rendering an expression as text in a chosen dialect.
    !
    ! The arena stores a normalised structure: subtraction is addition of a
    ! negation, division is multiplication by a reciprocal power, and operands
    ! of sums and products are sorted. That is right for interning and wrong for
    ! reading, so the printer undoes it -- a product whose factors include
    ! x**(-1) prints as a quotient, a term with a negative coefficient prints
    ! with a minus sign instead of "+ -1*", and a leading -1 factor prints as
    ! unary minus.
    !
    ! Parenthesisation is driven by operator precedence, not by wrapping
    ! everything. Redundant parentheses in generated Fortran are not merely
    ! ugly: they defeat the round-trip test that keeps printer and parser in
    ! agreement, and they make emitted kernels hard to review against the
    ! mathematics they came from.
    use, intrinsic :: iso_fortran_env, only: int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortsym_string, only: str_t, strbuf_t, str, chars
    use fortsym_arena, only: arena_t, NK_INT, NK_RAT, NK_REAL, NK_SYM, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, &
        NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_expr, only: expr_t
    use fortsym_exact, only: exact_to_real
    use fortsym_names, only: valid_fortran_symbol, same_fortran_name
    use fortsym_dialect, only: dialect_t, dialect, fn_spelling, const_spelling, &
        DIA_NATIVE, DIA_FORTRAN, DIA_WOLFRAM, fortran_function_supported, &
        fortran_function_arity_ok
    implicit none
    private

    public :: print_expr, print_expr_in, print_expr_sub, print_expr_latex
    public :: fortran_representable
    public :: fortran_roots_representable, fortran_roots_representable_kind
    public :: kernel_binding_t

    integer, parameter :: dp = real64

    !> Replacement for an applied function in generated Fortran.  An empty
    !> derivative_indices list binds the ordinary applied function; a nonempty
    !> list binds the canonical DerivativeN node produced by partial_derivative.
    !> The optional `%args%` marker in replacement is expanded to the original
    !> call arguments, which permits both component expressions and procedure
    !> calls to share one binding table.
    type :: kernel_binding_t
        type(str_t)              :: head
        integer, allocatable     :: derivative_indices(:)
        type(str_t)              :: replacement
    end type kernel_binding_t

    ! Binding powers. A child is parenthesised when its own precedence is lower
    ! than the context it appears in.
    integer, parameter :: PREC_ADD = 1
    integer, parameter :: PREC_MUL = 2
    integer, parameter :: PREC_POW = 3
    integer, parameter :: PREC_ATOM = 4

    !> pi to full real64 precision, for dialects with no symbolic constant.
    real(dp), parameter :: PI_VALUE = &
        3.141592653589793238462643383279502884_dp
    real(dp), parameter :: E_VALUE = &
        2.718281828459045235360287471352662498_dp

contains

    !> Render in fortsym's own notation.
    function print_expr(e) result(s)
        type(expr_t), intent(in) :: e
        type(str_t)              :: s
        s = print_expr_in(e, dialect(DIA_NATIVE))
    end function print_expr

    !> Render in a given dialect.
    function print_expr_in(e, d, ok) result(s)
        type(expr_t),    intent(in) :: e
        type(dialect_t), intent(in) :: d
        logical, intent(out), optional :: ok
        type(str_t)                 :: s
        type(strbuf_t) :: b
        integer :: no_ids(0)
        type(str_t) :: no_names(0)
        logical :: valid
        valid = .true.
        if (d%id == DIA_FORTRAN) valid = fortran_representable(e)
        if (present(ok)) ok = valid
        if (.not. valid) then
            s = str("")
            return
        end if
        call emit(b, e%a, e%id, d, PREC_ADD, no_ids, no_names)
        s = b%to_str()
    end function print_expr_in

    !> Render with some nodes replaced by names.
    !>
    !> This is what lets common subexpression elimination reuse the printer: a
    !> node that has been assigned to a temporary is emitted as that temporary's
    !> name instead of being expanded again. Keeping it here rather than in a
    !> second printer means generated kernels inherit every parenthesisation and
    !> literal-formatting rule automatically.
    function print_expr_sub(e, d, ids, names, ok, prechecked, bindings) result(s)
        type(expr_t),    intent(in) :: e
        type(dialect_t), intent(in) :: d
        integer,         intent(in) :: ids(:)
        type(str_t),     intent(in) :: names(:)
        logical, intent(out), optional :: ok
        logical, intent(in), optional :: prechecked
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        type(str_t)                 :: s
        type(strbuf_t) :: b
        logical :: valid
        valid = .true.
        if (d%id == DIA_FORTRAN) then
            if (present(prechecked)) then
                valid = prechecked
            else
                valid = fortran_representable(e)
            end if
        end if
        if (present(ok)) ok = valid
        if (.not. valid) then
            s = str("")
            return
        end if
        call emit(b, e%a, e%id, d, PREC_ADD, ids, names, bindings)
        s = b%to_str()
    end function print_expr_sub

    !> Render the expression as bare LaTeX math. This is deliberately a sibling
    !> walk rather than a spelling flag: fractions, scripts, roots and atomicity
    !> have two-dimensional syntax that the flat dialect printer cannot express.
    function print_expr_latex(e, symbol_names, symbol_values, ok, message) &
            result(s)
        type(expr_t), intent(in) :: e
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        type(str_t) :: s
        type(strbuf_t) :: b
        logical :: valid

        valid = size(symbol_names) == size(symbol_values)
        if (present(message)) message = ""
        if (.not. valid) then
            if (present(ok)) ok = .false.
            if (present(message)) message = "symbol override arrays differ in size"
            s = str("")
            return
        end if
        if (.not. associated(e%a) .or. e%id <= 0) then
            if (present(ok)) ok = .false.
            if (present(message)) message = "cannot render an invalid expression"
            s = str("")
            return
        end if

        call emit_latex(b, e%a, e%id, PREC_ADD, symbol_names, symbol_values)
        s = b%to_str()
        if (present(ok)) ok = .true.
    end function print_expr_latex

    !> Whether every arbitrary exact node reachable from e has a finite normal
    !> nearest-even binary64 projection for a real(dp) Fortran kernel.
    function fortran_representable(e, bindings) result(ok)
        type(expr_t), intent(in) :: e
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        logical                  :: ok
        logical, allocatable :: visited(:)

        allocate (visited(e%a%size()), source=.false.)
        if (present(bindings)) then
            ok = fortran_node_representable(e%a, e%id, visited, bindings=bindings)
        else
            ok = fortran_node_representable(e%a, e%id, visited)
        end if
    end function fortran_representable

    function fortran_roots_representable(roots, array_names, bindings) result(ok)
        type(expr_t), intent(in) :: roots(:)
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        logical                  :: ok
        logical, allocatable :: visited(:)
        integer :: k

        ok = .true.
        if (size(roots) == 0) return
        allocate (visited(roots(1)%a%size()), source=.false.)
        do k = 1, size(roots)
            if (present(array_names) .and. present(bindings)) then
                ok = fortran_node_representable(roots(k)%a, roots(k)%id, visited, &
                    array_names, bindings)
            else if (present(array_names)) then
                ok = fortran_node_representable(roots(k)%a, roots(k)%id, visited, &
                    array_names)
            else if (present(bindings)) then
                ok = fortran_node_representable(roots(k)%a, roots(k)%id, visited, &
                    bindings=bindings)
            else
                ok = fortran_node_representable(roots(k)%a, roots(k)%id, visited)
            end if
            if (.not. ok) return
        end do
    end function fortran_roots_representable

    !> Whether every reachable real value survives conversion to the selected
    !> generated kind. This is stricter than the historical binary64 gate and
    !> prevents a real32 kernel from compiling a literal as infinity.
    function fortran_roots_representable_kind(roots, real_kind, array_names, bindings) result(ok)
        type(expr_t), intent(in) :: roots(:)
        integer, intent(in) :: real_kind
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        logical :: ok
        logical, allocatable :: visited(:)
        integer :: k

        ok = .true.
        if (size(roots) == 0) return
        allocate (visited(roots(1)%a%size()), source=.false.)
        do k = 1, size(roots)
            if (present(array_names) .and. present(bindings)) then
                ok = fortran_node_representable_kind(roots(k)%a, roots(k)%id, &
                    visited, real_kind, array_names, bindings)
            else if (present(array_names)) then
                ok = fortran_node_representable_kind(roots(k)%a, roots(k)%id, &
                    visited, real_kind, array_names)
            else if (present(bindings)) then
                ok = fortran_node_representable_kind(roots(k)%a, roots(k)%id, &
                    visited, real_kind, bindings=bindings)
            else
                ok = fortran_node_representable_kind(roots(k)%a, roots(k)%id, &
                    visited, real_kind)
            end if
            if (.not. ok) return
        end do
    end function fortran_roots_representable_kind

    recursive function fortran_node_representable(a, id, visited, array_names, bindings) result(ok)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical,       intent(inout) :: visited(:)
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        logical                  :: ok
        real(dp) :: projected
        integer :: k
        character(:), allocatable :: text

        ok = .true.
        if (visited(id)) return
        visited(id) = .true.
        select case (a%kind_of(id))
        case (NK_SYM)
            ok = valid_fortran_symbol(chars(a%name_of(id)))
            return
        case (NK_BIG_INT, NK_BIG_RAT)
            projected = exact_to_real(chars(a%exact_text_of(id)), ok)
            if (ok) ok = ieee_is_finite(projected)
            return
        case (NK_BIG_REAL)
            text = chars(a%real_text_of(id))
            read (text, *, iostat=k) projected
            if (k /= 0) then
                ok = .false.
            else
                if (ok) ok = ieee_is_finite(projected)
            end if
            return
        case (NK_ALGEBRAIC)
            ok = .false.
            return
        case (NK_FUNC)
            if (chars(a%name_of(id)) == "Piecewise") then
                if (present(array_names) .and. present(bindings)) then
                    ok = fortran_piecewise_node_representable(a, id, visited, &
                        array_names, bindings)
                else if (present(array_names)) then
                    ok = fortran_piecewise_node_representable(a, id, visited, &
                        array_names=array_names)
                else if (present(bindings)) then
                    ok = fortran_piecewise_node_representable(a, id, visited, &
                        bindings=bindings)
                else
                    ok = fortran_piecewise_node_representable(a, id, visited)
                end if
                return
            end if
            if (present(array_names) .and. present(bindings)) then
                ok = fortran_function_node_representable(a, id, array_names, bindings)
            else if (present(array_names)) then
                ok = fortran_function_node_representable(a, id, array_names)
            else if (present(bindings)) then
                ok = fortran_function_node_representable(a, id, bindings=bindings)
            else
                ok = fortran_function_node_representable(a, id)
            end if
            if (.not. ok) return
        end select
        do k = 1, a%nargs_of(id)
            if (present(array_names) .and. present(bindings)) then
                ok = fortran_node_representable(a, a%arg_of(id, k), visited, &
                    array_names, bindings)
            else if (present(array_names)) then
                ok = fortran_node_representable(a, a%arg_of(id, k), visited, &
                    array_names)
            else if (present(bindings)) then
                ok = fortran_node_representable(a, a%arg_of(id, k), visited, &
                    bindings=bindings)
            else
                ok = fortran_node_representable(a, a%arg_of(id, k), visited)
            end if
            if (.not. ok) return
        end do
    end function fortran_node_representable

    recursive function fortran_node_representable_kind(a, id, visited, &
            real_kind, array_names, bindings) result(ok)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, real_kind
        logical, intent(inout) :: visited(:)
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        logical :: ok
        real(dp) :: projected
        integer :: k, ios
        character(:), allocatable :: text

        ok = .true.
        if (visited(id)) return
        visited(id) = .true.
        select case (a%kind_of(id))
        case (NK_SYM)
            ok = valid_fortran_symbol(chars(a%name_of(id)))
            return
        case (NK_BIG_INT, NK_BIG_RAT)
            projected = exact_to_real(chars(a%exact_text_of(id)), ok)
            if (ok) then
                select case (real_kind)
                case (real32)
                    projected = real(projected, real32)
                case (real64)
                    continue
                case default
                    ok = .false.
                end select
                if (ok) ok = ieee_is_finite(projected)
            end if
            return
        case (NK_BIG_REAL)
            text = chars(a%real_text_of(id))
            read (text, *, iostat=ios) projected
            if (ios /= 0) then
                ok = .false.
            else
                select case (real_kind)
                case (real32)
                    projected = real(projected, real32)
                case (real64)
                    continue
                case default
                    ok = .false.
                end select
                ok = ieee_is_finite(projected)
            end if
            return
        case (NK_ALGEBRAIC)
            ok = .false.
            return
        case (NK_REAL)
            projected = a%real_of(id)
            select case (real_kind)
            case (real32)
                projected = real(projected, real32)
            case (real64)
                continue
            case default
                ok = .false.
            end select
            if (ok) ok = ieee_is_finite(projected)
            return
        case (NK_FUNC)
            if (chars(a%name_of(id)) == "Piecewise") then
                if (present(array_names) .and. present(bindings)) then
                    ok = fortran_piecewise_node_representable_kind(a, id, visited, &
                        real_kind, array_names, bindings)
                else if (present(array_names)) then
                    ok = fortran_piecewise_node_representable_kind(a, id, visited, &
                        real_kind, array_names=array_names)
                else if (present(bindings)) then
                    ok = fortran_piecewise_node_representable_kind(a, id, visited, &
                        real_kind, bindings=bindings)
                else
                    ok = fortran_piecewise_node_representable_kind(a, id, visited, &
                        real_kind)
                end if
                return
            end if
            if (present(array_names) .and. present(bindings)) then
                ok = fortran_function_node_representable(a, id, array_names, bindings)
            else if (present(array_names)) then
                ok = fortran_function_node_representable(a, id, array_names)
            else if (present(bindings)) then
                ok = fortran_function_node_representable(a, id, bindings=bindings)
            else
                ok = fortran_function_node_representable(a, id)
            end if
            if (.not. ok) return
        end select
        do k = 1, a%nargs_of(id)
            if (present(array_names) .and. present(bindings)) then
                ok = fortran_node_representable_kind(a, a%arg_of(id, k), visited, &
                    real_kind, array_names, bindings)
            else if (present(array_names)) then
                ok = fortran_node_representable_kind(a, a%arg_of(id, k), visited, &
                    real_kind, array_names)
            else if (present(bindings)) then
                ok = fortran_node_representable_kind(a, a%arg_of(id, k), visited, &
                    real_kind, bindings=bindings)
            else
                ok = fortran_node_representable_kind(a, a%arg_of(id, k), visited, &
                    real_kind)
            end if
            if (.not. ok) return
        end do
    end function fortran_node_representable_kind

    recursive logical function fortran_piecewise_node_representable(a, id, &
            visited, array_names, bindings) result(ok)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(inout) :: visited(:)
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: k, branches_id, pair_id

        ok = .false.
        if (a%nargs_of(id) < 1) return
        branches_id = a%arg_of(id, 1)
        if (a%kind_of(branches_id) /= NK_FUNC) return
        if (chars(a%name_of(branches_id)) /= "List") return
        do k = 1, a%nargs_of(branches_id)
            pair_id = a%arg_of(branches_id, k)
            if (a%kind_of(pair_id) /= NK_FUNC) return
            if (chars(a%name_of(pair_id)) /= "List" .or. &
                a%nargs_of(pair_id) /= 2) return
            if (present(array_names) .and. present(bindings)) then
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 1), &
                    visited, array_names, bindings)) return
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 2), &
                    visited, array_names, bindings)) return
            else if (present(array_names)) then
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 1), &
                    visited, array_names)) return
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 2), &
                    visited, array_names)) return
            else if (present(bindings)) then
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 1), &
                    visited, bindings=bindings)) return
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 2), &
                    visited, bindings=bindings)) return
            else
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 1), &
                    visited)) return
                if (.not. fortran_node_representable(a, a%arg_of(pair_id, 2), &
                    visited)) return
            end if
        end do
        if (a%nargs_of(id) < 2) then
            ok = .true.
        else if (present(array_names) .and. present(bindings)) then
            ok = fortran_node_representable(a, a%arg_of(id, 2), visited, &
                array_names, bindings)
        else if (present(array_names)) then
            ok = fortran_node_representable(a, a%arg_of(id, 2), visited, &
                array_names)
        else if (present(bindings)) then
            ok = fortran_node_representable(a, a%arg_of(id, 2), visited, &
                bindings=bindings)
        else
            ok = fortran_node_representable(a, a%arg_of(id, 2), visited)
        end if
    end function fortran_piecewise_node_representable

    recursive logical function fortran_piecewise_node_representable_kind(a, id, &
            visited, real_kind, array_names, bindings) result(ok)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, real_kind
        logical, intent(inout) :: visited(:)
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: k, branches_id, pair_id

        ok = .false.
        if (a%nargs_of(id) < 1) return
        branches_id = a%arg_of(id, 1)
        if (a%kind_of(branches_id) /= NK_FUNC) return
        if (chars(a%name_of(branches_id)) /= "List") return
        do k = 1, a%nargs_of(branches_id)
            pair_id = a%arg_of(branches_id, k)
            if (a%kind_of(pair_id) /= NK_FUNC) return
            if (chars(a%name_of(pair_id)) /= "List" .or. &
                a%nargs_of(pair_id) /= 2) return
            if (present(array_names) .and. present(bindings)) then
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 1), &
                    visited, real_kind, array_names, bindings)) return
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 2), &
                    visited, real_kind, array_names, bindings)) return
            else if (present(array_names)) then
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 1), &
                    visited, real_kind, array_names)) return
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 2), &
                    visited, real_kind, array_names)) return
            else if (present(bindings)) then
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 1), &
                    visited, real_kind, bindings=bindings)) return
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 2), &
                    visited, real_kind, bindings=bindings)) return
            else
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 1), &
                    visited, real_kind)) return
                if (.not. fortran_node_representable_kind(a, a%arg_of(pair_id, 2), &
                    visited, real_kind)) return
            end if
        end do
        if (a%nargs_of(id) < 2) then
            ok = .true.
        else if (present(array_names) .and. present(bindings)) then
            ok = fortran_node_representable_kind(a, a%arg_of(id, 2), visited, &
                real_kind, array_names, bindings)
        else if (present(array_names)) then
            ok = fortran_node_representable_kind(a, a%arg_of(id, 2), visited, &
                real_kind, array_names)
        else if (present(bindings)) then
            ok = fortran_node_representable_kind(a, a%arg_of(id, 2), visited, &
                real_kind, bindings=bindings)
        else
            ok = fortran_node_representable_kind(a, a%arg_of(id, 2), visited, &
                real_kind)
        end if
    end function fortran_piecewise_node_representable_kind

    logical function fortran_function_node_representable(a, id, array_names, bindings) result(ok)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(str_t), intent(in), optional :: array_names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        character(:), allocatable :: name
        integer :: order, k

        name = chars(a%name_of(id))
        if (fortran_condition_function(name)) then
            ok = fortran_condition_arity_ok(name, a%nargs_of(id))
            return
        end if
        if (name == "If") then
            ok = a%nargs_of(id) == 3
            return
        end if
        if (name == "Boole") then
            ok = a%nargs_of(id) == 1
            return
        end if
        if (present(bindings)) then
            do k = 1, size(bindings)
                if (binding_matches(a, id, bindings(k))) then
                    ok = .true.
                    return
                end if
            end do
        end if
        ok = fortran_function_supported(name) .and. &
            fortran_function_arity_ok(name, a%nargs_of(id))
        if (.not. ok .and. present(array_names) .and. a%nargs_of(id) == 1) then
            do k = 1, size(array_names)
                if (same_fortran_name(name, chars(array_names(k)))) then
                    ok = .true.
                    exit
                end if
            end do
        end if
        if (.not. ok) return
        if (name == "besselj" .or. name == "bessely" .or. &
            name == "besseli" .or. name == "besselk") then
            order = a%arg_of(id, 1)
            ok = a%kind_of(order) == NK_INT .or. &
                (a%kind_of(order) == NK_RAT .and. a%den_of(order) == 1)
        end if
    end function fortran_function_node_representable

    logical function binding_matches(a, id, binding) result(matches)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(kernel_binding_t), intent(in) :: binding
        character(:), allocatable :: name, head
        integer :: order, k, index_id

        matches = .false.
        name = chars(a%name_of(id))
        head = chars(binding%head)
        if (derivative_order(name, order)) then
            if (.not. allocated(binding%derivative_indices)) return
            if (size(binding%derivative_indices) /= order) return
            if (a%nargs_of(id) < order + 1) return
            if (a%kind_of(a%arg_of(id, 1)) /= NK_SYM) return
            if (.not. same_fortran_name(chars(a%name_of(a%arg_of(id, 1))), head)) return
            do k = 1, order
                index_id = a%arg_of(id, k + 1)
                if (a%kind_of(index_id) == NK_INT) then
                    if (a%num_of(index_id) /= binding%derivative_indices(k)) return
                else if (a%kind_of(index_id) == NK_RAT) then
                    if (a%den_of(index_id) /= 1 .or. &
                        a%num_of(index_id) /= binding%derivative_indices(k)) return
                else
                    return
                end if
            end do
            matches = .true.
        else
            if (allocated(binding%derivative_indices)) then
                if (size(binding%derivative_indices) /= 0) return
            end if
            matches = same_fortran_name(name, head)
        end if
    end function binding_matches

    logical function derivative_order(name, order) result(is_derivative)
        character(*), intent(in) :: name
        integer, intent(out) :: order
        integer :: ios

        is_derivative = .false.
        order = 0
        if (len(name) <= 10) return
        if (name(:10) /= "Derivative") return
        read (name(11:), *, iostat=ios) order
        if (ios /= 0 .or. order < 1) then
            order = 0
            return
        end if
        is_derivative = .true.
    end function derivative_order

    pure logical function fortran_condition_function(name) result(ok)
        character(*), intent(in) :: name

        select case (name)
        case ("Less", "LessEqual", "Greater", "GreaterEqual", "Equal", &
                "Unequal", "And", "Or", "Not")
            ok = .true.
        case default
            ok = .false.
        end select
    end function fortran_condition_function

    pure logical function fortran_condition_arity_ok(name, n) result(ok)
        character(*), intent(in) :: name
        integer, intent(in) :: n

        if (name == "And" .or. name == "Or") then
            ok = n >= 2
        else if (name == "Not") then
            ok = n == 1
        else
            ok = n == 2
        end if
    end function fortran_condition_arity_ok

    !> Index of `id` in the substitution table, or 0.
    pure function subst_slot(id, ids) result(k)
        integer, intent(in) :: id, ids(:)
        integer             :: k
        integer :: j
        k = 0
        do j = 1, size(ids)
            if (ids(j) == id) then
                k = j
                return
            end if
        end do
    end function subst_slot

    !> Append the rendering of node `id`, parenthesising if its precedence is
    !> below `context`.
    recursive subroutine emit(b, a, id, d, context, ids, names, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: slot

        ! A node standing in for a temporary is emitted as its name, and its
        ! subtree is not walked -- that is the whole saving of CSE.
        slot = subst_slot(id, ids)
        if (slot > 0) then
            call b%append(chars(names(slot)))
            return
        end if

        select case (a%kind_of(id))
        case (NK_INT)
            call emit_integer(b, a, id, d, context)
        case (NK_RAT)
            call emit_rational(b, a, id, d, context)
        case (NK_BIG_INT, NK_BIG_RAT)
            call emit_big_exact(b, a, id, d, context)
        case (NK_BIG_REAL)
            call emit_big_real(b, a, id, d, context)
        case (NK_ALGEBRAIC)
            call emit_algebraic(b, a, id)
        case (NK_REAL)
            call emit_real(b, a, id, d, context)
        case (NK_SYM)
            if (d%id == DIA_FORTRAN) then
                select case (chars(a%name_of(id)))
                case ("True")
                    call b%append(".true.")
                case ("False")
                    call b%append(".false.")
                case default
                    call b%append(chars(a%name_of(id)))
                end select
            else
                call b%append(chars(a%name_of(id)))
            end if
        case (NK_CONST)
            call emit_constant(b, a, id, d)
        case (NK_ADD)
            call emit_sum(b, a, id, d, context, ids, names, bindings)
        case (NK_MUL)
            call emit_product(b, a, id, d, context, ids, names, bindings)
        case (NK_POW)
            call emit_power(b, a, id, d, context, ids, names, bindings)
        case (NK_FUNC)
            call emit_function(b, a, id, d, ids, names, bindings)
        case default
            call b%append("<?>")
        end select
    end subroutine emit

    subroutine emit_algebraic(b, a, id)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id

        call b%append(chars(a%algebraic_text_of(id)))
    end subroutine emit_algebraic

    ! ------------------------------------------------------------- atoms --

    !> A negative integer binds like a product, because -2**2 would otherwise
    !> read as (-2)**2 in a power context.
    subroutine emit_integer(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer(int64) :: v
        logical :: wrap

        v = a%num_of(id)
        ! Parenthesise a negative literal from multiplicative context upward.
        ! Without this a term following a minus sign emits as "2 - -1", which no
        ! Fortran compiler accepts: two arithmetic operators cannot be adjacent.
        wrap = v < 0_int64 .and. context >= PREC_MUL
        if (wrap) call b%append("(")
        call b%append(chars(str(v)))
        if (wrap) call b%append(")")
    end subroutine emit_integer

    !> An exact rational. Where the dialect allows it this is a/b; in Fortran it
    !> must become a quotient of typed reals, because 1/3 in Fortran is integer
    !> division and evaluates to zero.
    subroutine emit_rational(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        logical :: wrap

        wrap = context > PREC_MUL
        if (wrap) call b%append("(")
        call b%append(chars(str(a%num_of(id))))
        if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
        call b%append("/")
        call b%append(chars(str(a%den_of(id))))
        if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
        if (wrap) call b%append(")")
    end subroutine emit_rational

    !> An arbitrary-precision exact scalar. Native and CAS dialects accept the
    !> canonical decimal spelling directly. Fortran receives real literals so
    !> the source remains valid even though it has no unbounded integer kind.
    subroutine emit_big_exact(b, a, id, d, context, magnitude_only)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        logical, intent(in), optional :: magnitude_only
        character(:), allocatable :: text, numerator, denominator
        integer :: slash, first
        logical :: negative, magnitude, wrap
        real(dp) :: projected
        logical :: converted

        text = chars(a%exact_text_of(id))
        magnitude = .false.
        if (present(magnitude_only)) magnitude = magnitude_only
        if (d%id == DIA_FORTRAN) then
            projected = exact_to_real(text, converted)
            if (.not. converted) then
                return
            end if
            converted = ieee_is_finite(projected)
            if (.not. converted) then
                return
            end if
            if (magnitude) projected = abs(projected)
            wrap = projected < 0.0_dp .and. context >= PREC_MUL
            if (wrap) call b%append("(")
            call b%append(chars(str(projected)))
            call b%append(chars(d%real_suffix))
            if (wrap) call b%append(")")
            return
        end if
        negative = text(1:1) == "-"
        first = 1
        if (negative .and. magnitude) first = 2
        slash = index(text, "/")
        wrap = (negative .and. .not. magnitude .and. context >= PREC_MUL) .or. &
            (slash > 0 .and. context >= PREC_MUL)
        if (wrap) call b%append("(")

        if (slash == 0) then
            call b%append(text(first:))
        else
            numerator = text(first:slash - 1)
            denominator = text(slash + 1:)
            call b%append(numerator)
            call b%append("/")
            call b%append(denominator)
        end if
        if (wrap) call b%append(")")
    end subroutine emit_big_exact

    subroutine emit_real(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        real(dp) :: v
        logical :: wrap

        v = a%real_of(id)
        ! Same reason as emit_integer: "- -1.0_dp" is not valid Fortran.
        wrap = v < 0.0_dp .and. context >= PREC_MUL
        if (wrap) call b%append("(")
        if (d%compact_reals) then
            call b%append(compact_real(v))
        else
            call b%append(chars(str(v)))
        end if
        call b%append(chars(d%real_suffix))
        if (wrap) call b%append(")")
    end subroutine emit_real

    !> Emit a retained MPFR decimal without reducing it to real64 in CAS
    !> dialects. Fortran has only the real64 kernel path, so it receives the
    !> same checked projection used by fortran_representable.
    subroutine emit_big_real(b, a, id, d, context, magnitude_only)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        logical, intent(in), optional  :: magnitude_only
        character(:), allocatable :: text
        real(dp) :: projected
        logical :: magnitude, negative, wrap
        integer :: ios

        text = chars(a%real_text_of(id))
        magnitude = .false.
        if (present(magnitude_only)) magnitude = magnitude_only
        negative = .false.
        if (len(text) > 0) negative = text(1:1) == "-"

        if (d%id == DIA_FORTRAN) then
            read (text, *, iostat=ios) projected
            if (ios /= 0 .or. .not. ieee_is_finite(projected)) return
            if (magnitude) projected = abs(projected)
            wrap = projected < 0.0_dp .and. context >= PREC_MUL
            if (wrap) call b%append("(")
            call b%append(chars(str(projected)))
            call b%append(chars(d%real_suffix))
            if (wrap) call b%append(")")
            return
        end if

        if (magnitude .and. negative) then
            text = text(2:)
            negative = .false.
        end if
        if (d%id == DIA_WOLFRAM) text = wolfram_exponent(text)
        wrap = negative .and. context >= PREC_MUL
        if (wrap) call b%append("(")
        call b%append(text)
        if (wrap) call b%append(")")
    end subroutine emit_big_real

    !> Shortest decimal form that still round-trips through real64.
    !>
    !> The default 17-significant-digit form is right for generated Fortran,
    !> where losing a digit changes the compiled constant. It is wrong for
    !> comparing against a CAS: Mathics prints 2.5, and 2.5000000000000000E+000
    !> is structurally different text for the same number, so every real in the
    !> corpus would be scored as a disagreement.
    !>
    !> Tries increasing precision and stops at the first one that reads back
    !> exactly, so the shortening can never change the value.
    function compact_real(v) result(text)
        real(dp), intent(in)      :: v
        character(:), allocatable :: text
        character(32) :: buf
        real(dp) :: back
        integer :: digits, ios

        ! Fixed point first, within the range where it stays short. g0 switches
        ! to an exponent below 1e-3 and prints 0.001 as 0.1E-002, which is the
        ! same number spelled differently from every CAS this is compared with.
        if (v == 0.0_dp .or. (abs(v) >= 1.0e-4_dp .and. abs(v) < 1.0e16_dp)) then
            do digits = 1, 17
                write (buf, "(f0." // int_text(digits) // ")") v
                read (buf, *, iostat=ios) back
                if (ios == 0) then
                    if (back == v) then
                        text = with_leading_zero(trim(adjustl(buf)))
                        return
                    end if
                end if
            end do
        end if

        ! Outside that range, Wolfram's InputForm writes 1.*^-9 rather than an
        ! E exponent, and that is what the comparator parses back.
        do digits = 1, 17
            write (buf, "(es0." // int_text(digits) // ")") v
            read (buf, *, iostat=ios) back
            if (ios == 0) then
                if (back == v) then
                    text = wolfram_exponent(trim(adjustl(buf)))
                    return
                end if
            end if
        end do
        text = chars(str(v))
    end function compact_real

    !> ".001" is not a numeral in most readers; "0.001" is.
    function with_leading_zero(raw) result(text)
        character(*), intent(in)  :: raw
        character(:), allocatable :: text
        if (len(raw) > 0 .and. raw(1:1) == ".") then
            text = "0"//raw
        else if (len(raw) > 1 .and. raw(1:2) == "-.") then
            text = "-0."//raw(3:)
        else
            text = raw
        end if
    end function with_leading_zero

    !> Turn Fortran's 1.0E-09 into Wolfram's 1.0*^-9.
    function wolfram_exponent(raw) result(text)
        character(*), intent(in)  :: raw
        character(:), allocatable :: text, mantissa, expo
        integer :: epos, value, ios

        epos = scan(raw, "EeDd")
        if (epos == 0) then
            text = with_leading_zero(raw)
            return
        end if
        mantissa = with_leading_zero(raw(1:epos - 1))
        read (raw(epos + 1:), *, iostat=ios) value
        if (ios /= 0) then
            text = with_leading_zero(raw)
            return
        end if
        expo = int_text(value)
        if (value >= 0) then
            text = mantissa//"*^"//expo
        else
            text = mantissa//"*^-"//int_text(-value)
        end if
    end function wolfram_exponent

    function int_text(k) result(text)
        integer, intent(in)       :: k
        character(:), allocatable :: text
        character(8) :: buf
        write (buf, "(i0)") k
        text = trim(buf)
    end function int_text

    subroutine emit_constant(b, a, id, d)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        character(:), allocatable :: name

        name = chars(a%name_of(id))

        if (d%numeric_constants) then
            ! No symbolic constants in this dialect, so emit the value at full
            ! precision rather than letting the name escape as a free variable.
            select case (name)
            case ("pi")
                call b%append(chars(str(PI_VALUE)))
                call b%append(chars(d%real_suffix))
            case ("e")
                call b%append(chars(str(E_VALUE)))
                call b%append(chars(d%real_suffix))
            case default
                call b%append(name)
            end select
        else
            call b%append(chars(const_spelling(d, name)))
        end if
    end subroutine emit_constant

    ! --------------------------------------------------------- operators --

    !> A sum. Terms carrying a negative coefficient are printed with a minus
    !> sign and their coefficient negated, so the output reads a - b rather than
    !> a + (-1)*b.
    recursive subroutine emit_sum(b, a, id, d, context, ids, names, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: k, child
        logical :: wrap

        wrap = context > PREC_ADD
        if (wrap) call b%append("(")

        do k = 1, a%nargs_of(id)
            child = a%arg_of(id, k)
            if (k == 1) then
                call emit(b, a, child, d, PREC_ADD, ids, names, bindings)
            else if (is_negative_term(a, child)) then
                call b%append(" - ")
                call emit_negated(b, a, child, d, PREC_ADD, ids, names, bindings)
            else
                call b%append(" + ")
                call emit(b, a, child, d, PREC_ADD, ids, names, bindings)
            end if
        end do

        if (wrap) call b%append(")")
    end subroutine emit_sum

    !> Does this term carry an explicitly negative numeric coefficient?
    function is_negative_term(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k, child

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT)
            yes = a%num_of(id) < 0_int64
        case (NK_RAT)
            yes = a%num_of(id) < 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            yes = exact_is_negative(a, id)
        case (NK_REAL)
            yes = a%real_of(id) < 0.0_dp
        case (NK_BIG_REAL)
            yes = big_real_is_negative(a, id)
        case (NK_MUL)
            if (count_big_exact_factor(a, id) > 1) return
            do k = 1, a%nargs_of(id)
                child = a%arg_of(id, k)
                if (numeric_is_negative(a, child)) yes = .not. yes
            end do
            if (has_big_exact_factor(a, id)) then
                if (yes) yes = has_negative_big_exact_factor(a, id)
            end if
        end select
    end function is_negative_term

    !> Print a term with its sign removed; the caller has already emitted "-".
    recursive subroutine emit_negated(b, a, id, d, context, ids, names, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        type(arena_t), pointer :: ap
        integer :: k, n, negated
        integer, allocatable :: factors(:)

        select case (a%kind_of(id))
        case (NK_INT)
            ! No kind suffix: an integer stays an integer, exactly as in
            ! emit_integer. Suffixing here turned a negated -1 into 1.0_dp and
            ! put a real literal where an integer exponent belonged.
            call emit_compact_exact_magnitude(b, a, id, d, context)
        case (NK_RAT)
            call emit_compact_exact_magnitude(b, a, id, d, context)
        case (NK_BIG_INT, NK_BIG_RAT)
            call emit_big_exact(b, a, id, d, context, magnitude_only=.true.)
        case (NK_BIG_REAL)
            call emit_big_real(b, a, id, d, context, magnitude_only=.true.)
        case (NK_REAL)
            call b%append(chars(str(-a%real_of(id))))
            call b%append(chars(d%real_suffix))
        case (NK_MUL)
            ! Rebuild the factor list with the negative coefficient flipped. A
            ! coefficient of exactly -1 disappears, so -1*x prints as x rather
            ! than 1*x.
            n = a%nargs_of(id)
            allocate (factors(n))
            negated = 0
            do k = 1, n
                factors(k) = a%arg_of(id, k)
            end do
            call emit_product_factors(b, a, factors, d, context, ids, names, &
                negate=.true., bindings=bindings)
            deallocate (factors)
        case default
            call emit(b, a, id, d, context, ids, names, bindings)
        end select
    end subroutine emit_negated

    !> Emit the magnitude of a compact exact value without negating int64 in
    !> Fortran. Decimal slicing is defined even for INT64_MIN.
    subroutine emit_compact_exact_magnitude(b, a, id, d, context)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        character(:), allocatable :: text
        integer :: slash, first
        logical :: wrap

        text = chars(a%exact_text_of(id))
        first = 1
        if (text(1:1) == "-") first = 2
        slash = index(text, "/")
        wrap = slash > 0 .and. context > PREC_MUL
        if (wrap) call b%append("(")
        if (slash == 0) then
            call b%append(text(first:))
        else
            call b%append(text(first:slash - 1))
            if (d%id == DIA_FORTRAN) &
                call b%append(chars(d%int_real_suffix))
            call b%append("/")
            call b%append(text(slash + 1:))
            if (d%id == DIA_FORTRAN) &
                call b%append(chars(d%int_real_suffix))
        end if
        if (wrap) call b%append(")")
    end subroutine emit_compact_exact_magnitude

    !> A product, split into numerator and denominator. Factors that are powers
    !> with a negative integer exponent move to the denominator with the sign of
    !> the exponent flipped, so x*y**(-1) prints as x/y.
    recursive subroutine emit_product(b, a, id, d, context, ids, names, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer, allocatable :: factors(:)
        integer :: k, n

        n = a%nargs_of(id)
        allocate (factors(n))
        do k = 1, n
            factors(k) = a%arg_of(id, k)
        end do
        call emit_product_factors(b, a, factors, d, context, ids, names, &
            negate=.false., bindings=bindings)
        deallocate (factors)
    end subroutine emit_product

    !> True when a product has an odd number of negative numeric factors.
    !> Engine results may retain several numeric factors, so testing only
    !> whether any factor is negative emits doubled signs for expressions such
    !> as (-2)*(-1)*x.
    function product_is_negative(a, factors) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: factors(:)
        logical                   :: yes
        integer :: k
        yes = .false.
        do k = 1, size(factors)
            if (numeric_is_negative(a, factors(k))) yes = .not. yes
        end do
    end function product_is_negative

    recursive subroutine emit_product_factors(b, a, factors, d, context, ids, &
            names, negate, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: factors(:)
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        logical,         intent(in)    :: negate
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: k, nnum, nden, base, expo
        logical :: wrap, first, negative, structural_sign, preserve_signs
        integer, allocatable :: numer(:), denom(:)

        allocate (numer(size(factors)), denom(size(factors)))
        nnum = 0
        nden = 0

        do k = 1, size(factors)
            if (is_reciprocal(a, factors(k), base, expo)) then
                nden = nden + 1
                denom(nden) = factors(k)
            else
                nnum = nnum + 1
                numer(nnum) = factors(k)
            end if
        end do

        wrap = context > PREC_MUL
        if (wrap) call b%append("(")

        ! Emit one leading sign for the parity of every numeric sign. The
        ! caller's negate flag removes the sign already emitted by a sum.
        first = .true.
        negative = product_is_negative(a, factors)
        if (negate) negative = .not. negative
        structural_sign = negative .and. .not. negate .and. &
            has_negative_big_exact_in(factors, a) .and. &
            count_big_exact_in(factors, a) == 1
        preserve_signs = .not. negate .and. .not. structural_sign .and. &
            has_big_exact_in(factors, a) .and. &
            has_negative_numeric_in(factors, a)
        if (structural_sign) then
            call b%append("-(")
        else if (negative .and. .not. preserve_signs) then
            call b%append("-")
        end if

        if (nnum == 0) then
            ! Everything moved to the denominator, so the numerator is 1.
            call b%append("1")
            if (d%id == DIA_FORTRAN) call b%append(chars(d%int_real_suffix))
            first = .false.
        else
            do k = 1, nnum
                if (preserve_signs) then
                    if (.not. first) call b%append("*")
                    call emit(b, a, numer(k), d, PREC_MUL, ids, names, bindings)
                    first = .false.
                    cycle
                end if
                if (numeric_is_negative(a, numer(k))) then
                    ! The common leading sign already represents this factor's
                    ! sign. Emit its magnitude and drop unit factors.
                    if (is_minus_one(a, numer(k))) then
                        cycle
                    end if
                    if (.not. first) call b%append("*")
                    call emit_negated(b, a, numer(k), d, PREC_MUL, ids, names, bindings)
                    first = .false.
                    cycle
                end if
                if (.not. first) call b%append("*")
                call emit(b, a, numer(k), d, PREC_MUL, ids, names, bindings)
                first = .false.
            end do
            if (first) then
                call b%append("1")
                if (d%id == DIA_FORTRAN) &
                    call b%append(chars(d%int_real_suffix))
                first = .false.
            end if
        end if

        do k = 1, nden
            call b%append("/")
            if (is_reciprocal(a, denom(k), base, expo)) then
                if (expo == -1) then
                    ! Plain reciprocal: print the base at power precedence so a
                    ! compound base keeps its parentheses.
                    call emit(b, a, base, d, PREC_POW, ids, names, bindings)
                else
                    ! A higher reciprocal power: print base**|expo|.
                    call emit(b, a, base, d, PREC_POW, ids, names, bindings)
                    call b%append(chars(d%power))
                    call b%append(chars(str(-expo)))
                end if
            end if
        end do

        if (structural_sign) call b%append(")")
        if (wrap) call b%append(")")
        deallocate (numer, denom)
    end subroutine emit_product_factors

    function has_big_exact_in(factors, a) result(yes)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        logical                   :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, size(factors)
            kind = a%kind_of(factors(k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            yes = .true.
            return
        end do
    end function has_big_exact_in

    function count_big_exact_in(factors, a) result(count)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        integer                   :: count
        integer :: k, kind

        count = 0
        do k = 1, size(factors)
            kind = a%kind_of(factors(k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            count = count + 1
        end do
    end function count_big_exact_in

    function has_negative_big_exact_in(factors, a) result(yes)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        logical                   :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, size(factors)
            kind = a%kind_of(factors(k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            if (.not. exact_is_negative(a, factors(k))) cycle
            yes = .true.
            return
        end do
    end function has_negative_big_exact_in

    function has_negative_numeric_in(factors, a) result(yes)
        integer,       intent(in) :: factors(:)
        type(arena_t), intent(in) :: a
        logical                   :: yes
        integer :: k

        yes = .false.
        do k = 1, size(factors)
            if (.not. numeric_is_negative(a, factors(k))) cycle
            yes = .true.
            return
        end do
    end function has_negative_numeric_in

    function has_big_exact_factor(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k, kind

        yes = .false.
        do k = 1, a%nargs_of(id)
            kind = a%kind_of(a%arg_of(id, k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            yes = .true.
            return
        end do
    end function has_big_exact_factor

    function count_big_exact_factor(a, id) result(count)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        integer                   :: count
        integer :: k, kind

        count = 0
        do k = 1, a%nargs_of(id)
            kind = a%kind_of(a%arg_of(id, k))
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            count = count + 1
        end do
    end function count_big_exact_factor

    function has_negative_big_exact_factor(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        integer :: k, child, kind

        yes = .false.
        do k = 1, a%nargs_of(id)
            child = a%arg_of(id, k)
            kind = a%kind_of(child)
            if (kind /= NK_BIG_INT .and. kind /= NK_BIG_RAT) cycle
            if (.not. exact_is_negative(a, child)) cycle
            yes = .true.
            return
        end do
    end function has_negative_big_exact_factor

    !> True when the node is a power with a negative integer exponent, in which
    !> case it belongs in a denominator. Returns the base and the exponent.
    function is_reciprocal(a, id, base, expo) result(yes)
        type(arena_t), intent(in)  :: a
        integer,       intent(in)  :: id
        integer,       intent(out) :: base, expo
        logical                    :: yes
        integer :: e_id

        yes = .false.
        base = 0
        expo = 0
        if (a%kind_of(id) /= NK_POW) return
        e_id = a%arg_of(id, 2)
        if (a%kind_of(e_id) /= NK_INT) return
        if (a%num_of(e_id) >= 0_int64) return
        base = a%arg_of(id, 1)
        expo = int(a%num_of(e_id))
        yes = .true.
    end function is_reciprocal

    function is_numeric(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        yes = a%kind_of(id) == NK_INT .or. a%kind_of(id) == NK_RAT .or. &
            a%kind_of(id) == NK_BIG_INT .or. a%kind_of(id) == NK_BIG_RAT .or. &
            a%kind_of(id) == NK_REAL .or. a%kind_of(id) == NK_BIG_REAL
    end function is_numeric

    function numeric_is_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            yes = a%num_of(id) < 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            yes = exact_is_negative(a, id)
        case (NK_REAL)
            yes = a%real_of(id) < 0.0_dp
        case (NK_BIG_REAL)
            yes = big_real_is_negative(a, id)
        end select
    end function numeric_is_negative

    pure function big_real_is_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        character(:), allocatable :: text

        text = chars(a%real_text_of(id))
        yes = .false.
        if (len(text) > 0) yes = text(1:1) == "-"
    end function big_real_is_negative

    function exact_is_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        character(:), allocatable :: text

        text = chars(a%exact_text_of(id))
        yes = len(text) > 0
        if (yes) yes = text(1:1) == "-"
    end function exact_is_negative

    function is_minus_one(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer,       intent(in) :: id
        logical                   :: yes
        yes = .false.
        if (a%kind_of(id) == NK_INT) yes = a%num_of(id) == -1_int64
    end function is_minus_one

    !> Power. Exponentiation is right-associative, so the base is printed at a
    !> higher precedence than the exponent: a**b**c must not become (a**b)**c.
    recursive subroutine emit_power(b, a, id, d, context, ids, names, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: context
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        logical :: wrap

        wrap = context > PREC_POW
        if (wrap) call b%append("(")
        call emit(b, a, a%arg_of(id, 1), d, PREC_POW + 1, ids, names, bindings)
        call b%append(chars(d%power))
        call emit(b, a, a%arg_of(id, 2), d, PREC_POW, ids, names, bindings)
        if (wrap) call b%append(")")
    end subroutine emit_power

    recursive subroutine emit_function(b, a, id, d, ids, names, bindings)
        type(strbuf_t),  intent(inout) :: b
        type(arena_t),   intent(in)    :: a
        integer,         intent(in)    :: id
        type(dialect_t), intent(in)    :: d
        integer,         intent(in)    :: ids(:)
        type(str_t),     intent(in)    :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: k

        if (d%id == DIA_FORTRAN) then
            select case (chars(a%name_of(id)))
            case ("Piecewise")
                call emit_piecewise(b, a, id, d, ids, names, bindings)
                return
            case ("If")
                call emit_merge(b, a, a%arg_of(id, 2), a%arg_of(id, 3), &
                    a%arg_of(id, 1), d, ids, names, bindings)
                return
            case ("Boole")
                call emit_boole(b, a, id, d, ids, names, bindings)
                return
            end select
        end if
        if (d%id == DIA_FORTRAN .and. present(bindings)) then
            do k = 1, size(bindings)
                if (binding_matches(a, id, bindings(k))) then
                    call emit_binding(b, a, id, d, ids, names, bindings(k), bindings)
                    return
                end if
            end do
        end if

        ! List[] is an internal spelling for the empty list. Wolfram's
        ! InputForm writes that value as {}, and preserving the surface form is
        ! necessary for an independent parser to distinguish it from an empty
        ! call such as Directory[].
        if (d%id == DIA_WOLFRAM .and. chars(a%name_of(id)) == "List" .and. &
            a%nargs_of(id) == 0) then
            call b%append("{}")
            return
        end if

        call b%append(chars(fn_spelling(d, chars(a%name_of(id)))))
        ! Wolfram applies with brackets. Emitting parentheses here would produce
        ! text this dialect's own parser reads as a product, so the round trip
        ! would silently change the expression instead of failing.
        if (d%bracket_application) then
            call b%append("[")
        else
            call b%append("(")
        end if
        do k = 1, a%nargs_of(id)
            if (k > 1) call b%append(", ")
            ! Arguments sit inside brackets already, so they need none of
            ! their own whatever their precedence.
            if (d%id == DIA_FORTRAN .and. &
                a%kind_of(a%arg_of(id, k)) == NK_INT .and. &
                fortran_real_intrinsic_argument(a, id, k)) then
                ! Fortran does not convert integer actual arguments for generic
                ! intrinsics such as max/min/atan2, even when another argument
                ! is real, so an integer literal has to be written as a real.
                call b%append(chars(str(a%num_of(a%arg_of(id, k)))))
                call b%append(chars(d%int_real_suffix))
            else
                call emit(b, a, a%arg_of(id, k), d, PREC_ADD, ids, names, bindings)
            end if
        end do
        if (d%bracket_application) then
            call b%append("]")
        else
            call b%append(")")
        end if
    end subroutine emit_function

    recursive subroutine emit_piecewise(b, a, id, d, ids, names, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: default_id

        default_id = 0
        if (a%nargs_of(id) >= 2) default_id = a%arg_of(id, 2)
        call emit_piecewise_value(b, a, a%arg_of(id, 1), 1, default_id, d, &
            ids, names, bindings)
    end subroutine emit_piecewise

    recursive subroutine emit_piecewise_value(b, a, branches_id, branch_index, &
            default_id, d, ids, names, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: branches_id, branch_index, default_id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        integer :: pair_id

        if (branch_index > a%nargs_of(branches_id)) then
            if (default_id > 0) then
                call emit_piecewise_operand(b, a, default_id, d, ids, names, bindings)
            else
                call b%append("0")
                call b%append(chars(d%int_real_suffix))
            end if
            return
        end if

        pair_id = a%arg_of(branches_id, branch_index)
        call b%append("merge(")
        call emit_piecewise_operand(b, a, a%arg_of(pair_id, 1), d, ids, names, bindings)
        call b%append(", ")
        call emit_piecewise_value(b, a, branches_id, branch_index + 1, default_id, &
            d, ids, names, bindings)
        call b%append(", ")
        call emit_condition(b, a, a%arg_of(pair_id, 2), d, ids, names, bindings)
        call b%append(")")
    end subroutine emit_piecewise_value

    recursive subroutine emit_piecewise_operand(b, a, id, d, ids, names, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)

        if (d%id == DIA_FORTRAN .and. a%kind_of(id) == NK_INT) then
            call emit_integer(b, a, id, d, PREC_ADD)
            call b%append(chars(d%int_real_suffix))
        else
            call emit(b, a, id, d, PREC_ADD, ids, names, bindings)
        end if
    end subroutine emit_piecewise_operand

    recursive subroutine emit_merge(b, a, yes_id, no_id, condition_id, d, ids, &
            names, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: yes_id, no_id, condition_id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)

        call b%append("merge(")
        call emit_piecewise_operand(b, a, yes_id, d, ids, names, bindings)
        call b%append(", ")
        call emit_piecewise_operand(b, a, no_id, d, ids, names, bindings)
        call b%append(", ")
        call emit_condition(b, a, condition_id, d, ids, names, bindings)
        call b%append(")")
    end subroutine emit_merge

    recursive subroutine emit_boole(b, a, id, d, ids, names, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)

        call b%append("merge(1")
        call b%append(chars(d%int_real_suffix))
        call b%append(", 0")
        call b%append(chars(d%int_real_suffix))
        call b%append(", ")
        call emit_condition(b, a, a%arg_of(id, 1), d, ids, names, bindings)
        call b%append(")")
    end subroutine emit_boole

    recursive subroutine emit_condition(b, a, id, d, ids, names, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        character(:), allocatable :: name
        character(*), parameter :: relation_names(6) = [character(12) :: &
            "Less", "LessEqual", "Greater", "GreaterEqual", "Equal", "Unequal"]
        character(*), parameter :: relation_ops(6) = [character(3) :: &
            "<", "<=", ">", ">=", "==", "/="]
        integer :: k, relation

        if (a%kind_of(id) /= NK_FUNC) then
            call emit(b, a, id, d, PREC_ATOM, ids, names, bindings)
            return
        end if
        name = chars(a%name_of(id))
        relation = 0
        do k = 1, size(relation_names)
            if (name == relation_names(k)) then
                relation = k
                exit
            end if
        end do
        if (relation > 0) then
            call b%append("(")
            call emit(b, a, a%arg_of(id, 1), d, PREC_ADD, ids, names, bindings)
            call b%append(" "//trim(relation_ops(relation))//" ")
            call emit(b, a, a%arg_of(id, 2), d, PREC_ADD, ids, names, bindings)
            call b%append(")")
        else if (name == "Not") then
            call b%append("(.not. ")
            call emit_condition(b, a, a%arg_of(id, 1), d, ids, names, bindings)
            call b%append(")")
        else if (name == "And" .or. name == "Or") then
            call b%append("(")
            do k = 1, a%nargs_of(id)
                if (k > 1) then
                    if (name == "And") then
                        call b%append(" .and. ")
                    else
                        call b%append(" .or. ")
                    end if
                end if
                call emit_condition(b, a, a%arg_of(id, k), d, ids, names, bindings)
            end do
            call b%append(")")
        else
            call emit(b, a, id, d, PREC_ATOM, ids, names, bindings)
        end if
    end subroutine emit_condition

    recursive subroutine emit_binding(b, a, id, d, ids, names, binding, bindings)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(dialect_t), intent(in) :: d
        integer, intent(in) :: ids(:)
        type(str_t), intent(in) :: names(:)
        type(kernel_binding_t), intent(in) :: binding
        type(kernel_binding_t), intent(in), optional :: bindings(:)
        character(:), allocatable :: template
        integer :: marker, marker_end, first_arg, order, k

        template = chars(binding%replacement)
        marker = index(template, "%args%")
        if (marker == 0) then
            call b%append(template)
            return
        end if

        if (marker > 1) call b%append(template(:marker - 1))
        first_arg = 1
        if (derivative_order(chars(a%name_of(id)), order)) first_arg = order + 2
        do k = first_arg, a%nargs_of(id)
            if (k > first_arg) call b%append(", ")
            call emit(b, a, a%arg_of(id, k), d, PREC_ADD, ids, names, bindings)
        end do
        marker_end = marker + len("%args%")
        if (marker_end <= len(template)) call b%append(template(marker_end:))
    end subroutine emit_binding

    !> True when this Fortran intrinsic takes a real actual argument in this
    !> position, so an integer literal there has to be written as a real.
    !>
    !> The test is a whitelist of intrinsics rather than a test for what is not
    !> one. Indexing a declared array reaches emit_function with exactly the
    !> shape of a call -- x(1) is indistinguishable from a one-argument
    !> application -- and a subscript must stay an integer. Treating every
    !> unrecognised head as an intrinsic emitted x(1.0_dp), which is not valid
    !> Fortran.
    pure function fortran_real_intrinsic_argument(a, id, position) &
            result(is_real)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, position
        logical :: is_real

        select case (chars(a%name_of(id)))
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", &
                "exp", "log", "log10", "sqrt", "abs", &
                "erf", "erfc", "gamma", "loggamma", "max", "min", &
                "Min", "Max")
            is_real = .true.
        case ("besselj", "bessely", "besseli", "besselk")
            ! All four Fortran Bessel entry points take an integer order
            ! followed by a real value.
            is_real = position > 1
        case default
            ! A declared kernel array being indexed, or any other head fortsym
            ! does not know to be an intrinsic. Its integers stay integers.
            is_real = .false.
        end select
    end function fortran_real_intrinsic_argument

    ! -------------------------------------------------------------- LaTeX --

    recursive subroutine emit_latex(b, a, id, context, symbol_names, &
            symbol_values, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, context
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        logical, intent(in), optional :: magnitude
        logical :: positive_only

        positive_only = .false.
        if (present(magnitude)) positive_only = magnitude
        select case (a%kind_of(id))
        case (NK_INT)
            call emit_latex_integer(b, a, id, context, positive_only)
        case (NK_RAT)
            call emit_latex_rational(b, a, id, positive_only)
        case (NK_BIG_INT, NK_BIG_RAT)
            call emit_latex_exact(b, a, id, positive_only)
        case (NK_BIG_REAL)
            call emit_latex_big_real(b, a, id, positive_only)
        case (NK_ALGEBRAIC)
            call b%append(chars(a%algebraic_text_of(id)))
        case (NK_REAL)
            call emit_latex_real(b, a, id, context, positive_only)
        case (NK_SYM)
            call emit_latex_symbol(b, chars(a%name_of(id)), context, symbol_names, &
                symbol_values)
        case (NK_CONST)
            call emit_latex_constant(b, a, id)
        case (NK_ADD)
            call emit_latex_sum(b, a, id, context, symbol_names, symbol_values)
        case (NK_MUL)
            call emit_latex_product(b, a, id, context, symbol_names, &
                symbol_values, positive_only)
        case (NK_POW)
            call emit_latex_power(b, a, id, context, symbol_names, symbol_values)
        case (NK_FUNC)
            call emit_latex_function(b, a, id, symbol_names, symbol_values)
        case default
            call b%append("\mathrm{?}")
        end select
    end subroutine emit_latex

    subroutine emit_latex_integer(b, a, id, context, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, context
        logical, intent(in) :: magnitude
        character(:), allocatable :: text
        logical :: wrap

        text = chars(str(a%num_of(id)))
        if (magnitude .and. len(text) > 0 .and. text(1:1) == "-") then
            text = text(2:)
        end if
        wrap = .not. magnitude .and. a%num_of(id) < 0_int64 .and. &
            context >= PREC_POW
        if (wrap) call b%append("\left(")
        call b%append(text)
        if (wrap) call b%append("\right)")
    end subroutine emit_latex_integer

    subroutine emit_latex_rational(b, a, id, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(in) :: magnitude
        character(:), allocatable :: numerator
        logical :: negative

        negative = a%num_of(id) < 0_int64 .and. .not. magnitude
        numerator = chars(str(a%num_of(id)))
        if (negative) numerator = numerator(2:)
        if (negative) call b%append("-")
        call b%append("\frac{")
        call b%append(numerator)
        call b%append("}{")
        call b%append(chars(str(a%den_of(id))))
        call b%append("}")
    end subroutine emit_latex_rational

    subroutine emit_latex_exact(b, a, id, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(in) :: magnitude
        character(:), allocatable :: text, numerator, denominator
        integer :: slash, first
        logical :: negative

        text = chars(a%exact_text_of(id))
        negative = len(text) > 0 .and. text(1:1) == "-" .and. .not. magnitude
        first = 1
        if (len(text) > 0 .and. text(1:1) == "-") first = 2
        if (negative) call b%append("-")
        slash = index(text, "/")
        if (slash == 0) then
            call b%append(text(first:))
        else
            numerator = text(first:slash - 1)
            denominator = text(slash + 1:)
            call b%append("\frac{"//numerator//"}{"//denominator//"}")
        end if
    end subroutine emit_latex_exact

    subroutine emit_latex_big_real(b, a, id, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical, intent(in) :: magnitude
        character(:), allocatable :: text

        text = chars(a%real_text_of(id))
        if (magnitude .and. len(text) > 0 .and. text(1:1) == "-") then
            text = text(2:)
        end if
        call b%append(latex_number(text))
    end subroutine emit_latex_big_real

    subroutine emit_latex_real(b, a, id, context, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, context
        logical, intent(in) :: magnitude
        character(:), allocatable :: text
        logical :: wrap

        text = chars(str(a%real_of(id)))
        if (magnitude .and. len(text) > 0 .and. text(1:1) == "-") then
            text = text(2:)
        end if
        wrap = .not. magnitude .and. a%real_of(id) < 0.0_dp .and. &
            context >= PREC_POW
        if (wrap) call b%append("\left(")
        call b%append(latex_number(text))
        if (wrap) call b%append("\right)")
    end subroutine emit_latex_real

    function latex_number(text) result(rendered)
        character(*), intent(in) :: text
        character(:), allocatable :: rendered, mantissa, exponent
        integer :: position

        position = scan(text, "EeDd")
        if (position == 0) then
            rendered = text
            return
        end if
        mantissa = text(1:position - 1)
        exponent = text(position + 1:)
        rendered = mantissa//"\times 10^{"//exponent//"}"
    end function latex_number

    subroutine emit_latex_constant(b, a, id)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        character(:), allocatable :: name

        name = chars(a%name_of(id))
        select case (name)
        case ("pi"); call b%append("\pi")
        case ("e");  call b%append("\mathrm{e}")
        case ("i");  call b%append("\mathrm{i}")
        case default; call b%append(latex_default_symbol(name))
        end select
    end subroutine emit_latex_constant

    recursive subroutine emit_latex_sum(b, a, id, context, symbol_names, &
            symbol_values)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, context
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        integer :: k, child
        logical :: wrap

        wrap = context > PREC_ADD
        if (wrap) call b%append("\left(")
        do k = 1, a%nargs_of(id)
            child = a%arg_of(id, k)
            if (k == 1) then
                call emit_latex(b, a, child, PREC_ADD, symbol_names, symbol_values)
            else if (latex_negative_term(a, child)) then
                call b%append(" - ")
                call emit_latex(b, a, child, PREC_ADD, symbol_names, &
                    symbol_values, magnitude=.true.)
            else
                call b%append(" + ")
                call emit_latex(b, a, child, PREC_ADD, symbol_names, symbol_values)
            end if
        end do
        if (wrap) call b%append("\right)")
    end subroutine emit_latex_sum

    recursive subroutine emit_latex_product(b, a, id, context, symbol_names, &
            symbol_values, magnitude)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, context
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        logical, intent(in) :: magnitude
        integer :: k, base, expo, denominator_count, numerator_count
        logical :: negative, first, wrap

        denominator_count = 0
        numerator_count = 0
        do k = 1, a%nargs_of(id)
            if (latex_reciprocal(a, a%arg_of(id, k), base, expo)) then
                denominator_count = denominator_count + 1
            else
                numerator_count = numerator_count + 1
            end if
        end do

        negative = latex_product_negative(a, id)
        if (magnitude) negative = .not. negative
        wrap = context > PREC_MUL
        if (wrap) call b%append("\left(")
        if (negative) call b%append("-")

        if (denominator_count > 0) then
            call b%append("\frac{")
            first = .true.
            do k = 1, a%nargs_of(id)
                if (latex_reciprocal(a, a%arg_of(id, k), base, expo)) cycle
                if (latex_minus_one(a, a%arg_of(id, k)) .and. &
                    a%nargs_of(id) > 1) cycle
                if (.not. first) call b%append("\,")
                call emit_latex(b, a, a%arg_of(id, k), PREC_MUL, symbol_names, &
                    symbol_values, magnitude=.true.)
                first = .false.
            end do
            if (first) call b%append("1")
            call b%append("}{")
            first = .true.
            do k = 1, a%nargs_of(id)
                if (.not. latex_reciprocal(a, a%arg_of(id, k), base, expo)) cycle
                if (.not. first) call b%append("\,")
                call emit_latex(b, a, base, PREC_POW, symbol_names, symbol_values)
                if (expo < -1) then
                    call b%append("^{"//int_text(-expo)//"}")
                end if
                first = .false.
            end do
            call b%append("}")
        else
            first = .true.
            do k = 1, a%nargs_of(id)
                if (latex_minus_one(a, a%arg_of(id, k)) .and. &
                    a%nargs_of(id) > 1) then
                    cycle
                else if (latex_numeric_negative(a, a%arg_of(id, k))) then
                    if (.not. first) call b%append("\,")
                    call emit_latex(b, a, a%arg_of(id, k), PREC_MUL, &
                        symbol_names, symbol_values, magnitude=.true.)
                else
                    if (.not. first) call b%append("\,")
                    call emit_latex(b, a, a%arg_of(id, k), PREC_MUL, &
                        symbol_names, symbol_values)
                end if
                first = .false.
            end do
            if (first) call b%append("1")
        end if
        if (wrap) call b%append("\right)")
    end subroutine emit_latex_product

    recursive subroutine emit_latex_power(b, a, id, context, symbol_names, &
            symbol_values)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id, context
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        integer :: exponent_id
        logical :: wrap

        exponent_id = a%arg_of(id, 2)
        if (a%kind_of(exponent_id) == NK_RAT .and. a%num_of(exponent_id) == 1 &
            .and. a%den_of(exponent_id) == 2) then
            call b%append("\sqrt{")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("}")
            return
        end if
        if (a%kind_of(exponent_id) == NK_INT .and. a%num_of(exponent_id) < 0) then
            call b%append("\frac{1}{")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            if (a%num_of(exponent_id) < -1) then
                call b%append("^{"//int_text(int(-a%num_of(exponent_id)))//"}")
            end if
            call b%append("}")
            return
        end if

        wrap = context > PREC_POW
        if (wrap) call b%append("\left(")
        call emit_latex(b, a, a%arg_of(id, 1), PREC_POW + 1, symbol_names, &
            symbol_values)
        call b%append("^{")
        call emit_latex(b, a, exponent_id, PREC_ADD, symbol_names, symbol_values)
        call b%append("}")
        if (wrap) call b%append("\right)")
    end subroutine emit_latex_power

    recursive subroutine emit_latex_function(b, a, id, symbol_names, symbol_values)
        type(strbuf_t), intent(inout) :: b
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        character(:), allocatable :: name
        integer :: k

        name = chars(a%name_of(id))
        if (name == "partial" .and. a%nargs_of(id) == 2) then
            call b%append("\partial_{")
            call emit_latex(b, a, a%arg_of(id, 2), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("}\left(")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("\right)")
            return
        end if
        if (name == "curl_t" .and. a%nargs_of(id) == 1) then
            call b%append("\operatorname{curl}_{\mathrm{t}}\boldsymbol{")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("}")
            return
        end if
        if (name == "sqrt" .and. a%nargs_of(id) == 1) then
            call b%append("\sqrt{")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("}")
            return
        end if
        if (name == "exp" .and. a%nargs_of(id) == 1) then
            call b%append("\mathrm{e}^{")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("}")
            return
        end if
        if (name == "abs" .and. a%nargs_of(id) == 1) then
            call b%append("\left|")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("\right|")
            return
        end if
        if (name == "log" .and. a%nargs_of(id) == 1) then
            call b%append("\ln{\left(")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("\right)}")
            return
        end if
        if (name == "besselj" .and. a%nargs_of(id) == 2) then
            call b%append("J_{")
            call emit_latex(b, a, a%arg_of(id, 1), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("}{\left(")
            call emit_latex(b, a, a%arg_of(id, 2), PREC_ADD, symbol_names, &
                symbol_values)
            call b%append("\right)}")
            return
        end if
        call b%append("\")
        call b%append(latex_function_name(name))
        call b%append("{\left(")
        do k = 1, a%nargs_of(id)
            if (k > 1) call b%append(", ")
            call emit_latex(b, a, a%arg_of(id, k), PREC_ADD, symbol_names, &
                symbol_values)
        end do
        call b%append("\right)}")
    end subroutine emit_latex_function

    function latex_function_name(name) result(rendered)
        character(*), intent(in) :: name
        character(:), allocatable :: rendered

        select case (name)
        case ("sin"); rendered = "sin"
        case ("cos"); rendered = "cos"
        case ("tan"); rendered = "tan"
        case ("asin"); rendered = "arcsin"
        case ("acos"); rendered = "arccos"
        case ("atan"); rendered = "arctan"
        case ("sinh"); rendered = "sinh"
        case ("cosh"); rendered = "cosh"
        case ("tanh"); rendered = "tanh"
        case ("erf"); rendered = "operatorname{erf}"
        case ("erfc"); rendered = "operatorname{erfc}"
        case ("gamma"); rendered = "Gamma"
        case default; rendered = "operatorname{"//latex_escape(name)//"}"
        end select
    end function latex_function_name

    subroutine emit_latex_symbol(b, name, context, symbol_names, symbol_values)
        type(strbuf_t), intent(inout) :: b
        character(*), intent(in) :: name
        integer, intent(in) :: context
        type(str_t), intent(in) :: symbol_names(:), symbol_values(:)
        character(:), allocatable :: rendered
        integer :: k

        do k = 1, size(symbol_names)
            if (chars(symbol_names(k)) /= name) cycle
            rendered = chars(symbol_values(k))
            if (context > PREC_POW .and. (index(rendered, "^") > 0 .or. &
                index(rendered, "_") > 0)) then
                call b%append("\left("//rendered//"\right)")
            else
                call b%append(rendered)
            end if
            return
        end do
        rendered = latex_default_symbol(name)
        if (context > PREC_POW .and. (index(rendered, "^") > 0 .or. &
            index(rendered, "_") > 0)) then
            call b%append("\left("//rendered//"\right)")
        else
            call b%append(rendered)
        end if
    end subroutine emit_latex_symbol

    function latex_default_symbol(original) result(rendered)
        character(*), intent(in) :: original
        character(:), allocatable :: rendered
        character(:), allocatable :: name, base, subscript, superscript, modifier
        integer :: position, close_position

        name = original
        if (len(name) >= 7 .and. name(1:7) == "Global`") name = name(8:)
        rendered = latex_named_character(name)
        if (len(rendered) > 0) return

        close_position = len(name)
        position = index(name, "(")
        if (position > 1 .and. close_position > position .and. &
            name(close_position:close_position) == ")") then
            rendered = latex_base(name(:position - 1), "")//"_{"// &
                latex_escape(name(position + 1:close_position - 1))//"}"
            return
        end if

        superscript = ""
        position = index(name, "__")
        if (position > 1) then
            superscript = name(position + 2:)
            name = name(:position - 1)
        else
            position = index(name, "^")
            if (position > 1) then
                superscript = name(position + 1:)
                name = name(:position - 1)
            end if
        end if

        modifier = ""
        do while (latex_modifier_suffix(name, modifier, base))
            name = base
        end do

        subscript = ""
        position = index(name, "_")
        if (position > 1) then
            subscript = name(position + 1:)
            name = name(:position - 1)
        else
            position = latex_trailing_digit_start(name)
            if (position > 1) then
                subscript = name(position:)
                name = name(:position - 1)
            end if
        end if

        rendered = latex_base(name, subscript)
        if (len(superscript) > 0) rendered = rendered//"^{"// &
            latex_escape(superscript)//"}"
        if (len(modifier) > 0) rendered = latex_modifier(modifier, rendered)
    end function latex_default_symbol

    function latex_base(name, subscript) result(rendered)
        character(*), intent(in) :: name, subscript
        character(:), allocatable :: rendered, greek

        greek = latex_greek(name)
        if (len(greek) > 0) then
            rendered = greek
        else if (len(name) == 1 .and. is_latex_letter(name(1:1))) then
            rendered = name
        else
            rendered = "\mathrm{"//latex_escape(name)//"}"
        end if
        if (len(subscript) > 0) rendered = rendered//"_{"// &
            latex_escape(subscript)//"}"
    end function latex_base

    function latex_modifier_suffix(name, modifier, remainder) result(found)
        character(*), intent(in) :: name
        character(:), allocatable, intent(inout) :: modifier
        character(:), allocatable, intent(out) :: remainder
        logical :: found
        character(8), parameter :: modifiers(10) = [character(8) :: &
            "bar", "hat", "vec", "dot", "ddot", "tilde", "prime", &
            "norm", "abs", "bold"]
        integer :: k, position

        found = .false.
        remainder = name
        do k = 1, size(modifiers)
            position = len(name) - len_trim(modifiers(k))
            if (position < 1) cycle
            if (name(position:position) /= "_") cycle
            if (name(position + 1:) /= trim(modifiers(k))) cycle
            found = .true.
            modifier = trim(modifiers(k))
            remainder = name(:position - 1)
            return
        end do
    end function latex_modifier_suffix

    function latex_modifier(name, body) result(rendered)
        character(*), intent(in) :: name, body
        character(:), allocatable :: rendered, command

        select case (name)
        case ("bar"); command = "bar"
        case ("hat"); command = "hat"
        case ("vec"); command = "vec"
        case ("dot"); command = "dot"
        case ("ddot"); command = "ddot"
        case ("tilde"); command = "tilde"
        case ("prime"); command = "prime"
        case ("norm"); command = "lVert"; rendered = "\lVert "//body//" \rVert"; return
        case ("abs"); command = "left|"; rendered = "\left|"//body//"\right|"; return
        case ("bold"); command = "mathbf"
        case default; rendered = body; return
        end select
        rendered = "\"//command//"{"//body//"}"
    end function latex_modifier

    function latex_named_character(name) result(rendered)
        character(*), intent(in) :: name
        character(:), allocatable :: rendered
        character(:), allocatable :: inner
        integer :: n

        rendered = ""
        if (len(name) < 4) return
        if (name(1:2) /= "\[") return
        n = len(name)
        if (name(n:n) /= "]") return
        inner = name(3:n - 1)
        rendered = latex_greek(inner)
    end function latex_named_character

    function latex_greek(name) result(rendered)
        character(*), intent(in) :: name
        character(:), allocatable :: rendered

        select case (name)
        case ("Alpha", "alpha"); rendered = "\alpha"
        case ("Beta", "beta"); rendered = "\beta"
        case ("Gamma"); rendered = "\Gamma"
        case ("gamma"); rendered = "\gamma"
        case ("Delta"); rendered = "\Delta"
        case ("delta"); rendered = "\delta"
        case ("Epsilon", "epsilon"); rendered = "\epsilon"
        case ("Zeta", "zeta"); rendered = "\zeta"
        case ("Eta", "eta"); rendered = "\eta"
        case ("Theta"); rendered = "\Theta"
        case ("theta"); rendered = "\theta"
        case ("Iota", "iota"); rendered = "\iota"
        case ("Kappa", "kappa"); rendered = "\kappa"
        case ("Lambda"); rendered = "\Lambda"
        case ("lambda"); rendered = "\lambda"
        case ("Mu"); rendered = "\mu"
        case ("mu"); rendered = "\mu"
        case ("Nu", "nu"); rendered = "\nu"
        case ("Xi"); rendered = "\Xi"
        case ("xi"); rendered = "\xi"
        case ("Omicron", "omicron"); rendered = "\omicron"
        case ("Pi"); rendered = "\Pi"
        case ("pi"); rendered = "\pi"
        case ("Rho", "rho"); rendered = "\rho"
        case ("Sigma"); rendered = "\Sigma"
        case ("sigma"); rendered = "\sigma"
        case ("Tau", "tau"); rendered = "\tau"
        case ("Upsilon"); rendered = "\Upsilon"
        case ("upsilon"); rendered = "\upsilon"
        case ("Phi"); rendered = "\Phi"
        case ("phi"); rendered = "\phi"
        case ("Chi", "chi"); rendered = "\chi"
        case ("Psi"); rendered = "\Psi"
        case ("psi"); rendered = "\psi"
        case ("Omega"); rendered = "\Omega"
        case ("omega"); rendered = "\omega"
        case default; rendered = ""
        end select
    end function latex_greek

    pure function is_latex_letter(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = (c >= "A" .and. c <= "Z") .or. (c >= "a" .and. c <= "z")
    end function is_latex_letter

    pure function is_latex_digit(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = c >= "0" .and. c <= "9"
    end function is_latex_digit

    function latex_trailing_digit_start(name) result(position)
        character(*), intent(in) :: name
        integer :: position

        position = len(name) + 1
        do while (position > 1)
            if (.not. is_latex_digit(name(position - 1:position - 1))) exit
            position = position - 1
        end do
        if (position == len(name) + 1) position = 0
    end function latex_trailing_digit_start

    function latex_escape(text) result(rendered)
        character(*), intent(in) :: text
        character(:), allocatable :: rendered
        type(strbuf_t) :: b
        integer :: k

        do k = 1, len(text)
            select case (text(k:k))
            case ("&"); call b%append("\&")
            case ("%"); call b%append("\%")
            case ("$"); call b%append("\$")
            case ("#"); call b%append("\#")
            case ("_"); call b%append("\_")
            case ("{"); call b%append("\{")
            case ("}"); call b%append("\}")
            case ("\"); call b%append("\backslash{}");
            case default; call b%append(text(k:k))
            end select
        end do
        rendered = chars(b%to_str())
    end function latex_escape

    function latex_numeric_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical :: yes
        character(:), allocatable :: text

        yes = .false.
        select case (a%kind_of(id))
        case (NK_INT, NK_RAT)
            yes = a%num_of(id) < 0_int64
        case (NK_BIG_INT, NK_BIG_RAT)
            text = chars(a%exact_text_of(id))
            yes = len(text) > 0 .and. text(1:1) == "-"
        case (NK_REAL)
            yes = a%real_of(id) < 0.0_dp
        case (NK_BIG_REAL)
            text = chars(a%real_text_of(id))
            yes = len(text) > 0 .and. text(1:1) == "-"
        case (NK_ALGEBRAIC)
            yes = .false.
        end select
    end function latex_numeric_negative

    function latex_product_negative(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical :: yes
        integer :: k

        yes = .false.
        do k = 1, a%nargs_of(id)
            if (latex_numeric_negative(a, a%arg_of(id, k))) yes = .not. yes
        end do
    end function latex_product_negative

    function latex_minus_one(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        logical :: yes

        yes = a%kind_of(id) == NK_INT .and. a%num_of(id) == -1_int64
    end function latex_minus_one

    function latex_negative_term(a, id) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer :: k
        logical :: yes

        yes = latex_numeric_negative(a, id)
        if (a%kind_of(id) /= NK_MUL) return
        yes = .false.
        do k = 1, a%nargs_of(id)
            if (latex_numeric_negative(a, a%arg_of(id, k))) yes = .not. yes
        end do
    end function latex_negative_term

    function latex_reciprocal(a, id, base, exponent) result(yes)
        type(arena_t), intent(in) :: a
        integer, intent(in) :: id
        integer, intent(out) :: base, exponent
        logical :: yes
        integer :: exponent_id

        yes = .false.
        base = 0
        exponent = 0
        if (a%kind_of(id) /= NK_POW) return
        exponent_id = a%arg_of(id, 2)
        if (a%kind_of(exponent_id) /= NK_INT) return
        if (a%num_of(exponent_id) >= 0_int64) return
        base = a%arg_of(id, 1)
        exponent = int(a%num_of(exponent_id))
        yes = .true.
    end function latex_reciprocal

end module fortsym_print
