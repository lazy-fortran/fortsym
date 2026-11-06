module fortsym_emit
    ! One kernel definition, emitted for one or several targets.
    !
    ! A kernel_t collects output references and their expressions,
    !
    !     call k%jet(q, [members('f%vpar', 'f%dvpar', 'f%d2vpar')], partials(4, 2), &
    !                chain=c)
    !     call k%assign('zdot(1)', rdot)
    !
    ! and emit turns it into source: through emit_kernel for a Fortran leaf
    ! with derived-type references and bindings, or through the kernel IR for
    ! a list of targets. A chain_t supplies its needed intermediates as named
    ! locals. Bindings of the current context apply to both paths; the IR
    ! path reads a bound function or partial as the symbol of its name.
    use fortsym_arena, only: NK_FUNC, NK_SYM
    use fortsym_expr, only: expr_t, sym
    use fortsym_subs, only: subs_many
    use fortsym_chain, only: chain_t, fold_numbers
    use fortsym_context, only: kernel_binding_t, active_bindings
    use fortsym_print, only: binding_matches, binding_omits_arguments
    use fortsym_string, only: str_t, str, chars
    use fortsym_kernel, only: kernel_spec_t, emit_kernel
    use fortsym_kernel_ir, only: kernel_ir_t, lower_kernel_ir
    use fortsym_kernel_emit, only: kernel_emit_spec_t, emit_fortran_kernel_ir, &
        emit_cuda_device_ir, emit_metal_device_ir, TARGET_CUDA, TARGET_METAL
    use fortsym_jet, only: jet_names_t, derivative_set_t, jet
    implicit none
    private

    public :: kernel_t, emit

    type :: kernel_t
        type(expr_t), allocatable :: roots(:)
        type(str_t), allocatable :: refs(:)
        !> Known values, replaced in the roots and the chain locals before
        !> emission; a local that is itself replaced is not emitted.
        type(expr_t), allocatable :: old(:), new(:)
    contains
        procedure :: assign => kernel_assign
        procedure :: jet => kernel_jet
        procedure :: substitute => kernel_substitute
        procedure :: reads => kernel_reads
    end type kernel_t

    interface emit
        module procedure emit_fortran, emit_targets
    end interface emit

contains

    subroutine kernel_assign(self, ref, e)
        class(kernel_t), intent(inout) :: self
        character(*), intent(in) :: ref
        type(expr_t), intent(in) :: e

        if (.not. allocated(self%roots)) allocate (self%roots(0), self%refs(0))
        self%roots = [self%roots, e]
        self%refs = [self%refs, str(ref)]
    end subroutine kernel_assign

    !> Assign the partials in `set` of every q(m) to names(m). Outputs are
    !> grouped by total order, then by quantity, then in set order.
    subroutine kernel_jet(self, q, names, set, x, chain)
        class(kernel_t), intent(inout) :: self
        type(expr_t), intent(in) :: q(:)
        type(jet_names_t), intent(in) :: names(:)
        type(derivative_set_t), intent(in) :: set
        type(expr_t), intent(in), optional :: x(:)
        type(chain_t), intent(inout), optional :: chain
        type(derivative_set_t) :: level
        type(expr_t), allocatable :: d(:)
        character(:), allocatable :: ref
        integer :: m, iq, k

        if (size(names) /= size(q)) error stop "kernel jet: one name scheme per quantity"
        do m = 0, maxval(sum(set%orders, 1))
            level = set%of_order(m)
            do iq = 1, size(q)
                d = jet(q(iq), level, x, chain)
                do k = 1, level%count()
                    ref = names(iq)%of(level%orders(:, k))
                    if (len(ref) == 0) error stop "kernel jet: unnamed output partial"
                    call self%assign(ref, d(k))
                end do
            end do
        end do
    end subroutine kernel_jet

    subroutine kernel_substitute(self, old, new)
        class(kernel_t), intent(inout) :: self
        type(expr_t), intent(in) :: old(:), new(:)

        if (size(old) /= size(new)) error stop "kernel substitute: size mismatch"
        if (.not. allocated(self%old)) allocate (self%old(0), self%new(0))
        self%old = [self%old, old]
        self%new = [self%new, new]
    end subroutine kernel_substitute

    !> Whether the Fortran leaf of this kernel reads the input `name` or an
    !> element name(...) of it. Arguments of a bound function are not read
    !> unless its name contains %args%.
    logical function kernel_reads(self, name, chain) result(found)
        class(kernel_t), intent(in) :: self
        character(*), intent(in) :: name
        type(chain_t), intent(in), optional :: chain
        type(kernel_binding_t), allocatable :: bindings(:)
        type(expr_t), allocatable :: roots(:), values(:)
        type(str_t), allocatable :: locals(:)
        integer :: j

        call prepare(self, chain, .true., .false., roots, locals, values)
        call active_bindings(bindings)
        found = .false.
        do j = 1, size(roots)
            if (occurs(roots(j))) found = .true.
        end do
        do j = 1, size(values)
            if (occurs(values(j))) found = .true.
        end do

    contains

        recursive logical function occurs(e) result(yes)
            type(expr_t), intent(in) :: e
            character(:), allocatable :: symbol
            integer :: a

            yes = .false.
            if (e%kind() == NK_SYM) then
                symbol = chars(e%name())
                yes = symbol == name .or. index(symbol, name//'(') == 1
                return
            end if
            if (allocated(bindings)) then
                if (binding_omits_arguments(e%a, e%id, bindings)) return
            end if
            do a = 1, e%nargs()
                if (occurs(e%arg(a))) then
                    yes = .true.
                    return
                end if
            end do
        end function occurs

    end function kernel_reads

    !> A Fortran leaf through emit_kernel. The spec supplies the name, the
    !> arguments and the outputs; the kernel supplies output references and,
    !> with a chain, the named locals. Without ok, a failure stops.
    function emit_fortran(k, spec, chain, ok, message) result(source)
        type(kernel_t), intent(in) :: k
        type(kernel_spec_t), intent(in) :: spec
        type(chain_t), intent(in), optional :: chain
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        type(str_t) :: source
        type(kernel_spec_t) :: full
        type(expr_t), allocatable :: roots(:), values(:)
        type(str_t), allocatable :: locals(:)
        character(:), allocatable :: why
        logical :: good

        call prepare(k, chain, .true., .false., roots, locals, values)
        full = spec
        if (present(chain)) then
            full%locals = locals
            full%output_references = [locals, k%refs]
            roots = [values, roots]
        else
            full%output_references = k%refs
        end if
        source = emit_kernel(roots, full, good, message=why)
        call report(good, why, chars(spec%name), ok, message)
    end function emit_fortran

    !> One source per target from a single lowered IR: Fortran targets,
    !> TARGET_CUDA and TARGET_METAL. Bound functions become the symbols of
    !> their names. Without ok, a failure stops.
    function emit_targets(k, spec, targets, chain, share_products, ok, message) &
            result(sources)
        type(kernel_t), intent(in) :: k
        type(kernel_emit_spec_t), intent(in) :: spec
        integer, intent(in) :: targets(:)
        type(chain_t), intent(in), optional :: chain
        logical, intent(in), optional :: share_products
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message
        type(str_t), allocatable :: sources(:)
        type(kernel_emit_spec_t) :: each
        type(kernel_ir_t) :: ir
        type(expr_t), allocatable :: roots(:), values(:), names(:)
        type(str_t), allocatable :: locals(:)
        character(:), allocatable :: why
        logical :: good
        integer :: t, j

        allocate (sources(size(targets)))
        call prepare(k, chain, .false., .true., roots, locals, values)
        if (size(locals) > 0) then
            allocate (names(size(locals)))
            do j = 1, size(locals)
                names(j) = sym(roots(1)%a, chars(locals(j)))
            end do
            call lower_kernel_ir(roots, ir, good, why, names, values, &
                share_products=share_products)
        else
            call lower_kernel_ir(roots, ir, good, why, share_products=share_products)
        end if
        if (.not. good) then
            call report(good, why, chars(spec%name), ok, message)
            return
        end if
        do t = 1, size(targets)
            each = spec
            each%target = targets(t)
            select case (targets(t))
            case (TARGET_CUDA)
                sources(t) = emit_cuda_device_ir(ir, each, good, why)
            case (TARGET_METAL)
                sources(t) = emit_metal_device_ir(ir, each, good, why)
            case default
                sources(t) = emit_fortran_kernel_ir(ir, each, good, why)
            end select
            if (.not. good) exit
        end do
        call report(good, why, chars(spec%name), ok, message)
    end function emit_targets

    !> The roots after the known substitutions and, with a chain, the needed
    !> intermediates and their definitions. `fold` folds numbers after a
    !> known substitution; `bound` replaces bound functions by symbols before
    !> the chain decides which intermediates are needed.
    subroutine prepare(k, chain, fold, bound, roots, locals, values)
        type(kernel_t), intent(in) :: k
        type(chain_t), intent(in), optional :: chain
        logical, intent(in) :: fold, bound
        type(expr_t), allocatable, intent(out) :: roots(:), values(:)
        type(str_t), allocatable, intent(out) :: locals(:)
        integer, allocatable :: needed(:)
        logical :: substituted
        integer :: j

        roots = k%roots
        substituted = allocated(k%old)
        if (substituted) then
            do j = 1, size(roots)
                roots(j) = subs_many(roots(j), k%old, k%new)
                if (fold) roots(j) = fold_numbers(roots(j))
            end do
        end if
        if (bound) call bound_as_symbols(roots)
        allocate (locals(0), values(0))
        if (.not. present(chain)) return
        needed = chain%needed(roots)
        if (substituted) needed = pack(needed, [(.not. any(k%old%id == &
            chain%names(needed(j))%id), j=1, size(needed))])
        values = chain%defs(needed)
        locals = [(chain%names(needed(j))%name(), j=1, size(needed))]
        if (substituted) then
            do j = 1, size(values)
                values(j) = subs_many(values(j), k%old, k%new)
                if (fold) values(j) = fold_numbers(values(j))
            end do
        end if
        if (bound) call bound_as_symbols(values)
    end subroutine prepare

    !> Replace every applied function or partial that a binding of the
    !> current context names by the symbol of that name.
    subroutine bound_as_symbols(list)
        type(expr_t), intent(inout) :: list(:)
        type(kernel_binding_t), allocatable :: bindings(:)
        type(expr_t), allocatable :: old(:), new(:)
        integer :: j

        call active_bindings(bindings)
        if (.not. allocated(bindings)) return
        allocate (old(0), new(0))
        do j = 1, size(list)
            call collect(list(j))
        end do
        if (size(old) == 0) return
        do j = 1, size(list)
            list(j) = subs_many(list(j), old, new)
        end do

    contains

        recursive subroutine collect(e)
            type(expr_t), intent(in) :: e
            integer :: b, a

            if (e%kind() == NK_FUNC) then
                if (any(old%id == e%id)) return
                do b = 1, size(bindings)
                    if (.not. binding_matches(e%a, e%id, bindings(b))) cycle
                    if (index(chars(bindings(b)%replacement), '%args%') > 0) &
                        error stop "emit: a call binding has no IR form"
                    old = [old, e]
                    new = [new, sym(e%a, chars(bindings(b)%replacement))]
                    return
                end do
            end if
            do a = 1, e%nargs()
                call collect(e%arg(a))
            end do
        end subroutine collect

    end subroutine bound_as_symbols

    subroutine report(good, why, name, ok, message)
        logical, intent(in) :: good
        character(:), allocatable, intent(in) :: why
        character(*), intent(in) :: name
        logical, intent(out), optional :: ok
        character(:), allocatable, intent(out), optional :: message

        if (present(ok)) ok = good
        if (present(message)) then
            message = ''
            if (allocated(why)) message = why
        end if
        if (good .or. present(ok)) return
        if (allocated(why)) then
            error stop "emission of "//name//" failed: "//why
        else
            error stop "emission of "//name//" failed"
        end if
    end subroutine report

end module fortsym_emit
