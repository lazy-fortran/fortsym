module fortsym
    ! The short public Fortran surface. Explicit arenas and the lower-level
    ! modules remain available for callers that need independent state.
    use fortsym_arena, only: arena_t, node_kind_name, &
        NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, NK_POW, &
        NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, NK_ALGEBRAIC
    use fortsym_string, only: str_t, str, chars
    use fortsym_expr, only: expr_t, sym, num, rat, exact, real_expr, &
        real_text_expr, algebraic_expr, const, func, func_in, partial, pi_expr, e_expr, &
        i_expr, oo_expr, zoo_expr, nan_expr, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, &
        tanh, asinh, acosh, atanh, exp, log, sqrt, abs, erf, erfc, &
        gamma, besselj, legendrep, legendreq, &
        is_valid, same_arena, operator(+), operator(-), operator(*), &
        operator(/), operator(**), operator(==), operator(/=)
    use fortsym_relation, only: equal, unequal, less, less_equal, greater, &
        greater_equal
    use fortsym_predicates, only: is_number, is_algebraic
    use fortsym_subs, only: subs_impl => subs, subs_many_impl => subs_many
    use fortsym_eval, only: collect_free_symbols
    use fortsym_assume, only: assumption_context_t, &
        make_assumption_context, with_assumption, zero, negative, nonpositive, &
        positive, nonnegative, nonzero, real_valued, rational_valued, &
        integer_valued, positive_integer, algebraic_valued
    use fortsym_numeric, only: numeric_value, numeric_text, &
        numeric_precision_text, numeric_complex_text, &
        numeric_real_text_t, numeric_complex_text_t, numeric_callable_t
    use fortsym_engine, only: engine_result_t, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE, verdict_name
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_complexdom, only: complex_re_part => re_part, &
        complex_im_part => im_part, complex_conjugate => conjugate, &
        complex_arg_of => arg_of, complex_abs_of => abs_of, &
        complex_expand_expr => complex_expand
    use fortsym_kernel, only: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    use fortsym_backend, only: BACKEND_PROTOCOL_VERSION, EXPRESSION_SCHEMA, &
        BACKEND_PROVED, BACKEND_DISPROVED, BACKEND_UNKNOWN, &
        backend_evidence_t, backend_result_t, backend_status_name, &
        serialize_expression, deserialize_expression, assess_identity, &
        assess_equivalence, evidence_json, emit_backend_kernel
    use fortsym_ode, only: solve_ode
    use fortsym_chart, only: DIM, chart_t, chart_create, covariant_basis, &
        reciprocal_basis, metric_covariant, metric_contravariant, sqrtg, &
        jacobian, christoffel, grad, divergence, curl, laplacian
    use fortsym_chart_map, only: chart_map_t, chart_map_create, compose_maps, &
        map_valid, map_jacobian, &
        inverse_jacobian, transform_tensor, transform_form, pullback
    use fortsym_form, only: form_t, form, form_scalar, form_one, form_two, &
        form_three, form_zero, form_component, form_degree, form_valid, add_forms, &
        subtract_forms, negate_form, wedge, d, &
        exterior_diff, star, hodge_star, interior, interior_product, lie, &
        lie_derivative, flat, sharp, scale_form
    use fortsym_tensor, only: tensor_t, tensor, tensor_scalar, tensor_vector, &
        tensor_covector, tensor_from_components, tensor_from_matrix, &
        tensor_component, tensor_rank, tensor_variance, &
        tensor_density_weight, tensor_valid, tensor_same_arena, density, vector, covector, &
        raise, lower, tensor_product, contract, trace, &
        metric_covariant_tensor, metric_contravariant_tensor, UPPER, LOWER_VARIANCE, &
        MAX_RANK
    use fortsym_connection, only: covariant_diff, covariant_derivative, &
        christoffel_tensor, riemann_tensor, ricci_tensor, scalar_curvature, &
        einstein_tensor
    use fortsym_magnetic, only: b_con, b_cov, b_density, b_fourier, &
        b_fourier_density, j_fourier
    use fortsym_magnetic_weak, only: fourier_constitutive, &
        fourier_constitutive_t, fourier_weak_form, fourier_weak_form_t, &
        current_compatibility, nubar, fourier_constitutive_valid, &
        fourier_weak_form_valid, FOURIER_INVALID, FOURIER_LONGITUDINAL, &
        FOURIER_TRANSVERSE, SPACE_NONE, SPACE_NODAL, SPACE_EDGE, TRACE_NONE, &
        TRACE_NORMAL, TRACE_TANGENTIAL
    use fortsym_metric, only: metric_t, metric_create, metric_from_chart, &
        metric_det, metric_sqrtg, &
        metric_signature, metric_orientation, metric_valid, metric_arena, &
        metric_same_arena, metric_coordinates, metric_has_coordinates
    use fortsym_volume, only: metric_volume_density, levi_civita_symbol, &
        metric_levi_civita
    use fortsym_relativity, only: SPACETIME_DIM, spacetime_metric_t, &
        spacetime_metric_create, spacetime_metric_covariant, &
        spacetime_metric_contravariant, spacetime_metric_det, &
        spacetime_metric_sqrtg, spacetime_metric_signature, &
        spacetime_metric_orientation, spacetime_metric_dimension, &
        spacetime_metric_valid, &
        spacetime_metric_arena, spacetime_metric_coordinates, &
        spacetime_metric_has_coordinates, spacetime_christoffel, &
        spacetime_riemann, spacetime_ricci, spacetime_scalar_curvature, &
        spacetime_einstein, spacetime_geodesic_residual
    use fortsym_spacetime_form, only: spacetime_form_t, spacetime_form_zero, &
        spacetime_form_scalar, spacetime_form_one, spacetime_form_two, &
        spacetime_form_three, spacetime_form_four, spacetime_form_component, &
        spacetime_form_degree, spacetime_form_valid, spacetime_form_same_arena, &
        spacetime_wedge, &
        spacetime_d, spacetime_exterior_diff, spacetime_hodge, spacetime_star, &
        spacetime_codifferential, spacetime_interior, &
        spacetime_interior_product, spacetime_lie, spacetime_lie_derivative, &
        spacetime_laplace_de_rham
    use fortsym_maxwell, only: maxwell_field_strength, maxwell_gauge_transform, &
        maxwell_residual
    implicit none
    private

    public :: arena_t, node_kind_name
    public :: NK_INT, NK_RAT, NK_REAL, NK_SYM, NK_CONST, NK_ADD, NK_MUL, &
        NK_POW, NK_FUNC, NK_BIG_INT, NK_BIG_RAT, NK_BIG_REAL, NK_ALGEBRAIC
    public :: expr_t, sym, num, rat, exact, real_expr, real_text_expr, &
        algebraic_expr, const, &
        func, func_in, partial, equal, unequal, less, less_equal, greater, &
        greater_equal, pi_expr, e_expr, i_expr, oo_expr, zoo_expr, nan_expr, &
        is_valid, same_arena
    public :: is_number, is_algebraic
    public :: assumption_context_t, make_assumption_context, with_assumption, &
        zero, negative, nonpositive, positive, nonnegative, nonzero, real_valued, &
        rational_valued, integer_valued, positive_integer, algebraic_valued
    public :: str, chars
    public :: subs, subs_many, diff, simplify, refine, expand, factor, &
        operation_count, free_symbols
    public :: re_part, im_part, conjugate, arg_of, abs_of, complex_expand
    public :: kernel_spec_t, emit_kernel, KERNEL_SUBROUTINE
    public :: engine_result_t, zero_test, VERDICT_UNKNOWN, VERDICT_TRUE, &
        VERDICT_FALSE, verdict_name
    public :: numeric_value, numeric_text, numeric_precision_text, &
        numeric_real_text_t, numeric_complex_text, numeric_complex_text_t, &
        numeric_callable_t
    public :: BACKEND_PROTOCOL_VERSION, EXPRESSION_SCHEMA, BACKEND_PROVED, &
        BACKEND_DISPROVED, BACKEND_UNKNOWN, backend_evidence_t, &
        backend_result_t, backend_status_name, serialize_expression, &
        deserialize_expression, assess_identity, assess_equivalence, &
        evidence_json, emit_backend_kernel
    public :: solve_ode
    public :: DIM, chart_t, chart_create, covariant_basis, reciprocal_basis, &
        metric_covariant, metric_contravariant, sqrtg, jacobian, christoffel, &
        grad, divergence, curl, laplacian, chart_map_t, chart_map_create, compose_maps, &
        map_valid, map_jacobian, inverse_jacobian, transform_tensor, transform_form, &
        pullback, b_con, &
        b_cov, b_density, b_fourier, b_fourier_density, j_fourier, &
        fourier_constitutive, fourier_constitutive_t, fourier_weak_form, &
        fourier_weak_form_t, current_compatibility, nubar, &
        fourier_constitutive_valid, fourier_weak_form_valid, FOURIER_INVALID, &
        FOURIER_LONGITUDINAL, FOURIER_TRANSVERSE, SPACE_NONE, SPACE_NODAL, &
        SPACE_EDGE, TRACE_NONE, TRACE_NORMAL, TRACE_TANGENTIAL, &
        metric_t, metric_create, metric_from_chart, metric_det, metric_sqrtg, &
        metric_volume_density, levi_civita_symbol, metric_levi_civita, &
        metric_orientation, metric_valid, metric_arena, metric_same_arena, &
        metric_coordinates, metric_has_coordinates, &
        SPACETIME_DIM, spacetime_metric_t, spacetime_metric_create, &
        spacetime_metric_covariant, spacetime_metric_contravariant, &
        spacetime_metric_det, spacetime_metric_sqrtg, &
        spacetime_metric_signature, spacetime_metric_orientation, &
        spacetime_metric_valid, spacetime_metric_arena, &
        spacetime_metric_coordinates, spacetime_metric_has_coordinates, &
        spacetime_christoffel, spacetime_riemann, spacetime_ricci, &
        spacetime_scalar_curvature, spacetime_einstein, &
        spacetime_geodesic_residual, &
        spacetime_form_t, spacetime_form_zero, spacetime_form_scalar, &
        spacetime_form_one, spacetime_form_two, spacetime_form_three, &
        spacetime_form_four, spacetime_form_component, spacetime_form_degree, &
        spacetime_form_valid, spacetime_form_same_arena, spacetime_wedge, spacetime_d, &
        spacetime_exterior_diff, spacetime_hodge, spacetime_star, &
        spacetime_codifferential, spacetime_interior, &
        spacetime_interior_product, spacetime_lie, spacetime_lie_derivative, &
        spacetime_laplace_de_rham, &
        maxwell_field_strength, maxwell_gauge_transform, maxwell_residual, &
        form_t, form, form_scalar, form_one, form_two, &
        form_three, form_component, form_degree, form_valid, add_forms, &
        subtract_forms, negate_form, wedge, d, &
        exterior_diff, star, hodge_star, interior, interior_product, lie, &
        lie_derivative, flat, sharp, scale_form, tensor_t, tensor, &
        tensor_scalar, tensor_vector, tensor_covector, tensor_from_components, &
        tensor_from_matrix, tensor_component, tensor_rank, tensor_variance, &
        tensor_density_weight, tensor_valid, tensor_same_arena, density, vector, covector, raise, &
        lower, tensor_product, contract, trace, metric_covariant_tensor, &
        metric_contravariant_tensor, UPPER, LOWER_VARIANCE, MAX_RANK, &
        covariant_diff, covariant_derivative, christoffel_tensor, &
        riemann_tensor, ricci_tensor, scalar_curvature, einstein_tensor
    public :: operator(+), operator(-), operator(*), operator(/), operator(**), &
        operator(==), operator(/=)
    public :: sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh, &
        asinh, acosh, atanh, exp, log, sqrt, abs, erf, erfc, gamma, &
        besselj, legendrep, legendreq
    public :: default_arena, reset, symbols
    public :: assignment(=)

    type(arena_t), target, save :: default_store
    logical, save :: default_ready = .false.

    interface assignment(=)
        module procedure assign_character
    end interface assignment(=)

contains

    !> Return the process-local arena used by character assignment and symbols.
    !> It is single-threaded state. Callers that need concurrency should create
    !> an arena_t and use the explicit constructors from the same module.
    function default_arena() result(a)
        type(arena_t), pointer :: a
        call ensure_default()
        a => default_store
    end function default_arena

    !> Clear the convenience arena. Handles made before this call become stale,
    !> including handles whose node index is reused after the next construction.
    subroutine reset()
        call default_store%clear()
        default_ready = .false.
    end subroutine reset

    !> Replace one expression structurally.
    function subs(expression, old, new) result(result)
        type(expr_t), intent(in) :: expression, old, new
        type(engine_result_t) :: result

        if (.not. is_valid(expression)) then
            call report_failure(result, "subs: invalid expression")
            return
        end if
        if (.not. is_valid(old)) then
            call report_failure(result, "subs: invalid replacement")
            return
        end if
        if (.not. is_valid(new)) then
            call report_failure(result, "subs: invalid replacement")
            return
        end if
        if (.not. same_arena(expression, old) .or. &
            .not. same_arena(expression, new)) then
            call report_failure(result, "subs: expressions belong to different arenas")
            return
        end if

        result%value = subs_impl(expression, old, new)
        if (.not. is_valid(result%value)) then
            call report_failure(result, "subs: substitution failed")
        else
            result%ok = .true.
        end if
    end function subs

    !> Replace all old expressions simultaneously. Replacement expressions are
    !> not revisited, so swaps and coupled replacements do not cascade.
    function subs_many(expression, old, new) result(result)
        type(expr_t), intent(in) :: expression, old(:), new(:)
        type(engine_result_t) :: result
        integer :: k

        if (.not. is_valid(expression)) then
            call report_failure(result, "subs_many: invalid expression")
            return
        end if
        if (size(old) /= size(new)) then
            call report_failure(result, "subs_many: replacement size mismatch")
            return
        end if
        do k = 1, size(old)
            if (.not. is_valid(old(k)) .or. .not. is_valid(new(k))) then
                call report_failure(result, "subs_many: invalid replacement")
                return
            end if
            if (.not. same_arena(expression, old(k))) then
                call report_failure(result, "subs_many: expressions belong to different arenas")
                return
            end if
            if (.not. same_arena(expression, new(k))) then
                call report_failure(result, "subs_many: expressions belong to different arenas")
                return
            end if
        end do

        result%value = subs_many_impl(expression, old, new)
        if (.not. is_valid(result%value)) then
            call report_failure(result, "subs_many: substitution failed")
        else
            result%ok = .true.
        end if
    end function subs_many

    !> Differentiate and simplify through the native engine in the expression's
    !> arena. The low-level fortsym_diff module remains available when callers
    !> specifically need the unsimplified derivative DAG.
    function diff(expression, variable, assumptions) result(result)
        type(expr_t), intent(in) :: expression, variable
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression) .or. .not. is_valid(variable)) then
            call report_failure(result, "diff: invalid expression")
            return
        end if
        if (.not. same_arena(expression, variable)) then
            call report_failure(result, "diff: expressions belong to different arenas")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, "diff: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%diff(expression, variable)
    end function diff

    !> Simplify an expression with the native engine in its owning arena.
    function simplify(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "simplify: invalid expression")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "simplify: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%simplify(expression)
    end function simplify

    !> Refine an expression under an explicit assumption context. The native
    !> simplifier owns the guarded rewrite rules; refine is the named facade
    !> entry point for callers who are supplying domain facts.
    function refine(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        if (present(assumptions)) then
            result = simplify(expression, assumptions)
        else
            result = simplify(expression)
        end if
    end function refine

    !> Count operation occurrences using the same tree semantics as SymPy's
    !> count_ops. This is structural and does not simplify the expression.
    function operation_count(expression) result(n)
        type(expr_t), intent(in) :: expression
        integer                  :: n
        n = expression%operation_count()
    end function operation_count

    !> Collect the distinct free symbol names reachable from an expression.
    !> Caller-owned output avoids an allocatable function-result temporary. The
    !> traversal is owned by fortsym_eval; this facade only exposes the concise
    !> native spelling.
    subroutine free_symbols(expression, names)
        type(expr_t), intent(in) :: expression
        type(str_t), allocatable, intent(out) :: names(:)
        call collect_free_symbols(expression, names)
    end subroutine free_symbols

    !> Expand an expression with the native engine in its owning arena.
    function expand(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "expand: invalid expression")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "expand: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%expand(expression)
    end function expand

    !> Factor an expression with the native engine in its owning arena.
    function factor(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "factor: invalid expression")
            return
        end if

        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "factor: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%factor(expression)
    end function factor

    !> Return the native three-valued zero verdict for an expression.
    !> VERDICT_TRUE means proved zero, VERDICT_FALSE means proved nonzero, and
    !> VERDICT_UNKNOWN means the native engine declined to decide.
    function zero_test(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result
        type(native_engine_t) :: engine

        if (.not. is_valid(expression)) then
            call report_failure(result, "zero_test: invalid expression")
            return
        end if
        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "zero_test: assumptions belong to a different arena")
            return
        end if
        if (present(assumptions)) then
            engine = make_native_engine(expression%a, assumptions)
        else
            engine = make_native_engine(expression%a)
        end if
        result = engine%zero_test(expression)
    end function zero_test

    !> Apply one supported complex-domain operation through the shared
    !> complexdom owner, normalizing branch-aware results.
    function complex_operation(expression, assumptions, operation) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        integer, intent(in) :: operation
        type(engine_result_t) :: result
        type(assumption_context_t), target :: local_facts
        type(assumption_context_t), pointer :: facts
        type(native_engine_t) :: engine
        type(expr_t) :: value
        logical :: ok
        character(:), allocatable :: why

        if (.not. is_valid(expression)) then
            call report_failure(result, "complex operation: invalid expression")
            return
        end if
        if (.not. context_matches(expression, assumptions)) then
            call report_failure(result, &
                "complex operation: assumptions belong to a different arena")
            return
        end if

        if (present(assumptions)) then
            facts => assumptions
        else
            local_facts = make_assumption_context(expression%a)
            facts => local_facts
        end if

        select case (operation)
        case (1)
            call complex_re_part(expression, facts, value, ok, why)
        case (2)
            call complex_im_part(expression, facts, value, ok, why)
        case (3)
            call complex_conjugate(expression, facts, value, ok, why)
        case (4)
            call complex_arg_of(expression, facts, value, ok, why)
        case (5)
            call complex_abs_of(expression, facts, value, ok, why)
        case (6)
            call complex_expand_expr(expression, facts, value, ok, why)
        case default
            call report_failure(result, "complex operation: invalid operation")
            return
        end select
        if (.not. ok) then
            call report_failure(result, "complex operation refused: "//why)
            return
        end if

        ! The branch-aware results need the native simplifier for canonical
        ! principal-argument and modulus forms.
        if (operation == 4 .or. operation == 5 .or. operation == 6) then
            engine = make_native_engine(expression%a, facts)
            result = engine%simplify(value)
        else
            result%ok = .true.
            result%value = value
            result%verdict = VERDICT_UNKNOWN
            result%message = str("")
        end if
    end function complex_operation

    !> Return the real part under the supported complex-domain rules.
    function re_part(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        result = complex_operation(expression, assumptions, 1)
    end function re_part

    !> Return the imaginary part under the supported complex-domain rules.
    function im_part(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        result = complex_operation(expression, assumptions, 2)
    end function im_part

    !> Return the structural complex conjugate under the supported rules.
    function conjugate(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        result = complex_operation(expression, assumptions, 3)
    end function conjugate

    !> Return the principal argument under the supported branch rules.
    function arg_of(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        result = complex_operation(expression, assumptions, 4)
    end function arg_of

    !> Return the modulus under the supported complex-domain rules.
    function abs_of(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        result = complex_operation(expression, assumptions, 5)
    end function abs_of

    !> Return the rectangular complex expansion under the supported rules.
    function complex_expand(expression, assumptions) result(result)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, target, intent(in) :: assumptions
        type(engine_result_t) :: result

        result = complex_operation(expression, assumptions, 6)
    end function complex_expand

    !> Assigning text creates one symbol in the default arena. Text is never
    !> parsed as an expression.
    subroutine assign_character(lhs, rhs)
        type(expr_t), intent(out) :: lhs
        character(*), intent(in)   :: rhs
        call ensure_default()
        lhs = sym(default_store, trim(rhs))
    end subroutine assign_character

    subroutine report_failure(result, message)
        type(engine_result_t), intent(out) :: result
        character(*), intent(in) :: message

        result%ok = .false.
        result%value = expr_t()
        result%verdict = VERDICT_UNKNOWN
        result%message = str(message)
    end subroutine report_failure

    logical function context_matches(expression, assumptions) result(matches)
        type(expr_t), intent(in) :: expression
        type(assumption_context_t), optional, intent(in) :: assumptions

        matches = .true.
        if (.not. present(assumptions)) return
        matches = associated(assumptions%home)
        if (matches) matches = associated(assumptions%home, expression%a)
    end function context_matches

    !> Read up to eight whitespace- or comma-separated symbol names into scalar
    !> outputs. A missing output or an extra name sets ok=.false.; every output
    !> remains a valid symbol only when a corresponding name was present.
    subroutine symbols(names, first, second, third, fourth, fifth, sixth, &
            seventh, eighth, ok)
        character(*), intent(in) :: names
        type(expr_t), intent(out) :: first
        type(expr_t), intent(out), optional :: second, third, fourth, fifth
        type(expr_t), intent(out), optional :: sixth, seventh, eighth
        logical, intent(out), optional :: ok
        integer :: position
        logical :: good

        first = expr_t()
        if (present(second)) second = expr_t()
        if (present(third)) third = expr_t()
        if (present(fourth)) fourth = expr_t()
        if (present(fifth)) fifth = expr_t()
        if (present(sixth)) sixth = expr_t()
        if (present(seventh)) seventh = expr_t()
        if (present(eighth)) eighth = expr_t()

        call ensure_default()
        position = 1
        good = .true.
        call read_symbol(names, position, first, good)
        if (present(second)) call read_symbol(names, position, second, good)
        if (present(third)) call read_symbol(names, position, third, good)
        if (present(fourth)) call read_symbol(names, position, fourth, good)
        if (present(fifth)) call read_symbol(names, position, fifth, good)
        if (present(sixth)) call read_symbol(names, position, sixth, good)
        if (present(seventh)) call read_symbol(names, position, seventh, good)
        if (present(eighth)) call read_symbol(names, position, eighth, good)
        call skip_separators(names, position)
        if (position <= len_trim(names)) good = .false.
        if (present(ok)) ok = good
    end subroutine symbols

    subroutine ensure_default()
        if (default_ready) return
        call default_store%init()
        default_ready = .true.
    end subroutine ensure_default

    subroutine read_symbol(names, position, target, good)
        character(*), intent(in) :: names
        integer, intent(inout) :: position
        type(expr_t), intent(inout) :: target
        logical, intent(inout) :: good
        integer :: start, finish

        call skip_separators(names, position)
        if (position > len_trim(names)) then
            good = .false.
            return
        end if
        start = position
        do while (position <= len_trim(names))
            if (is_separator(names(position:position))) exit
            position = position + 1
        end do
        finish = position - 1
        target = sym(default_store, names(start:finish))
    end subroutine read_symbol

    subroutine skip_separators(names, position)
        character(*), intent(in) :: names
        integer, intent(inout) :: position
        do while (position <= len_trim(names))
            if (.not. is_separator(names(position:position))) return
            position = position + 1
        end do
    end subroutine skip_separators

    pure logical function is_separator(character)
        character, intent(in) :: character
        is_separator = character == ' ' .or. character == ',' .or. &
            iachar(character) == 9
    end function is_separator

end module fortsym
