module fortsym_capi
    ! Raw iso_c_binding interfaces to SymEngine's C ABI (symengine/cwrapper.h)
    ! and to fortsym's own shim (src/capi/fsym_shim.h).
    !
    ! This module is deliberately a transliteration and nothing more: no derived
    ! types, no allocation policy, no error translation. Everything above it in
    ! fortsym works with sym_t (see fortsym_expr) and never touches a c_ptr.
    !
    ! Handle representation. SymEngine's `basic` is
    !     typedef basic_struct basic[1];
    ! an array of one element, which in C decays to a pointer at every call
    ! site. Fortran therefore sees every `basic` argument as a plain c_ptr
    ! passed by value. Handles come from basic_new_heap() and are released with
    ! basic_free_heap(); the stack form (basic_new_stack) has no Fortran
    ! equivalent and is not bound.
    !
    ! Const-correctness. cwrapper.h marks input handles `const basic`, but the
    ! C type is a pointer either way, so both input and output handles bind as
    ! `type(c_ptr), value`. Which arguments a routine writes to is documented by
    ! cwrapper.h, not enforced here.
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_long, &
        c_int64_t, c_double, c_size_t
    implicit none
    public

    ! symengine_exceptions_t (symengine/symengine_exception.h). Every
    ! CWRAPPER_OUTPUT_TYPE result is one of these.
    integer(c_int), parameter :: SYMENGINE_NO_EXCEPTION = 0_c_int
    integer(c_int), parameter :: SYMENGINE_RUNTIME_ERROR = 1_c_int
    integer(c_int), parameter :: SYMENGINE_DIV_BY_ZERO = 2_c_int
    integer(c_int), parameter :: SYMENGINE_NOT_IMPLEMENTED = 3_c_int
    integer(c_int), parameter :: SYMENGINE_DOMAIN_ERROR = 4_c_int
    integer(c_int), parameter :: SYMENGINE_PARSE_ERROR = 5_c_int
    integer(c_int), parameter :: SYMENGINE_SERIALIZATION_ERROR = 6_c_int

    ! Rendering modes accepted by fsym_str_render (src/capi/fsym_shim.h).
    integer(c_int), parameter :: FSYM_STR_DEFAULT = 0_c_int
    integer(c_int), parameter :: FSYM_STR_CCODE = 1_c_int
    integer(c_int), parameter :: FSYM_STR_LATEX = 2_c_int
    integer(c_int), parameter :: FSYM_STR_JULIA = 3_c_int
    integer(c_int), parameter :: FSYM_EXACT_ADD = 1_c_int
    integer(c_int), parameter :: FSYM_EXACT_SUB = 2_c_int
    integer(c_int), parameter :: FSYM_EXACT_MUL = 3_c_int
    integer(c_int), parameter :: FSYM_EXACT_DIV = 4_c_int
    integer(c_int), parameter :: FSYM_ALGEBRAIC_ADD = 1_c_int
    integer(c_int), parameter :: FSYM_ALGEBRAIC_SUB = 2_c_int
    integer(c_int), parameter :: FSYM_ALGEBRAIC_MUL = 3_c_int
    integer(c_int), parameter :: FSYM_ALGEBRAIC_DIV = 4_c_int
    integer(c_int), parameter :: FSYM_ALGEBRAIC_CONJ = 1_c_int
    integer(c_int), parameter :: FSYM_ALGEBRAIC_SQRT = 2_c_int

    ! Verdicts from fsym_zero_test. UNKNOWN is not a failure: it means the
    ! expression left the fragment the symbolic procedure decides, and the
    ! caller should fall back to a numeric probe rather than trust a guess.
    integer(c_int), parameter :: FSYM_ZERO_UNKNOWN = 0_c_int
    integer(c_int), parameter :: FSYM_ZERO_TRUE = 1_c_int
    integer(c_int), parameter :: FSYM_ZERO_FALSE = 2_c_int

    interface

        ! -------------------------------------------------------- lifetime --

        function basic_new_heap() bind(c, name="basic_new_heap") result(s)
            import :: c_ptr
            type(c_ptr) :: s
        end function basic_new_heap

        subroutine basic_free_heap(s) bind(c, name="basic_free_heap")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_free_heap

        function basic_assign(a, b) bind(c, name="basic_assign") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: a, b
            integer(c_int)     :: rc
        end function basic_assign

        ! ---------------------------------------------------- construction --

        function symbol_set(s, c) bind(c, name="symbol_set") result(rc)
            import :: c_ptr, c_char, c_int
            type(c_ptr),    value      :: s
            character(kind=c_char), intent(in) :: c(*)
            integer(c_int)             :: rc
        end function symbol_set

        function basic_parse(b, str) bind(c, name="basic_parse") result(rc)
            import :: c_ptr, c_char, c_int
            type(c_ptr),    value      :: b
            character(kind=c_char), intent(in) :: str(*)
            integer(c_int)             :: rc
        end function basic_parse

        function integer_set_si(s, i) bind(c, name="integer_set_si") result(rc)
            import :: c_ptr, c_long, c_int
            type(c_ptr),    value :: s
            integer(c_long), value :: i
            integer(c_int)        :: rc
        end function integer_set_si

        function rational_set_si(s, i, j) bind(c, name="rational_set_si") &
                result(rc)
            import :: c_ptr, c_long, c_int
            type(c_ptr),    value  :: s
            integer(c_long), value :: i, j
            integer(c_int)         :: rc
        end function rational_set_si

        function real_double_set_d(s, d) bind(c, name="real_double_set_d") &
                result(rc)
            import :: c_ptr, c_double, c_int
            type(c_ptr),   value  :: s
            real(c_double), value :: d
            integer(c_int)        :: rc
        end function real_double_set_d

        function real_double_get_d(s) bind(c, name="real_double_get_d") &
                result(d)
            import :: c_ptr, c_double
            type(c_ptr), value :: s
            real(c_double)     :: d
        end function real_double_get_d

        ! ------------------------------------------------------- constants --

        subroutine basic_const_zero(s) bind(c, name="basic_const_zero")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_zero

        subroutine basic_const_one(s) bind(c, name="basic_const_one")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_one

        subroutine basic_const_minus_one(s) bind(c, name="basic_const_minus_one")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_minus_one

        subroutine basic_const_I(s) bind(c, name="basic_const_I")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_I

        subroutine basic_const_pi(s) bind(c, name="basic_const_pi")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_pi

        subroutine basic_const_E(s) bind(c, name="basic_const_E")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_E

        subroutine basic_const_EulerGamma(s) bind(c, name="basic_const_EulerGamma")
            import :: c_ptr
            type(c_ptr), value :: s
        end subroutine basic_const_EulerGamma

        ! ------------------------------------------------------ arithmetic --

        function basic_add(s, a, b) bind(c, name="basic_add") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a, b
            integer(c_int)     :: rc
        end function basic_add

        function basic_sub(s, a, b) bind(c, name="basic_sub") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a, b
            integer(c_int)     :: rc
        end function basic_sub

        function basic_mul(s, a, b) bind(c, name="basic_mul") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a, b
            integer(c_int)     :: rc
        end function basic_mul

        function basic_div(s, a, b) bind(c, name="basic_div") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a, b
            integer(c_int)     :: rc
        end function basic_div

        function basic_pow(s, a, b) bind(c, name="basic_pow") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a, b
            integer(c_int)     :: rc
        end function basic_pow

        function basic_neg(s, a) bind(c, name="basic_neg") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_neg

        function basic_atan2(s, a, b) bind(c, name="basic_atan2") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a, b
            integer(c_int)     :: rc
        end function basic_atan2

        ! -------------------------------------------- elementary functions --
        ! Every one-argument function shares the shape rc = f(out, in). They are
        ! bound individually because C has no way to enumerate them.

        function basic_abs(s, a) bind(c, name="basic_abs") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_abs

        function basic_sqrt(s, a) bind(c, name="basic_sqrt") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_sqrt

        function basic_cbrt(s, a) bind(c, name="basic_cbrt") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_cbrt

        function basic_exp(s, a) bind(c, name="basic_exp") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_exp

        function basic_log(s, a) bind(c, name="basic_log") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_log

        function basic_sin(s, a) bind(c, name="basic_sin") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_sin

        function basic_cos(s, a) bind(c, name="basic_cos") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_cos

        function basic_tan(s, a) bind(c, name="basic_tan") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_tan

        function basic_asin(s, a) bind(c, name="basic_asin") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_asin

        function basic_acos(s, a) bind(c, name="basic_acos") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_acos

        function basic_atan(s, a) bind(c, name="basic_atan") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_atan

        function basic_sinh(s, a) bind(c, name="basic_sinh") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_sinh

        function basic_cosh(s, a) bind(c, name="basic_cosh") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_cosh

        function basic_tanh(s, a) bind(c, name="basic_tanh") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_tanh

        function basic_asinh(s, a) bind(c, name="basic_asinh") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_asinh

        function basic_acosh(s, a) bind(c, name="basic_acosh") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_acosh

        function basic_atanh(s, a) bind(c, name="basic_atanh") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_atanh

        function basic_erf(s, a) bind(c, name="basic_erf") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_erf

        function basic_erfc(s, a) bind(c, name="basic_erfc") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_erfc

        function basic_gamma(s, a) bind(c, name="basic_gamma") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_gamma

        function basic_loggamma(s, a) bind(c, name="basic_loggamma") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_loggamma

        function basic_sign(s, a) bind(c, name="basic_sign") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_sign

        function basic_floor(s, a) bind(c, name="basic_floor") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_floor

        function basic_ceiling(s, a) bind(c, name="basic_ceiling") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_ceiling

        ! ------------------------------------------------------ comparison --

        function basic_eq(a, b) bind(c, name="basic_eq") result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: a, b
            integer(c_int)     :: r
        end function basic_eq

        function basic_neq(a, b) bind(c, name="basic_neq") result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: a, b
            integer(c_int)     :: r
        end function basic_neq

        function basic_hash(s) bind(c, name="basic_hash") result(h)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: s
            integer(c_size_t)  :: h
        end function basic_hash

        ! ------------------------------------------------------- transforms --

        function basic_diff(s, expr, sym) bind(c, name="basic_diff") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, expr, sym
            integer(c_int)     :: rc
        end function basic_diff

        function basic_expand(s, a) bind(c, name="basic_expand") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, a
            integer(c_int)     :: rc
        end function basic_expand

        function basic_subs2(s, e, a, b) bind(c, name="basic_subs2") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, e, a, b
            integer(c_int)     :: rc
        end function basic_subs2

        function basic_as_numer_denom(numer, denom, x) &
                bind(c, name="basic_as_numer_denom") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: numer, denom, x
            integer(c_int)     :: rc
        end function basic_as_numer_denom

        function basic_evalf(s, b, bits, real_only) bind(c, name="basic_evalf") &
                result(rc)
            import :: c_ptr, c_int, c_long
            type(c_ptr),    value  :: s, b
            integer(c_long), value :: bits
            integer(c_int),  value :: real_only
            integer(c_int)         :: rc
        end function basic_evalf

        ! -------------------------------------------------------- structure --

        function basic_get_type(s) bind(c, name="basic_get_type") result(id)
            import :: c_ptr, c_int
            type(c_ptr), value :: s
            integer(c_int)     :: id
        end function basic_get_type

        function basic_get_class_from_id(id) &
                bind(c, name="basic_get_class_from_id") result(p)
            import :: c_int, c_ptr
            integer(c_int), value :: id
            type(c_ptr)           :: p
        end function basic_get_class_from_id

        function basic_get_args(self, args) bind(c, name="basic_get_args") &
                result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: self, args
            integer(c_int)     :: rc
        end function basic_get_args

        function basic_free_symbols(self, symbols) &
                bind(c, name="basic_free_symbols") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: self, symbols
            integer(c_int)     :: rc
        end function basic_free_symbols

        function function_symbol_set(s, c, arg) &
                bind(c, name="function_symbol_set") result(rc)
            import :: c_ptr, c_char, c_int
            type(c_ptr),    value      :: s, arg
            character(kind=c_char), intent(in) :: c(*)
            integer(c_int)             :: rc
        end function function_symbol_set

        function basic_has_symbol(e, s) bind(c, name="basic_has_symbol") &
                result(r)
            import :: c_ptr, c_int
            type(c_ptr), value :: e, s
            integer(c_int)     :: r
        end function basic_has_symbol

        ! -------------------------------------------------------------- cse --

        function basic_cse(replacement_syms, replacement_exprs, reduced_exprs, &
                exprs) bind(c, name="basic_cse") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: replacement_syms, replacement_exprs
            type(c_ptr), value :: reduced_exprs, exprs
            integer(c_int)     :: rc
        end function basic_cse

        ! --------------------------------------------------------- vecbasic --

        function vecbasic_new() bind(c, name="vecbasic_new") result(v)
            import :: c_ptr
            type(c_ptr) :: v
        end function vecbasic_new

        subroutine vecbasic_free(self) bind(c, name="vecbasic_free")
            import :: c_ptr
            type(c_ptr), value :: self
        end subroutine vecbasic_free

        function vecbasic_push_back(self, value_) &
                bind(c, name="vecbasic_push_back") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: self, value_
            integer(c_int)     :: rc
        end function vecbasic_push_back

        function vecbasic_get(self, n, result_) bind(c, name="vecbasic_get") &
                result(rc)
            import :: c_ptr, c_size_t, c_int
            type(c_ptr),      value :: self, result_
            integer(c_size_t), value :: n
            integer(c_int)          :: rc
        end function vecbasic_get

        function vecbasic_size(self) bind(c, name="vecbasic_size") result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: self
            integer(c_size_t)  :: n
        end function vecbasic_size

        function vecbasic_linsolve(sol, sys, sym) &
                bind(c, name="vecbasic_linsolve") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: sol, sys, sym
            integer(c_int)     :: rc
        end function vecbasic_linsolve

        ! --------------------------------------------------------- setbasic --

        function setbasic_new() bind(c, name="setbasic_new") result(s)
            import :: c_ptr
            type(c_ptr) :: s
        end function setbasic_new

        subroutine setbasic_free(self) bind(c, name="setbasic_free")
            import :: c_ptr
            type(c_ptr), value :: self
        end subroutine setbasic_free

        subroutine setbasic_get(self, n, result_) bind(c, name="setbasic_get")
            import :: c_ptr, c_int
            type(c_ptr),   value :: self, result_
            integer(c_int), value :: n
        end subroutine setbasic_get

        function setbasic_size(self) bind(c, name="setbasic_size") result(n)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: self
            integer(c_size_t)  :: n
        end function setbasic_size

        ! ----------------------------------------------------- dense matrix --

        function dense_matrix_new() bind(c, name="dense_matrix_new") result(m)
            import :: c_ptr
            type(c_ptr) :: m
        end function dense_matrix_new

        function dense_matrix_new_vec(rows, cols, l) &
                bind(c, name="dense_matrix_new_vec") result(m)
            import :: c_ptr, c_int
            integer(c_int), value :: rows, cols
            type(c_ptr),    value :: l
            type(c_ptr)           :: m
        end function dense_matrix_new_vec

        function dense_matrix_new_rows_cols(r, c) &
                bind(c, name="dense_matrix_new_rows_cols") result(m)
            import :: c_ptr, c_int
            integer(c_int), value :: r, c
            type(c_ptr)           :: m
        end function dense_matrix_new_rows_cols

        subroutine dense_matrix_free(self) bind(c, name="dense_matrix_free")
            import :: c_ptr
            type(c_ptr), value :: self
        end subroutine dense_matrix_free

        function dense_matrix_get_basic(s, mat, r, c) &
                bind(c, name="dense_matrix_get_basic") result(rc)
            import :: c_ptr, c_int
            type(c_ptr),    value :: s, mat
            integer(c_int), value :: r, c
            integer(c_int)        :: rc
        end function dense_matrix_get_basic

        function dense_matrix_set_basic(mat, r, c, s) &
                bind(c, name="dense_matrix_set_basic") result(rc)
            import :: c_ptr, c_int
            type(c_ptr),    value :: mat, s
            integer(c_int), value :: r, c
            integer(c_int)        :: rc
        end function dense_matrix_set_basic

        function dense_matrix_rows(s) bind(c, name="dense_matrix_rows") &
                result(n)
            import :: c_ptr, c_long
            type(c_ptr), value :: s
            integer(c_long)    :: n
        end function dense_matrix_rows

        function dense_matrix_cols(s) bind(c, name="dense_matrix_cols") &
                result(n)
            import :: c_ptr, c_long
            type(c_ptr), value :: s
            integer(c_long)    :: n
        end function dense_matrix_cols

        function dense_matrix_det(s, mat) bind(c, name="dense_matrix_det") &
                result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, mat
            integer(c_int)     :: rc
        end function dense_matrix_det

        function dense_matrix_inv(s, mat) bind(c, name="dense_matrix_inv") &
                result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, mat
            integer(c_int)     :: rc
        end function dense_matrix_inv

        function dense_matrix_transpose(s, mat) &
                bind(c, name="dense_matrix_transpose") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, mat
            integer(c_int)     :: rc
        end function dense_matrix_transpose

        function dense_matrix_mul_matrix(s, matA, matB) &
                bind(c, name="dense_matrix_mul_matrix") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: s, matA, matB
            integer(c_int)     :: rc
        end function dense_matrix_mul_matrix

        function dense_matrix_jacobian(result_, A, x) &
                bind(c, name="dense_matrix_jacobian") result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: result_, A, x
            integer(c_int)     :: rc
        end function dense_matrix_jacobian

        ! ------------------------------------------------- fortsym own shim --

        function fsym_have_llvm() bind(c, name="fsym_have_llvm") result(r)
            import :: c_int
            integer(c_int) :: r
        end function fsym_have_llvm

        function fsym_have_mpfr() bind(c, name="fsym_have_mpfr") result(r)
            import :: c_int
            integer(c_int) :: r
        end function fsym_have_mpfr

        function fsym_symengine_version() &
                bind(c, name="fsym_symengine_version") result(p)
            import :: c_ptr
            type(c_ptr) :: p
        end function fsym_symengine_version

        function fsym_str_render(s, mode) bind(c, name="fsym_str_render") &
                result(n)
            import :: c_ptr, c_int, c_size_t
            type(c_ptr),    value :: s
            integer(c_int), value :: mode
            integer(c_size_t)     :: n
        end function fsym_str_render

        function fsym_str_fetch(buf, n) bind(c, name="fsym_str_fetch") &
                result(m)
            import :: c_char, c_size_t
            character(kind=c_char), intent(inout) :: buf(*)
            integer(c_size_t), value              :: n
            integer(c_size_t)                     :: m
        end function fsym_str_fetch

        function fsym_exact_normalize(value) &
                bind(c, name="fsym_exact_normalize") result(n)
            import :: c_char, c_size_t
            character(kind=c_char), intent(in) :: value(*)
            integer(c_size_t)                  :: n
        end function fsym_exact_normalize

        function fsym_exact_binary(left, right, operation) &
                bind(c, name="fsym_exact_binary") result(n)
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: left(*), right(*)
            integer(c_int), value              :: operation
            integer(c_size_t)                  :: n
        end function fsym_exact_binary

        function fsym_exact_pow_si(base, exponent) &
                bind(c, name="fsym_exact_pow_si") result(n)
            import :: c_char, c_int64_t, c_size_t
            character(kind=c_char), intent(in) :: base(*)
            integer(c_int64_t), value          :: exponent
            integer(c_size_t)                  :: n
        end function fsym_exact_pow_si

        function fsym_exact_fetch(buf, n) bind(c, name="fsym_exact_fetch") &
                result(m)
            import :: c_char, c_size_t
            character(kind=c_char), intent(inout) :: buf(*)
            integer(c_size_t), value              :: n
            integer(c_size_t)                     :: m
        end function fsym_exact_fetch

        function fsym_exact_get_d(value, result_value) &
                bind(c, name="fsym_exact_get_d") result(ok)
            import :: c_char, c_double, c_int
            character(kind=c_char), intent(in) :: value(*)
            real(c_double), intent(out)         :: result_value
            integer(c_int)                     :: ok
        end function fsym_exact_get_d

        function fsym_flint_is_shared() bind(c, name="fsym_flint_is_shared") &
                result(shared)
            import :: c_int
            integer(c_int) :: shared
        end function fsym_flint_is_shared

        function fsym_mpfr_is_shared() bind(c, name="fsym_mpfr_is_shared") &
                result(shared)
            import :: c_int
            integer(c_int) :: shared
        end function fsym_mpfr_is_shared

        function fsym_mpfr_ulp_error(reference, observed, error) &
                bind(c, name="fsym_mpfr_ulp_error") result(ok)
            import :: c_char, c_double, c_int
            character(kind=c_char), intent(in) :: reference(*)
            real(c_double), value                :: observed
            real(c_double), intent(out)          :: error
            integer(c_int)                        :: ok
        end function fsym_mpfr_ulp_error

        function fsym_algebraic_normalize(value) &
                bind(c, name="fsym_algebraic_normalize") result(n)
            import :: c_char, c_size_t
            character(kind=c_char), intent(in) :: value(*)
            integer(c_size_t)                  :: n
        end function fsym_algebraic_normalize

        function fsym_algebraic_i() bind(c, name="fsym_algebraic_i") result(n)
            import :: c_size_t
            integer(c_size_t) :: n
        end function fsym_algebraic_i

        function fsym_algebraic_from_re_im(real_part, imag_part) &
                bind(c, name="fsym_algebraic_from_re_im") result(n)
            import :: c_char, c_size_t
            character(kind=c_char), intent(in) :: real_part(*), imag_part(*)
            integer(c_size_t)                  :: n
        end function fsym_algebraic_from_re_im

        function fsym_algebraic_binary(left, right, operation) &
                bind(c, name="fsym_algebraic_binary") result(n)
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: left(*), right(*)
            integer(c_int), value              :: operation
            integer(c_size_t)                  :: n
        end function fsym_algebraic_binary

        function fsym_algebraic_unary(value, operation) &
                bind(c, name="fsym_algebraic_unary") result(n)
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int), value              :: operation
            integer(c_size_t)                  :: n
        end function fsym_algebraic_unary

        function fsym_algebraic_pow_si(base, exponent) &
                bind(c, name="fsym_algebraic_pow_si") result(n)
            import :: c_char, c_int64_t, c_size_t
            character(kind=c_char), intent(in) :: base(*)
            integer(c_int64_t), value          :: exponent
            integer(c_size_t)                  :: n
        end function fsym_algebraic_pow_si

        function fsym_algebraic_signs(value, real_sign, imag_sign) &
                bind(c, name="fsym_algebraic_signs") result(ok)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: value(*)
            integer(c_int), intent(out)         :: real_sign, imag_sign
            integer(c_int)                     :: ok
        end function fsym_algebraic_signs

        function fsym_algebraic_fetch(buf, n) &
                bind(c, name="fsym_algebraic_fetch") result(m)
            import :: c_char, c_size_t
            character(kind=c_char), intent(inout) :: buf(*)
            integer(c_size_t), value              :: n
            integer(c_size_t)                     :: m
        end function fsym_algebraic_fetch

        function fsym_series(out, ex, var, prec) bind(c, name="fsym_series") &
                result(rc)
            import :: c_ptr, c_int
            type(c_ptr),    value :: out, ex, var
            integer(c_int), value :: prec
            integer(c_int)        :: rc
        end function fsym_series

        function fsym_simplify(out, ex) bind(c, name="fsym_simplify") &
                result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: out, ex
            integer(c_int)     :: rc
        end function fsym_simplify

        function fsym_count_ops(ex) bind(c, name="fsym_count_ops") result(n)
            import :: c_ptr, c_int
            type(c_ptr), value :: ex
            integer(c_int)     :: n
        end function fsym_count_ops

        function fsym_zero_test(ex, max_nodes) bind(c, name="fsym_zero_test") &
                result(verdict)
            import :: c_ptr, c_int, c_long
            type(c_ptr),     value :: ex
            integer(c_long), value :: max_nodes
            integer(c_int)         :: verdict
        end function fsym_zero_test

        function fsym_normal_form(out, ex) bind(c, name="fsym_normal_form") &
                result(rc)
            import :: c_ptr, c_int
            type(c_ptr), value :: out, ex
            integer(c_int)     :: rc
        end function fsym_normal_form

    end interface

end module fortsym_capi
