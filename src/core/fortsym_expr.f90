module fortsym_expr
    ! The expression handle users actually hold, and the operators that build it.
    !
    ! An expr_t is a node index plus the arena it belongs to. Both are small and
    ! copied by value, so expressions can be assigned, stored in arrays and
    ! returned from functions without any ownership question.
    !
    ! The arena travels with the expression rather than living in a module
    ! variable. That costs one pointer per handle and buys two things: several
    ! independent problems can be under construction at once, and there is no
    ! global mutable state to reset between tests.
    !
    ! Operators are not pure. Building an expression interns nodes, which
    ! mutates the arena through the pointer, and Fortran rightly forbids that in
    ! a pure procedure. The mutation is invisible -- interning the same
    ! expression twice returns the same index and changes nothing.
    !
    ! Nothing here simplifies. `x - x` builds a sum, not zero. Deciding that the
    ! sum is zero is the engines' job, and keeping the two apart is what lets the
    ! frontend stay engine-agnostic.
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use fortsym_string, only: str_t
    use fortsym_arena, only: arena_t, &
        NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    implicit none
    private

    public :: expr_t
    public :: sym, num, rat, real_expr, const, func
    public :: pi_expr, e_expr, i_expr
    public :: is_valid, same_arena
    public :: operator(+), operator(-), operator(*), operator(/), &
        operator(**), operator(==), operator(/=)
    public :: sin, cos, tan, asin, acos, atan, atan2, &
        sinh, cosh, tanh, asinh, acosh, atanh, &
        exp, log, sqrt, abs, erf, erfc, gamma

    integer, parameter :: dp = real64

    !> A node in a particular arena. A default-initialised expr_t is invalid;
    !> is_valid distinguishes it from a real expression.
    type :: expr_t
        type(arena_t), pointer :: a => null()
        integer                :: id = 0
    contains
        procedure :: kind => expr_kind
        procedure :: nargs => expr_nargs
        procedure :: arg => expr_arg
        procedure :: name => expr_name
        procedure :: int_value => expr_int_value
        procedure :: den_value => expr_den_value
        procedure :: real_value => expr_real_value
        procedure :: node_count => expr_node_count
    end type expr_t

    interface num
        module procedure num_from_int32
        module procedure num_from_int64
    end interface num

    interface operator(+)
        module procedure add_ee, add_ei, add_ie, add_er, add_re, plus_e
    end interface operator(+)

    interface operator(-)
        module procedure sub_ee, sub_ei, sub_ie, sub_er, sub_re, neg_e
    end interface operator(-)

    interface operator(*)
        module procedure mul_ee, mul_ei, mul_ie, mul_er, mul_re
    end interface operator(*)

    interface operator(/)
        module procedure div_ee, div_ei, div_ie, div_er, div_re
    end interface operator(/)

    interface operator(**)
        module procedure pow_ee, pow_ei, pow_er
    end interface operator(**)

    interface operator(==)
        module procedure eq_ee
    end interface operator(==)

    interface operator(/=)
        module procedure ne_ee
    end interface operator(/=)

    ! Elementary functions, so that sin(x) reads the same whether x is a real or
    ! an expression.
    interface sin;   module procedure fn_sin;   end interface
        interface cos;   module procedure fn_cos;   end interface
            interface tan;   module procedure fn_tan;   end interface
                interface asin;  module procedure fn_asin;  end interface
                    interface acos;  module procedure fn_acos;  end interface
                        interface atan;  module procedure fn_atan;  end interface
                            interface atan2; module procedure fn_atan2; end interface
                                interface sinh;  module procedure fn_sinh;  end interface
                                    interface cosh;  module procedure fn_cosh;  end interface
                                        interface tanh;  module procedure fn_tanh;  end interface
                                            interface asinh; module procedure fn_asinh; end interface
                                                interface acosh; module procedure fn_acosh; end interface
                                                    interface atanh; module procedure fn_atanh; end interface
                                                        interface exp;   module procedure fn_exp;   end interface
                                                            interface log;   module procedure fn_log;   end interface
                                                                interface sqrt;  module procedure fn_sqrt;  end interface
                                                                    interface abs;   module procedure fn_abs;   end interface
                                                                        interface erf;   module procedure fn_erf;   end interface
                                                                            interface erfc;  module procedure fn_erfc;  end interface
                                                                                interface gamma; module procedure fn_gamma; end interface

                                                                                contains

                                                                                    ! ------------------------------------------------------ constructors --

                                                                                    function sym(a, name) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        character(*),          intent(in)    :: name
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%sym(name)
                                                                                    end function sym

                                                                                    function num_from_int32(a, value) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        integer(int32),        intent(in)    :: value
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%int(int(value, int64))
                                                                                    end function num_from_int32

                                                                                    function num_from_int64(a, value) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        integer(int64),        intent(in)    :: value
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%int(value)
                                                                                    end function num_from_int64

                                                                                    !> An exact rational. Kept exact rather than converted to a real, because a
                                                                                    !> generated kernel must emit 1/3 as a division of typed literals, not as a
                                                                                    !> rounded decimal.
                                                                                    function rat(a, numer, denom) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        integer(int64),        intent(in)    :: numer, denom
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%rat(numer, denom)
                                                                                    end function rat

                                                                                    function real_expr(a, value) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        real(dp),              intent(in)    :: value
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%real(value)
                                                                                    end function real_expr

                                                                                    function const(a, name) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        character(*),          intent(in)    :: name
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%const(name)
                                                                                    end function const

                                                                                    function pi_expr(a) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        type(expr_t)                         :: e
                                                                                        e = const(a, "pi")
                                                                                    end function pi_expr

                                                                                    function e_expr(a) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        type(expr_t)                         :: e
                                                                                        e = const(a, "e")
                                                                                    end function e_expr

                                                                                    function i_expr(a) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        type(expr_t)                         :: e
                                                                                        e = const(a, "i")
                                                                                    end function i_expr

                                                                                    !> Application of a named function to a list of arguments. The general form
                                                                                    !> behind every elementary function below, and the way a caller reaches a
                                                                                    !> function fortsym has no named wrapper for.
                                                                                    function func(name, fargs) result(e)
                                                                                        character(*), intent(in) :: name
                                                                                        type(expr_t), intent(in) :: fargs(:)
                                                                                        type(expr_t)             :: e
                                                                                        integer, allocatable :: ids(:)
                                                                                        integer :: k

                                                                                        allocate (ids(size(fargs)))
                                                                                        do k = 1, size(fargs)
                                                                                            ids(k) = fargs(k)%id
                                                                                        end do
                                                                                        e%a => fargs(1)%a
                                                                                        e%id = fargs(1)%a%func(name, ids)
                                                                                    end function func

                                                                                    ! ---------------------------------------------------------- predicates --

                                                                                    pure function is_valid(e) result(yes)
                                                                                        type(expr_t), intent(in) :: e
                                                                                        logical                  :: yes
                                                                                        yes = associated(e%a) .and. e%id > 0
                                                                                    end function is_valid

                                                                                    !> Two expressions can only be combined when they live in the same arena;
                                                                                    !> node indices from different arenas are unrelated integers.
                                                                                    pure function same_arena(x, y) result(yes)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        logical                  :: yes
                                                                                        yes = associated(x%a, y%a)
                                                                                    end function same_arena

                                                                                    ! ----------------------------------------------------------- accessors --

                                                                                    function expr_kind(self) result(k)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        integer                   :: k
                                                                                        k = self%a%kind_of(self%id)
                                                                                    end function expr_kind

                                                                                    function expr_nargs(self) result(n)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        integer                   :: n
                                                                                        n = self%a%nargs_of(self%id)
                                                                                    end function expr_nargs

                                                                                    function expr_arg(self, k) result(e)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        integer,       intent(in) :: k
                                                                                        type(expr_t)              :: e
                                                                                        e%a => self%a
                                                                                        e%id = self%a%arg_of(self%id, k)
                                                                                    end function expr_arg

                                                                                    function expr_name(self) result(s)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        type(str_t)               :: s
                                                                                        s = self%a%name_of(self%id)
                                                                                    end function expr_name

                                                                                    function expr_int_value(self) result(v)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        integer(int64)            :: v
                                                                                        v = self%a%num_of(self%id)
                                                                                    end function expr_int_value

                                                                                    function expr_den_value(self) result(v)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        integer(int64)            :: v
                                                                                        v = self%a%den_of(self%id)
                                                                                    end function expr_den_value

                                                                                    function expr_real_value(self) result(v)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        real(dp)                  :: v
                                                                                        v = self%a%real_of(self%id)
                                                                                    end function expr_real_value

                                                                                    !> Distinct nodes reachable from this expression. Shared subtrees count
                                                                                    !> once, so this measures the work a kernel does rather than how the
                                                                                    !> expression happens to be written.
                                                                                    function expr_node_count(self) result(n)
                                                                                        class(expr_t), intent(in) :: self
                                                                                        integer                   :: n
                                                                                        n = self%a%node_count(self%id)
                                                                                    end function expr_node_count

                                                                                    ! ----------------------------------------------------------- operators --

                                                                                    !> Equality is index identity, which hash-consing makes exact for structure:
                                                                                    !> equal indices mean the same expression tree. It is *not* mathematical
                                                                                    !> equality -- `x + x` and `2*x` are different structures. Deciding those
                                                                                    !> equal is what the zero test is for.
                                                                                    pure function eq_ee(x, y) result(yes)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        logical                  :: yes
                                                                                        yes = same_arena(x, y) .and. x%id == y%id
                                                                                    end function eq_ee

                                                                                    pure function ne_ee(x, y) result(yes)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        logical                  :: yes
                                                                                        yes = .not. eq_ee(x, y)
                                                                                    end function ne_ee

                                                                                    function add_ee(x, y) result(e)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        type(expr_t)             :: e
                                                                                        integer :: pair(2)
                                                                                        pair = [x%id, y%id]
                                                                                        e%a => x%a
                                                                                        e%id = x%a%add(pair)
                                                                                    end function add_ee

                                                                                    function plus_e(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = x
                                                                                    end function plus_e

                                                                                    !> Subtraction is addition of a negation; there is no separate node kind, so
                                                                                    !> `a - b` and `a + (-1)*b` intern alike and no printer has to handle two
                                                                                    !> shapes for one operation.
                                                                                    function sub_ee(x, y) result(e)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        type(expr_t)             :: e
                                                                                        e = add_ee(x, neg_e(y))
                                                                                    end function sub_ee

                                                                                    function neg_e(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        integer :: pair(2)
                                                                                        pair = [x%a%int(-1_int64), x%id]
                                                                                        e%a => x%a
                                                                                        e%id = x%a%mul(pair)
                                                                                    end function neg_e

                                                                                    function mul_ee(x, y) result(e)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        type(expr_t)             :: e
                                                                                        integer :: pair(2)
                                                                                        pair = [x%id, y%id]
                                                                                        e%a => x%a
                                                                                        e%id = x%a%mul(pair)
                                                                                    end function mul_ee

                                                                                    !> Division is multiplication by a reciprocal power, for the same reason
                                                                                    !> subtraction is addition of a negation.
                                                                                    function div_ee(x, y) result(e)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        type(expr_t)             :: e
                                                                                        integer :: inv, pair(2)
                                                                                        inv = x%a%pow(y%id, x%a%int(-1_int64))
                                                                                        pair = [x%id, inv]
                                                                                        e%a => x%a
                                                                                        e%id = x%a%mul(pair)
                                                                                    end function div_ee

                                                                                    function pow_ee(x, y) result(e)
                                                                                        type(expr_t), intent(in) :: x, y
                                                                                        type(expr_t)             :: e
                                                                                        e%a => x%a
                                                                                        e%id = x%a%pow(x%id, y%id)
                                                                                    end function pow_ee

                                                                                    ! Mixed-type forms, so a literal can appear on either side of an operator
                                                                                    ! without the caller wrapping it. Each lifts the literal into the arena the
                                                                                    ! expression already belongs to.

                                                                                    function lift_int(a, v) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        integer,               intent(in)    :: v
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%int(int(v, int64))
                                                                                    end function lift_int

                                                                                    function lift_real(a, v) result(e)
                                                                                        type(arena_t), target, intent(inout) :: a
                                                                                        real(dp),              intent(in)    :: v
                                                                                        type(expr_t)                         :: e
                                                                                        e%a => a
                                                                                        e%id = a%real(v)
                                                                                    end function lift_real

                                                                                    function add_ei(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = add_ee(x, lift_int(x%a, v))
                                                                                    end function add_ei

                                                                                    function add_ie(v, x) result(e)
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = add_ee(lift_int(x%a, v), x)
                                                                                    end function add_ie

                                                                                    function add_er(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = add_ee(x, lift_real(x%a, v))
                                                                                    end function add_er

                                                                                    function add_re(v, x) result(e)
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = add_ee(lift_real(x%a, v), x)
                                                                                    end function add_re

                                                                                    function sub_ei(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = sub_ee(x, lift_int(x%a, v))
                                                                                    end function sub_ei

                                                                                    function sub_ie(v, x) result(e)
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = sub_ee(lift_int(x%a, v), x)
                                                                                    end function sub_ie

                                                                                    function sub_er(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = sub_ee(x, lift_real(x%a, v))
                                                                                    end function sub_er

                                                                                    function sub_re(v, x) result(e)
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = sub_ee(lift_real(x%a, v), x)
                                                                                    end function sub_re

                                                                                    function mul_ei(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = mul_ee(x, lift_int(x%a, v))
                                                                                    end function mul_ei

                                                                                    function mul_ie(v, x) result(e)
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = mul_ee(lift_int(x%a, v), x)
                                                                                    end function mul_ie

                                                                                    function mul_er(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = mul_ee(x, lift_real(x%a, v))
                                                                                    end function mul_er

                                                                                    function mul_re(v, x) result(e)
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = mul_ee(lift_real(x%a, v), x)
                                                                                    end function mul_re

                                                                                    function div_ei(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = div_ee(x, lift_int(x%a, v))
                                                                                    end function div_ei

                                                                                    function div_ie(v, x) result(e)
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = div_ee(lift_int(x%a, v), x)
                                                                                    end function div_ie

                                                                                    function div_er(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = div_ee(x, lift_real(x%a, v))
                                                                                    end function div_er

                                                                                    function div_re(v, x) result(e)
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = div_ee(lift_real(x%a, v), x)
                                                                                    end function div_re

                                                                                    function pow_ei(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        integer,      intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = pow_ee(x, lift_int(x%a, v))
                                                                                    end function pow_ei

                                                                                    function pow_er(x, v) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        real(dp),     intent(in) :: v
                                                                                        type(expr_t)             :: e
                                                                                        e = pow_ee(x, lift_real(x%a, v))
                                                                                    end function pow_er

                                                                                    ! ------------------------------------------------ elementary functions --

                                                                                    function apply1(name, x) result(e)
                                                                                        character(*), intent(in) :: name
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        integer :: one(1)
                                                                                        one = [x%id]
                                                                                        e%a => x%a
                                                                                        e%id = x%a%func(name, one)
                                                                                    end function apply1

                                                                                    function fn_sin(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("sin", x)
                                                                                    end function fn_sin

                                                                                    function fn_cos(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("cos", x)
                                                                                    end function fn_cos

                                                                                    function fn_tan(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("tan", x)
                                                                                    end function fn_tan

                                                                                    function fn_asin(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("asin", x)
                                                                                    end function fn_asin

                                                                                    function fn_acos(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("acos", x)
                                                                                    end function fn_acos

                                                                                    function fn_atan(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("atan", x)
                                                                                    end function fn_atan

                                                                                    !> Two-argument arctangent. The argument order is atan2(y, x) as in Fortran
                                                                                    !> and C; getting it backwards is a classic generated-code bug, so the order
                                                                                    !> is fixed here once and every dialect printer inherits it.
                                                                                    function fn_atan2(y, x) result(e)
                                                                                        type(expr_t), intent(in) :: y, x
                                                                                        type(expr_t)             :: e
                                                                                        integer :: pair(2)
                                                                                        pair = [y%id, x%id]
                                                                                        e%a => y%a
                                                                                        e%id = y%a%func("atan2", pair)
                                                                                    end function fn_atan2

                                                                                    function fn_sinh(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("sinh", x)
                                                                                    end function fn_sinh

                                                                                    function fn_cosh(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("cosh", x)
                                                                                    end function fn_cosh

                                                                                    function fn_tanh(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("tanh", x)
                                                                                    end function fn_tanh

                                                                                    function fn_asinh(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("asinh", x)
                                                                                    end function fn_asinh

                                                                                    function fn_acosh(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("acosh", x)
                                                                                    end function fn_acosh

                                                                                    function fn_atanh(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("atanh", x)
                                                                                    end function fn_atanh

                                                                                    function fn_exp(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("exp", x)
                                                                                    end function fn_exp

                                                                                    function fn_log(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("log", x)
                                                                                    end function fn_log

                                                                                    function fn_sqrt(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("sqrt", x)
                                                                                    end function fn_sqrt

                                                                                    function fn_abs(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("abs", x)
                                                                                    end function fn_abs

                                                                                    function fn_erf(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("erf", x)
                                                                                    end function fn_erf

                                                                                    function fn_erfc(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("erfc", x)
                                                                                    end function fn_erfc

                                                                                    function fn_gamma(x) result(e)
                                                                                        type(expr_t), intent(in) :: x
                                                                                        type(expr_t)             :: e
                                                                                        e = apply1("gamma", x)
                                                                                    end function fn_gamma

                                                                                end module fortsym_expr
