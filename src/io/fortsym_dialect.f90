module fortsym_dialect
    ! Per-engine surface syntax, in one place.
    !
    ! fortsym holds expressions in its own arena and talks to every engine in
    ! that engine's own notation. The differences are small but unforgiving: a
    ! power is ** here and ^ there, pi is `pi`, `Pi` or `%pi`, and the natural
    ! logarithm is `log` everywhere except Yacas, where `log` does not exist and
    ! the function is `Ln`. Getting one of these wrong produces a plausible
    ! string that the other engine misreads, which is the worst failure mode
    ! available -- so they are tabulated once, here.
    !
    ! One table drives both directions. The printer asks for the spelling of a
    ! canonical name and the parser asks for the canonical name of a spelling,
    ! so a dialect cannot print something it would not read back. The round-trip
    ! test in test/io exists to keep that honest.
    !
    ! fortsym's canonical function names are the lowercase Fortran intrinsic
    ! names: sin, cos, asin, atan2, exp, log, sqrt, abs, erf, gamma.
    use fortsym_string, only: str_t, str
    implicit none
    private

    public :: dialect_t, dialect
    public :: DIA_NATIVE, DIA_SYMENGINE, DIA_YACAS, DIA_SYMPY, DIA_MAXIMA, &
        DIA_FORTRAN
    public :: fn_spelling, fn_canonical
    public :: const_spelling, const_canonical

    integer, parameter :: DIA_NATIVE = 1 !< fortsym's own notation
    integer, parameter :: DIA_SYMENGINE = 2
    integer, parameter :: DIA_YACAS = 3
    integer, parameter :: DIA_SYMPY = 4
    integer, parameter :: DIA_MAXIMA = 5
    integer, parameter :: DIA_FORTRAN = 6 !< generated Fortran source

    type :: dialect_t
        integer     :: id = DIA_NATIVE
        type(str_t) :: name
        !> Power operator: "**" or "^".
        type(str_t) :: power
        !> Suffix on real literals. Only Fortran needs one, and it is the
        !> difference between a double and a silently-single-precision constant.
        type(str_t) :: real_suffix
        !> Suffix on integer literals used as real values.
        type(str_t) :: int_real_suffix
        !> True when an exact rational may be printed as a/b. Fortran cannot:
        !> 1/3 there is integer division and evaluates to zero, so the printer
        !> must emit typed reals instead.
        logical     :: rational_as_ratio = .true.
        !> True when the dialect has no symbolic constants and pi must be
        !> emitted as a literal.
        logical     :: numeric_constants = .false.
    end type dialect_t

contains

    !> Build a dialect descriptor. Everything that varies between engines is set
    !> here rather than tested for at each use site.
    function dialect(id) result(d)
        integer, intent(in) :: id
        type(dialect_t)     :: d

        d%id = id
        d%power = str("**")
        d%real_suffix = str("")
        d%int_real_suffix = str("")
        d%rational_as_ratio = .true.
        d%numeric_constants = .false.

        select case (id)
        case (DIA_NATIVE)
            d%name = str("fortsym")
        case (DIA_SYMENGINE)
            d%name = str("symengine")
        case (DIA_YACAS)
            d%name = str("yacas")
            d%power = str("^")
        case (DIA_SYMPY)
            d%name = str("sympy")
        case (DIA_MAXIMA)
            d%name = str("maxima")
            d%power = str("^")
        case (DIA_FORTRAN)
            d%name = str("fortran")
            d%real_suffix = str("_dp")
            d%int_real_suffix = str(".0_dp")
            ! Integer division truncates, so an exact rational has to become a
            ! quotient of reals.
            d%rational_as_ratio = .false.
            ! Fortran has no pi; it has to be spelled out to full precision.
            d%numeric_constants = .true.
        case default
            d%name = str("fortsym")
        end select
    end function dialect

    !> Spelling of a canonical function name in this dialect.
    !>
    !> Only the genuine differences are listed; anything absent keeps its
    !> canonical name, which is correct for SymEngine, Maxima and Fortran, where
    !> the intrinsic names already agree.
    function fn_spelling(d, canonical) result(s)
        type(dialect_t), intent(in) :: d
        character(*),    intent(in) :: canonical
        type(str_t)                 :: s

        select case (d%id)
        case (DIA_YACAS)
            ! Yacas capitalises every function, spells the inverse trigonometric
            ! functions Arc*, and has no `log` at all -- the natural logarithm
            ! is `Ln`, while `Log` would be misread.
            select case (canonical)
            case ("sin");   s = str("Sin")
            case ("cos");   s = str("Cos")
            case ("tan");   s = str("Tan")
            case ("asin");  s = str("ArcSin")
            case ("acos");  s = str("ArcCos")
            case ("atan");  s = str("ArcTan")
            case ("atan2"); s = str("ArcTan2")
            case ("sinh");  s = str("Sinh")
            case ("cosh");  s = str("Cosh")
            case ("tanh");  s = str("Tanh")
            case ("asinh"); s = str("ArcSinh")
            case ("acosh"); s = str("ArcCosh")
            case ("atanh"); s = str("ArcTanh")
            case ("exp");   s = str("Exp")
            case ("log");   s = str("Ln")
            case ("sqrt");  s = str("Sqrt")
            case ("abs");   s = str("Abs")
            case ("erf");   s = str("Erf")
            case ("erfc");  s = str("Erfc")
            case ("gamma"); s = str("Gamma")
            case default;   s = str(canonical)
            end select
        case (DIA_SYMPY)
            ! SymPy is lowercase apart from Abs, which is a class name.
            select case (canonical)
            case ("abs"); s = str("Abs")
            case default; s = str(canonical)
            end select
        case default
            s = str(canonical)
        end select
    end function fn_spelling

    !> Canonical name for a spelling read in this dialect: the inverse of
    !> fn_spelling. Unknown names pass through, so a function fortsym has no
    !> wrapper for still round-trips as an opaque application.
    function fn_canonical(d, spelling) result(s)
        type(dialect_t), intent(in) :: d
        character(*),    intent(in) :: spelling
        type(str_t)                 :: s

        select case (d%id)
        case (DIA_YACAS)
            select case (spelling)
            case ("Sin");      s = str("sin")
            case ("Cos");      s = str("cos")
            case ("Tan");      s = str("tan")
            case ("ArcSin");   s = str("asin")
            case ("ArcCos");   s = str("acos")
            case ("ArcTan");   s = str("atan")
            case ("ArcTan2");  s = str("atan2")
            case ("Sinh");     s = str("sinh")
            case ("Cosh");     s = str("cosh")
            case ("Tanh");     s = str("tanh")
            case ("ArcSinh");  s = str("asinh")
            case ("ArcCosh");  s = str("acosh")
            case ("ArcTanh");  s = str("atanh")
            case ("Exp");      s = str("exp")
            case ("Ln");       s = str("log")
            case ("Sqrt");     s = str("sqrt")
            case ("Abs");      s = str("abs")
            case ("Erf");      s = str("erf")
            case ("Erfc");     s = str("erfc")
            case ("Gamma");    s = str("gamma")
            case default;      s = str(spelling)
            end select
        case (DIA_SYMPY)
            select case (spelling)
            case ("Abs"); s = str("abs")
            case default; s = str(spelling)
            end select
        case default
            s = str(spelling)
        end select
    end function fn_canonical

    !> Spelling of a named constant. fortsym's canonical names are pi, e and i.
    !>
    !> Maxima prefixes its built-in constants with %, and a bare `e` there is an
    !> ordinary symbol, so dropping the prefix would silently turn Euler's
    !> number into a free variable.
    function const_spelling(d, canonical) result(s)
        type(dialect_t), intent(in) :: d
        character(*),    intent(in) :: canonical
        type(str_t)                 :: s

        select case (d%id)
        case (DIA_MAXIMA)
            select case (canonical)
            case ("pi"); s = str("%pi")
            case ("e");  s = str("%e")
            case ("i");  s = str("%i")
            case default; s = str(canonical)
            end select
        case (DIA_YACAS)
            select case (canonical)
            case ("pi"); s = str("Pi")
            case ("e");  s = str("Exp(1)")
            case ("i");  s = str("I")
            case default; s = str(canonical)
            end select
        case (DIA_SYMENGINE, DIA_SYMPY)
            select case (canonical)
            case ("pi"); s = str("pi")
            case ("e");  s = str("E")
            case ("i");  s = str("I")
            case default; s = str(canonical)
            end select
        case default
            s = str(canonical)
        end select
    end function const_spelling

    function const_canonical(d, spelling) result(s)
        type(dialect_t), intent(in) :: d
        character(*),    intent(in) :: spelling
        type(str_t)                 :: s

        select case (d%id)
        case (DIA_MAXIMA)
            select case (spelling)
            case ("%pi"); s = str("pi")
            case ("%e");  s = str("e")
            case ("%i");  s = str("i")
            case default; s = str(spelling)
            end select
        case (DIA_YACAS)
            select case (spelling)
            case ("Pi"); s = str("pi")
            case ("I");  s = str("i")
            case default; s = str(spelling)
            end select
        case (DIA_SYMENGINE, DIA_SYMPY)
            select case (spelling)
            case ("pi"); s = str("pi")
            case ("E");  s = str("e")
            case ("I");  s = str("i")
            case default; s = str(spelling)
            end select
        case default
            s = str(spelling)
        end select
    end function const_canonical

end module fortsym_dialect
