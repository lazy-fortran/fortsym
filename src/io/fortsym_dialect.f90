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
    ! fortsym's canonical function names are lowercase mathematical names.
    ! The Fortran map below distinguishes standard intrinsics from the small
    ! runtime surface supplied by fortnum.
    ! DIA_SYMPY is an expression spelling for the subprocess adapter. Python
    ! callers use the C ABI package and do not receive a SymPy script session.
    ! DIA_WOLFRAM has a separate bounded script surface because the consumers
    ! provide an existing Wolfram derivation corpus.
    use fortsym_string, only: str_t, str
    implicit none
    private

    public :: dialect_t, dialect
    public :: DIA_NATIVE, DIA_SYMENGINE, DIA_YACAS, DIA_SYMPY, DIA_MAXIMA, &
        DIA_FORTRAN, DIA_WOLFRAM, DIA_LATEX
    public :: fn_spelling, fn_canonical
    public :: fortran_function_supported, fortran_function_arity_ok
    public :: fortran_function_uses_special
    public :: const_spelling, const_canonical

    integer, parameter :: DIA_NATIVE = 1 !< fortsym's own notation
    integer, parameter :: DIA_SYMENGINE = 2
    integer, parameter :: DIA_YACAS = 3
    integer, parameter :: DIA_SYMPY = 4
    integer, parameter :: DIA_MAXIMA = 5
    integer, parameter :: DIA_FORTRAN = 6 !< generated Fortran source
    !> Wolfram-language input. A documented subset, not the language: fortsym
    !> reads the derivation scripts its consumers already own. Verified against
    !> Mathics, never against a Wolfram product. See LEGAL.md section 5.1.
    integer, parameter :: DIA_WOLFRAM = 7
    integer, parameter :: DIA_LATEX = 8 !< write-only LaTeX output

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
        !> True when function application is written Head[args] rather than
        !> Head(args). Only Wolfram does; getting it wrong makes output that
        !> the same dialect's parser cannot read back.
        logical     :: bracket_application = .false.
        !> True when juxtaposition means multiplication: "a1 r" is a1*r.
        !> Wolfram writes products this way throughout, and a parser without it
        !> rejects most of a real derivation script.
        logical     :: implicit_multiplication = .false.
        !> True when reals print in their shortest round-tripping form. Right
        !> for comparing against a CAS, wrong for generated Fortran where a
        !> dropped digit changes the compiled constant.
        logical     :: compact_reals = .false.
        !> True for syntax that exists only in the Wolfram subset: prefix
        !> application with @, and \[Name] named characters. Gated because
        !> another dialect must still reject them as the errors they are there.
        logical     :: wolfram_syntax = .false.
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
        case (DIA_WOLFRAM)
            d%name = str("wolfram")
            d%power = str("^")
            d%bracket_application = .true.
            d%implicit_multiplication = .true.
            d%compact_reals = .true.
            d%wolfram_syntax = .true.
        case (DIA_FORTRAN)
            d%name = str("fortran")
            d%real_suffix = str("_dp")
            d%int_real_suffix = str(".0_dp")
            ! Integer division truncates, so an exact rational has to become a
            ! quotient of reals.
            d%rational_as_ratio = .false.
            ! Fortran has no pi; it has to be spelled out to full precision.
            d%numeric_constants = .true.
        case (DIA_LATEX)
            d%name = str("latex")
            d%power = str("^")
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
        case (DIA_WOLFRAM)
            s = wolfram_spelling(canonical)
        case (DIA_FORTRAN)
            select case (canonical)
            case ("loggamma"); s = str("log_gamma")
            case ("besselj");  s = str("bessel_jn")
            case ("bessely");  s = str("bessel_yn")
            case ("besseli");  s = str("bessel_in")
            case ("besselk");  s = str("bessel_kn")
            case ("Min");      s = str("min")
            case ("Max");      s = str("max")
            case default;       s = str(canonical)
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
        case (DIA_WOLFRAM)
            s = wolfram_canonical(spelling)
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
        case (DIA_FORTRAN)
            select case (spelling)
            case ("log_gamma"); s = str("loggamma")
            case ("bessel_jn"); s = str("besselj")
            case ("bessel_yn"); s = str("bessely")
            case ("bessel_in"); s = str("besseli")
            case ("bessel_kn"); s = str("besselk")
            case ("min");       s = str("Min")
            case ("max");       s = str("Max")
            case default;        s = str(spelling)
            end select
        case default
            s = str(spelling)
        end select
    end function fn_canonical

    !> Canonical heads accepted by the real scalar Fortran kernel emitter.
    !> Unknown heads must be rejected before source text is returned: emitting
    !> an unqualified identifier would defer a symbolic error to compilation.
    pure logical function fortran_function_supported(name) result(ok)
        character(*), intent(in) :: name

        select case (name)
        case ("sin", "cos", "tan", "asin", "acos", "atan", "atan2", &
                "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", &
                "exp", "log", "log10", "sqrt", "abs", "erf", "erfc", &
                "gamma", "loggamma", "max", "min", "Min", "Max", &
                "besselj", "bessely", "besseli", "besselk")
            ok = .true.
        case default
            ok = .false.
        end select
    end function fortran_function_supported

    !> Arity of a canonical function in the scalar Fortran kernel surface.
    pure logical function fortran_function_arity_ok(name, n) result(ok)
        character(*), intent(in) :: name
        integer, intent(in) :: n

        if (name == "max" .or. name == "min" .or. name == "Min" .or. &
            name == "Max") then
            ok = n >= 2
        else if (name == "atan2" .or. name == "besselj" .or. &
                name == "bessely" .or. name == "besseli" .or. &
                name == "besselk") then
            ok = n == 2
        else
            ok = n == 1
        end if
    end function fortran_function_arity_ok

    !> Whether a mapped function is provided by fortnum's special umbrella.
    pure logical function fortran_function_uses_special(name) result(ok)
        character(*), intent(in) :: name

        ok = name == "besseli" .or. name == "besselk"
    end function fortran_function_uses_special

    !> Spelling of a named constant. fortsym's canonical names are pi, e, i,
    !> oo, zoo, and nan.
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
        case (DIA_WOLFRAM)
            select case (canonical)
            case ("pi"); s = str("Pi")
            case ("e");  s = str("E")
            case ("i");  s = str("I")
            case ("oo"); s = str("Infinity")
            case ("zoo"); s = str("ComplexInfinity")
            case ("nan"); s = str("Indeterminate")
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
        case (DIA_WOLFRAM)
            select case (spelling)
            case ("Pi"); s = str("pi")
            case ("E");  s = str("e")
            case ("I");  s = str("i")
            case ("Infinity"); s = str("oo")
            case ("ComplexInfinity"); s = str("zoo")
            case ("Indeterminate"); s = str("nan")
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

    !> Wolfram spellings of fortsym's canonical function names.
    !>
    !> One table, used in both directions by wolfram_canonical below, so a
    !> spelling can never be readable but unwritable. Names fortsym has no
    !> wrapper for pass through unchanged and stay opaque applications.
    function wolfram_spelling(canonical) result(s)
        character(*), intent(in) :: canonical
        type(str_t)              :: s
        select case (canonical)
        case ("sin");       s = str("Sin")
        case ("cos");       s = str("Cos")
        case ("tan");       s = str("Tan")
        case ("cot");       s = str("Cot")
        case ("sec");       s = str("Sec")
        case ("csc");       s = str("Csc")
        case ("asin");      s = str("ArcSin")
        case ("acos");      s = str("ArcCos")
        case ("atan");      s = str("ArcTan")
        case ("atan2");     s = str("ArcTan")
        case ("sinh");      s = str("Sinh")
        case ("cosh");      s = str("Cosh")
        case ("tanh");      s = str("Tanh")
        case ("asinh");     s = str("ArcSinh")
        case ("acosh");     s = str("ArcCosh")
        case ("atanh");     s = str("ArcTanh")
        case ("exp");       s = str("Exp")
        case ("log");       s = str("Log")
        case ("sqrt");      s = str("Sqrt")
        case ("abs");       s = str("Abs")
        case ("sign");      s = str("Sign")
        case ("erf");       s = str("Erf")
        case ("erfc");      s = str("Erfc")
        case ("gamma");     s = str("Gamma")
        case ("loggamma");  s = str("LogGamma")
        case ("polygamma"); s = str("PolyGamma")
        case ("besselj");   s = str("BesselJ")
        case ("bessely");   s = str("BesselY")
        case ("besseli");   s = str("BesselI")
        case ("besselk");   s = str("BesselK")
        case ("re");        s = str("Re")
        case ("im");        s = str("Im")
        case ("conjugate"); s = str("Conjugate")
        case ("arg");       s = str("Arg")
        case default;       s = str(canonical)
        end select
    end function wolfram_spelling

    !> Inverse of wolfram_spelling.
    !>
    !> ArcTan is deliberately absent: Wolfram overloads it for both the
    !> one-argument and two-argument forms, so the arity decides which canonical
    !> name applies and only the parser knows the arity. Mapping it here would
    !> silently turn ArcTan[y, x] into a one-argument atan and swap the
    !> quadrant, which is the classic way generated code gets a sign wrong.
    function wolfram_canonical(spelling) result(s)
        character(*), intent(in) :: spelling
        type(str_t)              :: s
        select case (spelling)
        case ("Sin");       s = str("sin")
        case ("Cos");       s = str("cos")
        case ("Tan");       s = str("tan")
        case ("Cot");       s = str("cot")
        case ("Sec");       s = str("sec")
        case ("Csc");       s = str("csc")
        case ("ArcSin");    s = str("asin")
        case ("ArcCos");    s = str("acos")
        case ("Sinh");      s = str("sinh")
        case ("Cosh");      s = str("cosh")
        case ("Tanh");      s = str("tanh")
        case ("ArcSinh");   s = str("asinh")
        case ("ArcCosh");   s = str("acosh")
        case ("ArcTanh");   s = str("atanh")
        case ("Exp");       s = str("exp")
        case ("Log");       s = str("log")
        case ("Sqrt");      s = str("sqrt")
        case ("Abs");       s = str("abs")
        case ("Sign");      s = str("sign")
        case ("Erf");       s = str("erf")
        case ("Erfc");      s = str("erfc")
        case ("Gamma");     s = str("gamma")
        case ("LogGamma");  s = str("loggamma")
        case ("PolyGamma"); s = str("polygamma")
        case ("BesselJ");   s = str("besselj")
        case ("BesselY");   s = str("bessely")
        case ("BesselI");   s = str("besseli")
        case ("BesselK");   s = str("besselk")
        case ("Re");        s = str("re")
        case ("Im");        s = str("im")
        case ("Conjugate"); s = str("conjugate")
        case ("Arg");       s = str("arg")
        case default;       s = str(spelling)
        end select
    end function wolfram_canonical

end module fortsym_dialect
