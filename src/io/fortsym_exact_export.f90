module fortsym_exact_export
    use fortgen_expr_ir, only: expr_ir_t
    use fortsym_expr, only: expr_t, is_valid
    use fortsym_arena, only: NK_INT, NK_RAT, NK_BIG_INT, NK_BIG_RAT, &
        NK_SYM, NK_CONST, NK_ADD, NK_MUL, NK_POW, NK_FUNC
    use fortsym_assume, only: assumption_context_t
    use fortsym_string, only: chars
    implicit none
    private
    public :: exact_expr_ir, exact_python, exportable_text, exact_function_supported
contains
    logical function exact_function_supported(name,arity) result(ok)
        character(*), intent(in) :: name
        integer, intent(in) :: arity
        ok=.false.
        select case(name)
        case('sin','cos','tan','exp','log','sqrt','abs','sign','conjugate')
            ok=arity==1
        end select
    end function

    logical function exportable_text(text) result(ok)
        character(*), intent(in) :: text
        integer :: k
        ok=.false.
        if (len(text)>65536 .or. index(text,achar(0))>0) return
        do k=1,len(text)
            if (iachar(text(k:k))>127) return
        end do
        ok=.true.
    end function

    function exact_python(e, context, ok, why) result(text)
        type(expr_t), intent(in) :: e
        type(assumption_context_t), intent(in) :: context
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        character(:), allocatable :: text
        type(expr_ir_t) :: ir
        integer :: root
        call ir%init()
        root=exact_expr_ir(e,context,ir,ok,why)
        text=''
        if (ok) text=ir%python(root,ok,why)
    end function

    recursive integer function exact_expr_ir(e, context, ir, ok, why, level) result(id)
        type(expr_t), intent(in) :: e
        type(assumption_context_t), intent(in) :: context
        type(expr_ir_t), intent(inout) :: ir
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer, optional, intent(in) :: level
        integer :: depth
        integer, allocatable :: args(:)
        integer :: k, facts, slash, left, right, empty(0), pair(2)
        character(:), allocatable :: tag, text
        id=0
        ok=.false.
        why='exact export: invalid or foreign expression'
        depth=0
        if (present(level)) depth=level
        if (depth>=128) then
            why='exact export: depth budget exhausted'
            return
        end if
        if (.not. is_valid(e)) return
        if (.not. associated(e%a, context%home)) return
        text=''
        allocate(args(e%nargs()))
        do k=1,e%nargs()
            args(k)=exact_expr_ir(e%arg(k),context,ir,ok,why,depth+1)
            if (.not. ok) return
        end do
        facts=0
        select case(e%kind())
        case(NK_INT,NK_BIG_INT)
            tag='integer'
            text=chars(e%exact_text())
        case(NK_RAT,NK_BIG_RAT)
            text=chars(e%exact_text())
            slash=index(text,'/')
            if (slash == 0) then
                why='exact export: malformed rational'
                return
            end if
            left=ir%add('integer',text(:slash-1),empty,ok,why)
            if (.not. ok) return
            right=ir%add('integer',text(slash+1:),empty,ok,why)
            if (.not. ok) return
            pair(1)=left
            pair(2)=right
            id=ir%add('rational','',pair,ok,why)
            return
        case(NK_SYM)
            tag='symbol'
            text=chars(e%name())
            do k=1,context%n
                if (context%ids(k) == e%id) facts=context%facts(k)
            end do
        case(NK_CONST)
            tag='constant'
            text=chars(e%name())
            if (text == 'e') text='E'
            if (text == 'i') text='I'
        case(NK_ADD); tag='add'
        case(NK_MUL); tag='mul'
        case(NK_POW); tag='pow'
        case(NK_FUNC)
            if (.not. exact_function_supported(chars(e%name()),e%nargs())) then
                ok=.false.
                why='exact export: unsupported function head/arity '//chars(e%name())
                return
            end if
            tag='function'
            text=chars(e%name())
            if (text == 'abs') text='Abs'
        case default
            why='exact export: unsupported expression kind (no floating lowering)'
            return
        end select
        if (.not. exportable_text(text)) then
            ok=.false.
            why='exact export: names require NUL-free ASCII text'
            return
        end if
        id=ir%add(tag,text,args,ok,why,facts,'')
    end function
end module
