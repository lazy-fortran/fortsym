module fortsym_derivation
    ! Bounded execution transcript. Observations are never reconstructed from a DAG.
    use, intrinsic :: iso_fortran_env, only: int64
    use fortgen_expr_ir, only: python_string
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, is_valid, sym, exact, const, func, func_in, &
        operator(+), operator(-), operator(*), operator(/), operator(**)
    use fortsym_string, only: chars
    use fortsym_assume, only: assumption_context_t, record_assumption
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE, VERDICT_UNKNOWN
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_poly, only: poly_cancel
    use fortsym_subs, only: subs
    use fortsym_exact_export, only: exact_python, exportable_text
    use fortsym_assumption_export, only: sympy_assumption_keywords
    implicit none
    private
    public :: derivation_t, derivation_step_t, content_hash
    type :: derivation_step_t
        character(:), allocatable :: operation, literal, label
        integer, allocatable :: inputs(:)
        type(expr_t) :: value
        integer :: facts=0
    end type
    type :: derivation_t
        type(arena_t), pointer :: home => null()
        type(assumption_context_t) :: context
        type(derivation_step_t), allocatable :: steps(:)
        integer :: n=0, max_steps=10000
        character(:), allocatable :: source_hash, failure
    contains
        procedure :: init => derivation_init
        procedure :: observe
        procedure :: apply
        procedure :: assume => derivation_assume
        procedure :: name => derivation_name
        procedure :: check => derivation_check
        procedure :: condition => derivation_condition
        procedure :: refuse
        procedure :: export_sympy
        procedure :: find
    end type
contains
    subroutine derivation_init(self, home, source_hash, budget)
        class(derivation_t), intent(inout) :: self
        type(arena_t), target, intent(inout) :: home
        character(*), intent(in) :: source_hash
        integer, optional, intent(in) :: budget
        self%home => home
        call self%context%init(home)
        if (allocated(self%steps)) deallocate(self%steps)
        self%max_steps=10000
        if (present(budget)) self%max_steps=max(0,budget)
        allocate(self%steps(self%max_steps))
        self%n=0
        self%source_hash=source_hash
        self%failure=''
        if (len(source_hash) /= 64 .or. &
            verify(source_hash,'0123456789abcdef') /= 0) &
            call self%refuse('source SHA256 must contain 64 lowercase hex digits')
    end subroutine

    subroutine refuse(self, reason)
        class(derivation_t), intent(inout) :: self
        character(*), intent(in) :: reason
        character(32) :: step
        if (.not. allocated(self%failure)) self%failure=''
        if (len(self%failure)>0) return
        write(step,'(i0)') self%n+1
        self%failure='derivation step '//trim(step)//': '//reason
    end subroutine

    integer function find(self, value, step) result(id)
        class(derivation_t), intent(in) :: self
        type(expr_t), intent(in) :: value
        integer, optional, intent(in) :: step
        integer :: k
        id=0
        if (.not. is_valid(value)) return
        if (.not. associated(value%a,self%home)) return
        if (present(step)) then
            k=step
            if (k<1 .or. k>self%n) return
            if (self%steps(k)%value%id/=value%id) return
            id=k
            return
        end if
        do k=self%n,1,-1
            if (.not. is_valid(self%steps(k)%value)) cycle
            if (self%steps(k)%value%id == value%id) then
                id=k
                return
            end if
        end do
    end function

    subroutine observe(self, operation, inputs, value, literal, ok, why, input_steps)
        class(derivation_t), intent(inout) :: self
        character(*), intent(in) :: operation, literal
        type(expr_t), intent(in) :: inputs(:), value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer, optional, intent(in) :: input_steps(:)
        integer, allocatable :: ids(:)
        integer :: k, arity
        character(:), allocatable :: expected
        ok=.false.
        if (.not. allocated(self%failure)) call self%refuse('session not initialized')
        if (len(self%failure)>0) then
            why=self%failure
            return
        end if
        if (.not. exportable_text(literal)) &
            call self%refuse('literal requires NUL-free ASCII text')
        arity=-1
        select case(operation)
        case('integer','exact','rational','symbol','constant'); arity=0
        case('add','subtract','multiply','divide','power','differentiate'); arity=2
        case('simplify','expand','cancel','factor'); arity=1
        case('substitute'); arity=3
        case('function'); arity=size(inputs)
        end select
        if (arity /= size(inputs)) call self%refuse('unsupported operation/arity: '//operation)
        if (operation=='symbol') then
            do k=1,self%n
                if (self%steps(k)%operation/='symbol') cycle
                if (self%steps(k)%literal==literal) &
                    call self%refuse('symbol redefinition or scope conflict')
            end do
        end if
        if (.not. is_valid(value)) call self%refuse('invalid observed value')
        if (.not. associated(value%a,self%home)) call self%refuse('foreign arena')
        allocate(ids(size(inputs)))
        do k=1,size(inputs)
            if (present(input_steps)) then
                if (size(input_steps)/=size(inputs)) then
                    call self%refuse('input reference count mismatch')
                    why=self%failure
                    return
                end if
                ids(k)=self%find(inputs(k),input_steps(k))
            else
                ids(k)=self%find(inputs(k))
            end if
            if (ids(k)==0) call self%refuse('unrecorded input or foreign arena')
        end do
        if (self%n>=self%max_steps) call self%refuse('recording budget exhausted')
        ! Refuse nonexact/unsupported observations immediately, before publication.
        if (len(self%failure)==0) then
            expected=exact_python(value,self%context,ok,why)
            if (.not. ok) call self%refuse(why)
        end if
        if (len(self%failure)>0) then
            why=self%failure
            ok=.false.
            return
        end if
        self%n=self%n+1
        self%steps(self%n)%operation=operation
        self%steps(self%n)%literal=literal
        self%steps(self%n)%label=''
        self%steps(self%n)%inputs=ids
        self%steps(self%n)%value=value
        if (operation=='symbol') then
            do k=1,self%context%n
                if (self%context%ids(k)==value%id) &
                    self%steps(self%n)%facts=self%context%facts(k)
            end do
        end if
        ok=.true.
        why=''
    end subroutine

    function apply(self, operation, inputs, literal, ok, why) result(value)
        class(derivation_t), target, intent(inout) :: self
        character(*), intent(in) :: operation, literal
        type(expr_t), intent(in) :: inputs(:)
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(expr_t) :: value
        type(native_engine_t) :: engine
        type(engine_result_t) :: result
        integer :: arity,k
        ok=.false.
        if (.not. associated(self%home)) then
            call self%refuse('session not initialized')
            why=self%failure
            return
        end if
        if (len(self%failure)>0) then
            why=self%failure
            return
        end if
        if (.not. exportable_text(literal)) &
            call self%refuse('literal requires NUL-free ASCII text')
        arity=-1
        select case(operation)
        case('integer','exact','symbol','constant'); arity=0
        case('add','subtract','multiply','divide','power','differentiate'); arity=2
        case('simplify','expand','factor','cancel'); arity=1
        case('substitute'); arity=3
        case('function'); arity=size(inputs)
        end select
        if (arity/=size(inputs)) call self%refuse('unsupported operation/arity: '//operation)
        do k=1,size(inputs)
            if (self%find(inputs(k))==0) call self%refuse('unrecorded or foreign input')
        end do
        if (len(self%failure)>0) then
            why=self%failure
            return
        end if
        engine=make_native_engine(self%home,self%context)
        select case(operation)
        case('integer','exact'); value=exact(self%home,literal)
        case('symbol'); value=sym(self%home,literal)
        case('constant'); value=const(self%home,literal)
        case('function')
            if (size(inputs)==0) then
                value=func_in(self%home,literal)
            else
                value=func(literal,inputs)
            end if
        case('add'); value=inputs(1)+inputs(2)
        case('subtract'); value=inputs(1)-inputs(2)
        case('multiply'); value=inputs(1)*inputs(2)
        case('divide'); value=inputs(1)/inputs(2)
        case('power'); value=inputs(1)**inputs(2)
        case('substitute'); value=subs(inputs(1),inputs(2),inputs(3))
        case('differentiate'); result=engine%diff(inputs(1),inputs(2))
        case('simplify'); result=engine%simplify(inputs(1))
        case('expand'); result=engine%expand(inputs(1))
        case('factor'); result=engine%factor(inputs(1))
        case('cancel')
            call poly_cancel(self%home,inputs(1),value,ok,why)
            if (.not. ok) then
                call self%refuse(why)
                why=self%failure
                return
            end if
        end select
        select case(operation)
        case('differentiate','simplify','expand','factor')
            if (.not. result%ok) then
                call self%refuse(chars(result%message))
                why=self%failure
                return
            end if
            value=result%value
        end select
        call self%observe(operation,inputs,value,literal,ok,why)
    end function

    subroutine derivation_assume(self,value,facts,ok,why,step)
        integer, optional, intent(in) :: step
        class(derivation_t), intent(inout) :: self
        type(expr_t), intent(in) :: value
        integer, intent(in) :: facts
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: id,k,j
        ok=.false.
        id=self%find(value,step)
        if (id==0) then
            call self%refuse('assumption needs a recorded symbol')
        else if (self%steps(id)%operation/='symbol') then
            call self%refuse('scoped/composite or late assumption is unsupported')
        else
            if (.not. self%context%has(value,facts)) then
                do k=1,self%n
                    do j=1,size(self%steps(k)%inputs)
                        if (self%steps(self%steps(k)%inputs(j))%value%id==value%id) &
                            call self%refuse('assumption changed after use')
                    end do
                end do
            end if
        end if
        if (len(self%failure)==0) then
            call record_assumption(self%context,value,facts,ok,why)
            if (.not. ok) call self%refuse(why)
        end if
        if (len(self%failure)>0) then
            why=self%failure
            ok=.false.
            return
        end if
        do k=1,self%context%n
            if (self%context%ids(k)/=value%id) cycle
            do j=1,self%n
                if (self%steps(j)%operation/='symbol') cycle
                if (self%steps(j)%value%id==value%id) self%steps(j)%facts=self%context%facts(k)
            end do
        end do
        why=''
    end subroutine

    subroutine derivation_name(self,value,label,ok,why,step)
        integer, optional, intent(in) :: step
        class(derivation_t), intent(inout) :: self
        type(expr_t), intent(in) :: value
        character(*), intent(in) :: label
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer :: id,k
        id=self%find(value,step)
        if (.not. exportable_text(label)) call self%refuse('label requires NUL-free ASCII text')
        if (id==0 .or. len(label)==0) call self%refuse('invalid named intermediate')
        do k=1,self%n
            if (self%steps(k)%label==label) call self%refuse('duplicate intermediate name')
        end do
        if (len(self%failure)==0) self%steps(id)%label=label
        ok=len(self%failure)==0
        why=self%failure
    end subroutine

    subroutine directive(self,operation,value,label,ok,why,step)
        class(derivation_t), intent(inout) :: self
        character(*), intent(in) :: operation,label
        type(expr_t), intent(in) :: value
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        integer, optional, intent(in) :: step
        integer :: id
        if (.not. exportable_text(label)) call self%refuse('label requires NUL-free ASCII text')
        id=self%find(value,step)
        if (id==0) call self%refuse('directive needs recorded input')
        if (self%n>=self%max_steps) call self%refuse('recording budget exhausted')
        if (len(self%failure)==0) then
            self%n=self%n+1
            self%steps(self%n)%operation=operation
            self%steps(self%n)%literal=label
            self%steps(self%n)%label=''
            allocate(self%steps(self%n)%inputs(1))
            self%steps(self%n)%inputs(1)=id
        end if
        ok=len(self%failure)==0
        why=self%failure
    end subroutine

    subroutine derivation_condition(self,value,label,ok,why,step)
        integer, optional, intent(in) :: step
        class(derivation_t), intent(inout) :: self
        type(expr_t), intent(in) :: value
        character(*), intent(in) :: label
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        call directive(self,'condition_nonzero',value,label,ok,why,step)
    end subroutine

    subroutine derivation_check(self,value,status,label,ok,why,step)
        integer, optional, intent(in) :: step
        class(derivation_t), target, intent(inout) :: self
        type(expr_t), intent(in) :: value
        character(*), intent(in) :: status,label
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        type(native_engine_t) :: engine
        type(engine_result_t) :: result
        if (self%find(value,step)==0) call self%refuse('check needs recorded input')
        if (status/='zero' .and. status/='unknown') call self%refuse('unsupported check status')
        if (len(self%failure)==0) then
            engine=make_native_engine(self%home,self%context)
            result=engine%zero_test(value)
            if (status=='zero' .and. result%verdict/=VERDICT_TRUE) &
                call self%refuse('zero check failed: '//label)
            if (status=='unknown' .and. result%verdict/=VERDICT_UNKNOWN) &
                call self%refuse('UNKNOWN check failed: '//label)
        end if
        call directive(self,'check_'//status,value,label,ok,why,step)
    end subroutine

    function content_hash(text) result(digest)
        ! CRC64-ECMA, a portable content fingerprint, not a cryptographic signature.
        character(*), intent(in) :: text
        character(16) :: digest
        integer(int64) :: crc
        integer :: i,k
        crc=0_int64
        do i=1,len(text)
            crc=ieor(crc,shiftl(int(iachar(text(i:i)),int64),56))
            do k=1,8
                ! Select the GF(2) reduction without a conditional CRC loop.
                crc=ieor(shiftl(crc,1),iand(-ibits(crc,63,1), &
                    int(z'42F0E1EBA9EA3693',int64)))
            end do
        end do
        write(digest,'(z16.16)') crc
    end function

    function ref(id) result(text)
        integer, intent(in) :: id
        character(:), allocatable :: text
        character(32) :: digits
        write(digits,'(i0)') id
        text='v'//trim(digits)
    end function

    function export_sympy(self,ok,why) result(source)
        class(derivation_t), intent(inout) :: self
        logical, intent(out) :: ok
        character(:), allocatable, intent(out) :: why
        character(:), allocatable :: source, body, call, operation, expected, args
        character(1), parameter :: nl=new_line('a')
        integer :: i,j,k,denominator
        logical :: retained
        source=''
        ok=.false.
        if (.not. allocated(self%failure)) call self%refuse('session not initialized')
        if (len(self%failure)>0) then
            why=self%failure
            return
        end if
        body='named = {}'//nl//'conditions = []'//nl//'checks = {}'//nl
        do i=1,self%n
            operation=self%steps(i)%operation
            if (operation=='divide') then
                denominator=self%steps(i)%inputs(2)
                retained=.false.
                do k=1,self%n
                    if (self%steps(k)%operation/='condition_nonzero') cycle
                    j=self%steps(k)%inputs(1)
                    if (self%steps(j)%value%id==self%steps(denominator)%value%id) &
                        retained=.true.
                end do
                if (.not. retained) body=body//'conditions.append(('// &
                    python_string('denominator for '//ref(i))//', '// &
                    ref(denominator)//'))'//nl
            end if
            args=''
            do j=1,size(self%steps(i)%inputs)
                if (j>1) args=args//', '
                args=args//ref(self%steps(i)%inputs(j))
            end do
            select case(operation)
            case('check_zero')
                body=body//'assert sp.simplify('//args//') == 0, '// &
                    python_string(self%steps(i)%literal)//nl
                body=body//'checks['//python_string(self%steps(i)%literal)//"] = 'zero'"//nl
                cycle
            case('check_unknown')
                body=body//'checks['//python_string(self%steps(i)%literal)//"] = 'UNKNOWN'"//nl
                cycle
            case('condition_nonzero')
                body=body//'conditions.append(('//python_string(self%steps(i)%literal)// &
                    ', '//args//'))'//nl
                cycle
            case('symbol')
                call='sp.Symbol('//python_string(self%steps(i)%literal)// &
                    sympy_assumption_keywords(self%steps(i)%facts)//')'
            case('integer','exact','rational','constant')
                call=exact_python(self%steps(i)%value,self%context,ok,why)
                if (.not. ok) return
            case('add'); call='sp.Add('//args//')'
            case('multiply'); call='sp.Mul('//args//')'
            case('power'); call='sp.Pow('//args//')'
            case('subtract')
                call=ref(self%steps(i)%inputs(1))//' - '//ref(self%steps(i)%inputs(2))
            case('divide')
                call=ref(self%steps(i)%inputs(1))//' / '//ref(self%steps(i)%inputs(2))
            case('differentiate'); call='sp.diff('//args//')'
            case('substitute')
                call=ref(self%steps(i)%inputs(1))//'.subs('// &
                    ref(self%steps(i)%inputs(2))//', '//ref(self%steps(i)%inputs(3))//')'
            case('simplify','expand','cancel','factor'); call='sp.'//operation//'('//args//')'
            case('function')
                select case(self%steps(i)%literal)
                case('sin','cos','tan','exp','log','sqrt','abs','sign','conjugate')
                    operation=self%steps(i)%literal
                    if (operation=='abs') operation='Abs'
                    call='sp.'//operation//'('//args//')'
                case default
                    call='sp.Function('//python_string(self%steps(i)%literal)//')('//args//')'
                end select
            case default
                call self%refuse('export unsupported operation: '//operation)
                why=self%failure
                return
            end select
            body=body//'# step '//ref(i)//': '//self%steps(i)%operation//nl
            body=body//ref(i)//' = '//call//nl
            expected=exact_python(self%steps(i)%value,self%context,ok,why)
            if (.not. ok) return
            body=body//'assert sp.simplify('//ref(i)//' - ('//expected// &
                ')) == 0, '//python_string('observed step '//ref(i))//nl
            if (len(body)>16*1024*1024) then
                call self%refuse('export byte budget exhausted')
                why=self%failure
                return
            end if
            if (len(self%steps(i)%label)>0) body=body//'named['// &
                python_string(self%steps(i)%label)//'] = '//ref(i)//nl
        end do
        source='# FortSym scalar derivation transcript v1'//nl// &
            '# source-sha256: '//self%source_hash//nl// &
            '# body-crc64-ecma: '//content_hash(body)//nl// &
            '# Recorded backend: native FortSym; observed values are replay evidence.'//nl// &
            '# Coverage: explicit scalar operations/checks; Python control flow is not recorded.'//nl// &
            'TRANSCRIPT_VERSION = 1'//nl// &
            'if TRANSCRIPT_VERSION != 1:'//nl// &
            "    raise RuntimeError('unsupported transcript version')"//nl// &
            'import os'//nl// &
            "backend = os.environ.get('FORTSYM_REPLAY_BACKEND', 'sympy')"//nl// &
            "if backend == 'sympy':"//nl//'    import sympy as sp'//nl// &
            "    if sp.__version__ != '1.14.0':"//nl// &
            "        raise RuntimeError('requires SymPy 1.14.0')"//nl// &
            "elif backend == 'fortsym':"//nl//'    import fortsym.sympy as sp'//nl// &
            'else:'//nl//"    raise RuntimeError('unsupported replay backend')"//nl//body
        why=''
        ok=.true.
    end function
end module
