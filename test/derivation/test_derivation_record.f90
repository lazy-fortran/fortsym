program test_derivation_record
    use fortsym_derivation, only: derivation_t, content_hash
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, sym, num, operator(-)
    use fortsym_assume, only: FACT_POSITIVE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    implicit none
    type(arena_t), target :: arena, foreign
    type(derivation_t), target :: record, negative
    type(expr_t) :: x, one, two, f, derivative, at_two, value, alien
    type(native_engine_t) :: engine
    type(engine_result_t) :: result
    character(:), allocatable :: why, source
    character(1024) :: path, hash
    integer :: unit
    logical :: ok
    call arena%init()
    call foreign%init()
    hash=repeat('0',64)
    if (command_argument_count()>1) call get_command_argument(2,hash)
    call record%init(arena,trim(hash),100)
    x=record%apply('symbol',[expr_t::],'x',ok,why)
    call require(ok,why)
    call record%assume(x,FACT_POSITIVE,ok,why)
    call require(ok,why)
    one=record%apply('integer',[expr_t::],'1',ok,why)
    call require(ok,why)
    two=record%apply('integer',[expr_t::],'2',ok,why)
    call require(ok,why)
    f=record%apply('add',[x,one],'',ok,why)
    call require(ok,why)
    f=record%apply('power',[f,two],'',ok,why)
    call require(ok,why)
    derivative=record%apply('differentiate',[f,x],'',ok,why)
    call require(ok,why)
    at_two=record%apply('substitute',[derivative,x,two],'',ok,why)
    call require(ok,why)
    call record%name(at_two,'derivative_at_two',ok,why)
    call require(ok,why)
    if (record%steps(6)%operation/='differentiate') error stop 'lost operation order'
    if (record%steps(7)%operation/='substitute') error stop 'lost substitution'
    if (record%steps(1)%facts/=15) error stop 'lost positive assumptions'
    ! Independent derivative of x*x+2*x+1 at x=2 equals 6.
    engine=make_native_engine(arena)
    result=engine%zero_test(at_two-num(arena,6))
    if (result%verdict/=VERDICT_TRUE) error stop 'independent polynomial oracle'
    source=record%export_sympy(ok,why)
    call require(ok,why)
    if (content_hash('123456789')/='6C40DF5F0B497347') error stop 'CRC64 oracle'
    if (command_argument_count()>0) then
        call get_command_argument(1,path)
        open(newunit=unit,file=trim(path),status='replace', &
            access='stream',form='unformatted')
        write(unit) source
        close(unit)
    end if
    call negative%init(arena,trim(hash),1)
    value=negative%apply('integer',[expr_t::],'1',ok,why)
    call require(ok,why)
    value=negative%apply('integer',[expr_t::],'2',ok,why)
    if (ok .or. index(why,'budget')==0) error stop 'missing budget diagnostic'
    call negative%init(arena,trim(hash))
    alien=sym(foreign,'z')
    call negative%observe('simplify',[alien],x,'',ok,why)
    if (ok) error stop 'foreign input accepted'
    call negative%init(arena,trim(hash))
    value=negative%apply('unknown_option',[expr_t::],'',ok,why)
    if (ok) error stop 'unsupported operation accepted'
    call negative%init(arena,trim(hash))
    x=negative%apply('symbol',[expr_t::],'x',ok,why)
    call require(ok,why)
    value=negative%apply('simplify',[x],'',ok,why)
    call require(ok,why)
    call negative%assume(x,FACT_POSITIVE,ok,why)
    if (ok) error stop 'late assumption accepted'
    print *, 'native ordered recording/export PASS'
contains
    subroutine require(condition,message)
        logical, intent(in) :: condition
        character(*), intent(in) :: message
        if (.not. condition) then
            print *, message
            error stop 1
        end if
    end subroutine
end program
