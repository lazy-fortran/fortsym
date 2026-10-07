program test_fortran_real_kind
    use, intrinsic :: iso_fortran_env, only: int64, real32, real64, real128
    use fortsym_arena, only: arena_t
    use fortsym_expr, only: expr_t, num, rat, operator(-)
    use fortsym_parse, only: parse_expr_in
    use fortsym_dialect, only: dialect, DIA_FORTRAN, DIA_NATIVE
    use fortsym_engine, only: engine_result_t, VERDICT_TRUE
    use fortsym_engine_native, only: native_engine_t, make_native_engine
    use fortsym_check, only: suite_t, suite_begin, check_identity, suite_end
    implicit none
    type(arena_t), target :: arena
    type(expr_t) :: e
    type(native_engine_t) :: engine
    type(engine_result_t) :: decision
    type(suite_t) :: suite
    integer :: i
    integer(int64), parameter :: values(5) = [-2_int64,-1_int64,0_int64,1_int64,2_int64]
    logical :: good
    character(:), allocatable :: message
    character(32) :: value_text
    ! Compiled independent oracles in all supported emitter real precisions.
    do i=1,size(values)
        if (int(real(values(i),kind=kind(0.0_real32)),int64)/=values(i)) &
            error stop 'real32 exact conversion oracle'
        if (int(real(values(i),kind=kind(0.0_real64)),int64)/=values(i)) &
            error stop 'real64 exact conversion oracle'
        if (int(real(values(i),kind=kind(0.0_real128)),int64)/=values(i)) &
            error stop 'real128 exact conversion oracle'
    end do
    if (3/real(2,kind=kind(0.0_real64))/=1.5_real64) error stop 'real division oracle'
    call arena%init()
    engine=make_native_engine(arena)
    call suite_begin(suite,'bounded Fortran REAL and KIND readback')
    do i=1,size(values)
        write(value_text,'(i0)') values(i)
        call exact_case('real('//trim(value_text)//',kind=kind(0.0_dp))', &
            num(arena,values(i)))
    end do
    call exact_case('real(2)',num(arena,2_int64))
    call exact_case('real(2,dp)',num(arena,2_int64))
    call exact_case('real(2,kind)',num(arena,2_int64))
    call exact_case('REAL(2,KIND=KIND(0.0_dp))',num(arena,2_int64))
    call exact_case('3/real(2,kind=kind(0.0_dp))',rat(arena,3_int64,2_int64))
    ! REAL changes this integer in real32; it must not be erased generally.
    if (int(real(16777217_int64,real32),int64)==16777217_int64) &
        error stop 'rounded conversion negative oracle'
    e=parse_expr_in(arena,'real(16777217,kind=kind(0.0_dp))', &
        dialect(DIA_FORTRAN),good,message)
    if (.not. good) error stop 'General REAL syntax must be retained'
    decision=engine%zero_test(e-num(arena,16777217_int64))
    if (decision%verdict==VERDICT_TRUE) error stop 'General REAL conversion erased'
    e=parse_expr_in(arena,'real(x,kind=kind(0.0_dp))',dialect(DIA_FORTRAN),good,message)
    if (.not. good) error stop 'Symbolic REAL syntax must be retained'
    e=parse_expr_in(arena,'real(2,wrong=dp)',dialect(DIA_FORTRAN),good,message)
    if (good) error stop 'Unknown REAL keyword accepted'
    e=parse_expr_in(arena,'real(2,kind=dp,kind=dp)',dialect(DIA_FORTRAN),good,message)
    if (good) error stop 'Duplicate REAL kind accepted'
    e=parse_expr_in(arena,'real(2,kind=)',dialect(DIA_FORTRAN),good,message)
    if (good) error stop 'Missing REAL kind value accepted'
    e=parse_expr_in(arena,'real(2,kind=dp)',dialect(DIA_NATIVE),good,message)
    if (good) error stop 'Fortran keyword syntax leaked into native dialect'
    call suite_end(suite,'real-kind-readback.json')
contains
    subroutine exact_case(text,want)
        character(*), intent(in) :: text
        type(expr_t), intent(in) :: want
        type(expr_t) :: got
        logical :: valid
        character(:), allocatable :: why
        got=parse_expr_in(arena,text,dialect(DIA_FORTRAN),valid,why)
        if (.not. valid) then
            print *, text,why
            error stop 'Supported REAL conversion rejected'
        end if
        call check_identity(suite,engine,text,got-want)
    end subroutine
end program
