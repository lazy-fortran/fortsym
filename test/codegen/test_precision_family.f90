program test_precision_family
    use iso_fortran_env, only: int16, real32, real64
    use ieee_arithmetic, only: ieee_is_finite
    use fortgen_storage, only: pack_fp16,unpack_fp16,pack_bf16,unpack_bf16
    implicit none
    interface
        pure subroutine symbolic_polynomial_fp64(x,y,dy)
            import real64
            real(real64), intent(in) :: x
            real(real64), intent(out) :: y,dy
        end subroutine
        pure subroutine symbolic_polynomial_fp32(x,y,dy)
            import real32
            real(real32), intent(in) :: x
            real(real32), intent(out) :: y,dy
        end subroutine
        pure subroutine symbolic_polynomial_fp16(x,y,dy)
            import int16
            integer(int16), intent(in) :: x
            integer(int16), intent(out) :: y,dy
        end subroutine
        pure subroutine symbolic_polynomial_bf16(x,y,dy)
            import int16
            integer(int16), intent(in) :: x
            integer(int16), intent(out) :: y,dy
        end subroutine
    end interface
    real(real64) :: x,y,dy,reference,derivative,u
    real(real32) :: sy,sd
    integer(int16) :: hy,hd
    integer :: i
    do i=0,256
        x = real(i,real64)/129
        reference = x*x+2*x+1
        derivative = 2*x+2
        call symbolic_polynomial_fp64(x,y,dy)
        if(abs(y-reference)>8*epsilon(x)*reference) error stop 'fp64 value'
        if(abs(dy-derivative)>8*epsilon(x)*derivative) error stop 'fp64 derivative'
        call symbolic_polynomial_fp32(real(x,real32),sy,sd)
        if(abs(real(sy,real64)-reference)>8*epsilon(sy)*reference) &
            error stop 'fp32 value'
        if(abs(real(sd,real64)-derivative)>8*epsilon(sd)*derivative) &
            error stop 'fp32 derivative'
        u = 2.0_real64**(-11)
        call symbolic_polynomial_fp16(pack_fp16(x),hy,hd)
        if(abs(unpack_fp16(hy)-reference)>8*u*reference) error stop 'fp16 value'
        if(abs(unpack_fp16(hd)-derivative)>8*u*derivative) error stop 'fp16 derivative'
        u = 2.0_real64**(-8)
        call symbolic_polynomial_bf16(pack_bf16(x),hy,hd)
        if(abs(unpack_bf16(hy)-reference)>8*u*reference) error stop 'bf16 value'
        if(abs(unpack_bf16(hd)-derivative)>8*u*derivative) error stop 'bf16 derivative'
    end do
    call symbolic_polynomial_fp16(pack_fp16(1000.0_real64),hy,hd)
    if (ieee_is_finite(unpack_fp16(hy))) error stop 'half output overflow hidden'
    print *, 'single symbolic primal/derivative, four compiled formats PASS'
end program
