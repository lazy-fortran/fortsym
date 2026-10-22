program test_tensor_apply
    use, intrinsic :: iso_fortran_env, only: real64
    use tensor_apply_module, only: tensor_apply
    implicit none
    real(real64) :: u(8, 4), y(8, 4), b(2, 2, 2, 1), db(2, 2, 2, 1), g(8, 4, 4)
    real(real64) :: expected(4), gradients(2), flux(2), v(2, 4), m(2, 2)
    integer :: c, i, q
    b(:, :, 1, 1) = reshape([0.8_real64, 0.3_real64, 0.2_real64, 0.7_real64], [2, 2])
    b(:, :, 2, 1) = reshape([0.6_real64, 0.1_real64, 0.4_real64, 0.9_real64], [2, 2])
    db(:, :, 1, 1) = reshape([-1.0_real64, -1.0_real64, 1.0_real64, 1.0_real64], [2, 2])
    db(:, :, 2, 1) = 2*db(:, :, 1, 1)
    do c = 1, 8
        u(c, :) = real([1, 2, 4, 3]*c, real64)/8
    end do
    g(:, :, 1) = 2
    g(:, :, 2) = 0.25_real64
    g(:, :, 3) = -0.125_real64
    g(:, :, 4) = 3
    call tensor_apply(8, u, b, db, g, y)
    do c = 1, 8
        expected = 0
        do q = 0, 3
            do i = 0, 3
                v(1, i + 1) = db(mod(q, 2) + 1, mod(i, 2) + 1, 1, 1)* &
                              b(q/2 + 1, i/2 + 1, 2, 1)
                v(2, i + 1) = b(mod(q, 2) + 1, mod(i, 2) + 1, 1, 1)* &
                              db(q/2 + 1, i/2 + 1, 2, 1)
            end do
            gradients = matmul(v, u(c, :))
            m(1, :) = [g(c, q + 1, 1), g(c, q + 1, 2)]
            m(2, :) = [g(c, q + 1, 3), g(c, q + 1, 4)]
            flux = matmul(m, gradients)
            expected = expected + matmul(transpose(v), flux)
        end do
        if (maxval(abs(y(c, :) - expected)) > 1e-14_real64*maxval(abs(expected))) &
            error stop 'FortSym tensor adapter dense oracle'
    end do
end program
