program main
    integer, dimension(2, 5):: a, b
    integer, dimension(5, 2)::c
    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 4, 6, 5], shape(b))
    print *, a(2, :)
    ! a = reshape(a, shape(c))
    ! print *, a(2, :)
    ! b(:, :) = a(:, :)
    ! a(2, :) = 0
    ! print *, b(:, 1)
    ! print *, size(a, 1)
end program main