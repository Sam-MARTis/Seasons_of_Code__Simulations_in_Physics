function s1(n) result(s)
    real:: s
    
    integer, intent(in)::n
    integer::i
    s=0
    do i=0,n, 2
        s = s+ (4/((2.0*i)+1))
    end do
    do i=1,n, 2
        s = s - (4/((2.0*i)+1))
    end do
end function s1

function fac(n) result(facn)
    implicit none
    integer, intent(in):: n
    integer::facn, n_copy
    n_copy = n
    facn = 1
    do while(n_copy>0)
        facn = facn * n_copy
        n_copy = n_copy -1
    end do
end function fac


function s2(x, n) result(s)
    real, intent(in)::x
    integer, intent(in)::n 
    real:: s
    integer::fac
    integer::i
    s = 0
    do i=0,n
        s = s+ (x**i/fac(i))
    end do

end function s2

function s3(x, n) result(s)
    real, intent(in)::x
    integer, intent(in)::n 
    real:: s
    integer::fac
    integer::i
    s = 0
    do i=0,2*n, 4
        s = s+ ((x**(1.0*i))/fac(i))
    end do
    do i=2,2*n, 4
        s = s - ((x**(1.0*i))/fac(i))
    end do

end function s3


program main
    integer:: n
    real:: s1,s2, s3, x

    print *, "Enter n value(Note, don't enter too big values(choose n<20). Will ressult in overflow during calculation): "
    read *, n 
    print *, "Series convergent value (for pi) or n terms is: ", s1(n)
    print *, "|Series -Pi| = ", ABS(s1(n) - 4*ATAN(1.0))
    print *, "Enter value of x: "
    read *, x
    print *, "Series of e^x till n terms: ", s2(x, n)
    print *, "|Series - e^x| = ", ABS(s2(x, n) - exp(x))
    print *, "Series of cos(x) (note, x is in radians) till n terms: ", s3(x, n)
    print *, "|Series - cos(x)| = ", ABS(s3(x, n) - cos(x))




end program main