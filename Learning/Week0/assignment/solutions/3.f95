function singleFactorial(n) result(facn)
    implicit none
    integer, intent(in):: n
    integer::facn, n_copy
    n_copy = n
    facn = 1
    do while(n_copy>0)
        facn = facn * n_copy
        n_copy = n_copy -1
    end do
end function singleFactorial


function doubleFactorial(n) result(facn)
    implicit none
    integer, intent(in):: n
    integer::facn, n_copy
    n_copy = n
    facn = 1
    do while(n_copy>0)
        facn = facn * n_copy
        n_copy = n_copy -2
    end do
end function doubleFactorial



program main
    implicit none
    integer:: n, singleFactorial, doubleFactorial

    print *, "Enter n "
    read *, n
    print *, "Single Factorial of n is: ", singleFactorial(n)

    if(MOD(n, 2)==1) then
        print *, "Double Factorial of n is: ", doubleFactorial(n)
    end if

    
end program main
