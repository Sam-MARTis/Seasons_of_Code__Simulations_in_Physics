
function f(a) result(fx)
    implicit none
    real, intent(in)::a
    real:: fx
    if(a==2) then
        fx = 0
        
    elseif(a<2) then
        fx = (5*(a**2)) + (3*a) + 2
    elseif(a>2) then
        fx = (5*(a**2)) - (3*a) + 1
    else
        print *, "Unknown error"
        fx = 0
    end if

end function f

program main
    implicit none
    real:: x
    real::f
    print *, "Enter input x: "
    read *, x
    print *, f(x)
    
    ! print *, f(2.0)

end program main