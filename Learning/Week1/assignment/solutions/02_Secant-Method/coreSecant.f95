module derivative_calculator_mod
    implicit none
    integer, parameter, public:: wp = kind(1.0d0)
    
    contains
    function OVD(f, x) result(derivative)
        interface
            pure function target_func(t) result(y)
                import wp
                implicit none
                real(kind=wp), intent(in)::t
                real(kind=wp)::y
            end function target_func
        end interface

        procedure(target_func)::f 
        real(kind=wp), intent(in) :: x
        real(kind=wp):: derivative
        real(kind=wp) :: dx = 1e-10
        derivative = real(f(x+dx) - f(x))/dx
    end function OVD


end module derivative_calculator_mod



module Secant_Method
    use derivative_calculator_mod, only: wp, OVD
    ! use customFunctions, only: f1, find_E
    implicit none
    contains
    function findZero(f, x1, x2) result(x_2)
        interface
            pure function target_func(t) result(y)
                import wp
                implicit none
                real(kind=wp), intent(in)::t
                real(kind=wp)::y
            end function target_func
        end interface

        procedure(target_func)::f 
        real(kind=wp), intent(in) :: x1
        real(kind=wp), intent(in) :: x2
        real(kind=wp):: x_1 
        real(kind=wp):: x_2
        real(kind=wp):: x_temp
        integer:: i
        x_1 = x1 
        x_2 = x2


        do i=1, 100
            x_temp = x_2
            x_2 = x_2 - 0.1*(f(x_2)*(x_2 - x_1))/(f(x_2) - f(x_1))
            x_1 = x_temp
        end do
        ! real(kind=wp) :: dx = 1e-10
        ! x0 = x+1

        ! derivative = real(f(x+dx) - f(x))/dx
    end function findZero
    



end module Secant_Method

module customFunctions
    use derivative_calculator_mod, only: wp
    implicit none
    
    
    contains
    pure function f1(x) result(fx)
        real(kind=wp), intent(in)::x
        real(kind=wp)::fx
        fx = exp(2*x) + x - 5
    end function f1



end module customFunctions

program main
    use customFunctions, only: f1, wp
    use Secant_Method, only: findZero

    
    real(kind=wp):: sol
    ! real(kind=wp):: f1
    real(kind=wp):: initalGuess1
    real(kind=wp):: initalGuess2
    initalGuess1 = 0.5
    initalGuess2 = 0
    print *, f1(initalGuess1)
    print *, f1(initalGuess2)

    sol = findZero(f1, initalGuess1, initalGuess2)
    print *, sol
    
    

end program main