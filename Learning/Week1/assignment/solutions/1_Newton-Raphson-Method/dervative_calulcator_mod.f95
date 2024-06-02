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

