module Newton_Raphson
    use derivative_calculator_mod, only: wp, OVD
    ! use customFunctions, only: f1, find_E
    implicit none
    contains
    function findZero(f, x) result(x0)
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
        real(kind=wp):: x0 
        integer:: i
        x0 = x

        do i=1, 1000
            x0 = x0 - f(x0)/OVD(f, x0)
        end do
        ! real(kind=wp) :: dx = 1e-10
        ! x0 = x+1

        ! derivative = real(f(x+dx) - f(x))/dx
    end function findZero
    



end module Newton_Raphson