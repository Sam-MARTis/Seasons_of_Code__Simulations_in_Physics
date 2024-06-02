module customFunctions
    use derivative_calculator_mod, only: wp
    implicit none
    
    
    contains
    pure function f1(x) result(fx)
        real(kind=wp), intent(in)::x
        real(kind=wp)::fx
        fx = cos(x) - 0.226*x
    end function f1

    pure function find_E(kl) result(E)
        real(kind=wp), intent(in):: kl
        real(kind=wp)::E
        real(kind=wp)::K 

        K = kl/(2e-10)
        E = (K**2)* ((6.58e-16) **2) /(2*(9.1e-31))
    end function find_E

end module customFunctions