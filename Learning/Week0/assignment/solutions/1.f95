

module solution_functions
    use iso_fortran_env, only: real64
    implicit none
    public PI 
    real(real64) :: PI = 4.D0*ATAN(1.D0)

    
    contains
    function f1(a, b) result (c)
        real, intent(in)::a, b
        real(real64):: c 
        c = (a**b)/(b*(b-a))
    end function f1

    function f2(x, y, a) result (c)
        real, intent(in):: x,y, a
        real(real64):: c
        c = log10(x) + cosd(a) + abs(x**2 + y**2) + 2*sqrt(x*y)
      

    end function f2

    function f3(x, m, a) result(c)
        real, intent(in):: x, m, a
        real(real64):: c
        c = (1/(a*sqrt(2*PI))) * exp(sqrt(2*a*((x-m)**3)))
    end function f3




end module solution_functions

program main
    use solution_functions
    implicit none
    ! real:: f1, f2, f3
    print *, "(i) ", f1(3.0, 4.0)
    print *, "(ii) ", f2(1.0, 2.0, 15.0)
    print *, "(iii) ", f3(2.0, 1.0, 2.0)
end program main