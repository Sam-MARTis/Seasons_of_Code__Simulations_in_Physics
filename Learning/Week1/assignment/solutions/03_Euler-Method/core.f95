module DE
    integer, parameter, public ::dp = kind(1.0d0)
    contains
    function getddy(t, y, yp) result(ydp)
        implicit none
        ! integer, parameter :: dp = kind(1.0d0)
        real(kind=dp), intent(in)::y, yp, t
        real(kind=dp)::ydp
        ydp = 10* sin(t) - 6*y - 5*yp
    end function getddy
end module DE

module EulerMethod
    use DE, only: getddy, dp
        ! integer, parameter, public ::dp = kind(1.0d0)
    contains
    subroutine updateValues(t, y, yp, ydp, dt)
        implicit none
        ! integer, parameter :: dp = kind(1.0d0)
        ! real(kind=dp):: getddy
        real(kind=dp), intent(in):: dt
        real(kind=dp), intent(inout) :: t, y, yp, ydp
        ydp = getddy(t, y, yp)
        yp = yp + ydp*dt
        y = y + ((yp - ydp*dt/2)*dt)
        t = t + dt
        
        ! real(kind=dp), intent(out) ::  

        
    end subroutine updateValues
end module EulerMethod

program main
    ! integer, parameter ::wp = kind(1.0d0)
    use EulerMethod, only: updateValues, dp

    real(kind=dp) :: y = 0
    real(kind=dp):: dy = 5
    real(kind=dp):: ddy
    real(kind=dp):: t = 0
    real(kind=dp):: dt = 0.01
    integer:: i= 0

    open(1, file='solutionValues.txt', status='old')

    do i=1, 10000
        call updateValues(t, y, dy, ddy, dt)
        print*, t, y
        write(1,*) t, y
    end do
    close(1)
    call execute_command_line('gnuplot -p '//'plot2D.plt')




end program main