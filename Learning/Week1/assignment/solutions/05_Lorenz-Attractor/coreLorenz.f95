module StateTools
    integer, parameter, public ::dp = kind(1.0d0)

    contains

    
    function addState(n, state1, mul1, state2, mul2) result(resultState)
        integer, intent(in) :: n 
        real(kind=dp), intent(in), dimension(n):: state1, state2
        real(kind=dp), intent(in)::mul1, mul2 
        real(kind=dp), dimension(n):: resultState
        integer::i = 0
        do i=1,n 
            resultState(i) = state1(i)*mul1 + state2(i)*mul2
        end do
    end function addState

    function mul(n, state, mull) result(resultState)
        integer, intent(in) :: n 
        real(kind=dp), intent(in), dimension(n)::state
        real(kind=dp), intent(in)::mull
        real(kind=dp), dimension(n):: resultState
        integer::i = 0
        do i=1,n 
            resultState(i) = state(i)*mull
        end do
    end function mul

    
end module StateTools



module DE
    use StateTools, only: dp
    contains
    function f(state) result(stateP)
        implicit none
        real(kind=dp), dimension(:), intent(in):: state
        real(kind=dp), dimension(size(state)):: stateP

        stateP(1) = 10* (state(2) - state(1))
        stateP(2) = state(1)*(28 - state(3)) - state(2)
        stateP(3) = state(1)*state(2) -((8.0d0/3 )*state(3))
        stateP(4) = 1

    end function f
end module DE

module RK4
    use DE, only: f
    use StateTools, only: addState, mul, dp
    contains
    subroutine updateValues(n, state, dt)
        implicit none
        integer, intent(in)::n
        real(kind=dp), intent(in):: dt
        real(kind=dp), dimension(4, n) :: K_arr
        real(kind=dp), dimension(n), intent(inout) :: state

        real(kind=dp), dimension(n):: dState


        K_arr(1, :) = f(state)
        K_arr(2, :) = f(addState(n, state, 1.0d0, K_arr(1, :), dt/2))
        K_arr(3, :) = f(addState(n, state, 1.0d0, K_arr(2, :), dt/2))
        K_arr(4, :) = f(addState(n, state, 1.0d0, K_arr(3, :), dt))
        dState =  addState(n, K_arr(1, :), 1.0d0, K_arr(2, :), 2.0d0)
        dState =  addState(n, dState, 1.0d0, K_arr(3, :), 2.0d0)
        dState =  addState(n, dState, 1.0d0, K_arr(4, :), 1.0d0)
        dState = mul(n, dState, 1.0d0/6)
        ! dState =  addState(n, K_arr(1, :), 1.0d0, K_arr(2, :), 2.0d0)
        ! , 1.0d0, K_arr(n, :), 2.0d0)
        state = addState(n, state, 1.0d0, dState, dt)

        
    end subroutine updateValues
end module RK4

program main
    use RK4, only: updateValues, dp

    real(kind=dp) :: x = 0
    real(kind=dp):: y = 5
    real(kind=dp):: z = 0
    real(kind=dp):: t = 1
    real(kind=dp):: dt = 0.001
    real(kind=dp), dimension(4) :: state 
    integer:: i= 0
    state(1) = x
    state(2) = y
    state(3) = z
    state(4) = t


    open(1, file='solutionValues.txt', status='old')

    do i=1, 100000
        call updateValues(4, state, dt)
        ! print*, state(3), state(1)
        write(1,*) state(1), state(2), state(3)
    end do
    close(1)
    call execute_command_line('gnuplot -p '//'plot3D.plt')




end program main