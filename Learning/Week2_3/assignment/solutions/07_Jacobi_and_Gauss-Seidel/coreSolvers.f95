module StateTools
    integer, parameter, public ::dp = kind(1.0d0)

    contains

    
    function addState(state1, mul1, state2, mul2) result(resultState)
        real(kind=dp), intent(in), dimension(:):: state1
        real(kind=dp), intent(in), dimension(size(state1))::  state2
        real(kind=dp), intent(in)::mul1, mul2 
        real(kind=dp), dimension(size(state1)):: resultState
        integer::i = 0
        do i=1,size(state1)
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


    
    function norm(state, l) result(normVal)
        real(kind=dp), intent(in), dimension(:):: state
        integer, intent(in):: l 
        real(kind=dp):: normVal
        integer:: i
        normVal = 0.0d0
        if ( l==(-1) ) then
            do i=1, size(state)
                if(abs(state(i))>normVal) then
                    normVal = abs(state(i))
                end if
            end do
        else
            do i=1, size(state)
                normVal = normVal + ((abs(state(i)))**l)
            end do
            normVal = normVal**(1.0d0/l)
        end if
    end function norm
    



    subroutine copyState(targetState, originalState)
        real(kind=dp),dimension(:), intent(in):: originalState
        real(kind=dp),dimension(size(originalState)), intent(out)::targetState
        integer::i =  1
        do i=1, size(originalState)
            targetState(i) = originalState(i)
        end do

    end subroutine

    subroutine matrixBackward(operation_matrix)
        real(kind=dp), dimension(:,:), intent(inout)::operation_matrix
        integer::i,j
        real(kind=dp) :: mulFactor = 1.0d0
        do j=1,size(operation_matrix(1, :))
            mulFactor = operation_matrix(j,j)

            do i=1,size(operation_matrix(:,1))
                operation_matrix(i,j) = -operation_matrix(i,j)/mulFactor
            end do
            operation_matrix(j,j) = 0
        end do

    end subroutine

    subroutine scaleAnswerVectorForBackward(operation_matrix, rhs)
        real(kind=dp), dimension(:,:), intent(in)::operation_matrix
        real(kind=dp), dimension(:), intent(inout)::rhs
        integer::i
        do i=1,size(operation_matrix(1, :))
            rhs(i) = rhs(i)/operation_matrix(i,i)
        end do

    end subroutine

    subroutine prepareBackwardForSolver(operation_matrix, rhs)
        real(kind=dp), dimension(:,:), intent(inout)::operation_matrix
        real(kind=dp), dimension(:), intent(inout)::rhs
        call scaleAnswerVectorForBackward(operation_matrix, rhs)
        call matrixBackward(operation_matrix)
        
    end subroutine

    pure function dotProduct(v1, v2) result(dotVal)

        real(kind=dp), dimension(:), intent(in)::v1
        real(kind=dp), dimension(size(v1)), intent(in)::v2
        real(kind=dp)::dotVal
        integer:: i
        dotVal = 0
        do i=1,size(v1)
            dotVal = dotVal + (v1(i)*v2(i))
        end do
    end function

    function matrixMul(matrix, vector) result(transformedVector)
        real(kind=dp), dimension(:,:)::matrix
        real(kind=dp), dimension(size(matrix, 1))::vector
        real(kind=dp), dimension(size(matrix, 2)):: transformedVector
        integer::i,j
        do j=1,size(matrix,2)
            transformedVector(j) = 0
            do i=1,size(matrix,1)
                transformedVector(j) = transformedVector(j) + matrix(i,j)*vector(i)
            end do
        end do
    end function 

    subroutine applyMatrixMul(matrix, vector)
        real(kind=dp), dimension(:,:), intent(in)::matrix
        real(kind=dp), dimension(size(matrix, 1)), intent(inout)::vector
        vector = matrixMul(matrix, vector)
    end subroutine

    
end module StateTools



module Solvers
    use StateTools

    contains

    ! subroutine
    subroutine jacobiSolver(state, operator_matrix, rhs, max_iterations)
        integer, intent(in)::max_iterations
        real(kind=dp), dimension(:), intent(inout)::state
        real(kind=dp), dimension(:), intent(in)::rhs
        real(kind=dp), dimension(size(rhs)):: reverse_rhs

        real(kind=dp), dimension(size(rhs), size(rhs)), intent(in)::operator_matrix
        real(kind=dp), dimension(size(state), size(rhs)):: reverse_matrix
        ! real
        integer::iteration_count, i
        real(kind=dp), dimension(size(state)):: nextState
        ! real(kind=)
        call copyState(nextState, state)


        reverse_matrix = operator_matrix(:, :)


        reverse_rhs(:) = rhs(:)
        call prepareBackwardForSolver(reverse_matrix, reverse_rhs)

        do iteration_count = 1,max_iterations
            !Matrix needs to be diagonally dominant for convergence
            do concurrent (i = 1:size(state))
                nextState(i) = dotProduct(reverse_matrix(:, i), state) + reverse_rhs(i)             
            end do




            !Stop if error is within threshold
            if ( norm(addState(state, 1.0d0, nextState, -1.0d0), 2)<0.000001 ) then
                exit
            end if
            
            call copyState(state, nextState)
   
        end do
        call copyState(state, nextState)
        
    end subroutine

    subroutine gaussSeidelSolver(state, operator_matrix, rhs, max_iterations)
        integer, intent(in)::max_iterations
        real(kind=dp), dimension(:), intent(inout)::state
        real(kind=dp), dimension(:), intent(in)::rhs
        real(kind=dp), dimension(size(rhs)):: reverse_rhs

        real(kind=dp), dimension(size(rhs), size(rhs)), intent(in)::operator_matrix
        real(kind=dp), dimension(size(state), size(rhs)):: reverse_matrix
        ! real
        integer::blank, i
        real(kind=dp), dimension(size(state)):: tempState
        ! real(kind=)
        

        reverse_matrix = operator_matrix(:, :)


        reverse_rhs(:) = rhs(:)
        call prepareBackwardForSolver(reverse_matrix, reverse_rhs)
        ! print *, "Reverse matrix:"
        ! do i=1,size(reverse_matrix(1, :))
        !     print *, reverse_matrix(:, i)
        !     print *, operator_matrix(:, i)
        !     print *, ""
        ! end do
        ! print *, reverse_matrix
        ! print *, "Reverse end"
        ! print *, reverse_rhs
        ! print *, "Reverse rhs end"
        do blank = 1,max_iterations
            call copyState(tempState, state)
            !Matrix is diagonally dominant. Therefore convergence is guarenteed
            do i = 1, size(state)
                state(i) = dotProduct(reverse_matrix(:, i), state) + reverse_rhs(i)             
            end do
            ! if ( norm(addState(state, 1.0d0, tempState, -1.0d0), -1)<0.000001 ) then
                
            !     exit
            ! end if

            
        end do
        
    end subroutine
end module


program main
    use Solvers

    real(kind=dp), dimension(3):: jacobiSol, gaussSeidelSol
    real(kind=dp), dimension(3, 3)::operationalMatrix
    real(kind=dp), dimension(3):: equalsTo
    jacobiSol = [0,0,0]
    gaussSeidelSol = [0,0,0]
    equalsTo = [3,9,-6]
    ! call jacobiSolver(jacobiSol, 100)
    operationalMatrix = reshape([4.0d0, -1.0d0, -1.0d0, -2.0d0, 6.0d0, 1.0d0, -1.0d0, 1.0d0, 7.0d0], shape(operationalMatrix))
    call jacobiSolver(jacobiSol, operationalMatrix, equalsTo, 500)
    print *, "Jacobi solver gives: ", jacobiSol

    call gaussSeidelSolver(gaussSeidelSol, operationalMatrix, equalsTo, 500)
    print *, "Gauss Seidel solver gives: ", gaussSeidelSol

    print *, dotProduct(operationalMatrix(:, 1), jacobiSol)
    print *, equalsTo
    ! print *, dotProduct([1.0d0,2.0d0,3.0d0], [1.0d0,2.0d0,3.0d0])
    ! call gaussSeidelSolver(gaussSeidelSol, 100)
end program main