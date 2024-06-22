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
        ! l2 is euclidean norm while l(-1) will be interpreted at l_infty norm
        !Beware of overflow
        real(kind=dp), dimension(:)::state
        integer:: l 
        real(kind=dp):: normVal
        integer::i
        normVal = 0.0d0
        if ( l==(-1) ) then
            do i=1, size(state)
                if(state(i)>normVal) then
                    normVal = state(i)
                end if
            end do
        else
            do i=1, size(state)
                normVal = normVal + ((abs(state(i)))**(1.0d0*l))
            end do
            normVal = normVal**(1.0d0/l)
        end if
    end function



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
    function flatten(inputMatrix) result(outputVector)
        real(kind=dp), dimension(:,:)::inputMatrix
        real(kind=dp), dimension(size(inputMatrix, 1)*size(inputMatrix, 2)):: outputVector
        integer::i, j
        do i=1,size(inputMatrix, 1)
            do j=1,size(inputMatrix, 2)
                outputVector((i-1)*size(inputMatrix, 2)+j) = inputMatrix(i, j)
            end do
        end do
        
    end function
    function reverseFlattenToSquare(inputVector) result(outputMatrix)
        real(kind=dp), dimension(:)::inputVector
        real(kind=dp), dimension(int(size(inputVector)**0.5), int(size(inputVector)**0.5)):: outputMatrix
        integer::i, j
        if (int(size(inputVector)**0.5)**2/=size(inputVector)) then
            print*, "Invalid input vector size"
            stop
        end if
        do i=1,int(size(inputVector)**0.5)
            do j=1,int(size(inputVector)**0.5)
                outputMatrix(i, j) = inputVector((i-1)*int(size(inputVector)**0.5)+j)
            end do
        end do
    
        
    end function
    

    
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
        integer::blank, i
        real(kind=dp), dimension(size(state)):: tempState
        ! real(kind=)
        call copyState(tempState, state)


        reverse_matrix = operator_matrix(:, :)


        reverse_rhs(:) = rhs(:)
        call prepareBackwardForSolver(reverse_matrix, reverse_rhs)
        do blank = 1,max_iterations
            !Matrix is diagonally dominant. Therefore convergence is guarenteed
            do concurrent (i = 1:size(state))
                tempState(i) = dotProduct(reverse_matrix(:, i), state) + reverse_rhs(i)             
            end do
            ! if ( norm(addState(state, 1.0d0, tempState, -1.0d0), -1)<0.000001 ) then
                
            !     exit
            ! end if

            call copyState(state, tempState)
            ! print *, state
            ! print *, ""
        end do
        call copyState(state, tempState)
        
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



module customFunctions
    use StateTools, only: dp
    implicit none
    
    
    contains
    pure function p(x, y) result(fx)
        real(kind=dp), intent(in)::x, y
        real(kind=dp)::fx
        fx= 10*(x)
        ! if((x == 25) .and. (y==25)) then
        !     fx=1
        ! end if
    end function

end module customFunctions

module Poisson
    ! use customFunctions
    use StateTools
    use Solvers
    use customFunctions

    contains
    function makeSquareCellGrid(n) result(grid)
        integer, intent(in)::n
        real(kind=dp), dimension(n, n)::grid
        ! real(kind=dp) :: dx, dy
        integer::i, j
        do i=1,n

            do j=1,n
                grid(i, j) = 0
                ! grid(i, j, 1) = 0
                ! grid(i, j, 2) = p(i*dx, j*dy)
            end do
        end do
    end function

    function generateOperationMatrix(n, dx, dy) result (opMatrix)

    
            integer, intent(in) :: n
            real(kind=dp), dimension(n*n,n*n):: opMatrix
            real(kind=dp)::dx, dy
            opMatrix(:,:) = 0.0d0
            do j=1,n 
                do i=1,n 
                    if(i-1>0) then
                        opMatrix((i-1)*n+j, (i-2)*n+j) = 1.0d0/(dx*dy*(-1.0d0))
                    end if
                    if(i+1<=n) then
                        opMatrix((i-1)*n+j, i*n+j) = 1.0d0/(dx*dy*(-1.0d0))
                    end if
                    if(j-1>0) then
                        opMatrix((i-1)*n+j, (i-1)*n+j-1) = 1.0d0/(dx*dy*(-1.0d0))
                    end if
                    if(j+1<=n) then
                        opMatrix((i-1)*n+j, (i-1)*n+j+1) = 1.0d0/(dx*dy*(-1.0d0))
                    end if
                end do
            end do
            do i = 1,n*n 
                opMatrix(i, i) = -4.0d0/(dx*dy*(-1.0d0))
            end do

      
    end function
    function createChargeDistVector(n, x0, y0, dx, dy, func) result(chargeDist)
        interface
            pure function charge_dist(x, y) result(p)
                import dp
                implicit none
                real(kind=dp), intent(in)::x, y
                real(kind=dp)::p
            end function charge_dist
        end interface

        procedure(charge_dist)::func
        integer, intent(in) :: n
        real(kind=dp), intent(in) :: dx, dy, x0, y0
        real(kind=dp), dimension(n*n):: chargeDist
        ! integer::i, j
        ! do i=1,n
        !     do j=1,n
        !         chargeDist((i-1)*n+j) = 1.0d0*func(x0+j*dx, y0+i*dy)
        !     end do
        ! end do
        chargeDist(:) = 0
        chargeDist((int(n/4)-1)*n + int(n/2)) = 1
        chargeDist((int(3*n/4)-1)*n + int(n/2)) = 1
    end function
    ! call jacobiSolver(jacobiSol, operationalMatrix, equalsTo, 1000)
    ! print *, "Jacobi solver gives: ", jacobiSol
    ! subroutine solvePoisson(solMat, operationMatrix, equalsTo, iterations)

    ! end subroutine


end module Poisson

program main
    ! Simulation size 50 x 50, with 100^2 cells?
    use Poisson
    use StateTools
    integer:: i
    real(kind=dp), dimension(:,:), allocatable:: operation_matrix
    real(kind=dp)::delX, delY
    real(kind=dp):: width, height
    real(kind=dp)::startX, startY
    
    integer:: cellsPerUnitLength = 30
    
    real(kind=dp), dimension(:), allocatable:: chargeDist
    ! real(kind=dp), dimension(9):: mat = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    ! real(kind=dp), dimension(:, :), allocatable:: temp
    real(kind=dp), dimension(:, :), allocatable::grid
    real(kind=dp), dimension(:), allocatable::flattened_grid



    width = 5.0d0
    height = 5.0d0
    delX = width/cellsPerUnitLength
    delY = height/cellsPerUnitLength
    startX = 0.0d0
    startY = 0.0d0
    chargeDist = createChargeDistVector(cellsPerUnitLength, startX, startY, delX, delY, p)
    grid = makeSquareCellGrid(cellsPerUnitLength)
    ! print *, chargeDist

    operation_matrix = generateOperationMatrix(cellsPerUnitLength, delX, delY)
    ! print *, operation_matrix
    ! print *, generateOperationMatrix(cellsPerUnitLength, delX, delY)
    
    flattened_grid = flatten(grid)
    ! print *, flattened_grid
    ! print *, operation_matrix

    

    call jacobiSolver(flattened_grid, operation_matrix, chargeDist, 2000)
    ! print *, flattened_grid
    
    grid = reverseFlattenToSquare(flattened_grid)
    ! do i=1, size(grid, 1)
    !     print *, grid(i, :)
    !     print *, ""
    ! end do
    ! print *, grid

    ! do i=1,cellsPerUnitLength
    !     print *, grid(i, :)
    ! end do



    ! print *, chargeDist
    

    ! print *, reverseFlattenToSquare(mat)
    ! print *, size(reverseFlattenToSquare(mat), 1)
    ! print *, flatten(reverseFlattenToSquare(mat))
    ! print *, size(flatten(reverseFlattenToSquare(mat)), 1)
    ! temp = reverseFlattenToSquare(mat)
    ! do i=1,3
    !     print *, temp(i, :)
    !     print *, ""
    ! end do


    ! operation_matrix = generateOperationMatrix(10, delX, delY)
    
    ! print*, operation_matrix
    ! do i = 1,9
    !     print *, operation_matrix(i, :)
    !     print *, ""
    ! end do

    ! call gaussSeidelSolver(gaussSeidelSol, 100)


    open (1, file = 'solutionValues.txt', status="old")
    do i = 1,cellsPerUnitLength
      write(1,*) (grid(i, j), j = 1, cellsPerUnitLength)
    end do
    close(1)


    ! Now plot this in gnuplot using the following commands
    call execute_command_line('gnuplot -p plot.plt')

    

end program main