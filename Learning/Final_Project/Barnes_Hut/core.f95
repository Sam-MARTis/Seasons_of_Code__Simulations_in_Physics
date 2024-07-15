module custom_types
    
    implicit none
    integer, parameter:: PointsPerNode = 4

    type :: Points
        real :: mass = 1
        real :: x = 200
        real :: y = 200
        real :: vx = 10
        real :: vy = 10
        real :: size = 2
        real:: forceX = 0
        real:: forceY = 0
    end type Points

    type :: QuadTree
        ! Properties
        real :: x, y, width, height

        ! Pointers to subtrees
        type(QuadTree), pointer :: NW => null()
        type(QuadTree), pointer :: NE => null()
        type(QuadTree), pointer :: SE => null()
        type(QuadTree), pointer :: SW => null()

        ! Data
        logical :: isDivided = .false.
        integer :: pointsCount = 0
        integer :: pointsContained = 0
        real :: massContained = 0
        real, dimension(2):: com = [0, 0]
        type(Points), dimension(PointsPerNode) :: pointsArray
    end type QuadTree

contains

    recursive subroutine addPoints(self, point)
        type(QuadTree), intent(inout) :: self
        type(Points), intent(in) :: point

        logical:: isPointXInBounds, isPointYInBounds
        isPointXInBounds = (point%x >= self%x .and. point%x < (self%x + self%width))
        isPointYInBounds = (point%y >= self%y .and. point%y < (self%y + self%height))

        if ((isPointXInBounds .and. isPointYInBounds) .eqv. .false.) then
            return
        end if
        self%pointsContained = self%pointsContained + 1

        self%com(1) = (self%com(1) * self%massContained + point%mass * point%x) / (self%massContained + point%mass)
        self%com(2) = (self%com(2) * self%massContained + point%mass * point%y) / (self%massContained + point%mass)

        self%massContained = self%massContained + point%mass

        if (self%isDivided) then
            call addPoints(self%NW, point)
            call addPoints(self%NE, point)
            call addPoints(self%SE, point)
            call addPoints(self%SW, point)
        else
            if (self%pointsCount < PointsPerNode) then
                self%pointsArray(self%pointsCount + 1) = point
                self%pointsCount = self%pointsCount + 1
            else
                call subdivide(self)
                call addPoints(self%NW, point)
                call addPoints(self%NE, point)
                call addPoints(self%SE, point)
                call addPoints(self%SW, point)
            end if
        end if
    end subroutine addPoints

    function constructQuadTree(pointsToAdd) result(tree)
        type(QuadTree):: tree
        type(Points), dimension(:)::pointsToAdd
        integer:: i
        do i = 1, size(pointsToAdd)
            call addPoints(tree, pointsToAdd(i))
        end do
    end function

    function doesIntersect(tree, rx1, ry1, width, height) result (isIntersecting)
        type(QuadTree):: tree
        real :: rx1, ry1, width, height, rx2, ry2, x1, y1, x2, y2
        logical:: isIntersecting

        x1 = tree%x
        x2 = x1+tree%width

        y1 = tree%y
        y2 = y1+tree%width

        rx2 = rx1 + width
        ry2 = ry1 + height

        isIntersecting = (((x2 >= rx1) .and. (x1 < rx2)) .and. ((y1 < ry2) .and. (y2 >= ry1)))

    end function


    function inRange(x, y, rx1, ry1, width, height) result (isInside)
        real :: rx1, ry1, width, height, x, y
        logical:: isInside, isXIn, isYIn
        isXIn = (x>=rx1 .and. x<(rx1+width))
        isYIn = (y>ry1 .and. y<(ry1+height))
        isInside = isXIn .and. isYIn

    end function

    recursive function queryTreeRegionForPoints(tree, rx1, ry1, width, height) result (pointsArray)
        type(QuadTree):: tree
        real :: rx1, ry1, width, height
        type(Points), dimension(:), allocatable:: pointsArray, pointsArrayNW, pointsArrayNE, pointsArraySE, pointsArraySW
        integer:: i, n, validPointsCount, counter
        ! type(Points), dimension(tree%pointsCount):: pointsArr
        if(doesIntersect(tree, rx1, ry1, width, height) .eqv. .false.) then
            allocate(pointsArray(0))
            return
        end if

        if (tree%isDivided .eqv. .false.) then
            validPointsCount = 0
            counter = 1

            ! do i=1, tree%pointsCount
            !     pointsArr(i) = tree%pointsArray(i)
            ! end do
            ! pointsArray = pointsArr
            ! return
            do i=1, tree%pointsCount
                if(inRange(tree%pointsArray(i)%x, tree%pointsArray(i)%y, rx1, ry1, width, height)) then
                    validPointsCount = validPointsCount + 1
                end if
            end do

            allocate(pointsArray(validPointsCount))
            do i=1, tree%pointsCount

                if(inRange(tree%pointsArray(i)%x, tree%pointsArray(i)%y, rx1, ry1, width, height) .eqv. .true.) then
                    pointsArray(counter) = tree%pointsArray(i)
                    counter = counter + 1
                end if
            end do
        else
            pointsArrayNW = queryTreeRegionForPoints(tree%NW, rx1, ry1, width, height)
            pointsArrayNE = queryTreeRegionForPoints(tree%NE, rx1, ry1, width, height)
            pointsArraySE = queryTreeRegionForPoints(tree%SE, rx1, ry1, width, height)
            pointsArraySW = queryTreeRegionForPoints(tree%SW, rx1, ry1, width, height)
            n = size(pointsArrayNW) + size(pointsArrayNE) + size(pointsArraySE) + size(pointsArraySW)
            allocate(pointsArray(n))
            do i = 1, size(pointsArrayNW)
                pointsArray(i) = pointsArrayNW(i)
            end do
            do i = 1, size(pointsArrayNE)
                pointsArray(size(pointsArrayNW)+i) = pointsArrayNE(i)
            end do
            do i = 1, size(pointsArraySE)
                pointsArray(size(pointsArrayNW)+size(pointsArrayNE)+i) = pointsArraySE(i)
            end do
            do i = 1, size(pointsArraySW)
                pointsArray(size(pointsArrayNW)+size(pointsArrayNE)+size(pointsArraySE)+i) = pointsArraySW(i)
            end do

            return

        end if

        
    

    end function

    subroutine subdivide(self)
        type(QuadTree), intent(inout) :: self
        integer :: i
        if (self%isDivided) then
            error stop "QuadTree is already divided"
        end if
        if (self%pointsCount < PointsPerNode) then
            error stop "QuadTree node has less points than its maximum node capacity. Not supposed to subdivide"
        end if

        allocate(self%NW)
        allocate(self%NE)
        allocate(self%SE)
        allocate(self%SW)

        self%NW%x = self%x
        self%NW%y = self%y
        self%NW%width = self%width / 2
        self%NW%height = self%height / 2

        self%NE%x = self%x + self%width / 2
        self%NE%y = self%y
        self%NE%width = self%width / 2
        self%NE%height = self%height / 2

        self%SE%x = self%x + self%width / 2
        self%SE%y = self%y + self%height / 2
        self%SE%width = self%width / 2
        self%SE%height = self%height / 2

        self%SW%x = self%x
        self%SW%y = self%y + self%height / 2
        self%SW%width = self%width / 2
        self%SW%height = self%height / 2

        self%isDivided = .true.

        do i = 1, self%pointsCount
            call addPoints(self%NW, self%pointsArray(i))
            call addPoints(self%NE, self%pointsArray(i))
            call addPoints(self%SE, self%pointsArray(i))
            call addPoints(self%SW, self%pointsArray(i))
        end do
    end subroutine subdivide

    recursive subroutine deallocateQuadTree(self)
        type(QuadTree), intent(inout) :: self

        if (associated(self%NW)) then
            call deallocateQuadTree(self%NW)
            deallocate(self%NW)
        end if
        if (associated(self%NE)) then
            call deallocateQuadTree(self%NE)
            deallocate(self%NE)
        end if
        if (associated(self%SE)) then
            call deallocateQuadTree(self%SE)
            deallocate(self%SE)
        end if
        if (associated(self%SW)) then
            call deallocateQuadTree(self%SW)
            deallocate(self%SW)
        end if
    end subroutine deallocateQuadTree

end module custom_types

module Barnes_Hut 

    use custom_types
    implicit none
    
    contains

    recursive function findForceOnParticle(point, tree, G, theta_max) result(forceVal)
    type(Points):: point
    type(QuadTree):: tree
    real:: G, theta_max
    real, dimension(2):: forceVal
    real:: theta, distance, dx, dy
    integer:: i
    forceVal = [0,0]

    dx = tree%com(1) - point%x 
    dy = tree%com(2) - point%y
    distance = sqrt(dx**2 + dy**2)
    ! if (distance == 0) then
    !     forceVal(1) = 0
    !     forceVal(2) = 0
    !     return
    ! end if
    if (distance==0) then
        theta = theta_max + 1 !If distance is 0, then theta will be greater than theta_max
    else
        theta = tree%width / distance !Im assuming square quad tree.
    end if

    if(theta>theta_max) then
        if(tree%isDivided .eqv. .true.) then
            forceVal = findForceOnParticle(point, tree%NW, G, theta_max)
            forceVal = forceVal + findForceOnParticle(point, tree%NE, G, theta_max)
            forceVal = forceVal + findForceOnParticle(point, tree%SE, G, theta_max)
            forceVal = forceVal + findForceOnParticle(point, tree%SW, G, theta_max)
        else
            do i=1, tree%pointsCount
                dx = tree%pointsArray(i)%x - point%x
                dy = tree%pointsArray(i)%y - point%y
                distance = sqrt(dx**2 + dy**2)
                if (distance == 0) then
                    cycle
                end if
                forceVal(1) = forceVal(1) + G * point%mass * tree%pointsArray(i)%mass * dx / distance**3
                forceVal(2) = forceVal(2) + G * point%mass * tree%pointsArray(i)%mass * dy / distance**3
            end do
            
        end if
    else 
        !Since this is running, distance shouldnt be 0
        forceVal(1) = G * point%mass * tree%massContained * dx / distance**3
        forceVal(2) = G * point%mass * tree%massContained * dy / distance**3
    end if
    end function findForceOnParticle


    subroutine updatePositionAndVelocities(pointsToUpdate, dt)
        real, intent(in):: dt
        type(Points), dimension(:), intent(inout):: pointsToUpdate
        integer:: i

        do i = 1, size(pointsToUpdate)
            pointsToUpdate(i)%x = pointsToUpdate(i)%x + pointsToUpdate(i)%vx*dt/2.0
            pointsToUpdate(i)%vx = pointsToUpdate(i)%vx + pointsToUpdate(i)%forceX*dt
            pointsToUpdate(i)%x = pointsToUpdate(i)%x + pointsToUpdate(i)%vx*dt/2.0

            pointsToUpdate(i)%y = pointsToUpdate(i)%y + pointsToUpdate(i)%vy*dt/2.0
            pointsToUpdate(i)%vy = pointsToUpdate(i)%vy + pointsToUpdate(i)%forceY*dt
            pointsToUpdate(i)%y = pointsToUpdate(i)%y + pointsToUpdate(i)%vy*dt/2.0


            pointsToUpdate(i)%forceX = 0
            pointsToUpdate(i)%forceY = 0
        end do
    end subroutine

    subroutine updateStep(pointsArrayMain, G, theta_max, dt)
        type(Points), dimension(:), intent(inout):: pointsArrayMain
        real, intent(in):: G, theta_max, dt

        type(QuadTree):: mainTree
        integer:: i

        mainTree = constructQuadTree(pointsArrayMain)

        do i = 1, size(pointsArrayMain)
            call findForceOnParticle(pointsArrayMain(i), mainTree, G, theta_max)
        end do

        do i = 1, size(pointsArrayMain)
            call findForceOnParticle(pointsArrayMain(i), mainTree, G, theta_max)
        end do

        call updatePositionAndVelocities(pointsArrayMain, dt)

        call deallocateQuadTree(mainTree)
        
    end subroutine
end module Barnes_Hut


program main
    use custom_types
    implicit none

    type(QuadTree) :: root
    type(Points) :: point
    type(Points), dimension(:), allocatable:: pointsArray
    integer:: i
    point%x = 200
    point%y = 200
    point%vx = 10
    point%vy = 10
    point%size = 2



    root%x = 0
    root%y = 0
    root%width = 400
    root%height = 400




    call addPoints(root, point)
    call addPoints(root, Points(1, 1, 1, 1, 1))
    call addPoints(root, Points(2, 2, 2, 2, 2))
    call addPoints(root, Points(3, 3, 3, 3, 3))
    call addPoints(root, Points(4, 4, 4, 4, 4))
    call addPoints(root, Points(4, 20, 200, 4, 4))

    pointsArray= queryTreeRegionForPoints(root, 19.0, 190.0, 21.0, 10.1)

    

    ! print *, root%pointsArray(1)%x
    ! print *, root%pointsArray(1)%y
    ! print *, root%isDivided
    ! print *, root%pointsCount 
    ! print *, root%NW%pointsCount
    ! print *, root%NE%pointsCount
    ! print *, root%SE%pointsCount
    ! print *, root%SW%pointsCount
    do i = 1, size(pointsArray)
        print *, pointsArray(i)%x
        print *, pointsArray(i)%y
    end do  
    print *, size(pointsArray)



    call deallocateQuadTree(root)
end program main