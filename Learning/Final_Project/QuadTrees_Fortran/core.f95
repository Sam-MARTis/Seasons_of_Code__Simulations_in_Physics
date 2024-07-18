module custom_types
    
    implicit none
    integer, parameter:: PointsPerNode = 4

    type :: Body
        real :: mass = 1
        real :: x = 200
        real :: y = 200
        real :: vx = 10
        real :: vy = 10
        real :: size = 2
    end type Body

    type :: QuadTree
        ! Properties
        real :: x, y, width, height

        ! Pointers to subtrees
        type(QuadTree), dimension(:), allocatable:: subTrees

        ! Data
        logical :: isDivided = .false.
        integer :: pointsCount = 0
        integer :: pointsContained = 0
        real :: massContained = 0
        real, dimension(2):: com = [0, 0]
        type(Body), dimension(PointsPerNode) :: pointsArray
    end type QuadTree

contains

    recursive subroutine addPoints(self, point)
        type(QuadTree), intent(inout) :: self
        type(Body), intent(in) :: point
        integer:: iterVal

        logical:: isPointXInBounds, isPointYInBounds
        isPointXInBounds = (point%x >= self%x .and. point%x < (self%x + self%width))
        isPointYInBounds = (point%y >= self%y .and. point%y < (self%y + self%height))
        !First check if point to add is in valid boundry
        !If yes, then check if tree is divided
        !If divided, add to subtrees
        !If not divided, add to self

        if ((isPointXInBounds .and. isPointYInBounds) .eqv. .false.) then
            return
        end if
        self%pointsContained = self%pointsContained + 1

        self%com(1) = (self%com(1) * self%massContained + point%mass * point%x) / (self%massContained + point%mass)
        self%com(2) = (self%com(2) * self%massContained + point%mass * point%y) / (self%massContained + point%mass)

        self%massContained = self%massContained + point%mass

        if (self%isDivided) then
            do iterVal = 1, 4
                call addPoints(self%subTrees(iterVal), point)
            end do
        else
            if (self%pointsCount < PointsPerNode) then
                self%pointsArray(self%pointsCount + 1) = point
                self%pointsCount = self%pointsCount + 1
            else
                call subdivide(self)
                do iterVal = 1, 4
                    call addPoints(self%subTrees(iterVal), point)
                end do
            end if
        end if
    end subroutine addPoints

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
        type(Body), dimension(:), allocatable:: pointsArray, pointsArrayNW, pointsArrayNE, pointsArraySE, pointsArraySW
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
            pointsArrayNW = queryTreeRegionForPoints(tree%subTrees(1), rx1, ry1, width, height)
            pointsArrayNE = queryTreeRegionForPoints(tree%subTrees(2), rx1, ry1, width, height)
            pointsArraySE = queryTreeRegionForPoints(tree%subTrees(3), rx1, ry1, width, height)
            pointsArraySW = queryTreeRegionForPoints(tree%subTrees(4), rx1, ry1, width, height)
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
        integer :: i, iterVal
        if (self%isDivided) then
            error stop "QuadTree is already divided"
        end if
        if (self%pointsCount < PointsPerNode) then
            error stop "QuadTree node has less points than its maximum node capacity. Not supposed to subdivide"
        end if

        allocate(self%subTrees(4))

        self%subTrees(1)%x = self%x
        self%subTrees(1)%y = self%y
        self%subTrees(1)%width = self%width / 2
        self%subTrees(1)%height = self%height / 2

        self%subTrees(2)%x = self%x + self%width / 2
        self%subTrees(2)%y = self%y
        self%subTrees(2)%width = self%width / 2
        self%subTrees(2)%height = self%height / 2

        self%subTrees(3)%x = self%x + self%width / 2
        self%subTrees(3)%y = self%y + self%height / 2
        self%subTrees(3)%width = self%width / 2
        self%subTrees(3)%height = self%height / 2

        self%subTrees(4)%x = self%x
        self%subTrees(4)%y = self%y + self%height / 2
        self%subTrees(4)%width = self%width / 2
        self%subTrees(4)%height = self%height / 2

        self%isDivided = .true.

        do i = 1, self%pointsCount
            do iterVal = 1, 4
                call addPoints(self%subTrees(iterVal), self%pointsArray(i))
            end do
        end do
    end subroutine subdivide

    !!Not needed cause arrays used. Automatically deallocated when out of scope
    ! recursive subroutine deallocateQuadTree(self)
    !     type(QuadTree), intent(inout) :: self

    !     if (associated(self%NW)) then
    !         call deallocateQuadTree(self%NW)
    !         deallocate(self%NW)
    !     end if
    !     if (associated(self%NE)) then
    !         call deallocateQuadTree(self%NE)
    !         deallocate(self%NE)
    !     end if
    !     if (associated(self%SE)) then
    !         call deallocateQuadTree(self%SE)
    !         deallocate(self%SE)
    !     end if
    !     if (associated(self%SW)) then
    !         call deallocateQuadTree(self%SW)
    !         deallocate(self%SW)
    !     end if
    ! end subroutine deallocateQuadTree

end module custom_types


program main
    use custom_types
    implicit none

    type(QuadTree) :: root
    type(Body) :: point
    type(Body), dimension(:), allocatable:: bodies
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
    call addPoints(root, Body(1, 1, 1, 1, 1))
    call addPoints(root, Body(2, 2, 2, 2, 2))
    call addPoints(root, Body(3, 3, 3, 3, 3))
    call addPoints(root, Body(4, 4, 4, 4, 4))
    call addPoints(root, Body(4, 20, 200, 4, 4))

    bodies= queryTreeRegionForPoints(root, 0.0, 0.0, 21.0, 10.1)

    

    ! print *, root%pointsArray(1)%x
    ! print *, root%pointsArray(1)%y
    ! print *, root%isDivided
    ! print *, root%pointsCount
    ! print *, root%NW%pointsCount
    ! print *, root%NE%pointsCount
    ! print *, root%SE%pointsCount
    ! print *, root%SW%pointsCount
    do i = 1, size(bodies)
        print *, bodies(i)%x
        print *, bodies(i)%y
    end do  
    print *, size(bodies)



    ! call deallocateQuadTree(root)
end program main