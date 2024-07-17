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
        real:: forceX = 0
        real:: forceY = 0
    end type Body

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
        type(Body), dimension(PointsPerNode) :: pointsArray
    end type QuadTree

contains

    recursive subroutine addPoints(self, point)
        type(QuadTree), intent(inout) :: self
        type(Body), intent(in) :: point

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

    function constructQuadTree(pointsToAdd, x, y, width, height) result(tree)
        type(QuadTree):: tree
        real:: x, y, width, height
        type(Body), dimension(:)::pointsToAdd
        integer:: i
        tree%x = x
        tree%y = y
        tree%width = width
        tree%height = height
        do i = 1, size(pointsToAdd)
            call addPoints(tree, pointsToAdd(i))
        end do
    end function constructQuadTree

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

    end function doesIntersect


    function inRange(x, y, rx1, ry1, width, height) result (isInside)
        real :: rx1, ry1, width, height, x, y
        logical:: isInside, isXIn, isYIn
        isXIn = (x>=rx1 .and. x<(rx1+width))
        isYIn = (y>ry1 .and. y<(ry1+height))
        isInside = isXIn .and. isYIn

    end function inRange

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

module Random_Tools
    implicit none
    contains
    subroutine random_stduniform(u)
        implicit none
        real,intent(out) :: u
        real :: r
        call random_number(r)
        u = 1 - r
     end subroutine random_stduniform

    function random_uniform(a,b) result(x)
        implicit none
        real,intent(in) :: a,b

        real::x
        real :: u = 3
        call random_stduniform(u)
        x = (b-a)*u + a
     end function random_uniform
end module Random_Tools

module Body_Tools

    use custom_types
    use Random_Tools

    implicit none
    contains
    function createBody(mass, x, y, vx, vy, size) result(planet)
        real, intent(in) :: mass, x, y, vx, vy, size
        type(Body) :: planet
        planet%mass = mass
        planet%x = x
        planet%y = y
        planet%vx = vx
        planet%vy = vy
        planet%size = size 
        planet%forceX = 0.0
        planet%forceY = 0.0    
    end function createBody


    function createBodies(n) result(bodies)
        integer, intent(in) :: n
        type(Body), dimension(n):: bodies
        integer:: i
        real:: r1, r2, r3, r4, r5, r6
        do i = 1, n
            r1 = random_uniform(1.0, 10.0)
        r2 = random_uniform(50.0, 750.0)
        r3 = random_uniform(50.0, 750.0)
        r4 = random_uniform(-1.0, 1.0)
        r5 = random_uniform(-1.0, 1.0)

        r6 = r1/4
        bodies(i) = createBody(r1, r2, r3, r4, r5, r6)
        end do
    end function createBodies



end module Body_Tools

module Barnes_Hut 

    use custom_types
    implicit none
    
    contains

    recursive function findForceOnParticle(point, tree, G, theta_max) result(forceVal)
    type(Body):: point
    type(QuadTree):: tree
    real:: G, theta_max
    real, dimension(2):: forceVal
    real:: theta, distance, dx, dy
    integer:: i
    real::  forceAngle
    real:: forceMax = 1000
    forceVal = [0,0]

    dx = tree%com(1) - point%x 
    dy = tree%com(2) - point%y

    if (distance==0) then
        theta = theta_max + 1
    else
        theta = tree%width / distance
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
                forceAngle = atan2(dy, dx)
                forceVal(1) = forceVal(1) + G * point%mass * tree%pointsArray(i)%mass * cos(forceAngle) / distance**2
                forceVal(2) = forceVal(2) + G * point%mass * tree%pointsArray(i)%mass * sin(forceAngle) / distance**2
            end do
            
        end if
    else 
        forceAngle = atan2(dy, dx)
        forceVal(1) = forceVal(1) + G * point%mass * tree%pointsArray(i)%mass * cos(forceAngle) / distance**2
        forceVal(2) = forceVal(2) + G * point%mass * tree%pointsArray(i)%mass * sin(forceAngle) / distance**2

    end if

    if((forceVal(1)**2 + forceVal(2)**2)>forceMax**2) then
        forceAngle = atan2(forceVal(2), forceVal(1))
        forceVal(1) = forceMax * cos(forceAngle)
        forceVal(2) = forceMax * sin(forceAngle)
    end if




    end function findForceOnParticle


    subroutine updatePositionAndVelocities(pointsToUpdate, dt)
        real, intent(in):: dt
        type(Body), dimension(:), intent(inout):: pointsToUpdate
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
    end subroutine updatePositionAndVelocities

    subroutine updateStep(pointsArrayMain, x, y, width, height, G, theta_max, dt)
        type(Body), dimension(:), intent(inout):: pointsArrayMain
        real, intent(in):: x, y, width, height
        real, intent(in):: G, theta_max, dt

        type(QuadTree):: mainTree
        real, dimension(2):: forceValueTemp
        integer:: i

        mainTree = constructQuadTree(pointsArrayMain, x, y, width, height)

        do i = 1, size(pointsArrayMain)
            forceValueTemp = findForceOnParticle(pointsArrayMain(i), mainTree, G, theta_max)
            pointsArrayMain(i)%forceX = forceValueTemp(1)
            pointsArrayMain(i)%forceY = forceValueTemp(2)
            ! print *, forceValueTemp
        end do

        call updatePositionAndVelocities(pointsArrayMain, dt)




        call deallocateQuadTree(mainTree)
        
    end subroutine updateStep
end module Barnes_Hut


program main
    use custom_types
    use Barnes_Hut
    use Body_Tools
    implicit none


    integer, parameter:: noOfBodies = 3

    ! type(QuadTree) :: root
    type(Body), dimension(noOfBodies):: bodies
    real:: dt = 0.001
    real:: G = 100
    real:: time = 0
    integer:: i, j
    real:: theta_max = 1.5


    bodies = createBodies(noOfBodies)



    open(1, file='solutionValues.txt', status='old')
    write(1,*) size(bodies), dt, G

    do i = 1, size(bodies)
        write(1, *) bodies(i)%mass, bodies(i)%x, bodies(i)%y, bodies(i)%vx, bodies(i)%vy, bodies(i)%size
    end do

    do i= 1, 20000
        call updateStep(bodies, 0.0, 0.0, 800.0, 800.0, G, theta_max, dt)
        time = time + dt
        do j=1, size(bodies)
            write(1, *) time, bodies(j)%x, bodies(j)%y
        end do
    end do

    close(1)
    call execute_command_line("./sfml-app")
 
end program main