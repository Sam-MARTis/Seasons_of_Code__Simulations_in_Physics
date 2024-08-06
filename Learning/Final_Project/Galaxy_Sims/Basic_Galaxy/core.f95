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
        real :: x, y, width, height

        type(QuadTree), dimension(:), allocatable:: subTrees

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

    recursive function queryTreeRegionForPoints(tree, rx1, ry1, width, height) result (pointsArray)
        type(QuadTree):: tree
        real :: rx1, ry1, width, height
        type(Body), dimension(:), allocatable:: pointsArray, pointsArrayNW, pointsArrayNE, pointsArraySE, pointsArraySW
        integer:: i, n, validPointsCount, counter
        if(doesIntersect(tree, rx1, ry1, width, height) .eqv. .false.) then
            allocate(pointsArray(0))
            return
        end if

        if (tree%isDivided .eqv. .false.) then
            validPointsCount = 0
            counter = 1

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

        
    

    end function queryTreeRegionForPoints

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


    function createBodies(n, G_val, Center_Mass) result(bodies)
        integer :: n
        real:: G_val, Center_Mass
        type(Body), dimension(n):: bodies
        integer:: i
        real:: r1, r2, r3, r4, r5, r6, R, angle
        real:: rx0, ry0, rx1, ry1, xm, ym, K
        real:: dx, dy, vMag
        rx0 = 0.0
        ry0 = 0.0
        rx1 = 800.0
        ry1 = 800.0



        k= (G_val * Center_Mass)**0.5

        xm = (rx0 + rx1)/2.0
        ym = (ry0 + ry1)/2.0





        do i = 1, n-1
            R = random_uniform(6.0, min(rx1-xm, ry1-ym))
            angle = random_uniform(0.0, 6.26318531)
            r1 = random_uniform(1.0, 10.0)
        ! r2 = random_uniform(rx0, rx1)
        ! r3 = random_uniform(ry0, ry1)
        r2 = xm + R * cos(angle)
        r3 = ym+ R * sin(angle)
        ! r4 = random_uniform(-1.0, 1.0)
        ! r5 = random_uniform(-1.0, 1.0)
        dx = r2 - xm
        dy = r3 - ym

        vMag = K*1.0/((dx**2 + dy**2)**0.25)
    
        r4 = -vMag*sin(angle)
        r5 = vMag*cos(angle)


        !Adding noide
        r4 = r4 * random_uniform(0.9, 1.1)
        r5 = r5 * random_uniform(0.9, 1.1)


        
        r6 = 0.3 + r1/20
        bodies(i) = createBody(r1, r2, r3, r4, r5, r6)
        end do
        bodies(n) = createBody(Center_Mass, xm, ym, 0.0, 0.0, 4.0)
    end function createBodies



end module Body_Tools





module Barnes_Hut 

    use custom_types
    implicit none
    
    contains
    function dark_matter_distribution(x, y) result (factorValue)
        real, intent(in):: x, y
        real:: factorValue
        real:: dx, dy
        dx = (x - 400)/200.0
        dy = (y - 400)/200.0
        factorValue = 1 + 5*(1.2**(-((x*y)**2)))


    end function dark_matter_distribution



    subroutine makePointsInBoundry(pointsArr, x1, y1, x2, y2)
        type(Body), dimension(:), intent(inout):: pointsArr
        real, intent(in):: x1, x2, y1, y2
        integer:: i
        do i= 1, size(pointsArr)
            if(pointsArr(i)%x > x2) then
                pointsArr(i)%x = x2
            end if
            if(pointsArr(i)%y > y2) then
                pointsArr(i)%y = y2
            end if
            if(pointsArr(i)%x < x1) then
                pointsArr(i)%x = x1
            end if
            if(pointsArr(i)%y < y1) then
                pointsArr(i)%y = y1
            end if

        end do

    end subroutine makePointsInBoundry



    recursive function findForceOnParticle(point, tree, G, theta_max) result(forceVal)
    type(Body):: point
    type(QuadTree):: tree
    real:: G, theta_max
    real, dimension(2):: forceVal
    real:: theta, distance, dx, dy
    integer:: i
    real:: forceAngle
    real:: forceMagnitude
    real:: forceMax = 2000

    forceVal = [0.0, 0.0]

    ! Calculate distance between point and center of mass of the quadtree node
    dx = tree%com(1) - point%x
    dy = tree%com(2) - point%y
    distance = sqrt(dx**2 + dy**2)

    ! Avoid division by zero
    if (distance == 0) then
        theta = theta_max + 1
    else
        theta = min(tree%width, tree%height) / distance
    end if

    ! Determine if we should use the node's COM or recurse
    if (theta > theta_max) then
        ! Node is too large, recurse
        if (tree%isDivided) then
            do i = 1, 4
                forceVal = forceVal + findForceOnParticle(point, tree%subTrees(i), G, theta_max)
            end do
        else
            ! Calculate force from each point in this leaf node
            do i = 1, tree%pointsCount
                dx = tree%pointsArray(i)%x - point%x
                dy = tree%pointsArray(i)%y - point%y
                distance = sqrt(dx**2 + dy**2)

                if (distance /= 0.0) then
                    forceMagnitude = G * point%mass * tree%pointsArray(i)%mass / distance**2
                    forceMagnitude = forceMagnitude * dark_matter_distribution(tree%pointsArray(i)%x, tree%pointsArray(i)%y)
                    forceVal(1) = forceVal(1) + forceMagnitude * dx / distance
                    forceVal(2) = forceVal(2) + forceMagnitude * dy / distance
                end if
            end do
        end if
    else
        ! Use node's center of mass to calculate force
        if (distance /= 0.0) then
            forceMagnitude = G * dark_matter_distribution(tree%com(1), tree%com(2))* point%mass * tree%massContained / distance**2
            forceVal(1) = forceVal(1) + forceMagnitude * dx / distance
            forceVal(2) = forceVal(2) + forceMagnitude * dy / distance
        end if
    end if

    ! Ensure the force is not NaN
    if (isnan(forceVal(1)) .or. isnan(forceVal(2))) then
        print *, "Getting NaN values of force"
        forceVal = [0.0, 0.0]
    end if

    ! Limit the force if it exceeds forceMax
    if (forceVal(1)**2 + forceVal(2)**2 > forceMax**2) then
        forceVal = [0.0, 0.0]
        ! forceAngle = atan2(forceVal(2), forceVal(1))
        ! forceVal(1) = forceMax * cos(forceAngle)
        ! forceVal(2) = forceMax * sin(forceAngle)
    end if
end function findForceOnParticle


    subroutine updatePositionAndVelocities(pointsToUpdate, dt)
        real, intent(in):: dt
        type(Body), dimension(:), intent(inout):: pointsToUpdate
        integer:: i

        do i = 1, size(pointsToUpdate)

            
            pointsToUpdate(i)%x = pointsToUpdate(i)%x + pointsToUpdate(i)%vx*dt/2.0
            pointsToUpdate(i)%vx = pointsToUpdate(i)%vx + pointsToUpdate(i)%forceX*dt/pointsToUpdate(i)%mass
            pointsToUpdate(i)%x = pointsToUpdate(i)%x + pointsToUpdate(i)%vx*dt/2.0

            pointsToUpdate(i)%y = pointsToUpdate(i)%y + pointsToUpdate(i)%vy*dt/2.0
            pointsToUpdate(i)%vy = pointsToUpdate(i)%vy + pointsToUpdate(i)%forceY*dt/pointsToUpdate(i)%mass
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
        real:: x1, x2, y1, y2
        x1 = 50
        x2 = 750
        y1 = 50
        y2 = 750

        mainTree = constructQuadTree(pointsArrayMain, x, y, width, height)

        do i = 1, size(pointsArrayMain)
            forceValueTemp = findForceOnParticle(pointsArrayMain(i), mainTree, G, theta_max)
            ! forceValueTemp = [0.0, 0.0]
            pointsArrayMain(i)%forceX = forceValueTemp(1)
            pointsArrayMain(i)%forceY = forceValueTemp(2)

        end do

        call updatePositionAndVelocities(pointsArrayMain, dt)


        ! call makePointsInBoundry(pointsArrayMain, 50.0, 50.0, 700.0, 500.0)

    end subroutine updateStep
end module Barnes_Hut


program main
    use custom_types
    use Barnes_Hut
    use Body_Tools

    implicit none



    integer, parameter:: noOfBodies = 1000 !Pretty self-explanatory name
    real, parameter:: dt = 0.01 !Time step
    real, parameter:: G = 0.05 !Gravitational constant
    real, parameter:: mainMass = 100000.0 !Mass of central object
    real, parameter:: theta_max = 1.5 !Affects accuracy and speed. Higher is faster but less accurate
    integer, parameter:: iterations = 500000 !Iterations count. Iterations * dt = simulation length

    integer:: i, j, k
    real:: time = 0 
    type(Body), dimension(noOfBodies):: bodies
    bodies = createBodies(noOfBodies, G, mainMass)

    open(1, file='objectStates.txt', status='old')
    write(1,*) size(bodies), dt, G

    do i = 1, size(bodies)
        write(1, *) bodies(i)%mass, bodies(i)%x, bodies(i)%y, bodies(i)%vx, bodies(i)%vy, bodies(i)%size
    end do

    do i= 1, iterations
        do k = 1, 5

            call updateStep(bodies, 0.0, 0.0, 800.0, 800.0, G, theta_max, dt)
            time = time + dt
        end do
        do j=1, size(bodies)
            write(1, *) time, bodies(j)%x, bodies(j)%y
        end do
    end do
    close(1)

    call execute_command_line("./sfml-app")



    


    
    
end program main