module PlanetModules
    implicit none
    real::G = 100
    public

    type :: Body
    real::mass
    real::x
    real::y !2D for now. I'd rather not deal with Oct trees
    real::vx
    real::vy
    real::size
    integer:: ID
    real::forceX
    real::forceY
    end type


    contains 
    function createBody(mass, x, y, vx, vy, size, id) result(planet)
        real, intent(in) :: mass, x, y, vx, vy, size
        integer, intent(in):: id
        type(Body) :: planet
        planet%mass = mass
        planet%x = x
        planet%y = y
        planet%vx = vx
        planet%vy = vy
        planet%size = size
        planet%ID = id    
        planet%forceX = 0.0
        planet%forceY = 0.0    
    end function createBody

    subroutine planetForceUpdate(planet1, planet2)
        type(Body), intent(inout) :: planet1
        type(Body), intent(inout) :: planet2
        real::dx, dy, r, f, fx, fy, theta
        dx = planet2%x - planet1%x
        dy = planet2%y - planet1%y
        r = sqrt(dx**2 + dy**2)
        theta = atan2(dy, dx)
        f = G * planet1%mass * planet2%mass/ (r**2)
        if(f > 10000) then
            f = 10000
        end if
        fx = f * cos(theta)
        fy = f * sin(theta)
        planet1%forceX = planet1%forceX + fx
        planet1%forceY = planet1%forceY + fy
        planet2%forceX = planet2%forceX - fx
        planet2%forceY = planet2%forceY - fy
    end subroutine 

    subroutine updateState(planet1, dt)
        type(Body), intent(inout) :: planet1
        real, intent(in)::dt
        planet1%vx = planet1%vx + (planet1%forceX/planet1%mass) * dt
        planet1%vy = planet1%vy + (planet1%forceY/planet1%mass) * dt
        planet1%x = planet1%x + planet1%vx * dt
        planet1%y = planet1%y + planet1%vy * dt
        planet1%forceX = 0.0
        planet1%forceY = 0.0
    end subroutine

   
end module PlanetModules

subroutine update(planet1,  planet2, dt)
    use PlanetModules
    type(Body), intent(inout) :: planet1
    type(Body), intent(inout) ::  planet2
    real, intent(in) :: dt
    real::dx, dy, r, f, fx, fy, theta
    dx = planet2%x - planet1%x
    dy = planet2%y - planet1%y
    r = sqrt(dx**2 + dy**2)
    theta = atan2(dy, dx)
    f = 100 * planet1%mass * planet2%mass/ (r**2)
    fx = f * cos(theta)
    fy = f * sin(theta)
    planet1%vx = planet1%vx + fx/planet1%mass * dt
    planet1%vy = planet1%vy + fy/planet1%mass * dt
    planet2%vx = planet2%vx - fx/planet2%mass * dt
    planet2%vy = planet2%vy - fy/planet2%mass * dt
    planet1%x = planet1%x + planet1%vx * dt
    planet1%y = planet1%y + planet1%vy * dt
    planet2%x = planet2%x + planet2%vx * dt
    planet2%y = planet2%y + planet2%vy * dt
end subroutine update

program main
    use PlanetModules
    implicit none
    type(Body)::Earth
    type(Body)::Moon, Asteroid
    integer::i = 0
    real::dt = 0.01
    real::time= 0

    Earth = createBody(1000.0, 300.0, 300.0, 0.0, -2.0, 100.0, 1)
    Moon = createBody(200.0, 500.0, 300.0, 5.0, 10.0, 20.0, 2)
    Asteroid = createBody(20000.0, 400.0, 400.0, 1.0, -10.0, 20.0, 2)
    

    ! planet%mass = mass
    ! planet%x = x
    ! planet%y = y
    ! planet%vx = vx
    ! planet%vy = vy
    ! planet%size = size
    ! planet%ID = id    
    ! planet%forceX = 0.0
    ! planet%forceY = 0.0  

    ! Earth%mass = 1000
    ! Moon%mass = 200
    ! Earth%x = 300
    ! Moon%x = 500
    ! Earth%y = 300
    ! Moon%y = 300
    ! Earth%vx = 0
    ! Moon%vx = 5
    ! Earth%vy = -2
    ! Moon%vy = 10
    ! Earth%size = 100
    ! Moon%size = 20
    ! Earth%forceX = 0.0
    ! Earth%forceY = 0.0
    ! Moon%forceX = 0.0
    ! Moon%forceY = 0.0
    ! Earth%ID = 1
    ! Moon%ID = 1

    open(1, file='solutionValues.txt', status='old')
    write(1,*) 3, dt, G
    write(1, *) Earth%mass, Earth%x, Earth%y, Earth%vx, Earth%vy, Earth%size
    write(1, *) Moon%mass, Moon%x, Moon%y, Moon%vx, Moon%vy, Moon%size
    write(1, *) Asteroid%mass, Asteroid%x, Asteroid%y, Asteroid%vx, Asteroid%vy, Asteroid%size


    do i= 1, 100000
        ! call update(Earth, Moon, dt)
        call planetForceUpdate(Earth, Moon)
        call planetForceUpdate(Earth, Asteroid)
        call planetForceUpdate(Moon, Asteroid)
        call updateState(Earth, dt)
        call updateState(Moon, dt)
        call updateState(Asteroid, dt)
        time = time + dt
        write(1, *) time, Earth%x, Earth%y, Moon%x, Moon%y, Asteroid%x, Asteroid%y
    end do

    close(1)

    call execute_command_line("./sfml-app")
    




end program main