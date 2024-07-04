module customTypes
    implicit none
    public

    type :: Body
    real::mass
    real::x
    real::y !2D for now. I'd rather not deal with Oct trees
    real::vx
    real::vy
    real::size
    end type

   
end module


subroutine update(planet1,  planet2, dt, G)
    use customTypes
    type(Body), intent(inout) :: planet1
    type(Body), intent(inout) ::  planet2
    real, intent(in) :: dt, G
    real::dx, dy, r, f, fx, fy, theta
    dx = planet2%x - planet1%x
    dy = planet2%y - planet1%y
    r = sqrt(dx**2 + dy**2)
    theta = atan2(dy, dx)
    f = G * planet1%mass * planet2%mass/ (r**2)
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
    use customTypes
    implicit none
    type(Body)::Earth
    type(Body)::Moon
    integer::i = 0
    real::dt = 0.01
    real::time= 0
    real::G = 100

    Earth%mass = 1000
    Moon%mass = 200
    Earth%x = 400
    Moon%x = 700
    Earth%y = 500
    Moon%y = 500
    Earth%vx = 0
    Moon%vx = 0
    Earth%vy = -2
    Moon%vy = 10
    Earth%size = 100
    Moon%size = 20

    open(1, file='solutionValues.txt', status='old')
    write(1,*) 2, dt, G
    write(1, *) Earth%mass, Earth%x, Earth%y, Earth%vx, Earth%vy, Earth%size
    write(1, *) Moon%mass, Moon%x, Moon%y, Moon%vx, Moon%vy, Moon%size


    do i= 1, 10000
        call update(Earth, Moon, dt, G)
        time = time + dt
        write(1, *) time, Earth%x, Earth%y, Moon%x, Moon%y
    end do

    close(1)
    




end program main