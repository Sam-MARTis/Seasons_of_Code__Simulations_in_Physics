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





program main
    use customTypes
    implicit none
    type(Body)::Earth
    type(Body)::Moon

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


    




end program main