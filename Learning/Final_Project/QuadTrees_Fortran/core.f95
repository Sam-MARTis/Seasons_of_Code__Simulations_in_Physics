module custom_types
    implicit none

    type::Points
        real:: mass = 1
        real:: x = 200
        real:: y = 200
        real::vx = 10
        real::vy = 10
        real::size = 2

    end type

    type::QuadTree

    !Properties
    real::x, y, width, height

    !Pointers to subtrees
    type(QuadTree), pointer:: NW => null()
    type(QuadTree), pointer:: NE => null()
    type(QuadTree), pointer:: SE => null()
    type(QuadTree), pointer:: SW => null()

    !Data
    logical:: isDivided = .false.
    integer::pointsCount = 0
    type(Points), dimension(4):: pointsArr

    contains
    procedure:: subdivide



    end type
    
contains

subroutine subdivide(self)
    type(QuadTree), intent(inout) :: self

    
end subroutine subdivide

    
end module custom_types

program main
end program main