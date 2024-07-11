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
    end type
    
contains

subroutine addPoints(self, point)
    type(QuadTree), intent(inout) :: self
    type(Points) , intent(inout):: point
    if(((point%x>=self%x .and. point%x<(self%x + self%width)) .and. (point%y>=self%y .and. point%y<(self%y + self%width))) .eqv. .false.) then
        return
    end if
    if (self%isDivided .eqv. .true.) then
    end if



end subroutine addPoints

subroutine subdivide(self)
    type(QuadTree), intent(inout) :: self
    type(QuadTree), target :: nw, ne, se, sw


    self%NW => nw
    self%NE => ne
    self%SE => se
    self%SW => sw
end subroutine subdivide

    
end module custom_types

program main
end program main