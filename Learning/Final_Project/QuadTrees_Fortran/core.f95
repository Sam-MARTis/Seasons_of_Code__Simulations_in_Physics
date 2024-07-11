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
    type(Points), dimension(4):: pointsArray

    end type QuadTree
    
contains

recursive subroutine addPoints(self, point)
    type(QuadTree), intent(inout) :: self
    type(Points) , intent(inout):: point
    if(((point%x>=self%x .and. point%x<(self%x + self%width)) .and. (point%y>=self%y .and. point%y<(self%y + self%width))) .eqv. .false.) then
        return
    end if
    if (self%isDivided .eqv. .true.) then
        call addPoints(self%NW, point)
        call addPoints(self%NE, point)
        call addPoints(self%SE, point)
        call addPoints(self%SW, point)
    else
        if (self%pointsCount < 4) then
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

function createTree() result(tree)
    type(QuadTree) :: tree
end function createTree

subroutine subdivide(self)
    type(QuadTree), intent(inout) :: self
    type(QuadTree), pointer :: nw, ne, se, sw


    self%NW => nw
    self%NE => ne
    self%SE => se
    self%SW => sw
end subroutine subdivide

    
end module custom_types

program main
end program main