module custom_types
    implicit none

    type :: Points
        real :: mass = 1
        real :: x = 200
        real :: y = 200
        real :: vx = 10
        real :: vy = 10
        real :: size = 2
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
        type(Points), dimension(4) :: pointsArray
    end type QuadTree

contains

    recursive subroutine addPoints(self, point)
        type(QuadTree), intent(inout) :: self
        type(Points), intent(inout) :: point

        if (((point%x >= self%x .and. point%x < (self%x + self%width)) .and. (point%y >= self%y .and. point%y < (self%y + self%height))) .eqv. .false.) then
            return
        end if

        if (self%isDivided) then
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

    subroutine subdivide(self)
        type(QuadTree), intent(inout) :: self

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
