! Reference: http://rosettacode.org/wiki/Rosetta_Code
module Points_Module
  implicit none
 
  type point
     real :: x, y
  end type point
 
  interface operator (-)
     module procedure pt_sub
  end interface
 
  interface len
     module procedure pt_len
  end interface
 
  public :: point
  private :: pt_sub, pt_len
 
contains
 
  function pt_sub(a, b) result(c)
    type(point), intent(in) :: a, b
    type(point) :: c
 
    c = point(a%x - b%x, a%y - b%y)
  end function pt_sub
 
  function pt_len(a) result(l)
    type(point), intent(in) :: a
    real :: l
 
    l = sqrt((a%x)**2 + (a%y)**2)
  end function pt_len
 
end module Points_Module

module Polygons
  use Points_Module
  implicit none
 
  type polygon
     type(point), dimension(:), allocatable :: points
     integer, dimension(:), allocatable :: vertices
  end type polygon
 
contains
 
  function create_polygon(pts, vt)
    type(polygon) :: create_polygon
    type(point), dimension(:), intent(in) :: pts
    integer, dimension(:), intent(in) :: vt
 
    integer :: np, nv
 
    np = size(pts,1)
    nv = size(vt,1)
 
    allocate(create_polygon%points(np), create_polygon%vertices(nv))
    create_polygon%points = pts
    create_polygon%vertices = vt
 
  end function create_polygon
 
  subroutine free_polygon(pol)
    type(polygon), intent(inout) :: pol
 
    deallocate(pol%points, pol%vertices)
 
  end subroutine free_polygon
 
end module Polygons

module Ray_Casting_Algo
  use Polygons
  implicit none
 
  real, parameter, private :: eps = 0.00001
  private :: ray_intersects_seg
 
contains
 
  function ray_intersects_seg(p0, a0, b0) result(intersect)
    type(point), intent(in) :: p0, a0, b0
    logical :: intersect
 
    type(point) :: a, b, p
    real :: m_red, m_blue
 
    p = p0
    ! let variable "a" be the point with smallest y coordinate
    if ( a0%y > b0%y ) then
       b = a0
       a = b0
    else
       a = a0
       b = b0
    end if
 
    if ( (p%y == a%y) .or. (p%y == b%y) ) p%y = p%y + eps
 
    intersect = .false.
 
    if ( (p%y > b%y) .or. (p%y < a%y) ) return
    if ( p%x > max(a%x, b%x) ) return
 
    if ( p%x < min(a%x, b%x) ) then
       intersect = .true.
    else
       if ( abs(a%x - b%x) > tiny(a%x) ) then
          m_red = (b%y - a%y) / (b%x - a%x)
       else
          m_red = huge(m_red)
       end if
       if ( abs(a%x - p%x) > tiny(a%x) ) then
          m_blue = (p%y - a%y) / (p%x - a%x)
       else
          m_blue = huge(m_blue)
       end if
       if ( m_blue >= m_red ) then
          intersect = .true.
       else
          intersect = .false.
       end if
    end if
 
  end function ray_intersects_seg
 
  function point_is_inside(p, pol) result(inside)
    logical :: inside
    type(point), intent(in) :: p
    type(polygon), intent(in) :: pol
 
    integer :: i, cnt, pa, pb
 
    cnt = 0
    do i = lbound(pol%vertices,1), ubound(pol%vertices,1), 2
       pa = pol%vertices(i)
       pb = pol%vertices(i+1)
       if ( ray_intersects_seg(p, pol%points(pa), pol%points(pb)) ) cnt = cnt + 1
    end do
 
    inside = .true.
    if ( mod(cnt, 2) == 0 ) then
       inside = .false.
    end if
 
  end function point_is_inside
 
end module Ray_Casting_Algo

program Pointpoly
  use Points_Module
  use Ray_Casting_Algo
  implicit none
 
  character(len=16), dimension(4) :: names
  type(polygon), dimension(4) :: polys
  type(point), dimension(14) :: pts
  type(point), dimension(7) :: p
 
  integer :: i, j
 
  pts = (/ point(0,0), point(10,0), point(10,10), point(0,10), &
           point(2.5,2.5), point(7.5,2.5), point(7.5,7.5), point(2.5,7.5), &
           point(0,5), point(10,5), &
           point(3,0), point(7,0), point(7,10), point(3,10) /)
 
  polys(1) = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,1 /) )
  polys(2) = create_polygon(pts, (/ 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8, 8,5 /) )
  polys(3) = create_polygon(pts, (/ 1,5, 5,4, 4,8, 8,7, 7,3, 3,2, 2,5 /) )
  polys(4) = create_polygon(pts, (/ 11,12, 12,10, 10,13, 13,14, 14,9, 9,11 /) )
 
  !names = (/ "square", "square hole", "strange", "hexagon" /) !possible with ifort, impossible with gfortran
  names(1) = "square"
  names(2) = "square hole"
  names(3) = "strange"
  names(4) = "hexagon"
 
  p = (/ point(5,5), point(5, 8), point(-10, 5), point(0,5), point(10,5), &
         point(8,5), point(10,10) /)
 
  do j = 1, size(p)
     do i = 1, size(polys)
        write(*, "('point (',F8.2,',',F8.2,') is inside ',A,'? ', L)") &
             p(j)%x, p(j)%y, names(i), point_is_inside(p(j), polys(i))
     end do
     print *, ""
  end do

!# vtk DataFile Version 1.0
!vtk output
!ASCII
 
!DATASET POLYDATA
!POINTS           4 float
!   0.00000000       0.00000000     0.0
!   10.0000000       0.00000000     0.0
!   10.0000000       10.0000000     0.0
!   0.00000000       10.0000000     0.0

!LINES 4 12
!   2 0 1 
!   2 1 2
!   2 2 3
!   2 3 0
  
  open(1, file = 'polys1.vtk', status = 'replace')
  write(1,'(a)')'# vtk DataFile Version 1.0'
  write(1,'(a)')'vtk output'
  write(1,'(a)')'ASCII'
  write(1,*) ''
  write(1,'(a)')'DATASET POLYDATA'
  write(1,'(a)')'POINTS'//trim(' 4')//' float'
  do i = 1, 4
     write(1,*) polys(1)%points(i)%x, polys(1)%points(i)%y, '0.0'
  enddo 
  write(1,*) ''
  write(1,'(a)')'LINES'//trim(' 4')//trim(' 12')
  write(1,*) 2,0,1
  write(1,*) 2,1,2
  write(1,*) 2,2,3
  write(1,*) 2,3,0
  close(1)
 
  do i = 1, size(polys)
     call free_polygon(polys(i))
  end do
 
end program Pointpoly

