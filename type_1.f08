module type_1
  implicit none

  type :: type1
    procedure(type1interface), pointer :: point1
  end type

  interface
    subroutine type1interface(a)
      import type1
      implicit none
      class(type1) :: a

    end subroutine
  end interface

  contains

    subroutine type1subroutine(a)
      implicit none
      class(type1) :: a

      write(*,*) 'hello world'

    end subroutine type1subroutine
end module
