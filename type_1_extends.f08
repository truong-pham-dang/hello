module type_1_extends
    use type_1
    implicit none

    type, extends(type1) :: type2
    procedure(type2interface), pointer :: point2
    end type

  interface
    subroutine type2interface(a2)
      import type2
      implicit none
      class(type2) :: a2

    end subroutine
  end interface

  contains

    subroutine type2subroutine(a2)
      implicit none
      class(type2) :: a2

      write(*,*) 'hello world 2'

    end subroutine type2subroutine
end module
