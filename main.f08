
program test
  use type_1
  use type_1_extends
  implicit none

  type(type2) :: mytype2
  procedure(type2subroutine), pointer :: mypointer2
  procedure(type1subroutine), pointer :: mypointer1

  mytype2%point1 => type1subroutine
  mytype2%point2 => type2subroutine

  !mytype2%point1 => type2subroutine  !*1 !THIS IS NOT CONFORMING TO THE STANDARD (PROPOSAL)

  !mytype2%point2 => type1subroutine  !*2 !THIS IS NOT CONFORMING TO THE STANDARD (PROPOSAL)
  !mypointer2 => type1subroutine      !*2 !THIS IS NOT CONFORMING TO THE STANDARD (PROPOSAL)
  !mypointer2 => mypointer1           !*2 !THIS IS NOT CONFORMING TO THE STANDARD (PROPOSAL)


  call mytype2%point1

end program


