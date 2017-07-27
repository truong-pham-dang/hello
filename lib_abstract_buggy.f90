module lib_abstract_buggy
use type_abstract_buggy, only : abstract_buggy
implicit none
private
public :: raise_bug
contains
  subroutine raise_bug(bug, scalar)
  class(abstract_buggy), intent(INOUT) :: bug
  integer,               intent(IN)    :: scalar

  call bug%printf()
  bug = bug * scalar
  call bug%printf()
  bug = scalar * bug
  call bug%printf()
  return
  endsubroutine raise_bug
endmodule lib_abstract_buggy