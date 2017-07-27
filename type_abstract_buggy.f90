module type_abstract_buggy
implicit none
private
public :: abstract_buggy

type, abstract :: abstract_buggy
  contains
    ! public methods
    procedure(abstract_printf), public, deferred :: printf
    generic,                    public           :: operator(*) => buggy_multiply_scalar, scalar_multiply_buggy
    generic,                    public           :: assignment(=) => buggy_assign_buggy
    ! private methods
    procedure(abstract_buggy_multiply_scalar),       pass(lhs), private, deferred :: buggy_multiply_scalar
    procedure(scalar_multiply_abstract_buggy),       pass(rhs), private, deferred :: scalar_multiply_buggy
    procedure(abstract_buggy_assign_abstract_buggy), pass(lhs), private, deferred :: buggy_assign_buggy
endtype abstract_buggy
abstract interface
  subroutine abstract_printf(self)
  import :: abstract_buggy
  class(abstract_buggy), intent(IN) :: self
  endsubroutine abstract_printf

  function abstract_buggy_multiply_scalar(lhs, rhs) result(multy)
  import :: abstract_buggy
  class(abstract_buggy), intent(IN)  :: lhs
  integer,               intent(IN)  :: rhs
  class(abstract_buggy), allocatable :: multy
  endfunction abstract_buggy_multiply_scalar

  function scalar_multiply_abstract_buggy(lhs, rhs) result(multy)
  import :: abstract_buggy
  integer,               intent(IN)  :: lhs
  class(abstract_buggy), intent(IN)  :: rhs
  class(abstract_buggy), allocatable :: multy
  endfunction scalar_multiply_abstract_buggy

  pure subroutine abstract_buggy_assign_abstract_buggy(lhs, rhs)
  import :: abstract_buggy
  class(abstract_buggy), intent(INOUT) :: lhs
  class(abstract_buggy), intent(IN)    :: rhs
  endsubroutine abstract_buggy_assign_abstract_buggy
endinterface
endmodule type_abstract_buggy