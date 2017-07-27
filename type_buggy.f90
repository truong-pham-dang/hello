module type_buggy

   use type_abstract_buggy, only : abstract_buggy

   implicit none

   private

   public :: buggy

   type, extends(abstract_buggy) :: buggy
      private
      real, dimension(:), allocatable :: array
      integer                         :: scalar=0
   contains
      ! public methods
      procedure, pass(self), public :: printf
      procedure, pass(self), public :: set => set_buggy
      ! private methods
      procedure, pass(lhs), private :: buggy_multiply_scalar
      procedure, pass(rhs), private :: scalar_multiply_buggy
      procedure, pass(lhs), private :: buggy_assign_buggy
   end type buggy

   interface buggy
      procedure create_buggy
   end interface

contains

   pure function create_buggy(array, scalar) result(bug)
      real, dimension(:), intent(IN) :: array
      integer,            intent(IN) :: scalar
      type(buggy)                    :: bug

      bug%array = array
      bug%scalar = scalar
      return
   end function create_buggy

   pure subroutine set_buggy(self, array, scalar)

      class(buggy), intent(inout)    :: self
      real, dimension(:), intent(IN) :: array
      integer,            intent(IN) :: scalar

      self%array = array
      self%scalar = scalar

      return

   end subroutine set_buggy

   subroutine printf(self)
      class(buggy), intent(IN) :: self
      integer      :: i

      print "(A)", "Array:"
      do i=1, size(self%array)
         print*, self%array(i)
      enddo
      print "(A,I5)", "Scalar: ", self%scalar
   end subroutine printf

   function buggy_multiply_scalar(lhs, rhs) result(multy)
      class(buggy), intent(IN)           :: lhs
      integer,      intent(IN)           :: rhs
      class(abstract_buggy), allocatable :: multy
      type(buggy),           allocatable :: multy_tmp

      allocate(buggy :: multy_tmp)
      multy_tmp%array = lhs%array * rhs
      multy_tmp%scalar = lhs%scalar
      call move_alloc(multy_tmp, multy)
      return
   end function buggy_multiply_scalar

   pure function scalar_multiply_buggy(lhs, rhs) result(multy)
      integer,      intent(IN)           :: lhs
      class(buggy), intent(IN)           :: rhs
      class(abstract_buggy), allocatable :: multy
      type(buggy),           allocatable :: multy_tmp

      allocate(buggy :: multy_tmp)
      multy_tmp%array = rhs%array * lhs
      multy_tmp%scalar = rhs%scalar
      call move_alloc(multy_tmp, multy)
      return
   end function scalar_multiply_buggy

   pure subroutine buggy_assign_buggy(lhs, rhs)
      class(buggy),          intent(INOUT) :: lhs
      class(abstract_buggy), intent(IN)    :: rhs

      select type (rhs)
         class is(buggy)
            if (allocated(rhs%array)) lhs%array = rhs%array
            lhs%scalar = rhs%scalar
      endselect
      return
   end subroutine buggy_assign_buggy

end module type_buggy   