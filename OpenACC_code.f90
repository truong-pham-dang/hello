program OpenACC_code
  implicit none
  integer, parameter              :: dp = selected_real_kind(15,307)
  real, dimension(:), allocatable :: a, b
  real(dp)                        :: start_t, end_t
  integer, parameter              :: n = 1000000
  integer                         :: i 


  call cpu_time(start_t)

  do i = 1, 100 
  call random_seed
  allocate(a(n), b(n))
   
  call random_number(a)
  
  !$acc data copy(a,b)
  call process(a, b, n)
  !$acc end data
  
  deallocate(a, b)
  enddo  
  
  call cpu_time(end_t)

  write(*,20) end_t-start_t
  20 format('Total elapsed time is ', f10.5, ' seconds.')

  contains
  subroutine process( a, b, n )
    real, intent(inout)    :: a(n), b(n)
    integer, intent(in)    :: n
    integer                :: i

    !$acc parallel loop
	do i = 1, n
        b(i) = exp(sin(a(i)))
    enddo
	!$acc end parallel 
	
  end subroutine process
end program OpenACC_code
