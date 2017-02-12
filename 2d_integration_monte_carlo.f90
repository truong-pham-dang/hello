! FORTRAN version by DANG Truong
! 2017/02/12

program monte_carlo
implicit none
integer(4),parameter :: N = 1000000
real(8)              :: I
real(8)              :: x
real(8)              :: y
real(8)              :: s
integer(4)           :: id

s = 0.0d0
do id = 1, N
  call random_number(x)
  call random_number(y)
  s = s + dexp(x*y)
enddo

I = s/N
print*,'Integration of exp(xy) over [0,1]x[0,1] by Monte-Carlo method'
print*,I

end program monte_carlo

!MATLAB script
!s=0;
!for i=1:N  
!x=rand(0,1); 
!y=rand(0,1);
!s=s+exp(xy); 
!end 
!I = s/N;

