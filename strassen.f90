PROGRAM TEST_STRASSEN_MATMUL
! Copyright (c) 1992 by Richard J. Hanson @ IMSL, Inc.
! This program may be copied and used without
! restriction as long as this notice is retained.
! The formulas are from Strassen's paper, "Gaussian Elimination is Not optimal,"
! Numer. Math., Vol. 13, p. 354-356, (1969).
 
IMPLICIT NONE
INTEGER :: COUNT1,COUNT2,SIZE_OF_MATRICES, N, RATE
REAL, ALLOCATABLE, DIMENSION(:,:) :: A, B, C
REAL ERR
PRINT *, "Enter the SIZE_OF_MATRICES in the problem:"
READ *, SIZE_OF_MATRICES
PRINT *
N=SIZE_OF_MATRICES; ALLOCATE (A(N,N), B(N,N), C(N,N))
CALL RANDOM_NUMBER(A); CALL RANDOM_NUMBER(B)
CALL SYSTEM_CLOCK(COUNT1,RATE)
CALL STRASSEN_MATMUL(N, A, B, C)
CALL SYSTEM_CLOCK(COUNT2)
WRITE(*,*) ' STRASSEN_MATMUL time:',(COUNT2-COUNT1)/REAL(RATE)

 ERR=SUM(ABS(C))
CALL SYSTEM_CLOCK(COUNT1)
 C=C-MATMUL(A,B)
CALL SYSTEM_CLOCK(COUNT2)
WRITE(*,*) ' MATMUL time:',(COUNT2-COUNT1)/REAL(RATE)
  ERR=SUM(ABS(C))/ERR

DEALLOCATE (A, B, C)
 
PRINT *,"RELATIVE ERROR in STRASSEN_MATMUL vs NORMAL MULTIPLICATION, A*B: ", ERR
 
CONTAINS
RECURSIVE SUBROUTINE STRASSEN_MATMUL (N, A, B, C)
 
IMPLICIT NONE
INTEGER :: N, L
REAL, DIMENSION(:,:) :: A, B, C
REAL, DIMENSION(N/2,N/2) :: P1,P2,P3,P4,P5,P6,P7
 
IF (IAND(N,1) == 1 .or. N <= 150) THEN
                          ! Stop the recursion when an odd dimension or small
                          ! problem is encountered.  Here the word 'small'
                          ! means that the ordinary algorithm is more effective
                          ! for values less than this.  The above value is
                          ! taken from the study by Bailey, et al., J. of
                          ! Supercomputing, Jan., (1991), p. 361.
  C=MATMUL(A,B)
ELSE                      ! Continue dividing the problem into smaller ones of
                          ! half the size.
  L=N/2
 
  CALL STRASSEN_MATMUL (L,A(1:L,1:L)+A(L+1:N,L+1:N), B(1:L,1:L)+B(L+1:N,L+1:N), P1)
  CALL STRASSEN_MATMUL (L,A(L+1:N,1:L)+A(L+1:N,L+1:N), B(1:L,1:L)  , P2)
  CALL STRASSEN_MATMUL (L,A(1:L,1:L), B(1:L,L+1:N)-B(L+1:N,L+1:N), P3)
  CALL STRASSEN_MATMUL (L,A(L+1:N,L+1:N), B(L+1:N,1:L)-B(1:L,1:L), P4)
  CALL STRASSEN_MATMUL (L,A(1:L,1:L)+A(1:L,L+1:N), B(L+1:N,L+1:N), P5)
  CALL STRASSEN_MATMUL (L,A(L+1:N,1:L)-A(1:L,1:L), B(1:L,1:L)+B(1:L,L+1:N), P6)
  CALL STRASSEN_MATMUL (L,A(1:L,L+1:N)-A(L+1:N,L+1:N), &
                          B(L+1:N,1:L)+B(L+1:N,L+1:N), P7)
 
  C(1:L,1:L)    =P1+P4-P5+P7
  C(1:L,L+1:N)  =P3+P5
  C(L+1:N,1:L)  =P2+P4
  C(L+1:N,L+1:N)=P1+P3-P2+P6
 
END IF
 
END SUBROUTINE STRASSEN_MATMUL
 
 
END PROGRAM TEST_STRASSEN_MATMUL
