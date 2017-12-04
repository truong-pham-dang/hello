
! This illustrate the use of array expressions, array assignments,
! allocatable arrays, automatic arrays, and array sections. The module 
! LINEAR contains a subroutine for solving a set of linear equations, 
! and this is called from a main program that prompts the user for the
! problem and then solves it.

! It comes from Matcalf and Reid, end of Chapter 6.

MODULE LINEAR
   INTEGER, PARAMETER :: KIND = SELECTED_REAL_KIND(10)

CONTAINS
   SUBROUTINE SOLVE(A, PIV_TOL, B, OK)
   ! Arguments
      REAL(KIND), INTENT(INOUT) :: A(:,:)     ! The matrix A
      REAL(KIND), INTENT(IN)    :: PIV_TOL    ! Smallest acceptable pivot.
      REAL(KIND), INTENT(INOUT) :: B(:)
       ! The right-hand side vector on entry. Overwritten by the solution.
      LOGICAL, INTENT(OUT)      :: OK
                      ! True after a successful entry and false otherwise.

   ! Local variables
      INTEGER I               ! Row index.
      INTEGER J               ! Column index.
      INTEGER N               ! Matrix order.
      REAL(KIND) ROW(SIZE(B)) ! Automatic array needed for workspace;
                              ! SIZE is described in Section 8.12.2.
      REAL(KIND) ELEMENT      ! Workspace variable.

      N = SIZE(B)
      OK = SIZE(A, 1) == N .AND. SIZE(A, 2) == N
      IF (.NOT.OK) RETURN

      DO J = 1, N

!     Update elements in column J.
         DO I = 1, J - 1
             A(I+1:N, J) = A(I+1:N, J) - A(I,J) * A(I+1:N, I)
         END DO

!     Find pivot and check its size (using MAXVAL just to obtain a scalar)
         I = MAXVAL(MAXLOC(ABS(A(J:N, J)))) + J - 1
                  ! MAXVAL and MAXLOC are in Sections 8.11.1 and 8.14
         IF (ABS(A(I, J)) < PIV_TOL) THEN
            OK = .FALSE.
            RETURN
         END IF
!     If necessary, apply row interchange
         IF (I.NE.J) THEN
            ROW = A(J, :); A(J, :) = A(I, :); A(I, :) = ROW
            ELEMENT = B(J); B(J) = B(I); B(I) = ELEMENT
         END IF

!     Compute elements J+1 : N of J-th column.
         A(J+1:N, J) = A(J+1:N, J)/A(J, J)
      END DO

!  Forward substitution
      DO I = 1, N-1
          B(I+1:N) = B(I+1:N) - B(I)*A(I+1:N, I)
      END DO

!   Back-substitution
      DO J = N, 1, -1
         B(J) = B(J)/A(J, J)
         B(1:J-1) =  B(1:J-1) - B(J)*A(1:J-1, J)
      END DO
   END SUBROUTINE SOLVE
END MODULE LINEAR

PROGRAM MAIN
   USE LINEAR
   INTEGER I, N
   REAL(KIND), ALLOCATABLE :: A(:, :), B(:)
   LOGICAL OK

   WRITE(*, *) ' Matrix order?'
   READ(*, *) N
   ALLOCATE ( A(N, N), B(N) )
   DO I = 1, N
      WRITE(*,'(A, I2, A)') ' Elements of row ', I, ' of A?'
      READ(*,*) A(I,:)
      WRITE(*,'(A, I2, A)') ' Component ', I, ' of B?'
      READ(*,*) B(I)
   END DO

   CALL SOLVE(A, MAXVAL(ABS(A))*1E-10, B, OK)
   IF (OK) THEN
      WRITE(*, '(/A/ (5F12.4))') ' Solution is', B
                      ! Edit descriptors are described in Section 9.13
   ELSE
      WRITE(*, *) ' The matrix is singular'
   END IF
END PROGRAM MAIN
 
