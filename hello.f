      program hello_world
      include 'mpif.h'
      integer ierr, num_procs, my_id
      real :: start, finish

      
      call MPI_INIT ( ierr )
      call CPU_TIME(start)
c     find out MY process ID, and how many processes were started.

      call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
      call MPI_COMM_SIZE (MPI_COMM_WORLD, num_procs, ierr)

      print *, "Hello world! I'm process ", my_id, " out of ",
     &      num_procs, " processes."

      call MPI_FINALIZE ( ierr )
      call CPU_TIME(finish)
      print*,'Run time: ',finish - start, 'seconds.'
      
      

      stop
      end
