   #include <stdio.h>
   #include <mpi.h>
   #include <time.h>
     
   main(int argc, char **argv)
   {
      int ierr, num_procs, my_id;
      clock_t start, end;
      double time; 

      
      ierr = MPI_Init(&argc, &argv);
      start = clock();

      /* find out MY process ID, and how many processes were started. */

      ierr = MPI_Comm_rank(MPI_COMM_WORLD, &my_id);
      ierr = MPI_Comm_size(MPI_COMM_WORLD, &num_procs);

      printf("Hello world! I'm process %i out of %i processes\n", 
         my_id, num_procs);

      ierr = MPI_Finalize();
      end = clock();
      time = ((double)(end-start))/ CLOCKS_PER_SEC;
      printf("Run time: %f seconds.\n",time);
   }


