#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <omp.h>
#include <mpi.h>

extern char **environ;


int main(int argc, char **argv) {
        int rank, size;
        char hostname[128];

        MPI_Init(&argc, &argv);
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        MPI_Comm_size(MPI_COMM_WORLD, &size);

        gethostname(hostname, 128);

        printf("rank(%03d): comm size(%d) @ %s available %d threads\n", rank, size, hostname,
                        omp_get_num_procs());

        if (rank == 0) {
                printf("rank(%03d): # of args: %d\n", rank, argc);
                for (int i = 0; i < argc; ++i)
                        printf("rank(%03d): arg[%d]: %s\n", rank, i, argv[i]);
                for (char **env = environ; *env; *env++)
                        printf("rank(%03d): env: %s\n", rank, *env);

        }

        sleep(5);

        MPI_Barrier(MPI_COMM_WORLD);

        MPI_Finalize();

        return 0;
}
