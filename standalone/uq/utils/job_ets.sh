#!/bin/bash -l
# Standard output and error:
#SBATCH -o ./test.out.%j
#SBATCH -e ./test.err.%j
# Initial working directory:
#SBATCH -D ./
# Job Name:
#SBATCH -J test_h5_ets
# Queue (Partition):
#SBATCH --partition=general
# Number of nodes and MPI tasks per node:
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#
#SBATCH --mail-type=none
#SBATCH --mail-user=ljala@rzg.mpg.de
#
# Wall clock limit:
#SBATCH --time=24:00:00

# Run the program:
srun python h5_ets_in.py  > test.log
