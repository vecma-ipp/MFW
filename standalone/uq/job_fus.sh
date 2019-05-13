#!/bin/bash -l
# Standard output and error:
#SBATCH -o ./test.out.%j
#SBATCH -e ./test.err.%j
# Initial working directory:
#SBATCH -D ./
# Job Name:
#SBATCH -J fus_test
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
#SBATCH --time=3:00:00

# Run the program:
#srun python loop_test.py  > loop_test.log
srun ../bin/DRACO/loop_src_run ../data/AUG_28906_5/BGB_GEM_SPREAD/4FT/ ets_input.nml
