#!/bin/bash -l
# Standard output and error:
#SBATCH -o logs/test.out.%j
#SBATCH -e logs/test.err.%j
# Initial working directory:
#SBATCH -D ./
# Job Name:
#SBATCH -J fus_test
# Queue (Partition):
#SBATCH --partition=medium
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
srun python3 bc_src_test.py  > logs/test.log.${SLURM_JOBID}
