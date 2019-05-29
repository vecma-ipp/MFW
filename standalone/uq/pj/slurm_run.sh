#!/bin/bash -l
# Standard output and error:
#SBATCH -o logs/test.out.%j
#SBATCH -e logs/test.err.%j
# Initial working directory:
#SBATCH -D ./
# Job Name:
#SBATCH -J fus_test
# Queue (Partition):
#SBATCH --partition=general
# Number of nodes and MPI tasks per node:
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#
#SBATCH --mail-type=none
#SBATCH --mail-user=ljala@rzg.mpg.de
#
# Wall clock limit:
#SBATCH --time=5:00:00

# Run the program:
python ets_pj_test.py
