#!/bin/bash -l

## job name
#SBATCH --job-name=easyvvuq_pj

## stdout and stderr files
#SBATCH --output=test.out.%j
#SBATCH --error=test.err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=06:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --tasks-per-node=40

## queue name
#SBATCH --partition=medium

## grant
#SBATCH --mail-user=ljala@rzg.mpg.de

# Run the program:.
python ets_test.py > test.log.${SLURM_JOBID}
