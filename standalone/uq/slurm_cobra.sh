#!/bin/bash -l

## job name
#SBATCH --job-name=ets_uq

## stdout and stderr files
#SBATCH --output=logs/test.out.%j
#SBATCH --error=logs/test.err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=10:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --tasks-per-node=40

## queue name
#SBATCH --partition=medium

## grant
#SBATCH --mail-user=ljala@rzg.mpg.de

# Run the program:.
python test_uq1_ets.py > logs/test.log.${SLURM_JOBID}
