#!/bin/bash -l

## job name
#SBATCH --job-name=ets_uq

## stdout and stderr files
#SBATCH --output=outputs/logs/test.out.%j
#SBATCH --error=outputs/logs/test.err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=02:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --tasks-per-node=40

## queue name
#SBATCH --partition=medium

## grant
#SBATCH --mail-user=ljala@rzg.mpg.de

# Run the program in uq folder
python boundary_conditions.py > outputs/logs/test.log.${SLURM_JOBID}
