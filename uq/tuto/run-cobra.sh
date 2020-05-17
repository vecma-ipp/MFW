#!/bin/bash -l

## job name
#SBATCH --job-name=jet_uq

## stdout and stderr files
#SBATCH --output=pj-%j.out
#SBATCH --error=pj-%j.err

## wall time in format MINUTES:SECONDS
#SBATCH --time=02:00:00

## number of nodes and tasks per node
#SBATCH --nodes=2
#SBATCH --tasks-per-node=40

## queue name
#SBATCH --partition=medium

## grant
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jalal.lakhlili@ipp.mpg.de

export SYS=COBRA
export SCRATCH=/ptmp/ljala/

# to import jet
export PYTHONPATH=$PYTHONPATH:$PWD

# Run the UQ code
python jet_uq.py > pj-${SLURM_JOBID}.log
