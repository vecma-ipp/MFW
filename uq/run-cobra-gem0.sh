#!/bin/bash -l

## job name
#SBATCH --job-name=UQ_GEM0

## stdout and stderr files
#SBATCH --output=test-out.%j
#SBATCH --error=test-err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=0:30:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=1

## queue name
#SBATCH --partition=medium

## grant
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

export SYS=COBRA
export SCRATCH=/ptmp/yyudin/
export MPICMD=srun
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

# Run the UQ code
scontrol show --detail job $SLURM_JOBID
python3 tests/gem0.py > test-log.${SLURM_JOBID}
