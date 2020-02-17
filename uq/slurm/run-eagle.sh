#!/bin/bash

## job name
#SBATCH --job-name=mfw_uq

## stdout file
#SBATCH --output=log-out.%j

## stderr file
#SBATCH --error=log-err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=01:00:00

## number of nodes
#SBATCH --nodes=1

## tasks per node
#SBATCH --tasks-per-node=28

## queue name
#SBATCH --partition=fast

## grant
#SBATCH --account=vecma2020
#SBATCH --mail-user=jalal.lakhlili@ipp.mpg.de

export SYS=EAGLE
export MPICMD=mpirun

export MODULEPATH=$MODULEPATH:/home/plgrid-groups/plggvecma/.qcg-modules

# TO CHANGE
export SCRATCH=$PLG_USER_SCRATCH/plgljala
export PYTHONPATH=$PYTHONPATH:$HOME/workspace/mfw/ual/usr

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

module load python/3.7.3
module load ifort
module load impi
module load fftw
module unload gcc
module unload gmp

# Run the UQ code
python3.7 tests/loop_src_pj.py > log-trace.${SLURM_JOBID}
