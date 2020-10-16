#!/bin/bash -l

## job name
#SBATCH --job-name=GPR_SURR_T

## stdout and stderr files
#SBATCH --output=test-out.%j.gpr
#SBATCH --error=test-err.%j.gpr

## wall time in format MINUTES:SECONDS
#SBATCH --time=1:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=1

###SBATCH --partition=medium
#SBATCH --partition=skl_fua_prod
###SBATCH --qos=

## grant
#SBATCH --account=FUA34_UQMWA2
#SBATCH --mail-type=END
#SBATCH --mail-user=yyudin@ipp.mpg.de

export SYS=MARCONI
export SCRATCH=$CINECA_SCRATCH
export MPICMD=srun
export PYTHONPATH=$HOME/.local/lib/python3.6/site-packages/easyvvuq:$HOME/code/ual_python_interface

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

# Run the UQ code
scontrol show --detail job $SLURM_JOBID
python3 toy_surrogate.py > test-log.${SLURM_JOBID}.gpr
