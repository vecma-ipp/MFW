#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=22:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

###SBATCH --partition=medium
#SBATCH --partition=skl_fua_prod
###SBATCH --qos=

## grant
#SBATCH --account=FUA35_UQMWA3
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

export SYS=MARCONI
export SCRATCH=$CINECA_SCRATCH
#export PYTHONPATH=$HOME/.local/lib/python3.6/site-packages/easyvvuq:$PYTHONPATH

#export MPICMD=mpiexec
export MPICMD=srun
###export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

echo $SLURM_NODELIST
echo $SLURM_TASKS_PER_NODE

# Run the UQ code
scontrol show --detail job $SLURM_JOBID 
###scontrol show hostname $SLURM_JOB_NODELIST
lscpu -p

python3 tests/gem_notransp.py > test-loopntuq-log.${SLURM_JOBID}

