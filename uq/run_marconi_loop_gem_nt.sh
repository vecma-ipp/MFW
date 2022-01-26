#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=23:30:00

## number of nodes and tasks per node
#SBATCH --nodes=2 # MIND number of parameters in variation in the script
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

module load intel/pe-xe-2017--binary intelmpi/2017--binary

source $HOME/python394/bin/activate

export SYS=MARCONI
export SCRATCH=$CINECA_SCRATCH
export PYTHONPATH=/marconi/home/userexternal/yyudin00/code/ual_python_interface:$PYTHONPATH

#export MPICMD=mpiexec
export MPICMD=mpiexec #mpirun #intelmpi #srun
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

#try out for script with 'mpirun' execution model
export I_MPI_HYDRA_BOOTSTRAP_EXEC_EXTRA_ARGS="--exclusive"

echo '> In this run: use ExecuteLocal only + QCGPJ pool + default exec mode + commandline passed + 3 nodes + 4 params + mpiexec '
echo '' # \n should work?

echo $SLURM_NODELIST
echo $SLURM_TASKS_PER_NODE

# writing a file with description of used modules
#TODO: this works from CLI on login node, but not from SLURM while on a working one; figure out why
module list > module-used.${SLURM_JOBID}

# Run the UQ code
scontrol show --detail job $SLURM_JOBID 
###scontrol show hostname $SLURM_JOB_NODELIST
#lscpu -p

python3 tests/gem_notransp.py > test-loopntuq-log.${SLURM_JOBID}

