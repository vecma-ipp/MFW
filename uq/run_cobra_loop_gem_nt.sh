#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=12:00:00
###23:30:00 #CHANGED FOR DEBUGGING!

## number of nodes and tasks per node
# order=3, n_params=4, n_subd=8 -> 1024 across 40 (80 for hthreading, not used) cpus -> 27 nodes
###SBATCH --nodes=27 # MIND number of parameters in variation in the script
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#TODO: look for different mem specs
###SBATCH --mem=85000

#TODO: look for different qos specs for debugging -> may be some predefined ones...
#SBATCH --partition=medium
###SBATCH --qos=

#SBATCH -vvvvv
###SBATCH --slurmd-debug=3
#SBATCH --profile=all
###SBATCH --oversubscribe

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

source activate ${HOME}/conda-envs/python394

export SYS=COBRA
export SCRATCH=${SCRATCH}

#export PYTHONPATH=/u/yyudin/code/ual_python_interface:$PYTHONPATH
#export PYTHONPATH=/u/yyudin/code/MFW/uq:$PYTHONPATH

# MPI programs starter, MPCDF recommends using 'srun' only at COBRA!
export MPICMD=srun #mpiexec #mpirun #intelmpi

export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

# Trying to enable srun 
#export I_MPI_HYDRA_BOOTSTRAP_EXEC_EXTRA_ARGS="--exclusive"

# Define some global variables to configure UQ software
export MPIMOD=default #srunmpi
export EXECTEMPL=hydra_exclusive #short

#export NCORESPTASK=2

# TRYING TO DEBUG MPI_Cart_create in GEM called from QCG-PJ job
#export KMP_STACKSIZE=500000000
#ulimit -s unlimited

export POLORDER=3

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '${MPIMOD}' exec mode + '${SLURM_NNODES} \
' nodes + 1 params + pol-order '${POLORDER}' + commandline passed with '${MPICMD}' \n'

echo '> Here we take gradTi +/- 25% error at rho_tor=0.7 \n'

####################################
# Run the UQ code

# Echo SLURM environmental variables
scontrol show --detail job ${SLURM_JOBID}

#scontrol setdebug 3
#scontrol schedloglevel 1

python3 tests/gem_notransp.py > test-loopntuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"
scontrol show --detail job ${SLURM_JOBID}
