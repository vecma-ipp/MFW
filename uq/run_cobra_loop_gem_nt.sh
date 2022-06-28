#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=0:30:00
###23:30:00 #CHANGED FOR DEBUGGING!

## number of nodes and tasks per node
# order=3, n_params=4, n_subd=8 -> 1024 across 40 (80 for hthreading, not used) cpus -> 27 nodes
###SBATCH --nodes=27 # MIND number of parameters in variation in the script
#SBATCH --nodes=2 #4
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --mem=96000

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

######################################
# Loading modules
module load anaconda/3/2021.11 intel/21.5.0 impi/2021.5 mkl/2020.1 fftw-mpi/3.3.10

# Python set-up
#conda activate python394
source activate $HOME/conda-envs/python394

export SYS=COBRA
export SCRATCH=$SCRATCH

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

export NCORESPTASK=8

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '$MPIMOD' exec mode + '$SLURM_NNODES' nodes + 4 params + commandline passed with '$MPICMD' /n'

####################################
# Run the UQ code

# Echo SLURM environmental variables
#scontrol show --detail job $SLURM_JOBID 

python3 tests/gem_notransp.py > test-loopntuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"
