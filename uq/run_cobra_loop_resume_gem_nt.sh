#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=9:00:00

## number of nodes and tasks per node
# order=3, n_params=4, n_subd=8 -> 1024 across 40 (80 for hthreading, not used) cpus -> 27 nodes
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --partition=medium
###SBATCH --qos=

###SBATCH -vvvvv
###SBATCH --profile=all

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

####################################

#module load anaconda/3/2021.11 intel/21.5.0 mkl impi/2021.5 fftw-mpi/3.3.10

source activate ${HOME}/conda-envs/python394

export SYS=COBRA
export SCRATCH=${SCRATCH}

# PYTHONPATH ?

# MPI programs starter, MPCDF recommends using 'srun' only at COBRA!
export MPICMD=srun #mpiexec #mpirun #intelmpi

export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

# Define some global variables to configure UQ software
export MPIMOD=default #srunmpi

echo '> CPONUM and OLDCAMP are: '
echo ${CPONUM}
echo ${OLDCAMP}

export OLDCAMP=${1:-'aos1mzke'} #'brus48mm' #'1wu9k2wa'

export POLORDER=3

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '${MPIMOD}' exec mode + '${SLURM_NNODES} \
' nodes + 1 param + pol-order '${POLORDER}' + commandline passed with '${MPICMD}' \n'

####################################
# Run the UQ code

# Echo SLURM environmental variables
scontrol show --detail job ${SLURM_JOBID}

python3 tests/gem_nt_resume.py ${OLDCAMP}> test-loopntuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"

# Run postprocessing?
