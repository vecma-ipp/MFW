#!/bin/bash -l

## job name
#SBATCH --job-name=UQ_8FTgem_

## stdout and stderr files
#SBATCH --output=test-loopmftuq-out.%j
#SBATCH --error=test-loopmftuq-err.%j

#SBATCH --no-requeue

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=24:00:00

## number of nodes and tasks per node
###SBATCH --nodes=27
# next line: for running only the first slow flux tube
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

echo '> CPONUM, OLDCAMP, RUNRANGE and are: '
echo ${CPONUM}
echo ${OLDCAMP}
echo ${RUNRANGE}
echo ${CAMP_NAME_PREFIX}

#export OLDCAMP=${1:-'aos1mzke'} #'brus48mm' #'1wu9k2wa'

if [ -z "${POLORDER}" ]; then
    export POLORDER=2
fi

if [ -z "${CPONUM}" ]; then
    export CPONUM=1
fi

if [ -z "${RUNRANGE}" ]; then
    export RUNRANGE=81
fi

if [ -z "${CAMP_NAME_PREFIX}" ]; then
    export CAMP_NAME_PREFIX=UQ_8FTgem_
fi

if [ -z "${OLDCAMP}" ]; then
    #export OLDCAMP=22_2pb5u
    export OLDCAMP=csldvnei
fi

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '${MPIMOD}' exec mode + '${SLURM_NNODES}\
' nodes + 4 param + pol-order '${POLORDER}' + commandline passed with '${MPICMD}' \n'

####################################
# Run the UQ code

# Echo SLURM environmental variables
scontrol show --detail job ${SLURM_JOBID}

python3 tests/gem_mft_resume.py ${OLDCAMP}> test-loopmftuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"

# Run postprocessing?
