#!/bin/bash -l

## job name
#SBATCH --job-name=UQ_8FTGEM_

## stdout and stderr files
#SBATCH --output=test-aleatoric-out.%j
#SBATCH --error=test-aleatoric-err.%j

#SBATCH --no-requeue

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=5:00:00

## number of nodes and tasks per node
# next line: for running only the first slow flux tube
#SBATCH --nodes=5
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8?

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

####################################

#module load anaconda/3/2021.11 intel/21.5.0 mkl impi/2021.5 fftw-mpi/3.3.10

module load anaconda/3/2021.11

#source activate ${HOME}/conda-envs/python3114

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

# if [ -z "${NSAMPLES}" ]; then
#     export NSAMPLES=32
# fi

# if [ -z "${POLORDER}" ]; then
#     export POLORDER=2
# fi

# if [ -z "${CAMP_NAME_PREFIX}" ]; then
#     export CAMP_NAME_PREFIX=UQ_8FTGEM0_
# fi

####################################
# Run the UQ code - scan

CODENAME=gem0

#INPUTCOVLIST=( 0.01 0.05 ) # 0.1 0.2 0.25 0.5 ) # ( 0.3 0.5 )
INPUTCOV=0.01

NSAMPLESLIST=( 64 )

#TODO completely parallelisable!

#for INPUTCOV in ${INPUTCOVLIST[@]}; do
for NSAMPLES in ${NSAMPLESLIST[@]}; do

    echo -e '> For each of these scan runs: use ExecuteLocal only + QCGPJ pool + '${MPIMOD}' exec mode + '${SLURM_NNODES}\
    ' nodes + 2 QoIs + 8 flux tubes + n_samples '${NSAMPLES}' + commandline passed with '${MPICMD}' \n'

    # Echo SLURM environmental variables
    scontrol show --detail job ${SLURM_JOBID}

    echo "> Starting a UQ SLURM job for CoV[Q]="${INPUTCOV}

    python3 tests/evvuq_aleatoric_propagation_wf.py ${INPUTCOV} ${NSAMPLES} ${CODENAME} >> test-aleatoric-log.${SLURM_JOBID}

    echo "> Finished a UQ SLURM for CoV[Q]="${INPUTCOV}

done

# Run postprocessing?
