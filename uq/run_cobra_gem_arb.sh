#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT_ARB_

## stdout and stderr files
#SBATCH --output=test-arbuq-out.%j
#SBATCH --error=test-arbuq-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=24:00:00

## number of nodes and tasks per node
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

###################################################

source activate ${HOME}/conda-envs/python394

export SYS=COBRA
export SCRATCH=${SCRATCH}

# MPI programs starter, MPCDF recommends using 'srun' only at COBRA!
export MPICMD=srun #mpiexec #mpirun #intelmpi

export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

# Define some global variables to configure UQ software
export MPIMOD=default #srunmpi

#export NCORESPTASK=2

######################################################

if [ -n "${CAMP_NAME_PREFIX}" ]; then
    export CAMP_NAME_PREFIX=VARY_1FT_GEM_
fi

if [ -z "${POLORDER}" ]; then
    export POLORDER=2
fi

export RUNRANGE=6

export PARAM_FILE_NAME=gem_uq_new_param_vals_al_2.csv

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '${MPIMOD}' exec mode + '${SLURM_NNODES} \
' nodes + 4 params + '${RUNRANGE}' values + commandline passed with '${MPICMD}' \n'

#echo '> Here we take larger ti_ddrho and intermediate ti error at rho_tor=0.7 \n'
echo '> Here we take points suggested by GPR for GEM to yield Qi~=2e6 at rho_tor=0.7 \n'

####################################
# Run the UQ code

# Echo SLURM environmental variables
scontrol show --detail job ${SLURM_JOBID}

python3 tests/gem_arbitrary.py > test-arbuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"
scontrol show --detail job ${SLURM_JOBID}
