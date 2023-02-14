#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_8FT

## stdout and stderr files
#SBATCH --output=test-uq8ft-out.%j
#SBATCH --error=test-uq8ft-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=24:00:00

## number of nodes and tasks per node
# order=3, n_params=4, n_subd=8, n_ft=8 -> 8192  across 40 (80 for hthreading, not used) cpus 
# -> 216 nodes
#SBATCH --nodes=220
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --partition=medium
###SBATCH --qos=

#SBATCH -vvvvv
#SBATCH --profile=all
###SBATCH --oversubscribe

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

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

if [ -z "${POLORDER}" ]; then
    export POLORDER=2
fi

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '${MPIMOD}' exec mode + '${SLURM_NNODES} \
' nodes + 4 params + pol-order '${POLORDER}'+ 8 flux tubes + commandline passed with '${MPICMD}' \n'

echo '> Here we take all 4 params +/- 25% error at rho_tor=[...] \n'

####################################
# Run the UQ code

# Echo SLURM environmental variables
scontrol show --detail job ${SLURM_JOBID}


python3 tests/gem_multi_ft.py > test-uq8ft-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"
scontrol show --detail job ${SLURM_JOBID}
