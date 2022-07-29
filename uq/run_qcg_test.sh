#!/bin/bash -l

## job name
#SBATCH --job-name=QCG_TEST_

## stdout and stderr files
#SBATCH --output=test-qcg-out.%j
#SBATCH --error=test-qcg-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=0:30:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8

#SBATCH --mem=40000

#SBATCH --partition=medium

## grant
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

export MPICMD=srun
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

export MPIMOD=default

# ENV VARIABLES REQUIRED FOR THE QCG-PJ JOB

export QCG_PM_STEP_ID=0
export QCG_PM_EXEC_API_JOB_ID=0
export QCG_PM_NPROCS=8

####################################

#bash -l -c 'exec python3 -m easyvvuq.actions.execute_qcgpj_task /cobra/ptmp/yyudin/VARY_1FT_GEM_NT_h7khf58u/.qcgpj_in_act_6 /cobra/ptmp/yyudin/VARY_1FT_GEM_NT_h7khf58u/.qcgpj_in_prev_6' > test-qcg-log.${SLURM_JOBID}

bash -l -c 'exec python3 -m easyvvuq.actions.execute_qcgpj_task /cobra/ptmp/yyudin/VARY_1FT_GEM_NT_0v_v6zc7/.qcgpj_in_act_1 /cobra/ptmp/yyudin/VARY_1FT_GEM_NT_0v_v6zc7/.qcgpj_in_prev_1' > test-qcg-log.${SLURM_JOBID}
