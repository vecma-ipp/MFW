#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=23:30:00

## number of nodes and tasks per node
# order=3, n_params=4, n_subd=8 -> 1024 across 40(80?) cpn -> 27 nodes
###SBATCH --nodes=27 # MIND number of parameters in variation in the script
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

module load anaconda/3/2021.11 intel/21.5.0 impi/2021.5 mkl/2020.1 fftw-mpi/3.3.10

conda activate python394

export SYS=COBRA
export SCRATCH=$SCRATCH

#export PYTHONPATH=/u/yyudin/codes/ual_python_interface:$PYTHONPATH
#export PYTHONPATH=/u/yyudin/codes/MFW/uq:$PYTHONPATH

export MPICMD=mpiexec #srun #mpirun #intelmpi
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

# Define some global variables to configure UQ software
export MPIMOD=srunmpi #default

echo -e '> In this run: use ExecuteLocal only + QCGPJ pool + '$MPIMOD' exec mode + '$SLURM_NNODES' nodes + 4 params + commandline passed with '$MPICMD' /n'

# Run the UQ code

# Echo SLURM environmental variables
#scontrol show --detail job $SLURM_JOBID 

python3 tests/gem_notransp.py > test-loopntuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"

