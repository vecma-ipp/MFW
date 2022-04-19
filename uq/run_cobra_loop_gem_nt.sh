#!/bin/bash -l

## job name
#SBATCH --job-name=VARY_GEM_1FT

## stdout and stderr files
#SBATCH --output=test-loopntuq-out.%j
#SBATCH --error=test-loopntuq-err.%j

## wall time in format MINUTES:SECONDS
#SBATCH --time=23:30:00

## number of nodes and tasks per node
# order=3, n_params=4, n_subd=8 -> 1024 across 40 cpn -> 
###SBATCH --nodes=26 # MIND number of parameters in variation in the script
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

module load intel/21.5.0 impi/2021.5

#source $HOME/python394/bin/activate
cobra activate python394

export SYS=COBRA
export SCRATCH=$SCRATCH
#export PYTHONPATH=/marconi/home/userexternal/yyudin00/code/ual_python_interface:$PYTHONPATH

#export PYTHONPATH=/u/yyudin/codes/MFW/uq:$PYTHONPATH

export MPICMD=mpiexec #mpirun #intelmpi #srun
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

echo '> In this run: use ExecuteLocal only + QCGPJ pool + default exec mode + commandline passed + 4 nodes + 4 params + mpiexec \n'

# Run the UQ code
#scontrol show --detail job $SLURM_JOBID 

python3 tests/gem_notransp.py > test-loopntuq-log.${SLURM_JOBID}

echo "> Finished an UQ SLURM job!"

