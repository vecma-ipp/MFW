#!/bin/bash
#SBATCH --time=01:00:00                   # time limits
#SBATCH --nodes=1                         # nodes
#SBATCH --ntasks-per-node=1               # tasks per node
###SBATCH --cpus-per-task=1               # CPU per task
###SBATCH --mem=118GB                     # memory (max 180GB/node in skl_fua)
#SBATCH --partition=skl_fua_prod          # partition to be used
###SBATCH --qos=                          # quality of service
#SBATCH --job-name=MFW                    # job name
#SBATCH --err=test-err.%j                 # std-error file
#SBATCH --out=test-out.%j                 # std-output file
#SBATCH --account=FUA33_UQMWA 			      # account number
#SBATCH --mail-type=END				            # specify email notification
#SBATCH --mail-user=ljala@ipp.mpg.de	    # e-mail address

export SYS=MARCONI
export MPICMD=mpirun
export SCRATCH=$CINECA_SCRATCH 

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

python3 tests/loop_combined_pj.py > test-log.${SLURM_JOBID}
