#!/bin/bash
#SBATCH --time=6:00:00              # time limits
#SBATCH --nodes=54                  # nodes
#SBATCH --ntasks-per-node=48         # tasks per node
#SBATCH --partition=skl_fua_prod     # partition to be used
#SBATCH --job-name=MFW               # job name
#SBATCH --err=gem8ft.%j.err             # std-error file
#SBATCH --out=gem8ft.%j.out             # std-output file
#SBATCH --account=FUA34_UQMWA2 			 # account number
#SBATCH --mail-type=ALL				       # specify email notification
#SBATCH --mail-user=ljala@ipp.mpg.de # e-mail address

export SYS=MARCONI
export MPICMD="intelmpi"
export SCRATCH=$CINECA_SCRATCH 

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

module load intel/pe-xe-2017--binary
module load intelmpi/2017--binary

python3 tests/gem_multi_ft.py > gem8ft.${SLURM_JOBID}.log
