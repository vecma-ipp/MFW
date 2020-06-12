#!/bin/bash
#SBATCH --time=02:30:00                   
#SBATCH --nodes=2                         
#SBATCH --ntasks-per-node=48              
#SBATCH --partition=skl_fua_prod          
#SBATCH --job-name=TC3_2                    
#SBATCH --err=logs/ets.%j.err             
#SBATCH --out=logs/ets.%j.out             
#SBATCH --account=FUA33_UQMWA 			      
#SBATCH --mail-type=END      
#SBATCH --mail-user=ljala@ipp.mpg.de 

export SYS=MARCONI
export MPICMD=mpirun
export SCRATCH=$CINECA_SCRATCH 

# For QCG-PilotJob usage
ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

python3 tests/ets_pj_3.py > logs/ets.${SLURM_JOBID}.log
