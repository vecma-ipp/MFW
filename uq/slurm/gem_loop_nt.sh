#!/bin/bash -l

#SBATCH --job-name=gem_loop_notransp

#SBATCH --output=gem-loop-out.%j
#SBATCH --error=gem-loop-err.%j

#SBATCH --time=24:00:00

#SBATCH --nodes=1
#SBATCH --tasks-per-node=48

#SBATCH --partition=skl_fua_prod

#SBATCH --account=FUA35_UQMWA3
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

export SYS=MARCONI
export SCRATCH=$CINECA_SCRATCH
export MPICMD=srun
export LD_LIBRARY_PATH=${FFTW_HOME}/lib:${LD_LIBRARY_PATH}

ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES
export EASYPJ_CONFIG=conf.sh

#cd $SYS
#rm loop_gem_notransp
#cd ../../..
#make standalone
#cd standalone/bin

echo '> Running 1ft original AUG case with 50 internasl tst; TFILE and other files from cpo45, after totally 24671 calls; starting ti_transp.flux should be 2.83e6'

srun -n 8 MARCONI/loop_gem_notransp > gem-loop-log.${SLURM_JOBID}

