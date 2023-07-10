#!/bin/bash -l

#SBATCH --job-name=gem_loop_nt_8ft

#SBATCH --output=gem-loop-out.%j
#SBATCH --error=gem-loop-err.%j

#SBATCH --time=12:00:00

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --ntasks-per-core=1

#SBATCH --partition=medium

###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yehor.yudin@ipp.mpg.de

export SYS=COBRA
export SCRATCH=/ptmp/yyudin/
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

echo '> Running 8ft original AUG case with 50 internal time steps'

${MPICMD} -n 32 -N 1 loop_gem_notransp > gem-loop-log.${SLURM_JOBID}
