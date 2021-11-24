#!/bin/bash -l

#SBATCH --job-name=gem_loop_nt_mft

#SBATCH --output=gem-loop-out.%j
#SBATCH --error=gem-loop-err.%j

#SBATCH --time=24:00:00

#SBATCH --nodes=2
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

echo '> Running 8ft original AUG case with 50 internasl tst; using t0*.dat files after 9 run of '$((3988+500))'? calls'

srun -n 64 -N 2 MARCONI/loop_gem_notransp > gem-loop-log.${SLURM_JOBID}

