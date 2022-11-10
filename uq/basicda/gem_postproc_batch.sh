#!/bin/bash -l

## job name
#SBATCH --job-name=POSTPROCESS_GEM_

## stdout and stderr files
#SBATCH --output=postprocess-out.%j
#SBATCH --error=postprocess-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=24:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --partition=medium
###SBATCH --qos=

#SBATCH --profile=all
###SBATCH --oversubscribe

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

#############################################

source activate ${HOME}/conda-envs/python394
export SYS=COBRA
export SCRATCH=${SCRATCH}

echo 'Starts running postprocessing in batch'

export CAMP_NAME=akgbbn1a
export NUM_RUNS=81
export FRST_CPO=1
export LAST_CPO=9

for r in `seq ${FRST_CPO} ${LAST_CPO}`; do

  echo 'Running postprocessing for '${CAMP_NAME}' and submission #'${r}

  ./gem_postproc_vary_test.sh ${r} ${CAMP_NAME} 0 1 ${NUM_RUNS} 1 0 &> log_${CAMP_NAME}_${r}.log

  echo 'Finished postprocessing for '${CAMP_NAME}' and submission #'${r}

done

echo 'Finished running postprocessing in batch'

