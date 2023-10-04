#!/bin/sh

# Bash script to run pos-processing routines from gem_da.py on outputs of a code, here on transport CPO files from GEM

# NOTE: never run this for a campaing folder currently used by a code!!!

# Example usage: ./gem_postproc_vary_test.sh 12 moj202gj 0 1 1 16 0

#0. Set directories
#  folder of output CPO files, should be same as number of SLURM submissions (macro-macro-iterations)
#  should be the same as number of MMit TO process
# Get the number of 'mactoiteration'
CPONUM=${1:-1}

#UQCAMPDIR='dy6n5hp9' # folder id of a completed run with 100 GEM calls, and 11 series of runs 100 calls each
#UQCAMPDIR='moj202gj' #folder ID of a completed run with 450 GEM calls
# Get the name of the UQ campaign
UQCAMPDIR=${2:-'aos1mzke'} #'brus48mm' #'1wu9k2wa'

# True if all simulation files need to be copied out of the working directory
RUN_WITH_CP=${3:-1}

# True if all the postprocessing files should be stored in data directory
RUN_WITH_SAVE=${4:-1}

# Number of runs in current UQ campaign
RUNRANGESTART=${5:-1}
RUNRANGE=${6:-4} #16

# True if first the script should be run for the last 'macroiteration'
RUN_NOT_ONLY_ALL=${7:-1}

# True if the simulation data has to be read from .CSV files
READ_FROM_CSV=${8:-1}

#DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qairnbbz' # first run of 16 GEM cases in a script, n_it<=500
#DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qpyxg3bb' # first dir with 2 GEM runs
if [ -n "${CAMP_NAME_PREFIX}" ]; then
  export DIR_PREFIX=${CAMP_NAME_PREFIX}
else
  #export DIR_PREFIX='VARY_1FT_GEM_NT_' #'VARY_1FT_GEM_'
  export DIR_PREFIX=UQ_8FTgem_
fi
DIR=${SCRATCH}'/'${DIR_PREFIX}${UQCAMPDIR}

#DIR_SRC=${DIR}'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/'
DIR_SRC=${DIR}'/runs/'
RUNPATHTOP=runs_0-100000000/

CODEMDIR=code
if [[ "${SYS}" =~ ^(DRACO|COBRA|RAVEN)$ ]]; then
  CODEMDIR=code
else
  CODEMDIR=code
fi

DIR_CODE=${HOME}'/'${CODEMDIR}'/MFW/uq/basicda/'

DIR_OUTPUT='gem_data'

#1. Transfer output files from the run directories to a separate cpo dir

if [ "${RUN_WITH_CP}" -eq 1 ]; then

  cd ${DIR_SRC}
  mkdir cpo
  mkdir dat

  mkdir cpo/${CPONUM}
  mkdir dat/${CPONUM}

  echo "copying CPOs from the latest run dirs"
  #echo "RUN_WITH_CP="${RUN_WITH_CP}

  #for d in run*/ ; do
  for r in $(find ${RUNPATHTOP} -maxdepth 4 -mindepth 4 -type d -name "run_*" | sed "s|^\.\/||"); do 
      
      #echo "${r}"

      mkdir -p cpo/${CPONUM}/${r}

      ## this should either be a slower 'cp' or never run for a folder currently used by a code
      ## also never to be thought it's current CPO-s in the stated folder
      
      cp ${r}/gem_coretransp*.cpo cpo/${CPONUM}/${r}/

      mkdir -p dat/${CPONUM}/${r}
      cp ${r}/t*.dat dat/${CPONUM}/${r}/
      cp ${r}/h*.dat dat/${CPONUM}/${r}/
      cp ${r}/d*.dat dat/${CPONUM}/${r}/
      cp ${r}/fout_* dat/${CPONUM}/${r}/
      cp ${r}/stopped dat/${CPONUM}/${r}/

  done

  cd ${DIR}
  cp campaign.db ${DIR_CODE}/campaign_${UQCAMPDIR}_${CPONUM}.db
  #TODO: apparently after the gem_da.py is run the copied DB is corrupted, so this either has to be fixed or a new copy should be created

fi

#2. Run prosprocessing scripts
cd ${DIR_CODE}

CPONUMPR=$((${CPONUM}-1))

#export PYTHONPATH=/marconi/home/userexternal/yyudin00/code/ual_python_interface:/marconi/home/userexternal/yyudin00/code/MFW/uq/base:${PYTHONPATH}

export PYTHONPATH=/cobra/u/yyudin/codes/ual_python_interface:/cobra/u/yyudin/codes/MFW/uq:${PYTHONPATH}

# Reads gem_*.cpo -s from 'cpo' in run folder, for all runs (modify structure of script), all flux tubes 

# Command line arguments for main: folder with cpo-s; to read from original files or from csv; number of flux tubes; number of profile variants; file name to save

QUANTITIES=('ti' 'te' 'ni' 'ne')
#QUANTITIES=('ti')

if [ "${RUN_NOT_ONLY_ALL}" -eq 1 ]; then
  
  # first, run postprocessing for new batch

  if [ "${READ_FROM_CSV}" -ne 1 ]; then

    # read values of new batch from a CPO files

    echo RUNNUM=${RUNRANGE}
    python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 0 1 ${RUNRANGESTART} ${RUNRANGE} 'new_'${UQCAMPDIR}'_'${CPONUM}

  else

    for r in `seq ${RUNRANGESTART} ${RUNRANGE}`; do 

      for q in ${QUANTITIES[@]}; do

        cp ${DIR_OUTPUT}/gem_${q}_transp_flux_evol_new_${UQCAMPDIR}_${CPONUM}_${r}.csv ./

      done

    done

    python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 1 1 ${RUNRANGESTART} ${RUNRANGE} 'new_'${UQCAMPDIR}'_'${CPONUM}

  fi

  #3. Prepare combined files for analysis of series across long-term runs
  #cp ${DIR_OUTPUT}/gem_??_transp_flux_evol_all${NUMPR}.csv ./

  for r in `seq ${RUNRANGESTART} ${RUNRANGE}`; do
      
      for q in ${QUANTITIES[@]}; do

        cp ${DIR_OUTPUT}/gem_${q}_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv ./

        cat gem_${q}_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv gem_${q}_transp_flux_evol_new_${UQCAMPDIR}_${CPONUM}_${r}.csv > gem_${q}_transp_flux_evol_all_${UQCAMPDIR}_${CPONUM}_${r}.csv
      
      done

  done

fi

#cat gem_ti_transp_flux_evol_all${NUMPR}.csv gem_ti_transp_flux_evol_${CPONUM}.csv > gem_ti_transp_flux_evol_all${NUM}.csv

#4. Run the post-processing Python script for combined csv files
#python3 gem_da.py 'all'$NUM 1

if [ "${RUN_NOT_ONLY_ALL}" -ne 1 ]; then

    for r in `seq ${RUNRANGESTART} ${RUNRANGE}`; do 

      for q in ${QUANTITIES[@]}; do

        cp ${DIR_OUTPUT}/gem_${q}_transp_flux_evol_all_${UQCAMPDIR}_${CPONUM}_${r}.csv ./

      done

    done

fi

#python3 gem_da.py all_${UQCAMPDIR}_${CPONUM} 1 1 ${RUNRANGE} all_${UQCAMPDIR}_${CPONUM}
python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 1 1 ${RUNRANGESTART} ${RUNRANGE} all_${UQCAMPDIR}_${CPONUM}

#5. Put the resulting output files in a separate directory
if [ "${RUN_WITH_SAVE}" -eq 1 ]; then

  cd ${DIR_OUTPUT}
  mkdir ${UQCAMPDIR}

mv ../*${UQCAMPDIR}*.pdf ${UQCAMPDIR}/
  mv ../*${UQCAMPDIR}*.png ${UQCAMPDIR}/
  mv ../*${UQCAMPDIR}*.svg ${UQCAMPDIR}/
  mv ../*${UQCAMPDIR}*.txt ${UQCAMPDIR}/
  mv ../resuq*${UQCAMPDIR}*.csv ${UQCAMPDIR}/
  mv ../stat*${UQCAMPDIR}*.csv ${UQCAMPDIR}/
  cp ../gem*${UQCAMPDIR}*.csv ${UQCAMPDIR}/
  
  mv ../gem*${UQCAMPDIR}*.csv ./

  # Sort files into folder by quantity, number of run, type of file -> make sure the file is in folder and the ranges are set up!
  cp ../sort_files.sh ${UQCAMPDIR}/
  cd ${UQCAMPDIR}
  ./sort_files.sh
  #TODO: sort_files.sh copies flux time traces CSV files with wrong campaign names

fi
