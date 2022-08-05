#!/bin/sh

# Bash script to run pos-processing routines from gem_da.py on outputs of a code, here on transport CPO files from GEM

# NOTE: never run this for a campaing folder currently used by a code!!!

RUN_WITH_CP=${3:-1}

RUN_WITH_SAVE=${4:-1}

#0. Set directories
# folder of output CPO files, should be same as number of SLURM submissions (macro-macro-iterations)
# should be the same as number of MMit TO process
#CPONUM=2
CPONUM=${1:-2}

#RUNNUM=2
# number of runs in current UQ campaign
RUNRANGE=4 #16

#UQCAMPDIR='dy6n5hp9' # folder id of a completed run with 100 GEM calls, and 11 series of runs 100 calls each
# TODO new workflow with all the snapshot solved will have a different directory!
UQCAMPDIR='moj202gj' #folder ID of a completed run with 450 GEM calls
UQCAMPDIR=${2:-'aos1mzke'} #'brus48mm' #'1wu9k2wa'

#DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qairnbbz' # first run of 16 GEM cases in a script, n_it<=500
#DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qpyxg3bb' # first dir with 2 GEM runs
DIR=$SCRATCH'/VARY_1FT_GEM_NT_'$UQCAMPDIR

#DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/run_'$RUNNUM
DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/'

CODEMDIR=code
if [[ "$SYS" =~ ^(DRACO|COBRA|RAVEN)$ ]]; then
  CODEMDIR=code
else
  CODEMDIR=code
fi

DIR_CODE=$HOME'/'$CODEMDIR'/MFW/uq/basicda/'

DIR_OUTPUT='gem_data'

#1. Transfer output files from the run directories to a separate cpo dir

cd ${DIR_SRC}
#mkdir $DIR
mkdir cpo
mkdir dat

mkdir cpo/${CPONUM}
mkdir dat/${CPONUM}

#mv gem-loop*.* $DIR
#mv gem_coretransp*.* cpo

if [ ${RUN_WITH_CP} = 1 ]; then

  echo "copying CPOs from the latest run dirs"
  #echo "RUN_WITH_CP="${RUN_WITH_CP}

  for d in run*/ ; do #latest
      echo "${d}"

      mkdir cpo/${CPONUM}/${d}

      # this should either be a slower 'cp' or never run for a folder currently used by a code
      # alson never to be thought it's current CPO-s in the stated folder
      mv ${d}/gem_coretransp*.cpo cpo/${CPONUM}/${d}/

      mkdir dat/${CPONUM}/${d}
      cp ${d}/*.dat dat/${CPONUM}/${d}/
      cp ${d}/fout_* dat/${CPONUM}/${d}/
  done

  cd ${DIR}
  cp campaign.db ${DIR_CODE}/campaign_${UQCAMPDIR}_${CPONUM}.db

fi

#mv imp4dv_coretransp_0*.cpo $DIR
#mv gem_coretransp_0*.cpo $DIR
#mv fout_0* $DIR
#mv stopped $DIR
#mv *.dat $DIR
#cp $DIR/t00.dat ./

#2. Run prosprocessing scripts
cd ${DIR_CODE}

#NUM=$(($CPONUM-13))
#NUMPR=$((NUM-1))
#CPONUM=$(($NUM+13))
CPONUMPR=$((CPONUM-1))

#export PYTHONPATH=/marconi/home/userexternal/yyudin00/code/ual_python_interface:/marconi/home/userexternal/yyudin00/code/MFW/uq/base:${PYTHONPATH}

export PYTHONPATH=/cobra/u/yyudin/codes/ual_python_interface:/cobra/u/yyudin/codes/MFW/uq:${PYTHONPATH}

#TODO: make sure the script reads right things: gem_*.cpo -s from 'cpo' in run folder, for all runs (mofify structure of script), all flux tubes 

# command line arguments for main: folder with cpo-s; to read from original files or from csv; number of flux tubes; number of profile variants; file name to save

#python3 gem_da.py 'run'$RUNNUM'/cpo'$CPONUM'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM
#python3 gem_da.py $DIR_SRC'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM

if [[ ${RUN_WITH_CP} ]]; then
  python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 0 1 ${RUNRANGE} 'new_'${UQCAMPDIR}'_'${CPONUM} #latest
else
  python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 1 1 ${RUNRANGE} 'new_'${UQCAMPDIR}'_'${CPONUM}
fi

#3. Prepare combined files for analysis of series across long-term runs
#cp ${DIR_OUTPUT}/gem_??_transp_flux_evol_all${NUMPR}.csv ./

QUANTITIES=('ti' 'te' 'ni' 'ne')

for r in `seq 1 $RUNRANGE`; do #latest

    cp ${DIR_OUTPUT}/gem_??_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv ./
    
    for q in ${QUANTITIES[@]}; do

      cat gem_${q}_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv gem_${q}_transp_flux_evol_new_${UQCAMPDIR}_${CPONUM}_${r}.csv > gem_${q}_transp_flux_evol_all_${UQCAMPDIR}_${CPONUM}_${r}.csv
     
    done

done

#cat gem_ti_transp_flux_evol_all${NUMPR}.csv gem_ti_transp_flux_evol_${CPONUM}.csv > gem_ti_transp_flux_evol_all${NUM}.csv

#4. Run the post-processing Python script for combined csv files
#python3 gem_da.py 'all'$NUM 1

python3 gem_da.py all_${UQCAMPDIR}_${CPONUM} 1 1 ${RUNRANGE} all_${UQCAMPDIR}_${CPONUM} #latest

#5. Put the resulting output files in a separate directory
if [ ${RUN_WITH_SAVE} = 1 ]; then
  mv *.txt ${DIR_OUTPUT}/
  mv *.csv ${DIR_OUTPUT}/
  mv *.png ${DIR_OUTPUT}/
fi
