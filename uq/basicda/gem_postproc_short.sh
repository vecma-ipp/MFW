#!/bin/sh

# Bash script to run pos-processing routines from gem_da.py on outputs of a code, here on transport CPO files from GEM

#0. Set directories
# folder of output CPO files, should be same as number of SLURM submissions (macro-macro-iterations)
# should be the same as number of MMit TO process
CPONUM=${1:-12}

# number of runs in current UQ campaign
RUNRANGE=16

UQCAMPDIR='moj202gj' #folder ID of a completed run with 450 GEM calls

DIR=$SCRATCH'/VARY_1FT_GEM_NT_'$UQCAMPDIR

DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/'

DIR_CODE=$HOME'/code/MFW/uq/basicda/'

#1. Transfer output files from the run directories to a separete cpo dir

#cd ${DIR_SRC}
#mkdir cpo
#mkdir dat

#mkdir cpo/${CPONUM}
#mkdir dat/${CPONUM}

#for d in run*/ ; do
#    echo "${d}"
#
#    mkdir cpo/${CPONUM}/${d}
#    mv ${d}/gem_coretransp*.cpo cpo/${CPONUM}/${d}/
#
#    mkdir dat/${CPONUM}/${d}
#    cp ${d}/*.dat dat/${CPONUM}/${d}/
#    cp ${d}/fout_* dat/${CPONUM}/${d}/
#done

#2. Run prosprocessing scripts
cd ${DIR_CODE}

CPONUMPR=$((CPONUM-1))

# command line arguments for main: folder with cpo-s; to read from original files or from csv; number of flux tubes; number of profile variants; file name to save

### NEXT LINES ARE FOR DEBUG

export PYTHONPATH=/marconi/home/userexternal/yyudin00/code/ual_python_interface:/marconi/home/userexternal/yyudin00/code/MFW/uq/base:${PYTHONPATH}

#python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 0 1 ${RUNRANGE} debug_${UQCAMPDIR}_${CPONUM}
#python3 gem_da.py debug_${UQCAMPDIR}_${CPONUM} 1 1 ${RUNRANGE} debug_${UQCAMPDIR}_${CPONUM}

#3. Prepare combined files for analysis of series across long-term runs

#for r in `seq 1 $RUNRANGE`; do
#    cp GEM_plots/gem_??_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv ./
#    cat gem_ti_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv gem_ti_transp_flux_evol_new_${UQCAMPDIR}_${CPONUM}_${r}.csv > gem_ti_transp_flux_evol_all_${UQCAMPDIR}_${CPONUM}_${r}.csv
#done

#4. Run the post-processing Python script for the combined lists

python3 gem_da.py all_${UQCAMPDIR}_${CPONUM} 1 1 ${RUNRANGE} all_${UQCAMPDIR}_${CPONUM}

#5. Put the resulting output files in a separat directory
#mv *.txt GEM_plots/
#mv *.csv GEM_plots/
#mv *.png GEM_plots/

