#!/bin/sh

# Bash script to run pos-processing routines from gem_da.py on outputs of a code, here on transport CPO files from GEM

#0. Set directories
# folder of output CPO files, should be same as number of SLURM submissions (macro-macro-iterations)
#CPONUM=2
CPONUM=${1:-2}

#RUNNUM=2
# number of runs in current UQ campaign
RUNRANGE=16

UQCAMPDIR='dy6n5hp9' # folder id of a completed run with 100 GEM calls

DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qairnbbz' # first run of 16 GEM cases in a script, n_it<=500
#DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qpyxg3bb' # first dir with 2 GEM runs
DIR=$SCRATCH'/VARY_1FT_GEM_NT_'$UQCAMPDIR

#DIR='/marconi_scratch/userexternal/yyudin00/MFW_runs/cpo'$CPONUM'/'
#DIR='/marconi_scratch/userexternal/yyudin00/GEM_NT_test/run'$RUNNUM'/cpo'$CPONUM'/cpo'

#DIR_SRC='/marconi_scratch/userexternal/yyudin00/MFW_runs/1ft_stprof/'
#DIR_SRC='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_test/run1/'

#DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/run_'$RUNNUM
DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/'

DIR_CODE=$HOME'/code/MFW/uq/basicda/'

#1. Transfer output files from the run directories to a separete cpo dir

cd $DIR_SRC
#mkdir $DIR
mkdir cpo
#realpath cpo/
mkdir cpo/$CPONUM

#mv gem-loop*.* $DIR
#mv gem_coretransp*.* cpo

for d in run*/ ; do #latest
    echo "$d"
    mkdir cpo/$CPONUM/$d
    mv $d/gem_coretransp*.cpo cpo/$CPONUM/$d/
done

#mv imp4dv_coretransp_0*.cpo $DIR
#mv gem_coretransp_0*.cpo $DIR
#mv fout_0* $DIR
#mv stopped $DIR
#mv *.dat $DIR
#cp $DIR/t00.dat ./

#2. Run prosprocessing scripts
cd $DIR_CODE

#NUM=$(($CPONUM-13))
#NUMPR=$((NUM-1))
#CPONUM=$(($NUM+13))
CPONUMPR=$((CPONUM-1))

#TODO: make sure the script reads right things: gem_*.cpo -s from 'cpo' in run folder, for all runs (mofify structure of script), all flux tubes 

# command line arguments for main: folder with cpo-s; to read from original files or from csv; number of flux tubes; number of profile variants; file name to save

#python3 gem_da.py 'run'$RUNNUM'/cpo'$CPONUM'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM
#python3 gem_da.py $DIR_SRC'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM

python3 gem_da.py ${DIR_SRC}/cpo/${CPONUM} 0 1 ${RUNRANGE} 'new_'${UQCAMPDIR}'_'${CPONUM} #latest

#3. Prepare combined files for analysis of series across long-term runs
#cp GEM_plots/gem_??_transp_flux_evol_all${NUMPR}.csv ./

for r in `seq 1 $RUNRANGE`; do #latest
    cp GEM_plots/gem_??_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv ./
    cat gem_ti_transp_flux_evol_all_${UQCAMPDIR}_${CPONUMPR}_${r}.csv gem_ti_transp_flux_evol_new_${UQCAMPDIR}_${CPONUM}_${r}.csv > gem_ti_transp_flux_evol_all_${UQCAMPDIR}_${CPONUM}_${r}.csv
done

#cat gem_ti_transp_flux_evol_all${NUMPR}.csv gem_ti_transp_flux_evol_${CPONUM}.csv > gem_ti_transp_flux_evol_all${NUM}.csv

#4. Run the post-processing Python script
#python3 gem_da.py 'all'$NUM 1

python3 gem_da.py all_${UQCAMPDIR}_${CPONUM} 1 1 ${RUNRANGE} all_${UQCAMPDIR}_${CPONUM} #latest

#5. Put the resulting output files in a separat directory
mv *.txt GEM_plots/
mv *.csv GEM_plots/
mv *.png GEM_plots/

