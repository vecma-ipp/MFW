
#0. set directories
CPONUM=1
RUNNUM=2

DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qairnbbz'
#DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_qpyxg3bb'

#DIR='/marconi_scratch/userexternal/yyudin00/MFW_runs/cpo'$CPONUM'/'
#DIR='/marconi_scratch/userexternal/yyudin00/GEM_NT_test/run'$RUNNUM'/cpo'$CPONUM'/cpo'

#DIR_SRC='/marconi_scratch/userexternal/yyudin00/MFW_runs/1ft_stprof/'
#DIR_SRC='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_test/run1/'

#DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/run_'$RUNNUM
DIR_SRC=$DIR'/runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100/'

DIR_CODE=$HOME'/code/MFW/uq/basicda/'

#1. transfer output files from the run directory
#echo "$DIR_SRC"
cd $DIR_SRC
#mkdir $DIR
mkdir cpo
#realpath cpo/

#mv gem-loop*.* $DIR
#mv gem_coretransp*.* cpo

for d in run*/ ; do
    echo "$d"
    mkdir cpo/$d
    mv $d/gem_coretransp*.cpo cpo/$d/
done

#mv imp4dv_coretransp_0*.cpo $DIR
#mv gem_coretransp_0*.cpo $DIR
#mv fout_0* $DIR
#mv stopped $DIR
#mv *.dat $DIR
#cp $DIR/t00.dat ./

#2. run prosprocessing scripts
cd $DIR_CODE

#NUM=$(($CPONUM-13))
#NUMPR=$(($NUM-1))
#CPONUM=$(($NUM+13))

#TODO : doesn't read the new cpo-s correctly, probably wrong folder
#TODO: make sure the script reads right things: gem_*.cpo -s from 'cpo' in run folder, for all runs (mofify structure of scrip), all flux tubes 

#python3 gem_da.py 'run'$RUNNUM'/cpo'$CPONUM'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM  #TODO: change gem_da.py input attributes to read new cpo-s from different locations
#python3 gem_da.py $DIR_SRC'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM
python3 gem_da.py $DIR_SRC'/cpo' 0 1 16 'new_'$CPONUM

#cp GEM_plots/gem_??_transp_flux_evol_all${NUMPR}.csv ./
#cat gem_ti_transp_flux_evol_all${NUMPR}.csv gem_ti_transp_flux_evol_${CPONUM}.csv > gem_ti_transp_flux_evol_all${NUM}.csv

#python3 gem_da.py 'all'$NUM 1

#mv *.txt GEM_plots/
#mv *.csv GEM_plots/
#mv *.png GEM_plots/


