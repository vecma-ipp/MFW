
CPONUM=1
RUNNUM=1

#DIR='/marconi_scratch/userexternal/yyudin00/MFW_runs/cpo'$CPONUM'/'
DIR='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_test/run'$RUNNUM'/cpo'$CPONUM'/cpo'

#DIR_SRC='/marconi_scratch/userexternal/yyudin00/MFW_runs/1ft_stprof/'
#DIR_SRC='/marconi_scratch/userexternal/yyudin00/VARY_1FT_GEM_NT_test/run1/'

DIR_CODE=$HOME'/code/MFW/uq/basicda/'

# transfer output files from the run directory
mkdir $DIR

#cd $DIR_SRC

#mv gem-loop*.* $DIR
#mv imp4dv_coretransp_0*.cpo $DIR
#mv gem_coretransp_0*.cpo $DIR
#mv fout_0* $DIR
#mv stopped $DIR
#mv *.dat $DIR
#cp $DIR/t00.dat ./

# run prosprocessing scripts
cd $DIR_CODE

#NUM=$(($CPONUM-13))
#NUMPR=$(($NUM-1))
#CPONUM=$(($NUM+13))

#TODO : doesn't read the new cpo-s correctly, probably wrong folder

python3 gem_da.py 'run'$RUNNUM'/cpo'$CPONUM'/cpo' 0 1 'new_'$RUNNUM'_'$CPONUM  # change gem_da.py input attributes to read new cpo-s from different locations

#cp GEM_plots/gem_??_transp_flux_evol_all${NUMPR}.csv ./
#cat gem_ti_transp_flux_evol_all${NUMPR}.csv gem_ti_transp_flux_evol_${CPONUM}.csv > gem_ti_transp_flux_evol_all${NUM}.csv

#python3 gem_da.py 'all'$NUM 1

#mv *.txt GEM_plots/
#mv *.csv GEM_plots/
#mv *.png GEM_plots/


