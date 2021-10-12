
CPONUM=32

DIR='/marconi_scratch/userexternal/yyudin00/MFW_runs/cpo'$CPONUM'/'

mkdir $DIR

cd standalone/bin/

mv gem-loop*.* $DIR
mv imp4dv_coretransp_0*.cpo $DIR
mv gem_coretransp_0*.cpo $DIR
mv fout_0* $DIR
mv stopped $DIR
mv *.dat $DIR
cp $DIR/t00.dat ./

cd  ../../uq/basicda/


NUM=$(($CPONUM-13))
NUMPR=$(($NUM-1))
#CPONUM=$(($NUM+13))

python3 gem_da.py ''$CPONUM

cp 'GEM_plots/gem_ti_transp_flux_evol_all'$NUMPR'.csv' ./
cat 'gem_ti_transp_flux_evol_all'$NUMPR'.csv' 'gem_ti_transp_flux_evol_'$CPONUM'.csv' > 'gem_ti_transp_flux_evol_all'$NUM'.csv'

python3 gem_da.py 'all'$NUM 1

mv *.txt GEM_plots/
mv *.csv GEM_plots/
mv *.png GEM_plots/


