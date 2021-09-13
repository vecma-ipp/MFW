
NUM=5
NUMPR=$(($NUM-1))
CPONUM=$(($NUM+13))

python3 gem_da.py $CPONUM

cp 'GEM_plots/gem_ti_transp_flux_evol_all'$NUMPR'.csv' ./
cat 'gem_ti_transp_flux_evol_all'$NUMPR'.csv' 'gem_ti_transp_evol_'$CPONUM'.csv' > 'gem_ti_transp_flux_evol_all'$NUM'.csv'

python3 gem_da.py 'all'$NUM 1

mv *.txt GEM_plots/
mv *.csv GEM_plots/
mv *.png GEM_plots/


