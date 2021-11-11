
NUM=7

DIR_FILE=$SCRATCH'/MFW_runs/mft/'
DIR_CODE=$HOME'/code/MFW/uq/basicda/'

cd $DIR_FILE

# transfer output files from run folder
mkdir run$NUM
mkdir run$NUM/cpo

mv stopped run$NUM/
mv fout* run$NUM/
mv *.dat run$NUM/

mv imp4dv_coretransp_*.cpo run$NUM/

mv gem_coretransp_*.cpo run$NUM/cpo/

cp run$NUM/t*.dat ./

# run postprocessing scripts
cd $DIR_CODE

python3 gem_da.py 'mft/run'${NUM}'/cpo' 0 8 'mft'${NUM}  # change the gem_da.py input attributes

cp GEM_plots/gem_ti_transp_flux_evol_mft_all$(($NUM-1)).csv ./
cat gem_ti_transp_flux_evol_mft_all$(($NUM-1)).csv gem_ti_transp_flux_evol_mft$(($NUM)).csv > gem_ti_transp_flux_evol_mft_all$((NUM)).csv

python3 gem_da.py 'mft_all'${NUM} 1

mv *.txt GEM_plots/
mv *.csv GEM_plots/
mv *.png GEM_plots/

