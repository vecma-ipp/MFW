#!/bin/sh

campname=csldvnei
n_runs=648

n_start=3
n_finish=14

qs=('te' 'ti' 'ne' 'ni')

for ((i=n_start ; i<n_finish ; i++)); do

	echo "macroiteration number "${i}
	
	# run postprocessing script to read the CPOs
	./gem_postproc_vary_test.sh ${i} ${campname} 0 0 1 ${n_runs} 1 0 &> log_$(date +"%Y%m%d")_${campname}.txt${i}
	
	# delete unnecessary files
	echo "deleting files"
	
	rm timetraces_act_*_transp_flux_new_${campname}_${i}_[0-9]*.pdf
	#rm gem_*_transp_flux_evol_new_${campname}_${i}_[0-9]*.csv

	# for q in ${qs[@]} ; do
	# 	for j in `seq 1 ${n_runs}`; do
	# 		if [ -e gem_${q}_transp_flux_evol_all_${campname}_${i}_${j}.csv ]; then
	# 			rm gem_${q}_transp_flux_evol_all_${campname}_$((${i}-1))_${j}.csv 
	# 		fi
	# 	done
	# done
	
	#rm *_new_${campname}_${i}.*

done

# combine the CSV-s after the last run
python process_csv.py

# delete the unnecassary files

#rm gem_*_transp_flux_evol_*_${campname}_[0-9]*_[0-9]*.csv

for i in `seq ${n_start} ${n_finish}`; do
	for q in ${qs[@]} ; do
		for j in `seq 1 ${n_runs}`; do
			rm gem_${q}_transp_flux_evol_new_${campname}_${i}_${j}.csv
			#if [ -e gem_${q}_transp_flux_evol_all_${campname}_${i}_${j}.csv ]; then
			#	rm gem_${q}_transp_flux_evol_all_${campname}_$((${i}-1))_${j}.csv 
			fi
		done
	done
	rm *_new_${campname}_${i}.*
done
