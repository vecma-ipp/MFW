#!/bin/sh

datenow=$(date +"%Y%m%d")

### Prepare data for an iteration: CPOs 
itnum=0
itdir=prep_folder_${datenow}_${itnum}
mkdir ${itdir}

#TODO iterate over CPO definitions
cp ${sourcedir}/ets_equilibrium_out.cpo ${itdir}/gem0_equilibrium_in.cpo
cp ${sourcedir}/ets_coreprof_out.cpo    ${itdir}/gem0_coreprof_in.cpo


#TODO function to read final_point*csv from gem0_coreprof_in.cpo
#TODO see surrogate_scan.ipynb
python prepare_gem0_sample.py

### Prepare new surrogate with new data

cd ~/code/EasySurrogate/test/gem_gp/
#TODO modify the shell file for GPR
process_gpr.sh ${datenow} ${itnum} 

### Run M3-WF with the new surrogate
cd ~/code/MFW/muscle3/workflow/
#TODO modify the shell script for M3-WF
./gem_surr_workflow.sh ${datenow} ${itnum}
