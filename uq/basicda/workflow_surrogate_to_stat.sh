#!/bin/sh

datenow=$(date +"%Y%m%d")

origdir=$(pwd)

curr_id=datenow

### Prepare data for an iteration: CPOs 
itnum=0
itdir=prep_folder_${datenow}_${itnum}
mkdir ${itdir}

muscledir=~/code/MFW/muscle3/workflow/
runprefix=run_fusion_gem0_surr_
runsufix=/instances/transport/workdir/

runid=20240202_2
sourcedir=${muscledir}${runprefix}${runid}${runsufix}

#ATTENTION: override for testing
itdir=${PWD}

#TODO iterate over CPO definitions
cp ${sourcedir}/equilupdate_equilibrium_out.cpo ${itdir}/
cp ${sourcedir}/ets_coreprof_out.cpo    ${itdir}/

# Prepare run folder -> final coreprof CPO -> final plasma state -> grid around final state -> pyGEM0 dataset around final state
python prepare_gem0_sample.py ${itdir} ${curr_id} ${itnum}

### Prepare new surrogate with new data

cd ~/code/EasySurrogate/tests/gem_gp/
#TODO modify the shell file for GPR
process_gpr.sh ${datenow} ${itnum} 

### Run M3-WF with the new surrogate
cd ~/code/MFW/muscle3/workflow/
#TODO modify the shell script for M3-WF
./gem_surr_workflow.sh ${datenow} ${itnum}

### Collect the data from the M3-WF run and compare resulting profiles with the last iteration
cp run_fusion_gem0_surr_${datenow}_${itnum}/instances/transport/workdir/etc_coreprof_out.cpo ${origdir}/
cd ${origdir}

#conv=$(python compare_coreprof.py ${itnum})



