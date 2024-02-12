#!/bin/sh

locid=${1:-0}

### Define fixed folder and file names
echo ">>> Surrogate workflow: Setting up names"

#muscledir=~/code/MFW/muscle3
muscledir=/
muscleworkdir=workflow
runprefix=run_fusion_gem0_surr_
runsufix=/instances/transport/workdir

#commondir=/common
commondir=../../../../../../common
#TODO make an absolute common path

easysurrogatedir=~/code/EasySurrogate/tests/gem_gp

initcoreprofcpo=ets_coreprof_in.cpo
initequilibriumcpo=ets_equilibrium_in.cpo
initcoretranspcpo=ets_coretransp_in.cpo
inittoroidfieldcpo=ets_toroidfield_in.cpo
initcoresourcecpo=ets_coresource_in.cpo
initcoreimpurcpo=ets_coreimpur_in.cpo

lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
# TODO: make these scripts work at any directory -> should be already possible
surrogate_op=process_gpr_ind.sh
simulation_op=gem_surr_workflow_independent.sh
compare_op=compare_workflow_states.py

#copy_op="cp " # - option 1 - actually copy files
copy_op="ln -s "

### Start the workflow
locdir=$(pwd)
itnum=0
#locid=${locdir} #TODO define identifier according to the UQ case
#echo "locdir is ${locdir}" ###DEBUG

### Surrogate preparation
echo ">>> Making a new surrogate for simulation workflow"
# Copy the necessary files from common folder - the dataset CSV should already be there
surrogate_files=( ${surrogate_op} gem_data_ind.py train_model_ind.py test_model_ind.py )
surdata=gem0py_new_local.csv

locsurrogatedir=easysurrogate
mkdir ${locsurrogatedir}

${copy_op} ../${surdata} ${locdir}/${locsurrogatedir}/gem0py_new_${locid}_${itnum}.csv

for surr_file in ${surrogate_files[@]}; do
  ${copy_op} ../${commondir}/${surr_file} ${locdir}/${locsurrogatedir}/${surr_file}
done

cd ${locsurrogatedir}
# Run the surrogate script - should already copy the surrogates
./${surrogate_op} ${locid} ${itnum} # TODO: Should accept arbitrary id, read right files, copy files in a subfolder here

### Run the M3 workflow
cd ..
echo ">>> Running a new simulation workflow"
# Copy the initial state for the simulation, also postprocessing script
simulation_files=( ${simulation_op} ${compare_op} ${initcoreprofcpo} ${initequilibriumcpo} ${initcoretranspcpo} ${inittoroidfieldcpo} ${initcoresourcecpo} ${initcoreimpurcpo} read_profs.py gem-surr-mft-fusion-independent.ymmsl )

locsimulationdir=muscle3
mkdir ${locsimulationdir}

for sim_file in ${simulation_files[@]}; do
  ${copy_op} ../${commondir}/${sim_file} ${locdir}/${locsimulationdir}/${sim_file} 
done

${copy_op} ../${surdata} ${locdir}/${locsimulationdir}/ref_train_data.csv

cd ${locsimulationdir}
# Run the M3 workflow
./${simulation_op} ${locid} ${itnum} # YMMSL file has to know the local file location, script to accpet arbitrary id

# the folder with simulation results
simworkdir=${runprefix}${locid}_${itnum}${runsufix}

### Compare resulting profiles with a stored 'ground truth' stationary profile, and with the initial profile
echo ">>> Comparing the final state with ground-truth stationary state"
statelast=${simworkdir}/${lastcoreprofcpo}
state_gtst=../${commondir}/ets_coreprof_stst.cpo
python ${compare_op} ${statelast} ${state_gtst} 

echo ">>> Comparing the final state with the initial state"
stateinit=../${commondir}/${initcoreprofcpo}
python ${compare_op} ${statelast} ${stateinit} 

# Copy the resulting core profile to the root directory
cp ${statelast} ../../

echo ">>> Finished the surrogate workflow!"
##################################################################

# sourcedir=${muscledir}/${muscleworkdir}/gem0_surr_resume_data/

# datenow=$(date +"%Y%m%d")
# origdir=~/code/MFW/uq/basicda
# curr_id=${datenow}
  
# cp ${sourcedir}/${initequilibriumcpo} ${locdir}/
# cp ${sourcedir}/${initcoreprofcpo} ${locdir}/
