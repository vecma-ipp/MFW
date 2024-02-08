#!/bin/sh

locid=${1:-0}

### Define fixed folder and file names
echo ">>> Surrogate workflow: Setting up names"

#muscledir=~/code/MFW/muscle3
muscledir=/
muscleworkdir=workflow
runprefix=run_fusion_gem0_surr_
runsufix=/instances/transport/workdir

commondir=/common

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
surrogate_op=process_gpr.sh
simulation_op=gem_surr_workflow_independent.sh
compare_op=compare_workflow_states.py

### Start the workflow
locdir=${pwd}
itnum=0
#locid=${locdir} #TODO define identifier according to the UQ case

### Surrogate preparation
echo ">>> Making a new surrogate for simulation workflow"
# Copy the necessary files from common folder - the dataset CSV should already be there
surrogate_files=( ${surrogate_op} gem_data_ind.py train_model_ind.py test_model_ind.py )

locsurrogatedir=easysurrogate
mkdir ${locsurrogatedir}

for surr_file in ${surrogate_files[@]}; do
  cp ${commondir}/${surr_file} ${locdir}/${locsurrogatedir}/
done

cd ${locsurrogatedir}
# Run the surrogate script - should already copy the surrogates
./${surrogate_op} ${locid} ${itnum} # TODO: Should accept arbitrary id, read right files, copy files in a subfolder here

refsurdata={gem0py_new_training.csv}
### Run the M3 workflow
echo ">>> Running a new simulation workflow"
# Copy the initial state for the simulation, also postprocessing script
simulation_files=( ${initcoreprofcpo} ${initequilibriumcpo} ${initcoretranspcpo} ${inittoroidfieldcpo} ${initcoresourcecpo} ${initcoreimpurcpo} read_profs.py)

locsimulationdir=muscle3
mkdir ${locsimulationdir}

for sim_file in ${simulation_files[@]}; do
  cp ${commondir}/${sim_file} ${locdir}/${locsimulationdir}/
done

cp ${refsurdata} ${locdir}/${locsimulationdir}/ref_train_data.csv

cd ${locsimulationdir}/
# Run the M3 workflow
./${simulation_op} ${locid} ${itnum} # YYMSL file has to know the local file location, script to accpet arbitrary id

### Compare resulting profiles with a stored 'ground truth' stationary profile, and with the initial profile
echo ">>> Comparing the final state with ground-truth stationary state"
statelast=/${runsufix}/${lastcoreprofcpo}
python ${compare_op} ${statelast} ${state_gtst} 

echo ">>> Comparing the final state with the initial state"
stateinit=${commondir}/${initcoreprofcpo}
python ${compare_op} ${statelast} ${stateinit} 

echo ">>> Finished the surrogate workflow!"
##################################################################

# ### Define the starting point of the loop 
# echo ">>> Preparing initial state: filenames"
# sourcedir=${muscledir}/${muscleworkdir}/gem0_surr_resume_data/
# state_gtst=gem0wf_stst/${lastcoreprofcpo}

# datenow=$(date +"%Y%m%d")
# #origdir=$(pwd)
# origdir=~/code/MFW/uq/basicda
# curr_id=${datenow}
  
# ### Prepare data for an iteration: CPOs 
# echo ">>> Preparing data for iteration "${itnum}

# locdir=$(pwd)

# cp ${sourcedir}/${initequilibriumcpo} ${locdir}/
# cp ${sourcedir}/${initcoreprofcpo} ${locdir}/
