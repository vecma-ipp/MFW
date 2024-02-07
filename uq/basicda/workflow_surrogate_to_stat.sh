#!/bin/sh

### Define fixed folder and file names
echo ">>> Retraining workflow: Setting up names"
muscledir=~/code/MFW/muscle3
muscleworkdir=workflow
runprefix=run_fusion_gem0_surr_
runsufix=/instances/transport/workdir

easysurrogatedir=~/code/EasySurrogate/tests/gem_gp

lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
prepare_op=prepare_gem0_sample.py
surrogate_op=process_gpr.sh
simulation_op=gem_surr_workflow.sh
compare_op=compare_workflow_states.py

### Define the starting point of the loop - and prepare the first iteration
echo ">>> Preparing initial state"
sourcedir=${muscledir}/${muscleworkdir}/gem0_surr_resume_data/
cp ${sourcedir}/ets_coreprof_in.cpo ${sourcedir}/${lastcoreprofcpo}
cp ${sourcedir}/ets_equilibrium_in.cpo ${sourcedir}/${lastequilibriumcpo}
state_gtst=gem0wf_stst/${lastcoreprofcpo}

datenow=$(date +"%Y%m%d")

#TODO make a folder for every iteration
origdir=$(pwd)

curr_id=${datenow}

itnum_min=0
itnum_max=3

echo ">>> Entering the loop"
for((itnum=${itnum_min};itnum<${itnum_max};itnum++)); do
  
  ### Prepare data for an iteration: CPOs 
  echo ">>> Preparing data for iteration "${itnum}

  itdir=folder_${datenow}_${itnum}
  #ATTENTION: override for testing - ATM works fine
  itdir=${PWD}
  #mkdir ${itdir}

  cp ${sourcedir}/${lastequilibriumcpo} ${itdir}/
  cp ${sourcedir}/${lastcoreprofcpo} ${itdir}/

  ### Compare initial state and ground-truth stationary state
  echo "Comparing intial iteration state with ground-truth stationary state"
  
  ${compare_op} ${lastequilibriumcpo} ${state_gtst}

  ### Prepare run folder:
  # 	 final coreprof CPO -> final plasma state -> grid around final state -> pyGEM0 dataset around final state
  echo ">>> Preparing new training data set around iteration initial state"

  python ${prepare_op} ${itdir} ${curr_id} ${itnum}

  ### Prepare new surrogate with new data
  echo ">>> Making a new surrogate for simualtion workflow"

  cd ${easysurrogatedir}
 
  ./${surrogate_op} ${datenow} ${itnum} 

  ### Run M3-WF with the new surrogate
  echo ">>> Running a new simulation workdflow"

  cd ${muscledir}/${muscleworkdir}/
 
  # DEBUG: delete old M3-WF directory - should work in any way
  rm -r ${runprefix}${datenow}_${itnum} 

  # Prepare the necessary files
  cp ${origdir}/gem0py_new_${curr_id}_${itnum}.csv ${muscledir}/ref_train_data.csv
  # Original state for the new M3-WF run - two options:
  # 	1) Same inial state (~ from AUG shot)
  # 	2) Final state of the WF from the previous iteration
  # Choice - do not change the intial state

  ./${simulation_op} ${datenow} ${itnum}

  ### Collect the data from the M3-WF run and compare resulting profiles with the last iteration
  echo ">>> Comparing this and precious iteration states"
  
  prev_state_name=ets_coreprof_out_last.cpo
  mv ${origdir}/${lastcoreprofcpo} ${origdir}/${prev_state_name}
  cp ${runprefix}${datenow}_${itnum}${runsufix}/${lastcoreprofcpo} ${origdir}/
  cd ${origdir}

  state1=${lastcoreprofcpo}
  state2=${prev_state_name}
  conv=$(python ${compare_op} ${state1} ${state2})

  ### Compare resulting profiles with a stored 'ground truth' stationary profile
  echo ">>> Comparing this iteration state with ground-truth stationary state"
  
  python ${compare_op} ${state1} ${state_gtst} 

  ### Prepare for the next iteration
  echo ">>> Preparing for the next iteration"

  runid_prev=${datenow}_${itnum}
  sourcedir=${muscledir}/${muscleworkdir}/${runprefix}${runid_prev}${runsufix}/

done

echo ">>> Finished the retraining workflow!"

