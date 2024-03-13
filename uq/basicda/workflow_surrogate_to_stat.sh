#!/bin/sh

### Define fixed folder and file names
echo ">>> Retraining workflow: Setting up names"
codenameshort=gem0
codename=${codenameshort}py
muscledir=~/code/MFW/muscle3
muscleworkdir=workflow
runprefix=run_fusion_${codenameshort}_surr_
runsufix=/instances/transport/workdir

easysurrogatedir=~/code/EasySurrogate/tests/gem_gp

lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
prepare_op=prepare_gem0_sample.py
surrogate_op=process_gpr_ind.sh
simulation_op=gem_surr_workflow_independent.sh
compare_op=compare_workflow_states.py

### Define the starting point of the loop - and prepare the first iteration
echo ">>> Preparing initial state"

sourcedir=${muscledir}/${muscleworkdir}/${codenameshort}_surr_resume_data/
state_gtst=${codenameshort}wf_stst/${lastcoreprofcpo}
state_eq_gtst=${codenameshort}wf_stst/${lastequilibriumcpo}

datenow=$(date +"%Y%m%d")

#TODO make a folder for every iteration
origdir=$(pwd)

curr_id=${datenow}

itnum_min=0
itnum_max=8

# to start iteration from scratch or to continue
if [ ${itnum_min} == 0 ]
then
  echo ">> Copying initial state CPOs from: "${sourcedir}
  cp ${sourcedir}/ets_coreprof_in.cpo    ${sourcedir}/${lastcoreprofcpo}
  cp ${sourcedir}/ets_equilibrium_in.cpo    ${sourcedir}/${lastequilibriumcpo}
else
  sourcedir=${origdir}
fi

# to use equilibrium data or not
useequil=1
useequil_tocomp=1

echo ">>> Entering the loop"
for((itnum=${itnum_min};itnum<${itnum_max};itnum++)); do
  
  ### Prepare data for an iteration: CPOs 
  echo ">>> Preparing data for iteration "${itnum}

  itdir=folder_${datenow}_${itnum}
  #ATTENTION: override for testing - ATM works fine
  itdir=${PWD}
  #mkdir ${itdir}

  cp ${sourcedir}/${lastequilibriumcpo}    ${itdir}/
  cp ${sourcedir}/${lastcoreprofcpo}    ${itdir}/

  ### Compare initial state and ground-truth stationary state
  echo "Comparing intial iteration state with ground-truth stationary state"
  
  if [ ${useequil_tocomp} == 1 ]
  then
    title_plot="Distances between simulation and reference stationary states"
    python ${compare_op} ${lastcoreprofcpo} ${state_gtst} ${lastequilibriumcpo} ${state_eq_gtst} "${title_plot}"
  else
    python ${compare_op} ${lastcoreprofcpo} ${state_gtst}
  fi

  ### Prepare run folder:
  # 	 final coreprof CPO -> final plasma state -> grid around final state -> pyGEM0 dataset around final state
  echo ">>> Preparing new training data set around iteration initial state"

  if [ ${useequil_tocomp} == 1  ]
  then
    title_plot="Distances between simulation initial and final states"
    python ${prepare_op} ${itdir} ${curr_id} ${itnum} ${useequil} ${lastequilibriumcpo} "${title_plot}"
  else
    python ${prepare_op} ${itdir} ${curr_id} ${itnum}
  fi

  ### Prepare new surrogate with new data
  echo ">>> Making a new surrogate for simulation workflow"

  cp ${codename}_new_${curr_id}_${itnum}.csv ${easysurrogatedir}

  cd ${easysurrogatedir}
 
  ./${surrogate_op} ${datenow} ${itnum} ${easysurrogatedir} ${muscledir}

  ### Run M3-WF with the new surrogate
  # TODO unlike surrogate, switch to use equilibrium needs a change in YMMSL file
  echo ">>> Running a new simulation workflow"

  cd ${muscledir}/${muscleworkdir}/
 
  # DEBUG: delete old M3-WF directory - should work in any way
  rm -r ${runprefix}${datenow}_${itnum} 

  # Prepare the necessary files
  cp ${origdir}/${codenameshort}py_new_${curr_id}_${itnum}.csv    ${muscledir}/${muscleworkdir}/ref_train_data.csv
  # Original state for the new M3-WF run - two options:
  # 	1) Same inial state (~ from AUG shot)
  # 	2) Final state of the WF from the previous iteration
  # Choice - do not change the intial state

  ./${simulation_op} ${datenow} ${itnum}

  ### Collect the data from the M3-WF run and compare resulting profiles with the last iteration
  echo ">>> Comparing this and previous iteration states"
  
  prev_state_name=ets_coreprof_out_last_${datenow}_${itnum}.cpo
  prev_state_eq_name=equilupdate_equilibrium_out_last_${datenow}_${itnum}.cpo

  mv ${origdir}/${lastcoreprofcpo}    ${origdir}/${prev_state_name}
  cp ${runprefix}${datenow}_${itnum}${runsufix}/${lastcoreprofcpo}    ${origdir}/

  mv ${origdir}/${lastequilibriumcpo}    ${origdir}/${prev_state_eq_name}
  cp ${runprefix}${datenow}_${itnum}${runsufix}/${lastequilibriumcpo}    ${origdir}/

  cd ${origdir}

  state1=${lastcoreprofcpo}
  state2=${prev_state_name}

  state_eq1=${lastequilibriumcpo}
  state_eq2=${prev_state_eq_name}

  if [ ${useequil_tocomp} == 1 ]
  then
    title_plot="Distances between final states of simulations using consecutive surrogate iterations"
    python ${compare_op} ${state1} ${state2} ${state_eq1} ${state_eq2} "${title_plot}"
  else
  python ${compare_op} ${state1} ${state2}
  fi

  ### Compare resulting profiles with a stored 'ground truth' stationary profile
  echo ">>> Comparing this iteration state with ground-truth stationary state"
  
  if [ ${useequil_tocomp} == 1 ]
  then
    title_plot="Distances between simulation final and reference stationary states"
    python ${compare_op} ${state1} ${state_gtst} ${state_eq1} ${state_eq_gtst} "${title_plot}"
  else
    python ${compare_op} ${state1} ${state_gtst} 
  fi

  ### Prepare for the next iteration
  echo ">>> Preparing for the next iteration"

  runid_prev=${datenow}_${itnum}
  sourcedir=${muscledir}/${muscleworkdir}/${runprefix}${runid_prev}${runsufix}/

done

# Renaming the results of the las iteration
mv ${origdir}/${lastcoreprofcpo}    ${origdir}/ets_coreprof_out_last_${datenow}_${itnum_max}.cpo
mv ${origdir}/${lastequilibriumcpo}    ${origdir}/equilupdate_equilibrium_out_last_${datenow}_${itnum_max}.cpo

echo ">>> Finished the retraining workflow!"
