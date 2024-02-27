#!/bin/sh

### Define fixed folder and file names
echo ">>> Loop to compare workflow plasma states"
codenameshort=gem0
codename=${codenameshort}py
muscledir=~/code/MFW/muscle3
muscleworkdir=workflow
runprefix=run_fusion_${codenameshort}_surr_
runsufix=/instances/transport/workdir

#easysurrogatedir=~/code/EasySurrogate/tests/gem_gp

lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
#prepare_op=prepare_gem0_sample.py
#surrogate_op=process_gpr_ind.sh
#simulation_op=gem_surr_workflow_independent.sh
compare_op=compare_workflow_states.py

### Define the starting point of the loop - and prepare the first iteration
echo ">>> Setting up data"

#sourcedir=${muscledir}/${muscleworkdir}/${codenameshort}_surr_resume_data/
state_gtst=${codenameshort}wf_stst/${lastcoreprofcpo}
state_eq_gtst=${codenameshort}wf_stst/${lastequilibriumcpo}

#datenow=$(date +"%Y%m%d")
datenow=20240213
echo ">> Checking for the run on "${datenow}

origdir=$(pwd)

curr_id=${datenow}

itnum_min=0
itnum_max=4

# to use equilibrium data or not
useequil=1

echo ">>> Entering the loop"
for((itnum=${itnum_min};itnum<${itnum_max};itnum++)); do
  
  ### Prepare data for an iteration: CPOs 
  echo ">>> Preparing data for iteration "${itnum}

  #itdir=folder_${datenow}_${itnum}
  #itdir=${PWD}

  ### Compare initial state and ground-truth stationary state
  # echo "Comparing intial iteration state with ground-truth stationary state"
  
  # if [ ${useequil} == 1 ]
  # then
  #   python ${compare_op} ${lastcoreprofcpo} ${state_gtst} ${lastequilibriumcpo} ${state_eq_gtst}
  # else
  #   python ${compare_op} ${lastcoreprofcpo} ${state_gtst}
  # fi

  ### Collect the data from the M3-WF run and compare resulting profiles with the last iteration
  echo ">>> Comparing this and next iteration states"
  
  this_state_name=ets_coreprof_out_last_${datenow}_${itnum}.cpo
  this_state_eq_name=equilupdate_equilibrium_out_last_${datenow}_${itnum}.cpo

  next_state_name=ets_coreprof_out_last_${datenow}_$(( ${itnum}+1 )).cpo
  next_state_eq_name=equilupdate_equilibrium_out_last_${datenow}_$(( ${itnum}+1 )).cpo

  state1=${this_state_name}
  state2=${next_state_name}

  state_eq1=${this_state_eq_name}
  state_eq2=${next_state_eq_name}

  # NEXT IF THESE FILES ARE STILL NOT IN THE CURRENT FOLDER
  cp ${muscledir}/${muscleworkdir}/${runprefix}${datenow}_${itnum}${runsufix}/${lastequilibriumcpo}  ${state_eq1} 
  cp ${muscledir}/${muscleworkdir}/${runprefix}${datenow}_$(( ${itnum}+1 ))${runsufix}/${lastequilibriumcpo}  ${state_eq2} 

  if [ ${useequil} == 1 ]
  then
    python ${compare_op} ${state1} ${state2} ${state_eq1} ${state_eq2}
  else
  python ${compare_op} ${state1} ${state2}
  fi

  ### Compare resulting profiles with a stored 'ground truth' stationary profile
  echo ">>> Comparing this iteration state with ground-truth stationary state"
  
  if [ ${useequil} == 1 ]
  then
    python ${compare_op} ${state1} ${state_gtst} ${state_eq1} ${state_eq_gtst}
  else
    python ${compare_op} ${state1} ${state_gtst} 
  fi

  ### Prepare for the next iteration
  echo ">>> Preparing for the next iteration"

  #runid_prev=${datenow}_${itnum}
  #sourcedir=${muscledir}/${muscleworkdir}/${runprefix}${runid_prev}${runsufix}/

done

echo ">>> Finished the retraining workflow!"
