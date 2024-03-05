#!/bin/sh

### Define fixed folder and file names
echo ">>> Workflow: Setting up names"

runprefix=run_fusion_gem0_surr_
runsufix=/instances/transport/workdir
commondir=../../../../../../common

initcoreprofcpo=ets_coreprof_in.cpo
initequilibriumcpo=ets_equilibrium_in.cpo
lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
simulation_op=gem_surr_workflow_independent.sh
compare_op=compare_workflow_states.py

#copy_op="cp "    # - option 1 - actually copy files
#copy_op="ln -s " # - option 2 - sy:qm-link to /common/ folder

### Start the execution
locdir=$(pwd)

locid=${1:-0}
itnum=${2:-0}

# rename the old M3-WF directory
mv ${runprefix}${locid}_${itnum} ${runprefix}${locid}_${itnum}_orig

### Run the M3 workflow single instance

# launch the M3WF again from scratch
./${simulation_op} ${locid} ${itnum}
echo "exit status= "${exit_status}

# set the folder with simulation results
simworkdir=${runprefix}${locid}_${itnum}${runsufix}

# check the exit status?

### Compare resulting profiles with a stored 'ground truth' stationary profile, and with the initial profile
echo ">>> Comparing the final state with ground-truth stationary state"
statelast=${simworkdir}/${lastcoreprofcpo}
state_gtst=../${commondir}/ets_coreprof_stst.cpo
statelast_eq=${simworkdir}/${lastequilibriumcpo}
state_gtst_eq=../${commondir}/equilupdate_equilibrium_stst.cpo
python ${compare_op} ${statelast} ${state_gtst} ${statelast_eq} ${state_gtst_eq}

echo ">>> Comparing the final state with the initial state"
stateinit=../${commondir}/${initcoreprofcpo}
stateinit_eq=../${commondir}/${initequilibriumcpo}
python ${compare_op} ${statelast} ${stateinit} ${statelast_eq} ${stateinit_eq}

# Copy the resulting core profile to the root directory
cp ${statelast} ../

echo ">> Finished the M3WF in "${locdir}

##################################################################
