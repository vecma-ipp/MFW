#!/bin/sh

##################################

## job name
#SBATCH --job-name=SURR_WF_RESTART_
## stdout and stderr files
#SBATCH --output=test-aleatoric-restart-singlejob-out.%j
#SBATCH --error=test-aleatoric-restart-singlejob-err.%j

#SBATCH --no-requeue

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=0:30:00

## number of nodes and tasks per node
# next line: for running only the first slow flux tube
###SBATCH --nodes=1
#SBATCH --ntasks=8
#SBATCH --ntasks-per-node=8
#SBATCH --ntasks-per-core=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4096

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

####################################

echo "SLURM_JOB_CPUS_PER_NODE: "${SLURM_JOB_CPUS_PER_NODE}
scontrol show -o --detail job ${SLURM_JOB_ID}

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
#copy_op="ln -s " # - option 2 - sym-link to /common/ folder

### Start the execution
locdir=$(pwd)

locid=${1:-0}
itnum=${2:-0}

# rename the old M3-WF directory
time_now=$(date +"%Y%m%d_%H%M%S")
mv ${runprefix}${locid}_${itnum} ${runprefix}${locid}_${itnum}_${time_now}

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
