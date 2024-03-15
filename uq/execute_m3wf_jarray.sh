#!/bin/bash -l

##################################

## job array
#SBATCH --array=1-128

## job name
#SBATCH --job-name=UQ_AL_SURR_RESTART_

## stdout and stderr files
#SBATCH --output=test-aleatoric-restart-out.%A_%a
#SBATCH --error=test-aleatoric-restart-err.%A_%a

#SBATCH --no-requeue

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=0:14:00

## number of nodes and tasks per node
# next line: for running only the first slow flux tube

###SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --ntasks-per-core=1 # should disable hyperthreading
###SBATCH --cpus-per-task=1
#SBATCH --ntasks=8

##SBATCH --mem-per-cpu=2048MB
#SBATCH --mem=2GB

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

####################################

locid=${1:-0}

source activate ${HOME}/conda-envs/python3114

locsimulationdir=muscle3
scratchdir=/ptmp/yyudin/

rerun_script_orig_folder=/u/yyudin/code/MFW/uq/

lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
simulation_op=gem_surr_workflow_independent.sh
rerun_m3wf_op=execute_m3wf_single.sh

# how to start execution of each job
launch_op="./"       # - option 1.1 - shell script
#launch_op="sbatch " # -option 1.2 - SLURM script

#copy_op="cp "   # - option 2.1 - actually copy files
copy_op="ln -s " # - option 2.2 - sym-link to /common/ folder

### Start the execution
camp_file_dir=${1:-'UQ_8FTGEM0_WF_AL_lmebz6lt'}

cd ${scratchdir}/${camp_file_dir}

locdir=$(pwd)
echo "locdir: "${locdir}

# remove broken links
find . -xtype l -delete

### Run the M3 workflow instances

echo "slurm env vars : "
echo "SLURM_JOB_CPUS_PER_NODE: "${SLURM_JOB_CPUS_PER_NODE}
echo "SLURM_CPUS_ON_NODE: "${SLURM_CPUS_ON_NODE}
echo "SLURM_CPUS_PER_TASK: "${SLURM_CPUS_PER_TASK} 
echo "SLURM_NTASKS_PER_CORE: "${SLURM_NTASKS_PER_CORE}
echo "SLURM_NTASKS_PER_NODE: "${SLURM_NTASKS_PER_NODE}
echo "SLURM_TASKS_PER_NODE: "${SLURM_TASKS_PER_NODE}
echo "SLURM_NTASKS: "${SLURM_NTASKS}

echo "job array env vars : " $SLURM_ARRAY_JOB_ID $SLURM_ARRAY_TASK_ID $SLURM_ARRAY_TASK_MIN $SLURM_ARRAY_TASK_MAX $SLURM_ARRAY_TASK_COUNT
scontrol show -o --detail job ${SLURM_JOB_ID}
#################################
run_folder_list=( )
# find all the run directories without the final output file
while IFS=  read -r -d $'\0'; do 
  run_folder_list+=("$REPLY")
done < <(find ./ -maxdepth 6 -mindepth 6 -type d '!' -exec test -e "{}/"${lastcoreprofcpo} ';' -print0  | 
sed "s|^\.\/||")
#echo "first element of run_folder_list : "${run_folder_list} ###DEBUG

#echo "number of runs to restart: " "${#run_folder_list[@]}"

# Run every job as a part of a job array
#  select the job to run

echo "executing job task number "${SLURM_ARRAY_TASK_ID}
run_folder=${run_folder_list[${SLURM_ARRAY_TASK_ID}]}
echo "run folder "${run_folder}
SCRATCH_DIRECTORY=${scratchdir}/${camp_file_dir}/${run_folder}/${locsimulationdir}/

# go to the M3 local folder  #${runprefix}_${locid}_${itnum}/
cd ${scratchdir}/${camp_file_dir}/${run_folder}/${locsimulationdir}/
#echo "now in: "$(pwd) ###DEBUG

# copy/link the new executable script - will not work if a link already exists
${copy_op} ${rerun_script_orig_folder}/${rerun_m3wf_op}    ./${rerun_m3wf_op}

# launch the M3WF again from scratch
${launch_op}${rerun_m3wf_op} > test-aleatoric-restart-log_${SLURM_ARRAY_TASK_ID}.${SLURM_JOBID}
locpid=$!
# touch execm3.log 
# tail -f execm3.log --pid=${locpid}
echo "launched the job "${locpid}" for: "${run_folder}

# store the pid
#pids+=(${locpid})
#$((counter++))

echo ">>> Finished the executing all the jobs!"
##################################################################
exit 0
