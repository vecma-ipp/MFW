#!/bin/bash -l

##################################

## job name
#SBATCH --job-name=UQ_AL_SURR_RESTART_

## stdout and stderr files
#SBATCH --output=test-aleatoric-restart-out.%j
#SBATCH --error=test-aleatoric-restart-err.%j

#SBATCH --no-requeue

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=4:00:00

## number of nodes and tasks per node
# next line: for running only the first slow flux tube
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=1

#SBATCH --partition=medium
###SBATCH --qos=

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

####################################

locid=${1:-0}

#source activate ${HOME}/conda-envs/python3114

### Define fixed folder and file names
# echo ">>> Workflow: Setting up names"
# muscledir=/
# muscleworkdir=workflow
# runprefix=run_fusion_gem0_surr_
# runsufix=/instances/transport/workdir
# commondir=../../../../../../common

locsimulationdir=muscle3
scratchdir=/ptmp/yyudin/

rerun_script_orig_folder=/u/yyudin/code/MFW/uq/

# initcoreprofcpo=ets_coreprof_in.cpo
# initequilibriumcpo=ets_equilibrium_in.cpo
# initcoretranspcpo=ets_coretransp_in.cpo
# inittoroidfieldcpo=ets_toroidfield_in.cpo
# initcoresourcecpo=ets_coresource_in.cpo
# initcoreimpurcpo=ets_coreimpur_in.cpo

lastcoreprofcpo=ets_coreprof_out.cpo
lastequilibriumcpo=equilupdate_equilibrium_out.cpo

### Define script file names of each operation
simulation_op=gem_surr_workflow_independent.sh
rerun_m3wf_op=execute_m3wf_single.sh

# how to start execution of each job
# launch_op="./"       # - option 1.1 - shell script
launch_op="sbatch " # -option 1.2 - SLURM script

#copy_op="cp "   # - option 2.1 - actually copy files
copy_op="ln -s " # - option 2.2 - sim-link to /common/ folder

### Start the execution

camp_file_dir=${1:-'UQ_8FTGEM0_WF_AL_lmebz6lt'}

cd ${scratchdir}/${camp_file_dir}

locdir=$(pwd)
echo "locdir: "${locdir}

# remove broken links
find . -xtype l -delete

### Run the M3 workflow instances

#################################
# find all the run directories without the final output file
run_folder_list=$( find ./ -maxdepth 6 -mindepth 6 -type d '!' -exec test -e "{}/"${lastcoreprofcpo} ';' -print  | sed "s|^\.\/||" )

run_folder_array=( )
# same run directories to an array
while IFS=  read -r -d $'\0'; do 
  run_folder_array+=("$REPLY")
done < <(find ./ -maxdepth 6 -mindepth 6 -type d '!' -exec test -e "{}/"${lastcoreprofcpo} ';' -print0  | 
sed "s|^\.\/||")

echo "number of runs to restart: " "${#run_folder_array[@]}"

# TODO find all the unfinished runs: 
#     a. by the presence of ets_coreprof_out.cpo +
#     b. by the status in the EasyVVUQ campaign DB

# TODO thread-parallelise ? run multiple SLURM jobs asking for 1 core each?
# TODO run inside a SLURM job: and detach from job every time? +

#counter=1
countermax=4
pids=()
# go to each directory
#for run_folder in ${run_folder_list[@]}; 
for((i=0;i<${countermax};i++));

do
  run_folder=${run_folder_array[i]}

  # go to the M3 local folder  #${runprefix}_${locid}_${itnum}/
  cd ${scratchdir}/${camp_file_dir}/${run_folder}/${locsimulationdir}/
  #echo "now in: "$(pwd) ###DEBUG

  # copy/link the new executable script - will not work if a link already exists
  ${copy_op} ${rerun_script_orig_folder}/${rerun_m3wf_op}    ./${rerun_m3wf_op}

  # launch the M3WF again from scratch
  sleep 1
  ${launch_op}${rerun_m3wf_op} &
  locpid=$!
  # touch execm3.log 
  # tail -f execm3.log --pid=${locpid}
  echo "launched the job "${locpid}" for: "${run_folder}

  # store the pid
  pids+=(${locpid})
  #$((counter++))

done

# wait until the end of all jobs
# for pid in ${pids[@]}; do
#   wait ${pid}
#   echo "finished "${pid} 
# done
wait

echo ">>> Finished the executing all the jobs!"
##################################################################
