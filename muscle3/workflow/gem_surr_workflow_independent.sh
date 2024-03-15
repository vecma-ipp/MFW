#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cobra/u/yyudin/muscle3/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cobra/u/yyudin/code/json-fortran/build/lib/

echo 'Running gem_surr workflow in Fortran & Python'

. ~/muscle3/bin/muscle3.env

curr_id=${1:-1}

iternum=${2:-0}

runlabel='_'${iternum}

code_run_name=gem0_surr_

run_dir_name=run_fusion_${code_run_name}${curr_id}${runlabel}
mkdir ${run_dir_name}

# # DEBUG and checking section
# echo PATH:
# echo ${PATH}
# echo PYTHONPATH:
# echo ${PYTHONPATH}
# which python3
# echo "running the workflow with a root at "$(pwd)
# #python3 /u/yyudin/code/MFW/muscle3/src/gem_surr_mft_M3.py #DEBUG - script doomed to fail

#TODO: (a) modify the YMMSL beforehand
#      (b) pass id to muscle_manager: M3 'implementation' of gem_sur_imp has to accept arguments
#      (c) generate a sufficiently long (16 digits?) ID inside the M3 'implementation' -> done

timeouttime='18m' #TODO should be dependent on the number of iterations
starting_op='timeout '${timeouttime} #TODO HAS TO BE FIXED

#muscle_manager --log-level DEBUG --run-dir ${run_dir_name} --start-all gem-surr-mft-fusion-independent.ymmsl &
${starting_op} muscle_manager --log-level DEBUG --run-dir ${run_dir_name} --start-all gem-surr-mft-fusion-independent.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

# # Next 1 lines: time out the workflow in case it halts in the very end
# timeout ${timeouttime} bash -c "sleep ${timeouttime} && kill -s SIGTERM ${manager_pid}" &

# Next 2 lines: just to get additional info about processes and cores
# sleep 30
# ps -u yyudin -o user,pid,pcpu,rss,vsz,psr,args

# Next 4 lines are originally not commented out 
touch muscle3_manager.log
echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}
wait ${manager_pid}
exit_status=$!

# check the termination cause
# kill %1
echo "WF EXIT CODE: "${exit_status}
if [ ${exit_status} -eq 0 ]; then
    echo "Workflow completed successfully"
else
    echo "Workflow was killed after "${timeouttime}
fi

# Make plots for the evolution of core profiles
# TODO makle read_profs.py work independently of locations
echo "> Simulation finished, now postprocessing"
#cd ..
python read_profs.py ${code_run_name} ${curr_id}${runlabel}

# Save the results of the run and postprocessing
# cd workflow/
# tar -czvf ${run_dir_name}.tar.gz --exclude=*.cpo --exclude=*.dat ${run_dir_name}/ ${run_dir_name}_${curr_id}${runlabel}/
# mv run_fusion_${code_run_name}${curr_id}${runlabel}.tar.gz ../../..

echo "> Last line of simulation workflow shell script"
exit ${exit_status}