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

echo PATH:
echo ${PATH}

echo PYTHONPATH:
echo ${PYTHONPATH}

echo "running the workflow with a root at "$(pwd)

# #DEBUG - script doomed to fail:
# python3 /u/yyudin/code/MFW/muscle3/src/gem_surr_mft_M3.py

#TODO: (a) modify the YMMSL beforehand
#      (b) pass id to muscle_manager: M3 'implementation' of gem_sur_imp has to accept arguments
#      (c) generate a sufficiently long (16 digits?) inside the M3 'implementation' -> done

muscle_manager --log-level DEBUG --run-dir ${run_dir_name} --start-all gem-surr-mft-fusion-independent.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

touch muscle3_manager.log

echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}

wait

# Make plots for the evolution of core profiles
# TODO makle read_profs.py work independently of locations
#cd ..
python read_profs.py ${code_run_name} ${curr_id}${runlabel}

# Copy the resulting core profile to the root directory
# -> done in parent workflow.sh

# Save the results of the run and postprocessing
# cd workflow/
# tar -czvf ${run_dir_name}.tar.gz --exclude=*.cpo --exclude=*.dat ${run_dir_name}/ ${run_dir_name}_${curr_id}${runlabel}/
# mv run_fusion_${code_run_name}${curr_id}${runlabel}.tar.gz ../../..
