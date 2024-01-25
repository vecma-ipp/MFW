#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cobra/u/yyudin/muscle3/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cobra/u/yyudin/code/json-fortran/build/lib/

echo 'Running gem_surr workflow in Fortran & Python'

#. ~/muscle3_venv/bin/activate
. ~/muscle3/bin/muscle3.env

runlabel='_0'

currdate=$(date +"%Y%m%d")
code_run_name=gem0_surr_

run_dir_name=run_fusion_${code_run_name}${currdate}${runlabel}

#muscle_manager --start-all gem-surr-fusion.ymmsl &
#muscle_manager --start-all gem-surr-mft-fusion.ymmsl &
muscle_manager --log-level DEBUG --run-dir ${run_dir_name} --start-all gem-surr-mft-fusion.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

#TODO: may be start with no MPI vesion
#TODO compile wrapper implementations
#TODO make sure MUSCLE3 works with FORTRAN

touch muscle3_manager.log

echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}

wait

# Make plots for the evolution of core profiles
cd ..
python read_profs.py ${code_run_name} ${currdate}${runlabel}

# Save the results of the run and postprocessing
tar -czvf run_fusion_${code_run_name}${currdate}${runlabel}.tar.gz --exclude=*.cpo --exclude=*.dat run_fusion_${code_run_name}${currdate}${runlabel}/ run_fusion_${code_run_name}${currdate}${runlabel}_${currdate}${runlabel}/
mv run_fusion_${code_run_name}${currdate}${runlabel}.tar.gz ../../..
