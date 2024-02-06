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

iternum=${2:-2}

runlabel='_'${iternum}

if [ "$1" ]; then
    curr_id=$1
else
    curr_id=$(date +"%Y%m%d")
fi

#itnum=${2:-1}
#curr_id=${curr_id}_${itnum}

code_run_name=gem0_surr_

run_dir_name=run_fusion_${code_run_name}${curr_id}${runlabel}
mkdir ${run_dir_name}

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
python read_profs.py ${code_run_name} ${curr_id}${runlabel}

# Save the results of the run and postprocessing
cd workflow/
tar -czvf ${run_dir_name}.tar.gz --exclude=*.cpo --exclude=*.dat ${run_dir_name}/ ${run_dir_name}_${curr_id}${runlabel}/
mv run_fusion_${code_run_name}${curr_id}${runlabel}.tar.gz ../../..

