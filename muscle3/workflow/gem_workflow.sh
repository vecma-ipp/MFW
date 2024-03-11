#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

### Export libraries
#export LD_LIBRARY_PATH=$MUSCLE3_HOME/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cobra/u/yyudin/muscle3/lib
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/cobra/u/yyudin/code/json-fortran/build/lib/

username=yyudin

echo 'Running gem workflow in Fortran'

### Activate virtual environment
#. ~/muscle3_venv/bin/activate
#source  ~/muscle3/bin/muscle3.env
. ~/muscle3/bin/muscle3.env

### Define the working directory
runlabel='_1'

currdate=$(date +"%Y%m%d")
code_run_name=gem0_

run_dir_name=run_fusion_${code_run_name}${currdate}${runlabel}
mkdir ${run_dir_name}

### Launch the workflow
#muscle_manager --start-all gem-fusion-m3.ymmsl &
muscle_manager --log-level DEBUG --run-dir ${run_dir_name} --start-all gem-fusion-m3.ymmsl &
#muscle_manager --log-level DEBUG --start-all gem0-fusion-m3.ymmsl &
#muscle_manager --log-level DEBUG --run-dir ${run_dir_name} --start-all gem-fusion-noeq-m3.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

### Next two lines just to get additional info about processes and cores
sleep 60
ps -u yyudin -o user,pid,pcpu,rss,vsz,psr,args

touch muscle3_manager.log
echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}

wait

# ### Copy the results in a folder for the next submission and continuation of the run 
# folder_to_copy_from=${run_dir_name}
# folder_to_copy_to=gem_resume_data
# files_to_copy=('instances/transport/workdir/*_ets_equilibrium_out.cpo' 'instances/transport/workdir/*_ets_coreprof_out.cpo' 'instances/turbulence/workdir/t*.dat')
# # all *.dat files may be added, but the the resulting folder size would be very large
# files_destination=('ets_equilibrium_in.cpo' 'ets_coreprof_in.cpo')

# for i in ${!files_to_copy[@]} ; do
#   cp ${folder_to_copy_from}/${files_to_copy[$i]} ${folder_to_copy_to}/${files_destination[$i]}
# done

# ### Archive and store the run folder (especially the checkpoint files)
# ### -  it takes too long to do it on a workign node -> external wrapping script needed?
# tar -czvf ${run_dir_name}.tar.gz ${run_dir_name}/
# mv ${run_dir_name}.tar.gz /ghi/r/${username:0:1}/${username}

### Make plots for the evolution of core profiles
cd ..
python read_profs.py ${code_run_name} ${currdate}${runlabel}

### Save the results of the run and postprocessing
cd workflow/
tar -czvf ${run_dir_name}.tar.gz --exclude=*.cpo --exclude=*.dat ${run_dir_name}/ ${run_dir_name}_${currdate}${runlabel}/
mv run_fusion_${code_run_name}${currdate}${runlabel}.tar.gz ../../..
