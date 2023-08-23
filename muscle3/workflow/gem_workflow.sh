#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

echo 'Running gem workflow in Fortran'

#. ~/muscle3_venv/bin/activate
. ~/muscle3/bin/muscle3.env
#source  ~/muscle3/bin/muscle3.env

currdate=$(date +"%Y%m%d")

#muscle_manager --start-all gem-fusion-m3.ymmsl &
muscle_manager --log-level DEBUG --run-dir run_fusion_gem_${currdate} --start-all gem-fusion-m3.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

#export LD_LIBRARY_PATH=$MUSCLE3_HOME/lib:$LD_LIBRARY_PATH

#BINDIR=../bin/COBRA/
#SRCDIR=../src/

#next two lines just to get additional info about processes and cores
sleep 60
ps -u yyudin -o user,pid,pcpu,rss,vsz,psr,args

touch muscle3_manager.log
echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}

wait

# Copy the results in a folder for the next submission and continuation of the run 
# (ALTERNATIVELY: pass the folder of the current finished run)
# TODO: get the last run folder from within the script
folder_to_copy_from=run_fusion_gem_${currdate}
folder_to_copy_to=gem_resume_data
files_to_copy=('instances/transport/workdir/*_ets_equilibrium_in.cpo' 'instances/transport/workdir/*_ets_coreprof_in.cpo' 'instances/turbulence/workdir/*.dat')
files_destination=('ets_equilibrium_in.cpo' 'ets_coreprof_in.cpo')

for i in ${!files_to_copy[@]} ; do
  cp ${folder_to_copy_from}/${files_to_copy[$i]} ${folder_to_copy_to}/${files_destination[$i]}
done
