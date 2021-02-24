#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

echo 'Running gem workflow in Fortran'

. ~/muscle3_venv/bin/activate
muscle_manager gem-fusion.ymmsl &

manager_pid=$!

#export LD_LIBRARY_PATH=$MUSCLE3_HOME/lib:$LD_LIBRARY_PATH
BINDIR=../bin/MARCONI-GNU

$BINDIR/stop_M3 --muscle-instance=stop >'stop.log' 2>&1 &
$BINDIR/duplicate_M3 --muscle-instance=duplicate >'duplicate.log' 2>&1 &
mpirun -n 2 ./$BINDIR/gem_M3 --muscle-instance=turbulence >'turbulence.log' 2>&1 &
$BINDIR/chease_M3 --muscle-instance=equilibrium >'equilibrium.log' 2>&1 &
$BINDIR/init_M3 --muscle-instance=init >'init.log' 2>&1 &
$BINDIR/ets_M3 --muscle-instance=transport >'transport.log' 2>&1 &

touch muscle3_manager.log
tail -f muscle3_manager.log --pid=${manager_pid}

wait
