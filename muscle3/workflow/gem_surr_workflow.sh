#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

echo 'Running gem_surr workflow in Fortran & Python'

#. ~/muscle3_venv/bin/activate
. ~/muscle3/bin/muscle3.env


#muscle_manager --start-all gem-surr-fusion.ymmsl &
muscle_manager --start-all gem-surr-mft-fusion.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

#export LD_LIBRARY_PATH=$MUSCLE3_HOME/lib:$LD_LIBRARY_PATH
BINDIR=../bin/COBRA/
SRCDIR=../src/

#TODO: may be start with no MPI vesion
#TODO compile wrapper implementations
#TODO make sure MUSCLE3 works with FORTRAN

# $BINDIR/stop_M3 --muscle-instance=stop >'stop.log' 2>&1 &
# $BINDIR/duplicate_M3 --muscle-instance=duplicate >'duplicate.log' 2>&1 &
# python3 $SRCDIR/gem_surr_M3.py --muscle-instance=turbulence >'turbulence.log' 2>&1 &
# $BINDIR/chease_M3 --muscle-instance=equilibrium >'equilibrium.log' 2>&1 &
# $BINDIR/init_M3 --muscle-instance=init >'init.log' 2>&1 &
# $BINDIR/ets_M3 --muscle-instance=transport >'transport.log' 2>&1 &
# $BINDIR/imp4dv_M3 --muscle-instance=f2dv >'f2dv.log' 2>&1 &
# $BINDIR/cpdup_M3 --muscle-instance=cpdup >'cpdup.log' 2>&1 &

touch muscle3_manager.log

echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}

wait
