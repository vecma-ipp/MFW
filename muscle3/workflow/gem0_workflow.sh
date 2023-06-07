#!/bin/bash

if [ -z "$MUSCLE3_HOME" ] ; then
    echo 'Error: MUSCLE3_HOME is not set.'
    echo "Use 'MUSCLE3_HOME=/path/to/muscle3 $0' to run the example"
    exit 1
fi

echo 'Running gem_sur_imp4dv workflow in Fortran & Python'

. ~/muscle3/bin/muscle3.env
muscle_manager --start-all gem0-fusion.ymmsl &

echo 'MUSCLE_MANAGER started'

manager_pid=$!

BINDIR=../bin/COBRA/
SRCDIR=../src/

#TODO: may be start with no MPI vesion
#TODO compile wrapper implementations
#TODO make sure MUSCLE3 works with FORTRAN

touch muscle3_manager.log

echo '> Now launching the manager'
tail -f muscle3_manager.log --pid=${manager_pid}

wait
