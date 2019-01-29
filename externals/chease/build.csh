#!/bin/tcsh -fe

#source $ITMSCRIPTDIR/ITMv1 kepler test 4.10b.10_R2.2.0
    #source $ITMSCRIPTDIR/ITMv1 kepler test 4.10b_rc
    #module switch fc2k/rc
    #module switch scripts/R2.2
cd src-f90
make clean
make libchease_kepler
cd ..

# can test with: fc2k -kepler -docfile doc/chease.txt fc2k/chease.xml
echo " "
echo "Can execute: module switch fc2k/rc; fc2k -kepler -docfile doc/chease.txt fc2k/chease.xml"

# then can test with workflow in ./workflow_test/chease_test_workflow_nocpoin.xml which creates shot 12345, run=1 from no CPO (dummy shot=12345, run 4321 used)
# then can test with workflow in ./workflow_test/chease_test_workflow.xml using shot=12345, run=1 to create run=11

