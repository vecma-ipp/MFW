#! /bin/tcsh
echo "untaring the files..."
tar zxf libUAL.tar.gz
tar zxf tdi.tar.gz
#
echo "setting up the environment..."
setenv LD_LIBRARY_PATH /usr/java/j2re1.4.2_06/lib/i386:/usr/java/j2re1.4.2_06/lib/i386/server:$JAVA_HOME/jre/lib/i386:$JAVA_HOME/jre/lib/i386/server:$LD_LIBRARY_PATH
setenv LD_LIBRARY_PATH lib:$LD_LIBRARY_PATH
setenv MDS_PATH "tdi"
setenv euitm_path "enea142.efda-itm.eu::/pfs/work/imp3/coster/itmdb/itm_trees/test/4.06d/mdsplus/0;enea142.efda-itm.eu::/pfs/itmdb/itm_trees/public/models/4.06d/mdsplus"
#
echo "Starting program..."
mkdir -p data/OUTPUT
chmod u+x solver_test
./solver_test
echo "taring the results..."
tar zcvf ets.tar.gz data
