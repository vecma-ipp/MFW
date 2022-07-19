#!/bin/bash -l

rundate='19072022'
runtime='1100'

jobslurmid='5733612'
jobcampid='3qa2cg2a'
nodenums=('6047' '6049')

### Make directory - copy - archivize

mkdir run_${rundate}_${runtime}
mkdir run_${rundate}_${runtime}/input
mkdir run_${rundate}_${runtime}/input/tests
mkdir run_${rundate}_${runtime}/output
mkdir run_${rundate}_${runtime}/scratch

cp run_cobra_loop_gem_nt.sh run_${rundate}_${runtime}/input
cp tests/gem_notransp.py  run_${rundate}_${runtime}/input/tests

cp test-loopntuq-*.${jobslurmid} run_${rundate}_${runtime}/output
cp .*.hostfile run_${rundate}_${runtime}/output
cp api.log run_${rundate}_${runtime}/output
cp -r .qcgpjm-service-co${nodenum[0]}.* run_${rundate}_${runtime}/output

for nodenum in ${nodenums[@]}; do
    cp nl-agent-co${nodenum}.log run_${rundate}_${runtime}/output
done

cp -r /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid} run_${rundate}_${runtime}/scratch

tar -czvf run_${rundate}_${runtime}.tar.gz run_${rundate}_${runtime}

