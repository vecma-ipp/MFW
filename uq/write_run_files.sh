#!/bin/bash -l

rundate='21072022'
runtime='1515'

jobslurmid='5745295'
jobcampid='v4b7_ppx'
nodenums=('3326')

# first try: 19072022 1100 5733612 3qa2cg2a ('6047' '6049')

# default+srun+flags: 19072022 1800 5736377 4e92_3xx ('6035')
# default+srun+no_flags: 21072022 1500 5745061 17bf_svp ('4521')
# srunmpi+exec+no_flags: 21072022 1515 5745295 v4b7_ppx ('3326')

### Make directory - copy - archivize

# The folders
mkdir run_${rundate}_${runtime}
mkdir run_${rundate}_${runtime}/input
mkdir run_${rundate}_${runtime}/input/tests
mkdir run_${rundate}_${runtime}/output
mkdir run_${rundate}_${runtime}/scratch

# The inputs
cp run_cobra_loop_gem_nt.sh run_${rundate}_${runtime}/input
cp tests/gem_notransp.py run_${rundate}_${runtime}/input/tests

# The outputs
cp test-loopntuq-*.${jobslurmid} run_${rundate}_${runtime}/output
#cp .*.hostfile run_${rundate}_${runtime}/output
#cp api.log run_${rundate}_${runtime}/output
cp -r .qcgpjm-service-co${nodenums[0]}.* run_${rundate}_${runtime}/output

for nodenum in ${nodenums[@]}; do
    cp nl-agent-co${nodenum}.log run_${rundate}_${runtime}/output
done

# The results in scratch
#cp -r /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid} run_${rundate}_${runtime}/scratch
cp /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid}/.qcgpj_in_act_* run_${rundate}_${runtime}/scratch
cp /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid}/.qcgpj_in_prev_* run_${rundate}_${runtime}/scratch
cp /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid}/stdout_* run_${rundate}_${runtime}/scratch
cp /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid}/stderr_* run_${rundate}_${runtime}/scratch
cp /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid}/campaign.db run_${rundate}_${runtime}/scratch

# The archive
tar -czvf run_${rundate}_${runtime}.tar.gz run_${rundate}_${runtime}
