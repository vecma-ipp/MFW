#!/bin/bash -l

rundate='22072022'
runtime='1115'

jobslurmid='5749203'
jobcampid='1wu9k2wa'
nodenums=('4540' '4566' '4588' '4625')

### Recording QCG-PJ issues

# first try: 19072022 1100 5733612 3qa2cg2a ('6047' '6049')

# default+srun+flags: 19072022 1800 5736377 4e92_3xx ('6035')  -> SLURM halt

# srunmpi+exec+no_flags: 21072022 1500 5745061 17bf_svp ('4521') > MPI_Init erro
# default+srun+no_flags: 21072022 1515 5745295 v4b7_ppx ('3326') -> SLURM halt
# srunmpi+exec+no_flags: 21072022 1600 5745650 kzpli7gh ('4321') -> SLURM halt

# srunmpi+srun+no_flag: 21972022 1630 5745709 flwkdt2q ('4371') -> MPI_Init error
# default+srun+no_flags: 21972022 1645 5745754 ue1y9103 ('3570') -> SLURM halt

# default+srun+no_flags+npess32(2 independent nodes): 22072022 1030 5748864 sz6hftfj ('4585' '4590') -> 2/2 codes running!
# default+srun+no_flags+npess16(1 node): 22072022 1045 5748887 s491vudh ('3566') -> SLURM disables creation
# default+srun+no_flags+npess16(4 ind runs/ 2 nodes): 22072022 1100 5749169 a52yn85j ('3600' '3609' '3619' '3607') -> halt of 2/4 jobs
# default+srun+no_flags+npess32(4 ind runs/ 4 nodes): 22072022 1115 5749203 1wu9k2wa ('4540' '4566' '4588' '4625') -> 4/4 codes running!

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
cp -r /ptmp/yyudin/VARY_1FT_GEM_NT_${jobcampid}/common/ run_${rundate}_${runtime}/scratch

# The archive
tar -czvf run_${rundate}_${runtime}.tar.gz run_${rundate}_${runtime}
