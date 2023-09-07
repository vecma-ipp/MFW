#!/bin/sh

SCRATCH=$CINECA_SCRATCH
RUN_DIR_GEN=$SCRATCH'/VARY_1FT_GEM_NT_test'
#echo $RUN_DIR_GEN
#find $RUN_DIR_GEN/runs/ -type d -name run_*

# find list of run folders
RUN_DIR_LIST=$(find $RUN_DIR_GEN/runs/ -type d -name run_s*) # change the wildcard when needed
#echo $RUN_DIR_LIST

#create links for inputs and executable, and submit jobs

for R in $RUN_DIR_LIST; do
    echo $R
    cd $R
    mkdir MARCONI
    
    ln -s $RUN_DIR_GEN/common/gem.xml ./gem.xml
    ln -s $RUN_DIR_GEN/common/gem.xsd ./gem.xsd
    ln -s $RUN_DIR_GEN/common/gem_equilibrium_in.cpo ./gem_equilibrium_in.cpo
    ln -s $RUN_DIR_GEN/common/loop_gem_notransp ./MARCONI/loop_gem_notransp
    ln -s $HOME/code/MFW/uq/slurm/gem_loop_nt.sh ./slurm_batch.sh #check the script name

    # run SLURM scripts
    sbatch slurm_batch.sh # any modifications needed?

done
