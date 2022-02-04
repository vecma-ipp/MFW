#!/bin/sh

# File to run a SLURM workflow consisting of several consequtive EasyVVUQ campaigns running a set of continius GEM runs,
# implemented as chaining of SLURM submissions

#0. State the total number of campaigns to run, and ordinal number of the last campaign in previous sequence
# number of runs
NUMRUNS=3 
# no of last/current run
CURRUN=${1:-3}
# no of the first run in the new sequence
FRUN=$((${CURRUN}+1))
# no of the last run in the new sequence
LASTRUN=$((${CURRUN}+${NUMRUNS}))

# batch script to submit a single UQ campaign
COM=run_marconi_loop_resume_gem_nt.sh
#TODO: reduce submision time in $COM SLURM script and number of code calls in GEM iterator

#1. First submission of a campaign, retrieve the SLURM job-id
PREVID=$(sbatch --wait ${COM})
#1'. Save and process the outputs after the finish of the submission
cd basicda
./gem_postproc_vary_test.sh ${FRUN}
cd ..
FRUN=$((${FRUN}+1))

#2. Loop over the rest of the campaign submissions
for n in `seq ${FRUN} ${LASTRUN}`; do
    #TODO: make while loop, or rather do-until loop
    CURID=$(sbatch --dependency=afterany:${PREVID} --wait ${COM})
    #TODO: ideally use =afterok and make sure there are no errors in the UQ script
    #TODO make sure that the output files have either continius numerations, or stored separately
    
    cd basicda
    ./gem_postproc_vary_test.sh ${n}
    cd ..

    PREVID=$CURID
done

