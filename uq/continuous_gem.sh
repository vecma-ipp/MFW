#!/bin/sh

# File to run a SLURM workflow consisting of several consequtive EasyVVUQ campaigns running a set of continius GEM runs,
# implemented as chaining of SLURM submissions

# Launch with:
# nohup ./continuous_gem.sh 1 > script_workflow_new_1.log 2>&1 &

#0. State the total number of campaigns to run, and ordinal number of the last campaign in previous sequence
echo "STARTING THE WORKFLOW"
# number of runs
NUMRUNS=3 
# no of current run, which is the last finished submission
CURRUN=${1:-1}
# no of the first run in the new sequence
FRUN=$((${CURRUN}+1))
# no of the last run in the new sequence
LASTRUN=$((${CURRUN}+${NUMRUNS}))

# batch script to submit a single UQ campaign
#COM=run_marconi_loop_resume_gem_nt.sh # for MARCONI
COM=run_cobra_loop_resume_gem_nt.sh # for COBRA

# TODO: add a first campaign, probably started with a different SLURM script using different non-restart python UQ script, and extract the folder name
#ROOTCAMPDIR='moj202gj' # at MARCONI
#ROOTCAMPDIR='1wu9k2wa' # at COBRA
export ROOTCAMPDIR=${2:-'brus48mm'}

# directory ID of an original UQ campaign

echo "Before first submission, here are the numbers"
echo "Total number of new runs: "$NUMRUNS
echo "Last completed run number: "$CURRUN
echo "First new run number: "$FRUN
echo "Number of last run in this submission: "$LASTRUN

#1. First submission of a campaign, retrieve the SLURM job-id
echo "Starting the very first SLURM submission with UQ campaign"

CPONUM=${FRUN}
#PREVID=$(sbatch --parsable --wait ${COM} 2>&1 | sed 's/[S,a-z]* //g')
PREVID=$(sbatch --export=ALL,CPONUM=${FRUN},OLDCAMP=${ROOTCAMPDIR} --parsable --wait ${COM})

#1'. Save and process the outputs after the finish of the submission
echo "Finished the first new UQ campaign "${PREVID}

#NOTE: the call of postrpocessing scripts is moved to the SLURM submission
echo "Now postprocessing for campaign "${PREVID}
cd basicda
./gem_postproc_vary_test.sh ${FRUN} ${ROOTCAMPDIR}
cd ..

FRUN=$((${FRUN}+1))
echo "Finished prostprocessing, next run number: "$FRUN

#2. Loop over the rest of the campaign submissions
for n in `seq ${FRUN} ${LASTRUN}`; do
    #TODO: make while loop, or rather do-until loop
    
    echo "Starting next submission after "${PREVID}" , no "${n} 

    CPONUM=${n}
    #CURID=$(sbatch --export=ALL,CPONUM=${n} --parsable --dependency=afterany:${PREVID}:+3 --wait ${COM}  2>&1 | sed 's/[S,a-z]* //g')
    CURID=$(sbatch --export=ALL,CPONUM=${n},OLDCAMP=${ROOTCAMPDIR} --parsable --dependency=afterany:${PREVID}:+3 --wait ${COM})
    #TODO: ideally use =afterok and make sure there are no errors in the UQ script
    #TODO make sure that the output files either have continius numerations or stored separately
    
    echo "Finished UQ campaign "${CURID}" , no "${n}

    #NOTEL in principle the postprocessing script is called in SLURM submission, but if the argument (run number) is correct, postprocessing should be idempotent
    echo "Now postprocessing for campaign "${PREVID}
    cd basicda
    ./gem_postproc_vary_test.sh ${n} ${ROOTCAMPDIR}
    cd ..

    PREVID=${CURID}
    echo "Finished postprocessing for the campaing no "${n}
done

echo "FINISHED THE WORKFLOW"

