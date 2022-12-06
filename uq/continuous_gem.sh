#!/bin/sh

# File to run a SLURM workflow consisting of several consequtive EasyVVUQ campaigns running a set of continius GEM runs,
# implemented as chaining of SLURM submissions

# Launch with:
# nohup ./continuous_gem.sh 17 aos1mzke 1> script_workflow_latest3.log 2>&1 &
# nohup ./continuous_gem.sh 6  akgbbn1a 1> script_wf_20102022.log 2>&1 &
# nohup ./continuous_gem.sh 2  w468l7ng 1> script_wf_06122022.log 2>&1 &
# nohup ./continuous_gem.sh 1  rp1pw2y6 1> script_wf_05122022.log 2>&1 &

#0. State the total number of campaigns to run, and ordinal number of the last campaign in previous sequence
echo "STARTING THE WORKFLOW"
# number of runs
NUMRUNS=3
# no of current run, which is the last finished submission
CURRUN=${1:-0}
# no of the first run in the new sequence
FRUN=$((${CURRUN}+1))
# no of the last run in the new sequence
LASTRUN=$((${CURRUN}+${NUMRUNS}))

# polynomial order - current parameter regulating total number of code instances
POLORDER=3
INPUT_DIM=4
#ATTENTION: arbitrary param to set number of core instances
NUM_CODE_INSTS=3

# batch script to submit a single UQ campaign
#COM=run_marconi_loop_resume_gem_nt.sh # for MARCONI
COM=run_cobra_loop_resume_gem_nt.sh # for COBRA
COM0=run_cobra_loop_gem_nt.sh

RUNRANGESTART=1
#RUNRANGE=16
if [ -n "${NUM_CODE_INSTS}" ];
then
  RUNRANGE=${NUM_CODE_INSTS}
else
  RUNRANGE=$(( $((${POLORDER}+1)) ** ${INPUT_DIM} ))
fi

CPONUM=${FRUN}

echo "Before first submission, here are the numbers"
echo "Total number of new runs: "${NUMRUNS}

export ORIGDIR=$(pwd)

# Following can be anything but should be consistent with prevoius and further scripts
#export CAMP_NAME_PREFIX=VARY_1FT_GEM_NT_
export CAMP_NAME_PREFIX=VARY_1FT_GEM_NT_

#TODO: akgbbn1a cpo2+ are overwritten - if the very first campaign fails, the script will overwrite the old campaign output file 
# - the runs will be consistent though due to restoration from the old cpo numbers

if [ "${CURRUN}" -gt 0 ];
then 

  #ROOTCAMPDIR='moj202gj' # at MARCONI
  #ROOTCAMPDIR='1wu9k2wa' # at COBRA
  export ROOTCAMPDIR=${2:-'aos1mzke'} #brus48mm

  # directory ID of an original UQ campaign

  echo "Last completed run number: "${CURRUN}
  echo "First new run number: "${FRUN}
  echo "Number of last run in this submission: "${LASTRUN}

  # 0.0. Backing up snapshot files from the last runs

  BCKP_FILES=('gem_coretransp*.cpo' '*.dat' 'fout_0*' 'stopped')

  TMP=${RANDOM}
  #RUNPATHSHORT=runs/runs_0-100000000/runs_0-1000000/runs_0-10000/runs_0-100
  RUNPATHSHORT=runs/

  #mkdir ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/bckp/
  mkdir -p ${SCRATCH}/${CAMP_NAME_PREFIX}${ROOTCAMPDIR}/${RUNPATHSHORT}/bckp/${TMP}

  cd ${SCRATCH}/${CAMP_NAME_PREFIX}${ROOTCAMPDIR}/${RUNPATHSHORT}/

  #for r in `seq ${RUNRANGESTART} ${RUNRANGE}`; do 
  for r in $(find -maxdepth 5 -mindepth 5 -type d -name "run_*" | sed "s|^\.||"); do 

    #mkdir ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/bckp/${TMP}/run_${r}/
    mkdir -p bckp/${TMP}/${r}/

    for bckp_f in ${BCKP_FILES[@]} ; do 

      #cp ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/run_${r}/${bckp_f} ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/bckp/${TMP}/run_${r}/
      cp ${r}/${bckp_f} /bckp/${TMP}/${r}/

      cp ${SCRATCH}/${CAMP_NAME_PREFIX}${ROOTCAMPDIR}/campaign.db /bckp/${TMP}/

    done

  done

  # 0.1. Restoring snapshot files to the state of desired last completed run
  #for r in `seq ${RUNRANGESTART} ${RUNRANGE}`; do 
  for r in $(find -maxdepth 5 -mindepth 5 -type d -name "run_*" | sed "s|^\.||"); do 

    #cp ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/dat/${CURRUN}/run_${r}/*.dat ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/run_${r}/
    cp /dat/${CURRUN}/${r}/*.dat ${r}/

    #cp ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/dat/${CURRUN}/run_${r}/fout_0* ${SCRATCH}/VARY_1FT_GEM_NT_${ROOTCAMPDIR}/${RUNPATHSHORT}/run_${r}/
    cp /dat/${CURRUN}/${r}/fout_0* ${r}/

  done
  
  #cd ${HOME}/code/MFW/uq/
  cd ${ORIGDIR}

  #1. First submission of a campaign, retrieve the SLURM job-id
  echo "Starting the very first SLURM submission with UQ campaign"

  #PREVID=$(sbatch --parsable --wait ${COM} 2>&1 | sed 's/[S,a-z]* //g')
  PREVID=$(sbatch --export=ALL,CPONUM=${FRUN},OLDCAMP=${ROOTCAMPDIR},POLORDER=${POLORDER},RUNRANGE=${RUNRANGE},CAMP_NAME_PREFIX=${CAMP_NAME_PREFIX} --parsable --wait ${COM})

  #1'. Save and process the outputs after the finish of the submission
  echo "Finished the first new UQ campaign "${PREVID}
  #TODO: after here, somewhere here a dangling quotation mark is found

else
  # Option for starting the sequence from the very scratch, with no prior folder and DB

  echo "Last completed run number is 0, no previous runs: "${CURRUN}
  echo "First new run number should be 1: "${FRUN}
  echo "Number of last run in this submission: "${LASTRUN}

  #1. First submission of a campaign, retrieve the SLURM job-id
  echo "Starting the VERY ACTUAL first SLURM submission with UQ campaign"

  PREVID=$(sbatch --export=ALL,POLORDER=${POLORDER},RUNRANGE=${RUNRANGE} --parsable --wait ${COM0})

  #Extract the ROOTCAMPDIR from the submission
  ROOTCAMPDIR=$(<camp_temp_dir.txt) 
  # TODO: make distinguishable, a single name for all cuncurrently run workflows
  echo 'Campaign directory for this workflow is: '${ROOTCAMPDIR}

  #1'. Save and process the outputs after the finish of the submission
  echo "Finished the VERY ACTUALLY first new UQ campaign "${PREVID}

fi

#NOTE: the call of postrpocessing scripts is moved to the SLURM submission
echo "Now postprocessing for campaign "${PREVID}
cd basicda
./gem_postproc_vary_test.sh ${FRUN} ${ROOTCAMPDIR} 1 1 ${RUNRANGE} 1 0
cd ..

FRUN=$((${FRUN}+1))
echo "Finished prostprocessing, next run number: "${FRUN}

#2. Loop over the rest of the campaign submissions
for n in `seq ${FRUN} ${LASTRUN}`; do 
    #TODO: make while loop, or rather do-until loop
    
    echo "Starting next submission after "${PREVID}" , num "${n} 

    CPONUM=${n}
    #CURID=$(sbatch --export=ALL,CPONUM=${n} --parsable --dependency=afterany:${PREVID}:+3 --wait ${COM}  2>&1 | sed 's/[S,a-z]* //g')
    CURID=$(sbatch --export=ALL,CPONUM=${n},OLDCAMP=${ROOTCAMPDIR},POLORDER=${POLORDER},RUNRANGE=${RUNRANGE},CAMP_NAME_PREFIX=${CAMP_NAME_PREFIX} --parsable --dependency=afterany:${PREVID}:+3 --wait ${COM})
    #TODO: ideally use =afterok and make sure there are no errors in the UQ script
    #TODO make sure that the output files either have continuous numerations or stored separately
    
    echo "Finished UQ campaign "${CURID}" , num "${n}

    #NOTE: in principle the postprocessing script is called in SLURM submission, but if the argument (run number) is correct, postprocessing should be idempotent
    echo "Now postprocessing for campaign "${PREVID}
    cd basicda
    ./gem_postproc_vary_test.sh ${n} ${ROOTCAMPDIR} 1 1 ${RUNRANGE} 1 0
    cd ..

    PREVID=${CURID}
    echo "Finished postprocessing for the campaign num "${n}
done

yes | rm camp_temp_dir.txt

echo "FINISHED THE WORKFLOW"
