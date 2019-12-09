#!/bin/bash
#SBATCH --time=16:00:00                   # time limits
#SBATCH --nodes=1                         # nodes
#SBATCH --ntasks-per-node=1              # tasks per node
###SBATCH --cpus-per-task=1               # CPU per task
###SBATCH --mem=118GB                     # memory (max 180GB/node in skl_fua)
#SBATCH --partition=skl_fua_prod           # partition to be used
###SBATCH --qos=                          # quality of service
#SBATCH --job-name=MFW                    # job name
#SBATCH --err=test-%j.err                 # std-error file
#SBATCH --out=test-%j.out                 # std-output file
#SBATCH --account=FUA33_UQMWA 			      # account number
#SBATCH --mail-type=END				            # specify email notification
#SBATCH --mail-user=jalal.lakhlili@ipp.mpg.de	# e-mail address

export SYS=MARCONI
export MPICMD="mpirun -n 1"

python3 test_combined.py > outputs/logs/test.log.${SLURM_JOBID}
