#!/bin/bash
#SBATCH --time=01:00:00         	# time limits
#SBATCH --nodes=4 			# nodes
#SBATCH --ntasks-per-node=32		# tasks per node
###SBATCH --cpus-per-task=1		# CPU per task
###SBATCH --mem=118GB			# memory (max 180GB/node in skl_fua)
#SBATCH --partition=skl_fua_dbg		# partition to be used
###SBATCH --qos=			# quality of service
#SBATCH --job-name=MFW		# job name
#SBATCH --err=test-%j.err			# std-error file
#SBATCH --out=test-%j.out			# std-output file
#SBATCH --account=FUA33_UQMWA 			# account number
#SBATCH --mail-type=END				# specify email notification
#SBATCH --mail-user=onnie.luk@ipp.mpg.de	# e-mail address

export QCG_KERNEL_turb=${QCG_KERNEL_turb:-128}
export CPO_INPUT_DIR=AUG_28906_4/

cd $PWD

module load intel intelmpi mkl fftw jre
source /marconi_work/FUA33_UQMWA/MUSCLE/compat-1.1/etc/muscle.profile

export MUSCLE_TMP_DIR=/tmp/

export MUSCLE_KERNELS_DIR=../kernels/bin/MARCONI
./update_turb_parms.py

echo "GEM cpu parameters:"
grep -A4 "npesx" gem.xml |grep "<n"

echo "STARTING MUSCLE FROM $PWD"
muscle2 -amc unified.cxa.rb
