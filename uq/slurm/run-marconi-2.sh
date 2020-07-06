#!/bin/bash
#SBATCH --time=24:00:00         	# time limits
#SBATCH --nodes=22 			# nodes
#SBATCH --ntasks-per-node=48		# tasks per node
###SBATCH --cpus-per-task=1		# CPU per task
###SBATCH --mem=118GB			# memory (max 180GB/node in skl_fua)
#SBATCH --partition=skl_fua_prod	# partition to be used
###SBATCH --qos=			# quality of service
#SBATCH --job-name=prod-run		# job name
#SBATCH --err=test-%j.err			# std-error file
#SBATCH --out=test-%j.out			# std-output file
#SBATCH --account=FUA34_UQMWA2			#FUA33_UQMWA#FUA32_MCP-SB # account number
#SBATCH --mail-type=END				# specify email notification
#SBATCH --mail-user=onnie.luk@ipp.mpg.de	# e-mail address

export QCG_KERNEL_turb=${QCG_KERNEL_turb:-1024}
export CPO_INPUT_DIR=AUG_28906_6/

cd $PWD

module load intel intelmpi mkl fftw jre
source /marconi_work/FUA34_UQMWA2/MUSCLE/compat-1.2/etc/muscle.profile

export MUSCLE_TMP_DIR=/tmp/

export MUSCLE_KERNELS_DIR=../kernels/bin/MARCONI
./update_turb_parms.py

echo "GEM cpu parameters:"
grep -A4 "npesx" gem.xml |grep "<n"

echo "STARTING MUSCLE FROM $PWD"
muscle2 -amc unified.cxa.rb
