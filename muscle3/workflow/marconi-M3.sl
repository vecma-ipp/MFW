#!/bin/bash
#SBATCH --time=00:10:00         	# time limits
#SBATCH --nodes=1 			# nodes
#SBATCH --ntasks-per-node=2		# tasks per node
###SBATCH --cpus-per-task=1		# CPU per task
###SBATCH --mem=118GB			# memory (max 180GB/node in skl_fua)
#SBATCH --partition=skl_fua_dbg		# partition to be used (max. 2hr for dbg)
###SBATCH --qos=			# quality of service
#SBATCH --job-name=gem_M3		# job name
#SBATCH --err=test-%j.err			# std-error file
#SBATCH --out=test-%j.out			# std-output file
#SBATCH --account=FUA34_UQMWA2 			# account number
#SBATCH --mail-type=END				# specify email notification
#SBATCH --mail-user=onnie.luk@ipp.mpg.de	# e-mail address

cd $PWD

module purge
module use -p $WORK/MODULES
module load mfw/gnu-6.1.0
export SYS=MARCONI-GNU

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/marconi/home/userexternal/oluk0000/muscle3/lib


#echo "GEM cpu parameters:"
#grep -A4 "npesx" gem.xml |grep "<n"

echo "STARTING MUSCLE FROM $PWD"
#./gem0_workflow.sh	# gem0 workflow (no MPI)
MUSCLE3_HOME=/marconi/home/userexternal/oluk0000/muscle3 ./gem_workflow.sh

