#!/bin/bash
#SBATCH --time=1:00:00         	# time limits
#SBATCH --nodes=1 			# nodes
#SBATCH --ntasks-per-node=8		# tasks per node
###SBATCH --cpus-per-task=1		# CPU per task
#SBATCH --partition=medium	# partition to be used
###SBATCH --qos=			# quality of service
#SBATCH --job-name=gem_sur_M3		# job name
#SBATCH --err=test-%j.err			# std-error file
#SBATCH --out=test-%j.out			# std-output file
#SBATCH --account= 			# account number
#SBATCH --mail-type=END				# specify email notification
#SBATCH --mail-user=yehor.yudin@ipp.mpg.de	# e-mail address

cd $PWD

#module purge
#module use -p $WORK/MODULES
#module load mfw/gnu-6.1.0
export SYS=COBRA

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/cobra/u/yyudin/code/IMAS_PAF/


#echo "GEM cpu parameters:"
#grep -A4 "npesx" gem.xml |grep "<n"

echo "STARTING MUSCLE3 FROM $PWD"
#./gem0_workflow.sh	# gem0 workflow (no MPI)
MUSCLE3_HOME=/cobra/u/yyudin/IMAS_PAF/../muscle3 ./gem_sur_workflow.sh
