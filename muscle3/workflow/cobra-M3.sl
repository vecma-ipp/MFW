#!/bin/bash
#SBATCH --time=13:00:00         	# time limits
#SBATCH --nodes=1 		     	# nodes

#SBATCH --ntasks-per-node=40		# tasks per node
#SBATCH --ntasks-per-core=1     # tasks per core
###SBATCH --cpus-per-task=1		# CPU per task

#SBATCH --partition=medium	# partition to be used
###SBATCH --qos=			# quality of service
#SBATCH --job-name=gem_M3		# job name
#SBATCH --err=test-%j.err			# std-error file
#SBATCH --out=test-%j.out			# std-output file
###SBATCH --account= 			# account number
#SBATCH --mail-type=ALL				# specify email notification
#SBATCH --mail-user=yehor.yudin@ipp.mpg.de	# e-mail address

#cd $PWD

#module purge
#module use -p $WORK/MODULES
#module load mfw/gnu-6.1.0
#module load git/2.39 intel/21.5.0 cmake/3.26 anaconda/3/2021.11 mkl/2020.1 impi/2021.5 fftw-mpi/3.3.10

export SYS=COBRA

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/cobra/u/yyudin/muscle3/lib

# For pinning threads correctly: (might only be needed for OpenMP)

#export OMP_PLACES=cores
#export OMP_PROC_BIND=spread

# DEBUG (next 5 lines): trying to resolve what seems to be running all logical processes on a single core
#unset I_MPI_PMI_LIBRARY 
#export I_MPI_JOB_RESPECT_PROCESS_PLACEMENT=0   # the option -ppn only works if you set this before
#export I_MPI_HYDRA_BOOTSTRAP_EXEC_EXTRA_ARGS="--exclusive"
#export I_MPI_PIN_RESPECT_CPUSET=enable
#export OMPI_MCA_hwloc_base_binding_policy=none

#KMP - check if needed
#cgroups - how many processor accessible

#echo "GEM cpu parameters:"
#grep -A4 "npesx" gem.xml |grep "<n"

echo "STARTING MUSCLE3 FROM $PWD"
#./gem0_workflow.sh	# gem0 workflow (no MPI)

MUSCLE3_HOME=/u/yyudin/muscle3 ./gem_workflow.sh
