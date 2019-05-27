#!/bin/bash
#SBATCH --time=00:10:00         	# time limits
#SBATCH --nodes=1 			# nodes
#SBATCH --ntasks-per-node=4		# tasks per node
###SBATCH --cpus-per-task=1		# CPU per task
###SBATCH --mem=118GB			# memory (max 180GB/node in skl_fua)
#SBATCH --partition=skl_fua_dbg		# partition to be used
###SBATCH --qos=			# quality of service
#SBATCH --job-name=easyvvuq_pj		# job name
#SBATCH --err=error-%j.err			# std-error file
#SBATCH --out=output-%j.out			# std-output file
#SBATCH --account=FUA33_UQMWA			#FUA32_MCP-SB # account number
#SBATCH --mail-type=END				# specify email notification
#SBATCH --mail-user=onnie.luk@ipp.mpg.de	# e-mail address


module load python/3.6.4
. ~/.virtualenvs/easyvvuq-qcgpj/bin/activate

python3 test_pce_pj.py
