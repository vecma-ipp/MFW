#!/bin/bash -l
##SBATCH -N 74
#SBATCH -n 128
#SBATCH --mail-type=END
#SBATCH --mail-user=olivier.hoenen@ipp.mpg.de
#SBATCH -J ComPat_GEM128
#SBATCH -o test-%j.out 
#SBATCH -e test-%j.err 
#SBATCH -t 2:00:00 
#SBATCH -A compatpsnc2
#SBATCH -p plgrid
####SBATCH --mem 16000

export QCG_KERNEL_turb=${QCG_KERNEL_turb:-128}
export CPO_INPUT_DIR=AUG_28906_4/

cd $PWD

export MODULEPATH=/home/plgrid-groups/plggcompat/.qcg-modules:$MODULEPATH

module load compat
module load compat/apps/fusion

export MUSCLE_TMP_DIR=/dev/shm/

ulimit -s 32000

./update_turb_parms.py

echo "GEM cpu parameters:"
grep -A4 "npesx" gem.xml |grep "<n"

echo "STARTING MUSCLE FROM $PWD"
muscle2 -amc unified.cxa.rb


