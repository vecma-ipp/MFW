#!/bin/bash -l

## job name
#SBATCH --job-name=ARCHIVE_FILES_

## stdout and stderr files
#SBATCH --output=archive-out.%j
#SBATCH --error=archive-err.%j

## wall time in format (HOURS):MINUTES:SECONDS
#SBATCH --time=24:00:00

## number of nodes and tasks per node
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
###SBATCH --ntasks-per-core=1
###SBATCH --cpus-per-task=8

#SBATCH --partition=medium
###SBATCH --qos=

#SBATCH --profile=all
###SBATCH --oversubscribe

## grant
###SBATCH --account=
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yyudin@ipp.mpg.de

#############################################

export DIR_BASE=VARY_1FT_GEM_NT_
export DIR_NAME=akgbbn1a


echo 'Archiving files in '${DIR_BASE}${DIR_NAME}

tar -czvf  /r/y/yyudin/${DIR_BASE}${DIR_NAME}.tar.gz /ptmp/yyudin/${DIR_BASE}${DIR_NAME}/runs/dat/9/

tar -ztvf  /r/y/yyudin/${DIR_BASE}${DIR_NAME}.tar.gz &> log_${DIR_NAME}.txt

echo 'Finished archiving'

