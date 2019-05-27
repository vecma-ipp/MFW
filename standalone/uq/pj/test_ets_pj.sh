#!/bin/bash

## job name
#SBATCH --job-name=ets_pj

## stdout file
#SBATCH --output=output-%j.txt

## stderr file
#SBATCH --error=error-%j.txt

## wall time in format MINUTES:SECONDS
#SBATCH --time=10:00

## number of nodes
#SBATCH --nodes=1

## tasks per node
#SBATCH --tasks-per-node=4

## queue name
#SBATCH --partition=fast

## grant
#SBATCH --account=ETS_UQ

module load python/3.7.3
. ~/.virtualenvs/easyvvuq-qcgpj/bin/activate

python3 ets_pj_test.py
