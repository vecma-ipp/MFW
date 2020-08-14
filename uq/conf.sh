# Required to run submitted jobs in QCG-PilotJob context with MPI.
# 

ifeq ($(SYS),EAGLE) 
module load impi
endif

ifeq ($(SYS),COBRA) 
module load intel
module load impi
endif

ifeq ($(SYS),MARCONI) 
module load intel/pe-xe-2017--binary
module load intelmpi/2017--binary
endif
