# Required to run submitted jobs in QCG-PilotJob context with MPI.
# 

ifeq ($(SYS),EAGLE) 
module load impi
endif

ifeq ($(SYS),MARCONI) 
module load intel
module load intelmpi
endif
